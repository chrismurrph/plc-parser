(ns sys.aa9.smartgas
  (:require
    [sys.domain-utils :as du]
    [clojure.core.async :refer (close! alts! timeout <! <!! >! >!! put! chan go go-loop)]))

(defn issue-warnings [tag-reader num-points main-program-name]
  (let [read-tag (tag-reader main-program-name)
        buffer (read-tag "SMRTGS_W_Tubes_Seq_Buffer")
        tube-nums (map (comp :val/val first) buffer)
        biggest-tube (apply max tube-nums)]
    (when (> biggest-tube num-points)
      (println (str "WARNING: biggest-tube: <" biggest-tube ">, yet weather only being produced for: <" num-points "> tubes (first tube is 1)")))))

;;
;; Effectively does the job of sucking up the tube and putting into the analyser. The concentration can vary but is
;; not likely to (changes to weather are done by random selection of point and metric).
;;
(defn inject-weather-into-analyser [read-tag write-tag remote-ch]
  (let [
        plc-state {:purging          (read-tag "L1.S3.DO.4")
                   :switched-to-tube (read-tag "SMRTGS_W_Switched_To_Tube_Num")
                   ;:official-read-moment (read-tag "Official_Read_Moment")
                   ;:take-manual-sample   (read-tag "SMRTGS_W_Take_Manual_Sample")
                   }
        {:keys [switched-to-tube purging]} plc-state
        values-go-block (go
                          (>! remote-ch {:cmd       :get-values
                                         :point-num switched-to-tube})
                          (<! remote-ch))
        {:keys [oxygen methane carbon-dioxide carbon-monoxide]} (<!! values-go-block)
        ]
    (write-tag "A1_METHANE" (:value methane))
    (write-tag "A2_CARBON_MONOXIDE" (:value carbon-monoxide))
    (when (du/is-not? purging)
      (write-tag "A2_OXYGEN" (:value oxygen)))
    (write-tag "A1_CARBON_DIOXIDE" (:value carbon-dioxide))))
