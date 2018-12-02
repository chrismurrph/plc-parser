(ns stateful-instructions
  (:require [context :as con]
            [utils :as u]))

(def jump-to (atom nil))

(defn jumping? []
  @jump-to)

(defn lbl [infos energy tags]
  (assert (not (jumping?)) (str "If jumping supposed to be caught already"))
  ;(println (str "LBL is NOP when not jumping"))
  energy)

(defn jmp [infos energy tags]
  (if (not energy)
    false
    (let [_ (reset! jump-to (first tags))
          ;_ (println (str "JMP turned on for: " @jump-to))
          ]
      true)))

;;
;; Not yet struck executing multiple input and/or an output jsr calls. When do jsr instruction can find out the
;; :input-values and put them into jsr-atom as a vector. Then sbr instruction can populate :tag-names. :tag-names
;; will be map with name as key and input value as value. Get rid of the :input-values vector at this stage.
;; We will need a reader to use this map.
;;

(def jsr-atom (atom {:input-values []
                     :tag-names {}}))

(defn read-local-value [name]
  (get-in @jsr-atom [:tag-names name]))

(defn sbr [infos energy tags]
  ;(println (str "SBR: " @jsr-atom))
  energy)

(defn ret [infos energy tags]
  ;(println (str "RET: " @jsr-atom))
  energy)

(defn jsr [infos energy tags]
  (let [[routine-name [input-arg-count-str & tail]] tags
        scan-routine-fn (:info/scan-routine infos)
        routine-fn (:info/get-routine infos)
        routine (routine-fn routine-name)]
    (when (and energy (not (jumping?)))
      (let [scan-routine-fn (:info/scan-routine infos)
            _ (assert routine-fn)
            count-input-args (u/string->int input-arg-count-str)
            _ (assert (and (zero? (count tail)) (zero? count-input-args)) (str "Are these params: <" tail ">" (con/out-context)))
            ;_ (println "About to JSR routine: <" routine-name ">, with num inputs: " input-arg-count-str)
            _ (scan-routine-fn routine)
            ;_ (println "Scanned JSR routine: <" routine-name ">")
            ]
        ))
    (when (jumping?)
      (assert false (str "Having to do again - just want to see this happen - when jump is searching for a label that is behind it"))
      (scan-routine-fn routine)
      (when (jumping?)
        (assert false (str "Going and doing a routine one more time should have discovered that label")))))
  energy)
