(ns sys.cmd
  (:require
    [clojure.core.async :refer (close! alts! timeout <! <!! >! >!! put! chan go go-loop)]))

(def debug? true)

(defn p [txt]
  (when debug?
    (println txt)))

(def plc-remote (chan))
(def atmosphere-remote (chan))

;;
;; sets the weather, but don't expect the change to be picked up until the tube
;; has set for is sampled.
;;
(defn set-weather [point-num gas-kw value]
  (assert (number? point-num))
  (assert (keyword? gas-kw))
  (let [set-gas {:cmd       :set-value
                 :point-num point-num
                 :metric-id gas-kw
                 :value     value}]
    (go (>! atmosphere-remote set-gas)
        (let [res (<! atmosphere-remote)]
          (p (str "Was a success?:" res))))))

;;
;; Destructure for result:
;; {:keys [oxygen methane carbon-dioxide carbon-monoxide]}
;;
(defn get-weather [point-num]
  (let [values-go-block (go
                          (>! atmosphere-remote {:cmd       :get-values
                                                 :point-num point-num})
                          (<! atmosphere-remote))
        ]
    (<!! values-go-block)))

(defn slow []
  (>!! plc-remote {:cmd :slow}))

(defn fast []
  (>!! plc-remote {:cmd :fast}))

(defn fast-briefly []
  (>!! plc-remote {:cmd :fast-briefly}))

;;
;; case ToggleDept.BagSamplingStartPushDown =>
;;   briefTouch(requestor, c.HMI_W.Tog_Bag_Sam_Start, id, Some(c.SCADA_R_Bag_Sampling_Input), usualRequest)
;;
(defn bag-sample-on []
  (>!! plc-remote {:cmd      :touch-on
                   :tag-name "HMI_W.Tog_Bag_Sam_Start"}))

;;
;; case ToggleDept.BagSamplingStopPushDown =>
;;   briefTouch(requestor, c.HMI_W.Tog_Bag_Sam_Stop, id, Some(c.SCADA_R_Bag_Sampling_Input), usualRequest)
;;
(defn bag-sample-off []
  (>!! plc-remote {:cmd      :touch-on
                   :tag-name "HMI_W.Tog_Bag_Sam_Stop"}))

;;
;; case ToggleDept.AutoCalibrationPushDown =>
;;  briefTouch(requestor, c.Always.MonthlyOneShot, id, Some(c.SCADA_R_Analyser_Calibrating_Auto_Input), extraUsualRequest, debug = true)
;;
(defn auto-calibration-go []
  (>!! plc-remote {:cmd      :touch-on
                   :tag-name "Always.MonthlyOneShot"}))

;;
;; case ToggleDept.ManCalibrationStartPushDown =>
;;   briefTouch(requestor, c.HMI_W.Man_Cal_Enter, id, Some(c.SCADA_R_Analyser_Calibrating_Man_Input), manualCalibrationRequest)
;;
(defn man-calibration-on []
  (>!! plc-remote {:cmd      :touch-on
                   :tag-name "HMI_W.Man_Cal_Enter"}))

;;
;; case ToggleDept.ManCalibrationStopPushDown =>
;;   briefTouch(requestor, c.HMI_W.Man_Cal_Exit, id, Some(c.SCADA_R_Analyser_Calibrating_Man_Input), manualCalibrationRequest)
;;
(defn man-calibration-off []
  (>!! plc-remote {:cmd      :touch-on
                   :tag-name "HMI_W.Man_Cal_Exit"}))

(defn on-off-kw [on?]
  (if on? :turn-on :turn-off))

;;
;; case ToggleDept.ZeroCalibrationOn =>
;;   requestor ! ReadAck(id, turnOn(c.HMI_W.Man_Zero_Cal))
;;
;; case ToggleDept.ZeroCalibrationOff =>
;;   requestor ! ReadAck(id, turnOff(c.HMI_W.Man_Zero_Cal))
;;
(defn zero-flow-change [on?]
  (>!! plc-remote {:cmd      (on-off-kw on?)
                   :tag-name "HMI_W.Flo_Zero"}))
(defn zero-calibration-change [on?]
  (>!! plc-remote {:cmd      (on-off-kw on?)
                   :tag-name "HMI_W.Man_Zero_Cal"}))
(defn methane-flow-change [on?]
  (>!! plc-remote {:cmd      (on-off-kw on?)
                   :tag-name "HMI_W.Flo_CH4"}))
(defn carbon-dioxide-flow-change [on?]
  (>!! plc-remote {:cmd      (on-off-kw on?)
                   :tag-name "HMI_W.Flo_CO2"}))
(defn carbon-monoxide-flow-change [on?]
  (>!! plc-remote {:cmd      (on-off-kw on?)
                   :tag-name "HMI_W.Flo_CO"}))
(defn oxygen-flow-change [on?]
  (>!! plc-remote {:cmd      (on-off-kw on?)
                   :tag-name "HMI_W.Flo_O2"}))

;;
;; case ToggleDept.SpanO2PushDown =>
;;   briefTouch(requestor, c.HMI_W.Man_Span_O2, id, None, usualRequest)
;;
(defn span-oxygen-go []
  (>!! plc-remote {:cmd      :touch-on
                   :tag-name "HMI_W.Man_Span_O2"}))

(defn read-register [tag-name]
  (let [go-block (go
                   (>! plc-remote {:cmd      :read-register
                                   :tag-name tag-name})
                   (<! plc-remote))
        res (<!! go-block)
        ]
    res))

(defn write-register [tag-name tag-value]
  (>!! plc-remote {:cmd       :write-register
                   :tag-name  tag-name
                   :tag-value tag-value}))

;;
;; Test for a tag that only exists in one program, so will cause failure 2 out of 3 times. Hmm - not true because
;; of the existence of the cache. If the tag ("Byp_Pmp_1") has been set before it will exist in the cache and be
;; updated. Fact that it is not in the current program is not relevant. Important to note that the cache is the
;; 'one source of truth' after initial values have been retrieved from the .L5K file that parsed in.
;;
(defn failure-sometimes []
  (>!! plc-remote {:cmd      :touch-on
                   :tag-name "Byp_Pmp_1"}))

;;
;; Just wanted to test that these two crash, which they do. differently.
;;

(defn expect-problem-1 []
  (>!! plc-remote {:cmd      :touch-on
                   :tag-name "NotExists"}))

(defn expect-problem-2 []
  (>!! plc-remote {:cmd      :touch-on
                   :tag-name "HMI_W.NotExists"}))