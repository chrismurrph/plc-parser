(ns sys.system
  (:require
    [clojure.core.async :refer (close! alts! timeout <! <!! >! >!! put! chan go go-loop)]
    [sys.atmosphere :as atmos]
    [sys.plc :as plc]
    [sys.cmd :as cmd]
    [running :as run]
    [gen :as gen]
    [sys.aa9.controller :as aa9-controller]
    [sys.aa9.changes :as aa9-changes]
    [sys.aa9.smartgas :as aa9-smartgas]))

(def debug? true)

(defn p [txt]
  (when debug?
    (println txt)))

;;
;; Dereference this to get the stopping function that will stop the system
;;
(defonce system (atom nil))

(def metrics [{:metric-id :oxygen
               :generator (gen/generator 21.00
                                         11.0
                                         23.5)}
              {:metric-id :methane
               :generator (gen/generator 0.00018
                                         0.0
                                         1.1)}
              {:metric-id :carbon-dioxide
               :generator (gen/generator 0.04
                                         0.0
                                         1.36)}
              {:metric-id :carbon-monoxide
               :generator (gen/generator 0.2
                                         0.0
                                         56.0)}])

(def millis-pause 2000)
(def sticky-minutes 10)

(defn make-system-container! [site]
  (let [{:keys [site/name site/num-points]} site
        _ (println (str "[system] starting: " name))
        components [(plc/make-plc-component cmd/plc-remote cmd/atmosphere-remote site)
                    (atmos/make-atmosphere-component cmd/atmosphere-remote metrics num-points millis-pause sticky-minutes)]
        ;_ (println "Num of components in started system is " (count components))
        ;_ (example-call atmosphere-remote)
        ]
    (fn stop! []
      (println "[system] stopping")
      (reset! system nil)
      (doseq [{:keys [stop-f id] :as component} components]
        (stop-f)))))

(defn stop []
  (when-let [stop-system! @system]
    (stop-system!)))

(defn going? []
  (not= @system nil))

(def site {:site/name                "AA9"
           :site/dir-name            aa9-controller/dir-name
           ;; This is a hardware number, its not even in the PLC
           :site/num-points          32
           :site/inject-weather-fn   aa9-smartgas/inject-weather-into-analyser
           :site/issue-warnings-fn   aa9-smartgas/issue-warnings
           :site/controller-infos-fn aa9-controller/controller-infos
           :site/main-program        aa9-controller/main-program
           :site/changes             aa9-changes/changes})

(defn start []
  (stop)
  (run/reset-tags)
  (reset! system (make-system-container! site)))
