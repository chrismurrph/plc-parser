(ns sys.atmosphere
  (:require
    [clojure.core.async :refer (close! alts! timeout <! <!! >! >!! put! chan go go-loop)]
    [utils :as u]))

(def debug? false)

(defn p [txt]
  (when debug?
    (println txt)))

(defn make-values-at-point [metrics]
  (fn [point-num]
    (into {} (mapv (fn [{:keys [metric-id generator] :as metric}]
                     (let [generated-value (generator)
                           _ (assert generated-value (str "No value able to be generated for point " point-num ", metric: " metric-id))]
                       [metric-id
                        {:metric-id      metric-id
                         :point-num      point-num
                         :value          generated-value
                         :set-at-by-user nil}])) metrics))))

;;
;; If we generate 3 yet select the closest to the current value, then there will tend to be the
;; appearance of drift, rather than each value having no relation to the last
;;
(defn next-value [current-val generator]
  (let [possibs (into #{} (take 3 (repeatedly generator)))
        possibs-with-diff (map (fn [possib] [possib (- possib current-val)]) possibs)
        ;_ (println possibs-with-diff)
        least-diff (apply min (map (comp u/abs second) possibs-with-diff))
        res (ffirst (filter #(= least-diff (-> % second u/abs)) possibs-with-diff))
        _ (assert res (str "No next-value from <" current-val ">, possibs: " (seq possibs-with-diff) ", " least-diff))
        ]
    res))

(defn make-atmosphere-component [command-ch metrics num-points millis-pause sticky-minutes]
  (println (str "[atmosphere-component] starting"))
  (let [_ (assert (vector? metrics))
        values-maker (make-values-at-point metrics)
        initial-data (into {} (mapv (fn [num] [num (values-maker num)]) (range 1 (inc num-points))))
        ;_ (println initial-data)
        ;_ (println "DATA at 7:" (get initial-data 7))
        poison-ch (chan)
        ]
    (go-loop [counted 0
              data initial-data]
      (let [timeout-ch (timeout millis-pause)
            [msg ch] (alts! [command-ch poison-ch timeout-ch])]
        (cond
          (= ch command-ch)
          (let [_ (assert (map? msg) (str "msg received over control-ch must be a map, instead got: <" msg ">, <" (type msg) ">"))
                {:keys [cmd]} msg]
            (case cmd
              :get-values
              (let [point-num (:point-num msg)
                    _ (assert (<= point-num num-points) (str "Cannot get values at point " point-num " when greatest point is only " num-points))
                    res (get data point-num)
                    _ (assert (map? res) (str "Expected map, what got: <" res "> from point <" point-num ">"))]
                (>! command-ch res)
                (recur counted data))
              :set-value
              (let [{:keys [point-num metric-id value]} msg]
                (assert (pos? point-num))
                (assert (<= point-num num-points))
                (>! command-ch (boolean (get-in data [point-num metric-id])))
                (recur counted (update-in data [point-num metric-id] #(-> %
                                                                          (assoc :value value)
                                                                          (assoc :set-at-by-user counted)))))
              (assert false (str "cmd not understood: <" cmd ">"))))

          (= ch poison-ch)
          (do
            (println (str "[atmosphere-component] stopping"))
            )

          (= ch timeout-ch)
          (let [selected-point (inc (rand-int num-points))
                which-metric (rand-int (count metrics))
                metric (nth metrics which-metric)
                {:keys [generator metric-id]} metric
                {:keys [value set-at-by-user] :as picked} (get-in data [selected-point metric-id])
                auto-change-acceptable? (or (nil? set-at-by-user) (>= (* millis-pause (- counted set-at-by-user)) (* 60000 sticky-minutes)))
                ]
            (if auto-change-acceptable?
              (let [new-value (next-value (or value 0) generator)]
                (p (str "been picked: " picked " now given: " new-value))
                (recur (inc counted) (assoc-in data [selected-point metric-id :value] new-value)))
              (do
                (p (str "Change being skipped for " picked))
                (recur (inc counted) data)))))))
    {:stop-f (fn stop! []
               (close! poison-ch))
     :remote-control command-ch
     :id :atmosphere}))
