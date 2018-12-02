(ns makes
  (:require [convert :as conv]
            [utils :as u]))

#_(defn make-string [[[_ [_ size]] [_ [_ text]]]]
  ;(assert false (str size ", " text))
  [{:val/val (Long/parseLong size) :val/type :int} {:val/val text :val/type :single-quoted}])

(defn make-message [msg]
  ;(when (or (seq msg) (u/not-blank? msg)) (println (str "Ignoring: <" (seq msg) ">")))
  [{:val/val "path here?" :val/type :string}
   {:val/val false :val/type :unknown}
   {:val/val false :val/type :unknown}])

(defn make-timer [v]
  ;(assert false v)
  (let [res (mapv (comp (fn [n] {:val/val (conv/val-set (u/string->int n)) :val/type "DINT"}) second) (second v))
        ;_ (assert false res)
        ]
    res)
  )
