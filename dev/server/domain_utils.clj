(ns domain-utils
  (:require [domain-constants :as constants]))

;; @SuppressWarnings("unchecked")
(defn big-dec
  ([x]
    (big-dec nil x))
  ([debug-msg x]
   (let [
         _ (assert (not (decimal? x)) (str "Already decimal so no point calling"))
         ;_ (when debug-msg (println "BEFORE: " debug-msg))
         res (bigdec x)
         ;_ (when debug-msg (println "AFTER: " debug-msg))
         ]
     res)))

(defn check-dec [x]
  (assert (decimal? x))
  x)

(defn convert-dec [x]
  (if (decimal? x)
    x
    (big-dec x)))

(defn precise-big-dec [x]
  (assert (or (ratio? x) (not (decimal? x))) (str "Already decimal so no point calling"))
  (with-precision constants/PRECISION (bigdec x)))

(defn str->bigdec [s]
  (BigDecimal. s))
