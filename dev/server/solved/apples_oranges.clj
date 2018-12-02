(ns solved.apples-oranges
  (:require
    [clojure.string :as str]))

(defn between? [left right dropped-at]
  (and (>= dropped-at left) (<= dropped-at right)))

;; s -> t house
;; a apple tree
;; b orange tree
;;
(defn within [s t a b]
  (let [between-house? (partial between? s t)]
    (fn [apples oranges]
      (let [
            ;_ (println apples)
            ;_ (println oranges)
            apple-positions (map #(+ a %) apples)
            orange-positions (map #(+ b %) oranges)
            apples-between (count (filter identity (map #(between-house? %) apple-positions)))
            oranges-between (count (filter identity (map #(between-house? %) orange-positions)))]
        (println apples-between)
        (println oranges-between)))))

(defn x []
  (let [s_temp (read-line)
        s_t (str/split s_temp #"\s+")
        s (Integer/parseInt (s_t 0))
        t (Integer/parseInt (s_t 1))
        a_temp (read-line)
        a_t (str/split a_temp #"\s+")
        a (Integer/parseInt (a_t 0))
        b (Integer/parseInt (a_t 1))
        apple_temp (read-line)
        apple_t (str/split apple_temp #"\s+")
        apple (map #(Integer/parseInt %) apple_t)
        orange_temp (read-line)
        orange_t (str/split orange_temp #"\s+")
        orange (map #(Integer/parseInt %) orange_t)
        ]
    ;((within 7 11 5 15) [-2 2 1] [5 -6])
    ((within s t a b) apple orange)
    ))
