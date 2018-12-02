(ns query
  (:require [parse.navigation :as nav]))

;;
;; Lower level queries are in navigation. Here is where print out what might like to
;; query on. Input always comes from call to run/start.
;; Navigation queries will also be used by mutation (these here for now), which will do same as here but replace
;; the contents. Most queries from here will have a mutation as well.
;;
(defn query-routine [program name]
  (let [routines (:program/routines program)
        ;_ (println (count routines))
        res (first (keep-indexed #(nav/named-idx? %1 %2 name nav/routine-by-name-q) routines))
        ;id-res ((juxt :id #(get-in % [:input :parsed-value])) chosen-routine)
        ]
    res))

;;
;; Assuming first rung is 1 (not zero)
;;
(defn query-rung [routine rung-num]
  (nth routine (inc rung-num)))

(defn query-rung-idx [routine existing-rung]
  (assert (= (first existing-rung) :rung))
  (let [search-for existing-rung
        rungs routine
        idx (first (keep-indexed #(when (= %2 search-for) %1) rungs))]
    (pos? idx)
    (dec idx)))

#_(defn mutate-routine [info idx new-routine routine-name]
  ;(assert (vector? info) (str "Not vector but: " (type info)))
  (println (str idx "th routine (first is zero) called: <" routine-name "> has been replaced"))
  (assoc-in info [:program/routines idx :input :parsed-value] new-routine))

(defn mutate-rung [program routine-idx rung-num new-rung]
  (assert (= :rung (first new-rung)) (str "Improperly formed replacement rung: " new-rung))
  (assoc-in program [:program/routines routine-idx :input :parsed-value (inc rung-num)] new-rung))

