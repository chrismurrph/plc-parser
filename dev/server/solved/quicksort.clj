(ns solved.quicksort
  (:require [utils :as u]))

(defn swap [v i j]
  ;(println (str "swapping: " v " " i " " j))
  (assoc v j (v i) i (v j)))

;algorithm partition(A, lo, hi) is
;pivot := A[hi]
;i := lo     // place for swapping
;for j := lo to hi – 1 do
;  if A[j] ≤ pivot then
;    swap A[i] with A[j]
;    i := i + 1
;swap A[i] with A[hi]
;return i
(defn partit [v lo hi]
  (let [pivot (nth v hi)]
    (let [done-swapping (loop [i lo
                               j lo
                               coll v]
                          (if (> j hi)
                            coll
                            (let [a-j (nth coll j)]
                              (if (<= a-j pivot)
                                (let [new-coll (swap coll i j)]
                                  (recur (inc i) (inc j) new-coll))
                                (recur i (inc j) coll)))))]
      done-swapping)))

(defn divide-sort [v]
  (let [lo 0
        hi (dec (count v))
        res (partit v lo hi)]
    res))

(defn collect-left [coll]
  (let [size (count coll)
        ev? (even? size)
        end-left (if ev? (/ size 2) (int (/ size 2)))
        left (take end-left coll)
        ;_ (println (str "left count:" (count left)))
        ]
    (vec left)))

(defn collect-right [coll]
  (let [size (count coll)
        ev? (even? size)
        start-right (if ev? (/ size 2) (inc (int (/ size 2))))
        right (drop start-right coll)
        ;_ (println (str "right count:" (count right)))
        ]
    (vec right)))

(defn collect-centre [coll]
  (let [size (count coll)
        ev? (even? size)]
    (if ev?
      []
      (let [to-drop (int (/ size 2))]
        (into [] (take 1 (drop to-drop coll)))))))

(defn one-sort [v]
  (let [res (divide-sort v)
        _ (println "OUT" res)
        left (divide-sort (collect-left res))
        _ (println "OUT" (u/left-bank left res))
        centre (collect-centre res)
        right (divide-sort (collect-right res))
        together (vec (concat left centre right))
        _ (println "OUT" together)]
    together))

(defn x []
  (let [coll [1 3 9 8 2 7 5 1 3 9 8 2 7 5]
        sorted (one-sort coll)
        ]))


