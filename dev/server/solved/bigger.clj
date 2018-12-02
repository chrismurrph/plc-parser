(ns solved.bigger
  ;(:require [clojure.math.combinatorics :as combo])
  )

(def input ["5"
            "ab"
            "bb"
            "hefg"
            "dhck"
            "dkhc"])

(def output ["ba"
             "no answer"
             "hegf"
             "dhkc"
             "hcdk"
             ])

(defn index-of [coll desired]
  (first (keep-indexed (fn [idx val] (when (= val desired) idx)) coll)))

(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))

(defn cf [& args]
  (let [[uno dos] args
        one (str uno)
        two (str dos)]
    ;(println "sort:" args)
    ;(println "one:" one ", two:" two)
    (cond
      (and (nil? one) (nil? two)) 0
      (nil? one) -1
      (nil? two) 1
      :default (compare one two))))

(defn calc-next-1 [in-str]
  (let [perms (permutations in-str)
        ;_ (println "PERMS:" perms)
        ]
    (let [sorted (sort cf perms)
          ;_ (println "PERMS:" sorted)
          idx (index-of sorted (cons (first in-str) (rest in-str)))
          ;_ (println idx (count sorted))
          ]
      (if (nil? idx)
        "no answer"
        (apply str (nth sorted (inc idx)))))))

(defn calc-next-2 [in-str]
  (let [reversed (reverse in-str)
        last-ch (first reversed)
        second-last-ch (second reversed)]
    (if (> (compare last-ch second-last-ch) 0)
      (let []
        (apply str (concat (take (- (count in-str) 2) in-str) `(~last-ch ~second-last-ch))))
      (calc-next-1 in-str))))

(defn x []
  (let [
        ;input (line-seq (java.io.BufferedReader. *in*))
        results (map calc-next-2 (rest input))
        ]
    (doseq [res results]
      (println res))))

