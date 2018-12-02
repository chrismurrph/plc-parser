(ns solved.so)

(def coll
  {:a "aa"
   :b {:d "dd"
       :e {:f {:h "hh"
               :i "ii"}
           :g "hh"}}
   :c "cc"})
;;
;; divide into maps and non-maps
;; mark a dead end
;; only need one result
;; gave up when he tols me wants all - that's real tree walking
;;
#_(defn find-in [coll desired-value]
  (loop [result []
         m coll]
    (let [res (ffirst (filter (fn [[k v]] (= v desired-value)) m))]
      (if res
        (conj result res)
        (let [ms (filter (fn [[k v]] (map? v)) m)]
          (assert false (seq ms))
          (recur (conj result nil) nil))))))

(defn flatten-map
  ([path m]
   (if (map? m)
     (mapcat (fn [[k v]] (flatten-map (conj path k) v)) m)
     [[path m]]))
  ([m]
    (flatten-map [] m)))

(defn find-in [coll x]
  (->> (flatten-map coll)
       (filter (fn [[_ v]] (= v x)))
       (map first)))

(defn x []
  ;(assert (= (find-in coll "hh") [:b :e :f :h]))
  ;(find-in coll "hh")
  (flatten-map coll)
  )
