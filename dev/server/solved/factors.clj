(ns solved.factors)

(def a #{2 4})
(def b #{16 32 96})

#_(defn divisors [n]
  (into #{} (mapcat #(when (zero? (rem n %)) [% (/ n %)]) (range 1 (Math/ceil (Math/sqrt n))))))

(defn factors [n]
  (filter #(zero? (rem n %)) (range 1 (inc n))))

(defn factors [n]
  (into (sorted-set)
        (mapcat (fn [x] [x (/ n x)])
                (filter #(zero? (rem n %)) (range 1 (inc (Math/sqrt n)))) )))

#_(defn producer [list-a]
  (fn [from-b]
    (map #(/ from-b %) list-a)))

(defn all-factors? [list-a]
  (fn [from-b]
    (let [divs (factors from-b)]
      (clojure.set/subset? list-a divs))))

#_(defn x-1 []
  (let [f (producer a)
        res (into #{} (mapcat f b))
        lowest (apply min b)
        lower-only (filter #(<= % lowest) res)]
    (count lower-only)))

#_(defn x-2 []
  (let [candidates (into #{} (mapcat factors b))
        lowest-high (apply min b)
        highest-low (apply max a)
        lower-only (filter #(<= % lowest-high) candidates)
        higher-only (filter #(>= % highest-low) lower-only)
        factors-fn? (all-factors? a)
        res (filter factors-fn? higher-only)]
    higher-only))

(defn divide-evenly-bs [divisors]
  (fn [numerator]
    (let [perhaps-evenlys (map #(vector (zero? (rem numerator %)) %) divisors)
          to-keep (map second (filter (fn [[truth divisor]] (when truth divisor)) perhaps-evenlys))]
      to-keep)))

(defn divide-evenly-as [numerators]
  (fn [divisor]
    (let [
          ;_ (println "applying numerators " numerators " to divisors " divisor)
          perhaps-evenlys (map #(vector (zero? (rem % divisor)) %) numerators)
          to-keep (map second (filter (fn [[truth divisor]] (when truth divisor)) perhaps-evenlys))]
      to-keep)))

(defn x-3 []
  (let [
        lowest-of-high (apply min b)
        rest-of-high (remove #{lowest-of-high} b)
        ;_ (println "rest: " rest-of-high)
        candidates (factors lowest-of-high)
        highest-low (apply max a)
        lower-only (filter #(<= % lowest-of-high) candidates)
        higher-only (filter #(>= % highest-low) lower-only)
        factors-higher-rest (reduce
                       (fn [acc ele]
                         (let [res ((divide-evenly-bs acc) ele)]
                           res))
                       higher-only
                       rest-of-high)
        factors-lower (reduce
                             (fn [acc ele]
                               (let [res ((divide-evenly-as acc) ele)]
                                 res))
                             factors-higher-rest
                             a)
        ]
    factors-lower))

;(use '[clojure.string :only (split triml)])

(defn soln []
  (let [_ (read-line)
        a_temp (read-line)
        a_t (clojure.string/split a_temp #"\s+")
        a (map #(Integer/parseInt %) a_t)
        b_temp (read-line)
        b_t (clojure.string/split b_temp #"\s+")
        b (map #(Integer/parseInt %) b_t)
        lowest-of-high (apply min b)
        rest-of-high (remove #{lowest-of-high} b)
        ;_ (println "rest: " rest-of-high)
        candidates (factors lowest-of-high)
        highest-low (apply max a)
        lower-only (filter #(<= % lowest-of-high) candidates)
        higher-only (filter #(>= % highest-low) lower-only)
        factors-higher-rest (reduce
                              (fn [acc ele]
                                (let [res ((divide-evenly-bs acc) ele)]
                                  res))
                              higher-only
                              rest-of-high)
        factors-lower (reduce
                        (fn [acc ele]
                          (let [res ((divide-evenly-as acc) ele)]
                            res))
                        factors-higher-rest
                        a)
        ]
    (println (count factors-lower))))
