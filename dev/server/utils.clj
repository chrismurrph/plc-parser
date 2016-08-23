(ns utils)

(defn r []
  (require 'utils :reload))

(defn rm-punctuation [in-str]
  (apply str (remove #{\. \? \!} in-str)))

(defn abs [val]
  (if (neg? val)
    (* -1 val)
    val))

(defn whole-number? [n]
  (= 0.0 (rem n 1)))

(defn sqrt [n]
  (if (> n 0)
    (Math/sqrt n)
    0))

(defn exp [x pow-of]
  (Math/pow x pow-of))

(defn root [x root-of]
  (Math/pow x (/ root-of)))

(defn round [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn divide [num div]
  (let [res (/ num div)
        ;_ (assert (= res (int res)) (str "Got back fraction: " res))
        ]
    (round 0 res)))

(defn factorial [n]
  (reduce * (range 1N (inc n))))

(defn intersection
  [s1 s2]
  (if (< (count s2) (count s1))
    (recur s2 s1)
    (reduce (fn [result item]
              (if (contains? s2 item)
                result
                (disj result item)))
            s1 s1)))

(defn divisors [n]
  (into #{} (mapcat #(when (zero? (rem n %)) [% (/ n %)])
                    (range 1 (Math/ceil (Math/sqrt n))))))

(defn gcd [a b]
  (fn [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defn lcm [& numbers]
  (let [gcd-fn (fn [a b] (if (zero? b)
                        a
                        (recur b (mod a b))))
        lcm-inner (fn [num1 num2]
                (let [multiplied (* num1 num2)
                      gcd (gcd-fn num1 num2)
                      res (/ multiplied gcd)]
                  res))
        [head & tail] numbers]
    (if (nil? tail)
      head
      (lcm-inner head (apply lcm tail)))))

;;
;; from-world and to-world are maps of type {:min _ :max _}
;; These max and min are inclusive, so the exact middle when :min 0 and :max 10 is 5
;; Note that we need to do precision-scaling at the end, as there needs to be an exact
;; pixel location where to put circle on the graph
;;
(defn scale [from-world to-world from-val]
  (let [min-from (:min from-world)
        max-from (:max from-world)
        min-to (:min to-world)
        max-to (:max to-world)
        from-diff (- max-from min-from)
        to-diff (- max-to min-to)
        from-proportion (/ (- from-val min-from) from-diff)
        res (* to-diff from-proportion)
        rounded-res (int (Math/ceil res))
        ;_ (println "FROM VAL:" from-val " | RES:" rounded-res " | " res " | F:" from-world " | T:" to-world)
        ]
    rounded-res))

(defn distance [precision [x1 y1] [x2 y2]]
  (let [x-delta-squared (exp (- x2 x1) 2)
        y-delta-squared (exp (- y2 y1) 2)
        sum-of-differences (+ x-delta-squared y-delta-squared)
        now-squared (sqrt sum-of-differences)]
    (round precision now-squared)))

(defn perfect-square? [n]
  (-> n sqrt whole-number?))

(defn output-the-input []
  (let [input (line-seq (java.io.BufferedReader. *in*))]
    (doseq [v input]
      (println v))))

(defn gen-primes [n]
  (letfn [(sieve [s]
            (cons (first s)
                  (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                           (rest s))))))]
    (take n (sieve (iterate inc 2)))))

(defn comma-str->ints
  [string]
  (map #(Integer/parseInt %)
       (clojure.string/split string #",")))

(defn space-str->ints
  [string]
  (map #(Integer/parseInt %)
       (clojure.string/split string #" ")))

(defn many-line-reader [lines item-fn no-overall-header]
  (let [seed {:expecting (if no-overall-header :case-header :overall-header)
              :results   []}
        output (reduce
                 (fn [acc ele]
                   (let [nxt-acc (case (:expecting acc)
                                   :overall-header
                                   (assoc acc :expecting :case-header)
                                   :case-header
                                   (-> acc
                                       (assoc :current-header ele)
                                       (assoc :expecting :item))
                                   :item
                                   (let [res (item-fn ele (:current-header acc))]
                                     (-> acc
                                         (update :results conj res)
                                         (assoc :expecting :case-header)))
                                   acc)]
                     nxt-acc))
                 seed
                 lines)
        res (:results output)]
    res))


;(defn fibonacci-seq [size]
;  (loop [acc [1N 1N]
;         ele-at 2N]
;    (if (= ele-at size)
;      acc
;      (let [prior-val (nth acc (dec ele-at))
;            prior-prior-val (nth acc (- ele-at 2N))
;            next-val (+' prior-val prior-prior-val)]
;        (recur (conj acc next-val) (inc ele-at))))))

(defn fibonacci-seq [limit] (mod (nth (map first
                                           (iterate
                                             (fn fib-step [[a b]] [b (+ a b)]) [0N 1])) limit)
                                 100000007))

;(defn- bubble-max-key [k coll]
;  "Move a maximal element of coll according to fn k (which returns a number)
;   to the front of coll."
;  (let [max (apply max-key k coll)]
;    (cons max (remove #(identical? max %) coll))))

;(defn factorial [x]
;  (loop [n x f 1N]
;    (if (= n 1)
;      f
;      (recur (dec n) (* f n)))))

(defn combinations-count [pop sz]
  (/ (factorial pop) (* (factorial sz) (factorial (- pop sz)))))

(defn permutations-count [pop sz]
  (/ (factorial pop) (factorial (- pop sz))))

(comment
  "Exactly same as one below yet still doesn't work!!"
  (defn combinations [population sz]
    (cond
      (empty? population)
      '()

      (zero? sz)
      '(())

      :default
      (let [
            ;rest-of-combinations (combinations (rest population) (dec sz))
            ;now-multiplied-with-first (mapv #(conj % (first population)) (combinations (rest population) (dec sz)))
            ;really-rest (combinations (rest population) sz)
            ]
        (concat (map #(cons (first population) %) (combinations (rest population) (dec sz)))
                (combinations (rest population) sz))))))

(defn combinations [population sz]
  (cond
    (= sz 0) '(())
    (empty? population) '()
    :else (concat (mapv #(cons (first population) %) (combinations (rest population) (dec sz)))
                  (combinations (rest population) sz))))

(defn digits->number [digits]
  (reduce (fn [a b] (+ (* a 10) b)) 0 digits))

(comment
  (defn gcd [a b]
    (let [greatest (max a b)
          least (if (= greatest a) b a)
          end-point (min (Math/floor (/ greatest 2)) least)]
      (loop [res 1
             where-at 2]
        (if (<= where-at end-point)
          (let [next-up (inc where-at)]
            (if (and (= 0 (mod greatest where-at)) (= 0 (mod least where-at)))
              (recur where-at next-up)
              (recur res next-up)))
          res)))))

(comment
  (defn gcd [a b]
    (->> (map (fn [x]
                (filter #(zero? (mod x %)) (range 1 (inc x))))
              [a b])
         (map set)
         (apply clojure.set/intersection)
         (apply max))))




