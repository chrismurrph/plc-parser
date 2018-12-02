(ns utils
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn r []
  (require 'user :reload))

(defn get-now [] (.getTime (java.util.Date.)))

(def startup-time (get-now))

;(println (str "startup-time is " startup-time))

(defn elapsed []
  (- (get-now) startup-time))

(defn strip-edges [s]
  (apply str (take (- (count s) 2) (drop 1 s))))

(defn left-bank [left coll]
  (vec (concat left (drop (count left) coll))))

(defn right-bank [right coll]
  (let [diff (- (count coll) (count right))]
    (vec (concat (take diff coll) right))))

;; Seems like it was gonna work, but have no effect
;; (alter-var-root (var pp/*print-right-margin*) (constantly 200))
;; (alter-var-root (var pp/*print-miser-width*) (constantly 200))
;; (alter-var-root (var pp/*print-length*) (constantly 200))
;; pr-str might be proper way
;; (defn pp-str [x] (-> x clojure.pprint/pprint with-out-str))

(defn pp-str
  ([x n]
   (binding [pp/*print-right-margin* n]
     (-> x clojure.pprint/pprint with-out-str)))
  ([x]
   (pp-str x 1000)))

;(def pp-str (fn [s]
;              (pr-str s {:right-margin 200
;                         :not-exists   20
;                         :miser-width  200})))

(defn after [begin-marker s]
  (apply str (drop (count begin-marker) (drop-while #(not= % (first begin-marker)) s))))

(defn before [end-marker s]
  (apply str (take-while #(not= % (first end-marker)) s)))

(defn between [begin-marker end-marker s]
  (let [after-fn #(after begin-marker %)
        before-fn #(before end-marker %)
        blank->nil-fn #(if (= % "") nil %)]
    (-> s after-fn before-fn blank->nil-fn)))

(defn probe-on
  ([x]
   (println x)
   x)
  ([x msg]
    (println msg x)
    x))

(defn probe-stack
  ([x]
   (do
     (println x)
     (assert false))
   x)
  ([x msg]
   (do
     (println msg x)
     (assert false msg))
   x))

(defn probe-off
  ([x]
   x)
  ([x msg]
   x))

;;
;; Same as (not (nil? x))
;;
(defn is? [x]
  (or (boolean? x) x))

;; NumberFormatException
(defn string->int [s]
  (assert s)
  (assert (or (string? s) (char? s)) (str "Wrong type: <" (type s) ">, <" s ">"))
  (try
    (Long/parseLong (str s))
    (catch NumberFormatException _
      (assert false (str "Cannot parse as an int: <" s ">"))
      nil)))

;(defn int-between [s1 s2 s]
;  (string->int (subs s (count s1) (count s2))))

(defn has-key? [m kw]
  (boolean (and (map? m) (some #{kw} (keys m)))))

(defn not-blank? [x]
  (or (boolean? x) (and x (not= x ""))))

;;
;; Returns the index positions of those that satisfy the pred(icate)
;;
(defn positions
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn index-of [coll desired]
  (first (keep-indexed (fn [idx val] (when (= val desired) idx)) coll)))

(defn first-no-more [seq]
  (assert (= nil (second seq)) (str "Only supposed to be one. However:\nFIRST:\n" (first seq) "\nSECOND:\n" (second seq)))
  (first seq))

(def third #(nth % 2))

;
; s and value never change but from-index is recursed, so can use loop recur on just that
;
(defn- whole-word-index-of [^CharSequence s value ^long from-index after?]
  (let [whitespace? (fn [^Character c]
                      (or (= c \newline) (= c \return) (= c \space) (= c \tab)))
        _ (assert value)
        _ (assert s (str "Can't look for <" value "> in nil"))
        _ (assert from-index)
        res (str/index-of s value from-index)
        ]
    (if (nil? res)
      nil
      (let [just-before (first (take 1 (drop (dec res) s)))
            whitespace-before? (whitespace? just-before)
            end-res (+ res (count value))
            just-after (first (take 1 (drop end-res s)))
            whitespace-after? (whitespace? just-after)
            whole-word? (and whitespace-before? whitespace-after?)
            ]
        (if whole-word?
          (if after? (+ res (count value)) res)
          (whole-word-index-of s value end-res after?))))))

(defn indexes-of-whole-word [s value after?]
  (loop [acc [] idx 0]
    (let [res (whole-word-index-of s value idx after?)]
      (if (nil? res)
        acc
        (recur (conj acc res) (inc res))))))

(defn indexes-of [s value]
  (loop [acc [] idx 0]
    (let [res (str/index-of s value idx)]
      (if (nil? res)
        acc
        (recur (conj acc res) (inc res))))))

(defn insert-at [s x n]
  (apply str (concat (take n s) x (drop n s))))

(defn indexes-of-many-whole-words [s values after?]
  (map #(indexes-of-whole-word s %  after?) values))

#_(defn test-whole-word []
    (let [s prod-input
          want "TAG"
          res (u/whole-word-index-of s want 0)]
      res))

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

(defn gen-primes [n]
  (letfn [(sieve [s]
            (cons (first s)
                  (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                           (rest s))))))]
    (take n (sieve (iterate inc 2)))))

(defn comma-str->ints
  [string]
  (map #(Long/parseLong %)
       (clojure.string/split string #",")))

(defn space-str->ints
  [string]
  (map #(Long/parseLong %)
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



(defn string->int-not-strict [s]
  (assert (string? s) (str "Wrong type (not string), where value is: " s ", type is " (type s)))
  (try
    (Long/parseLong s)
    (catch NumberFormatException _
      nil)))

;;
;; Naive function that does not respect the sign bit
;; Where ever this is used but not from convert is a danger sign.
;;
(defn left-pad [xs pad-ele max-sz]
  (let [
        diff-count (- max-sz (count xs))
        ;_ (assert (or (zero? diff-count) (pos? diff-count)) (str "Max size is " max-sz ", yet already have " (count xs)))
        ]
    (if (> (count xs) max-sz)
      xs
      (concat (take diff-count (repeat pad-ele)) xs))))

(defn left-pad-integer [int pad-ele max-sz]
  (let [_ (assert (number? int))
        as-str (str int)
        diff-count (- max-sz (count as-str))
        _ (assert (or (zero? diff-count) (pos? diff-count)) (str "Max size is " max-sz ", yet already have " (count as-str)))
        padded (apply str (concat (repeat diff-count pad-ele) as-str))
        ]
    padded))

(defn sleep [n]
  (Thread/sleep n))

(defmacro assrt
  "Useful to use (rather than official version that this is o/wise a copy of) when don't want intermingling of
  the stack trace produced here with trace output that want to come before"
  {:added "1.0"}
  ([x]
   (when *assert*
     `(when-not ~x
        (sleep 30)
        (throw (new AssertionError (str "Assert failed: " (pr-str '~x)))))))
  ([x message]
   (when *assert*
     `(when-not ~x
        (sleep 30)
        (throw (new AssertionError (str "Assert failed: " ~message "\n" (pr-str '~x))))))))

(defn ends-with? [s ending]
  (= (dec (count s)) (str/last-index-of s ending)))

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

(defn transpose
  "Transposes the given nested sequence into nested vectors, as
  in matrix transposition.  E.g., (transpose [[1 2 3] [4 5 6]])
  would return [[1 4] [2 5] [3 6]]."
  [s]
  (vec (apply mapv vector s)))

(defn shift
  [row n]
  (let [n (- (count row) n)
        [first-part second-part] (split-at n row)]
    (vec (concat second-part first-part))))

(comment
  (defn digits->number [digits]
    (reduce (fn [a b] ('+ ('* a 10) b)) 0 digits)))

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




