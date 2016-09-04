 (ns parsing
   (:require [instaparse.core :as insta]
             [utils :as u]
             [clojure.string :as str]
             [clojure.pprint :as pp]))

(defn err->out [v]
  (assert v)
  (spit "bad_input.txt" v))

(defn err->out-debug [v msg]
  (assert v msg)
  (spit "bad_input.txt" v))

(defn parse-many-first [ebnf input]
  (let [my-parser (insta/parser ebnf)
        xs (insta/parses my-parser input)
        num-choices (count xs)
        msg (cond
              (zero? num-choices) "insta/parses might be able to be corrected"
              (> num-choices 1) (str "insta/parses shows more than one way to parse: " num-choices)
              (= 1 num-choices) "insta/parses shows one choice, so all good")
        res (cond
              ;; Just fix there being more than one rather than show them all
              ;(> num-choices 1) xs
              (= num-choices 1) (first xs)
              (zero? num-choices) (insta/parse my-parser input))
        _ (assert res (str "No result. Num of choices is: " num-choices))]
    {:res res :msg msg}))

(defn parse-one-only [ebnf input]
  (let [my-parser (insta/parser ebnf)
        res (insta/parse my-parser input)
        _ (assert res (str "No result"))]
    {:res res :msg nil}))

(defn parse-one-only-optimized [ebnf input]
  (let [my-parser (insta/parser ebnf)
        res (insta/parse my-parser input :optimize :memory)
        _ (assert res (str "No result"))]
    {:res res :msg nil}))

#_(defn test-whole-word []
  (let [s prod-input
        want "TAG"
        res (u/whole-word-index-of s want 0)]
    res))

(defn indexes-of [s value]
  (loop [acc [] idx 0]
    (let [res (u/whole-word-index-of s value idx)]
      (if (nil? res)
        acc
        (recur (conj acc res) (inc res))))))

;; #(-> % :value (str/replace #"\t" "        ") my-parser)
(def do-parse (fn [ebnf s]
                (parse-one-only ebnf s)))

(defn info [ebnf s begin-str end-str begin end]
  (let [_ (assert begin-str)
        _ (assert end-str)
        _ (assert end)
        _ (assert begin)
        real-end (+ end (count end-str))
        value (subs s begin real-end)
        rid-tabs-value (str/replace value #"\t" "        ")
        parsed-value (some-> ebnf (do-parse rid-tabs-value))
        ;; there should also be a msg, so that's another way finding out
        err? (if (-> parsed-value :res :reason) true false)]
    {:name         (str/trim begin-str)
     :value        rid-tabs-value
     :parsed-value parsed-value
     :begin        begin
     :end          real-end
     :err?         err?}))

(defn groups-of
  ([ebnf begin-str end-str s]
   (let [begins (indexes-of s begin-str)
         ends (indexes-of s end-str)
         num-begins (count begins)
         num-ends (count ends)
         _ (assert (= num-begins num-ends) (str "Number begins and ends: " num-begins "," num-ends " - not same for: " begin-str))
         res (map #(info ebnf s begin-str end-str %1 %2) begins ends)]
     res))
  ([begin-str end-str s]
   (groups-of nil begin-str end-str s)))

;; Used when there is only one
(defn first-of
  ([ebnf begin-str end-str s]
   (let [_ (assert s)
         _ (assert begin-str (str "No begin str: <" begin-str "> <" end-str "> <" (apply str (vec (take 150 s))) ">"))
         _ (assert end-str)
         begin (u/whole-word-index-of s begin-str 0)
         _ (assert begin (str "No begin-str found: <" begin-str "> in:- <" (apply str (vec (take 150 s))) ">"))
         end (u/whole-word-index-of s end-str 0)
         _ (assert (> end begin) (str "Begin must be before end: " begin " " end))
         ;_ (println "begin end " begin " " end ": " (vec (take 150 (drop begin s))))
         res (info ebnf s begin-str end-str begin end)]
     res))
  ([begin-str end-str s]
   (first-of nil begin-str end-str s)))

(defn find-problem [name m]
  (assert (map? m))
  (assert (= (:name m) name) (str "Name found is not " name " but <" (:name m) "> SEE: " (vec m)))
  (if (:err? m)
    (let [res (-> m :parsed-value :res)
          {:keys [line column reason]} res
          input (:value m)]
      [(str "Problem is at line " line " and col " column " b/c: " reason) input]
      {:msg (str "Problem is at line " line " and col " column " b/c: " reason) :value input :okay? false})
    {:msg "No problem" :okay? true}))

(defn find-problem-from-coll [name c]
  (let [potential-problems (map #(find-problem name %) c)
        bad-problem (some #(when-not (:okay? %) %) potential-problems)
        ;_ (println "Got " bad-problem)
        ]
    (if bad-problem
      bad-problem
      {:msg "No problem" :okay? true})))
