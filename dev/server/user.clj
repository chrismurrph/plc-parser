(ns user
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.pprint :as pp]))

(defn r []
  (require 'user :reload))

;(def everything-ebnf (slurp "parser.bnf"))
;(def tag-ebnf (slurp "tag.bnf"))

;(def curr-dev-input (slurp "version_assign_2.txt"))
(def prod-input (slurp "prod_input.L5K"))

;; :optimize :memory


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
    {:res res :msg msg :input input}))

(defn parse-one-only [ebnf input]
  (let [my-parser (insta/parser ebnf)
        res (insta/parse my-parser input)
        _ (assert res (str "No result"))]
    {:res res :msg nil :input input}))

(defn parse-one-only-optimized [ebnf input]
  (let [my-parser (insta/parser ebnf)
        res (insta/parse my-parser input :optimize :memory)
        _ (assert res (str "No result"))]
    {:res res :msg nil :input input}))

(defn two-x [ebnf input]
  (let [[msg [one two]] (parse-many-first ebnf input)]
    (spit "one.txt" (with-out-str (pp/pprint one)))
    (spit "two.txt" (with-out-str (pp/pprint two)))
    (println msg)))

(defn one-x [ebnf input]
  (let [res (parse-one-only ebnf input)]
    (pp/pprint res)))

(defn indexes-of [s value]
  (loop [acc [] idx 0]
    (let [res (str/index-of s value idx)]
      (if (nil? res)
        acc
        (recur (conj acc res) (inc res))))))

(defn info [s begin-str end-str begin end]
  (let [real-end (+ end (count end-str))
        value (subs s begin real-end)]
    {:name  (str/trim begin-str)
     :value value
     :begin begin
     :end   real-end}))

(defn groups-of [s begin-str end-str]
  (let [begins (indexes-of s begin-str)
        ends (indexes-of s end-str)
        num-begins (count begins)
        num-ends (count ends)
        _ (assert (= num-begins num-ends) (str "Number begins and ends: " num-begins "," num-ends " - not same for: " begin-str))
        res (map #(info s begin-str end-str %1 %2) begins ends)]
    res))

(defn first-of [s begin-str end-str]
  (let [begin (str/index-of s begin-str)
        end (str/index-of s end-str)
        res (info s begin-str end-str begin end)]
    res))

(defn break-up [s]
  (let [datatypes (groups-of s "DATATYPE " "END_DATATYPE")
        modules (groups-of s "MODULE " "END_MODULE")
        tag (first-of s "\tTAG" "END_TAG")
        routines (groups-of s "ROUTINE " "END_ROUTINE")
        res (concat datatypes modules [tag] routines)]
    res))

(defn usual-x [ebnf input]
  (let [res (parse-many-first ebnf input)]
    (pp/pprint res)))

(defn group-parser [groups group-name ebnf]
  (let [blocks (filter #(= (:name %) group-name) groups)
        my-parser (partial parse-one-only ebnf)
        _ (assert (seq blocks) (str "None found for " group-name))
        code-blocks (map #(-> % :value (str/replace #"\t" "        ") my-parser) blocks)
        ]
    code-blocks))

(defn x []
  (let [groups (break-up prod-input)
        ms (group-parser groups "ROUTINE" (slurp "routine.bnf"))
        results (map :res ms)
        first-bad-result (some :reason ms)]
    (spit "output.txt" (with-out-str (pp/pprint results)))
    (if first-bad-result
      (spit "bad_input.txt" (-> ms first :input))
      (spit "bad_input.txt" "All clear"))))

(defn x-old []
  (let [groups (break-up prod-input)
        display-results (map #(vector (:name %) (:begin %) (:end %)) groups)
        tags (filter #(= (:name %) "TAG") groups)
        program-tags (:value (first tags))
        nicer-program-tags (str/replace program-tags #"\t" "        ")
        [res msg] (parse-one-only (slurp "tag.bnf") nicer-program-tags)
        ]
    ;(println program-tags)
    ;(pp/pprint res)
    (when (:reason res)
      (do
        (println (str "See bad_input.txt line: " (:line res) ", column: " (:column res)))
        (spit "bad_input.txt" nicer-program-tags)))
    (when msg (println msg))))