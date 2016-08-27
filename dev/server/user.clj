(ns user
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.pprint :as pp]))

(defn r []
  (require 'user :reload))

(def ebnf (slurp "parser.bnf"))

(def input (slurp "version_assign_2.txt"))

(defn parse-it []
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
    [msg res]))

(defn two-x []
  (let [[msg [one two]] (parse-it)]
    (spit "one.txt" (with-out-str (pp/pprint one)))
    (spit "two.txt" (with-out-str (pp/pprint two)))
    (println msg)))

(defn x []
  (let [[msg res] (parse-it)]
    (pp/pprint res)
    (println msg)))