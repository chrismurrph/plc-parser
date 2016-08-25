(ns user
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.pprint :as pp]))

(defn r []
  (require 'user :reload))

(def ebnf (slurp "parser.bnf"))

(def input (slurp "version_assign.txt"))

(defn parse-it []
  (let [parser (insta/parser ebnf)
        xs (insta/parses parser input)
        num-choices (count xs)
        _ (when (zero? num-choices) (println "insta/parses is no good but can be corrected"))
        res (case num-choices
              1 (first xs)
              0 (insta/parse parser input))
        _ (assert res)]
    res))

(defn x []
  (let [res (parse-it)]
    (pp/pprint res)))