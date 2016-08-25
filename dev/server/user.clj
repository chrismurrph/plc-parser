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
        _ (when (zero? num-choices) (println "insta/parses might be able to be corrected"))
        _ (when (> num-choices 1) (println (str "insta/parses shows more than one way to parse: " num-choices)))
        _ (when (= 1 num-choices) (println "insta/parses shows one choice, so all good"))
        res (cond
              ;; Just fix there being more than one rather than show them all
              ;(> num-choices 1) xs
              (= num-choices 1) (first xs)
              (zero? num-choices) (insta/parse parser input))
        _ (assert res)]
    res))

(defn x []
  (let [res (parse-it)]
    (pp/pprint res)))