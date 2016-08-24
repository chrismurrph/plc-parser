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
        res (insta/parses parser input)
        _ (assert (seq res) (str "Empty list"))
        choices (count res)
        _ (assert (= choices 1))]
    (first res)))

(defn x []
  (let [res (parse-it)]
    (pp/pprint res)))