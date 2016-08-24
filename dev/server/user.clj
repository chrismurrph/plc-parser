(ns user
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(defn r []
  (require 'user :reload))

(def ebnf (slurp "parser.bnf"))

(def input (slurp "version_assign.txt"))

(defn parse []
  (let [parser (insta/parser ebnf)
        res (parser input)]
    res))

(defn x []
  (let [res (parse)]
    (clojure.pprint/pprint res)))