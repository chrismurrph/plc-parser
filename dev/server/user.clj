(ns user
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(defn r []
  (require 'user :reload))

(def ebnf (slurp "commentparser.bnf"))

(def input (slurp "newline-comment.txt"))

(defn x []
  (let [parser (insta/parser ebnf)
        res (parser input)]
    ;(println parser)
    res))