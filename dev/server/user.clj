(ns user
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(defn r []
  (require 'user :reload))

(defn x-1 []
  (let [ever-together? (fn [x1 v1 x2 v2]
                         (loop [at1 x1 at2 x2]
                           (let [faster-beyond? (or (and (> v2 v1) (> at2 at1)) (and (> v1 v2) (> at1 at2)))]
                             (cond
                               (= at1 at2) true
                               faster-beyond? false
                               :default (recur (+ at1 v1) (+ at2 v2))))))
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["0 2 5 3"]
        [x1 v1 x2 v2] (str->ints (first input))
        res (ever-together? x1 v1 x2 v2)
        out (if res "YES" "NO")
        ]
    (println out)))

(def ebnf-1
  "S = AB*
   AB = A B
   A = 'a' +
   B ='b' +")

(def ebnf-2 (slurp "commentparser.bnf"))

(def input-1 "aaaaabbbaaaabb")

(def input-2 "(*********************************************\n\n  Import-Export\n  Version   := RSLogix 5000 v20.04\n  Owner     := Illawarra Coal, South32\n  Exported  := Fri Jul 29 15:40:32 2016\n\n  Note:  File encoded in UTF-8.  Only edit file in a program \n         which supports UTF-8 (like Notepad, not Wordpad).\n\n**********************************************)")
(def input-3 (slurp "newline-comment.txt"))
(defn x []
  (let [parser (insta/parser ebnf-2)
        res (parser input-3)]
    ;(println parser)
    res))