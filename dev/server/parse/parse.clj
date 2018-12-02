(ns parse.parse
  (:require
    [instaparse.core :as insta]
    [utils :as u]
    [clojure.string :as str]
    [io-util :as io]))

(def dont-transform? false)

;;
;; Choose one of these three functions for how to parse
;; Unfortunately parse-many-first can hit the peformance problem and go forever
;; Only solution to test that don't have more than one path is to use a smaller structure
;; Hmm - however even that didn't do the trick
;;
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

(defn experiment [x]
  (let [_ (io/debug-crashed-input #(insta/transform {:switch (fn [a b] [:switch a b])} %) x)]))

(defn post-process [my-tree]
  (let [
        _ (assert (coll? my-tree))
        processed (insta/transform {:int       (fn [& args] [:int (apply str args)])
                                    :inst-args (fn [x] (str/split (u/strip-edges x) #","))
                                    :inst      (fn [& args] (vec args))
                                    :str-br    (fn [& args]
                                                 (let [size (-> args first second u/string->int)
                                                       str-value (-> args second second)
                                                       as-one (apply str (vec (take size str-value)))]
                                                   ;(println (str "ARGS: <" size ">, <" str-value ">"))
                                                   ;(println (str "as one: <" (apply str (vec (take size str-value))) ">"))
                                                   ;(assert false)
                                                   [:string as-one]
                                                   ))
                                    } my-tree)
        ;_ (u/debug-to-file processed)
        ]
    processed))

;;
;; msg is reserved for message that there are duplicate ways of parsing. Hence always nil from here
;;
(defn parse-one-only [ebnf input]
  (let [my-parser (insta/parser ebnf)
        res (insta/parse my-parser input)
        transformed (if dont-transform? res (post-process res))
        _ (assert transformed (str "No result"))]
    {:res transformed :msg nil}))

(defn parse-one-only-optimized [ebnf input]
  (let [my-parser (insta/parser ebnf)
        res (insta/parse my-parser input :optimize :memory)
        _ (assert res (str "No result"))]
    {:res res :msg nil}))
