(ns user
  (:require [clojure.pprint :as pp]
            [parsing :as par]
            [clojure.zip :as zip]))

(defn r []
  (require 'user :reload))

;;
;;      A
;;     / \
;;    B   C
;;   /\    \
;;  D  E    F
;;

(def example-given '(A (B (D) (E)) (C (F))))
(def example [:a [:b [:d] [:e]] [:c [:f]]])

(def structure [{:name :controller :cardinality :one}
                [{:name :tag :cardinality :one :bnf "tag" :tag "TAG"}]
                [{:name :program :cardinality :many :tag "PROGRAM"}
                 [{:name :tag :cardinality :one :bnf "tag"}
                  {:name :routine :cardinality :many :bnf "routine"}]]
                [{:name :datatypes :cardinality :many :bnf "datatype" :tag "DATATYPE"}]
                [{:name :modules :cardinality :many :bnf "module" :tag "MODULE"}]
                [{:name :config :cardinality :many :bnf "config" :tag "CONFIG"}]
                [{:name :task :cardinality :many :bnf "task" :tag "TASK"}]
                [{:name :trend :cardinality :many :bnf "trend" :tag "TREND"}]
                [{:name :add-on-instruction :cardinality :many :tag "ADD_ON_INSTRUCTION_DEFINITION"}
                 [{:name :parameters :cardinality :one :bnf "parameters"}
                  {:name :local-tags :cardinality :one :bnf "tag"}
                  {:name :routine :cardinality :one :bnf "routine"}]]])

(def z (zip/vector-zip structure))
(def locs (take-while (complement zip/end?) (iterate zip/next z)))
(def testing? false)

(defn shorten-result [x]
  (if testing?
    (cond
      (map? x) (keys x)
      (string? x) (apply str (vec (take 60 x)))
      :default x)
    x))

(defn parent-of [loc]
  (some-> loc zip/up zip/up first zip/node (update :result shorten-result)))

(defn has-children? [loc]
  (some-> (zip/right loc) zip/node vector?))

#_(defn visit-all []
    (doseq [loc locs]
      (let [node (zip/node loc)]
        (when (keyword? node)
          (println node "has parent" (parent-of loc))))))

(defn to-change? [loc]
  (let [node (zip/node loc)
        many? (= :many (:cardinality node))
        parent? (has-children? loc)
        parent (parent-of loc)
        is-map? (map? node)
        res? (and is-map? (some-> parent :result string?))
        _ (when res? (println res? "for" (:name node)))]
    res?))

(defn break-into-parse [loc-ref]
  (let [{:keys [name bnf tag cardinality]} (zip/node loc-ref)
        start-str tag
        end-str (str "END_" tag)
        _ (println "name: " bnf "for" name)
        ebnf (some-> bnf (str ".bnf") slurp)
        s (:result (parent-of loc-ref))
        func (if (= cardinality :many) par/groups-of par/first-of)
        res (func ebnf s start-str end-str)]
    res))

(defn modify [loc]
  (zip/edit loc assoc :result (break-into-parse loc)))

(defn modify-all [z]
  (loop [loc z]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
               (cond
                 (to-change? loc) (modify loc)
                 :else loc))))))

(defn from-loc [loc]
  (let [node (zip/node loc)]
    (if (map? node)
      {:node (update node :result shorten-result) :parent (parent-of loc)}
      nil)))

(defn visit-all [z]
  (loop [loc z results []]
    (if (zip/end? loc)
      (remove nil? results)
      (recur (zip/next loc) (conj results (from-loc loc))))))

(def prod-input (slurp "prod_input.L5K"))

(defn start-up [z]
  (-> z zip/down (zip/edit assoc :result prod-input) zip/root zip/vector-zip))

(defn x []
  (let [res (-> z start-up modify-all zip/vector-zip visit-all)]
    (when testing? (pp/pprint res))))