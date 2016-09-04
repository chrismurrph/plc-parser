(ns user
  (:require [clojure.pprint :as pp]
            [parsing :as par]
            [clojure.zip :as zip]))

(defn r []
  (require 'user :reload)
  (require 'parsing :reload))

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
                 [{:name :tag :cardinality :one :bnf "tag" :tag "TAG"}
                  {:name :routine :cardinality :many :bnf "routine" :tag "ROUTINE"}]]
                [{:name :datatypes :cardinality :many :bnf "datatype" :tag "DATATYPE"}]
                [{:name :modules :cardinality :many :bnf "module" :tag "MODULE"}]
                [{:name :config :cardinality :many :bnf "config" :tag "CONFIG"}]
                [{:name :task :cardinality :many :bnf "task" :tag "TASK"}]
                [{:name :trend :cardinality :many :bnf "trend" :tag "TREND"}]
                [{:name :add-on-instruction :cardinality :many :tag "ADD_ON_INSTRUCTION_DEFINITION"}
                 [{:name :parameters :cardinality :one :bnf "parameters" :tag "PARAMETERS"}
                  {:name :local-tags :cardinality :one :bnf "tag" :tag "LOCAL_TAGS"}
                  {:name :routine :cardinality :one :bnf "routine" :tag "ROUTINE"}]]])

(def z (zip/vector-zip structure))
(def locs (take-while (complement zip/end?) (iterate zip/next z)))
(def testing? true)

(defn shorten-result [x]
  (if testing?
    (cond
      (map? x) (keys x)
      (string? x) (apply str (vec (take 60 x)))
      :default x)
    x))

(defn parent-of [loc]
  (some-> loc zip/up zip/up first zip/node #_(update :result shorten-result)))

(defn has-children? [loc]
  (some-> (zip/right loc) zip/node vector?))

#_(defn visit-all []
    (doseq [loc locs]
      (let [node (zip/node loc)]
        (when (keyword? node)
          (println node "has parent" (parent-of loc))))))

(defn top-level-change? [loc]
  (let [node (zip/node loc)
        parent (parent-of loc)
        is-map? (map? node)
        res? (and is-map? (some-> parent :result string?))
        ;_ (when res? (println res? "for top level" (:name node)))
        ]
    res?))

(def not-map? (complement map?))

(defn many-of? [x]
  ;(println (type x)) clojure.lang.LazySeq
  ;(-> x not-map? seq?)
  (instance? clojure.lang.LazySeq x)
  )

(defn many-many-of? [x]
  ;(println (type x)) clojure.lang.LazySeq
  ;(-> x not-map? seq?)
  (let [first-ele (first x)
        ;_ (println (type first-ele))
        ]
    (and (instance? clojure.lang.PersistentArrayMap first-ele) (instance? clojure.lang.LazySeq x)))
  )

(defn second-level-change? [loc]
  (let [node (zip/node loc)
        parent? (has-children? loc)
        parent (parent-of loc)
        is-map? (map? node)
        res? (and is-map? (some-> parent :result many-of?))
        ;_ (println res? "for second level" (:name node))
        ]
    res?))

(defn third-level-change? [loc]
  (let [node (zip/node loc)
        node-name (:name node)
        parent? (has-children? loc)
        parent (parent-of loc)
        is-map? (map? node)
        res? (and is-map? (some-> parent :result many-many-of?))
        ;_ (when (and res? is-map?) (println res? "for third level" node-name))
        ]
    res?))

(defn break-into-parse [loc-ref]
  (let [{:keys [name bnf tag cardinality]} (zip/node loc-ref)
        start-str tag
        end-str (str "END_" tag)
        ;_ (println "name: " bnf "for" name)
        ebnf (some-> bnf (str ".bnf") slurp)
        parent (parent-of loc-ref)
        _ (println (type (:result parent)))
        s (:result parent)
        _ (assert (string? s) (str "Not string for " name))
        res (if (= cardinality :many)
              (par/groups-of ebnf start-str end-str "break-into-parse" s)
              (par/first-of ebnf start-str end-str "break-into-parse" s))]
    res))

;;
;; Here there will be a list of parent strings that need to be parsed. For example many
;; programs. In the case of tags this will produce many tags. In the case of routine this
;; will produce many of many routines, because there are many routines per program.
;; However what is returned is just a flattened list, that can be examined for problems
;;
(defn break-into-many-parse [loc-ref many-many?]
  (let [{:keys [name bnf tag cardinality]} (zip/node loc-ref)
        end-str (str "END_" tag)
        _ (assert tag (str "No tag found for " name))
        _ (println "name: " bnf "for" name "many-many? " many-many? "tag: " tag)
        ebnf (some-> bnf (str ".bnf") slurp)
        xs (:result (parent-of loc-ref))
        ;_ (println "xs:" xs)
        _ (assert (coll? xs))
        break-up-parse-fn (if many-many? par/groups-of par/first-of)
        mapping-fn (partial break-up-parse-fn ebnf tag end-str "break-into-many-parse")
        res (map mapping-fn (if many-many?
                              (map #(-> % :value mapping-fn) xs)
                              (map #(-> % :value mapping-fn) xs)))]
    res))

(defn check [res]
  (println "To check a " (type res))
  res)

(defn modify-all [z]
  (loop [loc z]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
               (cond
                 (third-level-change? loc) (zip/edit loc assoc :result (break-into-many-parse loc true))
                 (second-level-change? loc) (zip/edit loc assoc :result (break-into-many-parse loc false))
                 (top-level-change? loc) (zip/edit loc assoc :result (break-into-parse loc))
                 :else loc))))))

(defn from-loc [loc]
  (let [node (zip/node loc)]
    (if (map? node)
      {:node (update node :result shorten-result) :parent (parent-of loc)}
      nil)))

(defn check-loc [loc]
  (let [node (zip/node loc)]
    (if (map? node)
      (let [res (:result node)]
        (cond
          (map? res) (keys res)
          (string? res) (apply str (vec (take 150 res)))
          :default (map keys res)))
      nil)))

(defn visit-all [z]
  (loop [loc z results []]
    (if (zip/end? loc)
      (remove nil? results)
      (recur (zip/next loc) (conj results (check-loc loc))))))

(def prod-input (slurp "prod_input.L5K"))

(defn start-up [z]
  (-> z zip/down (zip/edit assoc :result prod-input) zip/root zip/vector-zip))

(defn x []
  (let [res (-> z start-up modify-all zip/vector-zip visit-all)]
    (when testing? (pp/pprint res))))