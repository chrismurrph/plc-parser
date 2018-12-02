(ns parse.edn-writer
  (:require [clojure.zip :as zip]
            [clojure.set :as set]
            [parse.parsing :as par]
            [utils :as u]
            [io-util :as io]
            [parse.navigation :as nav]))

;;
;; For any loc two up and first works to get parent
;; Encapsulating all and being the first is how you show you are the parent
;; Nodes use {} as their container.
;; [] are needed to show how many levels down. So simply count them as peel up.
;; Don't worry that the lowest level ones are all bound into the same [], whereas the
;; higher ones are not
;;
(def the-structure [{:name :controller :cardinality :one}
                    [{:name :tag :cardinality :one :bnf "tag" :tag "TAG"}]
                    [{:name :program :cardinality :many :bnf "program" :tag "PROGRAM"}
                     [{:name :tag :cardinality :one :bnf "tag" :tag "TAG"}
                      {:name :routine :cardinality :many :bnf "routine" :tag "ROUTINE"}]]
                    [{:name :datatypes :cardinality :many :bnf "datatype" :tag "DATATYPE"}]
                    [{:name :modules :cardinality :many :bnf "module" :tag "MODULE"}]
                    [{:name :config :cardinality :many :bnf "config" :tag "CONFIG"}]
                    [{:name :task :cardinality :many :bnf "task" :tag "TASK"}]
                    [{:name :trend :cardinality :many :bnf "trend" :tag "TREND"}]
                    [{:name :add-on-instruction :cardinality :many :bnf "add-on-instruction-definition" :tag "ADD_ON_INSTRUCTION_DEFINITION"}
                     [{:name :parameters :cardinality :one :bnf "parameters" :tag "PARAMETERS"}
                      {:name :local-tags :cardinality :one :bnf "local-tags" :tag "LOCAL_TAGS"}
                      {:name :routine :cardinality :one :bnf "routine" :tag "ROUTINE"}]]])

;;
;; Here datatypes could be a parent whereas parameters cannot be
;;
(def whole-structure-experiment
  [{:name :controller :cardinality :one}
   [[{:name :tag :cardinality :one :bnf "tag" :tag "TAG"}]
    [{:name :datatypes :cardinality :many :bnf "datatype" :tag "DATATYPE"}]
    [{:name :modules :cardinality :many :bnf "module" :tag "MODULE"}]
    [{:name :config :cardinality :many :bnf "config" :tag "CONFIG"}]
    [{:name :task :cardinality :many :bnf "task" :tag "TASK"}]
    [{:name :trend :cardinality :many :bnf "trend" :tag "TREND"}]
    [{:name :program :cardinality :many :bnf "program" :tag "PROGRAM"}
     [{:name :tag :cardinality :one :bnf "tag" :tag "TAG"}
      {:name :routine :cardinality :many :bnf "routine" :tag "ROUTINE"}]]
    [{:name :add-on-instruction :cardinality :many :bnf "add-on-instruction-definition" :tag "ADD_ON_INSTRUCTION_DEFINITION"}
     [{:name :parameters :cardinality :one :bnf "parameters" :tag "PARAMETERS"}
      {:name :local-tags :cardinality :one :bnf "local-tags" :tag "LOCAL_TAGS"}
      {:name :routine :cardinality :one :bnf "routine" :tag "ROUTINE"}]]]])

(def whole-structure-nearly-best
  [{:name :controller :cardinality :one}
   [{:name :tag :cardinality :one :bnf "tag" :tag "TAG"}
    {:name :datatypes :cardinality :many :bnf "datatype" :tag "DATATYPE"}
    {:name :modules :cardinality :many :bnf "module" :tag "MODULE"}
    {:name :config :cardinality :many :bnf "config" :tag "CONFIG"}
    {:name :task :cardinality :many :bnf "task" :tag "TASK"}
    {:name :trend :cardinality :many :bnf "trend" :tag "TREND"}
    [{:name :program :cardinality :many :bnf "program" :tag "PROGRAM"}
     [{:name :tag :cardinality :one :bnf "tag" :tag "TAG"}
      {:name :routine :cardinality :many :bnf "routine" :tag "ROUTINE"}]]
    [{:name :add-on-instruction :cardinality :many :bnf "add-on-instruction-definition" :tag "ADD_ON_INSTRUCTION_DEFINITION"}
     [{:name :parameters :cardinality :one :bnf "parameters" :tag "PARAMETERS"}
      {:name :local-tags :cardinality :one :bnf "local-tags" :tag "LOCAL_TAGS"}
      {:name :routine :cardinality :one :bnf "routine" :tag "ROUTINE"}]]]])

;;
;;      A
;;     / \
;;    B   C
;;   /\    \
;;  D  E    F
;;

(def example-given '(A (B (D) (E)) (C (F))))
(def example [:a [:b [:d] [:e]] [:c [:f]]])

(def part-structure
  [{:name :controller :cardinality :one}
   [{:name :program :cardinality :many :bnf "program" :tag "PROGRAM"}
    [{:name :routine :cardinality :many :bnf "routine" :tag "ROUTINE"}]]])

(def structure the-structure)

;;
;; Every node has a :result, which is always a vector of maps.
;; :input also has a map, which is the thing returned by init.
;; It in turn has :value in it, which is raw input.
;; In the case of the root it really is a raw value.
;; In other cases we turn the tabs into spaces, which is easier for error reporting
;; We don't need this feature at the root level because we never parse at the root
;; level. Apart from the root case, :value is the result of breaking apart a
;; parent's value into one or more pieces. If there are more then the parent has
;; multiple children.
;; Onlt leaf values will actually be parsed.
;;
(defn start-up [z file-name]
  (-> z zip/down (zip/edit assoc :result [{:input {:in-value (slurp file-name) :err? false}
                                           :id    (gensym)}]) zip/root zip/vector-zip))

(defn parent-of [loc]
  (some-> loc zip/up zip/up first zip/node))

(defn has-children? [loc]
  (some-> (zip/right loc) zip/node vector?))

(defn find-all [kw id z]
  (assert (keyword? kw))
  (loop [loc z results []]
    (if (zip/end? loc)
      results
      (let [node (zip/node loc)]
        (if (map? node)
          (let [;_ (println "want name and result" (keys node))
                _ (assert (set/subset? #{:name :result} (into #{} (keys node))))
                {:keys [name result]} node]
            (if (= name kw)
              (let [only-id (if id
                              (filter #(= (:pid %) id) result)
                              result)]
                (recur (zip/next loc) (concat results only-id)))
              (recur (zip/next loc) results)))
          (recur (zip/next loc) results))))))

(defn has-interesting-parent? [loc]
  (let [node (zip/node loc)
        parent (parent-of loc)
        is-map? (map? node)
        res? (and is-map? (some-> parent :result first :id))
        ;_ (when res? (println res? "for top level" (:name node)))
        ]
    res?))

(defn process [loc-ref]
  (let [{:keys [name bnf tag cardinality]} (zip/node loc-ref)
        end-str (str "END_" tag)
        ebnf (some-> bnf (str ".bnf") slurp)
        parent (parent-of loc-ref)
        parent? (has-children? loc-ref)
        xs (:result parent)
        ;_ (println "name:" bnf "for" name "is a parent?" parent? "type:" (type xs) "size:" (count xs))
        mapping-fn (partial par/groups-of ebnf tag end-str parent?)
        res (vec (mapcat #(mapping-fn %) xs))]
    res))

;;
;; Only modifying nodes, not changing the structure in any way
;;
(defn modify-all [z]
  (loop [loc z]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
               (cond
                 (has-interesting-parent? loc) (zip/edit loc assoc :result (process loc))
                 :else loc))))))

(defn to-edn [all name]
  (let [kw (keyword name)
        res (find-all kw nil all)
        ;; only necessary for program, but no harm doing for all
        res (map #(update % :input dissoc :in-value) res)
        _ (println (str "count: " (count res) " for " name))]
    [res name]))

(defn spit-to-file
  ([dir-name [res name] max-width]
   (let [_ (spit (str dir-name "/" name ".edn") (u/pp-str res max-width))]))
  ([dir-name [res name]]
   (let [_ (spit (str dir-name "/" name ".edn") (u/pp-str res 1000))])))

(def dont-format? false)
;;
;; These will always appear at beginning of a line
;;
(def newline-before [
                     "[:br"
                     "[:br-lev"
                     "[:rung"
                     ])
(def newline-after [
                    "[:rung"
                    ])

;;
;; Just doing /n because can rely on IDEA for rest
;;
(defn format-keywords [s]
  (assert (string? s) (str "Can only format a string, got " (type s)))
  (if dont-format?
    s
    (let [;_ (println (str "Formatting: <" (apply str (take 100 s)) ">"))
          places (sort (apply concat
                              (concat
                                (u/indexes-of-many-whole-words s newline-before false)
                                (u/indexes-of-many-whole-words s newline-after true))))
          res (reduce
                (fn [acc [idx ele]]
                  (u/insert-at acc "\n" (+ ele idx)))
                s
                (map-indexed vector places))]
      res)))

(def debug-file-1 "debug-1.edn")
(def debug-file-2 "debug-2.edn")

(defn format-one-routine-to-file [m debug-file]
  (let [name (-> m :input :parsed-value second second)
        ]
    (when (not (.exists (io/file debug-file)))
      (spit debug-file (-> m :input :parsed-value u/pp-str format-keywords)))
    m))

(defn updating-format-routine [m]
  (let [_ (assert (map? m))
        res (update-in m [:input :parsed-value] (fn [x]
                                                  (-> x str format-keywords)))
        _ (when (not (.exists (io/file debug-file-1)))
            (spit debug-file-1 m)
            (spit debug-file-2 res))]
    res))

;;
;; Writes from the .L5K file to a directory stucture of .edn files that can be run.
;; Takes some time, like about a minute.
;; Will get rid of all your debug instructions
;;
(defn edn-writer [name input-file-name]
  (assert (and name input-file-name))
  (io/rmdir name)                                           ;; Convenient while developing
  (assert (not (io/exists? name)) (str "Delete directory <" name "> to go again"))
  (io/mkdir name)
  (let [z (zip/vector-zip structure)
        all (-> z (start-up input-file-name) modify-all zip/vector-zip)
        ;; Taking first one works but is fragile
        global-tags (first (find-all :tag nil all))
        _ (spit (str name "/tags.edn") (u/pp-str global-tags))

        _ (spit-to-file name (to-edn all "datatypes") 200)
        _ (spit-to-file name (to-edn all "modules") 200)
        _ (spit-to-file name (to-edn all "config") 100)
        _ (spit-to-file name (to-edn all "task") 200)
        _ (spit-to-file name (to-edn all "trend"))
        [programs _] (to-edn all "program")
        program-names (map #(-> % :input :parsed-value nav/program-by-name-q) programs)
        _ (println program-names)
        [add-on-instructions _] (to-edn all "add-on-instruction")
        add-on-instruction-names (map #(-> % :input :parsed-value nav/add-on-instruction-by-name-q) add-on-instructions)
        _ (println add-on-instruction-names)
        ]
    (doseq [program-name program-names]
      (let [
            chosen-program (:id (some #(nav/named? % program-name nav/program-by-name-q) programs))
            _ (assert chosen-program (str "Not found program named: <" program-name ">"))
            tags (find-all :tag chosen-program all)
            routines (find-all :routine chosen-program all)
            diry-name (str name "/program_" program-name)
            ]
        (io/mkdir diry-name)
        (spit (str diry-name "/tags.edn") (u/pp-str tags))
        (io/delete-files [debug-file-1 debug-file-2])
        (spit (str diry-name "/routines.edn") (-> routines u/pp-str format-keywords))))
    (doseq [add-on-name add-on-instruction-names]
      (let [
            chosen-add-on (:id (some #(nav/named? % add-on-name nav/add-on-instruction-by-name-q) add-on-instructions))
            _ (assert chosen-add-on (str "Not found add-on named: <" add-on-name ">"))
            parameters (find-all :parameters chosen-add-on all)
            local-tags (find-all :local-tags chosen-add-on all)
            routine (find-all :routine chosen-add-on all)
            diry-name (str name "/add-on-instruction_" add-on-name)
            ]
        (io/mkdir diry-name)
        (spit (str diry-name "/parameters.edn") (u/pp-str parameters))
        (spit (str diry-name "/local-tags.edn") (u/pp-str local-tags))
        (spit (str diry-name "/routine.edn") (u/pp-str routine))))))
