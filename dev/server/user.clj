(ns user
  (:require [instaparse.core :as insta]
            [clojure.pprint :as pp]
            [utils :as u]
            [clojure.string :as str]
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

#_(defn parent-of [loc]
  (when-let [parent-loc (-> loc zip/up zip/up first)]
    (zip/node parent-loc)))

(defn parent-of [loc]
  (some-> loc zip/up zip/up first zip/node))

#_(defn has-children? [loc]
  (when-let [to-right (zip/right loc)]
    (-> to-right zip/node vector?)))

(defn has-children? [loc]
  (some-> (zip/right loc) zip/node vector?))

#_(defn visit-all []
  (doseq [loc locs]
    (let [node (zip/node loc)]
      (when (keyword? node)
        (println node "has parent" (parent-of loc))))))

(defn to-change? [loc]
  (let [node (zip/node loc)]
    (and (map? node) (has-children? loc) #_(= :many (:cardinality node)))))

(defn modify [loc]
  (zip/edit loc assoc :value "Hi!"))

(defn modify-all [z]
  (loop [loc z]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next
               (cond (to-change? loc) (modify loc)
                     :else loc))))))

(defn from-loc [loc]
  (let [node (zip/node loc)]
    (if (map? node)
      {:node node :parent (parent-of loc)}
      nil)))

(defn visit-all [z]
  (loop [loc z results []]
    (if (zip/end? loc)
      (remove nil? results)
      (recur (zip/next loc) (conj results (from-loc loc))))))


;(def everything-ebnf (slurp "parser.bnf"))
;(def tag-ebnf (slurp "tag.bnf"))

;(def curr-dev-input (slurp "version_assign_2.txt"))
(def prod-input (slurp "prod_input.L5K"))

;; :optimize :memory


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

(defn parse-one-only [ebnf input]
  (let [my-parser (insta/parser ebnf)
        res (insta/parse my-parser input)
        _ (assert res (str "No result"))]
    {:res res :msg nil}))

(defn parse-one-only-optimized [ebnf input]
  (let [my-parser (insta/parser ebnf)
        res (insta/parse my-parser input :optimize :memory)
        _ (assert res (str "No result"))]
    {:res res :msg nil}))

(defn test-whole-word []
  (let [s prod-input
        want "TAG"
        res (u/whole-word-index-of s want 0)]
    res))

(defn indexes-of [s value]
  (loop [acc [] idx 0]
    (let [res (u/whole-word-index-of s value idx)]
      (if (nil? res)
        acc
        (recur (conj acc res) (inc res))))))

;; #(-> % :value (str/replace #"\t" "        ") my-parser)
(def do-parse (fn [ebnf s]
                (parse-one-only ebnf s)))

(defn info [ebnf s begin-str end-str begin end]
  (let [_ (assert begin-str)
        _ (assert end-str)
        _ (assert end)
        _ (assert begin)
        real-end (+ end (count end-str))
        value (subs s begin real-end)
        rid-tabs-value (str/replace value #"\t" "        ")
        parsed-value (when ebnf (do-parse ebnf rid-tabs-value))
        ;; there should also be a msg, so that's another way finding out
        err? (if (-> parsed-value :res :reason) true false)]
    {:name         (str/trim begin-str)
     :value        rid-tabs-value
     :parsed-value parsed-value
     :begin        begin
     :end          real-end
     :err?         err?}))

(defn groups-of
  ([ebnf s begin-str end-str]
   (let [begins (indexes-of s begin-str)
         ends (indexes-of s end-str)
         num-begins (count begins)
         num-ends (count ends)
         _ (assert (= num-begins num-ends) (str "Number begins and ends: " num-begins "," num-ends " - not same for: " begin-str))
         res (map #(info ebnf s begin-str end-str %1 %2) begins ends)]
     res))
  ([s begin-str end-str]
   (groups-of nil s begin-str end-str)))

;; Used when there is only one
(defn first-of
  ([ebnf s begin-str end-str]
   (let [_ (assert s)
         _ (assert begin-str)
         _ (assert end-str)
         begin (u/whole-word-index-of s begin-str 0)
         _ (assert begin (str "No begin-str found: <" begin-str "> in: <" (vec (take 150 s)) ">"))
         end (u/whole-word-index-of s end-str 0)
         _ (assert (> end begin) (str "Begin must be before end: " begin " " end))
         ;_ (println "begin end " begin " " end ": " (vec (take 150 (drop begin s))))
         res (info ebnf s begin-str end-str begin end)]
     res))
  ([s begin-str end-str]
   (first-of nil s begin-str end-str)))

(defn break-up-controller [s]
  (let [tag (first-of (slurp "tag.bnf") s "TAG" "END_TAG")
        programs (groups-of s "PROGRAM" "END_PROGRAM")
        add-on-instructions (groups-of s "ADD_ON_INSTRUCTION_DEFINITION" "END_ADD_ON_INSTRUCTION_DEFINITION")
        datatypes (groups-of (slurp "datatype.bnf") s "DATATYPE" "END_DATATYPE")
        modules (groups-of (slurp "module.bnf") s "MODULE" "END_MODULE")
        configs (groups-of (slurp "config.bnf") s "CONFIG" "END_CONFIG")
        tasks (groups-of (slurp "task.bnf") s "TASK" "END_TASK")
        trends (groups-of (slurp "trend.bnf") s "TREND" "END_TREND")
        ;;add-on-instructions (groups-of (slurp "add-on-instruction.bnf") s "ADD_ON_INSTRUCTION_DEFINITION " "END_ADD_ON_INSTRUCTION_DEFINITION")
        res {:tag                 tag
             :programs            programs
             :add-on-instructions add-on-instructions
             :datatypes           datatypes
             :modules             modules
             :configs             configs
             :tasks               tasks
             :trends              trends
             }]
    res))

(defn break-up-program [s]
  (let [_ (assert s)
        tag (first-of (slurp "tag.bnf") s "TAG" "END_TAG")
        routines (groups-of (slurp "routine.bnf") s "ROUTINE" "END_ROUTINE")
        res {:tag      tag
             :routines routines}]
    res))

(defn break-up-add-on-instruction [s]
  (let [_ (assert s)
        parameters (first-of (slurp "parameters.bnf") s "PARAMETERS" "END_PARAMETERS")
        local-tags (first-of (slurp "tag.bnf") s "LOCAL_TAGS" "END_LOCAL_TAGS")
        routines (first-of (slurp "routine.bnf") s "ROUTINE" "END_ROUTINE")
        res {:parameters parameters
             :local-tags local-tags
             :routines   routines}]
    res))

(defn pp-str [x] (-> x pp/pprint with-out-str))

(defn err->out [v]
  (assert v)
  (spit "bad_input.txt" v))

(defn err->out-debug [v msg]
  (assert v msg)
  (spit "bad_input.txt" v))

(defn find-problem [name m]
  (assert (map? m))
  (assert (= (:name m) name) (str "Name found is not " name " but <" (:name m) "> SEE: " (vec m)))
  (if (:err? m)
    (let [res (-> m :parsed-value :res)
          {:keys [line column reason]} res
          input (:value m)]
      [(str "Problem is at line " line " and col " column " b/c: " reason) input]
      {:msg (str "Problem is at line " line " and col " column " b/c: " reason) :value input :okay? false})
    {:msg "No problem" :okay? true}))

(defn find-problem-from-coll [name c]
  (let [potential-problems (map #(find-problem name %) c)
        bad-problem (some #(when-not (:okay? %) %) potential-problems)
        ;_ (println "Got " bad-problem)
        ]
    (if bad-problem
      bad-problem
      {:msg "No problem" :okay? true})))

(defn errors-from-controller [controller]
  (let [tag-problem-finder-fn (partial find-problem "TAG")
        datatype-problem-finder-fn (partial find-problem-from-coll "DATATYPE")
        module-problem-finder-fn (partial find-problem-from-coll "MODULE")
        config-problem-finder-fn (partial find-problem-from-coll "CONFIG")
        task-problem-finder-fn (partial find-problem-from-coll "TASK")
        trend-problem-finder-fn (partial find-problem-from-coll "TREND")

        potential-tag-problem (-> controller :tag tag-problem-finder-fn)
        potential-datatype-problem (-> controller :datatypes datatype-problem-finder-fn)
        potential-module-problem (-> controller :modules module-problem-finder-fn)
        potential-config-problem (-> controller :configs config-problem-finder-fn)
        potential-task-problem (-> controller :tasks task-problem-finder-fn)
        potential-trend-problem (-> controller :trends trend-problem-finder-fn)

        current-potential potential-trend-problem
        ]
    (if (-> current-potential :okay? not)
      (do
        (println (pp-str (:msg current-potential)))
        (err->out-debug (:value current-potential) (str "No value: " (keys current-potential))))
      (do
        (println "All fine")
        (err->out "All fine")))))

;;
;; Know the structure of :programs, so look into all and first error that find put in "bad_input.txt"
;; with message to user so easy to look at it
;; At lowest level always returning [msg value] - so print the msg and output the value to file 
;;
(defn errors-from-programs [programs]
  (let [tag-problem-finder-fn (partial find-problem "TAG")
        routine-problem-finder-fn (partial find-problem "ROUTINE")
        tag-problems (remove #(-> % :okay?) (map #(-> % :tag tag-problem-finder-fn) programs))
        _ (assert (empty? tag-problems))
        routine-problems (remove #(-> % :okay?) (map #(-> % routine-problem-finder-fn) (mapcat :routines programs)))
        _ (assert (empty? routine-problems))
        examining-problem (first routine-problems)
        ]
    (if examining-problem
      (do
        (println (pp-str (:msg examining-problem)))
        (err->out (:value examining-problem)))
      (do
        (println "All fine")
        (err->out "All fine")))))

(defn errors-from-instructions [instructions]
  (let [tag-problem-finder-fn (partial find-problem-from-coll "LOCAL_TAGS")
        routine-problem-finder-fn (partial find-problem "ROUTINE")
        parameters-problem-finder-fn (partial find-problem "PARAMETERS")
        tag-problems (remove #(-> % :okay?) (map #(-> % :tag tag-problem-finder-fn) instructions))
        ;_ (assert (empty? tag-problems))
        ;routine-problems (remove #(-> % :okay?) (map #(-> % :routines routine-problem-finder-fn) instructions))
        ;_ (assert (empty? routine-problems))
        parameters-problems (remove #(-> % :okay?) (map #(-> % :parameters parameters-problem-finder-fn) instructions))
        examining-problem (first parameters-problems)
        ]
    (if examining-problem
      (do
        (println (pp-str (:msg examining-problem)))
        (err->out (:value examining-problem)))
      (do
        (println "All fine")
        (err->out "All fine")))))

(defn x-check []
  (let [controller (break-up-controller prod-input)
        programs (:programs controller)
        instructions (:add-on-instructions controller)
        ;_ (println (str "Num programs is " (count programs)))
        broken-up-programs (map #(-> % :value break-up-program) programs)
        broken-up-instructions (map #(-> % :value break-up-add-on-instruction) instructions)
        ]
    ;(errors-from-controller controller)
    ;(errors-from-programs broken-up-programs)
    (errors-from-instructions broken-up-instructions)
    #_(spit "output.txt" (pp-str parsed-programs))
    #_(when (-> first-program :tag :err?)
        (err->out (-> first-program :tag :value)))))

(defn start-up [z]
  (-> z zip/down (zip/edit assoc :input-value prod-input) zip/root zip/vector-zip))

(defn x []
  (pp/pprint (visit-all (zip/vector-zip (modify-all z #_(start-up z))))))