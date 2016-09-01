(ns user
  (:require [instaparse.core :as insta]
            [clojure.pprint :as pp]
            [utils :as u]
            [clojure.string :as str]))

(defn r []
  (require 'user :reload))

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

(defn two-x [ebnf input]
  (let [[msg [one two]] (parse-many-first ebnf input)]
    (spit "one.txt" (with-out-str (pp/pprint one)))
    (spit "two.txt" (with-out-str (pp/pprint two)))
    (println msg)))

(defn one-x [ebnf input]
  (let [res (parse-one-only ebnf input)]
    (pp/pprint res)))

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
        ;_ (spit "first_tag" (:value tag))
        programs (groups-of s "PROGRAM" "END_PROGRAM")
        datatypes (groups-of (slurp "datatype.bnf") s "DATATYPE" "END_DATATYPE")
        modules (groups-of (slurp "module.bnf") s "MODULE" "END_MODULE")
        configs (groups-of (slurp "config.bnf") s "CONFIG" "END_CONFIG")
        tasks (groups-of (slurp "task.bnf") s "TASK" "END_TASK")
        ;;add-on-instructions (groups-of (slurp "add-on-instruction.bnf") s "ADD_ON_INSTRUCTION_DEFINITION " "END_ADD_ON_INSTRUCTION_DEFINITION")
        res {:tag       tag
             :programs  programs
             :datatypes datatypes
             :modules   modules
             :configs   configs
             :tasks     tasks
             ;:add-on-instructions add-on-instructions
             }]
    res))

(defn break-up-program [s]
  (let [_ (assert s)
        tag (first-of (slurp "tag.bnf") s "TAG" "END_TAG")
        routines (groups-of (slurp "routine.bnf") s "ROUTINE " "END_ROUTINE")
        res {:tag tag
             :routines routines}]
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

        potential-tag-problem (-> controller :tag tag-problem-finder-fn)
        potential-datatype-problem (-> controller :datatypes datatype-problem-finder-fn)
        potential-module-problem (-> controller :modules module-problem-finder-fn)
        potential-config-problem (-> controller :configs config-problem-finder-fn)
        potential-task-problem (-> controller :tasks task-problem-finder-fn)

        current-potential potential-task-problem
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

(defn x []
  (let [controller (break-up-controller prod-input)
        programs (:programs controller)
        ;_ (println (str "Num programs is " (count programs)))
        parsed-programs (map #(-> % :value break-up-program) programs)
        ]
    (errors-from-controller controller)
    #_(errors-from-programs parsed-programs)
    #_(spit "output.txt" (pp-str parsed-programs))
    #_(when (-> first-program :tag :err?)
      (err->out (-> first-program :tag :value)))))