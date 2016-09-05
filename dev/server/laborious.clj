(ns laborious
  (:require
    [utils :as u]
    [clojure.pprint :as pp]
    [parsing :as par]
    [clojure.zip :as zip]))

(defn errors-from-controller [controller]
  (let [
        tag-problem-finder-fn (partial par/find-problem "TAG")
        datatype-problem-finder-fn (partial par/find-one-problem-from-many "DATATYPE")
        module-problem-finder-fn (partial par/find-one-problem-from-many "MODULE")
        config-problem-finder-fn (partial par/find-one-problem-from-many "CONFIG")
        task-problem-finder-fn (partial par/find-one-problem-from-many "TASK")
        trend-problem-finder-fn (partial par/find-one-problem-from-many "TREND")

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
        (println (u/pp-str (:msg current-potential)))
        (par/err->out-debug (:value current-potential) (str "No value: " (keys current-potential))))
      (do
        (println "All fine")
        (par/err->out "All fine")))))

;;
;; Know the structure of :programs, so look into all and first error that find put in "bad_input.txt"
;; with message to user so easy to look at it
;; At lowest level always returning [msg value] - so print the msg and output the value to file
;;
(defn errors-from-programs [programs]
  (let [tag-problem-finder-fn (partial par/find-problem "TAG")
        routine-problem-finder-fn (partial par/find-problem "ROUTINE")
        tag-problems (remove #(-> % :okay?) (map #(-> % :tag tag-problem-finder-fn) programs))
        _ (assert (empty? tag-problems))
        routine-problems (remove #(-> % :okay?) (map #(-> % routine-problem-finder-fn) (mapcat :routines programs)))
        _ (assert (empty? routine-problems))
        examining-problem (first routine-problems)
        ]
    (if examining-problem
      (do
        (println (u/pp-str (:msg examining-problem)))
        (par/err->out (:value examining-problem)))
      (do
        (println "All fine")
        (par/err->out "All fine")))))

(defn errors-from-instructions [instructions]
  (let [tag-problem-finder-fn (partial par/find-one-problem-from-many "LOCAL_TAGS")
        routine-problem-finder-fn (partial par/find-problem "ROUTINE")
        parameters-problem-finder-fn (partial par/find-problem "PARAMETERS")
        tag-problems (remove #(-> % :okay?) (map #(-> % :tag tag-problem-finder-fn) instructions))
        ;_ (assert (empty? tag-problems))
        ;routine-problems (remove #(-> % :okay?) (map #(-> % :routines routine-problem-finder-fn) instructions))
        ;_ (assert (empty? routine-problems))
        parameters-problems (remove #(-> % :okay?) (map #(-> % :parameters parameters-problem-finder-fn) instructions))
        examining-problem (first parameters-problems)
        ]
    (if examining-problem
      (do
        (println (u/pp-str (:msg examining-problem)))
        (par/err->out (:value examining-problem)))
      (do
        (println "All fine")
        (par/err->out "All fine")))))

(def prod-input (slurp "prod_input.L5K"))

(defn break-up-controller [s]
  (let [tag (par/first-of (slurp "tag.bnf") "TAG" "END_TAG" "debug" s)
        programs (par/groups-of nil "PROGRAM" "END_PROGRAM" "debug" s)
        add-on-instructions (par/groups-of nil "ADD_ON_INSTRUCTION_DEFINITION" "END_ADD_ON_INSTRUCTION_DEFINITION" "debug" s)
        datatypes (par/groups-of (slurp "datatype.bnf") "DATATYPE" "END_DATATYPE" "debug" s)
        modules (par/groups-of (slurp "module.bnf") "MODULE" "END_MODULE" "debug" s)
        configs (par/groups-of (slurp "config.bnf") "CONFIG" "END_CONFIG" "debug" s)
        tasks (par/groups-of (slurp "task.bnf") "TASK" "END_TASK" "debug" s)
        trends (par/groups-of (slurp "trend.bnf") "TREND" "END_TREND" "debug" s)
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
        tag (par/first-of (slurp "tag.bnf") "TAG" "END_TAG" "debug" s)
        routines (par/groups-of (slurp "routine.bnf") "ROUTINE" "END_ROUTINE" "debug" s)
        res {:tag      tag
             :routines routines}]
    res))

(defn break-up-add-on-instruction [s]
  (let [_ (assert s)
        parameters (par/first-of (slurp "parameters.bnf") "PARAMETERS" "END_PARAMETERS" "debug" s)
        local-tags (par/first-of (slurp "tag.bnf") "LOCAL_TAGS" "END_LOCAL_TAGS" "debug" s)
        routines (par/first-of (slurp "routine.bnf") "ROUTINE" "END_ROUTINE" "debug" s)
        res {:parameters parameters
             :local-tags local-tags
             :routines   routines}]
    res))

(defn x-check []
  (let [controller (break-up-controller prod-input)
        programs (:programs controller)
        instructions (:add-on-instructions controller)
        ;_ (println (str "Num programs is " (count programs)))
        broken-up-programs (map #(-> % :value break-up-program) programs)
        broken-up-instructions (map #(-> % :value break-up-add-on-instruction) instructions)
        ]
    ;(errors-from-controller controller)
    (errors-from-programs broken-up-programs)
    ;(errors-from-instructions broken-up-instructions)
    #_(spit "output.txt" (pp-str parsed-programs))
    #_(when (-> first-program :tag :err?)
        (err->out (-> first-program :tag :value)))))

