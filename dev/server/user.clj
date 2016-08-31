(ns user
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.pprint :as pp]))

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

(defn indexes-of [s value]
  (loop [acc [] idx 0]
    (let [res (str/index-of s value idx)]
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
         begin (str/index-of s begin-str)
         _ (assert begin (str "No begin-str found: <" begin-str "> in: <" (vec (take 150 s)) ">"))
         end (str/index-of s end-str)
         res (info ebnf s begin-str end-str begin end)]
     res))
  ([s begin-str end-str]
   (first-of nil s begin-str end-str)))

(defn old-break-up [s]
  (let [datatypes (groups-of nil s "DATATYPE " "END_DATATYPE")
        modules (groups-of nil s "MODULE " "END_MODULE")
        tag (first-of nil s "TAG" "END_TAG")
        routines (groups-of nil s "ROUTINE " "END_ROUTINE")
        res (concat datatypes modules [tag] routines)]
    res))

(defn break-up-controller [s]
  (let [tag (first-of (slurp "tag.bnf") s "TAG" "END_TAG")
        programs (groups-of s "PROGRAM " "END_PROGRAM")
        datatypes (groups-of (slurp "datatype.bnf") s "DATATYPE " "END_DATATYPE")
        ;modules (groups-of (slurp "module.bnf") s "MODULE " "END_MODULE")
        ;;add-on-instructions (groups-of (slurp "add-on-instruction.bnf") s "ADD_ON_INSTRUCTION_DEFINITION " "END_ADD_ON_INSTRUCTION_DEFINITION")
        res {:tag       tag
             :programs  programs
             :datatypes datatypes
             ;:modules   modules
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

(defn usual-x [ebnf input]
  (let [res (parse-many-first ebnf input)]
    (pp/pprint res)))

(defn group-parser [groups group-name ebnf]
  (let [blocks (filter #(= (:name %) group-name) groups)
        my-parser (partial parse-one-only ebnf)
        _ (assert (seq blocks) (str "None found for " group-name))
        code-blocks (map #(-> % :value (str/replace #"\t" "        ") my-parser) blocks)
        ]
    code-blocks))

(defn pp-str [x] (-> x pp/pprint with-out-str))

(defn err->out [v]
  (assert v)
  (spit "bad_input.txt" v))

(defn find-problem [name m]
  (assert (= (:name m) name) (str "Name found is not " name " but <" (:name m) "> SEE: " (vec m)))
  (if (:err? m)
    (let [res (-> m :parsed-value :res)
          {:keys [line column reason]} res
          input (:value m)]
      [(str "Problem is at line " line " and col " column " b/c: " reason) input]
      {:msg (str "Problem is at line " line " and col " column " b/c: " reason) :value input :okay? false})
    {:msg "No problem" :okay? true}))

;;
;; Know the structure of :programs, so look into all and first error that find put in "bad_input.txt"
;; with message to user so easy to look at it
;; At lowest level always returning [msg value] - so print the msg and output the value to file 
;;
(defn err-from-programs [programs]
  (println (count programs))
  (let [{:keys [msg value]} (find-problem "TAG" (-> programs last :tag))]
    (do
      (println (pp-str msg))
      (err->out value))
    (do
      (println "All fine")
      (err->out "All fine"))))

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
    (errors-from-programs parsed-programs)
    #_(spit "output.txt" (pp-str parsed-programs))
    #_(when (-> first-program :tag :err?)
      (err->out (-> first-program :tag :value)))))

(defn x-old []
  (let [groups (old-break-up prod-input)
        ms (group-parser groups "TAG" (slurp "tag.bnf"))
        results (map :res ms)
        first-bad-result (some #(when (:reason %) %) results)]
    (if first-bad-result
      (do
        (spit "output.txt" (pp-str first-bad-result))
        (spit "bad_input.txt" (-> ms first :input)))
      (do
        (spit "output.txt" (pp-str results))
        (spit "bad_input.txt" "All clear")))))

(defn x-old-old []
  (let [groups (old-break-up prod-input)
        display-results (map #(vector (:name %) (:begin %) (:end %)) groups)
        tags (filter #(= (:name %) "TAG") groups)
        program-tags (:value (first tags))
        nicer-program-tags (str/replace program-tags #"\t" "        ")
        [res msg] (parse-one-only (slurp "tag.bnf") nicer-program-tags)
        ]
    ;(println program-tags)
    ;(pp/pprint res)
    (when (:reason res)
      (do
        (println (str "See bad_input.txt line: " (:line res) ", column: " (:column res)))
        (spit "bad_input.txt" nicer-program-tags)))
    (when msg (println msg))))