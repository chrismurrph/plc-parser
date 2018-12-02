 (ns parse.parsing
   (:require [utils :as u]
             [parse.parse :as p]
             [clojure.string :as str]))

(defn err->out [v]
  (assert v (str "Can't send nothing to bad output"))
  (spit "bad_input.txt" v))

(defn err->out-debug [v msg]
  (assert v msg)
  (spit "bad_input.txt" v))

;; Intention is you call any parse function from here - replace what's there now
;; That's why nil? check being done even although will always be nil if the function
;; happens to be p/parse-one-only. Everything will break in production if a message
;; comes through, and that's what we want.
;; :msg only relevant when trying to parse twice to see if there is more than one way to parse.
(def do-parse (fn [ebnf s]
                (let [res (p/parse-one-only ebnf s)]
                  res
                  #_(if (nil? (:msg res))
                    (:res res)
                    res))))

(defn info [ebnf s begin-str end-str begin end parent?]
  (let [
        ;_ (println "info being called for " begin-str)
        _ (assert s)
        _ (assert begin-str)
        _ (assert end-str)
        _ (assert end)
        _ (assert begin)
        real-end (+ end (count end-str))
        value (subs s begin real-end)
        _ (assert value (str "Can't find a value between <" begin "> and <" real-end ">"))
        rid-tabs-value (str/replace value #"\t" "        ")
        parsed-value (some-> ebnf (do-parse rid-tabs-value))
        _ (assert (nil? (:msg parsed-value)) (:msg parsed-value))
        err? (if (-> parsed-value :res :reason) true false)
        name (str/trim begin-str)
        ;_ (println "info for" name)
        ]
    {:name         name
     :in-value     (cond
                     parent? rid-tabs-value
                     err? rid-tabs-value
                     :default nil)
     ;; To get rid of the double wrapping and make whole thing hiccup
     :parsed-value (if err? parsed-value (into [] (apply concat (:res parsed-value))))
     :begin        begin
     :end          real-end
     :err?         err?}))

(def debug? false)
(def debug-on "LOCAL_TAGS")

;;
;; The m being handed in here is one of the things the parent keeps in :result,
;; where result is map of keys [pid id input output]
;; In response we are returning many, each of which goes into input
;; Actually because it is a one-to-one wrapper, lets return many of the same type
;; that is coming in.
;; What is parsed is [:input :in-value] from the parent.
;;
(defn groups-of
  ([ebnf begin-str end-str parent? m]
   (let [_ (assert m)
         _ (assert begin-str)
         {:keys [id input]} m
         _ (assert input)
         {:keys [in-value]} input
         _ (assert in-value (str "No in-value, keys are: " (keys input) " for " begin-str))
         _ (when (and debug? (= begin-str debug-on))
             (println (str "Looking for <" begin-str "> in <" (apply str (take 100 in-value)) ">")))
         ;_ (assert (string? s) (str "groups-of not getting string for <" begin-str "> is <" s "> type " (type s) "called from" debug))
         begins (u/indexes-of-whole-word in-value begin-str false)
         ends (u/indexes-of-whole-word in-value end-str false)
         num-begins (count begins)
         num-ends (count ends)
         _ (assert (= num-begins num-ends) (str "Number begins and ends: " num-begins "," num-ends " - not same for: " begin-str))
         infos (map #(info ebnf in-value begin-str end-str %1 %2 parent?) begins ends)
         res (mapv (fn [info]
                     {:pid id
                      :id (gensym)
                      :input info}) infos)]
     res)))

(defn find-problem
  "Just formatting - whether there's a problem has already been worked out"
  [m]
  (assert (map? m))
  (let [err? (-> m :input :err?)
        _ (assert (boolean? err?) (str "Whether there's a problem has not yet been worked out for map with keys: " (keys (:input m)) ", actual:" (:input m)))
        res (if err?
              (let [res (-> m :input :parsed-value :res)
                    _ (assert res (str "If err? there needs to be reason, got none from " (-> m :input :parsed-value)))
                    {:keys [line column reason]} res
                    input (-> m :input :in-value)
                    _ (assert input (str "Can't be an error without input: <" (-> m :input :in-value) ">"))]
                ;[(str "Problem is at line " line " and col " column " b/c: " reason) input]
                {:msg (str "Problem (see bad_input.txt) is at line " line " and col " column " b/c:\n" (u/pp-str reason 70)) :msg-value input :okay? false})
              {:msg "No problem" :okay? true})]
    res))
