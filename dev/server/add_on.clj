(ns add-on
  (:require [utils :as u]
            [clojure.string :as str]
            [convert :as conv]))

(defn get-kw-for-instruction-name [infos instruction-name kw]
  (assert (and (:program-info infos) (:controller-info infos)))
  (let [f #(get % instruction-name)
        add-on-instructions (-> infos :controller-info :add-on-instructions)
        _ (assert add-on-instructions (str "No :add-on-instructions found: " (keys (:controller-info infos))))]
    (-> add-on-instructions f kw :input :parsed-value)))

(def param-name #(-> % second second))
(def param-type #(-> % (nth 2) second))

(defn predicate [kw affirm-value]
  (fn [def]
    (let [fourth (nth def 3)
          visible-assign (some #(when (= (first %) kw) %) (next fourth))]
      (= affirm-value (last visible-assign)))))

#_(defn param-visible? [def]
  (let [fourth (nth def 3)
        visible-assign (some #(when (= (first %) :visible-assign) %) (next fourth))]
    (= "Yes" (last visible-assign))))

(defn param-visible? [def] ((predicate :visible-assign "Yes") def))
(defn param-input? [def] ((predicate :usage-assign "Input") def))
(defn param-output? [def] ((predicate :usage-assign "Output") def))

(def local-default-value #(-> % (nth 3) second second (nth 2)))

;;
;; Create map-entry that will be used while in the routine. Hence the key will be the param name,
;; and everything else will be under its own key in the value.
;;
(defn bring-together-params [param-name type arg-name]
  [param-name {
               :type  type
               :arg   arg-name}])

;;
;; This one just for parameters. We will have another for local variables
;;
(defn create-params-at [param-names types arg-names]
  (into {} (map bring-together-params param-names types arg-names)))

(def locals-repo (atom nil))

(defn substitute-type [in-type]
  (case in-type
    "DINT" :int
    "REAL" :eng))

(defn locals-getter [name type default-value]
  (fn [tag]
    (assert (= tag name))
    (let [current-val (get @locals-repo tag)
          res (if current-val
                [(substitute-type type) (str current-val)]
                default-value)]
      (:val/val ((conv/seq-> (fn [_] type)) [nil res])))))

(defn locals-setter [name type default-value]
  (fn [tag value]
    (assert (= tag name))
    (swap! locals-repo assoc tag value)))

(defn bring-together-locals [name type default-value]
  [name {:local/type type
         ;:local/default-value (:val/val ((conv/seq-> (fn [_] type)) [nil default-value]))
         :local/getter (locals-getter name type default-value)
         :local/setter (locals-setter name type default-value)
         }])

(defn create-locals-at [local-tag-names
                        local-tag-types
                        local-tag-default-values]
  (into {} (map bring-together-locals
                local-tag-names local-tag-types local-tag-default-values)))

;;
;; An add on instruction is a little routine
;;
(defn run-add-on-instruction-hof [assemble-routine-f scan-routine-f infos instruction]
  (fn [in-energy]
    (let [_ (reset! locals-repo nil)
          ;_ (assert false (str "last-arg is: <" instruction ">"))
          [instruction-name data-structure & args] instruction
          get-kw (partial get-kw-for-instruction-name infos instruction-name)
          routine (get-kw :routine)
          parameters (get-kw :parameters)
          local-tags (next (get-kw :local-tags))
          ;_ (u/debug-to-file (fn [] (-> infos :info)))
          _ (assert routine)
          _ (assert parameters)
          _ (assert local-tags)
          input-defs (filter #(and (param-visible? %) (param-input? %)) (next parameters))
          output-defs (filter #(and (param-visible? %) (param-output? %)) (next parameters))
          ;_ (println (seq output-defs))
          readable-def-names (map param-name input-defs)
          readable-def-types (map param-type input-defs)
          writable-def-names (map param-name output-defs)
          writable-def-types (map param-type output-defs)
          ;_ (println "output names: " writable-def-names ", types: " writable-def-types)
          local-tag-names (map param-name local-tags)
          local-tag-types (map param-type local-tags)
          local-tag-default-values (map local-default-value local-tags)
          ;_ (println "local names: " local-tag-names ", types: " local-tag-types)
          locals-at (create-locals-at local-tag-names local-tag-types local-tag-default-values)
          input-params-at (create-params-at readable-def-names readable-def-types args)
          output-params-at (create-params-at writable-def-names writable-def-types [(last args)])
          ;_ (println "ARGS: " args)
          _ (assert true (str "output-params-at: " output-params-at))
          local-infos (assoc infos :add-on-context {:context/input-params-at  input-params-at
                                                    :context/output-params-at output-params-at
                                                    :context/locals-at        locals-at})
          runnable-routine (assemble-routine-f local-infos routine)
          _ (scan-routine-f false runnable-routine)
          ;_ (println "local-tags: " local-tags)
          ]
      (assert true (str "Nufin for " instruction-name ", args: " args ", for data structure: " data-structure))
      in-energy)))
