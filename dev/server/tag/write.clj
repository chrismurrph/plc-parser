(ns tag.write
  (:require [utils :as u]
            [clojure.string :as str]
            [instructions :as i]
            [tag.read :as read]
            [tag.common :as common]
            [tag.setters :as setters]
            [datatypes :as data]))

(defn alter-tag [aliases tag]
  (let [alias-for (:alias-for (first (filter #(= tag (:alias-name %)) aliases)))]
    (if alias-for
      (data/wrap-arg alias-for)
      tag)))

(defn tag-setter
  ([infos]
   (let [
         ;datatype-names (into #{} (map (comp second second) (-> infos :info :datatypes)))
         custom-grouped (common/get-custom-grouped-tags infos)
         aliases (-> infos :program-info :program/aliases)
         ;_ (assert (pos? (count aliases)))
         ]
     ;;
     ;; Make sure it is in the cache and set the value if it is different
     ;;
     (fn inner-fn
       ([context]
        (inner-fn context false))
       ([context debug?]
        (let [{:keys [count-in-array]} context]
          (fn [tag value]
            (assert (not (and (= "VAL" tag) (nil? context))) (str "S/always have context when VAL"))
            (let [_ (when debug? (println (str "---> context of <" tag ">: <" context "> nil?: " (nil? context))))
                  _ (assert (u/is? value) (str "Probably wrong that setting " tag " to nil"))
                  _ (assert (u/not-blank? tag) (str "tag-setter being called with blank name: <" tag "> for value: <" value ">"))
                  alias-altered-tag (alter-tag aliases tag)
                  _ (when (= tag "VAL" "A1_METHANE_RAW")
                      (assert (= tag alias-altered-tag) (str ""))
                      ;(println "Converted from <" tag "> to <" alias-altered-tag ">")
                      )
                  tag-retriever-lof ((read/internal-tag-retriever infos custom-grouped aliases debug?) context)
                  [type-kw old-value] (tag-retriever-lof alias-altered-tag)
                  _ (when debug? (assert (or (not= tag "VAL")
                                             (= type-kw :add-on-context))
                                         (str "VAL, yet not :add-on-context: <" context ">, type-kw: <" type-kw ">")))
                  _ (when (not= type-kw :hardware-data-output) (assert old-value (str "Probably wrong that previously set " tag " to nil")))
                  ;; Note that in some cases name and old-value will be maps while value will be a raw value
                  _ (when debug? (println "old value of" name "is" old-value "and want to set to" value))
                  _ (when debug?
                      (println (str "Type from: <" alias-altered-tag "> is: <" type-kw ">, old val: <" old-value ">, value: <" value ">")))
                  retrieve-tag-value (fn [tag]
                                       (let [altered-tag (alter-tag aliases tag)
                                             [_ old-value] (tag-retriever-lof altered-tag)]
                                         old-value))
                  ]
              (cond
                (= type-kw :add-on-context)
                (let [
                      ;_ (println "Type from" alias-altered-tag "is:" type-kw ", old val: <" old-value ">, value: <" value ">")
                      {:keys [context/output-params-at context/locals-at]} context
                      output-param-context (output-params-at tag)]
                  (if output-param-context
                    (let [{:keys [arg]} output-param-context
                          bound-arg arg
                          ;;
                          ;; Important to see that output mapping is working
                          ;;
                          ;_ (println "bound-arg: <" bound-arg "> from tag: <" tag ">")
                          ]
                      ;(assert false "Will not happen because outputs are changed into what they represent by tag-retriever")
                      (setters/maybe-set-different infos bound-arg value old-value type-kw count-in-array custom-grouped)
                      )
                    (let [{:keys [local/type local/getter local/setter] :as local-context} (locals-at tag)
                          current-val (getter tag)
                          _ (assert (= (:val/val old-value) current-val) (str "Can't improve by not using either tag retriever or getter, old value is: <" old-value ">"))
                          _ (assert true (str "For: <" tag ">, to change from: <" current-val "> to <" value ">"))]
                      (setter tag value))))

                (= type-kw :hardware-data-output)
                (let [funcs (-> infos :controller-info :info/output-data-functions)
                      _ (assert (u/is? funcs) (str ":info/output-data-functions not found in: " (keys infos)))
                      slot (u/string->int (second (str/split alias-altered-tag #":")))
                      f (get funcs slot)
                      _ (assert (fn? f) (str "Function not found in slot: <" slot ">, looking thru: <" (count funcs) "> funcs"))]
                  (f value))

                (:instruction/name value)
                (setters/set-instruction alias-altered-tag value old-value)

                (u/has-key? alias-altered-tag :unwrap/complex-tag-type)
                (setters/set-complex-tag infos retrieve-tag-value alias-altered-tag value old-value)

                :default
                (setters/maybe-set-different infos alias-altered-tag value old-value type-kw count-in-array custom-grouped))))))))))
