(ns tag.read
  (:require [running :as run]
            [datatypes :as data]
            [utils :as u]
            [clojure.string :as str]
            [instructions :as i]
            [tag.common :as common]
            [convert :as conv]
            [context :as con]
            [routine :as rou]
            [meta :as meta]))

(defn- ret-val-type [x x-type debug-desc]
  ;(println msg "DISP:" x)
  (assert (u/not-blank? x))
  (assert (or (sequential? x) (run/value-value? x)) (str "Not got proper type, got: <" x "> for <" debug-desc ">"))
  [x x-type])

(defn find-type [containing-type meta-data]
  (fn [v]
    ;(println (str "Use " v " to search in " meta-data " where top type is " containing-type))
    (let [res (if (and (= 1 (count v)) (string? (first meta-data)))
                (nth meta-data (first v))
                (let [
                      ;_ (println (str "Try into " meta-data " with " v))
                      ;; The extra wrapping of the [] around meta-data was surprising. It shows that
                      ;; the meta-data is one level further down than I thought - hopefully its correct
                      typ-at-idx (get-in [meta-data] v)]
                  typ-at-idx))
          ;; Not a problem for nothing to be returned. There will be no metadata when asking for say just
          ;; SCADA_R_Smartgas_Sample, even although we have collected [0] at Carbon_Dioxide b/c Carbon_Dioxide
          ;; comes first. We only need to use meta-data (and only have it) when specifically asking for
          ;; SCADA_R_Smartgas_Sample.Carbon_Dioxide, which will be dynamically. i.e. we don't store the exact
          ;; correct types, b/c we are storing the array as a whole.
          ;_ (assert res (str "Got nil when used " v " to search in meta-data <" meta-data "> where top type was " containing-type (con/out-context)))
          ]
      res)))

(defn dotty-to-seq [dotty-name]
  (let [res (str/split dotty-name #"\.")
        ;_ (println "in, out" dotty-name res)
        ]
    res))

(defn get-def [name tag-defs top-of-dotty-name]
  (assert (seq tag-defs) (str "No tag definitions"))
  (let [res (first (filter #(= (or top-of-dotty-name name) (-> % meta/tag-by-name-q)) tag-defs))
        _ (assert res (let [names (mapv #(-> % meta/tag-by-name-q) tag-defs)]
                        (str "No def found with name: <" name "> or <" top-of-dotty-name "> from " (count tag-defs) " tag definitions" (con/out-context) "names: " names)))]
    res))

(defn built-in? [reason-type]
  (some #{:built-in :built-in-single-idx-array-dot} [reason-type]))

;;
;; Values returned from here s/be map with {:val/val <>} in it
;; If array will be an array of these maps
;;
(defn- cached-value-from-name!
  "Anything that's not a whole array is cached - that's the side effect. Notice that simple custom types that
  are not arrays are also cached at lowest level of detail i.e. that includes the dot"
  ([infos name reason-type]
   (cached-value-from-name! infos name reason-type nil))
  ([infos name reason-type desc]
   (let [_ (assert (keyword? reason-type))
         _ (assert name (str "No name for " reason-type))
         _ (assert (string? name) (str "Not string? name for " reason-type ". name is: <" name ">"))
         quick-got-key (some #(when (= (:tag/name %) name) %) (keys @run/tags-repo))
         quick-gotten (get @run/tags-repo quick-got-key)
         ;_ (println "quick-got from" name "is" quick-got-key ", " quick-gotten)
         _ (when quick-got-key
             (assert quick-gotten))
         ]
     (if quick-got-key
       (ret-val-type quick-gotten (:tag/type quick-got-key) name)
       (let [{:keys [controller-info program-info]} infos
             {:keys [controller-tag-defs datatypes]} controller-info
             {:keys [program/defs]} program-info
             _ (assert (pos? (count controller-tag-defs)))
             ;_ (println (u/pp-str datatypes))
             tag-defs (concat defs controller-tag-defs)
             dot-at (str/index-of name ".")
             top-of-dotty-name (when dot-at (subs name 0 dot-at))
             ;_ (println "top-of-dotty-name:" top-of-dotty-name)
             def (get-def name tag-defs top-of-dotty-name)
             dotty-name (dotty-to-seq name)
             [head & trailing] dotty-name
             top-def (first (filter #(= head (u/probe-off (-> % meta/tag-by-name-q))) tag-defs))
             ;_ (println "top-def:" top-def)
             containing-type (meta/top-type top-def)
             ;meta-data (when (seq trailing) (data/number-seek datatypes containing-type trailing false))
             meta-data (meta/seek-meta infos head trailing)

             ;; Do not get meta-data when the containing-type is all that need, when just have a simple array
             ;;_ (assert meta-data (str "Got no meta-data from <" trailing ">, containing type: <" containing-type ">"))
             find-type-fn (find-type containing-type meta-data)
             ]
         (if dot-at
           (let [
                 ;_ (println (str "About to try with " dotty-name))
                 after-last-dot (last dotty-name)
                 ending (when (integer? (u/string->int-not-strict after-last-dot)) after-last-dot)
                 [dotty-value dotty-value-type-kw] (run/dotted-tag-initial-value tag-defs datatypes ending dotty-name reason-type def meta-data find-type-fn)
                 ]
             (let [_ (common/get-swap! name "simple dotty (no array)" dotty-value (fn [_] dotty-value-type-kw) nil)]
               (ret-val-type dotty-value _ (str "dotty-name: " name))))
           (if-let [;; Also checks that ends with . `an integer`
                    [ending dot-var] nil #_(run/alias-for def)]
             (let [_ (assert (pos? (count datatypes)))
                   ;[alias-value alias-value-type] (run/dotted-tag-initial-value tag-defs datatypes ending dot-var reason-type def)
                   _ (println ending dot-var)
                   _ (assert (not (u/string->int-not-strict ending)) (str "Not handling in here if ends with a number (like built ins no longer handled in here): <" ending ">"))
                   ;_ (setters/get-swap! name "alias value" alias-value (fn [_] alias-value-type) (select-keys infos [:observe/built-in-property]))
                   ;_ (println "Saved alias:" name ", given it ladder logic initialization value of" alias-value)
                   ]
               ;; Anytime there's an alias we are going to recurse into what its an alias for, however we want to recurse
               ;; earlier, so need to recognise an alias (and a number on the end) in calling method `tag-retriever` rather than here.
               ;; So this line of code will be used up there:
               ;(cached-value-from-name! infos (apply str (interpose "." (conj (vec dot-var) ending))) reason-type "alias recuring")
               ;; , and we don't want to back what was originally here, as alias's not being handled this low down:
               (assert false "No longer handling aliases here")
               )
             (let [[value val-type] (run/def->value find-type-fn def reason-type)
                   ;_ (println (str "INTERESTING: " value " " val-type))
                   _ (assert (not (nil? value)) (str "Got a nil initial value from " name))
                   val (if (not (sequential? value))
                         {:val/val (conv/val-set value) :val/type val-type}
                         value)
                   ;_ (assert (acceptable-built-in? val-type) (str "value type is supposed to be kw, instead is: <" val-type ">"))
                   ;_ (println "reason-type: " reason-type)
                   _ (u/assrt (or (not (vector? val)) (seq val)) (str "\nTrying to store an empty array for: <" name ">" (con/out-context)))
                   _ (common/get-swap! name (or desc "normal value") val (fn [_] val-type)
                                        (assoc (select-keys infos [:observe/built-in-property])
                                          :observe/built-in-kw (when (built-in? reason-type)
                                                                 val-type)))
                   ]
               (ret-val-type val val-type (str "value of: " name))))))))))

(defn get-index-value! [info index kw]
  (if (run/raw-int? index)
    {:val/val (u/string->int index)}
    (first (cached-value-from-name! info index kw))))

(defn get-name-type [context custom-grouped aliases tag debug?]
  (assert (pos? (count custom-grouped)))
  (cond
    (:context/input-params-at context) :add-on-context
    (map? tag) (do
                 (assert (u/has-key? tag :unwrap/complex-tag-type) (str "Does not have :unwrap/complex-tag-type: <" (keys tag) ">"))
                 (:unwrap/complex-tag-type tag))
    (data/data-input? tag) :hardware-data-input
    (data/data-output? tag) :hardware-data-output
    (run/raw-int? tag) :raw-int-value
    (run/raw-number? tag) :raw-number-value
    (run/raw-boolean? tag) :raw-boolean-value
    (data/channel-input? tag) :hardware-chan-input
    (data/channel-underrange-input? tag) :hardware-underrange-chan-input
    (data/channel-overrange-input? tag) :hardware-overrange-chan-input
    (run/number-ending? tag) :number-ending
    (some (into #{} (map :alias-name aliases)) [tag]) :alias
    (run/dot-into-datatype? custom-grouped tag debug?) :dot-into-datatype
    :default :name-value))

;;
;; A raw array can be of timers or counters
;;
(defn- value-from-raw-array
  [array idx]
  (let [index (:val/val idx)                                ;(run/string->int idx)
        ;; For some reason this not as a sequence, which is wrong
        res (nth array index)
        ;_ (println "value-from-raw-array RES:" res)
        ;_ (assert (run/value-value? res) (str "Not found proper value, instead: <" res ">" (con/out-context)))
        ]
    (if (run/value-value? res)
      ;;
      ;;(println "ARR:" array "IDX:" index "res: " (nth array index))
      ;; We will get a bug with first at some point
      {:temp/res         (:val/val res)
       :temp/type        (:val/type res)
       :temp/index-value index}
      ;;
      ;; Here each is an array, for instance a counter or a timer
      ;;
      {:temp/res res
       :temp/index-value index
       }
      )
    ))

(defn- value-from-attribute-array
  [array idx]
  (let [index (if (string? idx) (u/string->int idx) idx)
        res (nth array index)
        _ (assert (run/value-value? res) (str "Expected map with :val/val but got: <" res ">" (con/out-context)))
        ;_ (println "value-from-attribute-array RES:" res)
        ]
    ;;
    ;;(println "ARR:" array "IDX:" index "res: " (nth array index))
    ;; We will get a bug with first at some point
    {:tag/res         (:val/val res)
     :tag/index-value index}
    ))

(defn acceptable-built-in? [kw]
  (or (keyword? kw)
      (= "DINT" kw)
      (= "UNKNOWN" kw)
      (= "BOOL" kw)))

;;
;; Closes over info and returns our generic getter, that will get the value of any tag, that is
;; used by any instruction.
;; tag arg might not be a name, but already a value, or a map if dynamic access will be required
;; The tag is straight from the instruction are running
;; i.e. When a tag name is a map it has been made out of thin air - and this happens every time - :dynam
;;
(defn tag-retriever
  ([infos]
   (let [
         _ (assert (-> infos :controller-info :info/input-chan-functions))
         _ (assert (-> infos :controller-info :info/input-underrange-chan-functions))
         _ (assert (-> infos :controller-info :datatypes count pos?) (str "tag-retriever s/be given infos that have :datatypes: " (keys (:controller-info infos))))
         _ (assert (-> infos :controller-info :controller-tag-defs count pos?) "tag-retriever s/be given infos that have :controller-tag-defs")
         custom-grouped (common/get-custom-grouped-tags infos)
         aliases (-> infos :program-info :program/aliases)
         ]
     (tag-retriever infos custom-grouped aliases)))
  ([infos custom-grouped aliases]
   (fn [context]
     (fn [tag]
       (let [name-type (get-name-type context custom-grouped aliases tag false)
             ;_ (println "--*> name-type from <" tag "> in tag-retriever: <" name-type ">")
             info infos
             ;_ (println "KEYS: " (keys infos))
             ;_ (println "CONTEXT (tag-retriever): " context ", tag: " tag)
             _ (when (and false (str/starts-with? tag "HMI_R_PLC")) (println "tag to retrieve: " tag ", has name-type: " name-type))
             ;_ (println "NAMES: " datatype-names)
             _ (assert (not (and (= "VAL" tag) (nil? context))) (str "S/awlays have context when VAL"))
             ]
         (case name-type
           :alias (let [alias-for (:alias-for (first (filter #(= tag (:alias-name %)) aliases)))]
                    (((tag-retriever infos custom-grouped aliases) context) (data/wrap-arg alias-for)))
           :add-on-context (let [{:keys [context/input-params-at context/output-params-at context/locals-at]} context
                                 input-param-context (input-params-at tag)]
                             (if input-param-context
                               (let [{:keys [arg]} input-param-context]
                                 (((tag-retriever infos custom-grouped aliases) nil) (data/wrap-arg arg))) ;; intentional nil here
                               (let [local-context (locals-at tag)]
                                 (if local-context
                                   (let [{:keys [local/type local/getter]} local-context
                                         _ (assert getter (str "Need find bound value for: <" tag ">, got: <" local-context ">"))
                                         retrieved (getter tag)
                                         _ (assert retrieved (str "Retrieved nothing from <" tag ">"))]
                                     [:add-on-context {:val/val  retrieved
                                                       :val/type type}])
                                   (let [output-param-context (output-params-at tag)]
                                     (if output-param-context
                                       (let [{:keys [arg]} output-param-context
                                             ;_ (println (str "output arg to write: <" arg ">"))
                                             without-context (((tag-retriever infos custom-grouped aliases) nil) (data/wrap-arg arg))
                                             ;_ (println "Dropping: " (first without-context))
                                             ]
                                         [:add-on-context (second without-context)])))))))
           :raw-int-value [:raw-int-value {:val/val (conv/instr-val-set (u/string->int tag)) :val/type "DINT"}]
           :raw-boolean-value [:raw-boolean-value {:val/val (conv/instr-val-set tag) :val/type "BOOL"}]
           :raw-number-value [:raw-number-value {:val/val (conv/instr-val-set (-> tag read-string bigdec)) :val/type :eng}]
           :raw-array (let [{:keys [unwrap/array unwrap/index]} tag
                            array-value (first (cached-value-from-name! infos array :raw-array "a raw array"))
                            index-value (get-index-value! infos index :raw-array)
                            ;_ (println "IDX: " index-value)
                            {:keys [temp/res temp/index-value temp/type]} (value-from-raw-array array-value index-value)
                            ;_ (println (str "raw: " res))
                            ]
                        (if (vector? res)
                          [:raw-array
                           res]
                          [:raw-array
                           {:val/val  res
                            :val/type type}]))
           :dot-into-datatype (let [[array & tail] (str/split tag #"\.")
                                    array-value (first (cached-value-from-name! infos array :dot-into-datatype "custom dots"))
                                    res (meta/value-from-custom info
                                                           {:unwrap/array array :unwrap/dot-member tail}
                                                           array-value
                                                           (partial meta/find-containing-type))
                                    {:keys [dynam/dynamic-value dynam/value-type]} res
                                    ]
                                (if (sequential? dynamic-value)
                                  [:dot-into-datatype dynamic-value]
                                  [:dot-into-datatype
                                   {:val/val  (conv/val-set dynamic-value)
                                    :val/type value-type}]))
           :idx-within-attribute (let [{:keys [unwrap/array unwrap/index unwrap/dot-member]} tag
                                       index-value (get-index-value! infos index :idx-within-attribute)
                                       ;containing-type (data/find-containing-type infos array)
                                       ;real-meta-data (data/number-seek (-> infos :controller-info :datatypes) containing-type [(first (str/split dot-member #"\["))] false)
                                       real-meta-data (meta/seek-meta infos array [(first (str/split dot-member #"\["))])
                                       member-idx (-> real-meta-data first :seek/idx)
                                       second-idx (:val/val index-value)
                                       whole-array (first (cached-value-from-name! infos array :idx-within-attribute "an array"))
                                       array-value (nth whole-array member-idx)
                                       res (value-from-attribute-array array-value second-idx)]
                                   [:idx-within-attribute
                                    {:val/val (conv/val-set (:tag/res res))}])
           :single-idx-array-dot (let [{:keys [unwrap/array unwrap/index]} tag
                                       array-value (first (cached-value-from-name! infos array :single-idx-array-dot "an array"))
                                       index-value (get-index-value! infos index :single-idx-array-dot)
                                       ;_ (println "IDX: " index-value)
                                       res (meta/value-from-array-of-custom info tag array-value index-value)
                                       {:keys [dynam/dynamic-value dynam/value-type]} res]
                                   [:single-idx-array-dot
                                    {:val/val  (conv/val-set dynamic-value)
                                     :val/type value-type}])
           :single-idx-array-dot-array (let [{:keys [unwrap/array unwrap/index unwrap/second-index]} tag
                                             array-value (first (cached-value-from-name! infos array :single-idx-array-dot-array "an array"))
                                             index-value (get-index-value! infos index :single-idx-array-dot-array)
                                             second-index-value (int (:val/val (get-index-value! infos second-index :single-idx-array-dot-array)))
                                             sub-array (meta/value-from-array-of-custom info tag array-value index-value)
                                             ;_ (println (str "sub-array: " (keys sub-array)))
                                             {:keys [dynam/dynamic-value dynam/value-type]} sub-array
                                             res (nth dynamic-value second-index-value)
                                             ;_ (assert true (str res " at " second-index-value))
                                             ]
                                         [:single-idx-array-dot-array
                                          {:val/val  (conv/val-set (common/character->integer res))
                                           :val/type :unknown}])
           :number-ending (let [{:keys [unwrap/array unwrap/middle unwrap/dot-member]} tag
                                _ (assert array (str "No array found in: <" tag ">"))
                                array-value (first (cached-value-from-name! infos array :number-ending "number ending array"))
                                res (meta/value-from-number-ended info tag array-value (partial meta/find-containing-type))
                                {:keys [dynam/dynamic-value]} res
                                _ (assert (number? dynamic-value) (str "Expect to be 0 or 1 but got: <" dynamic-value ">"))
                                ]
                            [:number-ending
                             {:val/val (conv/val-set dynamic-value)}])
           :built-in (let [{:keys [unwrap/array unwrap/dot-member unwrap/dot-member-idx]} tag
                           _ (assert (nil? dot-member-idx)
                                     "Supposed to be nil, because by convention the hard work of looking into types is done here")
                           [array-value built-in-type-kw] (cached-value-from-name! (assoc infos :observe/built-in-property dot-member)
                                                                                   array :built-in "built in array")
                           ;_ (println (str "array-value: " array-value))
                           ;_ (println "built-in-type is" built-in-type-kw)
                           ;_ (println "dot-member is" dot-member)
                           _ (assert built-in-type-kw (str "Can't find type of " array " when " tag))
                           _ (assert (keyword? built-in-type-kw) (str "built-in-type is not kw but " (type built-in-type-kw) ": " built-in-type-kw (con/out-context)))
                           indexing-fn (as-> rou/built-ins $
                                             (get $ built-in-type-kw)
                                             (:indexing-fn $)
                                             (get $ dot-member))
                           _ (assert indexing-fn (str "Can't find indexing-fn for " built-in-type-kw ", needed for " dot-member (con/out-context)))
                           res (indexing-fn array-value)
                           {:keys [dynam/dynamic-value]} res
                           ]
                       [:built-in
                        {:val/val  (conv/val-set dynamic-value)
                         :val/type built-in-type-kw}])
           :built-in-single-idx-array-dot (let [{:keys [unwrap/array unwrap/dot-member unwrap/dot-member-idx unwrap/index]} tag
                                                [array-value _] (cached-value-from-name! (assoc infos :observe/built-in-property dot-member
                                                                                                      :observe/built-in-kw nil)
                                                                                         array :built-in-single-idx-array-dot "built in array LEN")
                                                in-parent-index (u/string->int index)
                                                indexed-value (nth array-value in-parent-index)
                                                {:keys [val/val val/type]} indexed-value
                                                ;_ (println (str "index: " index))
                                                ;_ (println (str "indexed-value: " indexed-value))
                                                ;_ (println "built-in-type is" type)
                                                indexing-fn (as-> rou/built-ins $
                                                                  (get $ type)
                                                                  (:indexing-fn $)
                                                                  (get $ dot-member))
                                                _ (assert indexing-fn (str "Can't find indexing-fn for " type ", needed for " dot-member (con/out-context)))
                                                res (indexing-fn indexed-value)
                                                ;_ (println "res: <" res ">")
                                                {:keys [dynam/dynamic-value dynam/dynamic-type]} res
                                                _ (assert type (str "Can't find type of " array " when " tag))]
                                            [:built-in-single-idx-array-dot
                                             {:val/val  (conv/val-set dynamic-value)
                                              :val/type dynamic-type
                                              :dynam/built-in-parent-type type
                                              :dynam/within-parent-index in-parent-index}])
           :name-value [:name-value
                        (first (cached-value-from-name! infos tag :name-value))]
           :hardware-chan-input (let [funcs (-> infos :controller-info :info/input-chan-functions)
                                      _ (assert (u/is? funcs) (str "Not from: <" (keys (:controller-info infos)) ">"))
                                      ;_ (println "tag: " tag)
                                      slot (u/string->int (second (str/split tag #":")))
                                      f (get funcs slot)
                                      _ (assert (fn? f) (str "No input channel input for slot: <" slot ">"))
                                      ch-number (partial u/between "Ch" "Data")
                                      ch-num (-> (str/split tag #"\.") second ch-number u/string->int)
                                      ;_ (assert false (str "Need choose a function from " funcs " for slot " slot))
                                      ]
                                  [:hardware-chan-input (f ch-num)])
           :hardware-underrange-chan-input (let [funcs (-> infos :controller-info :info/input-underrange-chan-functions)
                                                 _ (assert (u/is? funcs) (str "Not from: " (keys infos)))
                                                 slot (u/string->int (second (str/split tag #":")))
                                                 f (get funcs slot)
                                                 _ (assert (fn? f) (str "No underrange input channel input for slot: <" slot ">"))
                                                 ch-number (partial u/between "Ch" "Underrange")
                                                 ch-num (-> (str/split tag #"\.") second ch-number u/string->int)

                                                 ;_ (assert false (str "Need choose a function from " funcs " for slot " slot))
                                                 ]
                                             [:hardware-underrange-chan-input (f ch-num)])
           :hardware-overrange-chan-input (let [funcs (-> infos :controller-info :info/input-overrange-chan-functions)
                                                _ (assert (u/is? funcs) (str "Not from: " (keys (:controller-info infos))))
                                                slot (u/string->int (second (str/split tag #":")))
                                                f (get funcs slot)
                                                _ (assert (fn? f) (str "No overrange input channel input for slot: <" slot ">"))
                                                ch-number (partial u/between "Ch" "Overrange")
                                                ch-num (-> (str/split tag #"\.") second ch-number u/string->int)

                                                ;_ (assert false (str "Need choose a function from " funcs " for slot " slot))
                                                ]
                                            [:hardware-overrange-chan-input (f ch-num)])
           :hardware-data-input (let [funcs (-> infos :controller-info :info/input-data-functions)
                                      _ (assert (u/is? funcs) (str "Not from: " (keys (:controller-info infos))))
                                      slot (u/string->int (second (str/split tag #":")))
                                      f (get funcs slot)
                                      _ (assert (fn? f))
                                      ;_ (assert false (str "Need choose a function from " funcs " for slot " slot))
                                      ]
                                  [:hardware-data-input (f)])
           ;;
           ;; Even when writing to sumfin we read first. However for hardware we don't keep the value at all - that's
           ;; the job of external hardware. Not true - outputs can be used for input as well, and as part of this there
           ;; are instructions that for example move from an output (i.e. treating it like an input). For this reason
           ;; outputs will have input functions.
           ;;
           :hardware-data-output (let [funcs (-> infos :controller-info :info/output-input-data-functions)
                                       _ (assert (u/is? funcs) (str "Not from: " (keys (:controller-info infos))))
                                       slot (u/string->int (second (str/split tag #":")))
                                       f (get funcs slot)
                                       _ (assert (fn? f) (str "Not got an input function for output slot <" slot ">, where have them for slots: " (keys funcs)))]
                                   [:hardware-data-output (f)])))))))

;;
;; Used when retrieving to then set values in the cache. So more information is returned, and does not have to
;; be in format where keyword starts with :tag - which is required for when executing instructions
;;
(defn internal-tag-retriever
  ([infos]
   (let [
         _ (assert (-> infos :controller-info :datatypes count pos?) "tag-retriever s/be given infos that have :datatypes")
         custom-grouped (common/get-custom-grouped-tags infos)
         aliases (-> infos :program-info :program/aliases)]
     (internal-tag-retriever infos custom-grouped aliases)))
  ([infos datatype-names aliases]
   (internal-tag-retriever infos datatype-names aliases false))
  ([infos datatype-names aliases debug?]
   (fn [context]
     (fn [tag]
       (let [
             ;;_ (println "CONTEXT (internal-tag-retriever): " context)
             name-type (get-name-type context datatype-names aliases tag debug?)
             _ (when debug? (println "--*> name-type from <" tag "> in internal-tag-retriever: <" name-type ">"))
             ]
         (case name-type
           :single-idx-array-dot (let [{:keys [unwrap/array unwrap/index]} tag
                                       array-value (first (cached-value-from-name! infos array :single-idx-array-dot "an array"))
                                       index-value (get-index-value! infos index :single-idx-array-dot)
                                       ;_ (println "IDX: " index-value)
                                       res (meta/value-from-array-of-custom infos tag array-value index-value)]
                                   [:single-idx-array-dot res])
           :number-ending (let [{:keys [unwrap/array unwrap/middle unwrap/dot-member]} tag
                                _ (assert array (str "No array found in: <" tag ">"))
                                array-value (first (cached-value-from-name! infos array :number-ending "number ending array"))
                                res (meta/value-from-number-ended infos tag array-value (partial meta/find-containing-type))
                                {:keys [dynam/dynamic-value]} res
                                _ (assert (number? dynamic-value) (str "Expect to be 0 or 1 but got: <" dynamic-value ">"))
                                ]
                            [:number-ending res])
           (((tag-retriever infos datatype-names aliases) context) tag)))))))
