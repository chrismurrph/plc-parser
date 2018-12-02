(ns tag.setters
  (:require [running :as run]
            [clojure.string :as str]
            [utils :as u]
            [routine :as rou]
            [debug :as deb]
            [domain-constants :as constants]
            [convert :as conv]
            [context :as con]
            [tag.common :as common]))

(defn grab-existing-key [array]
  (u/probe-off (some #(when (= (:tag/name %) array) %) (keys @run/tags-repo))))

;;
;; Need to know that first place is ACC, second is PRE, third unknown, and we are putting last-time-ton at
;; fourth position. Only if there's a value at last-time-ton can we move the timer on by the increment we
;; calculate here
;;
(defn ton-reset [old-value]
  (let [last-time (get-in old-value [constants/TIMER-RECORDED-idx :val/val])
        this-time (u/elapsed)
        with-recorded-value (assoc-in old-value [constants/TIMER-RECORDED-idx :val/val] this-time)
        ]
    (if (not last-time)
      with-recorded-value
      (let [
            ;; Have a difference so can set a value for ACC
            increment (- this-time last-time)
            current-acc (get-in old-value [constants/TIMER-ACC-idx :val/val])
            with-acc-and-recorded (assoc-in with-recorded-value [constants/TIMER-ACC-idx :val/val] (+ current-acc increment))
            ]
        (assert true (str "2nd time visit, can change now to <" with-acc-and-recorded ">, from <" old-value ">"))
        with-acc-and-recorded))))

;; First is acc in counter
(defn ctu-reset [old-value]
  (update-in old-value [constants/TIMER-ACC-idx :val/val] inc))

;;
;; How update within when we store it as an array
;; (k)ey
;; (meta) data (the vector that `assoc-in` always has)
;; (x)alue
;;
(defn local-assoc-in! [k meta x]
  (assert (map? k))
  (assert meta)
  (assert (map? meta))
  ;(println (str "local-assoc-in! using meta: " (:observe/meta meta) " for " k " for " x (con/out-context)))
  (deb/observe-key k x meta)
  (swap! run/tags-repo update k assoc-in (:observe/meta meta) x))

(defn set-complex-tag [infos retrieve-tag-value tag value old-value]
  (let [name-type (:unwrap/complex-tag-type tag)]
    (case name-type
      :raw-array (let [
                       {:keys [unwrap/array unwrap/index unwrap/type]} tag
                       _ (assert index (str "Expect always have an idx"))
                       ;;
                       ;; Raw arrays are presumably all of one type so no need to record the type per array
                       ;; element.
                       ;;
                       {:keys [val/type val/val]} old-value
                       built-in-type? (vector? val)
                       _ (assert (or built-in-type? type) (str "Expect have a type: " tag))
                       simple-value (if (and (map? value) #_(some #{:val/val} (keys value)))
                                      (:val/val value)
                                      value)                ;; Here instruction directly setting a value
                       _ (assert (u/is? simple-value))
                       converted-value (run/convert type simple-value)
                       index-value (if (number? index)
                                     (assert false "Doubt ever happens")
                                     (:val/val (retrieve-tag-value index)))
                       meta-idx [index-value :val/val]
                       ]
                   (when (not= converted-value (:val/val old-value))
                     (local-assoc-in! (grab-existing-key array)
                                      {:observe/meta meta-idx :observe/cardinality :single}
                                      (conv/val-set converted-value))))
      :number-ending (let [{:keys [unwrap/array unwrap/middle unwrap/dot-member]} tag
                           existing-key (grab-existing-key array)
                           ;_ (println "old-value: " old-value ", existing-key: " existing-key)
                           _ (assert (map? old-value))
                           _ (assert (some #{:dynam/dynamic-value} (keys old-value)) (str "Expecting keys that lead with :dynam, got: <" old-value ">"))
                           {:keys [dynam/dynamic-value dynam/array-value dynam/value-type dynam/meta-idx dynam/meta-idx-res dynam/dot-member]} old-value
                           ;_ (println "meta-idx: " meta-idx)
                           simple-value (if (and (map? value))
                                          (:val/val value)
                                          ;; Here instruction directly setting a value (in which case will be boolean and need to be changed to a bit)
                                          value)
                           converted-value (if (boolean? simple-value)
                                             (if simple-value 1 0)
                                             simple-value)
                           ;;
                           ;; meta-idx-res (old value) is for example 64. We need to change the bit at dot-member to
                           ;; converted-value. `meta-idx` only goes up to 64. Quite a process:
                           ;; 1. old-value-as-bits
                           ;; 2. alter the bit at dot-member to converted-value (its a bit)
                           ;; 3. convert this array of values to a number again - s/be slightly different to 64
                           ;;
                           sz (conv/byte-size value-type)
                           old-value-as-bits (conv/dec->bit-array sz meta-idx-res)
                           ;left-pad #(u/left-pad % 0 dot-member)
                           ;_ (println (str "Want to put " converted-value " at " dot-member " in " old-value-as-bits))
                           altered-as-bits (reverse (assoc (-> old-value-as-bits reverse vec) dot-member converted-value))
                           as-decimal (conv/bit-array->dec altered-as-bits)
                           ;_ (println (str "here: " old-value-as-bits ", to: " altered-as-bits ", b/c: " converted-value " s/be at " dot-member ", gives: " as-decimal))
                           ;_ (println "CF: " meta-idx-res ", " as-decimal)
                           ]
                       (when (not= meta-idx-res as-decimal)
                         (local-assoc-in! existing-key
                                          {:observe/meta meta-idx :observe/cardinality :single}
                                          {:val/val (conv/val-set as-decimal) :val/type value-type})))
      :single-idx-array-dot (let [;; We want to update the particular value of an array we already have in the cache. Or in other words
                                  ;; we don't want to put {:unwrap/array SCADA_R_Readings, :unwrap/index SCADA_R_Official_Tube_Num, :unwrap/dot-member Ethane}
                                  ;; in the cache as the name.
                                  ;; value is also a map because it doesn't just contain the name, it also contains meta information
                                  ;; about how to update the value for SCADA_R_Readings (in this case).
                                  _ (assert (map? old-value))
                                  {:keys [dynam/value-type dynam/meta-idx]} old-value
                                  ;_ (println "OLD:" old-value " of <" tag ">")
                                  _ (assert (some #{:dynam/dynamic-value} (keys old-value)) "Expecting keys that lead with :dynam")
                                  {:keys [unwrap/array unwrap/index unwrap/dot-member]} tag
                                  ;_ (println (str "About to put " value " into " array " at " meta-idx))
                                  ;_ (println (str "Existing :single-idx-array-dot tag info for " array " is <" existing-key ">"))
                                  ;convert-type (:val/type (u/probe-on (get-in (get @run/tags-repo existing-key) meta-idx) "PROBE"))
                                  simple-value (if (and (map? value))
                                                 (:val/val value)
                                                 value)     ;; Here instruction directly setting a value
                                  _ (assert (u/is? simple-value))
                                  ;_ (println simple-value (type simple-value))
                                  ;_ (println (str ":single-idx-array-dot convert type: " value-type ", meta: <" meta-idx ">"))
                                  converted-value (run/convert value-type simple-value)
                                  ;_ (println (str "New value want is <" converted-value ">, at <" meta-idx ">. Existing value: <" (get @run/tags-repo existing-key) ">"))
                                  ]
                              (or (= :array-overrun converted-value)
                                  (local-assoc-in! (grab-existing-key array)
                                                   {:observe/meta meta-idx :observe/cardinality :single}
                                                   {:val/val (conv/val-set converted-value) :val/type value-type})))
      :idx-within-attribute (let [
                                  _ (assert (map? old-value))
                                  ;_ (println "OLD:" old-value)
                                  ;_ (println "TAG:" tag)
                                  ;_ (println "value:" value)
                                  index-value (-> tag :unwrap/index u/string->int)
                                  ;{:keys [tag/res tag/index-value]} old-value
                                  _ (assert index-value)
                                  _ (assert (integer? index-value) (type index-value))
                                  {:keys [unwrap/array unwrap/dot-member]} tag
                                  existing-key (grab-existing-key array)
                                  existing-type (:tag/type existing-key)
                                  real-meta-data (meta/number-seek (-> infos :controller-info :datatypes) existing-type [(first (str/split dot-member #"\["))] false)
                                  member-idx (-> real-meta-data first :seek/idx)
                                  meta-idx [member-idx index-value :val/val]
                                  ;_ (println "real-meta-data: " real-meta-data)
                                  repo-value (nth (nth (get @run/tags-repo existing-key) member-idx) index-value)
                                  _ (assert (:val/type repo-value) (str "No type found in: <" repo-value "> for <" tag ">"))
                                  convert-type (:val/type repo-value)
                                  converted-value (run/convert convert-type value)
                                  ;_ (println (str ":idx-within-attribute convert type: " convert-type))
                                  ;_ (println (str "Existing :idx-within-attribute tag info for " array " is <" existing-key ">."))
                                  ;_ (println (str "New value want is <" converted-value ">, at <" meta-idx ">. Existing array value: <" (get @run/tags-repo existing-key) ">"))
                                  ]
                              (or (= :array-overrun converted-value)
                                  (local-assoc-in! existing-key {:observe/meta        meta-idx
                                                                 :observe/cardinality :single} (conv/val-set converted-value))))
      :built-in (let [
                      {:keys [unwrap/array unwrap/dot-member]} tag
                      existing-key-res (some #(when (= (:tag/name %) array) %) (keys @run/tags-repo))
                      ;_ (println "key: " existing-key)
                      built-in-type-kw (:tag/type existing-key-res)
                      ;_ (assert false (str "TYPE: <" convert-type ">"))
                      ;; value is not the whole say COUNTER, but one of its members, so will be an integer.
                      ;built-in-type-kw (:val/type value)
                      ;_ (assert (keyword? built-in-type-kw) (str "Expecting kw but got <" built-in-type-kw "> from <" value ">, where tag: <" tag ">"))
                      ;;_ (println "Need get " dot-member " from " array ", where built in is " built-in-type-kw)
                      index-value (as-> rou/built-ins $
                                        (u/probe-off $ (str "starting, next: <" built-in-type-kw ">"))
                                        (get $ built-in-type-kw)
                                        (u/probe-off $ "from kw")
                                        (:index $)
                                        (u/probe-off $ "from indexing-fn")
                                        (get $ dot-member))
                      _ (assert index-value (str "No index-value. tag: " tag ", value: " value ", old-value: " old-value (con/out-context)))
                      meta-idx [index-value :val/val]
                      ]
                  (local-assoc-in! existing-key-res {:observe/meta              meta-idx
                                                     :observe/cardinality       :single
                                                     :observe/built-in-property dot-member} (:val/val value)))
      :built-in-single-idx-array-dot (let [{:keys [unwrap/array unwrap/dot-member]} tag
                                           existing-key (some #(when (= (:tag/name %) array) %) (keys @run/tags-repo))
                                           ;_ (println "key: " existing-key)
                                           _ (assert (map? old-value))
                                           {:keys [val/val dynam/built-in-parent-type dynam/within-parent-index]} old-value
                                           parent-value (:val/val (nth (get @run/tags-repo existing-key) within-parent-index))
                                           set-f (as-> rou/built-ins $
                                                       (u/probe-off $ (str "starting, next: <" built-in-parent-type ">"))
                                                       (get $ built-in-parent-type)
                                                       (u/probe-off $ "from kw")
                                                       (:set-fn $)
                                                       (get $ dot-member))
                                           new-val (set-f parent-value (:val/val value))
                                           ;new-parent (assoc (get @run/tags-repo existing-key) within-parent-index {:val/val new-val :val/type built-in-parent-type})
                                           ;_ (println (str "new-val: <" new-val ">"))
                                           ;_ (println (str "new-parent: <" new-parent ">"))
                                           ;_ (println (str "value need to write to a tag <" tag "> is: <" value ">, before: <" old-value ">" (con/out-context)))
                                           ;_ (assert false)
                                           ]
                                       (local-assoc-in! existing-key {:observe/meta              [within-parent-index]
                                                                      :observe/cardinality       :single
                                                                      :observe/built-in-property dot-member} {:val/val new-val :val/type built-in-parent-type})))))

#_(let [_ (assert (map? tag-name) (str "tag-name, expected map: " tag-name))
        {:keys [unwrap/index]} tag-name
        idx (u/string->int index)
        old-indexed-value (nth old-value idx)
        _ (assert old-indexed-value (str "No value from " old-value))]
    (if in-energy
      [(ton-reset old-indexed-value) old-indexed-value]
      [(assoc-in old-indexed-value [constants/TIMER-ACC-idx :val/val] 0) old-indexed-value]))

(defn set-instruction [tag-name value old-value]
  (case (:instruction/name value)
    :instru/res (let [
                      ;; old value looks something like this, so we just have to reset it (res function)
                      ;; [{:val/val 10, :val/type :int} {:val/val 5, :val/type :int} {:val/val 1, :val/type :int}]
                      ;; First is accumulated, which we set back to 0
                      reset-value (assoc-in old-value [constants/TIMER-ACC-idx :val/val] 0)
                      _ (when (= tag-name "S-witching_Timer")
                          (println (str "in res and acc set to: <" (get-in reset-value [constants/TIMER-ACC-idx :val/val]) ">")))
                      ]
                  (common/get-swap! tag-name "reset a counter or timer" reset-value nil {:observe/meta       [constants/TIMER-ACC-idx :val/val]
                                                                                  :observe/built-in-property "ACC"}))
    :instru/ton (let [
                      ;; if in-energy is false we set ACC (first position) to 0
                      ;; if true we need last-time-scanned to be set as last step.
                      in-energy (:instruction/in-energy value)
                      _ (assert (boolean? in-energy))
                      ;_ (println (str "old-value to change: " old-value))
                      reset-value (if in-energy
                                    (ton-reset old-value)
                                    (assoc-in old-value [constants/TIMER-ACC-idx :val/val] 0))
                      ;_ (assert false (str "CF: " reset-value " with " old-value " for " tag-name))
                      ]
                  (when (not= reset-value old-value)
                    (if (string? tag-name)
                      (do
                        (when (= tag-name "O-xygen_Purge")
                          (println (str "in ton for Oxygen_Purge and acc set to: <" (get-in reset-value [constants/TIMER-ACC-idx :val/val]) ">")))
                        (common/get-swap! tag-name "ton for timer" reset-value nil {:observe/meta              [constants/TIMER-ACC-idx :val/val]
                                                                             :observe/built-in-property "ACC"}))
                      (let [_ (assert false)]))))
    :instru/ctu (let [
                      ;; if in-energy is false ACC increment doesn't happen, so nothing happens
                      in-energy (:instruction/in-energy value)
                      _ (assert (boolean? in-energy))
                      reset-value (if in-energy
                                    (ctu-reset old-value)
                                    old-value)
                      ]
                  (common/get-swap! tag-name "ctu for counter" reset-value))))

(defn still-returned-map? [x]
  (and (map? x) (some #{:val/val} (keys x))))

(defn type-converter
  ([known-type tag old-value new-value type-kw]
   (type-converter known-type tag old-value new-value type-kw nil))
  ([known-type tag old-value new-value type-kw count]
   (assert (keyword? type-kw) (str "Before count s/come a kw, instead: <" type-kw ">"))
   (assert (not (still-returned-map? old-value)))
   (assert (not (still-returned-map? new-value)))
    ;;
    ;; Note that converter function returning here is how you would get from the new-value to the
    ;; old-value. The old-value keeps its type.
    ;;
   (cond
     (= (type old-value) (type new-value)) identity
     (= type-kw :hardware-data-output) identity
     (= known-type :string) str
     ;; No reason for any values to be nil
     ;;(nil? old-value) identity
     (and (string? new-value) (integer? old-value)) u/string->int
     (and (boolean? new-value) (= :int known-type)) rou/bool->integer
     ;; Won't need when known-type is made to be int, b/c above will kick in
     ;; (and (boolean? new-value) (integer? old-value)) rou/bool->integer
     (and (instance? Long new-value) (integer? old-value)) long
     (and (or (integer? new-value) (instance? Long new-value)) (= :eng known-type)) run/integer->eng
     (and (decimal? new-value) (or (integer? old-value) (instance? Long old-value))) long
     (and (decimal? old-value) (instance? Long new-value) (= "REAL" known-type)) bigdec
     (and count (sequential? old-value)) (run/array-convert old-value count)
     :default (u/assrt false (str "Can't convert from <" (type old-value) "> to <" (type new-value) ">, old: <" old-value
                                 ">, new: <" new-value "> for <" tag ">, known type: <" known-type ">" (con/out-context))))))

(defn maybe-set-different [infos tag value old-value type-kw count-in-array custom-grouped]
  (let [
        debug? (= tag "VAL")
        ;_ (when debug? (println "CONTEXT (tag-setter): " context))
        _ (when debug? (println (str "CF: " old-value " and " value ", types: " (type old-value) ", "
                                     (type value) ", count-in-array: <" count-in-array ">, type-kw: <" type-kw ">")))
        _ (assert (not debug?) (str "VAL s/not be used: <" tag ">"))
        raw-old-val (if (u/is? (:dynam/dynamic-value old-value))
                       (:dynam/dynamic-value old-value)
                       (cond
                         (u/is? (:val/val old-value))
                         (:val/val old-value)

                         (sequential? old-value)
                         old-value

                         :add-on-context
                         (:val/val old-value)

                         :default
                         (assert (= type-kw :hardware-data-output) (str "What's value in: <" old-value "> when type-kw: <" type-kw ">"))))

        real-new-val (if (and (not (map? value)) (run/raw-value? value)) value (:val/val value))
        ;_ (assert (= (:val/type value) (:val/type old-value)) (str "<" (:val/type value) "," (:val/type old-value) ">"))
        _ (assert (u/is? real-new-val) (str "No real-new-val obtained from: <" value "> of type: " (type value)))
        type-converter-fn (type-converter (:val/type old-value) tag raw-old-val real-new-val type-kw count-in-array)
        new-value (type-converter-fn real-new-val)
        ]
    (when (not= raw-old-val new-value)
      ;(println "old and new different old: <" real-old-val ">, new: <" new-value ">, orig from: " real-new-val)
      (if (run/dot-into-datatype? custom-grouped tag false)
        (let [[head & tail] (str/split tag #"\.")
              ;_ (println (str "Did need convert from " value " to " real-new-val " to " new-value))
              ;containing-type (data/find-containing-type infos head)
              ;meta-data (when (seq tail)
              ;            (data/number-seek (-> infos :controller-info :datatypes) containing-type tail false))
              meta-data (meta/seek-meta infos head tail)
              ;;;;;; meta-data nil
              ;;;;;; _ (assert false)
              ;;
              ;; [] (tag/up-to-byte-value (-> infos :info :datatypes) containing-type tail )
              ;;
              ;_ (println (str "meta-data is: <" meta-data ">"))
              ;_ (println (str "head, tail is: <" head "|" tail ">"))
              ]
          (case (count tail)
            0 (assert false (str "Need change from " raw-old-val " to " new-value " for " head))
            1 (if (sequential? new-value)
                (let [index-value (-> meta-data first :seek/idx)
                      ;_ (println "WARN: Assuming idx val 0 - need fix for: <" tag ">, where want to set from: <" real-old-val "> to <" new-value ">")
                      meta-idx [index-value]]
                  (local-assoc-in! (grab-existing-key head) {:observe/meta meta-idx :observe/cardinality :single} new-value))
                (let [_ (assert true (str "WARN: need fix for: <" tag ">, where want to set from: <" raw-old-val "> to <" new-value ">"))
                      bit-fill? (= (-> meta-data last :seek/type) :bit-fill)]
                  (if bit-fill?
                    (let [existing-key (some #(when (= (:tag/name %) head) %) (keys @run/tags-repo))
                          ;_ (println "Existing key: " existing-key)
                          ;_ (println "WARN: Need fix for: <" tag ">, where want to set from: <" real-old-val "> to <" new-value "> where bit-fill")
                          ;_ (println (str "First these: " meta-data))
                          [m-idx as-dec] (conv/effect-binary-change @run/tags-repo head meta-data new-value)
                          ]
                      (local-assoc-in! (grab-existing-key head) {:observe/meta m-idx :observe/cardinality :single} {:val/val (conv/val-set as-dec) :val/type (:val/type old-value)}))
                    (let [index-value (-> meta-data last :seek/idx)
                          meta-idx [index-value]]
                      (local-assoc-in! (grab-existing-key head) {:observe/meta meta-idx :observe/cardinality :single} {:val/val (conv/val-set new-value) :val/type (:val/type old-value)})))))
            2 (if-let [bit-fill? (and (= 3 (count meta-data)) (= (-> meta-data last :seek/type) :bit-fill))]
                (let [[m-idx as-dec] (conv/effect-binary-change @run/tags-repo head meta-data new-value)]
                  (local-assoc-in! (grab-existing-key head) {:observe/meta m-idx :observe/cardinality :single} {:val/val (conv/val-set as-dec) :val/type (:val/type old-value)}))
                (let [_ (assert (not (sequential? new-value)))
                      _ (assert true (str "WARN: need fix for: <" tag ">, where want to set from: <" raw-old-val "> to <"
                                          new-value ">, tail: <" tail ">" ", meta-data: <" meta-data ">"))
                      meta-idx (mapv :seek/idx meta-data)
                      fixed-new-val {:val/val (conv/val-set new-value) :val/type (:val/type old-value)}
                      ]
                  (local-assoc-in! (grab-existing-key head) {:observe/meta meta-idx :observe/cardinality :single} fixed-new-val)))

            (assert false (str "tail count is " (count tail) " for: " tag ", where want to set from: <" raw-old-val "> to <" new-value ">"))
            )
          )
        (common/get-swap! tag
                   "instruction has changed original value"
                   {:val/val (conv/val-set new-value) :val/type (:val/type old-value)}
                   (fn [_] (:val/type old-value))
                   nil)))))
