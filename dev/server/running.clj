(ns running
  (:require
    [utils :as u]
    [clojure.string :as str]
    [instructions :as i]
    [routine :as rou]
    [convert :as conv]
    [context :as con]
    [makes :as m]
    [domain-utils :as du]))

(def extras? true)
;;Not stored like this when there is no array involved
;;(def raw-sample-value [-0M -0M -0M -0M -0M -0M -0M -0M -0M])
(def default-tags {{:tag/name "S:FS" :tag/desc "default"} {:val/val 1 :val/type :unknown}})
(def tags-repo (atom default-tags))

(defn change-repo [old]
  (-> old
      ;(assoc {:tag/name "Raw_Sample.Carbon_Dioxide" :desc "extra"} {:val/val 0M :val/type :unknown})
      ;(assoc {:tag/name "SCADA_R_Smartgas_Sample.Carbon_Dioxide" :desc "extra"} {:val/val 0M :val/type :unknown})
      ;(assoc {:tag/name "PLC_TIME.uSec" :desc "extra"} {:val/val (int 99) :val/type :unknown})
      ;"SCADA_R_Analyser_Calibrating_Auto_Input"
      ;(assoc {:tag/name "SCADA_R_Analyser_Calibrating_Auto_Input" :tag/desc "extra"} {:val/val true :val/type :unknown})
      ))

(defn add-extras []
  (swap! tags-repo change-repo))

(defn reset-tags []
  (reset! tags-repo default-tags)
  (when extras?
    (add-extras)))

(defn integer->bool [x]
  ;(println "in:" x (type x))
  (assert (not (boolean? x)) "No point changing boolean into a boolean")
  (assert x "integer->bool not being given anything")
  (cond
    (= x 0) false
    (= x 1) true
    (= x "0") false
    (= x "1") true
    :default (do
               (assert false (str "integer->bool, unknown value: " x ", of type: " (type x)))
               false)))

(defn integer->eng [x]
  (du/big-dec "twelve" x))

(defn array-convert [dest-array size]
  (fn [element]
    (assert (sequential? dest-array))
    (assert (integer? size))
    ;(println (str "Yet to put " element " into " dest-array ", times: " size))
    (let [range-over (range (count dest-array))
          _ (assert (sequential? range-over))
          res (mapv (fn [idx]
                      {:val/val  (if (< idx size) element (:val/val (nth dest-array idx)))
                       :val/type (:val/type (nth dest-array idx))})
                    range-over)]
      res)))

(defn zero-one [se]
  (assert (= 2 (count se)))
  (let [[type-kw _] se]
    (case type-kw
      :int [:int "0"])))

(defn zeroify [example]
  (mapv zero-one example))

(defn pad-out-by [pad-num v]
  (assert (or (zero? pad-num) (pos? pad-num)))
  (if (zero? pad-num)
    v
    (let [example (first v)
          zeroed-example (zeroify example)
          padded (repeat pad-num zeroed-example)]
      (vec (concat v padded)))))

(defn get-array-dim [default def]
  (try
    (-> def (nth 2) (nth 2) second second u/string->int)
    (catch IndexOutOfBoundsException _
      default)))

(defn between-brackets-of-def
  ([se]
   (between-brackets-of-def se false))
  ([se debug?]
   (let [all-vectors (next se)
         ;_ (println "Between:" all-vectors)
         _ (assert (seq all-vectors) (str "Not much point if nothing: <" se ">"))
         start-gone (first (drop-while #(not= (first %) :br) all-vectors))
         _ (when debug? (println "start-gone:" start-gone))
         br (next start-gone)
         res (map-indexed (fn [idx val] [[idx] val]) br)
         _ (when debug? (println "between-brackets-of-def: <" res ">"))
         ]
     res)))

;;
;; We can't wrap here as can do when just a list being returned, as is case with between-brackets-of-def.
;; Here the data is a list of lists
;;
(defn between-multiple-brackets-of-def
  ([se]
   (between-multiple-brackets-of-def se false))
  ([se debug?]
   (let [all-vectors (next se)
         ;_ (println (str "Between: <" all-vectors ">"))
         start-gone (first (drop-while #(not= (first %) :br) all-vectors))
         ;_ (println (str "Go in again with: " start-gone))
         br (next start-gone)
         default-size (count br)
         dim (get-array-dim default-size se)
         ;_ (assert (= dim (count br)) (str "dim is: " dim ", while values: " (count br) " for " (-> se second second)))
         without-brs (mapv (comp vec next) br)
         padded-out (pad-out-by (- dim (count br)) without-brs)
         ;;
         ;; At the first level down in the tree now. Each of :typ/val is itself
         ;; a vector. :typ/nav will be a vector of size one, and :typ/val will
         ;; be a vector that will need to be broken down further. When it does
         ;; the vector of size 1 will become of size 2.
         ;;
         ;res (map-indexed (fn [idx val] {:typ/nav [idx] :typ/val val}) without-brs)
         ;;_ (assert false (str "without-brs: " without-brs))
         res (map-indexed (fn [idx val] [[idx] val]) padded-out)
         _ (when debug?
             (println (str "between-multiple-brackets-of-def: <" (count padded-out) "> where first " (count (first padded-out)) ", second: " (count (second padded-out))))
             (println "RES:" res))
         ]
     res)))

#_(defn between-brackets [coll]
    (let [_ (println "BT" coll)
          type-kw (-> coll second first)]
      (case (first v)
        :int (->int (second v))
        :eng (eng->decimal v)
        :br (between-brackets v)
        )))

(defn convert [meta-kw x]
  (if (= x :array-overrun)
    :array-overrun
    (let [_ (assert meta-kw (str "Can't convert: <" x "> without a keyword"))
          _ (assert (u/is? x) (str "No point converting from nil: <" x ">"))
          val (case meta-kw
                :base-two
                (cond
                  (true? x) '(1)
                  (false? x) '(0))
                :int
                (cond
                  (string? x) (Long/parseLong x)
                  (boolean? x) (rou/bool->integer x)
                  :default x)
                :eng
                (bigdec (conv/val-set x))
                :array-overrun :array-overrun
                )
          _ (assert (u/is? val) (str "Converted to nil from: <" x ">, type: " (type x)))]
      val)))

(defn all-after [coll kw]
  (loop [coll coll]
    ;(println "loop" coll)
    ;; TIL - destructuring doesn't give next but second
    (let [head (first coll)
          tail (next coll)
          ;_ (println "head" head)
          ;_ (println "tail" tail)
          ]
      (if (= kw (first head))
        tail
        (recur tail)))))

;; [:br [:int "0"] [:single-quoted-number]]
(defn clojurify-between-brackets [seq->]
  (fn [in]
    (reduce
      (fn [acc ele]
        (let [[nav val] ele
              _ (assert (vector? nav))
              ;; If don't need to go inside then only have one of them:
              have-many? (= :br (first val))
              conj-res (if have-many?
                         (mapv #(seq-> %) (map-indexed (fn [idx x] [(conj nav idx) x]) (next val)))
                         (seq-> ele))
              res (conj acc conj-res)
              ]
          res))
      []
      in)))

;;
;; Use typ/nav in combination with meta-data to return proper value for :val/type in {:val/val nil :val/type nil}
;;
(defn typed-value-provider [meta-data]
  (fn [{:keys [typ/nav typ/val] :as ask-typ}]
    (let [_ (assert val (str "Expect typ/val in " ask-typ))]
      {:val/val val :val/type nil})))

(defn brackets-count [def]
  (let [f (fn [ele] (or (not (vector? ele)) (not= :br (first ele))))
        data-part (first (drop-while f def))
        ;_ (println (str "Find brackets count from " data-part))
        ]
    (loop [num 0
           data data-part]
      (let [[head & tail] data
            ;_ (println "->" head tail)
            ]
        (if (and (= head :br) tail)
          (recur (inc num) (first tail))
          num)))))

;;
;; vals is the list, each element of which we want to give to seq->. But seq-> needs each element
;; to be {:keys [typ/nav typ/val]}. So we need to turn each element into this map, at the same
;; time making a typ/nav vector that is a double
;;
(defn double-to-single [seq->]
  (fn [[nav vals]]
    (let [_ (assert (vector? nav))
          values (map-indexed (fn [index val]
                                [(into nav [index]) val]) vals)
          ;_ (assert (= dim (count values)) (str "dim is: " dim ", while values: " (count values) " for " (-> def second second)))
          ]
      (println (str "values: " (seq values)))
      (mapv seq-> values))))

(defn triple-to-single [f]
  (fn [[nav vals]]
    (let [_ (assert (vector? nav))
          values (map-indexed (fn [index val]
                                [(into nav [index]) val]) vals)
          ;_ (assert (= dim (count values)) (str "dim is: " dim ", while values: " (count values) " for " (-> def second second)))
          ]
      ;(println (str "values: " (f values)))
      (f values))))

(defn multiple-to-single [seq-> f]
  (fn [[nav vals]]
    (let [_ (assert (vector? nav))
          values (map-indexed (fn [index val]
                                [(into nav [index]) val]) vals)
          ;_ (assert (= dim (count values)) (str "dim is: " dim ", while values: " (count values) " for " (-> def second second)))
          br-value (first (filter #(= :br (-> % second first)) values))
          ;_ (println "br value: " br-value)
          ]
      ;(println (str "values: " (seq values)))
      (if br-value
        (f values)
        (mapv seq-> values)))))

;;
;; Interesting case where we treating:
;; [:type [:custom-t "String_1"]]
;; , exactly as if it was:
;; [:type [:raw-t "STRING"]]
;;
;; Also making this work for Null_Char
(defn string-like [def]
  (let [str-like-value (-> def (nth 3) second)
        ;_ (assert false (str "see <" res "> from <" def ">"))
        ]
    [str-like-value :string]))

;;
;; [:def [:var "Readings_Age_Timer"] [:type [:custom-t "TIMER"]] [:enc [:assign [:description]]] [:br [:int "-1072480908"] [:int "1000"] [:int "304"]]]
;; [:def [:var "Program_Seconds_Counter"] [:type [:raw-t "COUNTER"]] [:enc [:assign [:description]]] [:br [:int "536870912"] [:int "0"] [:int "45166136"]]]
;; Example:
;; [:def [:var "ONS"] [:array-t [:type [:raw-t "BOOL"]] [:dim [:int "32"]]] [:enc [:assign [:radix [:radix-t "Decimal"]]]] [:open-br] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:base-two [:int "0"]] [:close-br]]
;; [:def       [:var SMRTGS_W_Pending_Tubes_Seq_Changes] [:type [:raw-t BOOL]] [:enc [:assign [:radix [:radix-t Decimal]]]] [:int 0]]
;; [:alias-def [:var Sample_Solenoid_3] [:dot-var [:var L1] . [:dot-var [:var S2] . [:dot-var [:var DO]]] . [:int 2]] [:enc [:assign [:radix [:radix-t Decimal]]]]]
;; [:def [:var L1] [:type [:custom-t Local_Chassis]] [:enc [:assign [:comment [:dot-comment COMMENT . [:var S7 [:dim [:int 2]]]] [:anything-string [:quote "] [:one-or-more-anythings [:anything-word Mass] [:anything-word Flow] [:anything-word Controller] [:anything-word Flow] [:anything-word Rate]] [:quote "]]]]] [:br [:br [:int 64]] [:br [:int 14849]] [:br [:int 524289]] [:br [:eng [:float -8.03574524] [:eng-pos] [:int 001]] [:eng [:float -8.87822800] [:eng-pos] [:int 001]] [:eng [:float -8.14311676] [:eng-pos] [:int 001]] [:eng [:float -5.32299843] [:eng-pos] [:int 001]] [:eng [:float -7.23173599] [:eng-pos] [:int 001]] [:eng [:float -6.63618774] [:eng-pos] [:int 001]] [:eng [:float -7.54913864] [:eng-pos] [:int 001]] [:eng [:float -1.36391068] [:eng-pos] [:int 000]] [:eng [:float -3.21100998] [:eng-pos] [:int 000]] [:eng [:float -1.38157654] [:eng-pos] [:int 000]] [:eng [:float -1.28931808] [:eng-pos] [:int 000]] [:eng [:float -1.69564056] [:eng-pos] [:int 000]] [:eng [:float -1.41886900] [:eng-pos] [:int 000]] [:eng [:float -1.36979675] [:eng-pos] [:int 000]] [:eng [:float -7.11553116] [:eng-pos] [:int 001]] [:eng [:float -7.82767639] [:eng-pos] [:int 001]] [:int 0] [:int 0]] [:br [:eng [:float -1.40709300] [:eng-pos] [:int 000]] [:eng [:float -4.49975200] [:eng-pos] [:int 001]] [:eng [:float -4.77554169] [:eng-pos] [:int 001]] [:eng [:float -1.32072449] [:eng-pos] [:int 000]] [:eng [:float -1.37176132] [:eng-pos] [:int 000]] [:eng [:float -1.47187042] [:eng-pos] [:int 000]] [:eng [:float -1.38157654] [:eng-pos] [:int 000]] [:eng [:float -1.41298294] [:eng-pos] [:int 000]] [:eng [:float -1.39531708] [:eng-pos] [:int 000]] [:eng [:float -7.31201935] [:eng-pos] [:int 001]] [:eng [:float -1.46990585] [:eng-pos] [:int 000]] [:eng [:float -8.96518478] [:eng-pos] [:int 001]] [:eng [:float -1.35213089] [:eng-pos] [:int 000]] [:eng [:float -1.41494370] [:eng-pos] [:int 000]] [:eng [:float 6.59561157] [:eng-neg] [:int 003]] [:eng [:float 2.36174469] [:eng-pos] [:int 001]] [:int 0] [:int 0]] [:br [:eng [:float 4.04768562] [:eng-pos] [:int 000]] [:eng [:float 1.74894047] [:eng-pos] [:int 001]] [:eng [:float 4.92462158] [:eng-neg] [:int 001]] [:eng [:float 2.00037231] [:eng-pos] [:int 001]] [:eng [:float 0.00000000] [:eng-pos] [:int 000]] [:eng [:float 0.00000000] [:eng-pos] [:int 000]] [:eng [:float 0.00000000] [:eng-pos] [:int 000]] [:eng [:float 0.00000000] [:eng-pos] [:int 000]] [:eng [:float 0.00000000] [:eng-pos] [:int 000]] [:eng [:float -1.25125889] [:eng-neg] [:int 003]] [:eng [:float -9.38444166] [:eng-neg] [:int 004]] [:eng [:float -1.25125889] [:eng-neg] [:int 003]] [:eng [:float -1.25125889] [:eng-neg] [:int 003]] [:eng [:float -1.25125889] [:eng-neg] [:int 003]] [:eng [:float -1.25125889] [:eng-neg] [:int 003]] [:eng [:float -1.87688833] [:eng-neg] [:int 003]] [:int 0] [:int 0]]]]
;;
(defn def->value [find-type def reason-type]
  (let [
        array? (= :array-t (-> def (nth 2) first))
        alias? (= :alias-def (first def))
        _ (assert (not alias?) "Initial value is not with the def, so this method is inappropriate for finding value of an alias")
        ;_ (println (str "DEF: <" def ">"))
        seq-> (conv/seq-> find-type)
        multiple-down (multiple-to-single seq-> (clojurify-between-brackets seq->))
        ]
    (if array?
      (let [element-type (-> def (nth 2) second second second)
            custom? (= :custom-t (-> def (nth 2) second second first))]
        (if custom?
          (let [int (brackets-count def)
                ;_ (println (str "brackets count is " int ", from <" def ">"))
                ]
            (case int
              1 [(mapv seq-> (between-brackets-of-def def)) "UNKNOWN"]
              2 [(mapv multiple-down (between-multiple-brackets-of-def def false)) "UNKNOWN"]
              3 [(mapv multiple-down (between-multiple-brackets-of-def def false)) "UNKNOWN"]))
          (case element-type
            "BOOL" (let [
                         between-brackets (between-brackets-of-def def false)
                         ;f (comp seq-> over-base-two)
                         simplified (mapv seq-> between-brackets)]
                     [simplified "BOOL"])
            "INT" [(mapv seq-> (between-brackets-of-def def)) "INT"]
            "DINT" [(mapv seq-> (between-brackets-of-def def)) "DINT"]
            "REAL" [(mapv seq-> (between-brackets-of-def def)) "REAL"]
            "TIMER" [(mapv m/make-timer (between-multiple-brackets-of-def def true)) :timer]
            (assert false (str "Problem non custom array: <" def ">" (con/out-context)))
            )))
      (let [type-of-type (-> def (nth 2) second first)
            ;_ (println "non array, type-of-type" type-of-type)
            custom-type-in (when (= type-of-type :custom-t)
                             (-> def (nth 2) second second))]
        (cond
          (= :raw-t type-of-type)
          (case (-> def (nth 2) second second)
            "TIMER" [(mapv (comp (fn [n] {:val/val (conv/val-set (u/string->int n)) :val/type "DINT"}) second second) (between-brackets-of-def def false)) :timer]
            "COUNTER" [(mapv (comp (fn [n] {:val/val (conv/val-set (u/string->int n)) :val/type "DINT"}) second second) (between-brackets-of-def def)) :counter]
            "STRING"
                     ;[(m/make-string (between-brackets-of-def def false)) :string]
            (let [res (-> def (nth 4) second)
                  _ (assert true res)]
              [res :string])
            "MESSAGE" [(m/make-message (between-brackets-of-def def false)) :message]
            "INT" [(-> def (nth 4) second u/string->int) "INT"]
            "DINT" [(-> def (nth 4) second u/string->int) "DINT"]
            "REAL" [(conv/eng->decimal (nth def 4)) "REAL"]
            "BOOL" [(-> def (nth 4) second i/string->bool) "BOOL"]
            (assert false (str "Problem :raw-t with: <" def ">" (con/out-context)))
            )

          (= :custom-t type-of-type)
          (let [
                ;_ (println "Custom type" custom-type-in)
                ;_ (println "reason-type:" reason-type)
                res (case reason-type
                      :idx-within-attribute [(mapv multiple-down (between-multiple-brackets-of-def (u/probe-off def) false)) custom-type-in]
                      :dot-into-datatype [((clojurify-between-brackets seq->) (between-brackets-of-def (u/probe-off def) false)) custom-type-in]
                      :number-ending [((clojurify-between-brackets seq->) (between-brackets-of-def def)) custom-type-in]
                      ;;
                      ;; Interesting case where we treating:
                      ;; [:type [:custom-t "String_1"]]
                      ;; , exactly as if it was:
                      ;; [:type [:raw-t "STRING"]]
                      ;;
                      :built-in (string-like def)
                      :name-value (string-like def)
                      (do
                        (assert false (str "default handling (prolly wrong) for reason type: " reason-type))
                        [(mapv #(seq-> %) (between-brackets-of-def def)) custom-type-in])
                      )
                ;_ (println "res: " res)
                ]
            res)

          :default (do
                     (println "BAD DEF:" def)
                     (assert false "Problem !!")))))))

;;
;; We never go right to the end, to the dots
;;
(defn strip-out-vars [mid]
  (loop [in mid res []]
    (let [[_ two & tail] in]
      ;(println "T" tail)
      (if (nil? tail)
        (conj res (second two))
        (recur (first tail) (conj res (second two)))))))

;;
;; [:alias-def
;;  [:var Sample_Solenoid_3]
;;  [:dot-var [:var L1] [:dot-var [:var S2] [:dot-var [:var DO]]] [:int 2]] [:enc [:assign [:radix [:radix-t Decimal]]]]]
;;
(defn- alias-for [def]
  (when (= :alias-def (first def))
    (let [dot-var (nth def 2)
          ending (-> dot-var (nth 3) second)
          ;_ (println "ending:" ending)
          _ (assert (integer? (u/string->int-not-strict ending)) (str "Expect alias to end in an integer, not: " ending))
          head (-> dot-var second second)
          middle (strip-out-vars (nth dot-var 2))
          ;_ (println "MID:" middle)
          together (concat [head] middle)
          ]
      [ending together])))

(def known-singletons #{"SCADA_R_Gas" "HMI_R_Gas" "HMI_R_PLC" "HMI_W" "Always"})
(defn known? [header]
  (let [starts-with #(str/starts-with? header %)]
    (some #(when (starts-with %) %) (vec known-singletons))))

;;
;; Does this even happen?
;;
(defn dot-into-datatype?-old [datatype-names x]
  (let [_ (assert (set? datatype-names))
        _ (assert (pos? (count datatype-names)))
        coll (str/split x #"\.")
        _ (assert (pos? (count coll)))
        datatype? (some datatype-names [(first coll)])
        _ (when (and datatype? (not (known? (first coll)))) (println (str "!!! Found another dot-into-datatype?: <" x ">")))
        ]
    datatype?))

(defn- datatype-of [custom-grouped var-name debug?]
  (let [mentions (filter #(= var-name (:reification %)) (u/probe-off (mapcat identity (vals custom-grouped))))
        ;Get plenty of zero mentions as everything goes thru here
        _ (when debug? (println (str "mentions: " (vec mentions) ", of: <" var-name ">")))
        ]
    (:datatype (u/first-no-more mentions))))

(defn dot-into-datatype? [custom-grouped x debug?]
  (let [_ (assert (pos? (count custom-grouped)))
        coll (str/split x #"\.")
        [head have-more] coll
        _ (assert head)
        ]
    ;;
    ;; Null_Char is a datatype, but not dotted into as they normally are
    ;;
    (when have-more
      (datatype-of custom-grouped head debug?))))

(defn number-ending? [x]
  (let [coll (str/split x #"\.")
        _ (assert (pos? (count coll)))
        ending (last coll)]
    (u/string->int-not-strict ending)))

(defn raw-boolean? [x]
  (let [res (boolean? x)
        ;_ (println "raw of " x " is " res)
        ]
    res))

(defn raw-int? [x]
  (let [res (-> x u/string->int-not-strict integer?)
        ;_ (println "raw of " x " is " res)
        ]
    res))

(defn raw-number? [x]
  (try
    (-> x read-string number? boolean)
    (catch NumberFormatException _
      false)))

(defn raw-value? [x]
  (let [res (or (vector? x) (boolean? x) (number? x) (decimal? x) (-> x u/string->int-not-strict integer?))
        ;_ (println "raw of " x " is " res)
        ]
    res))

(defn value-value? [x]
  (and (map? x) (some #{:val/val} (keys x)) (some #{:val/type} (keys x))))

;;
;; Result has {:val/val} in it, to be stripped if needed.
;;
(defn dotted-tag-initial-value
  [tag-defs datatypes ending dot-var reason-type def meta-data find-type]
  (let [_ (assert (some #{:idx-within-attribute :single-idx-array-dot :name-value} [reason-type]) (str "Got reason-type: " reason-type))
        ;_ (println "dot-var:" dot-var)
        [head & trailing] dot-var
        ;;
        ;; top-def is the def for L1 when have L1.S2.DO.2
        ;; It is where the initialization data is kept
        ;;
        top-def (first (filter #(= head (u/probe-off (-> % meta/tag-by-name-q))) tag-defs))
        ;_ (println "top-def:" top-def)
        containing-type (meta/top-type top-def)
        ;_ (println "containing-type:" containing-type)
        builtin-fn? (:indexing-fn (rou/built-in-by-name? containing-type))
        ;;
        ;; We store a builtin-fn? as an array rather than separately as say .DN, .PRE etc. Makes sense as an
        ;; array because they are related to one another. Here no storage is happening.
        ;; If coming through to here look right at beginning: unwrap-args
        ;;
        _ (assert (nil? builtin-fn?) (str "No longer using for built ins (prolly means not recognised trailing built in property): "
                                          containing-type " <" head "> <" trailing ">"))
        ;;
        ;; meta-data shows how to traverse an array of arrays so [0 0] is first first
        ;; This represents the index of S2 in L1 then of DO in S2
        ;;
        ;meta-data (data/number-seek datatypes containing-type trailing false)
        ;find-type (fn [v] :wow-2)
        ]
    (if builtin-fn?
      (let [[array-value built-in-type] (def->value find-type def reason-type)
            ]
        [array-value built-in-type])
      (let [
            bit-fill? (and (= 3 (count meta-data)) (= (-> meta-data last :seek/type) :bit-fill))
            _ (assert (not bit-fill?) (str "No longer happening here - captured at highest level as an array"))
            ;;
            ;; Note that the meta-data does not include the number on the end. If there is an ending
            ;; then it should be coming in in this function's arg <ending>
            ;;
            ;_ (println "meta-data" meta-data ", ending " ending)
            ;;
            ;; data is in the form that meta-data can handle, normal Clojure vectors
            ;;
            data (case (count meta-data)
                   1 (mapv #(as-> % $ ((conv/seq-> find-type) $))
                           (if (= reason-type :idx-within-attribute)
                             #_(between-multiple-brackets-of-def (u/probe-off top-def))
                             (assert false (str "Can't see this working"))
                             (between-brackets-of-def top-def)))
                   2 (mapv #(as-> % $ (next $) (mapv (conv/seq-> find-type) $)) (between-brackets-of-def top-def))
                   3 (let [
                           ;_ (println "reason-type: " reason-type)
                           ;_ (println "b/ween brackets: <" (between-brackets-of-def top-def) ">")
                           ;_ (println "meta: " meta-data)
                           _ (assert (= (-> meta-data last :seek/type) :bit-fill))
                           ;_ (println "top-def: " top-def)
                           ;_ (println "res: " (clojurify-between-brackets (between-brackets-of-def top-def)))
                           res ((clojurify-between-brackets find-type) (between-brackets-of-def top-def))
                           ;_ (println "clojurified: " res)
                           ]
                       res)
                   )

            ;_ (println "data" data)
            res (get-in data (map :seek/idx (if bit-fill? (take 2 meta-data) meta-data)))
            _ (assert (u/not-blank? res) (str "Found nothing using meta-data: " meta-data ", going into: " data))
            _ (assert (value-value? res))
            ;_ (println "res" res)
            ]
        (if (or ending bit-fill?)
          (let [as-binary (seq (conv/dec->bit-array (-> meta-data last :seek/type conv/byte-size) (:val/val res)))
                ;_ (println (str "as-binary: " as-binary))
                end-int (if ending (u/string->int ending) (-> meta-data last :seek/idx))
                ;_ (println (str "as-binary: " as-binary ", we want to alter at idx " end-int ", starting from right"))
                reversed (reverse as-binary)
                ending-res (nth reversed end-int)
                _ (assert (u/not-blank? ending-res))
                ;_ (println "ending-res" ending-res)
                ]
            [{:val/val (conv/val-set ending-res) :val/type (:val/type res)} (-> meta-data last :seek/type)])
          [res (-> meta-data last :seek/type)])))))

;;
;; Note that a converted value is returned
;;
;(defn tag-initial-value [def]
;  (let [res (def->value def)
;        ;_ (println "Getting" res "fFrom" def)
;        ]
;    res))

(defn read-edn-file [edn-names name]
  (let [f-name (str name ".edn")
        file-name (first (filter #(.endsWith % f-name) edn-names))
        ;_ (assert file-name (str "cannot find " f-name))
        ]
    (if file-name
      (let [tasks (read-string (slurp file-name))
            _ (assert (pos? (count tasks)))]
        tasks)
      (println (str "WARNING: " "cannot find " f-name)))))

;(defn tag-defs-from-many [tags]
;  (let [res (-> tags first :input :parsed-value second next)
;        num (count res)
;        _ (assert (> num 10) (str "Only got " num ":" (u/pp-str tags)))]
;    res))

(defn tag-defs-from-one [tag]
  (let [res (-> tag :input :parsed-value second next)
        num (count res)
        _ (assert (> num 10) (str "Only got " num ":" (u/pp-str tag)))]
    res))

(defn last-starts-with? [word in-str]
  (let [coll (str/split in-str #"/")]
    (str/starts-with? (last coll) word)))

(defn after-in-dir [word full-directory-name]
  (let [coll (str/split full-directory-name #"/")
        last-is (last coll)]
    (subs last-is (count word))))

(def slurp-edn (comp vec read-string slurp))

(defn slurp-with-key-add-ons [key-str routine-file-name parameters-file-name local-tags-file-name]
  [key-str {:routine    (u/first-no-more (slurp-edn routine-file-name))
            :parameters (u/first-no-more (slurp-edn parameters-file-name))
            :local-tags (u/first-no-more (slurp-edn local-tags-file-name))}])

(defn grab-alias [def]
  (let [name (-> def second second)
        [num leading] (alias-for def)
        alias-str (str (apply str (interpose "." leading)) "." num)
        ;_ (println "alias-str: <" alias-str "> name: <" name ">")
        ]
    {:alias-name name
     :alias-for alias-str}))

(defn explode-program [program-directory-names]
  (fn [program-name]
    (let [directory-name (some #(when (str/ends-with? % program-name) %) program-directory-names)
          program-tags (slurp-edn (str directory-name "/tags.edn"))
          _ (assert (= 1 (count program-tags)))
          defs (tag-defs-from-one (first program-tags))
          alias-defs (filter #(= (first %) :alias-def) defs)
          ;Common doesn't have any
          ;_ (assert (pos? (count alias-defs)) (str "No alias defs found in: " (u/pp-str defs)))
          aliases (mapv grab-alias alias-defs)
          ;_ (assert (pos? (count aliases)))]))
          routines (slurp-edn (str directory-name "/routines.edn"))
          ]
      {:program/name     program-name
       :program/defs     defs
       :program/routines routines
       :program/aliases  aliases})))

;;
;; Will hand back a map of vectors that will be useful.
;; For starters we will just return:
;; :program-routines
;; :program-tag-defs
;; :start-routine
;; :controller-tag-defs
;;
(defn start [dir-name]
  (let [directory (clojure.java.io/file dir-name)
        files (.listFiles directory)
        file-names (map #(.getPath %) files)
        directories (filter #(.isDirectory %) files)
        all-directory-names (map #(.getPath %) directories)
        edn-names (filter #(str/ends-with? % ".edn") file-names)
        rd (partial read-edn-file edn-names)
        ]
    (let [controller-tags (rd "tags")
          program-directory-names (filter #(last-starts-with? "program_" %) all-directory-names)
          name-only (fn [x]
                      (let [splits (str/split x #"_")]
                        (last splits)))
          program-names (mapv name-only program-directory-names)
          _ (println (str "Available program-names: " program-names))
          ;_ (assert (some #{program-name} program-names))
          program-exploder (explode-program program-directory-names)
          programs (map program-exploder program-names)
          controller-tag-defs (tag-defs-from-one controller-tags)
          datatypes (map #(-> % :input :parsed-value) (rd "datatypes"))
          ;_ (println "directory-names: " directory-names)
          add-on-instruction-dir-paths (filter #(last-starts-with? "add-on-instruction_" %) all-directory-names)
          add-on-instruction-names (map #(after-in-dir "add-on-instruction_" %) add-on-instruction-dir-paths)
          ;_ (println "program-directory-names: " add-on-instruction-dir-paths)
          add-on-routines (into {} (map slurp-with-key-add-ons add-on-instruction-names
                                        (map #(str % "/routine.edn") add-on-instruction-dir-paths)
                                        (map #(str % "/parameters.edn") add-on-instruction-dir-paths)
                                        (map #(str % "/local-tags.edn") add-on-instruction-dir-paths)))]
      ;; "R1S5Ch0_Conv" "O2_Zero_Cal_Reading" "Local_Logic" "Tubes_Seq"
      ;(println (tag-initial-value defs "Tubes_Seq"))
      {
       ;:program-tag-defs    defs
       ;:program-routines    routines
       ;:aliases             aliases
       :programs            programs
       :controller-tag-defs controller-tag-defs
       :datatypes           datatypes
       :add-on-instructions add-on-routines
       })))
