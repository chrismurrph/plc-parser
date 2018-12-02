(ns instructions
  (:require [utils :as u]
            [datatypes :as data]
            [clojure.string :as str]
            [convert :as conv]
            [context :as con]
            [stateful-instructions :as si]
            [domain-utils :as du]
            [domain-constants :as constants])
  (:import (java.math RoundingMode)))

;;
;; All values are now stored as maps in the cache, even array values. Any value that comes from a tag must be of this type.
;; As well as checking, if what we get is a thunk then execute it
;;
(defn func-rd [x]
  (if (fn? x)
    (func-rd (x))
    (do
      (assert (or (vector? x) (and (map? x) (some #{:val/val} (keys x)))) (str "Expect value to be map with :val/val in it, got: <" (seq x) ">"))
      (assert (or (vector? x) (-> x :val/val u/is?)) (str "If not a fn expect to be wrapped in :val/val, got: <" x ">"))
      x)))

(defn fn-reader [context get-tag-fn]
  ;context is only supplied if it is needed
  ;(assert context (str "No context supplied"))
  (assert (fn? get-tag-fn) (str "fn-reader needs be passed in a function"))
  (fn [tag]
    (assert tag)
    ;(println "tag: <" tag ">")
    (let [local-val (si/read-local-value tag)]
      (or local-val
          (let [[kw in] ((get-tag-fn context) tag)
                _ (assert in (str "Probably s/be result: <" kw "> <" in ">"))]
            (func-rd in))))))

(defn produce-reader [infos]
  (fn-reader (:add-on-context infos) (:info/get-tag infos)))

(defn produce-writer
  ([infos]
   (produce-writer infos false))
  ([infos debug?]
   ((:info/set-tag infos) (:add-on-context infos) debug?)))

(defn do-calc [f infos tags op-msg]
  (let [read-fn (produce-reader infos)
        [tag-a tag-b] tags
        x (read-fn tag-a)
        y (read-fn tag-b)
        a (:val/val x)
        b (:val/val y)
        res (try
              (f a b)
              (catch Exception x
                (when (not= a b) (println (str "UNEXPECTED MY WARNING for <" op-msg ">: " x " for <" a "> and <" b ">, tags: <" tag-a "> <" tag-b ">")))
                0))
        ]
    res))

(defn do-divide [infos tags]
  (let [read-fn (produce-reader infos)
        [tag-a tag-b] tags
        x (read-fn tag-a)
        y (read-fn tag-b)
        a (:val/val x)
        b (:val/val y)
        res (try
              (.divide (bigdec a) (bigdec b) constants/PRECISION RoundingMode/HALF_UP)
              (catch Exception x
                (when (not= a b) (println (str "MY do-divide WARNING: " x " for <" a "> and <" b ">, tags: <" tag-a "> <" tag-b ">")))
                0))
        ]
    res))

(defn add [infos energy tags]
  (if (not energy)
    false
    (let [
          res (do-calc + infos tags "add")
          dest-tag (nth tags 2)
          write-fn (produce-writer infos)
          _ (write-fn (data/wrap-arg dest-tag) res)]
      true)))

(defn sub [infos energy tags]
  (if (not energy)
    false
    (let [
          res (do-calc - infos tags "sub")
          dest-tag (nth tags 2)
          write-fn (produce-writer infos)
          _ (write-fn (data/wrap-arg dest-tag) res)]
      true)))

(defn div [infos energy tags]
  (if (not energy)
    false
    (let [calc-res (do-divide infos tags)
          res (du/convert-dec calc-res)
          dest-tag (nth tags 2)
          write-fn (produce-writer infos)
          _ (write-fn (data/wrap-arg dest-tag) res)]
      true)))

(defn mul [infos energy tags]
  (if (not energy)
    false
    (let [
          res (du/check-dec (do-calc * infos tags "mul"))
          ;_ (when (= res 1066288094M)
          ;    (println (str "result of mul: <" res ">, type: <" (type res) ">")))
          dest-tag (nth tags 2)
          write-fn (produce-writer infos)
          _ (write-fn (data/wrap-arg dest-tag) res)]
      true)))

(defn equal [tag-a tag-b x y]
  (let [a (:val/val x)
        b (:val/val y)
        type-a (type a)
        type-b (type b)
        _ (assert (= type-a type-b) (str "Bad type compare " x ", " y " for: " tag-a " -|- " tag-b (con/out-context)))
        ;; Too strong. Was getting problem between :int and "DINT"
        ;;_ (assert (= (:val/type x) (:val/type y)) (str "Bad :val/type compare " x ", " y))
        ]
    (= a b)))

(defn less-than [x y]
  (let [a (:val/val x)
        b (:val/val y)
        ;type-a (type a)
        ;type-b (type b)
        ;_ (assert (= type-a type-b) (str "Bad type compare " x ", " y))
        ;_ (assert (= (:val/type x) (:val/type y)) (str "Bad :val/type compare " x ", " y))
        ]
    (< (du/convert-dec a) (du/convert-dec b))))

(defn greater-than [x y]
  (let [a (:val/val x)
        b (:val/val y)
        ;type-a (type a)
        ;type-b (type b)
        ;_ (assert (= type-a type-b) (str "Bad type compare " x ", " y))
        ;_ (assert (= (:val/type x) (:val/type y)) (str "Bad :val/type compare " x ", " y))
        ]
    (> (du/big-dec "six" a) (du/big-dec "seven" b))))

(defn greater-than-or-equal [x y]
  (let [a (:val/val x)
        b (:val/val y)
        ]
    (>= (du/big-dec "eight" a) (du/big-dec "nine" b))))

(defn less-than-or-equal [x y]
  (let [a (:val/val x)
        b (:val/val y)
        ]
    (<= (du/big-dec "ten" a) (du/big-dec "eleven" b))))

(defn les [infos energy tags]
  (assert (boolean? energy))
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          ;_ (println (str "LES, to read: <" (first tags) ">"))
          x (read-fn (first tags))
          y (read-fn (second tags))
          res (less-than x y)
          ]
      res)))

(defn mid [infos energy tags]
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          [source-tag quantity-tag start-tag destination-tag] tags
          _ (assert false (str "Not yet coded MID: " tags))]
      true)))

(defn grt [infos energy tags]
  (assert (boolean? energy))
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          x (read-fn (first tags))
          y (read-fn (second tags))
          res (greater-than x y)
          ]
      res)))

(defn geq [infos energy tags]
  (assert (boolean? energy))
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          x (read-fn (first tags))
          y (read-fn (second tags))
          res (greater-than-or-equal x y)
          ]
      res)))

(defn leq [infos energy tags]
  (assert (boolean? energy))
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          x (read-fn (first tags))
          y (read-fn (second tags))
          res (less-than-or-equal x y)
          ]
      res)))

(defn neq [infos energy tags]
  (assert (boolean? energy))
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          [tag-a tag-b] tags
          x (read-fn tag-a)
          y (read-fn tag-b)
          res (not (equal tag-a tag-b x y))
          _ (when false
              (println (str "NEQ for: " (first tags) " " (second tags) ", VALS: " x " and " y " is " res)))
          ]
      res)))

(defn cmp [infos energy tags]
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          [tag-expression] tags
          _ (assert (= "1<=NumberOfConnections" tag-expression) (str "We faked this, so can't handle: <" tag-expression ">"))
          num-connections (-> "NumberOfConnections" read-fn :val/val)
          ]
      (<= 1 num-connections))))

(def kept-values (atom {}))
(defn clear-debug-cache []
  (reset! kept-values {}))

(defn equ [infos energy tags]
  (assert (boolean? energy))
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          [tag-a tag-b] tags
          x (read-fn tag-a)
          y (read-fn tag-b)
          res (equal tag-a tag-b x y)
          ;_ (println (str "EQU for: " (first tags) " " (second tags) ", VALS: " x " and " y " is " res))
          ]
      res)))

;;"WallClockTime" "?" "LocalDateTime" "PLC_TIME.Year"
(defn gsv [infos energy tags]
  (let [[one two three four] tags]
    (assert (and (= one "WallClockTime")
                 (= two "?")
                 (= three "LocalDateTime")
                 (= four "PLC_TIME.Year")))
    energy))

(defn _mod [infos energy tags]
  (when energy
    (let [read-fn (produce-reader infos)
          write-fn (produce-writer infos)
          [source-tag-a source-tag-b  dest-tag] tags
          dividend (-> source-tag-a read-fn :val/val)
          divisor (-> source-tag-b read-fn :val/val)
          _ (assert dividend)
          _ (assert divisor)
          res (mod dividend divisor)
          _ (write-fn (data/wrap-arg dest-tag) res)]))
  energy)

(defn mov [infos energy tags]
  (assert (boolean? energy))
  (when energy
    (let [read-fn (produce-reader infos)
          [tag-a tag-b] tags
          debug? false #_(and false (= tag-a "L1.S2.DO") (and (= tag-b "Local:2:O.Data")))
          _ (assert (u/not-blank? tag-a) "Blank tag-a in mov")
          _ (assert (u/not-blank? tag-b) "Blank tag-b in mov")
          value-a (read-fn tag-a)
          _ (when debug? (println "When read expect wrapped but: <" value-a "> from <" tag-a ">"))
          _ (assert (not (fn? value-a)))
          _ (assert (u/not-blank? value-a) (str "Blank return from " tag-a))
          _ (when debug? (println "To write new value:" value-a "to" tag-b))
          write-fn (produce-writer infos debug?)
          _ (write-fn (data/wrap-arg tag-b) value-a)]))
  energy)

(defn clr [infos energy tags]
  (let [write-fn (produce-writer infos)
        [dest-tag] tags
        _ (write-fn (data/wrap-arg dest-tag) 0)])
  energy)

;; Lets call JSR in a doseq
;; Example
;; ["ClearTransTrigger" "Index" "0" "MBTU_Connections[0].MBTI_NumberOfTransactions" "1"]
(defn _for [infos energy tags]
  (when energy
    (let [read-fn (produce-reader infos)
          write-fn (produce-writer infos)
          [routine-name index-tag start-at end-at step] tags
          start-value (:val/val (read-fn start-at))
          _ (assert (= 0 start-value) (str "start value is: <" start-value ">"))
          step-value (:val/val (read-fn step))
          _ (assert (= 1 step-value) (str "step value is: <" step-value ">"))
          end-value (:val/val (read-fn end-at))
          times end-value
          routine-fn (:info/get-routine infos)
          scan-routine-fn (:info/scan-routine infos)
          ]
      (doseq [time (range times)]
        (let [routine (routine-fn routine-name)
              ;_ (println "@@ About to scan routine: <" routine-name ">")
              _ (write-fn (data/wrap-arg index-tag) time)
              _ (scan-routine-fn false routine)]))))
  energy)

(defn btd-calc [src src-bit dest dest-bit length byte-size]
  (assert (not (or (nil? src) (nil? src-bit) (nil? dest) (nil? dest-bit) (nil? length) (nil? byte-size))))
  (let [src-as-bits (conv/dec->bit-array 2 byte-size src true)
        dest-as-bits (conv/dec->bit-array 2 byte-size dest true)
        to-copy (->> src-as-bits reverse (drop src-bit) (take length) reverse)
        end-size (+ dest-bit (count to-copy))
        start-size (- byte-size end-size)
        res (concat (take start-size dest-as-bits) to-copy (->> dest-as-bits reverse (take dest-bit) reverse))]
    ;(println "to-copy: " to-copy)
    (conv/unsigned-bit-array->dec res)))

(defn btd [infos energy tags]
  ;(assert false (str "energy: " energy (con/out-context)))
  (when energy
    (let [read-fn (produce-reader infos)
          write-fn (produce-writer infos)
          [src-tag src-bit-str dest-tag dest-bit-str length-str] tags
          src-bit (u/string->int src-bit-str)
          dest-bit (u/string->int dest-bit-str)
          length (u/string->int length-str)
          src-tag-value (-> src-tag read-fn :val/val)
          dest-tag-map-value (-> dest-tag read-fn)
          dest-type (:val/type dest-tag-map-value)
          _ (assert dest-type (str "No type found in value: <" dest-tag-map-value "> of <" (read-fn dest-tag) ">"))
          dest-size (conv/byte-size dest-type)
          ;_ (println "dest-size: " dest-size)
          res (btd-calc src-tag-value src-bit (:val/val dest-tag-map-value) dest-bit length dest-size)
          ]
      (write-fn (data/wrap-arg dest-tag) res)
      ;(println (str "dest-tag: " dest-tag " to get: <" res ">"))
      ))
  energy)

;;
;; source-tag can be an expression. We don't handle that yet.
;;
(defn cpt [infos energy tags]
  (when energy
    (let [read-fn (produce-reader infos)
          write-fn (produce-writer infos)
          [source-tag dest-tag] tags
          _ (write-fn (data/wrap-arg dest-tag) (read-fn source-tag))]))
  energy)

(defn swpb [infos energy tags]
  (when energy
    (let [read-fn (produce-reader infos)
          write-fn (produce-writer infos)
          [source-tag dest-tag] tags
          _ (assert false "Not yet coded SWPB")]))
  energy)

(defn delete [infos energy tags]
  (when energy
    (let [read-fn (produce-reader infos)
          write-fn (produce-writer infos)
          [source-tag dest-tag] tags
          _ (assert false "Not yet coded DELETE")]))
  energy)

(defn _or [infos energy tags]
  (when energy
    (let [read-fn (produce-reader infos)
          write-fn (produce-writer infos)
          [source-tag dest-tag] tags
          _ (assert false "Not yet coded OR")]))
  energy)

(defn _and [infos energy tags]
  (when energy
    (let [read-fn (produce-reader infos)
          write-fn (produce-writer infos)
          [tag-a tag-b dest-tag] tags
          [value-a value-b] (mapv read-fn [tag-a tag-b])
          _ (assert false (str "Need bitwise AND: " value-a "," value-b))]))
  energy)

(defn _or [infos energy tags]
  (when energy
    (let [read-fn (produce-reader infos)
          write-fn (produce-writer infos)
          [tag-a tag-b dest-tag] tags
          [value-a value-b] (mapv read-fn [tag-a tag-b])
          _ (assert false (str "Need bitwise OR: " value-a "," value-b))]))
  energy)

;;
;; If only ever appears in the output position then ought to return nil, but
;; I don't know that for sure, so for now returning true
;;
(defn res [infos energy tags]
  (when energy
    (let [write-fn (produce-writer infos)
          [dest-tag] tags
          _ (write-fn (data/wrap-arg dest-tag) {:instruction/name :instru/res})]))
  energy)

;;
;; Timer is reset if energy coming in is false
;;
(defn ton [infos energy tags]
  (let [write-fn (produce-writer infos)
        [dest-tag] tags
        _ (write-fn (data/wrap-arg dest-tag) {:instruction/name      :instru/ton
                              :instruction/in-energy energy})]
    energy))

(defn ctu [infos energy tags]
  (let [write-fn (produce-writer infos)
        [dest-tag] tags
        _ (write-fn (data/wrap-arg dest-tag) {:instruction/name      :instru/ctu
                              :instruction/in-energy energy})]
    energy))

;;
;; Output energize.
;; Just set the tag to the energy coming in
;;
(defn ote [infos energy tags]
  (let [
        ;_ (println (str "in ote with energy " energy " for " (first tags)))
        write-fn (produce-writer infos)
        [tag-a] tags
        _ (write-fn (data/wrap-arg tag-a) energy)]
    nil))

;;
;; Output unlatch
;; set the tag to the opposite of the energy coming in
;; Note I haven't set a bit
;;
(defn otu [infos energy tags]
  (when energy
    (let [write-fn (produce-writer infos)
          [tag-a] tags
          _ (write-fn (data/wrap-arg tag-a) false)]
      nil)))

(defn otl [infos energy tags]
  (when energy
    (let [write-fn (produce-writer infos)
          [tag-a] tags
          _ (write-fn (data/wrap-arg tag-a) true)]
      nil)))

;;
;; The instruction sets the Storage Bit if the preceding logic is true and clears it when false.
;; The Output Bit is set for one program scan when the Storage bit is set.  The Storage Bit is cleared when the
;; preceding logic is false. This instruction provides a one shot output bit, triggered when the preceding logic
;; transitions from false to true.
(defn osr [infos energy tags]
  (let [read-fn (produce-reader infos)
        write-fn (produce-writer infos)
        [storage-bit-tag output-bit-tag] tags
        storage-bit (read-fn storage-bit-tag)
        out-energy (if (not energy)
                     (do
                       (write-fn (data/wrap-arg storage-bit-tag) false)
                       false)
                     (if (not storage-bit)
                       (do
                         (write-fn (data/wrap-arg storage-bit-tag) true)
                         (write-fn output-bit-tag true)
                         true)
                       (do
                         (write-fn (data/wrap-arg output-bit-tag) false)
                         false)))
        ]
    out-energy))

#_(if (and energy (not storage-bit))
    (do
      (write-fn storage-bit-tag true)
      (write-fn output-bit-tag true))
    (write-fn storage-bit-tag false))
#_(when (and energy (not storage-bit))
    (write-fn storage-bit-tag true)
    (write-fn output-bit-tag true))
#_(when (and (not energy) storage-bit)
    (write-fn storage-bit-tag false)
    )

(defn ons [infos energy tags]
  (let [
        {:keys [unwrap/array unwrap/index]} (first tags)
        debug? (and energy (= index "10") (= "ONS" array))
        _ (when debug?
            (println "ons in energy: <" energy ">"))
        read-fn (produce-reader infos)
        write-fn (produce-writer infos)
        [output-bit-tag] tags
        output-bit-raw (read-fn output-bit-tag)
        _ (assert (= :base-two (:val/type output-bit-raw)))
        output-bit (= 1 (-> output-bit-raw :val/val first))
        _ (when debug?
            (println "ons output-bit: <" output-bit ">"))
        out-energy (if energy
                     (if (not output-bit)
                       (do
                         (write-fn (data/wrap-arg output-bit-tag) false)
                         true)
                       false)
                     (do
                       (write-fn (data/wrap-arg output-bit-tag) false)
                       false))
        _ (when debug?
            (println "ons out energy: <" out-energy ">"))]
    out-energy))

(defn string->bool [x]
  ;(println "in:" x (type x))
  (assert (not (boolean? x)) "No point changing boolean into a boolean")
  (assert x "string->bool not being given anything")
  (cond
    (= x "0") false
    (= x "1") true
    :default (do
               (assert false (str "string->bool, unknown value: " x ", of type: " (type x)))
               false)))

(defn ->bool [x]
  (cond
    (= x 1) true
    (= x 0) false
    (= x '(1)) true
    (= x '(0)) false
    (= x :array-overrun) false
    :default (assert false (str "Don't know whether <" x "> is true or false, type: <" (type x) ">, nil?: <" (nil? x) ">" (con/out-context)))))

(defn extract-truth [x]
  (if (boolean? x)
    x
    (->bool x)))

;;
;; If the underlying tag is set then we are going to return true and thus examine the next instruction
;; So closed means underlying tag is set/true/1
;; Examine means return true energy so the next instruction won't be skipped, or output coil will be set
;;
(defn xic [infos energy tags]
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          [tag-a] tags
          res (read-fn tag-a)
          _ (assert res (str "Expect sumfin back when read <" tag-a ">"))
          output (-> res :val/val extract-truth)
          ]
      output)))

(defn debug [infos energy tags]
  (let [read-fn (produce-reader infos)
        xs (map (comp :val/val read-fn) tags)
        _ (assert (seq xs) (str "Found no values for " tags))
        together (mapv vector tags xs)
        ]
    (doseq [[tag val] together]
      (let [curr-val (get @kept-values tag)]
        (when (not= curr-val val)
          (let [txt (if (nil? curr-val) "new" "changed")]
            (swap! kept-values assoc tag val)
            (println (str "===> DEBUG (when " txt "): <" tag ">, <" val ">"))))))
    energy))

(defn msg [infos energy tags]
  ;;(println (str "===> MSG: " (seq tags)))
  energy)

(defn debug-msg [infos energy tags]
  (println (str "===> MSG: " (seq tags)))
  energy)

(defn xic-d [infos energy tags]
  (debug infos energy tags)
  (xic infos energy tags))

(defn xio [infos energy tags]
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          _ (assert read-fn)
          [tag-a] tags
          res (read-fn tag-a)
          _ (assert res (str "Expect sumfin back when read <" tag-a ">"))
          output (not (-> res :val/val extract-truth))
          ]
      output)))

;; TEMPD : DINT (RADIX := Decimal, ExternalAccess := None, DefaultData := 0);
;; VAL : REAL (Description := "Converted Value", Usage := Output, RADIX := Float, Required := Yes, Visible := Yes, ExternalAccess := Read/Write, DefaultData := 0.00000000e+000);
;; COP(TEMPD,VAL,1);
;;
(defn cop [infos energy tags]
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          _ (assert read-fn)
          ;; Expect len not to be a number, at which point will need to do same thing as fll
          [tag-a tag-b len] tags
          length (u/string->int len)
          _ (assert (pos? length))
          ;; count-in-array is just sponging off :add-on-context
          write-fn ((:info/set-tag infos) (assoc (:add-on-context infos) :count-in-array length))
          dts (-> infos :controller-info :datatypes)
          _ (assert dts)
          _ (data/copy infos dts read-fn write-fn tag-a tag-b length)
          ]
      true)))

;;
;; Null_Char : String_1  := [0,'$00'];
;; FLL(0,Null_Char,1);
;; For this to make sense the second argument is the whole array and the first element of it get 0 copied in
;; There's no way that Null_Char can be interpretted as the first element of some bigger array, which is how I have
;; interpretted COP to work.
;; Alternative explanation is that you can think of it as first element in an array as long as there's only one
;; element being copied (last arg is 1)
;;
(defn fll [infos energy tags]
  (if (not energy)
    false
    (let [read-fn (produce-reader infos)
          _ (assert read-fn)
          [tag-a tag-b len-tag-or-number] tags
          val-to-copy (read-fn tag-a)
          ;; Assumption here that read-fn will handle say "10" is probably wrong
          len (-> len-tag-or-number read-fn :val/val)
          _ (assert (pos? len) (str "Expect +ive len " (con/out-context)))
          ;; count-in-array is just sponging off :add-on-context
          write-fn ((:info/set-tag infos) (assoc (:add-on-context infos) :count-in-array len) false)
          dts (-> infos :controller-info :datatypes)
          _ (assert (seq dts) (str "No datatypes"))
          ;_ (println "in FLL")
          _ (data/fill infos dts val-to-copy write-fn tag-b len)
          ;_ (println "done FILL")
          ]
      true)))

(defn nop [infos energy tags]
  energy)

(defn afi [infos energy tags]
  false)
