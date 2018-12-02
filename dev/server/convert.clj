(ns convert
  (:require [clojure.string :as str]
            [utils :as u]
            [context :as con]
            [domain-utils :as du]
            [domain-constants :as dc]))

;;
;; Given a decimal number n (usually base is 2) return an array of
;; 1s and 0s. Fills up from the right and you don't end up with any
;; leading zeros (how can you?? - the size is not known).
;;
;; Only call internally because a calling function will always need
;; to take care of the sign.
;;
;; Use dec->bit-array instead
;;
(defn- -to-digits
  [n base]
  (if (zero? n)
    [n]
    (loop [n n
           digits ()]
      (if (pos? n)
        (recur (quot n base)
               (conj digits (mod n base)))
        digits))))

;;
;; Gets a particular digit from a decimal number. We need to know the size if it is a -ive number, b/c then
;; the leftmost bit gets set. Theoretically that only matters if the leftmost/sign bit is being asked for
;; (i.e. is `n`). However we should always know the size, so making it the first param.
;;
;; Scope for refactoring as there are other functions that do same thing - look for reverse
;;
(defn dec->bit [byte-size x n]
  (let [
        ;_ (assert false byte-size)
        _ (assert (number? x) (str "x s/be number, but got: <" x ">"))
        _ (assert (number? n) (str "n s/be number, but got: <" n ">"))
        digits (-to-digits x 2)
        padded-digits (u/left-pad digits 0 byte-size)
        get #(nth % n)
        ;; Needs to be reversed because get goes from left, yet indexing is from the right.
        res (-> padded-digits reverse get int)]
    res))

#_(defn t []
  (dec->bit 14849 1))

(defn raw-type? [str]
  (some #{"DINT" "SINT" "INT"} [str]))

(defn byte-size [byte-str]
  (case byte-str
    "DINT" 32
    "EIGHT" 8
    "SINT" 8
    "INT" 16
    :int 32
    (assert false (str "No match to: <" byte-str ">" (con/out-context)))))

;;
;; If only I had known about these Java methods
;;
;; Given an integer value, we produce a binary String representation using (Integer/toString integer radix):
;; (Integer/toString 19 2) => "10011"
;; (Integer/toString 45881 2) => "1011001100111001"
;; In the other direction, we can take a binary String and create the corresponding integer by using (Integer/parseInt string radix):
;; (Integer/parseInt "101010101" 2) => 341
;; (Integer/parseInt "110110110" 2) => 438
;;

;;
;; Going. Use dec->bit-array instead
;;
(defn -to-type-digits [type-byte-size n b]
  (let [res (-to-digits n b)
        want-size (case type-byte-size
                    :bit-fill 8
                    ;"DINT" 32
                    32 32
                    16 16
                    8 8
                    ;"EIGHT" 8
                    )
        padded-res (u/left-pad res 0 want-size)]
    padded-res))

;;
;; Convert a number in base b that's coming in as a list to an integer. Do this by taking
;; off one at a time and multiplying by the power are up to.
;;
;; Don't use directly. Instead bit-array->dec or unsigned-bit-array->dec appreciate fact that
;; there's a signed bit there.
;;
(defn -from-type-digits [lis base]
  (let [in-list (reverse lis)
        up-to-idx (count lis)]
    (loop [acc 0
           idx 0]
      (if (= idx up-to-idx)
        (long acc)
        (let [at (nth in-list idx)
              new-idx (inc idx)
              power (u/exp base idx)
              pow-res (if (zero? power) 1 power)
              power-mult (* at pow-res)
              ;_ (println at power-mult)
              ]
          (recur (+ acc power-mult) new-idx))))))

;;
;; The only way I can think that -128 goes into 8 bits is if the byte is unsigned, in which case we
;; have to pretend the incoming number is 128 (not -128).
;;
(defn dec->bit-array
  ([size dec-num]
   (dec->bit-array 2 size dec-num false))
  ([base size dec-num]
    (dec->bit-array base size dec-num false))
  ([base size dec-num unsigned?]
   "If dec-num comes with a sign we will ignore it. This is quite bad"
   (let [sz (if unsigned? size (dec size))
         max-abs-val (if unsigned? (u/exp base sz) (dec (u/exp base sz)))
         abs-num (u/abs dec-num)
         ;; When get this assert have to call with unsigned? being true
         _ (assert (<= abs-num max-abs-val) (str "Value of <" dec-num "> does not fit into <" size "> bits where leftmost is " (if unsigned? "included" "signed")))
         abs-conv (-to-digits abs-num base)
         ;_ (println "abs-conv: " abs-conv)
         bit-array (u/left-pad abs-conv 0 sz)
         ]
     (if unsigned?
       (vec bit-array)
       (let [sign-bit-on? (boolean (neg? dec-num))
             sign-bit (if sign-bit-on? 1 0)
             res (into [sign-bit] bit-array)
             _ (assert (= (count res) size))]
         res)))))

;;
;; No base b/c really is only about bits and assume base 2
;; Assumes that the first bit is a signed bit i.e. can result in negative
;; Use unsigned version instead if thats what you want
;;
(defn bit-array->dec [bit-array]
  (let [sign-bit (first bit-array)
        signed? (= sign-bit 1)
        _ (assert (or signed? (= sign-bit 0)))
        unsigned-part (next bit-array)
        dec-num (-from-type-digits unsigned-part 2)]
    (if signed?
      (* -1 dec-num)
      dec-num)))

(defn unsigned-bit-array->dec [bit-array]
  (let [dec-num (-from-type-digits bit-array 2)]
    dec-num))

;;
;; Does tricky job of changing the binary value, returning all that's needed to assoc-in
;;
;; only read from repo so pass in @tags/tags-repo
;;
(defn effect-binary-change [tags-repo array-name meta-data new-value]
  (assert (some #{0 1} [new-value]))
  (assert (= (-> meta-data last :seek/type) :bit-fill))
  (let [
        ;_ (println (str "meta-data: " meta-data))
        up-to-meta-sz (dec (count meta-data))
        up-to-meta-idx (take up-to-meta-sz (mapv :seek/idx meta-data))
        existing-key (some #(when (= (:tag/name %) array-name) %) (keys tags-repo))
        meta-idx (into [existing-key] up-to-meta-idx)
        one-prior-old-value (get-in tags-repo meta-idx)
        as-binary (seq (dec->bit-array 2 8 (:val/val one-prior-old-value) true))
        ;;_ (println (str "as-binary: " as-binary))
        end-int (-> meta-data last :seek/idx)
        reversed (vec (reverse as-binary))
        changed (reverse (assoc reversed end-int new-value))
        as-dec (bit-array->dec changed)]
    [up-to-meta-idx as-dec]))

;;
;; -128
;; For an SINT (and INT and DINT, all I think) the leftmost bit is a sign bit.
;; This means that the maximum number is 128. (2 to power of 7)
;; So -128 means that all bits are turned on.
;;
(defn t []
  (assert (= [1 1 1 1 1 1 1 1] (dec->bit-array 8 -127)))
  (assert (= [0 1 1 1 1 1 1 1] (dec->bit-array 8 127)))
  ;;
  ;; true means minus will be ignored. How else can we cope?
  ;;
  (assert (= [1 0 0 0 0 0 0 0] (dec->bit-array 2 8 -128 true)))
  (assert (= [1 0 0 0 0 0 0 0] (dec->bit-array 2 8 128 true)))
  (assert (= [0 0 0 0 0 0 0 0] (dec->bit-array 8 0)))
  (assert (= [1 0 0 0 0 0 0 1] (dec->bit-array 8 -1)))

  (assert (= -127 (bit-array->dec [1 1 1 1 1 1 1 1])))
  (assert (= 127 (bit-array->dec [0 1 1 1 1 1 1 1])))
  (assert (= 0 (bit-array->dec [0 0 0 0 0 0 0 0])))
  (assert (= -1 (bit-array->dec [1 0 0 0 0 0 0 1])))

  (assert (= 255 (unsigned-bit-array->dec [1 1 1 1 1 1 1 1])))
  (assert (= 127 (unsigned-bit-array->dec [0 1 1 1 1 1 1 1])))
  (assert (= 0 (unsigned-bit-array->dec [0 0 0 0 0 0 0 0])))
  (assert (= 129 (unsigned-bit-array->dec [1 0 0 0 0 0 0 1])))

  )

;;
;; Idea was to covert to zero as read, but that didn't work out well when found that everything goes
;; through read - including say straight integer values in instructions. Perhaps recognition that it
;; is not a tag should be done earlier, and can borrow the same function for conversion. These values
;; that are in instructions s/not be going anywhere near the cache!
;; Easy enough to see returns from get-name-type that are :raw-int-value :raw-number-value :raw-boolean-value,
;; and don't zero them.
;;
(defn zero-value [x]
  (cond
    (string? x) ""
    (number? x) 0
    (boolean? x) false
    (and (seq? x) (= 1 (count x))) '(0)
    :default (assert false (str "VAL: <" x "> type: " (type x)))))

(defn- val-set-zero [zero? x]
  (assert (or #_(= x :array-overrun) (not (keyword? x))) (str "BAD: " x (con/out-context)))
  (assert (not
            (and (map? x)
                 (or (some #{:val/val} (keys x)) (some #{:tag/res} (keys x)))))
          (str "S/not set: <" x ">, has keys: " (keys x)))
  (assert (not (vector? x)) (str "S/not set to vector: <" x ">"))
  (assert (u/is? x) (str "Can't be setting nil at " (con/out-context)))
  (if zero? (zero-value x) x))

(defn val-set [x]
  (val-set-zero dc/zero-retrieved-value? x))

(defn instr-val-set [x]
  (val-set-zero false x))

(defn se->base-two [v]
  (assert (= :base-two (first v)) (str "Only take :base-two, can't do: <" v ">"))
  (let [as-int (-> v second second Long/parseLong)
        _ (assert as-int)
        ;_ (println "as-int:" as-int)
        ]
    (seq (-to-digits as-int 2))))

;; Assuming first is :int
;; Will fail if anything else so then we will make it work for all primitive types
(defn se->int [v]
  (assert (= :int (first v)) (str "Only take :int, can't do: <" v ">"))
  (-> v second Long/parseLong))

(defn pre-pad-zeros [n s]
  (str (apply str (vec (take n (repeat "0")))) s))

;;
;; signer determines which way you shift the decimal places.
;; There will be a bug in here where zeros need to be created
;;
(defn shift-dec-places->bigdec [s n signer]
  (let [_ (assert (or (= signer -1) (= signer 1)))
        dot-at (str/index-of s ".")
        _ (assert dot-at (str "No dot found in " s))
        new-dot-at' (+ dot-at (* n signer))
        ;pad-by (calc-pad-by signer dot-at new-dot-at)
        pad-by (if (= -1 signer) (inc (u/abs new-dot-at')) 0)
        new-dot-at (+ pad-by new-dot-at')
        ;_ (println "pad-by" pad-by "new-dot-at" new-dot-at)
        before (subs s 0 dot-at)
        after (subs s (inc dot-at))
        no-dot (str before after)
        ;_ (println "pad-by" pad-by "no-dot" no-dot)
        pre-padded (pre-pad-zeros pad-by no-dot)
        new-before (subs pre-padded 0 new-dot-at)
        new-after (str/replace (subs pre-padded new-dot-at) #"-" "0")
        ;_ (println (str "<" (if (= -1 signer) "-" "") new-before ">.<" new-after ">"))
        res (* signer (du/big-dec (str (if (= -1 signer) "-" "") new-before "." new-after)))]
    res))

;;  [:def
;;   [:var "O2_Cal_Reading"]
;;   [:type [:raw-t "REAL"]]
;;   [:enc [:assign [:radix [:radix-t "Float"]]]]
;;   [:eng [:float "2.02510509"] [:eng-pos] [:int "001"]]]
;; [:eng [:float "2.02510509"] [:eng-pos] [:int "001"]]
;; [:eng [:float "1.25"] [:eng-neg] [:int "001"]]
;; [:eng [:float "3.02316099"] [:eng-neg] [:int "001"]]
(defn eng->decimal [eng]
  (assert (sequential? eng) (str "Wrong type for eng->decimal (must be vector): " eng))
  (assert (= :eng (first eng)) (str "When doing eng->decimal first s/be :eng: <" eng ">" (con/out-context)))
  (if (= 2 (count eng))
    ;; simpler version of :eng when artifically created when doing a local add-on read
    (du/big-dec "thirteen" (second eng))
    ;;(BigDecimal. (str (second eng)))
    (let [
          ;_ (println eng)
          pos? (= :eng-pos (-> eng (nth 2) first))
          float-str (-> eng second second)
          signer (if pos? 1 -1)
          shift (-> eng (nth 3) second u/string->int)]
      (shift-dec-places->bigdec float-str shift signer))))

(defn detail-type-useful? [detail-type]
  (and (not= [nil] detail-type) (not (nil? detail-type))))

(defn seq-> [find-type-fn]
  (fn [m]
    (let [_ (assert (not (map? m)) (str "Expected not map, got: <" m "> of type: <" (type m) ">" (con/out-context)))
          ;_ (println "Getting: <" m ">")
          [nav val] m
          known-type (first val)
          ;; if asking for SCADA_R_Smartgas_Sample as a whole then type that will come out for one of the attributes
          ;; will be [nil]. We are asking for the container and only have meta-data for it. The types for members are
          ;; worked out dynamically
          detail-type (find-type-fn nav)
          contains-nothing? (= known-type :single-quoted-number)
          res (when (not contains-nothing?)
                (if (detail-type-useful? detail-type)
                  (case detail-type
                    "REAL" (eng->decimal val)
                    "DINT" (se->int val)
                    "SINT" (se->int val)
                    (assert false (str "Detail type is: <" detail-type ">, was using known-type: <" known-type ">" (con/out-context))))
                  (case known-type
                    :int (se->int val)
                    :eng (eng->decimal val)
                    :base-two (se->base-two val)
                    :string (second val)
                    ;;
                    ;; ["MBTU_Transactions_00[Index].ReqBuilt"]
                    ;;
                    (assert false (str "Known type is: <" known-type ">, was using known-type: <" detail-type ">, at <" val ">" (con/out-context))))))
          _ (assert (or contains-nothing? (u/is? res)) (str "No result from: " val))]
      (if (not contains-nothing?)
        {:val/val  (val-set res)
         :val/type (if (detail-type-useful? detail-type) detail-type known-type)}
        {:val/val  nil
         :val/type nil}))))
