(ns meta
  (:require [clojure.string :as str]
            [utils :as u]
            [convert :as conv]
            [context :as con]))

(defn zzz-idx [contents seek-var]
  (let [bit-fill (first (filter #(= :bit-fill (first %)) contents))
        zzz-name (-> bit-fill (nth 2) second)
        remaining (remove #(= :bit-fill (first %)) contents)
        where-at (first (u/positions #(= zzz-name (-> % (nth 2) second)) remaining))]
    ;(println "bit-fill zzz-name:" zzz-name)
    ;(println "remaining:" remaining)
    ;(println "where-at:" where-at)
    where-at))

;;
;; Don't have to do this with built in types, because the number is static i.e. "ACC" is 0 for a COUNTER
;;
;; Given a top-type and a dotted tag made up of all the elements to the end (that's v), return how you would
;; trace throu the tags and datatypes to get there. For each step there's an idx and a type.
;;
(defn number-seek [datatypes top-type v -debug?]
  (assert (not (integer? (last v))) "Last one is often an integer, but should be chopped off")
  (assert (seq datatypes))
  (if false #_(not (seq v))
    (let [_ (assert (not (vector? top-type)))
          datatype (first (filter #(= top-type (u/probe-off (-> % second second) (str "reviewing against: <" top-type ">"))) datatypes))
          contents (drop 3 datatype)
          types (mapv (comp second second) contents)
          _ (println (str "At: " top-type ", types: " types))
          ]
      types)
    (if (conv/raw-type? top-type)
      []
      (loop [seek-type top-type
             ensuing-vars v
             res []]
        (let [debug? (or -debug? (not (seq ensuing-vars)))
              probe (if debug? u/probe-on u/probe-off)
              _ (assert (seq ensuing-vars) (str "No ensuing-vars at: <" seek-type "> when already have <" res ">" (con/out-context)))
              datatype (first (filter #(= seek-type (u/probe-off (-> % second second) (str "reviewing against: <" seek-type ">"))) datatypes))
              _ (assert datatype (str "Not found custom data type: <" seek-type ">, started with: <" top-type ">" (con/out-context)))
              contents (drop 3 datatype)
              _ (when debug? (println (str "--> At: " seek-type ", contents: " (u/pp-str contents))))
              fill-type-kw (-> contents first first)
              end-it? (not= :custom-fill fill-type-kw)
              _ (when debug? (println "--> To end: " end-it?))
              seek-var (first ensuing-vars)
              ;_ (println "seek: <" seek-var ">")
              up-to (take-while #(not= seek-var (-> % (nth 2) second)) contents)
              ;_ (println (str "Up to " seek-var " is " (vec up-to)))
              nothing-found? (= (count up-to) (count contents))
              _ (when debug? (println "--> nothing-found: " nothing-found?))
              idx (count up-to)
              ]
          (if nothing-found?                                ;; We are either going to crash or end it
            (let [
                  ;;first second second will get us enabled here
                  ;[[:bit-fill [:var "Enabled"] [:var "ZZZZZZZZZZTube_Seq_I2"] [:integer "0"] [:enc [:assign [:description]]]]]
                  ;;first second second will get us A01 here
                  ;[[:bit-fill [:var "A01"] [:var "ZZZZZZZZZZGas_Alm0"] [:integer "0"] [:enc [:assign [:description]]]] [:bit-fill [:var "A02"] [:var "ZZZZZZZZZZGas_Alm0"] [:integer "1"]] [:bit-fill [:var "A03"] [:var "ZZZZZZZZZZGas_Alm0"] [:integer "2"]] [:bit-fill [:var "A04"] [:var "ZZZZZZZZZZGas_Alm0"] [:integer "3"]] [:bit-fill [:var "A05"] [:var "ZZZZZZZZZZGas_Alm0"] [:integer "4"]] [:bit-fill [:var "A06"] [:var "ZZZZZZZZZZGas_Alm0"] [:integer "5"]] [:bit-fill [:var "A07"] [:var "ZZZZZZZZZZGas_Alm0"] [:integer "6"]] [:bit-fill [:var "A08"] [:var "ZZZZZZZZZZGas_Alm0"] [:integer "7"]] [:bit-fill [:var "A09"] [:var "ZZZZZZZZZZGas_Alm9"] [:integer "0"]] [:bit-fill [:var "A10"] [:var "ZZZZZZZZZZGas_Alm9"] [:integer "1"]] [:bit-fill [:var "A11"] [:var "ZZZZZZZZZZGas_Alm9"] [:integer "2"]] [:bit-fill [:var "A12"] [:var "ZZZZZZZZZZGas_Alm9"] [:integer "3"]] [:bit-fill [:var "A13"] [:var "ZZZZZZZZZZGas_Alm9"] [:integer "4"]] [:bit-fill [:var "A14"] [:var "ZZZZZZZZZZGas_Alm9"] [:integer "5"]] [:bit-fill [:var "A15"] [:var "ZZZZZZZZZZGas_Alm9"] [:integer "6"]] [:bit-fill [:var "A16"] [:var "ZZZZZZZZZZGas_Alm9"] [:integer "7"]] [:bit-fill [:var "A17"] [:var "ZZZZZZZZZZGas_Alm18"] [:integer "0"]] [:bit-fill [:var "A18"] [:var "ZZZZZZZZZZGas_Alm18"] [:integer "1"]] [:bit-fill [:var "A19"] [:var "ZZZZZZZZZZGas_Alm18"] [:integer "2"]] [:bit-fill [:var "A20"] [:var "ZZZZZZZZZZGas_Alm18"] [:integer "3"]] [:bit-fill [:var "A21"] [:var "ZZZZZZZZZZGas_Alm18"] [:integer "4"]] [:bit-fill [:var "A22"] [:var "ZZZZZZZZZZGas_Alm18"] [:integer "5"]] [:bit-fill [:var "A23"] [:var "ZZZZZZZZZZGas_Alm18"] [:integer "6"]] [:bit-fill [:var "A24"] [:var "ZZZZZZZZZZGas_Alm18"] [:integer "7"]] [:bit-fill [:var "A25"] [:var "ZZZZZZZZZZGas_Alm27"] [:integer "0"]] [:bit-fill [:var "A26"] [:var "ZZZZZZZZZZGas_Alm27"] [:integer "1"]] [:bit-fill [:var "A27"] [:var "ZZZZZZZZZZGas_Alm27"] [:integer "2"]] [:bit-fill [:var "A28"] [:var "ZZZZZZZZZZGas_Alm27"] [:integer "3"]] [:bit-fill [:var "A29"] [:var "ZZZZZZZZZZGas_Alm27"] [:integer "4"]] [:bit-fill [:var "A30"] [:var "ZZZZZZZZZZGas_Alm27"] [:integer "5"]] [:bit-fill [:var "A31"] [:var "ZZZZZZZZZZGas_Alm27"] [:integer "6"]] [:bit-fill [:var "A32"] [:var "ZZZZZZZZZZGas_Alm27"] [:integer "7"]]]
                  bit-fills (filter #(= :bit-fill (first %)) contents)
                  _ (assert (seq bit-fills) (str "Nothing found for <" top-type "> for <" v ">"))
                  ;_ (println (str "bit-fills: " (vec bit-fills)))
                  bit-seek-idxs (u/positions #(= seek-var (-> (u/probe-off % "PROBE") second second)) bit-fills)
                  _ (assert (seq bit-seek-idxs) (str (vec bit-seek-idxs) ", when nothing found expect to find it: <" seek-var ">, as a :bit-fill in here: " (vec bit-fills)))
                  bit-seek-idx (first bit-seek-idxs)
                  ;;
                  ;; A particular bit might be many bytes across. The first number of last-parts gives us that. Often it will be 0
                  ;; because there are 8 or less altogether. We need to add to this number the position where the SINT is overall.
                  ;; So for example Enabled of Tube_Seq_Item is part of ZZZZZZZZZZTube_Seq_I2, which is third when you include all
                  ;; the fills that are not BIT fills. Thus although Enabled is in the first byte, the first byte is actually at
                  ;; 3rd position, coming after Tube_Num and Duration. zzz-idx works out the initial number of jumps.
                  ;;
                  initial-hops (zzz-idx contents seek-var)
                  last-parts [{:seek/idx (+ initial-hops (quot bit-seek-idx 8)) :seek/type :bit-container} {:seek/idx (rem bit-seek-idx 8) :seek/type :bit-fill}]
                  result (into res last-parts)
                  ;_ (println (str "initial-hops: " initial-hops))
                  ;_ (println (str "res: " res))
                  ;_ (println (str "seek idx: " bit-seek-idx))
                  ;_ (println (str "Looking for " seek-var))
                  ;_ (println (str "orig contents: <" (vec contents) ">"))
                  ;_ (println (str "bit fills: <" (vec bit-fills) ">"))
                  ;_ (println (str "indexes: " (vec bit-seek-idxs)))
                  ;_ (println (str "result: " result))
                  ;_ (assert false)
                  ]
              result)
            (if end-it?
              (let [last-part {:seek/idx idx :seek/type (-> datatype (nth (+ 3 idx)) second second)}
                    _ (when debug? (println "contents: <" contents ">"))]
                (conj res last-part))
              (let [now-seek-type (-> (nth contents idx) second second)
                    more-vars (next ensuing-vars)]
                (if (seq more-vars)
                  (recur now-seek-type more-vars (conj res {:seek/idx idx :seek/type (-> datatype (nth (+ 3 idx)) second second)}))
                  (conj res {:seek/idx idx :seek/type (-> datatype (nth (+ 3 idx)) second second)}))))))))))

(defn top-type [top-def]
  (let [type-if-simple (-> top-def (nth 2) second second)]
    (if (string? type-if-simple)
      type-if-simple
      (let [res (second type-if-simple)
            ;_ (println "top-type: <" res "> from <" type-if-simple ">")
            _ (assert (string? res))]
        res))))

(def tag-by-name-q (comp second second))

(defn assimilate-tag-defs [info]
  (let [{:keys [program-info controller-info]} info
        defs (:program/defs program-info)
        controller-tag-defs (:controller-tag-defs controller-info)
        _ (assert (seq defs))
        _ (assert (seq controller-tag-defs) (keys info))
        tag-defs (concat defs controller-tag-defs)
        ]
    tag-defs
    ))

(defn find-containing-type [info top-level-array]
  (let [_ (assert (and (:program-info info) (:controller-info info)))
        _ (assert (string? top-level-array) (str "Expected string. got: <" top-level-array ">, type: <" (type top-level-array) ">"))
        _ (assert (= 1 (count (str/split top-level-array #":"))) (str "Bad array: <" top-level-array ">. Ought to have no colons"))
        _ (assert (= 1 (count (str/split top-level-array #"\."))) (str "Bad array: <" top-level-array ">. Ought to have no dots"))
        tag-defs (assimilate-tag-defs info)
        _ (assert (> (count tag-defs) 3))
        top-def (first (filter #(= top-level-array (-> % tag-by-name-q)) tag-defs))
        containing-type (u/probe-off (top-type top-def))
        ;_ (println "top-def ->" top-def "has type" containing-type)
        ;_ (println (str "containing-type is: <" containing-type ">"))
        ]
    containing-type))

(defn seek-meta [infos head tail]
  (let [containing-type (find-containing-type infos head)
        res (when (seq tail)
              (number-seek (-> infos :controller-info :datatypes) containing-type tail false))]
    res))

(defn up-to-byte-value [datatypes containing-type nearly-to-end array-value]
  (let [how-to-seek (number-seek datatypes containing-type nearly-to-end false)
        ;; Might need some work (menage of what used to be in value-from-number-ended and value-from-custom)
        bit-contained-at (first (keep-indexed #(when (= (:seek/type %2) :bit-container) %1) how-to-seek))
        ;bit-contained? (= :bit-container (-> how-to-seek first :seek/type))
        ;_ (assert (not bit-contained?) (str "Don't expect to be bit contained for: " nearly-to-end " | " how-to-seek (con/out-context)))
        display-debug? false
        _ (when display-debug?
            (println (str "look into: " array-value))
            (println (str "how-to-seek: " how-to-seek))
            (println (str "bit-contained-at: " bit-contained-at)))
        ;meta-idx (mapv :seek/idx how-to-seek)
        meta-idx (mapv :seek/idx (take (if bit-contained-at (inc bit-contained-at) 2) how-to-seek))
        _ (when display-debug? (println "meta-idx: " meta-idx))
        res (get-in array-value meta-idx)
        _ (when display-debug? (println (str "value-from-number-ended RES: <" res ">")))
        ]
    [res meta-idx bit-contained-at how-to-seek]))

;;
;; The end number is a bit into a number. Here we return either 0 or 1 depending on whether that bit is set
;;
(defn value-from-number-ended
  [{:keys [controller-info] :as info}
   {:keys [unwrap/array unwrap/middle unwrap/dot-member] :as tag}
   array-value
   find-containing-type-fn]
  (let [containing-type (find-containing-type-fn info array)
        _ (assert containing-type (str "Not found a containing type from: <" array ">"))
        nearly-to-end (vec middle)
        [res meta-idx] (if (seq nearly-to-end)
                         (up-to-byte-value (:datatypes controller-info) containing-type nearly-to-end array-value)
                         [array-value nil])
        ;_ (println (str "meta-idx could use: " meta-idx " for " containing-type ", " (seq middle) ", " dot-member))
        byte-size (-> res :val/type conv/byte-size)
        dynamic-value (conv/dec->bit byte-size (:val/val res) (u/string->int dot-member))
        ]
    {:dynam/dynamic-value dynamic-value
     :dynam/array-value   array-value
     :dynam/value-type    (:val/type (get-in array-value meta-idx))
     :dynam/meta-idx      meta-idx
     :dynam/meta-idx-res  (:val/val res)
     :dynam/dot-member    (u/string->int dot-member)
     }))

;;
;; Get value of particular bit
;;
(defn- bit-into [num idx]
  ;(println "bit-into " num idx)
  (assert num)
  (assert idx)
  (let [as-binary (seq (conv/dec->bit-array 2 8 num true))
        reversed (reverse as-binary)
        ending-res (nth reversed idx)
        _ (assert (u/is? ending-res))
        ]
    ending-res))

(defn value-from-custom
  [{:keys [program-info controller-info] :as info}
   {:keys [unwrap/array unwrap/dot-member] :as tag}
   array-value
   find-containing-type-fn]
  (let [{:keys [datatypes]} controller-info
        containing-type (find-containing-type-fn info array)
        _ (assert containing-type (str "Not found a containing type from: <" array ">"))
        [res meta-idx bit-contained-at how-to-seek] (if (seq dot-member)
                                                      (up-to-byte-value datatypes containing-type dot-member array-value)
                                                      [array-value nil nil nil])
        _ (assert res (str "Could not find <" meta-idx "> (from <" dot-member ">) in <" array-value ">"))
        ;_ (println (str "value-from-custom RES: <" res ">, bit-contained? " bit-contained-at ", how-to-seek: " how-to-seek))
        dynamic-value (if bit-contained-at
                        (let [num (:val/val res)
                              _ (assert num (str "Never found a num inside: <" res ">"))]
                          (bit-into num (-> how-to-seek last :seek/idx)))
                        (if (sequential? res)
                          res
                          (:val/val res)))
        _ (assert (u/is? dynamic-value) (str "Got nothing from <" res ">, bit contained: <" bit-contained-at "> using <" meta-idx "> on <" array-value ">, tag: <" tag ">"))
        ]
    {:dynam/dynamic-value dynamic-value
     :dynam/array-value   array-value
     :dynam/value-type    (:val/type (get-in array-value meta-idx))
     :dynam/meta-idx      meta-idx
     }))

;;
;; We have just looked up, and keep in cache, the name/value of the array variable and
;; the name/value of the index to be looked up. We have the data to look into right here: array-value
;; The missing piece we don't have is the `.Ethane` at the end, or rather the index that it is.
;; We could cache this association (Ethane is 5th in Gases). But for now we are always going to look
;; it up.
;; See `unwrap-arg` for where the tag initially comes out of thin air - where we first decide it is not as
;; simple as the name of the tag is to be cachced - but instead only part/s to be cachced and both reading
;; and writing to deal with maps, each of which will have a :complex-tag-type
;;
(defn value-from-array-of-custom
  [{:keys [program-info controller-info] :as info}
   {:keys [unwrap/array unwrap/index unwrap/dot-member] :as tag}
   array-value
   index-value]
  (let [
        _ (assert controller-info)
        _ (assert (map? index-value) (str "index-value is not a map: <" index-value ">"))
        _ (assert (some #{:val/val} (keys index-value)))
        _ (assert (-> index-value :val/val integer?) (str "index-value not integer, is: " (-> index-value :val/val type)))
        how-to-seek (seek-meta info array [dot-member])
        bit-contained? (= :bit-container (-> how-to-seek first :seek/type))
        display-debug? (and false bit-contained?)
        _ (when display-debug? (println "look into: " array-value))
        _ (when display-debug? (println "how-to-seek: " how-to-seek))
        meta-idx [(:val/val index-value) (:seek/idx (first how-to-seek))]
        _ (when display-debug? (println "meta-idx: " meta-idx))
        res (get-in array-value meta-idx)
        _ (assert (not (nil? res)) (str "Nothing found at <" meta-idx "> for <" array ">, looking through: <" (seq array-value) ">" (con/out-context)))
        ;_ (println "value-from-array-of-custom RES:" res)
        dynamic-value (if (nil? res)
                        :array-overrun
                        (if bit-contained?
                          (bit-into (:val/val res) (-> how-to-seek last :seek/idx))
                          (:val/val res)))
        _ (assert (u/is? dynamic-value) (str "Got nothing from " res ", bit contained: " bit-contained? ", b4 unwrap: <" res ">"))
        ]
    ;; Enough info to do our indirect updating
    {:dynam/dynamic-value dynamic-value
     :dynam/array-value   array-value
     :dynam/value-type    (if (= dynamic-value :array-overrun)
                            :array-overrun
                            (:val/type (get-in array-value meta-idx)))
     :dynam/meta-idx      meta-idx
     }))
