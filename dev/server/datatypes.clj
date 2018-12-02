(ns datatypes
  (:require [utils :as u]
            [clojure.string :as str]
            [context :as con]
            [meta :as meta]))

(defn next-datatype-element [datatypes content-datatype current-element]
  (assert (seq datatypes))
  (let [datatype (first (filter #(= content-datatype (-> % second second)) datatypes))
        _ (assert datatype (str "Not found custom data type: <" content-datatype ">" (con/out-context)))
        contents (drop 3 datatype)
        indexed-contents (map-indexed vector contents)
        current (first (filter #(= current-element (-> % second (nth 2) second)) indexed-contents))
        _ (assert current (str "Not managed to get a current <" current-element "> when looking through: <" (vec indexed-contents) ">"))
        next-num (inc (first current))
        next-var (some #(when (= next-num (first %)) (-> % second (nth 2) second)) indexed-contents)]
    ;(println "contents: " contents)
    ;(println "next-var: " next-var)
    next-var)
  )

(defn data-input? [x]
  (let [coll (str/split x #":")
        ;_ (println coll)
        res (and (= 3 (count coll)) (= (first coll) "Local") (= (nth coll 2) "I.Data"))
        ]
    res))

(defn channel-input? [x]
  (let [_ (assert (not (map? x)) (str "Supposed be a string but got: <" (seq x) ">" (con/out-context)))
        coll (str/split x #":")
        ;_ (println coll)
        last-part #(nth coll 2)
        res (and (= 3 (count coll))
                 (= (first coll) "Local")
                 (= (apply str (take 4 (last-part))) "I.Ch")
                 (not (str/index-of (last-part) "range")))
        ]
    res))

(defn array? [x]
  (and (map? x) (= (:unwrap/complex-tag-type x) :raw-array)))

(defn channel-underrange-input? [x]
  (let [coll (str/split x #":")
        ;_ (println coll)
        last-part #(nth coll 2)
        res (and (= 3 (count coll))
                 (= (first coll) "Local")
                 (= (apply str (take 4 (last-part))) "I.Ch")
                 (str/index-of (last-part) "Underrange"))
        ]
    res))

(defn channel-overrange-input? [x]
  (let [coll (str/split x #":")
        ;_ (println coll)
        last-part #(nth coll 2)
        res (and (= 3 (count coll))
                 (= (first coll) "Local")
                 (= (apply str (take 4 (last-part))) "I.Ch")
                 (str/index-of (last-part) "Overrange"))
        ]
    res))

(defn data-output? [x]
  (let [coll (str/split x #":")
        res (and (= 3 (count coll)) (= (first coll) "Local") (= (nth coll 2) "O.Data"))
        ]
    res))

;;
;; Problem with HMI_R_PLC.S7 as there is no S8 and can't do 16 of them:
;; COP(Local:7:I.Ch0Data,HMI_R_PLC.S7,16);
;;
;; Normally equiv instruction looks like:
;; COP(Local:5:I.Ch0Data,L1.S5.AI00,16);
;; I'm assuming the above is copying 16 lots of 32 bits across
;; (AI00 is a REAL, which is 32 bits)
;; (And Ch0Data is an analgue input, same as a REAL)
;;
;; Or perhaps like:
;; COP(Local:2:O.Data,HMI_R_PLC.S2,1);
;; I'm assuming the above is copying all 32 bits across
;; (Data and S2 are 32 bits)
;;
;; S7 S5 S2 are all containers
;; S7 and S5 are all IF16, made up of 16 REALs, from AI00 to AI15
;; S2 is an OB32, which is 4 lots of 8 bits (32 altogether)
;; Local:2:O.Data is 32 lights
;;
;; Makes sense if it was meant to be:
;; COP(Local:7:I.Ch0Data,HMI_R_PLC.S7.AI00,16);
;;
;; Or else this language is smart enough to know to look to the deepest 'byte' (even 32 bit byte), so automatically
;; goes to AI00 to start the copying. Unlikely.
;;
(defn next-tag [infos datatypes tag]
  ;(assert nxt-dest-tag (str "No nxt-dest-tag found after: <" to ">"))
  (let [coll (str/split tag #"\.")
        ;_ (assert (some #{3 2} [(count coll)]) (str "Will have to alter this function for: <" tag ">" (con/out-context) ", b/c coll is: " coll))
        [head & tail] coll
        _ (assert (some #{:program-info} (keys infos)) (str "Not found :program-info: <" (keys infos) ">"))
        containing-type (meta/find-containing-type infos head)
        ]
    (when (some #{3 2} [(count coll)])
      (case (count coll)
        3 (let [leading (apply str (interpose "." (take 2 coll)))
                how-to-seek-3 (meta/number-seek datatypes containing-type tail false)
                content-datatype-3 (-> how-to-seek-3 first :seek/type)
                nxt-tag (next-datatype-element datatypes content-datatype-3 (last tail))]
            (str leading "." nxt-tag))
        2 (let [nxt-tag (next-datatype-element datatypes containing-type (last tail))
                _ (assert nxt-tag (str "No nxt-dest-tag found after: <" containing-type " then " (last tail) ">"))
                ]
            (str head "." nxt-tag))))))

(defn next-in [infos datatypes tag length]
  (cond
    (array? tag)
    (let [{:keys [unwrap/array unwrap/index]} tag
          new-index (-> index u/string->int inc str)
          res (assoc tag :unwrap/index new-index)]
      res)

    (channel-input? tag)
    (let [[before after] (str/split tag #"\.")
          ch-num (u/string->int (u/between "Ch" "Data" after))]
      (str before "." "Ch" (inc ch-num) "Data"))

    ;; "Local:2:O.Data"
    (or (data-input? tag) (data-output? tag))
    (let [_ (assert (= 1 length))]
      tag)

    ;; HMI_R_PLC.S7, or even Null_Char, in which case will return nil
    :default
    (next-tag infos datatypes tag)))

;;
;; When add one make sure to look for all usages of the others and copy same
;; Look at make-message in case need to do similar
;;
(def accumulated "ACC")
(def preset "PRE")
(def done? "DN")
(def timing? "TT")

(def path "Path")
(def er "ER")

(def len "LEN")

(defn- built-in-property? [dot-name]
  (some #{accumulated preset done? timing? path er len} [dot-name]))

;;
;; Where we decide that the tag that is being read is interesting, if this can be done just from
;; looking at it.
;; map values that are :complex-tag-type never get stored in the cache.
;; (however their constituent parts are in the cache)
;; Values that see in the cache are from now on going to have their keys as maps.
;; Every single key is going to change from being string/array/whatever to being {}
;; where [:tag/name :tag/type] are the keys.
;; So this change is orthogonal to :complex-tag-type - in both past and future
;;
(defn wrap-arg [arg]
  (if (map? arg)
    (do
      (assert (some #{:unwrap/complex-tag-type} (keys arg)) (str "unwrap-arg with:" arg))
      arg)
    (let [
          ;_ (println "unwrap-arg with:" arg)
          dot-split-up (str/split arg #"\.")
          sz (count dot-split-up)
          ;_ (println "dot-split-up: " dot-split-up)
          base-name (first dot-split-up)
          no-dots? (= base-name arg)
          dot-name (last dot-split-up)
          index-by (utils/between "[" "]" arg)
          dot-comes-before? (and (and (not no-dots?) index-by) (< (str/index-of arg ".") (str/index-of arg "[")))
          ;;_ (assert (or (not index-by) (= 1 (count (filter #(= % (first "[")) arg)))) (str "So far only handling one set of brackets" (con/out-context)))
          ;; Rubbish - the simple type handles as many dots as there are, for even complex types for example: L1.S4.DI.0
          ;;_ (assert (or no-dots? (= 1 (count (filter #(= % (first ".")) arg)))) (str "So far only handling one dot: " arg))
          var-type (cond
                     (and (not no-dots?) (= 2 (count (filter #(= % (first "[")) arg))) (u/ends-with? arg "]"))
                     :single-idx-array-dot-array

                     (and (not no-dots?) (nil? index-by) (= 2 (count dot-split-up)) (built-in-property? dot-name))
                     :built-in

                     ;; "SCADA_R_Sys.Alm[0]"
                     (and (not no-dots?) index-by dot-comes-before?)
                     :idx-within-attribute

                     ;; Main_Read_Buffer[0].LEN
                     (and (not no-dots?) index-by (built-in-property? dot-name))
                     :built-in-single-idx-array-dot

                     ;; SCADA_R_Readings[SCADA_R_Official_Tube_Num].Seconds_Timestamp
                     ;; "SCADA_R_Readings[Readings_Index_For_Age].Age"
                     (and (not no-dots?) index-by)
                     :single-idx-array-dot

                     (and (not no-dots?) (not (u/string->int-not-strict base-name)) (u/string->int-not-strict dot-name))
                     :number-ending

                     ;(and (not no-dots?) (not index-by))
                     ;:into-custom-type

                     (and no-dots? index-by)
                     :raw-array

                     ;; Here a built in (counter) is coming out as a :simple
                     ;; Ch_Faults
                     (nil? index-by)
                     :simple

                     :default
                     (assert false (str "Can't unwrap arg: " arg))
                     )
          ;_ (println (str "var type: " var-type " from <" arg "> split: <" dot-split-up ">"))
          ]
      (case var-type
        :simple arg
        :raw-array {
                    :unwrap/complex-tag-type :raw-array
                    :unwrap/array            (utils/before "[" arg)
                    :unwrap/index            index-by
                    }
        :idx-within-attribute {:unwrap/complex-tag-type :idx-within-attribute
                               :unwrap/array            (utils/before "." arg)
                               :unwrap/index            index-by
                               :unwrap/dot-member       (u/probe-off dot-name)}
        :single-idx-array-dot {:unwrap/complex-tag-type :single-idx-array-dot
                               :unwrap/array            (utils/before "[" arg)
                               :unwrap/index            index-by
                               :unwrap/dot-member       dot-name}
        :built-in-single-idx-array-dot {:unwrap/complex-tag-type :built-in-single-idx-array-dot
                                        :unwrap/array            (utils/before "[" arg)
                                        :unwrap/dot-member       dot-name
                                        :unwrap/index            index-by}
        :single-idx-array-dot-array {:unwrap/complex-tag-type :single-idx-array-dot-array
                                     :unwrap/array            (utils/before "[" arg)
                                     :unwrap/index            index-by
                                     :unwrap/second-index     (str (nth arg (dec (str/last-index-of arg "]"))))
                                     :unwrap/dot-member       (utils/before "[" dot-name)}
        :number-ending {:unwrap/complex-tag-type :number-ending
                        :unwrap/array            (utils/before "." arg)
                        :unwrap/middle           (take (- sz 2) (drop 1 dot-split-up))
                        :unwrap/dot-member       (u/probe-off dot-name)}
        :built-in {:unwrap/complex-tag-type :built-in
                   :unwrap/array            base-name
                   :unwrap/dot-member       dot-name
                   ;; At this stage we don't know if it is a counter, timer etc so, so can't say that because the
                   ;; dot-member is "ACC", which could belong to either
                   :unwrap/dot-member-idx   nil}))))

;;
;; copying is a side effect and there's no reason to mount up all elements as they are copied, so returns nil
;;
(defn copy [infos datatypes read-tag write-tag source-tag destination-tag total-size]
  (loop [from source-tag
         to destination-tag
         idx 0]
    (let [from-content (read-tag from)]
      (write-tag (wrap-arg to) from-content)
      (when (< (inc idx) total-size)
        (let [nxt-src-tag (next-in infos datatypes from total-size)
              nxt-dest-tag (next-in infos datatypes to total-size)
              _ (assert nxt-dest-tag (str "No nxt-dest-tag found after: <" to ">"))]
          (recur nxt-src-tag nxt-dest-tag (inc idx)))))))

(defn fill [infos datatypes tag-value write-tag destination-tag total-size]
  (loop [to destination-tag
         idx 0]
    (when (< idx total-size)
      (write-tag (wrap-arg to) tag-value)
      ;(println (str "\nDone copy (in fill): <" tag-value ">, to: <" to "> where count-in-array will be <" total-size ">"))
      (let [nxt-dest-tag (next-in infos datatypes to total-size)]
        (recur nxt-dest-tag (inc idx))))))
