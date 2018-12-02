(ns debug
  (:require [clojure.string :as str]
            [utils :as u]
            [running :as run]
            [context :as con]
            [routine :as rou]))

(def watch-changes-for
  #{
    "SMRTGS_W_Switched_To_Tube_Num"
    "SCADA_R_Official_Tube_Num"
    ;"Oxygen_Purge"
    ;"A2_OXYGEN"
    ;"A2_CARBON_MONOXIDE"
    ;"A2_CO_RAW"
    ;"MB6" "MBTU_MB_3xx[101]" "MBTU_MB_3xx[102]"
    ;"A1_CO2_RAW"
    ;"A1_METHANE"
    ;"A1_CARBON_DIOXIDE"
    "Manual_Mode"
    "SCADA_R_Bag_Sampling_Input"
    "SCADA_R_Analyser_Calibrating_Auto_Input"
    "Local_Logic[3]"
    ;Way too much output
    ;"HMI_R_Gas"
    ;"O2_Calibrated_Reading"
    }
  )

;; #:val{:val 64, :type :int} => #:val{:val true, :type nil}
;; DEBUG: <#:unwrap{:complex-tag-type :raw-array, :array "Local_Logic", :index "10"}>, <1>
(defn crash-value-predicate [name old new meta]
  (let [
        ;cf (get-in new [0 0 :val/val])
        ;_ (println "CF: " cf (type cf))
        res1 (and (boolean? (:val/val new)) #_(not (= 64 cf)) (str/starts-with? name "L1"))
        res2 (= name "VAL")
        ;_ (println "CRASH?:" name meta new res)
        ]
    (or res1 res2)))

(defn inner-value [name meta built-in-property built-in-kw outer-val]
  (assert outer-val)
  (if (sequential? outer-val)
    (if meta
      (let [_ (assert meta (str "No meta supplied for: <" name ">, value: <" outer-val ">"))
            elide-meta (if (= (last meta) :val/val) (take (-> meta count dec) meta) meta)
            inner (get-in outer-val elide-meta)
            _ (assert inner (str "meta " meta " didn't help from " outer-val))
            _ (assert (map? inner) (str "Inner of <" name "> is not a map but <" inner "> using meta: <" meta ">"))
            _ (assert (some #{:val/val} (keys inner)) (str "sequential inner must be wrapped in :val/val: <" inner ">"))]
        (:val/val inner))
      (if (nil? built-in-property)
        (do
          ; Not a problem, for instance if is L1
          ;(assert false (str "No meta or built in property for: <" name ">"))
          outer-val)
        (let [
              ;_ (println (str "No meta for: <" name ">, but: <" built-in-property " " built-in-kw ">"))
              get-fn #(get % built-in-property)
              indexing-fn (-> rou/built-ins built-in-kw :indexing-fn get-fn)
              _ (assert indexing-fn)
              inner (indexing-fn outer-val)
              ;_ (println "inner: " inner)
              ]
          (:dynam/dynamic-value inner))))
    (:val/val outer-val)))

(defn inside-val [val]
  (if (and (map? val) (some #{:val/val} (keys val)))
    (:val/val val)
    val))

;;
;; First up don't worry about cardinality (put to _) or observe/meta, and see what results are like
;;
(defn observe-key [k v mta]
  (let [name (:tag/name k)]
    (when (some watch-changes-for [name])
      (let [_ (assert (= 1 (count (str/split name #"\."))) (str "To fix need to watch whole array then only report changes to: " (str/split name #"\.")))
            _ (assert (= 1 (count (str/split name #"\["))) (str "To fix need to watch whole array then only report changes to: " name))
            old-v (get @run/tags-repo k)
            ;; meta is a luxury don't need always
            _ (assert (or (nil? mta) (map? mta)) (str "meta must be a map to be observed, got: <" mta "> for <" k "> <" v ">"))
            {:keys [observe/cardinality observe/meta observe/built-in-property observe/built-in-kw]} mta
            _ (assert (u/is? name))]
        (if (not= cardinality :single)
          (let [convert (partial inner-value name meta built-in-property built-in-kw)
                old (when old-v (convert old-v))
                new (convert v)]
            (assert (not (nil? new)) (str "Unlikely setting to nil for: <" k ">, <" v "> <" mta ">"))
            (assert (not (crash-value-predicate name old new meta))
                    (str "Existing tag info for " name ". Existing value: <" old ">. New value want: <" new ">, meta: <" meta "> " (con/out-context)))
            (when (= name "Switching_Timer") (assert built-in-property))
            (println (str name " :M: (" built-in-property ") " (if (nil? old) "nil" old) " => " new (con/out-context)))
            (assert (not= old new) (str "No change from <" old "> to <" new "> when :M:" (con/out-context))))
          (let [_ (assert (not (crash-value-predicate name old-v v meta)))
                old-single-v (get-in old-v meta)
                ;_ (assert (not (vector? old-v)) (str "Got vector <" old-v ">, info: <" mta ">"))
                _ (assert (not= old-single-v v) (str "No change from <" old-single-v "> to <" v "> when :S:" (con/out-context) ", meta: " meta))
                _ (assert (nil? built-in-property))
                ]
            (println (str name " :S: (" meta ") " (inside-val old-single-v) " => " (inside-val v) (con/out-context)))))))))
