(ns sys.domain-utils
  (:require [io-util :as io]
            [convert :as conv]
            [query :as q]))

(defn- apply-change [program change]
  (let [
        {:keys [routine-name rung-num expected-rung replace-with-rung]} change
        [rou-idx routine] (q/query-routine program routine-name)
        rung-idx (q/query-rung-idx routine expected-rung)
        _ (assert (or (nil? rung-num) (= rung-idx rung-num)))
        ; pointless
        ;_ (q/mutate-routine info rou-idx replace-with-rung routine-name)
        altered-info (q/mutate-rung program rou-idx rung-idx replace-with-rung)
        ]
    altered-info))

;;
;; Returns a program that has applied the changes
;;
(defn apply-changes [program changes]
  (reduce
    (fn [acc-program ele-change]
      (apply-change acc-program ele-change))
    program
    changes))

;;
;; idx is either 0,1,2 or 3 - for which 8 out of the 32 bits we need to turn into a decimal integer (Long in Clojure)
;; Everything comes in from right - opposite direction for both bits and bytes.
;; So if idx is 0 we want rightmost, which means drop 3 (* 8).
;; 0 drop 3
;; 1 drop 2
;; 2 drop 1
;; 3 drop 0
;;
(defn byte-as-sint [v idx]
  (let [drop-count (- 3 idx)
        our-bits (take 8 (drop (* 8 drop-count) v))]
    (conv/unsigned-bit-array->dec our-bits)))

(defn four-incoming-bytes [output]
  [{:val/val (byte-as-sint output 3)} {:val/val (byte-as-sint output 2)} {:val/val (byte-as-sint output 1)} {:val/val (byte-as-sint output 0)}])

;; Needs explanation. Output slots are flexible in that they can be set from outside as well. i.e. they can act as input slots.
;; Here we should be returning what they have been set to. They are OB32 - meaning 4 8 bit bytes.
(defn create-output-input-data-fn-for-slot [output]
  (fn [] (mapv #(assoc % :val/type :int) (four-incoming-bytes output))))

(defn create-output-data-fn-for-slot [out-at-atom slot-num]
  (fn [outgoing]
    (let [
          ;_ (when (= slot-num 2) (println "create output from " outgoing " for " slot-num))
          to-digits #(conv/dec->bit-array 2 (conv/byte-size "DINT") % true)
          bit-formatted (-> outgoing :val/val to-digits vec)]
      (swap! out-at-atom assoc slot-num bit-formatted))))


(defn initial-values [slot-size slot-numbers]
  (into {} (map (fn [num] [num (vec (repeat slot-size 0))]) slot-numbers)))

(def test-old {2 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0], 3 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]})
(def test-new {2 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0], 3 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0 0 0 0 0 0 0 0 1]})
;;
;; Takes the vals, keep-indexed where the vals are different. Then can use these indexes to
;; work out which keys to return. For good measure we check that the keys to both are the same.
;;
(defn changed-slots [old-state new-state]
  (let [old-keys (keys old-state)
        ;_ (println old-keys)
        _ (assert (pos? (count old-keys)))
        new-keys (keys new-state)
        _ (assert (= old-keys new-keys))
        old-binary-arrays (vals old-state)
        new-binary-arrays (vals new-state)
        together (map vector old-binary-arrays new-binary-arrays)
        ;_ (println "together: " together)
        diffs (keep-indexed (fn [idx [old new]] (when (not= old new) idx)) together)
        ;_ (println "diffs: " diffs)
        ]
    (map #(nth old-keys %) diffs)))

(defn t []
  (changed-slots test-old test-new))

(defn is? [x]
  (assert (int? x))
  (assert (some #{0 1} [x]))
  (cond
    (= x 1) true
    (= x 0) false))

(defn is-not? [x]
  (not (is? x)))

(defn log-name [site-name slot-num]
  (str site-name "_S" slot-num ".log"))

(defn append-slot-change [site-name slot-num new-state]
  (assert (number? slot-num))
  (assert new-state)
  (io/append-to-file (log-name site-name slot-num) (str (get new-state slot-num) "\n")))

;;
;; Create a watch so when there's a change in the atom we work out which slot
;; the change comes from and write to the output file
;;
(defn write-file-on-change [site-name]
  (fn [key atom old-state new-state]
    (when (not= old-state new-state)
      (let [slots (changed-slots old-state new-state)]
        (mapv #(append-slot-change site-name % new-state) slots)
        ; Do gt a proper stack trace
        ;(assert false)
        )
      )))

(defn output [slot-size site-name slot-numbers]
  (let [out-at (atom (initial-values slot-size slot-numbers))
        hardware-output-data-functions (into {} (map (fn [num] [num (create-output-data-fn-for-slot out-at num)]) slot-numbers))
        hardware-output-input-data-functions (into {} (map (fn [num] [num (create-output-input-data-fn-for-slot (get @out-at num))]) slot-numbers))
        _ (mapv #(append-slot-change site-name % @out-at) slot-numbers)
        ]
    (add-watch out-at :watcher (write-file-on-change site-name))
    {:hardware-output-data-functions hardware-output-data-functions
     :hardware-output-input-data-functions hardware-output-input-data-functions
     :atom out-at}))

