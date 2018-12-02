(ns user
  (:require [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [parse.parsing :as par]
            [utils :as u]
            [parse.edn-writer :as ew]
            [clojure.zip :as zip]
            [query :as q]
            [clojure.stacktrace]
            [sys.system :as sys]))

;;
;; Does not reset! any atoms
;;
(defn reset []
  ;(use 'clojure.stacktrace)
  (refresh)
  )

;;
;; Clears all tags. So does much more than normally is done with stopping a PLC. Is more
;; equivalent to turning the power off.
;;
(defn stop-reset []
  (sys/stop)
  (reset))

;;
;; I need to somehow require it for the compiler to pick it up, for now do it manually
;;
#_(defn s []
    (print-stack-trace *e 30))

(def z (zip/vector-zip ew/structure))
(def locs (take-while (complement zip/end?) (iterate zip/next z)))

#_(defn visit-all []
    (doseq [loc locs]
      (let [node (zip/node loc)]
        (when (keyword? node)
          (println node "has parent" (parent-of loc))))))

(defn sparse-format [x]
  [(-> x :input :name) (-> x :input keys) "ID" (:id x) "PARENT_ID" (:pid x)])

(defn huge-format [x]
  x)

(defn view-loc [format-fn loc]
  (let [node (zip/node loc)]
    (if (map? node)
      (let [res (:result node)]
        (map format-fn res))
      nil)))

;;
;; Returns a coll of 'what's wrong?' objects (ie maps)
;;
(defn check-loc [loc]
  (let [node (zip/node loc)]
    (if (map? node)
      (let [res (:result node)
            _ (assert res (str "Always expect result. None for " (:name node) "," (:bnf node)))
            _ (assert (coll? res))]
        (mapv par/find-problem res))
      [])))

(defn visit-all [z format-fn]
  (loop [loc z results []]
    (if (zip/end? loc)
      results
      (if-let [res (view-loc format-fn loc)]
        (let [new-results (conj results res)]
          (recur (zip/next loc) new-results))
        (recur (zip/next loc) results)))))

;;
;; Returns the first bad result it finds, or nil if there are none
;;
(defn check-all [z]
  (loop [loc z bad-result nil]
    (if bad-result
      bad-result
      (if (zip/end? loc)
        nil
        (if-let [potential-problems (check-loc loc)]
          (let [bad-problem (some #(when-not (:okay? %) %) potential-problems)
                ;_ (println (str "Got " bad-problem " from " (count potential-problems)))
                ]
            (recur (zip/next loc) bad-problem))
          (recur (zip/next loc) nil))))))

(def original-source "prod_input.L5K")

;;
;; See view-structure for description. The problem mentioned there doesn't stop us
;; being able to check every thing that needs to be parsed.
;;
(defn check-structure []
  (let [res (-> z (ew/start-up original-source) ew/modify-all zip/vector-zip check-all)
        ;_ (println "RES: " res)
        ]
    (if res
      (do
        (println (:msg res))
        (par/err->out (:msg-value res)))
      (do
        (println "All fine")
        (par/err->out "All fine")))))

;;
;; Visits each node. The thing we actually return from sparse-format is a vector that gives a sparse
;; description of the node visited. Look at the structure that is being visited. Notice that there
;; will be many programs, yet program is just one node. In such a case in the output produced there
;; will be a list (of vectors) at this position in the outer list. This might be okay for programs,
;; but stuffs up at the next level down. For instance with tag there as many tags as there are programs,
;; and probably they can be matched because in the same order. But with routine there are many per program,
;; so how do you know when the next routine is for the next program??
;;
(defn view-structure
  ([]
   (view-structure nil))
  ([file-name]
   (let [name (or file-name "output.clj")
         sparse-visit #(visit-all % sparse-format)
         huge-visit #(visit-all % huge-format)
         selective-visit (partial ew/find-all :program nil)
         visited (-> z (ew/start-up original-source) ew/modify-all zip/vector-zip huge-visit)
         res visited]
     (spit name (u/pp-str res)))))

#_(defn program-named? [program name]
    (assert (:id program) (str "Not passed in a program"))
    (assert name (str "Not passed in a name"))
    (let [name-observing (-> program :input :parsed-value second)]
      (println "name-observing:" name-observing)
      (when (= name name-observing)
        program)))

(defn xxx []
  (ew/edn-writer "sgc" original-source))

(defn c []
  (check-structure))

(def bulky-tags #{"SCADA_R_Sys" "SCADA_R_Readings" "A1_Discrete_Inputs" "A2_Discrete_Inputs"
                  #_"Tubes_Seq" "SMRTGS_W_Tubes_Seq_Buffer"})

;;
;; Each element is wrapped in a vector. Better to use a sorted-map
;;
(defn ordered-query []
  (spit "query.edn" (u/pp-str (sort-by #(-> % key :tag/name) (remove (fn [[k _]] (some bulky-tags [(:tag/name k)])) @running/tags-repo)) 200)))

(defn full-query []
  (spit "query.edn" (u/pp-str (into {} (sort-by #(-> % key :tag/name) @running/tags-repo)) 200)))

(defn simple-query []
  (spit "query.edn" (u/pp-str @running/tags-repo 200)))

(defn q []
  (clojure.java.io/delete-file "query.edn" true)
  (full-query))

(defn x []
  (let [_ (sys/start)]))

;;
;; ["COP" ["L1.S5.AI00" "L1.S6.AI00" "16"]]
;; MOV(SMRTGS_W_Switched_To_Tube_Num,HMI_R_Gas.Ch_No)
;;
#_(defn x-cust []
  (let [
        instruction-debug ["DEBUG" ["Always.PowerOn_5sec"]]
        instruction-1 ["OTU" ["Always.PowerOn_5sec"]]
        _ (i/clear-debug-cache)
        _ (run/reset-tags)
        infos (io/old-infos (run/start io/dir-name io/main-program))
        _ ((rou/run-instruction-hof infos instruction-debug) true)
        _ ((rou/run-instruction-hof infos instruction-1) true)
        _ ((rou/run-instruction-hof infos instruction-debug) true)
        ]))