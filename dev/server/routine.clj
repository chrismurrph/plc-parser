(ns routine
  (:require [parse.navigation :as nav]
            [utils :as u]
            [add-on :as add]
            [clojure.string :as str]
            [instructions :as i]
            [stateful-instructions :as si]
            [domain-constants :as constants]
            [context :as con]
            [datatypes :as data]))

;;
;; input for a rung is always true and we don't care about the result. However while reducing every element
;; does care
;;
(defn scan-rung
  ([runnables]
   (scan-rung false runnables))
  ([debug? runnables]
   (when debug? (println (str "--> About to scan " (count runnables) " runnables")))
   (reduce
     (fn [acc runnable-ele]
       (let [
             _ (when debug? (println "energy thru rung:" acc))
             _ (u/assrt (boolean? acc) (str "Energy s/be boolean as threading thru the rung, yet got <" acc ">"))
             ; just hof
             ;_ (println "runnable: " runnable-ele)
             res (runnable-ele acc)
             _ (when debug? (println (str "res from rung instruction: <" res ">")))
             ]
         res))
     true
     runnables)))

(def instruction-functions
  {"XIC"       i/xic
   "XIC-D"     i/xic-d
   "XIO"       i/xio
   "NOP"       i/nop
   "AFI"       i/afi
   "CLR"       i/clr
   "MOV"       i/mov
   "GSV"       i/gsv
   "MOD"       i/_mod
   "JSR"       si/jsr
   "SBR"       si/sbr
   "RET"       si/ret
   "LBL"       si/lbl
   "JMP"       si/jmp
   "ADD"       i/add
   "MSG"       i/msg
   "DEBUG_MSG" i/debug-msg
   "SUB"       i/sub
   "MID"       i/mid
   "DIV"       i/div
   "MUL"       i/mul
   "NEQ"       i/neq
   "CMP"       i/cmp
   "DEBUG"     i/debug
   "LES"       i/les
   "COP"       i/cop
   "FLL"       i/fll
   "GRT"       i/grt
   "LEQ"       i/leq
   "GEQ"       i/geq
   "EQU"       i/equ
   "OTE"       i/ote
   "OTU"       i/otu
   "FOR"       i/_for
   "OTL"       i/otl
   "OSR"       i/osr
   "ONS"       i/ons
   "RES"       i/res
   "TON"       i/ton
   "CTU"       i/ctu
   "CPT"       i/cpt
   "SWPB"      i/swpb
   "DELETE"    i/delete
   "OR"        i/_or
   "AND"       i/_and
   "BTD"       i/btd})

(defn- instruction-fn? [instruction-name]
  (get instruction-functions instruction-name))

(defn bool->integer [x]
  (assert (boolean? x))
  (cond
    (= x false) 0
    (= x true) 1
    :default (assert false)))

(defn -dn-fn [acc-idx pre-idx]
  (fn [array]
    (let [acc-val (:val/val (get array acc-idx))
          pre-val (:val/val (get array pre-idx))]
      ;; If acc has reached pre then the job is done
      {:dynam/dynamic-value (bool->integer (>= acc-val pre-val))})))

(defn -tt-fn [acc-idx pre-idx]
  (fn [array]
    (let [acc-val (:val/val (get array acc-idx))
          pre-val (:val/val (get array pre-idx))]
      ;; If acc still less than pre then the job is ongoing (timer timing)
      ;; Using pos? here will not be as good as setting the TT bit, because there will be a moment
      ;; when going but hasn't yet lifted past 0.
      {:dynam/dynamic-value (bool->integer (and (pos? acc-val) (< acc-val pre-val)))})))

(defn -len-fn []
  (fn [val]
    (let [value (:val/val val)]
      (assert (string? value) (str "Expect string but got <" value "> from <" val ">. FOR from - nil?: " (nil? val) ", type: " (type val) (con/out-context)))
      {:dynam/dynamic-value (count value)
       :dynam/dynamic-type  :int})))

(defn -len-update-fn []
  (fn [orig-val new-len]
    (assert (string? orig-val) (str "Expect string but got <" orig-val ">"))
    ;(assert false (str "calling -len-update-fn with " orig-val ", " new-len))
    (let [diff (- new-len (count orig-val))]
      (cond
        (zero? diff)
        orig-val

        (pos? diff)
        (apply str (concat orig-val (repeat diff " ")))

        (neg? diff)
        (take new-len orig-val)))))

#_(defn -path-fn []
    (fn [array]
      (assert false)))

(defn -value-from-built-in [idx array]
  (let [_ (assert idx)
        _ (assert (not (string? array)))
        _ (assert (sequential? array) (str "Can't take nth (" idx ") of: <" array "> of type: <" (type array) "> nil? " (nil? array) (con/out-context)))
        res (nth array idx)
        ;_ (println (str "value-from-built-in RES: <" res ">, when using <" array "> and <" idx ">"))
        ]
    {:dynam/dynamic-value (:val/val res)}))

;;
;; When change this structure also change tag.bnf and reprocess (user/xxx) - edn/writer
;; See make-message as example of what to do as well
;;
(def built-ins {:counter {:name        "COUNTER"
                          :indexing-fn {data/accumulated (partial -value-from-built-in constants/COUNTER-ACC-idx)
                                        data/preset         (partial -value-from-built-in constants/COUNTER-PRE-idx)
                                        data/done?          (-dn-fn constants/COUNTER-ACC-idx constants/COUNTER-PRE-idx)}
                          :index       {data/accumulated 0
                                        data/preset      1}}
                :timer   {:name        "TIMER"
                          :indexing-fn {data/accumulated (partial -value-from-built-in constants/TIMER-ACC-idx)
                                        data/preset      (partial -value-from-built-in constants/TIMER-PRE-idx)
                                        data/done?       (-dn-fn constants/TIMER-ACC-idx constants/TIMER-PRE-idx)
                                        data/timing?     (-tt-fn constants/TIMER-ACC-idx constants/TIMER-PRE-idx)}
                          :index       {data/accumulated constants/TIMER-ACC-idx
                                        data/preset      constants/TIMER-PRE-idx}}
                :string  {:name        "STRING"
                          :indexing-fn {data/len (-len-fn)}
                          :set-fn      {data/len (-len-update-fn)}}
                :message {:name        "MESSAGE"
                          :indexing-fn {data/path  (partial -value-from-built-in constants/PATH-idx)
                                        data/er    (partial -value-from-built-in constants/ER-idx)
                                        data/done? (partial -value-from-built-in constants/DN-idx)
                                        }
                          :index       {data/path  constants/PATH-idx
                                        data/er    constants/ER-idx
                                        data/done? constants/DN-idx}}})

(defn built-in-by-kw? [kw]
  (some #{kw} (keys built-ins)))

(defn built-in-by-name? [name]
  (some #(when (= (:name %) name) %) (vals built-ins)))

(defn- output-instruction? [instruction-name]
  (some #{"OTE" "OTU" "OTL"} [instruction-name]))

(defn- no-args-instruction? [instruction-name]
  (some #{"SBR" "AFI" "NOP" "RET"} [instruction-name]))

(defn run-instruction-hof [infos instruction]
  (fn [in-energy]
    (if (si/jumping?)
      (if (= "LBL" (first instruction))
        (let [wanting-to-jump-to @si/jump-to
              at-label (-> instruction second first)
              ]
          (when (= wanting-to-jump-to at-label)
            ;(println (str "Landing back, so removing the cause of jumping: " @si/jump-to ", " (-> instruction second first)))
            (reset! si/jump-to nil))
          true)
        (do
          ;(println (str "Skipping - happens a lot: " (first instruction)))
          true)
        )
      (let [[instruction-name & _] instruction
            _ (assert instruction-name (str "No name found: " instruction))
            ;_ (println (str "JMP in any: " @i/jump-to))
            ;;_ (println "instruction: " instruction)
            _ (assert (:program-info infos) (str "Need :info in infos paramter, but only got: <" (keys infos) ">"))
            _ (assert (-> infos :controller-info :datatypes count pos?))
            _ (assert (boolean? in-energy) (str "In energy not boolean?: <" in-energy ">, is nil?: " (nil? in-energy)))
            _ (assert (sequential? instruction) (str "instruction that closed over s/be hiccup vector. Instead got " (type instruction)))
            args (map data/wrap-arg (remove #(= % ",") (u/probe-off (-> instruction next first))))
            ;_ (println "ARGS:" args)
            _ (assert (or (no-args-instruction? instruction-name) (not (some (complement u/not-blank?) args)))
                      (str "blank arg: " (seq args) " for " instruction-name))
            instruction-fn (instruction-fn? instruction-name)
            _ (assert instruction-fn (str "No instruction function found for " instruction-name))
            {:keys [info/rung-num info/routine-name]} infos
            ;_ (println "ASSOC: " rung-num routine-name)
            _ (swap! con/context assoc :rung-num rung-num :routine-name routine-name)
            res (instruction-fn infos in-energy args)
            _ (swap! con/context dissoc :rung-num :routine-name)
            _ (assert (or (output-instruction? instruction-name) (not (nil? res))) (str "nil return from " instruction-name " when energy in was: <" in-energy ">"))
            _ (assert (or (nil? res) (boolean? res)) (str "No boolean but: <" res ">"))
            ;_ (println "inst to run:" instruction-name (first args) (second args) "returned" res)
            ]
        res))))

;;
;; Each of the functions will run one of the branch levels. So here we
;; need to push the enery thru all the branch-levels in a kind of 'in parallel'
;; way (they are autonomous at any rate) - which means using `map` and they all get
;; the same `in-energy`. Then with the output we return true if any are true - it there's
;; a way through.
;;
(defn scan-branch-hof [scan-branch-level-functions]
  (assert (sequential? scan-branch-level-functions) (str "Not sequential: " scan-branch-level-functions))
  (fn [in-energy]
    (if (not (si/jumping?))
      (let [_ (assert (boolean? in-energy))
            outputs (mapv (fn [f] (f in-energy)) scan-branch-level-functions)
            ;_ (println "outputs: " outputs)
            ]
        (boolean (some identity outputs)))
      false)))

(declare scan-branch-level-hof)

(defn scan-routine
  ([debug? routine-rungs]
   (assert (seq routine-rungs) (str "No rungs"))
   (doseq [rung routine-rungs]
     (scan-rung debug? rung)))
  ([routine-rungs]
   (scan-routine false routine-rungs)))

(declare assemble-routine)

;;
;; Will turn a data structure into runnable rungs.
;;
(defn assemble-runnable-rung [-infos rung rung-num]
  (let [rung-numbered-infos (assoc -infos :info/rung-num rung-num)]
    (loop [elements rung
           result []]
      (let [[head & tail] elements
            ;_ (println "tail size: " (count tail) ", head is: " head)
            ]
        (cond
          (nil? head)
          (do
            ;(println "all over: " (count result))
            result)

          (= :rung head)
          (recur tail result)

          (-> head first instruction-fn?)
          (let [inst (run-instruction-hof rung-numbered-infos head)
                ;_ (println "inst: " hof)
                ]
            (recur tail (conj result inst)))

          (and (sequential? (first head)) (= :add-on-instruction-name (ffirst head)))
          (let [instruction (into [(-> head first second)] (-> head next first))
                inst (add/run-add-on-instruction-hof assemble-routine scan-routine rung-numbered-infos instruction)
                ]
            (recur tail (conj result inst)))

          (= :br (first head))
          (let [br-levels (next head)
                levels (map next br-levels)
                ;; Each level is a sequential of instructions
                level-functions (mapv #(scan-branch-level-hof rung-numbered-infos %) levels)]
            (recur tail (conj result (scan-branch-hof level-functions))))

          :default
          (assert false (str "Don't recognise: " head (con/out-context)))
          )))))

;;
;; First rung of a routine is zero.
;;
(defn assemble-routine [infos routine]
  (let [rungs (-> routine next next)
        _ (assert (seq rungs) (str "No rungs found in routine: <" routine ">"))
        runnable-rungs (map #(assemble-runnable-rung infos %1 %2) rungs (range 0 (count rungs)))
        ]
    runnable-rungs))

;;
;; Out energy of each instruction goes to the next instruction. Assuming only contains instructions, which is wrong.
;; An instruction might actually be a rung. The word for something that is an instruction or a rung is a scannable.
;; This function can thus be used in multiple situations because we look at each element and see whether it is an
;; instruction or a runnable.
;;
(defn scan-branch-level-hof [infos instructions]
  (fn [in-energy]
    (assert (:program-info infos) (str "Need :info in infos paramter, but only got: <" (keys infos) ">"))
    (assert (boolean? in-energy) (str "Energy s/be boolean as about to scan branch level, yet got <" in-energy ">"))
    ;(println (str "Scanning a branch level with " (count instructions) " instructions (each instruction might in turn be a rung)"))
    (when (not (si/jumping?))
      (reduce
        (fn [acc [instr & tail]]
          (let [
                ;_ (println "energy into br level:" acc ", instr: <" instr ">, tail: <" tail ">")
                _ (assert (boolean? acc) (str "Energy s/be boolean as threading into br level, yet got <" acc "> nil? " (nil? acc)))
                ;_ (println "level element: " ele)
                ;instr-fn? (instruction-fn? instr)
                ;_ (when (not instr-fn?)
                ;    (println (str "WARNING: Is NOT instr? " instr)))
                ;;res (ele acc)
                res (cond
                      (instruction-fn? instr)
                      ((run-instruction-hof infos (into [instr] tail)) acc)

                      (and (sequential? instr) (= :add-on-instruction-name (first instr)))
                      ((add/run-add-on-instruction-hof assemble-routine scan-routine infos (into [(second instr)] (first tail))) acc)

                      :default
                      (let [
                            ;;
                            ;; The data structure representing a branch was not recursively turned into functions. In other
                            ;; words the functions list that is to be run later is not recursive. Instead the recursion is
                            ;; left until when running - here. In other words we are about to do what could have been done
                            ;; at prep time.
                            ;; Having a recursive list of functions in the first place would be better. In which case here
                            ;; ele would not be data but would be a hof. The only data structures that really ought to
                            ;; remain when running are instructions.
                            ;;
                            br-levels tail
                            ;_ (println (str "br-levels: " br-levels " is next from: <" instr ">"))
                            levels (map next br-levels)
                            ;_ (println "levels:" levels)
                            ;; Each level is a sequential of instructions
                            level-functions (mapv #(scan-branch-level-hof infos %) levels)
                            runner (scan-branch-hof level-functions)
                            ;_ (assert false (str "S/be a rung/instruction, but not yet coded for it: " (count level-functions)))
                            ]
                        (runner acc)))
                ;_ (println (str "res from branch instruction: <" res ">"))
                ]
            res))
        in-energy
        instructions))))

;;
;; in-infos (not info) is our global, available to the whole program, most functions
;;
(defn routine-assembler [in-infos tag-retriever-f tag-setter-f]
  (fn [routine-name]
    (let [routine (-> (some #(nav/named? % routine-name nav/routine-by-name-q) (-> in-infos :program-info :program/routines))
                      :input
                      :parsed-value)
          _ (assert routine (str "Could not find a routine named: <" routine-name ">"))
          here-infos {:info/get-tag      (tag-retriever-f in-infos)
                      :info/set-tag      (tag-setter-f in-infos)
                      ;; 'jump to subroutine' will require getting another - calling these functions
                      :info/get-routine  (routine-assembler in-infos tag-retriever-f tag-setter-f)
                      :info/scan-routine scan-routine
                      :info/routine-name routine-name
                      :program-info      (:program-info in-infos)
                      :controller-info   (:controller-info in-infos)}
          runnable-routine (assemble-routine here-infos routine)]
      runnable-routine)))
