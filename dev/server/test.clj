(ns test
  (:require [utils :as u]
            [instructions :as inst]
            [convert :as conv]
            [parse.edn-writer :as ew]
            [parse.navigation :as nav]
            [clojure.zip :as zip]))

(def original-source "prod_input.L5K")

(def z (zip/vector-zip ew/structure))
(defn test-format-routine []
  (let [all (-> z (ew/start-up original-source) ew/modify-all zip/vector-zip)
        programs (ew/find-all :program nil all)
        _ (println "Found" (count programs) "programs")
        search-for-program-name "ModbusMasterTCP"
        chosen-program (:id (some #(nav/named? % search-for-program-name nav/program-by-name-q) programs))
        _ (assert chosen-program (str "Not found program named: <" search-for-program-name ">"))
        routines (ew/find-all :routine chosen-program all)
        chosen-routine (some #(nav/named? % "CheckTransCode" nav/routine-by-name-q) routines)]
    (spit "routine.edn" (ew/format-keywords (u/pp-str chosen-routine)))
    ))

(def ex [{"a" 1} {"b" 2} {"c" 3}])

;; None of these two work
(defn s []
  ;(sort-by #(-> % key) ex)
  (sort ex)
  )

(def ex '([:br [:int "61"] [:int "0"] [:int "0"] [:int "0"]] [:br [:int "0"]]))

(defn a []
  (reduce
    (fn [acc ele]
      (let [_ (assert (= :br (first ele)))
            res (conj acc (mapv #(-> % conv/se->int) (next ele)))
            ]
        res))
    []
    ex))

(def test-def [:def [:var "SCADA_R_Sys"] [:type [:custom-t "SCADA_R_Sys"]] [:enc [:assign [:comment [:dot-comment "COMMENT" "." [:var "ALM" [:dim [:int 10]]]] [:anything-string [:quote "] [:one-or-more-anythings [:anything-word Precisive] [:anything-word CO-O2] [:anything-word Analyser] [:anything-word Fault]] [:quote "]]]]] [:br [:br [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]] [:base-two [:int 0]]]]])

(defn t-2 []
  (running/between-multiple-brackets-of-def (u/probe-off test-def)))

(defn t []
  (let [bits [1 1 1 1
              1 1 1 1
              1 1 1 1
              1 1 1 1
              1 1 1 1
              1 0 0 0
              0 0 0 0
              0 0 0 0]
        src-number (conv/-from-type-digits bits 2)
        res (inst/btd-calc src-number 3 src-number 10 6 32)]
    (println res "or" (conv/-to-type-digits 32 res 2))))

(defn t-1 []
  (let [src-bits [0 0 0 0
                  0 0 0 0
                  0 0 0 0
                  0 0 0 0
                  1 1 1 1
                  1 1 1 1
                  1 1 1 0
                  1 1 1 1]
        dest-bits [0 0 0 0
                   0 0 0 0
                   0 0 0 0
                   0 0 0 0
                   0 0 0 0
                   0 0 0 0
                   0 0 0 0
                   0 0 0 0]
        src-number (conv/-from-type-digits src-bits 2)
        src-bits-again (conv/-to-type-digits "DINT" src-number 2)
        dest-number (conv/-from-type-digits dest-bits 2)
        dest-bits-again (conv/-to-type-digits "DINT" dest-number 2)
        res (inst/btd-calc src-number 3 dest-number 5 10 32)]
    (println src-bits-again)
    (println dest-bits-again)
    (println res "or" (conv/-to-type-digits 32 res 2))
    ))
