(ns parse.navigation)

;;
;; Can be used with some to give particular routine, from which can get whatever want,
;; such as :id
;;
(defn named? [routine name f]
  (assert (fn? f))
  (assert (:id routine) (str "Not passed in a routine"))
  (assert name (str "Not passed in a name"))
  ;(println "Looking for " name)
  (let [parsed-value (-> routine :input :parsed-value)
        ;_ (println parsed-value)
        ;; Go 2nd here because :res is in first place
        name-observing (f parsed-value)]
    ;(println "name-observing: " name-observing)
    (when (= name name-observing)
      routine)))

(defn named-idx? [idx routine name f]
  (assert (fn? f))
  (assert (:id routine) (str "Not passed in a routine"))
  (assert name (str "Not passed in a name"))
  ;(println "Looking for " name)
  (let [parsed-value (-> routine :input :parsed-value)
        ;_ (println parsed-value)
        ;; Go 2nd here because :res is in first place
        name-observing (f parsed-value)]
    ;(println "name-observing: " name-observing)
    (when (= name name-observing)
      [idx parsed-value])))

(def routine-by-name-q (comp second second))
(def task-by-name-q (comp second second))
(def program-by-name-q second)
(def add-on-instruction-by-name-q second)
