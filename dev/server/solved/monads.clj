(ns solved.monads)

(defn half-double [n]
  [(/ n 2) (* 2 n)])

(defn inc-int [n]
  [(+ 5 n) (+ 10 n)])

;;
;; Now write a function that accepts an integer, calls half-double on it and
;; then calls inc-int on each of the numbers produced by half-double.
;;
(defn do-both-1 [int]
  (let [res (half-double int)]
    (println res)
    (mapcat inc-int res)))

(defn do-both-2 [int]
  (let [res (half-double int)]
    ;(println res)
    (apply concat (map inc-int res))))

(defn half-double [n]
  #{(/ n 2) (* 2 n)})

(defn half-double-bigdec [n]
  #{(bigdec (/ n 2)) (bigdec (* 2 n))})

(defn inc-int [n]
  #{(+ 5 n) (+ 10 n)})

(defn do-both-3 [int]
  (let [res (half-double int)]
    ;(println res)
    (apply clojure.set/union (map inc-int res))))

;;
;; ma -> (a -> mb) -> mb
;;
(defn m-bind [ns bigdec-fn]
  (apply clojure.set/union
         (map bigdec-fn ns)))

(defn do-both [n]
  (m-bind (half-double n) inc-int))

(defn x []
  ;(do-both 16)
  (m-bind (half-double 16) half-double-bigdec)
  )
