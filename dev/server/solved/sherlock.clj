(ns solved.sherlock)

;(def a [1 2 3 4])
;(def b [1 2 3])
;(def c [13 29 71])
;(def n (count a))
;(def m (count b))

;(def new-a (atom a))

'({0 ({:mod-res 0, :idx 0, :b-ele 1, :c-value 13} {:mod-res 0, :idx 1, :b-ele 1, :c-value 13} {:mod-res 0, :idx 2, :b-ele 1, :c-value 13} {:mod-res 0, :idx 3, :b-ele 1, :c-value 13})}
  {1 ({:mod-res 1, :idx 0, :b-ele 2, :c-value 29} {:mod-res 0, :idx 1, :b-ele 2, :c-value 29} {:mod-res 1, :idx 2, :b-ele 2, :c-value 29} {:mod-res 0, :idx 3, :b-ele 2, :c-value 29})}
  {2 ({:mod-res 1, :idx 0, :b-ele 3, :c-value 71} {:mod-res 2, :idx 1, :b-ele 3, :c-value 71} {:mod-res 0, :idx 2, :b-ele 3, :c-value 71} {:mod-res 1, :idx 3, :b-ele 3, :c-value 71})})

(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (cart (rest colls))]
      (cons x more))))

(def input-2
  [
  "100 100"
  "84691 84 90310 38506 4420 60522 47731 94325 9896 96263 79445 97489 51951 31545 87475 53968 63438 78537 7775 94328 63340 28121 2156 75467 31502 36991 2643 31932 38409 24712 45895 28197 23355 79694 23857 36426 14575 81221 8580 74432 35902 16480 49218 34067 7354 33447 48152 99080 66466 33792 59980 4784 47202 90629 89200 74934 91757 3497 32501 29211 95728 96221 68417 27328 48511 44965 87010 6504 63982 63676 8399 23492 86568 95922 60016 38674 87940 48153 72167 1449 29302 28333 61868 24593 28607 11431 60717 99835 29355 62034 48772 86918 19022 30739 97925 39679 90081 77068 8325 70607"
  "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1"
  "10003 41107 38238 98183 10212 37428 31258 29964 1515 11768 52977 80040 44374 81154 68546 99134 53541 51401 76763 82280 32737 41823 71826 35729 69609 90410 58949 34433 71449 29555 68825 92089 10898 76846 43377 96523 49262 53939 86542 37611 65157 26878 73517 84199 54882 11094 15819 22306 27307 22098 35844 1999 21410 38297 72455 93559 53974 5542 91151 71653 66013 15297 80206 63316 7965 73764 76456 14571 40720 81495 4979 97987 304 99876 4917 18256 44338 93737 68893 45904 58698 53310 97484 45368 1394 47611 11957 43269 58357 27176 54931 73541 38323 94685 85413 2024 16176 89291 52751 35740"
   ])

(def output-2
  "132386561 304189591 587640134 703860827 53785510 668598786 288967459 996225843 360240139 942885643 254070095 120700351 666111093 698340254 71241506 625044435 514037370 442115969 929453024 971375471 992482853 394231296 474199451 205663136 387862248 253298596 106822422 492642287 840689517 965870485 497395018 431355210 539855338 191489233 48226449 266785290 935276885 542649976 927936584 779041420 273983569 155290770 304799824 140794335 750121869 943204518 468298614 608386496 98395404 85411752 824899297 705273733 4249689 611883930 782277711 287412531 268144121 366083244 112688428 31929530 707868617 290790844 604036930 296346248 827870788 201010282 923049145 124393882 674503280 209241203 760575683 738355026 917670594 434211237 526694833 312240002 219433874 126681821 541072147 997270415 944801591 971471691 852398628 618268565 368471060 978466559 53324613 687709587 839111688 144011382 665888438 351793877 765409047 41473480 175779646 987365445 817885185 277181372 40218183 463265496")

(def input-1 ["4 3"
            "1 2 3 4"
            "1 2 3"
            "13 29 71"])
(def output-1 "13 754 2769 1508")

(def input input-1)
(def output output-1)

(defn exp [x pow-of]
  (Math/pow x pow-of))

(defn make-small []
  (let [modulo-by (+ 7 (exp 10 9))
        ;_ (println "modulo-by: " modulo-by)
        ]
    (fn [x]
      (let [
            ;_ (println "x: " x)
            smaller (rem x modulo-by)]
        smaller))))

(defn calculator [new-a]
  (let [smallify (make-small)]
    (fn [{:keys [idx c-value]}]
      (assert idx)
      (let [a-j (nth @new-a idx)
            product (smallify (*' a-j c-value))]
        (swap! new-a assoc idx product))))
  )

(defn x []
  (let [str->ints (fn [s]
                    (map #(Long/parseLong %)
                         (clojure.string/split s #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        all-num-lists (map str->ints input)
        [n m] (first all-num-lists)
        a (vec (second all-num-lists))
        new-a (atom a)
        b (vec (nth all-num-lists 2))
        c (vec (nth all-num-lists 3))
        all-mods-fn (fn [idx [b-ele c-ele]]
                      {idx (filter #(zero? (:mod-res %)) (map (fn [j] {:mod-res (mod j b-ele) :idx (dec j) :b-ele b-ele :c-value c-ele}) (range 1 (inc n))))})
        whole-matrix (map-indexed all-mods-fn (map vector b c))
        _ (println whole-matrix)
        ;mods-matrix (into #{} (map #(dissoc % :mod-res :b-ele)) (filter #(zero? (:mod-res %)) whole-matrix))
        ;_ (println mods-matrix)
        ;calculate (calculator new-a)
        ;_ (mapv calculate mods-matrix)
        ;res (apply str (interpose " " (mapv long @new-a)))
        ;_ (assert (= res output))
        ]
    ;(println res)
    )
  )
