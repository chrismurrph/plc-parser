(ns solved.steps)

(def debug? true)

(defn sqrt [n]
  (if (> n 0)
    (Math/sqrt n)
    0))

(defn exp [x pow-of]
  (Math/pow x pow-of))

(defn squared [x]
  (* x x))

(defn root [x root-of]
  (Math/pow x (/ root-of)))

(defn round [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn abs [val]
  (if (neg? val)
    (* -1 val)
    val))

(def precision 5)
(defn distance [[x1 y1] [x2 y2]]
  (let [x-delta-squared (exp (- x2 x1) 2)
        y-delta-squared (exp (- y2 y1) 2)
        sum-of-differences (+ x-delta-squared y-delta-squared)
        now-squared (sqrt sum-of-differences)]
    (round precision now-squared)))

(defn eq? [a b]
  (if (= a b)
    true
    (let [diff (abs (- a b))]
      (< diff 0.01))))

(defn my-zero? [a]
  (eq? a 0))

;;
;; returns 0 if circle meets point, -ive if doesn't get there and +ive for overreach
;; To coerce into same effect as circle-intersections:
;; 0 -> one point in a vector
;; +ive -> nil
;; -ive -> []
;;
(defn circle-point-intersection [{x0 :x y0 :y r0 :r} {x1 :x y1 :y}]
  (let [dist (distance [x0 y0] [x1 y1])]
    (- dist r0)))

(defn circle-intersections [{x0 :x y0 :y r0 :r} {x1 :x y1 :y r1 :r}]
  (let [dist (distance [x0 y0] [x1 y1])
        ;_ (println dist)
        ;_ (assert (not (and (eq dist 0) (eq r0 r1))) (str "Same circle"))
        ]
    (if (and (eq? dist 0) (eq? r0 r1))
      :same-circle
      (if (> dist (+ r0 r1))
        []                                                  ;; circles too far from one another to intersect (not gone far enough)
        (if (< dist (abs (- r0 r1)))
          nil                                               ;; circles are within each other (gone too far)
          (let [
                dx (- x1 x0)
                dy (- y1 y0)
                a (/ (+ (- (squared r0) (squared r1)) (squared dist)) (* 2 dist))
                h (sqrt (- (squared r0) (squared a)))
                xm (+ x0 (* a (/ dx dist)))
                ym (+ y0 (* a (/ dy dist)))
                xs0 (+ xm (* h (/ dy dist)))
                xs1 (- xm (* h (/ dy dist)))
                ys0 (+ ym (* h (/ dx dist)))
                ys1 (+ ym (* h (/ dx dist)))
                point-1 {:x xs0 :y ys0}
                point-2 {:x xs1 :y ys1}
                ]
            (if (= point-1 point-2)
              [point-1]                                     ;; circles touching
              [point-1 point-2])))))))

;;
;; Two circles are on the x axis. a needs to creep towards b, but not sure whether to
;; go to up or down. This function makes that decision.
;;
(defn from-to-closer-by [{x0 :x y0 :y} {x1 :x y1 :y} length]
  (assert (= x0 x1))
  (let [go-up? (> y1 y0)]
    (if go-up?
      {:x x0 :y (+ y0 length)}
      {:x x0 :y (- y0 length)})))

;;
;; Imagine point a on left. Move towards end-point in a straight line until get an
;; intersection or envelope. point b is also end point. It doesn't move but does have
;; two types of circles drawn around it.
;; Start with big circles and take big steps. By convention when there's an envelope we
;; reduce the size of the a circle, then the b circle. Actually need to do this for both
;; complete overlaps and the usual two points intersection, to try to get a circles 'kiss'.
;;
;;
(def last-reductions 2)

(defn prin [txt]
  (when debug?
    (println txt)))

(defn stepper [radius-a radius-b y-diff]
  (if (zero? y-diff)
    0
    (let [big-radius (max radius-a radius-b)
          little-radius (min radius-a radius-b)
          end-point {:x 0 :y y-diff}
          skip-by (int (/ y-diff big-radius))
          _ (prin (str "skip by " skip-by))
          start-point {:x 0 :y (* skip-by y-diff)}
          ]
      (loop [reason "started"
             times 0
             point-a {:x 0 :y (* skip-by y-diff)}
             moves-count (dec skip-by)
             reductions 0]
        (prin (str "RECUR: " reason))
        #_(when (>= times 20)
          (assert false "Enough"))
        (let [y-reached (:y point-a)
              within-reach? (> big-radius (- y-diff y-reached))
              _ (prin (str "y-diff, y-reached: " y-diff "," y-reached))
              _ (prin (str "within-reach: " within-reach?))
              little-exact? (= little-radius (- y-diff y-reached))
              big-exact? (= big-radius (- y-diff y-reached))
              _ (prin (str "little-exact?, big-exact?: " little-exact? "," big-exact?))
              a-circle-big? (case reductions
                              0 true
                              1 true
                              2 false
                              ;3 true
                              ;4 false
                              )
              b-circle-big? (case reductions
                              0 true
                              1 false
                              2 false
                              ;3 false
                              ;4 false
                              )
              no-circle-at-b? (case reductions
                                0 false
                                1 false
                                2 false
                                ;3 true
                                ;4 true
                                )
              already-circles (if no-circle-at-b? 1 2)
              a-circle (if a-circle-big? (merge start-point {:r big-radius}) (merge start-point {:r little-radius}))
              b-circle (if b-circle-big?
                         (merge end-point {:r big-radius})
                         (when (not no-circle-at-b?) (merge end-point {:r little-radius})))
              intersections (if no-circle-at-b?
                              (let [reach-diff (circle-point-intersection a-circle end-point)
                                    res (if (my-zero? reach-diff)
                                          [end-point]
                                          (if (pos? reach-diff)
                                            nil
                                            []))]
                                res)
                              (circle-intersections a-circle b-circle))]
          (cond
            (= intersections :same-circle)
            moves-count

            ;; kiss - we are done
            (= 1 (count intersections))
            (+ moves-count already-circles)

            ;; have tried everything and found no kiss, so answer is now known
            (= reductions last-reductions)
            (let [res (+ moves-count already-circles)
                  _ (prin (str "last reduction, returning add of: <" moves-count "|" already-circles ">"))]
              res)

            (nil? intersections)
            (recur "ballpark nil intersections" (inc times) point-a moves-count (inc reductions))

            (zero? (count intersections))
            (if within-reach?
              (recur "ballpark zero intersections" (inc times) point-a moves-count (inc reductions))
              (let [
                    ;; S/be at reductions 0. Need to keep marching on with big circle. Another move has been done
                    ;; empty - have further to go so lets keep using big circles
                    _ (assert (zero? reductions))
                    closer-point (from-to-closer-by point-a end-point big-radius)
                    ]
                (recur (str "keep marching from " point-a " to " closer-point) (inc times) closer-point (inc moves-count) 0)))

            ;; have tried everything and found no kiss, so answer is now known
            (and (> (count intersections) 1) (= reductions last-reductions))
            (+ moves-count already-circles)

            ;; In the ballpark, but have not tried all combinations to see if can get a kiss
            ;; But a doesn't need to travel any further
            (> (count intersections) 1)
            (recur "ballpark one intersection" (inc times) point-a moves-count (inc reductions))

            :default                                        ;; empty - have further to go so lets keep using big circles
            (assert false "S/not be here")
            ))))))

(defn x []
  (let [str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["1"
               "2 3 1"
               ;"1 2 0"
               ;"3 4 11"
               ;"10 2 5"
               ;"3 2 100"
               ;"100 200 1"
               ]
        [head & tail] (map str->ints input)
        _ (assert (= (count tail) (first head)))
        ]
    (doseq [[a b d] tail]
      (let [num-steps (stepper a b d)]
        (println num-steps)))))
