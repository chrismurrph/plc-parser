(ns gen
  (:require [domain-utils :as du]))

;;
;; Uses simple scaling (i.e. a triangle) rather than a skewed gaussian which ought to use. The main idea is
;; that results that are 'outside the function' are ignored and we go again.
;; The closer to modal, the bigger the chance the generated result will be accepted.
;;
(defn generator [modal-val low-val high-val]
  (fn []
    (let [gen-val (fn []
                    (let [diff-range (- high-val low-val)
                          picked-in-range (rand diff-range)
                          perhaps-res (+ low-val picked-in-range)
                          ;; partial distance left or right, that will be negative if to left
                          modal-delta (- perhaps-res modal-val)
                          extremity (if (neg? modal-delta) low-val high-val)
                          ;; full distance left or right, that will be negative if to left
                          total-dist (- extremity modal-val)
                          closeness-to-modal (- 1 (/ modal-delta total-dist))
                          ]
                      (when (<= (rand) closeness-to-modal)
                        (du/big-dec "fouteen" perhaps-res))))]
      (first (drop-while nil? (repeatedly gen-val))))))
