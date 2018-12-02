(ns solved.append)

(def tests [{:s "hackerhappy"
             :t "hackerrank"
             :k 8
             :e "No"}
            {:s "hackerhappy"
             :t "hackerrank"
             :k 9
             :e "Yes"}
            {:s "hackerhappy"
             :t "hackerrank"
             :k 10
             :e "No"}
            {:s "aba"
             :t "aba"
             :k 7
             :e "Yes"}
            {:s "abacus"
             :t "ab"
             :k 3
             :e "No"}
            {:s "abacus"
             :t "ab"
             :k 4
             :e "Yes"}
            {:s "a"
             :t "a"
             :k 1
             :e "No"}
            {:s "a"
             :t "a"
             :k 2
             :e "Yes"}
            {:s "a"
             :t "a"
             :k 3
             :e "Yes"}
            {:s "apple"
             :t "banana"
             :k 1
             :e "No"}])

(defn in-common [count-a count-b str-a str-b]
  (let [longest-len (max count-a count-b)
        res-1 (map (fn [idx a b] [idx (= a b)]) (range longest-len) str-a str-b)
        comm (take-while second res-1)
        res (some-> comm last first inc)
        _ (println (str "in common from " str-a " and " str-b ": " res))
        ]
    (if (nil? res) 0 res)))

(defn in-sync-with [at-least-moves k]
  (if (< k at-least-moves)
    false
    (if (= at-least-moves k)
      true
      (let [base-num (-' k at-least-moves)]
        (= 0 (rem base-num 2))))))

;; common-count (in-common count-a count-b s t)
(defn x [test]
  (let [{:keys [s t k e]} test
        count-a (count s)
        count-b (count t)
        comb-len (+' count-a count-b)
        reply (if (>= k comb-len)
                "Yes"
                (let [common-count (in-common count-a count-b s t)
                      a-moves (-' count-a common-count)
                      b-moves (-' count-b common-count)
                      _ (println "a-moves: " a-moves)
                      _ (println "b-moves: " b-moves)
                      at-least-moves (+' a-moves b-moves)
                      _ (println "reqed moves: " at-least-moves)
                      ]
                  (if (in-sync-with at-least-moves k)
                    "Yes"
                    "No")))
        ]
    (assert (= reply e) (str "Failed: " s " " t " " k " " e))))

(defn test-all []
  (map x tests))

(defn test-bad []
  (x (nth tests 6)))