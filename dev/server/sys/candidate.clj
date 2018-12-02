(ns sys.candidate
  (:require [utils :as u]))

;;
;; Theory with this fn is that if know when tag-name-want will come up next we can add something into
;; laters. Will not be necessary to use however when tag is already in the 'one source of truth' cache.
;; This may yet always be the case! Also perhaps a different/better approach is to have the cache return
;; an error (chance to use error monad) rather than just fail. If there's an error just put it in one later,
;; no need for the complication of this function at all.
;;
;; Order is "ModbusMasterTCP" "Common" "MainProgram". Say current-counted is at 2, so "Common". What will
;; current-counted be in the future when is first "MainProgram"? We can see it is the next one: 3. To
;; calculate:
;; current-counted + programs-count
;; 2 + 3 = 5
;; remainder will be 2 (0 is special condition)
;; Here we use 3 because "MainProgram" is the 3rd
;; 3 - 2 = 1
;; So add 1 to current-counted to get 3
;; Special condition of 0 means we can just return current-counted. If implement like this, then will need
;; caveat that caller may have to handle the event either right now or later. Simplest is to always handle
;; it later, so that's what we will do. Fits nicely with the count that go fast for being the same as the
;; programs-count.
;;
(defn counted-when-next [all-tag-names tag-name-want]
  (let [num-programs (count all-tag-names)
        name-is-at (inc (u/index-of all-tag-names tag-name-want))]
    (println (str tag-name-want " is at " name-is-at))
    (fn [current-counted]
      (let [next-count-at-same (+ current-counted num-programs)
            remainder (rem next-count-at-same num-programs)
            more-to-go (- name-is-at remainder)]
        (+ current-counted more-to-go)))))
