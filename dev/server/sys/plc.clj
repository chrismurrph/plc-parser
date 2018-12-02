(ns sys.plc
  (:require
    [clojure.core.async :refer (close! alts! timeout <! <!! >! >!! put! chan go go-loop)]
    [routine :as rou]
    [running :as run]
    [instructions :as i]
    [query :as q]
    [tag.read :as read-tag]
    [tag.write :as write-tag]
    [utils :as u]
    [datatypes :as data]
    [sys.domain-utils :as du]))

(def debug false)

(defn p [txt]
  (when debug
    (println txt)))

(def inject-weather-every 10)
;; Will turn all the fans on
(def short-millis 50)
;; Will keep the fans off (and mean weather gets injected only every 15 seconds (1.5 x 10))
(def long-millis 1500)

(defn pick-start-routine-name [info]
  (some #(when (q/query-routine info %) %) ["Main" "MainRoutine"]))

(defn program-infos [controller-info program-info]
  (let [merged (merge {:program-info program-info
                       :controller-info controller-info})]
    {:info/get-tag    (read-tag/tag-retriever merged)
     :info/set-tag    (write-tag/tag-setter merged)
     :program-info    program-info
     :controller-info controller-info
     :out             (:out controller-info)
     }))

(defn create-program-hof [controller-infos]
  (fn [updated-program]
    (let [infos (program-infos controller-infos updated-program)
          routine-name (pick-start-routine-name (:program-info infos))
          runnable-routine ((rou/routine-assembler infos read-tag/tag-retriever write-tag/tag-setter) routine-name)
          program-name (:program/name updated-program)
          _ (assert program-name)]
      {:name             program-name
       :infos            infos
       :runnable-routine runnable-routine})))

;;
;; Just for debugging
;;
(defn tag-defs [infos]
  (let [{:keys [controller-info program-info]} infos
        {:keys [controller-tag-defs datatypes]} controller-info
        {:keys [program/defs]} program-info
        _ (println (str "count program defs: " (count defs)))
        _ (println (str "count controller defs: " (count controller-tag-defs)))
        ]
    [controller-tag-defs defs]))

;;
;; All tags are read from the point of view of a program, so we follow that rule here
;; If have three programs then infos-by-name will have three map entries
;;
(defn tag-reader-hof [infos-by-name]
  (assert infos-by-name)
  (fn [program-name]
    (let [program-infos (get infos-by-name program-name)
          read-fn (:info/get-tag program-infos)
          _ (assert read-fn (str "No :info/get-tag in: <" (keys program-infos) "> from <" program-name ">. See: <" (keys infos-by-name) ">"))
          context nil                                       ;; intentionally nil
          ]
      (fn [tag]
        (let [res1 (-> ((read-fn context) (data/wrap-arg tag)) second)
              res2 (if (vector? res1) res1 (:val/val res1))]
          res2)))))

(defn tag-writer-hof [infos-by-name]
  (fn [program-name]
    (let [program-infos (get infos-by-name program-name)
          context nil                                       ;; intentionally nil
          write-fn ((:info/set-tag program-infos) context false)
          _ (assert write-fn (str "No :info/set-tag in: " (keys program-infos)))
          ]
      (fn [tag value]
        (write-fn (data/wrap-arg tag) value)))))

;;
;; inner function to be used when are recursing after have done a command and it would be a good thing for a few scans
;; (in most cases one would be sufficient but we do std-later-count regardless) to take place quickly so that the
;; effects on other tags can be seen quickly, rather than long-millis later.
;; When call laters-burst accompany it with short-millis
;;
(defn laters-burst [std-later-count command-ch]
  (fn [laters counted]
    (conj laters {:later/count-at (+ counted std-later-count)
                  :later/fn       (fn [_ _]
                                    (go (>! command-ch {:cmd :slow})))})))

(defn view-slots [output]
  (println @(:atom output)))

(defn update-program [all-changes]
  (fn [{:keys [program/name] :as program}]
    (let [changes (get all-changes name)
          _ (assert changes (str "No changes found for: <" name ">"))
          _ (assert (pos? (count changes)) (str "No changes found for: <" name ">"))
          ]
      (du/apply-changes program changes))))

(defn make-plc-component [command-ch atmosphere-remote-ch site]
  (let [{:keys [site/dir-name site/num-points site/main-program site/name site/changes
                site/issue-warnings-fn site/inject-weather-fn site/controller-infos-fn]} site
        _ (println (str "[plc-component] " dir-name " starting"))
        {:keys [programs datatypes controller-tag-defs add-on-instructions]} (run/start dir-name)
        num-programs (count programs)
        laters-burster (laters-burst num-programs command-ch)
        ;changes (aa9-change/aa9-changes name)
        updater (update-program changes)
        updated-programs (map updater programs)
        controller-infos (controller-infos-fn name add-on-instructions controller-tag-defs datatypes)
        program-creator (create-program-hof controller-infos)
        executable-programs (map program-creator updated-programs)
        infos-by-name (into {} (map (fn [{:keys [name infos]}] [name infos]) executable-programs))
        tag-reader (tag-reader-hof infos-by-name)
        tag-writer (tag-writer-hof infos-by-name)
        ;when-next (cand/counted-when-next (mapv :name executable-programs) main-program-name)
        poison-ch (chan)
        ]
    (issue-warnings-fn tag-reader num-points main-program)
    (go-loop [counted 1
              laters [{:later/count-at 10 :later/fn (fn [cnt name]
                                                      (println (str "Going slow now at " cnt ", happen to be in program: " name))
                                                      (go (>! command-ch {:cmd :slow})))}]
              millis short-millis
              ]
      (let [{:keys [name infos runnable-routine]} (nth executable-programs (rem (dec counted) num-programs))
            ;[controller-defs program-defs] (tag-defs infos)
            read-program-tag (tag-reader name)
            write-program-tag (tag-writer name)
            [msg ch] (alts! [command-ch poison-ch (timeout millis)])]
        (cond

          (= ch command-ch)
          (let [_ (assert (map? msg) (str "msg received over control-ch must be a map, instead got: <" msg ">, <" (type msg) ">"))
                {:keys [cmd tag-name tag-value]} msg]
            (case cmd
              :slow
              (recur counted
                     laters
                     long-millis)
              :fast
              (recur counted
                     laters
                     short-millis)

              :fast-briefly
              (recur counted
                     (laters-burster laters counted)
                     short-millis)

              ;;
              ;; Note there will be a problem with :turn-on :turn-off and :touch-on if ever the tag-name is not
              ;; global but rather local to a particular program - you see write-program-tag already closes over
              ;; the current program name. If ever a problem augment by calling counted-when-next and so doing the
              ;; real updating in :later/fn as doing with :read-register and :write-register.
              ;;

              :turn-on
              (do
                (write-program-tag tag-name true)
                (recur counted
                       (laters-burster laters counted)
                       short-millis))

              :turn-off
              (do
                (write-program-tag tag-name false)
                (recur counted
                       (laters-burster laters counted)
                       short-millis))

              :touch-on
              (do
                (write-program-tag tag-name true)
                (recur counted
                       (conj (laters-burster laters counted)
                             {:later/count-at (+ counted num-programs)
                              :later/fn       (fn [_ _]
                                                (write-program-tag tag-name false))})
                       short-millis))

              :read-register
              (let [res (read-program-tag tag-name)]
                (>! command-ch res)
                (recur counted
                       (laters-burster laters counted)
                       short-millis))

              :write-register
              (let [_ (assert (not (nil? tag-value)))
                    _ (write-program-tag tag-name tag-value)]
                (recur counted
                       (laters-burster laters counted)
                       short-millis))
              ))

          (= ch poison-ch)
          (do
            (println (str "[plc-component] " dir-name " stopping"))
            (view-slots (:out infos))
            ;; Only clear debug state. So not like switching off the PLC. What is in the tags is still available
            ;; for post mortem via (q).
            (i/clear-debug-cache))

          :default
          (let [do-laters-now (filter #(>= counted (:later/count-at %)) laters)
                do-laters-later (remove #(>= counted (:later/count-at %)) laters)]
            ;(println (str "----> In timer for: " name " when: " millis " at: " counted ", MainProgram next at " (when-next counted)))
            (rou/scan-routine runnable-routine)
            (when (= counted num-programs)
              ((rou/run-instruction-hof infos ["MOV" ["0" "S:FS"]]) true))
            (when (and (not (= 1 counted)) (zero? (rem counted inject-weather-every)))
              (inject-weather-fn read-program-tag write-program-tag atmosphere-remote-ch))
            (doseq [later do-laters-now]
              ((:later/fn later) counted name))
            (recur (inc counted)
                   do-laters-later
                   millis))
          )))
    {:stop-f (fn stop! []
               (close! poison-ch))
     :id     :plc}))
