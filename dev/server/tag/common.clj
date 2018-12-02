(ns tag.common
  (:require [running :as run]
            [debug :as deb]))

;;
;; Here I'm assuming that the character "0" becomes 0 rather than 48, which (-> "0" first long) would give us.
;; Actually I need to know if they are in hex or what. For now (-> "0" first long) will work so is prefered to
;; (-> x str utils/string->int), which won't work for letters!
;; Not 100% sure this is correct, and had better always use this function for when need to fix it globally!
(defn character->integer [x]
  (-> x long))

(defn simple-swap! [f k v meta]
  ;(println "meta into observe: <" meta ">")
  (deb/observe-key k v meta)
  (swap! run/tags-repo f k v))

;;
;; upsert - if there will update it, o/wise will insert a new one
;;
(defn get-swap!
  ([name desc new-value]
   (get-swap! name desc new-value nil nil))
  ([name desc new-value element-type-fn m]
   (let [
         _ (when (map? new-value)
             (assert (some #{:val/type} (keys new-value)) (str "Need to have a type: " name ", " new-value)))
         _ (when (sequential? new-value)
             ;(println (str "b/c " desc ", new val: " new-value " for " name))
             )
         existing-key (some #(when (= (:tag/name %) name) %) (keys @run/tags-repo))
         ;_ (when (some watch-changes-for [name])
         ;    (println (str "Existing tag info for " name " is <" (:tag/type existing-key) ">. New value want is <" (:val/val new-value) ">. Existing value: <" (:val/val (get @run/tags-repo existing-key)) ">")))
         ]
     (if existing-key
       (simple-swap! assoc existing-key new-value m)
       (let [_ (assert element-type-fn (str "Going to need to supply an element-type-fn here for " name))
             element-type (element-type-fn name)
             ;_ (assert (not= "TIMER" element-type) (str "Bad function: " element-type-fn))
             _ (assert element-type (str "Creating a tag for the first time, we have to know its type, not merely name: " name))]
         (simple-swap! assoc {:tag/name name :tag/type element-type :tag/desc desc} new-value m))))))

(defn type-mapping [custom-tag]
  (let [tag-name (-> custom-tag second second)
        tag-type (-> custom-tag (nth 2) second second)]
    {:datatype tag-type
     :reification tag-name}))

;;
;; We need the variables that are custom typed in order to recognise the top level array even when as go
;; through the rung instructions we are only going to see tags in dot form. Thus "L1.S3.DO" will be
;; recognised as having its data at "L1".
;;
(defn get-custom-grouped-tags [infos]
  (let [controller-tag-defs (-> infos :controller-info :controller-tag-defs)
        custom-tags (filter #(= :custom-t (-> % (nth 2) second first)) controller-tag-defs)
        custom-tag-objects (map type-mapping custom-tags)
        custom-grouped (group-by :datatype custom-tag-objects)]
    custom-grouped)
  )

