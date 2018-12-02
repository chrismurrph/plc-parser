(ns io-util
   (:require [clojure.java.io :as cio]
             [utils :as u]))

(def file cio/file)
(def delete-file cio/delete-file)

(defn delete-files [names]
  (doseq [name names]
    (delete-file name true)))

(defn exists? [name]
  (.exists (cio/file name)))

(defn mkdir [name]
  (.mkdir (java.io.File. name)))

(defn delete-recursively [fname]
  (let [func (fn [func f]
               (when (.isDirectory f)
                 (doseq [f2 (.listFiles f)]
                   (func func f2)))
               (cio/delete-file f true))]
    (func func (cio/file fname))))

(defn rmdir [name]
  (delete-recursively name))

(defn append-to-file
  "Uses spit to append to a file specified with its name as a string, or
   anything else that writer can take as an argument.  s is the string to
   append."
  [file-name s]
  (spit file-name s :append true))

;;
;; Does something and reports the output, but only once
;;
(defn debug-to-file [f]
  (assert (fn? f))
  (let [file-name "example.edn"]
    (if (not (.exists (cio/as-file file-name)))
      (spit file-name (u/pp-str (f))))))

;;
;; Does the function on the input but as a side effect outputs
;; the input for the crash, the first time the crash happens
;;
(defn debug-crashed-input [f input]
  (assert (fn? f))
  (let [file-name "example.edn"
        res (try
              (f input)
              (catch Exception x
                (println x)
                nil))]
    (if (and (not (.exists (cio/as-file file-name)))
             (nil? res))
      (spit file-name (u/pp-str input))
      res)))

(defn output-the-input []
  (let [input (line-seq (java.io.BufferedReader. *in*))]
    (doseq [v input]
      (println v))))
