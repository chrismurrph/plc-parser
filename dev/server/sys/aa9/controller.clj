(ns sys.aa9.controller
  (:require [sys.domain-utils :as du]
            [domain-utils :as u]
            [io-util :as io]))

;; These keyed by slot number
(def hardware-input-data-functions {4 (fn [] {:val/val 26})})

;; Here the channel looking for data from is required
(def hardware-input-ch-functions {5 (fn [_]
                                      (let [rand-between (u/big-dec "one" (+ (/ (- (rand-int 40) 20) 10) 50))
                                            _ (assert (and (>= rand-between 48) (<= rand-between 52)))]
                                        {:val/val rand-between}))
                                  6 (fn [_]
                                      (let [rand-between (u/big-dec "two" (+ (/ (- (rand-int 40) 20) 10) 50))
                                            _ (assert (and (>= rand-between 48) (<= rand-between 52)))]
                                        {:val/val rand-between}))
                                  7 (fn [ch] {:val/val (* 400 ch)})})
(def hardware-input-underrange-ch-functions {5 (fn [ch] {:val/val 1})
                                             6 (fn [ch] {:val/val 1})})
(def hardware-input-overrange-ch-functions {5 (fn [ch] {:val/val 1})
                                            6 (fn [ch] {:val/val 1})})

(def hardware-output-ch-functions {})

(defn- input-info [output]
  {:info/input-data-functions            hardware-input-data-functions
   :info/output-input-data-functions     (:hardware-output-input-data-functions output)
   :info/input-chan-functions            hardware-input-ch-functions
   :info/input-underrange-chan-functions hardware-input-underrange-ch-functions
   :info/input-overrange-chan-functions  hardware-input-overrange-ch-functions
   })

(defn- output-info [output]
  {:info/output-data-functions (:hardware-output-data-functions output)
   :info/output-chan-functions hardware-output-ch-functions
   })

;;
;; Assumes that every slot is 32 bits. When need to change look for 32 and "DINT".
;;
(def slot-size 32)
(def all-output-slots [2 3])

(defn controller-infos [site-name add-on-instructions controller-tag-defs datatypes]
  (let [slots all-output-slots
        _ (io/delete-files (mapv #(du/log-name site-name %) slots))
        out (du/output slot-size site-name slots)
        input (input-info out)
        output (output-info out)
        ]
    (merge {
            :out                 out
            :datatypes           datatypes
            :controller-tag-defs controller-tag-defs
            :add-on-instructions add-on-instructions
            }
           input output)))

(def dir-name "/home/chris/IdeaProjects/virtual_plc/sgc")
(def main-program "MainProgram")
(def common-program "Common")
(def modbus-program "ModbusMasterTCP")

