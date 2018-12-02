(ns context)

(def context (atom nil))

(defn out-context []
  (let [{:keys [routine-name rung-num]} @context]
    (when routine-name
      (str ", @ " routine-name "|" rung-num))))