(ns genetics.util)

(defn key-set
  "Returns the set of keys in the provided map."
  [m] (-> m keys set))
