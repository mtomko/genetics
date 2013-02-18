(ns genetics.util)

(defn key-set
  "Returns the set of keys in the provided map."
  [m] (-> m keys set))

(defn uniq
  "Eliminates repeated entries in the provided collection."
  [coll]
  (if (empty? coll) coll
    (letfn [(uniq- [prev coll acc]
              (if (empty? coll) acc
                  (let [elem (first coll)]
                    (if (= elem prev) (recur elem (rest coll) acc)
                      (recur elem (rest coll) (cons elem acc))))))]
      (let [elem (first coll)]
        (reverse (uniq- elem (rest coll) (list elem)))))))
