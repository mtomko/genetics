(ns genetics.test-graph-algorithms
  (:use [clojure.test]
        [genetics.graph]
        [genetics.graph-algorithms])
  (:import [genetics.graph WeightedDiGraph WeightedGraph]))

(def wt
  {:A {:B 1.2 :C 9.4}
   :B {:A 1.2}
   :C {:A 9.4}})

(def large-wadj
  {:A {:B 7 :D 5}
   :B {:A 7 :C 8 :D 9 :E 7}
   :C {:B 8 :E 5}
   :D {:A 5 :B 9 :E 15 :F 6}
   :E {:B 7 :C 5 :D 15 :F 8 :G 9}
   :F {:D 6 :E 8 :G 11}
   :G {:E 9 :F 11}})

(def large-wt
  {:A {:B 7 :D 5}
   :B {:A 7 :E 7}
   :C {:E 5}
   :D {:A 5 :F 6}
   :E {:B 7 :G 9}
   :F {:D 6}
   :G {:E 9}})

(def wg (WeightedDiGraph. wt))

(def large-wg (WeightedGraph. large-wadj))

(deftest test-mst
  (is (= wt (minimum-spanning-tree wg)))
  (is (= large-wt (minimum-spanning-tree large-wg))))
