(ns genetics.test-graph-algorithms
  (:use [clojure.test]
        [genetics.graph]
        [genetics.graph-algorithms])
  (:import [genetics.graph WeightedDiGraph]))

(def wt {:A {:B 1.2 :C 9.4}
         :B {:A 1.2}
         :C {:A 9.4}})

(def wg (WeightedDiGraph. wt))

(deftest test-mst
  (is (= wt (minimum-spanning-tree wg))))
