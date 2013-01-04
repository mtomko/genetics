(ns genetics.test-graph
  (:use [clojure.test]
        [genetics.graph])
  (:import [genetics.graph DiGraph WeightedDiGraph]))

(def g (DiGraph.
         {:A #{:B :C}
          :B #{}
          :C #{:B}
          :D #{:B}}))

(deftest test-digraph
  (is (= #{:A :B :C :D} (nodes g)))
  (is (= true (has-node? g :A)))
  (is (= true (has-node? g :C)))
  (is (= false (has-node? g :F)))
  (is (= #{:B :C} (adjacencies g :A)))
  (is (= true (adjacent? g :A :B)))
  (is (= true (adjacent? g :A :B)))
  (is (= false (adjacent? g :B :C))))

(def wt {:A {:B 1.2 :C 9.4}
         :B {:A 1.2}
         :C {:A 9.4}})

(def wg (WeightedDiGraph. wt))

(deftest test-wgraph
  (is (= #{:A :B :C} (nodes wg)))
  (is (= true (has-node? wg :A)))
  (is (= false (has-node? wg :F)))
  (is (= #{:B :C} (adjacencies wg :A)))
  (is (= true (adjacent? wg :A :B)))
  (is (= false (adjacent? wg :B :C)))
  (is (= 1.2 (weight wg :A :B)))
  (is (= Double/MAX_VALUE (weight wg :B :C))))

(deftest test-mst
  (is (= wt (minimum-spanning-tree wg))))
