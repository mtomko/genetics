(ns genetics.test_graph
  (:use [clojure.test]
        [genetics.graph])
  (:import [genetics.graph SimpleGraph]))

(def g (SimpleGraph. {:A #{:B :C} :B #{} :C #{:B} :D #{:B}}))

(deftest test-simplegraph
  (is (= #{:A :B :C :D} (nodes g)))
  (is (= true (has-node? g :A)))
  (is (= true (has-node? g :C)))
  (is (= false (has-node? g :F)))
  (is (= #{:B :C} (adjacencies g :A)))
  (is (= true (adjacent? g :A :B)))
  (is (= true (adjacent? g :A :B)))
  (is (= false (adjacent? g :B :C))))

