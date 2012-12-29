(ns genetics.test_graph
  (:use [clojure.test]
        [genetics.graph])
  (:import [genetics.graph DirectedGraph]))

(def g (DirectedGraph. {:A #{:B :C} :B #{} :C #{:B} :D #{:B}}))

(deftest test-digraph
  (is (= true (has-node? g :A)))
  (is (= true (has-node? g :C)))
  (is (= false (has-node? g :F)))
  (is (= true (adjacent? g :A :B)))
  (is (= true (adjacent? g :A :B)))
  (is (= false (adjacent? g :B :C))))

