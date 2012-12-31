(ns genetics.test-core
  (:use [clojure.test]
        [genetics.core]))

(def progeny
  (into {}
    (map
      #(vector (genotype (key %)) (val %))
      {#{:A :B :C } 479
       #{:a :b :c } 473
       #{:A :b :c } 15
       #{:a :B :C } 13
       #{:A :B :c } 9
       #{:a :b :C } 9
       #{:A :b :C } 1
       #{:a :B :c } 1})))


(deftest test-compute-distance
  (is (= 3.0 (compute-distance :A :B progeny)))
  (is (= 4.6 (compute-distance :A :C progeny)))
  (is (= 2.0 (compute-distance :B :C progeny))))
