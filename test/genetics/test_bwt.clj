(ns genetics.test-bwt
  (:use [clojure.test]
        [genetics.bwt]))

(deftest test-forward-bwt
  (is (= "annb$aa" (apply str (forward-bwt "banana$")))))
