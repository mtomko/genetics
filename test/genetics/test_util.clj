(ns genetics.test-util
  (:use [clojure.test]
        [genetics.util]))

(deftest test-uniq
  (is (= (list 1 2 3 4 5) (uniq (list 1 2 2 2 3 4 5)))))
