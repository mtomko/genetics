(ns genetics.test-heap
  (:use [clojure.test]
        [genetics.heap]))

(deftest heap-insert
  (let [heap-insert (heap-insert-by compare)]
    (is (= [1]                (heap-insert [] 1)))
    (is (= [1 2]              (heap-insert [1] 2)))
    (is (= [0 2 1]            (heap-insert [1 2] 0)))
    (is (= [0 2 1 4]          (heap-insert [0 2 1] 4)))
    (is (= [0 2 1 4 3]        (heap-insert [0 2 1 4] 3)))
    (is (= [0 2 1 4 3 2]      (heap-insert [0 2 1 4 3] 2)))
    (is (= [0 2 0.5 4 3 2 1]  (heap-insert [0 2 1 4 3 2] 0.5)))
    (is (= [0 2 0.5 4 3 2 1]  (reduce heap-insert [] '(1 2 0 4 3 2 0.5))))))

(deftest heap-remove
  (let [heap-remove (heap-remove-by compare)]
    (is (= []  (heap-remove [])))
    (is (= []  (heap-remove [0.5])))
    (is (= [0.5 2 1 4 3 2]  (heap-remove [0 2 0.5 4 3 2 1])))))
