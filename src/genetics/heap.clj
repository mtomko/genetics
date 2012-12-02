; Simple min heap implementation in pure Clojure.
; Implemented by Mark Tomko, mjt0229@gmail.com, December 2009
; Revised December 2012
; Copyright 2009, 2012 All rights reserved
(ns genetics.heap)

(defn- swap
  "Returns a copy of the provided vector with the ith and jth elements
  interchanged."
  [v #^Integer i #^Integer j]
  (-> v (assoc i (v j)) (assoc j (v i))))

(defn- index-of-parent
  "Returns the index of the parent element of the provided index in a
  binary heap."
  [#^Integer i]
  (quot (- i 1) 2))

(defn- indexes-of-children
  "Returns a vector containing the two indexes where the children of
  the provided index would be located in a binary heap.  It is
  possible that these locations are beyond the length of the heap."
  [#^Integer i]
  (let [i2 (* 2 i)]
    (vector (+ 1 i2) (+ 2 i2))))

(defn- index-of-min
  "Given a a comparison function, a collection, and 2 indexes,
  returns the index corresponding to the smallest element in the
  collection, according to the comparison function."
  ([cmp-fn heap i]
    (if (>= i (count heap)) nil i))
  ([cmp-fn heap i j]
    (cond
      (>= i (count heap)) (index-of-min cmp-fn heap j)
      (>= j (count heap)) (index-of-min cmp-fn heap i)
      :default
        (if (< (cmp-fn (heap i) (heap j)) 1) i j))))

(defn- heap-percolate-up
  "Returns a copy of the heap with the element at the provided index
  percolated toward the root until it is greater than its parent,
  according the the comparison function."
  [cmp-fn heap #^Integer index]
  (if (= 0 index) heap
    (let [element (heap index) pindex (index-of-parent index)]
      ; if the element is greater than its parent, return the heap
      (if (> (cmp-fn element (heap pindex)) -1) heap
        (recur cmp-fn (swap heap index pindex) pindex)))))

(defn- heap-percolate-down
  "Returns a copy of the heap with the element at the provided index
  percolated toward the leaves until it is less than or equal to its
  smallest child according to the comparison function, or it has no
  children."
  [cmp-fn heap #^Integer index]
  (if (< (count heap) index) heap
    (let [[child1 child2] (indexes-of-children index)
          child (index-of-min cmp-fn heap child1 child2)]
      (if (nil? child) heap
        ; if the element is less than or equal to its smallest child, return the heap
        (if (< (cmp-fn (heap index) (heap child)) 1) heap
          (recur cmp-fn (swap heap index child) child))))))

(defn- heap-insert
  "Returns the provided heap with the given element inserted into its
  proper position, using the provided comparison function."
  [cmp-fn heap element]
  (heap-percolate-up cmp-fn (conj heap element) (count heap)))

(defn- heap-remove
  "Returns the provided heap with its minimum element removed."
  [cmp-fn heap]
  (if (empty? heap) heap
    (let [new-heap-size (dec (count heap))
          new-heap (subvec (swap heap 0 new-heap-size) 0 new-heap-size)]
      ; percolate the element back down
      (heap-percolate-down cmp-fn new-heap 0))))

(defn heap-peek
  "Returns the value of the minimum element in the provided heap."
  [heap]
  (first heap))

(defn heap-remove-by
  "Returns a method that will use the provided comparison function to
  remove the minimum element from a heap."
  [cmp-fn]
  (partial heap-remove cmp-fn))

(defn heap-insert-by
  "Returns a method that will use the provided comparison function to
  insert an element into a heap."
  [cmp-fn]
  (partial heap-insert cmp-fn))

(defn make-heap
  "Returns a vector containing a heap insert function and a heap remove
  function, based on the provided element comparison function."
  [cmp-fn]
  [(heap-insert-by cmp-fn)
   (heap-remove-by cmp-fn)])

(defn min-cmp
  "A simple comparison function that returns -1 if v1 is less than v2,
  0 if they are equal, and 1 if v1 is greater than v2.  Providing this
  function to heap-insert-with and heap-remove-with will give functions
  for inserting and removing from a min heap."
  [v1 v2]
  (cond
    (= v1 v2) 0
    (< v1 v2) -1
    :default 1))

(defn max-cmp
  "A simple comparison function that returns -1 if v1 is greater than v2,
  0 if they are equals and 1 if v1 is less than v2.  Providing this
  function to heap-insert-with and heap-remove-with wil give functions
  for inserting and removing from a max heap."
  [v1 v2]
  (- (min-cmp v1 v2)))