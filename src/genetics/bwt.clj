(ns genetics.bwt)

;; Forward Burrows-Wheeler transformation

(defn rotate-1
  "Rotates the first element in the provided sequence to the end "
  [sequence]
  (conj (subvec sequence 1) (first sequence)))

(defn rotate-n
  "Rotates the first n elements in the provided vector to the end"
  [sequence n]
  (concat (subvec sequence n) (subvec sequence 0 n)))

(defn- all-rotations* 
  "Adds n rotations of the provided sequence to the list of existing rotations"
  [sequence rotations n]
  (if (> 1 n) rotations
    (let [new-sequence (rotate-1 sequence)]
      (recur (vec new-sequence) (cons sequence rotations) (dec n)))))

(defn all-rotations
  "Returns the list of all possible rotations of the provided input sequence"
  [sequence]
  (let [length (count sequence)]
    (all-rotations* (vec sequence) [] length)))

(defn forward-bwt
  "Performs the Burrows-Wheeler transformation on the provided input sequence"
  [sequence]
  (map last (sort (all-rotations sequence))))

