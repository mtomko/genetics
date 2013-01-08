(ns genetics.graph-algorithms
  (:use [genetics.util]
        [genetics.graph]))

(defn- edge-sort
  [e1 e2]
    (let [c (compare (:weight e1) (:weight e2))]
      (if (not= c 0)
        c
        (compare e1 e2))))

(defn- kruskal
  "Kruskal's algorithm for computing a minimum weight spanning tree for
  the graph."
  ([graph]
  ;; initialize the nodes, heap and MST, and then delegate
  (let [nodes (nodes graph)
        edges (apply sorted-set-by edge-sort (edge-list graph))
        tree {}]
      (kruskal nodes edges tree)))
  ;; this recursive implementation could probably be reframed in terms of
  ;; reduce or some other primitive
  ([nodes edges tree]
    (cond (empty? edges) {}               ;; in this case, no MST exists
          (= nodes (key-set tree)) tree   ;; this termination condition is broken!
          :else                           ;; check the next smallest edge and continue
            (let [edge (first edges)
                  n1 (:node1 edge)
                  n2 (:node2 edge)
                  w (:weight edge)]
              (if (and (contains? tree n1) (contains? tree n2))
                (recur nodes (rest edges) tree)
                ;; adds an edge in both directions, because we test for completion
                ;; by looking at the nodes in t2 - not ideal, and should be cleaned up
                (let [new-tree (-> tree (assoc-in [n1 n2] w) (assoc-in [n2 n1] w))]
                  (recur nodes (rest edges) new-tree)))))))

(defn minimum-spanning-tree
  "Computes a minimum weight spanning tree for the graph."
  [graph]
  (kruskal graph))

(defn total-weight
  "Computes the total weight of the graph. If the graph
  is undirected and explicitly contains both directed edges,
  the value returned by this function will be precisely twice
  the correct value."
  [graph]
  (reduce + (mapcat vals (for [[k v] (edges graph)] v))))
