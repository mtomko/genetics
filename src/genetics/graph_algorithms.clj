(ns genetics.graph-algorithms
  (:use [clojure.set]
        [genetics.util]
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
        forest (into {} (map #(vector % #{%}) nodes))
        tree {}]
      (kruskal nodes edges forest tree)))
  ([nodes edges forest tree]
    (if (empty? edges)
      (if (= nodes (get forest (first nodes)))
        tree {})
      (let [[n1 n2 w] (evec (first edges))
            n1-adj (get forest n1)
            n2-adj (get forest n2)]
        (if (= n1-adj n2-adj)
          (recur nodes (rest edges) forest tree)
          (let [new-adj (union n1-adj n2-adj)
                ;; update the forest to show the trees for each node
                new-forest (into forest (map #(vector % new-adj) new-adj))
                ;; adds an edge in both directions
                new-tree (-> tree (assoc-in [n1 n2] w) (assoc-in [n2 n1] w))]
            (recur nodes (rest edges) new-forest new-tree)))))))

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
