(ns genetics.graph)

(defn- key-set
  "Returns the set of keys in the provided map."
  [m] (set (keys m)))

(defprotocol IGraph
  (nodes [g] "Returns the nodes that comprise the graph")
  (edges [g] "Returns the set of edges contained in the graph")
  (adjacencies [g node] "Returns the set of nodes adjacent to the provided node")
  (has-node? [g node] "Returns true iff the provided node is present in the graph")
  (adjacent? [g n1 n2] "Returns true iff the provided nodes are adjacent in the graph"))

(defprotocol IWeightedGraph
  (weight [g n1 n2] "Returns the weight associated with the edge between the provided nodes"))

;; Default implementations
(defn- def-nodes
  "Default implementation of nodes from Graph protocol"
  [adj]
  (key-set adj))

(defn- def-edges
  "Default implementation of edges from Graph protocol"
  [adj]
  adj)

(defn- def-has-node?
  "Default implementation of has-node? from Graph protocol"
  [nodes node]
  (contains? nodes node))

(defn- def-adjacencies
  "Default implementation of adjacencies from Graph protocol"
  [adj node]
  (set (get adj node)))

(defn- def-adjacent?
  "Default implementation of adjacent? from Graph protocol"
  [nodes adjacencies n1 n2]
  (and
    (contains? nodes n1)
    (contains? nodes n2)
    (contains? (get adjacencies n1) n2)))

(deftype Graph [adj]
  IGraph
    (nodes [this] (def-nodes adj))
    (edges [this] (def-edges adj))
    (adjacencies [this node] (def-adjacencies adj node))
    (has-node? [this node] (def-has-node? (nodes this) node))
    (adjacent? [this n1 n2] (def-adjacent? (nodes this) adj n1 n2)))

(deftype WeightedGraph [wadj]
  IGraph
    (nodes [this] (def-nodes wadj))
    (edges [this] (def-edges wadj))
    (adjacencies [this node] (key-set (get wadj node)))
    (has-node? [this node] (def-has-node? (nodes this) node))
    (adjacent? [this n1 n2] (def-adjacent? (nodes this) wadj n1 n2))
  IWeightedGraph
    (weight [this n1 n2] (get-in wadj [n1 n2] Double/MAX_VALUE)))

(defrecord WeightedEdge [node1 node2 weight]
  Comparable
    (compareTo [this other]
      (letfn [(make-evec [edge]
                (vector (:weight edge) (:node1 edge) (:node2 edge)))]
        (compare (make-evec this) (make-evec other)))))

(defn- edge-seq
  [wadj]
  (for [n1 (keys wadj) [n2 weight] (get wadj n1)]
    (->WeightedEdge n1 n2 weight)))

(defn- kruskal
  "Kruskal's algorithm for computing a minimum weight spanning tree for
  the graph."
  ([graph]
  ;; initialize the nodes, heap and MST, and then delegate
  (let [nodes (nodes graph)
        edges (apply sorted-set (edge-seq (edges graph)))
        tree {}]
      (kruskal nodes edges tree)))
  ([nodes edges tree]
    (cond (empty? edges) {}               ;; in this case, no MST exists
          (= nodes (key-set tree)) tree   ;; in this case, we've completed the MST
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
