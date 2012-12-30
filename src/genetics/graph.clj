(ns genetics.graph
  (:use [genetics.heap]))

(defprotocol IGraph
  (nodes [g] "Returns the nodes that comprise the graph")
  (edges [g] "Returns the set of edges contained in the graph")
  (adjacencies [g node] "Returns the set of nodes adjacent to the provided node")
  (has-node? [g node] "Returns true iff the provided node is present in the graph")
  (adjacent? [g n1 n2] "Returns true iff the provided nodes are adjacent in the graph"))

(defprotocol IWeightedGraph
  (weight [g n1 n2] "Returns the weight associated with the edge between the provided nodes"))

;; Default implementations
(defn- nodes-
  "Default implementation of nodes from Graph protocol"
  [adj]
  (set (keys adj)))

(defn- edges-
  "Default implementation of edges from Graph protocol"
  [adj]
  adj)

(defn- has-node?-
  "Default implementation of has-node? from Graph protocol"
  [nodes node]
  (contains? nodes node))

(defn- adjacencies-
  "Default implementation of adjacencies from Graph protocol"
  [adj node]
  (set (get adj node)))

(defn- adjacent?-
  "Default implementation of adjacent? from Graph protocol"
  [nodes adjacencies n1 n2]
  (and
    (contains? nodes n1)
    (contains? nodes n2)
    (contains? (get adjacencies n1) n2)))

(deftype Graph [adj]
  IGraph
    (nodes [this] (nodes- adj))
    (edges [this] (edges- adj))
    (adjacencies [this node] (adjacencies- adj node))
    (has-node? [this node] (has-node?- (nodes this) node))
    (adjacent? [this n1 n2] (adjacent?- (nodes this) adj n1 n2)))

(deftype WeightedGraph [wadj]
  IGraph
    (nodes [this] (nodes- wadj))
    (edges [this] (edges- wadj))
    (adjacencies [this node] (set (keys (get wadj node))))
    (has-node? [this node] (has-node?- (nodes this) node))
    (adjacent? [this n1 n2] (adjacent?- (nodes this) wadj n1 n2))
  IWeightedGraph
    (weight [this n1 n2] (get (get wadj n1) n2 Double/MAX_VALUE)))

(defn- edge-seq
  [wadj]
  ;; this needs to de-dupe [n1 n2] and [n2 n1] edges
  (for [n1 (keys wadj) [n2 weight] (get wadj n1)]
    [n1 n2 weight]))

(defn- edge-cmp
  [n1 n2]
  (min-cmp (n1 2) (n2 2)))

(defn- kruskal-
  "Internal implementation of Kruskal's algorithm"
  [nodes tree heap heap-insert heap-remove]
  (cond (empty? heap) tree  ;; in this case, no MST exists
        (= nodes (set (keys tree))) tree ;; in this case, we've completed the MST
        :else
          (let [[n1 n2 w] (heap-peek heap)]
            (if (or (contains? tree n1) (contains? tree n2))
              (recur nodes tree (heap-remove heap) heap-insert heap-remove)
              ;; adds an undirected edge, because we test for completion
              ;; by looking at the nodes in t2 - not ideal, and should be cleaned up
              (let [t1 (assoc-in tree [n1 n2] w)
                    t2 (assoc-in t1 [n2 n1] w)]
                (recur nodes t2 (heap-remove heap) heap-insert heap-remove))))))

(defn- kruskal
  "Kruskal's algorithm for computing a minimum weight spanning tree for
  the graph."
  [graph]
  (let [[heap-insert heap-remove] (make-heap edge-cmp)
        nodes (nodes graph)
        edges (edge-seq (edges graph))
        heap (reduce heap-insert [] edges)
        tree {}]
      (kruskal- nodes tree heap heap-insert heap-remove)))

(defn minimum-spanning-tree
  "Computes a minimum weight spanning tree for the graph."
  [graph]
  (kruskal graph))
