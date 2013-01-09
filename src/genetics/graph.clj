(ns genetics.graph
  (:use [genetics.util]))

(defprotocol PEvec
  (evec [e]))

(defrecord Edge [node1 node2]
  PEvec
  (evec [this] (vector node1 node2))
  Comparable
    (compareTo [this other]
      (compare (evec this) (evec other))))

(defrecord WeightedEdge [node1 node2 weight]
  PEvec
  (evec [this] (vector node1 node2 weight))
  Comparable
    (compareTo [this other]
      (compare (evec this) (evec other))))

(defprotocol PGraph
  (nodes [g] "Returns the nodes that comprise the graph")
  (edges [g] "Returns the set of edges contained in the graph")
  (edge-list [g] "Returns a list of records for each edge in the graph")
  (adjacencies [g node] "Returns the set of nodes adjacent to the provided node")
  (has-node? [g node] "Returns true iff the provided node is present in the graph")
  (adjacent? [g n1 n2] "Returns true iff the provided nodes are adjacent in the graph"))

(defprotocol PWeightedGraph
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
  [adjacencies n1 n2]
    (contains? (get adjacencies n1) n2))

(deftype DiGraph [adj]
  PGraph
    (nodes [this] (def-nodes adj))
    (edges [this] (def-edges adj))
    (edge-list [this]
      (for [n1 (keys adj) n2 (get adj n1)]
        (->Edge n1 n2)))
    (adjacencies [this node] (def-adjacencies adj node))
    (has-node? [this node] (def-has-node? (nodes this) node))
    (adjacent? [this n1 n2] (def-adjacent? adj n1 n2)))

(deftype Graph [adj]
  PGraph
    (nodes [this] (def-nodes adj))
    (edges [this] (def-edges adj))
    (edge-list [this]
      (let [all-edges (for [n1 (keys adj) n2 (get adj n1)] (vec (sort [n1 n2])))]
        (for [[n1 n2] (set all-edges)]
           (->Edge n1 n2))))
    (adjacencies [this node] (def-adjacencies adj node))
    (has-node? [this node] (def-has-node? (nodes this) node))
    (adjacent? [this n1 n2] (or (def-adjacent? adj n1 n2) (def-adjacent? adj n2 n1))))

(deftype WeightedDiGraph [wadj]
  PGraph
    (nodes [this] (def-nodes wadj))
    (edges [this] (def-edges wadj))
    (edge-list [this]
      (for [n1 (keys wadj) [n2 weight] (get wadj n1)]
        (->WeightedEdge n1 n2 weight)))
    (adjacencies [this node] (key-set (get wadj node)))
    (has-node? [this node] (def-has-node? (nodes this) node))
    (adjacent? [this n1 n2] (def-adjacent? wadj n1 n2))
  PWeightedGraph
    (weight [this n1 n2] (get-in wadj [n1 n2] Double/MAX_VALUE)))

(deftype WeightedGraph [wadj]
  PGraph
    (nodes [this] (def-nodes wadj))
    (edges [this] (def-edges wadj))
    (edge-list [this]
      (let [all-edges
           ;; get all adjacencies (n1, n2, w)
            (for [n1 (keys wadj) [n2 weight] (get wadj n1)]
              ;; return the vector [n1 n2 w] where n1 < n2
              (conj (vec (sort [n1 n2])) weight))]
        (for [[n1 n2 weight] (set all-edges)]
            (->WeightedEdge n1 n2 weight))))
    (adjacencies [this node] (key-set (get wadj node)))
    (has-node? [this node] (def-has-node? (nodes this) node))
    (adjacent? [this n1 n2] (or (def-adjacent? wadj n1 n2) (def-adjacent? wadj n2 n1)))
  PWeightedGraph
    (weight [this n1 n2] (get-in wadj [n1 n2] Double/MAX_VALUE)))
