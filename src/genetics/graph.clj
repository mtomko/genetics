(ns genetics.graph
  (:require clojure.set))

(defprotocol Graph
  (nodes [g] "Returns the nodes that comprise the graph")
  (edges [g] "Returns the set of edges contained in the graph")
  (adjacencies [g node] "Returns the set of nodes adjacent to the provided node")
  (has-node? [g node] "Returns true iff the provided node is present in the graph")
  (adjacent? [g n1 n2] "Returns true iff the provided nodes are adjacent in the graph"))

(defprotocol WeightedGraph
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

(deftype SimpleGraph [adj]
  Graph
    (nodes [this] (nodes- adj))
    (edges [this] (edges- adj))
    (adjacencies [this node] (adjacencies- adj node))
    (has-node? [this node] (has-node?- (nodes this) node))
    (adjacent? [this n1 n2] (adjacent?- (nodes this) adj n1 n2)))

(deftype SimpleWeightedGraph [wadj]
  Graph
    (nodes [this] (nodes- wadj))
    (edges [this] (edges- wadj))
    (adjacencies [this node] (set (keys (get wadj node))))
    (has-node? [this node] (has-node?- (nodes this) node))
    (adjacent? [this n1 n2] (adjacent?- (nodes this) wadj n1 n2))
  WeightedGraph
    (weight [this n1 n2] (get (get wadj n1) n2 Double/MAX_VALUE)))