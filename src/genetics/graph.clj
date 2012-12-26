(ns genetics.graph
  (:require clojure.set))

(defprotocol Graph
  (nodes [g] "Returns the nodes that comprise the graph")
  (edges [g] "Returns the set of edges contained in the graph")
  (has-node? [g node] "Returns true iff the provided node is present in the graph" )
  (adjacent? [g n1 n2] "Returns true iff the provided nodes are adjacent in the graph")
  (adjacencies [g node] "Returns the set of nodes adjacent to the provided node")
  )

(defprotocol WeightedGraph
  (weight [n1 n2] "Returns the weight associated with the edge between the provided nodes"))

(deftype DirectedGraph [adj]
  Graph
    (nodes [this] (set (keys adj)))
    (edges [this] adj)
    (has-node? [this node] (contains? (nodes this) node))
    (adjacent? [this n1 n2] (and (has-node? this n1)
                              (contains? (adjacencies this n1) n2)))
    (adjacencies [this node] (set (get adj node))))
