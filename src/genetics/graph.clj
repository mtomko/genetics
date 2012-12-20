(ns genetics.graph
  (:require clojure.set))

(defprotocol Graph
  (nodes [g] "Returns the nodes that comprise the graph")
  (add-nodes [g nodes] "Adds the provided nodes to the graph")
  (has-node? [g node] "Returns true iff the provided node is present in the graph" )
  (edges [g] "Returns the set of edges contained in the graph" ))

(deftype DirectedGraph [nodes edges]
  Graph
    (nodes [this] nodes)
    (add-nodes [this n] (update-in [:nodes] conj n))
    (has-node? [this n] (contains? nodes n))
    (edges [this] edges))
