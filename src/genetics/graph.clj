(ns genetics.graph
  (:require clojure.set))

(def g {:a #{:b :c :d} :b {:e :c}})

(defn add-node
  [graph node]
  (assoc graph node #{}))

(defn add-directed-edge
  [graph v0 v1]
  (if (contains? graph v0)
    (assoc graph (conj (v0 graph) v1))
    (assoc graph v0 #{v1})))