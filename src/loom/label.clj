(ns ^{:doc "Graph label protocol and implementations for records from loom.graph"
      :author "Justin Kramer"}
  loom.label
  (:use [loom.attr :only [add-attr remove-attr attr]])
  (:import [loom.graph SimpleGraph SimpleDigraph
            SimpleWeightedGraph SimpleWeightedDigraph
            FlyGraph FlyDigraph WeightedFlyGraph WeightedFlyDigraph]))

(defprotocol LabeledGraph
  (add-label [g node label] [g n1 n2 label] "Add a label to node or edge")
  (remove-label [g node] [g n1 n2] "Remove a label from a node or edge")
  (label [g node] [g n1 n2] "Return the label on a node or edge"))

(def default-labeled-graph-impl
  {:add-label (fn
                ([g node label]
                   (add-attr g node :label label))
                ([g n1 n2 label]
                   (add-attr g n1 n2 :label label)))
   :remove-label (fn
                   ([g node]
                      (remove-attr g node :label))
                   ([g n1 n2]
                      (remove-attr g n1 n2 :label)))
   :label (fn
            ([g node]
               (attr g node :label))
            ([g n1 n2]
               (attr g n1 n2 :label)))})

(extend SimpleGraph
  LabeledGraph
  default-labeled-graph-impl)

(extend SimpleDigraph
  LabeledGraph
  default-labeled-graph-impl)

(extend SimpleWeightedGraph
  LabeledGraph
  default-labeled-graph-impl)

(extend SimpleWeightedDigraph
  LabeledGraph
  default-labeled-graph-impl)

(extend FlyGraph
  LabeledGraph
  default-labeled-graph-impl)

(extend FlyDigraph
  LabeledGraph
  default-labeled-graph-impl)

(extend WeightedFlyGraph
  LabeledGraph
  default-labeled-graph-impl)

(extend WeightedFlyDigraph
  LabeledGraph
  default-labeled-graph-impl)

(defn labeled?
  "Return true if g satisfies LabeledGraph"
  [g]
  (satisfies? LabeledGraph g))