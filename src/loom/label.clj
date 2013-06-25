(ns ^{:doc "Graph label protocol and implementations for records from loom.graph"
      :author "Justin Kramer"}
  loom.label
  (:require loom.basic-implementation)
  (:use [loom.attr :only [add-attr remove-attr attr]]
        [loom.graph :only [add-nodes add-edges add-node add-edge]])
  (:import [loom.basic_implementation BasicUndirectedGraph BasicDirectedGraph]))

(defprotocol LabeledGraph
  (add-label [g node label] [g n1 n2 label] "Add a label to node or edge")
  (remove-label [g node] [g n1 n2] "Remove a label from a node or edge")
  (label [g node] [g n1 n2] "Return the label on a node or edge"))

(def default-labeled-graph-impl
  {:add-label (fn
                ([g node label]
                   (add-attr (add-node g node) node :label label))
                ([g n1 n2 label]
                   (add-attr (add-edge g n1 n2) n1 n2 :label label)))
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

(extend BasicUndirectedGraph
  LabeledGraph
  default-labeled-graph-impl)

(extend BasicDirectedGraph
  LabeledGraph
  default-labeled-graph-impl)

(defn labeled?
  "Return true if g satisfies LabeledGraph"
  [g]
  (satisfies? LabeledGraph g))

(defn add-labeled-nodes
  "Adds nodes and respective labels to graph g:
  (add-labeled-nodes n1 \"n1 label\" n2 \"n2 label\")"
  [g & nodes+labels]
  (reduce
   (fn [g [node label]]
     (-> g
         (add-nodes node)
         (add-label node label)))
   g (partition 2 nodes+labels)))

(defn add-labeled-edges
  "Adds edges and respective labels to graph g:
  (add-labeled-edges [n1 n2] \"label 1\" [n2 n3] \"label 2\")"
  [g & edges+labels]
  (reduce
   (fn [g [[n1 n2 :as edge] label]]
     (-> g
         (add-edges edge)
         (add-label n1 n2 label)))
   g (partition 2 edges+labels)))