(ns ^{:doc "Defines derived graphs from existing graphs using maps and filters."
      :author "Horst Duchene"}
  loom.derived
  (:require [loom.graph :refer [digraph graph
                                nodes edges successors fly-graph predecessors
                                add-nodes* add-edges*
                                directed?]]))

(defn mapped-by
  "Return a Graph or a DiGraph which has as nodeset (set (map f (nodes g)). An
  edge [uu, vv] is an edge in the result iff g has an edge [u, v] such that [uu,
  vv] = [(f u), (f v)]."
  [f g]
  (-> (if (directed? g) (digraph) (graph))
      (add-nodes* (map f (nodes g)))
      (add-edges* (map #(map f %) (edges g)))))

(defn subgraph-starting-from
  "Return a subgraph of the given graph which contains all nodes and edges that
  can be reached from the given start node."
  [g start]
  (if (directed? g)
    (fly-graph :start start
               :successors #(successors g %)
               :predecessors #(predecessors g %))
    (fly-graph :start start
               :successors #(successors g %))))

(defn nodes-filtered-by
  "Return a new graph which has as nodes all nodes of g which satisfy the
  predicate."
  [pred g]
  (-> (if (directed? g) (digraph) (graph))
      (add-nodes* (filter pred (nodes g)))
      (add-edges* (filter #(and (pred (first %))
                                (pred (last %)))
                          (edges g)))))

(defn border-subgraph
  "Subgraph of g containing nodes which belong to the given subset of nodes and successors
  which do not belong to subset. Inner edges are also excluded."
  [g subset]
  (let [subset (set subset)
        border (->> subset
                    (filter (fn [n] (not (every? subset (successors g n)))))
                    (set))
        coborder (->> (flatten (map #(seq (successors g %)) border))
                      (filter #(not (subset %)))
                      (set))
        nodes (->> (clojure.set/union border coborder)
                   (filter #(not (nil? %)))
                   (set))
        fsuccessors (fn [n] (->> (successors g n)
                                 (filter coborder)))]
    (if (directed? g)
      (fly-graph :nodes nodes
                 :successors fsuccessors
                 :predecessors (fn [n] (filter #(nodes %)) (predecessors g n)))
      (fly-graph :nodes nodes
                 :successors fsuccessors))))
