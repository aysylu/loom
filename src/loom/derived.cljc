(ns ^{:doc "Defines derived graphs from existing graphs using maps and filters."
      :author "Horst Duchene"}
  loom.derived
  (:require [loom.graph :refer [digraph graph
                                nodes edges successors fly-graph predecessors
                                add-nodes* add-edges*
                                directed?]]
            clojure.set))

(defn mapped-by
  "Returns a Graph or a DiGraph which has as nodeset (set (map f (nodes g)). An
  edge [uu, vv] is an edge in the resulting graph iff g has an edge [u, v] such
  that [uu, vv] = [(f u), (f v)]."
  [f g]
  (-> (if (directed? g) (digraph) (graph))
      (add-nodes* (map f (nodes g)))
      (add-edges* (map #(map f %) (edges g)))))

(defn subgraph-reachable-from
  "Returns a subgraph of the given graph which contains all nodes and edges that
  can be reached from the given start node."
  [g start]
  (if (directed? g)
    (fly-graph :start start
               :successors #(successors g %)
               :predecessors #(predecessors g %))
    (fly-graph :start start
               :successors #(successors g %))))

(defn nodes-filtered-by
  "Returns a new graph which has as nodes all nodes of g which satisfy the
  predicate."
  [pred g]
  (-> (if (directed? g) (digraph) (graph))
      (add-nodes* (filter pred (nodes g)))
      (add-edges* (filter #(and (pred (first %))
                                (pred (last %)))
                          (edges g)))))
(defn edges-filtered-by
  "Returns a new graph which has as nodes all nodes of g and edges which satisfy
  the predicate."
  [pred g]
  (-> (if (directed? g) (digraph) (graph))
      (add-nodes* (nodes g))
      (add-edges* (filter pred (edges g)))))

(defn bipartite-subgraph
  "Returns the subgraph of g containing only the edge subset E which lead
  outside of the given subset. The nodes of the resulting graph are the start
  and endpoints of these edges.
  The resulting graph is thus the bipartite graph (U,V,E) where
  U = subset, V = (map last E).
  (see https://en.wikipedia.org/wiki/Bipartite_graph)."
  [g subset]
  (let [ ;; force set semantics
        subset (set subset)
        edges (filter #(and (subset (first %))
                            (not (subset (last %))))
                      (edges g))]
    (-> (if (directed? g) (digraph) (graph))
        (add-nodes* (flatten edges))
        (add-edges* edges))))

(defn surroundings
  "Returns th subgraph of g containing nodes which belong to the given subset of g and direct
  successors of them."
  [g subset]
  (let [nodes-of-resulting-graph (->> subset
                                      ;; first all successors of subset
                                      (map #(seq (successors g %)))
                                      flatten
                                      ;; add subset itself
                                      (clojure.set/union (set subset))
                                      ;; don't want to have nil as nodes
                                      (remove nil?)
                                      ;; remove duplicates
                                      set)
        fsuccessors (fn [n] (->> (successors g n)
                                 (filter #(nodes-of-resulting-graph %))))]
    (if (directed? g)
      (fly-graph :nodes nodes-of-resulting-graph
                 :successors fsuccessors
                 :predecessors (fn [n] (->> (predecessors g n)
                                            (filter #(nodes-of-resulting-graph %)))))
      (fly-graph :nodes nodes-of-resulting-graph
                 :successors fsuccessors))))
