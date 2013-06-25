(ns loom.basic-implementation
  (:require [loom.protocols :as p]))

(defrecord BasicUndirectedGraph [nodes
                                 edges
                                 weights
                                 node-data
                                 edge-data]
  p/PGraph
  (directed? [g] false)
  (nodes [g] nodes)
  (has-node? [g n] (contains? nodes n))
  (has-edge? [g n1 n2] (contains? (get edges n1) n2))
  (direct-successors [g n] (or (get-in g [:edges n]) #{}))
  p/PPredecessorGraph
  (direct-predecessors [g n] (p/direct-successors g n))
  p/PEditableGraph
  (mutable? [g] false)
  (add-node [g n]
    (-> g
        (update-in [:nodes] conj n)
        (update-in [:edges n] #(or % #{}))))
  (remove-node [g n]
    (-> g
        (#(reduce (fn [g n2] (p/remove-edge g n n2))
                  %
                  (get-in g [:edges n])))
        (update-in [:nodes] disj n)
        (update-in [:edges] dissoc n)
        (update-in [:node-data] dissoc n)))
  (add-edge [g n1 n2]
    (-> g
        (p/add-node n1)
        (p/add-node n2)
        (update-in [:edges n1] conj n2)
        (update-in [:edges n2] conj n1)
        (update-in [:weights #{n1 n2}] #(or % 1))))
  (remove-edge [g n1 n2]
    (-> g
        (update-in [:edges n1] disj n2)
        (update-in [:edges n2] disj n1)
        (update-in [:weights] dissoc #{n1 n2})
        (update-in [:edge-data] dissoc #{n1 n2})))
  p/PWeightedGraph
  (edge-weight [g n1 n2] (get-in g [:weights #{n1 n2}]))
  p/PEditableWeightedGraph
  (update-edge-weight [g n1 n2 f]
    (-> g
        (p/add-edge n1 n2)
        (update-in [:weights #{n1 n2}] f)))
  p/PNodeDataGraph
  (node-data [g n] (get-in g [:node-data n]))
  p/PEditableNodeDataGraph
  (update-node-data [g n f]
    (update-in g [:node-data n] f))
  p/PEdgeDataGraph
  (edge-data [g n1 n2] (get-in g [:edge-data #{n1 n2}]))
  p/PEditableEdgeDataGraph
  (update-edge-data [g n1 n2 f]
    (update-in g [:edge-data #{n1 n2}] f)))

(defrecord BasicDirectedGraph [nodes
                               edges
                               reverse-edges
                               weights
                               node-data
                               edge-data]
  p/PGraph
  (directed? [g] true)
  (nodes [g] nodes)
  (has-node? [g n] (contains? nodes n))
  (has-edge? [g n1 n2] (contains? (get edges n1) n2))
  (direct-successors [g n] (or (get-in g [:edges n]) #{}))
  p/PPredecessorGraph
  (direct-predecessors [g n]
    (or (get-in g [:reverse-edges n]) #{}))
  p/PEditableGraph
  (mutable? [g] false)
  (add-node [g n]
    (-> g
        (update-in [:nodes] conj n)
        (update-in [:edges n] #(or % #{}))
        (update-in [:reverse-edges n] #(or % #{}))))
  (remove-node [g n]
    (-> g
        (#(reduce (fn [g n2] (p/remove-edge g n n2))
                  %
                  (get-in g [:edges n])))
        (#(reduce (fn [g n2] (p/remove-edge g n2 n))
                  %
                  (get-in g [:reverse-edges n])))
        (update-in [:nodes] disj n)
        (update-in [:edges] dissoc n)
        (update-in [:reverse-edges] dissoc n)
        (update-in [:node-data] dissoc n)))
  (add-edge [g n1 n2]
    (-> g
        (p/add-node n1)
        (p/add-node n2)
        (update-in [:edges n1] conj n2)
        (update-in [:reverse-edges n2] conj n1)
        (assoc-in [:weights [n1 n2]] 1)))
  (remove-edge [g n1 n2]
    (-> g
        (update-in [:edges n1] disj n2)
        (update-in [:reverse-edges n2] disj n1)
        (update-in [:weights] dissoc [n1 n2])
        (update-in [:edge-data] dissoc [n1 n2])))
  p/PWeightedGraph
  (edge-weight [g n1 n2] (get-in g [:weights [n1 n2]]))
  p/PEditableWeightedGraph
  (update-edge-weight [g n1 n2 f]
    (-> g
        (p/add-edge n1 n2)
        (update-in [:weights [n1 n2]] f)))
  p/PNodeDataGraph
  (node-data [g n] (get-in g [:node-data n]))
  p/PEditableNodeDataGraph
  (update-node-data [g n f]
    (-> g
        (p/add-node n)
        (update-in  [:node-data n] f)))
  p/PEdgeDataGraph
  (edge-data [g n1 n2] (get-in g [:edge-data [n1 n2]]))
  p/PEditableEdgeDataGraph
  (update-edge-data [g n1 n2 f]
    (-> g
        (p/add-edge n1 n2)
        (update-in  [:edge-data [n1 n2]] f))))

(defn new-undirected-graph []
  (map->BasicUndirectedGraph {:nodes #{}
                              :edges {}
                              :weights {}
                              :node-data {}
                              :edge-data {}}))

(defn new-directed-graph []
  (map->BasicDirectedGraph {:nodes #{}
                            :edges {}
                            :reverse-edges {}
                            :weights {}
                            :node-data {}
                            :edge-data {}}))