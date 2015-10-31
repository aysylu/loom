(ns ^{:doc "Graph-generating functions"
      :author "Justin Kramer"}
  loom.gen
  (:require [loom.graph :refer [weighted? directed? add-nodes* add-edges*]]))

(defn gen-rand
  "Adds num-nodes nodes and approximately num-edges edges to graph g. Nodes
  used for each edge are chosen at random and may be chosen more than once."
  [g num-nodes num-edges & {:keys [min-weight max-weight loops seed]
                            :or {min-weight 1
                                 max-weight 1
                                 loops false
                                 seed (System/nanoTime)}}]
  {:pre [(or (not (weighted? g)) (< min-weight max-weight))]}
  (let [rnd (java.util.Random. seed)
        rand-w #(+ (.nextInt rnd (- max-weight min-weight)) min-weight)
        rand-n #(.nextInt rnd num-nodes)
        weighted? (weighted? g)
        nodes (range num-nodes)
        edges (for [_ (range num-edges)
                    :let [n1 (rand-n) n2 (rand-n)]
                    :when (or loops (not= n1 n2))]
                (if weighted?
                  [n1 n2 (rand-w)]
                  [n1 n2]))]
    (-> g
        (add-nodes* nodes)
        (add-edges* edges))))

(defn gen-rand-p
  "Adds num-nodes nodes to graph g with the probably p of an edge between
  each node."
  [g num-nodes p & {:keys [min-weight max-weight loops seed]
                    :or {min-weight 1
                         max-weight 1
                         loops false
                         seed (System/nanoTime)}}]
  {:pre [(or (not (weighted? g)) (< min-weight max-weight))]}
  (let [rnd (java.util.Random. seed)
        rand-w #(+ (.nextInt rnd (- max-weight min-weight)) min-weight)
        directed? (directed? g)
        weighted? (weighted? g)
        nodes (range num-nodes)
        edges (for [n1 nodes n2 nodes
                    :when (and (if directed?
                                 (or loops (not= n1 n2))
                                 (or (> n1 n2)
                                     (and loops (= n1 n2))))
                               (> p (.nextDouble rnd)))]
                (if weighted?
                  [n1 n2 (rand-w)]
                  [n1 n2]))]
    (-> g
        (add-nodes* nodes)
        (add-edges* edges))))
