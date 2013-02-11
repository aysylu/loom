(ns ^{:doc "Algorithms for solving network flow"
      :author "Robert Lachlan"}
  loom.flow
  (:require [loom.alg-generic :as gen :only [bf-path]]))


(defn residual-capacity [capacity flow v1 v2]
  "Computes the residual capacity between nodes v1 and v2.  Capacity is a function
   that takes two nodes, and returns the capacity on the edge between them, if
   any.  Flow is the adjacency map which represents the current flow in the
   network."
  (+
   (or (get-in flow [v2 v1]) 0)
   (- (or (capacity v1 v2) 0)
      (or (get-in flow [v1 v2]) 0))))

(defn flow-balance [flow]
  "Given a flow, returns a map of {node (sum(in weight) - sum(out weight))}"
  (loop [out {}, in {}, adj-list (seq flow)]
    (if-let [[node neighbours] (first adj-list)]
      (recur (assoc out node (- (reduce + (vals neighbours))))
             (merge-with + in neighbours)
             (next adj-list))
      (merge-with + out in))))

(defn satisfies-mass-balance? [flow source sink]
  "Given a flow, verifies whether at each node the sum of in edge weights is equal
   to the sum of out edge weights, except at the source and sink. The source
   should have positive net outflow, the sink negative, and together they
   should balance."
  (let [balance (flow-balance flow)]
    (and (<= (or (get balance source) 0) 0)
         (zero? (+ (or (get balance source) 0)
                   (or (get balance sink) 0)))
         (every? zero? (vals (dissoc balance source sink))))))

(defn satisfies-capacity-constraints? [flow capacity]
  "Given a flow map, and a capacity function, verifies that the flow on each edge
   is <= capacity of that edge."
  (empty?
   (remove (fn [[node flow-to-neighbors]]
             (every?
              (fn [[neighbor flow-value]] (<= flow-value (capacity node neighbor)))
              (seq flow-to-neighbors)))
           (seq flow))))

(defn is-admissible-flow? [flow capacity source sink]
  "Verifies that a flow satisfies capacity and mass balance constraints.  Does
   verify that a flow is maximum."
  (and (satisfies-mass-balance? flow source sink)
       (satisfies-capacity-constraints? flow capacity)))

(defn min-weight-along-path [path weight-fn]
  "Given a path, represented by a sequence of nodes, and weight-function, computes
   the minimum of the edge weights along the path.  If an edge on the path is
   missing, returns 0."
  (reduce min (map #(or (apply weight-fn %) 0)  (partition 2 1 path))))

(defn bf-find-augmenting-path [neighbors incoming capacity flow s t]
  "Finds a shortest path in the flow network along which there remains residual
   capacity.  Neighbours is a function which, given a vertex, returns the
   vertices connected by outgoing edges.  Incoming, similarly is a function to get
   vertices connected by incoming edges.  Capacity is a function which takes two
   vertices and returns the capacity between them.  Flow is an adjacency map which
   contains the current value of network flow.  s is the source node, t the sink."
  (gen/bf-path
   (fn [vertex] (distinct (filter #(> (residual-capacity capacity flow vertex %) 0)
                                  (concat (neighbors vertex) (incoming vertex)))))
   s t))

(defn augment-along-path [flow capacity path increase]
  "Given a flow represented as an adjacency map, returns an updated flow.
   Capacity is a function of two vertices, path is a sequence of nodes, and
   increase is the amount by which the flow should be augmented on this path.  An
   exception is thrown if the augmentation is impossible given capacity
   constraints."
  (let [vn0 (first path)
        vn1 (second path)
        forward-flow (or (get-in flow [vn0 vn1]) 0)
        forward-capacity (- (or (capacity vn0 vn1) 0) forward-flow)
        reverse-flow (or (get-in flow [vn1 vn0]) 0)
        forward-increase (min forward-capacity increase)
        pushback (- increase forward-increase)
        flow_1 (if (pos? forward-increase)
                 (assoc-in flow [vn0 vn1] (+ forward-flow forward-increase)) 
                 flow)
        flow_2 (if (pos? pushback)
                 (assoc-in flow_1 [vn1 vn0] (- reverse-flow pushback))
                 flow_1)]
    (cond (> pushback reverse-flow) (throw (java.lang.RuntimeException.
                                            (str "Path augmentation failure: " vn0 " " vn1)))
          (> (count path) 2) (recur flow_2 capacity (next path) increase)
          :else flow_2)))
        
(defn edmonds-karp
  "Computes the maximum flow on a network, using the edmonds-karp algorithm.
   Neighbors is a function that returns the outgoing neighbor vertices of a
   vertex.  Incoming is a function that returns the incoming neighbor vertices
   for a vertex.  Capacity is a function of two vertices that returns the
   capacity on the edge between them.  Source and sink are the unique vertices
   which supply and consume flow respectively.

   Returns a vector [flow value], where flow is an adjacency map that represents
   flows between vertices, and value is the quantity of flow passing from source
   to sink."
  ([neighbors incoming capacity source sink]
     (edmonds-karp neighbors incoming capacity source sink {}))
  ([neighbors incoming capacity source sink flow]
     (if-let [path (bf-find-augmenting-path neighbors
                                            incoming capacity flow source sink)]
       (recur neighbors incoming capacity source sink
              (augment-along-path flow capacity path
                                  (min-weight-along-path
                                   path (partial residual-capacity capacity flow))))
       (let [value (reduce + (vals (get flow source)))]
       [flow value]))))