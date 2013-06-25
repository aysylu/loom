(ns ^{:doc "Defines protocols for graphs, digraphs, and weighted graphs.

Also provides record implementations and constructors for simple graphs --
weighted, unweighted, directed, and undirected. The implementations are based
on adjacency lists."
      :author "Justin Kramer"}
  loom.graph
  (:require [clojure.set :as cljset]
            [loom.basic-implementation :as impl]
            [loom.protocols :as p])
  (:use [loom.alg-generic :only [bf-traverse]]))

(defn directed?
  "Returns true if the graph is directed and false if the
   graph is undirected. If it is undirected, all functions
   taking two nodes must be commutative with regard to
   these nodes."
  [g]
  (p/directed? g))

(defn graph?
  "Return true if g satisfies the Graph protocol"
  [g]
  (satisfies? p/PGraph g))

(defn weighted?
  "Return true if g satisfies the WeightedGraph protocol"
  [g]
  (satisfies? p/PWeightedGraph g))

(defn nodes
  "Returns a set or sequence of all nodes of the graph. May
   not contain duplicates."
  [g]
  (p/nodes g))

(defn has-node?
  "Returns true if the graph g contains the node n."
  [g n]
  (p/has-node? g n))

(defn has-edge?
  "Returns true if the graph g has an edge from node n1
   to node n2."
  [g n1 n2]
  (p/has-edge? g n1 n2))

(defn direct-successors
  "Returns a set or sequence of all nodes n2 for which
   (has-edge? g n n2) returns true. May not contain
  duplicates."
  ([g] (partial direct-successors g))
  ([g n]
   (p/direct-successors g n)))

(defn direct-predecessors
  "Returns a set or sequence of all nodes n2 for which
   (has-edge? g n2 n) returns true. May not contain
   duplicates."
  ([g] (partial direct-predecessors g))
  ([g n]
   (if (satisfies? p/PPredecessorGraph g)
     (p/direct-predecessors g n)
     (filter #(p/has-edge? g % n) (p/nodes g)))))

(defn mutable?
  "Returns true if the graph is mutated in place.
   If true is returned, the other functions change
   the graph passed as the first argument and return
   the same graph object. If false is returned, the
   functions return a new graph and the old graph is
   unchaged."
  [g]
  (p/mutable? g))

(defn add-node
  "Adds the node n to the graph g. If it already
   contained n, the graph will not be changed."
  [g n]
  (p/add-node g n))

(defn remove-node
  "Removes the node n from the graph g. If it did
   not contain n, the graph will not be changed."
  [g n]
  (p/remove-node g n))

(defn add-edge
  "Adds an edge from node n1 to node n2 to the graph g.
   If one or both of the nodes is not present it will
   be added to the graph. If the edge was already present,
   the graph will not be changed."
  [g n1 n2]
  (p/add-edge g n1 n2))

(defn remove-edge
  "Removes the edge from node n1 to the node n2 from
   the graph g. If it did not contain the edge, the graph
   will not be changed."
  [g n1 n2]
  (p/remove-edge g n1 n2))

(defn edge-weight
  "Returns the weight of the edge from node n1 to
   node n2."
  ([g] (partial edge-weight g))
  ([g n1 n2]
   (p/edge-weight g n1 n2)))

(defn update-edge-weight
  "Updates the weight of the edge from node n1 to node n2,
   where f is a function taking the old value and any
   additional args and returning the new one. If the graph
   did not contain the edge, it will be created."
  [g n1 n2 f & args]
  (p/update-edge-weight g n1 n2 #(apply f % args)))

(defn set-edge-weight
  "Sets the weight of the edge from node n1 to node n2 to w.
   If the graph did not contain the edge, it will be created."
  [g n1 n2 w]
  (p/update-edge-weight g n1 n2 (fn [_] w)))

(defn node-data
  "Returns the data of the node n."
  [g n]
  (p/node-data g n))

(defn update-node-data
  "Updates the data of the node n, where f is a function
   taking the old value and returning the new one. If the
   graph did not contain the node, it will be added."
  [g n f & args]
  (p/update-node-data g n #(apply f % args)))

(defn set-node-data
  "Sets the data of the node n to the given value. If the
   graph did not contain the node, it will be added."
  [g n data]
  (p/update-node-data g n (fn [_] data)))

(defn edge-data
  "Returns the data of the edge from node n1 to node n2."
  [g n1 n2]
  (p/edge-data g n1 n2))

(defn update-edge-data
  "Changes the data of the edge from node n1 to n2, where
   f is a function taking the old value and returning the
   new one. If the graph did not contain the edge, it will
   be added."
  [g n1 n2 f & args]
  (p/update-edge-data g n1 n2 #(apply f % args)))

(defn set-edge-data
  "Sets the data of the edge from node n1 to node n2 to the
   given value. If the graph did not contain the edge, it
   will be added."
  [g n1 n2 data]
  (p/update-edge-data g n1 n2 (fn [_] data)))

(defn remove-all
  "Removes all nodes and edges from graph g."
  [g]
  (reduce p/remove-node g (p/nodes g)))

(defn add-nodes
  "Add nodes to graph g."
  [g & nodes]
  (reduce p/add-node g nodes))

(defn add-nodes*
  "Add the nodes in 'nodes' to the graph."
  [g nodes]
  (reduce p/add-node g nodes))

(defn add-edges
  "Takes edges in the form [n1 n2] or [n1 n2 w] where w
   is the edge weight of the new edge and adds them to the
   graph g."
  [g & edges]
  (reduce #(if (next (next %2))
             (apply set-edge-weight %1 %2)
             (apply p/add-edge %1 %2))
          g
          edges))

(defn add-edges*
  "Takes a sequence of edges in the form [n1 n2] or
   [n1 n2 w] where w is the edge weight of the new edge
   and adds them to the graph g."
  [g edges]
  (reduce #(if (next (next %2))
             (apply set-edge-weight %1 %2)
             (apply p/add-edge %1 %2))
          g
          edges))

(defn remove-edges
  "Takes edges in the form [n1 n2] and removes them
   from the graph g."
  [g & edges]
  (reduce #(apply p/remove-edge %1 %2) g edges))

(defn remove-nodes
  "Remove nodes from graph g."
  [g & nodes]
  (reduce p/remove-node g nodes))

(defn neighbors
  "Returns a set of every node n2 for which (has-edge? g n n2)
   or (has-edge? g n2 n) returns true."
  ([g] (partial neighbors g))
  ([g n]
   (if (directed? g)
     (cljset/union (set (p/direct-successors g n))
                   (set (direct-predecessors g n))))
   (p/direct-successors g n)))

(defn directed-edges
  "Returns a sequence of all edges represented as a vector
   containing the source and target node. For undirected
   graphs, every edge between distinct nodes is included
   twice, once in every direction."
  [g]
  (for [n1 (p/nodes g)
        n2 (p/direct-successors g n1)]
    [n1 n2]))

(defn transpose
  "Reverses all edges of the graph g."
  [g]
  (if (p/directed? g)
    (if (p/mutable? g)
      (throw "Not yet implemented.")
      (if (weighted? g)
        (let [empty-graph (remove-all g)]
          (-> empty-graph
              (add-nodes* (p/nodes g))
              (add-edges* (map (fn [[n1 n2]] [n2 n1 (p/edge-weight g n1 n2)])
                               (directed-edges g)))))
        (let [empty-graph (remove-all g)]
          (-> empty-graph
              (add-nodes* (p/nodes g))
              (add-edges* (map (fn [[n1 n2]] [n2 n1])
                               (directed-edges g)))))))
    g))

(defn degree
  "The degree of the node n in graph g."
  [g n]
  (if (p/directed? g)
    (throw "Not yet implemented.")
    (count (direct-successors g n))))

(defn in-degree
  "The number of nodes n2 for which (has-edge? g n2 n)
   is true."
  [g n]
  (count (direct-predecessors g n)))

(defn out-degree
  "The number of nodes n2 for which (has-edge? g n n2)
   is true."
  [g n]
  (count (p/direct-successors g n)))

(defn weight
  "The weight of the edge from n1 to n2."
  [g n1 n2]
  (p/edge-weight g n1 n2))

(defn incoming
  "The direct predecessors of the node n."
  [g n]
  (direct-predecessors g n))

;;;
;;; Records for simple graphs -- one edge per vertex pair/direction,
;;; loops allowed
;;;
;; TODO: allow custom weight fn?
;; TODO: preserve metadata?
;; TODO: leverage zippers for faster record updates?

(comment

(defrecord SimpleGraph [nodeset adj])
(defrecord SimpleDigraph [nodeset adj in])
(defrecord SimpleWeightedGraph [nodeset adj])
(defrecord SimpleWeightedDigraph [nodeset adj in])

(def ^{:dynamic true
       :doc "Weight used when none is given for edges in weighted graphs"}
  *default-weight* 1)

(def default-graph-impls
  {:all
   {:nodes (fn [g]
             (:nodeset g))
    :edges (fn [g]
             (for [n1 (nodes g)
                   n2 (neighbors g n1)]
               [n1 n2]))
    :has-node? (fn [g node]
                 (contains? (:nodeset g) node))
    :has-edge? (fn [g n1 n2]
                 (contains? (get-in g [:adj n1]) n2))
    :degree (fn [g node]
              (count (get-in g [:adj node])))}

   ;; Unweighted graphs store adjacencies as {node #{neighbor}}
   :unweighted
   {:add-nodes* (fn [g nodes]
                  (reduce
                   (fn [g n]
                     (-> g
                         (update-in [:nodeset] conj n)
                         (assoc-in [:adj n] (or ((:adj g) n) #{}))))
                   g nodes))
    :neighbors (fn
                 ([g] (partial neighbors g))
                 ([g node] (get-in g [:adj node])))}

   ;; Weighted graphs store adjacencies as {node {neighbor weight}}
   :weighted
   {:add-nodes* (fn [g nodes]
                  (reduce
                   (fn [g n]
                     (-> g
                         (update-in [:nodeset] conj n)
                         (assoc-in [:adj n] (or ((:adj g) n) {}))))
                   g nodes))
    :neighbors (fn
                 ([g] (partial neighbors g))
                 ([g node] (keys (get-in g [:adj node]))))}})

(def default-digraph-impl
  {:incoming (fn
               ([g] (partial incoming g))
               ([g node] (get-in g [:in node])))
   :in-degree (fn [g node]
                (count (get-in g [:in node])))})

(def default-weighted-graph-impl
  {:weight (fn
             ([g] (partial weight g))
             ([g n1 n2] (get-in g [:adj n1 n2])))})

(defn- remove-adj-nodes [m nodes adjacents remove-fn]
  (reduce
   (fn [m n]
     (if (m n)
       (update-in m [n] #(apply remove-fn % nodes))
       m))
   (apply dissoc m nodes)
   adjacents))

(extend SimpleGraph
  Graph
  (assoc (apply merge (map default-graph-impls [:all :unweighted]))

    :add-edges*
    (fn [g edges]
      (reduce
       (fn [g [n1 n2]]
         (-> g
             (update-in [:nodeset] conj n1 n2)
             (update-in [:adj n1] (fnil conj #{}) n2)
             (update-in [:adj n2] (fnil conj #{}) n1)))
       g edges))

    :remove-nodes*
    (fn [g nodes]
      (let [nbrs (mapcat #(neighbors g %) nodes)]
        (-> g
            (update-in [:nodeset] #(apply disj % nodes))
            (assoc :adj (remove-adj-nodes (:adj g) nodes nbrs disj)))))

    :remove-edges*
    (fn [g edges]
      (reduce
       (fn [g [n1 n2]]
         (-> g
             (update-in [:adj n1] disj n2)
             (update-in [:adj n2] disj n1)))
       g edges))

    :remove-all
    (fn [g]
      (assoc g :nodeset #{} :adj {}))))

(extend SimpleDigraph
  Graph
  (assoc (apply merge (map default-graph-impls [:all :unweighted]))

    :add-edges*
    (fn [g edges]
      (reduce
       (fn [g [n1 n2]]
         (-> g
             (update-in [:nodeset] conj n1 n2)
             (update-in [:adj n1] (fnil conj #{}) n2)
             (update-in [:in n2] (fnil conj #{}) n1)))
       g edges))

    :remove-nodes*
    (fn [g nodes]
      (let [ins (mapcat #(incoming g %) nodes)
            outs (mapcat #(neighbors g %) nodes)]
        (-> g
            (update-in [:nodeset] #(apply disj % nodes))
            (assoc :adj (remove-adj-nodes (:adj g) nodes ins disj))
            (assoc :in (remove-adj-nodes (:in g) nodes outs disj)))))

    :remove-edges*
    (fn [g edges]
      (reduce
       (fn [g [n1 n2]]
         (-> g
             (update-in [:adj n1] disj n2)
             (update-in [:in n2] disj n1)))
       g edges))

    :remove-all
    (fn [g]
      (assoc g :nodeset #{} :adj {} :in {})))

  Digraph
  (assoc default-digraph-impl
    :transpose (fn [g]
                 (assoc g :adj (:in g) :in (:adj g)))))

(extend SimpleWeightedGraph
  Graph
  (assoc (apply merge (map default-graph-impls [:all :weighted]))

    :add-edges*
    (fn [g edges]
      (reduce
       (fn [g [n1 n2 & [w]]]
         (-> g
             (update-in [:nodeset] conj n1 n2)
             (assoc-in [:adj n1 n2] (or w *default-weight*))
             (assoc-in [:adj n2 n1] (or w *default-weight*))))
       g edges))

    :remove-nodes*
    (fn [g nodes]
      (let [nbrs (mapcat #(neighbors g %) nodes)]
        (-> g
            (update-in [:nodeset] #(apply disj % nodes))
            (assoc :adj (remove-adj-nodes (:adj g) nodes nbrs dissoc)))))

    :remove-edges*
    (fn [g edges]
      (reduce
       (fn [g [n1 n2]]
         (-> g
             (update-in [:adj n1] dissoc n2)
             (update-in [:adj n2] dissoc n1)))
       g edges))

    :remove-all
    (fn [g]
      (assoc g :nodeset #{} :adj {})))

  WeightedGraph
  default-weighted-graph-impl)

(extend SimpleWeightedDigraph
  Graph
  (assoc (apply merge (map default-graph-impls [:all :weighted]))

    :add-edges*
    (fn [g edges]
      (reduce
       (fn [g [n1 n2 & [w]]]
         (-> g
             (update-in [:nodeset] conj n1 n2)
             (assoc-in [:adj n1 n2] (or w *default-weight*))
             (update-in [:in n2] (fnil conj #{}) n1)))
       g edges))

    :remove-nodes*
    (fn [g nodes]
      (let [ins (mapcat #(incoming g %) nodes)
            outs (mapcat #(neighbors g %) nodes)]
        (-> g
            (update-in [:nodeset] #(apply disj % nodes))
            (assoc :adj (remove-adj-nodes (:adj g) nodes ins dissoc))
            (assoc :in (remove-adj-nodes (:in g) nodes outs disj)))))

    :remove-edges*
    (fn [g edges]
      (reduce
       (fn [g [n1 n2]]
         (-> g
             (update-in [:adj n1] dissoc n2)
             (update-in [:in n2] disj n1)))
       g edges))

    :remove-all
    (fn [g]
      (assoc g :nodeset #{} :adj {} :in {})))

  Digraph
  (assoc default-digraph-impl
    :transpose (fn [g]
                 (reduce (fn [tg [n1 n2]]
                             (add-edges* tg [[n2 n1 (weight g n1 n2)]]))
                         (assoc g :adj {} :in {})
                         (edges g))))

  WeightedGraph
  default-weighted-graph-impl)

)

;;;
;;; FlyGraph -- a read-only, ad-hoc graph which uses provided functions to
;;; return values for nodes, edges, etc. Members which are not functions get
;;; returned as-is. Edges can be inferred if nodes and neighbors are provided.
;;; Nodes and edges can be inferred if neighbors and start are provided.
;;;

(defn- call-or-return [f & args]
  (if (or (fn? f)
          (and (instance? clojure.lang.IFn f) (seq args)))
    (apply f args)
    f))

(def ^{:private true} default-flygraph-graph-impl
  {:nodes (fn [g]
            (if (or (:fnodes g) (not (:start g)))
              (call-or-return (:fnodes g))
              (bf-traverse (neighbors g) (:start g))))
   :edges (fn [g]
            (if (:fedges g)
              (call-or-return (:fedges g))
              (for [n (nodes g)
                    nbr (neighbors g n)]
                [n nbr])))
   :neighbors (fn
                ([g] (partial neighbors g))
                ([g node] (call-or-return (:fneighbors g) node)))
   :degree (fn [g node]
             (count (neighbors g node)))})

(def ^{:private true} default-flygraph-digraph-impl
  {:incoming (fn [g node] (call-or-return (:fincoming g) node))
   :in-degree (fn [g node] (count (direct-predecessors g node)))})

(def ^{:private true} default-flygraph-weighted-impl
  {:weight (fn [g n1 n2] (call-or-return (:fweight g) n1 n2))})

(defrecord FlyGraph [fnodes fedges fneighbors start])
(defrecord FlyDigraph [fnodes fedges fneighbors fincoming start])
(defrecord WeightedFlyGraph [fnodes fedges fneighbors fweight start])
(defrecord WeightedFlyDigraph [fnodes fedges fneighbors fincoming fweight start])

;; Deprecate the flygraphs?  Instead provide interfaces on algorithms to
;; run the algorithm on

#_(extend FlyGraph
  p/PGraph default-flygraph-graph-impl)

#_(extend FlyDigraph
  p/PGraph default-flygraph-graph-impl
  p/Digraph default-flygraph-digraph-impl)

#_(extend WeightedFlyGraph
  p/PGraph default-flygraph-graph-impl
  p/WeightedGraph default-flygraph-weighted-impl)

#_(extend WeightedFlyDigraph
  p/PGraph default-flygraph-graph-impl
  p/Digraph default-flygraph-digraph-impl
  p/WeightedGraph default-flygraph-weighted-impl)

;;;
;;; Utility functions and constructors
;;;

;; TODO: make this work with read-only graphs?
;; Could also gain speed being impl-specific
(defn subgraph
  "Return a graph without all but the given nodes"
  [g ns]
  (reduce remove-node g (filter (complement (set ns)) (nodes g))))

(defn add-path
  "Add a path of edges connecting the given nodes in order"
  [g & nodes]
  (reduce #(apply add-edge %1 %2) g (partition 2 1 nodes)))

(defn add-cycle
  "Add a cycle of edges connecting the given nodes in order"
  [g & nodes]
  (reduce #(apply add-edge %1 %2) g (partition 2 1 (concat nodes [(first nodes)]))))

(defn build-graph
  "Builds up a graph (i.e. adds edges and nodes) from any combination of
  other graphs, adjacency maps, edges, or nodes."
  [g & inits]
  (letfn [(build [g init]
            (cond
             ;; graph
             (graph? init)
             (if (and (weighted? g) (weighted? init))
               (reduce add-edges
                       (add-nodes* g (nodes init))
                       (for [n1 (nodes init)
                             n2 (direct-successors init n1)]
                         [n1 n2 (edge-weight init n1 n2)]))
               (-> g
                   (add-nodes* (nodes init))
                   (add-edges* (for [n1 (nodes init)
                                     n2 (direct-successors init n1)]
                                 [n1 n2]))))
             ;; adacency map
             (map? init)
             (let [es (if (map? (val (first init)))
                        (for [[n nbrs] init
                              [nbr wt] nbrs]
                          [n nbr wt])
                        (for [[n nbrs] init
                              nbr nbrs]
                          [n nbr]))]
               (-> g
                   (add-nodes* (keys init))
                   (add-edges* es)))
             ;; edge
             (sequential? init) (add-edges g init)
             ;; node
             :else (add-nodes g init)))]
    (reduce build g inits)))

(defn graph
  "Create an undirected graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (impl/new-undirected-graph) inits))

(defn directed-graph
  "Create a directed graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (impl/new-directed-graph) inits))

#_(defn weighted-graph
  [& inits]
  "Create an weighted, undirected graph. inits can be edges, adjacency maps,
  or graphs"
  (apply build-graph (SimpleWeightedGraph. #{} {}) inits))

#_(defn weighted-digraph
  "Create an weighted, directed graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (SimpleWeightedDigraph. #{} {} {}) inits))

#_(defn fly-graph
  "Create a read-only, ad-hoc graph which uses the provided functions
  to return values for nodes, edges, etc. If any members are not functions,
  they will be returned as-is. Edges can be inferred if nodes and
  neighbors are provided. Nodes and edges can be inferred if neighbors and
  start are provided."
  [& {:keys [nodes edges neighbors incoming weight start]}]
  (cond
   (and incoming weight)
   (WeightedFlyDigraph. nodes edges neighbors incoming weight start)
   incoming
   (FlyDigraph. nodes edges neighbors incoming start)
   weight
   (WeightedFlyGraph. nodes edges neighbors weight start)
   :else
   (FlyGraph. nodes edges neighbors start)))
