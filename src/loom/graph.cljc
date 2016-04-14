(ns
  ^{:doc "Defines protocols for graphs, digraphs, and weighted graphs.
Also provides record implementations and constructors for simple graphs --
weighted, unweighted, directed, and undirected. The implementations are based
on adjacency lists."
    :author "Justin Kramer"}
  loom.graph
  (:require [loom.alg-generic :refer [bf-traverse]]))

;;;
;;; Protocols
;;;

(defprotocol Graph
  (nodes [g] "Returns a collection of the nodes in graph g")
  (edges [g] "Edges in g. May return each edge twice in an undirected graph")
  (has-node? [g node] "Returns true when node is in g")
  (has-edge? [g n1 n2] "Returns true when edge [n1 n2] is in g")
  (successors [g] [g node]
    "Returns direct successors of node, or (partial successors g)")
  (out-degree [g node] "Returns the number of outgoing edges of node")
  (out-edges [g] [g node] "Returns all the outgoing edges of node"))

(defprotocol Digraph
  (predecessors [g] [g node]
    "Returns direct predecessors of node, or (partial predecessors g)")
  (in-degree [g node] "Returns the number of direct predecessors to node")
  (in-edges [g] [g node] "Returns all the incoming edges of node")
  (transpose [g] "Returns a graph with all edges reversed"))

(defprotocol WeightedGraph
  (weight [g] [g e] [g n1 n2] "Returns the weight of edge e or edge [n1 n2] or (partial weight g)"))

(defprotocol EditableGraph
  (add-nodes* [g nodes] "Add nodes to graph g. See add-nodes")
  (add-edges* [g edges] "Add edges to graph g. See add-edges")
  (remove-nodes* [g nodes] "Remove nodes from graph g. See remove-nodes")
  (remove-edges* [g edges] "Removes edges from graph g. See remove-edges")
  (remove-all [g] "Removes all nodes and edges from graph g"))

(defprotocol Edge
  (src [edge] "Returns the source node of the edge")
  (dest [edge] "Returns the dest node of the edge"))

; Default implementation for vectors
(extend-type #?(:cljs cljs.core.PersistentVector :clj clojure.lang.IPersistentVector)
  Edge
  (src [edge] (get edge 0))
  (dest [edge] (get edge 1)))

; Default implementation for maps
(extend-type #?(:cljs cljs.core.PersistentArrayMap :clj clojure.lang.IPersistentMap)
  Edge
  (src [edge] (:src edge))
  (dest [edge] (:dest edge)))

(extend-type #?(:cljs cljs.core.PersistentHashMap :clj clojure.lang.IPersistentMap)
  Edge
  (src [edge] (:src edge))
  (dest [edge] (:dest edge)))

;; Variadic wrappers

(defn add-nodes
  "Adds nodes to graph g. Nodes can be any type of object"
  [g & nodes]
  (add-nodes* g nodes))

(defn add-edges
  "Adds edges to graph g. For unweighted graphs, edges take the form [n1 n2].
  For weighted graphs, edges take the form [n1 n2 weight] or [n1 n2], the
  latter defaulting to a weight of 1"
  [g & edges]
  (add-edges* g edges))

(defn remove-nodes
  "Removes nodes from graph g"
  [g & nodes]
  (remove-nodes* g nodes))

(defn remove-edges
  "Removes edges from graph g. Do not include weights"
  [g & edges]
  (remove-edges* g edges))

(def ^{:dynamic true
       :doc "Weight used when none is given for edges in weighted graphs"}
  *default-weight* 1)

(def default-graph-impls
  {:all
   {:nodes '(fn [g]
              (:nodeset g))
    :edges '(fn [g]
              (for [n1 (nodes g)
                    e (out-edges g n1)]
                e))
    :has-node? '(fn [g node]
                  (contains? (:nodeset g) node))
    :has-edge? '(fn [g n1 n2]
                  (contains? (get-in g [:adj n1]) n2))
    :out-degree '(fn [g node]
                   (count (get-in g [:adj node])))
    :out-edges '(fn
                  ([g] (partial out-edges g))
                  ([g node] (for [n2 (successors g node)] [node n2])))}

   ;; Unweighted graphs store adjacencies as {node #{neighbor}}
   :unweighted
   {:add-nodes* '(fn [g nodes]
                   (reduce
                     (fn [g n]
                       (-> g
                         (update-in [:nodeset] conj n)
                         (assoc-in [:adj n] (or ((:adj g) n) #{}))))
                     g nodes))
    :successors '(fn
                   ([g] (partial successors g))
                   ([g node] (get-in g [:adj node])))}

   ;; Weighted graphs store adjacencies as {node {neighbor weight}}
   :weighted
   {:add-nodes* '(fn [g nodes]
                   (reduce
                     (fn [g n]
                       (-> g
                         (update-in [:nodeset] conj n)
                         (assoc-in [:adj n] (or ((:adj g) n) {}))))
                     g nodes))
    :successors '(fn
                   ([g] (partial successors g))
                   ([g node] (keys (get-in g [:adj node]))))}})

(def default-digraph-impl
  {:predecessors '(fn
                    ([g] (partial predecessors g))
                    ([g node] (get-in g [:in node])))
   :in-degree '(fn [g node]
                 (count (get-in g [:in node])))
   :in-edges '(fn
                ([g] (partial in-edges g))
                ([g node] (for [n2 (predecessors g node)] [n2 node])))})

(def default-weighted-graph-impl
  {:weight '(fn
              ([g] (partial weight g))
              ([g e] (weight g (src e) (dest e)))
              ([g n1 n2] (get-in g [:adj n1 n2])))})

(defn- remove-adj-nodes [m nodes adjacents remove-fn]
  (reduce
    (fn [m n]
      (if (m n)
        (update-in m [n] #(apply remove-fn % nodes))
        m))
    (apply dissoc m nodes)
    adjacents))

;;;
;;; Records for basic graphs -- one edge per vertex pair/direction,
;;; loops allowed
;;;
;; TODO: allow custom weight fn?
;; TODO: preserve metadata?
;; TODO: leverage zippers for faster record updates?

(defrecord
 BasicEditableGraph
 [nodeset adj]
 Graph
 (nodes [g] (:nodeset g))
 (edges [g] (for [n1 (nodes g) e (out-edges g n1)] e))
 (has-node? [g node] (contains? (:nodeset g) node))
 (has-edge? [g n1 n2] (contains? (get-in g [:adj n1]) n2))
 (out-degree [g node] (count (get-in g [:adj node])))
 (out-edges [g] (partial out-edges g))
 (out-edges [g node] (for [n2 (successors g node)] [node n2]))
 (successors [g] (partial successors g))
 (successors [g node] (get-in g [:adj node]))
 EditableGraph
 (add-nodes*
  [g nodes]
  (reduce (fn [g node] (update-in g [:nodeset] conj node)) g nodes))
 (add-edges*
  [g edges]
  (reduce
   (fn
    [g [n1 n2]]
    (->
     g
     (update-in [:nodeset] conj n1 n2)
     (update-in [:adj n1] (fnil conj #{}) n2)
     (update-in [:adj n2] (fnil conj #{}) n1)))
   g
   edges))
 (remove-nodes*
  [g nodes]
  (let
   [nbrs (mapcat (fn* [p1__66951#] (successors g p1__66951#)) nodes)]
   (->
    g
    (update-in
     [:nodeset]
     (fn* [p1__66952#] (apply disj p1__66952# nodes)))
    (assoc :adj (remove-adj-nodes (:adj g) nodes nbrs disj)))))
 (remove-edges*
  [g edges]
  (reduce
   (fn
    [g [n1 n2]]
    (-> g (update-in [:adj n1] disj n2) (update-in [:adj n2] disj n1)))
   g
   edges))
 (remove-all [g] (assoc g :nodeset #{} :adj {})))

(defrecord
 BasicEditableDigraph
 [nodeset adj in]
 Graph
 (nodes [g] (:nodeset g))
 (edges [g] (for [n1 (nodes g) e (out-edges g n1)] e))
 (has-node? [g node] (contains? (:nodeset g) node))
 (has-edge? [g n1 n2] (contains? (get-in g [:adj n1]) n2))
 (out-degree [g node] (count (get-in g [:adj node])))
 (out-edges [g] (partial out-edges g))
 (out-edges [g node] (for [n2 (successors g node)] [node n2]))
 (successors [g] (partial successors g))
 (successors [g node] (get-in g [:adj node]))
 EditableGraph
 (add-nodes*
  [g nodes]
  (reduce (fn [g node] (update-in g [:nodeset] conj node)) g nodes))
 (add-edges*
  [g edges]
  (reduce
   (fn
    [g [n1 n2]]
    (->
     g
     (update-in [:nodeset] conj n1 n2)
     (update-in [:adj n1] (fnil conj #{}) n2)
     (update-in [:in n2] (fnil conj #{}) n1)))
   g
   edges))
 (remove-nodes*
  [g nodes]
  (let
   [ins
    (mapcat (fn* [p1__69718#] (predecessors g p1__69718#)) nodes)
    outs
    (mapcat (fn* [p1__69719#] (successors g p1__69719#)) nodes)]
   (->
    g
    (update-in
     [:nodeset]
     (fn* [p1__69720#] (apply disj p1__69720# nodes)))
    (assoc :adj (remove-adj-nodes (:adj g) nodes ins disj))
    (assoc :in (remove-adj-nodes (:in g) nodes outs disj)))))
 (remove-edges*
  [g edges]
  (reduce
   (fn
    [g [n1 n2]]
    (-> g (update-in [:adj n1] disj n2) (update-in [:in n2] disj n1)))
   g
   edges))
 (remove-all [g] (assoc g :nodeset #{} :adj {} :in {}))
 Digraph
 (predecessors [g] (partial predecessors g))
 (predecessors [g node] (get-in g [:in node]))
 (in-degree [g node] (count (get-in g [:in node])))
 (in-edges [g] (partial in-edges g))
 (in-edges [g node] (for [n2 (predecessors g node)] [n2 node]))
 (transpose [g] (assoc g :adj (:in g) :in (:adj g))))

(defrecord
 BasicEditableWeightedGraph
 [nodeset adj]
 Graph
 (nodes [g] (:nodeset g))
 (edges [g] (for [n1 (nodes g) e (out-edges g n1)] e))
 (has-node? [g node] (contains? (:nodeset g) node))
 (has-edge? [g n1 n2] (contains? (get-in g [:adj n1]) n2))
 (out-degree [g node] (count (get-in g [:adj node])))
 (out-edges [g] (partial out-edges g))
 (out-edges [g node] (for [n2 (successors g node)] [node n2]))
 (successors [g] (partial successors g))
 (successors [g node] (keys (get-in g [:adj node])))
 EditableGraph
 (add-nodes*
  [g nodes]
  (reduce (fn [g node] (update-in g [:nodeset] conj node)) g nodes))
 (add-edges*
  [g edges]
  (reduce
   (fn
    [g [n1 n2 & [w]]]
    (->
     g
     (update-in [:nodeset] conj n1 n2)
     (assoc-in [:adj n1 n2] (or w *default-weight*))
     (assoc-in [:adj n2 n1] (or w *default-weight*))))
   g
   edges))
 (remove-nodes*
  [g nodes]
  (let
   [nbrs (mapcat (fn* [p1__70923#] (successors g p1__70923#)) nodes)]
   (->
    g
    (update-in
     [:nodeset]
     (fn* [p1__70924#] (apply disj p1__70924# nodes)))
    (assoc :adj (remove-adj-nodes (:adj g) nodes nbrs dissoc)))))
 (remove-edges*
  [g edges]
  (reduce
   (fn
    [g [n1 n2]]
    (->
     g
     (update-in [:adj n1] dissoc n2)
     (update-in [:adj n2] dissoc n1)))
   g
   edges))
 (remove-all [g] (assoc g :nodeset #{} :adj {}))
 WeightedGraph
 (weight [g] (partial weight g))
 (weight [g e] (weight g (src e) (dest e)))
 (weight [g n1 n2] (get-in g [:adj n1 n2])))

(defrecord
 BasicEditableWeightedDigraph
 [nodeset adj in]
 Graph
 (nodes [g] (:nodeset g))
 (edges [g] (for [n1 (nodes g) e (out-edges g n1)] e))
 (has-node? [g node] (contains? (:nodeset g) node))
 (has-edge? [g n1 n2] (contains? (get-in g [:adj n1]) n2))
 (out-degree [g node] (count (get-in g [:adj node])))
 (out-edges [g] (partial out-edges g))
 (out-edges [g node] (for [n2 (successors g node)] [node n2]))
 (successors [g] (partial successors g))
 (successors [g node] (keys (get-in g [:adj node])))
 EditableGraph
 (add-nodes*
  [g nodes]
  (reduce (fn [g node] (update-in g [:nodeset] conj node)) g nodes))
 (add-edges*
  [g edges]
  (reduce
   (fn
    [g [n1 n2 & [w]]]
    (->
     g
     (update-in [:nodeset] conj n1 n2)
     (assoc-in [:adj n1 n2] (or w *default-weight*))
     (update-in [:in n2] (fnil conj #{}) n1)))
   g
   edges))
 (remove-nodes*
  [g nodes]
  (let
   [ins
    (mapcat (fn* [p1__72122#] (predecessors g p1__72122#)) nodes)
    outs
    (mapcat (fn* [p1__72123#] (successors g p1__72123#)) nodes)]
   (->
    g
    (update-in
     [:nodeset]
     (fn* [p1__72124#] (apply disj p1__72124# nodes)))
    (assoc :adj (remove-adj-nodes (:adj g) nodes ins dissoc))
    (assoc :in (remove-adj-nodes (:in g) nodes outs disj)))))
 (remove-edges*
  [g edges]
  (reduce
   (fn
    [g [n1 n2]]
    (->
     g
     (update-in [:adj n1] dissoc n2)
     (update-in [:in n2] disj n1)))
   g
   edges))
 (remove-all [g] (assoc g :nodeset #{} :adj {} :in {}))
 Digraph
 (predecessors [g] (partial predecessors g))
 (predecessors [g node] (get-in g [:in node]))
 (in-degree [g node] (count (get-in g [:in node])))
 (in-edges [g] (partial in-edges g))
 (in-edges [g node] (for [n2 (predecessors g node)] [n2 node]))
 (transpose
  [g]
  (reduce
   (fn [tg [n1 n2]] (add-edges* tg [[n2 n1 (weight g n1 n2)]]))
   (assoc g :adj {} :in {})
   (edges g)))
 WeightedGraph
 (weight [g] (partial weight g))
 (weight [g e] (weight g (src e) (dest e)))
 (weight [g n1 n2] (get-in g [:adj n1 n2])))

;;;
;;; FlyGraph -- a read-only, ad-hoc graph which uses provided functions to
;;; return values for nodes, edges, etc. Members which are not functions get
;;; returned as-is. Edges can be inferred if nodes and successors are provided.
;;; Nodes and edges can be inferred if successors and start are provided.
;;;

(defn- call-or-return [f & args]
  (if (or (fn? f)
        (and (instance? clojure.lang.IFn f) (seq args)))
    (apply f args)
    f))

(def ^{:private true} default-flygraph-graph-impl
  {:nodes '(fn [g]
             (if (or (:fnodes g) (not (:start g)))
               (call-or-return (:fnodes g))
               (bf-traverse (successors g) (:start g))))
   :edges '(fn [g]
             (if (:fedges g)
               (call-or-return (:fedges g))
               (for [n (nodes g)
                     nbr (successors g n)]
                 [n nbr])))
   :successors '(fn
                  ([g] (partial successors g))
                  ([g node] (call-or-return (:fsuccessors g) node)))
   :out-degree '(fn [g node]
                  (count (successors g node)))
   :out-edges '(get-in default-graph-impls [:all :out-edges])
   :has-node? '(fn [g node]
                ;; cannot use contains? here because (nodes g) need not be a set.
                (some #{node} (nodes g)))
   :has-edge? '(fn [g n1 n2]
                (some #{[n1 n2]} (edges g)))})

(def ^{:private true} default-flygraph-digraph-impl
  {:predecessors '(fn [g node] (call-or-return (:fpredecessors g) node))
   :in-degree '(fn [g node] (count (predecessors g node)))
   :in-edges (get-in default-digraph-impl [:all :in-edges])})

(def ^{:private true} default-flygraph-weighted-impl
  {:weight '(fn
              ([g] (partial weight g))
              ([g e] (weight g (src e) (dest e)))
              ([g n1 n2] (call-or-return (:fweight g) n1 n2)))})

;; Deprecate the flygraphs?  Instead provide interfaces on algorithms to
;; run the algorithm on

(defrecord
 FlyGraph
 [fnodes fedges fsuccessors start]
 Graph
 (nodes
  [g]
  (if
   (or (:fnodes g) (not (:start g)))
   (call-or-return (:fnodes g))
   (bf-traverse (successors g) (:start g))))
 (edges
  [g]
  (if
   (:fedges g)
   (call-or-return (:fedges g))
   (for [n (nodes g) nbr (successors g n)] [n nbr])))
 (successors [g] (partial successors g))
 (successors [g node] (call-or-return (:fsuccessors g) node))
 (out-degree [g node] (count (successors g node)))
 (out-edges [g] (partial out-edges g))
 (out-edges [g node] (for [n2 (successors g node)] [node n2]))
 (has-node? [g node] (some #{node} (nodes g)))
 (has-edge? [g n1 n2] (some #{[n1 n2]} (edges g))))

(defrecord
 FlyDigraph
 [fnodes fedges fsuccessors fpredecessors start]
 Graph
 (nodes
  [g]
  (if
   (or (:fnodes g) (not (:start g)))
   (call-or-return (:fnodes g))
   (bf-traverse (successors g) (:start g))))
 (edges
  [g]
  (if
   (:fedges g)
   (call-or-return (:fedges g))
   (for [n (nodes g) nbr (successors g n)] [n nbr])))
 (successors [g] (partial successors g))
 (successors [g node] (call-or-return (:fsuccessors g) node))
 (out-degree [g node] (count (successors g node)))
 (out-edges [g] (partial out-edges g))
 (out-edges [g node] (for [n2 (successors g node)] [node n2]))
 (has-node? [g node] (some #{node} (nodes g)))
 (has-edge? [g n1 n2] (some #{[n1 n2]} (edges g)))

 Digraph
 (predecessors [g node] (call-or-return (:fpredecessors g) node))
 (in-degree [g node] (count (predecessors g node))))

(defrecord
 WeightedFlyGraph
 [fnodes fedges fsuccessors fweight start]
 Graph
 (nodes
  [g]
  (if
   (or (:fnodes g) (not (:start g)))
   (call-or-return (:fnodes g))
   (bf-traverse (successors g) (:start g))))
 (edges
  [g]
  (if
   (:fedges g)
   (call-or-return (:fedges g))
   (for [n (nodes g) nbr (successors g n)] [n nbr])))
 (successors [g] (partial successors g))
 (successors [g node] (call-or-return (:fsuccessors g) node))
 (out-degree [g node] (count (successors g node)))
 (out-edges [g] (partial out-edges g))
 (out-edges [g node] (for [n2 (successors g node)] [node n2]))
 (has-node? [g node] (some #{node} (nodes g)))
 (has-edge? [g n1 n2] (some #{[n1 n2]} (edges g)))
 WeightedGraph
 (weight [g] (partial weight g))
 (weight [g e] (weight g (src e) (dest e)))
 (weight [g n1 n2] (call-or-return (:fweight g) n1 n2)))

(defrecord
 WeightedFlyDigraph
 [fnodes fedges fsuccessors fpredecessors fweight start]
 Graph
 (nodes
  [g]
  (if
   (or (:fnodes g) (not (:start g)))
   (call-or-return (:fnodes g))
   (bf-traverse (successors g) (:start g))))
 (edges
  [g]
  (if
   (:fedges g)
   (call-or-return (:fedges g))
   (for [n (nodes g) nbr (successors g n)] [n nbr])))
 (successors [g] (partial successors g))
 (successors [g node] (call-or-return (:fsuccessors g) node))
 (out-degree [g node] (count (successors g node)))
 (out-edges [g] (partial out-edges g))
 (out-edges [g node] (for [n2 (successors g node)] [node n2]))
 (has-node? [g node] (some #{node} (nodes g)))
 (has-edge? [g n1 n2] (some #{[n1 n2]} (edges g)))
 Digraph
 (predecessors [g node] (call-or-return (:fpredecessors g) node))
 (in-degree [g node] (count (predecessors g node)))
 WeightedGraph
 (weight [g] (partial weight g))
 (weight [g e] (weight g (src e) (dest e)))
 (weight [g n1 n2] (call-or-return (:fweight g) n1 n2)))

;;;
;;; Utility functions and constructors
;;;

;; TODO: make this work with read-only graphs?
;; Could also gain speed being impl-specific
(defn subgraph
  "Returns a graph with only the given nodes"
  [g ns]
  (remove-nodes* g (remove (set ns) (nodes g))))

(defn add-path
  "Adds a path of edges connecting the given nodes in order"
  [g & nodes]
  (add-edges* g (partition 2 1 nodes)))

(defn add-cycle
  "Adds a cycle of edges connecting the given nodes in order"
  [g & nodes]
  (add-edges* g (partition 2 1 (concat nodes [(first nodes)]))))

(defn graph?
  "Returns true if g satisfies the Graph protocol"
  [g]
  (satisfies? Graph g))

(defn directed?
  "Returns true if g satisfies the Digraph protocol"
  [g]
  (satisfies? Digraph g))

(defn weighted?
  "Returns true if g satisfies the WeightedGraph protocol"
  [g]
  (satisfies? WeightedGraph g))

(defn editable?
  "Returns true if g satisfies the EditableGraph protocol"
  [g]
  (satisfies? EditableGraph g))

(defn build-graph
  "Builds up a graph (i.e. adds edges and nodes) from any combination of
  other graphs, adjacency maps, edges, or nodes."
  [g & inits]
  (letfn [(build [g init]
            (cond
              ;; graph
              (graph? init)
              (if (and (weighted? g) (weighted? init))
                (assoc
                  (reduce add-edges
                    (add-nodes* g (nodes init))
                    (for [[n1 n2] (edges init)]
                      [n1 n2 (weight init n1 n2)]))
                  :attrs (merge (:attrs g) (:attrs init)))
                (-> g
                  (add-nodes* (nodes init))
                  (add-edges* (edges init))
                  (assoc :attrs (merge (:attrs g) (:attrs init)))))
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
  "Creates an unweighted, undirected graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (BasicEditableGraph. #{} {}) inits))

(defn digraph
  "Creates an unweighted, directed graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (BasicEditableDigraph. #{} {} {}) inits))

(defn weighted-graph
  "Creates an weighted, undirected graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (BasicEditableWeightedGraph. #{} {}) inits))

(defn weighted-digraph
  "Creates an weighted, directed graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (BasicEditableWeightedDigraph. #{} {} {}) inits))

(defn fly-graph
  "Creates a read-only, ad-hoc graph which uses the provided functions
  to return values for nodes, edges, etc. If any members are not functions,
  they will be returned as-is. Edges can be inferred if nodes and
  successors are provided. Nodes and edges can be inferred if successors and
  start are provided."
  [& {:keys [nodes edges successors predecessors weight start]}]
  (cond
    (and predecessors weight)
    (WeightedFlyDigraph. nodes edges successors predecessors weight start)
    predecessors
    (FlyDigraph. nodes edges successors predecessors start)
    weight
    (WeightedFlyGraph. nodes edges successors weight start)
    :else
    (FlyGraph. nodes edges successors start)))