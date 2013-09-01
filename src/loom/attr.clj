(ns ^{:doc "Graph attribute protocol and implementations for records from
loom.graph. Common uses for attributes include labels and styling (color,
thickness, etc)."
      :author "Justin Kramer"}
  loom.attr
  (use [loom.graph :only [directed? nodes edges]])
  (:import [loom.graph BasicEditableGraph BasicEditableDigraph
            BasicEditableWeightedGraph BasicEditableWeightedDigraph
            FlyGraph FlyDigraph WeightedFlyGraph WeightedFlyDigraph]))

(defprotocol AttrGraph
  (add-attr [g node k v] [g n1 n2 k v] "Add an attribute to node or edge")
  (remove-attr [g node k] [g n1 n2 k] "Remove an attribute from a node or edge")
  (attr [g node k] [g n1 n2 k] "Return the attribute on a node or edge")
  (attrs [g node] [g n1 n2] "Return all attributes on a node or edge"))

(def default-attr-graph-impl
  {:add-attr (fn
               ([g node k v]
                  (assoc-in g [:attrs node k] v))
               ([g n1 n2 k v]
                  (let [g (assoc-in g [:attrs n1 ::edge-attrs n2 k] v)
                        g (if (directed? g) g
                              (assoc-in g [:attrs n2 ::edge-attrs n1 k] v))]
                    g)))
   :remove-attr (fn
                  ([g node k]
                     (update-in g [:attrs node] dissoc k))
                  ([g n1 n2 k]
                     (update-in g [:attrs n1 ::edge-attrs n2] dissoc k)))
   :attr (fn
           ([g node k]
              (get-in g [:attrs node k]))
           ([g n1 n2 k]
              (get-in g [:attrs n1 ::edge-attrs n2 k])))
   :attrs (fn
            ([g node]
               (dissoc (get-in g [:attrs node]) ::edge-attrs))
            ([g n1 n2]
               (get-in g [:attrs n1 ::edge-attrs n2])))})

(extend BasicEditableGraph
  AttrGraph
  default-attr-graph-impl)

(extend BasicEditableDigraph
  AttrGraph
  default-attr-graph-impl)

(extend BasicEditableWeightedGraph
  AttrGraph
  default-attr-graph-impl)

(extend BasicEditableWeightedDigraph
  AttrGraph
  default-attr-graph-impl)

(extend FlyGraph
  AttrGraph
  default-attr-graph-impl)

(extend FlyDigraph
  AttrGraph
  default-attr-graph-impl)

(extend WeightedFlyGraph
  AttrGraph
  default-attr-graph-impl)

(extend WeightedFlyDigraph
  AttrGraph
  default-attr-graph-impl)

(defn attr?
  "Return true if g satisfies AttrGraph"
  [g]
  (satisfies? AttrGraph g))

(defn add-attr-to-nodes
  "Adds an attribute to the given nodes"
  [g k v nodes]
  (reduce
   (fn [g n]
     (add-attr g n k v))
   g nodes))

(defn add-attr-to-edges
  "Adds an attribute to the given nodes"
  [g k v edges]
  (reduce
   (fn [g [n1 n2]]
     (add-attr g n1 n2 k v))
   g edges))

(defn add-attr-to-all
  "Adds an attribute to all nodes and edges"
  [g k v]
  (-> g
      (add-attr-to-nodes k v (nodes g))
      (add-attr-to-edges k v (edges g))))

(defn add-attrs-to-all
  "Adds attributes to all nodes and edges"
  [g & kvs]
  (reduce
   (fn [g [k v]]
     (-> g
         (add-attr-to-nodes k v (nodes g))
         (add-attr-to-edges k v (edges g))))
   g (partition 2 1 kvs)))


(defn hilite
  "Adds a red :color attribute to a node or edge"
  ([g node]
     (-> g
         (add-attr node :color :red)
         (add-attr node :fontcolor :red)
         (add-attr node :fillcolor "#ffeeee")
         (add-attr node :style "filled,bold")))
  ([g n1 n2]
     (-> g
         (add-attr n1 n2 :color :red)
         (add-attr n1 n2 :fontcolor :red)
         (add-attr n1 n2 :style :bold))))

(defn hilite-path
  "Hilites nodes and edges along a path"
  [g path]
  (reduce
   (fn [g [n1 n2]]
     (-> g
         (hilite n1)
         (hilite n2)
         (hilite n1 n2)))
   g (partition 2 1 path)))
