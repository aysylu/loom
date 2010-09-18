(ns ^{:doc "Graph algorithms. Any graph record/type that satisfies the
Graph, Digraph, or WeightedGraph protocols (as appropriate per algorithm)
can use these functions."
      :author "Justin Kramer"}
  loom.alg
  (:require [loom.alg-generic :as gen])
  (:use [loom.graph
         :only [add-edges nodes edges neighbors weight incoming degree
                in-degree weighted? directed? graph]
         :rename {neighbors nb weight wt}]
        [loom.alg-generic :only [trace-path]]))

;;;
;;; Convenience wrappers for loom.alg-generic functions
;;;

(defn- traverse-all
  [nodes traverse]
  (second
   (reduce
    (fn [[seen trav] n]
      (if (seen n)
        [seen trav]
        (let [ctrav (traverse n :seen (conj seen n))]
          [(into seen ctrav) (concat ctrav trav)])))
    [#{} []]
    nodes)))

;;TODO: options: :incoming, :when
(defn pre-traverse
  "Traverses graph g depth-first from start. Returns a lazy seq of nodes.
  When no starting node is provided, traverses the entire graph, connected
  or not."
  ([g]
     (traverse-all (nodes g) (partial gen/pre-traverse (nb g))))
  ([g start]
     (gen/pre-traverse (nb g) start)))

(defn pre-span
  "Return a depth-first spanning tree of the form {node [successors]}"
  ([g]
     (second
      (reduce
       (fn [[seen span] n]
         (if (seen n)
           [seen span]
           (let [[cspan cseen] (gen/pre-span (nb g) n :seen seen)]
             [(clojure.set/union seen cseen) (merge span {n []} cspan)])))
       [#{} {}]
       (nodes g))))
  ([g start]
     (gen/pre-span (nb g) start)))

(defn post-traverse
  "Traverses graph g depth-first, post-order from start. Returns a
  vector of the nodes."
  ([g]
     (traverse-all (nodes g) (partial gen/post-traverse (nb g))))
  ([g start]
     (gen/post-traverse (nb g) start)))

(defn topsort
  "Topological sort of a directed acyclic graph (DAG). Returns nil if
  g contains any cycles."
  ([g]
     (loop [seen #{}
            result ()
            [n & ns] (seq (nodes g))]
       (if-not n
         result
         (if (seen n)
           (recur seen result ns)
           (when-let [cresult (gen/topsort-component (nb g) n seen seen)]
             (recur (into seen cresult) (concat cresult result) ns))))))
  ([g start]
     (gen/topsort-component (nb g) start)))

(defn bf-traverse
  "Traverses graph g breadth-first from start. When f is provided, returns
  a lazy seq of (f node predecessor-map) for each node traversed. Otherwise,
  returns a lazy seq of the nodes."
  ([g]
     (traverse-all (nodes g) (partial gen/bf-traverse (nb g))))
  ([g start]
     (gen/bf-traverse (nb g) start))
  ([g start & {:as opts}]
     (apply gen/bf-traverse (nb g) start (apply concat opts))))

(defn bf-span
  "Return a breadth-first spanning tree of the form {node [successors]}"
  ([g]
     (second
      (reduce
       (fn [[seen span] n]
         (if (seen n)
           [seen span]
           (let [cspan (gen/bf-span (nb g) n :seen seen)]
             ;; FIXME: very inefficient
             [(into seen (concat (keys cspan) (apply concat (vals cspan))))
              (merge span {n []} cspan)])))
       [#{} {}]
       (nodes g))))
  ([g start]
     (gen/bf-span (nb g) start)))

(defn bf-path
  "Return a path from start to end with the fewest hops (i.e. irrespective
  of edge weights)"
  [g start end]
  (gen/bf-path (nb g) start end))

(defn bf-path-bi
  "Using a bidirectional breadth-first search, finds a path from start to
  end with the fewest hops (i.e. irrespective of edge weights). Can be much
  faster than a unidirectional search on certain types of graphs"
  [g start end]
  (gen/bf-path-bi (nb g) start end))

(defn dijkstra-traverse
  "Returns a lazy-seq of [current-node state] where state is a map in the
  format {node [distance predecessor]}. When f is provided, returns
  a lazy-seq of (f node state) for each node"
  ([g]
     (gen/dijkstra-traverse (nb g) (wt g) (first (nodes g))))
  ([g start]
     (gen/dijkstra-traverse (nb g) (wt g) start vector))
  ([g start f]
     (gen/dijkstra-traverse (nb g) (wt g) start f)))

(defn dijkstra-span
  "Finds all shortest distances from start. Returns a map in the format
  {node {successor distance}}"
  ([g]
     (gen/dijkstra-span (nb g) (wt g) (first (nodes g))))
  ([g start]
     (gen/dijkstra-span (nb g) (wt g) start)))

(defn dijkstra-path-dist
  "Finds the shortest path from start to end. Returns a vector:
  [path distance]"
  [g start end]
  (gen/dijkstra-path-dist (nb g) (wt g) start end))

(defn dijkstra-path
  "Finds the shortest path from start to end"
  [g start end]
  (first (dijkstra-path-dist g start end)))


;;;
;;; Graph algorithms
;;;

(defn shortest-path
  "Finds the shortest path from start to end in graph g, using Dijkstra's
  algorithm if the graph is weighted, breadth-first search otherwise."
  [g start end]
  (if (weighted? g)
    (dijkstra-path g start end)
    (bf-path g start end)))

(defn longest-shortest-path
  "Finds the longest shortest path beginning at start, using Dijkstra's
  algorithm if the graph is weighted, bread-first search otherwise."
  [g start]
  (reverse
   (if (weighted? g)
     (reduce
      (fn [path1 [n state]]
        (let [path2 (trace-path (comp second state) n)]
          (if (< (count path1) (count path2)) path2 path1)))
      [start]
      (dijkstra-traverse g start vector))
     (reduce
      (fn [path1 [n predmap]]
        (let [path2 (trace-path predmap n)]
          (if (< (count path1) (count path2)) path2 path1)))
      [start]
      (bf-traverse g start vector)))))

(defn connected-components
  "Return the connected components of undirected graph g as a vector of vectors"
  [g]
  (first
   (reduce
    (fn [[cc seen] n]
      (if (seen n)
        [cc seen]
        (let [c (vec (gen/bf-traverse (nb g) n :seen seen))]
          [(conj cc c) (into seen c)])))
    [[] #{}]
    (nodes g))))

;; TODO: weak & strong cc

(defn connect
  "Return graph g with all connected components connected to each other"
  [g]
  (reduce add-edges g (partition 2 1 (map first (connected-components g)))))

(defn density
  "Return the density of graph g"
  [g & {:keys [loops] :or {loops false}}]
  (let [order (count (nodes g))]
    (/ (count (edges g))
       (* order (if loops
                  order
                  (dec order))))))

(defn loners
  "Return nodes with no connections to other nodes (i.e., isolated nodes)"
  [g]
  (let [degree-total (if (directed? g)
                       #(+ (in-degree g %) (degree g %))
                       #(degree g %))]
    (filter (comp zero? degree-total) (nodes g))))

(defn distinct-edges
  "Distinct edges of g. Only useful for undirected graphs"
  [g]
  (if (directed? g)
    (edges g)
    (second
     (reduce
      (fn [[seen es] e]
        (let [eset (set (take 2 e))]
          (if (seen eset)
            [seen es]
            [(conj seen eset)
             (conj es e)])))
      [#{} []]
      (edges g)))))

;; TODO: MST, coloring, bipartite, matching, etc etc