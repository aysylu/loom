(ns ^{:doc "Graph algorithms. Any graph record/type that satisfies the
Graph, Digraph, or WeightedGraph protocols (as appropriate per algorithm)
can use these functions."
      :author "Justin Kramer"}
  loom.alg
  (:require [loom.alg-generic :as gen]
            [loom.flow :as flow])
  (:require [loom.graph
             :refer [add-nodes add-edges nodes edges successors weight predecessors
                     out-degree in-degree weighted? directed? graph transpose]
             :as graph]
            [loom.alg-generic :refer [trace-path preds->span]])
  (:require [clojure.data.priority-map :as pm]))

;;;
;;; Convenience wrappers for loom.alg-generic functions
;;;
(defn- traverse-all
  [nodes traverse]
  (persistent! (second
   (reduce
    (fn [[seen trav] n]
      (if (seen n)
        [seen trav]
        (let [ctrav (traverse n :seen (conj seen n))]
          [(into seen ctrav) (reduce conj! trav ctrav)])))
    [#{} (transient [])]
    nodes))))

(defn pre-traverse
  "Traverses graph g depth-first from start. Returns a lazy seq of nodes.
  When no starting node is provided, traverses the entire graph, connected
  or not."
  ([g]
     (traverse-all (nodes g) (partial gen/pre-traverse (graph/successors g))))
  ([g start]
     (gen/pre-traverse (graph/successors g) start)))

(defn pre-span
  "Return a depth-first spanning tree of the form {node [successors]}"
  ([g]
     (second
      (reduce
       (fn [[seen span] n]
         (if (seen n)
           [seen span]
           (let [[cspan seen] (gen/pre-span
                               (graph/successors g)
                               n :seen seen :return-seen true)]
             [seen (merge span {n []} cspan)])))
       [#{} {}]
       (nodes g))))
  ([g start]
     (gen/pre-span (graph/successors g) start)))

(defn post-traverse
  "Traverses graph g depth-first, post-order from start. Returns a
  vector of the nodes."
  ([g]
     (traverse-all (nodes g) (partial gen/post-traverse (graph/successors g))))
  ([g start & opts]
     (apply gen/post-traverse (graph/successors g) start opts)))

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
           (when-let [cresult (gen/topsort-component
                               (graph/successors g) n seen seen)]
             (recur (into seen cresult) (concat cresult result) ns))))))
  ([g start]
     (gen/topsort-component (graph/successors g) start)))

(defn bf-traverse
  "Traverses graph g breadth-first from start. When option :f is provided,
  returns a lazy seq of (f node predecessor-map depth) for each node traversed.
  Otherwise, returns a lazy seq of the nodes. When option :when is provided,
  filters successors with (f neighbor predecessor depth)."
  ([g]
     (first
      (reduce
       (fn [[cc predmap] n]
         (if (contains? predmap n)
           [cc predmap]
           (reduce
            (fn [[cc _] [n pm _]]
              [(conj cc n) pm])
            [cc predmap]
            (gen/bf-traverse (graph/successors g) n :f vector :seen predmap))))
       [[] {}]
       (nodes g))))
  ([g start]
     (gen/bf-traverse (graph/successors g) start))
  ([g start & opts]
     (apply gen/bf-traverse (graph/successors g) start opts)))

(defn bf-span
  "Return a breadth-first spanning tree of the form {node [successors]}"
  ([g]
     (preds->span
      (reduce
       (fn [predmap n]
         (if (contains? predmap n)
           predmap
           (last (gen/bf-traverse (graph/successors g) n
                                  :f (fn [_ pm _] pm)
                                  :seen predmap))))
       {}
       (nodes g))))
  ([g start]
     (gen/bf-span (graph/successors g) start)))

(defn bf-path
  "Return a path from start to end with the fewest hops (i.e. irrespective
  of edge weights)"
  [g start end & opts]
  (apply gen/bf-path (graph/successors g) start end opts))

(defn bf-path-bi
  "Using a bidirectional breadth-first search, finds a path from start to
  end with the fewest hops (i.e. irrespective of edge weights). Can be much
  faster than a unidirectional search on certain types of graphs"
  [g start end]
  (if (directed? g)
    (gen/bf-path-bi (graph/successors g) (predecessors g) start end)
    (gen/bf-path-bi (graph/successors g) (graph/successors g) start end)))

(defn dijkstra-traverse
  "Returns a lazy-seq of [current-node state] where state is a map in
  the format {node [distance predecessor]}. When f is provided,
  returns a lazy-seq of (f node state) for each node"
  ([g]
     (gen/dijkstra-traverse
      (graph/successors g) (graph/weight g) (first (nodes g))))
  ([g start]
     (gen/dijkstra-traverse (graph/successors g) (graph/weight g) start vector))
  ([g start f]
     (gen/dijkstra-traverse (graph/successors g) (graph/weight g) start f)))

(defn dijkstra-span
  "Finds all shortest distances from start. Returns a map in the
  format {node {successor distance}}"
  ([g]
     (gen/dijkstra-span
      (graph/successors g) (graph/weight g) (first (nodes g))))
  ([g start]
     (gen/dijkstra-span (graph/successors g) (graph/weight g) start)))

(defn dijkstra-path-dist
  "Finds the shortest path from start to end. Returns a vector:
  [path distance]"
  [g start end]
  (gen/dijkstra-path-dist (graph/successors g) (graph/weight g) start end))

(defn dijkstra-path
  "Finds the shortest path from start to end"
  [g start end]
  (first (dijkstra-path-dist g start end)))

(defn- can-relax-edge?
  "Test for whether we can improve the shortest path to v found so far
   by going through u."
  [[u v :as edge] weight costs]
  (let [vd (get costs v)
        ud (get costs u)
        sum (+ ud weight)]
    (> vd sum)))

(defn- relax-edge
  "If there's a shorter path from s to v via u,
    update our map of estimated path costs and
   map of paths from source to vertex v"
  [[u v :as edge] weight [costs paths :as estimates]]
  (let [ud (get costs u)
        sum (+ ud weight)]
    (if (can-relax-edge? edge weight costs)
      [(assoc costs v sum) (assoc paths v u)]
      estimates)))

(defn- relax-edges
  "Performs edge relaxation on all edges in weighted directed graph"
  [g start estimates]
  (->> (edges g)
       (reduce (fn [estimates [u v :as edge]]
                 (relax-edge edge (graph/weight g u v) estimates))
               estimates)))

(defn- init-estimates
  "Initializes path cost estimates and paths from source to all vertices,
   for Bellman-Ford algorithm"
  [graph start]
  (let [nodes (disj (nodes graph) start)
        path-costs {start 0}
        paths {start nil}
        infinities (repeat Double/POSITIVE_INFINITY)
        nils (repeat nil)
        init-costs (interleave nodes infinities)
        init-paths (interleave nodes nils)]
    [(apply assoc path-costs init-costs)
     (apply assoc paths init-paths)]))


;;;
;;; Graph algorithms
;;;

(defn bellman-ford
  "Given a weighted, directed graph G = (V, E) with source start,
   the Bellman-Ford algorithm produces map of single source shortest
   paths and their costs if no negative-weight cycle that is reachable
   from the source exits, and false otherwise, indicating that no
   solution exists."
  [g start]
  (let [initial-estimates (init-estimates g start)
        ;;relax-edges is calculated for all edges V-1 times
        [costs paths] (reduce (fn [estimates _]
                                (relax-edges g start estimates))
                              initial-estimates
                              (-> g nodes count dec range))
        edges (edges g)]
    (if (some
         (fn [[u v :as edge]]
           (can-relax-edge? edge (graph/weight g u v) costs))
         edges)
      false
      [costs
       (->> (keys paths)
            ;;remove vertices that are unreachable from source
            (remove #(= Double/POSITIVE_INFINITY (get costs %)))
            (reduce
             (fn [final-paths v]
               (assoc final-paths v
                      ;; follows the parent pointers
                      ;; to construct path from source to node v
                      (loop [node v
                             path ()]
                        (if node
                          (recur (get paths node) (cons node path))
                          path))))
             {}))])))

(defn dag?
  "Return true if g is a directed acyclic graph"
  [g]
  (boolean (topsort g)))

(defn shortest-path
  "Finds the shortest path from start to end in graph g, using Dijkstra's
  algorithm if the graph is weighted, breadth-first search otherwise."
  [g start end]
  (if (weighted? g)
    (dijkstra-path g start end)
    (bf-path g start end)))

(defn longest-shortest-path
  "Finds the longest shortest path beginning at start, using Dijkstra's
  algorithm if the graph is weighted, breadth-first search otherwise."
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
      (fn [path1 [n predmap _]]
        (let [path2 (trace-path predmap n)]
          (if (< (count path1) (count path2)) path2 path1)))
      [start]
      (bf-traverse g start :f vector)))))

(defn connected-components
  "Return the connected components of graph g as a vector of vectors. If g
  is directed, returns the weakly-connected components."
  [g]
  (let [nb (if-not (directed? g) (graph/successors g)
                   #(concat (graph/successors g %) (predecessors g %)))]
    (first
     (reduce
      (fn [[cc predmap] n]
        (if (contains? predmap n)
          [cc predmap]
          (let [[c pm] (reduce
                        (fn [[c _] [n pm _]]
                          [(conj c n) pm])
                        [[] nil]
                        (gen/bf-traverse nb n :f vector :seen predmap))]
            [(conj cc c) pm])))
      [[] {}]
      (nodes g)))))

(defn connected?
  "Returns true if g is connected"
  [g]
  (== (count (first (connected-components g))) (count (nodes g))))

(defn scc
  "Return the strongly-connected components of directed graph g as a vector of
  vectors. Uses Kosaraju's algorithm."
  [g]
  (let [gt (transpose g)]
    (loop [stack (reverse (post-traverse g))
           seen #{}
           cc (transient [])]
      (if (empty? stack)
        (persistent! cc)
        (if (seen (first stack))
          (recur (rest stack) seen cc)
          (let [[c seen] (post-traverse gt (first stack)
                                      :seen seen :return-seen true)]
            (recur (rest stack)
                 seen
                 (conj! cc c))))
        ))))

(defn strongly-connected?
  [g]
  (== (count (first (scc g))) (count (nodes g))))

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
                       #(+ (in-degree g %) (out-degree g %))
                       #(out-degree g %))]
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

(defn bipartite-color
  "Attempt a two-coloring of graph g. When successful, returns a map of
  nodes to colors (1 or 0). Otherwise, returns nil."
  [g]
  (letfn [(color-component [coloring start]
            (loop [coloring (assoc coloring start 1)
                   queue (conj clojure.lang.PersistentQueue/EMPTY start)]
              (if (empty? queue)
                coloring
                (let [v (peek queue)
                      color (- 1 (coloring v))
                      nbrs (graph/successors g v)]
                  ;; TODO: could be better
                  (if (some #(and (coloring %) (= (coloring v) (coloring %)))
                            nbrs)
                    nil ;not bipartite
                    (let [nbrs (remove coloring nbrs)]
                      (recur (into coloring (for [nbr nbrs] [nbr color]))
                             (into (pop queue) nbrs))))))))]
    (loop [[node & nodes] (seq (nodes g))
           coloring {}]
      (when coloring
        (if (nil? node)
          coloring
          (if (coloring node)
            (recur nodes coloring)
            (recur nodes (color-component coloring node))))))))

(defn bipartite?
  "Return true if g is bipartite"
  [g]
  (boolean (bipartite-color g)))

(defn bipartite-sets
  "Return two sets of nodes, one for each color of the bipartite coloring,
  or nil if g is not bipartite"
  [g]
  (when-let [coloring (bipartite-color g)]
    (reduce
     (fn [[s1 s2] [node color]]
       (if (zero? color)
         [(conj s1 node) s2]
         [s1 (conj s2 node)]))
     [#{} #{}]
     coloring)))

(defn max-flow
  "Returns [flow-map flow-value], where flow-map is a weighted adjacency map
   representing the maximum flow.  The argument should be a weighted digraph,
   where the edge weights are flow capacities.  Source and sink are the vertices
   representing the flow source and sink vertices.  Optionally, pass in
     :method :algorithm to use.  Currently, the only option is :edmonds-karp ."
  [g source sink & {:keys [method] :or {method :edmonds-karp}}]
  (let [method-set #{:edmonds-karp}
        n (graph/successors g),
        i (predecessors g),
        c (graph/weight g),
        s source,
        t sink
        [flow-map flow-value] (case method
                                :edmonds-karp (flow/edmonds-karp n i c s t)
                                (throw
                                 (java.lang.RuntimeException.
                                  (str "Method not found.  Choose from: "
                                       method-set))))]
    [flow-map flow-value]))



;; mst algorithms
;; convenience functions for mst algo
(defn- edge-weights
  "Wrapper function to return edges along with weights for a given graph.
   For un-weighted graphs a default value of one is produced. The function
   returns values of the form [[[u v] 10] [[x y] 20] ...]"
  [wg v]
  (let [edge-weight (fn [u v]
                      (if (weighted? wg) (weight wg u v) 1))]
    (map #(vec [%1 [v (edge-weight v %1)] ])
         (successors wg v)))
  )

(defn prim-mst-edges
  "An edge-list of an minimum spanning tree along with weights that
  represents an MST of the given  graph. Returns the MST edge-list
  for un-weighted graphs."
  ([wg]
     (cond
      (directed? wg) (throw (Exception.
                             "Spanning tree only defined for undirected graphs"))
      :else (let [mst (prim-mst-edges wg (nodes wg) nil #{} [])]
              (if (weighted? wg)
                mst
                (map #(vec [(first %1) (second %1)]) mst))))
     )
  ([wg n h visited acc]
     (cond
      (empty? n) acc
      (empty? h) (let [v (first n)
                       h  (into (partial (pm/priority-map-keyfn second)) (edge-weights wg v))
                       ]
                   (recur wg (disj n v) h (conj visited v) acc))
      :else (let [next_edge (peek h)
                  u (first (second next_edge))
                  v (first next_edge)
                  update-dist (fn [h [v [u wt]]]
                                (cond
                                 (nil? (get h v)) (assoc h v [u wt])
                                 (> (second (get h v)) wt) (assoc h v [u wt])
                                 :else h)
                                )]
              (let [wt (second (second next_edge))
                    visited (conj visited v)
                    h (reduce update-dist (pop h)
                              (filter #((complement visited) (first %) )
                                      (edge-weights wg v)))]
                (recur wg (disj n v) h (conj visited v)(conj acc [u v wt])))))))

(defn prim-mst
  "Minimum spanning tree of given graph. If the graph contains more than one
   component then returns a spanning forest of minimum spanning trees."
  [wg]
  (let [mst (apply graph/weighted-graph (prim-mst-edges wg))
        ]
    (cond
     (= ((comp count nodes) wg) ((comp count nodes) mst)) mst
     :else (apply add-nodes mst (filter #(zero? (out-degree wg %)) (nodes wg)))
     )))

(defn astar-path
  "Return the shortest path using A* algorithm. Returns a map of predecessors."
  ([g src target heur]
     (let [heur (if (nil? heur) (fn [x y] 0) heur)
           ;; store in q => {u [heur+dist parent act est]}
           q (pm/priority-map-keyfn first src [0 nil 0 0])
           explored (hash-map)]
       (astar-path g src target heur q explored))
       )
  ([g src target heur q explored]
     (cond
      ;; queue empty, target not reachable
      (empty? q) (throw (Exception. "Target not reachable from source"))
      ;; target found, build path and return
      (= (first (peek q)) target) (let [u (first (peek q))
                                        parent ((second (peek q)) 1)
                                        explored(assoc explored target parent)
                                        path (loop [s target acc {}]
                                               (cond
                                                (nil? s) acc
                                                (= s src) (assoc acc s nil)
                                                :else (recur (explored s)
                                                             (assoc acc s (explored s)))))
                                        ]
                                    path
                                    )
      ;; continue searching
      :else (let
                [curr-node (first (peek q))
                 curr-dist ((second (peek q)) 2)
                 ;; update path
                 explored (assoc explored curr-node ((second (peek q)) 1))
                 nbrs (filter (complement explored) (successors g curr-node))
                 ;; we do this for following reasons
                 ;; a. avoiding duplicate heuristics computation
                 ;; b. duplicate entries for nodes, which needs to be removed later
                 ;; TODO: this could be sped up if we priority-map supported transients
                 update-dist (fn [curr-node curr-dist q v]
                               (let [act (+ curr-dist
                                            (if (weighted? g) (weight g curr-node v) 1))
                                     est (if (nil? (get q v))
                                           (heur curr-node v) ((get q v) 3))
                                  ]
                                 (cond
                                  (or (nil? (get q v))
                                      (> ((get q v) 2) act))
                                  (assoc q v [(+ act est ) curr-node act est])
                                  :else q)))
                 q (reduce (partial update-dist curr-node curr-dist) (pop q)
                           nbrs)]
              (recur g src target heur q explored)))))

(defn astar-dist [g src target heur]
  "Return the length of the shortest path between src and target using
    the A* algorithm"
  (let [path (astar-path g src target heur)
        dist (reduce (fn [c [u v]]
                       (if (nil? v)
                         c
                         (+ c (if (weighted? g) (weight g v u) 1))
                         )
                       ) 0 path)]
    dist))



;; ;; Todo: MST, coloring, matching, etc etc
