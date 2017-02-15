(ns ^{:doc "Graph algorithms. Any graph record/type that satisfies the
Graph, Digraph, or WeightedGraph protocols (as appropriate per algorithm)
can use these functions."
      :author "Justin Kramer"}
  loom.alg
  (:require [loom.alg-generic :as gen]
            [loom.flow :as flow]
            [loom.graph
             :refer [add-nodes add-edges nodes edges successors weight predecessors
                     out-degree in-degree weighted? directed? graph digraph transpose]
             :as graph]
            [loom.alg-generic :refer [trace-path preds->span]]
            #?(:clj [clojure.data.priority-map :as pm]
               :cljs [tailrecursion.priority-map :as pm])
            [clojure.set :as clj.set]))

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
        (let [ctrav (traverse n :seen seen)]
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
  "Returns a depth-first spanning tree of the form {node [successors]}"
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
  "Returns a breadth-first spanning tree of the form {node [successors]}"
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
  "Returns a path from start to end with the fewest hops (i.e. irrespective
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
  "Tests for whether we can improve the shortest path to v found so far
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
        infinities (repeat #?(:clj Double/POSITIVE_INFINITY
                              :cljs js/Infinity))
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
   from the source exists, and false otherwise, indicating that no
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
            (remove #(= #?(:clj Double/POSITIVE_INFINITY
                           :cljs js/Infinity)
                        (get costs %)))
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
  "Returns true if g is a directed acyclic graph"
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

(defn- bellman-ford-transform
  "Helper function for Johnson's algorithm. Uses Bellman-Ford to remove negative weights."
  [wg]
  (let [q (first (drop-while (partial graph/has-node? wg) (repeatedly gensym)))
        es (for [v (graph/nodes wg)] [q v 0])
        bf-results (bellman-ford (graph/add-edges* wg es) q)]
    (if bf-results
      (let [[dist-q _] bf-results
            new-es (map (juxt first second (fn [[u v]]
                                             (+ (weight wg u v) (- (dist-q u)
                                                                   (dist-q v)))))
                        (graph/edges wg))]
        (graph/add-edges* wg new-es))
      false)))

(defn johnson
  "Finds all-pairs shortest paths using Bellman-Ford to remove any negative edges before
  using Dijkstra's algorithm to find the shortest paths from each vertex to every other.
  This algorithm is efficient for sparse graphs.

  If the graph is unweighted, a default weight of 1 will be used. Note that it is more efficient
  to use breadth-first spans for a graph with a uniform edge weight rather than Dijkstra's algorithm.
  Most callers should use shortest-paths and allow the most efficient implementation be selected
  for the graph."
  [g]
  (let [g (if (and (weighted? g) (some (partial > 0) (map (graph/weight g) (graph/edges g))))
            (bellman-ford-transform g)
            g)]
    (if (false? g)
      false
      (let [dist (if (weighted? g)
                   (weight g)
                   (fn [u v] (when (graph/has-edge? g u v) 1)))]
        (reduce (fn [acc node]
                  (assoc acc node (gen/dijkstra-span (successors g) dist node)))
                {}
                (nodes g))))))

(defn bf-all-pairs-shortest-paths
  "Uses bf-span on each node in the graph."
  [g]
  (reduce (fn [spans node]
            (assoc spans node (bf-span g node)))
          {}
          (nodes g)))

(defn all-pairs-shortest-paths
  "Finds all-pairs shortest paths in a graph. Uses Johnson's algorithm for weighted graphs
  which is efficient for sparse graphs. Breadth-first spans are used for unweighted graphs."
  [g]
  (if (weighted? g)
    (johnson g)
    (bf-all-pairs-shortest-paths g)))

(defn connected-components
  "Returns the connected components of graph g as a vector of vectors. If g
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
  "Returns the strongly-connected components of directed graph g as a vector of
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
  "Returns graph g with all connected components connected to each other"
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
  "Returns nodes with no connections to other nodes (i.e., isolated nodes)"
  [g]
  (let [degree-total (if (directed? g)
                       #(+ (in-degree g %) (out-degree g %))
                       #(out-degree g %))]
    (filter (comp zero? degree-total) (nodes g))))

(defn distinct-edges
  "Returns the distinct edges of g. Only useful for undirected graphs"
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
  "Attempts a two-coloring of graph g. When successful, returns a map of
  nodes to colors (1 or 0). Otherwise, returns nil."
  [g]
  (letfn [(color-component [coloring start]
            (loop [coloring (assoc coloring start 1)
                   queue (conj #?(:clj clojure.lang.PersistentQueue/EMPTY
                                  :cljs cljs.core/PersistentQueue.EMPTY) start)]
              (if (empty? queue)
                coloring
                (let [v (peek queue)
                      color (- 1 (coloring v))
                      nbrs (graph/successors g v)]
                  ;; TODO: could be better
                  (if (some #(and (coloring %) (= (coloring v) (coloring %)))
                            nbrs)
                    nil ; graph is not bipartite
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
  "Returns true if g is bipartite"
  [g]
  (boolean (bipartite-color g)))

(defn bipartite-sets
  "Returns two sets of nodes, one for each color of the bipartite coloring,
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

(defn- neighbor-colors
  "Given a putative coloring of a graph, returns the colors of all the
  neighbors of a given node."
  [g node coloring]
  (let [successors (graph/successors g node)
        neighbors (if-not (directed? g)
                    successors
                    (concat successors
                            (graph/predecessors g node)))]
    (set (remove nil?
                 (map #(get coloring %)
                      neighbors)))))

(defn coloring?
  "Returns true if a map of nodes to colors is a proper coloring of a graph."
  [g coloring]
  (letfn [(different-colors? [node]
            (not (contains? (neighbor-colors g node coloring)
                            (coloring node))))]
    (and (every? different-colors? (nodes g))
         (every? (complement nil?) (map #(get coloring %)
                                        (nodes g))))))

(defn greedy-coloring
  "Greedily color the vertices of a graph using the first-fit heuristic.
  Returns a map of nodes to colors (0, 1, ...)."
  [g]
  (loop [node-seq (bf-traverse g)
         coloring {}
         colors #{}]
    (if (empty? node-seq)
      coloring
      (let [node (first node-seq)
            possible-colors (clj.set/difference colors
                                                (neighbor-colors g
                                                                 node
                                                                 coloring))
            node-color (if (empty? possible-colors)
                         (count colors)
                         (apply min possible-colors))]
        (recur (rest node-seq)
               (conj coloring [node node-color])
               (conj colors node-color))))))

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
                                 (ex-info
                                  (str "Method not found.  Choose from: "
                                       method-set)
                                  {:method-set method-set})))]
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
  represents an MST of the given graph. Returns the MST edge-list
  for un-weighted graphs."
  ([wg]
     (cond
      (directed? wg) (throw (#?(:clj Exception. :cljs js/Error)
                             "Spanning tree only defined for undirected graphs"))
      :else (let [mst (prim-mst-edges wg (nodes wg) nil #{} [])]
              (if (weighted? wg)
                mst
                (map #(vec [(first %1) (second %1)]) mst)))))
  ([wg n h visited acc]
     (cond
      (empty? n) acc
      (empty? h) (let [v (first n)
                       h  (into (pm/priority-map-keyfn second) (edge-weights wg v))]
                   (recur wg (disj n v) h (conj visited v) acc))
      :else (let [next_edge (peek h)
                  u (first (second next_edge))
                  v (first next_edge)
                  update-dist (fn [h [v [u wt]]]
                                (cond
                                 (nil? (get h v)) (assoc h v [u wt])
                                 (> (second (get h v)) wt) (assoc h v [u wt])
                                 :else h))]
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
  "Returns the shortest path using A* algorithm. Returns a map of predecessors."
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
      (empty? q) (throw (ex-info "Target not reachable from source" {}))
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
                 nbrs (remove (into #{} (keys explored)) (successors g curr-node))
                 ;; we do this for following reasons
                 ;; a. avoiding duplicate heuristics computation
                 ;; b. duplicate entries for nodes, which needs to be removed later
                 ;; TODO: this could be sped up if we priority-map supported transients
                 update-dist (fn [curr-node curr-dist q v]
                               (let [act (+ curr-dist
                                            (if (weighted? g) (weight g curr-node v) 1))
                                     est (if (nil? (get q v))
                                           (heur v target) ((get q v) 3))
                                  ]
                                 (cond
                                  (or (nil? (get q v))
                                      (> ((get q v) 2) act))
                                  (assoc q v [(+ act est ) curr-node act est])
                                  :else q)))
                 q (reduce (partial update-dist curr-node curr-dist) (pop q)
                           nbrs)]
              (recur g src target heur q explored)))))

(defn astar-dist
  "Returns the length of the shortest path between src and target using
    the A* algorithm"
  [g src target heur]
  (let [path (astar-path g src target heur)
        dist (reduce (fn [c [u v]]
                       (if (nil? v)
                         c
                         (+ c (if (weighted? g) (weight g v u) 1))
                         )
                       ) 0 path)]
    dist))

(defn degeneracy-ordering
  "Returns sequence of vertices in degeneracy order."
  [g]
  (loop [ordered-nodes []
         node-degs (->> (zipmap (nodes g)
                                (map (partial out-degree g) (nodes g)))
                        (into (pm/priority-map)))
         k 0]
    (if (empty? node-degs)
      ordered-nodes
      (let [[n deg] (first node-degs)
            ;; This will be the adjacent nodes still in node-degs (not in ordered-nodes) decr'd by 1
            updated-degs (->> (map (juxt identity node-degs) (successors g n))
                              (filter second)
                              (map (juxt first (comp dec second)))
                              (into {}))]
        (recur (conj ordered-nodes n)
               (reduce (fn [n-ds [n d]] ;; Update this assoc'ing the updated-degs found above
                         (assoc n-ds n d))
                       (dissoc node-degs n)
                       updated-degs)
               (max k deg))))))

(defn- bk-gen [g [r p x] stack]
  (let [v-pivot (reduce (partial max-key (partial out-degree g)) p)]
    (loop [v v-pivot
           p (set p)
           x (set x)
           stack stack]
      (if (nil? v)
        stack
        (let [succ-v (set (successors g v))]
          (recur (-> (clj.set/difference (disj p v)
                                         (set (successors g v-pivot)))
                     first)
                 (disj p v)
                 (conj x v)
                 (conj stack [(conj r v)
                              (clj.set/intersection p succ-v)
                              (clj.set/intersection x succ-v)])))))))

(defn- bk
  "An iterative implementation of Bron-Kerbosch using degeneracy ordering
  at the outer loop and max-degree vertex pivoting in the inner loop."
  [g]
  (loop [vs (degeneracy-ordering g)
         max-clqs (seq [])
         p (set (nodes g))
         x #{}
         stack []]
    (cond
     ;; Done
     (and (empty? stack) (empty? vs))
     max-clqs

     ;; Empty stack, create a seed to generate stack items
     (empty? stack)
     (let [v (first vs)
           succ-v (set (successors g v))]
       (recur (rest vs)
              max-clqs
              (disj p v)
              (conj x v)
              [[#{v}
                (clj.set/intersection p succ-v)
                (clj.set/intersection x succ-v)]]))

     ;; Pull the next request off the stack
     :else
     (let [[r s-p s-x] (peek stack)]
       (cond
        ;; Maximal clique found
        (and (empty? s-p) (empty? s-x))
        (recur vs
               (cons r max-clqs)
               p
               x
               (pop stack))
        ;; No maximal clique that excludes x exists
        (empty? s-p)
        (recur vs
               max-clqs
               p
               x
               (pop stack))
        ;; Use this state to generate more states
        :else
        (recur vs
               max-clqs
               p
               x
               (bk-gen g [r s-p s-x] (pop stack))))))))

(defn maximal-cliques
  "Enumerate the maximal cliques using Bron-Kerbosch."
  [g]
  (bk g))

;;;
;;; Compare graphs
;;;
(defn subgraph?
  "Returns true iff g1 is a subgraph of g2. An undirected graph is never
  considered as a subgraph of a directed graph and vice versa."
  [g1 g2]
  (and (= (directed? g1) (directed? g2))
       (let [edge-test-fn (if (directed? g1)
                            graph/has-edge?
                            (fn [g x y]
                              (or (graph/has-edge? g x y)
                                  (graph/has-edge? g y x))))]
         (and (every? #(graph/has-node? g2 %) (nodes g1))
              (every? (fn [[x y]] (edge-test-fn g2 x y))
                      (edges g1))))))

(defn eql?
  "Returns true iff g1 is a subgraph of g2 and g2 is a subgraph of g1"
  [g1 g2]
  (and (subgraph? g1 g2)
       (subgraph? g2 g1)))

(defn isomorphism?
  "Given a mapping phi between the vertices of two graphs, determine
  if the mapping is an isomorphism, e.g., {(phi x), (phi y)} connected
  in g2 iff {x, y} are connected in g1."
  [g1 g2 phi]
  (eql? g2 (-> (if (directed? g1) (digraph) (graph))
               (graph/add-nodes* (map phi (nodes g1)))
               (graph/add-edges* (map (fn [[x y]] [(phi x) (phi y)])
                                      (edges g1))))))

;; ;; Todo: MST, coloring, matching, etc etc
