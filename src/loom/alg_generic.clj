(ns ^{:doc "Graph algorithms for use on any type of graph"
      :author "Justin Kramer"}
  loom.alg-generic)

;;;
;;; Utility functions
;;;

(defn trace-path
  "Using a map of nodes-to-preds, traces a node's family tree back to the
  source. Cycles are not accounted for."
  [preds node]
  (take-while identity (iterate preds node)))

(defn preds->span
  "Converts a map of the form {node predecessor} to a spanning tree of the
  form {node [successors]}"
  [preds]
  (reduce
   (fn [span [n p]]
     (if p
       (assoc span p (conj (span p []) n))
       span))
   {} preds))

;;;
;;; Depth-first traversal
;;;

(defn pre-traverse
  "Traverses a graph depth-first preorder from start, successors being a
  function that returns adjacent nodes. Returns a lazy seq of nodes."
  [successors start & {:keys [seen] :or {seen #{}}}]
  (letfn [(step [stack seen]
            (when-let [node (peek stack)]
              (cons
               node
               (lazy-seq
                (let [nbrs (remove seen (successors node))]
                  (step (into (pop stack) nbrs)
                        (into seen nbrs)))))))]
    (step [start]
          (conj seen start))))

;; TODO: graph-seq, analog of tree-seq

(defn pre-span
  "Return a depth-first spanning tree of the form {node [successors]}"
  [successors start & {:keys [seen return-seen] :or {seen #{}}}]
  (loop [seen seen
         preds {start nil}
         stack [start]]
    (if (empty? stack)
      ;; TODO: this is awkward, devise something better
      (if return-seen
        [(preds->span preds) seen]
        (preds->span preds))
      (let [v (peek stack)
            seen (conj seen v)]
        (if-let [u (first (remove seen (successors v)))]
          (recur seen (assoc preds u v) (conj stack u))
          (recur seen preds (pop stack)))))))

(defn post-traverse
  "Traverses a graph depth-first postorder from start, successors being a
  function that returns adjacent nodes. Returns a vector"
  [successors start & {:keys [seen return-seen] :or {seen #{}}}]
  ;; For most graphs, being lazy wouldn't matter
  (loop [seen seen
         result []
         stack [start]]
    (if (empty? stack)
      (if return-seen
        [result seen]
        result)
      (let [v (peek stack)
            seen (conj seen v)
            nbrs (remove seen (successors v))]
        (if (empty? nbrs)
          (recur seen (conj result v) (pop stack))
          (recur seen result (conj stack (first nbrs))))))))

(defn topsort-component
  "Topological sort of a component of a (presumably) directed graph.
  Returns nil if the graph contains any cycles. See loom.alg/topsort
  for a complete topological sort"
  ([successors start]
     (topsort-component successors start #{} #{}))
  ([successors start seen explored]
     (loop [seen seen
            explored explored
            result ()
            stack [start]]
       (if (empty? stack)
         result
         (if (explored (peek stack))
           (recur seen explored result (pop stack))
           (let [v (peek stack)
                 seen (conj seen v)
                 us (remove explored (successors v))]
             (if (seq us)
               (when-not (some seen us)
                 (recur seen explored result (into stack us)))
               (recur seen (conj explored v) (conj result v) (pop stack)))))))))

;;;
;;; Breadth-first traversal
;;;

(defn bf-traverse
  "Traverses a graph breadth-first from start, successors being a
  function that returns adjacent nodes. When :f is provided, returns
  a lazy seq of (f node predecessor-map depth) for each node traversed.
  Otherwise, returns a lazy seq of the nodes. When :when is provided,
  filters successors with (f neighbor predecessor depth)."
  [successors start & {:keys [f when seen]}]
  (let [f (or f (fn [n p d] n))
        nbr-pred (or when (constantly true))]
    (letfn [(step [queue preds]
              (when-let [[node depth] (peek queue)]
                (cons
                 (f node preds depth)
                 (lazy-seq
                  (let [nbrs (->> (successors node)
                                  (remove #(contains? preds %))
                                  (filter #(nbr-pred % node (inc depth))))]
                    (step (into (pop queue) (for [nbr nbrs] [nbr (inc depth)]))
                          (reduce #(assoc %1 %2 node) preds nbrs)))))))]
      (step (conj clojure.lang.PersistentQueue/EMPTY [start 0])
            (if (map? seen)
              (assoc seen start nil)
              (into {start nil} (for [s seen] [s nil])))))))

(defn bf-span
  "Return a breadth-first spanning tree of the form {node [successors]}"
  [successors start & {:keys [seen]}]
  (preds->span
   (last
    (bf-traverse successors start
                 :f (fn [_ pm _] pm)
                 :seen seen))))

(defn bf-path
  "Return a path from start to end with the fewest hops (i.e. irrespective
  of edge weights), successors being a function that returns adjacent nodes"
  [successors start end & {:as opts}]
  (let [opts (merge opts {:f vector})]
    (when-let [preds (some (fn [[_ pm _]] (when (pm end) pm))
                           (apply bf-traverse successors start (apply concat opts)))]
      (reverse (trace-path preds end)))))

(defn- shared-keys
  "Return a lazy-seq of the keys that exist in both m1 and m2"
  [m1 m2]
  (if (< (count m2) (count m1))
    (recur m2 m1)
    (filter (partial contains? m2) (keys m1))))

(defn bf-path-bi
  "Using a bidirectional breadth-first search, finds a path from start to
  end with the fewest hops (i.e. irrespective of edge weights), outgoing
  and predecessors being functions which return adjacent nodes. Can be much faster
  than a unidirectional search on certain types of graphs"
  [outgoing predecessors start end]
  (let [done? (atom false)
        preds1 (atom {}) ;from start to end
        preds2 (atom {}) ;from end to start
        search (fn [nbrs n preds]
                 (dorun
                  (take-while
                   (fn [_] (not @done?))
                   (bf-traverse
                    nbrs n :f (fn [_ pm _] (reset! preds pm))))))
        search1 (future (search outgoing start preds1))
        search2 (future (search predecessors end preds2))
        ;; TODO: watchers?
        find-intersects #(shared-keys @preds1 @preds2)]
    (loop [intersects (find-intersects)]
      (if (or (seq intersects) (future-done? search1) (future-done? search2))
        (do
          (reset! done? true)
          (cond
           (seq intersects)
           (let [intersect (apply min-key
                                  #(+ (count (trace-path @preds1 %))
                                      (count (trace-path @preds2 %)))
                                  intersects)]
             (concat
              (reverse (trace-path @preds1 intersect))
              (rest (trace-path @preds2 intersect))))
           (@preds1 end) (reverse (trace-path @preds1 end))
           (@preds2 start) (trace-path @preds2 start)))
        (recur (find-intersects))))))

;; FIXME: Decide whether this can be optimized and is worth keeping
#_(defn bf-path-bi2
  "Non-threaded version of bf-path-bi. Tends to be slower."
  [outgoing predecessors start end]
  (loop [preds {start nil}
         succs {end nil}
         q1 [start]
         q2 [end]]
    (when (and (seq q1) (seq q2))
      (if (<= (count q1) (count q2))
        (let [pairs (for [node q1 nbr (outgoing node)
                          :when (not (contains? preds nbr))]
                      [nbr node])
              preds (into preds pairs)
              q1 (map first pairs)]
          (if-let [i (some #(when (contains? succs %) %) q1)]
            (concat
             (reverse (trace-path preds i))
             (rest (trace-path succs i)))
            (recur preds succs q1 q2)))
        (let [pairs (for [node q2 nbr (predecessors node)
                          :when (not (contains? succs nbr))]
                      [nbr node])
              succs (into succs pairs)
              q2 (map first pairs)]
          (if-let [i (some #(when (contains? preds %) %) q2)]
            (concat
             (reverse (trace-path preds i))
             (rest (trace-path succs i)))
            (recur preds succs q1 q2)))))))

;;;
;;; Dijkstra
;;;

(defn dijkstra-traverse
  "Returns a lazy-seq of [current-node state] where state is a map in the
  format {node [distance predecessor]}. When f is provided, returns
  a lazy-seq of (f node state) for each node"
  ([successors dist start]
     (dijkstra-traverse successors dist start vector))
  ([successors dist start f]
     (letfn [(step [[state pq]]
               (when-let [[dist-su _ u :as fpq] (first pq)]
                 (cons
                  (f u state)
                  (lazy-seq
                   (step
                    (reduce
                     (fn [[state pq] v]
                       (let [dist-suv (+ dist-su (dist u v))
                             dist-sv (first (state v))]
                         (if (and dist-sv (>= dist-suv dist-sv))
                           [state pq]
                           (let [pq (if dist-sv
                                      (disj pq [dist-sv (hash v) v])
                                      pq)]
                             [(assoc state v [dist-suv u])
                              (conj pq [dist-suv (hash v) v])]))))
                     [state (disj pq fpq)]
                     (successors u)))))))]
       (step [{start [0 nil]}
              ;; Poor man's priority queue. Caveats:
              ;; 1) Have to keep it in sync with current state
              ;; 2) Have to include hash codes for non-Comparable items
              ;; 3) O(logn) operations
              ;; Tried clojure.contrib.priority-map but it wasn't any faster
              (sorted-set [0 (hash start) start])]))))

(defn dijkstra-span
  "Finds all shortest distances from start, where successors and dist
  are functions called as (successors node) and (dist node1 node2).
  Returns a map in the format {node {successor distance}}"
  [successors dist start]
  (reduce
   (fn [span [n [d p]]]
     (if p
       (assoc-in span [p n] d)
       span))
   {}
   (second (last (dijkstra-traverse successors dist start)))))

(defn dijkstra-path-dist
  "Finds the shortest path from start to end, where successors and dist
  are functions called as (successors node) and (dist node1 node2).
  Returns a vector: [path distance]"
  [successors dist start end]
  (if-let [[_ end-state] (first (filter
                                 (fn [[node _]] (= end node))
                                 (dijkstra-traverse successors dist start)))]
    [(reverse (trace-path (comp second end-state) end))
     (first (end-state end))]))

(defn dijkstra-path
  "Finds the shortest path from start to end, where successors and dist
  are functions called as (successors node) and (dist node1 node2)"
  [successors dist start end]
  (first (dijkstra-path-dist successors dist start end)))

;; FIXME: Research proper way to do this
#_(defn dijkstra-path-dist-bi
  "Finds a path -- not necessarily the shortest -- from start to end
  birectionally, where successors and dist are functions called as
  (successors node) and (dist node1 node2). Returns a vector: [path distance]"
  [successors dist start end]
  ;; TODO: make this work better with directed graphs (predecessors fn)
  (let [done? (atom false)
        processed1 (atom #{})
        processed2 (atom #{})
        state1 (atom nil)
        state2 (atom nil)
        find-intersect (fn [] (some #(when (@processed1 %) %) @processed2))
        search (fn [n processed state]
                 (dorun
                  (take-while
                   (fn [_] (not @done?))
                   (dijkstra-traverse successors dist n 
                                      #(do
                                         (swap! processed conj %1)
                                         (reset! state %2))))))
        search1 (future (search start processed1 state1))
        search2 (future (search end processed2 state2))]
    (loop [intersect (find-intersect)]
      (if (or intersect (future-done? search1))
        (do
          (prn intersect)
          (reset! done? true)
          (cond
           intersect [(concat
                       (reverse (trace-path (comp second @state1) intersect))
                       (rest (trace-path (comp second @state2) intersect)))
                      (+ (first (@state1 intersect))
                         (first (@state2 intersect)))]
           (@state1 end) [(reverse (trace-path (comp second @state1) end))
                          (first (@state1 end))]
           (@state2 start) [(trace-path (comp second @state2) start)
                            (first (@state2 start))]))
          
        (recur (find-intersect))))))

