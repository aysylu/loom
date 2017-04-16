(ns ^{:doc "Network simplex algorithm for solving network flow"
      :author "Daniel Hopkins"}
  loom.network-simplex
  (:require
   [loom.attr
    :refer [add-attr
            add-attr-to-nodes
            add-attr-to-edges
            attr
            attrs]]
   [loom.graph
    :refer [nodes
            edges
            add-nodes
            add-edges
            weighted-digraph]]))

;; This is a port to cljc of networkx's network simplex implementation in python:
;; https://github.com/networkx/networkx/blob/master/networkx/algorithms/flow/networksimplex

;; All credit is due to the networkx team, whose code comments are also included below:


;; Find a minimum cost flow satisfying all demands in digraph G.
;; This is a primal network simplex algorithm that uses the leaving
;; arc rule to prevent cycling.

;; G is a digraph with edge costs and capacities and in which nodes
;; have demand, i.e., they want to send or receive some amount of
;; flow. A negative demand means that the node wants to send flow, a
;; positive demand means that the node want to receive flow. A flow on
;; the digraph G satisfies all demand if the net flow into each node
;; is equal to the demand of that node."

;; References
;; ----------
;; .. [1] Z. Kiraly, P. Kovacs.
;;        Efficient implementation of minimum-cost flow algorithms.
;;        Acta Universitatis Sapientiae, Informatica 4 (1):67--118. 2012.
;; .. [2] R. Barr, F. Glover, D. Klingman.
;;        Enhancement of spanning tree labeling procedures for network
;;        optimization.
;;        INFOR 17 (1):16--34. 1979.

(declare extract
         add-super-source
         init-spanning-tree
         pivot-loop
         summarize)

(defn solve
  "Run entire network simplex pipeline"
  ([g]
   (solve g {:capacity :capacity
             :cost :cost
             :demand :demand}))
  ([g attr-keys]
   (-> g
       (extract attr-keys)
       add-super-source
       init-spanning-tree
       pivot-loop
       summarize)))

(defn build-graph
  "Build graph from list of edges w/ attrs and a list of nodes w/ attrs."
  [e n]
  (as-> (weighted-digraph) g
    (apply add-nodes g (set (concat (map first e) (map second e) (map first n))))
    (apply add-edges g (map vector (map first e) (map second e)))
    (reduce
     (fn [g [u v a]]
       (reduce
        (fn [g [ak av]]
          (add-attr g [u v] ak av))
        g a))
     g e)
    (reduce
     (fn [g [n a]]
       (reduce
        (fn [g [ak av]]
          (add-attr g n ak av))
        g a))
     g n)))

(defn sqrt [x] #?(:clj (Math/sqrt x)
                  :cljs (.sqrt js/Math x)))

(defn ceil [x] #?(:clj (Math/ceil x)
                  :cljs (.ceil js/Math x)))

(defn floor [x] #?(:clj (Math/floor x)
                  :cljs (.floor js/Math x)))

(defn- wget
  "Get index i from v, wrapping negative indicies if necessary."
  [v i]
  (assert (counted? v)
          "inefficient count operation")
  (v (mod i (count v))))

(defn- abs [i] (max i (- i)))

(defn- extract
  "Extract relevant data from the graph and setup data structures.

  Number all nodes and edges and hereafter reference them using only their numbers."
  [g {:keys [capacity cost demand] :as attr-keys}]
  (let [nodes (nodes g)
        N (vec nodes) ;; nodes
        nc (count N)
        NI (zipmap nodes (range nc)) ;; node indices
        D (mapv #(or (attr g % demand) 0) nodes) ;; node demands

        edges (edges g)
        E (vec edges) ;; edges
        ec (count E)
        EI (zipmap edges (range ec)) ;; edge indices
        S (mapv (comp NI first) edges) ;; edge sources
        T (mapv (comp NI second) edges) ;; edge targets

        C (mapv #(or (attr g % cost) 0) edges) ;; edge costs; default 0
        _U (mapv #(or (attr g % capacity) 0) edges)
        inf (or (* 3 ;; take max across three dimensions and multiply by three.
                   (max (apply + _U) ;; sum of capacities
                        (apply + (map abs C)) ;; sum of costs
                        (apply max (map abs D)) ;; max demand
                        ))
                1 ;; default
                )
        U (mapv #(or (attr g % capacity) inf) edges) ;; edge capacities; default "infinity"
        ]
    ;; quick checks
    (assert (pos? nc) "Graph has no nodes")
    (assert (zero? (apply + D)) "Total node demand is not zero")
    (doseq [[e u] (map vector EI U)]
      (assert (not (neg? u)) (str "edge " e " has negative capacity")))
    ;; create map to pass around
    {:N N :NI NI :D D :E E :EI EI :S S :T T :U U :C C
     :nc nc
     :ec ec
     :inf inf}))

(defn- add-super-source
  "Add a dummy node -1 and connect all existing nodes to it with infinite-
  capacity dummy edges. Node -1 will serve as the root of the
  spanning tree of the network simplex method. The new edges will used to
  trivially satisfy the node demands and create an initial strongly
  feasible spanning tree."
  [{:keys [N D S T C U nc inf] :as omni}]
  (let [;; TODO(drhops): map-indexed okay in rn?
        ss-edges (map-indexed (fn [p d]
                                ;; zero-demand nodes must have edges towards root
                                ;; for strong feasibility
                                (if (pos? d)
                                  [-1 p]
                                  [p -1]))
                              D)

        S (apply conj S (map first ss-edges))
        T (apply conj T (map second ss-edges))

        C (apply conj C (repeat nc inf))
        U (apply conj U (repeat nc inf))]
    (merge omni
           {:S S :T T :C C :U U})))

(defn- init-spanning-tree
  "Construct the initial spanning tree."
  [{:keys [N D EI nc ec inf] :as omni}]
  (let [;; size nc+ec
        flows (vec (concat (repeat ec 0) (map abs D))) ;; edge flows

        ;; size nc
        phis ;; node potentials
        (mapv (fn [d]
               (if (pos? d) (- inf) inf))
              D)
        edges (-> (range ec (+ ec nc)) vec) ;; edges to parents

        ;; size nc+1
        ;; the following vectors are allowed to have negative indices, hence must be mod'ed
        parents (-> (repeat nc -1) vec (conj nil)) ;; parent nodes
        sizes (-> (repeat nc 1) vec (conj (inc nc))) ;; subtree sizes
        nexts (-> (range 1 nc) vec (conj -1 0)) ;; next nodes in depth-first thread
        prevs (-> (range -1 nc) vec) ;; previous nodes in depth-first thread
        lasts (-> (range nc) vec (conj (dec nc))) ;; last descendants in depth-first thread
        ]

    (assoc omni
           :flows flows
           :phis phis
           :parents parents
           :edges edges
           :sizes sizes
           :nexts nexts
           :prevs prevs
           :lasts lasts)))

(defn- reduced-cost
  "Reduced cost of edge i"
  [{:keys [S T C U flows phis] :as omni} i]
  (let [c (C i)
        phi-s (phis (S i))
        phi-t (phis (T i))]
    (* (+ c (- phi-s) phi-t)
       (if (zero? (flows i)) 1 -1))))

(defn- find-entering-edges
  "Yield entering edges until none can be found.

  Entering edges are found by combining Dantzig's rule and Bland's
  rule. The edges are cyclically grouped into blocks of size B. Within
  each block, Dantzig's rule is applied to find an entering edge. The
  blocks to search is determined following Bland's rule."
  [{:keys [S T EI C flows blockmark] :as omni}]
  (let [ec (count EI)
        _ (assert (pos? ec))
        B (int (ceil (sqrt ec))) ;; pivot block size
        M (int (ceil (/ ec B))) ;; number of blocks needed to cover all edges
        ]
    (loop [m 0 ;; number of consecutive blocks without eligible entering edges
           f (or blockmark 0) ;; first edge in block
           ]
      (let [l (+ f B)
            [l edges] ;; wrap around if needed
            (if (<= l ec)
              [l (range f l)]
              (let [l' (- l ec)]
                [l' (concat (range f ec) (range l'))]))
            f l
            ;; find the first edge with the lowest reduced cost
            ;; reverse because min-key finds last min, not first like python/networkx
            i (apply min-key (partial reduced-cost omni) (reverse edges))
            c (reduced-cost omni i)]
        (if (>= m M)
          ;; All edges have nonnegative reduced costs. The current flow is optimal.
          nil
          (if (neg? c)
            ;; entering edge found
            (let [[p q]
                  (if (zero? (flows i))
                    [(S i) (T i)]
                    [(T i) (S i)])]
              [i p q f])
            ;; no entering edge found
            (recur (inc m) f)))))))

(defn- find-apex
  "Find LCA of p and q in the spanning tree."
  [{:keys [sizes parents] :as omni} p q]
  (loop [p p
         q q]
    (if (= p q)
      p
      (let [p (loop [p p]
                (if (>= (wget sizes p) (wget sizes q))
                  p
                  (recur (wget parents p))))
            q (loop [q q]
                (if (>= (wget sizes q) (wget sizes p))
                  q
                  (recur (wget parents q))))
            [p' q'] (if (and (not= p q)
                             (= (wget sizes p) (wget sizes q)))
                      [(wget parents p)
                       (wget parents q)]
                      [p q])]
        (recur p' q')))))

(defn- trace-path
  "Return the nodes and edges on the path from node p to its ancestor w."
  [{:keys [parents edges] :as omni} p w]
  (loop [p p
         Wn [p]
         We []]
    (if (= p w)
      [Wn We]
      (let [p' (wget parents p)]
        (recur p'
               (conj Wn p')
               (conj We (edges p)))))))

(defn- find-cycle
  "Return the nodes and edges on the cycle containing edge i == (p, q)
  when the latter is added to the spanning tree.
  The cycle is oriented in the direction from p to q."
  [omni i p q]
  (let [w (find-apex omni p q)
        [Wn We] (trace-path omni p w)
        Wn (-> Wn reverse vec)
        We (-> We reverse vec (conj i))
        [WnR WeR] (trace-path omni q w)
        WnR (butlast WnR)
        Wn (vec (concat Wn WnR))
        We (vec (concat We WeR))]
    [Wn We]))

(defn- residual-capacity
  "Return the residual capacity of an edge i in the direction away
  from its endpoint p."
  [{:keys [S U flows]} i p]
  (if (= (S i) p)
    (- (U i) (flows i))
    (flows i)))

(defn- find-leaving-edge
  "Return the leaving edge in a cycle represented by Wn and We."
  [{:keys [S T] :as omni} Wn We]
  (let [[j s] (apply min-key
                     (fn [[i p]]
                       (residual-capacity omni i p))
                     ;; network x reverses We and Wn because python's min returns
                     ;; the first min, whereas min-key returns the last min.
                     (map vector We Wn))
        t (if (= s (S j))
            (T j)
            (S j))]
    [j s t]))

(defn- augment-flow!
  "Augment f units of flow along a cycle represented by Wn and We."
  [{:keys [S flows] :as omni} Wn We f]
  (reduce (fn [omni [i p]]
            (update-in omni [:flows i]
                       #(+ (or % 0) (if (= (S i) p) f (- f)))))
          omni
          (map vector We Wn)))

(defn- trace-subtree
  "Yield the nodes in the subtree rooted at a node p."
  ([{:keys [lasts nexts]} p]
   (trace-subtree nexts p (lasts p)))
  ([nexts p l]
   (if (= p l)
     (list p)
     (cons p (lazy-seq (trace-subtree nexts (wget nexts p) l))))))

(defn- remove-edge!
  "Remove an edge (s, t) where parent[t] == s from the spanning tree."
  [{:keys [nc edges parents nexts prevs lasts sizes]:as omni} s t]
  (assert (= s (parents t))
          "s is not the parent of t")
  (let [size-t (sizes t)
        prev-t (prevs t)
        last-t (lasts t)
        next-last-t (wget nexts last-t)
        nci (inc nc)]
    (-> omni
        ;; remove (s, t)
        (assoc-in [:edges t] nil)
        (assoc-in [:parents t] nil)
        ;; remove subtree rooted at t from the depth-first thread.
        (assoc-in [:nexts (mod prev-t nci)] next-last-t)
        (assoc-in [:prevs (mod next-last-t nci)] prev-t)
        (assoc-in [:nexts (mod last-t nci)] t)
        (assoc-in [:prevs t] last-t)
        ;; Update the subtree sizes and last descendants of the (old) ancestors of t.
        ((fn [omni]
           (loop [omni omni
                  s s]
             (if (nil? s)
               omni
               (recur
                 (-> omni
                     (update-in [:sizes (mod s nci)]
                                #(- % size-t))
                     (update-in [:lasts (mod s nci)]
                                #(if (= % last-t)
                                   prev-t
                                   %)))
                 (wget (:parents omni) s)))))))))

(defn- make-root!
  "Make a node q the root of its containing subtree."
  [{:keys [nc] :as omni} q]
  (let [[ancestors q]
        (loop [ancestors []
               q q]
          (if (nil? q)
            [(reverse ancestors) q]
            (recur (conj ancestors q) (wget (:parents omni) q))))]
    (reduce
     (fn [omni [p q]]
       (let [size-p ((:sizes omni) p)
             last-p ((:lasts omni) p)
             prev-q ((:prevs omni) q)
             last-q ((:lasts omni) q)
             next-last-q (wget (:nexts omni) last-q)
             nci (inc nc)]
         (as-> omni omni
           ;; make p a child of q
           (assoc-in omni [:edges p] ((:edges omni) q))
           (assoc-in omni [:edges q] nil)
           (assoc-in omni [:parents p] q)
           (assoc-in omni [:parents q] nil)
           (assoc-in omni [:sizes p] (- size-p ((:sizes omni) q)))
           (assoc-in omni [:sizes q] size-p) ;;!
           ;; remove the subtree rooted at q from the depth-first thread
           (assoc-in omni [:nexts (mod prev-q nci)] next-last-q)
           (assoc-in omni [:prevs (mod next-last-q nci)] prev-q)
           (assoc-in omni [:nexts (mod last-q nci)] q)
           (assoc-in omni [:prevs q] last-q)
             (let [[omni last-p]
                   (if (= last-p last-q)
                     ;; update
                     [(assoc-in omni [:lasts p] prev-q) prev-q]
                     ;; keep same
                     [omni last-p])]
               ;; add remaining parts of the subtree rooted at p
               ;; as a subtree of q in the depth-first thread
               (-> omni
                   (assoc-in [:prevs p] last-q)
                   (assoc-in [:nexts (mod last-q nci)] p)
                   (assoc-in [:nexts (mod last-p nci)] q)
                   (assoc-in [:prevs q] last-p)
                   (assoc-in [:lasts q] last-p))))))
     omni
     (map vector ancestors (rest ancestors)))))

(defn- add-edge!
  "Add an edge (p, q) to the spanning tree where q is the root of a subtree."
  [{:keys [lasts nexts prevs sizes parents edges nc] :as omni} i p q]
  (let [last-p (lasts p)
        next-last-p (nexts last-p)
        size-q (sizes q)
        last-q (lasts q)
        nci (inc nc)]
    (as-> omni omni
        ;; make q a child of p
        (assoc-in omni [:edges q] i)
        (assoc-in omni [:parents q] p)
        ;; insert the subtree rooted at q into the depth-first thread
        (assoc-in omni [:nexts (mod last-p nci)] q)
        (assoc-in omni [:prevs q] last-p)
        (assoc-in omni [:prevs (mod next-last-p nci)] last-q)
        (assoc-in omni [:nexts (mod last-q nci)] next-last-p)
        ;; update the subtree sizes and last descendants of the (new) ancestors of q
        (loop [omni omni
               p p]
          (if (nil? p)
            omni
            (recur
             (-> omni
                 (update-in [:sizes (mod p nci)]
                            #(+ (or % 0) size-q))
                 (update-in [:lasts (mod p nci)]
                            #(if (= % last-p) last-q %)))
             (wget (:parents omni) p)))))))

(defn- update-potentials!
  "Update the potentials of the nodes in the subtree rooted at a node
  q connected to its parent p by an edge i"
  [{:keys [T C nc ec phis] :as omni} i p q]
  (let [d (if (= q (T i))
            (+ (phis p) (- (C i)) (- (phis q)))
            (+ (phis p) (C i) (- (phis q))))
        trace (trace-subtree omni q)]
    (reduce (fn [omni q]
              (update-in omni [:phis q] #(+ (or % 0) d)))
            omni
            trace)))

(defn- index-of [coll v]
  (let [i (count (take-while #(not= v %) coll))]
    (when (or (< i (count coll))
              (= v (last coll)))
      i)))

(defn- pivot
  [omni entering-edge]
  (let [[i p q f] entering-edge
        [Wn We] (find-cycle omni i p q)
        [j s t] (find-leaving-edge omni Wn We)
        capacity (residual-capacity omni j s)]
    (as-> omni omni
      (assoc omni :blockmark f)
          (if-not (pos? capacity)
            ;; nothing to augment
            omni
            (augment-flow! omni Wn We capacity))
          (if (= i j)
            ;; do nothing more if the entering edge is the same as the leaving edge
            omni
            (let [[s t] ;; ensure that s is the parent of t
                  (if (not= s (wget (:parents omni) t))
                    [t s]
                    [s t])
                  [p q] ;; ensure that q is in the subtree rooted at t
                  (if (> (index-of We i) (index-of We j))
                    [q p]
                    [p q])]
              (-> omni
                  (remove-edge! s t)
                  (make-root! q)
                  (add-edge! i p q)
                  (update-potentials! i p q)))))))

(defn- pivot-loop
  "Pivot loop"
  [omni]
  (loop [omni omni]
    (let [ee (find-entering-edges omni)]
      (if-not ee
        ;; no eligible edge; done
        omni
        ;; found eligible edge
        (recur (pivot omni ee))))))

(defn- feasible?
  "Infeasibility and unboundedness detection"
  [{:keys [N D S T flows] :as omni}]
  ;; does flow satisfy all node demands?
  (every? #(zero? (wget flows (- (inc %)))) (range (count N))))

(defn- summarize
  "Flow cost calculation and flow map construction"
  [{:keys [N D E EI S T U C flows parents phis] :as omni}]

  (if-not (feasible? omni)
    [nil nil]
    (let [nc (count N)
          ec (count EI)

          flows (subvec flows 0 ec)
          flow-cost (apply + (map * C flows))

          D' (reduce
              (fn [D [s t f]]
                (-> D
                    (update s #(+ (or % 0) f))
                    (update t #(- (or % 0) f))))
              D
              (map vector S T flows))

          S (mapv #(N %) (take ec S)) ;; use original nodes
          T (mapv #(N %) (take ec T)) ;; use original nodes

          flow-map (reduce
                    (fn [m [s t f]]
                      (-> m
                          (update-in [s t] #(+ (or % 0) f))))
                    {}
                    (map vector S T flows))]

      [flow-cost flow-map])))
