(ns loom.test.alg
  (:use [loom.graph] :reload)
  (:use [loom.alg]
        [clojure.test]))

;; http://en.wikipedia.org/wiki/Dijkstra's_algorithm
(def g1
  (graph
   [1 2 7] [1 3 9] [1 6 14] [2 3 10] [2 4 15]
   [3 4 11] [3 6 2] [4 5 6] [5 6 9]))

;; http://www.algolist.com/Dijkstra's_algorithm
(def g2
  (graph
   [:r :g 10] [:r :b 5] [:r :o 8] [:g :b 3] [:b :p 7] [:p :o 2]))

;; http://fr.wikipedia.org/wiki/Algorithme_de_Dijkstra
(def g4
  (graph
   [:a :b 85]
   [:b :f 80]
   [:f :i 250]
   [:i :j 84]
   [:a :c 217]
   [:c :g 186]
   [:c :h 103]
   [:d :h 183]
   [:h :j 167]
   [:a :e 173]
   [:e :j 502]))

;; Algorithm Design Manual, p 179
(def g5
  (directed-graph {:a [:b :c]
            :b [:c :d]
            :c [:e :f]
            :d []
            :e [:d]
            :f [:e]
            :g [:a :f]}))

(def g6 (graph [0 1] [1 2] [1 3] [2 4] [3 4] [0 5]))

(def g7 (directed-graph [1 2] [2 3] [3 1] [5 6] [6 7]))

(def g8 (graph {1 [2 3 4] 5 [6 7 8]}))

;; Algorithm Design Manual, p 182
(def g9
  (directed-graph {8 #{6},
                   7 #{5},
                   6 #{7},
                   5 #{6},
                   4 #{1 6 8},
                   3 #{1},
                   2 #{3 4 5},
                   1 #{2}}))

;; http://en.wikipedia.org/wiki/Strongly_connected_component
(def g10
  (directed-graph {:a [:b]
                   :b [:c :e :f]
                   :c [:d :g]
                   :d [:c :h]
                   :e [:a :f]
                   :f [:g]
                   :g [:f]
                   :h [:g :d]}))

;; Weighted directed graph with a negative-weight cycle
;; which is reachable from sources :a, :b, :d, and :e.
;; http://www.seas.gwu.edu/~simhaweb/alg/lectures/module9/module9.html
(def g11
  (directed-graph [:a :b 3]
                  [:b :c 4]
                  [:b :d 5]
                  [:d :e 2]
                  [:e :b -8]))

;; Weighted directed graph with a non-negative-weight cycle,
;; similar to g11, but with the edge [:e :b] reweighed.
(def g12
  (directed-graph [:a :b 3]
                  [:b :c 4]
                  [:b :d 5]
                  [:d :e 2]
                  [:e :b -7]))

(deftest depth-first-test
  (are [expected got] (= expected got)
    #{1 2 3 5 6 7} (set (pre-traverse g7))
    #{1 2 3} (set (pre-traverse g7 1))
    #{1 2 3 4 5 6 7 8} (set (pre-traverse g8))
    #{1 2 3 4 5 6 7 8} (set (post-traverse g8))
    [:d :e :f :c :b :a :g] (post-traverse g5 :g)
    false (not (some #{(post-traverse g7 1)} [[3 2 1] [2 3 1]]))
    #{1 2 3 4 5 6 7 8} (set (nodes (directed-graph (pre-span g8))))
    #{2 3 4} (set (neighbors (directed-graph (pre-span g8)) 1))
    #{1 5} (set (neighbors (directed-graph (pre-span g6)) 0))
    true (let [span (directed-graph (pre-span g6))]
           (and (or (= #{3} (set (neighbors span 4)))
                    (= #{2} (set (neighbors span 4))))
                (or (= #{3} (set (neighbors span 1)))
                    (= #{2} (set (neighbors span 1))))))
    [:g :a :b :c :f :e :d] (topsort g5)
    nil (topsort g7)
    [5 6 7] (topsort g7 5)))

(deftest breadth-first-test
  (are [expected got] (= expected got)
    #{1 2 3 5 6 7} (set (bf-traverse g7))
    #{1 2 3} (set (bf-traverse g7 1))
    #{1 2 3 4 5 6 7 8} (set (bf-traverse g8))
    #{1 2 3 4 5 6 7 8} (set (nodes (directed-graph (bf-span g8))))
    #{2 3} (set (neighbors (directed-graph (bf-span g6)) 1))
    false (not (some #{(bf-traverse (remove-nodes g6 5))}
                     [[0 1 2 3 4] [0 1 3 2 4]]))
    #{:r} (set (bf-traverse g2 :r :when #(< %3 1)))
    #{:r :o :b :g} (set (bf-traverse g2 :r :when #(< %3 2)))
    #{:r :o :b :g :p} (set (bf-traverse g2 :r :when #(< %3 3)))
    [:a :e :j] (bf-path g4 :a :j)
    [:a :c :h :j] (bf-path g4 :a :j :when (fn [n p d] (not= :e n)))
    [:a :e :j] (bf-path-bi g4 :a :j)
    true (some #(= % (bf-path-bi g5 :g :d)) [[:g :a :b :d] [:g :f :e :d]])))

(deftest dijkstra-test
  (are [expected got] (= expected got)
    [:a :c :h :j] (dijkstra-path g4 :a :j)
    [[:a :c :h :j] 487] (dijkstra-path-dist g4 :a :j)
    [[:r :o :p] 10] (dijkstra-path-dist g2 :r :p)
    #{:r :g :b :o :p} (set (map first (dijkstra-traverse g2)))
    {:r {:o 8 :b 5} :b {:g 8} :o {:p 10}} (dijkstra-span g2 :r)))

(deftest connectivity-test
  (are [expected got] (= expected got)
    [#{1 2 3 4} #{5 6 7 8} #{9}] (map set (connected-components
                                           (add-nodes g8 9)))
    [#{:r :g :b :o :p}] (map set (connected-components g2))
    [#{1 2 3 4 5 6 8 7}] (map set (connected-components g9))
    true (connected? g6)
    false (connected? g7)
    true (connected? g9)
    #{#{2 3 4 1} #{8} #{7 5 6}} (set (map set (scc g9)))
    #{#{:b :e :a} #{:h :d :c} #{:f :g}} (set (map set (scc g10)))
    false (strongly-connected? g9)
    true (strongly-connected? (directed-graph g2))
    #{1 2 3 4 5 6 7 8} (set (nodes (connect g8)))
    #{:r :g :b :o :p} (set (nodes (connect g2)))))

(deftest other-stuff-test
  (are [expected got] (= expected got)
    false (dag? g2)
    true (dag? (directed-graph (bf-span g2)))
    true (dag? g5)
    [:a :c :h :j] (shortest-path g4 :a :j)
    #{9 10} (set (loners (add-nodes g8 9 10)))
    ;; TODO: the rest
    ))

(deftest bellman-ford-test
  (are [expected graph start]
       (= expected (bellman-ford graph start))

       false g11 :a
       false g11 :b
       [{:e Double/POSITIVE_INFINITY,
         :d Double/POSITIVE_INFINITY,
         :b Double/POSITIVE_INFINITY,
         :a Double/POSITIVE_INFINITY,
         :c 0}{:c [:c]}] g11 :c
       false g11 :d
       false g11 :e
       [{:e 10,
         :d 8,
         :b 3,
         :c 7,
         :a 0}
        {:a [:a],
         :c [:a :b :c],
         :b [:a :b],
         :d [:a :b :d],
         :e [:a :b :d :e]}] g12 :a
       [{:e 7,
         :d 5,
         :c 4,
         :a Double/POSITIVE_INFINITY,
         :b 0}
        {:b [:b],
         :c [:b :c],
         :d [:b :d],
         :e [:b :d :e]}] g12 :b
       [{:e Double/POSITIVE_INFINITY,
         :d Double/POSITIVE_INFINITY,
         :b Double/POSITIVE_INFINITY,
         :a Double/POSITIVE_INFINITY,
         :c 0}
        {:c [:c]}] g12 :c
       [{:e 2,
         :b -5,
         :c -1,
         :a Double/POSITIVE_INFINITY,
         :d 0}
        {:d [:d],
         :c [:d :e :b :c],
         :b [:d :e :b],
         :e [:d :e]}] g12 :d
       [{:d -2,
         :b -7,
         :c -3,
         :a Double/POSITIVE_INFINITY,
         :e 0}
        {:e [:e],
         :c [:e :b :c],
         :b [:e :b],
         :d [:e :b :d]}] g12 :e))

(deftest bipartite-test
  (are [expected got] (= expected got)
    {0 1, 1 0, 5 0, 2 1, 3 1, 4 0} (bipartite-color g6)
    {5 1, 1 1, 2 0, 3 0, 4 0, 6 0, 7 0, 8 0} (bipartite-color g8)
    nil (bipartite-color g1)
    true (bipartite? g6)
    true (bipartite? g8)
    false (bipartite? g1)
    #{#{2 3 4 6 7 8} #{1 5}} (set (bipartite-sets g8))))

