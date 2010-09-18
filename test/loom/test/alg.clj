(ns loom.test.alg
  (:use [loom.graph] :reload)
  (:use [loom.alg]
        [clojure.test]))

;; http://en.wikipedia.org/wiki/Dijkstra's_algorithm
(def g1
  (weighted-graph
   [1 2 7] [1 3 9] [1 6 14] [2 3 10] [2 4 15]
   [3 4 11] [3 6 2] [4 5 6] [5 6 9]))

;; http://www.algolist.com/Dijkstra's_algorithm
(def g2
  (weighted-graph
   [:r :g 10] [:r :b 5] [:r :o 8] [:g :b 3] [:b :p 7] [:p :o 2]))

;; http://fr.wikipedia.org/wiki/Algorithme_de_Dijkstra
(def g4
  (weighted-graph
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

;; Algorithm Design Manual
(def g5
  (digraph {:a [:b :c]
            :b [:c :d]
            :c [:e :f]
            :d []
            :e [:d]
            :f [:e]
            :g [:a :f]}))

(def g6 (graph [0 1] [1 2] [1 3] [2 4] [3 4] [0 5]))

(def g7 (digraph [1 2] [2 3] [3 1] [5 6] [6 7]))

(def g8 (graph {1 [2 3 4] 5 [6 7 8]}))
  
(deftest depth-first-test
  (are [expected got] (= expected got)
    #{1 2 3 5 6 7} (set (pre-traverse g7))
    #{1 2 3} (set (pre-traverse g7 1))
    #{1 2 3 4 5 6 7 8} (set (pre-traverse g8))
    #{1 2 3 4 5 6 7 8} (set (post-traverse g8))
    [:d :e :f :c :b :a :g] (post-traverse g5 :g)
    false (not (some #{(post-traverse g7 1)} [[3 2 1] [2 3 1]]))
    #{1 2 3 4 5 6 7 8} (set (nodes (digraph (pre-span g8))))
    #{2 3 4} (set (neighbors (digraph (pre-span g8)) 1))
    #{1 5} (set (neighbors (digraph (pre-span g6)) 0))
    true (let [span (digraph (pre-span g6))]
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
    #{1 2 3 4 5 6 7 8} (set (nodes (digraph (bf-span g8))))
    #{2 3} (set (neighbors (digraph (bf-span g6)) 1))
    false (not (some #{(bf-traverse (remove-nodes g6 5))}
                     [[0 1 2 3 4] [0 1 3 2 4]]))
    [:a :e :j] (bf-path g4 :a :j)
    [:a :e :j] (bf-path-bi g4 :a :j)))

(deftest dijkstra-test
  (are [expected got] (= expected got)
    [:a :c :h :j] (dijkstra-path g4 :a :j)
    [[:a :c :h :j] 487] (dijkstra-path-dist g4 :a :j)
    [[:r :o :p] 10] (dijkstra-path-dist g2 :r :p)
    #{:r :g :b :o :p} (set (map first (dijkstra-traverse g2)))
    {:r {:o 8 :b 5} :b {:g 8} :o {:p 10}} (dijkstra-span g2 :r)))

(deftest connections-test
  (are [expected got] (= expected got)
    [#{1 2 3 4} #{5 6 7 8} #{9}] (map set (connected-components
                                           (add-nodes g8 9)))
    [#{:r :g :b :o :p}] (map set (connected-components g2))
    #{1 2 3 4 5 6 7 8} (set (nodes (connect g8)))
    #{:r :g :b :o :p} (set (nodes (connect g2)))))

(deftest other-stuff-test
  (are [expected got] (= expected got)
    [:a :c :h :j] (shortest-path g4 :a :j)
    [:a :e :j] (shortest-path (graph g4) :a :j)
    #{9 10} (set (loners (add-nodes g8 9 10)))
    ;; TODO: the rest
    ))
