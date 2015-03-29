(ns loom.test.graph
  (:require [loom.graph :refer :all]
            [loom.gen :as gg]
            [clojure.test :refer :all]
            ;;
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]))

(deftest simple-graph-test
  (let [g1 (graph [1 2] [1 3] [2 3] 4)
        g2 (graph {1 [2 3] 2 [3] 4 []})
        g3 (graph g1)
        g4 (graph g3 (digraph [5 6]) [7 8] 9)
        g5 (graph)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (nodes g1))
           #{[1 2] [2 1] [1 3] [3 1] [2 3] [3 2]} (set (edges g1))
           (set (nodes g2)) (set (nodes g1))
           (set (edges g2)) (set (edges g1))
           (set (nodes g3)) (set (nodes g1))
           (set (nodes g3)) (set (nodes g1))
           #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
           #{[1 2] [2 1] [1 3] [3 1] [2 3]
             [3 2] [5 6] [6 5] [7 8] [8 7]} (set (edges g4))
             #{} (set (nodes g5))
             #{} (set (edges g5))
             true (has-node? g1 4)
             true (has-edge? g1 1 2)
             false (has-node? g1 5)
             false (has-edge? g1 4 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
           #{2 3} (set (successors g1 1))
           #{1 2} (set (successors g1 3))
           #{} (set (successors g1 4))
           2 (out-degree g1 1)
           2 (out-degree g1 3)
           0 (out-degree g1 4)))
    (testing "Add & remove"
      (are [expected got] (= expected got)
           #{1 2 3 4 5} (set (nodes (add-nodes g1 5)))
           #{:a :b :c} (set (nodes (add-nodes g5 :a :b :c)))
           #{{:id 1} {:id 2}} (set (nodes (add-nodes g5 {:id 1} {:id 2})))
           #{[1 2] [2 1]} (set (edges (add-edges g5 [1 2])))
           #{1 2} (set (nodes (remove-nodes g1 3 4)))
           #{[1 2] [2 1]} (set (edges (remove-nodes g1 3 4)))
           #{1 2 3 4} (set (nodes (remove-edges g1 [1 2] [2 1] [1 3] [3 1])))
           #{[2 3] [3 2]} (set (edges (remove-edges
                                       g1 [1 2] [2 1] [1 3] [3 1])))))))

(deftest simple-digraph-test
  (let [g1 (digraph [1 2] [1 3] [2 3] 4)
        g2 (digraph {1 [2 3] 2 [3] 4 []})
        g3 (digraph g1)
        g4 (digraph g3 (graph [5 6]) [7 8] 9)
        g5 (digraph)
        g6 (transpose g1)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (nodes g1))
           #{1 2 3 4} (set (nodes g6))
           #{[1 2] [1 3] [2 3]} (set (edges g1))
           #{[2 1] [3 1] [3 2]} (set (edges g6))
           (set (nodes g2)) (set (nodes g1))
           (set (edges g2)) (set (edges g1))
           (set (nodes g3)) (set (nodes g1))
           (set (nodes g3)) (set (nodes g1))
           #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
           #{[1 2] [1 3] [2 3] [5 6] [6 5] [7 8]} (set (edges g4))
           #{} (set (nodes g5))
           #{} (set (edges g5))
           true (has-node? g1 4)
           true (has-edge? g1 1 2)
           false (has-node? g1 5)
           false (has-edge? g1 2 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
           #{2 3} (set (successors g1 1))
           #{} (set (successors g1 3))
           #{} (set (successors g1 4))
           2 (out-degree g1 1)
           0 (out-degree g1 3)
           0 (out-degree g1 4)
           #{1 2} (set (predecessors g1 3))
           #{} (set (predecessors g1 1))
           2 (in-degree g1 3)
           0 (in-degree g1 1)
           #{1 2} (set (successors g6 3))
           #{} (set (successors g6 1))
           2 (out-degree g6 3)
           0 (out-degree g6 1)))
    (testing "Add & remove"
      (are [expected got] (= expected got)
           #{1 2 3 4 5} (set (nodes (add-nodes g1 5)))
           #{:a :b :c} (set (nodes (add-nodes g5 :a :b :c)))
           #{{:id 1} {:id 2}} (set (nodes (add-nodes g5 {:id 1} {:id 2})))
           #{[1 2]} (set (edges (add-edges g5 [1 2])))
           #{1 2} (set (nodes (remove-nodes g1 3 4)))
           #{[1 2]} (set (edges (remove-nodes g1 3 4)))
           #{1 2 3 4} (set (nodes (remove-edges g1 [1 2] [1 3])))
           #{[2 3]} (set (edges (remove-edges g1 [1 2] [1 3])))))))

(deftest simple-weighted-graph-test
  (let [g1 (weighted-graph [1 2 77] [1 3 88] [2 3 99] 4)
        g2 (weighted-graph {1 {2 77 3 88} 2 {3 99} 4 []})
        g3 (weighted-graph g1)
        g4 (weighted-graph g3 (weighted-digraph [5 6 88]) [7 8] 9)
        g5 (weighted-graph)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (nodes g1))
           #{[1 2] [2 1] [1 3] [3 1] [2 3] [3 2]} (set (edges g1))
           (set (nodes g2)) (set (nodes g1))
           (set (edges g2)) (set (edges g1))
           (set (nodes g3)) (set (nodes g1))
           (set (nodes g3)) (set (nodes g1))
           #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
           #{[1 2] [2 1] [1 3] [3 1] [2 3]
             [3 2] [5 6] [6 5] [7 8] [8 7]} (set (edges g4))
             #{} (set (nodes g5))
             #{} (set (edges g5))
             true (has-node? g1 4)
             true (has-edge? g1 1 2)
             false (has-node? g1 5)
             false (has-edge? g1 4 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
           #{2 3} (set (successors g1 1))
           #{1 2} (set (successors g1 3))
           #{} (set (successors g1 4))
           2 (out-degree g1 1)
           2 (out-degree g1 3)
           0 (out-degree g1 4)))
    (testing "Add & remove"
      (are [expected got] (= expected got)
           #{1 2 3 4 5} (set (nodes (add-nodes g1 5)))
           #{:a :b :c} (set (nodes (add-nodes g5 :a :b :c)))
           #{{:id 1} {:id 2}} (set (nodes (add-nodes g5 {:id 1} {:id 2})))
           #{[1 2] [2 1]} (set (edges (add-edges g5 [1 2])))
           #{1 2} (set (nodes (remove-nodes g1 3 4)))
           #{[1 2] [2 1]} (set (edges (remove-nodes g1 3 4)))
           #{1 2 3 4} (set (nodes (remove-edges g1 [1 2] [2 1] [1 3] [3 1])))
           #{[2 3] [3 2]} (set (edges (remove-edges
                                       g1 [1 2] [2 1] [1 3] [3 1])))))
    (testing "Weight"
      (are [expected got] (= expected got)
           77 (weight g1 1 2)
           77 (weight g2 1 2)
           77 (weight g3 1 2)
           88 (weight g4 6 5)
           1 (weight g4 7 8)))))

(deftest simple-weighted-digraph-test
  (let [g1 (weighted-digraph [1 2 77] [1 3 88] [2 3 99] 4)
        g2 (weighted-digraph {1 {2 77 3 88} 2 {3 99} 4 []})
        g3 (weighted-digraph g1)
        g4 (weighted-digraph g3 (weighted-graph [5 6 88]) [7 8] 9)
        g5 (weighted-digraph)
        g6 (transpose g1)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (nodes g1))
           #{1 2 3 4} (set (nodes g6))
           #{[1 2] [1 3] [2 3]} (set (edges g1))
           #{[2 1] [3 1] [3 2]} (set (edges g6))
           (set (nodes g2)) (set (nodes g1))
           (set (edges g2)) (set (edges g1))
           (set (nodes g3)) (set (nodes g1))
           (set (nodes g3)) (set (nodes g1))
           #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
           #{[1 2] [1 3] [2 3] [5 6] [6 5] [7 8]} (set (edges g4))
           #{} (set (nodes g5))
           #{} (set (edges g5))
           true (has-node? g1 4)
           true (has-edge? g1 1 2)
           false (has-node? g1 5)
           false (has-edge? g1 2 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
           #{2 3} (set (successors g1 1))
           #{} (set (successors g1 3))
           #{} (set (successors g1 4))
           2 (out-degree g1 1)
           0 (out-degree g1 3)
           0 (out-degree g1 4)
           #{1 2} (set (predecessors g1 3))
           #{} (set (predecessors g1 1))
           2 (in-degree g1 3)
           0 (in-degree g1 1)
           #{1 2} (set (successors g6 3))
           #{} (set (successors g6 1))
           2 (out-degree g6 3)
           0 (out-degree g6 1)))
    (testing "Add & remove"
      (are [expected got] (= expected got)
           #{1 2 3 4 5} (set (nodes (add-nodes g1 5)))
           #{:a :b :c} (set (nodes (add-nodes g5 :a :b :c)))
           #{{:id 1} {:id 2}} (set (nodes (add-nodes g5 {:id 1} {:id 2})))
           #{[1 2]} (set (edges (add-edges g5 [1 2])))
           #{1 2} (set (nodes (remove-nodes g1 3 4)))
           #{[1 2]} (set (edges (remove-nodes g1 3 4)))
           #{1 2 3 4} (set (nodes (remove-edges g1 [1 2] [1 3])))
           #{[2 3]} (set (edges (remove-edges g1 [1 2] [1 3])))))
    (testing "Weight"
      (are [expected got] (= expected got)
           77 (weight g1 1 2)
           77 (weight g2 1 2)
           77 (weight g3 1 2)
           77 (weight g6 2 1)
           88 (weight g4 6 5)
           1 (weight g4 7 8)))))

(deftest fly-graph-test
  (let [fg1 (fly-graph :nodes [1 2 3]
                       :successors #(if (= 3 %) [1] [(inc %)])
                       :weight (constantly 88))
        fg2 (fly-graph :successors #(if (= 3 %) [1] [(inc %)])
                       :start 1)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
           #{1 2 3} (set (nodes fg1))
           #{1 2 3} (set (nodes fg2))
           #{[1 2] [2 3] [3 1]} (set (edges fg1))
           #{[1 2] [2 3] [3 1]} (set (edges fg2))
           88 (weight fg1 1 2)))
    ;; TODO: finish
    ))

(deftest utilities-test
  (testing "Predicates"
    (are [expected got] (= expected got)
         true (every? true? (map graph? [(graph [1 2])
                                         (digraph [1 2])
                                         (weighted-graph [1 2])
                                         (weighted-digraph [1 2])
                                         (fly-graph :successors [1 2])
                                         (reify Graph)]))
         true (every? true? (map directed? [(digraph [1 2])
                                            (weighted-digraph [1 2])
                                            (fly-graph :predecessors [1 2])
                                            (reify Digraph)]))
         true (every? true? (map weighted? [(weighted-graph [1 2])
                                            (weighted-digraph [1 2])
                                            (fly-graph :weight (constantly 1))
                                            (reify WeightedGraph)]))))
  (testing "Adders"
    (let [g (weighted-digraph [1 2] [2 3] [3 1])
          sg (subgraph g [1 2])
          pg (add-path (digraph) 1 2 3 4 5)
          cg (add-cycle (digraph) 1 2 3)]
      (are [expected got] (= expected got)
           #{1 2} (set (nodes sg))
           #{[1 2]} (set (edges sg))
           true (graph? sg)
           true (directed? sg)
           true (weighted? sg)
           #{[1 2] [2 3] [3 4] [4 5]} (set (edges pg))
           #{[1 2] [2 3] [3 1]} (set (edges cg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NamedGraph
;;
(deftest simple-rename-test
  (let [g (graph [1 2] [2 3] [3 4] [3 5])
        dg (digraph [1 2] [2 3] [3 4] [3 5])]
    (testing "Simple node renaming on graphs and digraphs"
      (are [expected got] (= expected got)
           (graph [1 2] [2 3] [3 4] [3 5]) (rename-nodes g {5 4 4 5})
           (digraph [1 2] [2 3] [3 4]) (rename-nodes dg {5 4})
           (graph [2 5] [5 1] [1 3] [1 4]) (rename-nodes g {1 2 2 5 5 4 4 3 3 1})
           (digraph [2 5] [5 1] [1 3] [1 4]) (rename-nodes dg {1 2 2 5 5 4 4 3 3 1})))
    (testing "Node-merge inducing renamings on graphs and digraphs"
      (are [expected got] (= expected got)
           (graph [1 2] [2 3] [3 4]) (rename-nodes g {5 4})
           (digraph [1 2] [2 3] [3 4]) (rename-nodes dg {5 4})
           (graph [1 2] [2 3] [3 1]) (rename-nodes g {4 1 5 1})
           (digraph [1 2] [2 3] [3 1]) (rename-nodes dg {4 1 5 1})))))

(def ^:dynamic *effort* 1000)

;; gen(erator) of strictly positive integers
(def pos-gen (gen/fmap inc gen/nat))

;; gen-maker for ints [0, n-1]
(defn lt-gen-maker [n] (gen/choose 0 (dec n)))

(def pos-pair-gen (gen/tuple pos-gen pos-gen))

;; gen-maker of random graphs
(defn graph-gen-maker [gc t]
  "Expects t to be a stream."
  (gen/fmap #(gg/gen-rand (gc) (get % 0) (get % 1)) t))

;; gen-maker of fixed-length tuples with elements from elem-gen stream
(defn len-tup-gen-maker [elem-gen l]
  (gen/return (gen/sample elem-gen l)))

;; gen-maker of a random map [0, t@0-1] -> [0, t@0-1]
(defn rename-map-maker [t]
  "Expects a tuple/vector, not a stream."
  (gen/fmap #(apply hash-map %)
            (len-tup-gen-maker (lt-gen-maker (get t 0)) (* 2 (get t 0)))))

;; sync for graph and renamer generators
(defn graph-rename-scenario [grc]
  (gen/bind pos-pair-gen
            (fn [p] (gen/tuple (graph-gen-maker grc (gen/return p))
                               (rename-map-maker p)))))

(def rename-nodes-impls-graph
  ;; rename-nodes-alt is much simpler
  ;; and we test two implementations against each other
  (prop/for-all [[g rnm] (graph-rename-scenario graph)]
                (=
                 (rename-nodes-alt g rnm)
                 (rename-nodes g rnm))))

(def rename-nodes-impls-digraph
  ;; rename-nodes-alt is much simpler
  ;; and we test two implementations against each other
  (prop/for-all [[g rnm] (graph-rename-scenario digraph)]
                (=
                 (rename-nodes-alt g rnm)
                 (rename-nodes g rnm))))

(tc/quick-check *effort* rename-nodes-impls-digraph)

(deftest generative-test-rename-nodes-x
  (is (:result (tc/quick-check *effort* rename-nodes-impls-graph)))
  (is (:result (tc/quick-check *effort* rename-nodes-impls-digraph))))

;;;;;;
;;
;; generator patterns
;;
;; *NOTE*: it's really about streams of vaues vs values
;;

(defn gen-maker [op s]
  (gen/fmap #(op %) s))

(def quad-gen (gen-maker #(* % %) gen/nat))
(def cube-gen (gen-maker #(* % % %) gen/nat))

(def synced-quad-cube-gen
  (gen/bind gen/nat
            (fn [t] (gen/tuple (gen-maker #(* % %) (gen/return t))
                               (gen-maker #(* % % %) (gen/return t))))))
