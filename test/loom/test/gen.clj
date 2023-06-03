(ns loom.test.gen
  (:require [clojure.test :refer (deftest testing is are)]
            [loom.graph :refer (graph digraph weighted-graph weighted-digraph graph? nodes edges)]
            [loom.alg :refer (clustering-coefficient)]
            [loom.gen :refer (gen-circle gen-newman-watts gen-barabasi-albert)]))

(deftest gen-barabasi-albert-test
  (let [g (graph)
        percentage (fn [num percent]
                     (* (/ percent 100) num))
        expected-edge-count (fn [num-nodes]
                              (+ 2 (* 2 (dec num-nodes))))]
    (testing "Construction"
      (are [graphs] loom.graph/graph?
        (gen-barabasi-albert g 10 1)
        (gen-barabasi-albert g 20 2)
        (gen-barabasi-albert g 42 5)))
    (testing "Node Count"
      ;; Because creating the graph involves probabilistic decisions
      ;; the actual number of nodes may be a bit lower than the expected count
      (are [num-nodes degree] (< (percentage num-nodes 95) (count (nodes (gen-barabasi-albert g num-nodes degree))))
        200 3
        100 1
        567 7
        980 20))
    (testing "Edge Count"
      ;; same problem as with node count
      (are [num-nodes degree] (< (percentage (expected-edge-count num-nodes) 95) (count (edges (gen-barabasi-albert g num-nodes degree))))
        200 3
        100 1
        567 7
        980 20))))