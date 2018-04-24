(ns loom.test.gen
  (:require [clojure.test :refer (deftest testing is)]
            [loom.gen :refer (gen-circle gen-newman-watts)]))

(deftest build-circle-test
         (let [g1 (gen-circle (loom.graph/graph) 5 1)
               g2 (gen-circle (loom.graph/graph) 20 3)
               g3 (gen-circle (loom.graph/digraph) 25 2)
               g4 (gen-circle (loom.graph/weighted-graph {0 {1 42 2 42} 1 {2 42 3 42} 2 {3 42 4 42}}) 10 1)
               g5 (gen-circle (loom.graph/weighted-graph) 10 1)
               g6 (gen-circle (loom.graph/weighted-digraph [0 1 42] [1 2 42] [2 3 42] [1 0 43] [2 1 43] [3 2 43]) 10 1)
               g7 (gen-circle (loom.graph/weighted-digraph) 10 1)
               ]
           (testing "Generating circle-graphs from the different graph types"
             (is (= (into #{} (range 5)) (loom.graph/nodes g1)))
             (is (= (into #{} (range 20)) (loom.graph/nodes g2)))
             (is (= (into #{} (range 25)) (loom.graph/nodes g3)))
             (is (= (into #{} (range 10)) (loom.graph/nodes g4)))
             (is (= (into #{} (range 10)) (loom.graph/nodes g5)))
             (is (= (into #{} (range 10)) (loom.graph/nodes g6)))
             (is (= (into #{} (range 10)) (loom.graph/nodes g7))))))
