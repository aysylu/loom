(ns loom.test.derived
  (:require [loom.derived :refer :all]
            [loom.graph :refer :all]
            [loom.alg :refer :all]
            [clojure.test :refer :all]))

(deftest derived-graph-test
  (let [g  (graph [1 2] [1 3] [2 3] 4)
        dg (digraph [1 2] [1 3] [2 3] 4)]
    (testing "mapped-by"
      (are [expected got] (= expected got)
        true (eql? g
                   (mapped-by identity g))
        true (eql? (graph [2 3] [2 4] [3 4] 5)
                   (mapped-by inc g))
        true (eql? (graph [2 0] [2 1] [0 1] 2)
                   (mapped-by #(mod % 3) g))
           ;; digraph
        true (eql? dg
                   (mapped-by identity dg))
        true (eql? (digraph [2 3] [2 4] [3 4] 5)
                   (mapped-by inc dg))
        true (eql? (digraph [2 0] [1 2] [1 0])
                   (mapped-by #(mod % 3) dg))))
    (testing "nodes filtered"
      (are [expected got] (= expected got)
        true (eql? (graph)
                   (nodes-filtered-by #{} g))
        true (eql? (graph [1 2] 4)
                   (nodes-filtered-by #{1 2 4} g))

        true (eql? (digraph [1 2] 4)
                   (nodes-filtered-by #{1 2 4} dg))))

    (testing "subgraph from start node"
      (are [expected got] (= expected got)
        true (eql? (graph [1 2] [1 3] [2 3])
                   (subgraph-starting-from g 2))
        true (eql? (graph 4)
                   (subgraph-starting-from g 4))
        true (eql? (digraph [2 3])
                   (subgraph-starting-from dg 2))))))

(deftest border-subgraph-test
  (let [dg (digraph [1 2] [2 3] [4 5] [5 6] [3 4] [2 4] [1 6])
        ug (graph dg)]
   (testing "border subgraph"
     (are [expected got] (= expected got)
       '([1 6] [2 4] [3 4]) (sort (edges (border-subgraph dg [1 2 3])))
       '([1 6] [2 4] [3 4]) (sort (edges (border-subgraph ug [1 2 3])))

       '([5 6]) (sort (edges (border-subgraph dg [4 5])))
       true (eql? (graph [2 3] [2 4] [3 4] [5 6])
                  (border-subgraph ug [4 5]))))))

(comment
  (use 'loom.io)

  (defn cycle-n [n] (fly-graph :successors (fn [node] [(mod (inc node) n)])
                               :predecessors (fn [node] [(mod (dec node) n)])
                               :nodes (range n)))
  (view (cycle-n 5))
  (view (mapped-by #(* % %) (cycle-n 15)))

  (def g1 (graph [1 2] [1 3] [2 3] 4))
  (def g2 (digraph [1 2] [1 3] [2 3] 4))
  (view g2)
  (view (mapped-by inc g1))
  (view (mapped-by #(mod % 3) g1))
  (view (subgraph-starting-from g2 2))

  (def g (digraph [1 2] [2 3] [4 5] [5 6] [3 4] [2 4] [1 6]))
  (view (border-subgraph g [1 2 3]))
  )
