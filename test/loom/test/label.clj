(ns loom.test.label
  (:use [loom graph label])
  (:use [clojure.test]))

(deftest labeled-graph-test
  (let [g (digraph [1 2] [2 3] [2 4] [3 5] [4 5])
        lg1 (-> g
               (add-label 1 "node label")
               (add-label 2 3 "edge label"))
        lg2 (-> (digraph)
                (add-labeled-nodes
                 1 "node label 1"
                 2 "node label 2")
                (add-labeled-edges
                 [1 2] "edge label 1"
                 [2 3] "edge label 2"))]
    (is (= "node label" (label lg1 1)))
    (is (= "edge label" (label lg1 2 3)))
    (is (= #{1 2 3} (set (nodes lg2))))
    (is (= #{[1 2] [2 3]} (set (edges lg2))))
    (is (= "node label 1") (label lg2 1))
    (is (= "node label 2") (label lg2 2))
    (is (= "edge label 1") (label lg2 1 2))
    (is (= "edge label 2") (label lg2 2 3))))