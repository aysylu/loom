(ns loom.test.io
    (:require [loom.graph :refer (digraph)]
              [loom.attr :refer (add-attr attr add-attr-to-nodes add-attr-to-edges)]
              [loom.io :refer (dot-str)]
              #?@(:clj [[clojure.test :refer :all]]))
    #?@(:cljs [(:require-macros [cljs.test :refer (deftest testing are is)])]))

(deftest support-html-label-test
  (let [g (digraph [1 2])
        lg1 (-> g
                (add-attr 1 :label "node1 label")
                (add-attr 2 :label "<node2 label>"))]
    (is (= "digraph \"graph\" {\n1392991556 -> -971005196\n1392991556 [label=\"node1 label\"] [\"label\"=\"node1 label\"]\n-971005196 [label=\"<node2 label>\"] [\"label\"=<node2 label>]\n}" (dot-str lg1)))))
