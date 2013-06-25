(ns loom.test.flow
  (:use [loom.graph] :reload)
  (:use [loom.flow]
        [loom.alg :only [max-flow]]
        [clojure.test]))


;; Trivial case
(def g0
  (directed-graph
   [:s :t 100]))

;; From Cormen et al. Algorithms, 3 ed. p. 726-727
(def g1
  (directed-graph
   [:s :v1 16]
   [:s :v2 13]
   [:v1 :v3 12]
   [:v2 :v1 4]
   [:v2 :v4 14]
   [:v3 :v2 9]
   [:v3 :t 20]
   [:v4 :v3 7]
   [:v4 :t 4]))

;; Source and sink disconnected
(def g2
  (directed-graph
   [:s :a 5]
   [:b :t 10]))


(deftest edmonds-karp-test
  (are [max-value network] (let [[flow value] (edmonds-karp (partial neighbors network)
                                                            (partial direct-predecessors network)
                                                            (partial edge-weight network)
                                                            :s :t)]
                             (and (= max-value value)
                                  (is-admissible-flow? flow (partial edge-weight network)
                                                       :s :t)))
       23 g1
       100 g0
       0 g2))


(deftest max-flow-convenience-test
  (are [max-value network]
       (let [[flow value] (max-flow (directed-graph network) :s :t)]
         (and (= max-value value)
              (is-admissible-flow? flow (partial edge-weight network) :s :t)))
       23 g1))




