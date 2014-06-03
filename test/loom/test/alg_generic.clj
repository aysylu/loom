(ns loom.test.alg-generic
  (:require [loom.alg-generic :as lag]
            [clojure.set :as set]
            [clojure.test :refer [deftest are]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]))

(defn dag-samples-gen
  [dag percent]
  (let [dag-size (count dag)
        sample-count (int (* percent dag-size))]
    (gen/bind (apply gen/tuple
                     ;; May collide but this is fine.
                     (repeat (* 2 sample-count) (gen/choose 0 dag-size)))
              (fn [samples]
                (gen/tuple (gen/return dag) (gen/return samples))))))

(defn gen-dag
  ([] (gen-dag [#{}] 10))
  ([nodes] (gen-dag [#{}] nodes))
  ([dag-so-far nodes]
     (gen/bind (gen/frequency [[80 (gen/return 1)]
                               [19 (gen/return 2)]
                               [1 (gen/return 0)]])
               (fn [parent-count]
                 (gen/bind (gen/such-that
                            (fn [& parents]
                              (when (not (empty? parents))
                                (apply distinct? parents)))
                            (apply gen/tuple
                                   (repeat (min (count dag-so-far) parent-count)
                                           (gen/choose 0 (dec (count dag-so-far))))))
                           (fn [parents]
                             (if (< 0 nodes)
                               (gen-dag (conj dag-so-far (set parents))
                                        (dec nodes))
                               (dag-samples-gen dag-so-far 1/2))))))))


(defn anc-model-new [] {})

(defn anc-model-add
  [anc-model node & parents]
  (let [ancs (reduce set/union
                     (map #(get anc-model %)
                          parents))
        ancs (into ancs parents)
        ancs (disj ancs nil)]
    (assoc anc-model node ancs)))

(defn anc-model-anc?
  [anc-model childer parenter]
  (boolean
   (get
    (get anc-model childer)
    parenter)))

(defn anc->anc-model
  [ancestry]
  (let [anc-nodes (lag/ancestry-nodes ancestry)]
    (zipmap anc-nodes
            (map #(set (lag/ancestors ancestry %)) anc-nodes))))

(def dag-similarity-props
  (prop/for-all [[dag samples] (gen/bind (gen/choose 0 100)
                                         (fn [dag-size]
                                           (gen-dag dag-size)))]
                (let [anc (reduce (fn [a [i ps]]
                                    (apply lag/ancestry-add a i (seq ps)))
                                  (lag/ancestry-new)
                                  (map-indexed vector dag))
                      anc-model (reduce (fn [a [i ps]]
                                          (apply anc-model-add a i (seq ps)))
                                        (anc-model-new)
                                        (map-indexed vector dag))
                      samp-pairs (partition 2 samples)
                      anc-to-model (anc->anc-model anc)]
                  (and
                   (= anc-model anc-to-model)
                   (every?
                    (fn [[a b]]
                      (and
                       (= (lag/ancestor? anc b a)
                          (anc-model-anc? anc-model b a))
                       (= (lag/ancestor? anc a b)
                          (anc-model-anc? anc-model a b))))
                    samp-pairs)))))

(defspec ^:test-check-fast dag-similarity-100
  100
  dag-similarity-props)

(defspec ^:test-check-slow dag-similarity-2000
  2000
  dag-similarity-props)

(def g1
  {:a [:b :c]
   :b [:d]
   :c [:d]
   :d nil})

(def g2
  {:a [:b]
   :b [:a]})

(def g3
  {:a [:b]
   :b [:a :c :d]
   :c [:b :e]
   :d [:b :c :e]
   :e [:c :d :f]
   :f []})

(def g4 ; like g3 with some loops
  {:a [:b]
   :b [:a :c :d]
   :c [:b :c :e]
   :d [:b :c :e]
   :e [:c :d :f]
   :f [:f]})

(def g5 ; like g1 but as an undirected graph
  {:a [:b :c]
   :b [:d :a]
   :c [:a :d]
   :d [:c :b]})

(deftest tracing-paths
  (are [g n p] (= (sort (lag/trace-paths g n)) p)
       {:a nil} :a
       [[:a]]

       {:a #{:b} :b nil} :a
       [[:a :b]]

       g1 :a
       [[:a :b :d] [:a :c :d]]))

(deftest bf-paths-bi-test
  (are [g start end paths] (= (lag/bf-paths-bi g g start end) paths)
       g2 :a :b
       [[:a :b]]

       g3 :a :c
       [[:a :b :c]]

       g3 :a :e
       [[:a :b :c :e] [:a :b :d :e]]))

(deftest edge-traverse
  (are [g start f expected] (= expected (f g start))
       g1 :a lag/pre-edge-traverse
       '([:a :b] [:b :d] [:a :c] [:c :d])
       
       g1 :a lag/post-edge-traverse
       '([:c :d] [:a :c] [:b :d] [:a :b])
       
       g1 :d lag/pre-edge-traverse
       nil
       
       g1 :d lag/post-edge-traverse
       ()
       
       g4 :a lag/pre-edge-traverse
       '([:a :b] [:b :a] [:b :c] [:c :b] [:c :c] [:c :e] [:e :c] [:e :d] [:d :b] [:d :c] [:d :e] [:e :f] [:f :f] [:b :d])
       
       g4 :a lag/post-edge-traverse
       '([:b :d] [:f :f] [:e :f] [:d :e] [:d :c] [:d :b] [:e :d] [:e :c] [:c :e] [:c :c] [:c :b] [:b :c] [:b :a] [:a :b])
       
       g4 :c lag/pre-edge-traverse
       '([:c :b] [:b :a] [:a :b] [:b :c] [:b :d] [:d :b] [:d :c] [:d :e] [:e :c] [:e :d] [:e :f] [:f :f] [:c :c] [:c :e])
       
       g4 :c lag/post-edge-traverse
       '([:c :e] [:c :c] [:f :f] [:e :f] [:e :d] [:e :c] [:d :e] [:d :c] [:d :b] [:b :d] [:b :c] [:a :b] [:b :a] [:c :b])
       
       g4 :f lag/pre-edge-traverse
       '([:f :f])
       
       g4 :f lag/post-edge-traverse
       '([:f :f])
       
       g5 :a lag/pre-edge-traverse
       '([:a :b] [:b :d] [:d :c] [:c :a] [:c :d] [:d :b] [:b :a] [:a :c])
       
       g5 :a lag/post-edge-traverse
       '([:a :c] [:b :a] [:d :b] [:c :d] [:c :a] [:d :c] [:b :d] [:a :b])
       ))
