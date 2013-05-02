(ns ^{:doc "Defines an interface between Titanium and loom."
      :author "Aysylu"}
  loom.titanium
  (:require [clojure.set :as set])
  (:require [clojurewerkz.titanium.graph :as tg])
  (:require [clojurewerkz.titanium.elements :as te])
  (:require [clojurewerkz.titanium.edges :as edges])
  (:require [clojurewerkz.titanium.vertices :as nodes])
  (:require loom.io)
  (:use [loom.graph]))

(defn titanium->loom
  "Converts titanium graph into loom representation"
  ([titanium-graph &
    {:keys [node-fn edge-fn]
     :or {node-fn (nodes/get-all-vertices)
          edge-fn (map (juxt edges/head-vertex
                             edges/tail-vertex)
                       (edges/get-all-edges))}}]
   (let [nodes (set node-fn)
         edges (set edge-fn)]
   (reify
     Graph
     (nodes [_] nodes)
     (edges [_] edges)
     (has-node? [g node] (contains? (nodes g) node))
     (has-edge? [g n1 n2] (contains? (edges g) [n1 n2]))
     (neighbors [g] (partial neighbors g))
     (neighbors [g node] (filter (nodes g) (nodes/connected-out-vertices node [])))
     (degree [g node] (count (neighbors g node)))
     Digraph
     (incoming [g] (partial incoming g))
     (incoming [g node] (filter (nodes g) (nodes/connected-in-vertices node [])))
     (in-degree [g node] (count (incoming g node)))))))

(defn view [g]
  (loom.io/view (if (satisfies? Graph g)
                  g
                  (titanium->loom g))
                :node-label nodes/to-map
                :edge-label
                (fn [n1 n2]
                  (println n1 n2)
                  (println (edges/edges-between n1 n2))
                  (println (edges/edges-between n2 n1))
                  (mapv edges/to-map
                    (edges/edges-between n1 n2)))))

(let [g (tg/open {"storage.backend" "inmemory"})]
  (tg/transact!
    (let [ 
          v1 (nodes/create! {:age 28 :name "Michael"})
          v2 (nodes/create! {:age 26 :name "Alex"})
          e (edges/connect!
              v1
              "friend"
              v2
              {:since 2008})]
      (view (titanium->loom g)))))
