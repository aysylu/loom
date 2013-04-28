(ns ^{:doc "Defines an interface between Titanium and loom."
      :author "Aysylu"}
  loom.titanium
  (:require [clojurewerkz.titanium.graph :as tg])
  (:require [clojurewerkz.titanium.elements :as te])
  (:require [clojurewerkz.titanium.edges :as edges])
  (:require [clojurewerkz.titanium.vertices :as nodes])
  (:require loom.io)
  (:use [loom.graph]))

(defn titanium->loom
  "Converts titalalalsdklaga;lkjdgl;kaj"
  [titanium-graph]
  (reify
    Graph
    ; implement graph methods
    (nodes [_] (nodes/get-all-vertices))
    (edges [_] (map (juxt edges/head-vertex edges/tail-vertex) (edges/get-all-edges)))
    (has-node? [g node] (some #(= % node) (nodes g)))
    (has-edge? [g n1 n2] (some #(= % [n1 n2]) (edges g)))
    (neighbors [g] (partial neighbors g))
    (neighbors [g node] (nodes/connected-out-vertices node []))
    (degree [g node] (count (neighbors g node)))
    Digraph
    ; digraph methods
    (incoming [g] (partial incoming g))
    (incoming [g node] (nodes/connected-in-vertices node []))
    (in-degree [g node] (count (neighbors g node)))
    ))

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
                    (edges/edges-between n2 n1)))))

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
