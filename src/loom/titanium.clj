(ns ^{:doc "Defines an interface between Titanium and loom."
      :author "Aysylu"}
  loom.titanium
  (:require [clojurewerkz.titanium.graph :as tg])
  (:require [clojurewerkz.titanium.elements :as te])
  (:require [clojurewerkz.titanium.edges :as edges])
  (:require [clojurewerkz.titanium.vertices :as nodes])
  (:use [loom.graph]))

(defn titanium->loom
  "Converts titalalalsdklaga;lkjdgl;kaj"
  [titanium-graph]
  (reify
    Graph
    ; implement graph methods
    (nodes [_] (tg/get-vertices titanium-graph))
    (edges [_] (map (juxt edges/head-vertex edges/tail-vertex) (tg/get-edges titanium-graph)))
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

(def dg (digraph [1 2] [2 3] {3 [4 5] 5 [6 7]} 7 8 9))
(degree dg 5)
(require 'loom.io)
(let [g (tg/open-in-memory-graph)
            v1 (tg/add-vertex g {:age 28 :name "Michael"})
            v2 (tg/add-vertex g {:age 26 :name "Alex"})
            e  (tg/add-edge g v1 v2 "friend" {:since 2008})
            xs (set (tg/get-edges g))]
      (loom.io/view (titanium->loom g)))
(let [g  (tg/open-in-memory-graph)
            v1 (tg/add-vertex g {:age 28 :name "Michael"})
            v2 (tg/add-vertex g {:age 26 :name "Alex"})
            xs (set (tg/get-vertices g))]
      (println xs))
