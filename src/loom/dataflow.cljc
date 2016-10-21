(ns ^{:doc "Dataflow analysis framework"
      :author "Aysylu Greenberg"}
  loom.dataflow
  (:require [loom.graph :as g]))

(defn dataflow-analysis
  "Performs dataflow analysis using iterative worklist-based algorithm.
   Must provide the graph and its start node, join and transfer functions."
  [{:keys [start graph join transfer]}]
  (let [start (cond
                (set? start) start
                (and (coll? start) (not (map? start))) (set start)
                :else #{start})]
    (loop [out-values {}
           queue (into
                   #?(:clj  clojure.lang.PersistentQueue/EMPTY
                      :cljs #queue [])
                   start)]
      (let [node (peek queue)
            worklist (pop queue)
            in-value (join (mapv out-values (g/predecessors graph node)))
            out (transfer node in-value)
            update? (not= out (get out-values node))
            out-values (if update?
                         (assoc out-values node out)
                         out-values)
            workset (set worklist)
            worklist (if update?
                       (->> (g/successors graph node)
                            (remove workset)
                            (into worklist)))]
        (if (seq worklist)
          (recur out-values worklist)
          out-values)))))
