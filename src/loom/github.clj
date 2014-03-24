(ns ^{:doc "Analyzes Clojure repositories on Github."
      :author "Aysylu Greenberg"}
  loom.github
  (:require [clojure.set :as set]
            [clojure.string :as str]
            loom.io
            clojure.pprint
            [loom.graph :as g]
            [loom.ssa :as ssa]))

(def project-file-dir
  (java.io.File. "/Users/aysylu/Downloads/project-files/"))

(def project-clj-files
    (->> (.listFiles project-file-dir)
         (remove #(-> % .getName (.startsWith "NOCONTENT")))))

(defn file->project [f]
  (try
    (let [edn (->> f
                   slurp
                   read-string
                   :content
                   read-string)]
      (cond
        (= (symbol "defproject") (first edn))
        (let [[_ name version & rest] edn]
          (apply hash-map :name name :version version rest))
        :else nil))
    (catch Exception ex nil)))

(defn add-to-adj-list
  [adj project]
  (let [deps (:dependencies project) ]
    (if (vector? deps)
      (try
        (assoc adj (:name project) (set (map first (:dependencies project))))
        (catch Exception e
          #_(clojure.pprint/pprint deps)
          adj
          )
        ) 
      (do (when-not (nil? deps)
            #_(clojure.pprint/pprint deps))
          adj))))

(def populated-graph
  (reduce
    (fn [adj file]
      (->> (file->project file)
           (add-to-adj-list adj)))
    {}
    project-clj-files))

(def loom-graph (loom.graph/digraph
                  (->> populated-graph
                       (seq)
                       (shuffle)
                       #_(take 5000)
                       (into {}))))
;(def loom-graph (loom.graph/digraph {'aysylu/loom #{'org.clojure/clojure} 'aysylu/titanium-loom #{'org.clojure/clojure 'aysylu/loom}}))

(defn join [in-values]
  (reduce into #{} in-values))

(def analysis
  (loom.ssa/dataflow-analysis
    :start (->> loom-graph
                (loom.graph/nodes)
                (filter #(zero? (loom.graph/in-degree loom-graph %)))
                (filter #(contains? (loom.graph/successors loom-graph %) 'com.datomic/datomic-free))
                )
    :graph loom-graph
    :join join
    :transfer (fn [node in-value] (if (set? in-value) (conj in-value node) #{node})))
  )

(def results
  (->> analysis
       (map (fn [[lib libs]]
             [lib (count libs)]))
       (sort-by second)
       (reverse)))
