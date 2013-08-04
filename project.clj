(defproject heffalump/loom "0.3.0-SNAPSHOT"
  :description "Graph library for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [io.rkn/core.async "0.1.0-SNAPSHOT"]] 
  :profiles {:dev 
             {:dependencies [[org.clojure/clojure "1.5.1"]]}})
