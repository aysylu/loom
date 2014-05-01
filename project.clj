(defproject aysylu/loom "0.4.3-SNAPSHOT"
  :description "Graph library for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10."}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.priority-map "0.0.5"]]
  :url "https://github.com/aysylu/loom"
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.5.1"]]}}
  :aliases {"release" ["do" "clean," "with-profile" "default" "deploy" "clojars"]})
