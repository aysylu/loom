(defproject aysylu/loom "0.5.1-SNAPSHOT"
  :description "Graph library for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]]
  :url "https://github.com/aysylu/loom"
  :test-selectors {:default (fn [m] (not (:test-check-slow m)))
                   :all (constantly true)
                   :test-check-slow :test-check-slow}
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.6.0"]
                             [org.clojure/test.check "0.5.7"]]}}
  :aliases {"release" ["do" "clean," "with-profile" "default" "deploy" "clojars"]})
