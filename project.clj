(defproject aysylu/loom "0.6.1-SNAPSHOT"
  :min-lein-version "2.0.0"
  :description "Graph library for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [tailrecursion/cljs-priority-map "1.1.0"]]
  :url "https://github.com/aysylu/loom"
  :test-selectors {:default (fn [m] (not (:test-check-slow m)))
                   :all (constantly true)
                   :test-check-slow :test-check-slow}
  
  :profiles {:dev [:cljs
                   {:dependencies [[lein-doo "0.1.7"]
                                   [org.clojure/test.check "0.9.0"]
                                   [com.cemerick/piggieback "0.2.1"]]
                    :repl-options {:init (set! *print-length* 50)
                                   :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}]

             :cljs {:dependencies [[org.clojure/clojurescript "1.9.89"]]
                    :plugins [[lein-cljsbuild "1.1.3" :exclusions [org.clojure/clojure]]]
                    :cljsbuild {:builds
                                [{:id "min"
                                  :source-paths ["src"]
                                  :compiler {:output-to "target/classes/public/js/viewer.js"
                                             :optimizations :advanced
                                             :pretty-print false}}]}}} 
  
  :aliases {"test" ["do" "clean," "test" ":all"]
            "release" ["do" "clean," "with-profile" "default" "deploy" "clojars"]}

  :plugins  [[codox "0.8.12"]]
  :codox  {:src-dir-uri "https://github.com/aysylu/loom/blob/master/"
           :src-linenum-anchor-prefix "L"
           :exclude loom.multigraph})
