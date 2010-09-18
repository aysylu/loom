(defproject loom "0.1.0-SNAPSHOT"
  :description "Graph library for Clojure"
  :author "Justin Kramer"
  :dependencies [[org.clojure/clojure "1.3.0-master-SNAPSHOT"]
                 [org.apache.xmlrpc/xmlrpc-client "3.1.3"]
                 [robert/hooke "1.0.2"]
                 ;[org.clojure.contrib/priority-map "1.3.0-SNAPSHOT"]
                 ;[vijual "0.1.0-SNAPSHOT"]
                 ]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]]
  :jvm-opts ["-Xmx1g"])
