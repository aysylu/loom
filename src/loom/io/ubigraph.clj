(ns ^{:doc "API for Ubigraph"
      :author "Justin Kramer"}
  loom.io.ubigraph
  (:use [loom.graph :only [add-nodes* add-edges*]]
        [loom.alg-generic :only [trace-path]]
        [robert.hooke :only [add-hook]])
  (:import [org.apache.xmlrpc.client XmlRpcClient XmlRpcClientConfigImpl]
           [org.apache.xmlrpc XmlRpcException]))

(def ^{:private true} client
  (let [config (doto (XmlRpcClientConfigImpl.)
                 (.setServerURL (java.net.URL. "http://127.0.0.1:20738/RPC2")))]
    (doto (XmlRpcClient.)
      (.setConfig config))))

(defn call [op & args]
  "Executes a Ubigraph operation via XML-RPC"
  (try
    (.execute client (str "ubigraph." (name op)) args)
    (catch Exception e (throw (RuntimeException. e)))))

(def node->id (atom {}))
(def edge->id (atom {}))

(defn clear
  []
  (call :clear)
  (reset! node->id {})
  (reset! edge->id {}))

(defn hook
  "Hooks the Graph protocol functions into Ubigraph"
  []
  (letfn [(show-nodes [nodes]
            (doseq [n nodes]
              (when-not (@node->id n)
                (let [id (call :new_vertex)]
                  (swap! node->id assoc n (Integer. id))))))]
    (alter-var-root
     #'add-nodes*
     (fn [f]
       (fn [g nodes]
         (show-nodes nodes)
         (f g nodes))))
    (alter-var-root
     #'add-edges*
     (fn [f]
       (fn [g edges]
         (doseq [[n1 n2] edges]
           (when-not (@node->id n1)
             (show-nodes [n1]))
           (when-not (@node->id n2)
             (show-nodes [n2]))
           (call :new_edge (@node->id n1) (@node->id n2))
         (f g edges)))))))

