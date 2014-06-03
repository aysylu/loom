(ns ^{:doc "Output and view graphs in various formats"
      :author "Justin Kramer"}
  loom.io
  (:require [loom.graph :refer [directed? weighted? nodes edges weight]]
            [loom.alg :refer [distinct-edges loners]]
            [loom.attr :refer [attr? attr attrs]]
            [clojure.string :refer [escape]]
            [clojure.java.io :refer [file as-file]]
            [clojure.java.shell :refer [sh]])
  (:import (java.io File FileWriter
                    FileOutputStream)))

(defn- dot-esc
  [s]
  (escape s {\" "\\\"" \newline "\\n"}))

(defn- dot-attrs
  [attrs]
  (when (seq attrs)
    (let [sb (StringBuilder. "[")]
      (doseq [[k v] attrs]
        (when (pos? (.length (str v)))
          (when (< 1 (.length sb))
            (.append sb \,))
          (doto sb
            (.append \")
            (.append (dot-esc (if (keyword? k) (name k) (str k))))
            (.append "\"=\"")
            (.append (dot-esc (if (keyword? v) (name v) (str v))))
            (.append \"))))
      (.append sb "]")
      (str sb))))

(defn dot-str
  "Render graph g as a DOT-format string. Calls (node-label node) and
  (edge-label n1 n2) to determine what labels to use for nodes and edges,
  if any. Will detect graphs that satisfy AttrGraph and include attributes,
  too."
  [g & {:keys [graph-name node-label edge-label]
        :or {graph-name "graph"} :as opts }]
  (let [node-label (if node-label node-label
                       (if (attr? g)
                         #(attr g % :label) (constantly nil)))
        edge-label (if edge-label edge-label
                       (if (attr? g)
                         #(attr g %1 %2 :label) (constantly nil)))
        d? (directed? g)
        w? (weighted? g)
        sb (doto (StringBuilder.
                  (if d? "digraph \"" "graph \""))
             (.append (dot-esc graph-name))
             (.append "\" {\n"))]
    (when (:graph opts)
      (doto sb
        (.append "  graph ")
        (.append (dot-attrs (:graph opts)))))
    (doseq [[n1 n2] (distinct-edges g)]
      (let [n1l (str (or (node-label n1) n1))
            n2l (str (or (node-label n2) n2))
            el (if w? (weight g n1 n2) (edge-label n1 n2))
            eattrs (assoc (if (attr? g)
                            (attrs g n1 n2) {})
                     :label el)]
        (doto sb
          (.append "  \"")
          (.append (dot-esc n1l))
          (.append (if d? "\" -> \"" "\" -- \""))
          (.append (dot-esc n2l))
          (.append \"))
        (when (or (:label eattrs) (< 1 (count eattrs)))
          (.append sb \space)
          (.append sb (dot-attrs eattrs)))
        (.append sb "\n")))
    (doseq [n (nodes g)]
      (doto sb
        (.append "  \"")
        (.append (dot-esc (str (or (node-label n) n))))
        (.append \"))
      (when-let [nattrs (when (attr? g)
                          (dot-attrs (attrs g n)))]
        (.append sb \space)
        (.append sb nattrs))
      (.append sb "\n"))
    (str (doto sb (.append "}")))))

(defn dot
  "Writes graph g to f (string or File) in DOT format. args passed to dot-str"
  [g f & args]
  (spit (str (file f)) (apply dot-str g args)))

(defn- os
  "Returns :win, :mac, :unix, or nil"
  []
  (condp
      #(<= 0 (.indexOf ^String %2 ^String %1))
      (.toLowerCase (System/getProperty "os.name"))
    "win" :win
    "mac" :mac
    "nix" :unix
    "nux" :unix
    nil))

(defn- open
  "Open the given file (a string, File, or file URI) in the default
  application for the current desktop environment. Returns nil"
  [f]
  (let [f (file f)]
    ;; There's an 'open' method in java.awt.Desktop but it hangs on Windows
    ;; using Clojure Box and turns the process into a GUI process on Max OS X.
    ;; Maybe it's ok for Linux?
    (do
      (condp = (os)
        :mac (sh "open" (str f))
        :win (sh "cmd" (str "/c start " (-> f .toURI .toURL str)))
        :unix (sh "xdg-open" (str f)))
      nil)))

(defn- open-data
  "Write the given data (string or bytes) to a temporary file with the
  given extension (string or keyword, with or without the dot) and then open
  it in the default application for that extension in the current desktop
  environment. Returns nil"
  [data ext]
  (let [ext (name ext)
        ext (if (= \. (first ext)) ext (str \. ext))
        tmp (java.io.File/createTempFile (subs ext 1) ext)]
    (if (string? data)
      (with-open [w (java.io.FileWriter. tmp)]
        (.write w ^String data))
      (with-open [w (java.io.FileOutputStream. tmp)]
        (.write w ^bytes data)))
    (.deleteOnExit tmp)
    (open tmp)))

(defn render-to-bytes
  "Renders the graph g in the PNG format using GraphViz and returns PNG data
  as a byte array.
  Requires GraphViz's 'dot' (or a specified algorithm) to be installed in
  the shell's path. Possible algorithms include :dot, :neato, :fdp, :sfdp,
  :twopi, and :circo"
  [g & {:keys [alg] :or {alg "dot"} :as opts}]
  (let [dot (apply dot-str g (apply concat opts))
        {png :out} (sh (name alg) "-Tpng" :in dot :out-enc :bytes)]
    png))

(defn render-to-file
  "Renders the graph g in the PNG format using GraphViz and saves the
  the image as a file. Returns the java.io.File object.
  Requires GraphViz's 'dot' (or a specified algorithm) to be installed in
  the shell's path. Possible algorithms include :dot, :neato, :fdp, :sfdp,
  :twopi, and :circo"
  [g f & {:keys [alg] :or {alg "dot"} :as opts}]
  (let [dot (apply dot-str g (apply concat opts))
        fname (.getAbsolutePath (as-file f))
        {png :out} (sh (name alg) "-Tpng" (str "-o" fname) :in dot :out-enc :bytes)]
    png))

(defn view
  "Converts graph g to a temporary PNG file using GraphViz and opens it
  in the current desktop environment's default viewer for PNG files.
  Requires GraphViz's 'dot' (or a specified algorithm) to be installed in
  the shell's path. Possible algorithms include :dot, :neato, :fdp, :sfdp,
  :twopi, and :circo"
  [g & opts]
    (open-data (apply render-to-bytes g opts) :png))
