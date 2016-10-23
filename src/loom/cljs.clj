(ns loom.cljs
  (:refer-clojure :exclude (extend)))

;; What's going on here?
;; 
;; Loom uses `extend` extensively to provide protocol implementations for its
;; various graph types. ClojureScript does not offer `extend` (only
;; `extend-type` and `extend-protocol` are available). I did not want to
;; refactor all the graph type definitions to use `extend-type` (either to
;; duplicate all the protocol method impls, or delegate to the shared protocol
;; fns that could be provided to `extend` in Clojure). Further, the protocol
;; method impl maps in `loom.graph` are public, so I didn't want to change them
;; from the perspective of a Clojure consumer (which might base other graph
;; impls on those "base" maps).
;; 
;; This namespace is my hack/solution. Protocol method impl maps are now defined
;; using `def-protocol-impls`, which behaves just as `def` in Clojure. But in
;; ClojureScript, `def-protocol-impls` stores the provided map. An
;; "implementation" of `extend` for ClojureScript is also provided here, which
;; looks up those previously-defined maps, and uses their contents as the basis
;; for an equivalent `extend-type` form.
;; 
;; There are various aspects of this that are wince-inducing, including the
;; questionable-but-working resolution of `extend` map symbols, and the ad-hoc
;; pattern matching and "application" on the ClojureScript side of certain
;; functions that _loom_ uses to manipulate `extend` protocol method impl maps
;; (`get-in`, `merge`). While such things are completely fine in Clojure, if
;; later changes to loom manipulate those maps using other functions, the code
;; below will need to be changed to accommodate them.
;; 
;; For all sorts of reasons, this is _not_ a general-purpose `extend`
;; replacement, and so ClojureScript consumers of Loom will not be able to
;; e.g. reliably reuse base protocol method impl maps as one can in Clojure.
;; 
;; - Chas

(def ^:private protocol-impls (atom {}))

(defn- resolve-symbol [ns sym]
  (-> ns :name str (symbol (str sym))))

(defmacro def-protocol-impls [name impl-map]
  (if-let [ns (:ns &env)]
    (let [impl-map (reduce
                    (fn [impls [method impl]]
                      (case (first impl)
                        fn (assoc impls method impl)
                        get-in (let [[_ other-impl-map-name path] impl
                                      other-impl-map (@protocol-impls
                                                      (resolve-symbol ns other-impl-map-name))]
                                  (assoc impls method (get-in other-impl-map path)))))
                    {}
                    impl-map)]
      (swap! protocol-impls assoc (resolve-symbol ns name) impl-map)
      nil)
    `(def ~name ~impl-map)))

(defn- resolve-impl-map [env imap]
  (cond
    (map? imap) imap

    (and (seq? imap) (= 'merge (first imap)))
    (apply merge (map #(resolve-impl-map env %) (rest imap)))

    (symbol? imap)
    (@protocol-impls
     (if (namespace imap)
       imap
       (resolve-symbol (:ns env) imap)))
    :default
    (throw (ex-info "Unsupported `extend` impl map"
                    {:impl-map imap}))))

(defmacro extend [type & protocols+impls]
  `(extend-type ~type
         ~@(reduce
             (fn [impls [protocol imap]]
               (let [impl-map (resolve-impl-map &env imap)]
                 (-> (conj impls protocol)
                     (into (map (fn [[method [_ & arities]]]
                                  (cons (symbol (name method))
                                        arities))
                                impl-map)))))
             []
             (partition 2 protocols+impls))))


