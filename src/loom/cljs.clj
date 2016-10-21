(ns loom.cljs
  (:refer-clojure :exclude (extend)))

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


