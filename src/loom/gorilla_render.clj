(ns loom.gorilla-render
  (:require [gorilla-renderable.core :as render]
            [clojure.data.codec.base64 :as b64]
            [loom.io :as io]))

(defrecord LoomView [content opts])

(defn loom-view [content & opts] (LoomView. content opts))

(extend-type LoomView
  render/Renderable
  (render [self]
    (let [bytes (apply io/render-to-bytes (:content self) (:opts self))]
      {:type    :html
       :content (format "<img src=\"data:image/PNG;base64,%1$s\"/>" (String. (b64/encode bytes)))
       :value   (pr-str self)})))