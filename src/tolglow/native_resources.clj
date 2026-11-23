(ns tolglow.native-resources
  "Serve compiled Studio assets from GraalVM Native Image’s resource: URLs."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [ring.util.response :as response])
  (:import [java.net URL]))

(def ^:private assets
  (delay
    (with-open [input (io/reader (io/resource "tolglow/native-assets.edn"))]
      (edn/read (java.io.PushbackReader. input)))))

(defn asset-data [index ^URL url]
  (let [path (str/replace-first (.getPath url) #"^/" "")]
    (when-some [length (get index path)]
      {:content (.openStream url) :content-length length})))

(defmethod response/resource-data :resource [url]
  (asset-data @assets url))
