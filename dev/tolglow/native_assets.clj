(ns tolglow.native-assets
  "Index the public files present in the paired Studio release JAR."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.util.jar JarFile JarEntry]))

(def ^:private prefixes ["studio/" "gpu/" "js/compute/"])

(defn- public-file [^JarEntry entry]
  (let [path (.getName entry)]
    (when (and (not (.isDirectory entry))
               (some #(str/starts-with? path %) prefixes))
      [path (.getSize entry)])))

(defn -main [jar-path destination]
  (with-open [jar (JarFile. ^String jar-path)]
    (let [files (into (sorted-map) (keep public-file) (enumeration-seq (.entries jar)))]
      (doseq [required ["studio/index.html" "studio/js/main.js"]]
        (when-not (contains? files required)
          (throw (ex-info "Studio release is missing a compiled asset" {:path required}))))
      (io/make-parents destination)
      (spit destination (pr-str files)))))
