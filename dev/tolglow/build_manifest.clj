(ns tolglow.build-manifest
  "Read the sibling Afterglow project's dependencies without running its plugins."
  (:require [clojure.java.io :as io]))

(defn fork-path []
  (.getCanonicalPath (io/file (or (not-empty (System/getenv "AFTERGLOW_ROOT")) "../afterglow"))))

(defn paired-project [root]
  (let [path (io/file (fork-path))
        fork (if (.isAbsolute path) path (io/file root path))
        file (io/file fork "project.clj")]
    (with-open [reader (java.io.PushbackReader. (io/reader file))]
      (let [form (binding [*read-eval* false] (read reader))]
        (when-not (= 'defproject (first form))
          (throw (ex-info "Expected the paired Afterglow Leiningen project" {:file (str file)})))
        (apply hash-map (drop 3 form))))))

(defn dependencies [app-deps]
  (let [dep-key (fn [[lib]] (if (namespace lib) lib (symbol (name lib) (name lib))))]
    (->> (concat (:dependencies (paired-project ".")) app-deps)
         (reduce #(assoc %1 (dep-key %2) %2) (sorted-map))
         vals vec)))

(defn fork-source [path] (str (io/file (fork-path) path)))
