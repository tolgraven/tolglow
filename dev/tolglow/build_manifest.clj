(ns tolglow.build-manifest
  "Build configuration derived from canonical dependency data in both checkouts.
  Reading the paired Lein manifest does not evaluate its code or plugins."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn- qualified-lib [lib]
  (if (namespace lib) lib (symbol (name lib) (name lib))))

(defn- project-data [file]
  (with-open [reader (java.io.PushbackReader. (io/reader file))]
    (let [form (binding [*read-eval* false] (read reader))]
      (when-not (= 'defproject (first form))
        (throw (ex-info "Paired project must declare defproject" {:file (str file)})))
      (apply hash-map (drop 3 form)))))

(defn- coordinate [lib {:keys [mvn/version exclusions] :as spec}]
  (when-not (string? version)
    (throw (ex-info "Lein build needs a Maven version or expanded local checkout"
                    {:library lib :coordinate spec})))
  (cond-> [(qualified-lib lib) version]
    (seq exclusions) (into [:exclusions (mapv qualified-lib exclusions)])))

(defn manifest
  "Derive the complete JVM project from deps.edn and its paired Afterglow source.
  Local fork paths remain explicit, so no stale installed Afterglow jar can win."
  [root]
  (let [root (io/file root)
        deps (edn/read-string (slurp (io/file root "deps.edn")))
        fork-path (or (System/getenv "AFTERGLOW_ROOT")
                      (get-in deps [:deps 'tolgraven/afterglow :local/root]))
        _ (when-not (string? fork-path)
            (throw (ex-info "A paired Afterglow checkout is required" {})))
        fork (if (.isAbsolute (io/file fork-path)) (io/file fork-path) (io/file root fork-path))
        paired (project-data (io/file fork "project.clj"))
        paired-deps (into {} (map (fn [[lib & spec]] [(qualified-lib lib) (into [(qualified-lib lib)] spec)]))
                          (:dependencies paired))
        direct-deps (into {} (for [[lib spec] (:deps deps) :when (not (:local/root spec))]
                              [(qualified-lib lib) (coordinate lib spec)]))
        paths (:paths deps)
        resources (vec (filter #(= "resources" %) paths))
        source (vec (remove #(= "resources" %) paths))]
    {:dependencies (mapv val (sort-by (comp str key) (merge paired-deps direct-deps)))
     :source-paths (conj source (str (io/file fork-path "src")))
     :resource-paths (conj resources (str (io/file fork-path "resources")))
     :repositories (merge (:repositories paired)
                          (into {} (for [[id spec] (:mvn/repos deps)] [id (:url spec)])))}))
