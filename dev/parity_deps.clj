;; Read dependency data, never evaluate the Leiningen project or its plugins.
;; Used by bin/test legacy; keeps the test classpath aligned with project.clj.
(require '[clojure.java.io :as io])

(defn qualified-lib [lib]
  (if (namespace lib) lib (symbol (name lib) (name lib))))

(let [[project-path source-root] *command-line-args*
      form (with-open [reader (java.io.PushbackReader. (io/reader project-path))]
             (binding [*read-eval* false] (read reader)))
      project (apply hash-map (drop 3 form))
      deps (into {}
                 (map (fn [[lib version & options]]
                        (let [options (apply hash-map options)]
                          [(qualified-lib lib)
                           (cond-> {:mvn/version version}
                             (:exclusions options)
                             (assoc :exclusions (mapv qualified-lib (:exclusions options))))])))
                 (:dependencies project))]
  ;; The historical Sonatype 8.0.0-SNAPSHOT is no longer resolvable. Tests
  ;; use its stable release; application project.clj is deliberately unchanged.
  (prn (cond-> {:deps (assoc-in deps ['org.deepsymmetry/beat-link :mvn/version] "8.0.0")
                :mvn/repos (into {} (map (fn [[id url]] [id {:url url}]))
                                 (:repositories project))}
         source-root (assoc :paths ["." "test" "dev"
                                    (str source-root "/src")
                                    (str source-root "/resources")]))))
