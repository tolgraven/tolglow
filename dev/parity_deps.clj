;; Read dependency data, never evaluate the Leiningen project or its plugins.
;; Used by bin/test parity; keeps the test classpath aligned with project.clj.
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
  ;; The application manifest pins the released Beat Link dependency.
  (prn (cond-> {:deps deps
                :mvn/repos (into {} (map (fn [[id url]] [id {:url url}]))
                                 (:repositories project))}
         source-root (assoc :paths ["." "resources" "test" "dev"
                                    (str source-root "/src")
                                    (str source-root "/resources")]))))
