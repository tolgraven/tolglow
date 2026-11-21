;; deps.edn owns Studio coordinates; the helper expands the actual paired fork.
;; No second dependency/version list or installed Afterglow jar is maintained.
(load-file "dev/tolglow/build_manifest.clj")

(defproject tolgraven/tolglow "0.2.0-SNAPSHOT"
  :description "Functional, live-programmable lighting with a re-frame performance studio."
  :url "https://github.com/tolgraven/tolglow"
  :dependencies ~(:dependencies (tolglow.build-manifest/manifest "."))
  :source-paths ~(:source-paths (tolglow.build-manifest/manifest "."))
  :resource-paths ~(:resource-paths (tolglow.build-manifest/manifest "."))
  :repositories ~(:repositories (tolglow.build-manifest/manifest "."))
  :test-paths ["test"]
  :main tolglow.application
  :target-path "target/%s"
  :jvm-opts ["-Djava.awt.headless=true"]
  :profiles {:dev {:source-paths ["dev"]}
             :uberjar {:aot [tolglow.application]}}
  :test-selectors
  {:default (fn [metadata]
              (not= 'tolglow.studio.terminal-docker-test (ns-name (:ns metadata))))
   :docker (fn [metadata]
             (= 'tolglow.studio.terminal-docker-test (ns-name (:ns metadata))))}
  :aliases {"test-pure" ["run" "-m" "tolglow.test-runner" "pure"]
            "test-parity" ["run" "-m" "tolglow.test-runner" "parity"]}
  :min-lein-version "2.13.1")
