(load-file "dev/tolglow/build_manifest.clj")

(defproject tolgraven/tolglow "0.2.0-SNAPSHOT"
  :description "Functional, live-programmable lighting with a re-frame performance studio."
  :url "https://github.com/tolgraven/tolglow"
  :dependencies ~(tolglow.build-manifest/dependencies
                  '[[org.clojure/clojure "1.12.7"]
                    [org.clojure/data.json "2.5.2"]
                    [com.cognitect/transit-clj "1.2.372"]
                    [metosin/reitit-ring "0.11.0"]
                    [thi.ng/color "1.5.1" :exclusions [org.clojure/clojurescript]]
                    [org.slf4j/slf4j-api "2.0.20"]
                    [commons-codec/commons-codec "1.23.0"]
                    [org.deepsymmetry/beat-link "8.0.0"]
                    [ring/ring-core "1.15.6"]
                    [ring/ring-jetty-adapter "1.15.6"]])
  :source-paths ["src" "src/shared" ~(tolglow.build-manifest/fork-source "src")]
  :resource-paths ["resources" ~(tolglow.build-manifest/fork-source "resources")]
  :repositories ~(merge {"central" "https://repo.maven.apache.org/maven2/"}
                        (:repositories (tolglow.build-manifest/paired-project ".")))
  :test-paths ["test"]
  :main tolglow.application
  :target-path "target/%s"
  :jvm-opts ["-Djava.awt.headless=true"]
  :profiles {:dev {:source-paths ["dev"]}
             :embedded-host ^:leaky {:source-paths ["native/EmbeddedHost/src"]
                             :test-paths ["native/EmbeddedHost/test"]
                             :dependencies [[org.babashka/sci "0.15.60"]]}
             :native-gpu {:dependencies ~(get-in (tolglow.build-manifest/paired-project ".")
                                                 [:profiles :native-gpu :dependencies])
                          :test-paths ["test-native"]}
             :benchmark {:source-paths [~(tolglow.build-manifest/fork-source "dev")]
                         :jvm-opts ["-Xms768m" "-Xmx768m"]}
             :cljs {:source-paths ^:replace ["src/studio" "src/shared" ~(tolglow.build-manifest/fork-source "src")]
                    :dependencies ^:replace [[org.clojure/clojure "1.12.7"]
                                   [org.clojure/clojurescript "1.12.145"]
                                   [com.fasterxml.jackson.core/jackson-core "2.22.3"]
                                   [thi.ng/color "1.5.1" :exclusions [org.clojure/clojurescript]]
                                   [thheller/shadow-cljs "3.5.14"]
                                   ;; Match shadow-cljs's own POM; Lein's nearest
                                   ;; dependency selection otherwise admits CLJS's older compiler.
                                   [com.google.javascript/closure-compiler "v20261021"]
                                   [metosin/reitit-frontend "0.11.0"]
                                   [com.cognitect/transit-cljs "0.8.280"]
                                   [re-frame "1.4.7"]
                                   [reagent "2.0.1"]]}
             :uberjar {:aot [tolglow.application]}}
  :test-selectors
  {:default (fn [metadata]
              (not= 'tolglow.studio.terminal-docker-test (ns-name (:ns metadata))))
   :docker (fn [metadata]
             (= 'tolglow.studio.terminal-docker-test (ns-name (:ns metadata))))}
  :aliases {"check-requests" ["run" "-m" "tolglow.check-requests"]
            "test-pure" ["run" "-m" "tolglow.test-runner" "pure"]
            "test-parity" ["run" "-m" "tolglow.test-runner" "parity"]}
  :min-lein-version "2.13.1")
