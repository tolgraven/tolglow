(ns tolglow.build-manifest-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [afterglow.render.gpu :as gpu]
            [tolglow.build-manifest :as manifest]))

(deftest lein-uses-the-paired-sources-and-merges-direct-overrides
  (let [deps (manifest/dependencies '[[org.clojure/clojure "test-version"]
                                    [commons-codec/commons-codec "codec-override"]])
        versions (into {} (map (juxt first second)) deps)]
    (is (= "test-version" (versions 'org.clojure/clojure)))
    (is (= "codec-override" (versions 'commons-codec/commons-codec)))
    (is (not (contains? versions 'commons-codec)))
    (is (not-any? #(contains? versions %) '[afterglow tolgraven/afterglow]))
    (is (string? (versions 'org.jogamp.java3d/java3d-core)))
    (is (= (count deps) (count versions)))
    (is (.isFile (io/file (manifest/fork-source "src/afterglow/show.clj"))))
    (is (.isDirectory (io/file (manifest/fork-source "resources"))))))

(deftest lighting-shaders-load-from-the-paired-renderer
  (doseq [[asset path] gpu/resources]
    (let [paired (io/file (manifest/fork-source "resources") path)]
      (is (.isFile paired) path)
      (is (not (.exists (io/file "resources" path))) (str "Tolglow shadows the fork's " path))
      (is (= (slurp paired :encoding "UTF-8") (gpu/source asset)) path))))

(deftest source-tree-contains-clojure-and-local-documentation
  (let [files (filter #(.isFile %) (file-seq (io/file "src")))
        misplaced (remove #(re-find #"\.(clj|cljs|cljc|md)$" (.getName %)) files)]
    (is (empty? misplaced) (str "Move assets, scripts and fixtures out of src/: " (mapv str misplaced)))))

(deftest device-entry-points-are-classpath-resources
  (doseq [asset ["push-display/worker.cjs" "ableton-live/tolglow-live-bridge.maxpat"
                 "ableton-live/tolglow-bus-receive.maxpat" "ableton-live/tolglow-bus-send.maxpat"
                 "ableton-live/live_api.js" "ableton-live/max_adapter.js" "ableton-live/parameter_observers.js" "ableton-live/node_bridge.js"]]
    (is (some? (io/resource (str "devices/" asset))) asset)))
