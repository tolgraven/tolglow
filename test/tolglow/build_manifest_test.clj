(ns tolglow.build-manifest-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
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
