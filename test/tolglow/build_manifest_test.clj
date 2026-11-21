(ns tolglow.build-manifest-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [tolglow.build-manifest :as manifest]))

(deftest complete-project-uses-canonical-versions-and-the-paired-source
  (let [config (manifest/manifest ".")
        deps (edn/read-string (slurp "deps.edn"))
        versions (into {} (map (fn [[id version]] [id version])) (:dependencies config))]
    (is (= (get-in deps [:deps 'org.clojure/clojure :mvn/version]) (versions 'org.clojure/clojure)))
    (is (= (get-in deps [:deps 'ring/ring-core :mvn/version]) (versions 'ring/ring-core)))
    (is (string? (versions 'nrepl/nrepl)))
    (is (string? (versions 'org.jogamp.java3d/java3d-core)))
    (is (not (contains? versions 'tolgraven/afterglow)))
    (is (= (count versions) (count (:dependencies config))))
    (is (= ["src" "../afterglow/src"] (:source-paths config)))
    (is (= ["resources" "../afterglow/resources"] (:resource-paths config)))
    (is (every? #(.isDirectory (io/file %)) (concat (:source-paths config) (:resource-paths config))))))
