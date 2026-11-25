(ns tolglow.legacy-frame-oracle-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [tolglow.legacy-frame-oracle.compare :as compare]
            [tolglow.legacy-frame-oracle.scenario :as scenario]))

(deftest original-and-translated-whole-frame-receipts
  (let [fixture-data #(scenario/read-edn (io/resource (str "tolglow/legacy_frame_oracle/" %)))
        {:keys [original translated]} (compare/reports (fixture-data "legacy.edn") true)]
    (is (= (fixture-data "paired-report.edn") original))
    (is (= (fixture-data "translated-report.edn") translated))
    (is (= 39 (reduce + (vals (:difference-totals translated)))))))
