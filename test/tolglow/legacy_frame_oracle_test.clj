(ns tolglow.legacy-frame-oracle-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [tolglow.legacy-frame-oracle.compare :as compare]
            [tolglow.legacy-frame-oracle.scenario :as scenario]))

(defn- fixture-data [file]
  (scenario/read-edn (io/resource (str "tolglow/legacy_frame_oracle/" file))))

(deftest original-and-translated-whole-frame-receipts
  (let [{:keys [original translated]} (compare/reports (fixture-data "legacy.edn") true)]
    (is (= (fixture-data "paired-report.edn") original))
    (is (= (fixture-data "translated-report.edn") translated))
    (is (= 39 (reduce + (vals (:difference-totals translated)))))))

(defn- set-legacy-byte [legacy frame address value]
  (assoc-in legacy [:frames frame :universes 1 (dec address)] value))

(deftest bytes-outside-the-fixture-footprint-are-compared
  (let [legacy (fixture-data "legacy.edn")
        last-frame (dec (count (:frames legacy)))
        tampered (-> legacy (set-legacy-byte 0 1 7) (set-legacy-byte last-frame 512 9))
        {:keys [original]} (compare/reports tampered false)
        changes (fn [frame] (set (get-in original [:frames frame :byte-differences])))]
    (testing "the first and last universe addresses appear as legacy/paired byte pairs"
      (is (contains? (changes 0) [1 1 7 0]))
      (is (contains? (changes last-frame) [1 512 9 0])))
    (testing "the translated receipt rejects a difference without a declared reason"
      (let [error (try (compare/reports tampered true) nil
                       (catch clojure.lang.ExceptionInfo error error))]
        (is (= "Unexpected changed channel in translated render" (ex-message error)))
        (is (= {:frame :idle :change [1 1 7 0]} (ex-data error)))))))

(deftest changed-receipts-fail-with-a-focused-difference
  (let [directory (doto (io/file (System/getProperty "java.io.tmpdir")
                                 (str "legacy-frame-oracle-" (System/nanoTime)))
                    .mkdirs)
        path #(.getPath (io/file directory %))
        expected (assoc-in (fixture-data "paired-report.edn")
                           [:frames 0 :byte-differences 0] [1 42 0 254])
        legacy-path (path "legacy.edn")]
    (try
      (scenario/write-edn! legacy-path (fixture-data "legacy.edn"))
      (scenario/write-edn! (path "expected.edn") expected)
      (let [error (try (binding [*out* (java.io.StringWriter.)]
                         (compare/-main legacy-path (path "report.edn") (path "expected.edn")))
                       nil
                       (catch clojure.lang.ExceptionInfo error error))]
        (is (= "Parity evidence changed, review the emitted report" (ex-message error)))
        (is (= [[nil nil nil 254]]
               (get-in (ex-data error) [:only-expected :frames 0 :byte-differences])))
        (is (= (fixture-data "paired-report.edn") (scenario/read-edn (path "report.edn")))
            "The emitted report is written before the comparison for review"))
      (finally
        (doseq [file (reverse (file-seq directory))] (.delete ^java.io.File file))))))
