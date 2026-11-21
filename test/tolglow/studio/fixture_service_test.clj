(ns tolglow.studio.fixture-service-test
  (:require [clojure.test :refer [deftest is]] [clojure.edn :as edn]
            [tolglow.studio.fixture-service :as service]
            [tolglow.studio.fixtures-test :as fixture-api]
            [tolglow.integrations.fixtures-test :as fixture-data]
            [tolglow.integrations.gdtf-test :as gdtf-data]
            [afterglow.render.frame :as render]
            [clojure.data.json :as json])
  (:import [java.util Base64]
           [java.util.concurrent Executors Callable CountDownLatch TimeUnit]))

(deftest complete-catalog-search-fetch-import-workflow-never-publishes
  (let [calls (atom []) manager (service/create-manager {:provider (fixture-api/provider calls)})]
    (try
      (is (empty? @calls))
      (is (= 0 (:catalog-count (service/snapshot manager))))
      (service/refresh! manager {})
      (service/refresh! manager {:manufacturer "example"})
      (is (= 1 (:total (service/search! manager {:query "rgbw"}))))
      (is (= 2 (count @calls)))
      (let [fetched (service/fetch! manager {:key "example/head"})
            imported (service/import! manager {:key "example/head" :mode "7ch" :id "my-head"})]
        (is (= ["7ch"] (get-in fetched [:fixture :modes])))
        (is (nil? (get-in fetched [:fixture :text])))
        (is (:valid? imported))
        (is (= "my-head" (get-in imported [:definition :id])))
        (is (= [1 2] (get-in imported [:definition :channels :pan])))
        (is (= 3 (count @calls)))
        (is (not (contains? (service/export-state manager) :registry)))
        (is (not (contains? (service/export-state manager) :rig)))
        (is (= 1 (:document-count (service/snapshot manager)))))
      (finally (service/close! manager)))))

(deftest remote-search-is-explicit-and-results-are-bounded
  (let [calls (atom []) manager (service/create-manager {:provider (fixture-api/provider calls)})]
    (try
      (is (:offline? (service/search! manager {:query "head"})))
      (is (empty? @calls))
      (let [result (service/search! manager {:query "head" :remote? true :manufacturer "example" :limit 1})]
        (is (false? (:offline? result)))
        (is (= ["example/head"] (mapv :key (:results result))))
        (is (= 1 (:total result)))
        (is (= ["example"] (get-in @calls [0 :body :manufacturersQuery])))
        (is (= 0 (:document-count (service/snapshot manager)))))
      (finally (service/close! manager)))))

(deftest exported-cache-restores-offline-with-validated-source-and-provenance
  (let [calls (atom []) manager (service/create-manager {:provider (fixture-api/provider calls)})
        offline (service/create-manager {:provider (fn [_] (throw (ex-info "Network is disabled" {})))})]
    (try
      (service/refresh! manager {:manufacturer "example"})
      (service/fetch! manager {:key "example/head" :retrieved-at "2026-11-21"})
      (let [state (edn/read-string (pr-str (service/export-state manager)))]
        (is (= state (service/validate-state state)))
        (is (= 1 (:document-count (service/restore! offline state))))
        (is (:cached? (service/fetch! offline {:key "example/head"})))
        (is (:valid? (service/import! offline {:key "example/head" :mode "7ch"})))
        (is (= "2026-11-21" (get-in (service/import! offline {:key "example/head"}) [:provenance :retrieved-at])))
        (is (= 2 (:total (service/search! offline {:query ""}))))
        (is (= state (service/export-state offline))))
      (finally (service/close! manager) (service/close! offline)))))

(deftest invalid-restoration-and-failed-network-leave-current-cache-intact
  (let [manager (service/create-manager {:provider (fixture-api/provider (atom []))})]
    (try
      (service/fetch! manager {:key "example/head"})
      (let [state (service/export-state manager)]
        (doseq [invalid [(assoc state :runtime (atom {}))
                         (assoc state :activate? true)
                         (assoc-in state [:limits :max-documents] 0)
                         (assoc-in state [:documents "example/head" :byte-count] 1)
                         (assoc-in state [:documents "example/head" :text] "{}")
                         (assoc-in state [:documents "example/head" :provenance :source] "https://untrusted.invalid/fixture")
                         (assoc-in state [:entries "example/head" :key] "../escape")]]
          (is (thrown? clojure.lang.ExceptionInfo (service/restore! manager invalid)))
          (is (= state (service/export-state manager))))
        (reset! (:provider manager) (constantly {:status 503 :body "Unavailable"}))
        (is (thrown? clojure.lang.ExceptionInfo (service/fetch! manager {:key "example/head" :refresh? true})))
        (is (= state (service/export-state manager))))
      (finally (service/close! manager)))))

(deftest uploads-support-text-and-base64-gdtf-without-provider-use
  (let [manager (service/create-manager {:provider (fn [_] (throw (ex-info "No network allowed" {})))})
        encoded (.encodeToString (Base64/getEncoder) (gdtf-data/zip-bytes [["description.xml" gdtf-data/description]]))]
    (try
      (is (:valid? (service/import! manager {:format "ofl" :content (json/write-str fixture-data/ofl)})))
      (is (:valid? (service/import-file {:format :qxf :content fixture-data/qxf :id :upload})))
      (let [result (service/import-file {:format :gdtf :content-base64 encoded :source "upload:head.gdtf"})]
        (is (:valid? result))
        (is (= [1 5] (get-in result [:definition :channels :pan])))
        (is (= :archive-bytes (get-in result [:provenance :checksum-kind]))))
      (is (thrown? clojure.lang.ExceptionInfo (service/import-file {:format :gdtf :content-base64 "!!!"})))
      (is (thrown? clojure.lang.ExceptionInfo (service/import-file {:format :ofl :content (apply str (repeat (inc (* 4 1024 1024)) "x"))})))
      (is (= 0 (:catalog-count (service/snapshot manager))))
      (finally (service/close! manager)))))

(deftest upload-mode-selection-is-an-interactive-result-with-intact-labels
  (let [input (assoc fixture-data/ofl :name "RGBW Head with Spaces"
                     :modes [{:name "Standard 16 bit" :channels ["Pan" "Pan fine" "Red" "Green" "Blue" "White" "Reset"]}
                             {:name "Compact RGBW" :channels ["Red" "Green" "Blue" "White"]}])
        request {:format :ofl :content (json/write-str input) :id "new-fixture-type"}
        choice (service/import-file request)
        imported (service/import-file (assoc request :mode "Compact RGBW"))]
    (is (false? (:valid? choice)))
    (is (= ["Standard 16 bit" "Compact RGBW"] (:modes choice)))
    (is (= :mode-required (get-in choice [:diagnostics 0 :code])))
    (is (nil? (:candidate choice)))
    (is (:valid? imported))
    (is (= "new-fixture-type" (get-in imported [:definition :id])))
    (is (= "RGBW Head with Spaces" (get-in imported [:definition :name])))
    (is (= "Compact RGBW" (get-in imported [:definition :mode])))))

(deftest concurrent-fetches-share-one-owned-cache-transaction
  (let [entered (CountDownLatch. 1) release (CountDownLatch. 1) calls (atom 0)
        provider (fn [_]
                   (swap! calls inc) (.countDown entered)
                   (when-not (.await release 5 TimeUnit/SECONDS) (throw (ex-info "Test deadline" {})))
                   {:status 200 :body (json/write-str fixture-data/ofl)})
        manager (service/create-manager {:provider provider}) pool (Executors/newFixedThreadPool 2)]
    (try
      (let [a (.submit pool ^Callable #(service/fetch! manager {:key "example/head"}))]
        (is (.await entered 5 TimeUnit/SECONDS))
        (let [b (.submit pool ^Callable #(service/fetch! manager {:key "example/head"}))]
          (.countDown release)
          (is (= #{false true} (set (map :cached? [(.get a 5 TimeUnit/SECONDS) (.get b 5 TimeUnit/SECONDS)]))))
          (is (= 1 @calls))
          (is (= 1 (:document-count (service/snapshot manager))))))
      (finally (.countDown release) (.shutdownNow pool) (service/close! manager)))))

(deftest close-is-owned-idempotent-and-prevents-future-work
  (let [closed (atom 0)
        manager (service/create-manager {:provider (with-meta (fixture-api/provider (atom [])) {:close! #(swap! closed inc)})})]
    (service/close! manager)
    (service/close! manager)
    (is (= 1 @closed))
    (is (thrown? clojure.lang.ExceptionInfo (service/search! manager {})))
    (is (thrown? clojure.lang.ExceptionInfo (service/import! manager {:format :qxf :content fixture-data/qxf})))))

(deftest instantiate-preserves-explicit-rgbw-fine-offsets-and-calibration
  (let [definition {:id :head-type :version 2 :name "RGBW head" :footprint 8
                    :channels {:red 1 :green 2 :blue 3 :white 4 :pan [5 8]
                               :tilt {:offset 6 :fine-offset 7 :inverted-from 1}}
                    :calibration {:pan {:min-deg -270 :max-deg 270} :tilt {:min-deg -135 :max-deg 135}}
                    :rgbw-policy :extract-min}
        options {:id "head-left" :position [1 3 2] :rotation-deg [0 90 0] :universe 1 :address 508}
        result (service/instantiate definition options)
        fixture (:fixture result) patch (when fixture (render/compile-patch [fixture]))]
    (is (:valid? result))
    (is (= (:channels definition) (:channels fixture)))
    (is (= (:calibration definition) (:calibration fixture)))
    (is (= :extract-min (:rgbw-policy fixture)))
    (is (= :head-type (:fixture-type-id fixture)))
    (is (= 2 (:fixture-type-version fixture)))
    (is (= [1 3 2] (:position fixture)))
    (is (= [2 2] (get-in patch [:by-id "head-left" :compiled-channels :pan :fine])))
    (is (= [0 90 0] (:rotation-deg fixture)))
    (is (empty? (:diagnostics result)))))

(deftest instantiate-does-not-guess-unmapped-controls-or-movement-ranges
  (let [definition (:definition (service/import-file {:format :ofl :content (json/write-str fixture-data/ofl)}))
        options {:id "new-head" :position [0 3 0] :universe 1 :address 1}
        result (service/instantiate definition options)
        codes (set (map :code (:diagnostics result)))
        mapped (assoc-in definition [:channels :shutter] 7)
        calibrated (assoc options :calibration {:pan {:min-deg -270 :max-deg 270}})]
    (is (false? (:valid? result)))
    (is (nil? (:fixture result)))
    (is (codes :unmapped-required-channel))
    (is (codes :missing-calibration))
    (is (:valid? (service/instantiate mapped calibrated)))
    (is (false? (:valid? (service/instantiate mapped (assoc calibrated :address 513)))))
    (is (false? (:valid? (service/instantiate (assoc mapped :footprint 6) calibrated))))))
