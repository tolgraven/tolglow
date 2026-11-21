(ns tolglow.studio.fixture-host-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.application :as app]
            [tolglow.studio.fixtures-test :as catalog]
            [tolglow.integrations.fixtures-test :as fixture-data])
  (:import [java.io ByteArrayInputStream]))

(defn- request [handler method path payload]
  (let [response (handler {:request-method method :uri path :headers {"host" "localhost"}
                           :body (ByteArrayInputStream. (.getBytes (json/write-str payload) "UTF-8"))})]
    {:status (:status response) :data (json/read-str (:body response) :key-fn keyword)}))

(defn- publish! [handler definition]
  (request handler :post "/api/fixtures/publish" {:source (pr-str definition)}))

(defn- patch! [handler type-id id address & [extra]]
  (request handler :post "/api/fixtures/patch"
           (merge {:fixture-type-id type-id :id id :universe 1 :address address
                   :position [-2 3 0] :rotation-deg [0 30 0] :rgbw-policy "none" :calibration {}}
                  extra)))

(defn- fixture-types [runtime] (get-in @(:state runtime) [:model :registry :fixtures]))
(defn- rig [runtime] (get-in @(:state runtime) [:model :rig]))
(defn- no-network [_] (throw (ex-info "Host test does not permit network access" {})))

(def rgbw-type
  {:id "rgbw-par" :version 1 :name "RGBW PAR with spaces" :manufacturer "Handmade fixtures"
   :mode "Five channel RGBW + dimmer" :footprint 5
   :channels {:red 1 :green 2 :blue 3 :white 4 :dimmer 5} :rgbw-policy :none})

(deftest catalog-import-is-a-draft-until-explicit-publication-and-patching
  (let [calls (atom []) runtime (app/start! (assoc (app/default-config) :web-server {:port 0} :show {:fixture-options {:provider (catalog/provider calls)}}))]
    (try
      (let [handler (studio/handler runtime) initial-rig (rig runtime) initial-types (fixture-types runtime)]
        (is (= 200 (:status (request handler :get "/api/fixtures" {}))))
        (is (empty? @calls))
        (is (= 200 (:status (request handler :post "/api/fixtures/refresh" {:manufacturer "example"}))))
        (is (= 1 (get-in (request handler :post "/api/fixtures/search" {:query "RGBW"}) [:data :total])))
        (is (= ["7ch"] (get-in (request handler :post "/api/fixtures/fetch" {:key "example/head"}) [:data :fixture :modes])))
        (let [result (request handler :post "/api/fixtures/import" {:key "example/head" :mode "7ch"})
              definition (edn/read-string (get-in result [:data :source]))]
          (is (= 200 (:status result)))
          (is (true? (get-in result [:data :valid?])))
          (is (= "Example Lighting" (:manufacturer definition)))
          (is (= "https://open-fixture-library.org/example/head.json" (get-in definition [:provenance :source])))
          (is (= "MIT" (get-in definition [:provenance :license :spdx])))
          (is (= [1 2] (get-in definition [:channels :pan])))
          (is (some #(= "unmapped-channel" (:code %)) (get-in result [:data :diagnostics])))
          (is (= initial-rig (rig runtime)))
          (is (= initial-types (fixture-types runtime)))
          (is (empty? (:instances (show/snapshot runtime))))
          ;; Typed source can retain a namespaced ID while the JSON patch command
          ;; addresses that type by its exact wire spelling.
          (let [reviewed (-> definition (assoc :id :library/imported-head)
                             (assoc-in [:channels :shutter] 7)
                             (assoc :calibration {:pan {:min-deg -270 :max-deg 270}}))]
            (is (= "accepted" (get-in (publish! handler reviewed) [:data :status])))
            (is (= reviewed (get (fixture-types runtime) :library/imported-head)))
            (is (= initial-rig (rig runtime)))
            (is (= "accepted" (get-in (patch! handler "library/imported-head" "imported-head-1" 200
                                                         {:calibration (:calibration reviewed)}) [:data :status])))
            (is (= (:channels reviewed) (:channels (last (:fixtures (rig runtime))))))
            (is (= :library/imported-head (:fixture-type-id (last (:fixtures (rig runtime))))))
            (is (empty? (:instances (show/snapshot runtime))))))
        (is (= 2 (count @calls)) "Offline search/import never fetches another document"))
      (finally (app/stop! runtime)))))

(deftest manual-types-preserve-human-labels-and-rig-patches-are-atomic
  (let [runtime (app/start! (assoc (app/default-config) :web-server {:port 0} :show {:fixture-options {:provider no-network}}))]
    (try
      (let [handler (studio/handler runtime)
            original (rig runtime)]
        (is (= "accepted" (get-in (publish! handler rgbw-type) [:data :status])))
        (is (= rgbw-type (get (fixture-types runtime) "rgbw-par")))
        (is (= original (rig runtime)))
        (is (= "Five channel RGBW + dimmer"
               (:mode (first (filter #(= "rgbw-par" (:id %)) (:fixture-types (show/snapshot runtime)))))))
        (is (= "accepted" (get-in (patch! handler "rgbw-par" "manual-par" 200
                                                  {:rgbw-policy "extract-min"}) [:data :status])))
        (let [fixture (last (:fixtures (rig runtime)))]
          (is (= "rgbw-par" (:fixture-type-id fixture)))
          (is (= :extract-min (:rgbw-policy fixture)))
          (is (= [-2 3 0] (:position fixture)))
          (is (= [0 30 0] (:rotation-deg fixture))))
        (let [before (rig runtime)]
          (is (= 422 (:status (patch! handler "rgbw-par" "overlap" 202))))
          (is (= before (rig runtime)))
          (is (= 422 (:status (patch! handler "rgbw-par" "manual-par" 220))))
          (is (= before (rig runtime))))
        (let [sparse {:id :rig/sparse-head :version 1 :name "Sparse calibrated head" :footprint 8
                      :channels {:dimmer 1 :pan [2 5]}
                      :calibration {:pan {:min-deg -270 :max-deg 270}}}]
          (is (= "accepted" (get-in (publish! handler sparse) [:data :status])))
          (is (= "accepted" (get-in (patch! handler "rig/sparse-head" "sparse" 300
                                                    {:calibration (:calibration sparse)}) [:data :status])))
          (let [before (rig runtime)]
            (is (= 422 (:status (patch! handler "rgbw-par" "reserved-hole" 306)))
                "The entire eight-slot footprint is reserved, including unmapped trailing holes")
            (is (= before (rig runtime))))
          (is (= "accepted" (get-in (patch! handler "rig/sparse-head" "cross-universe" 510
                                                    {:calibration (:calibration sparse)}) [:data :status])))
          (is (= [2 5] (:pan (:channels (last (:fixtures (rig runtime))))))))
        (let [before (fixture-types runtime)
              rejected (publish! handler (assoc rgbw-type :name "Changed without a new version"))]
          (is (= 422 (:status rejected)))
          (is (= before (fixture-types runtime))))
        (is (empty? (:instances (show/snapshot runtime)))))
      (finally (app/stop! runtime)))))

(deftest unsupported-upload-keeps-typed-candidate-and-diagnostics-without-show-mutation
  (let [runtime (app/start! (assoc (app/default-config) :web-server {:port 0} :show {:fixture-options {:provider no-network}}))]
    (try
      (let [handler (studio/handler runtime) original (rig runtime) types (fixture-types runtime)
            source (assoc fixture-data/ofl :name "Matrix Mode with Human Labels"
                          :modes [{:name "Matrix and red" :channels [{:insert "matrixChannels"} "Red"]}])
            result (request handler :post "/api/fixtures/import"
                            {:format "ofl" :content (json/write-str source) :source "upload:matrix.json"})
            candidate (edn/read-string (get-in result [:data :source]))]
        (is (= 200 (:status result)))
        (is (false? (get-in result [:data :valid?])))
        (is (nil? (get-in result [:data :definition])))
        (is (= "Matrix Mode with Human Labels" (:name candidate)))
        (is (= "Matrix and red" (:mode candidate)))
        (is (= {:red 2} (:channels candidate)))
        (is (= "matrixChannels" (get-in candidate [:source-data "modes" 0 "channels" 0 "insert"])))
        (is (some #(= "matrix-mode" (:code %)) (get-in result [:data :diagnostics])))
        (is (= original (rig runtime)))
        (is (= types (fixture-types runtime)))
        (let [multi (assoc source :modes [{:name "Standard 16 bit" :channels ["Red"]}
                                         {:name "Compact RGBW" :channels ["Red" "Green" "Blue" "White"]}])
              choices (request handler :post "/api/fixtures/import" {:format "ofl" :content (json/write-str multi)})
              selected (request handler :post "/api/fixtures/import"
                                {:format "ofl" :content (json/write-str multi) :mode "Compact RGBW"})]
          (is (= ["Standard 16 bit" "Compact RGBW"] (get-in choices [:data :modes])))
          (is (= "mode-required" (get-in choices [:data :diagnostics 0 :code])))
          (is (true? (get-in selected [:data :valid?])))
          (is (= "Compact RGBW" (:mode (edn/read-string (get-in selected [:data :source])))))))
      (finally (app/stop! runtime)))))
