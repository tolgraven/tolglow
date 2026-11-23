(ns tolglow.visualization.cue-pads-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.control.views :as views]
            [afterglow.control.devices.push-2 :as push-2]
            [afterglow.control.codecs.push-2 :as codec]
            [tolglow.demo :as demo]
            [tolglow.lighting-match :as matching]
            [tolglow.show :as show]
            [afterglow.control.devices.session :as session]
            [tolglow.visualization.cue-pads :as pads]
            [tolglow.studio.query-stream :as queries]))

(def main [pads/query-id :main [0 0] 1])
(def other [pads/query-id "other" [0 0] 1])

(defn- rig []
  (-> (demo/model)
      (assoc-in [:pages :main :cells] {[0 0] :ember [0 3] :merlot})
      (assoc-in [:pages :other] {:id :other :cells {[0 0] :orchid [1 0] :sweep}})
      (assoc-in [:registry :effects :ember :outputs]
                {:color (demo/constant [1 0 0]) :dimmer {:op :beat}})
      (update-in [:registry :effects :ember] dissoc :envelope)))

(defn- at [model t]
  {:model (-> model (assoc :time-ms t :last-frame-time-ms t)
              (assoc-in [:transport :beat] (/ t 500.0)))})

(deftest mounted-regions-share-immutable-effect-keyframes
  (let [service (pads/manager) model (rig) input (at model 0) a :ipad b :push]
    (is (empty? @(:cache service)))
    (is (nil? (pads/govern! service 50 25)))
    (is (= {:ember [0.0 0.0 0.0]} (pads/present! service a input main)))
    (let [entry (:ember @(:cache service))]
      (is (= {:ember [0.125 0.0 0.0]} (pads/present! service b (at model 62.5) main)))
      (is (identical? entry (:ember @(:cache service))))
      (is (= #{:orchid} (set (keys (pads/present! service a (at model 75) other)))))
      (is (identical? entry (:ember @(:cache service))))
      (pads/release! service b)
      (is (nil? (:ember @(:cache service))))
      (is (empty? (pads/present! service a (at model 80) [pads/query-id :other [8 0] 8])))
      (is (empty? @(:cache service)))
      (is (= 0 (:clients (pads/status service))))
      (is (empty? (:instances (:model input))))
      (is (nil? (:output input))))))

(deftest sample-errors-and-opt-outs-keep-the-authored-color
  (let [service (pads/manager) model (rig)]
    (is (empty? (pads/present! service :a (at (assoc-in model [:registry :cues :ember :pad-preview] false) 0) main)))
    (is (nil? (pads/govern! service 55 25)) "A static swatch never holds back live throttling")
    (is (empty? (pads/present! service :a (at (assoc-in model [:registry :effects :ember :outputs :dimmer]
                                                         {:op :bad}) 0) main)))
    (is (= [0.125 0.0 0.0] (:ember (pads/present! service :a (at model 62.5) main))))))

(deftest preview-pressure-escalates-before-live-effects
  (let [service (pads/manager)]
    (pads/present! service :surface (at (rig) 0) main)
    (is (every? true? (repeatedly 8 #(pads/govern! service 24 25))))
    (is (= 1600 (pads/sample-period-ms service)))
    (is (false? (pads/govern! service 24 25)))
    (dotimes [_ 32] (pads/govern! service 8 25))
    (is (= 800 (pads/sample-period-ms service)))
    (pads/release! service :surface)
    (is (= {:level 0 :clients 0} (pads/status service)))))

(deftest web-and-physical-pad-readers-release-independently
  (let [service (pads/manager) source (at (rig) 75) model (:model source)
        push (pads/push-feedback service (constantly source)) handle ((:open! push))
        render! #((:project! push) handle % model)
        local {:page-id :main :origin [0 0] :ready? true}
        store (queries/create (constantly nil) (fn []) {pads/query-id
                   (fn [q] (let [lease (Object.)] {:sample! #(pads/present! service lease % q)
                                                   :close! #(pads/release! service lease)}))})
        web (get (queries/reconcile! store source #{main}) main)]
    (let [view (render! local) web-colors (:value (queries/read-result web source))
          profile (push-2/profile {:page-id :main :led-batch-size 16})
          state (merge (:initial-state profile) local {:palettes (zipmap (range 128) (repeat [0 0 0 0]))})
          messages (:midi ((:feedback profile) state view {:time-ms 75}))]
      (is (= (:ember web-colors) (get-in view [::views/pad-colors :ember])))
      (is (contains? (::views/pad-colors view) :merlot) "Hardware still shows the full page")
      (is (some #{(codec/palette-write 0 36 [10 0 0 0])} messages))
      (is (some #{[0x90 36 36]} messages)))
    (is (nil? (::views/pad-colors (render! (assoc local :target-picker {:rows []})))))
    (is (pos? (:clients (pads/status service))))
    (queries/reconcile! store source #{})
    (is (zero? (:clients (pads/status service))))
    (is (nil? (::views/pad-colors (render! (assoc local :ready? false)))))
    ((:close! push) handle)))

(deftest push-entry-point-previews-without-starting-a-cue-or-lighting-a-fixture
  (let [runtime (show/create-runtime {:model (rig) :workspace-options nil})
        input (atom nil) sent (atom [])
        capture {:open! (fn [receive!] (reset! input receive!) :capture)
                 :send! (fn [_ messages] (swap! sent into messages)) :close! (fn [_])}]
    (try
      (show/tick! runtime 0)
      (show/tick! runtime 75)
      (let [snapshot @(:state runtime)
            device (show/connect-device! runtime :push-2 {:session-id "captured-push" :midi capture
                                                         :led-batch-size 16 :schedule! nil})
            service (:cue-pad-previews runtime)]
        (doseq [index (range 128)]
          (@input (assoc (codec/palette-write 0 index [0 0 0 0]) 6 4)))
        (is (some #{(codec/palette-write 0 36 [10 0 0 0])} @sent))
        (is (identical? (:model snapshot) (:model @(:state runtime))))
        (is (identical? (:output snapshot) (:output @(:state runtime))))
        (is (pos? (:clients (pads/status service))))
        (is (empty? (get-in snapshot [:model :instances])))
        (session/suspend! device)
        (is (zero? (:clients (pads/status service)))))
      (finally (show/stop! runtime)))))

(deftest idle-previews-follow-the-current-meter-and-hdr-mode
  (let [service (pads/manager)
        model (-> (rig)
                  (assoc-in [:transport :meter] {:beats-per-bar 3 :bars-per-phrase 2})
                  (assoc-in [:registry :effects :ember :outputs :dimmer] {:op :bar}))
        frame (at model 750)]
    (is (= [0.5 0.0 0.0] (:ember (pads/present! service :grid frame main))))
    (let [hdr-model (assoc-in model [:registry :effects :ember :outputs :dimmer] (demo/constant 1.4))
          sample (at hdr-model 750)]
      (is (empty? (pads/present! service :grid sample main)))
      (is (= [1.0 0.0 0.0] (:ember (pads/present! service :grid (assoc sample :preview-hdr {:white-mix 0}) main)))))))

(deftest cue-targets-constrain-the-representative-head
  (let [service (pads/manager)
        model (-> (rig) (assoc-in [:registry :cues :ember :selector] {:fixture-ids [:head-4]}))
        see! (fn [m] (pads/present! service :grid (at m 75) main))]
    (is (contains? (see! model) :ember))
    (is (= :head-4 (get-in @(:cache service) [:ember :plan :rig 0 :id])))
    (is (empty? (see! (assoc-in model [:registry :cues :ember :pad-preview :fixture-id] :head-1))))))

(deftest representative-fixture-retains-the-venue-brightness-reference
  (let [dimmer {:id :weak :universe 1 :address 1 :channels {:dimmer 1} :measured-relative-brightness 0.1}
        effect {:id :ember :version 1 :scene-owner :scene :brightness-reference :global
                :params {matching/target-param {:type :number :default 0.1 :bounds [0 1]}}
                :outputs {:dimmer 1.0}}
        model (-> (rig) (assoc-in [:registry :effects :ember] effect)
                  (assoc-in [:registry :cues :ember :swatch] "#ffffff")
                  (assoc-in [:rig :fixtures] [dimmer (assoc dimmer :id :bright :address 2 :measured-relative-brightness 1.0)]))
        colors (pads/present! (pads/manager) :page (at model 50) main)]
    (is (= [1.0 1.0 1.0] (:ember colors)))))
