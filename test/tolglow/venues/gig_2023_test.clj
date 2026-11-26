(ns tolglow.venues.gig-2023-test
  "The 2023 gig rig loads as a venue: its types publish, its patch validates
  inside the room, and its documented config starts a preview show."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.application :as app]
            [tolglow.fixtures.shutter :as shutter]
            [tolglow.show :as show]
            [tolglow.venues.gig-2023 :as gig]))

(def ^:private by-id (delay (into {} (map (juxt :id identity)) (gig/fixtures))))

(deftest the-patch-matches-the-documented-address-table
  (is (= {:moving-1 1 :moving-2 17
          :moving-mini-1 100 :moving-mini-2 114 :moving-mini-3 128 :moving-mini-4 142
          :wash-1 301 :wash-2 308 :wash-3 315 :wash-4 322 :wash-5 329 :wash-6 336
          :wash-7 343 :wash-8 350 :wash-2-1 357 :wash-2-2 363}
         (update-vals @by-id :address)))
  (is (= #{1} (into #{} (map :universe) (gig/fixtures))))
  (is (= [:moving :moving-mini :wash :wash-2] (vec (distinct (map (comp first :groups) (gig/fixtures))))))
  (is (= {:rgbw-12-12-moving 1 :rgbw-60-moving-beam 1 :rgbw-7-12-moving 4
          :cameo-thunder-wash-600-rgbw 8 :cameo-thunder-wash-600-rgb 2}
         (frequencies (map :fixture-type-id (gig/fixtures)))))
  (testing "the 15-channel beam mode is published but, as at the gig, not patched"
    (is (contains? gig/fixture-types :rgbw-60-moving-beam-15ch))
    (is (= 15 (get-in gig/fixture-types [:rgbw-60-moving-beam-15ch :footprint])))))

(deftest every-fixture-sits-inside-the-room
  (let [{:keys [width height depth]} gig/room]
    (doseq [{:keys [id position]} (gig/fixtures)
            :let [[x y z] position]]
      (is (<= (- (/ width 2)) x (/ width 2)) (str id " x"))
      (is (<= 0 y height) (str id " y"))
      (is (<= (- (/ depth 2)) z (/ depth 2)) (str id " z")))))

(deftest thunder-wash-strobe-follows-the-manual-ranges
  (doseq [[type-id hz] [[:cameo-thunder-wash-600-rgb [0 30]] [:cameo-thunder-wash-600-rgbw [1 30]]]
          :let [channel (get-in gig/fixture-types [type-id :channels :strobe])]]
    (is (shutter/valid-channel-options? :strobe channel) (str type-id))
    (is (= 2 (:offset channel)))
    (is (= [[0 5] [6 10] [11 250] [251 255]] (mapv (juxt :start :end) (:functions channel))))
    (is (= hz (:hz (nth (:functions channel) 2))))
    (testing (str type-id " open, closed and strobing bytes")
      (is (= [true 0.0] (shutter/response :strobe channel 0 0.0)))
      (is (= [false 0.0] (shutter/response :strobe channel 8 0.0)))
      (is (first (shutter/response :strobe channel 130 0.0)))
      (is (pos? (second (shutter/response :strobe channel 130 0.0))))
      (is (= [true 0.0] (shutter/response :strobe channel 255 0.0))))))

(deftest the-model-carries-the-room-truss-and-published-types
  (let [model (gig/model)]
    (is (= gig/room (get-in model [:rig :room])))
    (is (= [:main-truss] (mapv :id (get-in model [:rig :trusses]))))
    (is (= 16 (count (get-in model [:rig :fixtures]))))
    (is (= (set (keys gig/fixture-types)) (set (keys (get-in model [:registry :fixtures])))))))

(deftest the-fixtures-module-config-patches-the-same-rig
  (let [runtime (app/start! {:init {:components [:show] :modules [:fixtures]}
                             :show {:workers 1 :workspace-options nil :recovery false}
                             :fixtures gig/fixtures-config})]
    (try
      (is (= (update-vals @by-id :address)
             (into {} (map (juxt :id :address)) (get-in @(:state runtime) [:model :rig :fixtures]))))
      (finally (app/stop! runtime)))))

(deftest the-documented-config-starts-a-preview-show-with-the-gig-response
  (let [runtime (app/start! (-> gig/config
                                (assoc :web-server false)
                                (update :show merge {:recovery false :workspace-options nil :workers 1})))]
    (try
      (let [snapshot (show/snapshot runtime)
            model (:model @(:state runtime))]
        (is (= {:type :preview} (get-in snapshot [:status :output])))
        (is (= gig/room (get-in model [:rig :room])))
        (is (= 16 (count (get-in model [:rig :fixtures]))))
        (is (= (:global gig/output-response)
               (get-in model [:settings :output-response :global]))))
      (finally (app/stop! runtime)))))
