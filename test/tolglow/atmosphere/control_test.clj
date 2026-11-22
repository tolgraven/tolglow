(ns tolglow.atmosphere.control-test
  (:require [afterglow.control.model :as model]
            [afterglow.output.scheduler :as scheduler]
            [afterglow.render.frame :as frame]
            [afterglow.runtime.events :as events]
            [clojure.test :refer [deftest is]]
            [tolglow.atmosphere.control :as fog]
            [tolglow.atmosphere.deadline :as deadline]
            [tolglow.show :as show]))

(def jet {:id :jet :universe 1 :address 1 :channels {:fog 1}
          :fogger {:flow-m3-min 60 :mixing-seconds 0.12 :duty {:max-on-s 0.1 :min-off-s 0.2 :max-level 0.35}}})
(def rig {:fixtures [jet] :room {:width 1 :height 1 :depth 1 :fog-half-life-s 3}})
(def patch (frame/compile-patch [jet]))
(def base (model/new-show {:rig rig}))
(def options {:enabled? true :target 0.3 :hysteresis 0.05 :fog-fixture-ids [:jet]})
(def armed (fog/configure base options))
(defn observed [level time]
  {:epoch 0 :fog-level level :last-presented-ms time :sources [{:fixture-id :jet :flow-m3-s 0 :last-presented-ms time}]})

(deftest hysteresis-duty-cooldown-and-arbitration
  (let [at #(fog/advance armed patch (observed %1 %2) %3 %2 false)
        low (at 0.24 0 nil)
        band (at 0.3 25 low)
        high (at 0.36 50 band)
        idle (at 0.30 75 high)
        cooling (at 0.23 150 idle)
        next-pulse (at 0.23 301 cooling)
        sel #(get-in % [:pumps :jet])]
    (is (= [:injecting :injecting :idle :idle :resting :injecting]
           (mapv #(get-in % [:pumps :jet :mode]) [low band high idle cooling next-pulse])))
    (is (= 0.35 (:demand (sel low))))
    (is (= [100.0 401.0] (mapv #(get-in % [:pumps :jet :until-ms]) [low next-pulse])))
    (is (= #{:fog/above-target :fog/demand-ended}
           (set (map #(get-in % [:event :type]) (events/pending (fog/publish (events/begin armed) band high))))))
    (is (empty? (fog/demands (fog/arbitrate low [{:assignments [{:target-id :jet :attribute :fog :value 0.1}]}]))))
    (is (= :no-recent-dmx (get-in (fog/advance armed patch (observed 0.1 0) nil 800 false) [:pumps :jet :mode])))
    (is (empty? (fog/demands (fog/advance armed patch (observed 0.1 0) nil 20 true))))
    (is (= [:other] (mapv :target-id (get-in (fog/music-pulses patch (observed 0.24 0) nil low
                  {:assignments [{:target-id :jet} {:target-id :other}]} [] 0 false) [:block :assignments]))))
    (is (thrown? Exception (fog/configure armed {:hysteresis 0})))
    (is (false? (get-in (fog/disarm-restored armed) [:settings :fog-control :enabled?])))))

(deftest publisher-guard-copies-only-expired-auto-slots
  (let [original (byte-array [80 10 19])
        packet {1 original}
        leases (deadline/remember nil 7 [{:until-ms 100 :slots [[1 0] [1 1]]}])]
    (is (identical? packet (deadline/guard leases packet 7 99)))
    (is (= [0 0 19] (vec (get (deadline/guard leases packet 7 100) 1))))
    (is (= [80 10 19] (vec original)))
    (let [manual (deadline/remember leases 8 [])]
      (is (= [0 0 19] (vec (get (deadline/guard manual packet 7 70) 1))))
      (is (identical? packet (deadline/guard manual packet 8 200))))))

(deftest independent-delivery-stops-an-automatic-pulse-if-the-renderer-stalls
  (let [wall (atom 0) raw (atom nil) seen (atom [])
        rt (show/create-runtime {:model base :independent-output? true :physical-now-ms #(deref wall)
                                 :sink {:send! (fn [packet _] (reset! raw (bit-and 255 (aget ^bytes (get packet 1) 0))))}
                                 :event-admit (fn [m xs] (swap! seen into xs) {:model m :deliveries []})})
        frame! (fn [t] (reset! wall t) (show/tick! rt (double t)) (scheduler/publish! (:publisher rt)))]
    (try
      (let [ack (show/submit! rt :settings {:scope :fog-control :values options})]
        (frame! 0) (is (= :accepted (:status @ack))) (is (zero? @raw))
        (frame! 25) (frame! 50)
        (is (<= 88 @raw 90))
        (is (get-in (show/snapshot rt) [:output :fog-control :fresh?]))
        (is (= 0.3 (get-in @(:state rt) [:model :variables :fog/target])))
        (is (some #(= :fog/below-target (:type %)) @seen))
        (reset! wall 200)
        (scheduler/publish! (:publisher rt))
        (is (zero? @raw))
        (frame! 225)
        (is (pos? (get-in (show/snapshot rt) [:output :atmosphere :fog-level])))
        (is (zero? @raw)))
      (finally (show/stop! rt)))))

(deftest music-and-feedback-share-device-limits-and-yield-to-cues
  (let [music {:kind :assignments :assignments [{:target-id :jet :attribute :fog :value 0.5 :blend :max}]}
        at (fn [m old t skipped? authored]
             (fog/frame m patch (observed 0.1 t) old t skipped? authored music))
        first (at base nil 0 false [])
        swap-owner (at armed (:status first) 150 false [])
        manual (at armed nil 0 false [{:batch {:target-ids [:jet] :outputs {:fog {}}}}])
        suspended (at armed nil 0 true [])]
    (is (= [0.35] (mapv :value (:leases first))))
    (is (empty? (:leases swap-owner)))
    (is (empty? (:leases manual)))
    (is (= :manual (get-in manual [:status :pumps :jet :mode])))
    (is (empty? (:leases suspended)))
    (let [inverted (frame/compile-patch [(assoc-in jet [:fogger :dmx-range] [255 0])])
          s (fog/advance armed inverted (observed 0.1 0) nil 0 false)]
      (is (= :no-zero-off (get-in s [:pumps :jet :mode])))
      (is (empty? (fog/demands s))))))
