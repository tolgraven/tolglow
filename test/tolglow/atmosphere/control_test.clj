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
    (is (nil? (:block (fog/music-pulses patch (observed 0.36 50) low high
                                        {:assignments [{:target-id :other :value 0.5}]} [] 50 false))))
    (is (thrown? Exception (fog/configure armed {:hysteresis 0})))
    (is (false? (get-in (fog/disarm-restored armed) [:settings :fog-control :enabled?])))))

(deftest publisher-guard-copies-only-expired-auto-slots
  (let [original (byte-array [80 10 19])
        packet {1 original}
        leases (deadline/remember nil 7 [{:until-ms 100 :off-bytes {[1 0] 0 [1 1] 0}}])]
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
      (is (= :injecting (get-in s [:pumps :jet :mode])))
      (is (< (abs (- 0.65 (get-in (fog/demands s) [0 :value]))) 1e-9))
      (is (= {[1 0] 255} (get-in (fog/demands s) [0 :off-bytes])))
      (is (= [255 42] (mapv #(bit-and 255 %) (get (deadline/restore-packet inverted [:jet] {1 (byte-array [80 42])}) 1)))))))

(deftest restoring-an-active-automatic-pulse-preserves-other-channels-and-disarms
  (let [wall (atom 0) raw (atom nil)
        sink {:send! (fn [packets _] (reset! raw (bit-and 255 (aget ^bytes (get packets 1) 0))))}
        rt (show/create-runtime {:model base :sink sink :physical-now-ms #(deref wall)})]
    (try
      (let [config (show/submit! rt :settings {:scope :fog-control :values options})]
        (show/tick! rt 0) (is (= :accepted (:status @config)))
        (reset! wall 25) (show/tick! rt 25)
        (reset! wall 50) (show/tick! rt 50)
        (is (pos? @raw))
        (let [doc (show/export-show-source rt)
              receipt (show/submit! rt :import {:source doc})]
          (reset! wall 75) (show/tick! rt 75)
          (is (= :accepted (:status @receipt)))
          (is (zero? @raw))
          (is (= false (get-in (show/snapshot rt) [:settings :fog-control :enabled?])))))
      (finally (show/stop! rt))))
  (let [b (byte-array [80 42]), restored (deadline/restore-packet patch [:jet] {1 b})]
    (is (= [0 42] (vec (get restored 1))))
    (is (= [80 42] (vec b)))))

(deftest split-universe-reversed-nozzle-has-the-same-optical-target-and-physical-off
  (let [jet (-> jet (assoc :address 512 :channels {:fog {:offset 1 :fine-offset 2 :inverted-from 90}})
                (assoc-in [:fogger :dmx-range] [220.25 20.25]))
        model (model/new-show {:rig (assoc rig :fixtures [jet])})
        now (atom 0) bytes (atom nil)
        rt (show/create-runtime {:model model :independent-output? true :physical-now-ms #(deref now)
                                 :sink {:send! (fn [out _]
                                                 (reset! bytes [(bit-and 255 (aget ^bytes (get out 1) 511))
                                                                (bit-and 255 (aget ^bytes (get out 2) 0))]))}})
        cycle! #(do (reset! now %) (show/tick! rt (double %)) (scheduler/publish! (:publisher rt)))]
    (try
      (let [ack (show/submit! rt :settings {:scope :fog-control :values options})]
        (cycle! 0)
        (is (= :accepted (:status @ack)))
        (is (= [220 64] @bytes))
        (cycle! 25) (cycle! 50)
        (is (= [150 64] @bytes))
        (reset! now 200)
        (scheduler/publish! (:publisher rt))
        (is (= [220 64] @bytes)))
      (finally (show/stop! rt)))))

(deftest malformed-device-duty-suspends-both-automatic-sources
  (let [bad (frame/compile-patch [(assoc-in jet [:fogger :duty] {:max-level "full"})])
        raw {:kind :assignments :assignments [{:target-id :jet :attribute :fog :value 0.5 :blend :max}]}
        setpoint (fog/frame armed bad (observed 0.05 0) nil 0 false [] raw)
        music (fog/frame base bad (observed 0.05 0) nil 0 false [] raw)]
    (is (= :invalid-duty (get-in setpoint [:status :pumps :jet :mode])))
    (is (= :invalid-duty (get-in music [:status :music-pumps :jet :mode])))
    (is (every? empty? (map :leases [setpoint music])))))

(deftest slow-mixing-jets-stop-on-queued-volume-before-the-room-crosses-the-band
  (let [rt (show/create-runtime {:model (assoc base :rig (-> rig
                         (assoc-in [:room :fog-half-life-s] 180)
                         (assoc-in [:fixtures 0 :fogger :flow-m3-min] 300)
                         (assoc-in [:fixtures 0 :fogger :mixing-seconds] 20)))})]
    (try
      (let [ack (show/submit! rt :settings {:scope :fog-control :values (assoc options :target 0.15 :hysteresis 0.025)})
            frames (mapv (fn [t]
                           (show/tick! rt (double t))
                           (let [out (:output @(:state rt))] [(:fog-control out) (bit-and 255 (aget ^bytes (get-in out [:universes 1]) 0))]))
                         (range 0 2200 25))
            held (filter #(and (get-in % [0 :coasting?]) (< (get-in % [0 :level]) 0.125)) frames)]
        (is (= :accepted (:status @ack)))
        (is (some #(pos? (second %)) frames))
        (is (> (count held) 20))
        (is (every? #(and (zero? (second %)) (>= (get-in % [0 :coasting-peak :fog-level]) 0.175)) held)))
      (finally (show/stop! rt)))))
