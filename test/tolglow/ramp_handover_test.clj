(ns tolglow.ramp-handover-test
  "A ramp that takes over a running ramp continues from its position and its
  velocity (#896), observed through captured 16-bit ArtDmx packets from the
  real owner tick."
  (:require [afterglow.control.model :as control]
            [afterglow.output.artnet :as artnet]
            [clojure.test :refer [deftest is testing]]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.persistence :as files]
            [tolglow.show :as show]))

(def ^:private effect
  {:id :wash :version 1 :category :light
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}
            :tint {:type :color :default [0.0 0.0 0.0]}
            :steps {:type :integer :default 0 :bounds [0 40]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- live-model []
  (control/new-show
   {:show-id :handover-test :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects {:wash effect}
               :cues {:wash {:id :wash :effect-id :wash :version 1}}}
    ;; A 16-bit dimmer, so a frame's value resolves well below one DMX step.
    :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer {:offset 1 :fine-offset 2}}}]}}))

(def ^:private poster {:adapter :agent :session-id "handover"})

(def ^:private step-tolerance
  "The declared tolerance: one 8-bit DMX step."
  1.0)

(defn- dmx-steps
  "The captured 16-bit dimmer in 8-bit DMX steps."
  [packet]
  (* 255.0 (/ (+ (* 256 (bit-and 255 (nth packet 18))) (bit-and 255 (nth packet 19))) 65535.0)))

(defn- harness []
  (let [packets (atom [])
        sink {:send! (fn [universes _] (swap! packets conj (vec (artnet/artdmx-packet 1 1 (get universes 1)))))}]
    {:rt (show/create-runtime {:model (live-model) :sink sink :workspace-options nil}) :packets packets}))

(defn- at! [rt time payload]
  (let [reply (show/submit! rt :command payload nil)]
    (show/tick! rt time)
    (deref reply 1000 ::missing)))

(defn- ramps [rt] (get-in @(:state rt) [:model :event-subscribers :ramps]))

(def ^:private frame-ms 25)

(def ^:private handover-ms 1250)

(def ^:private end-ms
  "The superseding ramp spans 4 beats (2000 ms at 120 BPM) from the handover."
  3250)

(defn- span [unit] (if (= :ms unit) {:ms 2000} {:beats 4}))

(defn- scenario
  "Ramp the level 0 -> 1 over 4 beats (or 2000 ms) with ease-in-out from beat
  0.5, and at beat 2.5, its midpoint and fastest point, supersede it with a
  ramp to 0.2 over 4 beats (or 2000 ms) on curve. Frames every 25 ms."
  [old-unit new-unit curve]
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))
            frame (fn [t] [t (dmx-steps (peek @packets))])
            up (at! rt 250 {:op :param/ramp :event-id "up" :instance-id id :param :level :to 1.0
                            :over (span old-unit) :curve :ease-in-out :source poster})
            before (mapv (fn [t] (show/tick! rt t) (frame t)) (range 275 handover-ms frame-ms))
            down (at! rt handover-ms {:op :param/ramp :event-id "down" :instance-id id :param :level :to 0.2
                                      :over (span new-unit) :curve curve :source poster})
            handover (frame handover-ms)
            ramp (first (ramps rt))
            view (show/timeline rt {:bars 2})
            backup (files/freeze (:model @(:state rt)))
            after (mapv (fn [t] (show/tick! rt t) (frame t)) (range (+ handover-ms frame-ms) 3600 frame-ms))]
        {:up up :down down :ramp ramp :backup backup
         :shape (:shape (some #(when (= "down" (:event-id %)) %) (:ramps view)))
         :frames (vec (concat before [handover] after))
         :level (get-in @(:state rt) [:model :instances id :params :level])
         :recent (get-in @(:state rt) [:model :event-subscribers :recent])
         :packets @packets})
      (finally (show/stop! rt)))))

(defn- first-differences [frames] (mapv - (rest (map second frames)) (map second frames)))

(defn- kink
  "The largest change in first difference on the frames around the handover:
  a velocity jump shows here, as smooth curvature is far below a DMX step."
  [frames]
  (let [index (first (keep-indexed #(when (= handover-ms (first %2)) %1) frames))
        d (first-differences frames)]
    (apply max (for [i (range (- index 3) (+ index 3))] (Math/abs (double (- (d (inc i)) (d i))))))))

(def ^:private pairs
  [[:beats :beats :linear] [:beats :ms :smoothstep] [:ms :beats :ease-out] [:ms :ms :linear]])

(deftest a-superseding-ramp-carries-the-velocity-across-beat-and-millisecond-pairs
  (doseq [[old-unit new-unit curve :as pair] pairs]
    (let [{:keys [up down ramp shape frames level recent]} (scenario old-unit new-unit curve)
          context (pr-str pair)]
      (is (= [:accepted :accepted] [(:status up) (:status down)]) context)
      (testing (str "the takeover keeps position and velocity " context)
        (is (< (Math/abs (- 0.5 (:from ramp))) 1.0e-9) "it starts where the outgoing ramp was")
        ;; Ease-in-out over 4 beats is moving at 2 x 1/4 per beat at its midpoint;
        ;; the newcomer spans 4 beats, so it leaves at 2.0 per unit of its progress.
        (is (< (Math/abs (- 2.0 (:handover ramp))) 1.0e-6) (pr-str (:handover ramp)))
        (is (<= (kink frames) step-tolerance)
            (str "no first-difference jump beyond one DMX step at the handover: " (kink frames))))
      (testing (str "the target is reached exactly at the planned end " context)
        (is (= 0.2 level))
        (is (= 0.2 (:value (some #(when (and (= "down" (:event-id %)) (= :completed (:status %))) %) recent))))
        (is (every? #(< (Math/abs (- (second %) (* 255.0 (/ 13107 65535.0)))) 1.0e-9)
                    (filter #(>= (first %) end-ms) frames))
            "every frame from the planned end shows the exact 16-bit target")
        (is (> (second (some #(when (= (- end-ms frame-ms) (first %)) %) frames)) 51.0)
            "the frame before the planned end has not arrived yet"))
      (testing (str "the timeline :shape is the sampled output, handover term included " context)
        (is (= 17 (count shape)))
        (doseq [i (range 17)
                :let [t (+ handover-ms (* 125 i))
                      sampled (/ (second (some #(when (= t (first %)) %) frames)) 255.0)
                      drawn (+ 0.5 (* (shape i) (- 0.2 0.5)))]]
          (is (< (Math/abs (- sampled drawn)) 2.0e-4) (pr-str {:i i :sampled sampled :drawn drawn})))
        (is (some #(< % 0.0) shape) "the carried velocity rises above the start before the ramp falls")))))

(deftest without-the-carried-velocity-the-same-handover-kinks
  ;; The control for the tolerance above: the position-only takeover from #893.
  (with-redefs [subscribers/takeover (fn [model previous _action _schema beat now]
                                       (when previous {:from (second (#'subscribers/sample-ramp model previous beat now))}))]
    (doseq [[old-unit new-unit curve] pairs]
      (is (> (kink (:frames (scenario old-unit new-unit curve))) 5.0) (pr-str [old-unit new-unit curve])))))

(deftest a-handover-replays-byte-identically-and-survives-a-backup
  (doseq [[old-unit new-unit curve] pairs]
    (let [one (scenario old-unit new-unit curve)
          two (scenario old-unit new-unit curve)]
      (is (= (:packets one) (:packets two)) "replaying the same command log gives identical ArtDmx output")
      (let [restored (files/thaw (live-model) (:backup one))
            ramp (first (get-in restored [:event-subscribers :ramps]))]
        (is (= (select-keys (:ramp one) [:from :to :curve :handover :unit :start :end])
               (select-keys ramp [:from :to :curve :handover :unit :start :end]))
            "the backup keeps the velocity the ramp took over with")))))

(defn- simple-scenario
  "A ramp on param from 'from' to mid over 4 beats, superseded at beat 2.5 by a
  ramp to 'to' over 2 beats. Returns the new ramp."
  [param first-to curve second-to]
  (let [{:keys [rt]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))]
        (at! rt 250 {:op :param/ramp :event-id "a" :instance-id id :param param :to first-to
                     :over {:beats 4} :curve curve :source poster})
        (doseq [t (range 275 handover-ms frame-ms)] (show/tick! rt t))
        (at! rt handover-ms {:op :param/ramp :event-id "b" :instance-id id :param param :to second-to
                             :over {:beats 2} :source poster})
        (first (ramps rt)))
      (finally (show/stop! rt)))))

(deftest colours-and-integers-carry-velocity-per-component-on-the-continuous-track
  (let [ramp (simple-scenario :tint [1.0 0.5 0.0] :linear [0.0 0.0 1.0])]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1.0e-9) [0.5 0.25 0.0] (:from ramp))))
    ;; Linear over 4 beats at [0.25 0.125 0] per beat, into a 2-beat ramp.
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1.0e-9) [0.5 0.25 0.0] (:handover ramp))) (pr-str (:handover ramp))))
  (let [ramp (simple-scenario :steps 20 :linear 0)]
    (is (= 10 (:from ramp)) "an integer starts from its rounded value")
    (is (< (Math/abs (- 10.0 (:handover ramp))) 1.0e-9) "and carries its unrounded velocity: 5 per beat over 2 beats"))
  (testing "a curve that cannot start with a finite slope keeps its authored start"
    (let [{:keys [rt]} (harness)]
      (try
        (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))]
          (at! rt 250 {:op :param/ramp :event-id "a" :instance-id id :param :level :to 1.0 :over {:beats 4} :source poster})
          (at! rt 750 {:op :param/ramp :event-id "b" :instance-id id :param :level :to 0.0 :over {:beats 4}
                       :curve {:type :power :exponent 0.5} :source poster})
          (is (not (contains? (first (ramps rt)) :handover))))
        (finally (show/stop! rt))))))

(deftest a-value-held-at-its-bound-carries-no-velocity
  ;; a's midpoint moves at 1 per beat; b, to 0.9 over 4 beats, carries that
  ;; into an overshoot that the [0 1] bounds hold at 1.0 by beat 2.5, where c
  ;; takes over: the held level is not moving, so c starts at rest.
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))
            ramp! (fn [t event-id to beats curve]
                    (at! rt t {:op :param/ramp :event-id event-id :instance-id id :param :level :to to
                               :over {:beats beats} :curve curve :source poster}))]
        (ramp! 250 "a" 1.0 2 :ease-in-out)
        (doseq [t (range 275 750 frame-ms)] (show/tick! rt t))
        (ramp! 750 "b" 0.9 4 :linear)
        (is (< (Math/abs (- 4.0 (:handover (first (ramps rt))))) 1.0e-9))
        (doseq [t (range 775 handover-ms frame-ms)] (show/tick! rt t))
        (ramp! handover-ms "c" 0.0 2 :linear)
        (let [c (first (ramps rt))]
          (is (= 255.0 (dmx-steps (peek @packets))) "b is held at the upper bound")
          (is (= [1.0 0.0] [(:from c) (:handover c)]) (pr-str c))))
      (finally (show/stop! rt)))))
