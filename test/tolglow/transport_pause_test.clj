(ns tolglow.transport-pause-test
  "Transport stop pauses every change over time (#699). Each scenario starts a
  change through the real show owner, stops the transport part-way, holds the
  stop, plays again and classifies the captured DMX (or the owned state that a
  scenario names) as :frozen, :keeps-running or :jumps."
  (:require [afterglow.control.model :as control]
            [afterglow.reactive :as graph]
            [clojure.test :refer [deftest is testing]]
            [tolglow.show :as show]))

(def ^:private frame-ms 100)

(def ^:private stop-at 2000)

(def ^:private play-at 7000)

(def ^:private end-at 11000)

(def ^:private level-param {:level {:type :number :default 1.0 :bounds [0.0 1.0]}})

(defn- effect [id outputs & {:as extra}]
  (merge {:id id :version 1 :category :light :params level-param :outputs outputs} extra))

(def ^:private effects
  {:wash (effect :wash {:dimmer {:op :param :key :level}})
   :ms-attack (effect :ms-attack {:dimmer {:op :param :key :level}} :envelope {:attack {:ms 4000}})
   :ms-release (effect :ms-release {:dimmer {:op :param :key :level}} :envelope {:release {:ms 4000}})
   :beat-attack (effect :beat-attack {:dimmer {:op :param :key :level}} :envelope {:attack {:beats 8}})
   :ms-tween (effect :ms-tween {:dimmer {:op :tween :spec {:keyframes [{:at {:ms 0} :value 0.0}
                                                                      {:at {:ms 4000} :value 1.0}]}}})
   :ms-clock (effect :ms-clock {:dimmer {:op :min :args [{:op :divide :args [{:op :time} {:op :const :value 4000.0}]}
                                                         {:op :const :value 1.0}]}})
   :quarter (effect :quarter {:dimmer 0.25})
   :three-quarter (effect :three-quarter {:dimmer 0.75})})

(def ^:private cues
  (into {} (map (fn [id] [id {:id id :effect-id id :version 1}])) (keys effects)))

(defn- live-model []
  (assoc (control/new-show {:show-id :pause-test :transport {:playing? true :beat 0.0 :bpm 120.0}
                            :registry {:effects effects :cues cues}})
         :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}))

(defn- runtime []
  (let [dmx (atom nil)
        sink {:send! (fn [universes _] (reset! dmx (bit-and 255 (first (get universes 1)))))}]
    (assoc (show/create-runtime {:model (live-model) :sink sink :workspace-options nil}) ::dmx dmx)))

(defn- tick! [rt t]
  (show/tick! rt t)
  (is (nil? (get-in @(:state rt) [:status :error])) (pr-str (:status @(:state rt)))))

(defn- submit! [rt t type payload]
  (let [reply (show/submit! rt type payload)]
    (tick! rt t)
    (let [ack (deref reply 1000 ::missing)]
      (is (= :accepted (:status ack)) (pr-str [type payload ack]))
      ack)))

(defn- command! [rt t payload] (submit! rt t :command payload))

(defn- dmx [rt] @(::dmx rt))

(defn- model [rt] (:model @(:state rt)))

(defn- set-playing! [rt t playing?]
  (command! rt t {:op :transport/set :transport {:playing? playing?}}))

(defn run-scenario
  "Tick 100 ms frames from 0, stop at 2 s, hold until 7 s, play, run to 11 s.
  setup! runs at t=0 (and may submit later actions through :at, a map of time to
  function). Returns every frame as [t value]."
  [rt {:keys [setup! at sample] :or {sample dmx at {}}}]
  (setup! rt)
  (into [[0 (sample rt)]]
        (for [t (range frame-ms (inc end-at) frame-ms)]
          (do (cond
                (= t stop-at) (set-playing! rt t false)
                (= t play-at) (set-playing! rt t true)
                (contains? at t) ((get at t) rt t)
                :else (tick! rt t))
              [t (sample rt)]))))

(defn- value-at [frames t] (some (fn [[ft v]] (when (= ft t) v)) frames))

(defn classify
  "How a change behaved across the stop: :keeps-running when any held frame
  differs from the stop frame, :jumps when the first frame after play moves by
  more than two and a half frames of the normal progress just before the stop
  (and at least three steps), else :frozen."
  [frames]
  (let [stopped (value-at frames stop-at)
        ;; Normal progress per frame shortly before the stop.
        rate (apply max (map (fn [t] (Math/abs (double (- (value-at frames t) (value-at frames (- t frame-ms))))))
                             (range (- stop-at 400) (inc stop-at) frame-ms)))
        held (keep (fn [[t v]] (when (< stop-at t play-at) v)) frames)
        resumed (value-at frames (+ play-at frame-ms))]
    (cond
      (some #(not= stopped %) held) :keeps-running
      (> (Math/abs (double (- resumed stopped))) (max 3.0 (* 2.5 rate))) :jumps
      :else :frozen)))

(defn- launch! [rt t cue & [launch]]
  (:instance-id (command! rt t (cond-> {:op :cue/launch :cue-id cue} launch (assoc :launch launch)))))

(defn- scenario [setup]
  (let [rt (runtime)]
    (try
      (let [frames (run-scenario rt setup)]
        {:frames frames :class (classify frames) :model (model rt)})
      (finally (show/stop! rt)))))

(defn- rising-after-play? [frames]
  (< (value-at frames (+ play-at frame-ms)) (value-at frames (+ play-at 1000))))

(deftest beat-ramp-freezes-on-stop-and-continues-on-play
  (let [{:keys [frames class]}
        (scenario {:setup! (fn [rt]
                             (let [id (launch! rt 0 :wash {:params {:level 0.0}})]
                               (command! rt 0 {:op :param/ramp :event-id "beat-swell" :instance-id id :param :level
                                               :to 1.0 :over {:beats 8}})))})]
    (is (= :frozen class) (pr-str frames))
    (is (rising-after-play? frames))))

(deftest millisecond-ramp-pauses-with-the-transport
  (let [{:keys [frames class]}
        (scenario {:setup! (fn [rt]
                             (let [id (launch! rt 0 :wash {:params {:level 0.0}})]
                               (command! rt 0 {:op :param/ramp :event-id "ms-swell" :instance-id id :param :level
                                               :to 1.0 :over {:ms 4000}})))})]
    (is (= :keeps-running class) (pr-str frames))))

(deftest millisecond-system-attack-pauses-with-the-transport
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :wash {:system {:attack {:ms 4000}}})})]
    (is (= :keeps-running class) (pr-str frames))))

(deftest millisecond-effect-envelope-attack-pauses-with-the-transport
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :ms-attack)})]
    (is (= :keeps-running class) (pr-str frames))))

(deftest beat-effect-envelope-attack-freezes-on-stop
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :beat-attack)})]
    (is (= :frozen class) (pr-str frames))
    (is (rising-after-play? frames))))

(deftest millisecond-release-pauses-with-the-transport
  (let [id (atom nil)
        {:keys [frames class model]}
        (scenario {:setup! #(reset! id (launch! % 0 :ms-release))
                   :at {1000 (fn [rt t] (command! rt t {:op :instance/end :instance-id @id}))}})]
    (is (= :keeps-running class) (pr-str frames))
    (is (nil? (get-in model [:instances @id])) "the release finished by the end of the run")))

(deftest millisecond-timed-launch-pauses-with-the-transport
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :wash {:mode :timed :duration {:ms 3000}})})]
    (is (= :keeps-running class) (pr-str frames))))

(deftest millisecond-output-tween-pauses-with-the-transport
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :ms-tween)})]
    (is (= :keeps-running class) (pr-str frames))))

(deftest millisecond-time-expression-pauses-with-the-transport
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :ms-clock)})]
    (is (= :keeps-running class) (pr-str frames))))

(deftest millisecond-response-envelope-pauses-with-the-transport
  (let [{:keys [frames class]}
        (scenario {:setup! (fn [rt]
                             (launch! rt 0 :wash)
                             (submit! rt 0 :response {:id :swell :params [:level] :operation :multiply
                                                      :envelope {:attack {:ms 4000}}}))})]
    (is (= :keeps-running class) (pr-str frames))))

(deftest millisecond-sequence-pauses-with-the-transport
  (let [{:keys [frames class]}
        (scenario {:setup! (fn [rt]
                             (submit! rt 0 :composition {:id :steps :mode :sequence :loop :once :timebase :ms
                                                         :slots [{:id :a :cue-id :quarter :duration-ms 3000}
                                                                 {:id :b :cue-id :three-quarter :duration-ms 3000}]})
                             (submit! rt 0 :composition-run {:id :steps}))})]
    (is (= :frozen class) (pr-str frames))
    (testing "the first slot keeps its remaining second after play"
      (is (= 64 (value-at frames (- (+ play-at 1000) frame-ms))) (pr-str frames))
      (is (= 191 (value-at frames (+ play-at 1100))) (pr-str frames)))))

(deftest millisecond-auto-planner-pauses-with-the-transport
  (let [{:keys [frames class]}
        (scenario {:setup! #(submit! % 0 :auto {:enabled? true :seed 3 :scope {:cue-ids [:quarter :three-quarter]}
                                                :interval {:ms 1000} :count 1})
                   :sample #(get-in (model %) [:auto :state :tick] 0)})]
    (is (= :keeps-running class) (pr-str frames))))

(def ^:private spatial-model
  (assoc (control/new-show {:show-id :pause-spatial :transport {:playing? true :beat 0.0 :bpm 120.0}})
         :rig {:fixtures [{:id :left :position [0 1 0] :universe 1 :address 1 :channels {:red 1 :green 2 :blue 3}}]}))

(deftest spatial-ball-freezes-on-stop-and-continues-on-play
  (let [rt (show/create-runtime {:model spatial-model :sink {:send! (fn [_ _])} :workspace-options nil})
        x #(or (some-> (get-in (show/snapshot %) [:spatial :signals :moving :position]) first (* 1000.0) Math/round) 0)]
    (try
      (submit! rt 0 :settings {:scope :spatial
                               :values {:enabled? true :gravity [0 0 0]
                                        :volumes {:moving {:shape :ball :radius 0.1 :position [0 1 0]
                                                           :velocity [0.2 0 0]}}}})
      (let [lease (graph/acquire! (show/reactions rt) [:spatial/body :moving] (fn [_]))]
        (try
          (let [frames (run-scenario rt {:setup! (fn [_]) :sample x})]
            (is (= :frozen (classify frames)) (pr-str frames))
            (is (rising-after-play? frames)))
          (finally ((:close! lease)))))
      (finally (show/stop! rt)))))
