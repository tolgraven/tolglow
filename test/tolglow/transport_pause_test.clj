(ns tolglow.transport-pause-test
  "Transport stop pauses every change over time (#699). Each scenario starts a
  change through the real show owner, stops the transport part-way, holds the
  stop, plays again and classifies the captured DMX (or the owned state that a
  scenario names) as :frozen, :keeps-running or :jumps."
  (:require [afterglow.control.model :as control]
            [afterglow.reactive :as graph]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.persistence :as files]
            [tolglow.show :as show])
  (:import [java.net InetAddress ServerSocket]))

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
   :modulator {:id :modulator :version 1 :params {} :modulation {:params [:level] :ranges {:level [0.6 0.6]}}}
   :quarter (effect :quarter {:dimmer 0.25})
   :three-quarter (effect :three-quarter {:dimmer 0.75})})

(def ^:private cues
  (into {} (map (fn [id] [id {:id id :effect-id id :version 1}])) (keys effects)))

(def ^:private tempos
  "Real show tempos run from 70 to 180 BPM."
  [70.0 120.0 180.0])

(defn- live-model [bpm]
  (assoc (control/new-show {:show-id :pause-test :transport {:playing? true :beat 0.0 :bpm bpm}
                            :registry {:effects effects :cues cues}})
         :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}))

(defn- runtime
  ([] (runtime 120.0))
  ([bpm] (runtime bpm (live-model bpm)))
  ([_bpm model]
   (let [dmx (atom nil)
         sink {:send! (fn [universes _] (reset! dmx (bit-and 255 (first (get universes 1)))))}]
     (assoc (show/create-runtime {:model model :sink sink :workspace-options nil}) ::dmx dmx))))

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
  differs from the stop frame, :jumps when the stop frame or the first frame
  after play moves by more than two and a half frames of the normal progress
  just before the stop (and at least three steps), else :frozen."
  [frames]
  (let [stopped (value-at frames stop-at)
        ;; Normal progress per frame shortly before the stop, excluding the
        ;; stop frame itself so a jump on that edge cannot hide in it.
        rate (apply max (map (fn [t] (Math/abs (double (- (value-at frames t) (value-at frames (- t frame-ms))))))
                             (range (- stop-at 400) stop-at frame-ms)))
        limit (max 3.0 (* 2.5 rate))
        held (keep (fn [[t v]] (when (< stop-at t play-at) v)) frames)
        resumed (value-at frames (+ play-at frame-ms))]
    (cond
      (> (Math/abs (double (- stopped (value-at frames (- stop-at frame-ms))))) limit) :jumps
      (some #(not= stopped %) held) :keeps-running
      (> (Math/abs (double (- resumed stopped))) limit) :jumps
      :else :frozen)))

(defn run-continuous
  "The same frames as run-scenario for a show that never stops."
  [rt {:keys [setup! at sample] :or {sample dmx at {}}}]
  (setup! rt)
  (into [[0 (sample rt)]]
        (for [t (range frame-ms (inc end-at) frame-ms)]
          (do (if (contains? at t) ((get at t) rt t) (tick! rt t))
              [t (sample rt)]))))

(defn resumes-exactly?
  "Every frame after play equals the frame of a never-stopping twin that many
  milliseconds after the stop, and every frame up to the stop matches too: the
  stop removed exactly the held interval."
  [frames twin]
  (let [held (- play-at stop-at)]
    (and (every? (fn [[t v]] (= v (value-at twin t))) (filter #(<= (first %) stop-at) frames))
         (every? (fn [[t v]] (= v (value-at twin (- t held)))) (filter #(>= (first %) play-at) frames)))))

(defn- launch! [rt t cue & [launch]]
  (:instance-id (command! rt t (cond-> {:op :cue/launch :cue-id cue} launch (assoc :launch launch)))))

(defn- scenario [setup & [bpm]]
  (let [rt (runtime (or bpm 120.0))]
    (try
      (let [frames (run-scenario rt setup)]
        {:frames frames :class (classify frames) :model (model rt)})
      (finally (show/stop! rt)))))

(defn- rising-after-play? [frames]
  (< (value-at frames (+ play-at frame-ms)) (value-at frames (+ play-at 1000))))

(defn- falling-after-play? [frames]
  (> (value-at frames (+ play-at frame-ms)) (value-at frames (+ play-at 1000))))

(defn- ramp! [rt unit]
  (let [id (launch! rt 0 :wash {:params {:level 0.0}})]
    (command! rt 0 {:op :param/ramp :event-id (str "swell-" (name (key (first unit)))) :instance-id id
                    :param :level :to 1.0 :over unit})))

(deftest beat-ramp-freezes-on-stop-and-continues-on-play
  (doseq [bpm tempos]
    (let [{:keys [frames class]} (scenario {:setup! #(ramp! % {:beats 8})} bpm)]
      (is (= :frozen class) (pr-str bpm frames))
      (is (rising-after-play? frames) (pr-str bpm)))))

(deftest millisecond-ramp-pauses-with-the-transport
  (doseq [bpm tempos]
    (let [{:keys [frames class]} (scenario {:setup! #(ramp! % {:ms 4000})} bpm)]
      (is (= :frozen class) (pr-str bpm frames))
      (is (rising-after-play? frames) (pr-str bpm))
      (testing "the ramp keeps its remaining two seconds after play"
        (is (< (value-at frames (+ play-at 1900)) 255) (pr-str frames))
        (is (= 255 (value-at frames (+ play-at 2100))) (pr-str frames))))))

(deftest millisecond-system-attack-pauses-with-the-transport
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :wash {:system {:attack {:ms 4000}}})})]
    (is (= :frozen class) (pr-str frames))
    (is (rising-after-play? frames))))

(deftest millisecond-effect-envelope-attack-pauses-with-the-transport
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :ms-attack)})]
    (is (= :frozen class) (pr-str frames))
    (is (rising-after-play? frames))))

(deftest beat-effect-envelope-attack-freezes-on-stop
  (doseq [bpm tempos]
    (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :beat-attack)} bpm)]
      (is (= :frozen class) (pr-str bpm frames))
      (is (rising-after-play? frames) (pr-str bpm)))))

(deftest millisecond-release-pauses-with-the-transport
  (let [id (atom nil)
        rt (runtime)
        instance #(get-in (model rt) [:instances @id])]
    (try
      (let [frames (run-scenario rt {:setup! #(reset! id (launch! % 0 :ms-release))
                                     :at {1000 (fn [rt t] (command! rt t {:op :instance/end :instance-id @id}))
                                          9900 (fn [rt t] (tick! rt t) (is (some? (instance)) "three seconds of release remain after play"))}})]
        (is (= :frozen (classify frames)) (pr-str frames))
        (is (falling-after-play? frames))
        (is (nil? (instance)) "the release still finishes"))
      (finally (show/stop! rt)))))

(deftest millisecond-timed-launch-pauses-with-the-transport
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :wash {:mode :timed :duration {:ms 3000}})})]
    (is (= :frozen class) (pr-str frames))
    (testing "the remaining second runs after play"
      (is (= 255 (value-at frames (+ play-at 900))) (pr-str frames))
      (is (= 0 (value-at frames (+ play-at 1100))) (pr-str frames)))))

(deftest millisecond-output-tween-pauses-with-the-transport
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :ms-tween)})]
    (is (= :frozen class) (pr-str frames))
    (is (rising-after-play? frames))))

(deftest millisecond-time-expression-pauses-with-the-transport
  (let [{:keys [frames class]} (scenario {:setup! #(launch! % 0 :ms-clock)})]
    (is (= :frozen class) (pr-str frames))
    (is (rising-after-play? frames))))

(deftest millisecond-response-envelope-pauses-with-the-transport
  (let [{:keys [frames class]}
        (scenario {:setup! (fn [rt]
                             (launch! rt 0 :wash)
                             (submit! rt 0 :response {:id :swell :params [:level] :operation :multiply
                                                      :envelope {:attack {:ms 4000}}}))})]
    (is (= :frozen class) (pr-str frames))
    (is (rising-after-play? frames))))

(deftest a-response-triggered-after-a-stop-starts-at-once-on-held-instances
  (let [rt (runtime)]
    (try
      (launch! rt 0 :wash)
      (set-playing! rt 500 false)
      (tick! rt 3000)
      (set-playing! rt 3000 true)
      (submit! rt 3100 :response {:id :swell :params [:level] :operation :multiply :envelope {:attack {:ms 1000}}})
      (tick! rt 3600)
      (is (< 100 (dmx rt) 155) "half way through the attack, not delayed by the held instance clock")
      (finally (show/stop! rt)))))

(deftest a-timed-modulation-owner-keeps-its-controller-through-the-stop
  (let [{:keys [frames class]}
        (scenario {:setup! (fn [rt]
                             (launch! rt 0 :wash)
                             (launch! rt 0 :modulator {:mode :one-shot :duration {:ms 3000}}))})]
    (is (= :frozen class) (pr-str frames))
    (is (= 153 (value-at frames (+ play-at 900))) (pr-str frames))
    (is (= 255 (value-at frames (+ play-at 1100))) (pr-str frames))))

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
    (is (= :frozen class) (pr-str frames))
    (is (rising-after-play? frames))))

(deftest an-explicit-instance-pause-spanning-the-stop-resumes-where-it-paused
  (let [rt (runtime) id (atom nil)]
    (try
      (reset! id (launch! rt 0 :ms-tween))
      (tick! rt 1000)
      (command! rt 1000 {:op :instance/pause :instance-id @id})
      (let [paused (dmx rt)]
        (set-playing! rt 2000 false)
        (tick! rt 5000)
        (set-playing! rt 5000 true)
        (tick! rt 6000)
        (is (= paused (dmx rt)) "still explicitly paused")
        (command! rt 6000 {:op :instance/resume :instance-id @id})
        (tick! rt 6100)
        (is (< paused (dmx rt) (+ paused 12)) (pr-str paused (dmx rt))))
      (finally (show/stop! rt)))))

(deftest an-explicitly-paused-instance-holds-its-response-across-the-stop
  (let [rt (runtime) id (atom nil)]
    (try
      (reset! id (launch! rt 0 :wash))
      (submit! rt 0 :response {:id :swell :params [:level] :operation :multiply :envelope {:attack {:ms 4000}}})
      (tick! rt 1000)
      (command! rt 1000 {:op :instance/pause :instance-id @id})
      (let [paused (dmx rt)]
        (set-playing! rt 2000 false)
        (tick! rt 5000)
        (set-playing! rt 5000 true)
        (tick! rt 6000)
        (is (= paused (dmx rt)))
        (command! rt 6000 {:op :instance/resume :instance-id @id})
        (tick! rt 6100)
        (is (< paused (dmx rt) (+ paused 12)) (pr-str paused (dmx rt))))
      (finally (show/stop! rt)))))

(deftest a-save-made-during-the-stop-resumes-from-the-held-position
  (let [rt (runtime)]
    (try
      (launch! rt 0 :ms-tween)
      (doseq [t (range 100 2000 100)] (tick! rt t))
      (set-playing! rt 2000 false)
      (let [stopped (dmx rt)]
        (tick! rt 4000)
        (let [{saved :model frame-output :frame-output} (show/export-show rt)
              restored (runtime 120.0)]
          (try
            (submit! restored 0 :import {:source (files/write-string saved frame-output)})
            (doseq [t (range 4100 7000 100)] (tick! restored t))
            (is (= stopped (dmx restored)) "the restored show is still held")
            (set-playing! restored 7000 true)
            (tick! restored 7100)
            (is (< stopped (dmx restored) (+ stopped 12)) (pr-str stopped (dmx restored)))
            (finally (show/stop! restored)))))
      (finally (show/stop! rt)))))

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

(deftest a-response-saved-during-the-stop-keeps-its-held-count-after-import
  (let [rt (runtime)
        held-count #(get-in (first (vals (:instances %))) [:system :transport-held-ms])]
    (try
      (launch! rt 0 :wash)
      (submit! rt 0 :response {:id :swell :params [:level] :operation :multiply :envelope {:attack {:ms 4000}}})
      (doseq [t (range 100 2000 100)] (tick! rt t))
      (set-playing! rt 2000 false)
      (let [stopped (dmx rt)]
        (tick! rt 4000)
        (let [{saved :model frame-output :frame-output} (show/export-show rt)
              restored (runtime)]
          (is (= 2000 (held-count saved)))
          (try
            (submit! restored 0 :import {:source (files/write-string saved frame-output)})
            (is (= 2000 (held-count (model restored))) "the backup round trip keeps the held count")
            (doseq [t (range 4100 7000 100)] (tick! restored t))
            (is (= stopped (dmx restored)) "the restored response is still held")
            (set-playing! restored 7000 true)
            (tick! restored 7100)
            (is (<= stopped (dmx restored) (+ stopped 12)) (pr-str stopped (dmx restored)))
            (finally (show/stop! restored)))))
      (finally (show/stop! rt)))))

(deftest a-show-that-starts-stopped-holds-from-its-first-frame
  (let [rt (runtime 120.0 (assoc-in (live-model 120.0) [:transport :playing?] false))]
    (try
      (launch! rt 0 :ms-tween)
      (doseq [t (range 100 3001 100)] (tick! rt t))
      (is (= 0 (dmx rt)) "nothing moves before the first play")
      (set-playing! rt 3000 true)
      (tick! rt 5000)
      (is (< 120 (dmx rt) 135) "half of the tween after two seconds of play")
      (finally (show/stop! rt)))))

(defn- ramp-end [rt] (:end (first (get-in (model rt) [:event-subscribers :ramps]))))

(defn- ramped [rt]
  (let [id (launch! rt 0 :wash {:params {:level 0.0}})]
    (command! rt 0 {:op :param/ramp :event-id "r" :instance-id id :param :level :to 1.0 :over {:ms 4000}})
    (is (== 4000 (ramp-end rt)))))

(deftest irregular-repeated-and-very-long-stop-frames-hold-exactly-once
  (let [rt (runtime)]
    (try
      (ramped rt)
      (tick! rt 1000)
      (set-playing! rt 1000 false)
      (doseq [t [1000 1000 1001 1500 1500 1733 2999 3000 3000]] (tick! rt t))
      (is (== 6000 (ramp-end rt)) "held exactly the 2000 ms stopped so far")
      (testing "an earlier frame time cannot hold or release anything"
        (tick! rt 2500)
        (is (== 6000 (ramp-end rt))))
      (testing "a ten hour stop"
        (tick! rt (+ 3000 (* 10 3600 1000)))
        (is (== (+ 6000 (* 10 3600 1000)) (ramp-end rt))))
      (finally (show/stop! rt)))))

(deftest stop-and-play-in-one-frame-hold-only-committed-stopped-intervals
  (let [rt (runtime)
        both! (fn [t first-playing? then-playing?]
                (let [a (show/submit! rt :command {:op :transport/set :transport {:playing? first-playing?}})
                      b (show/submit! rt :command {:op :transport/set :transport {:playing? then-playing?}})]
                  (tick! rt t)
                  (is (= [:accepted :accepted] (map (comp :status deref) [a b])))))]
    (try
      (ramped rt)
      (both! 500 false true)
      (tick! rt 1000)
      (is (== 4000 (ramp-end rt)) "a stop that never committed holds nothing")
      (set-playing! rt 1000 false)
      (both! 1300 true false)
      (tick! rt 1600)
      (is (== 4600 (ramp-end rt)) "a play that never committed releases nothing")
      (finally (show/stop! rt)))))

;; A pending hot reload on a held frame (#663): the candidate and the engine's
;; fallback both read the held model, so a rejected revision leaves the show
;; exactly as a twin that never published it.

(def ^:private reload-effects
  {:tween (assoc (:ms-tween effects) :id :tween)
   :wash (:wash effects)
   :modulator {:id :modulator :version 1 :params {}
               :modulation {:params [:level] :ranges {:level [0.6 0.6]} :select {:cue-ids [:wash]}}}})

(defn- reload-run [publications failing]
  (let [out (atom [])
        m (assoc (control/new-show {:show-id :pause-reload :transport {:playing? true :beat 0.0 :bpm 120.0}
                                    :registry {:effects reload-effects
                                               :cues (into {} (map (fn [id] [id {:id id :effect-id id :version 1
                                                                                 :reload-policy :preserve-state}]))
                                                           (keys reload-effects))}})
                 :rig {:fixtures [{:id :a :universe 1 :address 1 :channels {:dimmer 1}}
                                  {:id :b :universe 1 :address 2 :channels {:dimmer 1}}]})
        rt (cond-> (show/create-runtime {:model m :workspace-options nil :effect-log! (fn [_])
                                         :sink {:send! (fn [u _] (swap! out conj (mapv #(bit-and 255 %) (take 2 (get u 1)))))}})
             ;; Reject a revision at the compiler boundary, as tolglow.effects.modulation-reload-test does.
             failing (update :compile (fn [compile-effect]
                                        (fn [{:keys [id version] :as definition}]
                                          (if (= failing [id version])
                                            (throw (ex-info "Compiler rejected this revision" {:effect-id id}))
                                            (compile-effect definition))))))]
    (try
      (let [tween (launch! rt 0 :tween {:targets [:a]})
            wash (launch! rt 0 :wash {:targets [:b] :params {:level 0.0}})]
        (launch! rt 0 :modulator {:mode :one-shot :duration {:ms 3000}})
        (command! rt 0 {:op :param/ramp :event-id "r" :instance-id wash :param :level :to 1.0 :over {:ms 2500}})
        (vec (for [t (range 100 6001 100)]
               (do (cond
                     (= t 1000) (set-playing! rt t false)
                     (= t 3500) (set-playing! rt t true)
                     (contains? publications t)
                     (let [reply (show/submit! rt :definition {:kind :effects :definition (get publications t)})]
                       (tick! rt t)
                       (is (= :accepted (:status (deref reply 2000 nil)))))
                     :else (tick! rt t))
                   [t (peek @out) (get-in (model rt) [:instances tween :effect-version])]))))
      (finally (show/stop! rt)))))

(deftest a-reload-pending-on-a-held-frame-reads-the-held-clock-on-both-outcomes
  (let [control (reload-run {} nil)
        tween-2 (assoc (:tween reload-effects) :version 2)
        owner-2 (assoc-in (assoc (:modulator reload-effects) :version 2) [:modulation :ranges :level] [0.2 0.2])
        at (fn [frames t] (some (fn [[ft v]] (when (= ft t) v)) frames))
        diff (fn [a b] (pr-str (keep (fn [[[t x] [_ y]]] (when (not= x y) [t x y])) (map vector a b))))]
    (is (= 1 (count (set (keep (fn [[t v]] (when (< 1000 t 3500) v)) control)))) "the twin holds through the stop")
    (testing "the timed owner expires on its private clock: 3000 ms plus the 2500 ms stop"
      (is (= 153 (second (at control 5400))))
      (is (= 255 (second (at control 5600)))))
    (doseq [[label failing] [["rejected effect revision" [:tween 2]] ["accepted effect revision" nil]]]
      (testing label
        (let [frames (reload-run {2000 tween-2} failing)]
          (is (= (if failing 1 2) (last (peek frames))) "the reload was attempted")
          (is (= (map second control) (map second frames)) (diff control frames)))))
    (testing "a rejected owner revision samples the fallback controllers on the held frame"
      (let [frames (reload-run {2000 owner-2} [:modulator 2])]
        (is (= (map second control) (map second frames)) (diff control frames))))
    (testing "an accepted owner revision applies its range at once and still holds"
      (let [frames (reload-run {2000 owner-2} nil)]
        (is (= (map (comp first second) control) (map (comp first second) frames)) "the tween is untouched")
        (is (= #{51} (set (keep (fn [[t [_ b]]] (when (<= 2000 t 5400) b)) frames))))
        (is (= 255 (second (at frames 5600))))))))

;; The Room's private audition timeline replays the live holds.

(def ^:private ms-swell
  {:id :ms-swell :version 1 :outputs {:color {:op :const :value [1.0 1.0 1.0]} :dimmer {:op :const :value 1.0}}
   :envelope {:attack {:ms 2000}}})

(deftest an-audition-holds-millisecond-progress-with-the-live-show
  (let [start (fn [] (show/create-runtime {:workspace-options nil :audition {:max-live-share ##Inf} :model (demo/model)
                                           :sink {:send! (fn [_ _])}}))
        audition (start) applied (start)
        send! (fn [rt c] (show/submit! rt (assoc c :source {:adapter :repl :session-id "test"})))
        both! (fn [t] (doseq [rt [audition applied]] (show/tick! rt t) (is (show/await-audition rt 5000))))
        playing! (fn [t playing?]
                   (let [replies (mapv #(send! % {:op :transport/set :transport {:playing? playing?}}) [audition applied])]
                     (both! t)
                     (is (every? #(= :accepted (:status @%)) replies))))
        ops [{:op :registry/publish :kind :effects :definition ms-swell} {:op :effect/launch :effect-id :ms-swell}]
        sample (fn [t] (both! t) [t (show/snapshot audition) (show/snapshot applied)])]
    (try
      (run! both! (range 25 101 25))
      (let [staged (send! audition {:op :scene-draft/stage :draft-id "scene" :operations ops})
            _ (send! applied {:op :scene-draft/stage :draft-id "scene" :operations ops})
            taken (send! applied {:op :scene-draft/apply :draft-id "scene" :expected-draft-revision 0 :command-id "take"})
            frames (concat (doall (map sample (range 125 1000 25)))
                           (do (playing! 1000 false) [])
                           (doall (map sample (range 1025 3000 25)))
                           (do (playing! 3000 true) [])
                           (doall (map sample (range 3025 4500 25))))
            held (set (keep (fn [[t a]] (when (< 1000 t 3000) (get-in a [:draft-output :values :head-1 :emitted-color 0])))
                            frames))]
        (is (= [:accepted :accepted] (map (comp :status deref) [staged taken])))
        (doseq [[t a c] frames]
          (is (= (get-in c [:output :values]) (get-in a [:draft-output :values])) (str "frame " t)))
        (is (= 1 (count held)) (pr-str held))
        (is (< 0 (first held) 1) "held part-way through the attack"))
      (finally (show/stop! audition) (show/stop! applied)))))

;; Link play/stop following, through a loopback Carabiner peer.

(defn- eventually [pred]
  (let [deadline (+ (System/nanoTime) 3000000000)]
    (loop [] (or (pred) (when (< (System/nanoTime) deadline) (Thread/sleep 5) (recur))))))

(deftest a-link-stop-holds-millisecond-progress
  (with-open [server (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))]
    (.setSoTimeout server 3000)
    (let [rt (runtime)
          clock #(:clock (show/sync-action! rt :status {}))
          base {:peers 1 :start 123456 :bpm 120.0}
          run-for! (fn [ms] (let [deadline (+ (System/nanoTime) (* ms 1000000))]
                              (loop [] (show/tick! rt) (Thread/sleep 20) (when (< (System/nanoTime) deadline) (recur)))))]
      (try
        (show/sync-action! rt :configure {:config {:selected-source :ableton-link
                                                    :link {:enabled? true :follow-transport? true
                                                           :port (.getLocalPort server)}}})
        (show/sync-action! rt :start {})
        (with-open [socket (.accept server) reader (io/reader socket) writer (io/writer socket)]
          (.setSoTimeout socket 3000)
          (let [status! (fn [message] (.write writer (str "status " (pr-str message) "\n")) (.flush writer))]
            (is (= "enable-start-stop-sync" (.readLine reader)))
            (is (= "status" (.readLine reader)))
            (status! (assoc base :beat 0.0 :playing true))
            (is (eventually #(= :locked (:status (clock)))))
            (show/tick! rt)
            (let [launch (show/submit! rt :command {:op :cue/launch :cue-id :ms-tween})]
              (show/tick! rt)
              (is (= :accepted (:status (deref launch 1000 nil)))))
            (status! (assoc base :beat 1.0 :playing true))
            (run-for! 1000)
            (status! (assoc base :beat 2.0 :playing false))
            (is (eventually #(= :paused (:status (clock)))))
            (show/tick! rt)
            (is (false? (get-in (model rt) [:transport :playing?])))
            (let [stopped (dmx rt) held (atom #{})]
              (dotimes [_ 50] (show/tick! rt) (swap! held conj (dmx rt)) (Thread/sleep 20))
              (is (= #{stopped} @held) "frozen through the Link stop")
              (status! (assoc base :beat 2.0 :playing true))
              (is (eventually #(= :locked (:status (clock)))))
              (show/tick! rt)
              (is (true? (get-in (model rt) [:transport :playing?])))
              (is (<= stopped (dmx rt) (+ stopped 8)) "no catch-up on play")
              (run-for! 500)
              (is (< stopped (dmx rt)) "continues after play"))))
        (finally (show/stop! rt))))))

;; #702: show-level held time for global millisecond phases, and routing ADSR
;; envelopes in both units.

(defn- orb-model [bpm]
  (assoc (control/new-show {:show-id :pause-orb :transport {:playing? true :beat 0.0 :bpm bpm}})
         :rig {:fixtures [{:id :left :position [0 1 0] :universe 1 :address 1 :channels {:red 1 :green 2 :blue 3}}]}))

(defn- orb-runtime [bpm]
  (let [dmx (atom nil)]
    (assoc (show/create-runtime {:model (orb-model bpm) :workspace-options nil
                                 :sink {:send! (fn [universes _] (reset! dmx (bit-and 255 (first (get universes 1)))))}})
           ::dmx dmx)))

(def ^:private global-saw
  "A shared show-clock oscillator: :phase defaults to :global."
  {:waveform :saw :period {:ms 8000}})

(defn- spawn-orb! [rt t]
  (submit! rt t :spatial-cue {:op :spawn :id :orb :page-id :main :cell [0 0]
                              :volume {:shape :ball :position [0 1 0] :velocity [0 0 0] :gravity [0 0 0]
                                       :radius 0.5 :selection #{:position}
                                       :falloff {:inner-m 0.5 :outer-m 0.5 :curve :linear}}
                              :paint {:red 1.0} :sources {:red global-saw}}))

(defn- orb-scenario [bpm]
  (let [rt (orb-runtime bpm)]
    (try
      (let [frames (run-scenario rt {:setup! #(spawn-orb! % 0)})]
        {:frames frames :class (classify frames) :model (model rt) :snapshot (show/snapshot rt)})
      (finally (show/stop! rt)))))

(deftest a-global-millisecond-oscillator-pauses-with-the-transport
  (doseq [bpm tempos]
    (let [{:keys [frames class model snapshot]} (orb-scenario bpm)
          red #(value-at frames %)]
      (is (= :frozen class) (pr-str bpm frames))
      (is (rising-after-play? frames) (pr-str bpm))
      (testing "the saw reads show time: two seconds before the stop, then on from there"
        (is (= 64 (red stop-at)) (pr-str bpm frames))
        (is (= (red (- stop-at 1000)) (- (red (+ play-at 1000)) 64)) (pr-str bpm frames))
        (is (= 191 (red end-at)) "six seconds of show time at the end, not eleven"))
      (testing "the held coordinate is saved state and reaches the snapshot"
        (is (== (- play-at stop-at) (get-in model [:transport :held-ms])))
        (is (== (- play-at stop-at) (get-in snapshot [:transport :held-ms])))))))

(defn- route-spec [envelope]
  {:version 1 :id :pause-route :revision 1
   :sources {"fader" {:kind :control :range [0.0 1.0] :timeout {:beats 4096}}}
   :destinations {:level {:cue-id :wash :param :level :range [0.0 1.0] :base 0.0 :combine :max}}
   :routes [{:id :input :source "fader" :destination :level :envelope envelope}]})

(defn- gate! [rt t id on?]
  ((:events! (show/sync-callbacks rt))
   {:type :sync/observation
    :observation {:source-id "fader" :event-id id :value (if on? 1.0 0.0) :gate? on? :confidence 1.0
                  :observed-at-ms t :kind :control :origin :control}})
  (tick! rt t))

(defn- routed! [rt envelope]
  (submit! rt 0 :patch-save {:source (pr-str (route-spec envelope))})
  (submit! rt 0 :patch-enabled {:enabled? true})
  (launch! rt 0 :wash)
  (gate! rt 0 "on" true))

(defn- routed-scenario [bpm envelope & [at]]
  (scenario {:setup! #(routed! % envelope) :at (or at {})} bpm))

(deftest routing-envelope-attacks-pause-with-the-transport-in-both-units
  (doseq [bpm tempos
          envelope [{:attack {:ms 4000}} {:attack {:beats 8}}]]
    (let [{:keys [frames class]} (routed-scenario bpm envelope)]
      (is (< 0 (value-at frames stop-at) 255) (pr-str bpm envelope frames))
      (is (= :frozen class) (pr-str bpm envelope frames))
      (is (rising-after-play? frames) (pr-str bpm envelope)))))

(deftest routing-envelope-releases-pause-with-the-transport-in-both-units
  (doseq [bpm tempos
          envelope [{:attack {:ms 0} :release {:ms 4000}} {:attack {:beats 0} :release {:beats 8}}]]
    (let [{:keys [frames class]} (routed-scenario bpm envelope {1000 (fn [rt t] (gate! rt t "off" false))})]
      (is (< 0 (value-at frames stop-at) 255) (pr-str bpm envelope frames))
      (is (= :frozen class) (pr-str bpm envelope frames))
      (is (falling-after-play? frames) (pr-str bpm envelope))
      (when (:ms (:release envelope))
        (testing "the release keeps its remaining three seconds after play"
          (is (pos? (value-at frames (+ play-at 2900))) (pr-str frames))
          (is (zero? (value-at frames (+ play-at 3100))) (pr-str frames)))))))

(deftest a-gate-that-opens-during-the-stop-starts-its-attack-on-play
  (doseq [envelope [{:attack {:ms 1000}} {:attack {:beats 2}}]]
    (let [rt (runtime)]
      (try
        (submit! rt 0 :patch-save {:source (pr-str (route-spec envelope))})
        (submit! rt 0 :patch-enabled {:enabled? true})
        (launch! rt 0 :wash)
        (set-playing! rt 500 false)
        (gate! rt 1000 "on" true)
        (doseq [t (range 1100 3001 100)] (tick! rt t))
        (is (= 0 (dmx rt)) (pr-str envelope))
        (set-playing! rt 3000 true)
        (tick! rt 3500)
        (is (< 115 (dmx rt) 140) (pr-str envelope (dmx rt)))
        (finally (show/stop! rt))))))

(deftest the-held-show-clock-survives-a-backup-round-trip
  (let [rt (orb-runtime 120.0)]
    (try
      (spawn-orb! rt 0)
      (doseq [t (range 100 2000 100)] (tick! rt t))
      (set-playing! rt 2000 false)
      (let [stopped (dmx rt)]
        (tick! rt 4000)
        (let [{saved :model frame-output :frame-output} (show/export-show rt)
              restored (orb-runtime 120.0)]
          (is (== 2000 (get-in saved [:transport :held-ms])))
          (try
            (submit! restored 0 :import {:source (files/write-string saved frame-output)})
            (is (== 2000 (get-in (model restored) [:transport :held-ms])) "the import keeps the held count")
            (doseq [t (range 4100 7000 100)] (tick! restored t))
            (is (= stopped (dmx restored)) "the restored oscillator is still held")
            (set-playing! restored 7000 true)
            (tick! restored 7100)
            (is (<= stopped (dmx restored) (+ stopped 4)) (pr-str stopped (dmx restored)))
            (finally (show/stop! restored)))))
      (finally (show/stop! rt)))))

(deftest a-show-saved-without-a-held-count-reads-host-time-as-before
  (let [rt (orb-runtime 120.0)]
    (try
      (spawn-orb! rt 0)
      (doseq [t (range 100 2001 100)] (tick! rt t))
      (let [{saved :model frame-output :frame-output} (show/export-show rt)
            restored (orb-runtime 120.0)]
        (is (not (contains? (:transport saved) :held-ms)) "a show that never stopped saves no held count")
        (try
          (submit! restored 0 :import {:source (files/write-string saved frame-output)})
          (is (not (contains? (:transport (model restored)) :held-ms)))
          (tick! restored 4000)
          (is (= 128 (dmx restored)) "half of the eight second saw at four seconds of host time")
          (finally (show/stop! restored))))
      (finally (show/stop! rt)))))

(deftest a-saved-held-count-must-be-finite-and-nonnegative
  (let [saved (assoc-in (live-model 120.0) [:transport :held-ms] 2500.0)]
    (is (= 2500.0 (get-in (files/validate-model saved) [:transport :held-ms])))
    (doseq [bad [-1 ##Inf "2500"]]
      (is (thrown? clojure.lang.ExceptionInfo (files/validate-model (assoc-in saved [:transport :held-ms] bad)))
          (pr-str bad)))))

;; The energy conductor moves the show too: its target slew and its decision
;; cadence run on the held show clock.

(defn- conductor-runtime [bpm]
  (let [ids [:a :b :c :d]
        fixture (fn [id address] {:id id :universe 1 :address address :position [(* 0.5 (dec address)) 0 0]
                                  :channels {:dimmer 1} :groups [:stage] :max-watts 100
                                  :measured-relative-brightness 1.0})
        looks {:full {:id :full :version 1 :targets ids :params {} :outputs {:dimmer 1.0}}
               :conduct {:id :conduct :version 1 :targets [] :params {} :outputs {} :show-control? true}}]
    (show/create-runtime
      {:model (assoc (control/new-show {:show-id :pause-conductor :transport {:playing? true :beat 0.0 :bpm bpm}
                                        :registry {:effects looks
                                                   :cues (into {} (for [id (keys looks)] [id {:id id :version 1 :effect-id id}]))}})
                     :rig {:fixtures (mapv fixture ids (range 1 5))})
       :workspace-options nil})))

(defn- conduct! [rt settings]
  (launch! rt 0 :full)
  (submit! rt 0 :settings {:scope :autopilot
                           :values (merge {:enable-cue :conduct :scope {:group-id :stage} :strategy :instant :seed 71
                                           :targets {:brightness 0.5 :activity 0.23} :window {:bars 1}}
                                          settings)})
  (launch! rt 0 :conduct))

(defn- conductor-scenario [bpm settings sample]
  (let [rt (conductor-runtime bpm)]
    (try
      (let [frames (run-scenario rt {:setup! #(conduct! % settings) :sample sample})]
        {:frames frames :class (classify frames)})
      (finally (show/stop! rt)))))

(defn- slewed-brightness [rt]
  (Math/round (* 1000.0 (get-in (show/snapshot rt) [:autopilot :targets :brightness] 1.0))))

(defn- decisions [rt]
  (get-in (show/snapshot rt) [:autopilot :decision :decision] 0))

(deftest the-conductor-target-slew-pauses-with-the-transport
  (doseq [bpm tempos
          release [{:ms 4000} {:beats 8}]]
    (let [{:keys [frames class]} (conductor-scenario bpm {:interval {:beats 1} :target-response {:release release}}
                                                     slewed-brightness)]
      (is (< 500 (value-at frames stop-at) 1000) (pr-str bpm release frames))
      (is (= :frozen class) (pr-str bpm release frames))
      (is (falling-after-play? frames) (pr-str bpm release)))))

(deftest the-conductor-decides-on-show-time-and-not-while-stopped
  (doseq [bpm tempos]
    (testing "a millisecond cadence keeps its place: no decision is due on play"
      (let [{:keys [frames class]} (conductor-scenario bpm {:interval {:ms 1500}} decisions)
            stopped (value-at frames stop-at)]
        (is (= :frozen class) (pr-str bpm frames))
        ;; Decisions at 0 and 1.5 s; the next falls due at 3 s of show time, 8 s of host time.
        (is (= 2 stopped) (pr-str bpm frames))
        (is (= stopped (value-at frames (+ play-at 900))) (pr-str bpm frames))
        (is (= (inc stopped) (value-at frames (+ play-at 1000))) (pr-str bpm frames))))
    (testing "a musical cadence"
      (let [{:keys [frames class]} (conductor-scenario bpm {:interval {:beats 1}} decisions)]
        (is (= :frozen class) (pr-str bpm frames))
        (is (rising-after-play? frames) (pr-str bpm))))))

;; The fixture output response (attack/release of delivered light) follows show
;; time: a stop freezes it on both edges and play resumes it exactly.

(defn- response-run [bpm envelope continuous? & [release-at]]
  (let [rt (runtime bpm) id (atom nil)
        at (if release-at {release-at (fn [rt t] (command! rt t {:op :instance/remove :instance-id @id}))} {})
        setup {:setup! (fn [rt] (submit! rt 0 :settings {:scope :output-response :values {:enabled? true :global envelope}})
                         (reset! id (launch! rt 0 :wash)))
               :at at}]
    (try ((if continuous? run-continuous run-scenario) rt setup)
         (finally (show/stop! rt)))))

(deftest the-output-response-attack-pauses-with-the-transport
  (doseq [bpm tempos
          attack [{:ms 6000} {:ms 6000 :mode :linear} {:beats 12} {:blend 0.02}]]
    (let [frames (response-run bpm {:attack attack} false)
          twin (response-run bpm {:attack attack} true)]
      (is (< 0 (value-at frames stop-at) 255) (pr-str bpm attack frames))
      (is (= :frozen (classify frames)) (pr-str bpm attack frames))
      (is (rising-after-play? frames) (pr-str bpm attack))
      (is (resumes-exactly? frames twin) (pr-str bpm attack frames twin)))))

(deftest the-output-response-release-pauses-with-the-transport
  (doseq [bpm tempos
          release [{:ms 6000} {:ms 6000 :mode :linear} {:beats 12}]]
    (let [envelope {:attack {:ms 0} :release release}
          frames (response-run bpm envelope false 1000)
          twin (response-run bpm envelope true 1000)]
      (is (< 0 (value-at frames stop-at) 255) (pr-str bpm release frames))
      (is (= :frozen (classify frames)) (pr-str bpm release frames))
      (is (falling-after-play? frames) (pr-str bpm release))
      (is (resumes-exactly? frames twin) (pr-str bpm release frames twin)))))

(deftest the-ambient-pause-look-still-bypasses-the-output-response
  (let [rt (runtime)]
    (try
      (submit! rt 0 :settings {:scope :output :values {:pause-look {:mode :ambient :level 0.0}}})
      (submit! rt 0 :settings {:scope :output-response :values {:enabled? true :global {:attack {:ms 6000}}}})
      (launch! rt 0 :wash)
      (tick! rt 1900)
      (is (< 0 (dmx rt) 255))
      (set-playing! rt 2000 false)
      (is (= 0 (dmx rt)) "the ambient look (level zero here) replaces the shaped output; nothing is frozen")
      (finally (show/stop! rt)))))
