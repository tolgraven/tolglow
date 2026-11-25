(ns tolglow.event-subscribers-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [afterglow.sync.session :as sync-session]
            [tolglow.audio.capture-test :as pcm-test]
            [tolglow.connections :as connections]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.application :as app]
            [tolglow.clock :as clock]
            [tolglow.integrations.analysis-test :as audio-test]
            [tolglow.integrations.reference-position :as reference-position]
            [tolglow.demo :as demo]
            [tolglow.event-subscribers :as sub]
            [tolglow.events.callbacks :as callbacks]
            [tolglow.persistence :as files]
            [tolglow.transit :as wire])
  (:import [java.util.concurrent Callable CountDownLatch FutureTask ThreadPoolExecutor TimeUnit]))

(def kick {:category :audio :type :audio/kick :emitter {:kind :audio :id :analysis}
           :data {:confidence 0.92} :time {:beat 0}})
(def launch {:op :cue/launch :select {:ids [:ember :ice] :pick :random} :launch {:mode :one-shot}})
(defn listener [actions] {:id :kicks :enabled? true :match {:type :audio/kick} :actions actions})
(defn configured [actions] (sub/configure (demo/model) {:op :save :subscriber (listener actions)}))

(deftest bounded-registry-candidates-exclude-show-control-and-respect-page-and-ids
  (let [model (-> (demo/model)
                  (assoc-in [:registry :effects :code] {:id :code :role :show-control})
                  (assoc-in [:registry :cues :code] {:id :code :effect-id :ember :kind :code-cue})
                  (assoc-in [:registry :cues :also-code] {:id :also-code :effect-id :code})
                  (assoc-in [:pages :small :cells] {[1 1] :ember [1 2] :code}))]
    (is (= [:ember] (sub/candidates model {:op :cue/launch :select {:page-id :small}})))
    (is (= [:ember] (sub/candidates model {:op :effect/launch :select {:page-id :small}})))
    (is (not-any? #{:code :also-code} (sub/candidates model {:op :cue/launch})))
    (is (empty? (sub/candidates model {:op :cue/launch :select {:ids []}})))
    (is (empty? (sub/candidates model {:op :cue/launch :select {:categories []}})))
    (is (empty? (sub/candidates model {:op :cue/launch :select {:page-id :unknown}})))
    (is (= (set (sub/candidates model {:op :effect/launch}))
           (set (get-in (sub/catalog model) [:candidates :effects]))))))

(deftest matching-randomness-delay-replay-and-feedback
  (let [model (configured [launch {:op :instance/opacity :select {:pick :all} :value 0 :delay {:bars 1/2}}
                           {:op :instance/kill :select {:pick :all} :delay {:beats 1/2}}])
        admitted (sub/admit model [kick (assoc kick :type :audio/energy)
                                   (assoc kick :emitter {:kind :adapter :id :event-subscriber})
                                   (assoc kick :emitter {:kind :command :id :event-subscriber})])
        execute #(sub/advance % 0 {})
        started (execute (:model admitted))
        id (ffirst (:instances started))
        step #(sub/advance (assoc-in %1 [:transport :beat] %2) 10 {})]
    (is (= {0 [:kicks]} (:deliveries admitted)))
    (is (= started (execute (:model admitted))))
    (is (#{:ember :ice} (get-in started [:instances id :cue-id])))
    (is (= {:beats 1} (get-in started [:instances id :duration])))
    (is (= 1.0 (get-in (step started 1) [:instances id :system :opacity])))
    (is (zero? (get-in (step started 2) [:instances id :system :opacity])))
    (is (empty? (:instances (step started 5/2))))
    (let [rebased (clock/rebase started {:beat 100})]
      (is (== (+ 100 (get-in started [:event-subscribers :runs 0 :due-beat]))
             (get-in rebased [:event-subscribers :runs 0 :due-beat]))))
    (let [restored (files/thaw model (files/read-string (files/write-string started)))]
      (is (= (get-in started [:event-subscribers :runs]) (get-in restored [:event-subscribers :runs])))
      (is (= (get-in started [:event-subscribers :seed]) (get-in restored [:event-subscribers :seed]))))))

(deftest bounded-admission-and-stale-edits
  (let [model (configured [launch])
        overflow (:model (sub/admit model (vec (repeat 300 kick))))]
    (is (= 256 (count (get-in overflow [:event-subscribers :runs]))))
    (is (= 44 (get-in overflow [:event-subscribers :dropped])))
    (is (thrown? Exception (sub/configure model {:op :save :expected-subscriber nil :subscriber (listener [launch])})))
    (is (empty? (get-in (sub/configure model {:op :remove :id :kicks :expected-subscriber (listener [launch])})
                        [:event-subscribers :subscribers])))
    (is (thrown? Exception (sub/configure model {:op :save :subscriber (listener [{:op :instance/opacity :value 2}])})))
    (is (thrown? Exception (sub/configure model {:op :save :subscriber (listener [(assoc launch :delay {:ms 200})])})))))

(deftest ring-registration-and-real-host-capture-callback-launch-unplaced-effect
  (let [runtime (show/create-runtime) app (studio/handler runtime)
        defn (listener [{:op :effect/launch :select {:ids [:ember] :pick :first}}])
        req {:uri "/api/event-subscribers" :headers {"content-type" "application/transit+json"
                                                     "accept" "application/transit+json"}}]
    (try
      (let [receipt (future (app (assoc req :request-method :post
                                        :body (java.io.StringReader. (wire/encode {:op :save :subscriber defn :expected-epoch 0})))))]
        (loop [n 0] (when (and (not (realized? receipt)) (< n 200)) (show/tick! runtime 0) (Thread/sleep 5) (recur (inc n))))
        (is (= :accepted (:status (wire/decode (:body (deref receipt 1000 {})))))))
      (is (= [defn] (:subscribers (wire/decode (:body (app (assoc req :request-method :get)))))))
      (let [analyzed (audio-test/run-signal audio-test/kick-train 1300)]
        (is (= 1 (count (audio-test/events-of analyzed :audio/kick))))
        (is (true? ((:events! (show/audio-callbacks runtime)) (:events analyzed)))))
      (show/tick! runtime 1)
      (is (empty? (:instances (show/snapshot runtime))))
      (show/tick! runtime 25)
      (let [inst (first (:instances (show/snapshot runtime)))]
        (is (= :ember (:effect-id inst)))
        (is (nil? (:cue-id inst)))
        (is (= :event-subscriber (get-in inst [:source :adapter])))
        (is (= 1 (count (:instances (:model (show/export-show runtime)))))))
      (finally (show/stop! runtime)))))

(deftest captured-pcm-subscribers-follow-committed-audio-clock-confidence-and-epoch
  (let [kick-times [500 1000 1500 1700 2100 2600 3100]
        sample-rate 8000 block-ms 50 block-frames (quot (* block-ms sample-rate) 1000) duration-ms 3250
        values (for [index (range (quot (* duration-ms sample-rate) 1000))
                     :let [ms (/ (* index 1000.0) sample-rate)
                           onset (last (take-while #(<= % ms) kick-times))
                           age (when onset (- ms onset))]]
                 (if (and age (< age 160))
                   (long (* 32767 (audio-test/sine 65 (* 0.8 (Math/exp (/ (- age) 45.0))) age)))
                   0))
        closed (atom 0) midi-input (promise) clock-steps (atom {}) committed-audio (atom [])
        model (sub/configure (demo/model)
                             {:op :save :subscriber {:id :beats :match {:type :audio/beat}
                               :actions [{:op :effect/launch :select {:ids [:ember] :pick :first}}]}})
        runtime (show/create-runtime
                 {:model model :workspace-options nil
                  :audio-options {:open-source!
                                  (fn [_]
                                    (let [block (atom -1)]
                                      (update (pcm-test/memory-source values (* 2 block-frames) closed) :read!
                                              (fn [read!]
                                                (fn [out offset length]
                                                  (when-let [step! (get @clock-steps (* block-ms (swap! block inc)))]
                                                    (step!))
                                                  (read! out offset length))))))}
                  :sync-options {:midi-factory (fn [_ _] {:open! #(deliver midi-input %) :close! (fn [_])})}})
        conn (:connections runtime) sync (connections/sync conn)]
    (try
      (show/attach-observer! runtime :audio-regression
        (fn [] {:events! (fn [_ events _ _] (swap! committed-audio into (filter #(= :audio (:category %)) events)))
                :frame! (fn [_]) :tracing? (constantly false) :close! (fn [])}))
      (show/sync-action! runtime :configure {:clock {:timeout-ms 300 :on-stale :freewheel}
                                            :midi {:enabled? true :input-index 0}})
      (show/sync-action! runtime :start {})
      (let [receive! (deref midi-input 2000 nil)
            midi! (fn [at-ms byte] (receive! {:type :midi/message :source-id :midi-clock :time-ms at-ms :bytes [byte]}))
            start-clock! (fn [start ticks]
                           (midi! start 0xfa)
                           (doseq [i (range ticks)] (midi! (+ start (* i (/ 500.0 24))) 0xf8))
                           (when-not (sync-session/flush! sync) (throw (ex-info "MIDI clock did not settle" {}))))]
        (reset! clock-steps
                {1000 #(show/tick! runtime 1000)
                 1500 #(do (show/sync-action! runtime :source {:source-id :midi-clock})
                           (start-clock! 1100 20)
                           (show/tick! runtime 1500))
                 1700 #(show/tick! runtime 1700)
                 2100 #(show/tick! runtime 2100)
                 2600 #(do (midi! 2600 0xfc)
                           (when-not (sync-session/flush! sync) (throw (ex-info "MIDI stop did not settle" {})))
                           (show/tick! runtime 2600))
                 3100 #(do (start-clock! 2600 25) (show/tick! runtime 3100))})
        ;; The injected stream reads faster than real time, so keep receipt time past its sample clock.
        (swap! (:clock runtime) assoc :time-ms duration-ms)
        (let [handle (connections/start-audio! conn {:default? true :sample-rate sample-rate :block-frames block-frames
                                                    :analyzer-options {:beat-tolerance-ms 115.0}}
                                               (show/audio-callbacks runtime))
              result (deref (:result handle) 5000 ::timeout)]
          (is (= :completed (:status result)) (pr-str (:error result))))
        (show/tick! runtime 3250)
        (show/tick! runtime 3300)
        (let [kicks (mapv :data (filter #(= :audio/kick (:type %)) @committed-audio))
              beats (mapv :data (filter #(= :audio/beat (:type %)) @committed-audio))]
          (is (= (mapv #(+ % 5.0) kick-times) (mapv :time-ms kicks)))
          (is (= [nil :manual :midi-clock :midi-clock :midi-clock nil :midi-clock]
                 (mapv #(get-in % [:alignment :clock-source]) kicks)))
          (is (= [1005.0 1505.0 3105.0] (mapv :time-ms beats)))
          (is (= [[:manual 0 2] [:midi-clock 1 1] [:midi-clock 2 1]]
                 (mapv (juxt :clock-source :clock-epoch :beat) beats)))
          (is (= 1.0 (:clock-confidence (first beats))))
          (is (< 0.5 (:clock-confidence (second beats) 0.0) 1.0))
          (is (= [true :midi-clock 1 1]
                 ((juxt :aligned? :clock-source :clock-epoch :beat) (:alignment (nth kicks 3 nil)))))
          (is (= {:aligned? true :clock-confidence 0.0 :clock-epoch 1 :beat 2}
                 (select-keys (:alignment (nth kicks 4 nil)) [:aligned? :clock-confidence :clock-epoch :beat])))
          (is (= 3 (count (filter #(= :ember (:effect-id %)) (:instances (show/snapshot runtime))))))
          (is (not (contains? (get-in (show/export-show runtime) [:model :transport]) :confidence)))
          (is (= 1 @closed))))
      (finally (show/stop! runtime)))))

(deftest captured-section-change-reaches-an-explicit-owner-subscriber-once
  (let [closed (atom 0) observed (atom [])
        ;; The in-memory source returns immediately. Start the owner clock
        ;; at the end of the captured interval and keep its musical origin.
        model (-> (demo/model)
                  (assoc :time-ms 5000.0)
                  (update :transport assoc :time-ms 5000.0 :beat 10.0))
        runtime (show/create-runtime
                 {:model model :workspace-options nil
                  :audio-options {:open-source! (fn [_] (pcm-test/memory-source (pcm-test/section-pcm-values) 719 closed))}})
        subscriber {:id :sections :match {:type :audio/section-change}
                    :actions [{:op :effect/launch :select {:ids [:ember] :pick :first}}]}]
    (try
      (is (some #{{:category :audio :type :audio/section-change}} (:events (sub/catalog model))))
      (show/attach-observer! runtime :section-regression
        (fn [] {:events! (fn [_ events _ _]
                          (swap! observed into (filter #(= :audio/section-change (:type %)) events)))
                :frame! (fn [_]) :tracing? (constantly false) :close! (fn [])}))
      (let [receipt (show/register-event-subscriber! runtime subscriber)]
        (show/tick! runtime 5000)
        (is (= :accepted (:status (deref receipt 1000 ::timeout)))))
      (is (empty? (:instances (show/snapshot runtime))))
      (let [handle (connections/start-audio! (:connections runtime)
                                             {:default? true :sample-rate 8000.0 :block-frames 640
                                              :analyzer-options {:sections {:beats-per-window 2}}}
                                             (show/audio-callbacks runtime))
            result (deref (:result handle) 5000 ::timeout)]
        (is (= :completed (:status result)) (pr-str (:error result))))
      (doseq [time [5001 5002 5020 5050]] (show/tick! runtime time))
      (let [[event] @observed
            instances (:instances (show/snapshot runtime))]
        (is (= 1 @closed (count @observed) (count instances)))
        (is (= {:boundary-beat 4.0 :confirmed-at-beat 8.0 :clock-source :manual :clock-epoch 0}
               (select-keys (:data event) [:boundary-beat :confirmed-at-beat :clock-source :clock-epoch])))
        (is (= [:ember :event-subscriber]
               ((juxt :effect-id #(get-in % [:source :adapter])) (first instances)))))
      (finally (show/stop! runtime)))))

(defn- capture-pattern-boundary [policy pause? & [after-admission]]
  (let [owner (atom nil) observed (atom []) closed (atom 0) pause-receipt (atom nil)
        open (fn [_]
               (let [source (pcm-test/memory-source (pcm-test/section-pcm-values) 1280 closed)
                     reads (atom -1)]
                 (update source :read!
                         (fn [read!]
                           (fn [out offset length]
                             (let [time-ms (* 80 (swap! reads inc))
                                   runtime @owner]
                               ;; Pace the injected capture timestamps independently of owner frame ticks.
                               (swap! (:clock runtime) assoc :time-ms (+ time-ms 82.0))
                               (when (#{80 3920 4000} time-ms)
                                 (when (and pause? (= 4000 time-ms))
                                   (reset! pause-receipt (show/submit! runtime {:op :transport/set
                                                                              :transport {:playing? false}})))
                                 (show/tick! runtime time-ms))
                               (read! out offset length)))))))
        runtime (show/create-runtime (cond-> {:workspace-options nil :audio-options {:open-source! open}}
                                       policy (assoc :section-patterns policy)))
        subscriber {:id :track-patterns :match {:category :music :type :music/section-pattern}
                    :actions [(cond-> {:op :effect/launch :select {:ids [:ember] :pick :first}}
                                after-admission (assoc :delay {:beats 1}))]}]
    (reset! owner runtime)
    (try
      (show/attach-observer! runtime :patterns
        (fn [] {:events! (fn [_ events _ _]
                          (swap! observed into (filter #(#{:audio/section-change :clock/bar :music/section-pattern}
                                                         (:type %)) events)))
                :frame! (fn [_]) :tracing? (constantly false) :close! (fn [])}))
      (let [reply (show/register-event-subscriber! runtime subscriber)]
        (show/tick! runtime 0)
        (is (= :accepted (:status (deref reply 1000 ::timeout)))))
      (let [handle (connections/start-audio! (:connections runtime)
                                             {:default? true :sample-rate 8000.0 :block-frames 640
                                              :analyzer-options {:sections {:beats-per-window 2}}}
                                             (show/audio-callbacks runtime))
            result (deref (:result handle) 5000 ::timeout)]
        (is (= :completed (:status result)) (pr-str (:error result)))
        (when pause?
          (is (= :accepted (:status (deref @pause-receipt 1000 ::timeout))))
          (let [reply (show/submit! runtime {:op :transport/set :transport {:playing? true}})]
            (show/tick! runtime 4001)
            (is (= :accepted (:status (deref reply 1000 ::timeout))))))
        (when after-admission
          (is (empty? (:instances (show/snapshot runtime))))
          (is (= 1 (count (get-in @(:state runtime) [:model :event-subscribers :runs]))))
          (after-admission runtime))
        (doseq [time [4001 4100 4500 5000]] (show/tick! runtime time))
        {:events @observed :instances (:instances (show/snapshot runtime))
         :model (:model (show/export-show runtime)) :run-id (:run-id handle) :closed @closed})
      (finally (show/stop! runtime)))))

(deftest stopping-capture-revokes-an-admitted-delayed-causal-pattern
  (let [policy {:enabled? true :on :bar :patterns [:midnight-groove] :show-seed 514}
        healthy (capture-pattern-boundary policy false (constantly nil))
        stopped (capture-pattern-boundary policy false #(show/audio-action! % :stop {}))]
    (doseq [{:keys [events]} [healthy stopped]]
      (is (= 1 (count (filter #(= :music/section-pattern (:type %)) events)))))
    (is (= [:ember] (mapv :effect-id (:instances healthy))))
    (is (empty? (:instances stopped)))))

(deftest opted-in-captured-patterns-publish-on-owner-bars-and-respect-a-pause
  (let [policy {:enabled? true :on :bar :patterns [:midnight-groove] :show-seed 514}
        cases [(capture-pattern-boundary policy false)
               (capture-pattern-boundary nil false)
               (capture-pattern-boundary policy true)]
        [enabled inert paused] cases
        [intent] (filterv #(= :music/section-pattern (:type %)) (:events enabled))
        [section] (filterv #(= :audio/section-change (:type %)) (:events enabled))
        bars (filter #(= :clock/bar (:type %)) (:events enabled))]
    (is (some #{{:category :music :type :music/section-pattern}} (:events (sub/catalog (demo/model)))))
    (doseq [{:keys [events run-id model closed]} cases]
      (let [sections (filterv #(= :audio/section-change (:type %)) events)]
        (is (= 1 closed (count sections)))
        (is (= run-id (get-in (first sections) [:data :capture-run-id])))
        (is (not (contains? model :section-pattern-state)))))
    (is (= 1 (count (filter #(= :music/section-pattern (:type %)) (:events enabled)))))
    (is (= [:music :causal :midnight-groove 8.0 4000]
           [(:category intent) (get-in intent [:data :mode]) (get-in intent [:data :pattern])
            (get-in intent [:data :show-beat]) (get-in intent [:time :time-ms])]))
    (is (= (get-in section [:data :clock-session-id]) (get-in intent [:data :scope :clock 2])))
    (is (some #(= (:time %) (:time intent)) bars))
    (is (= [[:ember :event-subscriber]]
           (mapv (juxt :effect-id #(get-in % [:source :adapter])) (:instances enabled))))
    (doseq [result [inert paused]]
      (is (not-any? #(= :music/section-pattern (:type %)) (:events result)))
      (is (empty? (:instances result))))))

(defn- play-reference-boundary [pattern interruption & [{:keys [delayed?]}]]
  (let [clock-ms (atom 27000)
        provenance {:source-id :local-player :session-id "test-session" :clock-id :physical}
        reference {:id "bass-arrangement" :version "sections-v1"}
        profile {:version 1 :reference reference :duration-ms 8000.0
                 :sections [{:id 0 :start-position-ms 0.0 :end-position-ms 4000.0
                             :start-beat 0.0 :boundary-confidence 1.0 :boundary-kind :file-start}
                            {:id 1 :start-position-ms 4000.0 :end-position-ms 8000.0
                             :start-beat 8.0 :boundary-confidence 0.9 :boundary-kind :confirmed-change
                             :features {:signature {:bass-notes [40]}}}]}
        runtime (show/create-runtime {:workspace-options nil :physical-now-ms #(deref clock-ms)
                                      :section-patterns {:enabled? true :on :bar :show-seed 514
                                                         :patterns [pattern]}
                                      :section-reference {:profile profile :provenance provenance}})
        {:keys [publish! now-ms! state!]} (show/section-reference-callbacks runtime)
        position (atom (reference-position/new-state provenance))
        events (atom [])
        observe (fn [type extra]
                  (let [now (now-ms!)]
                    (publish! (swap! position (fn [state]
                                                (reference-position/advance state now
                                                  (merge {:type type :provenance provenance :occurrence (:occurrence state)
                                                          :observed-at-ms now} extra)))))))
        tick (fn [time-ms]
               (reset! clock-ms (+ 27000 time-ms))
               (show/tick! runtime time-ms))
        effects #(mapv :effect-id (:instances (show/snapshot runtime)))]
    (try
      (show/attach-observer! runtime :reference-patterns
        (fn [] {:events! (fn [_ emitted _ _] (swap! events into emitted))
                :frame! (fn [_]) :tracing? (constantly false) :close! (fn [])}))
      (let [replies (mapv (fn [[pattern effect]]
                            (show/register-event-subscriber! runtime
                              {:id pattern :match {:type :music/section-pattern :pattern pattern}
                               :actions [(cond-> {:op :effect/launch :select {:ids [effect] :pick :first}}
                                           delayed? (assoc :delay {:beats 1}))]}))
                          [[:slow :ember] [:pulse :ice]])
            unrelated (when delayed?
                        (show/register-event-subscriber! runtime
                          (listener [{:op :cue/launch :select {:ids [:ice] :pick :first} :delay {:beats 1}}])))]
        (show/tick! runtime 0)
        (is (= [:accepted :accepted] (mapv #(:status (deref % 1000 ::timeout)) replies)))
        (when unrelated (is (= :accepted (:status (deref unrelated 1000 ::timeout))))))
      (doseq [position-ms [3700 3750 3800]]
        (reset! clock-ms (+ 27000 position-ms))
        (is (observe :match {:match {:reference reference :position-ms position-ms
                                     :rate 1.0 :score 0.95}})))
      (is (= :locked (:status (state!))))
      (tick 3850)
      (is (= :reference (get-in @(:state runtime) [:section-pattern-state :pending :mode])))
      (when interruption (reset! clock-ms 30950))
      (case interruption
        :seek (let [stale @position]
                (is (observe :seek {}))
                (is (nil? (publish! stale)))
                (is (= :unavailable (:status (state!)))))
        :override (let [reply (show/submit! runtime :settings
                                {:scope :section-patterns :values {:override? true}})]
                    (tick 3950)
                    (is (= :accepted (:status (deref reply 1000 ::timeout))))
                    (is (nil? (get-in @(:state runtime) [:section-pattern-state :pending]))))
        nil)
      (when delayed?
        (is (true? ((:events! (show/audio-callbacks runtime)) [{:type :audio/kick}]))))
      (tick 4000)
      (when delayed?
        (is (= {:kicks 9.0 pattern 9.0}
               (into {} (map (juxt :subscriber :due-beat))
                     (get-in @(:state runtime) [:model :event-subscribers :runs])))))
      (when (= :seek-after-admission interruption)
        (reset! clock-ms 31010)
        (is (observe :seek {}))
        (is (= :unavailable (:status (state!))))
        (doseq [time-ms [4050 4090 4130]]
          (reset! clock-ms (+ 27000 time-ms))
          (is (observe :match {:match {:reference reference :position-ms (- time-ms 3000)
                                       :rate 1.0 :score 0.95}})))
        (is (= [:locked 1] ((juxt :status :occurrence) (state!)))))
      (tick (if (= :seek-after-admission interruption) 4130 4100))
      (let [before-delay (effects)]
        (when delayed? (tick 4500))
        {:patterns (filterv #(= :music/section-pattern (:type %)) @events)
         :bars (filterv #(= :clock/bar (:type %)) @events)
         :effects-before-delay before-delay :effects (effects)})
      (finally (show/stop! runtime)))))

(deftest trusted-reference-uses-its-monotonic-clock-and-revokes-a-seeked-forecast
  (let [first-run (play-reference-boundary :slow nil)
        repeated (play-reference-boundary :slow nil)
        different (play-reference-boundary :pulse nil)
        seeked (play-reference-boundary :slow :seek)
        overridden (play-reference-boundary :slow :override)
        [intent] (:patterns first-run)]
    (is (= 1 (count (:patterns first-run)) (count (:patterns repeated))))
    (is (= [:reference 8.0 4000 "bass-arrangement"]
           [(get-in intent [:data :mode]) (get-in intent [:data :show-beat])
            (get-in intent [:data :time-ms]) (get-in intent [:data :reference :id])]))
    (is (= (select-keys (:data intent) [:pattern :decision-seed :delivery-id])
           (select-keys (get-in repeated [:patterns 0 :data]) [:pattern :decision-seed :delivery-id])))
    (is (some #(= (:time %) (:time intent)) (:bars first-run)))
    (is (= [:ember] (:effects first-run) (:effects repeated)))
    (is (= [:ice] (:effects different)))
    (doseq [suppressed [seeked overridden]]
      (is (empty? (:patterns suppressed)))
      (is (empty? (:effects suppressed))))))

(deftest a-seek-revokes-an-admitted-pattern-without-affecting-other-delayed-cues
  (let [healthy (play-reference-boundary :slow nil {:delayed? true})
        sought (play-reference-boundary :slow :seek-after-admission {:delayed? true})]
    (is (= 1 (count (:patterns healthy)) (count (:patterns sought))))
    (is (empty? (:effects-before-delay healthy)))
    (is (empty? (:effects-before-delay sought)))
    (is (= 2 (count (:effects healthy))))
    (is (= #{:ember :ice} (set (:effects healthy))))
    (is (= [:ice] (:effects sought)))))

(defn- reference-observer [{:keys [publish! now-ms! provenance alignment-options]}]
  (let [position (atom (reference-position/new-state provenance alignment-options))]
    {:position position
     :observe! (fn [type match]
                 (let [now (now-ms!)
                       observation {:type type :provenance provenance :observed-at-ms now
                                    :occurrence (:occurrence @position) :match match}]
                   (publish! (swap! position reference-position/advance now observation))))}))

(deftest reattaching-a-section-reference-fences-old-observations-and-forecasts
  (let [clock-ms (atom 27000)
        clock-read (atom nil)
        provenance {:source-id :mpv :session-id :first :clock-id :physical}
        reference {:id :recording :version 1}
        profile {:version 1 :reference reference :duration-ms 12000.0
                 :sections (mapv (fn [id]
                                   {:id id :start-position-ms (* id 4000.0) :end-position-ms (* (inc id) 4000.0)
                                    :start-beat (* id 8.0) :boundary-confidence 1.0
                                    :features {:signature {:bass-notes [(+ 36 id)]}}
                                    :boundary-kind (if (zero? id) :file-start :confirmed-change)})
                                 (range 3))}
        runtime (show/create-runtime {:workspace-options nil
                                      :physical-now-ms #(do (when-let [before-read @clock-read] (before-read)) @clock-ms)
                                      :section-patterns {:enabled? true :on :bar :show-seed 514 :patterns [:warm :pulse]}
                                      :section-reference {:profile profile :provenance provenance
                                                          :position-options {:max-age-ms 600.0}}})
        callback (show/section-reference-callbacks runtime)
        {:keys [position observe!]} (reference-observer callback)
        events (atom [])
        at! #(reset! clock-ms (+ 27000 %))
        tick! (fn [time-ms] (at! time-ms) (show/tick! runtime time-ms))
        sample! (fn [observe track offset cursors]
                  (doseq [cursor cursors]
                    (at! (+ offset cursor))
                    (is (observe :match {:reference track :position-ms cursor :rate 1.0 :score 1.0}))))]
    (try
      (show/attach-observer! runtime :reference-reattach
        (fn [] {:events! (fn [_ emitted _ _]
                           (swap! events into (filter #(= :music/section-pattern (:type %)) emitted)))
                :frame! (fn [_]) :tracing? (constantly false) :close! (fn [])}))
      (tick! 0)
      (at! 100)
      (is (observe! :seek nil))
      (sample! observe! reference 0 [3700 3750 3800])
      (tick! 3850)
      (tick! 4000)
      (is (= [1] (mapv #(get-in % [:data :section :id]) @events)))
      (sample! observe! reference 0 [7700 7750 7800])
      (tick! 7850)
      (is (= 2 (get-in @(:state runtime) [:section-pattern-state :pending :section :id])))
      (is (= 1 (:occurrence ((:state! callback)))))
      (let [entered (promise)
            release (promise)
            stale @position]
        (reset! clock-read (fn [] (deliver entered true) (deref release 3000 nil)))
        (let [publishing (future ((:publish! callback) stale))
              _ (is (true? (deref entered 3000 ::timeout)))
              _ (reset! clock-read nil)
              replacement (try (show/attach-section-reference! runtime :second)
                               (finally (deliver release true)))
              next-observer (:observe! (reference-observer replacement))]
          (is (nil? (deref publishing 3000 ::timeout)))
          (is (nil? ((:state! callback))))
          (is (nil? ((:publish! replacement) stale)))
          (is (= {:source-id :mpv :session-id :second :clock-id :physical} (:provenance replacement)))
          (is (= (:alignment-options callback) (:alignment-options replacement)))
          (is (= [0 0 :unavailable] ((juxt :occurrence :generation :status) ((:state! replacement)))))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already used"
                                (show/attach-section-reference! runtime :first)))
          (sample! next-observer {:id :other-recording :version 1} 4200 [3700 3750 3800])
          (tick! 8000)
          (is (= 1 (count @events)))
          (is (nil? (get-in @(:state runtime) [:section-pattern-state :pending])))

          ;; The fresh session can deliver the same verified section again.
          (sample! next-observer reference 8000 [3700 3750 3800])
          (tick! 11850)
          (tick! 12000)
          (is (= [[1 :first] [1 :second]]
                 (mapv (fn [event] [(get-in event [:data :section :id])
                                   (get-in event [:data :fence :provenance :session-id])])
                       @events)))

          (let [epoch (get-in @(:state runtime) [:model :epoch])
                receipt (show/submit! runtime :import {:source (show/export-show-source runtime)})]
            (tick! 12025)
            (is (= :accepted (:status (deref receipt 2000 ::timeout))))
            (is (< epoch (get-in @(:state runtime) [:model :epoch])))
            (is (nil? (show/section-reference-callbacks runtime)))
            (is (nil? ((:state! replacement))))
            (at! 12050)
            (is (nil? (next-observer :unavailable nil)))
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already used"
                                  (show/attach-section-reference! runtime :second)))
            (let [restored (show/attach-section-reference! runtime :third)]
              (is (= :third (get-in restored [:provenance :session-id])))
              (is (= :unavailable (:status ((:state! restored)))))
              (is (nil? (next-observer :unavailable nil)))))))
      (finally (show/stop! runtime)))))

(deftest middleware-can-update-variables-and-queue-actions-without-blocking-the-owner
  (let [entered (promise) release (promise)
        middleware (fn [context event]
                     (deliver entered true) (deref release 3000 nil)
                     (-> context (assoc-in [:variables :melody] (:type event))
                         (assoc :actions [launch])))
        runtime (show/create-runtime {:event-callback-options {:callbacks {:performer middleware} :timeout-ms 4000}})]
    (try
      (let [receipt (show/register-event-subscriber! runtime (listener [{:op :callback :implementation {:type :registered :id :performer}}]))]
        (show/tick! runtime 0)
        (is (= :accepted (:status @receipt))))
      ((:events! (show/audio-callbacks runtime)) [{:type :audio/kick}])
      (show/tick! runtime 10)
      (show/tick! runtime 20)
      (is (true? (deref entered 1000 false)))
      (let [n (:frame-number (show/snapshot runtime))]
        (show/tick! runtime 30)
        (is (< n (:frame-number (show/snapshot runtime)))))
      (deliver release true)
      (loop [i 0]
        (when (and (< i 100) (empty? (:instances (show/snapshot runtime))))
          (Thread/sleep 4) (show/tick! runtime (+ 40 i)) (recur (inc i))))
      (is (= :audio/kick (get-in @(:state runtime) [:model :variables :melody])))
      (is (= 1 (count (:instances (show/snapshot runtime)))))
      (finally (deliver release true) (show/stop! runtime)))))

(defn- play-kick! [runtime]
  (let [model #(:model @(:state runtime))
        start (:time-ms (model))]
    (is (true? ((:events! (show/audio-callbacks runtime)) [kick])))
    (show/tick! runtime (inc start))
    (loop [n 2]
      (if (and (< n 150) (seq (get-in (model) [:event-subscribers :runs])))
        (do (show/tick! runtime (+ start n)) (Thread/sleep 5) (recur (inc n)))
        (model)))))

(deftest a-service-opened-before-the-repl-reload-keeps-ticking-and-closes
  (let [named (fn [context _] (assoc-in context [:variables :callback-owner] :named))
        runtime (-> (show/create-runtime {:workspace-options nil
                                          :event-callback-options {:callbacks {:kicks named}}})
                    (update :event-callbacks dissoc :inline-handlers :inline-inputs))
        service (:event-callbacks runtime)
        registration (listener [{:op :callback :implementation {:type :registered :id :kicks}}])]
    (try
      (callbacks/publish! service (:model @(:state runtime)))
      (let [saved (show/register-event-subscriber! runtime registration)]
        (show/tick! runtime 0)
        (is (= :accepted (:status (deref saved 1000 {})))))
      (let [raw (show/register-event-subscriber! runtime (assoc (listener []) :middleware (fn [context _] context)))]
        (show/tick! runtime 1)
        (let [receipt (deref raw 1000 {})]
          (is (= :rejected (:status receipt)))
          (is (re-find #"Restart the runtime.*process-local" (:message receipt ""))))
        (is (= [registration] (get-in (show/snapshot runtime) [:event-subscribers :subscribers]))))
      (is (= :named (get-in (play-kick! runtime) [:variables :callback-owner])))
      (let [reference (listener [{:op :callback :implementation {:type :inline :callback-id (str (random-uuid))}}])
            saved (show/register-event-subscriber! runtime reference)]
        (show/tick! runtime 10)
        (is (= :accepted (:status (deref saved 1000 {}))))
        (is (re-find #"Process-local callback unavailable" (get-in (play-kick! runtime)
                                                                  [:event-subscribers :last-error :message] ""))))
      (finally (show/stop! runtime)))
    (is (.isShutdown ^java.util.concurrent.ThreadPoolExecutor (:pool service)))))

(deftest a-queued-worker-can-use-the-previous-callback-signature
  (let [named (fn [context _] (assoc-in context [:variables :callback-owner] :named))
        service (dissoc (callbacks/create {:callbacks {:kicks named}}) :inline-handlers :inline-inputs)
        ^ThreadPoolExecutor pool (:pool service)
        workers-started (CountDownLatch. 2)
        release (promise)
        implementation {:type :registered :id :kicks}
        model (demo/model)
        run {:event kick :index 0 :steps [{:op :callback :implementation implementation}]}
        task (FutureTask. ^Callable (reify Callable
                                     (call [_] (#'callbacks/invoke service model run))))]
    (try
      (is (identical? named (#'callbacks/resolve-handler service implementation)))
      (dotimes [_ 2]
        (.execute pool ^Runnable (fn [] (.countDown workers-started) @release)))
      (is (.await workers-started 5 TimeUnit/SECONDS))
      (.execute pool task)
      (is (= 1 (.size (.getQueue pool))))
      (deliver release true)
      (is (= [:callback-owner :named]
             (get-in (.get task 5 TimeUnit/SECONDS) [:edits :callback-owner :after])))
      (finally
        (deliver release true)
        (callbacks/close! service)))))

(deftest transit-restores-only-the-exact-runtime-owned-event-callback
  (let [calls (atom [])
        middleware (fn [owner]
                     (fn [context _]
                       (swap! calls conj owner)
                       (assoc-in context [:variables :callback-owner] owner)))
        runtime (fn [id callbacks]
                  (show/create-runtime {:model (assoc (demo/model) :show-id id)
                                        :workspace-options nil
                                        :event-callback-options {:callbacks callbacks}}))
        a (runtime :closures/a {:kicks (middleware :named)})
        b (runtime :closures/b {})]
    (try
      (doseq [[current owner] [[a :a] [b :b]]]
        (let [reply (show/register-event-subscriber! current (assoc (listener []) :middleware (middleware owner)))]
          (show/tick! current 0)
          (is (= :accepted (:status (deref reply 1000 {}))))))
      (let [handlers (get-in a [:event-callbacks :inline-handlers])
            registered @handlers]
        (show/tick! a 1)
        (is (identical? registered @handlers) "An idle tick reuses the registered callback map"))
      (let [source-a (show/export-show-source a)
            source-b (show/export-show-source b)
            definition (fn [source] (get-in (files/read-string source) [:model :event-subscribers :subscribers 0]))
            a-definition (definition source-a)
            b-definition (definition source-b)
            implementation #(get-in % [:actions 0 :implementation])
            import! (fn [source] (show/submit! a :import {:source source}))]
        (is (= :kicks (:id a-definition) (:id b-definition)))
        (is (= :inline (:type (implementation a-definition)) (:type (implementation b-definition))))
        (is (not= (:callback-id (implementation a-definition)) (:callback-id (implementation b-definition))))

        (testing "The same runtime can reimport its callback, including when a competing import is rejected"
          (let [own (import! source-a)
                stale (import! source-b)]
            (is (not (realized? stale)))
            (show/tick! a 1)
            (is (= :accepted (:status (deref own 1000 {}))))
            (let [receipt (deref stale 1000 {})]
              (is (= :rejected (:status receipt)))
              (is (re-find #"previous show session" (:message receipt ""))))
            (is (= :a (get-in (play-kick! a) [:variables :callback-owner])))
            (is (= [:a] @calls))))

        (testing "A different subscriber cannot reuse the reference in the same runtime"
          (let [reply (show/submit! a :event-subscriber {:op :save :subscriber (assoc a-definition :id :copy)})]
            (show/tick! a 20)
            (is (= :accepted (:status (deref reply 1000 {}))))
            (let [model (play-kick! a)]
              (is (= :copy (get-in model [:event-subscribers :last-error :subscriber-id])))
              (is (= [:a :a] @calls)))))

        (testing "A different show's reference stays visible and fails without calling either implementation"
          (let [foreign (import! source-b)]
            (show/tick! a 30)
            (is (= :accepted (:status (deref foreign 1000 {}))))
            (let [catalog ((studio/handler a) {:uri "/api/event-subscribers" :request-method :get
                                              :headers {"accept" "application/transit+json"}})]
              (is (= [b-definition] (:subscribers (wire/decode (:body catalog))))))
            (let [model (play-kick! a)
                  fault (get-in (show/snapshot a) [:event-subscribers :last-error])]
              (is (= :closures/b (:show-id model)))
              (is (nil? (get-in model [:variables :callback-owner])))
              (is (= [:a :a] @calls))
              (is (= {:type :event/handler-failed :subscriber-id :kicks}
                     (select-keys fault [:type :subscriber-id])))
              (is (re-find #"Process-local callback unavailable.*Re-register" (:message fault "")))
              (is (empty? @(get-in a [:event-callbacks :inline-handlers]))))))

        (testing "An explicitly named reference still finds the host callback with the same name"
          (let [registration (listener [{:op :callback :implementation {:type :registered :id :kicks}}])
                saved (show/register-event-subscriber! b registration)]
            (show/tick! b 1)
            (is (= :accepted (:status (deref saved 1000 {}))))
            (let [named (import! (show/export-show-source b))]
              (show/tick! a 40)
              (is (= :accepted (:status (deref named 1000 {}))))
              (is (= :named (get-in (play-kick! a) [:variables :callback-owner])))
              (is (= [:a :a :named] @calls))))))
      (finally (show/stop! a) (show/stop! b)))))

(deftest all-selection-chunks-preserve-chosen-targets-and-save-cursor
  (let [model (-> (configured [{:op :cue/launch :select {:pick :all}}])
                  (assoc-in [:registry :cues]
                            (into {} (for [i (range 140) :let [id (str "pad-" i)]]
                                       [id {:id id :version 1 :effect-id :ember}]))))
        started (sub/advance (:model (sub/admit model [kick])) 0 {})
        with-new-cue (assoc-in started [:registry :cues :late] {:id :late :effect-id :ember :version 1})
        finished (sub/advance with-new-cue 25 {})]
    (is (= 128 (count (:instances started))))
    (is (= 128 (get-in started [:event-subscribers :runs 0 :offset])))
    (is (= 140 (count (:instances finished))))
    (is (not-any? #(= :late (:cue-id %)) (vals (:instances finished))))
    (is (empty? (get-in finished [:event-subscribers :runs])))))

(deftest callback-failure-is-local-and-registered-source-fn-can-be-restored
  (let [author (show/create-runtime)
        document (try
                   (let [level 7
                         receipt (show/reg-event-subscriber!
                                  author {:id :kicks :match {:type :audio/kick}
                                          :bindings {"n" level}
                                          :middleware (fn [context event]
                                                        (assoc-in context [:variables :count] n))})]
                     (show/tick! author 0)
                     (is (= :accepted (:status @receipt)))
                     (files/write-string (:model @(:state author))))
                   (finally (show/stop! author)))
        restored (files/thaw (demo/model) (files/read-string document))]
    (is (= {:trusted-source? true} (:event-callback-options (app/parse-options ["--trust-event-callbacks" "true"]))))
    (is (thrown? Exception (app/parse-options ["--trust-event-callbacks" "not-a-boolean"])))
    (doseq [trusted? [false true]]
      (let [runtime (show/create-runtime (cond-> {:model restored} trusted? (assoc :event-callback-options {:trusted-source? true})))]
        (try
          ((:events! (show/audio-callbacks runtime)) [{:type :audio/kick}])
          (show/tick! runtime 1)
          (show/tick! runtime 2)
          (loop [n 0]
            (when (and (< n 150) (seq (get-in @(:state runtime) [:model :event-subscribers :runs])))
              (Thread/sleep 5) (show/tick! runtime (+ 3 n)) (recur (inc n))))
          (if trusted?
            (is (= 7 (get-in @(:state runtime) [:model :variables :count])))
            (is (re-find #"disabled" (get-in @(:state runtime) [:model :event-subscribers :last-error :message] ""))))
          (is (nil? (get-in (show/snapshot runtime) [:status :error])))
          (finally (show/stop! runtime)))))))

(deftest one-broken-subscriber-leaves-its-peers-running-and-a-portable-diagnostic
  (let [model (-> (configured [launch])
                  (sub/configure {:op :save :subscriber (assoc (listener [launch]) :id :sibling)}))
        admitted (:model (sub/admit model [kick]))
        run (fn [run]
              (if (= :kicks (:subscriber run))
                (assoc-in run [:steps 0] {:op :invalid-at-runtime}) run))
        result (sub/advance (update-in admitted [:event-subscribers :runs] #(mapv run %)) 0 {})
        diagnostic (get-in result [:variables :runtime :handler-faults 0])]
    (is (= 1 (count (:instances result))))
    (is (= :event/handler-failed (:type diagnostic)))
    (is (= :kicks (:subscriber-id diagnostic)))
    (is (seq (:stacktrace diagnostic)))
    (is (empty? (get-in result [:event-subscribers :runs])))))

(deftest json-scene-subscriber-registers-typed-event-and-gate-paths
  (let [runtime (show/create-runtime)
        app (studio/handler runtime)
        path ["data" "audio/energy"]
        action {:op "lighting/scene" :scene {:op "apply" :group-id "front"}
                :event-path path :gate {:path path :above 0.7 :below 0.2}}
        payload {:op "save" :subscriber {:id "from-json" :enabled? true
                                         :match {:type "audio/kick"} :actions [action]}}]
    (try
      (let [receipt (future (app {:uri "/api/event-subscribers" :request-method :post
                                  :headers {"content-type" "application/json" "accept" "application/transit+json"}
                                  :body (java.io.StringReader. (json/write-str payload))}))]
        (loop [n 0] (when (and (not (realized? receipt)) (< n 200))
                      (show/tick! runtime 0) (Thread/sleep 5) (recur (inc n))))
        (is (= :accepted (:status (wire/decode (:body (deref receipt 1000 {})))))))
      (let [registration (first (:subscribers (wire/decode (:body (app {:uri "/api/event-subscribers" :request-method :get
                                                                        :headers {"accept" "application/transit+json"}})))))
            step (first (:actions registration))]
        (is (= [:data :audio/energy] (:event-path step) (get-in step [:gate :path]))))
      (finally (show/stop! runtime)))))
