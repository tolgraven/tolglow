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
            [tolglow.demo :as demo]
            [tolglow.event-subscribers :as sub]
            [tolglow.persistence :as files]
            [tolglow.transit :as wire]))

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
                                   (assoc-in kick [:emitter :id] :event-subscriber)])
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
