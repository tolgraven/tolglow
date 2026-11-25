(ns tolglow.integrations.live-sync-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [afterglow.sync :as sync]
            [afterglow.sync.session :as session]
            [tolglow.integrations.live-sync :as live-sync]
            [tolglow.integrations.live-transport :as live-transport]
            [tolglow.integrations.live-parameters :as live-params]
            [tolglow.integrations.ableton :as ableton]
            [tolglow.qa.live-pickup :as pickup])
  (:import [java.net InetSocketAddress]
           [java.io ByteArrayOutputStream]
           [javax.sound.midi MidiSystem ShortMessage]
           [com.sun.net.httpserver HttpServer HttpHandler]))

(defn envelope [id] {:show-id :show :epoch 1 :time-ms 0.0 :command-id id})
(defn callbacks [commands events]
  (let [id (atom 0)]
    {:events! #(swap! events conj %) :commands! #(swap! commands into %)
     :envelope! #(envelope (str "capture-" (swap! id inc)))}))
(defn fake-midi [opens closes receiver sent]
  (fn [_ _]
    {:open! (fn [receive!] (swap! opens inc) (reset! receiver receive!) :owned)
     :close! (fn [handle] (is (= :owned handle)) (swap! closes inc))
     :send! (fn [_ bytes] (swap! sent into bytes))}))
(def midi-config {:selected-source :midi-clock :midi {:enabled? true :input-index 0 :control? true}
                  :live {:mappings {36 {:cue-id :red :mode :held}}}})
(defn send-midi! [manager at bytes]
  (is (session/ingest! manager {:type :midi/message :source-id :midi-clock :time-ms at :bytes bytes})))

(deftest config-and-restore-are-inert-and-portable
  (let [opens (atom 0) factory (fn [& _] (swap! opens inc) (throw (Exception. "unexpected factory")))
        manager (live-sync/create-manager {:midi-factory factory :beat-link-factory factory :live-factory factory})]
    (try
      (let [state {:version 1 :wanted? true
                   :config {:selected-source "ableton-live"
                            :live {:enabled? true :launch-grid {:phrases 1}
                                   :mappings {"36" {:cue-id :red :mode "timed" :duration {:bars 2} :grid "off"}}
                                   :pairing {:track-index 0 :clip-slot-index 3 :expected-track-name "Lights"}}}}
            normalized (live-sync/validate-state state)]
        (sync/restore! manager normalized)
        (is (= 0 @opens))
        (is (= :stopped (:state (sync/status manager))))
        (is (= {:bars 2} (get-in (sync/export-state manager) [:config :live :mappings 36 :duration])))
        (is (= :off (get-in (sync/export-state manager) [:config :live :mappings 36 :grid])))
        (is (= {:phrases 1} (get-in (sync/export-state manager) [:config :live :launch-grid])))
        (is (= (assoc normalized :wanted? false) (sync/export-state manager)))
        (is (= :waiting (:status (session/clock-at manager 0))))
        (sync/select-source! manager :manual)
        (is (nil? (:clock (session/clock-at manager 0)))))
      (doseq [config [{:selected-source :unknown} {:midi {:enabled? true}}
                      {:live {:endpoint "https://example.com/live"}}
                      {:live {:pairing {:track-id 42}}}
                      {:live {:pairing {:track-index -1 :clip-slot-index 0}}}
                      {:live {:mappings {36 {:cue-id :red :duration {:beats 1 :ms 5}}}}}
                      {:live {:launch-grid nil}}
                      {:live {:launch-grid {:ms 50}}}
                      {:live {:launch-grid {:bars 0}}}
                      {:live {:mappings {36 {:cue-id :red :grid {:beats 1 :bars 1}}}}}
                      {:live {:parameters {1 {:cue-id :red :param :level :instance-id 7}}}}
                      {:beat-link {:source-id :midi-clock}}]]
        (is (thrown? clojure.lang.ExceptionInfo (live-sync/validate-config config))))
      (is (thrown? clojure.lang.ExceptionInfo (live-sync/validate-state {:version 1 :config {} :wanted? :yes})))
      (finally (sync/close! manager)))))

(deftest held-off-before-ack-and-stop-cancel-the-original-launch
  (let [manager (live-sync/create-manager {:config midi-config :now-ms (constantly 0)
                                      :midi-factory (fake-midi (atom 0) (atom 0) (atom nil) (atom []))})
        commands (atom []) cb (callbacks commands (atom []))]
    (try
      (let [run (sync/start! manager cb)]
        (is (session/flush! manager))
        (send-midi! manager 17 [0x90 36 100]) (is (session/flush! manager))
        (let [first-launch (last @commands)]
          (is (= :cue/launch (:op first-launch)))
          (is (= :off (:grid first-launch)))
          (is (= {:time-ms 17} (:raw first-launch)))
          (is (= (:command-id first-launch) (:gesture-id first-launch)))
          (is (= 36 (get-in first-launch [:source :note])))
          (is (= (:run-id run) (get-in first-launch [:source :sync-run-id])))
          (is (live-sync/acknowledge! manager 36 {:status :pending :command-id (:command-id first-launch)}
                                     (envelope "pending")))
          (send-midi! manager 23 [0x90 36 0]) (is (session/flush! manager))
          (let [release (last @commands)]
            (is (= [:cue/launch :cue/cancel-launch] (mapv :op @commands)))
            (is (= (:source first-launch) (:source release)))
            (is (= (:command-id first-launch) (:launch-command-id release)))
            (is (= :held-release (:reason release))))
          (is (false? (live-sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id first-launch)
                                                       :instance-id :late} (envelope "late"))))))
      (send-midi! manager 0 [0x90 36 100]) (is (session/flush! manager))
      (let [old-launch (last @commands)]
        (is (:stopped? (sync/stop! manager)))
        (is (= (:command-id old-launch) (:launch-command-id (last @commands))))
        (is (= (:source old-launch) (:source (last @commands))))
        (sync/start! manager cb) (is (session/flush! manager))
        (send-midi! manager 0 [0x90 36 100]) (is (session/flush! manager))
        (let [new-launch (last @commands) prior (count @commands)]
          (is (false? (live-sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id new-launch)
                                                    :sync-run-id (get-in old-launch [:source :sync-run-id]) :instance-id :wrong}
                                        (envelope "wrong"))))
          (is (= prior (count @commands)))
          (is (false? (live-sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id old-launch)
                                                       :instance-id :old} (envelope "end-old"))))
          (is (= prior (count @commands)))
          (is (live-sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id new-launch) :instance-id :new}
                                 (envelope "ack-new")))
          (send-midi! manager 0 [0x80 36 0]) (is (session/flush! manager))
          (is (= :new (:instance-id (last @commands))))
          (is (false? (live-sync/acknowledge! manager 36 {} (envelope "missing"))))))
      (finally (sync/close! manager)))))

(deftest handoff-releases-owned-held-instance-and-cc-resolves-current-instance
  (let [config (assoc-in midi-config [:live :parameters] {74 {:cue-id :red :param :level :bounds [10 20]}})
        manager (live-sync/create-manager {:config config :now-ms (constantly 0)
                                      :midi-factory (fake-midi (atom 0) (atom 0) (atom nil) (atom []))})
        commands (atom []) cb (assoc (callbacks commands (atom [])) :resolve-instance! (constantly :current))]
    (try
      (sync/start! manager cb) (is (session/flush! manager))
      (send-midi! manager 0 [0xb0 74 127]) (is (session/flush! manager))
      (is (= {:op :param/set :param :level :value 20.0 :instance-id :current}
             (select-keys (last @commands) [:op :param :value :instance-id])))
      (send-midi! manager 0 [0x90 36 100]) (is (session/flush! manager))
      (live-sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id (last @commands)) :instance-id :held}
                         (envelope "ack"))
      (is (live-sync/handoff! manager :live (envelope "handoff"))) (is (session/flush! manager))
      (is (= :held (:instance-id (last @commands))))
      (is (= :live (get-in (sync/export-state manager) [:config :live :mode])))
      (let [n (count @commands)]
        (send-midi! manager 0 [0x90 36 100]) (is (session/flush! manager))
        (is (= n (count @commands))))
      (finally (sync/close! manager)))))

(deftest outbound-routes-are-exclusive-and-delivery-is-not-a-clip-write
  (let [midi (atom []) requests (atom [])
        resources {:midi {:transport {:send! (fn [_ data] (swap! midi into data))}}
                   :live {:transport {:request! (fn [_ id op data] (swap! requests conj [id op data]) {:status :executed})}}}
        bridge (ableton/new-bridge {:mappings {36 {:cue-id :red}} :channel 2})
        cmd (merge (envelope "launch") {:op :cue/launch :cue-id :red})
        ack {:status :accepted :command-id "launch" :instance-id :red-1}
        recording {:track-armed? true :recording? true :writable? true :destination-id "clip:3" :beat 2.0 :song-beat 98.0}
        plan (ableton/record-command bridge cmd ack recording)]
    (is (= false (:clip-write-confirmed? (live-transport/execute-plan! resources :midi plan recording cmd ack))))
    (is (= [[0x91 36 127]] @midi)) (is (empty? @requests))
    (let [result (live-transport/execute-plan! resources :bridge plan recording cmd ack)]
      (is (:clip-write-confirmed? result)) (is (= :executed (:status result))))
    (is (= [[0x91 36 127]] @midi))
    (is (= :record-event (second (first @requests))))
    (is (= {:song-beat 98.0 :instance-id :red-1 :beat 2.0}
           (select-keys (nth (first @requests) 2) [:song-beat :instance-id :beat])))
    (let [uncertain (assoc-in resources [:live :transport :request!] (fn [& _] {:status :indeterminate}))]
      (is (= :indeterminate (:status (live-transport/execute-plan! uncertain :bridge plan recording cmd ack))))
      (is (false? (:clip-write-confirmed? (live-transport/execute-plan! uncertain :bridge plan recording cmd ack)))))))

(deftest real-local-http-bridge-validates-acknowledgement-identity
  (let [requests (atom []) wrong-id? (atom false)
        server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext server "/live"
      (reify HttpHandler
        (handle [_ exchange]
          (with-open [input (.getRequestBody exchange)]
            (let [request (json/read-str (slurp input) :key-fn keyword)
                  _ (swap! requests conj request)
                  body (.getBytes (json/write-str {:id (if @wrong-id? "wrong" (:id request)) :protocol-version 1
                                                   :status "executed" :result {:track-armed? true}}) "UTF-8")]
              (.sendResponseHeaders exchange 200 (alength body))
              (with-open [out (.getResponseBody exchange)] (.write out body)))))))
    (.start server)
    (let [transport (live-transport/transport {:endpoint (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/live")
                                               :timeout-ms 1500}) handle ((:open! transport) identity)]
      (try
        (is (empty? @requests))
        (is (= :executed (:status ((:request! transport) handle "one" :inspect {}))))
        (is (= {:id "one" :protocol-version 1 :op "inspect" :payload {}} (first @requests)))
        (reset! wrong-id? true)
        (is (thrown? clojure.lang.ExceptionInfo ((:request! transport) handle "two" :inspect {})))
        (finally ((:close! transport) handle) (.stop server 0))))))

(deftest malformed-live-status-cannot-replace-last-known-recording
  (let [manager (live-sync/create-manager {:now-ms (constantly 0)})]
    (try
      (sync/start! manager {:events! identity}) (is (session/flush! manager))
      (session/ingest! manager {:type :live/status :status {:beat 1 :clock {:beat 1 :bpm 120 :playing? true}}})
      (is (session/flush! manager))
      (let [valid (:recording (sync/status manager))]
        (session/ingest! manager {:type :live/status :status {:beat 2 :clock {:beat 2 :bpm 0 :playing? true}}})
        (is (session/flush! manager))
        (is (= valid (:recording (sync/status manager))))
        (is (= :invalid-live-status (get-in (sync/status manager) [:last-error :code]))))
      (finally (sync/close! manager)))))

(deftest recording-owner-gates-fresh-armed-status-and-deduplicates-receipts
  (let [now (atom 0) written (atom []) closed (atom 0) inspected (promise) executed (promise)
        recording {:track-armed? true :recording? true :writable? true :destination-id "clip:3"
                   :beat 2 :song-beat 98 :clock {:beat 98 :bpm 120 :playing? true}}
        manager (live-sync/create-manager
                 {:now-ms #(deref now)
                  :config {:live {:enabled? true :poll-ms 10000 :mappings {36 {:cue-id :red}}}}
                  :live-factory (fn [_]
                                  {:open! (fn [_] :live) :close! (fn [_] (swap! closed inc))
                                   :request! (fn [_ id op payload]
                                               (when (= :record-event op) (swap! written conj payload))
                                               {:id id :status :executed :result (when (= :inspect op) recording)})})})
        timing {:raw {:time-ms 100 :beat 98.2} :requested-at {:time-ms 150 :beat 98.3}
                :grid {:beats 1} :due-beat 99
                :dispatched-at {:time-ms 520 :beat 99.04} :admitted-at {:time-ms 520 :beat 99.04}}
        cmd (merge (envelope "record-one") {:op :cue/launch :cue-id :red :time-ms 520 :cue-timing timing})
        ack {:status :accepted :command-id "record-one" :instance-id :red-1}]
    (try
      (sync/start! manager {:events! (fn [event]
                                      (case (:type event) :live/status (deliver inspected true)
                                            :live/execution (deliver executed event) nil))})
      (is (true? (deref inspected 2000 false)))
      (is (live-sync/record! manager cmd (assoc ack :status :pending :instance-id nil)))
      (is (session/flush! manager))
      (is (empty? @written))
      (is (live-sync/record! manager cmd ack))
      (is (= :executed (:status (deref executed 2000 nil))))
      (is (= 1 (count @written)))
      (is (= :red-1 (:instance-id (first @written))))
      (is (= timing (:cue-timing (first @written))))
      (is (= 520 (:time-ms (first @written))))
      (is (= 3.04 (:beat (first @written))))
      (is (= 99.04 (:song-beat (first @written))))
      (is (live-sync/record! manager cmd ack)) (is (session/flush! manager))
      (is (= 1 (count @written)))
      (session/ingest! manager {:type :live/status :status (assoc recording :track-armed? false)})
      (live-sync/record! manager (assoc cmd :command-id "unarmed") (assoc ack :command-id "unarmed"))
      (is (session/flush! manager)) (is (= 1 (count @written)))
      (session/ingest! manager {:type :live/status :status recording :time-ms 0})
      (live-sync/record! manager (assoc cmd :command-id "stale" :time-ms 40000) (assoc ack :command-id "stale"))
      (live-sync/record! manager (assoc cmd :command-id "echo" :source {:origin :live-playback}) (assoc ack :command-id "echo"))
      (is (session/flush! manager)) (is (= 1 (count @written)))
      (is (:stopped? (sync/stop! manager))) (is (= 1 @closed))
      (finally (sync/close! manager)))))

(deftest pickup-lane-records-before-the-brace-and-loops-at-its-end
  (let [events (pickup/clip-events pickup/fixture)
        rows (filterv :level (pickup/playback events))
        recording {:track-armed? true :recording? true :writable? true :destination-id "clip:3"
                   :beat 0 :song-beat 0 :looping? true :loop-start 2 :loop-end 6
                   :clock {:beat 0 :bpm 120 :playing? true}}
        writes (java.util.concurrent.LinkedBlockingQueue.)
        inspected (promise)
        manager (live-sync/create-manager
                 {:now-ms (constantly 0)
                  :config {:live {:enabled? true :poll-ms 10000 :mappings {36 {:cue-id :wash}}
                                  :parameters {1 {:cue-id :wash :param :level
                                                  :bounds [0 1] :lane-note 100}}}}
                  :live-factory (fn [_]
                                  {:open! (constantly :live) :close! (constantly nil)
                                   :request! (fn [_ id op data]
                                               (when (= :record-event op) (.add writes data))
                                               {:id id :status :executed :result (when (= :inspect op) recording)})})})]
    (is (= [1/2 1 5/2 9/2 13/2 17/2] (mapv :song-beat rows)))
    (is (= [33 64 96 127 96 127] (mapv :velocity rows)))
    (is (= [1/2 1 5/2 9/2 5/2 9/2] (mapv :clip-beat rows)))
    (try
      (sync/start! manager {:events! #(when (= :live/status (:type %)) (deliver inspected true))})
      (is (true? (deref inspected 3000 false)))
      (live-sync/record! manager (assoc (envelope "start") :op :cue/launch :cue-id :wash)
                         {:status :accepted :command-id "start" :instance-id :gate})
      (let [launch (.poll writes 3 java.util.concurrent.TimeUnit/SECONDS)]
        (is (zero? (:beat launch))) (is (= 36 (get-in launch [:event :note]))))
      ;; Live reports entry into its loop before the first two earlier edits reach the recording worker.
      (session/ingest! manager {:type :live/status :time-ms 1000
                                :status (-> recording (assoc :beat 2 :song-beat 2) (assoc-in [:clock :beat] 2))})
      (doseq [{:keys [song-beat clip-beat velocity level]} rows]
        (let [id (str "rec-" song-beat) cmd (assoc (envelope id) :op :param/set :instance-id :gate
                                                   :param :level :value level :time-ms (* 500 song-beat))]
          (is (live-sync/record! manager cmd {:command-id id :status :accepted}))
          (let [intent (.poll writes 3 java.util.concurrent.TimeUnit/SECONDS)]
            (is (= [velocity 100] ((juxt :velocity :note) (:event intent))))
            (is (= (mapv double [song-beat clip-beat]) ((juxt :song-beat :beat) intent))))))
      (session/ingest! manager {:type :live/status :time-ms 5000
                                :status (assoc recording :destination-id "clip:other")})
      (doseq [[id at] [["previous-clip" 4750] ["new-clip" 5000]]]
        (live-sync/record! manager (assoc (envelope id) :op :param/set :instance-id :gate
                                         :time-ms at :param :level :value 1)
                           {:status :accepted :command-id id}))
      (is (= ["new-clip" "clip:other"] ((juxt :command-id :destination-id)
                                         (.poll writes 3 java.util.concurrent.TimeUnit/SECONDS))))
      (finally (sync/close! manager)))))

(deftest a-manually-edited-midi-export-changes-only-the-pickup-step
  (with-open [input (io/input-stream pickup/fixture)]
    (let [clip (MidiSystem/getSequence input)
          track (first (.getTracks clip))
          half-beat (quot (.getResolution clip) 2)
          export (ByteArrayOutputStream.)]
      (doseq [i (range (.size track))
              :let [event (.get track i) message (.getMessage event)]
              :when (and (= half-beat (.getTick event)) (instance? ShortMessage message)
                         (= ShortMessage/NOTE_ON (.getCommand message)) (= 100 (.getData1 message)))]
        (.setMessage message ShortMessage/NOTE_ON (.getChannel message) 100 96))
      (MidiSystem/write clip 1 export)
      (let [trace (pickup/playback (pickup/clip-events (.toByteArray export)))
            levels (filterv :level trace)]
        (is (= [96 64 96 127 96 127] (mapv :velocity levels)))
        (is (= (double (/ 95 126)) (:level (first levels))))
        (is (= [0 2 6] (mapv :song-beat (filter #(= :cue/launch (:command %)) trace))))
        (is (= [3/2 11/2 19/2] (mapv :song-beat (filter #(= :instance/end (:command %)) trace))))))))

(deftest live-callbacks-require-current-bridge-and-session-and-deduplicate-events
  (let [requests (atom []) commands (atom [])
        manager (live-sync/create-manager
                 {:now-ms (constantly 35)
                  :config {:live {:enabled? true :launch-grid {:bars 1}
                                  :mappings {36 {:cue-id :red :mode :persistent :grid {:beats 1/2}}}}}
                  :live-factory (fn [_] {:open! (fn [_] :live) :close! (fn [_])
                                         :request! (fn [_ id op payload]
                                                     (swap! requests conj [op payload])
                                                     {:id id :status :executed :result {}})})})
        cb (callbacks commands (atom []))
        base {:protocol-version 1 :bridge-id "tolglow-m4l" :kind "midi" :event-id "process:1"
              :event {:type "note-on" :channel 1 :note 36 :velocity 100 :time-ms 999 :source-id :untrusted}}]
    (try
      (is (false? (live-sync/ingest-live! manager base)))
      (let [first-run (sync/start! manager cb) message (assoc base :sync-session-id (:run-id first-run))]
        (is (session/flush! manager))
        (doseq [invalid [(assoc message :kind 10) (assoc message :event 4) (dissoc message :event-id)
                         (assoc message :bridge-id "another") (assoc message :sync-session-id "old")]]
          (is (false? (live-sync/ingest-live! manager invalid))))
        (is (true? (live-sync/ingest-live! manager message)))
        (is (false? (live-sync/ingest-live! manager message)))
        (is (session/flush! manager)) (is (= 1 (count @commands)))
        (let [launch (first @commands)]
          (is (= {:beats 1/2} (:grid launch)))
          (is (= {:time-ms 35} (:raw launch)))
          (is (= :ableton-live (get-in launch [:source :source-id])))
          (is (live-sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id launch)
                                                  :instance-id :live-1} (envelope "fired"))))
        (is (:stopped? (sync/stop! manager)))
        (is (false? (live-sync/ingest-live! manager (assoc message :event-id "process:2"))))
        (let [second-run (sync/start! manager cb)]
          (is (session/flush! manager))
          (is (false? (live-sync/ingest-live! manager (assoc message :event-id "process:3"))))
          (is (true? (live-sync/ingest-live! manager (assoc message :event-id "process:4" :sync-session-id (:run-id second-run)))))
          (is (session/flush! manager)) (is (= 2 (count @commands)))))
      (finally (sync/close! manager)))))

(deftest captured-midi-emits-typed-patch-observations-without-needing-cue-mappings
  (let [events (atom []) commands (atom [])
        manager (live-sync/create-manager {:now-ms (constantly 20) :config {:midi {:control? false}}})]
    (try
      (sync/start! manager (callbacks commands events))
      (send-midi! manager 10 [0x91 60 100])
      (send-midi! manager 11 [0x91 60 0])
      (send-midi! manager 12 [0xb2 74 127])
      (session/ingest! manager {:type :live/midi :event {:type :cc :channel 1 :controller 1 :value 63}})
      (session/ingest! manager {:type :live/midi :event {:type :note-on :channel 1 :note 60 :velocity 100 :origin :tolglow-feedback}})
      (is (session/flush! manager))
      (let [observations (mapv :observation (filter #(= :sync/observation (:type %)) @events))]
        (is (= ["midi/midi-clock/ch/2/note/60" "midi/midi-clock/ch/2/note/60" "midi/midi-clock/ch/3/cc/74"
                "live/ableton-live/ch/1/cc/1"] (mapv :source-id observations)))
        (is (= [100 0 127 63] (mapv :value observations)))
        (is (= [true false true true] (mapv :gate? observations)))
        (is (= [10 11 12 20] (mapv :observed-at-ms observations)))
        (is (= [:midi-input :midi-input :midi-input :live-playback] (mapv :origin observations)))
        (is (= 4 (count (set (map :event-id observations)))))
        (is (= {:transport :midi :source-id :midi-clock :channel 2 :signal :velocity :note 60} (:port (first observations))))
        (is (every? #(and (not (contains? % :clock)) (not (contains? % :epoch))) observations)))
      (is (empty? @commands))
      (finally (sync/close! manager)))))

(deftest late-live-inspection-cannot-rearm-a-clip-after-pushed-stop
  (let [reading (promise) release (promise) inspections (atom 0)
        status {:destination-id "clip:old" :beat 0 :track-armed? true :recording? true :writable? true}
        transport {:open! (fn [_] :opened) :close! (fn [_])
                   :request! (fn [_ id op _]
                               (when (= op :inspect)
                                 (swap! inspections inc)
                                 (deliver reading true)
                                 (deref release 4000 nil))
                               {:id id :status :executed :result status})}
        manager (live-sync/create-manager {:now-ms (constantly 0) :config {:live {:enabled? true}}
                                      :live-factory (fn [_] transport)})]
    (try
      (sync/start! manager {:events! identity})
      (is (true? (deref reading 2000 false)))
      (let [stopped (assoc status :track-armed? false :recording? false)]
        (is (session/ingest! manager {:type :live/status :status stopped}))
        (is (session/flush! manager))
        (deliver release true)
        (is (loop [deadline (+ (System/nanoTime) 3000000000)]
              (cond (pos? (get-in (sync/status manager) [:metrics :stale-live-statuses] 0)) true
                    (>= (System/nanoTime) deadline) false
                    :else (do (Thread/sleep 10) (recur deadline)))))
        (is (= stopped (select-keys (:recording (sync/status manager)) (keys stopped))))
        (is (= 1 @inspections)))
      (finally (deliver release true) (sync/close! manager)))))

(deftest live-parameter-source-preserves-native-units-and-typed-restore-data
  (let [spec {:path "live_set tracks 0 devices 1 chains 2 devices 3 parameters 4"
              :track-name "Keys" :device-name "Synth" :name "Cutoff" :range [20 20000] :quantized? false}
        config (live-sync/validate-config {:live {:source-id :performance/live :observed-parameters {:cutoff spec}}})
        live (:live config)
        sample {:id "cutoff" :path (:path spec) :value 305.25 :automation-state 1}
        observation #(get (live-params/observation live "event-1" 7.5 %) :observation)]
    (is (= {"cutoff" spec} (:observed-parameters live)))
    (is (= config (:config (live-sync/validate-state {:version 1 :config config}))))
    (is (= {:source-id "live/performance/live/parameter/cutoff" :value 305.25 :kind :automation}
           (select-keys (observation sample) [:source-id :value :kind])))
    (is (true? (:gate? (observation (assoc sample :value (first (:range spec)))))))
    (is (= :playing (get-in (observation sample) [:port :automation-state])))
    (is (= [20 20000] (get-in (observation sample) [:port :range])))
    (is (nil? (observation (assoc sample :value ##NaN))))
    (is (nil? (observation (assoc sample :id "another"))))
    (is (nil? (observation (assoc sample :path "live_set tracks 1 mixer_device volume"))))
    (is (nil? (live-params/observation (assoc live :mode :live) "event-2" 8 sample)))
    (is (thrown? clojure.lang.ExceptionInfo (live-params/descriptor (assoc spec :path "live_set tracks 0"))))))

(deftest parameter-routing-follows-live-ownership-and-input-lifetime
  (let [config (assoc-in midi-config [:live :parameters] {11 {:cue-id :red :param :level :bounds [0 1]}})
        manager (live-sync/create-manager {:config config :now-ms (constantly 0)
                                           :midi-factory (fake-midi (atom 0) (atom 0) (atom nil) (atom []))})]
    (try
      (is (nil? (live-sync/parameter-routing manager)))
      (sync/start! manager (callbacks (atom []) (atom [])))
      (is (session/flush! manager))
      (let [routing (live-sync/parameter-routing manager)]
        (is (= :level (get-in routing [:parameters 11 :param])))
        (is (= [:midi-clock] (:source-ids routing))))
      (live-sync/handoff! manager :live (envelope "handoff"))
      (is (session/flush! manager))
      (is (nil? (live-sync/parameter-routing manager)))
      (live-sync/handoff! manager :lighting (envelope "return"))
      (is (session/flush! manager))
      (is (some? (live-sync/parameter-routing manager)))
      (sync/stop! manager)
      (is (nil? (live-sync/parameter-routing manager)))
      (finally (sync/close! manager)))))
