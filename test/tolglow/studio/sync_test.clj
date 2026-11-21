(ns tolglow.studio.sync-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [tolglow.studio.sync :as sync]
            [tolglow.integrations.ableton :as ableton])
  (:import [java.net DatagramPacket InetAddress InetSocketAddress]
           [com.sun.net.httpserver HttpServer HttpHandler]
           [org.deepsymmetry.beatlink Beat DeviceAnnouncement]
           [java.util.concurrent CountDownLatch TimeUnit]))

(defn near? [a b] (< (Math/abs (double (- a b))) 1.0e-7))
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
  (is (sync/ingest! manager {:type :midi/message :source-id :midi-clock :time-ms at :bytes bytes})))

(deftest config-and-restore-are-inert-and-portable
  (let [opens (atom 0) factory (fn [& _] (swap! opens inc) (throw (Exception. "unexpected factory")))
        manager (sync/create-manager {:midi-factory factory :beat-link-factory factory :live-factory factory})]
    (try
      (let [state {:version 1 :wanted? true
                   :config {:selected-source "ableton-live"
                            :live {:enabled? true :mappings {"36" {:cue-id :red :mode "timed" :duration {:bars 2}}}
                                   :pairing {:track-index 0 :clip-slot-index 3 :expected-track-name "Lights"}}}}
            normalized (sync/validate-state state)]
        (sync/restore! manager normalized)
        (is (= 0 @opens))
        (is (= :stopped (:state (sync/status manager))))
        (is (= {:bars 2} (get-in (sync/export-state manager) [:config :live :mappings 36 :duration])))
        (is (= (assoc normalized :wanted? false) (sync/export-state manager)))
        (is (= :waiting (:status (sync/clock-at manager 0))))
        (sync/select-source! manager :manual)
        (is (nil? (:clock (sync/clock-at manager 0)))))
      (doseq [config [{:selected-source :unknown} {:midi {:enabled? true}}
                      {:live {:endpoint "https://example.com/live"}}
                      {:live {:pairing {:track-id 42}}}
                      {:live {:pairing {:track-index -1 :clip-slot-index 0}}}
                      {:live {:mappings {36 {:cue-id :red :duration {:beats 1 :ms 5}}}}}
                      {:live {:parameters {1 {:cue-id :red :param :level :instance-id 7}}}}
                      {:beat-link {:source-id :midi-clock}}]]
        (is (thrown? clojure.lang.ExceptionInfo (sync/validate-config config))))
      (is (thrown? clojure.lang.ExceptionInfo (sync/validate-state {:version 1 :config {} :wanted? :yes})))
      (finally (sync/close! manager)))))

(deftest explicit-start-ticks-stop-and-late-input-ownership
  (let [opens (atom 0) closes (atom 0) receive (atom nil) now (atom 0.0)
        manager (sync/create-manager {:config midi-config :now-ms #(deref now)
                                      :midi-factory (fake-midi opens closes receive (atom []))})
        commands (atom []) events (atom [])]
    (try
      (is (false? (sync/ingest! manager {:type :anything})))
      (sync/start! manager (callbacks commands events))
      (is (sync/flush! manager))
      (is (= 1 @opens))
      (is (thrown? clojure.lang.ExceptionInfo (sync/configure! manager {})))
      (send-midi! manager 0 [0xfa])
      (doseq [tick (range 25)] (send-midi! manager (* tick (/ 500.0 24)) [0xf8]))
      (reset! now 500.0)
      (is (sync/flush! manager))
      (let [clock (:clock (sync/clock-at manager 500))]
        (is (near? 1.0 (:beat clock))) (is (near? 120 (:bpm clock)))
        (is (near? 1.0 (:confidence clock))) (is (:playing? clock)))
      (send-midi! manager 500 [0xfc])
      (is (sync/flush! manager))
      (is (false? (get-in (sync/clock-at manager 1000) [:clock :playing?])))
      (is (:stopped? (sync/stop! manager)))
      (is (= :disconnected (:status (sync/clock-at manager 1000))))
      (is (= 1 @closes))
      (is (false? (@receive {:type :midi/message :source-id :midi-clock :time-ms 1100 :bytes [0xfa]})))
      (is (:stopped? (sync/stop! manager)))
      (is (= 1 @closes))
      (finally (sync/close! manager)))))

(deftest held-off-before-ack-and-old-session-acks-target-exact-instances
  (let [manager (sync/create-manager {:config midi-config :now-ms (constantly 0)
                                      :midi-factory (fake-midi (atom 0) (atom 0) (atom nil) (atom []))})
        commands (atom []) cb (callbacks commands (atom []))]
    (try
      (let [run (sync/start! manager cb)]
        (is (sync/flush! manager))
        (send-midi! manager 0 [0x90 36 100]) (is (sync/flush! manager))
        (let [first-launch (last @commands)]
          (is (= :cue/launch (:op first-launch)))
          (is (= 36 (get-in first-launch [:source :note])))
          (is (= (:run-id run) (get-in first-launch [:source :sync-run-id])))
          (send-midi! manager 0 [0x80 36 0]) (is (sync/flush! manager))
          (is (= 1 (count @commands)))
          (is (sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id first-launch) :instance-id :original}
                                 (envelope "end-original")))
          (is (= [:cue/launch :instance/end] (mapv :op @commands)))
          (is (= :original (:instance-id (last @commands))))))
      (send-midi! manager 0 [0x90 36 100]) (is (sync/flush! manager))
      (let [old-launch (last @commands)]
        (is (:stopped? (sync/stop! manager)))
        (sync/start! manager cb) (is (sync/flush! manager))
        (send-midi! manager 0 [0x90 36 100]) (is (sync/flush! manager))
        (let [new-launch (last @commands) prior (count @commands)]
          (is (false? (sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id new-launch)
                                                    :sync-run-id (get-in old-launch [:source :sync-run-id]) :instance-id :wrong}
                                        (envelope "wrong"))))
          (is (= prior (count @commands)))
          (is (sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id old-launch) :instance-id :old}
                                 (envelope "end-old")))
          (is (= :old (:instance-id (last @commands))))
          (is (sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id new-launch) :instance-id :new}
                                 (envelope "ack-new")))
          (send-midi! manager 0 [0x80 36 0]) (is (sync/flush! manager))
          (is (= :new (:instance-id (last @commands))))
          (is (false? (sync/acknowledge! manager 36 {} (envelope "missing"))))))
      (finally (sync/close! manager)))))

(deftest handoff-releases-owned-held-instance-and-cc-resolves-current-instance
  (let [config (assoc-in midi-config [:live :parameters] {74 {:cue-id :red :param :level :bounds [10 20]}})
        manager (sync/create-manager {:config config :now-ms (constantly 0)
                                      :midi-factory (fake-midi (atom 0) (atom 0) (atom nil) (atom []))})
        commands (atom []) cb (assoc (callbacks commands (atom [])) :resolve-instance! (constantly :current))]
    (try
      (sync/start! manager cb) (is (sync/flush! manager))
      (send-midi! manager 0 [0xb0 74 127]) (is (sync/flush! manager))
      (is (= {:op :param/set :param :level :value 20.0 :instance-id :current}
             (select-keys (last @commands) [:op :param :value :instance-id])))
      (send-midi! manager 0 [0x90 36 100]) (is (sync/flush! manager))
      (sync/acknowledge! manager 36 {:status :accepted :command-id (:command-id (last @commands)) :instance-id :held}
                         (envelope "ack"))
      (is (sync/handoff! manager :live (envelope "handoff"))) (is (sync/flush! manager))
      (is (= :held (:instance-id (last @commands))))
      (is (= :live (get-in (sync/export-state manager) [:config :live :mode])))
      (let [n (count @commands)]
        (send-midi! manager 0 [0x90 36 100]) (is (sync/flush! manager))
        (is (= n (count @commands))))
      (finally (sync/close! manager)))))

(deftest real-beat-link-getters-convert-packet-age-and-announcement-identity
  ;; Construct actual library data objects; no singleton listeners or sockets.
  (let [bytes (byte-array 96) address (InetAddress/getByAddress (byte-array [10 0 0 2]))]
    (doseq [[i value] [[0x21 2] [0x55 0x10] [0x5a 0x2e] [0x5b 0xe0] [0x5c 3]]]
      (aset-byte bytes i (unchecked-byte value)))
    (System/arraycopy (.getBytes "CDJ-3000" "UTF-8") 0 bytes 0x0b 8)
    (let [beat (Beat. (DatagramPacket. bytes 96 address 50001))
          data (sync/beat-data beat 1000 (+ (.getTimestamp beat) 2500000))]
      (is (= 997.5 (:time-ms data))) (is (= 2.5 (:packet-age-ms data)))
      (is (= 120.0 (:bpm data))) (is (= 3 (:beat-within-bar data)))
      (is (= {:device-number 2 :device-name "CDJ-3000" :address "10.0.0.2"} (:peer data))))
    (let [packet (byte-array 54)]
      (aset-byte packet 0x24 (byte 2))
      (doseq [[index b] (map-indexed vector [0 1 2 3 4 255])] (aset-byte packet (+ 0x26 index) (unchecked-byte b)))
      (System/arraycopy (.getBytes "CDJ-3000" "UTF-8") 0 packet 0x0c 8)
      (is (= {:device-number 2 :device-name "CDJ-3000" :address "10.0.0.2"
              :hardware-address "00:01:02:03:04:ff" :observed-at-ms 12}
             (sync/announcement-data (DeviceAnnouncement. (DatagramPacket. packet 54 address 50000)) 12))))))

(deftest beat-link-pairing-does-not-follow-a-different-player
  (let [peer {:device-number 2 :address "10.0.0.2" :hardware-address "00:01:02:03:04:ff"}
        manager (sync/create-manager {:now-ms (constantly 1000)
                                      :config {:selected-source :beat-link
                                               :beat-link {:peer (select-keys peer [:device-number :hardware-address]) :align-bars? true}}})]
    (try
      (sync/start! manager {:events! identity}) (is (sync/flush! manager))
      (sync/ingest! manager {:type :peer/found :peer peer})
      (sync/ingest! manager {:type :beat-link/beat :source-id :beat-link :time-ms 1000 :bpm 120
                            :beat-within-bar 3 :bar-meaningful? true :peer (assoc peer :device-number 3)})
      (is (sync/flush! manager)) (is (= :waiting (:status (sync/clock-at manager 1000))))
      (sync/ingest! manager {:type :beat-link/beat :source-id :beat-link :time-ms 1000 :bpm 120
                            :beat-within-bar 3 :bar-meaningful? true :peer peer})
      (is (sync/flush! manager)) (is (= 2.0 (get-in (sync/clock-at manager 1000) [:clock :beat])))
      (sync/ingest! manager {:type :peer/lost :peer peer}) (is (sync/flush! manager))
      (is (= :disconnected (:status (sync/clock-at manager 1000))))
      (is (empty? (:peers (sync/status manager))))
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
    (is (= false (:clip-write-confirmed? (sync/execute-plan! resources :midi plan recording cmd ack))))
    (is (= [[0x91 36 127]] @midi)) (is (empty? @requests))
    (let [result (sync/execute-plan! resources :bridge plan recording cmd ack)]
      (is (:clip-write-confirmed? result)) (is (= :executed (:status result))))
    (is (= [[0x91 36 127]] @midi))
    (is (= :record-event (second (first @requests))))
    (is (= {:song-beat 98.0 :instance-id :red-1 :beat 2.0}
           (select-keys (nth (first @requests) 2) [:song-beat :instance-id :beat])))
    (let [uncertain (assoc-in resources [:live :transport :request!] (fn [& _] {:status :indeterminate}))]
      (is (= :indeterminate (:status (sync/execute-plan! uncertain :bridge plan recording cmd ack))))
      (is (false? (:clip-write-confirmed? (sync/execute-plan! uncertain :bridge plan recording cmd ack)))))))

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
    (let [transport (sync/live-http-transport {:endpoint (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/live")
                                               :timeout-ms 1500}) handle ((:open! transport) identity)]
      (try
        (is (empty? @requests))
        (is (= :executed (:status ((:request! transport) handle "one" :inspect {}))))
        (is (= {:id "one" :protocol-version 1 :op "inspect" :payload {}} (first @requests)))
        (reset! wrong-id? true)
        (is (thrown? clojure.lang.ExceptionInfo ((:request! transport) handle "two" :inspect {})))
        (finally ((:close! transport) handle) (.stop server 0))))))

(deftest malformed-live-status-cannot-replace-last-known-recording
  (let [manager (sync/create-manager {:now-ms (constantly 0)})]
    (try
      (sync/start! manager {:events! identity}) (is (sync/flush! manager))
      (sync/ingest! manager {:type :live/status :status {:beat 1 :clock {:beat 1 :bpm 120 :playing? true}}})
      (is (sync/flush! manager))
      (let [valid (:recording (sync/status manager))]
        (sync/ingest! manager {:type :live/status :status {:beat 2 :clock {:beat 2 :bpm 0 :playing? true}}})
        (is (sync/flush! manager))
        (is (= valid (:recording (sync/status manager))))
        (is (= :invalid-live-status (get-in (sync/status manager) [:last-error :code]))))
      (finally (sync/close! manager)))))

(deftest recording-owner-gates-fresh-armed-status-and-deduplicates-receipts
  (let [now (atom 0) written (atom []) closed (atom 0) inspected (promise) executed (promise)
        recording {:track-armed? true :recording? true :writable? true :destination-id "clip:3"
                   :beat 2 :song-beat 98 :clock {:beat 98 :bpm 120 :playing? true}}
        manager (sync/create-manager
                 {:now-ms #(deref now)
                  :config {:live {:enabled? true :poll-ms 10000 :mappings {36 {:cue-id :red}}}}
                  :live-factory (fn [_]
                                  {:open! (fn [_] :live) :close! (fn [_] (swap! closed inc))
                                   :request! (fn [_ id op payload]
                                               (when (= :record-event op) (swap! written conj payload))
                                               {:id id :status :executed :result (when (= :inspect op) recording)})})})
        cmd (merge (envelope "record-one") {:op :cue/launch :cue-id :red})
        ack {:status :accepted :command-id "record-one" :instance-id :red-1}]
    (try
      (sync/start! manager {:events! (fn [event]
                                      (case (:type event) :live/status (deliver inspected true)
                                            :live/execution (deliver executed event) nil))})
      (is (true? (deref inspected 2000 false)))
      (is (sync/record! manager cmd ack))
      (is (= :executed (:status (deref executed 2000 nil))))
      (is (= 1 (count @written)))
      (is (= :red-1 (:instance-id (first @written))))
      (is (sync/record! manager cmd ack)) (is (sync/flush! manager))
      (is (= 1 (count @written)))
      (sync/ingest! manager {:type :live/status :status (assoc recording :track-armed? false)})
      (sync/record! manager (assoc cmd :command-id "unarmed") (assoc ack :command-id "unarmed"))
      (is (sync/flush! manager)) (is (= 1 (count @written)))
      (sync/ingest! manager {:type :live/status :status recording :time-ms 0})
      (sync/record! manager (assoc cmd :command-id "stale" :time-ms 40000) (assoc ack :command-id "stale"))
      (sync/record! manager (assoc cmd :command-id "echo" :source {:origin :live-playback}) (assoc ack :command-id "echo"))
      (is (sync/flush! manager)) (is (= 1 (count @written)))
      (is (:stopped? (sync/stop! manager))) (is (= 1 @closed))
      (finally (sync/close! manager)))))

(deftest cancellation-interrupts-acquisition-and-closes-already-owned-resources
  (let [entered (promise) release (CountDownLatch. 1) closed (atom 0)
        manager (sync/create-manager {:config {:beat-link {:enabled? true} :midi {:enabled? true :input-index 0}}
                                      :beat-link-factory (fn [& _] {:open! (fn [_] :dj) :close! (fn [_] (swap! closed inc))})
                                      :midi-factory (fn [& _] {:open! (fn [_] (deliver entered true) (.await release 5 TimeUnit/SECONDS) :midi)
                                                             :close! (fn [_] (swap! closed inc))})})]
    (try
      (sync/start! manager {:events! identity})
      (is (true? (deref entered 1000 false)))
      (is (:stopped? (sync/stop! manager)))
      (is (= 1 @closed))
      (is (false? (:active? (sync/status manager))))
      (finally (.countDown release) (sync/close! manager)))))

(deftest live-callbacks-require-current-bridge-and-session-and-deduplicate-events
  (let [requests (atom []) commands (atom [])
        manager (sync/create-manager
                 {:now-ms (constantly 0)
                  :config {:live {:enabled? true :mappings {36 {:cue-id :red :mode :persistent}}}}
                  :live-factory (fn [_] {:open! (fn [_] :live) :close! (fn [_])
                                         :request! (fn [_ id op payload]
                                                     (swap! requests conj [op payload])
                                                     {:id id :status :executed :result {}})})})
        cb (callbacks commands (atom []))
        base {:protocol-version 1 :bridge-id "tolglow-m4l" :kind "midi" :event-id "process:1"
              :event {:type "note-on" :channel 1 :note 36 :velocity 100}}]
    (try
      (is (false? (sync/ingest-live! manager base)))
      (let [first-run (sync/start! manager cb) message (assoc base :sync-session-id (:run-id first-run))]
        (is (sync/flush! manager))
        (doseq [invalid [(assoc message :kind 10) (assoc message :event 4) (dissoc message :event-id)
                         (assoc message :bridge-id "another") (assoc message :sync-session-id "old")]]
          (is (false? (sync/ingest-live! manager invalid))))
        (is (true? (sync/ingest-live! manager message)))
        (is (false? (sync/ingest-live! manager message)))
        (is (sync/flush! manager)) (is (= 1 (count @commands)))
        (is (:stopped? (sync/stop! manager)))
        (is (false? (sync/ingest-live! manager (assoc message :event-id "process:2"))))
        (let [second-run (sync/start! manager cb)]
          (is (sync/flush! manager))
          (is (false? (sync/ingest-live! manager (assoc message :event-id "process:3"))))
          (is (true? (sync/ingest-live! manager (assoc message :event-id "process:4" :sync-session-id (:run-id second-run)))))
          (is (sync/flush! manager)) (is (= 2 (count @commands)))))
      (finally (sync/close! manager)))))

(deftest captured-midi-emits-typed-patch-observations-without-needing-cue-mappings
  (let [events (atom []) commands (atom [])
        manager (sync/create-manager {:now-ms (constantly 20) :config {:midi {:control? false}}})]
    (try
      (sync/start! manager (callbacks commands events))
      (send-midi! manager 10 [0x91 60 100])
      (send-midi! manager 11 [0x91 60 0])
      (send-midi! manager 12 [0xb2 74 127])
      (sync/ingest! manager {:type :live/midi :event {:type :cc :channel 1 :controller 1 :value 63}})
      (sync/ingest! manager {:type :live/midi :event {:type :note-on :channel 1 :note 60 :velocity 100 :origin :tolglow-feedback}})
      (is (sync/flush! manager))
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

(deftest open-failure-cleans-prior-resources-and-can-be-retried
  (let [closed (atom 0) attempts (atom 0)
        manager (sync/create-manager {:config {:beat-link {:enabled? true} :midi {:enabled? true :input-index 0}}
                                      :beat-link-factory (fn [& _] {:open! (fn [_] :beatlink) :close! (fn [_] (swap! closed inc))})
                                      :midi-factory (fn [& _] (swap! attempts inc) (throw (Exception. "injected open failure")))})]
    (try
      (dotimes [_ 2]
        (let [run (sync/start! manager {:events! identity})]
          (is (= :error (:status (deref (:result run) 2000 nil))))
          (is (false? (:active? (sync/status manager))))))
      (is (= 2 @closed @attempts))
      (finally (sync/close! manager)))))

(deftest overflow-reports-lost-phase-and-recovers-only-on-explicit-resync
  (let [gate (CountDownLatch. 1) entered (promise) overflow (promise) events (atom []) now (atom 0)
        manager (sync/create-manager {:config {:selected-source :midi-clock} :queue-capacity 8 :now-ms #(deref now)})]
    (try
      (sync/start! manager {:events! (fn [event]
                                      (swap! events conj event)
                                      (when (= :sync/input-overflow (:type event)) (deliver overflow true))
                                      (when (and (= :sync/clock (:type event)) (not (realized? entered)))
                                        (deliver entered true) (.await gate 2 TimeUnit/SECONDS)))})
      (send-midi! manager 0 [0xfa]) (is (true? (deref entered 1000 false)))
      (doseq [tick (range 8)] (is (sync/ingest! manager {:type :midi/message :source-id :midi-clock :time-ms tick :bytes [0xf8]})))
      (is (false? (sync/ingest! manager {:type :midi/message :source-id :midi-clock :time-ms 9 :bytes [0xf8]})))
      (reset! now 100) (.countDown gate)
      (is (true? (deref overflow 1000 false)))
      (is (= 1 (get-in (sync/status manager) [:metrics :dropped-input-events])))
      (is (some #(= :sync/input-overflow (:type %)) @events))
      (is (false? (get-in (sync/clock-at manager 100) [:clock :playing?])))
      ;; The overflow callback runs before dequeueing the old backlog. Wait for
      ;; a barrier using its queue directly so test admission cannot add a loss.
      (let [barrier (promise) run (:active @(:state manager)) queue (:queue run)]
        (is (.offer queue {:type :barrier :promise barrier ::sync/generation @(:binding-generation run)} 1000 TimeUnit/MILLISECONDS))
        (is (true? (deref barrier 1000 false))))
      (send-midi! manager 100 [0xfa]) (send-midi! manager 100 [0xf8]) (is (sync/flush! manager))
      (is (true? (get-in (sync/clock-at manager 100) [:clock :playing?])))
      (finally (.countDown gate) (sync/close! manager)))))
