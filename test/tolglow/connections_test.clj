(ns tolglow.connections-test
  (:require [afterglow.control.model :as model]
            [afterglow.reactive :as reactive]
            [clojure.edn :as edn]
            [clojure.test :refer :all]
            [tolglow.agents :as agents]
            [tolglow.audio.capture :as audio]
            [tolglow.connections :as connections]
            [tolglow.fixtures.library :as fixtures]
            [tolglow.persistence :as persistence]
            [afterglow.sync :as sync]
            [afterglow.sync.session :as session]
            [tolglow.sensors :as sensors])
  (:import [java.util.concurrent CountDownLatch TimeUnit]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn show [] (model/new-show {:show-id :test :epoch 3 :value 2}))

(defn no-io-options [calls]
  {:credential-store {:get! (fn [_] (swap! calls conj :credential-get) nil)
                      :put! (fn [_] (swap! calls conj :credential-put) "credential-00000000-0000-0000-0000-000000000001")
                      :delete! (fn [_] (swap! calls conj :credential-delete))}
   :agent-options {:transport (fn [& _] (swap! calls conj :provider))}
   :fixture-options {:provider (fn [& _] (swap! calls conj :fixture-provider))}
   :audio-options {:open-source! (fn [_] (swap! calls conj :audio-open))}})

(defn error-code [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:connections/error (ex-data error)))))

(def agent-state
  {:version 1 :config {:provider :self-hosted :model "local" :endpoint "http://127.0.0.1:1234/v1/chat/completions"}
   :credential-ref nil :selected-conversation-id "saved"
   :conversations [{:id "saved" :title "Saved conversation" :messages [{:role :user :content "Red please"}
                                                                       {:role :assistant :content "Ready"}]
                    :status :completed :updated-at-ms 1000}]})

(defn value-sub [{:keys [db]} _] (:value @db))

(defn change-fx [_ value _] {:op :master/set :value (/ value 10.0)})

(def sub-descriptor {:id ::value :kind :sub :implementation {:type :var :symbol "tolglow.connections-test/value-sub"}})

(def fx-descriptor {:id ::change :kind :fx :implementation {:type :var :symbol "tolglow.connections-test/change-fx"}})

(deftest construction-export-and-close-are-inert-and-owned
  (let [calls (atom []) conn (connections/create (show) (no-io-options calls))
        agent (connections/agents conn) capture (connections/audio conn) fixture (connections/fixtures conn)
        graph (connections/reactive conn)]
    (try
      (is (= [] @calls))
      (is (= {:closed? false :restoring? false :generation 0 :cleanup-errors []} (connections/status conn)))
      (is (= :idle (:state (audio/status capture))))
      (doseq [callbacks [nil {:events! 3} {:events! identity :clock! 7}]]
        (is (= :invalid-callbacks (error-code #(connections/start-audio! conn {:default? true} callbacks)))))
      (let [data (connections/export-state conn)]
        (is (= data (edn/read-string (pr-str data))))
        (is (= 4194304 (get-in data [:integrations :fixtures :limits :max-cache-bytes])))
        (is (= {:version 1 :config nil :capture-wanted? false} (get-in data [:integrations :audio-capture])))
        (is (= [] (get-in data [:integrations :agents :conversations]))))
      (is (:closed? (connections/close! conn)))
      (is (= [] (:cleanup-errors (connections/close! conn))))
      (is (:closed? (audio/status capture)))
      (is (thrown? clojure.lang.ExceptionInfo (fixtures/export-state fixture)))
      (is (thrown? clojure.lang.ExceptionInfo (reactive/publish! graph)))
      (is (false? (:ready? (agents/public-config agent))))
      (doseq [operation [connections/agents connections/audio connections/fixtures connections/reactive connections/sync connections/export-state]]
        (is (= :closed (error-code #(operation conn)))))
      (is (= [] @calls))
      (finally (connections/close! conn)))))

(deftest default-credential-adapter-does-not-create-files-at-construction
  (let [directory (Files/createTempDirectory "tolglow-connections" (make-array FileAttribute 0))
        file (.resolve directory "new-directory/credentials.edn")
        conn (connections/create (show) {:credential-path (str file)})]
    (try
      (is (not (Files/exists file (make-array java.nio.file.LinkOption 0))))
      (connections/export-state conn)
      (is (not (Files/exists (.getParent file) (make-array java.nio.file.LinkOption 0))))
      (finally (connections/close! conn) (Files/delete directory)))))

(deftest export-retains-settings-chat-and-logical-pairings-without-replaying-tools
  (let [calls (atom []) initial (assoc (show) :integrations {:agents agent-state
                                                           :ableton {:peer "laptop" :port-id "midi-2" :mode :lighting}
                                                           :beat-link {:player 2 :enabled? true}
                                                           :audio {:input-gain 0.8}})
        conn (connections/create initial (no-io-options calls))]
    (try
      (let [data (connections/export-state conn)
            replacement (merge (assoc initial :epoch 4) data)]
        (is (= "saved" (get-in data [:integrations :agents :selected-conversation-id])))
        (is (= "midi-2" (get-in data [:integrations :ableton :port-id])))
        (is (= {:input-gain 0.8} (get-in data [:integrations :audio])))
        (is (= [] @calls))
        (connections/restore! conn replacement)
        (is (= data (connections/export-state conn)))
        (is (:ready? (agents/public-config (connections/agents conn))))
        (is (= [] @calls)))
      (finally (connections/close! conn)))))

(deftest credential-references-resolve-locally-without-exporting-secrets
  (let [calls (atom []) secret "test-only-private-key"
        opts (assoc-in (no-io-options calls) [:credential-store :scope]
                       (fn [audience]
                         {:get! (fn [_] (swap! calls conj :lookup)
                                  (when (= (first (edn/read-string audience)) :show-provider) secret))}))
        saved (-> agent-state (assoc :credential-ref "credential-00000000-0000-0000-0000-000000000001")
                  (assoc :config {:provider :openai :model "test-model"}))
        conn (connections/create (assoc (show) :integrations {:agents saved}) opts)]
    (try
      (is (= [:lookup] @calls))
      (is (:ready? (agents/public-config (connections/agents conn))))
      (is (not (.contains (pr-str (connections/export-state conn)) secret)))
      (is (= (:credential-ref saved) (get-in (connections/export-state conn) [:integrations :agents :credential-ref])))
      (finally (connections/close! conn)))))

(deftest complete-show-file-roundtrip-restores-owned-configuration-and-cache
  (let [initial (-> (show) (dissoc :value) (assoc :rig {:fixtures []})
                    (assoc-in [:transport :beat] 0.0)
                    (assoc :integrations {:agents agent-state :audio {:input-gain 0.8}
                                          :audio-capture {:version 1 :config {:default? true} :capture-wanted? true}}))
        calls (atom []) conn (connections/create initial (no-io-options calls))]
    (try
      (fixtures/restore! (connections/fixtures conn) (assoc (fixtures/export-state (connections/fixtures conn)) :revision 7))
      (let [saved (merge initial (connections/export-state conn))
            document (persistence/read-string (persistence/write-string saved))
            restored (persistence/thaw initial document)]
        (connections/restore! conn restored)
        (is (= 4 (:epoch restored)))
        (is (= 7 (:revision (fixtures/export-state (connections/fixtures conn)))))
        (is (= {:input-gain 0.8} (get-in (connections/export-state conn) [:integrations :audio])))
        (is (true? (get-in (connections/export-state conn) [:integrations :audio-capture :capture-wanted?])))
        (is (= :idle (:state (audio/status (connections/audio conn)))))
        (is (= [] @calls)))
      (finally (connections/close! conn)))))

(deftest invalid-restore-and-untrusted-source-leave-current-bundle-unchanged
  (let [calls (atom []) conn (connections/create (show) (no-io-options calls))
        old [(connections/agents conn) (connections/audio conn) (connections/fixtures conn) (connections/reactive conn)]
        before (connections/export-state conn)
        source {:registrations [{:id ::source :kind :sub :implementation {:type :source :namespace "user"
                                                                         :form "(fn [context query] 10)" :bindings {}}}]}]
    (try
      (doseq [bad [(assoc (show) :integrations {:agents (assoc agent-state :version 999)})
                   (assoc (show) :integrations {:fixtures {:version 1}})
                   (assoc (show) :integrations {:audio-capture {:version 1 :config {} :capture-wanted? true}})
                   (assoc (show) :integrations {:other (Object.)})
                   (assoc (show) :reactive source)]]
        (is (thrown? clojure.lang.ExceptionInfo (connections/restore! conn bad)))
        (is (every? true? (map identical? old [(connections/agents conn) (connections/audio conn)
                                              (connections/fixtures conn) (connections/reactive conn)])))
        (is (= before (connections/export-state conn))))
      (is (= [] @calls))
      (is (zero? (:generation (connections/status conn))))
      (finally (connections/close! conn)))))

(deftest a-cleanup-exception-does-not-leak-other-resources-or-private-errors
  (let [closed (atom 0)
        provider (with-meta (fn [& _] nil)
                   {:close! #(do (swap! closed inc) (throw (Exception. "private provider endpoint")))})
        conn (connections/create (show) {:fixture-options {:provider provider}})
        graph (connections/reactive conn) capture (connections/audio conn)]
    (is (= [{:resource :fixtures :code :cleanup-failed}] (:cleanup-errors (connections/close! conn))))
    (is (= 1 @closed))
    (is (:closed? (audio/status capture)))
    (is (thrown? clojure.lang.ExceptionInfo (reactive/publish! graph)))
    (is (not (.contains (pr-str (connections/status conn)) "private provider")))
    (connections/close! conn)
    (is (= 1 @closed))))

(deftest two-concurrent-commits-have-one-owner-and-one-stale-candidate
  (let [conn (connections/create (show))
        candidates [(connections/prepare-restore conn (assoc (show) :epoch 4))
                    (connections/prepare-restore conn (assoc (show) :epoch 5))]
        start (promise)
        jobs (mapv (fn [candidate]
                     (future @start (try {:retired (connections/commit-restore! conn candidate)}
                                         (catch clojure.lang.ExceptionInfo e {:error (:connections/error (ex-data e))})))) candidates)]
    (try
      (deliver start true)
      (let [results (mapv #(deref % 3000 ::timeout) jobs)]
        (is (not-any? #{::timeout} results))
        (is (= 1 (count (keep :retired results))))
        (is (= [:stale-candidate] (vec (keep :error results))))
        (is (= 1 (:generation (connections/status conn))))
        (doseq [retired (keep :retired results)] (connections/retire! retired)))
      (finally (doseq [candidate candidates] (connections/abort-prepared! candidate))
               (connections/close! conn)))))

(deftest prepared-resources-commit-once-with-no-cleanup-until-retirement
  (let [calls (atom []) conn (connections/create (show) (no-io-options calls))
        old-audio (connections/audio conn) old-graph (connections/reactive conn)
        candidate (connections/prepare-restore conn (assoc (show) :epoch 4))]
    (try
      (is (= 4 (:model-epoch candidate)))
      (is (identical? old-graph (connections/reactive conn)))
      (is (= [] @calls))
      (let [retired (connections/commit-restore! conn candidate)]
        (is (= 1 (:generation (connections/status conn))))
        (is (false? (:closed? (audio/status old-audio))))
        (is (not (identical? old-graph (connections/reactive conn))))
        (is (= :consumed-candidate (error-code #(connections/commit-restore! conn candidate))))
        (is (false? (:aborted? (connections/abort-prepared! candidate))))
        (is (:retired? (connections/retire! retired)))
        (is (:closed? (audio/status old-audio)))
        (is (false? (:retired? (connections/retire! retired)))))
      (finally (connections/abort-prepared! candidate) (connections/close! conn)))))

(deftest stale-foreign-and-aborted-candidates-cannot-replace-resources
  (let [conn (connections/create (show)) other (connections/create (show))
        a (connections/prepare-restore conn (assoc (show) :epoch 4))
        b (connections/prepare-restore conn (assoc (show) :epoch 5))]
    (try
      (is (= :foreign-candidate (error-code #(connections/commit-restore! other a))))
      (connections/retire! (connections/commit-restore! conn a))
      (is (= :stale-candidate (error-code #(connections/commit-restore! conn b))))
      (is (:aborted? (connections/abort-prepared! b)))
      (is (false? (:aborted? (connections/abort-prepared! b))))
      (is (:closed? (audio/status (get-in b [:bundle :audio]))))
      (is (= :consumed-candidate (error-code #(connections/commit-restore! conn b))))
      (let [candidate (connections/prepare-restore conn (show))]
        (connections/close! conn)
        (is (= :closed (error-code #(connections/commit-restore! conn candidate))))
        (is (:aborted? (connections/abort-prepared! candidate))))
      (finally (connections/abort-prepared! a) (connections/abort-prepared! b)
               (connections/close! conn) (connections/close! other)))))

(deftest reactive-state-and-actions-survive-restore-without-duplicate-watch-effects
  (let [conn (connections/create (show)) graph (connections/reactive conn)]
    (try
      (reactive/reg-sub! graph ::value value-sub sub-descriptor)
      (reactive/reg-fx! graph ::change change-fx fx-descriptor)
      (reactive/ratom graph ::level 7)
      (reactive/watch! graph ::watch [::value] ::change)
      (connections/publish! conn (show))
      (is (= [{:op :master/set :value 0.2}] (connections/drain-actions! conn)))
      (reactive/queue-action! graph {:op :blackout/set :value true})
      (let [data (connections/export-state conn) saved (merge (assoc (show) :epoch 4) data)]
        (connections/restore! conn saved)
        (is (= 7 @(reactive/source (connections/reactive conn) ::level)))
        (is (= [{:op :blackout/set :value true}] (connections/drain-actions! conn)))
        (is (= [] (connections/drain-actions! conn)))
        (connections/publish! conn saved)
        (is (= [] (connections/drain-actions! conn)) "same saved value does not retrigger")
        (connections/publish! conn (assoc saved :value 5))
        (is (= [{:op :master/set :value 0.5}] (connections/drain-actions! conn)))
        (is (= 5 @(reactive/subscribe (connections/reactive conn) [::value]))))
      (finally (connections/close! conn)))))

(deftest explicit-trusted-source-restore-rebuilds-a-real-subscription
  (let [model (assoc (show) :reactive {:registrations [{:id ::source :kind :sub
                                                      :implementation {:type :source :namespace "user"
                                                                       :form "(fn [{:keys [db]} _] (+ (:value @db) offset))"
                                                                       :bindings {"offset" 3}}}]})
        conn (connections/create model {:reactive-options {:trusted-source? true}})]
    (try
      (is (= 5 @(reactive/subscribe (connections/reactive conn) [::source])))
      (is (= (get-in model [:reactive :registrations]) (:registrations (:reactive (connections/export-state conn)))))
      (finally (connections/close! conn)))))

(deftest restoring-capture-stops-old-worker-and-resumes-with-a-new-time-origin
  (let [entered (CountDownLatch. 1) released (CountDownLatch. 1) opens (atom 0) closes (atom 0)
        now (atom 1000.0) features (atom []) old-done (atom 0)
        conn (connections/create
              (show)
              {:audio-options
               {:now-ms #(swap! now + 20.0)
                :open-source! (fn [_]
                                (if (= 1 (swap! opens inc))
                                  {:read! (fn [& _] (.countDown entered) (.await released 3 TimeUnit/SECONDS) -1)
                                   :close! #(do (swap! closes inc) (.countDown released))}
                                  (let [read? (atom false)]
                                    {:started-at-ms @now
                                     :read! (fn [bytes offset _]
                                              (if (compare-and-set! read? false true)
                                                (do (dotimes [n 960] (aset-byte bytes (+ offset n) (byte 0))) 960) -1))
                                     :close! #(swap! closes inc)})))}})
        first-handle (connections/start-audio! conn {:default? true} {:events! identity :done! (fn [_] (swap! old-done inc))})]
    (try
      (is (.await entered 2 TimeUnit/SECONDS))
      (let [saved (merge (assoc (show) :epoch 4) (connections/export-state conn))
            candidate (connections/prepare-restore conn saved)
            retired (connections/commit-restore! conn candidate)]
        (is (= 1 @opens))
        (is (zero? @closes) "commit never closes a source or waits for its worker")
        (is (not (realized? (:result first-handle))))
        (connections/retire! retired)
        (is (= :stopped (:status (deref (:result first-handle) 1000 ::timeout))))
        (is (= 1 @closes))
        (is (zero? @old-done) "retired callbacks cannot enter the replacement show")
        (is (= :idle (:state (audio/status (connections/audio conn)))))
        (reset! now 10000.0)
        (let [second-handle (connections/resume-audio! conn {:events! identity :features! #(swap! features conj %)})]
          (is (= :completed (:status (deref (:result second-handle) 3000 ::timeout))))
          (is (= 2 @opens @closes))
          (is (seq @features))
          (is (every? #(>= (:time-ms %) 10000.0) @features))
          (is (not= (:run-id first-handle) (:run-id second-handle))))
        (is (:stopped? (connections/stop-audio! conn)))
        (is (= {:started? false :reason :not-requested} (connections/resume-audio! conn {:events! identity}))))
      (finally (.countDown released) (connections/close! conn)))))

(deftest retired-agent-workers-cancel-without-replaying-history
  (let [entered (promise) gate (CountDownLatch. 1) calls (atom 0) done (atom [])
        conn (connections/create (assoc (show) :integrations {:agents agent-state})
                                 {:agent-options {:transport (fn [& _] (swap! calls inc) (deliver entered true)
                                                              (.await gate 5 TimeUnit/SECONDS) {:status 200})}})
        handle (agents/start-chat! (connections/agents conn) {:message "Run"}
                                   {:snapshot! (constantly (show)) :dispatch! (fn [& _] {})
                                    :emit! identity :done! #(swap! done conj %)})]
    (try
      (is (true? (deref entered 2000 false)))
      (let [saved (merge (assoc (show) :epoch 4) (connections/export-state conn))
            candidate (connections/prepare-restore conn saved)
            retired (connections/commit-restore! conn candidate)]
        (is (= 1 @calls))
        (is (not (realized? (:result handle))) "commit does not wait or cancel")
        (is (:retired? (connections/retire! retired)))
        (is (= :cancelled (:status (deref (:result handle) 2000 ::timeout))))
        (is (= 1 (count @done)))
        (is (= 0 (:active-runs (agents/public-config (connections/agents conn)))))
        (is (= 1 @calls) "saved chat does not restart a provider request")
        (is (seq (:conversations (agents/history (connections/agents conn))))))
      (finally (.countDown gate) (connections/close! conn)))))

(def sync-config {:selected-source :midi-clock
                  :midi {:enabled? true :input-index 0 :expected-input-name "Injected MIDI"}
                  :live {:mappings {36 {:cue-id :ember :mode :held}}}})

(deftest sync-configuration-and-saved-intent-are-inert-until-explicit-resume
  (let [factories (atom 0) opens (atom 0) closes (atom 0) events (atom []) now (atom 0.0)
        factory (fn [config logical-clock]
                  (swap! factories inc)
                  {:open! (fn [_] (swap! opens inc) {:config config :origin (logical-clock)})
                   :send! (fn [& _]) :close! (fn [_] (swap! closes inc))})
        conn (connections/create (show) {:sync-options {:midi-factory factory :now-ms #(deref now)}})]
    (try
      (is (= 0 @factories @opens))
      (is (= {:started? false :reason :not-requested} (connections/resume-sync! conn {:events! identity})))
      (is (false? (get-in (connections/export-state conn) [:integrations :sync :wanted?])))
      (let [first-manager (connections/sync conn)
            first-run (connections/start-sync! conn sync-config {:events! #(swap! events conj %)})]
        (is (session/flush! first-manager))
        (is (= 1 @factories @opens))
        (let [saved (merge (assoc (show) :epoch 4) (connections/export-state conn))
              candidate (connections/prepare-restore conn saved)
              retired (connections/commit-restore! conn candidate)]
          (is (true? (get-in saved [:integrations :sync :wanted?])))
          (is (= {:cue-id :ember :mode :held} (get-in saved [:integrations :sync :config :live :mappings 36])))
          (is (= 1 @factories @opens) "preparation and commit open no sync resources")
          (is (zero? @closes) "commit does not close the old ports")
          (is (= :stopped (:state (sync/status (connections/sync conn)))))
          (is (session/ingest! first-manager {:type :peer/found :peer {:device-number 2 :address "127.0.0.2"}}))
          (is (session/flush! first-manager))
          (is (= [] @events) "old active manager cannot call the new host after resource commit")
          (connections/retire! retired)
          (is (= :stopped (:status (deref (:result first-run) 1000 ::timeout))))
          (is (= 1 @closes))
          (reset! now 5000.0)
          (let [second-run (connections/resume-sync! conn {:events! #(swap! events conj %)})
                second-manager (connections/sync conn)]
            (is (session/flush! second-manager))
            (is (= 2 @factories @opens))
            (is (not= (:run-id first-run) (:run-id second-run)))
            (is (session/ingest! second-manager {:type :midi/message :source-id :midi-clock :time-ms 5000.0 :bytes [0xfa]}))
            (is (session/flush! second-manager))
            (is (some #(= :sync/clock (:type %)) @events))
            (is (:stopped? (connections/stop-sync! conn)))
            (is (= 2 @closes))
            (is (false? (get-in (connections/export-state conn) [:integrations :sync :wanted?])))
            (is (= {:started? false :reason :not-requested} (connections/resume-sync! conn {:events! identity}))))))
      (finally (connections/close! conn)))))

(deftest invalid-sync-data-or-callbacks-do-not-reconfigure-or-open-resources
  (let [opened (atom 0)
        conn (connections/create (show) {:sync-options {:midi-factory (fn [& _] (swap! opened inc))}})
        before (connections/export-state conn) manager (connections/sync conn)]
    (try
      (is (= :invalid-callbacks (error-code #(connections/start-sync! conn sync-config {:events! 99}))))
      (is (= before (connections/export-state conn)))
      (doseq [saved [{:version 1 :wanted? "yes" :config {}}
                    {:version 1 :wanted? true :config {:midi {:enabled? true}}}
                    {:version 1 :wanted? false :config {:live {:endpoint "https://unselected.example"}}}]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (connections/restore! conn (assoc (show) :integrations {:sync saved}))))
        (is (identical? manager (connections/sync conn)))
        (is (= before (connections/export-state conn))))
      (is (zero? @opened))
      (finally (connections/close! conn)))))

(deftest sensor-pairings-share-the-vault-and-staged-resources-retire-inertly
  (let [secret (atom nil) calls (atom [])
        reference "credential-00000000-0000-0000-0000-000000000007"
        store {:put! (fn [value] (reset! secret value) reference)
               :get! (fn [ref] (swap! calls conj ref) @secret) :delete! (fn [_] (reset! secret nil))}
        conn (connections/create (show) {:credential-store store :sensor-options {:now-ms (constantly 0)}})
        original (connections/sensors conn)]
    (try
      (is (= [] @calls))
      (is (nil? (:udp (sensors/status original))))
      (let [pair (sensors/pair! original "phone")
            configured (:model (sensors/configure (show) "phone" {:version 1 :channels []} (:pairing pair)))
            prepared (connections/prepare-restore conn configured)
            retired (connections/commit-restore! conn prepared)
            replacement (connections/sensors conn)]
        (is (not (identical? original replacement)))
        (is (= [reference] @calls))
        (is (false? (get-in (sensors/status replacement) [:devices "phone" :armed?])))
        (is (= (:sensors configured) (:sensors (connections/export-state conn configured))))
        (is (not (.contains (pr-str (connections/export-state conn configured)) @secret)))
        (connections/retire! retired)
        (is (:closed? (sensors/status original)))
        (connections/close! conn)
        (is (:closed? (sensors/status replacement))))
      (finally (connections/close! conn)))))
