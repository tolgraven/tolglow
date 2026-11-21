(ns tolglow.agents-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [tolglow.agents :as agents]
            [tolglow.agents.credentials :as credentials])
  (:import [java.util.concurrent CountDownLatch TimeUnit]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn stream-text [text]
  (str "data: " (json/write-str {:choices [{:index 0 :delta {:content text}}]}) "\n\n"
       "data: [DONE]\n\n"))

(def fixture-snapshot
  {:show-id :main :epoch 4 :revision 12 :time-ms 1000
   :rig {:fixtures [{:id :left :name "Left RGBW" :position [-1 3 0]
                     :channels {:red 1 :green 2 :blue 3 :white 4 :dimmer 5}
                     :capabilities #{:color :dimmer} :api-key "must-not-be-in-context"}]}
   :effects [{:id :ember :version 3 :params {:level {:type :number :default 0.3 :bounds [0 1]}}}]
   :cues [{:id :red-cue :effect-id :ember :version 2}]
   :instances [{:id "4:1" :cue-id :red-cue :phase :running}]
   :status {:output {:type :preview}} :provider {:credential "also-private"}})

(defn callbacks [events completed]
  {:snapshot! (constantly fixture-snapshot)
   :dispatch! (fn [_ _ _] {:ok? true})
   :emit! #(swap! events conj %)
   :done! #(swap! completed conj %)})

(defn configure-local! [manager]
  (agents/configure! manager {:provider :self-hosted :model "local"
                              :endpoint "http://127.0.0.1:1/v1/chat/completions"}))

(defn await-result [handle] (deref (:result handle) 5000 ::timeout))

(deftest configuration-is-network-free-and-credentials-never-enter-public-state
  (let [calls (atom 0) manager (agents/create-manager {:transport (fn [& _] (swap! calls inc))})
        secret "runtime-secret-12345"]
    (try
      (let [config (agents/configure! manager {:provider "openai" :model "chosen-model" :api-key secret})]
        (is (:ready? config))
        (is (:credential-configured? config))
        (is (= 0 @calls))
        (is (not (str/includes? (pr-str config) secret)))
        (is (not (str/includes? (pr-str @(:state manager)) secret))))
      (is (:credential-configured? (agents/configure! manager {:model "new-model"})))
      (let [changed (agents/configure! manager {:provider :anthropic :model "different-model"})]
        (is (:configured? changed))
        (is (false? (:ready? changed)))
        (is (false? (:credential-configured? changed))))
      (is (:ready? (configure-local! manager)))
      (agents/configure! manager {:credential secret})
      (is (false? (:credential-configured? (agents/configure! manager {:clear-credential? true}))))
      (finally (agents/close! manager)))))

(deftest invalid-configuration-is-rejected-without-leaking-payload
  (let [manager (agents/create-manager)]
    (try
      (doseq [payload [{:provider :openai :model "" :credential "private-secret"}
                       {:provider :unknown :model "x" :credential "private-secret"}
                       {:provider :self-hosted :model "x" :endpoint "file:///tmp/private-secret"}
                       {:provider :openai :model "x" :credential "private-secret\nheader"}
                       {:provider :openai :model "x" :credential "private-secret" :max-rounds 33}]]
        (let [error (try (agents/configure! manager payload) nil (catch Exception error error))]
          (is error)
          (is (not (str/includes? (str error (ex-data error)) "private-secret")))))
      (is (false? (:configured? (agents/public-config manager))))
      (finally (agents/close! manager)))))

(deftest chat-captures-current-context-streams-and-calls-completion-once
  (let [requests (atom []) events (atom []) done (atom []) secret "secret-for-this-run"
        manager (agents/create-manager
                 {:transport (fn [request consume! _]
                               (swap! requests conj request)
                               (consume! (stream-text (str "Result " secret)))
                               {:status 200})})]
    (try
      (agents/configure! manager {:provider :openai :model "model" :credential secret})
      (let [handle (agents/start-chat! manager {:message "Make a moody red cue." :run-id "one"} (callbacks events done))
            result (await-result handle)
            sent (json/read-str (:body (first @requests)) :key-fn keyword)
            prompt (get-in sent [:messages 0 :content])]
        (is (= :completed (:status result)))
        (is (= "Result [REDACTED]" (:text result)))
        (is (= 1 (count @done)))
        (is (= 0 (:active-runs (agents/public-config manager))))
        (is (str/includes? prompt "Left RGBW"))
        (is (str/includes? prompt "publish_effect"))
        (is (str/includes? prompt "one-shot"))
        (is (not (str/includes? prompt "must-not-be-in-context")))
        (is (not (str/includes? prompt "also-private")))
        (is (not (str/includes? (pr-str [@events @done sent]) secret)))
        (is (= "Bearer secret-for-this-run" (get-in (first @requests) [:headers "authorization"])))
        (is (thrown? clojure.lang.ExceptionInfo
                     (agents/start-chat! manager {:message "Retry" :run-id "one"} (callbacks events done))))
        ;; Returned history contains a server system prompt; replace it rather
        ;; than rejecting ordinary continued conversation or keeping stale rig.
        (let [next (agents/start-chat! manager {:message "Now purple." :messages (:messages result) :run-id "two"}
                                       (callbacks events done))]
          (is (= :completed (:status (await-result next))))
          (is (= 1 (count (filter #(= "system" (:role %))
                                  (:messages (json/read-str (:body (second @requests)) :key-fn keyword))))))))
      (finally (agents/close! manager)))))

(deftest context-size-is-bounded-for-large-pixel-rigs
  (let [snapshot (assoc fixture-snapshot :rig {:fixtures (mapv (fn [i] {:id (str "pixel-" i) :channels {:red 1 :green 2 :blue 3}})
                                                             (range 10000))})
        summary (agents/summarize-snapshot snapshot)
        prompt (agents/system-prompt snapshot)]
    (is (= 10000 (:fixture-count summary)))
    (is (<= (count (:fixtures summary)) 64))
    (is (< (count prompt) 45000))))

(deftest worker-and-queue-bounds-cancellation-and-close-are-explicit
  (let [gate (CountDownLatch. 1) entered (promise) events (atom []) done (atom [])
        manager (agents/create-manager {:workers 1 :queue-size 1
                                        :transport (fn [_ consume! _]
                                                     (deliver entered true)
                                                     (.await gate 10 TimeUnit/SECONDS)
                                                     (consume! (stream-text "done"))
                                                     {:status 200})})]
    (try
      (configure-local! manager)
      (let [one (agents/start-chat! manager {:message "One" :run-id "one"} (callbacks events done))]
        (is (true? (deref entered 3000 false)))
        (let [two (agents/start-chat! manager {:message "Two" :run-id "two"} (callbacks events done))]
          (is (= 2 (:active-runs (agents/public-config manager))))
          (is (= :busy (try (agents/start-chat! manager {:message "Three"} (callbacks events done)) nil
                            (catch clojure.lang.ExceptionInfo error (:agents/error (ex-data error))))))
          (is (true? ((:cancel! two))))
          (is (false? ((:cancel! two))))
          (is (= :cancelled (:status (await-result two))))
          ((:cancel! one))
          (is (= :cancelled (:status (await-result one))))
          (is (= 2 (count @done)))
          (is (= 0 (:active-runs (agents/public-config manager))))))
      (is (:closed? (agents/close! manager)))
      (is (false? (:ready? (agents/public-config manager))))
      (is (false? (:credential-configured? (agents/public-config manager))))
      (is (= :closed (try (agents/start-chat! manager {:message "After close"} (callbacks events done)) nil
                          (catch clojure.lang.ExceptionInfo error (:agents/error (ex-data error))))))
      (finally (.countDown gate) (agents/close! manager)))))

(deftest queued-runs-retain-their-start-time-provider-configuration
  (let [gate (CountDownLatch. 1) entered (promise) requests (atom [])
        manager (agents/create-manager
                 {:workers 1 :queue-size 1
                  :transport (fn [request consume! _]
                               (swap! requests conj request)
                               (when (= 1 (count @requests)) (deliver entered true) (.await gate 5 TimeUnit/SECONDS))
                               (consume! (stream-text "ok")) {:status 200})})
        hooks (callbacks (atom []) (atom []))]
    (try
      (agents/configure! manager {:provider :openai :model "old-model" :credential "old-key"})
      (let [one (agents/start-chat! manager {:message "One"} hooks)]
        (is (true? (deref entered 3000 false)))
        (let [two (agents/start-chat! manager {:message "Two"} hooks)]
          (agents/configure! manager {:model "new-model" :credential "new-key"})
          (.countDown gate)
          (is (= :completed (:status (await-result one))))
          (is (= :completed (:status (await-result two))))
          (is (= ["old-model" "old-model"] (mapv #(get (json/read-str (:body %)) "model") @requests)))
          (is (= ["Bearer old-key" "Bearer old-key"] (mapv #(get-in % [:headers "authorization"]) @requests)))
          (is (= "new-model" (:model (agents/public-config manager))))))
      (finally (.countDown gate) (agents/close! manager)))))

(deftest callback-failure-does-not-leak-worker-capacity
  (let [manager (agents/create-manager {:transport (fn [_ consume! _] (consume! (stream-text "ok")) {:status 200})})]
    (try
      (configure-local! manager)
      (let [handle (agents/start-chat! manager {:message "Go"}
                                       {:snapshot! (constantly fixture-snapshot)
                                        :dispatch! (fn [& _] {})
                                        :emit! (fn [_] (throw (Exception. "Disconnected")))
                                        :done! (fn [_] (throw (Exception. "Closed sink")))})]
        (is (= :cancelled (:status (await-result handle))))
        (is (= 0 (:active-runs (agents/public-config manager)))))
      (finally (agents/close! manager)))))

(deftest closing-an-active-manager-cancels-runs-and-clears-secret-references
  (let [entered (promise) gate (CountDownLatch. 1) done (atom [])
        manager (agents/create-manager {:transport (fn [_ consume! _]
                                                     (deliver entered true)
                                                     (.await gate 10 TimeUnit/SECONDS)
                                                     (consume! (stream-text "late")) {:status 200})})]
    (try
      (agents/configure! manager {:provider :openai :model "model" :credential "close-secret"})
      (let [handle (agents/start-chat! manager {:message "Go"} (callbacks (atom []) done))]
        (is (true? (deref entered 3000 false)))
        (is (:closed? (agents/close! manager)))
        (is (= :cancelled (:status (await-result handle))))
        (is (= 1 (count @done)))
        (is (false? (:credential-configured? (agents/public-config manager))))
        (is (= 0 (:active-runs (agents/public-config manager)))))
      (finally (.countDown gate) (agents/close! manager)))))

(deftest invalid-input-is-rejected-before-consuming-capacity
  (let [requests (atom 0)
        manager (agents/create-manager {:max-message-chars 8 :max-history-messages 2
                                        :transport (fn [& _] (swap! requests inc))})]
    (try
      (configure-local! manager)
      (doseq [payload [{:message "message-too-long"} {:message ""}
                       {:message "Go" :messages [{:role :invalid :content "bad"}]}]]
        (is (= :invalid-message
               (try (agents/start-chat! manager payload (callbacks (atom []) (atom []))) nil
                    (catch clojure.lang.ExceptionInfo error (:agents/error (ex-data error)))))))
      (is (= 0 @requests))
      (is (= 0 (:active-runs (agents/public-config manager))))
      (finally (agents/close! manager)))))

(defn tool-stream [id name arguments]
  (str "data: " (json/write-str {:choices [{:index 0 :delta {:content "Inspecting."
                                                                         :tool_calls [{:index 0 :id id :function {:name name :arguments (json/write-str arguments)}}]}}]}) "\n\n"
       "data: [DONE]\n\n"))

(def empty-export
  {:version 1 :config {:provider :self-hosted :model "local" :endpoint "http://127.0.0.1:1/v1/chat/completions"}
   :credential-ref nil :selected-conversation-id nil :conversations []})

(deftest persisted-credentials-and-conversations-restore-without-network-or-tools
  (let [directory (Files/createTempDirectory "tolglow-agent-keys-" (make-array FileAttribute 0))
        path (.resolve directory "credentials.edn") store (credentials/file-store path)
        requests (atom []) transport (fn [request consume! _] (swap! requests conj request)
                                      (consume! (stream-text "Saved reply")) {:status 200})
        one (agents/create-manager {:credential-store store :transport transport})
        two (agents/create-manager {:credential-store (credentials/file-store path) :transport transport})
        hooks (callbacks (atom []) (atom []))]
    (try
      (agents/configure! one {:provider :openai :model "remembered-model" :credential "fake-persistent-key"})
      (is (= :completed (:status (await-result (agents/start-chat! one {:message "First question" :conversation-id "chat-one"} hooks)))))
      (let [saved (agents/export-state one) reference (:credential-ref saved)]
        (is (credentials/valid-reference? reference))
        (is (not (str/includes? (pr-str saved) "fake-persistent-key")))
        (is (not (str/includes? (pr-str @(:state one)) "fake-persistent-key")))
        (agents/close! one)
        (is (:ready? (agents/restore! two saved)))
        (is (= 1 (count @requests)))
        (is (= "chat-one" (:selected-conversation-id (agents/history two))))
        (is (= [:user :assistant] (mapv :role (get-in (agents/history two) [:conversations 0 :messages]))))
        (is (= :completed (:status (await-result (agents/start-chat! two {:message "Continue" :conversation-id "chat-one"} hooks)))))
        (let [request (last @requests) sent (json/read-str (:body request) :key-fn keyword)]
          (is (= "Bearer fake-persistent-key" (get-in request [:headers "authorization"])))
          (is (= ["system" "user" "assistant" "user"] (mapv :role (:messages sent)))))
        (is (= 4 (count (get-in (agents/history two) [:conversations 0 :messages]))))
        (agents/configure! two {:clear-credential? true})
        (is (nil? ((:get! store) reference)))
        (is (nil? (:credential-ref (agents/export-state two)))))
      (finally (agents/close! one) (agents/close! two)
               (doseq [file (reverse (file-seq (.toFile directory)))] (.delete file))))))

(deftest missing-local-key-keeps-settings-but-does-not-claim-readiness
  (let [manager (agents/create-manager) reference "credential-12345678-1234-1234-1234-123456789abc"
        saved (assoc empty-export :config {:provider :openai :model "remembered"} :credential-ref reference)]
    (try
      (let [result (agents/restore! manager saved)]
        (is (:configured? result))
        (is (false? (:ready? result)))
        (is (= :store-unavailable (:credential-error result)))
        (is (= reference (:credential-ref (agents/export-state manager)))))
      (is (= :not-configured
             (try (agents/start-chat! manager {:message "Go"} (callbacks (atom []) (atom []))) nil
                  (catch clojure.lang.ExceptionInfo error (:agents/error (ex-data error))))))
      (finally (agents/close! manager)))))

(deftest portable-agent-state-validates-matched-tool-history-and-secret-references
  (let [entry {:id "chat" :title "Tools" :updated-at-ms 0 :status :completed
               :messages [{:role :user :content "Inspect"}
                          {:role :assistant :content "" :tool-calls [{:id "tool-one" :name "inspect_show" :arguments {}}]}
                          {:role :tool :tool-call-id "tool-one" :content {:ok? true}}]}
        saved (assoc empty-export :conversations [entry] :selected-conversation-id "chat")]
    (is (= (dissoc saved :config) (dissoc (agents/validate-state saved) :config)))
    (is (= 8 (get-in (agents/validate-state saved) [:config :max-rounds])))
    (doseq [bad [(assoc saved :api-key "forbidden") (assoc-in saved [:config :credential] "forbidden")
                 (assoc saved :credential-ref "plaintext-key")
                 (assoc-in saved [:conversations 0 :messages] [{:role :tool :tool-call-id "orphan" :content {}}])
                 (update-in saved [:conversations 0 :messages] pop)
                 (assoc-in saved [:conversations 0 :updated-at-ms] Double/NaN)
                 (assoc saved :selected-conversation-id "missing")]]
      (is (thrown? clojure.lang.ExceptionInfo (agents/validate-state bad))))))

(deftest tool-calls-and-results-survive-bounded-history-round-trips
  (let [calls (atom 0) manager (agents/create-manager
                               {:max-history-messages 3
                                :transport (fn [_ consume! _]
                                             (consume! (if (= 1 (swap! calls inc)) (tool-stream "inspect" "inspect_show" {})
                                                           (stream-text "Ready"))) {:status 200})})
        hooks (callbacks (atom []) (atom []))]
    (try
      (configure-local! manager)
      (is (= :completed (:status (await-result (agents/start-chat! manager {:message "Inspect" :conversation-id "bounded"} hooks)))))
      (let [saved (agents/export-state manager) messages (get-in saved [:conversations 0 :messages])]
        (is (= [:assistant :tool :assistant] (mapv :role messages)))
        (is (= "inspect" (get-in messages [0 :tool-calls 0 :id]) (:tool-call-id (second messages))))
        (is (= {:ok? true} (:content (second messages))))
        (is (get-in saved [:conversations 0 :truncated?]))
        (is (= saved (agents/validate-state saved))))
      (is (= :completed (:status (await-result (agents/start-chat! manager {:message "Continue" :conversation-id "bounded"} hooks)))))
      (is (<= (count (get-in (agents/history manager) [:conversations 0 :messages])) 3))
      (finally (agents/close! manager)))))

(deftest in-flight-tool-snapshot-retains-unknown-outcomes-without-replaying
  (let [entered (CountDownLatch. 1) gate (CountDownLatch. 1) dispatches (atom 0) calls (atom 0)
        manager (agents/create-manager {:transport (fn [_ consume! _]
                                                     (consume! (if (= 1 (swap! calls inc)) (tool-stream "action" "inspect_show" {})
                                                                   (stream-text "Continued"))) {:status 200})})
        restored-calls (atom 0) restored (agents/create-manager {:transport (fn [& _] (swap! restored-calls inc))})
        hooks (assoc (callbacks (atom []) (atom []))
                     :dispatch! (fn [& _] (swap! dispatches inc) (.countDown entered) (.await gate) {:ok? true}))]
    (try
      (configure-local! manager)
      (let [handle (agents/start-chat! manager {:message "Inspect" :conversation-id "interrupted"} hooks)]
        (is (.await entered 2 TimeUnit/SECONDS))
        (is (= :conversation-busy
               (try (agents/start-chat! manager {:message "Again" :conversation-id "interrupted"} hooks) nil
                    (catch clojure.lang.ExceptionInfo error (:agents/error (ex-data error))))))
        (let [saved (agents/export-state manager) messages (get-in saved [:conversations 0 :messages])]
          (is (= :interrupted (get-in saved [:conversations 0 :status])))
          (is (= [:user :assistant :tool] (mapv :role messages)))
          (is (= :outcome-unknown-after-interruption (get-in messages [2 :content :error])))
          (agents/restore! restored saved)
          (is (zero? @restored-calls))
          (is (= 1 @dispatches)))
        (is (:completed? (agents/cancel-all! manager)))
        (is (= :cancelled (:status (await-result handle))))
        (is (:ready? (agents/public-config manager)))
        (is (= :completed (:status (await-result (agents/start-chat! manager {:message "Continue" :conversation-id "interrupted"} hooks)))))
        (is (= 1 @dispatches)))
      (finally (.countDown gate) (agents/close! manager) (agents/close! restored)))))

(deftest partial-stream-text-and-failure-status-are-portable
  (let [entered (CountDownLatch. 1) gate (CountDownLatch. 1)
        manager (agents/create-manager {:transport (fn [_ consume! _]
                                                     (consume! (str "data: " (json/write-str {:choices [{:delta {:content "Partial response"}}]}) "\n\n"))
                                                     (.countDown entered) (.await gate) {:status 200})})]
    (try
      (configure-local! manager)
      (let [handle (agents/start-chat! manager {:message "Begin" :conversation-id "partial"} (callbacks (atom []) (atom [])))]
        (is (.await entered 2 TimeUnit/SECONDS))
        (is (= "Partial response" (get-in (agents/export-state manager) [:conversations 0 :messages 1 :content])))
        (agents/cancel-all! manager)
        (is (= :cancelled (:status (await-result handle))))
        (let [saved (agents/export-state manager)]
          (is (= :cancelled (get-in saved [:conversations 0 :status])))
          (is (= "Partial response" (get-in saved [:conversations 0 :messages 1 :content])))))
      (finally (.countDown gate) (agents/close! manager)))))

(deftest conversation-count-is-bounded-and-selection-persists
  (let [manager (agents/create-manager {:conversation-limit 2
                                        :transport (fn [_ consume! _] (consume! (stream-text "Reply")) {:status 200})})]
    (try
      (configure-local! manager)
      (doseq [id ["one" "two" "three"]]
        (is (= :completed (:status (await-result (agents/start-chat! manager {:message id :conversation-id id} (callbacks (atom []) (atom []))))))))
      (is (= ["two" "three"] (mapv :id (:conversations (agents/history manager)))))
      (agents/select-conversation! manager "two")
      (is (= "two" (:selected-conversation-id (agents/export-state manager))))
      (is (thrown? clojure.lang.ExceptionInfo (agents/select-conversation! manager "one")))
      (finally (agents/close! manager)))))
