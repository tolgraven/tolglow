(ns tolglow.integrations.providers-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [tolglow.integrations.providers :as providers])
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent CountDownLatch Executors TimeUnit]))

(def messages [{:role :system :content "Use the shared show controls."}
               {:role :user :content "Make a red cue."}])

(defn event [data] (str "data: " (json/write-str data) "\r\n\r\n"))

(deftest provider-requests-separate-credentials-and-provider-schemas
  (doseq [provider [:openai :openrouter :anthropic]]
    (let [request (providers/request {:provider provider :model "test-model"} messages "secret")
          body (json/read-str (:body request) :key-fn keyword)]
      (is (= :post (:method request)))
      (is (= "test-model" (:model body)))
      (is (:stream body))
      (is (not (str/includes? (pr-str (providers/redact-request request)) "secret")))
      (if (= provider :anthropic)
        (do (is (= "secret" (get-in request [:headers "x-api-key"])))
            (is (= "Use the shared show controls." (:system body)))
            (is (= ["user"] (mapv :role (:messages body)))))
        (is (= "Bearer secret" (get-in request [:headers "authorization"]))))))
  (is (nil? (get-in (providers/request {:provider :self-hosted :endpoint "http://localhost:8000/v1/chat/completions" :model "local"}
                                      messages nil) [:headers "authorization"])))
  (is (= {:provider :openai :model "x"}
         (providers/public-config {:provider :openai :model "x" :api-key "secret" :credential "secret"})))
  (is (thrown? clojure.lang.ExceptionInfo (providers/request {:provider :openai :model "x"} messages "bad\nheader"))))

(deftest structured-replies-and-strict-tools-use-each-provider-shape
  (let [schema {:type "object" :properties {:ok {:type "boolean"}} :required ["ok"] :additionalProperties false}
        tool {:name "check" :description "Check." :parameters schema :strict true}
        body #(json/read-str (:body (providers/request % messages "secret")) :key-fn keyword)
        anthropic (body {:provider :anthropic :model "m" :output-schema {:name "reply" :schema schema}})
        openai (body {:provider :openai :model "m" :output-schema {:name "reply" :schema schema}})
        openrouter (body {:provider :openrouter :model "m" :tools [tool]})
        plain (body {:provider :openrouter :model "m" :tools [(dissoc tool :strict)]})]
    (is (= {:format {:type "json_schema" :schema schema}} (:output_config anthropic)))
    (is (nil? (:response_format anthropic)))
    (is (= {:type "json_schema" :json_schema {:name "reply" :strict true :schema schema}} (:response_format openai)))
    (is (nil? (:provider openai)) "only OpenRouter routes by parameter support")
    (is (true? (get-in openrouter [:tools 0 :function :strict])))
    (is (= {:require_parameters true} (:provider openrouter)))
    (is (nil? (:provider plain)))
    (is (not (contains? (get-in plain [:tools 0 :function]) :strict)))
    (is (true? (get-in (body {:provider :anthropic :model "m" :tools [tool]}) [:tools 0 :strict])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (providers/request {:provider :anthropic :model "m" :output-schema {:name "two words" :schema schema}}
                                    messages "secret")))
    (testing "effort merges into output_config; parallel tool calls apply to OpenAI-compatible tools only"
      (is (= {:effort "low" :format {:type "json_schema" :schema schema}}
             (:output_config (body {:provider :anthropic :model "claude-opus-5-5" :effort "low"
                                    :output-schema {:name "reply" :schema schema}}))))
      (is (thrown? clojure.lang.ExceptionInfo (providers/request {:provider :openai :model "m" :effort "low"} messages "secret")))
      (is (false? (:parallel_tool_calls (body {:provider :openai :model "m" :tools [tool] :parallel-tool-calls false}))))
      (is (not (contains? (body {:provider :anthropic :model "m" :tools [tool] :parallel-tool-calls false}) :parallel_tool_calls)))
      (is (not (contains? (body {:provider :openai :model "m" :parallel-tool-calls false}) :parallel_tool_calls))))))

(deftest effort-is-gated-by-the-model-not-only-the-provider
  (let [effort (fn [model level]
                 (try (get-in (json/read-str (:body (providers/request {:provider :anthropic :model model :effort level}
                                                                       messages "secret"))
                                             :key-fn keyword)
                              [:output_config :effort])
                      (catch clojure.lang.ExceptionInfo e (ex-message e))))]
    (testing "every level on the current models"
      (doseq [model ["claude-opus-5-5" "claude-sonnet-5-5" "claude-haiku-5-5" "claude-fable-5-2" "claude-opus-4-7"]
              level ["low" "medium" "high" "xhigh" "max"]]
        (is (= level (effort model level)) (str model " " level))))
    (testing "xhigh came with Opus 4.7"
      (doseq [model ["claude-opus-4-6" "claude-sonnet-4-6" "claude-mythos-preview"]]
        (is (= "max" (effort model "max")))
        (is (= (str "Model " model " takes effort low, medium, high, max, not \"xhigh\"") (effort model "xhigh")))))
    (testing "Opus 4.5 takes low to high, and a dated snapshot ID resolves to its model"
      (is (= "high" (effort "claude-opus-4-5-20251101" "high")))
      (is (re-find #"not \"max\"" (effort "claude-opus-4-5" "max")))
      (is (re-find #"not \"xhigh\"" (effort "claude-opus-4-5-20251101" "xhigh"))))
    (is (= "Model claude-haiku-4-5-20251001 takes no effort level" (effort "claude-haiku-4-5-20251001" "low")))
    (is (re-find #"not recorded in providers/effort-levels" (effort "claude-opus-9" "low")) "unknown models fail closed")
    (is (re-find #"not \"extreme\"" (effort "claude-opus-5-5" "extreme")))
    (is (= "Only Anthropic requests take :effort"
           (ex-message (is (thrown? clojure.lang.ExceptionInfo
                                    (providers/request {:provider :openrouter :model "anthropic/claude-opus-5-5" :effort "low"}
                                                       messages "secret"))))))
    (is (nil? (effort "claude-haiku-4-5" nil)) "no effort, no check")))

(deftest openai-gets-max-completion-tokens-and-other-providers-max-tokens
  ;; OpenAI deprecated max_tokens and its reasoning models refuse it; OpenRouter,
  ;; Anthropic and self-hosted OpenAI-compatible servers take max_tokens.
  (doseq [[provider endpoint key] [[:openai nil :max_completion_tokens]
                                   [:openrouter nil :max_tokens]
                                   [:anthropic nil :max_tokens]
                                   [:self-hosted "http://127.0.0.1:8080/v1/chat/completions" :max_tokens]]]
    (let [sent (json/read-str (:body (providers/request (cond-> {:provider provider :model "m" :max-tokens 900}
                                                          endpoint (assoc :endpoint endpoint))
                                                        messages "secret"))
                              :key-fn keyword)]
      (is (= 900 (get sent key)) (str provider))
      (is (= 1 (count (select-keys sent [:max_tokens :max_completion_tokens]))) (str provider " sends one limit")))))

(deftest streamed-refusals-and-stop-details-become-events
  (is (= [{:type :refusal/delta :text "I can't help with that." :choice 0}
          {:type :choice/finished :reason "stop" :choice 0}
          {:type :done}]
         (:events (providers/parse-sse :openai (str (event {:choices [{:index 0 :delta {:refusal "I can't help with that."}}]})
                                                    (event {:choices [{:index 0 :delta {} :finish_reason "stop"}]})
                                                    "data: [DONE]\n\n")))))
  (is (= {:type :choice/finished :reason "refusal" :usage {:output_tokens 3}
          :stop-details {:type "refusal" :category "cyber"}}
         (first (:events (providers/parse-sse :anthropic (event {:type "message_delta"
                                                                 :delta {:stop_reason "refusal"
                                                                         :stop_details {:type "refusal" :category "cyber"}}
                                                                 :usage {:output_tokens 3}})))))))

(def openai-stream
  (str ": heartbeat\r\n\r\n"
       (event {:choices [{:index 0 :delta {:content "Röd "}}]})
       (event {:choices [{:index 0 :delta {:content "cue" :tool_calls [{:index 0 :id "call-1"
                                                                         :function {:name "cue_launch" :arguments "{\"cue-id\":"}}]}}]})
       (event {:choices [{:index 0 :delta {:tool_calls [{:index 0 :function {:arguments "\"red\"}"}}]}
                          :finish_reason "tool_calls"}]})
       "data: [DONE]\r\n\r\n"))

(deftest streaming-handles-arbitrary-chunk-boundaries-and-tool-fragments
  (let [result (reduce (fn [{:keys [state events]} chunk]
                         (let [next (providers/stream-feed state chunk)]
                           {:state (:state next) :events (into events (:events next))}))
                       {:state (providers/new-stream :openai) :events []}
                       (map str openai-stream))]
    (is (= "Röd cue" (apply str (map :text (filter #(= :text/delta (:type %)) (:events result))))))
    (is (= :done (:type (last (:events result)))))
    (is (= [{:id "call-1" :name "cue_launch" :index 0 :input {:cue-id "red"}}]
           (providers/completed-tools (:state result))))
    (is (empty? (:events (providers/stream-feed (:state result) openai-stream))))))

(deftest anthropic-stream-normalizes-text-usage-and-completion
  (let [stream (str (event {:type "message_start" :message {:id "msg-1" :usage {:input_tokens 4}}})
                    (event {:type "content_block_delta" :index 0 :delta {:type "text_delta" :text "Hello"}})
                    (event {:type "message_delta" :delta {:stop_reason "end_turn"} :usage {:output_tokens 1}})
                    (event {:type "message_stop"}))
        result (providers/parse-sse :anthropic stream)]
    (is (= [:message/start :text/delta :choice/finished :done] (mapv :type (:events result))))
    (is (= "Hello" (:text (second (:events result)))))))

(deftest truncated-and-malformed-streams-fail-explicitly
  (is (= :truncated-stream (-> (providers/parse-sse :openai (event {:choices [{:delta {:content "partial"}}]}))
                               :events last :error :code)))
  (is (= :malformed-stream-event (-> (providers/parse-sse :openai "data: {bad}\n\n") :events first :error :code))))

(deftest injectable-transport-is-called-once-and-reports-http-errors
  (let [calls (atom [])
        request (providers/request {:provider :openai :model "test"} messages "secret")
        transport (fn [req] (swap! calls conj req) {:status 200 :body openai-stream})
        result (providers/invoke! transport request)]
    (is (:ok? result))
    (is (= "Röd cue" (:text result)))
    (is (= [request] @calls))
    (is (= {:ok? false :error {:code :http-error :status 401}}
           (providers/invoke! (constantly {:status 401 :body "credential failure"}) request)))))

(deftest neutral-tool-history-encodes-for-openai-and-anthropic
  (let [history (into messages [{:role :assistant :content "" :tool-calls [{:id "a" :name "inspect_show" :arguments {}}]}
                               {:role :tool :tool-call-id "a" :content {:fixtures 4}}
                               {:role :user :content "Use those fixtures."}])
        declarations [{:name "inspect_show" :description "Inspect" :parameters {:type "object"}}]
        openai (json/read-str (:body (providers/request {:provider :openai :model "x" :tools declarations} history "secret")) :key-fn keyword)
        anthropic (json/read-str (:body (providers/request {:provider :anthropic :model "x" :tools declarations} history "secret")) :key-fn keyword)]
    (is (= "inspect_show" (get-in openai [:messages 2 :tool_calls 0 :function :name])))
    (is (= "a" (get-in openai [:messages 3 :tool_call_id])))
    (is (= "function" (get-in openai [:tools 0 :type])))
    (is (= "tool_use" (get-in anthropic [:messages 1 :content 0 :type])))
    (is (= "tool_result" (get-in anthropic [:messages 2 :content 0 :type])))
    (is (= "Use those fixtures." (get-in anthropic [:messages 2 :content 1 :text])))
    (is (= {:type "object"} (get-in anthropic [:tools 0 :input_schema])))))

(defn with-http-server [handler f]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        executor (Executors/newCachedThreadPool)]
    (.setExecutor server executor)
    (.createContext server "/" (reify HttpHandler (handle [_ exchange] (handler exchange))))
    (.start server)
    (try (f (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/"))
         (finally (.stop server 0) (.shutdownNow executor)))))

(deftest real-http-transport-streams-utf8-from-a-local-server
  (with-http-server
    (fn [exchange]
      (try
        (slurp (.getRequestBody exchange))
        (.add (.getResponseHeaders exchange) "Content-Type" "text/event-stream; charset=utf-8")
        (.sendResponseHeaders exchange 200 0)
        (with-open [out (.getResponseBody exchange)]
          ;; Split every UTF-8 byte; transport must decode characters across reads.
          (doseq [value (.getBytes openai-stream StandardCharsets/UTF_8)]
            (.write out (bit-and value 255)) (.flush out)))
        (finally (.close exchange))))
    (fn [url]
      (let [chunks (atom [])
            request (providers/request {:provider :self-hosted :endpoint url :model "local"} messages nil)
            response (providers/http-transport! request #(swap! chunks conj %) {:timeout-ms 5000})
            decoded (providers/parse-sse :self-hosted (apply str @chunks))]
        (is (= 200 (:status response)))
        (is (= openai-stream (apply str @chunks)))
        (is (= "Röd cue" (apply str (map :text (filter #(= :text/delta (:type %)) (:events decoded))))))))))

(deftest actual-http-body-can-be-cancelled-and-timed-out
  (let [finish (CountDownLatch. 1)]
    (with-http-server
      (fn [exchange]
        (try
          (slurp (.getRequestBody exchange))
          (.sendResponseHeaders exchange 200 0)
          (with-open [out (.getResponseBody exchange)]
            (.write out (.getBytes ": started\n\n" StandardCharsets/UTF_8)) (.flush out)
            (.await finish 30 TimeUnit/SECONDS))
          (catch Exception _)
          (finally (.close exchange))))
      (fn [url]
        (let [request (providers/request {:provider :self-hosted :endpoint url :model "local"} messages nil)
              ;; Signal on the first chunk consumed by the client. This exercises
              ;; cancellation of an open response body even on a busy runner.
              consumed (promise)
              call (providers/start-http! request (fn [_] (deliver consumed true)) {:timeout-ms 30000})]
          (try
            (is (true? (deref consumed 20000 false)))
            ((:cancel! call))
            (let [result (try @(:result call)
                              (catch Exception exception
                                (ex-data (or (.getCause exception) exception))))]
              (is (= :cancelled (:provider/error result))))
            (let [result (try (providers/http-transport! request (fn [_]) {:timeout-ms 250})
                              (catch clojure.lang.ExceptionInfo exception (ex-data exception)))]
              (is (= :timeout (:provider/error result))))
            (finally (.countDown finish))))))))

(deftest a-peer-disconnect-is-a-transport-error
  (with-http-server
    (fn [exchange] (.close exchange))
    (fn [url]
      (let [request (providers/request {:provider :self-hosted :endpoint url :model "local"} messages nil)
            failure (try
                      (providers/http-transport! request (fn [_]) {:timeout-ms 5000})
                      nil
                      (catch clojure.lang.ExceptionInfo exception exception))]
        (is (= :transport (:provider/error (ex-data failure))))))))
