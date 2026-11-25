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
