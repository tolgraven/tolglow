(ns tolglow.integrations.providers
  "HTTP request and streaming event adapters. This namespace never opens a
  connection or stores credentials. invoke! accepts a caller-owned transport;
  incremental transports can call stream-feed as decoded text chunks arrive."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import [java.io InputStream InputStreamReader]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration]
           [java.util.concurrent Executors ThreadFactory TimeUnit]))

(def ^:private endpoints
  {:openai "https://api.openai.com/v1/chat/completions"
   :anthropic "https://api.anthropic.com/v1/messages"
   :openrouter "https://openrouter.ai/api/v1/chat/completions"})

(def ^:private providers #{:openai :anthropic :openrouter :self-hosted})

(defn public-config
  "Safe provider settings for UI/export; never includes supplied credentials."
  [config]
  (select-keys config [:provider :endpoint :model :stream? :max-tokens :temperature]))

(defn- json-data [value]
  (walk/postwalk (fn [x]
                   (cond (keyword? x) (subs (str x) 1)
                         (set? x) (vec (sort-by str x))
                         (map? x) (into {} (map (fn [[k v]] [(str k) v])) x)
                         :else x)) value))

(defn content-string
  "Encode show data for a provider while retaining qualified Clojure identifiers."
  [content]
  (if (string? content) content (json/write-str (json-data content))))

(defn- openai-message [{:keys [role content tool-calls tool-call-id]}]
  (cond-> {:role (name role) :content (content-string (or content ""))}
    (= role :tool) (assoc :tool_call_id tool-call-id)
    (seq tool-calls) (assoc :tool_calls
                           (mapv (fn [{:keys [id name arguments]}]
                                   {:id id :type "function"
                                    :function {:name name :arguments (content-string arguments)}}) tool-calls))))

(defn- anthropic-messages [messages]
  (reduce
   (fn [result {:keys [role content tool-calls tool-call-id]}]
     (let [role (if (= role :tool) "user" (name role))
           blocks (if tool-call-id
                    [{:type "tool_result" :tool_use_id tool-call-id :content (content-string content)}]
                    (into (if (seq (content-string (or content "")))
                            [{:type "text" :text (content-string content)}] [])
                          (map (fn [{:keys [id name arguments]}]
                                 {:type "tool_use" :id id :name name
                                  :input (if (string? arguments) (json/read-str arguments :key-fn keyword) arguments)})
                               tool-calls)))]
       (if (= role (:role (peek result)))
         (update-in result [(dec (count result)) :content] into blocks)
         (conj result {:role role :content blocks}))))
   [] messages))

(defn- provider-tools [provider definitions]
  (mapv (fn [definition]
          (let [definition (or (:function definition) definition)
                schema (or (:parameters definition) (:input_schema definition))]
            (if (= provider :anthropic)
              (assoc (select-keys definition [:name :description]) :input_schema schema)
              {:type "function" :function (assoc (select-keys definition [:name :description]) :parameters schema)})))
        definitions))

(defn request
  "Build a text-chat request for OpenAI Chat Completions, Anthropic Messages,
  OpenRouter, or an OpenAI-compatible self-hosted endpoint. :endpoint is a full
  URL, not a base URL. Credential is separate from config and show state.
  Neutral tool messages use :tool-call-id and content data; assistant tool calls
  use {:id :name :arguments}. Generic tools have :name/:description/:parameters."
  [config messages credential]
  (let [{:keys [provider model temperature tools tool-choice]} config
        endpoint (or (:endpoint config) (get endpoints provider))
        stream? (get config :stream? true)
        max-tokens (get config :max-tokens 2048)]
    (when-not (and (providers provider) (string? model) (not (str/blank? model))
                   (string? endpoint) (instance? Boolean stream?)
                   (integer? max-tokens) (pos? max-tokens)
                   (or (nil? temperature) (and (number? temperature) (Double/isFinite (double temperature))
                                              (<= 0 temperature 2))))
      (throw (ex-info "Invalid provider/model/endpoint/generation settings" {:config (public-config config)})))
    (let [uri (URI. endpoint)]
      (when-not (and (#{"https" "http"} (.getScheme uri)) (.getHost uri)
                     (nil? (.getUserInfo uri)) (nil? (.getFragment uri)))
        (throw (ex-info "Endpoint must be an HTTP(S) URL without embedded credentials" {}))))
    (when-not (or (and (= provider :self-hosted) (nil? credential))
                  (and (string? credential) (not (str/blank? credential))
                       (not (re-find #"[\r\n]" credential))))
      (throw (ex-info "A nonempty server-side credential is required" {:provider provider})))
    (when-not (and (vector? messages) (seq messages)
                   (every? #(and (map? %) (#{:system :user :assistant :tool} (:role %))
                                  (or (= :tool (:role %)) (string? (:content %)))
                                  (or (not= :tool (:role %)) (string? (:tool-call-id %)))) messages))
      (throw (ex-info "Invalid conversation role, text content, or tool-call identity" {})))
    (let [anthropic? (= provider :anthropic)
          system (str/join "\n\n" (map :content (filter #(= :system (:role %)) messages)))
          conversation (if anthropic? (remove #(= :system (:role %)) messages) messages)
          _ (when (empty? conversation) (throw (ex-info "At least one conversational message is required" {})))
          body (cond-> {:model model :messages (if anthropic? (anthropic-messages conversation)
                                                  (mapv openai-message conversation))
                        :stream stream? :max_tokens max-tokens}
                 (and anthropic? (not (str/blank? system))) (assoc :system system)
                 (some? temperature) (assoc :temperature temperature)
                 tools (assoc :tools (provider-tools provider tools))
                 tool-choice (assoc :tool_choice tool-choice)
                 (and stream? (not anthropic?)) (assoc :stream_options {:include_usage true}))
          headers (cond-> {"content-type" "application/json"
                           "accept" (if stream? "text/event-stream" "application/json")}
                    (and credential (not anthropic?)) (assoc "authorization" (str "Bearer " credential))
                    anthropic? (assoc "x-api-key" credential "anthropic-version" "2023-06-01"))]
      {:method :post :url endpoint :headers headers :body (json/write-str body)
       :provider provider :stream? stream?})))

(defn redact-request
  "For diagnostics only. Full requests include credentials and must not be logged."
  [req]
  (update req :headers #(into {} (map (fn [[k v]] [k (if (#{"authorization" "x-api-key" "cookie"} (str/lower-case k))
                                                       "[REDACTED]" v)])) %)))

(defn- openai-events [data]
  (vec
   (concat
    (when (:error data) [{:type :error :error (:error data)}])
    (when (:usage data) [{:type :usage :usage (:usage data)}])
    (mapcat (fn [{:keys [index delta finish_reason]}]
              (concat
               (when (string? (:content delta)) [{:type :text/delta :text (:content delta) :choice (or index 0)}])
               (map (fn [tool]
                      {:type :tool/delta :index (:index tool) :id (:id tool)
                       :name (get-in tool [:function :name])
                       :arguments-delta (get-in tool [:function :arguments] "")}) (:tool_calls delta))
               (when finish_reason [{:type :choice/finished :reason finish_reason :choice (or index 0)}])))
            (:choices data)))))

(defn- anthropic-events [data]
  (case (:type data)
    "message_start" [{:type :message/start :id (get-in data [:message :id])
                      :usage (get-in data [:message :usage])}]
    "content_block_start" (let [block (:content_block data)]
                             (case (:type block)
                               "text" (if (seq (:text block)) [{:type :text/delta :text (:text block)}] [])
                               "tool_use" [{:type :tool/delta :index (:index data) :id (:id block)
                                            :name (:name block) :input (:input block) :arguments-delta ""}]
                               []))
    "content_block_delta" (let [delta (:delta data)]
                             (case (:type delta)
                               "text_delta" [{:type :text/delta :text (:text delta)}]
                               "input_json_delta" [{:type :tool/delta :index (:index data)
                                                    :arguments-delta (:partial_json delta)}]
                               []))
    "content_block_stop" [{:type :tool/block-finished :index (:index data)}]
    "message_delta" [{:type :choice/finished :reason (get-in data [:delta :stop_reason])
                      :usage (:usage data)}]
    "message_stop" [{:type :done}]
    "error" [{:type :error :error (:error data)}]
    []))

(defn new-stream [provider]
  (when-not (providers provider) (throw (ex-info "Unknown stream provider" {:provider provider})))
  {:provider provider :buffer "" :done? false :failed? false :tools {} :event-count 0})

(defn- decode-event [provider frame]
  (let [lines (str/split-lines frame)
        data (->> lines (filter #(str/starts-with? % "data:"))
                  (map #(let [value (subs % 5)] (if (str/starts-with? value " ") (subs value 1) value)))
                  (str/join "\n"))]
    (cond
      (str/blank? data) []
      (= "[DONE]" data) [{:type :done}]
      :else (try
              (let [payload (json/read-str data :key-fn keyword)]
                (if (= provider :anthropic) (anthropic-events payload) (openai-events payload)))
              (catch Exception _ [{:type :error :error {:code :malformed-stream-event
                                                        :message "Provider sent invalid JSON in a complete SSE event"}}])))))

(defn- accumulate-tool [state event]
  (if (= :tool/delta (:type event))
    (update-in state [:tools (:index event)]
               (fn [tool]
                 (cond-> (update (or tool {:arguments ""}) :arguments str (:arguments-delta event ""))
                   (:id event) (assoc :id (:id event))
                   (:name event) (assoc :name (:name event))
                   (:input event) (assoc :input (:input event)))))
    state))

(defn stream-feed
  "Consume arbitrary decoded string chunks, preserving incomplete SSE records.
  CRLF may itself be split across chunks. Returns {:state next :events [...]};
  after terminal done/error, subsequent chunks are ignored. Tool JSON fragments
  accumulate under :state/:tools; no generated tool is executed here."
  [state chunk]
  (when-not (string? chunk) (throw (ex-info "Stream chunks must be decoded strings" {})))
  (if (:done? state)
    {:state state :events []}
    (let [buffer (str (:buffer state) chunk)
          normalized (str/replace buffer "\r\n" "\n")
          pieces (str/split normalized #"\n\n" -1)
          frames (butlast pieces)
          events (->> frames (mapcat #(decode-event (:provider state) %)) vec)
          events (loop [remaining events accepted []]
                   (if-let [event (first remaining)]
                     (if (#{:done :error} (:type event)) (conj accepted event)
                         (recur (next remaining) (conj accepted event))) accepted))
          next (reduce accumulate-tool state events)]
      (if (> (count (last pieces)) 1048576)
        {:state (assoc next :buffer "" :done? true :failed? true)
         :events (conj events {:type :error :error {:code :stream-buffer-limit
                                                   :message "Incomplete SSE event exceeded one MiB of text"}})}
        {:state (-> next (assoc :buffer (last pieces)
                               :done? (boolean (some #(#{:done :error} (:type %)) events))
                               :failed? (boolean (some #(= :error (:type %)) events)))
                    (update :event-count + (count events)))
         :events events}))))

(defn finish-stream
  "Mark EOF. A stream that never sent its terminal marker is reported truncated;
  an incomplete trailing JSON/tool fragment must not be mistaken for success."
  [state]
  (if (:done? state)
    {:state state :events []}
    {:state (assoc state :done? true :failed? true)
     :events [{:type :error :error {:code :truncated-stream :message "Provider stream ended before its completion marker"}}]}))

(defn parse-sse [provider text]
  (let [parsed (stream-feed (new-stream provider) text)
        ended (finish-stream (:state parsed))]
    {:state (:state ended) :events (into (:events parsed) (:events ended))}))

(defn completed-tools
  "Decode completed accumulated arguments as data. Returns per-tool errors on
  invalid JSON; callers still validate against tool schemas and control rules."
  [state]
  (if (or (not (:done? state)) (:failed? state)) []
    (mapv (fn [[index {:keys [arguments input] :as tool}]]
          (try
            (assoc (dissoc tool :arguments) :index index
                   :input (if (str/blank? arguments) (or input {}) (json/read-str arguments :key-fn keyword)))
            (catch Exception _ {:index index :id (:id tool) :error :invalid-tool-json})))
          (sort-by key (:tools state)))))

(defn invoke!
  "Call an injected transport exactly once with the prepared request. Transport
  returns {:status integer :body string}; no default network implementation is
  supplied. Incremental/cancellable HTTP transports should expose stream-feed
  directly rather than collecting an unbounded response through this helper."
  [transport req]
  (let [{:keys [status body]} (transport req)]
    (if-not (and (integer? status) (<= 200 status 299))
      {:ok? false :error {:code :http-error :status status}}
      (try
        (if (:stream? req)
          (let [{:keys [state events]} (parse-sse (:provider req) body)]
            {:ok? (not-any? #(= :error (:type %)) events) :events events
             :text (apply str (map :text (filter #(= :text/delta (:type %)) events)))
             :tools (completed-tools state)})
          (let [data (json/read-str body :key-fn keyword)]
            (if (:error data)
              {:ok? false :error (:error data)}
              {:ok? true :text (if (= :anthropic (:provider req))
                                (apply str (map :text (filter #(= "text" (:type %)) (:content data))))
                                (get-in data [:choices 0 :message :content] ""))
               :response data})))
        (catch Exception _ {:ok? false :error {:code :invalid-response}})))))

(defn http-transport!
  "Explicit optional Java HTTP boundary. POSTs only when called, decodes UTF-8
  incrementally and invokes on-chunk! for streamed response text. Returns status,
  headers and a body for nonstreamed responses. A caller-owned :cancelled? atom
  can abort headers or body reads. Whole-call timeout closes the response stream;
  connect timeout is separately configurable. No automatic redirects or retries.
  Options: :timeout-ms (60000), :connect-timeout-ms (10000), :max-response-chars
  (2097152), :cancelled? (atom false). Callback work should return promptly."
  ([req on-chunk!] (http-transport! req on-chunk! {}))
  ([req on-chunk! options]
   (let [{:keys [timeout-ms connect-timeout-ms max-response-chars]
          :or {timeout-ms 60000 connect-timeout-ms 10000 max-response-chars 2097152}} options
         cancelled? (or (:cancelled? options) (atom false))]
     (when-not (and (every? #(and (integer? %) (pos? %)) [timeout-ms connect-timeout-ms max-response-chars])
                    (instance? clojure.lang.IAtom cancelled?) (ifn? on-chunk!))
       (throw (ex-info "Invalid HTTP transport limits or cancellation token" {})))
     (when @cancelled? (throw (ex-info "Provider request cancelled" {:provider/error :cancelled})))
     (let [client (-> (HttpClient/newBuilder) (.connectTimeout (Duration/ofMillis connect-timeout-ms)) .build)
           builder (-> (HttpRequest/newBuilder (URI. (:url req)))
                       (.timeout (Duration/ofMillis timeout-ms))
                       (.method (str/upper-case (name (:method req)))
                                (HttpRequest$BodyPublishers/ofString (:body req) StandardCharsets/UTF_8)))
           _ (doseq [[header value] (:headers req)] (.header builder header value))
           response-future (.sendAsync client (.build builder) (HttpResponse$BodyHandlers/ofInputStream))
           body-stream (atom nil)
           timed-out? (atom false)
           abort! (fn []
                    (.cancel response-future true)
                    (when-let [^InputStream stream @body-stream]
                      (try (.close stream) (catch Exception _))))
           watch-key (Object.)
           scheduler (Executors/newSingleThreadScheduledExecutor
                      (reify ThreadFactory
                        (newThread [_ runnable]
                          (doto (Thread. runnable "tolglow-provider-timeout") (.setDaemon true)))))
           timeout-task (.schedule scheduler ^Runnable (fn [] (reset! timed-out? true) (abort!))
                                   (long timeout-ms) TimeUnit/MILLISECONDS)]
       (add-watch cancelled? watch-key (fn [_ _ _ value] (when value (abort!))))
       (try
         (when @cancelled? (abort!) (throw (ex-info "Cancelled" {})))
         (let [response (.get response-future)
               ^InputStream stream (.body response)
               status (.statusCode response)]
           (reset! body-stream stream)
           (when (or @cancelled? @timed-out?) (abort!) (throw (ex-info "Aborted" {})))
           (if-not (<= 200 status 299)
             {:status status :headers (.map (.headers response)) :body ""}
             (with-open [reader (InputStreamReader. stream StandardCharsets/UTF_8)]
               (let [buffer (char-array 4096) result (StringBuilder.)]
                 (loop [received 0]
                   (when (or @cancelled? @timed-out?) (throw (ex-info "Aborted" {})))
                   (let [n (.read reader buffer)]
                     (if (= -1 n)
                       {:status status :headers (.map (.headers response))
                        :body (when-not (:stream? req) (str result)) :received-chars received}
                       (let [received (+ received n) chunk (String. buffer 0 n)]
                         (when (> received max-response-chars)
                           (throw (ex-info "Provider response exceeded configured limit" {:provider/error :response-limit})))
                         (if (:stream? req) (on-chunk! chunk) (.append result chunk))
                         (recur received)))))))))
         (catch Exception exception
           (throw (ex-info (cond @timed-out? "Provider request timed out"
                                 @cancelled? "Provider request cancelled"
                                 :else "Provider transport failed")
                           {:provider/error (cond @timed-out? :timeout @cancelled? :cancelled
                                                  :else (or (:provider/error (ex-data exception)) :transport))}
                           exception)))
         (finally
           (remove-watch cancelled? watch-key)
           (.cancel timeout-task false)
           (.shutdownNow scheduler)
           (when-let [^InputStream stream @body-stream]
             (try (.close stream) (catch Exception _)))))))))

(defn start-http!
  "Start the explicit HTTP boundary on a worker. Returns a dereferenceable
  :result and :cancel! function; cancelling closes any active response body."
  ([req on-chunk!] (start-http! req on-chunk! {}))
  ([req on-chunk! options]
   (let [cancelled? (or (:cancelled? options) (atom false))]
     {:result (future (http-transport! req on-chunk! (assoc options :cancelled? cancelled?)))
      :cancel! #(compare-and-set! cancelled? false true)})))
