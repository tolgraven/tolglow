(ns tolglow.studio.agent-route-test
  "HTTP agents under agent limits (#903): the voluntary {:adapter \"agent\"}
  downgrade on the operator routes, and the agent tool route that only the
  listener's agent credential reaches. Both drive the real Studio handler over
  the show's command path; the host test binds a loopback listener."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.integrations.agent :as agent]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.access :as access]
            [tolglow.studio.web :as web])
  (:import [java.io ByteArrayInputStream]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers
            WebSocket$Listener WebSocketHandshakeException]
           [java.util Base64]
           [java.util.concurrent ExecutionException TimeUnit]))

(def operator-token "test-only-operator-credential-903")

(def agent-token "test-only-agent-credential-903-x")

(def ^:private secrets {"TEST_OPERATOR" operator-token "TEST_AGENT" agent-token})

(defn- ring-post [app uri payload]
  (let [response (app {:request-method :post :uri uri :scheme :http :headers {"host" "127.0.0.1"}
                       :body (ByteArrayInputStream. (.getBytes (json/write-str payload) "UTF-8"))})]
    [(:status response) (json/read-str (:body response) :key-fn keyword)]))

(defn- upcoming [runtime] (set (map :event-id (:upcoming (show/schedule runtime)))))

(deftest a-client-that-claims-to-be-an-agent-is-held-to-agent-limits
  (let [r (show/start! {:model (demo/model) :recovery false})
        ;; The loopback listener admits every client as the operator.
        app (access/listener-handler (studio/handler r) {})
        bot {:adapter "agent" :session-id "bot"}
        post! (partial ring-post app)
        budget #(get-in @(:state r) [:model :settings :lighting-budget])
        source-of #(get-in @(:state r) [:model :control :history % :command :source])]
    (try
      (testing "the command path records the downgraded source"
        (let [[status ack] (post! "/api/command" {:op "cue/launch" :cue-id "ember" :command-id "bot-launch" :source bot})]
          (is (= [200 "accepted"] [status (:status ack)]))
          (is (= {:adapter :agent :session-id "http/bot"} (source-of "bot-launch")))))
      (testing "it cannot schedule master or blackout; the operator, on the same route, can"
        (doseq [event [{:id "m" :op "master/set" :value 0.2 :in {:bars 1}}
                       {:id "b" :op "blackout/set" :value true :in {:bars 1}}]]
          (let [[status ack] (post! "/api/command" {:op "schedule/post" :events [event] :source bot})]
            (is (= [422 "rejected"] [status (:status ack)]) (:op event))
            (is (re-find #"authoritative" (str (:message ack))))))
        (is (empty? (upcoming r)))
        (let [[status ack] (post! "/api/command" {:op "schedule/post" :source {:session-id "tab"}
                                                  :events [{:id "web-master" :op "master/set" :value 0.2 :in {:bars 4}}]})]
          (is (= [200 "accepted"] [status (:status ack)]))))
      (testing "it cancels only what it posted itself, never a web-posted event"
        (is (= 200 (first (post! "/api/command" {:op "schedule/post" :source {:session-id "tab"}
                                                 :events [{:id "web-1" :op "cue/launch" :cue-id "ice" :in {:bars 8}}]}))))
        (is (= 200 (first (post! "/api/command" {:op "schedule/post" :source bot
                                                 :events [{:id "bot-1" :op "cue/launch" :cue-id "ice" :in {:bars 8}}]}))))
        (let [[_ denied] (post! "/api/command" {:op "schedule/cancel" :event-ids ["web-1" "web-master"] :source bot})
              [_ other] (post! "/api/command" {:op "schedule/cancel" :event-ids ["bot-1"]
                                               :source {:adapter "agent" :session-id "other"}})]
          (is (= [] (:event-ids denied)))
          (is (= [] (:event-ids other)) "another agent session does not own it either")
          (is (= #{"web-1" "web-master" "bot-1"} (upcoming r))))
        (let [[_ own] (post! "/api/command" {:op "schedule/cancel" :event-ids ["bot-1" "web-1"] :source bot})]
          (is (= ["bot-1"] (:event-ids own)))
          (is (= #{"web-1" "web-master"} (upcoming r)))))
      (testing "it may tighten the operator's lighting budget but not loosen it past the operator's setting"
        (is (= 200 (first (post! "/api/settings" {:scope "lighting-budget" :values {:enabled? true :global {:limit 0.8}}}))))
        (let [[status ack] (post! "/api/settings" {:scope "lighting-budget" :values {:global {:limit 0.5}} :source bot})]
          (is (= [200 "accepted"] [status (:status ack)])))
        (is (= 0.5 (get-in (budget) [:global :limit])))
        (let [[status ack] (post! "/api/settings" {:scope "lighting-budget" :values {:global {:limit 0.9}} :source bot})]
          (is (= [422 "rejected"] [status (:status ack)]))
          (is (= "agent-budget-loosening" (get-in ack [:details :code]))))
        (is (= 0.5 (get-in (budget) [:global :limit])) "the refusal changes nothing")
        (is (= 200 (first (post! "/api/settings" {:scope "lighting-budget" :values {:global {:limit 1.0}}})))
            "the same request without the claim is the unrestricted operator"))
      (testing "it gives up the operator's edit history"
        (let [[status ack] (post! "/api/command" {:op "history/undo" :runtime-id (:id r) :expected-epoch 0
                                                  :expected-cursor 0 :expected-history-position 0 :source bot})]
          (is (= [422 "rejected" "operator-required"] [status (:status ack) (:code ack)]))))
      (testing "the operator's listener is not an agent credential"
        (let [[status ack] (post! access/agent-tool-path {:tool "inspect_schedule"})]
          (is (= [403 "agent-credential-required"] [status (:code ack)]))))
      (finally (show/stop! r)))))

(deftest construction-keeps-the-agent-credential-separate
  (let [handler (constantly {:status 200})]
    (with-redefs-fn {#'access/secret-setting #(get (assoc secrets "SAME" operator-token) %)}
      (fn []
        (is (ifn? (access/listener-handler handler {:auth {:token-env "TEST_OPERATOR" :agent-token-env "TEST_AGENT"}})))
        (doseq [auth [{:token-env "TEST_OPERATOR" :agent-token-env "SAME"}
                      {:token-env "TEST_OPERATOR" :agent-token-env nil}]]
          (is (thrown? clojure.lang.ExceptionInfo (access/listener-handler handler {:auth auth})) (pr-str auth)))))
    (is (thrown? clojure.lang.ExceptionInfo (access/authenticated handler operator-token "short")))))

(deftest the-agent-credential-binds-an-http-agent-to-the-agent-tool-route
  (let [r (show/start! {:model (demo/model) :recovery false})
        {:keys [port stop!]} (with-redefs-fn {#'access/secret-setting secrets}
                               #(studio/listen! r {:port 0 :auth {:token-env "TEST_OPERATOR" :agent-token-env "TEST_AGENT"}}))
        client (HttpClient/newHttpClient)
        root (str "127.0.0.1:" port)
        request! (fn [method path headers & [payload]]
                   (let [req (HttpRequest/newBuilder (URI/create (str "http://" root path)))]
                     (doseq [[k v] headers] (.header req k v))
                     (when (= method :post)
                       (.header req "Content-Type" "application/json")
                       (.POST req (HttpRequest$BodyPublishers/ofString (json/write-str (or payload {})))))
                     (let [response (.send client (.build req) (HttpResponse$BodyHandlers/ofString))
                           body (.body response)]
                       {:status (.statusCode response)
                        :cookie (.orElse (.firstValue (.headers response) "Set-Cookie") nil)
                        :body (when (str/starts-with? (str (.orElse (.firstValue (.headers response) "Content-Type") "")) "application/json")
                                (json/read-str body :key-fn keyword))})))
        as-agent {"Authorization" (str "Bearer " agent-token)}
        as-operator {"Authorization" (str "Bearer " operator-token)}
        tool! (fn [payload] (request! :post access/agent-tool-path as-agent payload))
        instance-source #(get-in @(:state r) [:model :instances % :source])]
    (try
      (testing "an unauthenticated client gains neither agent nor operator rights"
        (doseq [headers [{} {"Authorization" "Bearer not-the-agent-credential-at-all"}
                         ;; The agent token in the operator's Basic sign-in form.
                         {"Authorization" (str "Basic " (.encodeToString (Base64/getEncoder)
                                                                         (.getBytes (str "studio:" agent-token) "UTF-8")))}]]
          (is (= 401 (:status (request! :post access/agent-tool-path headers {:tool "inspect_schedule"})))
              (pr-str headers))
          (is (= 401 (:status (request! :post "/api/command" headers
                                        {:op "cue/launch" :cue-id "ember" :source {:adapter "agent" :session-id "x"}}))))
          (is (= 401 (:status (request! :get "/api/state" headers)))))
        (is (empty? (:instances (:model @(:state r))))))
      (testing "the agent token runs a declared tool as {:adapter :agent ...} with its listener-assigned session"
        (let [{:keys [status body]} (request! :get access/agent-tool-path as-agent)]
          (is (= 200 status))
          (is (= (mapv :name agent/default-tools) (mapv :name (:tools body)))))
        (let [{:keys [status body cookie]} (tool! {:tool "launch_cue" :session-id "claude" :run-id "turn-1"
                                                   :arguments {:cue-id "ember"}})]
          (is (= [200 "accepted"] [status (:status body)]))
          (is (nil? cookie) "the agent credential opens no browser session")
          (is (= {:adapter :agent :session-id "http-agent/claude" :run-id "turn-1"}
                 (instance-source (:instance-id body))))))
      (testing "tool calls are held to the declared schemas"
        (let [{:keys [status body]} (tool! {:tool "launch_cue" :arguments {:cue-id "ember" :source {:adapter "web"}}})]
          (is (= [422 "invalid-tool-call"] [status (:code body)])))
        (let [{:keys [status body]} (tool! {:tool "launch_cue" :arguments {}})]
          (is (= [422 "invalid-tool-call"] [status (:code body)])))
        (let [{:keys [status body]} (tool! {:tool "set_master" :arguments {}})]
          (is (= [422 "unknown-tool"] [status (:code body)]))))
      (testing "a refusal the show raises answers 422 with a stable code"
        (let [epoch (get-in @(:state r) [:model :epoch])
              {:keys [status body]} (tool! {:tool "launch_cue" :expected-epoch (inc epoch) :command-id "stale"
                                            :arguments {:cue-id "ember"}})]
          (is (= [422 "rejected" "stale-show-session"] [status (:status body) (:code body)]))
          (is (= "http-agent/agent/stale" (:command-id body))))
        (let [{:keys [status body]} (tool! {:tool "set_lighting_budget" :arguments {:group-id "no-such-group" :limit 0.5}})]
          (is (= [422 "rejected" "tool-refused"] [status (:status body) (:code body)]))
          (is (re-find #"existing group" (:message body)))))
      (testing "and to the agent limits, on the same validated command path"
        (let [{:keys [status body]} (tool! {:tool "schedule_events" :session-id "claude"
                                            :arguments {:events [{:id "m" :op "master/set" :value 0.1 :in {:bars 1}}]}})]
          (is (= [422 "rejected"] [status (:status body)]))
          (is (re-find #"authoritative" (:message body))))
        (is (= 200 (:status (request! :post "/api/command" as-operator
                                      {:op "schedule/post" :events [{:id "web-1" :op "cue/launch" :cue-id "ice" :in {:bars 8}}]}))))
        (let [{:keys [body]} (tool! {:tool "cancel_scheduled" :session-id "claude" :arguments {:event-ids ["web-1"]}})]
          (is (= [] (:event-ids body)))
          (is (= #{"web-1"} (upcoming r))))
        (testing "Studio and the agent read each other's schedule from one projection"
          (is (= "accepted" (get-in (tool! {:tool "schedule_events" :session-id "claude"
                                            :arguments {:events [{:id "agent-1" :op "cue/launch" :cue-id "ice" :in {:bars 8}}]}})
                                    [:body :status])))
          (let [studio-view (get-in (request! :get "/api/state" as-operator) [:body :schedule :upcoming])
                agent-view (get-in (tool! {:tool "inspect_schedule" :session-id "claude"}) [:body :upcoming])]
            (is (= {:adapter "agent" :session-id "http-agent/claude"}
                   (select-keys (:source (first (filter #(= "agent-1" (:event-id %)) studio-view))) [:adapter :session-id])))
            (is (= #{"web-1" "agent-1"} (set (map second agent-view)))))
          (is (= ["agent-1"] (get-in (request! :post "/api/command" as-operator {:op "schedule/cancel" :event-ids ["agent-1"]})
                                     [:body :event-ids]))
              "the operator may cancel the agent's event"))
        (is (= 200 (:status (request! :post "/api/settings" as-operator
                                      {:scope "lighting-budget" :values {:enabled? true :global {:limit 0.8}}}))))
        (is (= "accepted" (get-in (tool! {:tool "set_lighting_budget" :arguments {:limit 0.5}}) [:body :status])))
        (let [{:keys [status body]} (tool! {:tool "set_lighting_budget" :arguments {:limit 0.9}})]
          (is (= [422 "agent-budget-loosening"] [status (get-in body [:details :code])]))))
      (testing "the operator credential is refused on the agent route"
        (let [signed (request! :get "/api/state" as-operator)
              cookie (first (str/split (:cookie signed) #";"))]
          (doseq [headers [as-operator {"Cookie" cookie}]]
            (let [{:keys [status body]} (request! :post access/agent-tool-path headers {:tool "inspect_schedule"})]
              (is (= [403 "agent-credential-required"] [status (:code body)]) (pr-str (keys headers)))))
          (testing "while the agent header beside an operator cookie still speaks as the agent there"
            (let [{:keys [status body cookie]} (request! :post access/agent-tool-path (assoc as-agent "Cookie" cookie)
                                                         {:tool "launch_cue" :session-id "cookie" :arguments {:cue-id "ice"}})]
              (is (= [200 "accepted"] [status (:status body)]))
              (is (nil? cookie))
              (is (= "http-agent/cookie" (:session-id (instance-source (:instance-id body)))))))
          (testing "and the agent credential on every operator route, even beside an operator cookie"
            (doseq [[method path] [[:get "/"] [:get "/api/state"] [:get "/api/chat/history"] [:post "/api/command"]
                                   [:post "/api/settings"] [:post "/api/repl/eval"] [:post "/api/agent/tool/x"]]
                    headers [as-agent (assoc as-agent "Cookie" cookie)]]
              (is (= 403 (:status (request! method path headers {:op "cue/launch" :cue-id "ember"})))
                  (str method " " path)))
            (let [failure (try (.get (.buildAsync (doto (.newWebSocketBuilder client)
                                                    (.header "Authorization" (str "Bearer " agent-token)))
                                                  (URI/create (str "ws://" root "/api/state/stream?session=agent"))
                                                  (reify WebSocket$Listener))
                                     3 TimeUnit/SECONDS)
                               (catch ExecutionException error (.getCause error)))]
              (is (instance? WebSocketHandshakeException failure))
              (is (= 403 (some-> ^WebSocketHandshakeException failure .getResponse .statusCode)))))))
      (finally (stop!) (show/stop! r)))))

(deftest agent-calls-are-bounded-per-session-in-flight-and-per-window
  ;; The runtime is not scheduled: a launch waits on the show queue until the
  ;; test ticks it, which holds that call in flight.
  (let [r (show/create-runtime {:model (demo/model) :workspace-options nil})
        clock (atom 0)
        {:keys [port stop!]} (with-redefs-fn {#'access/secret-setting secrets}
                               #(studio/listen! r {:port 0 :auth {:token-env "TEST_OPERATOR" :agent-token-env "TEST_AGENT"}
                                                   :agent-limits {:per-session 1 :in-flight 2 :calls 6 :window-ms 1000
                                                                  :now-ms (fn [] @clock)}}))
        client (HttpClient/newHttpClient)
        call! (fn [payload]
                (let [response (.send client (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" port access/agent-tool-path)))
                                                 (.header "Authorization" (str "Bearer " agent-token))
                                                 (.header "Content-Type" "application/json")
                                                 (.POST (HttpRequest$BodyPublishers/ofString (json/write-str payload)))
                                                 .build)
                                      (HttpResponse$BodyHandlers/ofString))]
                  [(.statusCode response) (json/read-str (.body response) :key-fn keyword)
                   (.orElse (.firstValue (.headers response) "Retry-After") nil)]))
        inspect (fn [session] {:tool "inspect_schedule" :session-id session})
        held (future (call! {:tool "launch_cue" :session-id "loop" :arguments {:cue-id "ember"}}))]
    (try
      (testing "a session with a call in flight is refused a second one; another session is not"
        (let [busy (loop [n 0]
                     (let [[status body] (call! (inspect "loop"))]
                       (if (or (= 429 status) (>= n 200)) [status (:code body)]
                         (do (Thread/sleep 5) (recur (inc n))))))]
          (is (= [429 "agent-session-busy"] busy)))
        (is (= 200 (first (call! (inspect "other"))))))
      (testing "release by the show frees the session"
        (loop [n 0]
          (when (and (not (realized? held)) (< n 400))
            (show/tick! r (inc (get-in @(:state r) [:model :time-ms])))
            (Thread/sleep 5) (recur (inc n))))
        (is (= 200 (first (deref held 1000 [::timeout]))))
        (is (= 200 (first (call! (inspect "loop"))))))
      (testing "the credential's calls per window are bounded, invalid ones included, and recover after the window"
        (swap! clock + 1000)
        (let [responses (vec (repeatedly 8 #(call! {:tool "no_such_tool" :session-id "spam"})))
              [status refused retry-after] (peek responses)]
          (is (= (concat (repeat 6 422) [429 429]) (map first responses)))
          (is (= [429 "agent-call-budget" "1"] [status (:code refused) retry-after])))
        (is (= 429 (first (call! (inspect "fresh")))) "every session shares the credential's budget")
        (swap! clock + 1000)
        (is (= 200 (first (call! (inspect "fresh"))))))
      (finally (future-cancel held) (stop!) (show/stop! r)))))

(deftest agent-limits-are-validated-listener-options
  (is (= web/default-agent-limits (web/agent-limits nil)))
  (doseq [bad [{:per-session 0} {:per-session 5 :in-flight 4} {:calls -1} {:window-ms 1.5} {:now-ms 3}]]
    (is (thrown? clojure.lang.ExceptionInfo (web/agent-limits bad)) (pr-str bad))))
