(ns tolglow.studio.web-test
  (:require [afterglow.render.gpu :as gpu]
            [clojure.test :refer :all]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty]
            [ring.core.protocols :as protocols]
            [tolglow.transit :as transit]
            [tolglow.studio.web :as web])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.io ByteArrayInputStream IOException OutputStream]))

(defn request [method uri data]
  {:request-method method :uri uri :headers {"host" "localhost"}
   :body (ByteArrayInputStream. (.getBytes (json/write-str data) "UTF-8"))})

(deftest ring-middleware-decodes-params-and-serves-binary-resources
  (let [seen (atom [])
        app (web/app {:heartbeat! #(swap! seen conj %) :snapshot! (constantly {:show-id :main})
                      :definition! (fn [kind id] {:kind kind :id id})
                      :submit! (fn [type payload] (doto (promise) (deliver {:status :accepted :type type :payload payload})))})
        state (app {:request-method :get :uri "/api/state" :query-string "session=ipad%20one"})
        definition (app {:request-method :get :uri "/api/definition" :query-string "kind=effects&id=user%2Fwash"})
        command (app (request :post "/api/command" {:op "page/put" :page {:id "page-a" :cells [{:x 1 :y 2 :cue-id "red"}]}}))]
    (is (= ["ipad one" nil] @seen))
    (is (= "main" (get (json/read-str (:body state)) "show-id")))
    (is (= "{:id :user/wash :kind :effects}" (get (json/read-str (:body definition)) "source")))
    (is (= 200 (:status command)))
    (doseq [uri ["/" "/studio/preview.js"]]
      (let [asset (app {:request-method :get :uri uri})]
        (is (= 200 (:status asset)))
        (is (get-in asset [:headers "Content-Type"]))
        (when (= uri "/") (is (= "text/html; charset=utf-8" (get-in asset [:headers "Content-Type"]))))
        (when (instance? java.io.InputStream (:body asset)) (.close ^java.io.InputStream (:body asset)))))
    (is (= 404 (:status (app {:request-method :get :uri "/studio/../deps.edn"}))))
    (is (= 403 (:status (app (assoc-in (request :post "/api/command" {}) [:headers "origin"] "https://foreign.test")))))
    (is (= 413 (:status (app (request :post "/api/command" {:value (apply str (repeat 262145 "x"))})))))))

(deftest studio-serves-the-paired-afterglow-renderer
  (let [app (web/app {})]
    (doseq [[asset path] gpu/resources]
      (let [response (app {:request-method :get :uri (str "/" path)})]
        (is (= 200 (:status response)) path)
        (when (#{:expression-runner :moving-head-solver} asset)
          (is (= "text/javascript" (get-in response [:headers "Content-Type"])) path))
        (is (= (gpu/source asset) (slurp (:body response) :encoding "UTF-8")) path)))))

(deftest stream-cancels-producer-when-ring-output-fails
  (let [cancelled? (atom false)
        app (web/app {:chat! (fn [_ emit! _]
                              (emit! {:type :text/delta :text "hello"})
                              {:cancel! #(reset! cancelled? true)})})
        response (app (request :post "/api/chat" {:message "test"}))
        broken (proxy [OutputStream] [] (write [& _] (throw (IOException. "Client disconnected"))))]
    (is (thrown? IOException (protocols/write-body-to-stream (:body response) response broken)))
    (is @cancelled?)))

(deftest actual-ring-jetty-stream-arrives-before-producer-finishes
  (let [release (promise) finished (promise)
        app (web/app {:chat! (fn [_ emit! done!]
                              (future
                                (try
                                  (emit! {:type :text/delta :text "first"})
                                  @release
                                  (done! {:status :completed})
                                  (finally (deliver finished true))))
                              {:cancel! #(deliver release true)})})
        server (jetty/run-jetty app {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (let [port (.getLocalPort (first (.getConnectors server)))
            req (-> (HttpRequest/newBuilder (URI. (str "http://127.0.0.1:" port "/api/chat")))
                    (.timeout (java.time.Duration/ofSeconds 5))
                    (.POST (java.net.http.HttpRequest$BodyPublishers/ofString "{\"message\":\"test\"}")) .build)
            response (.send (HttpClient/newHttpClient) req (HttpResponse$BodyHandlers/ofInputStream))]
        (with-open [reader (io/reader (.body response))]
          (is (= 200 (.statusCode response)))
          (is (= {"type" "text/delta" "text" "first"} (json/read-str (subs (.readLine reader) 6))))
          (is (not (realized? finished)) "First chunk arrives while producer is still waiting")
          (deliver release true)
          (.readLine reader)
          (is (= "chat/done" (get (json/read-str (subs (.readLine reader) 6)) "type")))
          (is (= true (deref finished 1000 false)))))
      (finally (deliver release true) (.stop server)))))

(deftest reitit-routes-match-pages-methods-and-stream-upgrades
  (let [upgrades (atom [])
        app (web/app {:state-stream! (fn [request]
                                      (swap! upgrades conj (:headers request))
                                      {:status 101 :headers {} :body nil})
                      :snapshot! (constantly {})
                      :heartbeat! (constantly nil)})]
    (doseq [path ["/perform" "/compose" "/rig" "/library" "/assistant" "/connections"
                  "/develop" "/repl" "/push-display"]]
      (let [page (app {:request-method :get :uri path})]
        (is (= [200 "text/html; charset=utf-8"]
               [(:status page) (get-in page [:headers "Content-Type"])]))
        (when (instance? java.io.Closeable (:body page)) (.close ^java.io.Closeable (:body page)))))
    (let [rejected (app (request :post "/api/state" {}))]
      (is (= 405 (:status rejected)))
      (is (= "GET, HEAD" (get-in rejected [:headers "Allow"]))))
    (is (= 405 (:status (app (request :get "/api/command" {})))))
    (is (= 404 (:status (app (request :put "/api/nonexistent" {})))))
    (is (= 404 (:status (app (request :get "/unknown-view" {})))))
    (is (= 404 (:status (app (request :get "/studio/no-such-file.js" {})))))
    (is (= 200 (:status (app (request :head "/rig" {})))))
    (is (= 405 (:status (app (request :head "/api/state/stream" {})))))
    (is (empty? @upgrades))
    (is (= 101 (:status (app {:request-method :get :uri "/api/state/stream"
                              :headers {"upgrade" "websocket"}}))))
    (is (= [{"upgrade" "websocket"}] @upgrades))))

(deftest the-listener-names-who-issued-each-command
  ;; Every HTTP path whose :source decides precedence, ownership or agent limits.
  (let [received (atom []) beats (atom [])
        app (web/app {:heartbeat! #(swap! beats conj %)
                      :submit! (fn [type payload]
                                 (swap! received conj [type payload])
                                 (doto (promise) (deliver {:status :accepted})))})
        claims [{:adapter "agent" :session-id "run-7" :run-id "x"}
                {:adapter "repl" :session-id "repl"}
                {:adapter "auto" :session-id "auto/0/3"}
                {:adapter "push-3" :session-id "push3-owner" :device-id "p3"}
                {:adapter "surface" :session-id "xt-main"}
                {:adapter "lighting-scene" :group-id "front"}
                {:adapter "native-ios" :session-id "phone"}]
        bodies {"/api/command" {:op "cue/launch" :cue-id "ember"}
                "/api/auto" {:enabled? true :at "next-bar"}
                "/api/settings" {:scope "lighting-budget" :values {:global {:limit 1.0}}}
                "/api/lighting-scene" {:op "apply" :group-id "front" :fixture-ids ["head-1"]}}
        transit-post (fn [uri data]
                       (app {:request-method :post :uri uri
                             :headers {"host" "localhost" "content-type" "application/transit+json"}
                             :body (ByteArrayInputStream. (.getBytes ^String (transit/encode data) "UTF-8"))}))]
    (doseq [[uri body] bodies claim claims
            post [#(app (request :post %1 %2)) transit-post]]
      (reset! received [])
      (is (= 200 (:status (post uri (assoc body :source claim)))))
      (let [[[_ payload]] @received]
        (is (= (cond-> {:adapter :web} (:session-id claim) (assoc :session-id (:session-id claim)))
               (:source payload))
            (str uri " " claim))))
    (testing "a command without a source, or with an unusable session, is still the web surface"
      (reset! received [])
      (app (request :post "/api/command" {:op "cue/launch" :cue-id "ember"}))
      (app (request :post "/api/command" {:op "cue/launch" :cue-id "ember" :source {:adapter "agent" :session-id 7}}))
      (app (request :post "/api/command" {:op "cue/launch" :cue-id "ember"
                                          :source {:adapter "agent" :session-id (apply str (repeat 129 "s"))}}))
      (is (= (repeat 3 {:adapter :web}) (map (comp :source second) @received))))
    (testing "where :source is request data, it reaches the show unchanged"
      (reset! received [])
      (app (request :post "/api/definition" {:kind "effects" :source "{:id :wash :version 1 :params {} :outputs {}}"}))
      (app (request :post "/api/patch/save" {:source "{:stems []}"}))
      (is (= [[:definition "{:id :wash :version 1 :params {} :outputs {}}"] [:patch-save "{:stems []}"]]
             (map (fn [[type payload]] [type (:source payload)]) @received))))
    (is (some #{"run-7"} @beats) "The tab's session still renews its lease")))

(deftest json-and-form-labelled-bodies-use-the-route-size-limit
  (let [app (web/app {:heartbeat! identity :submit! (fn [& _] (throw (AssertionError. "unexpected dispatch")))})
        text (apply str (repeat 262145 " "))]
    (doseq [mime ["application/json" "application/x-www-form-urlencoded"]]
      (is (= 413 (:status (app {:request-method :post :uri "/api/command"
                                :headers {"content-type" mime}
                                :body (java.io.StringReader. text)})))))))

(deftest invitation-images-require-the-current-stage-id
  (let [png (byte-array [1 2 3])
        app (web/app {:sensor! (fn [op {:keys [id stage-id]}]
                                 (case op
                                   :stage-qr (when (= id "active") png)
                                   :stage-stop {:stopped? (= "active" stage-id)}))})
        fetch (fn [id] (app {:request-method :get :uri "/api/sensors/stage/qr" :query-string (str "id=" id)}))
        current (fetch "active")]
    (is (= {"stopped?" true}
           (json/read-str (:body (app (request :post "/api/sensors/stage/stop" {:stage-id "active"}))))))
    (is (= 404 (:status (fetch "expired"))))
    (is (= "image/png" (get-in current [:headers "Content-Type"])))
    (is (= "no-store" (get-in current [:headers "Cache-Control"])))
    (with-open [image (:body current)]
      (is (= [1 2 3] (vec (.readAllBytes image)))))))
