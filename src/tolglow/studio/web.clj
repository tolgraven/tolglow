(ns tolglow.studio.web
  "Portable Ring application. Services are explicit callbacks; the HTTP adapter
  does not own the show clock, registry, provider credentials, or effect state."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.head :refer [wrap-head]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.core.protocols :as protocols]
            [ring.util.response :as response])
  (:import [java.io PipedInputStream PipedOutputStream IOException]))

(defn json-data [data]
  (walk/postwalk (fn [x] (cond (keyword? x) (subs (str x) 1)
                              (set? x) (vec x)
                              (map? x) (into {} (map (fn [[k v]] [(if (string? k) k (str k)) v])) x)
                              :else x)) data))

(def keyword-fields
  #{:op :kind :mode :loop :scope :intent :provider :reload-policy :blend :attribute
    :param :type :rgbw-policy :cue-id :effect-id :page-id :show-id :key :axis :curve
    :timebase :operation :retrigger :unit :distribution :match})

(defn normalize-json [data]
  (walk/postwalk
   (fn [x]
     (if (map? x)
       (into {} (map (fn [[k v]]
                       [k (cond
                            (and (string? v) (or (keyword-fields k) (= k :id))) (keyword v)
                            (and (= k :params) (= v "all")) :all
                            (and (vector? v) (#{:params :attributes :targets :effect-ids :cue-ids :fixture-ids :categories} k))
                            (mapv #(if (string? %) (keyword %) %) v)
                            :else v)])) x) x)) data))

(defn json-response [status data]
  {:status status :headers {"Content-Type" "application/json; charset=utf-8" "Cache-Control" "no-store"}
   :body (json/write-str (json-data data))})

(defn read-edn
  "Read exactly one EDN definition; no trailing forms or custom readers."
  [source]
  (when-not (string? source) (throw (ex-info "Definition source must be text" {})))
  (with-open [reader (java.io.PushbackReader. (java.io.StringReader. source))]
    (let [eof (Object.) value (edn/read {:eof eof :readers {}} reader)]
      (when (or (identical? eof value)
                (not (identical? eof (edn/read {:eof eof :readers {}} reader))))
        (throw (ex-info "Expected exactly one EDN definition" {})))
      value)))

(defn- read-json [request]
  (with-open [reader (io/reader (or (:body request) (java.io.StringReader. "{}")))]
    (let [buffer (char-array 4096) text (StringBuilder.)
          limit (cond
                  (#{"/api/import" "/api/fixtures/import" "/api/fixtures/publish"} (:uri request)) (* 32 1024 1024)
                  (= "/api/workspaces/write" (:uri request)) (* 13 1024 1024)
                  :else 262144)]
      (loop []
        (let [n (.read reader buffer)]
          (when (pos? n)
            (when (> (+ (.length text) n) limit)
              (throw (ex-info "Request body too large" {:http/status 413})))
            (.append text buffer 0 n)
            (recur))))
      (json/read-str (if (zero? (.length text)) "{}" (str text)) :key-fn keyword))))

(defn wrap-json-body [handler]
  (fn [request]
    (handler (if (and (= :post (:request-method request)) (str/starts-with? (:uri request) "/api/"))
               (assoc request :json-params
                      (let [data (read-json request)]
                        (if (str/starts-with? (:uri request) "/api/fixtures/") data (normalize-json data)))) request))))

(defn wrap-errors [handler]
  (fn [request]
    (try (handler request)
         (catch Exception error
           (json-response (or (:http/status (ex-data error)) 400)
                          {:message (.getMessage error) :details (ex-data error)})))))

(defn wrap-same-origin [handler]
  (fn [request]
    (let [origin (get-in request [:headers "origin"])
          host (get-in request [:headers "host"])]
      (if (and (#{:post :put :patch :delete} (:request-method request)) origin
               (not= host (.getAuthority (java.net.URI. origin))))
        (json-response 403 {:message "Cross-origin mutations are not accepted"})
        (handler request)))))

(defn wrap-isolation [handler]
  (fn [request]
    (update (handler request) :headers merge
            {"Cross-Origin-Opener-Policy" "same-origin"
             "Cross-Origin-Embedder-Policy" "require-corp"
             "Cross-Origin-Resource-Policy" "same-origin"
             "X-Content-Type-Options" "nosniff"})))

(defrecord EventStreamBody [input]
  java.io.Closeable
  (close [_] (.close ^java.io.InputStream input))
  protocols/StreamableResponseBody
  (write-body-to-stream [_ _ output]
    (try
      (let [buffer (byte-array 4096)]
        (loop []
          (let [n (.read ^java.io.InputStream input buffer)]
            (when (pos? n)
              (.write ^java.io.OutputStream output buffer 0 n)
              (.flush ^java.io.OutputStream output)
              (recur)))))
      (finally (.close ^java.io.InputStream input) (.close ^java.io.OutputStream output)))))

(defn event-stream
  "A bounded streaming Ring body, independent of any server's channel API.
  Closing the response body cancels the producer and releases its pipe."
  ([start! payload] (event-stream start! payload {:done-event :chat/done}))
  ([start! payload {:keys [done-event]}]
  (let [cancel (atom nil) closed? (atom false)
        input (proxy [PipedInputStream] [16384]
                (close []
                  (when (compare-and-set! closed? false true)
                    (when-let [cancel! @cancel] (cancel!)))
                  (proxy-super close)))
        output (PipedOutputStream. input)
        emit! (fn [event]
                (locking output
                  (.write output (.getBytes (str "data: " (json/write-str (json-data event)) "\n\n") "UTF-8"))
                  (.flush output)))
        done! (fn [result]
                (try (when done-event (emit! {:type done-event :result result}))
                     (catch IOException _) (finally (.close output))))]
    (try
      (let [{cancel! :cancel!} (start! payload emit! done!)]
        (reset! cancel cancel!)
        (when (and @closed? cancel!) (cancel!))
        {:status 200 :headers {"Content-Type" "text/event-stream; charset=utf-8"
                               "Cache-Control" "no-cache, no-transform" "X-Accel-Buffering" "no"}
         :body (->EventStreamBody input)})
      (catch Exception error (.close input) (.close output) (throw error))))))

(def operations
  {"/api/command" :command "/api/definition" :definition "/api/rig" :rig "/api/aim-path" :aim-path
   "/api/composition" :composition "/api/composition/run" :composition-run
   "/api/composition/stop" :composition-stop "/api/import" :import "/api/auto" :auto
   "/api/response" :response "/api/response/release" :response-release
   "/api/response/delete" :response-delete "/api/page/generate" :page-generate "/api/settings" :settings
   "/api/modulation/start" :modulation-start "/api/modulation/stop" :modulation-stop
   "/api/modulation/hold" :modulation-hold "/api/modulation/resume" :modulation-resume
   "/api/modulation/cue" :modulation-cue "/api/waveforms/publish" :waveform
   "/api/patch/save" :patch-save "/api/patch/enabled" :patch-enabled})

(defn- asset [uri]
  (when (and (not (re-find #"(?:^|/)\.\.(?:/|$)|%" uri))
             (or (#{"/" "/push-display" "/repl"} uri) (str/starts-with? uri "/studio/") (str/starts-with? uri "/gpu/")))
    (when-let [resource (response/resource-response (if (#{"/" "/push-display" "/repl"} uri) "studio/index.html" (subs uri 1)))]
      (cond-> (response/header resource "Cache-Control" "no-cache")
        (#{"/" "/push-display" "/repl"} uri) (response/content-type "text/html; charset=utf-8")))))

(defn- routes [{:keys [snapshot! definition! export! submit! heartbeat! provider-get! provider-put! chat!
                      chat-history! chat-select! fixture! audio! sync! waveform! expressive! patch! repl! repl-eval! workspace! terminal! sensor! app-agent!]}]
  (fn [{:keys [uri request-method json-params query-params] :as request}]
    (case request-method
      :get
      (if (and workspace! (str/starts-with? uri "/api/workspaces"))
        (let [operation (get {"/api/workspaces" :list "/api/workspaces/status" :status
                              "/api/workspaces/tree" :tree "/api/workspaces/file" :file
                              "/api/workspaces/diff" :diff} uri)]
          (if operation (json-response 200 (workspace! operation (into {} (map (fn [[k v]] [(keyword k) v])) query-params)))
              (json-response 404 {:message "Workspace route not found"})))
      (case uri
        "/api/application-agents" (if app-agent! (json-response 200 (app-agent! :list {}))
                                       (json-response 503 {:message "Application agents unavailable"}))
        "/api/application-agents/status" (if app-agent! (json-response 200 (app-agent! :status {:id (get query-params "id")}))
                                              (json-response 503 {:message "Application agents unavailable"}))
        "/api/application-agents/events" (if app-agent! (json-response 200 (app-agent! :events {:id (get query-params "id")
                                                                                              :cursor (get query-params "cursor")}))
                                              (json-response 503 {:message "Application agents unavailable"}))
        "/api/state" (do (heartbeat! (get query-params "session")) (json-response 200 (snapshot! query-params)))
        "/api/settings" (json-response 200 (:settings (snapshot! {})))
        "/api/definition" (if-let [definition (definition! (keyword (get query-params "kind" "effects"))
                                                            (keyword (get query-params "id" "")))]
                            (json-response 200 {:source (pr-str definition)})
                            (json-response 404 {:message "Definition not found"}))
        "/api/export" {:status 200 :headers {"Content-Type" "application/edn"
                                            "Content-Disposition" "attachment; filename=tolglow-show.edn"}
                       :body (pr-str (export!))}
        "/api/providers" (if provider-get! (json-response 200 (provider-get!))
                             (json-response 503 {:message "Provider service unavailable"}))
        "/api/chat/history" (if chat-history! (json-response 200 (chat-history!))
                                 (json-response 503 {:message "Chat history unavailable"}))
        "/api/fixtures" (if fixture! (json-response 200 (fixture! :snapshot {}))
                             (json-response 503 {:message "Fixture library unavailable"}))
        "/api/audio" (if audio! (json-response 200 (audio! :status {}))
                          (json-response 503 {:message "Audio service unavailable"}))
        "/api/audio/inputs" (if audio! (json-response 200 (audio! :inputs {}))
                                 (json-response 503 {:message "Audio service unavailable"}))
        "/api/sync" (if sync! (json-response 200 (sync! :status {}))
                         (json-response 503 {:message "Synchronization unavailable"}))
        "/api/sync/ports" (if sync! (json-response 200 (sync! :ports {}))
                               (json-response 503 {:message "Synchronization unavailable"}))
        "/api/waveforms" (if waveform! (json-response 200 (waveform! :registry {}))
                              (json-response 503 {:message "Waveform registry unavailable"}))
        "/api/patch" (if patch! (json-response 200 (patch!))
                          (json-response 503 {:message "Routing unavailable"}))
        "/api/repl" (if repl! (json-response 200 (repl! :snapshot {:session-id (get query-params "session-id" "main")}))
                         (json-response 503 {:message "REPL unavailable"}))
        "/api/repl/complete" (if repl! (json-response 200 (repl! :complete {:session-id (get query-params "session-id" "main")
                                                                          :prefix (get query-params "prefix" "")}))
                                  (json-response 503 {:message "REPL unavailable"}))
        "/api/repl/doc" (if repl! (json-response 200 (repl! :doc {:session-id (get query-params "session-id" "main")
                                                                :symbol (get query-params "symbol" "")}))
                             (json-response 503 {:message "REPL unavailable"}))
        "/api/terminal" (if terminal!
                          (json-response 200 (terminal! (if (get query-params "session-id") :read :status)
                                                       {:session-id (get query-params "session-id") :cursor (get query-params "cursor")}))
                          (json-response 503 {:message "Docker terminal unavailable"}))
        "/api/sensors" (if sensor! (json-response 200 (sensor! :status {}))
                            (json-response 503 {:message "Sensor inputs unavailable"}))
        (or (asset uri) (json-response 404 {:message "Route not found"}))))
      :post
      (cond
        (str/starts-with? uri "/api/application-agents/")
        (if-let [operation (get {"/api/application-agents/configure" :configure
                                "/api/application-agents/start" :start
                                "/api/application-agents/cancel" :cancel} uri)]
          (if app-agent! (json-response 200 (app-agent! operation json-params))
              (json-response 503 {:message "Application agents unavailable"}))
          (json-response 404 {:message "Application-agent route not found"}))
        (and sensor! (str/starts-with? uri "/api/sensors/"))
        (if-let [operation (get {"/api/sensors/pair" :pair "/api/sensors/configure" :configure
                                "/api/sensors/sample" :sample "/api/sensors/arm" :arm
                                "/api/sensors/calibrate" :calibrate "/api/sensors/osc/start" :osc-start
                                "/api/sensors/osc/stop" :osc-stop} uri)]
          (let [result (sensor! operation json-params)]
            (json-response (if (= :rejected (:status result)) 422 200) result))
          (json-response 404 {:message "Sensor route not found"}))
        (and terminal! (str/starts-with? uri "/api/terminal/"))
        (if-let [operation (get {"/api/terminal/open" :open "/api/terminal/input" :input
                                "/api/terminal/resize" :resize "/api/terminal/close" :close} uri)]
          (json-response 200 (terminal! operation json-params))
          (json-response 404 {:message "Terminal route not found"}))
        (and workspace! (str/starts-with? uri "/api/workspaces/"))
        (if-let [operation (get {"/api/workspaces/create" :create "/api/workspaces/write" :write
                                "/api/workspaces/check" :check "/api/workspaces/stage" :stage} uri)]
          (json-response 200 (workspace! operation json-params))
          (json-response 404 {:message "Workspace route not found"}))
        (= uri "/api/repl/eval") (if repl-eval! (event-stream repl-eval! json-params {})
                                      (json-response 503 {:message "REPL unavailable"}))
        (and repl! (#{"/api/repl/session" "/api/repl/scratch" "/api/repl/interrupt"} uri))
        (json-response 200 (repl! (keyword (subs uri 10)) json-params))
        (= uri "/api/expressive/preview")
        (if expressive! (json-response 200 (expressive! json-params))
            (json-response 503 {:message "Expressive mapping preview unavailable"}))
        (and waveform! (= uri "/api/waveforms/preview"))
        (json-response 200 (waveform! :preview json-params))
        (= uri "/api/providers") (if provider-put! (json-response 200 (provider-put! json-params))
                                      (json-response 503 {:message "Provider service unavailable"}))
        (= uri "/api/chat") (if chat! (event-stream chat! json-params)
                               (json-response 503 {:message "Provider service unavailable"}))
        (= uri "/api/chat/select") (if chat-select! (json-response 200 (chat-select! (:conversation-id json-params)))
                                       (json-response 503 {:message "Chat history unavailable"}))
        (and fixture! (str/starts-with? uri "/api/fixtures/"))
        (let [operation (keyword (subs uri 14))]
          (if (#{:search :refresh :fetch :import :publish :patch} operation)
            (let [result (fixture! operation json-params)]
              (json-response (if (= :rejected (:status result)) 422 200) result))
            (json-response 404 {:message "Fixture route not found"})))
        (and audio! (#{"/api/audio/start" "/api/audio/stop"} uri))
        (json-response 200 (audio! (if (= uri "/api/audio/start") :start :stop) json-params))
        (and sync! (= uri "/api/live")) (json-response 200 (sync! :live json-params))
        (and sync! (str/starts-with? uri "/api/sync/"))
        (let [operation (keyword (subs uri 10))]
          (if (#{:configure :start :stop :source :handoff} operation)
            (json-response 200 (sync! operation json-params))
            (json-response 404 {:message "Synchronization route not found"})))
        (operations uri)
        (let [type (operations uri)
              _ (heartbeat! (get-in json-params [:source :session-id]))
              payload (cond-> json-params
                        (and (= type :command) (= :page/put (:op json-params))
                             (vector? (get-in json-params [:page :cells])))
                        (update-in [:page :cells] #(into {} (map (fn [{:keys [x y cue-id]}] [[x y] cue-id])) %))
                        (= :definition type) (assoc :definition (read-edn (:source json-params))))
              receipt (deref (submit! type payload) 5000 {:status :pending :message "Command remains queued"})]
          (json-response (if (= :rejected (:status receipt)) 422 200) receipt))
        :else (json-response 404 {:message "Route not found"}))
      (json-response 405 {:message "Method not allowed"}))))

(defn app
  "Construct a Ring handler from owned application services. No server starts."
  [services]
  (-> (routes services)
      wrap-json-body
      wrap-same-origin
      wrap-params
      (wrap-content-type {:mime-types {"wasm" "application/wasm" "js" "text/javascript"}})
      wrap-head
      wrap-errors
      wrap-isolation))
