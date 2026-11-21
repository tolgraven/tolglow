(ns tolglow.studio.web
  "Portable Ring application. Services are explicit callbacks; the HTTP adapter
  does not own the show clock, registry, provider credentials, or effect state."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [reitit.ring :as ring]
            [tolglow.studio.page-routes :as pages]
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
  {"/api/command" :command "/api/definition" :definition "/api/rig" :rig
   "/api/composition" :composition "/api/composition/run" :composition-run
   "/api/composition/stop" :composition-stop "/api/import" :import "/api/auto" :auto
   "/api/response" :response "/api/response/release" :response-release
   "/api/response/delete" :response-delete "/api/page/generate" :page-generate "/api/settings" :settings
   "/api/modulation/start" :modulation-start "/api/modulation/stop" :modulation-stop
   "/api/modulation/hold" :modulation-hold "/api/modulation/resume" :modulation-resume
   "/api/modulation/cue" :modulation-cue "/api/waveforms/publish" :waveform
   "/api/patch/save" :patch-save "/api/patch/enabled" :patch-enabled})

(defn- not-found [_] (json-response 404 {:message "Route not found"}))
(defn- ok [value] (json-response 200 value))
(defn- acknowledgement [result]
  (json-response (if (= :rejected (:status result)) 422 200) result))

(defn- available
  "Install an optional service at router construction. Some endpoints are
  available only when their service is configured."
  [service unavailable-message handler]
  (cond service handler
        unavailable-message (constantly (json-response 503 {:message unavailable-message}))
        :else not-found))

(defn- submit-handler [submit! heartbeat! operation]
  (fn [{:keys [json-params]}]
    (heartbeat! (get-in json-params [:source :session-id]))
    (let [payload (cond-> json-params
                    (and (= operation :command) (= :page/put (:op json-params))
                         (vector? (get-in json-params [:page :cells])))
                    (update-in [:page :cells] #(into {} (map (fn [{:keys [x y cue-id]}] [[x y] cue-id])) %))
                    (= operation :definition) (assoc :definition (read-edn (:source json-params))))
          receipt (deref (submit! operation payload) 5000 {:status :pending :message "Command remains queued"})]
      (acknowledgement receipt))))

(defn- query-fields [request defaults]
  (reduce-kv (fn [result k v] (assoc result k (get (:query-params request) (name k) v)))
             {} defaults))

(defn- workspace-query [{:keys [query-params]}]
  (into {} (map (fn [[k v]] [(keyword k) v])) query-params))

(defn- read-action
  ([service operation message] (read-action service operation message (constantly {})))
  ([service operation message params]
   (available service message (fn [request] (ok (service operation (params request)))))))

(defn- get-routes
  [{:keys [snapshot! definition! export! heartbeat! provider-get! chat-history! patch!
           app-agent! fixture! audio! sync! waveform! workspace! terminal! sensor! repl!]}]
  (merge
   {"/api/state" (fn [{:keys [query-params]}]
                   (heartbeat! (get query-params "session"))
                   (ok (snapshot! query-params)))
    "/api/settings" (fn [_] (ok (:settings (snapshot! {}))))
    "/api/definition" (fn [{:keys [query-params]}]
                        (if-let [definition (definition! (keyword (get query-params "kind" "effects"))
                                                         (keyword (get query-params "id" "")))]
                          (ok {:source (pr-str definition)})
                          (json-response 404 {:message "Definition not found"})))
    "/api/export" (fn [_] {:status 200 :headers {"Content-Type" "application/edn"
                                                 "Content-Disposition" "attachment; filename=tolglow-show.edn"}
                           :body (pr-str (export!))})
    "/api/providers" (available provider-get! "Provider service unavailable" (fn [_] (ok (provider-get!))))
    "/api/chat/history" (available chat-history! "Chat history unavailable" (fn [_] (ok (chat-history!))))
    "/api/patch" (available patch! "Routing unavailable" (fn [_] (ok (patch!))))
    "/api/application-agents" (read-action app-agent! :list "Application agents unavailable")
    "/api/application-agents/status" (read-action app-agent! :status "Application agents unavailable"
                                                 #(query-fields % {:id nil}))
    "/api/application-agents/events" (read-action app-agent! :events "Application agents unavailable"
                                                 #(query-fields % {:id nil :cursor nil}))
    "/api/fixtures" (read-action fixture! :snapshot "Fixture library unavailable")
    "/api/audio" (read-action audio! :status "Audio service unavailable")
    "/api/audio/inputs" (read-action audio! :inputs "Audio service unavailable")
    "/api/sync" (read-action sync! :status "Synchronization unavailable")
    "/api/sync/ports" (read-action sync! :ports "Synchronization unavailable")
    "/api/waveforms" (read-action waveform! :registry "Waveform registry unavailable")
    "/api/sensors" (read-action sensor! :status "Sensor inputs unavailable")
    "/api/repl" (read-action repl! :snapshot "REPL unavailable" #(query-fields % {:session-id "main"}))
    "/api/repl/complete" (read-action repl! :complete "REPL unavailable" #(query-fields % {:session-id "main" :prefix ""}))
    "/api/repl/doc" (read-action repl! :doc "REPL unavailable" #(query-fields % {:session-id "main" :symbol ""}))
    "/api/terminal" (available terminal! "Docker terminal unavailable"
                               (fn [request]
                                 (let [args (query-fields request {:session-id nil :cursor nil})]
                                   (ok (terminal! (if (:session-id args) :read :status) args)))))}
   (into {} (for [[path op] {"/api/workspaces" :list "/api/workspaces/status" :status
                             "/api/workspaces/tree" :tree "/api/workspaces/file" :file "/api/workspaces/diff" :diff}]
              [path (read-action workspace! op nil workspace-query)]))))

(defn- post-actions
  "Each registered action gets its own literal route. Construct handlers once,
  including the chosen acknowledgement format and unavailable response."
  [prefix service respond operation-paths & [unavailable-message]]
  (into {} (for [[suffix operation] operation-paths]
             [(str prefix suffix)
              (available service unavailable-message #(respond (service operation (:json-params %))))])))

(defn- post-routes [{:keys [submit! heartbeat! provider-put! chat! chat-select! app-agent!
                             sensor! terminal! workspace! repl! repl-eval! waveform! expressive! fixture! audio! sync!]}]
  (merge
   (into {} (for [[path op] operations] [path (submit-handler submit! heartbeat! op)]))
   (post-actions "/api/application-agents/" app-agent! ok
                 {"configure" :configure "start" :start "cancel" :cancel} "Application agents unavailable")
   (post-actions "/api/sensors/" sensor! acknowledgement
                 {"pair" :pair "configure" :configure "sample" :sample "arm" :arm "calibrate" :calibrate
                  "osc/start" :osc-start "osc/stop" :osc-stop})
   (post-actions "/api/terminal/" terminal! ok {"open" :open "input" :input "resize" :resize "close" :close})
   (post-actions "/api/workspaces/" workspace! ok {"create" :create "write" :write "check" :check "stage" :stage})
   (post-actions "/api/repl/" repl! ok {"session" :session "scratch" :scratch "interrupt" :interrupt})
   (post-actions "/api/waveforms/" waveform! ok {"preview" :preview})
   (post-actions "/api/fixtures/" fixture! acknowledgement
                 {"search" :search "refresh" :refresh "fetch" :fetch "import" :import "publish" :publish "patch" :patch})
   (post-actions "/api/audio/" audio! ok {"start" :start "stop" :stop})
   (post-actions "/api/sync/" sync! ok {"configure" :configure "start" :start "stop" :stop "source" :source "handoff" :handoff})
   (post-actions "/api/" sync! ok {"live" :live})
   {"/api/repl/eval" (available repl-eval! "REPL unavailable" #(event-stream repl-eval! (:json-params %) {}))
    "/api/expressive/preview" (available expressive! "Expressive mapping preview unavailable" #(ok (expressive! (:json-params %))))
    "/api/providers" (available provider-put! "Provider service unavailable" #(ok (provider-put! (:json-params %))))
    "/api/chat" (available chat! "Provider service unavailable" #(event-stream chat! (:json-params %)))
    "/api/chat/select" (available chat-select! "Chat history unavailable" #(ok (chat-select! (get-in % [:json-params :conversation-id]))))}))

(defn- studio-page [_]
  (-> (response/resource-response "studio/index.html")
      (response/header "Cache-Control" "no-cache")
      (response/content-type "text/html; charset=utf-8")))

(defn- asset [{:keys [uri] :as request}]
  (or (when-not (re-find #"(?:^|/)\.\.(?:/|$)|%" uri)
        (some-> (response/resource-response (subs uri 1) {:index-files? false})
                (response/header "Cache-Control" "no-cache")))
      (not-found request)))

(defn- read-methods [handler] {:get handler :head (wrap-head handler)})

(defn- method-not-allowed [request]
  (let [allowed (->> (:result (ring/get-match request)) (keep (fn [[method endpoint]] (when endpoint (str/upper-case (name method))))))]
    (response/header (json-response 405 {:message "Method not allowed"}) "Allow" (str/join ", " (sort allowed)))))

(defn- routes [services]
  (let [gets (into {} (map (fn [[path handler]] [path (read-methods handler)])) (get-routes services))
        posts (into {} (map (fn [[path handler]] [path {:post (wrap-json-body handler)}])) (post-routes services))
        stream (:state-stream! services)
        route-table (into (mapv (fn [[path data]] [path (merge data (read-methods studio-page))]) pages/routes)
                          (concat [["/studio/*path" (read-methods asset)] ["/gpu/*path" (read-methods asset)]
                                   ["/api/state/stream" {:get (available stream "State stream unavailable" stream)}]]
                                  (merge-with merge gets posts)))]
    (ring/ring-handler
     (ring/router route-table {::ring/default-options-endpoint nil})
     (ring/create-default-handler {:not-found not-found :method-not-allowed method-not-allowed}))))

(defn app
  "Construct a Ring handler from owned application services. No server starts."
  [services]
  (-> (routes services)
      wrap-same-origin
      wrap-params
      (wrap-content-type {:mime-types {"wasm" "application/wasm" "js" "text/javascript"}})
      wrap-errors
      wrap-isolation))
