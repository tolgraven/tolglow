(ns tolglow.studio.display
  "Owned offscreen re-frame display boundary. render! only replaces a latest-view
  mailbox; browser capture, PNG decoding and native output run on one worker."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [afterglow.control.display :as presentation]
            [afterglow.control.devices.push-3.display :as push-3]
            [afterglow.control.devices.transports :as transports])
  (:import [java.awt.image BufferedImage]
           [java.lang ProcessHandle]
           [java.io BufferedReader BufferedWriter ByteArrayInputStream File InputStreamReader OutputStreamWriter]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.util Base64]
           [java.util.concurrent ArrayBlockingQueue Callable ExecutionException Executors ExecutorService Future ThreadFactory TimeUnit TimeoutException]
           [javax.imageio ImageIO]))

(def width 960)
(def height 160)

(defn- thread-factory ^ThreadFactory [^String name thread-ref]
  (reify ThreadFactory
    (newThread [_ runnable]
      (let [thread (doto (Thread. runnable name) (.setDaemon true))]
        (reset! thread-ref thread)
        thread))))

(defn- positive-ms! [value field]
  (when-not (and (integer? value) (<= 10 value 60000))
    (throw (ex-info "Display deadline must be an integer in 10..60000 milliseconds" {:field field})))
  value)

(defn- destroy-process! [^Process process timeout-ms]
  (let [descendants (with-open [stream (.descendants (.toHandle process))] (vec (.toArray stream)))
        interrupted? (atom (Thread/interrupted))
        wait! #(try (.waitFor process timeout-ms TimeUnit/MILLISECONDS)
                    (catch InterruptedException _ (reset! interrupted? true) false))]
    (try
      (doseq [^ProcessHandle child descendants] (.destroy child))
      (.destroy process)
      (when-not (wait!)
        (.destroyForcibly process)
        (wait!))
      ;; A Node parent may exit before a browser descendant obeys TERM. The
      ;; parent's successful wait must not release our ownership of that child.
      (doseq [^ProcessHandle child descendants] (when (.isAlive child) (.destroyForcibly child)))
      (finally (when @interrupted? (.interrupt (Thread/currentThread)))))))

(defn- remove-script! [{:keys [^File script-file temporary-script?]}]
  (when temporary-script? (Files/deleteIfExists (.toPath script-file))))

(defn- worker-file! [worker-script]
  (let [resource (when-not worker-script (io/resource "devices/push-display/worker.cjs"))]
    (if (or worker-script (= "file" (some-> resource .getProtocol)))
      (let [file (io/file (or worker-script resource))]
        (when-not (.isFile file)
          (throw (ex-info "Display worker script is not available as a local file" {})))
        {:script-file (.getAbsoluteFile file) :temporary-script? false})
      (do
        (when-not resource (throw (ex-info "Packaged display worker script is unavailable" {})))
        (let [file (.toFile (Files/createTempFile "tolglow-display-" ".cjs"
                                                (make-array java.nio.file.attribute.FileAttribute 0)))]
          (try
            (with-open [input (io/input-stream resource) output (io/output-stream file)]
              (io/copy input output))
            {:script-file file :temporary-script? true}
            (catch Throwable error (Files/deleteIfExists (.toPath file)) (throw error))))))))

(defn- read-response! [^BufferedReader reader]
  (let [line (.readLine reader)]
    (when-not line (throw (ex-info "Display worker closed its response stream" {})))
    (when (> (count line) (* 3 1024 1024)) (throw (ex-info "Display response exceeds its bound" {})))
    (json/read-str line :key-fn keyword)))

(defn- await! [^ExecutorService executor f timeout-ms]
  (let [^Future task (.submit executor ^Callable (reify Callable (call [_] (f))))]
    (try (.get task timeout-ms TimeUnit/MILLISECONDS)
         (catch ExecutionException error (throw (.getCause error)))
         (catch TimeoutException cause
           (.cancel task true)
           (throw (ex-info "Offscreen display deadline exceeded" {:type ::deadline :timeout-ms timeout-ms} cause))))))

(defn- close-process! [{:keys [lock closed? process executor shutdown-timeout-ms] :as handle}]
  (locking lock
    (when-not @closed?
      (reset! closed? true)
      (try (when process (destroy-process! process shutdown-timeout-ms))
           (finally
             (when executor (.shutdownNow ^ExecutorService executor))
             (remove-script! handle))))))

(defn- startup-error [error {:keys [^Process process ipc-thread opened-at]}]
  (if-not (instance? clojure.lang.ExceptionInfo error) error
    (let [^Thread thread @ipc-thread]
      (ex-info (.getMessage ^Throwable error)
               (merge (ex-data error)
                      {:stage :startup :elapsed-ms (/ (- (System/nanoTime) opened-at) 1.0e6)
                       :process-started? (some? process) :process-id (when process (.pid process))
                       :process-alive? (boolean (and process (.isAlive process)))
                       :exit-code (when (and process (not (.isAlive process))) (.exitValue process))
                       :ipc-thread-state (some-> thread .getState str)
                       :ipc-stack (when thread (mapv str (take 6 (.getStackTrace thread))))
                       :stderr :inherited}) error))))

(defn frame-request
  "Encode a passive capture request without collapsing :type and :semantic/type."
  [id view]
  (json/write-str {:id id :view view}
                  :key-fn #(if (keyword? %) (subs (str %) 1) (str %))))

(defn process-capture
  "Construct an inert Node/Chromium capture backend. :browser-executable is
  required. :url defaults to the studio's loopback /push-display page. The bundled
  worker is extracted lazily when inside a JAR and removed with its process.
  :worker-script overrides the resource without transferring file ownership.
  Configure :node-executable and optionally an installed :playwright-module.
  For authenticated Studio, :auth-token-env names its operator token variable.
  No resource lookup, extraction, browser download or process occurs before :open!."
  [{:keys [url browser-executable node-executable worker-script playwright-module no-sandbox? auth-token-env
           startup-timeout-ms capture-timeout-ms scene-startup-timeout-ms scene-capture-timeout-ms shutdown-timeout-ms]
    :or {url "http://127.0.0.1:8500/push-display" node-executable "node"
         startup-timeout-ms 10000 capture-timeout-ms 2000 scene-startup-timeout-ms 45000
         scene-capture-timeout-ms 10000 shutdown-timeout-ms 1000}}]
  (doseq [[field value] [[:startup-timeout-ms startup-timeout-ms] [:capture-timeout-ms capture-timeout-ms]
                         [:scene-startup-timeout-ms scene-startup-timeout-ms] [:scene-capture-timeout-ms scene-capture-timeout-ms]
                         [:shutdown-timeout-ms shutdown-timeout-ms]]]
    (positive-ms! value field))
  (when-not (and (string? browser-executable) (seq browser-executable))
    (throw (ex-info "Configure an installed Chromium executable explicitly" {})))
  (let [config (cond-> {:url url :executablePath browser-executable
                        :startupTimeoutMs startup-timeout-ms :captureTimeoutMs capture-timeout-ms
                        :sceneStartupTimeoutMs scene-startup-timeout-ms :sceneCaptureTimeoutMs scene-capture-timeout-ms
                        :noSandbox (boolean no-sandbox?)}
                 playwright-module (assoc :playwrightModule playwright-module)
                 auth-token-env (assoc :authTokenEnv auth-token-env))]
    {:open!
     (fn []
       (let [owned (atom (merge (worker-file! worker-script)
                               {:lock (Object.) :request-lock (Object.) :closed? (atom false)
                                :ipc-thread (atom nil) :scene-mode? (volatile! false) :opened-at (System/nanoTime)
                                :shutdown-timeout-ms shutdown-timeout-ms}))]
         (try
           (let [process (.start (doto (ProcessBuilder. ^java.util.List
                                         [node-executable (str (:script-file @owned)) (json/write-str config)])
                                  (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)))
                 _ (swap! owned assoc :process process)
                 executor (Executors/newSingleThreadExecutor (thread-factory "tolglow-display-ipc" (:ipc-thread @owned)))
                 _ (swap! owned assoc :executor executor)
                 reader (BufferedReader. (InputStreamReader. (.getInputStream process) StandardCharsets/UTF_8))
                 writer (BufferedWriter. (OutputStreamWriter. (.getOutputStream process) StandardCharsets/UTF_8))
                 _ (swap! owned assoc :reader reader :writer writer :counter (atom 0))
                 ready (await! executor #(read-response! reader) startup-timeout-ms)]
             (when-not (and (= "ready" (:status ready)) (= width (:width ready)) (= height (:height ready)))
               (throw (ex-info "Display worker did not become ready" {:response ready})))
             @owned)
           (catch Throwable error
             (let [diagnostic (startup-error error @owned)]
               (close-process! @owned)
               (throw diagnostic))))))
     :capture!
     (fn [{:keys [request-lock closed? reader writer executor counter scene-mode?] :as handle} view]
       (locking request-lock
         (when @closed? (throw (ex-info "Display capture is closed" {})))
         (let [id (inc @counter) request (frame-request id view)
               scene? (boolean (and (:scene view) (contains? #{:simulator :rig "simulator" "rig"} (:display-mode view))))
               budget (cond (not scene?) capture-timeout-ms
                            @scene-mode? scene-capture-timeout-ms
                            :else (max scene-startup-timeout-ms scene-capture-timeout-ms))]
           (when (> (alength (.getBytes ^String request StandardCharsets/UTF_8)) 70000)
             (throw (ex-info "Display request exceeds its bound" {})))
           (reset! counter id)
           (try
             (let [response (await! executor #(do (.write ^BufferedWriter writer request)
                                                  (.newLine ^BufferedWriter writer) (.flush ^BufferedWriter writer)
                                                  (read-response! reader)) budget)]
               (vreset! scene-mode? scene?)
               (when-not (and (= id (:id response)) (= "captured" (:status response))
                              (= width (:width response)) (= height (:height response)) (string? (:png response)))
                 (throw (ex-info "Invalid offscreen display response" {:response (dissoc response :png)})))
               (let [bytes (.decode (Base64/getDecoder) ^String (:png response))
                     image (ImageIO/read (ByteArrayInputStream. bytes))]
                 (when-not (and image (= width (.getWidth image)) (= height (.getHeight image)))
                   (throw (ex-info "Offscreen PNG dimensions do not match Push 2" {})))
                 image))
             (catch Throwable error (close-process! handle) (throw error))))))
     :close! close-process!}))

(defn blit!
  "Copy a privately owned decoded PNG into the sink's existing image. Called
  only by the display worker, never from a model update or effect render."
  [^BufferedImage target ^BufferedImage image]
  (when-not (and (instance? BufferedImage image) (= width (.getWidth image)) (= height (.getHeight image))
                 (= width (.getWidth target)) (= height (.getHeight target)))
    (throw (ex-info "Display sink and capture must both be 960x160 images" {})))
  (let [graphics (.createGraphics target)]
    (try (.drawImage graphics image 0 0 nil) target (finally (.dispose graphics)))))

(defn- change! [handle f & args]
  (locking (:lock handle) (reset! (:state handle) (apply f @(:state handle) args))))

(defn- validate-view! [view]
  (when-not (map? view) (throw (ex-info "Display view must be a portable map" {})))
  (loop [pending [view] budget 65536]
    (when (neg? budget) (throw (ex-info "Display view exceeds its structural size bound" {})))
    (when (seq pending)
      (let [item (peek pending) pending (pop pending)]
        (cond
          (record? item) (throw (ex-info "Display view cannot contain records or resources" {}))
          (map? item) (recur (into pending (mapcat identity item)) (- budget (* 2 (count item))))
          (or (vector? item) (set? item) (list? item)) (recur (into pending item) (- budget (count item)))
          (string? item) (recur pending (- budget (count item)))
          (or (keyword? item) (boolean? item) (nil? item)
              (and (number? item) (Double/isFinite (double item)))) (recur pending (dec budget))
          :else (throw (ex-info "Display view must contain immutable portable data" {:class (class item)}))))))
  view)

(defn transport
  "Build the asynchronous display session transport from explicit :capture and
  :sink maps. One capture is in flight and one latest pending view is retained.
  With :project!, the last immutable view is re-projected at the bounded cadence
  even when model/base controls are unchanged, so live readings stay current.
  Session :render! only submits a view. Failures are visible through status!;
  resources close on the worker. close! waits at most :shutdown-timeout-ms."
  [{:keys [capture sink max-fps shutdown-timeout-ms project!]
    :or {max-fps 20 shutdown-timeout-ms 1000}}]
  (when-not (and (map? capture) (every? ifn? (map capture [:open! :capture! :close!]))
                 (map? sink) (every? ifn? (map sink [:open! :render! :close!]))
                 (number? max-fps) (Double/isFinite (double max-fps)) (<= 1 max-fps 60)
                 (or (nil? project!) (ifn? project!)))
    (throw (ex-info "Display needs capture/sink operations and max-fps in 1..60" {})))
  (positive-ms! shutdown-timeout-ms :shutdown-timeout-ms)
  (let [stop-token (Object.)]
    {:open!
     (fn []
       ;; Startup belongs to explicit device connection, before live input.
       (let [capture-handle ((:open! capture))
             sink-handle (try ((:open! sink))
                              (catch Throwable error ((:close! capture) capture-handle) (throw error)))
             handle {:capture-handle capture-handle :sink-handle sink-handle :lock (Object.)
                     :queue (ArrayBlockingQueue. 1)
                     :state (atom {:status :active :submitted 0 :captured 0 :dropped 0})}
             worker (Thread.
                      (fn []
                        (try
                          (loop [last-at 0 last-entry nil]
                            (let [first-entry (if (and project! last-entry)
                                                (or (.poll ^ArrayBlockingQueue (:queue handle)) last-entry)
                                                (.take ^ArrayBlockingQueue (:queue handle)))]
                              (when-not (identical? stop-token first-entry)
                                (let [delay (- (/ 1.0e9 max-fps) (- (System/nanoTime) last-at))]
                                  (when (pos? delay) (Thread/sleep (long (/ delay 1.0e6)))))
                                (let [pending (.poll ^ArrayBlockingQueue (:queue handle))
                                      entry (or pending first-entry)]
                                  (when (and pending (not (identical? stop-token pending))
                                             (not (identical? first-entry last-entry)))
                                    (change! handle update :dropped inc))
                                  (when-not (or (identical? stop-token entry) (not= :active (:status @(:state handle))))
                                    (let [started (System/nanoTime)
                                          view (if project! (validate-view! (project! (:view entry))) (:view entry))
                                          frame ((:capture! capture) capture-handle view)]
                                      (when (= :active (:status @(:state handle)))
                                        ((:render! sink) sink-handle frame)
                                        (change! handle #(-> % (update :captured inc)
                                                            (assoc :last-sequence (:sequence entry)
                                                                   :last-render-ms (/ (- (System/nanoTime) started) 1.0e6)))))
                                      (recur started entry)))))))
                          (catch InterruptedException _)
                          (catch Throwable error (change! handle assoc :status :failed :error (.getMessage error)))
                          (finally
                            (doseq [[backend resource] [[capture capture-handle] [sink sink-handle]]]
                              (try ((:close! backend) resource)
                                   (catch Throwable error (change! handle assoc :close-error (.getMessage error)))))
                            (change! handle #(if (= :failed (:status %)) % (assoc % :status :closed))))))
                      "tolglow-push-display")]
         (.setDaemon worker true) (.start worker) (assoc handle :worker worker)))
     :render!
     (fn [handle view]
       (validate-view! view)
       (locking (:lock handle)
         (when-not (= :active (:status @(:state handle)))
           (throw (ex-info "Display worker is not active" @(:state handle))))
         (let [sequence (inc (:submitted @(:state handle)))
               dropped? (some? (.poll ^ArrayBlockingQueue (:queue handle)))]
           (reset! (:state handle) (cond-> (assoc @(:state handle) :submitted sequence)
                                    dropped? (update :dropped inc)))
           (.offer ^ArrayBlockingQueue (:queue handle) {:sequence sequence :view view})))
       nil)
     :status! #(deref (:state %))
     :close!
     (fn [handle]
       (locking (:lock handle)
         (when (= :active (:status @(:state handle)))
           (reset! (:state handle) (assoc @(:state handle) :status :closing)))
         (.clear ^ArrayBlockingQueue (:queue handle))
         (.offer ^ArrayBlockingQueue (:queue handle) stop-token))
       (.interrupt ^Thread (:worker handle))
       (.join ^Thread (:worker handle) shutdown-timeout-ms)
       (when (.isAlive ^Thread (:worker handle))
         (change! handle assoc :shutdown-timeout? true))
       nil)}))

(defn native-display
  "Build the asynchronous Push 2 display. Wayang renders the projected view
  directly into its owned image on the display worker."
  [options]
  (assoc (transport (assoc options
                           :capture {:open! (constantly nil) :close! (constantly nil)
                                     :capture! (fn [_ view] view)}
                           :sink (transports/wayang-display)))
         :device :push-2 :input :view
         :display-capabilities presentation/parameter-only-capabilities))

(defn browser-display
  "Create an offscreen re-frame display for a tethered Push. Push 2 uses Wayang.
  For Push 3, pass :device :push-3 and :usb {:bus b :address a} from enumeration.
  Both routes accept an injected image sink for capture and replay."
  [{:keys [device sink usb] :or {device :push-2} :as options}]
  (when-not (#{:push-2 :push-3} device)
    (throw (ex-info "Unknown Push display device" {:device device})))
  (let [sink (or sink (case device
                        :push-2 (transports/wayang-display {:renderer! blit!})
                        :push-3 (push-3/sink usb)))]
    (when (and (:device sink) (not= device (:device sink)))
      (throw (ex-info "Display and sink select different devices" {:device device :sink (:device sink)})))
    (assoc (transport (assoc options :capture (process-capture options) :sink sink)) :device device :input :view)))
