(ns tolglow.bench-subscribers
  "Actual Jetty/WebSocket subscriber cost alongside oracle-checked show ticks."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [ring.adapter.jetty :as jetty]
            [tolglow.bench-show :as bench]
            [tolglow.studio :as studio]
            [tolglow.transit :as transit])
  (:import [java.lang.management ManagementFactory]
           [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent ConcurrentHashMap Executors ThreadFactory TimeUnit]
           [java.util.concurrent.atomic AtomicLong]
           [java.util.concurrent.locks LockSupport]))

(defn- options [args]
  (when (odd? (count args)) (throw (ex-info "Every option requires a value" {})))
  (let [result (reduce (fn [m [k v]]
                         (case k
                           "--heads" (assoc m :heads (mapv parse-long (str/split v #",")))
                           "--clients" (assoc m :clients (mapv parse-long (str/split v #",")))
                           "--movers" (assoc m :movers (parse-long v))
                           "--workers" (assoc m :workers (parse-long v))
                           "--warmup" (assoc m :warmup (parse-long v))
                           "--samples" (assoc m :samples (parse-long v))
                           "--period-ms" (assoc m :period-ms (parse-long v))
                           "--output" (assoc m :output v)
                           (throw (ex-info "Unknown option" {:option k}))))
                       {:heads [1000 10000] :clients [0 1 4] :movers 8 :workers 2
                        :warmup 40 :samples 120 :period-ms 25} (partition 2 args))]
    (when-not (and (seq (:heads result)) (every? pos-int? (:heads result))
                   (seq (:clients result)) (every? #(and (nat-int? %) (<= % 16)) (:clients result))
                   (nat-int? (:movers result)) (pos-int? (:workers result)) (<= (:workers result) 16)
                   (pos-int? (:warmup result)) (pos-int? (:samples result))
                   (pos-int? (:period-ms result)) (<= (:period-ms result) 1000))
      (throw (ex-info "Invalid subscriber benchmark counts" {:options result})))
    result))

(defn- apply-message [before message]
  (when-not (= "tolglow-state/2" (:protocol message))
    (throw (ex-info "Unexpected stream protocol" {})))
  (case (:kind message)
    :full (:view message)
    :patch
    (do (when-not (= (:token before) (:base-token message))
          (throw (ex-info "Patch does not reference the receiver's applied baseline" {})))
        (reduce (fn [view [op path value]]
                  (case op
                    :set (if (empty? path) value (assoc-in view path value))
                    :remove (update-in view (vec (butlast path)) dissoc (peek path))))
                (:view before) (:changes message)))
    (throw (ex-info "Unexpected state message kind" {:kind (:kind message)}))))

(defn- acknowledge! [^WebSocket socket token]
  (.sendText socket (transit/encode {:op :ack :token token}) true))

(defn- connect-client [^HttpClient http port name fast? ^ConcurrentHashMap commits]
  (let [state (atom {:events []}) error (atom nil) ready (promise)
        auto-ack? (atom fast?) buffer (StringBuilder.) started (System/nanoTime)
        socket (.get (.buildAsync (.newWebSocketBuilder http)
                                  (URI. (str "ws://127.0.0.1:" port "/api/state/stream?session=" name))
                                  (reify WebSocket$Listener
                                    (onOpen [_ socket] (.request socket 1))
                                    (onText [_ socket chunk last?]
                                      (try
                                        (.append buffer ^CharSequence chunk)
                                        (if-not last?
                                          (do (.request socket 1) nil)
                                          (let [received (System/nanoTime)
                                                text (.toString buffer)
                                                message (transit/decode text)
                                                before @state
                                                view (apply-message before message)
                                                applied (System/nanoTime)
                                                frame (long (or (:frame-number view) 0))
                                                committed (.get commits frame)
                                                event {:frame frame :kind (:kind message)
                                                       :bytes (alength (.getBytes text StandardCharsets/UTF_8))
                                                       :received-nanos received :applied-nanos applied
                                                       :decode-apply-ms (/ (- applied received) 1e6)
                                                       :commit-to-apply-ms (when committed (/ (- applied (long committed)) 1e6))}]
                                            (when (and (:view before) (< frame (long (or (:frame-number (:view before)) 0))))
                                              (throw (ex-info "Subscriber frame order regressed" {})))
                                            (.setLength buffer 0)
                                            (swap! state #(-> % (assoc :view view :token (:token message))
                                                             (update :events conj event)))
                                            (deliver ready {:hydration-ms (/ (- applied started) 1e6) :bytes (:bytes event)})
                                            (.request socket 1)
                                            (when @auto-ack? (acknowledge! socket (:token message)))))
                                        (catch Exception failure
                                          (reset! error failure) (deliver ready failure) (.abort socket) nil)))
                                    (onError [_ _ failure]
                                      (reset! error failure) (deliver ready failure))))
                     10 TimeUnit/SECONDS)
        initial (deref ready 10000 ::timeout)]
    (when (or (= ::timeout initial) (instance? Throwable initial))
      (.abort socket)
      (throw (ex-info "Subscriber did not hydrate" {:client name} (when (instance? Throwable initial) initial))))
    {:name name :socket socket :state state :error error :auto-ack? auto-ack? :initial initial}))

(defn- await-frame! [client frame]
  (let [deadline (+ (System/nanoTime) 10000000000)]
    (loop []
      (when-let [error @(:error client)] (throw (ex-info "Subscriber failed" {:client (:name client)} error)))
      (when (< (long (or (get-in @(:state client) [:view :frame-number]) 0)) (long frame))
        (when (> (System/nanoTime) deadline)
          (throw (ex-info "Subscriber did not catch up" {:client (:name client) :frame frame})))
        (LockSupport/parkNanos 1000000)
        (recur)))))

(defn- allocation-snapshot []
  (let [bean (ManagementFactory/getThreadMXBean)]
    (when (and (instance? com.sun.management.ThreadMXBean bean)
               (.isThreadAllocatedMemorySupported ^com.sun.management.ThreadMXBean bean)
               (.isThreadAllocatedMemoryEnabled ^com.sun.management.ThreadMXBean bean))
      (into {} (keep (fn [id]
                      (let [bytes (.getThreadAllocatedBytes ^com.sun.management.ThreadMXBean bean (long id))]
                        (when (not (neg? bytes)) [id bytes])))) (.getAllThreadIds bean)))))

(defn- allocation-change [before after frames]
  (when (and before after)
    (let [bytes (reduce-kv (fn [total id value] (+ total (- value (get before id 0)))) 0 after)]
      {:bytes bytes :bytes-per-host-tick (/ (double bytes) frames)
       :threads-at-start (count before) :threads-at-end (count after)
       :unavailable-retired-threads (count (remove #(contains? after %) (keys before)))})))

(defn- summary [values] (when (seq values) (bench/distribution values)))

(defn- pace-until! [deadline]
  (loop []
    (let [remaining (- (long deadline) (System/nanoTime))]
      (when (pos? remaining)
        (LockSupport/parkNanos remaining)
        (recur)))))

(defn- client-summary [client first-frame final-frame duration-ms]
  (let [events (filterv #(<= first-frame (:frame %) final-frame) (:events @(:state client)))
        observed (count (distinct (map :frame events)))
        expected (inc (- final-frame first-frame))]
    {:name (:name client) :initial (:initial client)
     :delivered-frames observed :coalesced-frames (- expected observed)
     :delivered-frames-per-second (/ (* 1000.0 observed) duration-ms)
     :payload-bytes (summary (map :bytes events))
     :commit-to-apply-ms (summary (keep :commit-to-apply-ms events))
     :decode-apply-ms (summary (map :decode-apply-ms events))
     :delivery-interval-ms (summary (map #(/ (- (:applied-nanos %2) (:applied-nanos %1)) 1e6) events (rest events)))
     :observations events}))

(defn- run-oracle [{:keys [model preview-indices]} opts]
  (let [sink (bench/capture-sink)
        runtime (studio/create-runtime {:model model :workers 1 :batch? false :sink sink :workspace-options nil})]
    (try (mapv #(bench/host-step! runtime sink preview-indices %) (range (+ (:warmup opts) (:samples opts))))
         (finally (studio/stop! runtime)))))

(defn- preview-fields [frame]
  (update frame :preview #(mapv (fn [v] (select-keys v [:id :pan :tilt :focus :zoom :strobe
                                                       :emitted-color :intensity :aim :direction])) %)))

(defn- profile-case [{:keys [model preview-indices]} expected client-count opts]
  (let [{:keys [workers warmup samples period-ms]} opts
        sink (bench/capture-sink)
        runtime (studio/create-runtime {:model model :workers workers :sink sink :workspace-options nil})
        commits (ConcurrentHashMap.) clients (atom []) server (atom nil) http-client (atom nil)
        sequence-number (AtomicLong.)
        executor (when (pos? client-count)
                   (Executors/newFixedThreadPool (+ 2 client-count)
                     (reify ThreadFactory
                       (newThread [_ task]
                         (doto (Thread. task (str "tolglow-profile-client-" (.incrementAndGet sequence-number)))
                           (.setDaemon true))))))
        period-nanos (* (long period-ms) 1000000)
        next-tick (volatile! (System/nanoTime))
        run-tick (fn [i]
                   (pace-until! @next-tick)
                   (let [start (System/nanoTime)
                         actual (bench/host-step! runtime sink preview-indices i)
                         elapsed (/ (- (System/nanoTime) start) 1e6)]
                     (vreset! next-tick (+ start period-nanos))
                     (bench/check! (preview-fields (nth expected i)) (preview-fields actual))
                     {:frame (get-in actual [:model :frame-number]) :ms elapsed}))]
    (add-watch (:state runtime) ::commit-time
               (fn [_ _ old next]
                 (let [frame (get-in next [:model :frame-number])]
                   (when (not= frame (get-in old [:model :frame-number]))
                     (.put commits (long frame) (System/nanoTime))))))
    (try
      (when (pos? client-count)
        (reset! server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false}))
        (let [http (.build (.executor (HttpClient/newBuilder) executor))
              port (.getLocalPort (first (.getConnectors @server)))]
          (reset! http-client http)
          (doseq [i (range client-count)]
            (swap! clients conj (connect-client http port (str "profile-fast-" i) true commits)))
          (swap! clients conj (connect-client http port "profile-slow" false commits))))
      (let [fast (take client-count @clients) slow (when (pos? client-count) (last @clients))]
        (dotimes [i warmup] (run-tick i))
        (let [warm-frame (get-in @(:state runtime) [:model :frame-number])]
          (doseq [client fast] (await-frame! client warm-frame)))
        (let [gc-before (bench/gc-snapshot) allocated-before (allocation-snapshot)
              started (System/nanoTime)
              frames (mapv #(run-tick (+ warmup %)) (range samples))
              render-end (System/nanoTime) final-frame (:frame (peek frames))
              _ (doseq [client fast] (await-frame! client final-frame))
              delivery-end (System/nanoTime) allocated-after (allocation-snapshot)
              gc-after (bench/gc-snapshot)
              duration-ms (/ (- delivery-end started) 1e6)
              slow-before (when slow (count (:events @(:state slow))))
              slow-resume (when slow
                            (when-not (= 1 slow-before)
                              (throw (ex-info "Unacknowledged subscriber received extra frames" {:messages slow-before})))
                            (let [start (System/nanoTime)]
                              (reset! (:auto-ack? slow) true)
                              (.get (acknowledge! (:socket slow) (:token @(:state slow))) 5 TimeUnit/SECONDS)
                              (await-frame! slow final-frame)
                              (/ (- (System/nanoTime) start) 1e6)))
              final-view (dissoc (studio/snapshot runtime) :status)]
          (doseq [client @clients]
            (when-not (= final-view (dissoc (:view @(:state client)) :status))
              (throw (ex-info "Applied subscriber view differs from current production snapshot" {:client (:name client)}))))
          {:fast-clients client-count :withheld-ack-clients (if slow 1 0)
           :visible-output-targets (count (get-in final-view [:output :values]))
           :rig-targets (count (get-in @(:patch runtime) [:compiled :target-order]))
           :host-tick-ms (bench/distribution (map :ms frames))
           :ticks-over-period (count (filter #(> (:ms %) period-ms) frames))
           :host-ticks-per-second (/ (* 1000.0 samples) (/ (- render-end started) 1e6))
           :render-span-ms (/ (- render-end started) 1e6) :delivery-drain-ms (/ (- delivery-end render-end) 1e6)
           :allocation (allocation-change allocated-before allocated-after samples)
           :gc-before gc-before :gc-after gc-after
           :clients (mapv #(client-summary % (:frame (first frames)) final-frame duration-ms) fast)
           :slow-client (when slow {:messages-before-ack slow-before :resume-to-latest-ms slow-resume
                                   :initial (:initial slow) :latest-frame final-frame})
           :host-observations frames}))
      (finally
        (remove-watch (:state runtime) ::commit-time)
        (doseq [client @clients] (.abort ^WebSocket (:socket client)))
        (studio/stop! runtime)
        (when @server (.stop @server))
        ;; Java21's client owns a selector as well as the supplied executor.
        ;; Bound teardown even if a failed connection still owns a handshake.
        ;; Older JDKs reclaim their client on GC after sockets abort.
        (try
          (when (instance? java.lang.AutoCloseable @http-client)
            (.shutdownNow ^HttpClient @http-client)
            (when-not (.awaitTermination ^HttpClient @http-client (java.time.Duration/ofSeconds 5))
              (throw (ex-info "Owned subscriber client did not stop" {}))))
          (finally (when executor (.shutdownNow executor))))))))

(defn -main [& args]
  (try
    (let [opts (options args)
          report {:benchmark :connected-studio-subscribers :version 1 :measured-at (str (java.time.Instant/now))
                  :source-revisions {:tolglow (bench/revision ".") :afterglow (bench/revision (System/getenv "AFTERGLOW_ROOT"))}
                  :source-working-trees {:tolglow (bench/source-status ".") :afterglow (bench/source-status (System/getenv "AFTERGLOW_ROOT"))}
                  :environment {:java (System/getProperty "java.version") :clojure (clojure-version)
                                :os (System/getProperty "os.name") :architecture (System/getProperty "os.arch")
                                :processors (.availableProcessors (Runtime/getRuntime)) :cpu-model (bench/cpu-model)
                                :heap-bytes (.maxMemory (Runtime/getRuntime))
                                :jvm-arguments (vec (.getInputArguments (ManagementFactory/getRuntimeMXBean)))}
                  :options (dissoc opts :output)
                  :notes ["Actual owned loopback Jetty and JVM WebSocket clients, production Transit encoder/decoder and per-client ACKs."
                          "Every host tick matches scalar model/intents/ArtDMX bytes/preview. Every client validates base tokens and final reconstructed snapshot."
                          "ArtDMX capture only; no physical output. Browser JavaScript, UI/GPU drawing, remote networks and physical packet delivery are excluded."
                          "Production snapshot includes rig definitions but bounds live output projection; each run records visible-output-targets. This is not a full-pixel preview transfer benchmark."
                          "Host ticks are paced without catch-up bursts; host duration is separate from asynchronous commit-to-client decode/apply latency."
                          "period-ms controls wall-clock pacing only. The deterministic scalar and typed shows both advance their logical clocks by 25ms per tick."
                          "Warmup/hydration and slow-client catch-up are outside measured allocation; final fast-client drain is included."
                          "Allocation/GC span all live JVM threads and include oracle checks and client instrumentation; unlike the isolated host profiler's owner/worker allocation."
                          "A small timestamp watch observes frame commit; no projection or socket work is moved onto the frame owner."
                          "Client counters report coalesced frames; one withheld-ACK client must receive no second message until released."]
                  :results (mapv (fn [pixels]
                                   (let [data (bench/corpus pixels (:movers opts)) expected (run-oracle data opts)]
                                     {:pixels pixels :moving-heads (:movers opts)
                                      :runs (mapv (fn [clients]
                                                    (binding [*out* *err*] (println "Profiling subscribers:" pixels "pixels," clients "fast +" (if (pos? clients) 1 0) "withheld ACK"))
                                                    (profile-case data expected clients opts)) (:clients opts))})) (:heads opts))}]
      (if-let [path (:output opts)]
        (do (io/make-parents path)
            (with-open [writer (io/writer path)]
              (binding [*out* writer pprint/*print-right-margin* 220] (pprint/pprint report)))
            (println "Wrote" path))
        (pprint/pprint report)))
    (finally (shutdown-agents))))
