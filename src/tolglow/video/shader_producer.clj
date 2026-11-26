(ns tolglow.video.shader-producer
  "The independently scheduled producer behind shader cues (#673).

  The lighting frame never renders a shader. After each committed frame the
  show hands `sync!` the running shader instances with the uniforms that
  frame evaluated (`tolglow.video.shader/requests`). Each instance then runs
  on its own cadence (its :fps, for example 25 or 30 against 40 Hz lighting):
  at every slot it renders the newest committed uniforms on the GPU worker
  and, once the readback completes, publishes the frame. The next lighting
  frame latches whichever frame completed last. A slot that comes round while
  the previous render is still running is dropped and counted, never caught
  up. A slot whose program, uniforms and target are unchanged re-uses the
  published frame, so a paused instance or a stopped transport costs nothing.

  Feed references: `frames` returns {:frames {feed-id frame} :instance-frames
  {instance-id {feed-id frame}}}. A feed id resolves to the newest-launched
  running instance on that feed; a shader cue's own `{:op :video}` output
  reads its own instance's frame first.

  Compilation is off the lighting deadline, on the renderer's worker. A
  version whose GLSL does not compile leaves the instance rendering its last
  working program (with the uniforms that program declares) and reports the
  driver log. A frame belongs to its instance: when the instance ends, its
  cadence stops, its frame is dropped from the feed and the program is
  released once no other instance renders it. At most `max-instances`
  instances render at once, in launch order.

  The GPU is an external boundary: a renderer implements `ShaderRenderer`.
  `native-renderer` loads Afterglow's JOGL renderer, which needs the
  `+native-gpu` profile; without it shader cues are listed as unavailable."
  (:require [tolglow.video.source :as source])
  (:import [java.util.concurrent Future ScheduledThreadPoolExecutor ThreadFactory TimeUnit TimeoutException
            ExecutionException]))

(def max-instances
  "Shader instances rendered at once."
  8)

(def compile-timeout-ms 10000)

(def history-size
  "Per-instance timing samples kept for status and measurement."
  512)

(defprotocol ShaderRenderer
  (compile-shader [renderer shader] "A Future of {:compiled? true}; fails with the driver log.")
  (render-frame [renderer shader request] "A Future of {:pixels :width :height :draw-ms :readback-ms :gpu-ms}.")
  (release-shader [renderer shader] "A Future; frees the program.")
  (describe-renderer [renderer] "Portable driver and cache status.")
  (close-renderer [renderer] "Release the context."))

(defn native-renderer
  "Afterglow's JOGL fragment-shader renderer. Throws ex-info with
  :code :shader/no-gpu-renderer when JOGL is not on the classpath."
  ([] (native-renderer {}))
  ([options]
   (let [resolve! (fn [sym]
                    (try (requiring-resolve sym)
                         (catch Throwable e
                           (throw (ex-info "Shader cues need Afterglow's native GPU renderer (start the show with the +native-gpu profile)"
                                           {:code :shader/no-gpu-renderer} e)))))
         open! (resolve! 'afterglow.render.gpu.shader-frames/open!)
         compile! (resolve! 'afterglow.render.gpu.shader-frames/compile!)
         render! (resolve! 'afterglow.render.gpu.shader-frames/render!)
         release! (resolve! 'afterglow.render.gpu.shader-frames/release!)
         status (resolve! 'afterglow.render.gpu.shader-frames/status)
         close! (resolve! 'afterglow.render.gpu.shader-frames/close!)
         r (open! options)]
     (reify ShaderRenderer
       (compile-shader [_ shader] (compile! r shader))
       (render-frame [_ shader request] (render! r shader request))
       (release-shader [_ shader] (release! r shader))
       (describe-renderer [_] (assoc (status r) :backend :native-gpu))
       (close-renderer [_] (close! r 5000))))))

(defn- await-future [^Future f timeout-ms]
  (try (.get f (long timeout-ms) TimeUnit/MILLISECONDS)
       (catch ExecutionException e (throw (or (.getCause e) e)))
       (catch TimeoutException e
         (.cancel f false)
         (throw (ex-info "The shader renderer did not finish in time" {:code :shader/timeout :timeout-ms timeout-ms} e)))))

(defn create
  "A producer. :renderer is a ShaderRenderer or a no-argument factory
  (default `native-renderer`, opened on first use); the producer closes a
  renderer it opened. :now-ms is the wall clock for frame timestamps;
  :threads sizes the cadence pool."
  ([] (create {}))
  ([{:keys [renderer now-ms threads] :or {now-ms #(System/currentTimeMillis) threads 4}}]
   {:renderer-factory (if (satisfies? ShaderRenderer renderer) (constantly renderer) (or renderer native-renderer))
    :owns-renderer? (not (satisfies? ShaderRenderer renderer))
    :renderer (atom nil)
    :renderer-error (atom nil)
    :renderer-lock (Object.)
    :now-ms now-ms
    :requests (atom {})
    :last-offered (atom {})
    ;; instance id -> mutable cadence entry (an atom), touched by that instance's ticks.
    :entries (atom {})
    ;; instance id -> {:feed :order :frame}
    :published (atom {})
    :stopped? (atom false)
    :pool (delay (doto (ScheduledThreadPoolExecutor.
                        (int threads)
                        (reify ThreadFactory
                          (newThread [_ task] (doto (Thread. ^Runnable task "tolglow-shader-video") (.setDaemon true)))))
                   (.setRemoveOnCancelPolicy true)))}))

(def retry-after-ms
  "A renderer that failed to open is retried after this long."
  2000)

(defn- renderer! [producer]
  (or @(:renderer producer)
      (locking (:renderer-lock producer)
        (or @(:renderer producer)
            (let [failed (:at-ns @(:renderer-error producer))]
              (when (or (nil? failed) (> (- (System/nanoTime) (long failed)) (* 1000000 retry-after-ms)))
                (try
                  (let [r ((:renderer-factory producer))]
                    (reset! (:renderer-error producer) nil)
                    (reset! (:renderer producer) r))
                  (catch Exception e
                    (reset! (:renderer-error producer) {:message (ex-message e) :at-ns (System/nanoTime)
                                                        :code (:code (ex-data e) :shader/renderer-failed)})
                    nil))))))))

(defn- drop-renderer!
  "Forget a renderer whose context closed (timeout or driver loss); the next
  slot opens a new one through the factory."
  [producer r error]
  (when (compare-and-set! (:renderer producer) r nil)
    (reset! (:renderer-error producer) {:message (ex-message error)
                                        :code (:code (ex-data error) :shader/renderer-failed)})
    (when (:owns-renderer? producer)
      (try (close-renderer r) (catch Throwable _ nil)))))

(defn- push-sample [v x] (let [v (conj (or v []) x)] (if (> (count v) history-size) (subvec v (- (count v) history-size)) v)))

(defn- compatible-uniforms
  "Values for program's uniforms: the request's where the name and type still
  match, otherwise the last values that program rendered."
  [program request last-values]
  (into {}
        (map (fn [[n type]]
               [n (if (= type (get-in request [:shader :uniforms n]))
                    (get-in request [:uniforms n])
                    (get last-values n))]))
        (:uniforms program)))

(defn- shader-in-use? [producer shader except-id]
  (some (fn [[id entry]] (and (not= id except-id) (= shader (:program @entry)))) @(:entries producer)))

(declare schedule!)

(defn- retire! [producer id]
  (when-let [entry (get @(:entries producer) id)]
    (swap! (:entries producer) dissoc id)
    (swap! (:published producer) dissoc id)
    (when-let [^Future f (:future @entry)] (.cancel f false))
    (let [program (:program @entry)]
      (when (and program (not (shader-in-use? producer program id)))
        (when-let [r @(:renderer producer)]
          (try (release-shader r program) (catch Exception _ nil)))))))

(defn- ensure-program!
  "Compile the request's shader when it changed. Returns the program to render
  (the request's, or the last working one), or nil."
  [producer entry request r]
  (let [{:keys [program failed]} @entry
        wanted (:shader request)]
    (if (or (= wanted program) (= wanted failed))
      program
      (let [started (System/nanoTime)]
        (try
          (await-future (compile-shader r wanted) compile-timeout-ms)
          (swap! entry assoc :program wanted :failed nil :error nil
                 :compile-ms (/ (- (System/nanoTime) started) 1.0e6))
          (swap! entry update :compiles (fnil inc 0))
          wanted
          (catch clojure.lang.ExceptionInfo e
            (let [data (ex-data e)]
              (if (#{:compile :link} (:stage data))
                (do (swap! entry assoc :failed wanted
                           :error {:code :shader/compile-failed :stage (:stage data) :log (:log data)
                                   :message (ex-message e)})
                    (swap! entry update :compile-failures (fnil inc 0))
                    program)
                (throw e)))))))))

(defn- tick!
  "Render one slot for an instance, then schedule its next slot."
  [producer id slot]
  (when-let [entry (get @(:entries producer) id)]
    (let [request (get @(:requests producer) id)]
      (when (and request (not @(:stopped? producer)))
        (swap! entry update :slots (fnil inc 0))
        (when-let [r (renderer! producer)]
          (try
            (when-let [program (ensure-program! producer entry request r)]
              (let [uniforms (if (= program (:shader request))
                               (:uniforms request)
                               (compatible-uniforms program request (:last-uniforms @entry)))
                    target (select-keys request [:width :height :viewport])
                    input [program uniforms target]]
                (if (and (= input (:rendered-input @entry)) (get @(:published producer) id))
                  (swap! entry update :unchanged (fnil inc 0))
                  (let [started (System/nanoTime)
                        period-ms (/ 1000.0 (:fps request))
                        result (await-future (render-frame r program (assoc target :uniforms uniforms))
                                             (max 250 (long (* 4 period-ms))))
                        elapsed (/ (- (System/nanoTime) started) 1.0e6)
                        now ((:now-ms producer))
                        sequence (inc (:sequence @entry 0))
                        frame (source/frame {:source-id (:feed request) :width (:width result) :height (:height result)
                                             :pixels (:pixels result) :sequence sequence :captured-at-ms now
                                             :label (str "Shader · " (name (first (:effect request))))})]
                    ;; An instance that ended while its frame rendered publishes nothing.
                    (when (identical? entry (get @(:entries producer) id))
                      (swap! (:published producer) assoc id {:feed (:feed request) :order (:order request) :frame frame}))
                    (swap! entry (fn [e]
                                   (-> e
                                       (assoc :sequence sequence :rendered-input input :last-uniforms uniforms
                                              :last-render {:draw-ms (:draw-ms result) :readback-ms (:readback-ms result)
                                                            :gpu-ms (:gpu-ms result) :render-ms elapsed})
                                       (update :rendered (fnil inc 0))
                                       (update :render-ms push-sample elapsed)
                                       (update :draw-ms push-sample (:draw-ms result))
                                       (update :published-at push-sample now))))))))
            (catch Exception e
              (swap! entry #(-> % (update :failures (fnil inc 0))
                                (assoc :error {:code (:code (ex-data e) :shader/render-failed) :message (ex-message e)})))
              (when (#{:afterglow.render.gpu.shader-frames/closed :afterglow.render.gpu.shader-frames/timeout
                       :shader/timeout}
                     (or (:type (ex-data e)) (:code (ex-data e))))
                (drop-renderer! producer r e)))))
        (schedule! producer id entry (:fps request) slot)))))

(defn- schedule!
  "Schedule the slot after `slot` on the instance's own origin. Slots that
  passed while this one ran are counted as dropped and skipped."
  [producer id entry fps slot]
  (when (and (not @(:stopped? producer)) (identical? entry (get @(:entries producer) id)))
    (let [now (System/nanoTime)
          {:keys [origin-ns period-ns]}
          (cond
            ;; A new frame rate starts a new cadence origin.
            (not= fps (:fps @entry))
            (swap! entry assoc :fps fps :origin-ns (- now (* (long slot) (long (/ 1.0e9 fps))))
                   :period-ns (long (/ 1.0e9 fps)))
            ;; Opening the GPU context and the first compile are startup
            ;; latency, reported as :first-frame-ms, not dropped frames.
            (nil? (:first-frame-ms @entry))
            (swap! entry (fn [e] (cond-> (assoc e :origin-ns (- now (* (long slot) (long (:period-ns e)))))
                                   (:sequence e) (assoc :first-frame-ms (/ (- now (long (:admitted-ns e))) 1.0e6)))))
            :else @entry)
          due (inc (quot (- now (long origin-ns)) (long period-ns)))
          next-slot (max (inc (long slot)) due)
          missed (max 0 (- next-slot (inc (long slot))))
          delay-ns (max 0 (- (+ (long origin-ns) (* next-slot (long period-ns))) now))]
      (when (pos? missed) (swap! entry update :dropped (fnil + 0) missed))
      (try
        (swap! entry assoc :future
               (.schedule ^ScheduledThreadPoolExecutor @(:pool producer)
                          ^Runnable (fn [] (try (tick! producer id next-slot) (catch Throwable _ nil)))
                          (long delay-ns) TimeUnit/NANOSECONDS))
        (catch java.util.concurrent.RejectedExecutionException _ nil)))))

(defn- admit! [producer id request]
  (let [period-ns (long (/ 1.0e9 (:fps request)))
        entry (atom {:fps (:fps request) :origin-ns (System/nanoTime) :admitted-ns (System/nanoTime) :period-ns period-ns
                     :feed (:feed request) :effect (:effect request)})]
    (swap! (:entries producer) assoc id entry)
    (try
      (swap! entry assoc :future
             (.submit ^ScheduledThreadPoolExecutor @(:pool producer)
                      ^Runnable (fn [] (try (tick! producer id 0) (catch Throwable _ nil)))))
      (catch java.util.concurrent.RejectedExecutionException _ nil))))

(defn sync!
  "Offer the committed shader instances (`tolglow.video.shader/requests`).
  Cheap enough to call after every lighting frame: it stores the requests,
  and starts or retires cadences only when the set of instances changed.
  Never renders or waits on the GPU."
  [producer requests]
  (when-not @(:stopped? producer)
    (let [requests (or requests {})
          admitted (set (map key (take max-instances (sort-by (juxt (comp :order val) (comp str key)) requests))))
          [previous _] (reset-vals! (:requests producer) (select-keys requests admitted))]
      (reset! (:last-offered producer) requests)
      (when-not (= (set (keys previous)) admitted)
        (let [running (set (keys @(:entries producer)))]
          (doseq [id running :when (not (contains? admitted id))] (retire! producer id))
          (doseq [id admitted :when (not (contains? running id))] (admit! producer id (get requests id))))))))

(defn frames
  "Latest completed frames for the lighting latch:
  {:frames {feed-id frame} :instance-frames {instance-id {feed-id frame}}}."
  [producer]
  (let [published @(:published producer)]
    {:frames (into {}
                   (map (fn [[feed entries]] [feed (:frame (last (sort-by :order entries)))]))
                   (group-by :feed (vals published)))
     :instance-frames (into {} (map (fn [[id {:keys [feed frame]}]] [id {feed frame}])) published)}))

(defn- median [xs] (when (seq xs) (nth (sort xs) (quot (count xs) 2))))

(defn status
  "Portable producer status: one row per rendering instance, plus the
  renderer and any reason it is unavailable."
  [producer]
  (let [requests @(:requests producer)
        published @(:published producer)]
    {:renderer (some-> @(:renderer producer) describe-renderer (select-keys [:backend :renderer :version :programs :targets :renders :compile-failures :queued]))
     :error (some-> @(:renderer-error producer) (dissoc :at-ns))
     :refused (vec (sort (map str (keys (apply dissoc (or @(:last-offered producer) {}) (keys requests))))))
     :instances
     (vec (for [[id entry] (sort-by (comp str key) @(:entries producer))
                :let [e @entry r (get requests id) f (:frame (get published id))]]
            (merge {:instance-id id :feed (:feed e) :effect (:effect e) :fps (:fps e)
                    :width (:width r) :height (:height r)
                    :slots (:slots e 0) :rendered (:rendered e 0) :dropped (:dropped e 0)
                    :unchanged (:unchanged e 0) :failures (:failures e 0)
                    :compiles (:compiles e 0) :compile-failures (:compile-failures e 0)
                    :sequence (:sequence f) :captured-at-ms (:captured-at-ms f) :first-frame-ms (:first-frame-ms e)
                    :paused? (:paused? r false)
                    :render-ms-median (median (:render-ms e)) :draw-ms-median (median (:draw-ms e))
                    :last-render (:last-render e)
                    :last-working? (and (some? (:failed e)) (some? (:program e)))}
                   (when (:error e) {:error (:error e)}))))}))

(defn samples
  "Raw per-instance timing samples (render ms, draw ms, publish times) for
  measurement harnesses."
  [producer]
  (into {} (for [[id entry] @(:entries producer)]
             [id (select-keys @entry [:render-ms :draw-ms :published-at :slots :rendered :dropped :unchanged :failures
                                      :first-frame-ms :fps :feed])])))

(defn audition!
  "Compile shader on the renderer without publishing anything. Returns
  {:compiled? true} or {:compiled? false :stage :log :message}. Runs on the
  calling thread, which waits; never call it from the lighting frame."
  [producer shader]
  (if-let [r (renderer! producer)]
    (try (await-future (compile-shader r shader) compile-timeout-ms) {:compiled? true}
         (catch clojure.lang.ExceptionInfo e
           (let [d (ex-data e)]
             {:compiled? false :stage (:stage d) :log (:log d) :message (ex-message e)
              :errors (:errors d)})))
    {:compiled? false :message (:message @(:renderer-error producer)) :code (:code @(:renderer-error producer))}))

(defn stop!
  "Stop every cadence, drop published frames and close an owned renderer."
  [producer]
  (reset! (:stopped? producer) true)
  (doseq [id (keys @(:entries producer))] (retire! producer id))
  (reset! (:requests producer) {})
  (when (realized? (:pool producer))
    (.shutdownNow ^ScheduledThreadPoolExecutor @(:pool producer))
    (.awaitTermination ^ScheduledThreadPoolExecutor @(:pool producer) 5 TimeUnit/SECONDS))
  (when-let [r @(:renderer producer)]
    (reset! (:renderer producer) nil)
    (when (:owns-renderer? producer)
      (try (close-renderer r) (catch Throwable _ nil)))))
