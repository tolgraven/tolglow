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
  up. A frame that outlives the slot's wait is late, not a failure: the job
  keeps running, later slots are dropped until it lands, and it is then
  published. A cue whose frames take longer than its own period waits
  `late-duty` times its render time before the next one, so one heavy cue
  cannot monopolise the shared GPU worker. The renderer is replaced when
  Afterglow reports its context closed, or by the watchdog when it has had
  work queued without finishing any job for `:watchdog-ms`; a job is timed
  from the renderer's last progress, not from when it was queued. Each loss
  is recorded with its time and the next renderer opens after an
  exponentially growing delay; after `:max-losses` losses in a row the
  producer stops reopening and reports every running cue failed
  (:shader/renderer-unresponsive) until all of them have ended. A slot whose
  program, uniforms and target are unchanged re-uses the published frame, so
  a paused instance or a stopped transport costs nothing.

  Feed references: `frames` returns {:frames {feed-id frame} :instance-frames
  {instance-id {feed-id frame}}}. A feed id resolves to the newest-launched
  running instance on that feed; a shader cue's own `{:op :video}` output
  reads its own instance's frame first.

  Compilation is off the lighting deadline, on the renderer's worker. A
  version whose GLSL does not compile leaves the instance rendering its last
  working program (with the uniforms that program declares) and reports the
  driver log. A frame belongs to its instance: when the instance ends, its
  cadence stops, its frame is dropped from the feed and the program is
  released once no other instance renders it. The newest launches render:
  at most `max-instances` instances within the pixel budget; older ones stop
  rendering and are listed as refused, and a feed resolves only to an
  instance that renders.

  The GPU is an external boundary: a renderer implements `ShaderRenderer`.
  `native-renderer` loads Afterglow's JOGL renderer, which needs the
  `+native-gpu` profile; without it shader cues are listed as unavailable."
  (:require [tolglow.video.source :as source])
  (:import [java.util.concurrent Future ScheduledThreadPoolExecutor ThreadFactory TimeUnit TimeoutException
            ExecutionException]))

(def max-instances
  "Shader instances rendered at once."
  8)

(def history-size
  "Per-instance timing samples kept for status and measurement."
  512)

(defprotocol ShaderRenderer
  (compile-shader [renderer shader] "A Future of {:compiled? true}; fails with the driver log.")
  (render-frame [renderer shader request] "A Future of {:pixels :width :height :draw-ms :readback-ms :gpu-ms}.")
  (release-shader [renderer shader] "A Future; frees the program.")
  (describe-renderer [renderer] "Portable driver and cache status; :terminated? true once a closed renderer has released its context.")
  (close-renderer [renderer] "Release the context. false means its worker is still busy and cleanup is pending."))

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

(defn- await-future
  "Wait for a renderer Future. Running out of the producer's own patience is
  not a renderer failure: the job keeps running (its result is collected at
  a later slot) and the caller sees :shader/late."
  [^Future f timeout-ms]
  (try (.get f (long timeout-ms) TimeUnit/MILLISECONDS)
       (catch ExecutionException e (throw (or (.getCause e) e)))
       (catch TimeoutException e
         (throw (ex-info "The shader frame is late" {:code :shader/late :timeout-ms timeout-ms} e)))))

(def renderer-lost
  "Afterglow error types after which its context is already closed."
  #{:afterglow.render.gpu.shader-frames/closed :afterglow.render.gpu.shader-frames/timeout
    :afterglow.render.gpu.native-context/context-lost})

(def late-duty
  "A cue whose frame takes longer than its own period on the GPU worker (draw
  plus readback) waits this many times that before the next one, so it uses at most a quarter of the
  shared GPU worker and other cues keep their rate."
  3)

(def default-pixel-rate
  "Pixels per second a show renders by default, summed over admitted
  instances (width × height × fps): two 1920 × 1080 feeds at 30 fps, about
  500 MB/s of frame arrays. One 1080p30 feed measured clean against the
  40 Hz lighting interval; one 4096 × 2160 feed at 30 fps did not (a 34 ms
  tail, 3 % of intervals more than 5 ms off). A show raises it explicitly
  with :shader-options {:max-pixel-rate n}."
  (* 2 1920 1080 30))

(def default-watchdog-ms
  "A renderer that has had work queued for this long without finishing a
  single job has wedged: it is abandoned and, after a delay, a new one opens.
  Jobs queued behind one another do not add up, because each finished job is
  progress. Afterglow's own fence timeout (1 s) handles an ordinary hung
  draw."
  15000)

(def default-max-losses
  "Renderers lost in a row before the producer stops reopening. A loss counts
  towards the run when its renderer was lost within `healthy-periods`
  watchdog periods of opening. Each wedged renderer can leave a worker
  thread stuck in the driver with its GL context, so this also bounds the
  contexts abandoned while a cause persists."
  4)

(def healthy-periods
  "A renderer that lasted this many watchdog periods before it was lost
  starts a new run of losses."
  4)

(def poll-ms
  "How often a thread waiting on a compile checks the renderer's progress."
  25)

(defn create
  "A producer. :renderer is a ShaderRenderer or a no-argument factory
  (default `native-renderer`, opened on first use); the producer closes a
  renderer it opened. :now-ms is the wall clock for frame timestamps;
  :threads sizes the cadence pool (one per admitted instance plus one, so a
  waiting instance never holds up another's slots); :render-timeout-ms is
  how long a slot waits for its frame before calling it late (default 5000);
  :max-pixel-rate is the pixel budget (default `default-pixel-rate`);
  :watchdog-ms abandons a wedged renderer (default `default-watchdog-ms`);
  :max-losses bounds renderers lost in a row (default `default-max-losses`);
  :clock-ns is the monotonic clock for the watchdog and reopen delays
  (default System/nanoTime)."
  ([] (create {}))
  ([{:keys [renderer now-ms threads render-timeout-ms max-pixel-rate watchdog-ms max-losses clock-ns]
     :or {now-ms #(System/currentTimeMillis) threads (inc max-instances) render-timeout-ms 5000
          max-pixel-rate default-pixel-rate watchdog-ms default-watchdog-ms max-losses default-max-losses
          clock-ns #(System/nanoTime)}}]
   {:renderer-factory (if (satisfies? ShaderRenderer renderer) (constantly renderer) (or renderer native-renderer))
    :owns-renderer? (not (satisfies? ShaderRenderer renderer))
    :renderer (atom nil)
    :renderer-error (atom nil)
    :renderer-lock (Object.)
    ;; The current renderer's jobs: {:renderer :opened-ns :progress-ns
    ;; :pending {Future {:at-ns :instance-id}}}, on :clock-ns.
    :watch (atom nil)
    ;; Renderers lost in the current run.
    :losses (atom 0)
    ;; Renderers dropped by this producer whose close has not finished.
    :abandoned (atom #{})
    :now-ms now-ms
    :clock-ns clock-ns
    :render-timeout-ms render-timeout-ms
    :max-pixel-rate max-pixel-rate
    :watchdog-ms watchdog-ms
    :max-losses max-losses
    :opened (atom 0)
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
  "A renderer that failed to open is retried after this long. A wedged
  renderer's replacement waits this long, doubling with each further loss in
  a row; the first replacement for a context Afterglow closed opens at once."
  2000)

(defn- clock ^long [producer] (long ((:clock-ns producer))))

(defn- since-ms [producer since-ns] (/ (- (clock producer) (long since-ns)) 1.0e6))

(defn- prune-abandoned!
  "Forget abandoned renderers that have since released their context."
  [producer]
  (doseq [r @(:abandoned producer)]
    (when (try (:terminated? (describe-renderer r)) (catch Throwable _ false))
      (swap! (:abandoned producer) disj r))))

(defn- renderer! [producer]
  (or @(:renderer producer)
      (locking (:renderer-lock producer)
        (or @(:renderer producer)
            (let [{:keys [at-ns final?] wait-ms :retry-after-ms} @(:renderer-error producer)]
              (when-not (or final? (and at-ns (< (since-ms producer at-ns) (double wait-ms))))
                (prune-abandoned! producer)
                (if (>= (count @(:abandoned producer)) (long (:max-losses producer)))
                  ;; Every abandoned context is still held by a stuck worker:
                  ;; open no more until one is released.
                  (do (reset! (:renderer-error producer)
                              {:code :shader/renderer-unresponsive :abandoned (count @(:abandoned producer))
                               :message (str (count @(:abandoned producer)) " abandoned shader renderers have not released their GPU context; no new one opens until one does")
                               :at-ns (clock producer) :retry-after-ms retry-after-ms})
                      nil)
                  (try
                    (let [r ((:renderer-factory producer))
                          now (clock producer)]
                      (swap! (:opened producer) inc)
                      (reset! (:watch producer) {:renderer r :opened-ns now :progress-ns now :pending {}})
                      (reset! (:renderer-error producer) nil)
                      (reset! (:renderer producer) r))
                    (catch Exception e
                      (reset! (:renderer-error producer) {:message (ex-message e) :at-ns (clock producer)
                                                          :retry-after-ms retry-after-ms
                                                          :code (:code (ex-data e) :shader/renderer-failed)})
                      nil)))))))))

(defn- reopen-delay-ms
  "Delay before the renderer after the `losses`th loss in a row opens."
  [losses wedged?]
  (if (and (= 1 losses) (not wedged?)) 0 (* retry-after-ms (bit-shift-left 1 (dec (long losses))))))

(defn- drop-renderer!
  "Forget a renderer whose context Afterglow already closed (its fence timeout
  or a lost driver) or that wedged. The loss is recorded with its time; the
  next slot after the reopen delay opens a new renderer through the factory,
  unless this loss ends a run of :max-losses. The old renderer is closed on
  its own thread, never on a cadence thread, and counts as abandoned until
  its close finishes."
  [producer r error wedged?]
  (when (identical? r @(:renderer producer))
    (locking (:renderer-lock producer)
      (when (compare-and-set! (:renderer producer) r nil)
        (let [now (clock producer)
              opened (long (:opened-ns @(:watch producer) now))
              healthy? (> (- now opened) (* (long healthy-periods) (long (:watchdog-ms producer)) 1000000))
              losses (if healthy? 1 (inc (long @(:losses producer))))
              final? (>= losses (long (:max-losses producer)))
              cause (:code (ex-data error) :shader/renderer-failed)]
          (reset! (:losses producer) losses)
          (reset! (:renderer-error producer)
                  (merge (select-keys (ex-data error) [:instance-id :stalled-ms])
                         (if final?
                           {:code :shader/renderer-unresponsive :cause cause :losses losses :final? true
                            :message (str "The shader renderer failed " losses " times in a row; shader cues stay stopped until they have all ended")}
                           {:code cause :message (ex-message error) :losses losses
                            :at-ns now :retry-after-ms (reopen-delay-ms losses wedged?)})))
          (when (:owns-renderer? producer)
            (swap! (:abandoned producer) conj r)
            (doto (Thread. ^Runnable (fn []
                                       ;; A close that throws has finished; false means its worker is still busy.
                                       (when-not (false? (try (close-renderer r) (catch Throwable _ true)))
                                         (swap! (:abandoned producer) disj r)))
                           "tolglow-shader-renderer-close")
              (.setDaemon true)
              (.start))))))))

(defn- track!
  "Note job f, submitted to renderer r for instance id. Returns f."
  [producer r id ^Future f]
  (let [now (clock producer)]
    (swap! (:watch producer) #(if (identical? r (:renderer %)) (assoc-in % [:pending f] {:at-ns now :instance-id id}) %))
    f))

(defn- sweep!
  "Forget r's finished jobs; a finished job is progress. r's watch, or nil
  when r is no longer the current renderer."
  [producer r]
  (let [w @(:watch producer)]
    (when (identical? r (:renderer w))
      (let [done (into [] (comp (map key) (filter #(.isDone ^Future %))) (:pending w))]
        (if (empty? done)
          w
          (let [now (clock producer)
                w (swap! (:watch producer) #(if (identical? r (:renderer %))
                                              (-> % (update :pending (fn [p] (apply dissoc p done))) (assoc :progress-ns now))
                                              %))]
            (when (identical? r (:renderer w)) w)))))))

(defn- stalled
  "When renderer r has had work queued without finishing any job for longer
  than the watchdog: {:stalled-ms :instance-id} for its oldest job. A job
  counts from the later of its submission and r's last progress, so jobs
  queued behind one another on the one worker never add up."
  [producer r]
  (when-let [{:keys [pending progress-ns]} (sweep! producer r)]
    (when (seq pending)
      (let [[_ {:keys [at-ns instance-id]}] (apply min-key (comp :at-ns val) pending)
            ms (since-ms producer (max (long at-ns) (long progress-ns)))]
        (when (> ms (double (:watchdog-ms producer)))
          {:stalled-ms ms :instance-id instance-id})))))

(defn- wedged-error [data]
  (ex-info "The shader renderer stopped responding" (assoc data :code :shader/renderer-wedged)))

(defn- await-job
  "Wait for job f on renderer r for as long as r keeps finishing jobs. Throws
  :shader/renderer-wedged once r has finished nothing for the watchdog
  period (dropping it), or when r has been replaced meanwhile."
  [producer r ^Future f]
  (loop []
    (let [result (try (.get f poll-ms TimeUnit/MILLISECONDS)
                      (catch ExecutionException e (throw (or (.getCause e) e)))
                      (catch TimeoutException _ ::pending))]
      (cond
        (not (identical? ::pending result)) (do (sweep! producer r) result)
        (not (identical? r @(:renderer producer))) (throw (wedged-error {}))
        :else (if-let [stall (stalled producer r)]
                (let [e (wedged-error stall)]
                  (drop-renderer! producer r e true)
                  (throw e))
                (recur))))))

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
          (try (track! producer r id (release-shader r program)) (catch Exception _ nil)))))))

(defn- ensure-program!
  "Compile the request's shader when it changed. Returns the program to render
  (the request's, or the last working one), or nil. The compile may wait
  behind other jobs on the worker; it is abandoned only when the renderer
  stops making progress."
  [producer id entry request r]
  (let [{:keys [program failed]} @entry
        wanted (:shader request)]
    (if (or (= wanted program) (= wanted failed))
      program
      (let [started (System/nanoTime)]
        (try
          (await-job producer r (track! producer r id (compile-shader r wanted)))
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

(defn- publish!
  "Publish a completed render and set the cue's next allowed start."
  [producer id entry {:keys [request input uniforms started]} result]
  (let [finished (System/nanoTime)
        elapsed (/ (- finished (long started)) 1.0e6)
        ;; The duty cap follows the cue's own worker time, not time spent
        ;; queued behind other cues.
        worker-ms (if (:draw-ms result) (+ (double (:draw-ms result)) (double (:readback-ms result 0))) elapsed)
        period-ms (/ 1000.0 (:fps request))
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
                       ;; A compile failure stays reported; a late or failed render does not.
                       (update :error #(when (= :shader/compile-failed (:code %)) %))
                       (assoc :not-before-ns (when (> worker-ms period-ms)
                                               (+ finished (long (* late-duty worker-ms 1.0e6)))))
                       (cond-> (> worker-ms period-ms) (update :throttled (fnil inc 0)))
                       (update :rendered (fnil inc 0))
                       (update :render-ms push-sample elapsed)
                       (update :draw-ms push-sample (:draw-ms result))
                       (update :published-at push-sample now))))))

(defn- collect-late!
  "A render that outlived an earlier slot's wait. Publish it if it has
  finished. True while it is still running, so this slot is dropped."
  [producer id entry]
  (when-let [{:keys [^Future future renderer] :as job} (:in-flight @entry)]
    (if-not (.isDone future)
      ;; A job on a renderer that has since been dropped is abandoned at once;
      ;; on the current one, only when the renderer has stopped making progress.
      (if-let [stall (if (identical? renderer @(:renderer producer)) (stalled producer renderer) {})]
        (let [error (wedged-error stall)]
          (swap! entry #(-> % (dissoc :in-flight) (update :failures (fnil inc 0))
                            (assoc :error {:code :shader/renderer-wedged :message (ex-message error)})))
          (drop-renderer! producer renderer error true)
          false)
        true)
      (do (swap! entry dissoc :in-flight)
          (sweep! producer renderer)
          (try (publish! producer id entry job (await-future future 0))
               (catch Exception e
                 (swap! entry #(-> % (update :failures (fnil inc 0))
                                   (assoc :error {:code (:code (ex-data e) :shader/render-failed) :message (ex-message e)})))))
          false))))

(defn- tick!
  "Render one slot for an instance, then schedule its next slot. While an
  earlier frame is still rendering the slot is dropped, never queued."
  [producer id slot]
  (when-let [entry (get @(:entries producer) id)]
    (let [request (get @(:requests producer) id)]
      (when (and request (not @(:stopped? producer)))
        (if (collect-late! producer id entry)
          (swap! entry update :dropped (fnil inc 0))
          (do
            (swap! entry update :slots (fnil inc 0))
            (if-let [r (renderer! producer)]
              (try
                (when-let [program (ensure-program! producer id entry request r)]
                  (let [uniforms (if (= program (:shader request))
                                   (:uniforms request)
                                   (compatible-uniforms program request (:last-uniforms @entry)))
                        target (select-keys request [:width :height :viewport])
                        input [program uniforms target]]
                    (if (and (= input (:rendered-input @entry)) (get @(:published producer) id))
                      (swap! entry update :unchanged (fnil inc 0))
                      (let [job {:future (track! producer r id (render-frame r program (assoc target :uniforms uniforms)))
                                 :renderer r :request request :input input :uniforms uniforms :started (System/nanoTime)}]
                        (swap! entry assoc :in-flight job)
                        (let [result (await-future (:future job) (:render-timeout-ms producer))]
                          (swap! entry dissoc :in-flight)
                          (sweep! producer r)
                          (publish! producer id entry job result))))))
                (catch Exception e
                  (let [code (:code (ex-data e))
                        type (:type (ex-data e))]
                    (if (= :shader/late code)
                      ;; Keep the job: a later slot publishes it when it lands.
                      (swap! entry #(-> % (update :late (fnil inc 0)) (assoc :error {:code :shader/late :message (ex-message e)})))
                      (do (swap! entry #(-> % (dissoc :in-flight) (update :failures (fnil inc 0))
                                            (assoc :error {:code (or code :shader/render-failed) :message (ex-message e)})))
                          (when (renderer-lost type) (drop-renderer! producer r e false)))))))
              ;; The producer has given up on the renderer: this cue has failed.
              (let [error @(:renderer-error producer)]
                (when (and (= :shader/renderer-unresponsive (:code error))
                           (not= (:code error) (:code (:error @entry))))
                  (swap! entry #(-> % (update :failures (fnil inc 0))
                                    (assoc :error (select-keys error [:code :message :losses :abandoned])))))))))
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
          ;; A late cue waits out its duty cap; the slots it skips are dropped.
          allowed (if-let [nb (:not-before-ns @entry)]
                    (long (Math/ceil (/ (double (- (long nb) (long origin-ns))) (double period-ns))))
                    0)
          next-slot (max (inc (long slot)) due allowed)
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
          ;; The newest launches render: walk back from the newest within the
          ;; instance and pixel-rate limits; older instances stop rendering.
          admitted (loop [[[id r] & more] (reverse (sort-by (juxt (comp :order val) (comp str key)) requests))
                          admitted #{} rate 0]
                     (if (or (nil? id) (= max-instances (count admitted)))
                       admitted
                       (let [cost (* (:width r 1) (:height r 1) (:fps r 1))]
                         (if (> (+ rate cost) (:max-pixel-rate producer))
                           (recur more admitted rate)
                           (recur more (conj admitted id) (+ rate cost))))))
          [previous _] (reset-vals! (:requests producer) (select-keys requests admitted))]
      (reset! (:last-offered producer) requests)
      (when-not (= (set (keys previous)) admitted)
        ;; A launch after every shader cue has ended tries a renderer again.
        (when (and (empty? previous) (:final? @(:renderer-error producer)))
          (locking (:renderer-lock producer)
            (reset! (:losses producer) 0)
            (reset! (:renderer-error producer) nil)))
        (let [running (set (keys @(:entries producer)))]
          (doseq [id running :when (not (contains? admitted id))] (retire! producer id))
          (doseq [id admitted :when (not (contains? running id))] (admit! producer id (get requests id))))))))

(defn pixel-budget
  "Pixels per second this producer renders, summed over its instances."
  [producer] (:max-pixel-rate producer))

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
     :error (when-let [{:keys [at-ns retry-after-ms] :as e} @(:renderer-error producer)]
              (cond-> (dissoc e :at-ns)
                at-ns (assoc :retry-in-ms (max 0.0 (- (double retry-after-ms) (since-ms producer at-ns))))))
     :opened @(:opened producer)
     ;; Dropped renderers whose close has not finished (a stuck worker keeps its context).
     :abandoned (count @(:abandoned producer))
     :pixel-budget (:max-pixel-rate producer)
     ;; Older launches beyond the instance or pixel-rate limit do not render.
     :refused (vec (sort (map str (keys (apply dissoc (or @(:last-offered producer) {}) (keys requests))))))
     :instances
     (vec (for [[id entry] (sort-by (comp str key) @(:entries producer))
                :let [e @entry r (get requests id) f (:frame (get published id))]]
            (merge {:instance-id id :feed (:feed e) :effect (:effect e) :fps (:fps e)
                    :width (:width r) :height (:height r)
                    :slots (:slots e 0) :rendered (:rendered e 0) :dropped (:dropped e 0)
                    :unchanged (:unchanged e 0) :failures (:failures e 0)
                    :late (:late e 0) :throttled (:throttled e 0)
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
             [id (select-keys @entry [:render-ms :draw-ms :published-at :slots :rendered :dropped :unchanged :failures :late :throttled
                                      :first-frame-ms :fps :feed])])))

(defn audition!
  "Compile shader on the renderer without publishing anything. Returns
  {:compiled? true} or {:compiled? false :stage :log :message}. Runs on the
  calling thread, which waits; never call it from the lighting frame."
  [producer shader]
  (if-let [r (renderer! producer)]
    (try (await-job producer r (track! producer r nil (compile-shader r shader))) {:compiled? true}
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
