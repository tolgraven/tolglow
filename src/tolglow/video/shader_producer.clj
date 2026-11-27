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
  exponentially growing delay. A loss is blamed on the cue whose job stalled
  or whose frame overran Afterglow's fence; after `:max-strikes` on the same
  shader within `:loss-window-ms` that cue alone goes dark and gives up its
  slot until its GLSL changes or it is launched again, while the others
  carry on. Only losses no cue can be blamed for make the producer give up
  (`:max-losses` within the window), and then every cue goes dark. A slot whose
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

(def default-max-strikes
  "Renderer losses one cue's shader may cause before that cue stops
  rendering. A loss is the cue's when its job was the one the watchdog found
  stalled (the job running on Afterglow's single FIFO worker), or when its
  frame tripped Afterglow's fence timeout. The count is kept per shader: a
  new version of the cue starts again from zero."
  3)

(def default-max-losses
  "Renderer losses no cue can be blamed for (a lost driver, a stalled
  audition) within `default-loss-window-ms` before the producer stops
  reopening. Also the number of abandoned renderers still holding a GPU
  context (a wedged worker stuck in the driver) at which no new one opens."
  4)

(def default-loss-window-ms
  "Losses older than this no longer count towards giving up or towards the
  reopen delay."
  (* 10 60 1000))

(def healthy-periods
  "A renderer that made progress for this many watchdog periods after it
  opened was healthy: its replacement waits only the first reopen delay."
  4)

(def max-reopen-delay-ms
  "The reopen delay stops doubling here."
  60000)

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
  :max-strikes stops a cue whose shader caused that many renderer losses
  (default `default-max-strikes`); :max-losses bounds losses no cue can be
  blamed for within :loss-window-ms, and abandoned renderers (defaults
  `default-max-losses`, `default-loss-window-ms`); :clock-ns is the
  monotonic clock for the watchdog and reopen delays (default
  System/nanoTime)."
  ([] (create {}))
  ([{:keys [renderer now-ms threads render-timeout-ms max-pixel-rate watchdog-ms max-strikes max-losses
            loss-window-ms clock-ns]
     :or {now-ms #(System/currentTimeMillis) threads (inc max-instances) render-timeout-ms 5000
          max-pixel-rate default-pixel-rate watchdog-ms default-watchdog-ms max-strikes default-max-strikes
          max-losses default-max-losses loss-window-ms default-loss-window-ms clock-ns #(System/nanoTime)}}]
   {:renderer-factory (if (satisfies? ShaderRenderer renderer) (constantly renderer) (or renderer native-renderer))
    :owns-renderer? (not (satisfies? ShaderRenderer renderer))
    :renderer (atom nil)
    :renderer-error (atom nil)
    :renderer-lock (Object.)
    ;; The current renderer's jobs: {:renderer :opened-ns :progress-ns
    ;; :pending {Future {:at-ns :instance-id}}}, on :clock-ns.
    :watch (atom nil)
    ;; Recent renderer losses, oldest first: [{:at-ns :blamed? :code}].
    :losses (atom [])
    ;; Submitting a job and recording it happen under one lock, numbered, so
    ;; the recorded order is the order of the renderer's queue.
    :submit-lock (Object.)
    :job-seq (atom 0)
    ;; instance id -> {:shader :request-shader :code :strikes :feed :effect ...}:
    ;; cues stopped for the renderer losses their shader caused.
    :halted (atom {})
    ;; Renderers dropped by this producer whose close has not finished.
    :abandoned (atom #{})
    :now-ms now-ms
    :clock-ns clock-ns
    :render-timeout-ms render-timeout-ms
    :max-pixel-rate max-pixel-rate
    :watchdog-ms watchdog-ms
    :max-strikes max-strikes
    :max-losses max-losses
    :loss-window-ms loss-window-ms
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
  renderer's replacement waits this long, doubling with each further recent
  loss up to `max-reopen-delay-ms`; the first replacement for a context
  Afterglow closed opens at once."
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
  "Delay before the next renderer opens, after the `n`th recent loss."
  [n wedged?]
  (if (and (= 1 n) (not wedged?))
    0
    (min max-reopen-delay-ms (* retry-after-ms (bit-shift-left 1 (min 16 (dec (long n))))))))

(def ^:private halt-messages
  {:shader/renderer-unresponsive "This cue's shader stalled the GPU renderer"
   :shader/frame-timeout "This cue's frames overran Afterglow's GPU fence timeout"})

(defn- effective-shader
  "The shader instance id renders: the request's, or its last working
  program while the request's does not compile."
  [entry request]
  (let [{:keys [program failed]} @entry]
    (if (and failed (= failed (:shader request))) program (:shader request))))

(defn- recent-strikes
  "Strike times of entry's `shader` within the loss window."
  [producer e shader now]
  (when (= shader (get-in e [:strikes :shader]))
    (let [window-ns (* (long (:loss-window-ms producer)) 1000000)]
      (filterv #(< (- (long now) (long %)) window-ns) (get-in e [:strikes :at])))))

(defn- halt!
  "Stop instance id, whose shader caused `strikes` renderer losses: its frame
  leaves the feed now, and the next `sync!` frees its admission slot and
  pixel-budget share."
  [producer id entry request shader code strikes]
  (swap! (:halted producer) assoc id {:shader shader :request-shader (:shader request) :code code :strikes strikes
                                      :feed (:feed request) :effect (:effect request) :fps (:fps request)
                                      :width (:width request) :height (:height request)
                                      :message (str (halt-messages code) " " strikes " times; it stays stopped until its GLSL changes or it is launched again")})
  (swap! (:published producer) dissoc id)
  (swap! entry update :failures (fnil inc 0)))

(defn- strike!
  "Blame instance id's `shader` (the shader of the job that failed) for a
  renderer loss. Strikes count within the loss window, per shader; a shader
  the cue no longer renders (a new version saved meanwhile) is not struck.
  After :max-strikes the cue stops. True when the instance is running."
  [producer id shader code]
  (when-let [entry (when (some? id) (get @(:entries producer) id))]
    (let [request (get @(:requests producer) id)]
      (when (and request (= shader (effective-shader entry request)))
        (let [now (clock producer)
              e (swap! entry (fn [e] (assoc e :strikes {:shader shader :at (conj (or (recent-strikes producer e shader now) []) now)})))
              n (count (get-in e [:strikes :at]))]
          (when (>= n (long (:max-strikes producer)))
            (halt! producer id entry request shader code n))))
      true)))

(defn- halted?
  "True while instance id is stopped for the shader it is still offered with."
  [producer id request]
  (when-let [h (get @(:halted producer) id)]
    (= (:request-shader h) (:shader request))))

(defn- drop-renderer!
  "Forget a renderer whose context Afterglow already closed (its fence timeout
  or a lost driver) or that wedged. The loss is recorded with its time and the
  next slot after the reopen delay opens a new renderer through the factory.
  A wedge strikes the cue whose job stalled; blamed? says the caller has
  already struck a cue (a fence timeout). Only losses no cue can be blamed for
  count towards giving up (:max-losses within :loss-window-ms); a context
  Afterglow closed after another failure (::closed) never does. The old
  renderer is closed on its own thread, never on a cadence thread, and counts
  as abandoned until its close finishes."
  [producer r error {:keys [wedged? blamed? instance-id]}]
  (when (identical? r @(:renderer producer))
    (locking (:renderer-lock producer)
      (when (compare-and-set! (:renderer producer) r nil)
        (let [now (clock producer)
              data (ex-data error)
              {:keys [opened-ns progress-ns]} @(:watch producer)
              ;; Healthy: the renderer kept finishing jobs for a while after it opened.
              healthy? (>= (- (long (or progress-ns now)) (long (or opened-ns now)))
                           (* (long healthy-periods) (long (:watchdog-ms producer)) 1000000))
              blamed? (or blamed? (and wedged? (strike! producer (:instance-id data) (:shader data) :shader/renderer-unresponsive)))
              cause (or (:code data) (:type data) :shader/renderer-failed)
              counts? (not (or blamed? (= :afterglow.render.gpu.shader-frames/closed (:type data))))
              window-ns (* (long (:loss-window-ms producer)) 1000000)
              recent (conj (filterv #(< (- now (long (:at-ns %))) window-ns) @(:losses producer))
                           {:at-ns now :counts? counts? :code cause})
              unblamed (count (filter :counts? recent))
              final? (and counts? (>= unblamed (long (:max-losses producer))))]
          (reset! (:losses producer) recent)
          ;; Giving up: every cue goes dark rather than holding its last frame.
          (when final? (reset! (:published producer) {}))
          (reset! (:renderer-error producer)
                  (merge (select-keys data [:instance-id :stalled-ms])
                         (when instance-id {:instance-id instance-id})
                         (if final?
                           {:code :shader/renderer-unresponsive :cause cause :losses (count recent) :final? true
                            :message (str "The shader renderer failed " unblamed " times without a cue to blame; shader cues stay stopped until one is launched or edited")}
                           {:code cause :message (ex-message error) :losses (count recent) :blamed? (boolean blamed?)
                            :at-ns now :retry-after-ms (reopen-delay-ms (if healthy? 1 (count recent)) wedged?)})))
          (when (:owns-renderer? producer)
            (swap! (:abandoned producer) conj r)
            (doto (Thread. ^Runnable (fn []
                                       ;; A close that throws has finished; false means its worker is still busy.
                                       (when-not (false? (try (close-renderer r) (catch Throwable _ true)))
                                         (swap! (:abandoned producer) disj r)))
                           "tolglow-shader-renderer-close")
              (.setDaemon true)
              (.start))))))))

(defn- submit!
  "Submit a job for instance id (nil for an audition) rendering `shader` to
  renderer r by calling `f`, and record it. Afterglow's enqueue never
  blocks; doing both under one lock with a sequence number makes the
  recorded order the order of r's queue. Returns the job's Future."
  [producer r id shader f]
  (locking (:submit-lock producer)
    (let [^Future fut (f)
          job {:at-ns (clock producer) :seq (swap! (:job-seq producer) inc) :instance-id id :shader shader}]
      (swap! (:watch producer) #(if (identical? r (:renderer %)) (assoc-in % [:pending fut] job) %))
      fut)))

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
  than the watchdog: {:stalled-ms :instance-id :shader} for its oldest job,
  the one at the head of the worker's queue. A job counts from the later of
  its submission and r's last progress, so jobs queued behind one another on
  the one worker never add up."
  [producer r]
  (when-let [{:keys [pending progress-ns]} (sweep! producer r)]
    (when (seq pending)
      (let [[_ {:keys [at-ns instance-id shader]}] (apply min-key (comp :seq val) pending)
            ms (since-ms producer (max (long at-ns) (long progress-ns)))]
        (when (> ms (double (:watchdog-ms producer)))
          {:stalled-ms ms :instance-id instance-id :shader shader})))))

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
        (not (identical? r @(:renderer producer))) (throw (ex-info "The shader renderer was replaced" {:code :shader/renderer-replaced}))
        :else (if-let [stall (stalled producer r)]
                (let [e (wedged-error stall)]
                  (drop-renderer! producer r e {:wedged? true})
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
          (try (submit! producer r id program #(release-shader r program)) (catch Exception _ nil)))))))

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
          (await-job producer r (submit! producer r id wanted #(compile-shader r wanted)))
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
    ;; An instance that ended, stopped or lost its renderer for good while its
    ;; frame rendered publishes nothing.
    (when (and (identical? entry (get @(:entries producer) id))
               (not (contains? @(:halted producer) id))
               (not (:final? @(:renderer-error producer))))
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

(defn- renderer-lost!
  "After instance id's job rendering `shader` on renderer r failed with e: when Afterglow has
  closed the context, drop r. A frame that overran the fence is this cue's
  fault; a closed context is a consequence of another failure."
  [producer id shader r e]
  (let [type (:type (ex-data e))]
    (when (renderer-lost type)
      (let [blamed? (and (= :afterglow.render.gpu.shader-frames/timeout type)
                         (strike! producer id shader :shader/frame-timeout))]
        (drop-renderer! producer r e {:blamed? blamed? :instance-id (when blamed? id)})))))

(defn- collect-late!
  "A render that outlived an earlier slot's wait. Publish it if it has
  finished. True while it is still running, so this slot is dropped."
  [producer id entry]
  (when-let [{:keys [^Future future renderer] :as job} (:in-flight @entry)]
    (if-not (.isDone future)
      (if-not (identical? renderer @(:renderer producer))
        ;; Its renderer has since been dropped: abandon the job at once.
        (do (swap! entry #(-> % (dissoc :in-flight) (update :failures (fnil inc 0))
                              (assoc :error {:code :shader/renderer-replaced :message "The shader renderer was replaced"})))
            false)
        ;; On the current renderer, only once it has stopped making progress.
        (if-let [stall (stalled producer renderer)]
          (let [error (wedged-error stall)]
            (swap! entry #(-> % (dissoc :in-flight) (update :failures (fnil inc 0))
                              (assoc :error {:code :shader/renderer-wedged :message (ex-message error)})))
            (drop-renderer! producer renderer error {:wedged? true})
            false)
          true))
      (do (swap! entry dissoc :in-flight)
          (sweep! producer renderer)
          (try (publish! producer id entry job (await-future future 0))
               (catch Exception e
                 (swap! entry #(-> % (update :failures (fnil inc 0))
                                   (assoc :error {:code (:code (ex-data e) :shader/render-failed) :message (ex-message e)})))
                 (renderer-lost! producer id (first (:input job)) renderer e)))
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
            (if-let [r (when-not (halted? producer id request) (renderer! producer))]
              (try
                (when-let [program (ensure-program! producer id entry request r)]
                  (let [uniforms (if (= program (:shader request))
                                   (:uniforms request)
                                   (compatible-uniforms program request (:last-uniforms @entry)))
                        target (select-keys request [:width :height :viewport])
                        input [program uniforms target]]
                    (if (and (= input (:rendered-input @entry)) (get @(:published producer) id))
                      (swap! entry update :unchanged (fnil inc 0))
                      (let [job {:future (submit! producer r id program #(render-frame r program (assoc target :uniforms uniforms)))
                                 :renderer r :request request :input input :uniforms uniforms :started (System/nanoTime)}]
                        (swap! entry assoc :in-flight job)
                        (let [result (await-future (:future job) (:render-timeout-ms producer))]
                          (swap! entry dissoc :in-flight)
                          (sweep! producer r)
                          (publish! producer id entry job result))))))
                (catch Exception e
                  (let [code (:code (ex-data e))
                        failed-shader (first (:input (:in-flight @entry)))]
                    (if (= :shader/late code)
                      ;; Keep the job: a later slot publishes it when it lands.
                      (swap! entry #(-> % (update :late (fnil inc 0)) (assoc :error {:code :shader/late :message (ex-message e)})))
                      (do (swap! entry #(-> % (dissoc :in-flight) (update :failures (fnil inc 0))
                                            (assoc :error {:code (or code :shader/render-failed) :message (ex-message e)})))
                          (renderer-lost! producer id failed-shader r e))))))
              ;; The producer has given up on the renderer: this cue has failed.
              (let [error @(:renderer-error producer)]
                (when (and (not (halted? producer id request))
                           (= :shader/renderer-unresponsive (:code error))
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
          ;; A stopped cue stays stopped while it is offered with the same
          ;; shader; a new version or a new launch clears it.
          halted (let [h @(:halted producer)
                       stale (keep (fn [[id {:keys [request-shader]}]]
                                     (when (not= request-shader (:shader (get requests id))) id))
                                   h)]
                   (if (seq stale) (apply swap! (:halted producer) dissoc stale) h))
          ;; The newest launches render: walk back from the newest within the
          ;; instance and pixel-rate limits; older instances stop rendering.
          ;; A stopped cue takes no slot or pixel-budget share.
          admitted (loop [[[id r] & more] (reverse (sort-by (juxt (comp :order val) (comp str key))
                                                            (apply dissoc requests (keys halted))))
                          admitted #{} rate 0]
                     (if (or (nil? id) (= max-instances (count admitted)))
                       admitted
                       (let [cost (* (:width r 1) (:height r 1) (:fps r 1))]
                         (if (> (+ rate cost) (:max-pixel-rate producer))
                           (recur more admitted rate)
                           (recur more (conj admitted id) (+ rate cost))))))
          [previous _] (reset-vals! (:requests producer) (select-keys requests admitted))]
      (reset! (:last-offered producer) requests)
      ;; After giving up, a launch from no running cue or a new version of a
      ;; running cue tries a renderer again.
      (when (and (:final? @(:renderer-error producer))
                 (or (and (empty? previous) (seq admitted))
                     (some #(and (contains? previous %) (not= (:shader (get previous %)) (:shader (get requests %)))) admitted)))
        (locking (:renderer-lock producer)
          (reset! (:losses producer) [])
          (reset! (:renderer-error producer) nil)))
      (when-not (= (set (keys previous)) admitted)
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
  (prune-abandoned! producer)
  (let [requests @(:requests producer)
        published @(:published producer)
        entries @(:entries producer)
        halted @(:halted producer)
        now (clock producer)
        stopped (fn [h] (select-keys h [:code :strikes :message]))]
    {:renderer (some-> @(:renderer producer) describe-renderer (select-keys [:backend :renderer :version :programs :targets :renders :compile-failures :queued]))
     :error (when-let [{:keys [at-ns retry-after-ms] :as e} @(:renderer-error producer)]
              (cond-> (dissoc e :at-ns)
                at-ns (assoc :retry-in-ms (max 0.0 (- (double retry-after-ms) (since-ms producer at-ns))))))
     :opened @(:opened producer)
     ;; Dropped renderers whose close has not finished (a stuck worker keeps its context).
     :abandoned (count @(:abandoned producer))
     :pixel-budget (:max-pixel-rate producer)
     ;; Older launches beyond the instance or pixel-rate limit do not render.
     :refused (vec (sort (map str (keys (apply dissoc (or @(:last-offered producer) {}) (concat (keys requests) (keys halted)))))))
     :instances
     (vec (sort-by (comp str :instance-id)
                   (concat
                    (for [[id entry] entries
                          :let [e @entry r (get requests id) f (:frame (get published id)) h (get halted id)]]
                      (merge {:instance-id id :feed (:feed e) :effect (:effect e) :fps (:fps e)
                              :width (:width r) :height (:height r)
                              :slots (:slots e 0) :rendered (:rendered e 0) :dropped (:dropped e 0)
                              :unchanged (:unchanged e 0) :failures (:failures e 0)
                              :late (:late e 0) :throttled (:throttled e 0)
                              :compiles (:compiles e 0) :compile-failures (:compile-failures e 0)
                              ;; Renderer losses blamed on the shader the cue renders, within the window.
                              :strikes (if h (:strikes h) (count (recent-strikes producer e (effective-shader entry r) now)))
                              :halted? (some? h)
                              :sequence (:sequence f) :captured-at-ms (:captured-at-ms f) :first-frame-ms (:first-frame-ms e)
                              :paused? (:paused? r false)
                              :render-ms-median (median (:render-ms e)) :draw-ms-median (median (:draw-ms e))
                              :last-render (:last-render e)
                              :last-working? (and (some? (:failed e)) (some? (:program e)))}
                             (cond h {:error (stopped h)} (:error e) {:error (:error e)})))
                    ;; A stopped cue no longer has a cadence; it is still listed.
                    (for [[id h] halted :when (not (contains? entries id))]
                      (merge (zipmap [:slots :rendered :dropped :unchanged :failures :late :throttled :compiles :compile-failures] (repeat 0))
                             (select-keys h [:feed :effect :fps :width :height :strikes])
                             {:instance-id id :halted? true :paused? false :last-working? false :error (stopped h)})))))}))

(defn active?
  "True while any shader instance renders or is listed as stopped."
  [producer]
  (boolean (or (seq @(:entries producer)) (seq @(:halted producer)))))

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
    (try (await-job producer r (submit! producer r nil shader #(compile-shader r shader))) {:compiled? true}
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
