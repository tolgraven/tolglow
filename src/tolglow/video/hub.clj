(ns tolglow.video.hub
  "Runtime-owned video sources and the per-frame latch (#672).

  The hub opens the sources named in [:settings :video :sources] outside the
  lighting frame. A Syphon source runs the native helper
  (`native/VideoBridge`, path from TOLGLOW_VIDEO_BRIDGE) as a child process
  writing a TGVF file; closing its stdin stops it, as the Mac app does for the
  show itself. Without a helper the source stays listed with an error, so a
  show saved on a Mac still loads elsewhere.

  `latch` runs inside the lighting frame and only reads each source's newest
  frame. While the transport is stopped it returns the previous latch, so a
  video cue freezes with every other effect and resumes on the current frame
  without catching up. A paused instance keeps the frames it saw when it
  paused."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.video.source :as source])
  (:import [java.io BufferedReader File]
           [java.util.concurrent TimeUnit]))

(def stale-after-ms
  "A source with no new frame for this long reads as stale in status."
  1000)

(def max-helpers
  "Syphon helper processes one hub runs at once."
  4)

(defn- start-process [command]
  (-> (ProcessBuilder. ^java.util.List (vec command)) (.redirectErrorStream true) (.start)))

(defn create
  "An empty hub. :bridge is the helper executable (default TOLGLOW_VIDEO_BRIDGE)
  and :feed-dir the directory for TGVF files (default a private temp dir,
  removed again by `stop!`). :show-directory (default the working directory,
  which the Mac app sets to the show folder) is the only place :image and
  :feed files may come from; :roots replaces the whole list. :spawn starts a
  helper from its argument vector (tests)."
  ([] (create {}))
  ([{:keys [bridge feed-dir now-ms show-directory roots spawn]
     :or {bridge (System/getenv "TOLGLOW_VIDEO_BRIDGE")
          now-ms #(System/currentTimeMillis)
          spawn start-process}}]
   {:bridge bridge :feed-dir feed-dir :now-ms now-ms :spawn spawn
    :roots (vec (or roots [(.getCanonicalFile (io/file (or show-directory (System/getProperty "user.dir"))))]))
    :created-dir (atom nil)
    :lock (Object.) :reconcile-lock (Object.) :list-lock (Object.) :sources (atom {})
    :requested (atom ::none)
    :worker (delay (java.util.concurrent.Executors/newSingleThreadExecutor
                    (reify java.util.concurrent.ThreadFactory
                      (newThread [_ task] (doto (Thread. ^Runnable task "tolglow-video-sources") (.setDaemon true))))))}))

(defn- feed-directory ^File [hub]
  (or (some-> (:feed-dir hub) io/file (doto .mkdirs))
      (let [dir (File. (System/getProperty "java.io.tmpdir") (str "tolglow-video-" (.pid (java.lang.ProcessHandle/current))))]
        (.mkdirs dir)
        (reset! (:created-dir hub) dir)
        dir)))

(defn- safe-name [id]
  (str/replace (str (if (keyword? id) (subs (str id) 1) id)) #"[^A-Za-z0-9._-]" "_"))

(defn- unavailable
  ([message config] (unavailable message config nil))
  ([message config code]
   (reify source/VideoSource
     (latest [_] nil)
     (describe [_] {:kind (:kind config) :connected? false :error message :code code
                    :label (or (:file config) (:resource config) (str (:app config) " · " (:name config)))})
     (close! [_] nil))))

(defn- start-syphon [hub id {:keys [app name max-width max-height] :as config}]
  (let [bridge (:bridge hub)]
    (if-not (and bridge (.canExecute (io/file bridge)))
      {:source (unavailable "The Syphon video bridge is not installed (set TOLGLOW_VIDEO_BRIDGE)" config)}
      (let [file (File. (feed-directory hub) (str (safe-name id) "-" (System/nanoTime) ".tgvf"))
            command (cond-> [bridge "feed" "--app" app "--name" name "--out" (str file)]
                      max-width (into ["--max-width" (str max-width)])
                      max-height (into ["--max-height" (str max-height)]))
            ^Process process ((:spawn hub) command)
            events (atom [])
            reader (doto (Thread. ^Runnable
                                  (fn []
                                    (with-open [in ^BufferedReader (io/reader (.getInputStream process))]
                                      (doseq [line (line-seq in)]
                                        (let [event (try (json/read-str line :key-fn keyword) (catch Exception _ {:event "log" :line line}))]
                                          (swap! events #(vec (take-last 16 (conj % event)))))))))
                     (.setDaemon true) (.setName (str "tolglow-video-" (safe-name id))) (.start))
            feed (source/feed id {:file (str file)})]
        {:process process :file file :reader reader :events events
         :source (reify source/VideoSource
                   (latest [_] (source/latest feed))
                   (describe [_] (merge (source/describe feed)
                                        {:kind :syphon :app app :name name
                                         :running? (.isAlive process)
                                         :last-event (last @events)}))
                   (close! [_] (source/close! feed)))}))))

(defn- open-entry
  "Open one source. A refused file or a failed decode leaves the source
  listed as unavailable with the reason and its :code. A helper that fails to
  start throws, so the pass can stop the helpers it already started."
  [hub id config helpers]
  (if (= :syphon (:kind config))
    (if (>= helpers max-helpers)
      {:config config :source (unavailable (str "At most " max-helpers " Syphon sources run at once") config :video/too-many-helpers)}
      (assoc (start-syphon hub id config) :config config))
    {:config config
     :source (try (source/open id config (:roots hub))
                  (catch Exception e
                    (unavailable (or (ex-message e) (str (class e))) config (:code (ex-data e)))))}))

(defn- close-entry! [{:keys [source ^Process process ^File file]}]
  (try (source/close! source) (catch Exception _ nil))
  (when process
    (try (.close (.getOutputStream process)) (catch Exception _ nil))
    (when-not (.waitFor process 2 TimeUnit/SECONDS)
      (.destroy process)
      ;; A helper that ignores SIGTERM must not outlive its source.
      (when-not (.waitFor process 2 TimeUnit/SECONDS)
        (.destroyForcibly process)
        (.waitFor process 2 TimeUnit/SECONDS))))
  (when file (.delete file)))

(defn reconcile!
  "Open, replace and close sources to match settings (an id → config map).
  Runs outside the lighting frame. Passes are serialized, but helper
  processes start and stop outside the hub's state lock, which only guards
  reading and replacing the source map. If a helper fails to start, the
  helpers this pass already started are stopped and the pass's new sources
  are listed as unavailable with the reason. Returns the ids now listed."
  [hub configs]
  (locking (:reconcile-lock hub)
    (let [configs (or configs {})
          current (locking (:lock hub) @(:sources hub))
          keep? (fn [[id entry]] (= (:config entry) (get configs id)))
          retired (map val (remove keep? current))
          kept (into {} (filter keep?) current)
          _ (locking (:lock hub) (reset! (:sources hub) kept))
          _ (run! close-entry! retired)
          pending (sort-by (comp str key) (remove (fn [[id _]] (contains? kept id)) configs))
          running (count (filter :process (vals kept)))
          opened (reduce (fn [opened [id config]]
                           (try
                             (assoc opened id (open-entry hub id config (+ running (count (filter :process (vals opened))))))
                             (catch Exception e
                               (run! close-entry! (vals opened))
                               (reduced (into {} (for [[id config] pending]
                                                   [id {:config config
                                                        :source (unavailable (str "The video bridge did not start: "
                                                                                  (or (ex-message e) (str (class e))))
                                                                             config :video/helper-start-failed)}]))))))
                         {} pending)]
      (locking (:lock hub) (reset! (:sources hub) (merge kept opened)))
      (set (keys (merge kept opened))))))

(defn request-reconcile!
  "Queue a reconcile on the hub's own thread when the configured sources
  changed. Cheap enough to call after every committed frame; never blocks on
  a helper process."
  [hub configs]
  (let [configs (or configs {})
        ;; A stopped hub stays stopped: a frame that commits while the show
        ;; stops must not open helpers that nothing will close.
        [previous _] (swap-vals! (:requested hub) #(if (= ::stopped %) % configs))]
    (when-not (or (= ::stopped previous) (= previous configs))
      (try
        (.execute ^java.util.concurrent.ExecutorService @(:worker hub)
                  ^Runnable (fn [] (when (= configs @(:requested hub))
                                     (try (reconcile! hub configs) (catch Exception _ nil)))))
        (catch java.util.concurrent.RejectedExecutionException _ nil)))))

(defn stop!
  "Close every source and helper process."
  [hub]
  (reset! (:requested hub) ::stopped)
  (when (realized? (:worker hub))
    (.shutdown ^java.util.concurrent.ExecutorService @(:worker hub))
    (.awaitTermination ^java.util.concurrent.ExecutorService @(:worker hub) 5 TimeUnit/SECONDS))
  (reconcile! hub {})
  ;; Remove the per-process feed directory this hub created, once empty.
  (when-let [^File dir @(:created-dir hub)]
    (when (empty? (.list dir)) (.delete dir))
    (reset! (:created-dir hub) nil)))

(defn source-of [hub id] (get-in @(:sources hub) [id :source]))

(defn latch
  "The video frame input for one lighting frame: newest frames by source id,
  frames held for paused instances, and the mapping settings. Pure apart from
  reading each source's latest frame. `produced` is the shader producer's
  {:frames :instance-frames} (#673): its feeds join the source frames, and a
  shader instance's own frame is kept under :instance-frames. While the
  transport is stopped both stay as they were; a paused instance keeps its
  own frame."
  ([hub model previous] (latch hub model previous nil))
  ([hub model previous produced]
   (let [playing? (not (false? (get-in model [:transport :playing?])))
         fresh? (or playing? (nil? previous))
         frames (if fresh?
                  (into (into {} (keep (fn [[id {:keys [source]}]]
                                         (when-let [f (try (source/latest source) (catch Exception _ nil))] [id f])))
                              @(:sources hub))
                        (:frames produced))
                  (:frames previous))
         paused (into #{} (keep (fn [[id instance]] (when (get-in instance [:system :paused?]) id))) (:instances model))
         instance-frames (if fresh?
                           (merge (:instance-frames produced {}) (select-keys (:instance-frames previous) paused))
                           (:instance-frames previous {}))
         held (into {} (for [id paused]
                         ;; The frames this instance read on its last running frame.
                         [id (or (get-in previous [:held id]) (:frames previous) frames)]))]
     {:frames frames
      :instance-frames (select-keys instance-frames (keys (:instances model)))
      :held held
      :frozen? (not playing?)
      :mappings (get-in model [:settings :video :mappings] {})})))

(defn status
  "Portable source status for Studio and the REPL."
  [hub latch]
  (let [now ((:now-ms hub))]
    (vec (for [[id {:keys [source config]}] (sort-by (comp str key) @(:sources hub))]
           (let [d (source/describe source)
                 f (get-in latch [:frames id])
                 ;; Only a live feed ages; a still or test grid never sends another frame.
                 live? (contains? #{:feed :syphon} (:kind d))
                 age (when-let [t (when live? (:captured-at-ms f))] (max 0 (- now t)))]
             (merge {:id id :config config}
                    (select-keys d [:kind :label :connected? :error :code :running? :last-event])
                    (when f {:width (:width f) :height (:height f) :sequence (:sequence f)})
                    {:age-ms age
                     :frozen? (boolean (:frozen? latch))
                     :stale? (boolean (or (nil? f) (false? (:connected? d))
                                          (and age (> age stale-after-ms))))}))))))

(defn list-syphon
  "Ask the helper for the current Syphon servers. Returns {:servers [...]} or
  {:error ...}; never throws for a missing helper. One listing helper runs at
  a time, however many clients ask at once."
  [hub]
  (let [bridge (:bridge hub)]
    (if-not (and bridge (.canExecute (io/file bridge)))
      {:servers [] :error "The Syphon video bridge is not installed (set TOLGLOW_VIDEO_BRIDGE)"}
      (locking (:list-lock hub)
        (let [process (let [command [bridge "list"]] (.start (ProcessBuilder. ^java.util.List command)))]
          (.close (.getOutputStream process))
          (if (.waitFor process 5 TimeUnit/SECONDS)
            (let [lines (str/split-lines (slurp (.getInputStream process)))
                  report (some #(let [e (try (json/read-str % :key-fn keyword) (catch Exception _ nil))]
                                  (when (= "servers" (:event e)) e)) lines)]
              {:servers (vec (:servers report))})
            (do (.destroyForcibly process) {:servers [] :error "The video bridge did not answer"})))))))
