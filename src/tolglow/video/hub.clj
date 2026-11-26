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

(defn create
  "An empty hub. :bridge is the helper executable (default TOLGLOW_VIDEO_BRIDGE)
  and :feed-dir the directory for TGVF files (default a private temp dir)."
  ([] (create {}))
  ([{:keys [bridge feed-dir now-ms]
     :or {bridge (System/getenv "TOLGLOW_VIDEO_BRIDGE")
          now-ms #(System/currentTimeMillis)}}]
   {:bridge bridge :feed-dir feed-dir :now-ms now-ms
    :lock (Object.) :sources (atom {})
    :requested (atom ::none)
    :worker (delay (java.util.concurrent.Executors/newSingleThreadExecutor
                    (reify java.util.concurrent.ThreadFactory
                      (newThread [_ task] (doto (Thread. ^Runnable task "tolglow-video-sources") (.setDaemon true))))))}))

(defn- feed-directory ^File [hub]
  (or (some-> (:feed-dir hub) io/file (doto .mkdirs))
      (let [dir (File. (System/getProperty "java.io.tmpdir") (str "tolglow-video-" (.pid (java.lang.ProcessHandle/current))))]
        (.mkdirs dir)
        dir)))

(defn- safe-name [id]
  (str/replace (str (if (keyword? id) (subs (str id) 1) id)) #"[^A-Za-z0-9._-]" "_"))

(defn- unavailable [message config]
  (reify source/VideoSource
    (latest [_] nil)
    (describe [_] {:kind (:kind config) :label (str (:app config) " · " (:name config)) :connected? false :error message})
    (close! [_] nil)))

(defn- start-syphon [hub id {:keys [app name max-width max-height] :as config}]
  (let [bridge (:bridge hub)]
    (if-not (and bridge (.canExecute (io/file bridge)))
      {:source (unavailable "The Syphon video bridge is not installed (set TOLGLOW_VIDEO_BRIDGE)" config)}
      (let [file (File. (feed-directory hub) (str (safe-name id) "-" (System/nanoTime) ".tgvf"))
            command (cond-> [bridge "feed" "--app" app "--name" name "--out" (str file)]
                      max-width (into ["--max-width" (str max-width)])
                      max-height (into ["--max-height" (str max-height)]))
            process (-> (ProcessBuilder. ^java.util.List (vec command))
                        (.redirectErrorStream true)
                        (.start))
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

(defn- open-entry [hub id config]
  (if (= :syphon (:kind config))
    (assoc (start-syphon hub id config) :config config)
    {:config config
     :source (try (source/open id config)
                  (catch Exception e (unavailable (or (ex-message e) (str (class e))) config)))}))

(defn- close-entry! [{:keys [source ^Process process ^File file]}]
  (try (source/close! source) (catch Exception _ nil))
  (when process
    (try (.close (.getOutputStream process)) (catch Exception _ nil))
    (when-not (.waitFor process 2 TimeUnit/SECONDS)
      (.destroy process)
      (.waitFor process 2 TimeUnit/SECONDS)))
  (when file (.delete file)))

(defn reconcile!
  "Open, replace and close sources to match settings (an id → config map).
  Runs outside the lighting frame; process start and stop happen outside the
  source map update. Returns the ids now open."
  [hub configs]
  (let [configs (or configs {})
        [retired opened]
        (locking (:lock hub)
          (let [current @(:sources hub)
                retired (keep (fn [[id entry]] (when (not= (:config entry) (get configs id)) entry)) current)
                kept (into {} (filter (fn [[id entry]] (= (:config entry) (get configs id)))) current)
                opened (into kept (for [[id config] configs :when (not (contains? kept id))]
                                    [id (open-entry hub id config)]))]
            (reset! (:sources hub) opened)
            [retired opened]))]
    (run! close-entry! retired)
    (set (keys opened))))

(defn request-reconcile!
  "Queue a reconcile on the hub's own thread when the configured sources
  changed. Cheap enough to call after every committed frame; never blocks on
  a helper process."
  [hub configs]
  (let [configs (or configs {})
        [previous _] (reset-vals! (:requested hub) configs)]
    (when-not (= previous configs)
      (.execute ^java.util.concurrent.ExecutorService @(:worker hub)
                ^Runnable (fn [] (when (= configs @(:requested hub))
                                   (try (reconcile! hub configs) (catch Exception _ nil))))))))

(defn stop!
  "Close every source and helper process."
  [hub]
  (reset! (:requested hub) ::stopped)
  (when (realized? (:worker hub))
    (.shutdown ^java.util.concurrent.ExecutorService @(:worker hub))
    (.awaitTermination ^java.util.concurrent.ExecutorService @(:worker hub) 5 TimeUnit/SECONDS))
  (reconcile! hub {}))

(defn source-of [hub id] (get-in @(:sources hub) [id :source]))

(defn latch
  "The video frame input for one lighting frame: newest frames by source id,
  frames held for paused instances, and the mapping settings. Pure apart from
  reading each source's latest frame."
  [hub model previous]
  (let [playing? (not (false? (get-in model [:transport :playing?])))
        frames (if (or playing? (nil? previous))
                 (into {} (keep (fn [[id {:keys [source]}]]
                                  (when-let [f (try (source/latest source) (catch Exception _ nil))] [id f])))
                       @(:sources hub))
                 (:frames previous))
        held (into {} (for [[id instance] (:instances model)
                            :when (get-in instance [:system :paused?])]
                        ;; The frames this instance read on its last running frame.
                        [id (or (get-in previous [:held id]) (:frames previous) frames)]))]
    {:frames frames
     :held held
     :frozen? (not playing?)
     :mappings (get-in model [:settings :video :mappings] {})}))

(defn status
  "Portable source status for Studio and the REPL."
  [hub latch]
  (let [now ((:now-ms hub))]
    (vec (for [[id {:keys [source config]}] (sort-by (comp str key) @(:sources hub))]
           (let [d (source/describe source)
                 f (get-in latch [:frames id])
                 age (when-let [t (:captured-at-ms f)] (max 0 (- now t)))]
             (merge {:id id :config config}
                    (select-keys d [:kind :label :connected? :error :running? :last-event])
                    (when f {:width (:width f) :height (:height f) :sequence (:sequence f)})
                    {:age-ms age
                     :frozen? (boolean (:frozen? latch))
                     :stale? (boolean (or (nil? f) (false? (:connected? d))
                                          (and age (> age stale-after-ms))))}))))))

(defn list-syphon
  "Ask the helper for the current Syphon servers. Returns {:servers [...]} or
  {:error ...}; never throws for a missing helper."
  [hub]
  (let [bridge (:bridge hub)]
    (if-not (and bridge (.canExecute (io/file bridge)))
      {:servers [] :error "The Syphon video bridge is not installed (set TOLGLOW_VIDEO_BRIDGE)"}
      (let [process (let [command [bridge "list"]] (.start (ProcessBuilder. ^java.util.List command)))]
        (.close (.getOutputStream process))
        (if (.waitFor process 5 TimeUnit/SECONDS)
          (let [lines (str/split-lines (slurp (.getInputStream process)))
                report (some #(let [e (try (json/read-str % :key-fn keyword) (catch Exception _ nil))]
                                (when (= "servers" (:event e)) e)) lines)]
            {:servers (vec (:servers report))})
          (do (.destroy process) {:servers [] :error "The video bridge did not answer"}))))))
