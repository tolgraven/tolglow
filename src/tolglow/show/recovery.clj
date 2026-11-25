(ns tolglow.show.recovery
  "An independently scheduled, bounded recovery file for each show."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io FileOutputStream]
           [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]
           [java.util HexFormat]
           [java.util.concurrent ScheduledThreadPoolExecutor ThreadFactory TimeUnit]))

(defn- default-directory []
  (let [home (System/getProperty "user.home")]
    (if (str/includes? (str/lower-case (System/getProperty "os.name")) "mac")
      (io/file home "Library" "Application Support" "Tolglow" "Recovery")
      (io/file (or (not-empty (System/getenv "XDG_STATE_HOME")) (io/file home ".local" "state"))
               "tolglow" "recovery"))))

(defn options [value]
  (when-not (or (nil? value) (boolean? value) (map? value))
    (throw (ex-info "Invalid show recovery options" {})))
  (let [{:keys [enabled? interval-ms directory] :as options}
        (merge {:enabled? true :interval-ms 10000}
               (cond (false? value) {:enabled? false} (map? value) value))]
    (when-not (and (every? #{:enabled? :interval-ms :directory} (keys options))
                   (boolean? enabled?) (pos-int? interval-ms))
      (throw (ex-info "Invalid show recovery options" {})))
    (assoc options :directory (.getAbsoluteFile (io/file (or directory (default-directory)))))))

(defn- checkpoint-file [directory show-id name]
  (let [identity (.getBytes (pr-str show-id) StandardCharsets/UTF_8)
        key (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") identity))]
    (io/file directory key name)))

(defn write-source!
  "Force a complete temporary file and publish it with one local atomic move.
  An optional eligibility check runs after the expensive write."
  ([path source] (write-source! path source (constantly true)))
  ([path ^String source eligible?]
   (let [file (.getAbsoluteFile (io/file path))]
     (io/make-parents file)
     (let [temp (Files/createTempFile (.toPath (.getParentFile file)) ".tolglow-show-" ".tmp"
                                     (make-array FileAttribute 0))]
       (try
         (with-open [stream (FileOutputStream. (.toFile temp))]
           (.write stream (.getBytes source StandardCharsets/UTF_8))
           (.force (.getChannel stream) true))
         (when (eligible?)
           (Files/move temp (.toPath file) (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                                  StandardCopyOption/REPLACE_EXISTING]))
           (.getAbsolutePath file))
         (finally (Files/deleteIfExists temp)))))))

(defn- autosave! [{:keys [directory status open? active?]} capture! current?]
  (try
    (when-let [{:keys [show-id source] :as captured} (when (and @open? @active?) (capture!))]
      (when-let [path (write-source! (checkpoint-file directory show-id "recovery.transit") source
                                    #(and @open? (current? captured)))]
        (swap! status assoc :last-saved (assoc (dissoc captured :source) :path path) :last-error nil)))
    (catch Exception error
      (when @open? (swap! status assoc :last-error (ex-message error))))))

(defn start! [{:keys [enabled? interval-ms] :as config} active? capture! current?]
  (let [owner (assoc config :open? (atom true) :active? (atom active?) :status (atom {}))]
    (if enabled?
      (let [factory (reify ThreadFactory
                      (newThread [_ task] (doto (Thread. task "tolglow-show-recovery") (.setDaemon true))))
            executor (ScheduledThreadPoolExecutor. 1 factory)]
        (.scheduleWithFixedDelay executor ^Runnable #(autosave! owner capture! current?)
                                 interval-ms interval-ms TimeUnit/MILLISECONDS)
        (assoc owner :executor executor))
      owner)))

(defn activate! [owner]
  (reset! (:active? owner) true))

(defn close! [{:keys [open? ^ScheduledThreadPoolExecutor executor]}]
  (when (and open? (compare-and-set! open? true false) executor)
    (.shutdownNow executor))
  nil)

(defn status [owner]
  (merge (select-keys owner [:enabled? :interval-ms]) (some-> owner :status deref)))

(defn remember-manual! [{:keys [directory status]} show-id path]
  (try
    (write-source! (checkpoint-file directory show-id "manual.edn") (pr-str {:show-id show-id :path path}))
    (swap! status dissoc :manual-error)
    (catch Exception error (swap! status assoc :manual-error (ex-message error))))
  path)

(defn- named-file [directory show-id]
  (let [bookmarked (checkpoint-file directory show-id "manual.edn")]
    (when (and (.isFile bookmarked) (<= (.length bookmarked) 16384))
      (try
        (let [{:keys [path] :as entry} (edn/read-string (slurp bookmarked :encoding "UTF-8"))]
          (when (and (= show-id (:show-id entry)) (string? path) (.isAbsolute (io/file path)))
            (io/file path)))
        (catch Exception _ nil)))))

(defn latest-file [{:keys [directory]} show-id kind]
  (let [file (case kind
               :recovery (checkpoint-file directory show-id "recovery.transit")
               :named (named-file directory show-id)
               (throw (ex-info "Unknown show checkpoint kind" {:kind kind})))]
    (when (and file (.isFile file)) file)))
