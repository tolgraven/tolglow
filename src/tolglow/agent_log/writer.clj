(ns tolglow.agent-log.writer
  "The owned IO boundary for the agent decision log (#856): one append-only
  JSON Lines file per session with content-addressed blobs in blobs/ beside
  it. Records reach the file through a bounded queue drained by the writer's
  own thread. log! never blocks: when the queue is full the record is
  dropped and counted, so a slow or stuck disk cannot hold up an agent run,
  let alone a frame. Nothing here runs inside swap!, and nothing is shared or
  exported as a side effect of logging."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.agent-log :as agent-log])
  (:import [java.io File]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption StandardOpenOption OpenOption CopyOption]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))

(defn blob-file
  "Where blob ref (blob:sha256:<hex>) lives for the log at path."
  ^File [path ref]
  (let [[_ hex] (re-matches #"blob:sha256:([0-9a-f]{64})" (str ref))]
    (when-not hex (throw (ex-info "Not a blob reference" {:ref ref})))
    (io/file (.getParentFile (.getAbsoluteFile (io/file path))) "blobs" "sha256" hex)))

(defn- append-line! [path ^String line]
  (let [file (io/file path)]
    (io/make-parents file)
    (Files/write (.toPath file) (.getBytes line StandardCharsets/UTF_8)
                 ^"[Ljava.nio.file.OpenOption;"
                 (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/APPEND
                                         StandardOpenOption/WRITE StandardOpenOption/DSYNC]))))

(defn- write-blob!
  "Write text at its content address once: to a temporary file, then an
  atomic move, so a reader never sees a partial blob."
  [path ref ^String text]
  (let [target (blob-file path ref)]
    (when-not (.exists target)
      (io/make-parents target)
      (let [tmp (File/createTempFile "blob" ".tmp" (.getParentFile target))]
        (try
          (Files/write (.toPath tmp) (.getBytes text StandardCharsets/UTF_8) ^"[Ljava.nio.file.OpenOption;" (into-array OpenOption []))
          (Files/move (.toPath tmp) (.toPath target)
                      ^"[Ljava.nio.file.CopyOption;" (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
          (finally (.delete tmp)))))))

(def ^:private stop (Object.))

(defn open!
  "Open the log at path (a .jsonl file) and start its writer thread.
  options: :capacity of the queue (default 256 records), and the boundary
  functions :append! [path line] and :put-blob! [path ref text], which
  default to the file system. Returns the writer; close! it."
  ([path] (open! path {}))
  ([path {:keys [capacity append! put-blob!] :or {capacity 256 append! append-line! put-blob! write-blob!}}]
   (when-not (and (pos-int? capacity) (<= capacity 65536)) (throw (ex-info "Queue capacity is 1 to 65536 records" {:capacity capacity})))
   (let [queue (ArrayBlockingQueue. (int capacity))
         counters (into {} (map (fn [k] [k (AtomicLong.)])) [:accepted :written :dropped :refused :failed])
         last-error (atom nil)
         thread (Thread.
                 ^Runnable
                 (fn []
                   (loop []
                     (let [entry (.take queue)]
                       (when-not (identical? entry stop)
                         (try
                           (doseq [[ref text] (:blobs entry)] (put-blob! path ref text))
                           (append! path (str (json/write-str (:record entry)) "\n"))
                           (.incrementAndGet ^AtomicLong (:written counters))
                           (catch Throwable t
                             (.incrementAndGet ^AtomicLong (:failed counters))
                             (reset! last-error (.getMessage t))))
                         (recur)))))
                 (str "tolglow-agent-log " (.getName (io/file path))))]
     (.setDaemon thread true)
     (.start thread)
     {:path (str path) :queue queue :counters counters :last-error last-error :thread thread :closed? (atom false)})))

(defn log!
  "Queue entry ({:record :blobs}, from agent-log/decision-record) for the
  writer thread. Returns true when queued, false when the writer is closed or
  its queue is full; never blocks."
  [writer entry]
  (let [{:keys [queue counters closed?]} writer]
    (if (and (not @closed?) (.offer ^ArrayBlockingQueue queue entry))
      (do (.incrementAndGet ^AtomicLong (:accepted counters)) true)
      (do (.incrementAndGet ^AtomicLong (:dropped counters)) false))))

(defn refuse!
  "Count a decision that made no valid record (agent-log/decision-hook)."
  [writer data]
  (.incrementAndGet ^AtomicLong (get-in writer [:counters :refused]))
  (reset! (:last-error writer) (str "Refused record: " (pr-str (select-keys data [:agent-log/error :errors]))))
  nil)

(defn stats
  "Counts of accepted, written, dropped (queue full or closed), refused
  (invalid record) and failed (IO error) records, and the last error."
  [writer]
  (assoc (update-vals (:counters writer) #(.get ^AtomicLong %))
         :queued (.size ^ArrayBlockingQueue (:queue writer))
         :last-error @(:last-error writer)))

(defn close!
  "Stop accepting records, let the thread write what is queued and wait up
  to timeout-ms (default 5000) for it. Returns stats and :drained?."
  ([writer] (close! writer 5000))
  ([writer timeout-ms]
   (reset! (:closed? writer) true)
   (let [^Thread thread (:thread writer)
         deadline (+ (System/nanoTime) (* 1000000 (long timeout-ms)))]
     ;; The stop marker waits for room behind queued records, up to the deadline.
     (loop []
       (when-not (or (.offer ^ArrayBlockingQueue (:queue writer) stop 10 TimeUnit/MILLISECONDS)
                     (> (System/nanoTime) deadline)
                     (not (.isAlive thread)))
         (recur)))
     (.join thread (max 1 (quot (- deadline (System/nanoTime)) 1000000)))
     (assoc (stats writer) :drained? (not (.isAlive thread))))))

(defn read-records
  "Every record in the log at path, in order, as keyword-keyed data."
  [path]
  (with-open [reader (io/reader path)]
    (into [] (comp (remove str/blank?) (map #(json/read-str % :key-fn keyword))) (line-seq reader))))

(defn blob-loader
  "A function from blob ref to its text for the log at path. It returns nil
  for a missing blob and refuses one whose content no longer matches its
  address."
  [path]
  (fn [ref]
    (let [file (blob-file path ref)]
      (when (.exists file)
        (let [text (String. (Files/readAllBytes (.toPath file)) StandardCharsets/UTF_8)]
          (when-not (= ref (agent-log/blob-ref text))
            (throw (ex-info "A blob no longer matches its content address" {:ref ref})))
          text)))))
