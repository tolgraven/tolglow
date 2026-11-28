(ns tolglow.signal-tracks.recorder
  "The owned writer for signal tracks (#885) on a running show, and the REPL
  operations Studio would share.

    (require '[tolglow.signal-tracks.recorder :as tracks])
    (def recorder (tracks/attach! runtime {:id \"set-2026-11-28\"
                                           :attached-to {:kind :run-take :take-id \"run-12\"}}))
    ;; ... perform ...
    (def bundle (tracks/finish! recorder))
    @(tracks/store! runtime bundle)      ; into the show, so full backups keep it
    (tracks/recordings runtime)
    (tracks/summary runtime \"set-2026-11-28\" \"stem/bass/level\" {:grid {:beats 1}})

  The recorder is a show frame observer. Under the owner lock it only offers
  the committed model, an immutable value, to a bounded queue; it never
  waits. One owned thread folds the queue with
  `tolglow.signal-tracks/observe`. A frame the full queue refuses is counted
  and marks the recording truncated; a blocked writer never delays a frame."
  (:require [tolglow.show :as show]
            [tolglow.signal-tracks :as signal-tracks])
  (:import (java.util.concurrent ArrayBlockingQueue TimeUnit)
           (java.util.concurrent.atomic AtomicLong)))

(def default-queue-capacity 1024)

(defn attach!
  "Start recording runtime's committed frames. Options as
  signal-tracks/new-recording (:id, :attached-to, :max-rows), plus
  :queue-capacity (default 1024 frames) and :before-fold, a function the
  writer thread calls before folding each frame (tests use it to stall the
  writer). Returns the recorder for finish!."
  [runtime {:keys [id queue-capacity before-fold] :or {queue-capacity default-queue-capacity} :as options}]
  (let [recording (signal-tracks/new-recording options)
        queue (ArrayBlockingQueue. (int queue-capacity))
        state (atom recording)
        offered (AtomicLong.) folded (AtomicLong.) refused (AtomicLong.)
        stopping (volatile! false)
        errors (atom [])
        writer (Thread.
                ^Runnable
                (fn []
                  (loop []
                    (let [model (.poll queue 20 TimeUnit/MILLISECONDS)]
                      (cond
                        model (do (try (when before-fold (before-fold))
                                       (swap! state signal-tracks/observe model)
                                       (catch Throwable error
                                         (swap! errors #(vec (take-last 16 (conj % (ex-message error)))))))
                                  (.incrementAndGet folded)
                                  (recur))
                        @stopping nil
                        :else (recur)))))
                (str "tolglow-signal-tracks-" id))
        observer-id [::recorder id]]
    (.setDaemon writer true)
    (show/attach-observer! runtime observer-id
                           (fn []
                             ;; Under the owner lock: the baseline is the last committed model.
                             (swap! state signal-tracks/start (:model @(:state runtime)))
                             (.start writer)
                             {:frame! (fn [committed]
                                        (if (.offer queue (:model committed))
                                          (.incrementAndGet offered)
                                          (.incrementAndGet refused)))
                              :events! (fn [& _])
                              :tracing? (constantly false)
                              :close! (fn [] (vreset! stopping true))}))
    {:runtime runtime :observer-id observer-id :state state :queue queue :writer writer
     :offered offered :folded folded :refused refused :stopping stopping :errors errors}))

(defn settle!
  "Wait until the writer has folded every frame offered so far (default up
  to 10 s). Returns true when it has. Never call it under the owner lock."
  ([recorder] (settle! recorder 10000))
  ([{:keys [^AtomicLong offered ^AtomicLong folded]} timeout-ms]
   (let [deadline (+ (System/nanoTime) (* timeout-ms 1000000))]
     (loop []
       (cond (>= (.get folded) (.get offered)) true
             (> (System/nanoTime) deadline) false
             :else (do (Thread/sleep 1) (recur)))))))

(defn progress
  "Frames offered, folded and refused so far, and writer errors."
  [{:keys [^AtomicLong offered ^AtomicLong folded ^AtomicLong refused errors]}]
  {:offered (.get offered) :folded (.get folded) :refused (.get refused) :errors @errors})

(defn finish!
  "Stop recording, fold what is queued and return the finished bundle
  (signal-tracks/finish). Throws if the writer cannot finish in time."
  [{:keys [runtime observer-id state stopping ^Thread writer ^AtomicLong refused] :as recorder}]
  (show/detach-observer! runtime observer-id)
  (vreset! stopping true)
  (.join writer 10000)
  (when (.isAlive writer)
    (throw (ex-info "The signal track writer did not finish" (progress recorder))))
  (signal-tracks/finish @state (.get refused)))

(defn store!
  "Store a finished bundle in the show, replacing a recording with the same
  ID. Returns the receipt promise the next frame delivers."
  [runtime bundle]
  (show/submit! runtime :signal-tracks {:op :put :bundle bundle}))

(defn remove!
  "Remove a stored recording and the tracks only it named. Returns the receipt promise."
  [runtime id]
  (show/submit! runtime :signal-tracks {:op :remove :id id}))

(defn recordings
  "The stored recordings: ID, attachment, frames, truncation and each
  track's ID, kind, unit, range, row count and dropped count."
  [runtime]
  (let [model (:model @(:state runtime))
        {:keys [recordings tracks]} (signal-tracks/stored model)]
    (mapv (fn [[id recording]]
            (assoc (select-keys recording [:id :attached-to :frames :frames-dropped :truncated?])
                   :tracks (mapv (fn [[track-id hash]]
                                   (let [track (get tracks hash)]
                                     (assoc (select-keys track [:kind :unit :range :dropped :truncated?])
                                            :id track-id :rows (count (:rows track)) :hash hash)))
                                 (:tracks recording))))
          (sort-by key recordings))))

(defn bundle
  "The stored bundle of recording id."
  [runtime id]
  (signal-tracks/stored-bundle (:model @(:state runtime)) id))

(defn summary
  "The per-grid summary of one stored track (signal-tracks/summary)."
  ([runtime id track-id] (summary runtime id track-id {}))
  ([runtime id track-id options]
   (some-> (bundle runtime id) (signal-tracks/track track-id) (signal-tracks/summary options))))
