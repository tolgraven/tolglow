(ns tolglow.studio.audio
  "Explicit JavaSound capture boundary for the functional audio analyzer. One
  bounded worker owns each manager's stream and analyzer state. Requiring this
  namespace, creating a manager and listing inputs never open a device."
  (:require [tolglow.integrations.analysis :as analysis])
  (:import [javax.sound.sampled AudioSystem AudioFormat DataLine$Info TargetDataLine]
           [java.util UUID]))

(defn- failure [code message] (ex-info message {:audio/error code}))
(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))
(defn- monotonic-ms [] (/ (double (System/nanoTime)) 1000000.0))

(defn list-inputs
  "Enumerate capture-capable mixer metadata without opening lines. Mixer indices
  describe this enumeration, not permanent device IDs; optional :mixer-name on
  start! guards against a stale UI selection. The system default is selected
  explicitly with {:default? true}."
  []
  (->> (AudioSystem/getMixerInfo)
       (map-indexed (fn [index info]
                      (let [mixer (AudioSystem/getMixer info)
                            capture? (some #(.isAssignableFrom TargetDataLine (.getLineClass %))
                                           (.getTargetLineInfo mixer))]
                        (when capture?
                          {:mixer-index index :name (.getName info) :vendor (.getVendor info)
                           :description (.getDescription info) :version (.getVersion info)}))))
       (remove nil?) vec))

(defn- settings [config]
  (when-not (map? config) (throw (failure :invalid-config "Audio configuration must be an object")))
  (let [opts (merge {:sample-rate 48000.0 :channels 1 :block-frames 480 :buffer-blocks 8
                    :big-endian? false :analyzer-options {}} config)
        index (:mixer-index opts)]
    (when-not (and (finite? (:sample-rate opts)) (<= 8000 (:sample-rate opts) 192000)
                   (#{1 2} (:channels opts)) (integer? (:block-frames opts)) (<= 1 (:block-frames opts) 16384)
                   (integer? (:buffer-blocks opts)) (<= 2 (:buffer-blocks opts) 64)
                   (boolean? (:big-endian? opts))
                   (or (and (integer? index) (<= 0 index) (not (:default? opts)))
                       (and (nil? index) (true? (:default? opts))))
                   (or (nil? (:mixer-name opts)) (string? (:mixer-name opts))))
      (throw (failure :invalid-config "Choose an input and valid mono/stereo signed-16 PCM settings")))
    (analysis/new-analyzer (:analyzer-options opts))
    (select-keys opts [:sample-rate :channels :block-frames :buffer-blocks :big-endian?
                       :analyzer-options :mixer-index :mixer-name :default?])))

(defn validate-config
  "Pure capture configuration validation/normalization. Does not enumerate or
  open a device; safe for validating a portable connections document."
  [config] (settings config))

(defn open-java-sound!
  "Explicitly open the selected signed-16 PCM input. Called only by start!.
  Injectable sources use the same {:read! [bytes offset length] :close! []}
  contract. A read may return byte count, -1 EOF, or {:bytes n :dropped-frames n
  :possible-overflow? bool}. Report exact dropped frames only when known."
  ([config] (open-java-sound! config monotonic-ms))
  ([config now-ms]
   (let [{:keys [sample-rate channels block-frames buffer-blocks big-endian? mixer-index mixer-name]} (settings config)
         format (AudioFormat. (float sample-rate) 16 (int channels) true big-endian?)
         info (DataLine$Info. TargetDataLine format)
         mixer-info (when (some? mixer-index)
                      (let [mixers (AudioSystem/getMixerInfo)]
                        (when (>= mixer-index (alength mixers))
                          (throw (failure :missing-input "Selected audio input is no longer available")))
                        (aget mixers mixer-index)))
         _ (when (and mixer-name mixer-info (not= mixer-name (.getName mixer-info)))
             (throw (failure :changed-input "Audio input enumeration changed; select the device again")))
         ^TargetDataLine line (if mixer-info (.getLine (AudioSystem/getMixer mixer-info) info) (AudioSystem/getLine info))
         frame-size (* channels 2)]
     (try
       (.open line format (int (* frame-size block-frames buffer-blocks)))
       (let [started-at-ms (now-ms)]
         (.start line)
         {:started-at-ms started-at-ms
          :description {:name (if mixer-info (.getName mixer-info) "System default input")
                        :format :signed-16-pcm :timestamp-method :sample-counter-start-estimate}
          :read! (fn [^bytes bytes offset length]
                   (let [available (.available line) capacity (.getBufferSize line)
                         n (.read line bytes (int offset) (int length))]
                     {:bytes n :buffered-frames (quot (.available line) frame-size)
                      ;; JavaSound exposes capacity, not a portable lost-frame
                      ;; count. A full buffer is only a possible-overflow signal.
                      :possible-overflow? (>= available (- capacity frame-size))}))
          :close! (fn [] (try (.stop line) (finally (.close line))))})
       (catch Throwable error (.close line) (throw error))))))

(defn decode-pcm16
  "Copy signed-16 PCM bytes into a new mono double array. Mono uses /32768;
  stereo averages normalized left/right. Never modifies input bytes."
  [^bytes bytes offset byte-count channels big-endian?]
  (when-not (#{1 2} channels) (throw (failure :invalid-pcm "PCM input must be mono or stereo")))
  (let [frame-size (* channels 2)]
    (when-not (and (#{1 2} channels) (boolean? big-endian?)
                   (integer? offset) (integer? byte-count) (<= 0 offset) (<= 0 byte-count)
                   (<= (+ offset byte-count) (alength bytes)) (zero? (mod byte-count frame-size)))
      (throw (failure :invalid-pcm "PCM data must contain complete mono/stereo frames")))
    (let [frames (quot byte-count frame-size) out (double-array frames)]
      (dotimes [frame frames]
        (let [base (+ offset (* frame frame-size))
              total (loop [channel 0 total 0.0]
                      (if (= channel channels) total
                          (let [at (+ base (* channel 2)) a (bit-and 255 (aget bytes at)) b (bit-and 255 (aget bytes (inc at)))
                                unsigned (if big-endian? (+ (bit-shift-left a 8) b) (+ a (bit-shift-left b 8)))
                                signed (if (>= unsigned 32768) (- unsigned 65536) unsigned)]
                            (recur (inc channel) (+ total (/ signed 32768.0))))))]
          (aset-double out frame (/ total channels))))
      out)))

(defn create-manager
  "Create an inactive manager. Test seams: :open-source! receives validated
  config and returns read!/close!; :now-ms supplies monotonic milliseconds.
  No device/thread starts here. Call close! when the studio host shuts down."
  ([] (create-manager {}))
  ([options]
   (let [now-ms (get options :now-ms monotonic-ms)
         opener (get options :open-source! #(open-java-sound! % now-ms))]
     (when-not (and (ifn? now-ms) (ifn? opener))
       (throw (failure :invalid-options "Audio source and clock must be functions")))
     {:lock (Object.) :options {:open-source! opener :now-ms now-ms}
      :state (atom {:closed? false :run nil :public {:state :idle :running? false}})})))

(defn status [manager]
  (let [{:keys [closed? public]} @(:state manager)] (assoc public :closed? closed?)))

(defn- update-public! [manager run f & args]
  (swap! (:state manager) (fn [state]
                           (if (identical? run (:run state))
                             (update state :public #(apply f % args)) state))))

(defn- error-data [error]
  {:code (or (:audio/error (ex-data error)) :capture-error)
   :message (or (.getMessage ^Throwable error) (.getName (class error)))})

(defn- close-source! [run]
  (when-let [source @(:source run)]
    (when (compare-and-set! (:source-closed? run) false true)
      (try ((:close! source)) nil
           (catch Exception error
             (let [data (error-data error)] (reset! (:close-error run) data) data))))))

(defn- callback! [manager run name callback value]
  (when callback
    (try
      (let [result (callback value)]
        (when (and (= name :events!) (or (false? result) (= false (:accepted? result))))
          (update-public! manager run update-in [:metrics :dropped-events] (fnil + 0) (count value))))
      (catch Exception error
        (update-public! manager run
                        #(-> % (update-in [:metrics :callback-errors] (fnil inc 0))
                             (assoc :last-callback-error (assoc (error-data error) :callback name))))))))

(defn- read-block! [source run ^bytes buffer frame-size]
  (loop [offset 0 empty-reads 0 operations 0 dropped 0 discarded 0 overflow? false buffered 0]
    (cond
      @(:cancelled? run) {:bytes 0 :cancelled? true}
      (= offset (alength buffer)) {:bytes offset :dropped-frames dropped :discarded-frames discarded
                                  :possible-overflow? overflow? :buffered-frames buffered}
      (> operations (+ 32 (* 4 (alength buffer)))) (throw (failure :unstable-input "Audio source never completes a bounded block"))
      :else
      (let [remaining (- (alength buffer) offset)
            raw ((:read! source) buffer offset remaining)
            result (if (map? raw) raw {:bytes raw})
            n (:bytes result) skipped (get result :dropped-frames 0)
            _ (when-not (and (integer? n) (<= -1 n remaining) (integer? skipped) (<= 0 skipped)
                             (<= skipped Long/MAX_VALUE))
                (throw (failure :invalid-read "Audio source returned invalid byte/lost-frame counts")))
            _ (when (and (pos? skipped) (not (zero? (mod offset frame-size))))
                (throw (failure :unaligned-gap "Audio discontinuity split a PCM frame")))
            discard (if (pos? skipped) (quot offset frame-size) 0)
            _ (when (and (pos? skipped) (pos? n) (pos? offset)) (System/arraycopy buffer offset buffer 0 n))
            offset (if (pos? skipped) 0 offset)
            dropped (+ dropped skipped) discarded (+ discarded discard)
            overflow? (or overflow? (:possible-overflow? result)) buffered (get result :buffered-frames buffered)]
        (cond
          (= -1 n) {:bytes offset :eof? true :dropped-frames dropped :discarded-frames discarded
                    :possible-overflow? overflow? :buffered-frames buffered}
          (zero? n) (if (>= empty-reads 15) (throw (failure :stalled-input "Audio source repeatedly returned no data"))
                        (recur offset (inc empty-reads) (inc operations) dropped discarded overflow? buffered))
          :else (recur (+ offset n) 0 (inc operations) dropped discarded overflow? buffered))))))

(defn- finish! [manager run result callbacks]
  (close-source! run)
  (let [close-error @(:close-error run)
        result (cond-> result close-error (assoc :close-error close-error))]
    (update-public! manager run merge {:state (:status result) :running? false}
                    (select-keys result [:error :close-error]))
    (try (when-let [done! (:done! callbacks)] (done! result))
         (catch Exception _ nil)
         (finally
           ;; Keep the slot until completion callbacks finish: repeated starts
           ;; cannot accumulate blocked completion threads.
           (locking (:lock manager)
             (swap! (:state manager) #(if (identical? run (:run %)) (assoc % :run nil) %)))
           (deliver (:result run) result)))))

(defn- capture! [manager run config callbacks]
  (let [last-analyzer (volatile! (analysis/new-analyzer (:analyzer-options config)))
        result
        (try
          (if @(:cancelled? run)
            {:status :stopped}
            (let [source ((get-in manager [:options :open-source!]) config)
                  _ (reset! (:source run) source)
                  _ (when-not (and (ifn? (:read! source)) (ifn? (:close! source)))
                      (throw (failure :invalid-source "Audio source must provide read! and close!")))
                  now-ms (get-in manager [:options :now-ms])
                  origin (or (:started-at-ms source) (now-ms))
                  _ (when-not (and (finite? origin) (<= 0 origin))
                      (throw (failure :invalid-clock "Capture start time must be finite and nonnegative")))
                  frame-size (* 2 (:channels config)) buffer (byte-array (* frame-size (:block-frames config)))]
              (when @(:cancelled? run) (close-source! run))
              (update-public! manager run merge {:state :running :running? true :source (:description source)})
              (loop [cursor 0 state @last-analyzer timing-valid? true]
                (if @(:cancelled? run)
                  {:status :stopped :analyzer-state state}
                  (let [read (read-block! source run buffer frame-size) byte-count (:bytes read)
                        _ (when-not (zero? (mod byte-count frame-size))
                            (throw (failure :truncated-pcm "Audio input ended with a partial PCM frame")))
                        lost (get read :dropped-frames 0) discarded (get read :discarded-frames 0)
                        cursor (+ cursor lost discarded) n (quot byte-count frame-size)
                        possible-overflow? (:possible-overflow? read)
                        timing-valid? (and timing-valid? (not possible-overflow?))
                        state (if possible-overflow?
                                (assoc (analysis/new-analyzer (:analyzer-options config)) :sequence (:sequence state)) state)]
                    (update-public! manager run
                                    #(-> % (update-in [:metrics :dropped-input-frames] + lost)
                                         (update-in [:metrics :discarded-input-frames] + discarded)
                                         (update-in [:metrics :possible-overflows] + (if possible-overflow? 1 0))
                                         (assoc :sample-counter cursor :timing-valid? timing-valid?
                                                :buffered-frames (:buffered-frames read))))
                    (if (zero? n)
                      {:status (if @(:cancelled? run) :stopped :completed) :analyzer-state state}
                      (let [started (+ origin (* 1000.0 (/ cursor (:sample-rate config))))
                            end (+ started (* 1000.0 (/ n (:sample-rate config))))
                            arrived (now-ms)
                            _ (when-not (and (finite? arrived) (>= arrived (- end 1.0e-7)))
                                (throw (failure :capture-clock-skew "Capture clock precedes the sample counter; check source timing")))
                            clock (when (and timing-valid? (:clock! callbacks))
                                    (try ((:clock! callbacks))
                                         (catch Exception error
                                           (update-public! manager run assoc :last-clock-error (error-data error)) nil)))
                            analyzed (analysis/analyze state {:samples (decode-pcm16 buffer 0 byte-count (:channels config) (:big-endian? config))
                                                              :sample-rate (:sample-rate config) :started-at-ms started
                                                              :received-at-ms arrived :clock clock})
                            state (:state analyzed)
                            features (when-let [features (:features analyzed)]
                                       (assoc features :capture {:run-id (:run-id run) :timing-valid? timing-valid?
                                                                 :sample-counter (+ cursor n) :received-at-ms arrived}))
                            events (mapv #(update % :timing assoc :capture-clock-valid? timing-valid?
                                                 :timestamp-method :sample-counter-start-estimate) (:events analyzed))]
                        (vreset! last-analyzer state)
                        (update-public! manager run
                                        #(-> % (update-in [:metrics :frames] + n)
                                             (update-in [:metrics :blocks] inc)
                                             (assoc :features features :sample-counter (+ cursor n))))
                        (when-not @(:cancelled? run)
                          (when (seq events) (callback! manager run :events! (:events! callbacks) events))
                          (when features (callback! manager run :features! (:features! callbacks) features))
                          (when-let [subscriptions (:subscriptions callbacks)]
                            (let [published (analysis/publish! subscriptions events)]
                              (update-public! manager run update-in [:metrics :subscriber-errors] + (count (:errors published))))))
                        (if (:eof? read) {:status :completed :analyzer-state state}
                            (recur (+ cursor n) state timing-valid?)))))))))
          (catch Throwable error
            {:status (if @(:cancelled? run) :stopped :error)
             :error (when-not @(:cancelled? run) (error-data error)) :analyzer-state @last-analyzer}))]
    (finish! manager run result callbacks)))

(defn start!
  "Start explicitly selected capture on one daemon worker. Required callbacks:
  :events! receives an event vector and must enqueue quickly (false means queue
  rejection, counted as dropped events). Optional :clock! [] returns canonical
  transport, :features! receives latest features, :done! receives final result,
  :subscriptions is a fixed caller-owned analysis subscription map.

  Returns {:run-id :stop! :result promise}. Busy/closed/config errors are immediate;
  device-open/read failures resolve the result and appear in status. Slow host
  callbacks create capture backpressure; no unbounded audio/event queue is added."
  [manager config callbacks]
  (let [config (settings config)]
    (when-not (and (map? callbacks) (ifn? (:events! callbacks))
                   (every? #(or (nil? %) (ifn? %)) ((juxt :clock! :features! :done!) callbacks))
                   (or (nil? (:subscriptions callbacks)) (map? (:subscriptions callbacks))))
      (throw (failure :invalid-callbacks "Capture requires an event handler and optional callable callbacks")))
    (let [run (locking (:lock manager)
                (let [{:keys [closed? run]} @(:state manager)]
                  (when closed? (throw (failure :closed "Audio manager is closed")))
                  (when run (throw (failure :busy "Audio capture is already active")))
                  (let [run {:run-id (str (UUID/randomUUID)) :cancelled? (atom false) :source (atom nil)
                             :source-closed? (atom false) :close-error (atom nil) :result (promise) :thread (atom nil)}
                        thread (doto (Thread. #(capture! manager run config callbacks) "tolglow-audio-capture") (.setDaemon true))]
                    (reset! (:thread run) thread)
                    (swap! (:state manager) assoc :run run
                           :public {:state :starting :running? false :run-id (:run-id run) :config config
                                    :metrics {:frames 0 :blocks 0 :dropped-input-frames 0 :discarded-input-frames 0
                                              :possible-overflows 0 :dropped-events 0 :callback-errors 0 :subscriber-errors 0}})
                    run)))
          stop-run! (fn []
                      (reset! (:cancelled? run) true)
                      (update-public! manager run assoc :state :stopping)
                      (let [error (close-source! run)]
                        (.interrupt ^Thread @(:thread run))
                        (cond-> {:stopping? true :run-id (:run-id run)} error (assoc :close-error error))))]
      (.start ^Thread @(:thread run))
      {:run-id (:run-id run) :stop! stop-run! :result (:result run)})))

(defn stop!
  "Cancel capture, close the source to unblock reads, and wait at most timeout-ms
  (default 1000) for worker completion. Calling repeatedly is harmless. A source's
  close! must itself return promptly. No device is opened or restarted here."
  ([manager] (stop! manager 1000))
  ([manager timeout-ms]
   (when-not (and (integer? timeout-ms) (<= 0 timeout-ms 10000))
     (throw (failure :invalid-timeout "Audio stop timeout must be 0..10000 ms")))
   (if-let [run (:run @(:state manager))]
     (do
       (reset! (:cancelled? run) true)
       (update-public! manager run assoc :state :stopping)
       (let [close-error (close-source! run) thread ^Thread @(:thread run)]
         (.interrupt thread)
         (when-not (identical? thread (Thread/currentThread))
           (try (when (pos? timeout-ms) (.join thread (long timeout-ms)))
                (catch InterruptedException _ (.interrupt (Thread/currentThread)))))
         (cond-> {:stopped? (realized? (:result run)) :run-id (:run-id run)}
           close-error (assoc :close-error close-error))))
     {:stopped? true})))

(defn close! [manager]
  (locking (:lock manager) (swap! (:state manager) assoc :closed? true))
  (assoc (stop! manager) :closed? true))
