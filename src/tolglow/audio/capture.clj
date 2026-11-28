(ns tolglow.audio.capture
  "Explicit JavaSound capture boundary for the functional audio analyzer. One
  bounded worker owns each manager's stream and analyzer state. Requiring this
  namespace, creating a manager and listing inputs never open a device."
  (:require [tolglow.audio.drift :as drift]
            [tolglow.audio.pcm :as pcm]
            [tolglow.audio.shape :as shape]
            [tolglow.audio.stems :as stems]
            [tolglow.audio.shape-worker :as shape-worker]
            [tolglow.integrations.analysis :as analysis]
            [tolglow.waveforms :as waveforms])
  (:import [javax.sound.sampled AudioSystem AudioFormat AudioFormat$Encoding AudioInputStream DataLine$Info TargetDataLine]
           [java.io File]
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
        {:keys [channels input-channel mixer-index file]} opts]
    (when-not (and (pcm/valid-layout? channels input-channel)
                   (or (nil? input-channel) (contains? config :channels)))
      (throw (failure :invalid-config "Use mono or stereo, or specify 1–64 PCM channels and an input channel within that range")))
    (when-not (and (finite? (:sample-rate opts)) (<= 8000 (:sample-rate opts) 192000)
                   (integer? (:block-frames opts)) (<= 1 (:block-frames opts) 16384)
                   (integer? (:buffer-blocks opts)) (<= 2 (:buffer-blocks opts) 64)
                   (boolean? (:big-endian? opts))
                   (or (and (integer? mixer-index) (<= 0 mixer-index Integer/MAX_VALUE) (not (:default? opts)) (nil? file))
                       (and (nil? mixer-index) (true? (:default? opts)) (nil? file))
                       ;; A recorded file replays like an input (#675).
                       (and (nil? mixer-index) (not (:default? opts)) (string? file) (<= 1 (count file) 4096)))
                   (or (nil? (:file-paced? opts)) (boolean? (:file-paced? opts)))
                   (or (nil? (:mixer-name opts)) (string? (:mixer-name opts))))
      (throw (failure :invalid-config "Choose an input and valid signed-16 PCM settings")))
    (analysis/new-analyzer (:analyzer-options opts))
    (cond-> (select-keys opts [:sample-rate :channels :block-frames :buffer-blocks :big-endian?
                               :analyzer-options :mixer-index :mixer-name :default? :file :file-paced?])
      (some? input-channel) (assoc :input-channel input-channel)
      (some? (:stems opts)) (assoc :stems (try (stems/validate-config (:stems opts) channels)
                                               (catch clojure.lang.ExceptionInfo error
                                                 (throw (failure :invalid-config (.getMessage error)))))))))

(defn validate-config
  "Pure capture configuration validation/normalization. Does not enumerate or
  open a device; safe for validating a portable connections document."
  [config] (settings config))

(defn- recording-file
  "The recording a :file input names, refused unless it is an existing regular
  WAV or AIFF file. A FIFO or device would block the capture worker forever,
  and JavaSound would otherwise also render MIDI and other formats."
  ^File [path]
  (let [file (.getAbsoluteFile (File. (str path)))]
    (when-not (re-find #"(?i)\.(wav|wave|aif|aiff|aifc)$" (.getName file))
      (throw (failure :file-format "A replayed recording must be a .wav or .aiff file")))
    (when-not (.isFile file)
      (throw (failure :file-format "The recording is not an existing regular file")))
    file))

(defn file-format
  "Read a recording's sample rate, channel count and length without opening a
  device. Use it to fill a :file capture configuration."
  [path]
  (let [file-format (AudioSystem/getAudioFileFormat (recording-file path))
        format (.getFormat file-format)
        frames (.getFrameLength file-format)]
    {:sample-rate (double (.getSampleRate format)) :channels (.getChannels format)
     :frames (when (pos? frames) frames)}))

(defn file-config
  "A :file capture configuration for a recording, with its own sample rate and
  channel count. overrides may add :input-channel, :stems and the rest."
  ([path] (file-config path {}))
  ([path overrides]
   (let [{:keys [sample-rate channels]} (file-format path)]
     (settings (merge {:file (str path) :sample-rate sample-rate :channels channels}
                      (when (> channels 2) {:input-channel 1})
                      overrides)))))

(defn- open-file!
  "Replay a recording through the capture contract, converted to signed-16
  PCM. Paced by default, so the show hears it in real time; :file-paced? false
  reads as fast as the worker can (offline analysis)."
  [{:keys [file sample-rate channels big-endian? file-paced?]} now-ms]
  (let [source (AudioSystem/getAudioInputStream (recording-file file))
        format (.getFormat source)
        _ (when-not (and (== (double sample-rate) (double (.getSampleRate format))) (= channels (.getChannels format)))
            (.close source)
            (throw (failure :file-format (str "The recording is " (.getSampleRate format) " Hz with " (.getChannels format)
                                              " channels; configure the same rate and channel count"))))
        target (AudioFormat. (float sample-rate) 16 (int channels) true (boolean big-endian?))
        ^AudioInputStream stream (if (= target format) source
                                     (try (AudioSystem/getAudioInputStream target source)
                                          (catch IllegalArgumentException _
                                            (.close source)
                                            (throw (failure :file-format "The recording cannot be converted to signed 16-bit PCM")))))
        frame-size (* 2 channels) read-frames (atom 0)
        paced? (not (false? file-paced?))
        frames (.getFrameLength stream)
        ;; An unpaced replay is analysis of audio that has already happened:
        ;; place its first sample one recording length in the past, so every
        ;; block has arrived by the time it is read.
        started-at-ms (if paced? (now-ms)
                          (if (pos? frames) (- (now-ms) (* 1000.0 (/ frames (double sample-rate))))
                              (do (.close stream)
                                  (throw (failure :file-format "An unpaced replay needs a recording of known length")))))]
    {:started-at-ms started-at-ms
     :description {:name (.getName (File. ^String file)) :format :signed-16-pcm :timestamp-method :file-sample-counter}
     :read! (fn [^bytes bytes offset length]
              (when paced?
                ;; Deliver a block once its last sample would have been heard.
                (let [due (+ started-at-ms (* 1000.0 (/ (+ @read-frames (quot length frame-size)) (double sample-rate))))
                      wait (long (Math/ceil (- due (now-ms))))]
                  (when (pos? wait) (Thread/sleep wait))))
              (let [n (.read stream bytes (int offset) (int (* frame-size (quot length frame-size))))]
                (when (pos? n) (swap! read-frames + (quot n frame-size)))
                n))
     :close! (fn [] (.close stream))}))

(declare open-line!)

(defn open-java-sound!
  "Explicitly open the selected signed-16 PCM input. Called only by start!.
  A :file configuration replays a recording instead and opens no device.
  Injectable sources use the same {:read! [bytes offset length] :close! []}
  contract. A read may return byte count, -1 EOF, or {:bytes n :dropped-frames n
  :possible-overflow? bool}. Report exact dropped frames only when known.
  Optional :started-at-ms stamps the first sample in the manager's clock, and
  :sample-clock :device says the samples follow a device clock that may drift
  against it, so capture corrects their timestamps (`tolglow.audio.drift`);
  the default, :host, trusts the sample counter as it is."
  ([config] (open-java-sound! config monotonic-ms))
  ([config now-ms]
   (if (:file config)
     (open-file! (settings config) now-ms)
     (open-line! (settings config) now-ms))))

(defn- open-line! [config now-ms]
  (let [{:keys [sample-rate channels block-frames buffer-blocks big-endian? mixer-index mixer-name]} config
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
         ;; The line's sample clock is the device crystal, which drifts against
         ;; the host clock; capture tracks and corrects that (#912).
         {:started-at-ms started-at-ms :sample-clock :device
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
       (catch Throwable error (.close line) (throw error)))))

(defn- pcm16-sample ^double [^bytes bytes ^long at big-endian?]
  (let [a (aget bytes at) b (aget bytes (inc at))
        signed (if big-endian?
                 (+ (bit-shift-left a 8) (bit-and 255 b))
                 (+ (bit-and 255 a) (bit-shift-left b 8)))]
    (/ signed 32768.0)))

(defn decode-pcm16
  "Decode complete interleaved signed-16 PCM frames into a new mono double
  array. An explicit one-based input channel selects that channel. Without a
  selection, mono passes through and stereo averages left and right."
  ([bytes offset byte-count channels big-endian?]
   (decode-pcm16 bytes offset byte-count channels big-endian? nil))
  ([^bytes bytes offset byte-count channels big-endian? input-channel]
   (when-not (pcm/valid-layout? channels input-channel)
     (throw (failure :invalid-pcm "Invalid PCM width or input channel")))
   (let [frame-size (* channels 2)
         limit (alength bytes)]
     (when-not (and (boolean? big-endian?) (integer? offset) (integer? byte-count)
                    (<= 0 offset limit) (<= 0 byte-count (- limit offset))
                    (zero? (mod byte-count frame-size)))
       (throw (failure :invalid-pcm "PCM data must contain complete interleaved frames")))
     (let [frames (quot byte-count frame-size)
           out (double-array frames)
           channel-offset (* 2 (dec (or input-channel 1)))
           stereo-average? (and (nil? input-channel) (= 2 channels))]
       (dotimes [frame frames]
         (let [at (+ offset channel-offset (* frame frame-size))
               sample (pcm16-sample bytes at big-endian?)]
           (aset-double out frame (if stereo-average?
                                    (* 0.5 (+ sample (pcm16-sample bytes (+ at 2) big-endian?)))
                                    sample))))
       out))))

(defn create-manager
  "Create an inactive manager. Optional :config exposes saved settings while
  idle. Test seams: :open-source! receives validated
  config and returns read!/close!; :now-ms supplies monotonic milliseconds.
  No device/thread starts here. Call close! when the studio host shuts down."
  ([] (create-manager {}))
  ([options]
   (let [config (some-> (:config options) settings)
         now-ms (get options :now-ms monotonic-ms)
         opener (get options :open-source! #(open-java-sound! % now-ms))]
     (when-not (and (ifn? now-ms) (ifn? opener))
       (throw (failure :invalid-options "Audio source and clock must be functions")))
     {:lock (Object.) :options {:open-source! opener :now-ms now-ms}
      :state (atom {:closed? false :run nil :public (cond-> {:state :idle :running? false}
                                                      config (assoc :config config))})})))

(defn status [manager]
  (let [{:keys [closed? public]} @(:state manager)] (assoc public :closed? closed?)))

(defn waveform "Return the current bounded PCM projection without opening an input." [manager]
  (or (:waveform (status manager)) (waveforms/capture-view (waveforms/new-capture))))

(def history-options
  "A longer, coarser summary of the same PCM for the musical timeline: 512
  columns of 60 ms, about 30 seconds."
  {:max-bins 512 :bin-ms 60.0})

(defn history-waveform
  "Return the long capture history in the same shape as waveform. It is kept
  beside the public status, so status readers do not carry it."
  [manager]
  (waveforms/capture-view (or (:history @(:state manager)) (waveforms/new-capture history-options))))

(defn active-run-id [manager] (get-in @(:state manager) [:run :run-id]))

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

(defn advance-shapes
  "The capture thread's share of audio-shaped LFOs (#676): feed one block to
  the extractor bank for the requested specs ({lfo-id spec-with-:digest}; a
  spec that fails validation is skipped). Returns [bank jobs]. Window-end
  analysis is not done here; hand the jobs to a `shape-worker`."
  [bank specs block]
  (let [specs (into {} (filter (fn [[_ spec]] (empty? (shape/validate-spec (dissoc spec :digest)))))
                    (when (map? specs) specs))
        {:keys [bank jobs]} (shape/advance-bank (or bank (shape/new-bank (:sample-rate block))) specs block)]
    [bank jobs]))

(defn- capture! [manager run config]
  (let [last-analyzer (volatile! (analysis/new-analyzer (:analyzer-options config)))
        capture-state (volatile! (waveforms/new-capture))
        history-state (volatile! (waveforms/new-capture history-options))
        stem-state (volatile! (some-> (:stems config) stems/new-state))
        shape-state (volatile! nil)
        ;; Window-end analysis runs here, never on the capture thread.
        shapes (shape-worker/start!
                (fn [result]
                  (when-not @(:cancelled? run)
                    (callback! manager run :shapes! (:shapes! @(:callbacks run))
                               (update-vals result #(assoc % :run-id (:run-id run))))))
                (fn [error] (update-public! manager run update-in [:metrics :callback-errors] (fnil inc 0))))
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
                  _ (when-not (contains? #{nil :host :device} (:sample-clock source))
                      (throw (failure :invalid-source "Audio source :sample-clock must be :host or :device")))
                  device-clock? (= :device (:sample-clock source))
                  _ (when (and device-clock? (> (:block-frames config) drift/max-block-frames))
                      (throw (failure :invalid-config (str "A device input corrects its clock drift in blocks of at most "
                                                           drift/max-block-frames " frames"))))
                  ;; Owned by this worker: delay-locked drift correction (#912).
                  tracker (volatile! (when device-clock? (drift/new-tracker (:sample-rate config) (:block-frames config))))
                  frame-size (* 2 (:channels config)) buffer (byte-array (* frame-size (:block-frames config)))]
              (when @(:cancelled? run) (close-source! run))
              (update-public! manager run merge {:state :running :running? true :source (:description source)})
              (loop [cursor 0 state @last-analyzer stream-valid? true]
                (if @(:cancelled? run)
                  {:status :stopped :analyzer-state state}
                  (let [callbacks @(:callbacks run)
                        read (read-block! source run buffer frame-size) byte-count (:bytes read)
                        _ (when-not (zero? (mod byte-count frame-size))
                            (throw (failure :truncated-pcm "Audio input ended with a partial PCM frame")))
                        lost (get read :dropped-frames 0) discarded (get read :discarded-frames 0)
                        cursor (+ cursor lost discarded) n (quot byte-count frame-size)
                        possible-overflow? (:possible-overflow? read)
                        stream-valid? (and stream-valid? (not possible-overflow?))
                        timing-valid? (and stream-valid? (or (nil? @tracker) (drift/valid? @tracker)))
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
                      (let [nominal-start (+ origin (* 1000.0 (/ cursor (:sample-rate config))))
                            nominal-end (+ nominal-start (* 1000.0 (/ n (:sample-rate config))))
                            arrived (now-ms)
                            _ (when-not (finite? arrived)
                                (throw (failure :invalid-clock "Capture clock must be finite")))
                            ;; The delay to the newest captured frame: what the line
                            ;; still buffers was captured after this block ended.
                            buffered-ms (* 1000.0 (/ (or (:buffered-frames read) 0) (:sample-rate config)))
                            _ (when-let [t @tracker]
                                (vreset! tracker (drift/observe t (- nominal-end origin) (- arrived nominal-end buffered-ms))))
                            correction (if-let [t @tracker] (:correction-ms t) 0.0)
                            drift-metrics (if-let [t @tracker] (drift/metrics t) {:tracking? false :sample-clock :host})
                            timing-valid? (and stream-valid? (or (nil? @tracker) (drift/valid? @tracker)))
                            started (+ nominal-start correction)
                            end (+ started (* 1000.0 (/ n (:sample-rate config))))
                            _ (when-not (>= arrived (- end 1.0e-7))
                                (throw (failure :capture-clock-skew "Capture clock precedes the sample counter; check source timing")))
                            clock (when (and timing-valid? (:clock! callbacks))
                                    (try ((:clock! callbacks))
                                         (catch Exception error
                                           (update-public! manager run assoc :last-clock-error (error-data error)) nil)))
                            samples (decode-pcm16 buffer 0 byte-count (:channels config)
                                                  (:big-endian? config) (:input-channel config))
                            block {:samples samples :sample-rate (:sample-rate config)
                                   :channels (:channels config) :input-channel (:input-channel config)
                                   :source-id (:run-id run)
                                   :started-at-ms started :clock clock :timing-valid? timing-valid?
                                   :discontinuity? (or possible-overflow? (pos? lost) (pos? discarded))}
                            specs (when-let [specs! (:shape-specs! callbacks)]
                                    (try (specs!) (catch Exception _ nil)))
                            [bank jobs] (advance-shapes @shape-state specs block)
                            captured (waveforms/capture-block @capture-state block)
                            history (waveforms/capture-block @history-state block)
                            waveform (waveforms/capture-view captured)
                            analyzed (analysis/analyze state {:samples samples
                                                              :sample-rate (:sample-rate config) :started-at-ms started
                                                              :received-at-ms arrived :clock clock})
                            state (:state analyzed)
                            ;; Each named stem reads its own channel (#675).
                            stemmed (when-let [stem-analyzer @stem-state]
                                      (stems/analyze stem-analyzer
                                                     {:blocks (into {} (map (fn [{:keys [id channel]}]
                                                                              [id (decode-pcm16 buffer 0 byte-count (:channels config)
                                                                                                (:big-endian? config) channel)]))
                                                                    (:stems config))
                                                      :sample-rate (:sample-rate config) :started-at-ms started
                                                      :received-at-ms arrived :clock clock}))
                            features (when-let [features (:features analyzed)]
                                       (cond-> (assoc features :capture {:run-id (:run-id run) :timing-valid? timing-valid?
                                                                         :sample-counter (+ cursor n) :received-at-ms arrived})
                                         stemmed (assoc :stems (:features stemmed))))
                            events (mapv (fn [event]
                                           (cond-> (update event :timing assoc :capture-clock-valid? timing-valid?
                                                           :timestamp-method :sample-counter-start-estimate)
                                             (= :audio/section-change (:type event))
                                             (assoc :capture-run-id (:run-id run))))
                                         (into (:events analyzed) (:events stemmed)))]
                        (vreset! last-analyzer state)
                        (when stemmed (vreset! stem-state (:state stemmed)))
                        (vreset! capture-state captured)
                        (vreset! history-state history)
                        (vreset! shape-state bank)
                        (swap! (:state manager)
                               (fn [state]
                                 (if (identical? run (:run state))
                                   (-> state
                                       (assoc :history history)
                                       (update :public
                                               #(-> % (update-in [:metrics :frames] + n)
                                                    (update-in [:metrics :blocks] inc)
                                                    (assoc-in [:metrics :clock-drift] drift-metrics)
                                                    (assoc :features features :sample-counter (+ cursor n) :waveform waveform
                                                           :timing-valid? timing-valid? :block-end-ms end))))
                                   state)))
                        (when-not @(:cancelled? run)
                          (when (seq events) (callback! manager run :events! (:events! callbacks) events))
                          (when features (callback! manager run :features! (:features! callbacks) features))
                          ((:submit! shapes) jobs)
                          (when-let [subscriptions (:subscriptions callbacks)]
                            (let [published (analysis/publish! subscriptions events)]
                              (update-public! manager run update-in [:metrics :subscriber-errors] + (count (:errors published))))))
                        (if (:eof? read) {:status :completed :analyzer-state state}
                            (recur (+ cursor n) state stream-valid?)))))))))
          (catch Throwable error
            {:status (if @(:cancelled? run) :stopped :error)
             :error (when-not @(:cancelled? run) (error-data error)) :analyzer-state @last-analyzer}))]
    ;; Shapes of windows the input completed are still published; a stop drops them.
    ((:stop! shapes) (= :completed (:status result)) 2000)
    (finish! manager run result @(:callbacks run))))

(defn- validate-callbacks! [callbacks]
  (when-not (and (map? callbacks) (ifn? (:events! callbacks))
                 (every? #(or (nil? %) (ifn? %)) ((juxt :clock! :features! :done! :shape-specs! :shapes!) callbacks))
                 (or (nil? (:subscriptions callbacks)) (map? (:subscriptions callbacks))))
    (throw (failure :invalid-callbacks "Capture requires an event handler and optional callable callbacks")))
  callbacks)

(defn rebind!
  "Replace callbacks for the next audio block without reopening the source or
  waiting for reads. The caller invalidates old callback ownership first; an
  already analyzed block may still finish through those old guarded callbacks."
  [manager callbacks]
  (validate-callbacks! callbacks)
  (locking (:lock manager)
    (let [{:keys [closed? run]} @(:state manager)]
      (when closed? (throw (failure :closed "Audio manager is closed")))
      (if (and run (not @(:cancelled? run)))
        (do (reset! (:callbacks run) callbacks) {:rebound? true :run-id (:run-id run)})
        {:rebound? false :reason :inactive}))))

(defn start!
  "Start explicitly selected capture on one daemon worker. Required callbacks:
  :events! receives an event vector and must enqueue quickly (false means queue
  rejection, counted as dropped events). Optional :clock! [] returns canonical
  transport, :features! receives latest features, :done! receives final result,
  :shape-specs! [] returns audio-shape extractor specs and :shapes! receives the
  newest extraction per LFO when a window completes (#676),
  :subscriptions is a fixed caller-owned analysis subscription map.

  Returns {:run-id :stop! :result promise}. Busy/closed/config errors are immediate;
  device-open/read failures resolve the result and appear in status. Slow host
  callbacks create capture backpressure; no unbounded audio/event queue is added."
  [manager config callbacks]
  (let [config (settings config)]
    (validate-callbacks! callbacks)
    (let [run (locking (:lock manager)
                (let [{:keys [closed? run]} @(:state manager)]
                  (when closed? (throw (failure :closed "Audio manager is closed")))
                  (when run (throw (failure :busy "Audio capture is already active")))
                  (let [run {:run-id (str (UUID/randomUUID)) :cancelled? (atom false) :source (atom nil)
                             :callbacks (atom callbacks)
                             :source-closed? (atom false) :close-error (atom nil) :result (promise) :thread (atom nil)}
                        thread (doto (Thread. #(capture! manager run config) "tolglow-audio-capture") (.setDaemon true))]
                    (reset! (:thread run) thread)
                    (swap! (:state manager) assoc :run run :history nil
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
  ([manager timeout-ms] (stop! manager timeout-ms ::any))
  ([manager timeout-ms expected-run-id]
   (when-not (and (integer? timeout-ms) (<= 0 timeout-ms 10000))
     (throw (failure :invalid-timeout "Audio stop timeout must be 0..10000 ms")))
   (if-let [run (let [run (:run @(:state manager))]
                 (when (or (= ::any expected-run-id) (= expected-run-id (:run-id run))) run))]
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
