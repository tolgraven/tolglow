(ns tolglow.qa.audio-lfo
  "Offline report for audio-shaped LFOs (#676). Replays the deterministic
  passages (dry sine, delayed synth, plucked phrase) at two tempos through the
  real extractor, owner admission and morph, and writes:

  - `report.edn`: per passage, mode and tempo, every window's status, level,
    confidence, pitch, latency and table jump, the largest frame-to-frame LFO
    step overall and on commit frames, and the extraction/sampling cost;
  - one SVG per passage and tempo: the band-isolated input envelope over the
    bars, each committed shape, and the LFO as the owner samples it per frame;
  - the passages as 16-bit WAV files, so the same material can be replayed
    through a real interface or Live;
  - the six-LFO capture budget at 70 and 180 BPM, the cycle-mode transport
    offset sweep (#912) through the replay and through the real capture and
    shape workers on simulated time, and ten-minute drift runs of a device
    sample clock against the host clock.

  lein run -m tolglow.qa.audio-lfo <out-dir>

  No device, port or network is opened."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [tolglow.audio-lfo :as lfo]
            [tolglow.audio.capture :as capture]
            [tolglow.audio.fixtures :as fixtures]
            [tolglow.audio.shape :as shape]
            [tolglow.audio.shape-worker :as shape-worker]))

(def tempos [84.0 172.0])

(def modes [:envelope :cycle])

(def bars 8)

(def settings
  "Per mode: the envelope of the bass band scanned once per bar, and one pitch
  cycle of the full-range input scanned once per beat. The 60–250 Hz band
  strips a bass synth's harmonics, so its cycle is close to a sine."
  {:envelope {:config {:extract :envelope :band :bass} :scan 4.0}
   :cycle {:config {:extract :cycle :band :full} :scan 1.0}})

(defn- round [x digits]
  (when (number? x) (let [scale (Math/pow 10 digits)] (/ (Math/round (* scale (double x))) scale))))

(defn- band-envelope
  "RMS of the band-isolated passage in columns of `column-ms`."
  [samples sample-rate band column-ms]
  (let [[filtered] (shape/filter-samples (shape/filter-design band sample-rate) [] samples)
        ^doubles filtered filtered
        size (long (* sample-rate (/ column-ms 1000.0)))]
    (mapv (fn [start]
            (let [end (min (alength filtered) (+ start size))]
              (Math/sqrt (/ (loop [i start sum 0.0] (if (= i end) sum (recur (inc i) (+ sum (* (aget filtered i) (aget filtered i))))))
                            (max 1 (- end start))))))
          (range 0 (alength filtered) size))))

(defn- run [kind mode bpm]
  (let [samples (fixtures/passage kind {:bpm bpm :bars bars})
        ;; Warm the JIT once so the measured pass is not the first.
        options (assoc (get settings mode) :bpm bpm)
        _ (lfo/replay samples options)
        result (lfo/replay samples options)]
    {:passage kind :extract mode :bpm bpm :samples samples :result result}))

(defn- summary [{:keys [passage extract bpm result]}]
  (let [{:keys [commits steps extract-us sample-us blocks]} result
        rounded #(update-vals (dissoc % :count) (fn [v] (round v 2)))]
    {:passage passage :extract extract :bpm bpm :blocks blocks
     :windows (mapv (fn [{:keys [window status committed? level-db confidence frequency-hz latency-ms table-jump]}]
                      (cond-> {:bars [(round (/ (:start-beat window) 4.0) 3) (round (/ (:end-beat window) 4.0) 3)]
                               :status status :committed? committed? :level-db (round level-db 1)
                               :latency-ms (round latency-ms 2) :table-jump (round table-jump 9)}
                        confidence (assoc :confidence (round confidence 3))
                        frequency-hz (assoc :frequency-hz (round frequency-hz 2))))
                    commits)
     :max-frame-step (round (:max steps) 4)
     :max-frame-step-on-commit (round (:max-on-commit steps) 4)
     :max-table-jump (round (reduce max 0.0 (map :table-jump commits)) 9)
     :latency-ms {:min (round (reduce min (map :latency-ms commits)) 2) :max (round (reduce max (map :latency-ms commits)) 2)}
     :extract-us-per-block (rounded extract-us)
     :finish-us-per-window (rounded (:finish-us result))
     :sample-us-per-frame (rounded sample-us)}))

;; SVG ------------------------------------------------------------------------------

(defn- polyline [points class]
  (str "<polyline class=\"" class "\" points=\""
       (str/join " " (map (fn [[x y]] (str (round x 2) "," (round y 2))) points)) "\"/>"))

(defn- svg [kind bpm runs]
  (let [width 1200 lane 150 top 40
        sample-rate 48000.0
        samples (:samples (first runs))
        beats (* 4.0 bars)
        x (fn [beat] (* width (/ beat beats)))
        envelope (band-envelope samples sample-rate :bass 10.0)
        peak (max 1.0e-9 (reduce max envelope))
        env-points (map-indexed (fn [i v] [(* width (/ i (double (dec (count envelope))))) (- (+ top lane) (* (- lane 10) (/ v peak)))]) envelope)
        grid (str/join (for [b (range 0 (inc beats))]
                         (str "<line class=\"" (if (zero? (mod b 4)) "bar" "beat") "\" x1=\"" (x b) "\" x2=\"" (x b)
                              "\" y1=\"" top "\" y2=\"" (+ top (* 3 lane) 40) "\"/>")))
        lfo-lane (fn [index {:keys [extract result]}]
                   (let [y0 (+ top lane 20 (* index (+ lane 10)))
                         trace (map (fn [[beat value]] [(x beat) (- (+ y0 lane) (* (- lane 10) value))]) (:frames result))
                         shapes (for [{:keys [beat committed? status]} (:commits result)]
                                  (str "<line class=\"commit " (name status) (when committed? " committed") "\" x1=\"" (x beat) "\" x2=\"" (x beat)
                                       "\" y1=\"" y0 "\" y2=\"" (+ y0 lane) "\"/>"))]
                     (str "<text x=\"6\" y=\"" (+ y0 14) "\">LFO from " (name extract) " shapes of the "
                          (shape/band-label (get-in settings [extract :config :band])) " band, scanned every "
                          (long (get-in settings [extract :scan])) " beat(s)</text>"
                          (str/join shapes) (polyline trace "lfo"))))]
    (str "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 " width " " (+ top (* 3 lane) 60) "\" width=\"" width "\">"
         "<style>text{font:12px system-ui;fill:#c9d2c4}polyline{fill:none;stroke-width:1.4}"
         ".env{stroke:#8fb7d8}.lfo{stroke:#b7d79b}line.beat{stroke:#1f271c}line.bar{stroke:#3d4a36}"
         "line.commit{stroke:#6d5a3a;stroke-dasharray:3 3}line.commit.committed{stroke:#e2c48f;stroke-dasharray:none}"
         "line.commit.silent{stroke:#744e3f}</style>"
         "<rect width=\"100%\" height=\"100%\" fill=\"#0d120c\"/>"
         "<text x=\"6\" y=\"20\">" (name kind) " · " (long bpm) " BPM · " bars " bars · gold lines: committed shapes, dashed: held windows</text>"
         grid
         "<text x=\"6\" y=\"" (+ top 14) "\">Bass band input level, 10 ms RMS</text>"
         (polyline env-points "env")
         (str/join (map-indexed lfo-lane runs))
         "</svg>")))

;; Simulated input through the real capture and shape workers --------------------------

(defn- passage-at
  "Linear interpolation of a looped passage at a fractional sample index."
  ^double [^doubles passage ^double index]
  (let [n (alength passage)
        i (long (Math/floor index))
        f (- index i)
        a (aget passage (int (mod i n)))
        b (aget passage (int (mod (inc i) n)))]
    (+ a (* f (- b a)))))

(defn simulated-input
  "A capture source, host clock and transport on simulated time, for running
  the real capture worker and shape worker without a device or a wall clock
  (#912). The passage (mono, at :sample-rate, looped) plays in time with the
  transport, which starts at beat 0 when the first sample is captured. Options:

  - :ppm how fast the device's sample clock runs against the host clock
    (positive: more samples per host second);
  - :latency-ms and :jitter-ms, the delivery delay after a block's last sample
    (the jitter is uniform and seeded by :seed);
  - :clock-offset-ms how late the reported transport runs against the audio;
  - :backlog {:from-s :to-s :ms}: reads in that span arrive ms late and report
    those ms as :buffered-frames, like a line that fell behind;
  - :seconds of audio, :start-ms the host time of the first sample,
    :sample-clock (:device by default, so capture tracks drift).

  Returns {:now-ms :open-source! :clock! :real-ms}. :real-ms gives the host time
  at which a sample index was really captured; :clock! reports the transport
  at the current simulated time. Reads never sleep: each read moves the
  simulated clock to its block's arrival."
  [^doubles passage {:keys [bpm sample-rate ppm latency-ms jitter-ms clock-offset-ms seconds start-ms seed sample-clock backlog]
                     :or {sample-rate 48000.0 ppm 0.0 latency-ms 4.0 jitter-ms 0.0 clock-offset-ms 0.0
                          start-ms 1000.0 seed 912 sample-clock :device}}]
  (let [sample-rate (double sample-rate) bpm (double bpm)
        device-rate (* sample-rate (+ 1.0 (* 1.0e-6 (double ppm))))
        real-ms (fn [index] (+ (double start-ms) (* 1000.0 (/ (double index) device-rate))))
        total (long (* (double seconds) device-rate))
        per-beat (* sample-rate (/ 60.0 bpm))
        now (atom (double start-ms))
        random (java.util.Random. (long seed))
        transport-beat (fn [at-ms] (* (- (double at-ms) (double start-ms)) (/ bpm 60000.0)))]
    {:real-ms real-ms
     :now-ms (fn [] @now)
     :clock! (fn [] (let [at @now]
                      {:time-ms at :beat (transport-beat (- at (double clock-offset-ms))) :bpm bpm :playing? true
                       :meter {:beats-per-bar 4} :source :manual}))
     :open-source! (fn [_config]
                     (let [frames (atom 0)]
                       {:started-at-ms (double start-ms) :sample-clock sample-clock
                        :description {:name "Simulated device" :format :signed-16-pcm}
                        :read! (fn [^bytes out offset length]
                                 (let [start @frames
                                       count (min (quot length 2) (- total start))]
                                   (if (<= count 0)
                                     -1
                                     (do (dotimes [i count]
                                           (let [index (+ start i)
                                                 beat (transport-beat (real-ms index))
                                                 value (passage-at passage (* beat per-beat))
                                                 pcm (long (* 32767.0 (max -1.0 (min 1.0 value))))
                                                 at (+ offset (* 2 i))]
                                             (aset-byte out at (unchecked-byte (bit-and pcm 255)))
                                             (aset-byte out (inc at) (unchecked-byte (bit-shift-right pcm 8)))))
                                         (swap! frames + count)
                                         (let [end-ms (real-ms (+ start count))
                                               behind (if (and backlog (< (* 1000.0 (:from-s backlog)) (- end-ms (double start-ms))
                                                                          (* 1000.0 (:to-s backlog))))
                                                        (double (:ms backlog)) 0.0)
                                               arrival (+ end-ms (double latency-ms) behind
                                                          (* (double jitter-ms) (.nextDouble random)))]
                                           (swap! now max arrival)
                                           {:bytes (* 2 count)
                                            :buffered-frames (long (Math/floor (* behind (/ sample-rate 1000.0))))})))))
                        :close! (fn [])}))}))

(defn capture-run
  "Run a simulated input through a real capture manager and shape worker for
  one audio LFO config. Returns every published extraction, the capture
  status at the end, and the largest error between a block's end timestamp
  and its real capture time, over all blocks and after `:settle-s` seconds."
  [passage {:keys [config sample-rate block-frames settle-s] :or {sample-rate 48000.0 block-frames 480 settle-s 20.0}
            :as options}]
  (let [{:keys [now-ms open-source! clock! real-ms]} (simulated-input passage (assoc options :sample-rate sample-rate))
        model (lfo/configure {:transport {:beat 0.0 :bpm (:bpm options) :playing? true :meter {:beats-per-bar 4}}}
                             {:lfo-id :sim :config config})
        specs (lfo/extractor-specs model)
        extractions (atom [])
        errors (atom {:max 0.0 :settled 0.0 :blocks 0})
        manager (capture/create-manager {:now-ms now-ms :open-source! open-source!})
        observe! (fn [_]
                   (let [{:keys [block-end-ms sample-counter]} (capture/status manager)]
                     (when (and block-end-ms sample-counter)
                       (let [error (Math/abs (- (double block-end-ms) (double (real-ms sample-counter))))
                             settled? (>= (/ sample-counter sample-rate) settle-s)]
                         (swap! errors #(cond-> (-> % (update :max max error) (update :blocks inc))
                                          settled? (update :settled max error)))))))]
    (try
      (let [{:keys [result]} (capture/start! manager {:default? true :sample-rate sample-rate :channels 1 :block-frames block-frames}
                                             {:events! (constantly true) :clock! clock! :features! observe!
                                              :shape-specs! (constantly specs)
                                              :shapes! (fn [shapes] (swap! extractions conj (:sim shapes)))})
            outcome (deref result 600000 {:status :timeout})]
        {:result (select-keys outcome [:status :error]) :extractions @extractions :timestamp-error-ms @errors
         :status (dissoc (capture/status manager) :waveform :features :config)})
      (finally (capture/close! manager)))))

(def sweep-offsets
  "Transport offsets, ms: negative early (the audio late), positive late."
  [-30.0 -25.0 -20.0 -10.0 -5.0 0.0 1.0 2.0 5.0 8.0 10.0])

(def sweep-tempos [70.0 120.0 180.0])

(def expected-hz
  "The fundamental each passage holds at a bar's end. The plucked phrase ends
  its played bar on a three-note chord, whose common period YIN reports, so it
  has no single expected pitch."
  {:dry-sine 110.0 :delay-synth 82.5})

(def played-bar?
  "Which windows hold played music. The plucked phrase plays one bar and then
  rests for one, so its odd bars are rests: with the transport offset they
  hold the previous bar's damped ring or the next pluck and are not shaped."
  {:guitar-phrase #(even? (get-in % [:window :index]))})

(defn- window-summary
  ([windows] (window-summary nil windows))
  ([passage windows]
   (let [played? (get played-bar? passage #(not= :silent (:status %)))
         played (filter played? windows)]
     {:windows (count windows) :played (count played)
      :statuses (frequencies (map :status played))
      :rest-statuses (frequencies (map :status (remove played? windows)))
      :min-confidence (some->> (seq (keep :confidence played)) (reduce min))
      :hz (some->> (seq (keep :frequency-hz played)) ((juxt #(reduce min %) #(reduce max %))))})))

(defn offset-sweep
  "Cycle-mode extraction of every passage at each tempo and transport offset,
  through the offline replay and through the real capture and shape workers
  on simulated time. Returns one row per passage, tempo, offset and path."
  ([] (offset-sweep {}))
  ([{:keys [passages tempos offsets bars] :or {passages fixtures/passages tempos sweep-tempos offsets sweep-offsets bars 8}}]
   (vec (for [kind passages bpm tempos
              :let [samples (fixtures/passage kind {:bpm bpm :bars bars})
                    config {:extract :cycle :band :full}]
              offset offsets
              path [:replay :capture]]
          (let [windows (case path
                          :replay (:commits (lfo/replay samples {:bpm bpm :config config :clock-offset-ms offset}))
                          :capture (:extractions (capture-run samples {:bpm bpm :config config :clock-offset-ms offset
                                                                       :seconds (* bars 4.0 (/ 60.0 bpm))})))]
            (merge {:passage kind :bpm bpm :offset-ms offset :path path} (window-summary kind windows)))))))

(defn sweep-passes?
  "Every played window :ok with confidence ≥ 0.8 and, where the passage has
  one, the expected fundamental within 1 Hz."
  [{:keys [passage statuses min-confidence hz played]}]
  (and (pos? played) (= played (get statuses :ok 0)) (<= 0.8 min-confidence)
       (if-let [expected (expected-hz passage)] (every? #(< (Math/abs (- expected %)) 1.0) hz) true)))

(defn drift-run
  "The drift acceptance run: a device clock ppm off the host for minutes of
  simulated audio at 8 kHz, through real capture with drift tracking (or
  without, with :sample-clock :host), with a cycle LFO on the delayed synth."
  [ppm {:keys [minutes bpm sample-clock] :or {minutes 10.0 bpm 120.0 sample-clock :device} :as options}]
  (let [sample-rate 8000.0
        passage (fixtures/passage :delay-synth {:bpm bpm :bars 16 :sample-rate sample-rate})
        run (capture-run passage (merge {:bpm bpm :sample-rate sample-rate :block-frames 400 :ppm ppm :seconds (* 60.0 minutes)
                                         :latency-ms 4.0 :jitter-ms 3.0 :sample-clock sample-clock
                                         :config {:extract :cycle :band :full}}
                                        (select-keys options [:backlog :block-frames :latency-ms])))]
    (assoc run :summary (window-summary :delay-synth (:extractions run)))))

;; Capture-thread budget ------------------------------------------------------------

(def budget-lfos
  "Six cycle-mode LFOs on one bar grid, mixed bands, two sharing the bass band:
  every window closes on the same capture block."
  {:sub {:band :sub :extract :cycle} :bass {:band :bass :extract :cycle}
   :bass-fine {:band :bass :extract :cycle :points 63} :low-mid {:band :low-mid :extract :cycle}
   :mid {:band :mid :extract :cycle} :full {:band :full :extract :cycle}})

(defn capture-budget
  "Play a passage in real time, 480-frame blocks at 48 kHz, through the bank
  for six LFOs as the capture thread does, with the real shape worker
  finishing window-end jobs. Returns the capture-side cost per block (bank plus
  hand-off), the cost of the blocks that close windows, each LFO's results and
  the worst delay from a window's closing block to its published shape, against
  the window's length."
  [bpm bars]
  (let [sample-rate 48000.0 block 480
        samples (fixtures/passage :delay-synth {:bpm bpm :bars bars})
        specs (into {} (map (fn [[id spec]] [id (assoc spec :digest (hash spec))])) budget-lfos)
        submitted (atom {})
        delays (atom [])
        results (atom {})
        worker (shape-worker/start!
                (fn [result]
                  (doseq [[id extraction] result]
                    (swap! results update id (fnil conj []) (:status extraction))
                    (when-let [at (get @submitted [id (:seq extraction)])]
                      (swap! delays conj (/ (- (:finished-ns extraction) at) 1.0e6))))))
        origin (System/nanoTime)]
    (try
      (loop [start 0 bank (shape/new-bank sample-rate) costs [] edges []]
        (if (>= (+ start block) (alength samples))
          (do ((:stop! worker) true 5000)
              {:bpm bpm :lfos (count specs) :bands (count (distinct (map :band (vals specs))))
               :window-ms (* 4 (/ 60000.0 bpm))
               :block-us (lfo/timing costs) :closing-block-us (lfo/timing edges)
               :results (into (sorted-map) @results)
               :shape-delay-ms {:max (reduce max 0.0 @delays) :count (count @delays)}})
          (let [due (+ origin (long (* 1.0e9 (/ (+ start block) sample-rate))))]
            (loop [] (let [wait (- due (System/nanoTime))] (when (pos? wait) (Thread/sleep (max 0 (quot wait 1000000)) (int (mod wait 1000000))) (recur))))
            (let [t0 (System/nanoTime)
                  {:keys [bank jobs]} (shape/advance-bank bank specs
                                                          {:samples (java.util.Arrays/copyOfRange samples (int start) (int (+ start block)))
                                                           :sample-rate sample-rate :started-at-ms (* 1000.0 (/ start sample-rate))
                                                           :clock {:time-ms 0.0 :beat 0.0 :bpm bpm :playing? true}})
                  handed (System/nanoTime)
                  _ (doseq [job jobs] (swap! submitted assoc [(:id job) (:seq job)] handed))
                  _ ((:submit! worker) jobs)
                  cost (- (System/nanoTime) t0)]
              (recur (+ start block) bank (conj costs cost) (cond-> edges (seq jobs) (conj cost)))))))
      (finally ((:stop! worker) false 1000)))))

(defn- percentiles [nanos]
  (let [sorted (vec (sort nanos)) n (count sorted)
        at (fn [q] (/ (nth sorted (min (dec n) (long (Math/floor (* q n))))) 1000.0))]
    {:median (at 0.5) :p99 (at 0.99) :p999 (at 0.999) :max (/ (peek sorted) 1000.0) :count n}))

(defn capture-cost
  "The capture thread's cost per block, by thread CPU time and wall time, for
  the six budget LFOs on the delayed synth at 48 kHz in 480-frame blocks, run
  as fast as the worker goes (no pacing), through the whole capture loop:
  decode, analyzer, waveforms, the extractor bank, drift tracking and
  hand-off. Each block is timed between the source's reads, on the capture
  thread. `capture` names the capture functions, so another revision's loop
  can be measured in the same JVM. Returns microseconds."
  ([bpm seconds] (capture-cost {:create-manager capture/create-manager :start! capture/start! :close! capture/close!} bpm seconds))
  ([{:keys [create-manager start! close!]} bpm seconds]
   (let [sample-rate 48000.0
         passage (fixtures/passage :delay-synth {:bpm bpm :bars 8})
         {:keys [now-ms open-source! clock!]} (simulated-input passage {:bpm bpm :sample-rate sample-rate :seconds seconds})
         bean (java.lang.management.ManagementFactory/getThreadMXBean)
         cpu (atom []) wall (atom []) last (atom nil)
         timed (fn [config]
                 (let [source (open-source! config)]
                   (assoc source :read!
                          (fn [out offset length]
                            (let [c (.getCurrentThreadCpuTime bean) w (System/nanoTime)]
                              (when-let [[c0 w0] @last] (swap! cpu conj (- c c0)) (swap! wall conj (- w w0)))
                              (reset! last [c w])
                              ((:read! source) out offset length))))))
         specs (into {} (map (fn [[id spec]] [id (assoc spec :digest (hash spec))])) budget-lfos)
         manager (create-manager {:now-ms now-ms :open-source! timed})]
     (try
       (deref (:result (start! manager {:default? true :sample-rate sample-rate :channels 1 :block-frames 480}
                               {:events! (constantly true) :clock! clock! :shape-specs! (constantly specs) :shapes! (fn [_])}))
              600000 nil)
       {:bpm bpm :cpu-us (percentiles @cpu) :wall-us (percentiles @wall)}
       (finally (close! manager))))))

(defn- drift-summary [ppm sample-clock]
  (let [{:keys [result status timestamp-error-ms summary]} (drift-run ppm {:sample-clock sample-clock})]
    {:ppm ppm :sample-clock sample-clock :minutes 10.0 :result result
     :timing-valid? (:timing-valid? status) :clock-drift (get-in status [:metrics :clock-drift])
     :timestamp-error-ms timestamp-error-ms :windows summary}))

(defn -main [& [out-dir]]
  (let [directory (io/file (or out-dir "target/audio-lfo-676"))
        runs (vec (for [kind fixtures/passages bpm tempos mode modes] (run kind mode bpm)))
        budget (mapv #(capture-budget % 5) [70.0 180.0])
        sweep (offset-sweep)
        drift (mapv #(apply drift-summary %) [[-100.0 :device] [100.0 :device] [-100.0 :host] [-1000.0 :device]])
        cost (mapv #(capture-cost % 60.0) [70.0 180.0])
        report {:issue [676 912] :settings settings :bars bars :capture-budget budget :sample-rate 48000.0 :block-frames 480 :fps 60.0
                :offset-sweep (mapv #(assoc % :passes? (sweep-passes? %)) sweep) :drift drift :capture-cost cost
                :clojure (clojure-version) :java (System/getProperty "java.version")
                :excluded "device capture and decode, the capture worker's other analysis, owner scheduling, IPC and rendering"
                :runs (mapv summary runs)}]
    (.mkdirs directory)
    (spit (io/file directory "report.edn") (with-out-str (pprint/pprint report)))
    (doseq [[[kind bpm] group] (group-by (juxt :passage :bpm) runs)]
      (spit (io/file directory (str (name kind) "-" (long bpm) ".svg")) (svg kind bpm group)))
    (doseq [kind fixtures/passages]
      (with-open [out (io/output-stream (io/file directory (str (name kind) "-120.wav")))]
        (.write out (fixtures/wav-bytes (fixtures/passage kind {:bpm 120.0 :bars bars}) 48000.0))))
    (println "audio-lfo report" (.getPath directory))
    (doseq [{:keys [bpm block-us closing-block-us shape-delay-ms window-ms results]} budget]
      (println (format "budget %3.0f BPM, 6 cycle LFOs: block us %s, closing block us %s, shape delay max %.1f ms of a %.0f ms window, results %s"
                       bpm (pr-str ((juxt :median :p99 :max) block-us)) (pr-str ((juxt :median :max) closing-block-us))
                       (double (:max shape-delay-ms)) window-ms (pr-str (update-vals results count)))))
    (doseq [{:keys [passage bpm offset-ms path passes? statuses rest-statuses min-confidence hz]} (:offset-sweep report)
            :when (not passes?)]
      (println (format "sweep below the bar: %-13s %3.0f BPM, transport %+5.1f ms, %-7s %s rest %s min confidence %.3f Hz %s"
                       (name passage) bpm offset-ms (name path) (pr-str statuses) (pr-str rest-statuses)
                       (double (or min-confidence 0.0)) (pr-str hz))))
    (println (format "sweep: %d of %d passage/tempo/offset/path rows pass" (count (filter :passes? (:offset-sweep report)))
                     (count (:offset-sweep report))))
    (doseq [{:keys [bpm cpu-us wall-us]} cost]
      (println (format "capture loop %3.0f BPM, thread CPU us %s, wall us %s (median p99 p99.9 max)" bpm
                       (pr-str ((juxt :median :p99 :p999 :max) cpu-us)) (pr-str ((juxt :median :p99 :p999 :max) wall-us)))))
    (doseq [{:keys [ppm sample-clock result timing-valid? clock-drift timestamp-error-ms windows]} drift]
      (println (format "drift %+6.0f ppm %-7s %s valid %s drift %s ppm correction %.2f ms, block error max %.2f settled %.2f ms, windows %s"
                       ppm (name sample-clock) (pr-str (:status result)) timing-valid? (pr-str (some-> (:ppm clock-drift) Math/round))
                       (double (or (:correction-ms clock-drift) 0.0)) (:max timestamp-error-ms) (:settled timestamp-error-ms)
                       (pr-str (:statuses windows)))))
    (doseq [{:keys [passage extract bpm windows max-table-jump latency-ms extract-us-per-block sample-us-per-frame]} (:runs report)]
      (println (format "%-14s %-9s %5.0f  %-62s jump %.1e  latency %s ms  extract %s us/block  sample %s us/frame"
                       (name passage) (name extract) bpm (str/join " " (map (comp name :status) windows))
                       (double max-table-jump) (pr-str ((juxt :min :max) latency-ms))
                       (pr-str ((juxt :median :p99) extract-us-per-block)) (pr-str ((juxt :median :p99) sample-us-per-frame)))))))
