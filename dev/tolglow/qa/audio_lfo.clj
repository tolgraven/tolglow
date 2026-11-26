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
    through a real interface or Live.

  lein run -m tolglow.qa.audio-lfo <out-dir>

  No device, port or network is opened."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [tolglow.audio-lfo :as lfo]
            [tolglow.audio.fixtures :as fixtures]
            [tolglow.audio.shape :as shape]))

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

(defn -main [& [out-dir]]
  (let [directory (io/file (or out-dir "target/audio-lfo-676"))
        runs (vec (for [kind fixtures/passages bpm tempos mode modes] (run kind mode bpm)))
        report {:issue 676 :settings settings :bars bars :sample-rate 48000.0 :block-frames 480 :fps 60.0
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
    (doseq [{:keys [passage extract bpm windows max-table-jump latency-ms extract-us-per-block sample-us-per-frame]} (:runs report)]
      (println (format "%-14s %-9s %5.0f  %-62s jump %.1e  latency %s ms  extract %s us/block  sample %s us/frame"
                       (name passage) (name extract) bpm (str/join " " (map (comp name :status) windows))
                       (double max-table-jump) (pr-str ((juxt :min :max) latency-ms))
                       (pr-str ((juxt :median :p99) extract-us-per-block)) (pr-str ((juxt :median :p99) sample-us-per-frame)))))))
