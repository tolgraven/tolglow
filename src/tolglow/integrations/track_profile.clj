(ns tolglow.integrations.track-profile
  "Offline, recording-relative section profiles from operator-supplied audio."
  (:require [clojure.java.io :as io]
            [tolglow.audio.capture :as capture]
            [tolglow.audio.pcm :as pcm]
            [tolglow.integrations.analysis :as analysis]
            [tolglow.integrations.sections :as sections])
  (:import [java.io File]
           [java.nio.charset StandardCharsets]
           [java.nio.file Path]
           [java.security MessageDigest]
           [java.util HexFormat]
           [javax.sound.sampled AudioFormat AudioInputStream AudioSystem]))

(def ^:private profile-version 1)
(def ^:private algorithm :causal-bass-and-accents-v1)
(def ^:private hop-ms 10.0)

(defn- finite? [value]
  (and (number? value) (Double/isFinite (double value))))

(defn- options [{:keys [beat-grid beats-per-window input-channel] :or {beats-per-window 4} :as options}]
  (let [{:keys [bpm anchor-position-ms anchor-beat]
         :or {anchor-position-ms 0.0 anchor-beat 0.0}} beat-grid]
    (when-not (and (map? options) (every? #{:beat-grid :beats-per-window :input-channel} (keys options))
                   (map? beat-grid) (every? #{:bpm :anchor-position-ms :anchor-beat} (keys beat-grid))
                   (every? finite? [bpm anchor-position-ms anchor-beat]) (<= 1.0 bpm 1000.0)
                   (integer? beats-per-window) (<= 1 beats-per-window 16)
                   (or (nil? input-channel)
                       (and (integer? input-channel) (<= 1 input-channel pcm/max-channels))))
      (throw (ex-info "Track analysis requires a constant beat grid and a window of 1–16 beats" {})))
    {:beat-grid {:bpm (double bpm) :anchor-position-ms (double anchor-position-ms)
                 :anchor-beat (double anchor-beat)}
     :beats-per-window (int beats-per-window) :input-channel (some-> input-channel int)}))

(defn- local-file ^File [path]
  (let [^File file (cond
                     (instance? Path path) (.toFile ^Path path)
                     (or (instance? File path) (string? path)) (io/file path))]
    (when-not (and file (.isFile file))
      (throw (ex-info "Track analysis requires an existing local audio file" {})))
    file))

(defn- utf8 ^bytes [^String value] (.getBytes value StandardCharsets/UTF_8))
(defn- sha256 ^MessageDigest [] (MessageDigest/getInstance "SHA-256"))
(defn- sha-id [^bytes digest] (str "sha256:" (.formatHex (HexFormat/of) digest)))

(defn- frame-position-ms [frames rate] (* 1000.0 (/ frames rate)))

(defn- relative-beat [{:keys [bpm anchor-position-ms]} position-ms]
  (/ (- position-ms anchor-position-ms) (/ 60000.0 bpm)))

(defn- beat-position-ms [{:keys [bpm anchor-position-ms]} beat]
  (+ anchor-position-ms (* beat (/ 60000.0 bpm))))

(defn- half-beat-edge [beat]
  (let [edge (* 0.5 (Math/rint (* 2.0 beat)))]
    (if (< (Math/abs (- beat edge)) 1.0e-9) edge beat)))

(defn- reference-event [event grid position-ms]
  (let [boundary (:boundary-beat event)
        anchor (:anchor-beat grid)]
    (-> event
        (dissoc :boundary-beat :available-at-beat)
        (update :confirmed-at-beat + anchor)
        (assoc :beat (+ anchor boundary)
               :position-ms (beat-position-ms grid boundary)
               :confirmation-position-ms position-ms))))

(defn- scan-pcm [^AudioInputStream input rate channels hop-frames
                 {:keys [beat-grid beats-per-window input-channel]}]
  (let [frame-bytes (* 2 channels)
        buffer (byte-array (* frame-bytes hop-frames))
        digest (doto (sha256)
                 (.update (utf8 (str "tolglow/s16le/v1/" rate "/" channels "\n"))))]
    (loop [frames 0 analyzer (analysis/new-analyzer {:hop-ms hop-ms})
           detector (sections/new-detector {:beats-per-window beats-per-window})
           windows [] events []]
      (let [n (.readNBytes input buffer 0 (alength buffer))]
        (if (zero? n)
          (do
            (when (or (zero? frames) (and (<= 0 (.getFrameLength input)) (not= frames (.getFrameLength input))))
              (throw (ex-info "Track audio is empty or has fewer frames than declared" {:frames frames})))
            {:frames frames :id (sha-id (.digest digest)) :windows windows :events events})
          (let [samples (capture/decode-pcm16 buffer 0 n channels false input-channel)
                end (+ frames (alength ^doubles samples))
                position-ms (frame-position-ms end rate)
                result (analysis/analyze analyzer {:samples samples :sample-rate rate
                                                   :started-at-ms (frame-position-ms frames rate)
                                                   :received-at-ms position-ms})
                ;; The final partial hop contributes to identity and duration.
                observation (when (= n (alength buffer))
                              (sections/observe-hop detector
                                                    {:beat (half-beat-edge (relative-beat beat-grid position-ms))
                                                     :features (:features result)}))]
            (.update digest buffer 0 n)
            (recur end (:state result) (or (:state observation) detector)
                   (cond-> windows (:window observation) (conj (:window observation)))
                   (cond-> events (:event observation)
                     (conj (reference-event (:event observation) beat-grid position-ms))))))))))

(defn- signature [{:keys [bass-pattern accent-pattern]}]
  {:bass-notes (mapv (fn [notes]
                      (ffirst (sort-by (fn [[note weight]] [(- weight) note]) notes))) bass-pattern)
   :accent-weights (mapv #(Math/round (* 32.0 %)) accent-pattern)})

(defn- summarize [windows]
  (let [eligible (filterv :eligible? windows)
        n (count eligible)]
    (if (pos? n)
      (let [window-power (map (fn [{:keys [rms]}] (* rms rms)) eligible)]
        {:eligible-windows n
         :rms (Math/sqrt (/ (reduce + window-power) n))
         :mean-pitch-coverage (/ (transduce (map :pitch-coverage) + eligible) n)
         :signature (signature (apply max-key :confidence eligible))})
      {:eligible-windows 0})))

(defn- section-timeline [grid duration-ms windows events]
  (let [anchor (:anchor-beat grid)
        points (into [{:position-ms 0.0 :beat (+ anchor (relative-beat grid 0.0)) :confidence 1.0}] events)
        ends (conj (subvec points 1) {:position-ms duration-ms :beat (+ anchor (relative-beat grid duration-ms))})]
    (loop [pairs (seq (map vector points ends)) windows windows families {} result []]
      (if-let [[start end] (first pairs)]
        (let [[within following] (split-with #(< (beat-position-ms grid (:start-beat %)) (:position-ms end)) windows)
              features (summarize within)
              sig (:signature features)
              family (when sig (get families sig (count families)))
              section (cond-> {:id (count result)
                               :start-position-ms (:position-ms start) :end-position-ms (:position-ms end)
                               :start-beat (:beat start) :end-beat (:beat end)
                               :boundary-kind (if (:type start) :confirmed-change :file-start)
                               :boundary-confidence (:confidence start) :features features}
                        family (assoc :family-id family))]
          (recur (next pairs) following (cond-> families sig (assoc sig family)) (conj result section)))
        result))))

(defn- beat-markers [grid duration-ms]
  (let [anchor (:anchor-beat grid)
        first-beat (long (Math/ceil (+ anchor (half-beat-edge (relative-beat grid 0.0)))))
        end-beat (long (Math/ceil (+ anchor (half-beat-edge (relative-beat grid duration-ms)))))]
    (into [] (comp (map (fn [beat] {:beat beat :position-ms (beat-position-ms grid (- beat anchor))}))
                   (filter #(<= 0.0 (:position-ms %)))
                   (filter #(< (:position-ms %) duration-ms)))
          (range first-beat end-beat))))

(defn analyze-file
  "Read a local Java Sound audio file into a portable track profile. Supply
  {:beat-grid {:bpm ... :anchor-position-ms 0.0 :anchor-beat 0.0}} with the
  recording's constant beat grid. Optional :beats-per-window defaults to four.
  Mono passes through and stereo defaults to the average. A one-based
  :input-channel selects a channel and is required above two channels.

  Positions use recording-relative milliseconds. Duration uses the actual
  decoded frame count. The reference ID hashes canonical signed-16-bit PCM
  with the original rate and channel layout. The reference version also
  identifies the analysis configuration. Repeat families share a signature
  from comparable complete windows. Unconfirmed changes are absent."
  [path opts]
  (let [options (options opts)
        file (local-file path)]
    (with-open [source (AudioSystem/getAudioInputStream file)]
      (let [format (.getFormat source)
            rate (double (.getSampleRate format)) channels (.getChannels format)
            hop-frames (Math/round (* rate (/ hop-ms 1000.0)))
            grid (:beat-grid options)]
        (when-not (and (<= 8000.0 rate 192000.0) (pcm/valid-layout? channels (:input-channel options))
                       (<= (/ (frame-position-ms hop-frames rate) (/ 60000.0 (:bpm grid))) 0.125))
          (throw (ex-info "Track format or tempo cannot supply the analyzer's required hop cadence" {})))
        (let [target (AudioFormat. (float rate) 16 channels true false)]
          (with-open [input (AudioSystem/getAudioInputStream target source)]
            (let [{:keys [frames id windows events]} (scan-pcm input rate channels hop-frames options)
                  duration-ms (frame-position-ms frames rate)
                  version-key [profile-version algorithm id (:bpm grid) (:anchor-position-ms grid)
                               (:anchor-beat grid) (:beats-per-window options) (:input-channel options)]
                  version (sha-id (.digest (sha256) (utf8 (pr-str version-key))))]
              {:version profile-version :reference {:id id :version version}
               :provenance {:kind :local-audio-file :identity-basis :native-rate-signed-pcm16-le
                            :sample-rate-hz rate :channels channels :frames frames}
               :analysis (assoc options :algorithm algorithm :hop-frames hop-frames)
               :duration-ms duration-ms :beats (beat-markers grid duration-ms)
               :sections (section-timeline grid duration-ms windows events) :events events})))))))
