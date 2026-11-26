(ns tolglow.audio.stems
  "Named mono stems from one multichannel input (#675): kick, snare, bass,
  vocals and so on, each on its own one-based channel. Every stem has its own
  band filter, envelope follower and onset detector. The onset gate is set in
  beats and converted with the captured tempo, clamped to 70–180 BPM, so a
  retrigger guard tightens as the music speeds up.

  Pure: explicit state and blocks in, {:state :features :events} out. No IO,
  clocks, threads or callbacks. The capture worker runs it beside the ordinary
  analyzer; onsets become :audio/onset events for event subscribers, and stem
  levels become audio ports of the existing signal patch (tolglow.routing).
  Arrays are owned by the current call. Band isolation uses the shared
  Linkwitz–Riley filter bank of tolglow.audio.shape (#676)."
  (:require [afterglow.time :as time]
            [tolglow.audio.shape :as shape]))

(def max-stems 16)

(def tempo-range
  "Tempo the onset gate is designed for. A captured tempo outside it is
  clamped, so a stray clock never opens or closes the gate beyond musical use."
  [70.0 180.0])

(def fallback-bpm 120.0)

(def defaults
  {:band nil :gain-db 0.0 :attack-ms 2.0 :release-ms 120.0 :floor-db -54.0 :range-db 20.0
   :rise-db 6.0 :rearm-db 3.0 :hold-decay-db-per-s 3.0 :isolation-db 18.0 :min-interval {:beats 1/6}})

(def presets
  "Starting points per instrument. Every value can be overridden per stem."
  {:kick {:band [30.0 160.0] :release-ms 140.0}
   :snare {:band [150.0 8000.0] :release-ms 120.0}
   :hats {:band [5000.0 16000.0] :release-ms 60.0 :min-interval {:beats 1/8}}
   :bass {:band [20.0 1000.0] :release-ms 80.0 :min-interval {:beats 1/4}}
   ;; Plucks over a ringing string rise most in the upper band and only a
   ;; few decibels, so the guitar listens high with a smaller rise.
   :guitar {:band :high :release-ms 250.0 :rise-db 3.5 :min-interval {:beats 1/4}}
   :vocals {:band [120.0 6000.0] :attack-ms 10.0 :release-ms 300.0 :rise-db 8.0 :min-interval {:beats 1/2}}
   :keys {:band [60.0 8000.0] :attack-ms 5.0 :release-ms 250.0 :min-interval {:beats 1/4}}
   :full {}})

(def ^:private stem-keys
  (into #{:id :channel :preset :label} (keys defaults)))

(defn- fail! [message data] (throw (ex-info message (assoc data :audio/error :invalid-stems))))

(defn- finite? [x] (time/finite-number? x))

(defn- clamp ^double [^double x ^double lo ^double hi] (max lo (min hi x)))

(defn stem-id
  "Canonical stem identity: a short lowercase keyword. Studio sends text."
  [id]
  (let [text (cond (keyword? id) (name id) (string? id) id :else nil)]
    (when (and text (re-matches #"[a-z][a-z0-9-]{0,31}" text)) (keyword text))))

(defn emitter-id
  "Event emitter identity for one stem's onsets, matched by event subscribers
  as {:type :audio/onset :emitter-id :stem/kick}."
  [id]
  (keyword "stem" (name id)))

(defn port-id
  "Signal patch source identity for one stem feature, e.g. \"stem/bass/level\"."
  [id feature]
  (str "stem/" (name id) "/" (name feature)))

(defn- duration-ok? [duration]
  (and (empty? (time/validate-duration duration)) (pos? (val (first duration)))
       (contains? #{:beats :ms} (key (first duration)))))

(defn- canonical-stem [channels index stem]
  (when-not (and (map? stem) (every? stem-keys (keys stem)))
    (fail! "A stem has an id, a one-based channel and optional detector settings" {:index index}))
  (let [id (stem-id (:id stem))
        preset (when (contains? stem :preset)
                 (let [p (if (string? (:preset stem)) (keyword (:preset stem)) (:preset stem))]
                   (when-not (contains? presets p) (fail! "Unknown stem preset" {:index index :preset (:preset stem)}))
                   p))
        merged (merge defaults (get presets preset) (dissoc stem :preset :id))
        merged (cond-> merged (string? (:band merged)) (update :band keyword))
        {:keys [channel band isolation-db]} merged]
    (when-not id (fail! "Stem id must be a short lowercase name such as kick or bass-di" {:index index}))
    (when-not (and (integer? channel) (<= 1 channel channels))
      (fail! "Stem channel must name one of the captured PCM channels" {:index index :id id :channels channels}))
    (when-not (or (nil? band) (try (shape/band-range band) true (catch clojure.lang.ExceptionInfo _ false)))
      (fail! "Stem band is a named band (:sub :bass :low-mid :mid :high :full) or increasing Hz bounds in 10..20000" {:id id}))
    (when-not (and (every? #(finite? (get merged %)) [:gain-db :attack-ms :release-ms :floor-db :range-db :rise-db
                                                      :rearm-db :hold-decay-db-per-s])
                   (or (nil? isolation-db) (and (finite? isolation-db) (< 0 isolation-db 96)))
                   (<= -48 (:gain-db merged) 48) (< 0 (:attack-ms merged) 1000) (< 0 (:release-ms merged) 10000)
                   (<= -120 (:floor-db merged) -6) (< 0 (:range-db merged) 96) (< 0 (:rise-db merged) 48)
                   (< 0 (:rearm-db merged) 48) (<= 0 (:hold-decay-db-per-s merged) 96)
                   (duration-ok? (:min-interval merged))
                   (or (nil? (:label merged)) (and (string? (:label merged)) (<= 1 (count (:label merged)) 64))))
      (fail! "Invalid stem detector settings" {:id id}))
    (cond-> (assoc merged :id id :channel channel :band (if (vector? band) (mapv double band) band))
      preset (assoc :preset preset))))

(defn validate-config
  "Return canonical stems for a capture of `channels` PCM channels, or throw.
  Input is a vector of {:id :channel} maps with an optional :preset and
  detector overrides. Ids are unique; two stems may read the same channel
  (for example, a kick band and a bass band of one drum-bus input)."
  [stems channels]
  (when-not (and (vector? stems) (<= 1 (count stems) max-stems))
    (fail! "Stems are a vector of 1–16 named channels" {}))
  (let [canonical (vec (map-indexed #(canonical-stem channels %1 %2) stems))]
    (when-not (apply distinct? (map :id canonical))
      (fail! "Stem ids must be unique" {:ids (mapv :id canonical)}))
    canonical))

(defn effective-bpm
  "Tempo used to convert a beat gate: the captured clock's BPM clamped to
  tempo-range, or fallback-bpm without a clock."
  [clock]
  (let [[lo hi] tempo-range bpm (:bpm clock)]
    (if (and (finite? bpm) (pos? bpm)) (clamp (double bpm) lo hi) fallback-bpm)))

(defn interval-ms
  "Minimum milliseconds between two onsets for a {:beats n} or {:ms n} gate."
  [duration bpm]
  (let [[unit value] (first duration)]
    (case unit
      :ms (double value)
      :beats (* (double value) (/ 60000.0 (double bpm))))))

(defn- design [band sample-rate] (shape/filter-design (or band :full) sample-rate))

(defn- one-pole [ms sample-rate] (- 1.0 (Math/exp (/ -1000.0 (* ms sample-rate)))))

(def hop-ms
  "Every stem is analysed in the same 4 ms hops, so simultaneous onsets on
  different stems can be compared hop for hop."
  4.0)

(def flux-hops
  "The rise is measured against the quietest of this many preceding hops
  (20 ms), so an attack that builds over a few hops still counts in full."
  5)

(defn- fresh-stem [stem sample-rate]
  (merge (select-keys stem [:spec :onset-count :bleed-count :last-onset-ms :last-onset-beat])
         {:design (design (get-in stem [:spec :band]) sample-rate) :filter-state []
          ;; envelope, hop energy, hop peak
          :scratch [0.0 0.0 0.0]
          :history [] :hold-db nil :latch-db nil :armed? true :gate-from-ms nil
          :block-peak 0.0 :last-db nil}))

(defn new-state
  "Immutable detector state for validated stems. Onset counts and sequence
  numbers survive discontinuities; filters and detectors restart."
  [stems]
  {:version 1
   :stems (mapv (fn [spec] {:spec spec :onset-count 0 :bleed-count 0 :last-onset-ms nil :last-onset-beat nil}) stems)
   :sequence 0 :sample-rate nil :origin-ms nil :sample-index 0 :last-end-ms nil
   :hop-samples nil :hop-n 0 :hop-start nil :window []})

(defn- initialize [state sample-rate start-ms]
  (assoc state :sample-rate sample-rate :origin-ms start-ms :sample-index 0 :hop-n 0 :hop-start nil :window []
         :hop-samples (max 1 (long (Math/round (* sample-rate (/ hop-ms 1000.0)))))
         :stems (mapv #(fresh-stem % sample-rate) (:stems state))))

(defn- segment!
  "Follow and accumulate band-filtered samples [offset,end) into the scratch
  array [envelope hop-energy hop-peak]. Returns the segment's peak."
  [^doubles samples offset end ^doubles s attack release]
  (let [attack (double attack) release (double release) end (long end)]
    (loop [i (long offset) env (aget s 0) energy (aget s 1) peak 0.0]
      (if (= i end)
        (do (aset s 0 env) (aset s 1 energy) (aset s 2 (max (aget s 2) peak)) peak)
        (let [x (aget samples i)
              a (Math/abs x)
              env (if (> a env) (+ env (* attack (- a env))) (+ env (* release (- a env))))]
          (recur (inc i) env (+ energy (* x x)) (max peak a)))))))

(defn- db ^double [^double x] (* 20.0 (Math/log10 (max 1.0e-9 x))))

(defn level
  "Envelope as 0–1 over the stem's floor-to-full-scale decibel range."
  [envelope floor-db]
  (clamp (/ (- (db (double envelope)) floor-db) (- floor-db)) 0.0 1.0))

(defn- clock-beat [clock time-ms]
  (when (and clock (finite? (:beat clock)) (finite? (:time-ms clock)) (finite? (:bpm clock)))
    (if (false? (:playing? clock))
      (double (:beat clock))
      (+ (:beat clock) (/ (- time-ms (:time-ms clock)) (/ 60000.0 (:bpm clock)))))))

(defn- decide
  "Close one hop for one stem. Returns {:stem detector-without-onset
  :onset detector-after-onset :candidate event} (the last two only for a
  candidate onset). The envelope, not the raw hop energy, is compared, so a
  low note's zero crossings inside a 4 ms hop never look like attacks."
  [stem ^doubles s hop-start-ms clock]
  (let [{:keys [spec history hold-db latch-db armed? gate-from-ms]} stem
        {:keys [floor-db range-db rise-db rearm-db hold-decay-db-per-s min-interval]} spec
        level-db (max (- floor-db 40.0) (db (aget s 0)))
        peak-db (db (aget s 2))
        hold (max level-db (- (or hold-db (- floor-db 40.0)) (* hold-decay-db-per-s (/ hop-ms 1000.0))))
        reference (when (seq history) (reduce min history))
        flux (if reference (- level-db reference) 0.0)
        bpm (effective-bpm clock)
        gate (interval-ms min-interval bpm)
        spaced? (or (nil? gate-from-ms) (>= (- hop-start-ms gate-from-ms) (- gate 1.0e-6)))
        ;; Re-arm once the level falls rearm-db below its post-onset peak, or
        ;; once the gate has passed and it is decaying at all: a ringing note
        ;; can then be followed by the next pluck without a gap.
        armed (or armed? (and latch-db (or (< level-db (- latch-db rearm-db))
                                           (and spaced? (< level-db (- latch-db 0.5))))))
        onset? (and reference armed spaced? (>= level-db floor-db) (>= flux rise-db) (>= level-db (- hold range-db)))
        base (assoc stem :hold-db hold :last-db level-db :gate-ms gate :bpm bpm
                    :history (vec (take-last flux-hops (conj history level-db))))
        _ (do (aset s 1 0.0) (aset s 2 0.0))]
    (if-not onset?
      {:hop-peak-db peak-db :stem (assoc base :armed? armed :latch-db (when-not armed (max level-db (or latch-db level-db))))}
      (let [beat (clock-beat clock hop-start-ms)]
        {:stem base :hop-peak-db peak-db
         :onset (assoc base :armed? false :latch-db level-db :gate-from-ms hop-start-ms)
         :gate-from-ms gate-from-ms
         :candidate (cond-> {:type :audio/onset :time-ms hop-start-ms
                             :source {:type :stem-analysis :id (emitter-id (:id spec))}
                             :inference :band-flux-onset :confidence (clamp (/ flux (* 2.0 rise-db)) 0.0 1.0)
                             :stem (:id spec) :channel (:channel spec) :level-db level-db :peak-db peak-db :flux-db flux
                             :gate-ms gate :bpm bpm :transport-playing? (not (false? (:playing? clock)))
                             :timing {:basis :hop-start :hop-ms hop-ms :filter-delay-compensated? false
                                      :includes-processing-time? false}}
                      beat (assoc :beat beat))}))))

(def confirm-hops
  "A candidate onset waits this many further hops (8 ms) before it is emitted,
  so bleed is judged on each stem's peak over its first 12 ms rather than on
  a first hop that the band filters have not yet let through."
  2)

(def bleed-window-ms
  "Onsets this close on two stems can be one sound: band filters with
  different edges let the same attack through a few milliseconds apart."
  (* 3 hop-ms))

(defn- bleed?
  "A candidate is bleed when another stem has an onset within bleed-window-ms
  whose filtered peak over the same window is at least the candidate stem's
  :isolation-db above the candidate's. Peaks, not envelopes, are compared so
  each stem's attack time does not decide. Different instruments landing
  together at similar levels keep both onsets."
  [record isolation-db window]
  (and isolation-db
       (some (fn [other]
               (and (not= (:stem other) (:stem record))
                    (<= (Math/abs (double (- (:time-ms other) (:time-ms record)))) (+ bleed-window-ms 1.0e-6))
                    (>= (- (:peak-db other) (:peak-db record)) isolation-db)))
             window)))

(defn- close-hop
  "Decide every stem for one hop, confirm candidates old enough to judge,
  reject bleed across stems, and return [stems window emitted-events]."
  [stems window scratches hop-start-ms clock]
  (let [decisions (mapv #(decide %1 %2 hop-start-ms clock) stems scratches)
        peaks (into {} (map (fn [d] [(get-in d [:stem :spec :id]) (:hop-peak-db d)])) decisions)
        index (into {} (map-indexed (fn [i d] [(get-in d [:stem :spec :id]) i])) decisions)
        ;; Detectors commit at once, so gating and re-arming follow the attack.
        stems (mapv #(or (:onset %) (:stem %)) decisions)
        window (into (mapv (fn [r] (if (< (:hops r) confirm-hops)
                                     (-> r (update :peak-db max (peaks (:stem r))) (update :hops inc)) r))
                           window)
                     (keep (fn [{:keys [candidate gate-from-ms]}]
                             (when candidate
                               {:stem (:stem candidate) :time-ms (:time-ms candidate) :peak-db (:peak-db candidate)
                                :hops 0 :candidate candidate :previous-gate-from-ms gate-from-ms})))
                     decisions)
        [stems window events]
        (reduce (fn [[stems window events] [w record]]
                  (if-not (and (:candidate record) (= confirm-hops (:hops record)))
                    [stems window events]
                    (let [i (index (:stem record))
                          candidate (assoc (:candidate record) :peak-db (:peak-db record))
                          window (assoc window w (dissoc record :candidate))]
                      (if (bleed? record (get-in stems [i :spec :isolation-db]) window)
                        ;; Bleed does not hold the stem's own gate shut.
                        [(update stems i #(-> % (update :bleed-count inc)
                                              (assoc :gate-from-ms (:previous-gate-from-ms record))))
                         window events]
                        [(update stems i #(-> % (assoc :last-onset-ms (:time-ms candidate) :last-onset-beat (:beat candidate))
                                              (update :onset-count inc)))
                         window (conj events candidate)]))))
                [stems window []] (map-indexed vector window))]
    [stems
     (filterv #(<= (- hop-start-ms (:time-ms %)) (+ bleed-window-ms (* hop-ms (inc confirm-hops)))) window)
     events]))

(defn- feature [stem]
  (let [{:keys [spec scratch]} stem envelope (nth scratch 0 0.0)]
    (cond-> {:id (:id spec) :channel (:channel spec)
             :level (level envelope (:floor-db spec)) :envelope (clamp envelope 0.0 1.0)
             :peak (clamp (:block-peak stem 0.0) 0.0 16.0)
             :onset-count (:onset-count stem) :bleed-count (:bleed-count stem)
             :gate-ms (or (:gate-ms stem) (interval-ms (:min-interval spec) fallback-bpm))}
      (:label spec) (assoc :label (:label spec))
      (:preset spec) (assoc :preset (:preset spec))
      (:last-db stem) (assoc :level-db (:last-db stem))
      (:bpm stem) (assoc :bpm (:bpm stem))
      (:last-onset-ms stem) (assoc :last-onset-ms (:last-onset-ms stem))
      (:last-onset-beat stem) (assoc :last-onset-beat (:last-onset-beat stem)))))

(defn- owned [samples]
  (cond
    (instance? (Class/forName "[D") samples) samples
    (sequential? samples) (double-array samples)
    :else (fail! "Stem samples must be a numeric block" {})))

(defn analyze
  "Consume one contiguous block for every stem and return {:state :features
  :events :discontinuity?}.

  Input: {:blocks {stem-id mono-samples} :sample-rate Hz :started-at-ms
  capture time of the first sample :received-at-ms boundary arrival time
  :clock optional {:time-ms :beat :bpm :playing?}}. All blocks have the same
  length. A gap, overlap or sample-rate change restarts filters and detectors
  rather than inventing onsets. :features maps each stem id to its level,
  envelope, peak and onset counters. Each :audio/onset event carries the hop
  start time, its stem, flux, gate and, with a clock, its beat."
  [state {:keys [blocks sample-rate started-at-ms received-at-ms clock]}]
  (when-not (and (= 1 (:version state)) (map? blocks) (finite? sample-rate) (<= 8000 sample-rate 192000)
                 (finite? started-at-ms) (<= 0 started-at-ms) (finite? received-at-ms))
    (fail! "Stem block requires state, per-stem samples, sample rate and timestamps" {}))
  (let [arrays (mapv (fn [stem] (let [id (get-in stem [:spec :id])]
                                  (when-not (contains? blocks id)
                                    (fail! "Missing samples for a configured stem" {:id id}))
                                  (owned (get blocks id)))) (:stems state))
        n (alength ^doubles (first arrays))
        _ (when-not (and (pos? n) (every? #(= n (alength ^doubles %)) arrays))
            (fail! "Every stem block must contain the same number of samples" {}))
        tolerance (/ 500.0 sample-rate)
        discontinuity? (and (:sample-rate state)
                            (or (not= (double sample-rate) (double (:sample-rate state)))
                                (> (Math/abs (double (- started-at-ms (:last-end-ms state)))) tolerance)))
        state (if (or (nil? (:sample-rate state)) discontinuity?)
                (initialize state (double sample-rate) started-at-ms) state)
        rate (double sample-rate) index (long (:sample-index state)) hop-samples (long (:hop-samples state))
        specs (mapv :spec (:stems state))
        gains (mapv #(Math/pow 10.0 (/ (:gain-db %) 20.0)) specs)
        filtered (mapv (fn [stem ^doubles samples gain]
                         (let [input (if (== 1.0 (double gain)) samples (amap samples i out (* (double gain) (aget samples i))))]
                           (shape/filter-samples (:design stem) (:filter-state stem) input)))
                       (:stems state) arrays gains)
        arrays (mapv first filtered)
        scratches (mapv #(double-array (:scratch %)) (:stems state))
        attacks (mapv #(one-pole (:attack-ms %) rate) specs)
        releases (mapv #(one-pole (:release-ms %) rate) specs)
        ms-at (fn [i] (+ (:origin-ms state) (* 1000.0 (/ (double i) rate))))
        [stems window hop-n hop-start events]
        (loop [offset 0 stems (mapv #(assoc % :block-peak 0.0) (:stems state)) window (:window state [])
               hop-n (long (:hop-n state)) hop-start (:hop-start state) events []]
          (if (= offset n)
            [stems window hop-n hop-start events]
            (let [end (min n (+ offset (- hop-samples hop-n)))
                  hop-start (or hop-start (ms-at (+ index offset)))
                  stems (mapv (fn [i stem]
                                (let [peak (segment! (arrays i) offset end (scratches i) (attacks i) (releases i))]
                                  (update stem :block-peak max peak)))
                              (range) stems)
                  hop-n (+ hop-n (- end offset))]
              (if (= hop-n hop-samples)
                (let [[stems window emitted] (close-hop stems window scratches hop-start clock)]
                  (recur end stems window 0 nil (into events emitted)))
                (recur end stems window hop-n hop-start events)))))
        stems (mapv (fn [stem ^doubles s [_ z]] (assoc stem :scratch (vec s) :filter-state z)) stems scratches filtered)
        sequence (:sequence state)
        events (vec (map-indexed (fn [i event]
                                   (assoc event :sequence (+ sequence i 1) :observed-at-ms received-at-ms
                                          :latency-ms (- received-at-ms (:time-ms event))))
                                 events))]
    {:state (assoc state :stems stems :window window :sample-index (+ index n) :sequence (+ sequence (count events))
                   :hop-n hop-n :hop-start hop-start
                   :last-end-ms (+ started-at-ms (* 1000.0 (/ n rate))))
     :features (into {} (map (fn [stem] [(get-in stem [:spec :id]) (feature stem)])) stems)
     :events events
     :discontinuity? (boolean discontinuity?)}))

(defn replay
  "Offline: run whole per-stem sample arrays through analyze in capture-sized
  blocks. clock-at receives each block's start time and returns a clock or
  nil. Returns {:events [...] :frames [{:time-ms :features}]} for tests,
  harnesses and REPL auditioning of a recording."
  [stems {:keys [samples sample-rate block-frames clock-at started-at-ms]
          :or {block-frames 480 started-at-ms 0.0 clock-at (constantly nil)}}]
  (let [arrays (into {} (map (fn [[id values]] [id (owned values)])) samples)
        total (alength ^doubles (val (first arrays)))]
    (loop [offset 0 state (new-state stems) events [] frames []]
      (if (>= offset total)
        {:events events :frames frames :state state}
        (let [end (min total (+ offset block-frames))
              start-ms (+ started-at-ms (* 1000.0 (/ offset (double sample-rate))))
              end-ms (+ started-at-ms (* 1000.0 (/ end (double sample-rate))))
              blocks (into {} (map (fn [[id ^doubles a]] [id (java.util.Arrays/copyOfRange a (int offset) (int end))])) arrays)
              result (analyze state {:blocks blocks :sample-rate sample-rate :started-at-ms start-ms
                                     :received-at-ms end-ms :clock (clock-at start-ms)})]
          (recur end (:state result) (into events (:events result))
                 (conj frames {:time-ms end-ms :features (:features result)})))))))

(defn score-onsets
  "Compare detected onset times with expected ones, both in milliseconds.
  Each expected onset matches at most one detection within tolerance-ms.
  Returns matched pairs, misses, false positives and absolute timing errors."
  [detected expected tolerance-ms]
  (let [detected (vec (sort detected))]
    (loop [expected (sort expected) unused (set (range (count detected))) pairs [] missed []]
      (if-let [target (first expected)]
        (let [candidate (->> unused
                             (filter #(<= (Math/abs (double (- (nth detected %) target))) tolerance-ms))
                             (sort-by #(Math/abs (double (- (nth detected %) target))))
                             first)]
          (if candidate
            (recur (rest expected) (disj unused candidate) (conj pairs [target (nth detected candidate)]) missed)
            (recur (rest expected) unused pairs (conj missed target))))
        (let [errors (mapv (fn [[target found]] (- found target)) pairs)
              absolute (map #(Math/abs (double %)) errors)]
          {:matched (count pairs) :missed missed
           :false-positives (mapv #(nth detected %) (sort unused))
           :errors-ms errors
           :max-abs-error-ms (if (seq absolute) (reduce max absolute) 0.0)
           :mean-abs-error-ms (if (seq absolute) (/ (reduce + absolute) (count absolute)) 0.0)})))))
