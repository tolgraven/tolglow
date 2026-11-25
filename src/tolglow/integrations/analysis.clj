(ns tolglow.integrations.analysis
  "Causal audio feature extraction with explicit stream state and timestamps.
  Kick/energy/breakdown heuristics are calibrated signal descriptors, not a
  general music, downbeat or emotion classifier. No IO or mutable state is held
  by the analyzer; subscriptions are an independently invoked host boundary."
  (:require [tolglow.integrations.bands :as bands]
            [tolglow.integrations.sections :as sections]))

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

(defn- clamp [x lo hi] (max lo (min hi x)))

(defn- approach [old value elapsed-ms time-constant-ms]
  (+ old (* (- 1.0 (Math/exp (/ (- elapsed-ms) time-constant-ms))) (- value old))))

(def ^:private defaults
  {:source-id :audio-input :hop-ms 10.0 :calibration-ms 500.0
   :kick-band-hz [35.0 160.0] :noise-floor 0.003
   :kick-baseline-ms 1500.0 :kick-threshold-ratio 2.2
   :kick-deviation-multiplier 3.0 :kick-rise-ratio 0.2
   :kick-refractory-ms 140.0 :kick-rearm-ratio 0.5
   :energy-attack-ms 40.0 :energy-release-ms 200.0
   :energy-baseline-ms 8000.0 :energy-event-ms 100.0
   :breakdown-reference-floor 0.015 :breakdown-enter-ratio 0.22
   :breakdown-exit-ratio 0.55 :breakdown-enter-ms 700.0 :breakdown-exit-ms 150.0
   :beat-tolerance-ms 75.0 :beat-clock-confidence-min 0.5})

(defn- options [input]
  (when-not (and (map? input) (every? (conj (set (keys defaults)) :sections) (keys input)))
    (throw (ex-info "Unknown audio analysis option" {})))
  (when (contains? input :sections)
    (let [grid (:sections input)]
      (when-not (and (map? grid) (every? #{:anchor-beat :beats-per-window} (keys grid)))
        (throw (ex-info "Section analysis requires a grid options map" {})))
      (sections/new-detector grid)))
  (let [opts (merge defaults input) [low high] (:kick-band-hz opts)
        positive (vals (dissoc opts :source-id :kick-band-hz :beat-clock-confidence-min :sections))]
    (when-not (and (or (keyword? (:source-id opts)) (string? (:source-id opts)))
                   (every? #(and (finite? %) (pos? %)) positive)
                   (every? finite? [low high]) (< 0 low high)
                   (<= 2 (:hop-ms opts) 50) (<= (:hop-ms opts) (:calibration-ms opts))
                   (> (:kick-threshold-ratio opts) 1)
                   (< 0 (:kick-rearm-ratio opts) 1)
                   (< 0 (:breakdown-enter-ratio opts) (:breakdown-exit-ratio opts) 1)
                   (finite? (:beat-clock-confidence-min opts))
                   (<= 0 (:beat-clock-confidence-min opts) 1))
      (throw (ex-info "Invalid audio analysis thresholds" {})))
    opts))

(defn publish-features
  "Publish the normalized audio energy as a show variable and retain the
  remaining analyzer measurements together with their capture timestamp."
  [model features]
  (let [model (assoc-in model [:variables :audio :features] (dissoc features :audio/energy))]
    (if (contains? features :audio/energy)
      (assoc-in model [:variables :audio/energy] (:audio/energy features))
      (update model :variables dissoc :audio/energy))))

(defn new-analyzer
  "Create immutable analysis state. Configure thresholds per actual input gain;
  defaults are normalized PCM heuristics. :calibration-ms gates onsets and
  breakdowns while baselines initialize. Supply :sections {} to enable feel
  analysis, with optional :anchor-beat and :beats-per-window. No input samples
  or callbacks retained."
  ([] (new-analyzer {}))
  ([opts] {:version 1 :options (options opts) :sequence 0 :sample-index 0
           :sample-rate nil :origin-ms nil :last-end-ms nil :hop nil
           :kick-armed? true :kick-latch-level 0.0 :breakdown? false}))

(defn- biquad [kind frequency sample-rate]
  ;; Robert Bristow-Johnson cookbook Butterworth high/low-pass coefficients.
  ;; Direct form II transposed; private per-call delay storage is serialized
  ;; into immutable scalars at the boundary.
  (let [omega (/ (* 2.0 Math/PI frequency) sample-rate)
        c (Math/cos omega) alpha (/ (Math/sin omega) (Math/sqrt 2.0))
        a0 (+ 1.0 alpha) b0 (/ (if (= kind :high) (+ 1.0 c) (- 1.0 c)) 2.0)
        b1 (if (= kind :high) (- (* 2.0 b0)) (* 2.0 b0))]
    [(/ b0 a0) (/ b1 a0) (/ b0 a0) (/ (* -2.0 c) a0) (/ (- 1.0 alpha) a0)]))

(defn- initialize [state sample-rate start-ms]
  (let [[low high] (get-in state [:options :kick-band-hz])]
    (when-not (< high (/ sample-rate 2.0))
      (throw (ex-info "Kick band must be below Nyquist" {})))
    (merge (new-analyzer (:options state))
           {:sequence (:sequence state) :sample-rate sample-rate :origin-ms start-ms
            :hop-samples (long (Math/round (* sample-rate (/ (get-in state [:options :hop-ms]) 1000.0))))
            :bands (bands/start sample-rate)
            :filter-coefficients [(biquad :high low sample-rate) (biquad :low high sample-rate)]
            :filter-delay [0.0 0.0 0.0 0.0]
            :hop {:n 0 :energy 0.0 :kick-energy 0.0 :peak 0.0}})))

(defn- validate-clock [clock]
  (when (and clock
             (not (and (map? clock) (every? finite? ((juxt :time-ms :beat :bpm) clock))
                       (<= 0 (:time-ms clock)) (< 0 (:bpm clock) 1000)
                       (or (keyword? (:source clock)) (string? (:source clock)))
                       (finite? (get clock :confidence 1.0))
                       (<= 0 (get clock :confidence 1.0) 1))))
    (throw (ex-info "Clock needs source, time-ms, beat, bpm and optional confidence" {})))
  clock)

(defn- clock-beat-at [clock time-ms]
  (+ (:beat clock) (/ (- time-ms (:time-ms clock)) (/ 60000.0 (:bpm clock)))))

(defn- alignment [clock time-ms tolerance-ms]
  (when (and clock (not= false (if (contains? clock :playing?) (:playing? clock) (:running? clock))))
    (let [period (/ 60000.0 (:bpm clock))
          position (clock-beat-at clock time-ms)
          beat (long (Math/round (double position)))
          error (* (- position beat) period)]
      {:beat beat :phase-error-beats (- position beat) :phase-error-ms error
       :aligned? (<= (Math/abs error) tolerance-ms)
       :bpm (:bpm clock) :clock-source (:source clock)
       :clock-epoch (get clock :source-epoch (:epoch clock))
       :clock-confidence (get clock :confidence 1.0)})))

(defn- emit [state events type time-ms observed-at-ms inference confidence data]
  (let [sequence (inc (:sequence state))
        event (merge {:type type :sequence sequence :time-ms time-ms
                      :observed-at-ms observed-at-ms :latency-ms (- observed-at-ms time-ms)
                      :source {:type :audio-analysis :id (get-in state [:options :source-id])}
                      :inference inference :confidence (clamp confidence 0.0 1.0)
                      :timing {:basis :window-midpoint :filter-delay-compensated? false
                               :includes-processing-time? false}}
                     data)]
    [(assoc state :sequence sequence) (conj events event)]))

(defn- section-clock-id [clock confidence-min]
  (let [epoch (get clock :source-epoch (:epoch clock))
        session (:session-id clock)
        playing? (get clock :playing? (:running? clock))
        confidence (:confidence clock)]
    (when (and (true? playing?) (not (:stale? clock))
               (finite? confidence) (>= confidence confidence-min)
               (integer? epoch) (<= 0 epoch Long/MAX_VALUE)
               (or (nil? session) (keyword? session) (uuid? session)
                   (and (string? session) (<= 1 (count session) 128))))
      (cond-> {:clock-source (:source clock) :clock-epoch epoch}
        session (assoc :clock-session-id session)))))

(defn- prepare-sections [state clock]
  (if-let [grid (get-in state [:options :sections])]
    (let [clock-id (section-clock-id clock (get-in state [:options :beat-clock-confidence-min]))]
      (if (and clock-id (= clock-id (get-in state [:section-state :clock-id])))
        state
        (cond-> (dissoc state :section-state)
          (:features state) (update :features dissoc :audio/section)
          clock-id (assoc :section-state {:clock-id clock-id
                                         :anchor-beat (double (:anchor-beat grid 0.0))
                                         :detector (sections/new-detector (dissoc grid :anchor-beat))}))))
    state))

(defn- section-grid-beat [clock end-ms anchor-beat]
  (let [relative (- (clock-beat-at clock end-ms) anchor-beat)
        edge (* 0.5 (Math/rint (* 2.0 relative)))]
    ;; Equivalent clock snapshots can differ by rounding at a half-beat edge.
    (if (< (Math/abs (- relative edge)) 1.0e-9) edge relative)))

(defn- section-step [state events end-ms received-at-ms clock]
  (if-let [{:keys [clock-id anchor-beat detector last-beat feel] :as section-state} (:section-state state)]
    (let [beat (section-grid-beat clock end-ms anchor-beat)
          ;; Use the detector's documented continuity limit for the retained
          ;; feature as well as the underlying comparison.
          discontinuity? (and last-beat (or (<= beat last-beat) (> (- beat last-beat) 0.125)))
          {:keys [event] next-detector :state}
          (sections/observe-hop detector {:beat beat
                                          :features (:features state) :discontinuity? discontinuity?})
          feel (if event
                 (-> (merge clock-id (dissoc event :type :available-at-beat)
                            {:confirmed-at-ms end-ms :clock-confidence (:confidence clock)
                             :confidence (min (:confidence event) (:confidence clock))})
                     (update :boundary-beat + anchor-beat)
                     (update :confirmed-at-beat + anchor-beat))
                 (when-not discontinuity? feel))
          section-state (assoc section-state :detector next-detector :last-beat beat :feel feel)
          state (cond-> (assoc state :section-state section-state)
                  feel (assoc-in [:features :audio/section] feel))]
      (if event
        (emit state events :audio/section-change end-ms received-at-ms :beat-synchronous-bass-and-accents
              (:confidence feel)
              (assoc feel :available-at-beat (max (:confirmed-at-beat feel) (clock-beat-at clock received-at-ms))
                          :latency-ms (max 0.0 (- received-at-ms end-ms))
                          :timing {:basis :confirmation-hop-end :filter-delay-compensated? false
                                   :includes-processing-time? false}))
        [state events]))
    [state events]))

(defn- feature-step [state events hop received-at-ms clock]
  (let [opts (:options state) n (:n hop) rate (:sample-rate state)
        duration (* 1000.0 (/ n rate))
        end-ms (+ (:origin-ms state) (* 1000.0 (/ (:sample-index state) rate)))
        time-ms (- end-ms (/ duration 2.0))
        age (* 1000.0 (/ (:sample-index state) rate))
        confidence (clamp (/ age (:calibration-ms opts)) 0.0 1.0)
        calibrated? (>= confidence 1.0)
        rms (Math/sqrt (/ (:energy hop) n))
        kick (Math/sqrt (/ (:kick-energy hop) n))
        mean (get state :kick-mean kick) deviation (get state :kick-deviation 0.0)
        threshold (max (:noise-floor opts) (* mean (:kick-threshold-ratio opts))
                       (+ mean (* deviation (:kick-deviation-multiplier opts))))
        rise (max 0.0 (- kick (get state :previous-kick-rms kick)))
        armed? (or (:kick-armed? state) (< kick (* (:kick-latch-level state) (:kick-rearm-ratio opts))))
        onset? (and calibrated? armed? (> kick threshold)
                    (> rise (max (* 0.25 (:noise-floor opts)) (* mean (:kick-rise-ratio opts)))))
        kick? (and onset? (or (nil? (:last-kick-ms state))
                              (>= (- time-ms (:last-kick-ms state)) (:kick-refractory-ms opts))))
        kick-confidence (if kick? (clamp (- 1.0 (/ threshold kick)) 0.0 1.0) 0.0)
        old-envelope (get state :unclipped-rms-envelope rms)
        energy (approach old-envelope rms duration (if (> rms old-envelope)
                                                    (:energy-attack-ms opts) (:energy-release-ms opts)))
        baseline (get state :energy-baseline energy)
        reference (if (:breakdown? state) (:breakdown-reference state) baseline)
        relative (/ energy (max (:noise-floor opts) reference))
        candidate? (and calibrated? (> reference (:breakdown-reference-floor opts))
                        (if (:breakdown? state) (> relative (:breakdown-exit-ratio opts))
                            (< relative (:breakdown-enter-ratio opts))))
        candidate-since (when candidate? (or (:breakdown-candidate-since state) time-ms))
        confirmed? (and candidate-since
                        (>= (- time-ms candidate-since)
                            (if (:breakdown? state) (:breakdown-exit-ms opts) (:breakdown-enter-ms opts))))
        breakdown-before? (:breakdown? state)
        breakdown? (if confirmed? (not breakdown-before?) breakdown-before?)
        [bands band-features] (bands/finish-hop (:bands state) (:noise-floor opts))
        features (merge band-features {:time-ms time-ms :rms rms :peak (:peak hop) :kick-rms kick
                  :kick-threshold threshold :kick-rise rise :audio/energy (clamp energy 0.0 1.0)
                  :energy-dbfs (* 20.0 (Math/log10 (max 1.0e-12 rms)))
                  :energy-baseline reference :relative-energy relative
                  :energy-class (cond (< relative (:breakdown-enter-ratio opts)) :low
                                      (> relative 1.8) :high :else :typical)
                  :breakdown? breakdown? :calibrated? calibrated? :confidence confidence
                  :interpretation :relative-signal-energy-not-emotion
                  :sample-rate rate :window-samples n :window-latency-ms duration
                  :input-age-ms (- received-at-ms end-ms)})
        state (assoc state :bands bands :features features :previous-kick-rms kick
                     :kick-mean (approach mean kick duration (:kick-baseline-ms opts))
                     :kick-deviation (approach deviation (Math/abs (- kick mean)) duration (:kick-baseline-ms opts))
                     :kick-armed? (and armed? (not kick?))
                     :kick-latch-level (if kick? kick (if armed? 0.0 (max kick (:kick-latch-level state))))
                     :unclipped-rms-envelope energy
                     :energy-baseline (if breakdown? baseline (approach baseline energy duration (:energy-baseline-ms opts)))
                     :breakdown? breakdown? :breakdown-candidate-since (when-not confirmed? candidate-since)
                     :breakdown-reference (if breakdown? (or (:breakdown-reference state) reference) nil))
        aligned (when kick? (alignment clock time-ms (:beat-tolerance-ms opts)))
        [state events] (if kick?
                         (emit (assoc state :last-kick-ms time-ms) events :audio/kick time-ms received-at-ms
                               :adaptive-band-onset kick-confidence
                               {:kick-rms kick :threshold threshold :alignment aligned
                                :window-latency-ms duration}) [state events])
        beat-key (when aligned ((juxt :clock-source :clock-epoch :beat) aligned))
        [state events] (if (and kick? (:aligned? aligned)
                                (>= (:clock-confidence aligned) (:beat-clock-confidence-min opts))
                                (not= beat-key (:last-beat-key state)))
                         (emit (assoc state :last-beat-key beat-key) events :audio/beat time-ms received-at-ms
                               :kick-confirmed-clock (* kick-confidence (:clock-confidence aligned)) aligned)
                         [state events])
        [state events] (if confirmed?
                         (emit state events (if breakdown? :audio/breakdown-start :audio/breakdown-end)
                               candidate-since received-at-ms :relative-energy-hysteresis confidence
                               {:relative-energy relative :energy-baseline reference
                                :confirmed-at-ms time-ms :confirmation-ms (- time-ms candidate-since)})
                         [state events])
        [state events] (if (or (nil? (:last-energy-event-ms state))
                               (>= (- time-ms (:last-energy-event-ms state)) (:energy-event-ms opts)))
                         (emit (assoc state :last-energy-event-ms time-ms) events :audio/energy time-ms received-at-ms
                               :smoothed-rms confidence (select-keys features [:rms :peak :audio/energy :energy-dbfs
                                                                              :relative-energy :energy-class :breakdown?]))
                         [state events])]
    (section-step state events end-ms received-at-ms clock)))

(defn- segment
  [^doubles samples offset end coefficients ^doubles delay hop]
  (let [h (first coefficients) l (second coefficients)
        h0 (double (nth h 0)) h1 (double (nth h 1)) h2 (double (nth h 2))
        ha1 (double (nth h 3)) ha2 (double (nth h 4))
        l0 (double (nth l 0)) l1 (double (nth l 1)) l2 (double (nth l 2))
        la1 (double (nth l 3)) la2 (double (nth l 4))]
    (loop [index (long offset) energy (double (:energy hop)) kick-energy (double (:kick-energy hop))
           peak (double (:peak hop)) h-z1 (aget delay 0) h-z2 (aget delay 1)
           l-z1 (aget delay 2) l-z2 (aget delay 3)]
      (if (= index end)
        (do (aset-double delay 0 h-z1) (aset-double delay 1 h-z2)
            (aset-double delay 2 l-z1) (aset-double delay 3 l-z2)
            {:n (+ (:n hop) (- end offset)) :energy energy :kick-energy kick-energy :peak peak})
        (let [sample (aget samples index)
              hi (+ (* h0 sample) h-z1) lo (+ (* l0 hi) l-z1)]
          (recur (inc index) (+ energy (* sample sample)) (+ kick-energy (* lo lo))
                 (max peak (Math/abs sample)) (+ (* h1 sample) (- (* ha1 hi)) h-z2)
                 (- (* h2 sample) (* ha2 hi)) (+ (* l1 hi) (- (* la1 lo)) l-z2)
                 (- (* l2 hi) (* la2 lo))))))))

(defn- owned-samples [samples]
  (let [array? (and samples (.isArray (class samples)))
        values (cond array? samples
                     (sequential? samples) (vec (take 65537 samples))
                     :else (throw (ex-info "Audio samples must be an ordered numeric block" {})))
        n (if array? (java.lang.reflect.Array/getLength values) (count values))]
    (when-not (and (<= 1 n 65536)
                   (every? #(and (finite? %) (<= (Math/abs (double %)) 16.0)) values))
      (throw (ex-info "Audio block needs 1..65536 finite PCM samples with magnitude <=16" {})))
    (double-array values)))

(defn analyze
  "Consume a contiguous block and return {:state :features :events}.

  Input: {:samples normalized mono PCM :sample-rate Hz :started-at-ms capture
  timestamp of first sample :received-at-ms timestamp at boundary :clock optional
  {:time-ms :beat :bpm :source :source-epoch :confidence :playing? :session-id}}. The clock
  also accepts :epoch as a fallback for :source-epoch. Both timestamps use the
  same monotonic domain. received-at must follow the complete capture block.
  Blocks may have arbitrary lengths; a fixed hop is carried across calls.

  Discontinuities/rate changes reset filters/calibration and set :discontinuity?
  rather than inventing onsets/breakdowns. Regressing/overlapping timestamps are
  rejected. :audio/beat is a detected kick close to a supplied running clock beat;
  it is never a tempo estimate or clock mutation. Arrival age excludes actual
  execution/device latency, and timestamps are not filter-delay compensated.

  Opt-in section analysis requires explicit running state, confidence at or
  above :beat-clock-confidence-min, and a source epoch. Clock loss, source or
  session changes and phase discontinuities clear its comparison and feature.
  The comparison needs completed hops no more than 1/8 beat apart, and the
  latest confirmation is available under :audio/section in the features.
  The section event's :time-ms is the confirming hop end. :boundary-beat and
  :confirmation-delay-beats describe the earlier change on the supplied grid."
  [state {:keys [samples sample-rate started-at-ms received-at-ms clock]}]
  (when-not (and (= 1 (:version state)) (map? (:options state))
                 (finite? sample-rate) (<= 8000 sample-rate 192000)
                 (every? finite? [started-at-ms received-at-ms]) (<= 0 started-at-ms)
                 (some? samples))
    (throw (ex-info "Audio block requires analyzer state, sample rate and monotonic timestamps" {})))
  (let [^doubles values (owned-samples samples) n (alength values)
        end-ms (+ started-at-ms (* 1000.0 (/ n sample-rate)))
        tolerance (/ 500.0 sample-rate)
        _ (when (< received-at-ms (- end-ms 1.0e-7))
            (throw (ex-info "received-at-ms cannot precede capture completion" {})))
        _ (when (and (:last-end-ms state) (< started-at-ms (- (:last-end-ms state) tolerance)))
            (throw (ex-info "Audio blocks must not overlap or regress" {})))
        clock (validate-clock clock)
        discontinuity? (and (:sample-rate state)
                            (or (not= (double sample-rate) (double (:sample-rate state)))
                                (> (Math/abs (- started-at-ms (:last-end-ms state))) tolerance)))
        state (if (or (nil? (:sample-rate state)) discontinuity?)
                (initialize state (double sample-rate) started-at-ms) state)
        state (prepare-sections state clock)
        delay (double-array (:filter-delay state))
        [state events]
        (loop [offset 0 state state events []]
          (if (= offset n) [state events]
              (let [hop (:hop state) end (min n (+ offset (- (:hop-samples state) (:n hop))))
                    hop (segment values offset end (:filter-coefficients state) delay hop)
                    state (-> state (update :sample-index + (- end offset))
                              (update :bands bands/accumulate values offset end))]
                (if (= (:n hop) (:hop-samples state))
                  (let [[state events] (feature-step state events hop received-at-ms clock)]
                    (recur end (assoc state :hop {:n 0 :energy 0.0 :kick-energy 0.0 :peak 0.0}) events))
                  (recur end (assoc state :hop hop) events)))))]
    {:state (assoc state :last-end-ms end-ms :filter-delay (vec delay))
     :features (:features state) :events events :discontinuity? (boolean discontinuity?)}))

(defn new-subscriptions [] {})

(defn subscribe
  "Return a new caller-owned subscription map. Types is a set/sequence of event
  keywords, or :all. Handlers receive one event when publish! is explicitly run."
  [subscriptions id types handler]
  (when-not (and (map? subscriptions) (or (keyword? id) (string? id)) (ifn? handler)
                 (or (= types :all) (and (coll? types) (seq types) (every? keyword? types))))
    (throw (ex-info "Subscription requires identity, event types and handler" {})))
  (assoc subscriptions id {:types (if (= types :all) :all (set types)) :handler handler}))

(defn unsubscribe [subscriptions id] (dissoc subscriptions id))

(defn publish!
  "Explicit IO boundary, separate from analyze and effect evaluation. Calls each
  matching handler once in event/ID order. One handler error does not starve the
  rest; returns delivery count and exception data for host diagnostics. Do not
  call this function inside swap! or a retried state transition."
  [subscriptions events]
  (reduce (fn [result event]
            (reduce (fn [result [id {:keys [types handler]}]]
                      (if (or (= types :all) (contains? types (:type event)))
                        (try (handler event) (update result :delivered inc)
                             (catch Exception error
                               (update result :errors conj {:subscription-id id :event-sequence (:sequence event)
                                                            :exception error}))) result))
                    result (sort-by (comp str key) subscriptions)))
          {:delivered 0 :errors []} events))
