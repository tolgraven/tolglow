(ns tolglow.audio-lfo
  "Audio-shaped LFOs (#676): a band-isolated shape extracted once per musical
  window becomes a registered waveform that morphs slowly toward each new
  shape. The capture worker extracts shapes with `tolglow.audio.shape`; this
  namespace is the show owner's side. State is plain data under
  [:audio-lfos :lfos id]. Morph progress follows the transport beat, so it holds
  while the transport is stopped and resumes where it left off.

  Each LFO publishes one live registered waveform whose template is its latest
  target shape, for pickers, previews, selectors and effects that lower a
  waveform. A parameter LFO bound to that waveform samples the morphing shape
  through `live-table`."
  (:require [afterglow.time :as time]
            [tolglow.audio.shape :as shape]
            [tolglow.registry :as registry]
            [tolglow.signals :as signals]))

(def schema-version 1)

(def operations
  "Owner command ops."
  #{:audio-lfo/configure :audio-lfo/remove})

(def curves #{:linear :smoothstep :sine})

(def rest-shapes #{:low :mid :sine})

(def silence-policies
  "`:hold` keeps the current target; `:release` morphs to the rest shape."
  #{:hold :release})

(def history-size 4)

(def max-lfos
  "Audio LFOs a show can configure. Each adds window accumulation on the
  capture thread and, in cycle mode, a YIN pass per window on the shape worker."
  8)

(def spec-keys [:band :extract :window :points :silence-db :min-confidence])

(def default-config
  (merge shape/default-spec
         {:morph {:bars 1} :curve :smoothstep :smoothing 0.0 :on-silence :hold :rest-shape :mid}))

(defn- fail!
  ([message] (fail! message {}))
  ([message details] (throw (ex-info message {:control/error :invalid-audio-lfo :details details}))))

(defn- check [condition message & [details]] (when-not condition (fail! message (or details {}))))

(defn- musical? [period]
  (and (map? period) (= 1 (count period)) (#{:beats :bars :phrases} (ffirst period))
       (time/finite-number? (val (first period))) (pos? (val (first period)))))

(defn config-errors
  "Return [] or path/message errors for a complete or partial configuration."
  [config]
  (let [{:keys [name morph curve smoothing on-silence rest-shape] :as config} (merge default-config config)
        problem (fn [path message] [{:path path :message message}])]
    (vec (concat
          (shape/validate-spec (select-keys config spec-keys))
          (when-not (and (string? name) (<= 1 (count name) 64)) (problem [:name] "Name the LFO (1–64 characters)"))
          (when-not (musical? morph) (problem [:morph] "Morph time needs positive beats, bars or phrases"))
          (when-not (curves curve) (problem [:curve] "Curve is :linear, :smoothstep or :sine"))
          (when-not (and (time/finite-number? smoothing) (<= 0 smoothing 0.95))
            (problem [:smoothing] "Smoothing keeps 0..0.95 of the previous target"))
          (when-not (silence-policies on-silence) (problem [:on-silence] "Silence policy is :hold or :release"))
          (when-not (rest-shapes rest-shape) (problem [:rest-shape] "Rest shape is :low, :mid or :sine"))
          (when-not (registry/data? config) (problem [] "Configuration must be portable data"))))))

(defn waveform-id [lfo-id] (keyword "audio" (name lfo-id)))

(defn extractor-spec [config] (select-keys (merge default-config config) spec-keys))

(defn spec-digest
  "Identity of an extractor spec. Extractions carry it, so a shape measured
  under an older band or window never lands after a reconfiguration."
  [config]
  (hash (extractor-spec config)))

(defn- round4 [x] (/ (Math/round (* 10000.0 (double x))) 10000.0))

(defn rest-shape [kind points]
  (mapv (fn [j] (case kind
                  :low 0.0 :mid 0.5
                  :sine (round4 (+ 0.5 (* 0.5 (Math/sin (/ (* 2.0 Math/PI j) points)))))))
        (range points)))

(defn- resample
  "Periodic resampling of a shape to another point count."
  [table points]
  (if (= points (count table)) table
      (mapv #(round4 (shape/table-value table (/ % (double points)))) (range points))))

;; Morph -------------------------------------------------------------------------

(defn progress
  "Morph progress 0..1 at a transport beat, after the curve. Without a start or
  a beat the morph is complete. A transport that jumped back before the start
  also counts as complete; a seek through `rebase-clock` keeps progress."
  [{:keys [morph-start-beat morph-beats config]} beat]
  (if (or (nil? morph-start-beat) (not (time/finite-number? beat))
          (not (and (time/finite-number? morph-beats) (pos? morph-beats))) (< beat morph-start-beat))
    1.0
    (let [p (min 1.0 (/ (- beat morph-start-beat) morph-beats))]
      (case (:curve config :smoothstep)
        :linear p
        :smoothstep (* p p (- 3.0 (* 2.0 p)))
        :sine (- 0.5 (* 0.5 (Math/cos (* Math/PI p))))))))

(defn shape-at
  "The morphed shape table at a transport beat."
  [{:keys [from to] :as lfo} beat]
  (let [p (progress lfo beat)]
    (if (or (>= p 1.0) (not= (count from) (count to))) to
        (mapv (fn [a b] (+ a (* p (- b a)))) from to))))

(defn live-table
  "The morphing table of a live audio waveform at a beat, or nil when the
  waveform has no owner state. Used by frame-owner waveform sampling."
  [model waveform beat]
  (when (and (:live? waveform) (= :audio-lfo (get-in waveform [:source :type])))
    (when-let [lfo (get-in model [:audio-lfos :lfos (get-in waveform [:source :lfo-id])])]
      (when (= (:id waveform) (:waveform-id lfo))
        (shape-at lfo beat)))))

;; Registered waveform -----------------------------------------------------------

(defn table-expression
  "Numeric IR for periodic linear interpolation of a table over the template's
  phase: a sum of triangular hats at u = n × fract(phase), with the first point
  repeated at u = n. At most 63 points fit the IR's 64-argument sum."
  [table]
  (let [n (count table)
        u {:op :multiply :args [{:op :const :value (double n)}
                                {:op :fract :args [{:op :param :key signals/phase-input}]}]}
        hat (fn [i] {:op :max :args [{:op :const :value 0.0}
                                     {:op :subtract :args [{:op :const :value 1.0}
                                                           {:op :abs :args [{:op :subtract :args [u {:op :const :value (double i)}]}]}]}]})]
    {:op :add :args (mapv (fn [i] {:op :multiply :args [{:op :const :value (double (nth table (mod i n)))} (hat i)]})
                          (range (inc n)))}))

(defn waveform-definition
  "The registered waveform for an LFO: its current target as a static table.
  `:live? true` keeps only the newest version in registry history."
  [lfo version]
  (let [{:keys [config to]} lfo]
    {:id (:waveform-id lfo) :version version :name (str "Audio · " (:name config)) :category :audio
     :range [0.0 1.0] :params {} :discontinuities [] :live? true
     :source {:type :audio-lfo :lfo-id (:id lfo) :band (:band config) :extract (:extract config)}
     :expression (table-expression to)}))

(defn- publish-waveform [model lfo]
  (let [registry (or (:waveforms model) (signals/new-registry))
        current (get-in registry [:waveforms (:waveform-id lfo)])
        candidate (waveform-definition lfo (:version current 1))]
    (if (= (dissoc current :version) (dissoc candidate :version))
      model
      (assoc model :waveforms (signals/publish-live registry candidate)))))

;; Commit ------------------------------------------------------------------------

(defn- meter [model] (time/normalize-meter (get-in model [:transport :meter])))

(defn commit
  "Apply one extraction at the owner's transport beat. An :ok shape is smoothed
  into the target and the morph restarts from the shape currently showing, so
  the LFO never jumps. Silence holds or releases to the rest shape; low
  confidence always holds. Returns the new LFO state."
  [lfo extraction beat model-meter]
  (let [{:keys [config to]} lfo
        points (:points config)
        target (case (:status extraction)
                 :ok (let [fresh (resample (mapv round4 (:shape extraction)) points)
                           keep (double (:smoothing config))]
                       (mapv #(round4 (+ (* keep %1) (* (- 1.0 keep) %2))) to fresh))
                 :silent (when (= :release (:on-silence config)) (rest-shape (:rest-shape config) points))
                 nil)
        lfo (assoc lfo :status (:status extraction)
                   :last (cond-> (select-keys extraction [:status :seq :level-db :confidence :frequency-hz :periods :window])
                           (and (time/finite-number? beat) (get-in extraction [:window :end-beat]))
                           (assoc :latency-beats (- beat (get-in extraction [:window :end-beat])) :committed-at-beat beat))
                   :source-seq (select-keys extraction [:run-id :seq]))
        lfo (cond-> lfo (:input extraction) (assoc :input (:input extraction)))]
    (if (and target (not= target to))
      (-> lfo
          (assoc :from (shape-at lfo beat) :to target
                 :morph-start-beat (when (time/finite-number? beat) (double beat))
                 :morph-beats (double (time/duration->beats (:morph config) model-meter)))
          (update :history #(vec (take-last history-size (conj (vec %) to))))
          (update :commits (fnil inc 0)))
      lfo)))

(defn- newer? [lfo extraction]
  (let [{:keys [run-id seq]} (:source-seq lfo)]
    (or (not= run-id (:run-id extraction)) (nil? seq) (> (:seq extraction) seq))))

(defn admit
  "Commit the latest extraction per LFO from the capture worker. Extractions
  for removed LFOs, an older extractor spec or an already committed sequence
  are ignored. Republishes a waveform whose target changed."
  [model shapes]
  (let [beat (get-in model [:transport :beat])
        model-meter (meter model)]
    (reduce-kv (fn [model id extraction]
                 (let [lfo (get-in model [:audio-lfos :lfos id])]
                   (if (and lfo (map? extraction) (integer? (:seq extraction))
                            (= (:spec-digest extraction) (spec-digest (:config lfo)))
                            (newer? lfo extraction))
                     (let [updated (commit lfo extraction beat model-meter)
                           model (assoc-in model [:audio-lfos :lfos id] updated)]
                       (if (= (:to updated) (:to lfo)) model (publish-waveform model updated)))
                     model)))
               model (or shapes {}))))

(defn extractor-specs
  "{lfo-id spec} for the capture worker, each with its :digest."
  [model]
  (into {} (map (fn [[id lfo]] [id (assoc (extractor-spec (:config lfo)) :digest (spec-digest (:config lfo)))]))
        (get-in model [:audio-lfos :lfos])))

;; Commands ----------------------------------------------------------------------

(defn new-state [] {:schema-version schema-version :lfos {}})

(defn- new-lfo [id config]
  (let [shape (rest-shape (:rest-shape config) (:points config))]
    {:id id :waveform-id (waveform-id id) :config config :from shape :to shape
     :morph-start-beat nil :morph-beats nil :status :waiting :history [] :commits 0}))

(defn configure
  "Create an LFO or change its configuration. Changing points resamples the
  current shapes; changing the extractor spec waits for the next window."
  [model {:keys [lfo-id config name]}]
  (check (keyword? lfo-id) "An audio LFO needs a keyword ID")
  (check (or (nil? config) (map? config)) "Configuration must be a map")
  (let [state (or (:audio-lfos model) (new-state))
        existing (get-in state [:lfos lfo-id])
        _ (check (or existing (< (count (:lfos state)) max-lfos))
                 (str "A show can have at most " max-lfos " audio LFOs. Remove one before adding another.")
                 {:max max-lfos})
        unknown (remove (set (concat (keys default-config) [:name])) (keys config))
        _ (check (empty? unknown) (str "Unknown audio LFO settings " (vec unknown)) {:keys (vec unknown)})
        config (merge default-config (:config existing) {:name (or name (get-in existing [:config :name]) (clojure.core/name lfo-id))}
                      config)
        errors (config-errors config)
        _ (check (empty? errors) (str "Invalid audio LFO: " (:message (first errors))) {:errors errors})
        lfo (if existing
              (cond-> (assoc existing :config config)
                (not= (:points config) (count (:to existing)))
                (-> (update :from resample (:points config)) (update :to resample (:points config))
                    (update :history #(mapv (fn [t] (resample t (:points config))) %)))
                (not= (spec-digest config) (spec-digest (:config existing)))
                (assoc :status :waiting :source-seq nil))
              (new-lfo lfo-id config))
        taken (get-in model [:waveforms :waveforms (:waveform-id lfo)])]
    (check (or (nil? taken) (= {:type :audio-lfo :lfo-id lfo-id} (select-keys (:source taken) [:type :lfo-id])))
           "Another waveform already uses this audio LFO's waveform ID" {:waveform-id (:waveform-id lfo)})
    (publish-waveform (assoc model :audio-lfos (assoc-in state [:lfos lfo-id] lfo)) lfo)))

(defn apply-operation
  "Pure [model command] operation for afterglow.control.model/apply-domain-command."
  [model {:keys [op lfo-id] :as command}]
  (case op
    :audio-lfo/configure
    (let [created? (not (get-in model [:audio-lfos :lfos lfo-id]))
          model (configure model command)]
      {:model model
       :events [{:type (if created? :audio-lfo/created :audio-lfo/configured) :lfo-id lfo-id}]
       :result {:lfo-id lfo-id :waveform-id (waveform-id lfo-id)}})

    :audio-lfo/remove
    ;; The registered waveform stays as a frozen snapshot of the last target,
    ;; so bindings to it keep working.
    (do (check (get-in model [:audio-lfos :lfos lfo-id]) "Unknown audio LFO" {:lfo-id lfo-id})
        {:model (update-in model [:audio-lfos :lfos] dissoc lfo-id)
         :events [{:type :audio-lfo/removed :lfo-id lfo-id}]})))

;; Clock, persistence and projection ---------------------------------------------

(defn rebase-clock
  "Shift beat anchors by offset beats when the show rebases its musical clock."
  [state offset]
  (update state :lfos update-vals
          (fn [lfo] (cond-> lfo (time/finite-number? (:morph-start-beat lfo)) (update :morph-start-beat + offset)))))

(defn- table? [table points]
  (and (vector? table) (= points (count table)) (every? #(and (time/finite-number? %) (<= 0 % 1)) table)))

(defn validate-state
  "Validate saved audio LFO data without consulting the show."
  [state]
  (when-not (and (map? state) (= schema-version (:schema-version state)) (map? (:lfos state)) (registry/data? state))
    (throw (ex-info "Invalid saved audio LFO state" {:path [:audio-lfos]})))
  (doseq [[id {:keys [config from to history] :as lfo}] (:lfos state)]
    (let [errors (config-errors config)
          points (:points config)]
      (when-not (and (keyword? id) (= id (:id lfo)) (empty? errors) (= (waveform-id id) (:waveform-id lfo))
                     (table? from points) (table? to points) (vector? history) (every? #(table? % points) history))
        (throw (ex-info "Invalid saved audio LFO" {:path [:audio-lfos :lfos id] :errors errors})))))
  state)

(defn restore
  "A restored show resumes each LFO at its saved target, waiting for the next
  window of a new capture."
  [state]
  (validate-state state)
  (update state :lfos update-vals
          #(assoc % :from (:to %) :morph-start-beat nil :status :waiting :source-seq nil)))

(defn projection
  "Studio and REPL view of every LFO at the model's transport beat."
  [model]
  (let [beat (get-in model [:transport :beat])]
    (->> (vals (get-in model [:audio-lfos :lfos]))
         (sort-by (comp str :id))
         (mapv (fn [{:keys [config] :as lfo}]
                 (assoc (select-keys lfo [:id :waveform-id :config :status :last :input :from :to :history
                                          :morph-start-beat :morph-beats :commits])
                        :band-label (shape/band-label (:band config))
                        :current (shape-at lfo beat)
                        :progress (progress lfo beat)))))))

(defn lfo-view [model lfo-id]
  (some #(when (= lfo-id (:id %)) %) (projection model)))

;; Offline replay ----------------------------------------------------------------

(defn timing
  "Median, p99 and maximum of nanosecond samples, in microseconds."
  [nanos]
  (let [sorted (vec (sort nanos)) n (count sorted)
        at (fn [q] (/ (nth sorted (min (dec n) (long (Math/floor (* q n))))) 1000.0))]
    (when (pos? n) {:median (at 0.5) :p99 (at 0.99) :max (/ (peek sorted) 1000.0) :count n})))

(defn replay
  "Run mono PCM through the same extractor, admission and morph an LFO uses
  live, on a steady transport that starts at beat 0 and time 0. Blocks arrive
  when their last sample has been captured and the owner admits them on the
  next frame. Options: :config (LFO settings), :sample-rate (48000), :bpm (120),
  :block-frames (480), :fps (60), :scan scan period in beats (1).

  Returns :commits (each with the frame, beat, latency and the largest table
  change at that frame), :frames [beat value] of the LFO scanned over the scan
  period, :steps (largest frame-to-frame value change overall and on commit
  frames), and median, p99 and maximum costs in microseconds: :extract-us per
  block on the capture side (`shape/advance-bank`), :finish-us per window-end
  job (`shape/finish-job`, which runs on the shape worker live and inline here)
  and :sample-us per frame."
  [^doubles samples {:keys [config sample-rate bpm block-frames fps scan]
                     :or {sample-rate 48000.0 bpm 120.0 block-frames 480 fps 60.0 scan 1.0}}]
  (let [sample-rate (double sample-rate) bpm (double bpm)
        beat-at #(* (/ (double %) 60000.0) bpm)
        model (configure {:transport {:beat 0.0 :bpm bpm :playing? true :meter {:beats-per-bar 4}}}
                         {:lfo-id :replay :config config})
        spec (get (extractor-specs model) :replay)
        n (alength samples)
        blocks (long (Math/ceil (/ n (double block-frames))))
        duration-ms (* 1000.0 (/ n sample-rate))
        frames (long (Math/floor (/ (* duration-ms fps) 1000.0)))]
    (loop [frame 0 block 0 extractor (shape/new-bank sample-rate)
           pending nil model model out [] commits [] extract-ns [] finish-ns [] sample-ns []]
      (if (= frame frames)
        (let [steps (map (fn [[_ a] [_ b]] (Math/abs (- (double b) (double a)))) out (rest out))
              commit-frames (set (map :frame commits))
              indexed (map-indexed vector steps)]
          {:commits commits :frames out
           :steps {:max (reduce max 0.0 steps)
                   :max-on-commit (reduce max 0.0 (keep (fn [[i step]] (when (commit-frames (inc i)) step)) indexed))}
           :extract-us (timing extract-ns)
           :finish-us (timing finish-ns)
           :sample-us (timing sample-ns)
           :blocks block :duration-ms duration-ms})
        (let [time-ms (/ (* 1000.0 frame) fps)
              ;; Deliver every block captured by this frame's time.
              [block extractor pending extract-ns finish-ns]
              (loop [block block extractor extractor pending pending extract-ns extract-ns finish-ns finish-ns]
                (let [start (* block block-frames)
                      end (min n (+ start block-frames))
                      end-ms (* 1000.0 (/ end sample-rate))]
                  (if (or (>= start n) (> end-ms time-ms))
                    [block extractor pending extract-ns finish-ns]
                    (let [chunk (java.util.Arrays/copyOfRange samples (int start) (int end))
                          t0 (System/nanoTime)
                          {:keys [bank jobs]}
                          (shape/advance-bank extractor {:replay spec}
                                              {:samples chunk :sample-rate sample-rate
                                               :started-at-ms (* 1000.0 (/ start sample-rate))
                                               :clock {:time-ms 0.0 :beat 0.0 :bpm bpm :playing? true
                                                       :meter {:beats-per-bar 4}}})
                          elapsed (- (System/nanoTime) t0)
                          finished (mapv (fn [job]
                                           (let [t (System/nanoTime) e (shape/finish-job job)]
                                             [e (- (System/nanoTime) t)])) jobs)]
                      (recur (inc block) bank
                             (if-let [[e] (peek finished)]
                               (assoc e :run-id :replay :arrived-ms end-ms)
                               pending)
                             (conj extract-ns elapsed)
                             (into finish-ns (map second) finished))))))
              beat (beat-at time-ms)
              model (assoc-in model [:transport :beat] beat)
              before (get-in model [:audio-lfos :lfos :replay])
              admitted (if pending (admit model {:replay pending}) model)
              lfo (get-in admitted [:audio-lfos :lfos :replay])
              t0 (System/nanoTime)
              table (shape-at lfo beat)
              value (shape/table-value table (/ beat scan))
              elapsed (- (System/nanoTime) t0)
              committed? (and pending (not= (:commits lfo) (:commits before)))
              commit (when pending
                       {:frame frame :beat beat :time-ms time-ms :status (:status pending)
                        :committed? (boolean committed?)
                        :latency-ms (- time-ms (* 60000.0 (/ (get-in pending [:window :end-beat]) bpm)))
                        :window (:window pending) :level-db (:level-db pending)
                        :confidence (:confidence pending) :frequency-hz (:frequency-hz pending)
                        :table-jump (reduce max 0.0 (map #(Math/abs (- (double %1) (double %2)))
                                                         (shape-at before beat) table))})]
          (recur (inc frame) block extractor nil admitted (conj out [beat value])
                 (cond-> commits commit (conj commit)) extract-ns finish-ns (conj sample-ns elapsed)))))))
