(ns tolglow.timeline
  "Musical timeline projection: what just happened and what is about to happen,
  on the transport's beat clock, in a window of bars around now. Studio, the
  REPL and agents read this one map (docs/timeline.md). Everything here is a
  pure function of committed show data. No clock, device or effect is read or
  advanced, and no stateful signal is evaluated."
  (:require [afterglow.render.easing :as easing]
            [afterglow.time :as time]
            [tolglow.event-subscribers :as subscribers]))

(def version 1)

(def query-id
  "Studio state-stream query: [:timeline/view {:bars n}]."
  :timeline/view)

(def default-bars 8)

(def max-bars 32)

(def ^:private beat-line-bars
  "Quarter-note lines are listed up to this many bars a side; wider windows
  keep bars and phrases only."
  16)

(def ^:private shape-samples 17)

(def ^:private section-limit 32)

(def ^:private stale-audio-ms 2000.0)

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

(defn- round-to [x places]
  (if (finite? x)
    (let [scale (Math/pow 10.0 places)] (/ (Math/round (* scale (double x))) scale))
    x))

(defn options
  "Validate a window request: {:bars n} shows n bars before and after now."
  [{:keys [bars] :or {bars default-bars} :as request}]
  (when-not (and (or (nil? request) (map? request)) (every? #{:bars} (keys request))
                 (integer? bars) (<= 1 bars max-bars))
    (throw (ex-info (str "A timeline window is {:bars n} with n from 1 to " max-bars) {:options request})))
  {:bars bars})

;; Window and grid -------------------------------------------------------------

(defn- window
  "Visible span plus a one-bar margin, snapped to bars so that the listed
  events, ramps, sections and grid lines change only at a bar crossing."
  [beat meter bars]
  (let [bpb (:beats-per-bar meter)
        bar (Math/floor (/ beat bpb))
        span (double (* bars bpb))]
    {:bars bars :past-beats span :future-beats span
     :from-beat (double (* bpb (- bar bars 1))) :to-beat (double (* bpb (+ bar bars 2)))}))

(defn- multiples
  "Exact multiples of step within [from, to], as [index beat] pairs."
  [step from to]
  (let [step (rationalize step)
        first-index (long (Math/ceil (double (/ (rationalize from) step))))
        last-index (long (Math/floor (double (/ (rationalize to) step))))]
    (for [index (range first-index (inc last-index))] [index (double (* step index))])))

(defn grid
  "Beat, bar and phrase lines counted from beat zero in the transport meter,
  the same origin the scheduler's :at and :grid positions use. A line carries
  its zero-based :bar and :phrase index where it starts one."
  [meter {:keys [from-beat to-beat bars]}]
  (let [{:keys [beats-per-bar bars-per-phrase]} meter
        phrase-beats (* beats-per-bar bars-per-phrase)
        beats (when (<= bars beat-line-bars)
                (for [[_ beat] (multiples 1 from-beat to-beat)] [beat {:beat beat :kind :beat}]))
        bar-lines (for [[index beat] (multiples beats-per-bar from-beat to-beat)]
                    [beat {:beat beat :kind :bar :bar index}])
        phrases (for [[index beat] (multiples phrase-beats from-beat to-beat)]
                  [beat {:beat beat :kind :phrase :bar (* index bars-per-phrase) :phrase index}])]
    ;; Later kinds win where lines coincide: phrase over bar over beat.
    {:lines (vec (vals (into (sorted-map) (concat beats bar-lines phrases))))}))

;; Positions ---------------------------------------------------------------------

(defn position
  "Zero-based transport coordinates of a beat, as the scheduler's :at accepts
  them: {:phrase p :bar b :beat x}, with :bar counted from beat zero and :beat
  inside that bar."
  [meter beat]
  (when (finite? beat)
    (let [{:keys [beats-per-bar bars-per-phrase]} meter
          bar (long (Math/floor (double (/ beat beats-per-bar))))]
      {:bar bar :beat (round-to (- beat (* bar beats-per-bar)) 3)
       :phrase (long (Math/floor (/ (double bar) bars-per-phrase)))})))

(defn- with-position [meter from to entry]
  (cond-> entry (finite? (get entry from)) (assoc to (position meter (get entry from)))))

;; Events and ramps --------------------------------------------------------------

(defn- title [{:keys [label op event-id]}]
  (or label (when op (str (namespace op) " " (name op))) event-id))

(defn- within? [{:keys [from-beat to-beat]} beat]
  (and (finite? beat) (<= from-beat beat to-beat)))

(defn- pending-event [entry]
  (let [scheduled? (= :scheduled (:origin entry))]
    (-> (select-keys entry [:event-id :op :category :label :origin :source :due-beat :anchor :posted-beat
                            :subscriber-id :step :steps :target :param :variable :value :select :macro-id
                            :at :in :grid :delay])
        (assoc :key (str "pending/" (:event-id entry) "/" (:step entry 0))
               :state :pending :beat (:due-beat entry) :title (title entry)
               ;; :schedule/cancel matches scheduled events; subscriber steps belong to their subscriber.
               :cancellable? scheduled?))))

(defn- outcome-event [entry]
  (-> (select-keys entry [:event-id :op :category :label :source :due-beat :at-beat :time-ms :subscriber-id
                          :message :reason :by :destination :to :value :rebased-beats])
      ;; Keyed by the owner time it happened at, which a loop or seek never
      ;; moves, so Studio keeps the same element (and its focus) across one.
      (assoc :key (str (name (:status entry)) "/" (:event-id entry) "/" (or (:time-ms entry) (:at-beat entry)))
             :state (:status entry) :beat (:at-beat entry) :title (title entry) :cancellable? false)
      ;; A loop or seek moved :due-beat with the transport; the beat it was
      ;; posted for is the original coordinate.
      (cond-> (and (number? (:rebased-beats entry)) (number? (:due-beat entry)))
        (assoc :posted-due-beat (- (:due-beat entry) (:rebased-beats entry))))))

(defn- ramp-outcome? [entry] (and (= :ramp (:category entry)) (contains? entry :destination)))

(defn- curve-shape
  "Eased fraction at evenly spaced progress, so a display draws the curve the
  owner samples without a second easing implementation."
  [curve]
  (try
    (let [sample (easing/evaluator curve)
          last-index (dec shape-samples)]
      (mapv #(round-to (sample (/ (double %) last-index)) 4) (range shape-samples)))
    (catch clojure.lang.ExceptionInfo _ nil)))

(defn- beats-per-ms [{:keys [bpm]}]
  (if (and (finite? bpm) (pos? bpm)) (/ bpm 60000.0) 0.0))

(defn- ms->beat
  "Place a logical-millisecond instant on the beat axis at the current tempo.
  An :ms ramp keeps running while the transport is paused, so this is a display
  estimate that the owner re-projects every committed frame."
  [transport beat now-ms instant-ms]
  (+ beat (* (beats-per-ms transport) (- instant-ms now-ms))))

(defn- ramp-span [transport beat now-ms {:keys [unit start end]}]
  (when (and (finite? start) (finite? end))
    (if (= :ms unit)
      [(ms->beat transport beat now-ms start) (ms->beat transport beat now-ms end)]
      [(double start) (double end)])))

(defn- ramp-bar [transport beat now-ms state entry]
  (when-let [[start-beat end-beat] (ramp-span transport beat now-ms entry)]
    (let [curve (:curve entry :linear)]
      (cond-> (-> (select-keys entry [:event-id :label :source :destination :from :to :over :unit :reason :by])
                  ;; :started-ms (the frame time it started) survives a rebase, unlike :start.
                  (assoc :key (str (:event-id entry) "/" (pr-str (:destination entry)) "/"
                                   (or (:started-ms entry) (:start entry)))
                         :state state :category :ramp :op :param/ramp :title (title (assoc entry :op :param/ramp))
                         :start-beat start-beat :end-beat end-beat :curve curve :shape (curve-shape curve)
                         :cancellable? (= :running state)))
        (= :cancelled state) (assoc :stopped-beat (:at-beat entry))
        ;; A millisecond ramp ends on the owner's clock, not on a beat.
        (and (= :running state) (= :ms (:unit entry)))
        (assoc :remaining-ms (round-to (max 0.0 (- (:end entry) now-ms)) 0))))))

(defn- pending-ramp
  "A scheduled :param/ramp that has not fired. It starts at its due beat and
  captures :from then, so only the planned span and curve are known."
  [transport meter entry]
  (let [[unit amount] (first (:over entry))
        length (when (finite? amount)
                 (if (= :ms unit) (* amount (beats-per-ms transport)) (time/duration->beats (:over entry) meter)))
        curve (:curve entry :linear)]
    (when (and length (pos? length))
      (-> (select-keys entry [:event-id :label :source :to :over :origin :due-beat :anchor :select :param :variable :target])
          (assoc :key (str "pending/" (:event-id entry)) :state :pending :category :ramp :op :param/ramp
                 :title (title entry) :unit (if (= :ms unit) :ms :beats)
                 :start-beat (:due-beat entry) :end-beat (+ (:due-beat entry) (double length))
                 :curve curve :shape (curve-shape curve)
                 :cancellable? (= :scheduled (:origin entry)))))))

(defn- overlaps? [{:keys [from-beat to-beat]} {:keys [start-beat end-beat]}]
  (and (<= start-beat to-beat) (>= end-beat from-beat)))

(defn- events-and-ramps
  "Split the shared schedule projection into point events and ramp bars inside
  the window. A ramp's own start marker is its bar: the fired :param/ramp
  outcome is listed only when no bar can be drawn for it."
  [model beat window]
  (let [{:keys [upcoming ramps recent]} (subscribers/schedule-view model)
        transport (:transport model)
        meter (time/normalize-meter (:meter transport))
        now-ms (:time-ms model 0)
        ramp-op? #(= :param/ramp (:op %))
        ;; Outcomes carry the beat they happened at, which a loop or seek moves
        ;; with the transport (subscribers/rebase-clock). Guard anyway: a beat
        ;; ahead of now is not history.
        past (filterv #(and (finite? (:at-beat %)) (<= (:at-beat %) (+ beat 1.0e-6))) recent)
        running (keep #(ramp-bar transport beat now-ms :running %) ramps)
        planned (keep #(pending-ramp transport meter %) (filter ramp-op? upcoming))
        finished (keep #(when (ramp-outcome? %) (ramp-bar transport beat now-ms (:status %) %)) past)
        bars (filterv #(overlaps? window %) (concat finished running planned))
        drawn (into #{} (map :event-id) (concat finished running))
        planned-ids (into #{} (map :event-id) planned)
        pending (->> upcoming
                     (remove #(and (ramp-op? %) (planned-ids (:event-id %))))
                     (map pending-event))
        outcomes (->> past
                      (remove #(and (ramp-outcome? %) (drawn (:event-id %))))
                      (remove #(and (ramp-op? %) (= :fired (:status %)) (drawn (:event-id %))))
                      (map outcome-event))]
    {:events (->> (concat outcomes pending) (filter #(within? window (:beat %)))
                  (sort-by (juxt :beat :key)) vec)
     :ramps (vec (sort-by (juxt :start-beat :key) bars))}))

;; Sections ------------------------------------------------------------------------

(defn remember-sections
  "Keep a bounded history of confirmed causal section changes under
  [:variables :audio :sections], newest last. The analyzer publishes each
  change once; a timeline needs the recent boundaries, not only the latest."
  [model events]
  (let [changes (into [] (comp (filter #(= :audio/section-change (:type %)))
                               (filter #(finite? (:boundary-beat %)))
                               (map #(select-keys % [:boundary-beat :confirmed-at-beat :confidence :evidence
                                                     :clock-source :clock-epoch :time-ms :sequence])))
                      events)]
    (if (seq changes)
      (update-in model [:variables :audio :sections]
                 #(vec (take-last section-limit (into (vec %) changes))))
      model)))

(def ^:private driver-labels {:bass-pattern "Bass" :groove-accents "Groove"})

(defn- causal-label [drivers]
  (if-let [names (seq (keep driver-labels drivers))]
    (str (apply str (interpose " + " names)) " change")
    "Section change"))

(defn- causal-sections
  "Confirmed boundaries on the current clock, oldest first. Each section runs
  to the next boundary; the newest is open and ends at now."
  [model beat window]
  (let [{:keys [source source-epoch]} (:transport model)
        current? (fn [{:keys [clock-source clock-epoch boundary-beat]}]
                   (and (= clock-source (or source :manual)) (= clock-epoch source-epoch)
                        (<= boundary-beat beat)))
        boundaries (->> (get-in model [:variables :audio :sections])
                        (filter current?)
                        (sort-by :boundary-beat)
                        ;; A repeated boundary keeps its latest confirmation.
                        (partition-by :boundary-beat) (map last))
        ends (concat (map :boundary-beat (rest boundaries)) [nil])]
    (for [[{:keys [boundary-beat confirmed-at-beat confidence evidence]} end] (map vector boundaries ends)
          :when (>= (or end beat) (:from-beat window))]
      (cond-> {:key (str "causal/" boundary-beat) :source :causal :state :confirmed
               :start-beat (double boundary-beat) :label (causal-label (:drivers evidence))
               :drivers (vec (:drivers evidence)) :confidence confidence
               :confirmed-at-beat confirmed-at-beat}
        end (assoc :end-beat (double end))
        (nil? end) (assoc :open? true)))))

(defn- family-label [{:keys [id family-id]}]
  (if (and (integer? family-id) (<= 0 family-id 25))
    (str (char (+ (int \A) family-id)))
    (str "Section " id)))

(defn- reference-sections
  "Sections of the matched recording placed on the transport through the
  reference lock: position and rate say how far away each boundary is, and the
  transport tempo turns that into beats. They are a forecast only while the
  lock holds; the DJ can always play something else."
  [model beat window {:keys [profile lock]}]
  (when (and (= :locked (:status lock)) (finite? (:position-ms lock)) (finite? (:rate lock)) (pos? (:rate lock))
             (= (select-keys (:reference lock) [:id :version]) (select-keys (:reference profile) [:id :version])))
    (let [rate (beats-per-ms (:transport model))
          at (fn [position-ms] (+ beat (* rate (/ (- position-ms (:position-ms lock)) (:rate lock)))))]
      (for [{:keys [id start-position-ms end-position-ms boundary-confidence] :as section} (:sections profile)
            :let [start (at start-position-ms) end (at end-position-ms)]
            :when (overlaps? window {:start-beat start :end-beat end})]
        {:key (str "reference/" id) :source :reference
         :state (if (> start beat) :predicted :matched)
         :start-beat start :end-beat end :label (family-label section)
         :section-id id :family-id (:family-id section) :confidence boundary-confidence}))))

(defn- match-status [{:keys [profile lock]}]
  (when lock
    (cond-> (select-keys lock [:status :reason :reference :confirmations :rate :lookahead-ms])
      profile (assoc :profile-reference (:reference profile)))))

;; Audio ---------------------------------------------------------------------------

(defn- tempo-anchors
  "Time-to-beat anchors for captured audio: the beat markers the capture worker
  took from the authoritative transport as each block arrived, then the
  committed transport itself. Only the latest run with rising beats is kept, so
  a seek or loop does not fold older audio over the new position."
  [markers transport model-time]
  (let [now-ms (or (:time-ms transport) model-time)
        now {:time-ms now-ms :beat (:beat transport)}
        ordered (->> markers
                     (filter #(and (finite? (:time-ms %)) (finite? (:beat %)) (<= (:time-ms %) now-ms)
                                   (<= (:beat %) (:beat now))))
                     (sort-by :time-ms))
        rising (reduce (fn [run marker]
                         (if (and (seq run) (not (and (> (:time-ms marker) (:time-ms (peek run)))
                                                      (> (:beat marker) (:beat (peek run))))))
                           [marker]
                           (conj run marker)))
                       [] ordered)]
    (if (and (seq rising) (<= (:time-ms now) (:time-ms (peek rising))))
      rising
      (conj rising now))))

(defn- beat-at
  "Piecewise-linear beat for a capture time. Before the first anchor it follows
  the first segment; with a single anchor it follows the transport tempo."
  [anchors transport time-ms]
  (let [n (count anchors)]
    (if (< n 2)
      (let [{anchor-ms :time-ms anchor-beat :beat} (first anchors)]
        (+ anchor-beat (* (beats-per-ms transport) (- time-ms anchor-ms))))
      (let [index (loop [lo 0 hi (- n 2)]
                    ;; Last segment whose start is at or before time-ms.
                    (if (>= lo hi) lo
                        (let [mid (quot (+ lo hi 1) 2)]
                          (if (<= (:time-ms (nth anchors mid)) time-ms) (recur mid hi) (recur lo (dec mid))))))
            a (nth anchors index) b (nth anchors (inc index))
            slope (/ (- (:beat b) (:beat a)) (- (:time-ms b) (:time-ms a)))]
        (+ (:beat a) (* slope (- time-ms (:time-ms a))))))))

(defn- audio-lane
  "Measured capture columns keyed by a stable column number, each
  [start-beat end-beat min max rms low mid high]. A keyed map lets a state
  stream send only the columns that arrived or left."
  [model beat window {:keys [bins beat-markers timing-valid? available?] :as waveform} reference-waveform]
  (let [transport (:transport model)
        base {:available? (boolean (and available? (seq bins)))
              ;; Seam for the matched-recording look-ahead (#514). The track
              ;; profile on this base carries sections and beats but no
              ;; waveform summary, so there is nothing to draw ahead of now.
              :reference (if reference-waveform
                           {:available? false :reason :reference-waveform-not-implemented}
                           {:available? false :reason :no-reference-waveform})}]
    (if-not (and (:available? base) timing-valid? (finite? beat))
      (assoc base :columns {} :timing-valid? (boolean timing-valid?))
      (let [anchors (tempo-anchors beat-markers transport (:time-ms model))
            width (some #(when-not (:partial? %) (- (:end-time-ms %) (:start-time-ms %))) bins)
            width (if (and (finite? width) (pos? width)) width 20.0)
            ;; Audio heard while the transport stood still has no beat of its own.
            placeable? (or (:playing? transport) (<= 2 (count anchors)))
            columns (into {}
                          (keep (fn [{:keys [start-time-ms end-time-ms rms bands] low :min high :max}]
                                  (let [from (beat-at anchors transport start-time-ms)
                                        to (beat-at anchors transport end-time-ms)]
                                    (when (and placeable? (>= to (:from-beat window)) (< from to) (<= from beat))
                                      [(long (Math/round (double (/ start-time-ms width))))
                                       [(round-to from 4) (round-to (min to beat) 4)
                                        (round-to low 3) (round-to high 3) (round-to rms 3)
                                        (round-to (:low bands 0.0) 3) (round-to (:mid bands 0.0) 3)
                                        (round-to (:high bands 0.0) 3)]]))))
                          (when placeable? bins))]
        (assoc base :columns columns :timing-valid? true :column-ms width
               :legend [:start-beat :end-beat :min :max :rms :low :mid :high]
               :source-id (:source-id waveform) :generation (:generation waveform))))))

;; Meters ----------------------------------------------------------------------------

(defn- unit-interval [x] (when (finite? x) (max 0.0 (min 1.0 (double x)))))

(defn- meters
  "Level readings beside the timeline, each normalized to 0..1 where it has a
  natural scale. :available? false means the source is absent or stale; a
  display must not draw a live value for it."
  [model output]
  (let [features (get-in model [:variables :audio :features])
        fresh? (and (finite? (:time-ms features))
                    (<= (- (:time-ms model 0) (:time-ms features)) stale-audio-ms))
        energy (get-in model [:variables :audio/energy])
        relative (:relative-energy features)
        pilot (get-in model [:variables :lighting/autopilot])
        scene (get-in output [:lighting-activity :scene])
        budget (get-in output [:lighting-budget :global])
        axis (fn [id label instant-key]
               (let [statistics (get-in pilot [:statistics id])
                     value (or (:instant statistics) (get scene instant-key))]
                 (cond-> {:id id :label label :available? (finite? value) :value (unit-interval value)}
                   (finite? (:mean statistics)) (assoc :mean (unit-interval (:mean statistics)))
                   (and (:enabled? pilot) (finite? (get-in pilot [:targets id])))
                   (assoc :target (unit-interval (get-in pilot [:targets id]))))))]
    [(cond-> {:id :rms :label "RMS" :available? (boolean (and fresh? (finite? (:rms features))))}
       fresh? (assoc :value (unit-interval (:rms features)) :peak (unit-interval (:peak features))
                     :dbfs (round-to (:energy-dbfs features) 1)))
     ;; Loudness against the analyzer's rolling baseline: 1.0 is typical for the
     ;; last few seconds, so a breakdown or a drop moves it within a beat.
     (cond-> {:id :energy-now :label "Now" :available? (boolean (and fresh? (finite? relative)))}
       (and fresh? (finite? relative))
       (assoc :value (unit-interval (/ relative 2.0)) :relative (round-to relative 3)
              :class (:energy-class features) :breakdown? (boolean (:breakdown? features))))
     (cond-> {:id :audio/energy :label "Energy" :available? (boolean (and fresh? (finite? energy)))}
       (and fresh? (finite? energy)) (assoc :value (unit-interval energy)))
     (cond-> (axis :brightness "Light" :brightness)
       (and (finite? (:used-watts budget)) (finite? (:limit-watts budget)))
       (assoc :used-watts (round-to (:used-watts budget) 1) :limit-watts (round-to (:limit-watts budget) 1)))
     (axis :activity "Activity" :score)]))

;; Track feel ------------------------------------------------------------------------

(def ^:private colour-text
  "CSS colour text a palette string may hold: hex or a colour function of
  numbers. Anything else (url(), declarations) is dropped, because any show
  variable writer can fill the slot."
  #"#[0-9a-fA-F]{3,8}|(?:rgba?|hsla?|hwb|lab|lch|oklab|oklch)\([0-9.,%/\s+-]*(?:deg|turn|rad)?[0-9.,%/\s+-]*\)")

(defn- css-colour
  "A palette colour as CSS text: colour strings pass through, [r g b] in 0..1
  becomes #rrggbb, a gradient or split entry shows its first colour."
  [entry]
  (cond
    (string? entry) (when (re-matches colour-text entry) entry)
    (and (vector? entry) (= 3 (count entry)) (every? number? entry))
    (apply format "#%02x%02x%02x" (map #(-> (double %) (max 0.0) (min 1.0) (* 255.0) Math/round int) entry))
    (seq (:gradient entry)) (css-colour (:color (first (:gradient entry))))
    (seq (:split entry)) (css-colour (first (:split entry)))
    :else nil))

(defn- feel-view
  "The active track-feel personality as the Studio slot draws it. Accepts the
  personality projection's active entry (#647: :id :name :icon SVG text,
  palette of colours, gradients or splits) or a hand-made map with :label,
  :icon-svg or :icon-url and CSS colour strings. An image URL must be a data
  URI, so the slot never makes Studio fetch anything. Other keys are dropped;
  nil when nothing drawable is left."
  [feel]
  (when (map? feel)
    (let [{:keys [id label name icon icon-svg icon-url palette]} feel
          data-uri (fn [x] (when (and (string? x) (re-find #"^data:image/" x)) x))
          svg (or icon-svg (when (and (string? icon) (re-find #"^\s*<svg" icon)) icon))
          url (or (data-uri icon-url) (data-uri icon))
          colours (into [] (comp (map css-colour) (remove nil?) (take 8)) palette)]
      (not-empty
       (cond-> {}
         (some? id) (assoc :id id)
         (string? label) (assoc :label label)
         (string? name) (assoc :name name)
         (string? svg) (assoc :icon-svg svg)
         (string? url) (assoc :icon-url url)
         (seq colours) (assoc :palette colours))))))

;; Projection ------------------------------------------------------------------------

(defn view
  "Project the timeline for {:model :output} of one committed frame. Optional
  inputs: :waveform (the capture manager's timeline history), :reference
  {:profile :lock} from the section reference, :reference-waveform (unused
  seam) and :feel (the active track-feel personality, shown as given).
  Options: {:bars n} either side of now."
  ([inputs] (view inputs {}))
  ([{:keys [model output waveform reference reference-waveform feel]} request]
   (let [{:keys [bars]} (options request)
         transport (:transport model)
         meter (time/normalize-meter (:meter transport))
         beat (double (or (:beat transport) 0.0))
         window (window beat meter bars)
         {:keys [events ramps]} (events-and-ramps model beat window)]
     {:version version
      :beat beat :bpm (:bpm transport) :playing? (boolean (:playing? transport))
      :position (position meter beat)
      :time-ms (:time-ms model) :epoch (:epoch model)
      :meter meter :window window
      :grid (grid meter window)
      :events (mapv #(with-position meter :beat :position %) events)
      :ramps (mapv #(->> % (with-position meter :start-beat :start-position)
                         (with-position meter :end-beat :end-position))
                   ramps)
      :sections {:items (mapv #(with-position meter :start-beat :start-position %)
                              (concat (causal-sections model beat window)
                                      (reference-sections model beat window reference)))
                 :match (match-status reference)}
      :audio (audio-lane model beat window waveform reference-waveform)
      :meters (meters model output)
      :feel (feel-view feel)})))
