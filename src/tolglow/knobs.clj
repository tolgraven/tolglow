(ns tolglow.knobs
  "Linked knobs (#666): one normalized operator knob mapped to several typed
  destinations. A knob stores links only; every destination keeps its own
  identity, value and editing. Named knobs, not macros, so they stay apart
  from saved composition macros (#216/#546) and the :macro/run action.

  The model holds versioned portable data under :knobs. Definition edits are
  pure domain operations admitted through afterglow.control.model receipts.
  Setting a knob value commits the value and edits linked waveform fields
  atomically, then the show owner sends each parameter and show-variable write
  through its shared command reducer, so ramps, manual holds and receipts
  behave exactly as for any other committed write. See docs/linked-knobs.md."
  (:require [afterglow.control.params :as params]
            [afterglow.render.easing :as easing]
            [afterglow.time :as time]
            [clojure.string :as str]
            [tolglow.authoring.cue-variations :as ratios]
            [tolglow.composition.step-schedule :as step-schedule]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.registry :as registry]))

(def schema-version
  "2 (#667): links may carry an :envelope and {:step size} grids, and a knob
  keeps its in-flight envelope moves (:motion) and who created it
  (:created-by). A build that knows only 1 refuses version 2 state with a
  version mismatch; this build restores version 1 state unchanged."
  2)

(def ^:private restorable-versions #{1 2})

(def operations
  "Owner command ops for knob definitions and values."
  #{:knob/create :knob/rename :knob/remove :knob/link :knob/unlink :knob/set :knob/nudge})

(def value-operations
  "Ops whose committed value fans out to destination writes."
  #{:knob/set :knob/nudge})

(def lfo-fields
  "Waveform binding fields a link can drive besides registered waveform inputs."
  #{:from :to :period})

(def period-schema
  "Typed destination for a waveform period: exact beats, quantized to musical ratios."
  {:type :number :bounds [1/64 64] :default 1 :unit :beats :semantic/type :beat-ratio
   :name "Waveform period"})

(def ^:private boundary-tolerance
  "Knob positions within this distance below a step boundary select the upper step,
  so a boundary typed as a decimal behaves like the exact fraction."
  1e-9)

(def ^:private free-denominators (range 1 17))

(def stepped-envelope
  "Default envelope for a stepped destination (choice, switch, integer, beat
  ratio, grid): the smoothed knob takes one beat to cross the whole range
  either way, so a spike lasting a fraction f of a beat moves it at most f."
  {:attack {:beats 1} :release {:beats 1}})

(def ^:private envelope-units #{:beats :bars :phrases})

(def max-grid-steps
  "Most values a {:step size} link may step through; a finer grid is continuous in practice."
  1024)

(defn new-state [] {:schema-version schema-version :knobs {} :next-order 0})

(defn- fail!
  ([message] (fail! message {}))
  ([message details]
   (throw (ex-info message {:control/error :invalid-knob :details details}))))

(defn- check [condition message & [details]] (when-not condition (fail! message (or details {}))))

(defn- id? [x] (or (keyword? x) (and (string? x) (<= 1 (count x) 128))))

(defn- unit-fraction? [x] (and (time/finite-number? x) (<= 0 x 1)))

(defn- state [model] (or (:knobs model) (new-state)))

;; ---------------------------------------------------------------------------
;; Link structure (independent of the current show)

(def ^:private link-keys #{:id :destination :range :direction :curve :input :quantize :envelope :unit :dangling})

(def ^:private destination-keys
  {:param #{:kind :cue-id :instance-id :param}
   :lfo #{:kind :cue-id :instance-id :param :field :input}
   :variable #{:kind :variable}})

(defn- target-selector? [{:keys [cue-id instance-id] :as destination}]
  (and (= 1 (count (filter #(contains? destination %) [:cue-id :instance-id])))
       (or (nil? cue-id) (id? cue-id))
       (or (nil? instance-id) (and (string? instance-id) (seq instance-id)))))

(defn destination-errors
  "Structural errors for a destination map, without consulting the show."
  [{:keys [kind param field input variable] :as destination}]
  (cond
    (not (map? destination)) ["A destination must be a map"]
    (not (destination-keys kind)) ["Destination kind must be :param, :lfo or :variable"]
    (not (every? (destination-keys kind) (keys destination))) ["Unknown destination field"]
    (= :variable kind) (when-not (and (keyword? variable) (not (subscribers/reserved-variable? variable)))
                         ["Choose your own show variable; runtime variables are reserved"])
    (not (target-selector? destination)) ["Name exactly one :cue-id or :instance-id"]
    (not (keyword? param)) ["Name the destination parameter"]
    (and (= :lfo kind) (not= 1 (count (filter #(contains? destination %) [:field :input]))))
    ["A waveform destination names one :field or one waveform :input"]
    (and (= :lfo kind) (contains? destination :field) (not (lfo-fields field)))
    ["A waveform :field is :from, :to or :period"]
    (and (= :lfo kind) (contains? destination :input) (not (keyword? input)))
    ["A waveform :input is a registered waveform parameter keyword"]
    :else nil))

(defn- curve-error [curve]
  (try (easing/sample curve 0.5) nil
       (catch Exception _ "Invalid curve; use an afterglow.render.easing curve such as :linear or :ease-in")))

(defn- quantize-error [quantize]
  (when-not (or (nil? quantize) (= :integer quantize)
                (and (map? quantize) (= [:ratios] (keys quantize)) (#{:preferred :free} (:ratios quantize)))
                (and (map? quantize) (= [:step] (keys quantize)) (time/finite-number? (:step quantize))
                     (pos? (:step quantize))))
    "Quantize is :integer, {:ratios :preferred}, {:ratios :free} or {:step size}"))

(defn- envelope-error
  "Why an envelope is malformed, or nil. An envelope is {:attack d :release d},
  either optional, each one musical duration ({:beats 1}, {:bars 1/2}): the
  time the smoothed knob takes to cross the whole 0–1 range rising (attack)
  or falling (release). Zero or absent moves at once."
  [envelope]
  (when (some? envelope)
    (when-not (and (map? envelope) (seq envelope) (every? #{:attack :release} (keys envelope))
                   (every? (fn [d] (and (map? d) (= 1 (count d)) (envelope-units (key (first d)))
                                        (time/finite-number? (val (first d))) (not (neg? (val (first d))))
                                        (<= (val (first d)) 64)))
                           (vals envelope)))
      "An envelope is {:attack {:beats n} :release {:beats n}}: musical durations from 0 to 64 (beats, bars or phrases)")))

(defn link-errors
  "Structural errors for one link, without consulting the show."
  [{:keys [id destination range direction curve input envelope unit dangling] :as link}]
  (vec (concat
        (when-not (map? link) ["A link must be a map"])
        (when (map? link)
          (concat
           (when-not (every? link-keys (keys link)) ["Unknown link field"])
           (when-not (id? id) ["A link needs a stable ID"])
           (destination-errors destination)
           (when-not (and (vector? range) (= 2 (count range)) (every? registry/data? range))
             ["A link range is [from to] in destination units"])
           (when-not (#{:up :down} (or direction :up)) ["Direction is :up or :down"])
           (some-> (curve-error (or curve :linear)) vector)
           (when-not (or (nil? input)
                         (and (vector? input) (= 2 (count input)) (every? unit-fraction? input)
                              (< (first input) (second input))))
             ["A knob input window is [low high] within 0–1 with low below high"])
           (some-> (quantize-error (:quantize link)) vector)
           (some-> (envelope-error envelope) vector)
           (when-not (or (nil? unit) (keyword? unit)) ["A unit is a keyword"])
           (when-not (or (nil? dangling) (and (map? dangling) (keyword? (:reason dangling))))
             ["A dangling marker needs a keyword :reason"]))))))

(defn- motion-entry? [{:keys [from start] :as entry}]
  (and (map? entry) (every? #{:from :start :source :schedule :commits} (keys entry))
       (unit-fraction? from) (time/finite-number? start)))

(defn- knob-errors [knob]
  (if-not (map? knob) ["A knob must be a map"]
          (vec (concat
                (when-not (every? #{:id :name :value :links :order :recipe :motion :created-by} (keys knob))
                  ["Unknown knob field"])
                ;; In-flight envelope moves by link ID (see `advance-envelopes`).
                (when-not (or (nil? (:motion knob)) (and (map? (:motion knob)) (registry/data? (:motion knob))
                                                         (every? motion-entry? (vals (:motion knob)))))
                  ["Envelope moves are {:from 0–1 :start beat} by link ID"])
                (when-not (or (nil? (:created-by knob)) (and (map? (:created-by knob)) (registry/data? (:created-by knob))))
                  ["A knob's creator is a source map"])
                ;; A generated knob (#667) keeps the recipe it was made from.
                (when-not (or (nil? (:recipe knob)) (and (map? (:recipe knob)) (registry/data? (:recipe knob))))
                  ["A knob recipe is a data map"])
                (when-not (id? (:id knob)) ["A knob needs a stable ID"])
                (when-not (and (string? (:name knob)) (<= 1 (count (:name knob)) 128)) ["A knob needs a name"])
                (when-not (unit-fraction? (:value knob)) ["A knob value is between 0 and 1"])
                (when-not (and (integer? (:order knob)) (not (neg? (:order knob))))
                  ["A knob needs a creation order"])
                (when-not (vector? (:links knob)) ["Knob links are a vector"])
                (when (vector? (:links knob))
                  (concat (mapcat link-errors (:links knob))
                          (when-not (apply distinct? nil (map :id (:links knob)))
                            ["Link IDs must be distinct within a knob"])))))))

(defn validate-state
  "Validate saved knob data without consulting the show. Missing or
  incompatible destinations are reported by the projection, not here."
  [knobs]
  (when-not (and (map? knobs) (restorable-versions (:schema-version knobs)) (map? (:knobs knobs))
                 (integer? (:next-order knobs)) (not (neg? (:next-order knobs)))
                 (registry/data? knobs))
    (throw (ex-info "Invalid saved knob state" {:path [:knobs]})))
  (doseq [[id knob] (:knobs knobs)]
    (let [errors (cond-> (knob-errors knob) (not= id (:id knob)) (conj "Knob key differs from its ID"))]
      (when (seq errors)
        (throw (ex-info (str "Invalid saved knob: " (first errors)) {:path [:knobs :knobs id] :errors errors})))))
  knobs)

;; ---------------------------------------------------------------------------
;; Destination resolution against the current show

(defn- running-targets
  "Instances a destination addresses. A cue ID addresses every running launch
  of that cue in launch order, as scheduler and agent actions do."
  [model {:keys [cue-id instance-id]}]
  (if instance-id
    (when-let [instance (get-in model [:instances instance-id])] [instance])
    (->> (vals (:instances model))
         (filter #(and (= cue-id (:cue-id %)) (= :running (:phase %))))
         (sort-by :order) vec)))

(defn- waveform-schema [model binding input]
  (some #(when (= (:waveform binding) (:id %)) (get-in % [:params input]))
        (parameter-sources/waveform-options model)))

(defn- target-schema
  "Typed schema for one destination on one running instance, or a reason."
  [model {:keys [kind param field input]} instance]
  (let [definition (:definition instance)]
    (case kind
      :param (or (get-in definition [:params param])
                 {:reason :param-missing :message (str "The effect has no parameter " param)})
      :lfo (if-let [entry (get-in instance [:parameter-bindings param])]
             (case field
               (:from :to) (:schema entry)
               :period period-schema
               nil (or (waveform-schema model (:binding entry) input)
                       {:reason :input-missing :message (str "The bound waveform has no input " input)}))
             {:reason :lfo-missing :message (str "No waveform is bound to " param)}))))

(defn- cue-schema
  "Schema for a cue destination when no launch is running: the cue's effect."
  [model {:keys [kind cue-id param]}]
  (let [cue (get-in model [:registry :cues cue-id])
        effect (when cue (registry/cue-effect (:registry model) cue))]
    (cond
      (nil? cue) {:reason :cue-missing :message (str "The cue " cue-id " no longer exists")}
      (nil? effect) {:reason :cue-missing :message "The cue's effect is unavailable"}
      (not (contains? (:params effect) param)) {:reason :param-missing :message (str "The effect has no parameter " param)}
      (= :param kind) (get-in effect [:params param])
      :else nil)))

(defn- ratio-schema? [schema]
  (and (= :number (:type schema))
       (or (= :beat-ratio (:semantic/type schema)) (#{:beats :bars :phrases} (:unit schema)))))

(defn- exact [x] (if (decimal? x) (rationalize x) x))

(defn- compute-ratio-steps
  "Ascending exact beat ratios between the link endpoints and schema bounds."
  [mode schema [a b]]
  (let [low (min (exact a) (exact b)) high (max (exact a) (exact b))
        [lo hi] (or (:bounds schema) [low high])
        within? #(and (<= low % high) (<= (exact lo) % (exact hi)) (pos? %))
        candidates (case mode
                     :preferred (for [n ratios/preferred-components d ratios/preferred-components] (/ n d))
                     :free (for [d free-denominators
                                 n (range (max 1 (long (Math/floor (* low d)))) (inc (long (Math/ceil (* high d)))))]
                             (/ n d)))]
    (vec (sort (distinct (filter within? candidates))))))

(def ^:private ratio-step-cache
  "Pure memo of ratio step lists. A free-ratio link over a wide range has
  thousands of exact fractions, and one knob turn or projection asks for the
  same list several times per link; recomputing it each time costs
  milliseconds on the show owner."
  (atom {}))

(defn- ratio-steps [mode schema [a b]]
  (let [key [mode (:bounds schema) (exact a) (exact b)]]
    (or (get @ratio-step-cache key)
        (let [steps (compute-ratio-steps mode schema [a b])]
          (swap! ratio-step-cache #(assoc (if (>= (count %) 256) {} %) key steps))
          steps))))

(defn- grid-value
  "The k-th value of a step grid from anchor. Exact numbers stay exact; a
  floating grid is rounded to nine decimals so 0.1 steps read 0.3, not
  0.30000000000000004."
  [anchor step k]
  (let [v (+ anchor (* k step))]
    (if (and (rational? anchor) (rational? step)) v (/ (Math/round (* (double v) 1e9)) 1e9))))

(defn- compute-grid-steps
  "Ascending values of a {:step size} grid anchored at the schema's lower bound
  (or zero) between the link endpoints and inside the bounds, or nil when there
  are more than `max-grid-steps`. An integer destination takes whole steps only."
  [step schema [a b]]
  (let [[lo hi] (:bounds schema)
        anchor (or lo 0)
        low (max (min a b) (or lo (min a b))) high (min (max a b) (or hi (max a b)))
        first-k (Math/ceil (- (/ (- (double low) anchor) (double step)) 1e-9))
        last-k (Math/floor (+ (/ (- (double high) anchor) (double step)) 1e-9))
        n (inc (- last-k first-k))]
    (when (and (Double/isFinite n) (<= 1 n max-grid-steps))
      (let [first-k (long first-k) last-k (long last-k)
            values (mapv #(grid-value anchor step %) (range first-k (inc last-k)))]
        (if (= :integer (:type schema))
          (when (and (integer? anchor) (integer? step)) (mapv long values))
          values)))))

(def ^:private grid-step-cache
  "Pure memo of grid step lists, like `ratio-step-cache`."
  (atom {}))

(defn- grid-steps [step schema [a b]]
  (let [key [step (:type schema) (:bounds schema) a b]]
    (if (contains? @grid-step-cache key)
      (get @grid-step-cache key)
      (let [steps (compute-grid-steps step schema [a b])]
        (swap! grid-step-cache #(assoc (if (>= (count %) 256) {} %) key steps))
        steps))))

(defn- discrete-steps
  "Ordered discrete values a link steps through, from its first endpoint to its
  second, or nil for a continuous link."
  [{:keys [range quantize]} schema]
  (let [[a b] range]
    (cond
      (:ratios quantize) (let [steps (ratio-steps (:ratios quantize) schema range)]
                           (if (> (exact a) (exact b)) (vec (rseq steps)) steps))
      (and (:step quantize) (#{:number :integer} (:type schema)) (every? registry/finite-number? range))
      (when-let [steps (grid-steps (:step quantize) schema range)]
        (if (> a b) (vec (rseq steps)) steps))
      (= :enum (:type schema)) (let [values (vec (or (:values schema) (:options schema)))
                                     i (.indexOf ^java.util.List values a) j (.indexOf ^java.util.List values b)]
                                 (when (and (<= 0 i) (<= 0 j))
                                   (if (<= i j) (subvec values i (inc j)) (vec (rseq (subvec values j (inc i)))))))
      (= :boolean (:type schema)) (vec (distinct [a b]))
      :else nil)))

(defn grid-counts
  "Grid values each component of a {:step size} vector link passes through
  between its endpoints, counted in doubles so a tiny step cannot overflow."
  [step schema [a b]]
  (let [anchor (double (or (first (:bounds schema)) 0))]
    (mapv (fn [x y]
            (let [lo (min (double x) (double y)) hi (max (double x) (double y))]
              (inc (- (Math/floor (+ (/ (- hi anchor) step) 1e-9)) (Math/ceil (- (/ (- lo anchor) step) 1e-9))))))
          a b)))

(defn stepped?
  "Whether a link moves its destination in discrete steps, so it gets the
  stepped default envelope and its crossings are scheduled: enum, boolean,
  beat-ratio and grid links (a vector's grid per component), and integer
  destinations."
  [{:keys [quantize] :as link} schema]
  (boolean (or (= :integer (:type schema))
               (and (= :integer quantize) (= :number (:type schema)))
               (and (:step quantize) (= :vector (:type schema)))
               (and (not= :vector (:type schema)) (discrete-steps link schema)))))

(defn compatibility-error
  "Why a link cannot drive a destination schema, or nil when it can."
  [{:keys [range quantize] :as link} schema]
  (let [[a b] range type (:type schema)]
    (cond
      (and (:ratios quantize) (not (ratio-schema? schema)))
      "Musical ratio steps need a beat-ratio destination"
      (and (:ratios quantize) (not (and (every? registry/finite-number? range) (pos? a) (pos? b))))
      "A ratio range needs positive beat values"
      (and (:ratios quantize) (empty? (discrete-steps link schema)))
      "No musical ratio lies within that range and the destination bounds"
      (and (= :integer quantize) (not (#{:number :integer} type)))
      "Integer steps need a numeric destination"
      (and (:step quantize) (not (#{:number :integer :vector} type)))
      "Grid steps need a number, integer or vector destination"
      (and (:step quantize) (= :integer type) (not (integer? (:step quantize))))
      "An integer destination steps by whole numbers"
      (and (:step quantize) (#{:number :integer} type) (every? registry/finite-number? range)
           (empty? (discrete-steps link schema)))
      (str "A grid step of " (:step quantize) " puts no value, or more than " max-grid-steps
           ", between the endpoints; choose a step that gives 2 to " max-grid-steps " values")
      (and (:step quantize) (#{:number :integer} type) (every? registry/finite-number? range)
           (< (count (discrete-steps link schema)) 2))
      (str "A grid step of " (:step quantize) " is wider than the range, so the link could not move")
      (and (:step quantize) (= :vector type) (every? vector? range) (= (count (first range)) (count (second range)))
           (every? registry/finite-number? (concat (first range) (second range))))
      (let [counts (grid-counts (:step quantize) schema range)]
        (cond (some #(or (not (Double/isFinite %)) (> % max-grid-steps)) counts)
              (str "A grid step of " (:step quantize) " gives a component more than " max-grid-steps
                   " values; choose a coarser step")
              (not-any? #(>= % 2) counts)
              (str "A grid step of " (:step quantize) " is wider than every component's range, so the link could not move")))
      (#{:gradient :range} type) (str "A knob cannot drive a " (name type) " destination yet")
      (not (#{:number :integer :color :vector :enum :boolean} type)) "Unsupported destination type"
      (not (and (params/valid-value? schema a) (params/valid-value? schema b)))
      "Both range endpoints must satisfy the destination schema"
      (and (= :enum type) (nil? (discrete-steps link schema))) "Enum endpoints must be declared values"
      :else nil)))

(def ^:private variable-schema {:type :number})

(defn destination-view
  "Resolve a link's destination against the show: {:status :ok :schema s
  :targets [...]} for an addressable destination, {:status :inactive} for a cue
  without a running launch, or {:status :dangling :reason r :message m} when the
  target is gone or no longer compatible with the link."
  [model {:keys [destination dangling] :as link}]
  (let [{:keys [kind cue-id]} destination
        dangle (fn [reason message] {:status :dangling :reason reason :message message})]
    (cond
      dangling (dangle (:reason dangling) (or (:message dangling) "The destination did not survive the restore"))
      (= :variable kind) (if-let [error (compatibility-error link variable-schema)]
                           (dangle :incompatible error)
                           {:status :ok :schema variable-schema :targets []})
      :else
      (let [targets (running-targets model destination)
            cue-level (when cue-id (cue-schema model destination))]
        (cond
          (= :cue-missing (:reason cue-level)) (dangle :cue-missing (:message cue-level))
          (and (:instance-id destination) (empty? targets))
          (dangle :instance-missing "That launch is no longer running")
          ;; Without a running launch, check the effect version the cue would launch.
          (empty? targets)
          (cond
            (:reason cue-level) (dangle (:reason cue-level) (:message cue-level))
            (and (map? cue-level) (compatibility-error link cue-level))
            (dangle :incompatible (compatibility-error link cue-level))
            :else (cond-> {:status :inactive :reason :not-running :message "No launch of that cue is running"}
                    (map? cue-level) (assoc :schema cue-level)))
          :else
          (let [schemas (map #(target-schema model destination %) targets)
                problem (some #(when (:reason %) %) schemas)
                schema (first schemas)
                error (when-not problem (some #(compatibility-error link %) schemas))]
            (cond
              problem (dangle (:reason problem) (:message problem))
              error (dangle :incompatible error)
              :else {:status :ok :schema schema :targets (mapv :id targets)})))))))

;; ---------------------------------------------------------------------------
;; Resolution: knob value -> destination value

(defn- curved
  "Knob position through the link's input window and curve, in 0–1, before direction."
  [{:keys [input curve]} value]
  (let [[low high] (or input [0 1])
        x (cond (<= value low) 0.0 (>= value high) 1.0 :else (/ (- value low) (- high low)))]
    (min 1.0 (max 0.0 (double (easing/sample (or curve :linear) x))))))

(defn shaped
  "Knob position through the link's input window, curve and direction, in 0–1."
  [{:keys [direction] :as link} value]
  (let [y (curved link value)]
    (if (= :down direction) (- 1.0 y) y)))

(defn step-index
  "Equal-width steps without hysteresis: position u selects step floor(u n),
  so a position exactly on a boundary k/n selects step k in either sweep
  direction. The same knob value always resolves to the same step."
  [u n]
  (max 0 (min (dec n) (long (Math/floor (+ (* (double u) n) boundary-tolerance))))))

(defn link-step
  "Step of n a knob value selects on a stepped link. Boundaries are placed in
  knob space before direction: a knob position on a boundary selects the step
  a slightly higher position selects, so :direction :down over [a b] steps
  exactly like :up over [b a], boundaries included."
  [{:keys [direction] :as link} value n]
  (let [k (step-index (curved link value) n)]
    (if (= :down direction) (- (dec n) k) k)))

(defn- constrain [schema value]
  (let [[low high] (or (:bounds schema) (when (= :color (:type schema)) [0 1]))
        clamp #(if low (max low (min high %)) %)]
    (case (:type schema)
      :integer (long (clamp (Math/round (double value))))
      (:color :vector) (mapv clamp value)
      (clamp value))))

(defn- snap-component
  "A vector component on its {:step size} grid, anchored like
  `compute-grid-steps`: the nearest grid value inside the bounds, so clamping
  can never move it off the grid. Grid counts are bounded by the link check."
  [schema step x]
  (let [anchor (or (first (:bounds schema)) 0)
        hi (second (:bounds schema))
        k (Math/round (/ (- (double x) anchor) (double step)))
        k (if (and hi (> (double (grid-value anchor step k)) hi)) (dec k) k)]
    (grid-value anchor step (max 0 k))))

(defn- plain-value [{:keys [range quantize] :as link} schema value]
  (let [u (shaped link value)
        [a b] range]
    (if-let [steps (discrete-steps link schema)]
      (nth steps (link-step link value (count steps)))
      (let [lerp #(+ % (* u (- %2 %)))
            continuous (if (#{:color :vector} (:type schema)) (mapv lerp a b) (lerp a b))
            continuous (cond
                         (and (= :integer quantize) (= :number (:type schema))) (Math/round (double continuous))
                         (and (:step quantize) (= :vector (:type schema)))
                         (mapv #(snap-component schema (:step quantize) %) continuous)
                         :else continuous)]
        (constrain schema continuous)))))

(defn step-count
  "Number of steps a stepped link moves through (for a vector grid, the most
  any component takes), or nil for a continuous link."
  [link schema]
  (cond
    (and (= :vector (:type schema)) (get-in link [:quantize :step]))
    (let [[a b] (map #(plain-value link schema %) [0.0 1.0])
          step (get-in link [:quantize :step])]
      (inc (long (reduce max 0 (map #(Math/round (/ (Math/abs (- (double %1) (double %2))) step)) a b)))))
    (discrete-steps link schema) (count (discrete-steps link schema))
    (stepped? link schema)
    (let [[a b] (map #(plain-value link schema %) [0.0 1.0])]
      (inc (Math/abs (long (- (long a) (long b))))))
    :else nil))

(defn resolve-value
  "Destination value for a knob position on one link and destination schema."
  [link schema value]
  (plain-value link schema value))

;; ---------------------------------------------------------------------------
;; Step boundaries along a move (#667)

(defn- invert-curve
  "x in 0–1 with (easing/sample curve x) = y, by bisection. Easing curves are
  monotone, so this finds the knob position of a curved boundary."
  [curve y]
  (loop [lo 0.0 hi 1.0 n 0]
    (if (>= n 60) (/ (+ lo hi) 2.0)
        (let [mid (/ (+ lo hi) 2.0)]
          (if (< (double (easing/sample (or curve :linear) mid)) y) (recur mid hi (inc n)) (recur lo mid (inc n)))))))

(defn step-boundaries
  "Knob positions (0–1, before the window, curve and direction) where a
  stepped link's destination can change, found from the link's shape without
  resolving a value: equal steps at k/n, whole-number rounding points of an
  integer, and each vector component's grid midpoints. A boundary may change
  nothing (an endpoint clamped by bounds); callers only commit changes."
  [{:keys [input curve direction quantize] [a0 b0 :as ends] :range :as link} schema]
  (let [[lo hi] (or input [0 1])
        knob (fn [y] (when (< 0.0 y 1.0) (+ lo (* (invert-curve curve y) (- hi lo)))))
        ;; A shaped fraction u (after direction) back to the curved fraction y.
        unshaped (fn [u] (if (= :down direction) (- 1.0 u) u))
        crossings (fn [a b marks] (for [m marks :let [u (/ (- (double m) a) (- b a))] :when (< 0.0 u 1.0)] (unshaped u)))
        ys (cond
             (discrete-steps link schema)
             (let [n (count (discrete-steps link schema))] (map #(/ (double %) n) (range 1 n)))
             (or (= :integer (:type schema)) (and (= :integer quantize) (= :number (:type schema))))
             (let [[a b] (map double ends)]
               (when (not= a b)
                 (crossings a b (map #(+ % 0.5) (range (long (Math/floor (min a b))) (long (Math/ceil (max a b))))))))
             (and (:step quantize) (= :vector (:type schema)))
             (let [step (double (:step quantize)) anchor (double (or (first (:bounds schema)) 0))]
               (mapcat (fn [a b]
                         (let [a (double a) b (double b)]
                           (when (not= a b)
                             (crossings a b (for [k (range (long (Math/floor (/ (- (min a b) anchor) step)))
                                                           (inc (long (Math/ceil (/ (- (max a b) anchor) step)))))]
                                              (+ anchor (* (+ k 0.5) step)))))))
                       a0 b0))
             :else nil)]
    (vec (sort (distinct (keep knob ys))))))

;; ---------------------------------------------------------------------------
;; Envelopes (#667): smoothed knob moves in musical time

(defn envelope-beats
  "{:attack beats :release beats} for a link's envelope in the given meter."
  [{:keys [envelope]} meter]
  (into {:attack 0 :release 0}
        (for [[stage duration] envelope] [stage (time/duration->beats duration meter)])))

(defn envelope-level
  "The smoothed knob position of a move at beat: from its :from level towards
  target at the link's rate (the whole range per attack when rising, per
  release when falling), reaching target exactly. Beats are the transport's,
  so a stopped transport holds the level and a resumed one carries on without
  catching up, and the move stretches with the tempo."
  [link meter {:keys [from start]} target beat]
  (let [d (- (double target) (double from))
        beats (get (envelope-beats link meter) (if (pos? d) :attack :release))
        elapsed (max 0.0 (- (double beat) (double start)))]
    (cond
      (zero? d) (double target)
      (or (zero? beats) (>= elapsed (* (Math/abs d) (double beats)))) (double target)
      :else (+ (double from) (* (Math/signum d) (/ elapsed (double beats)))))))

(defn display
  "Shared value display for Studio, Push, REPL and agents."
  [value schema unit]
  (let [unit (or unit (:unit schema))
        ratio? (and (ratio-schema? schema) (rational? value))
        text (cond
               ratio? (str (if (integer? value) value (str (numerator value) "/" (denominator value))))
               (float? value) (format "%.3f" (double value))
               (vector? value) (str "[" (str/join " " (map #(format "%.3f" (double %)) value)) "]")
               (keyword? value) (name value)
               (boolean? value) (if value "on" "off")
               :else (str value))]
    (cond-> {:value value :text (if unit (str text " " (name unit)) text)}
      unit (assoc :unit unit)
      ratio? (assoc :ratio (if (integer? value) [value 1] [(numerator value) (denominator value)])))))

;; ---------------------------------------------------------------------------
;; Reading and writing destinations

(defn- current-value [model {:keys [kind param field input variable]} instance-id]
  (case kind
    :variable (get-in model [:variables variable])
    :param (get-in model [:instances instance-id :params param])
    :lfo (let [binding (get-in model [:instances instance-id :parameter-bindings param :binding])]
           (case field
             (:from :to) (get binding field)
             :period (let [period (:period binding {:beats 1})]
                       (if (contains? period :beats) (:beats period)
                           (time/duration->beats period (get-in model [:instances instance-id :timing-meter]))))
             nil (get-in binding [:inputs input]
                         (:default (waveform-schema model binding input)))))))

(defn- same-value? [a b]
  (cond (and (number? a) (number? b)) (< (Math/abs (- (double a) (double b))) 1e-9)
        (and (vector? a) (vector? b) (= (count a) (count b))) (every? true? (map same-value? a b))
        :else (= a b)))

(defn- write-lfo [model {:keys [param field input]} instance-id value]
  (let [binding (get-in model [:instances instance-id :parameter-bindings param :binding])
        edited (case field
                 (:from :to) (assoc binding field value)
                 :period (assoc binding :period {:beats value})
                 nil (assoc-in binding [:inputs input] value))]
    (if (= edited binding) model
        (parameter-sources/bind model {:instance-id instance-id :param param :binding edited}))))

(defn- open-gesture? [model instance-id param]
  (some? (get-in model [:control :edit-history :open [:instance-param instance-id param]])))

(defn destination-key
  "Comparable identity of a destination, used to find links that share it."
  [{:keys [destination]}]
  (select-keys destination [:kind :cue-id :instance-id :param :field :input :variable]))

;; ---------------------------------------------------------------------------
;; Owner operations

(defn- knob! [model knob-id]
  (or (get-in model [:knobs :knobs knob-id])
      (fail! "Unknown knob" {:knob-id knob-id})))

(defn- link-id
  "Link IDs are stored as text, so a keyword sent by a form surface edits the
  same link as the text ID the REPL used."
  [id]
  (if (keyword? id) (subs (str id) 1) id))

(defn- normalized-link [link]
  (cond-> (merge {:direction :up :curve :linear :input [0 1]} (update (dissoc link :dangling) :id link-id))
    (nil? (:quantize link)) (dissoc :quantize)
    (nil? (:envelope link)) (dissoc :envelope)
    (nil? (:unit link)) (dissoc :unit)))

(defn- declared-schema
  "The schema a parameter or waveform destination declares, before any
  compatibility check, or nil."
  [model {:keys [destination]}]
  (when (#{:param :lfo} (:kind destination))
    (let [schema (if-let [instance (first (running-targets model destination))]
                   (target-schema model destination instance)
                   (when (:cue-id destination) (cue-schema model destination)))]
      (when (and (map? schema) (not (:reason schema))) schema))))

(defn- declared-endpoints
  "A JSON or form surface names enum values as text. Each text endpoint that
  names exactly one declared keyword value becomes that value; anything else
  is left for the compatibility check to refuse."
  [link schema]
  (if-not (and (= :enum (:type schema)) (some string? (:range link)))
    link
    (let [values (or (:values schema) (:options schema))
          declared (fn [x] (if (string? x)
                             (let [named (filter #(and (keyword? %) (= x (subs (str %) 1))) values)]
                               (if (and (not (some #{x} values)) (= 1 (count named))) (first named) x))
                             x))]
      (update link :range #(mapv declared %)))))

(defn- checked-link
  "Validate a link structurally and against the current show. A link may target
  a cue that is not running yet; a missing or incompatible target is refused."
  [model link]
  (let [errors (link-errors link)]
    (check (empty? errors) (first errors) {:link-id (:id link)}))
  (let [link (normalized-link link)
        link (if-let [schema (declared-schema model link)] (declared-endpoints link schema) link)
        view (destination-view model link)]
    (check (not= :dangling (:status view)) (:message view) {:reason (:reason view)})
    (when (= :lfo (get-in link [:destination :kind]))
      (check (= :ok (:status view)) "Bind a waveform to that parameter on a running launch first"))
    (when-let [schema (:schema view)]
      (check (nil? (compatibility-error link schema)) (compatibility-error link schema)))
    link))

(defn- meter [model] (get-in model [:transport :meter]))

(defn- beat [model] (double (get-in model [:transport :beat] 0)))

(defn enveloped?
  "Whether a link smooths the knob with a non-zero attack or release."
  [{:keys [envelope]}]
  (boolean (some (fn [d] (pos? (val (first d)))) (vals envelope))))

(defn output-level
  "The knob position a link outputs now: its in-flight move's level, or the
  knob value when it is settled."
  [model knob link]
  (if-let [entry (get-in knob [:motion (:id link)])]
    (envelope-level link (meter model) entry (:value knob) (beat model))
    (double (:value knob))))

(defn- retarget
  "The knob's moves after its value changes to value. Each enveloped link
  takes over from the level it outputs now and heads for the new value at its
  rate, so a move that is already running carries on from where it is
  (never from its old start). A link already at value settles."
  [model knob value source]
  (reduce (fn [motion {:keys [id] :as link}]
            (let [previous (get motion id)
                  level (output-level model knob link)]
              (if (or (not (enveloped? link)) (== level (double value)))
                (dissoc motion id)
                (assoc motion id (cond-> {:from level :start (beat model) :commits (:commits previous 0)}
                                   (map? source) (assoc :source (select-keys source [:adapter :session-id :device-id
                                                                                     :scheduled-event-id])))))))
          (or (:motion knob) {}) (:links knob)))

(defn- knob-value-writes
  "Apply waveform-field links in place and return [model outcomes writes].
  Parameter and variable writes are returned for the owner's command reducer.
  A link with an envelope in motion writes nothing now: `advance-envelopes`
  commits its steps as the move crosses them."
  [model knob]
  (reduce
   (fn [[model outcomes writes] link]
     (let [view (destination-view model link)
           base {:link-id (:id link)}]
       (cond
         (not= :ok (:status view))
         [model (conj outcomes (assoc base :status :skipped :reason (or (:reason view) (:status view)))) writes]

         (contains? (:motion knob) (:id link))
         [model (conj outcomes (assoc base :status :moving :target (resolve-value link (:schema view) (:value knob))))
          writes]

         :else
         (let [{:keys [kind param variable]} (:destination link)
               value (resolve-value link (:schema view) (:value knob))]
           (case kind
             :variable [model (conj outcomes (assoc base :status :sent :value value))
                        (conj writes {:link-id (:id link) :op :variable/set :variable variable :value value})]
             :param (let [open (filter #(open-gesture? model % param) (:targets view))
                          free (remove (set open) (:targets view))]
                      [model
                       (conj outcomes (cond-> (assoc base :status (if (seq free) :sent :skipped) :value value)
                                        (seq open) (assoc :held-by-gesture (vec open))
                                        (empty? free) (assoc :reason :gesture-open)))
                       (into writes (map (fn [id] {:link-id (:id link) :op :param/set :instance-id id :param param :value value})) free)])
             :lfo (try
                    [(reduce #(write-lfo %1 (:destination link) %2 value) model (:targets view))
                     (conj outcomes (assoc base :status :applied :value value)) writes]
                    (catch clojure.lang.ExceptionInfo error
                      [model (conj outcomes (assoc base :status :rejected :message (.getMessage error))) writes])))))))
   [model [] []] (:links knob)))

(defn- dissoc-motion [model knob-id link-id]
  (let [motion (dissoc (get-in model [:knobs :knobs knob-id :motion]) link-id)]
    (if (seq motion)
      (assoc-in model [:knobs :knobs knob-id :motion] motion)
      (update-in model [:knobs :knobs knob-id] dissoc :motion))))

(defn- forget-motion
  "Drop a link's in-flight move when the link is replaced or removed."
  [model knob-id link-id]
  (if (contains? (get-in model [:knobs :knobs knob-id :motion]) link-id)
    (dissoc-motion model knob-id link-id)
    model))

(defn apply-operation
  "Pure [model command] operation for afterglow.control.model/apply-domain-command."
  [model {:keys [op knob-id] :as command}]
  (let [knobs (state model)]
    (case op
      :knob/create
      (let [{:keys [name value links recipe]} command
            value (or value 0.0)]
        (check (id? knob-id) "A knob needs a stable ID")
        (check (not (contains? (:knobs knobs) knob-id)) "A knob with that ID already exists" {:knob-id knob-id})
        (check (and (string? name) (<= 1 (count name) 128)) "Give the knob a name")
        (check (unit-fraction? value) "A knob value is between 0 and 1")
        (check (or (nil? links) (vector? links)) "Links are a vector")
        (check (or (nil? recipe) (and (map? recipe) (registry/data? recipe))) "A knob recipe is a data map")
        (let [links (mapv #(checked-link model %) (or links []))]
          (check (or (empty? links) (apply distinct? (map :id links))) "Link IDs must be distinct within a knob")
          {:model (assoc model :knobs (-> knobs
                                          (assoc-in [:knobs knob-id] (cond-> {:id knob-id :name name :value value
                                                                                       :links links :order (:next-order knobs)}
                                                                       recipe (assoc :recipe recipe)
                                                                       ;; Who made it, so an agent removes only its own knobs.
                                                                       (map? (:source command))
                                                                       (assoc :created-by (select-keys (:source command) [:adapter :session-id]))))
                                          (update :next-order inc)))
           :events [{:type :knob/created :knob-id knob-id}]
           :result {:knob-id knob-id}}))

      :knob/rename
      (do (knob! model knob-id)
          (check (and (string? (:name command)) (<= 1 (count (:name command)) 128)) "Give the knob a name")
          {:model (assoc-in model [:knobs :knobs knob-id :name] (:name command))
           :events [{:type :knob/renamed :knob-id knob-id :name (:name command)}]})

      :knob/remove
      ;; Destinations keep their definitions and current values.
      (do (knob! model knob-id)
          {:model (update-in model [:knobs :knobs] dissoc knob-id)
           :events [{:type :knob/removed :knob-id knob-id}]})

      :knob/link
      (let [knob (knob! model knob-id)
            link (checked-link model (:link command))
            links (:links knob)
            index (first (keep-indexed #(when (= (:id link) (:id %2)) %1) links))]
        {:model (-> (assoc-in model [:knobs :knobs knob-id :links]
                              (if index (assoc links index link) (conj links link)))
                    (forget-motion knob-id (:id link)))
         :events [{:type :knob/linked :knob-id knob-id :link-id (:id link)}]
         :result {:link-id (:id link) :replaced? (some? index)}})

      :knob/unlink
      (let [knob (knob! model knob-id)
            link-id (link-id (:link-id command))]
        (check (some #(= link-id (:id %)) (:links knob)) "Unknown link" {:link-id link-id})
        {:model (-> (assoc-in model [:knobs :knobs knob-id :links] (vec (remove #(= link-id (:id %)) (:links knob))))
                    (forget-motion knob-id link-id))
         :events [{:type :knob/unlinked :knob-id knob-id :link-id link-id}]})

      (:knob/set :knob/nudge)
      (let [knob (knob! model knob-id)
            value (if (= :knob/set op)
                    (:value command)
                    (do (check (time/finite-number? (:delta command)) "A nudge needs a finite delta")
                        (min 1.0 (max 0.0 (+ (double (:value knob)) (double (:delta command)))))))
            _ (check (unit-fraction? value) "A knob value is between 0 and 1")
            motion (retarget model knob value (:source command))
            knob (cond-> (assoc knob :value value) (seq motion) (assoc :motion motion) (empty? motion) (dissoc :motion))
            [model outcomes writes] (knob-value-writes (assoc-in model [:knobs :knobs knob-id] knob) knob)]
        {:model model
         :events [{:type :knob/changed :knob-id knob-id :value value}]
         :result {:knob-id knob-id :value value :links outcomes :writes writes}}))))

(defn set-value
  "Owner dispatch for :knob/set and :knob/nudge. command is the show's shared
  reducer [model payload time-ms authority] -> {:model :ack :events}. The knob
  value and waveform-field links commit first; each parameter or variable write
  then goes through the reducer with a derived command ID and the knob
  command's source, in link order. A rejected write leaves the others applied
  and is reported on its link. Replaying the command ID replays the writes."
  [model payload time-ms authority command]
  (let [admitted (command model payload time-ms authority)
        ack (:ack admitted)]
    (if-not (= :accepted (:status ack))
      admitted
      (let [base (:command-id ack)
            source (get-in admitted [:model :control :history base :command :source] (:source payload))
            [model acks events]
            (reduce (fn [[model acks events] [index {:keys [link-id] :as write}]]
                      (let [sub (command model (-> (dissoc write :link-id)
                                                   (assoc :command-id (str base "/" index) :source source))
                                         time-ms authority)]
                        [(:model sub) (conj acks [link-id (:ack sub)]) (into events (:events sub))]))
                    [(:model admitted) [] (vec (:events admitted))]
                    (map-indexed vector (:writes ack)))
            by-link (group-by first acks)
            outcomes (mapv (fn [{:keys [link-id status] :as outcome}]
                             (if-let [results (seq (map second (get by-link link-id)))]
                               (let [rejected (first (remove #(= :accepted (:status %)) results))]
                                 (cond-> (assoc outcome :status (if rejected :rejected :applied))
                                   rejected (assoc :message (:message rejected) :code (:code rejected))))
                               outcome))
                           (:links ack))]
        (cond-> {:model model :events events
                 ;; A replay reports the revision the original fan-out reached.
                 :ack (-> ack (dissoc :writes)
                          (assoc :links outcomes
                                 :revision (if (:replayed? admitted)
                                             (reduce max (:revision ack) (keep (comp :revision second) acks))
                                             (:revision model))))}
          (:replayed? admitted) (assoc :replayed? true))))))

;; ---------------------------------------------------------------------------
;; Envelope moves on the owner's beat clock (#667)

(defn- move-schedule
  "Step crossings of a stepped link's move from :from to target, as positions
  0–1 along the move; segment values fill in as frames reach them."
  [link schema {:keys [from]} target]
  (let [d (- (double target) (double from))
        bounds (when-not (zero? d)
                 (keep #(let [p (/ (- % (double from)) d)] (when (< 0.0 p 1.0) p)) (step-boundaries link schema)))]
    (step-schedule/plan bounds)))

(defn- commit-link
  "Write one link's value now: waveform fields in place, parameters and
  variables through the owner's reducer with a derived command ID and the
  source of the knob turn that started the move. An open gesture holds its
  launch, as for a knob turn. Returns the model."
  [model command time-ms knob link view value id-base source]
  (let [{:keys [kind param variable]} (:destination link)
        submit (fn [model n payload]
                 (:model (command model (assoc payload :command-id (str id-base "/" n)
                                               :source (or source {:adapter :knob :session-id (str (:id knob))}))
                                  time-ms)))]
    (case kind
      :variable (submit model 0 {:op :variable/set :variable variable :value value})
      :param (reduce (fn [model [n id]]
                       (if (open-gesture? model id param) model
                           (submit model n {:op :param/set :instance-id id :param param :value value})))
                     model (map-indexed vector (:targets view)))
      :lfo (try (reduce #(write-lfo %1 (:destination link) %2 value) model (:targets view))
                (catch clojure.lang.ExceptionInfo _ model)))))

(defn- advance-link
  [model command time-ms knob-id {:keys [id] :as link} entry]
  (let [knob (get-in model [:knobs :knobs knob-id])
        view (destination-view model link)
        path [:knobs :knobs knob-id :motion id]]
    (cond
      (not (enveloped? link)) (dissoc-motion model knob-id id)
      ;; A cue that is not running holds nothing; the move goes on in time.
      (not= :ok (:status view)) model
      :else
      (let [schema (:schema view)
            target (double (:value knob))
            from (double (:from entry))
            level (envelope-level link (meter model) entry target (beat model))
            settled? (== level target)
            along #(resolve-value link schema (+ from (* % (- target from))))
            stepped (stepped? link schema)
            schedule (when stepped (or (:schedule entry) (move-schedule link schema entry target)))
            p (if (== from target) 1.0 (/ (- level from) (- target from)))
            [value schedule] (cond settled? [(resolve-value link schema target) schedule]
                                   stepped (step-schedule/lookup schedule along p #(resolve-value link schema level))
                                   :else [(resolve-value link schema level) nil])
            ;; Commit only what differs from the destination now, so a step that
            ;; already happened never fires again, also across a retarget.
            changed? (if (= :variable (get-in link [:destination :kind]))
                       (not (same-value? value (current-value model (:destination link) nil)))
                       (some #(not (same-value? value (current-value model (:destination link) %))) (:targets view)))
            commits (:commits entry 0)
            model (if changed?
                    (commit-link model command time-ms knob link view value
                                 (str "knob-envelope/" (:epoch model) "/" (pr-str knob-id) "/" id "/" (:start entry) "/" commits)
                                 (:source entry))
                    model)]
        (if settled?
          (dissoc-motion model knob-id id)
          (assoc-in model path (cond-> (assoc entry :commits (cond-> commits changed? inc))
                                 schedule (assoc :schedule schedule))))))))

(defn advance-envelopes
  "Owner step for every in-flight envelope move, after the frame's transport
  beat is frozen. A stepped link looks its value up in the move's crossing
  schedule, computed once per move, and commits only when the move crosses a
  step; a continuous link resolves its level and commits when it changes. A
  settled move ends. command is the show's [model payload time-ms] reducer."
  [model time-ms command]
  (reduce (fn [model {knob-id :id links :links}]
            (reduce (fn [model link]
                      (if-let [entry (get-in model [:knobs :knobs knob-id :motion (:id link)])]
                        (advance-link model command time-ms knob-id link entry)
                        model))
                    model links))
          model
          (sort-by :order (filter #(seq (:motion %)) (vals (get-in model [:knobs :knobs]))))))

;; ---------------------------------------------------------------------------
;; Projection shared by every surface

(defn- target-label [model {:keys [kind cue-id instance-id param field input variable]}]
  (let [cue (or cue-id (get-in model [:instances instance-id :cue-id]))
        owner (str (if cue (name cue) instance-id))]
    (case kind
      :variable (str "variable " (name variable))
      :param (str owner " · " (name param))
      :lfo (str owner " · " (name param) " LFO " (name (or field input))))))

(defn link-view
  "Projection of one link at a knob value: resolved value and units, the
  destination's committed value, and status (:ok, :inactive or :dangling).
  A link whose envelope is moving resolves at the level it outputs now and
  carries :moving {:level :target}, the target resolved at the knob value."
  [model knob link shared]
  (let [view (destination-view model link)
        schema (:schema view)
        unit (or (:unit link) (:unit schema))
        entry (get-in knob [:motion (:id link)])
        level (if entry (output-level model knob link) (:value knob))
        resolved (when schema (try (resolve-value link schema level) (catch Exception _ nil)))
        targets (:targets view)
        current (cond
                  (= :variable (get-in link [:destination :kind])) (current-value model (:destination link) nil)
                  (seq targets) (current-value model (:destination link) (first targets)))
        diverged? (boolean (and (= :ok (:status view)) (some? resolved)
                                (if (= :variable (get-in link [:destination :kind]))
                                  (not (same-value? current resolved))
                                  (some #(not (same-value? (current-value model (:destination link) %) resolved)) targets))))
        steps (when schema (discrete-steps link schema))]
    (cond-> (merge (select-keys link [:id :destination :range :direction :curve :input :quantize :envelope])
                   {:label (target-label model (:destination link)) :status (:status view)
                    :shared-with (vec (sort-by str (disj (get shared (destination-key link) #{}) (:id knob))))})
      (:reason view) (assoc :reason (:reason view))
      (:message view) (assoc :message (:message view))
      schema (assoc :schema (select-keys schema [:type :bounds :unit :values :options :size :step :semantic/type :name]))
      (some? resolved) (assoc :resolved (display resolved schema unit))
      (some? current) (assoc :current (display current schema unit))
      (seq targets) (assoc :targets targets)
      diverged? (assoc :diverged? true)
      (and entry schema) (assoc :moving {:level level
                                         :target (display (resolve-value link schema (:value knob)) schema unit)})
      steps (assoc :steps (count steps)
                   :step (let [i (.indexOf ^java.util.List steps resolved)]
                           (if (<= 0 i) i (link-step link level (count steps))))))))

(defn projection
  "Knobs in creation order with every link resolved at the knob's value."
  [model]
  (let [knobs (sort-by :order (vals (:knobs (state model))))
        shared (reduce (fn [index knob]
                         (reduce #(update %1 (destination-key %2) (fnil conj #{}) (:id knob)) index (:links knob)))
                       {} knobs)]
    {:knobs (mapv (fn [knob]
                    (-> (select-keys knob [:id :name :value :recipe])
                        (assoc :links (mapv #(link-view model knob % shared) (:links knob)))))
                  knobs)}))

(defn knob-view
  "Projection of one knob, or nil."
  [model knob-id]
  (some #(when (= knob-id (:id %)) %) (:knobs (projection model))))

;; ---------------------------------------------------------------------------
;; Restore

(defn- restored-motion
  "Moves of a restored knob that still fit it: entries for links that exist
  and have an envelope, without their cached schedule (recomputed against
  the current schema). Anything else is dropped."
  [{:keys [motion links]}]
  (let [enveloped (set (map :id (filter enveloped? links)))]
    (not-empty (into {} (for [[id entry] motion :when (and (enveloped id) (motion-entry? entry))]
                          [id (dissoc entry :schedule)])))))

(defn restore
  "Rebind saved knobs to restored launch IDs. A link to a launch that did not
  survive keeps its old identity and is marked dangling, so it stays visible
  and editable instead of being dropped or resolving to an unrelated launch.
  Version 1 state is restored as version 2. In-flight envelope moves are kept
  for links that still have an envelope and dropped otherwise."
  [knobs instance-ids epoch]
  (validate-state knobs)
  (-> knobs
      (assoc :schema-version schema-version)
      (update :knobs update-vals
              (fn [knob]
                (let [knob (update knob :links
                                   (fn [links]
                                     (mapv (fn [{:keys [destination] :as link}]
                                             (if-let [old (:instance-id destination)]
                                               (if-let [new-id (get instance-ids old)]
                                                 (assoc-in link [:destination :instance-id] new-id)
                                                 (assoc link :dangling {:reason :instance-missing :epoch epoch
                                                                        :message (str "Launch " old " did not survive the restore")}))
                                               link))
                                           links)))
                      motion (restored-motion knob)]
                  (if motion (assoc knob :motion motion) (dissoc knob :motion)))))))

;; ---------------------------------------------------------------------------
;; Encoder surfaces (Push and other relative controllers)

(def encoder-step
  "Knob change per encoder detent, matching the Push master encoder: 1/100 of
  the range, or 1/1000 with Shift."
  {:coarse 1/100 :fine 1/1000})

(defn encoder-command
  "The shared command for a relative encoder turn of delta detents on a knob.
  An adapter supplies its own source; device addresses stay in the adapter."
  [knob-id delta fine?]
  {:op :knob/nudge :knob-id knob-id
   :delta (* delta (if fine? (:fine encoder-step) (:coarse encoder-step)))})

(defn projected-encoder-display
  "encoder-display for one knob already taken from projection, so a surface
  showing several knobs resolves their links once."
  [knob]
  (when-let [{:keys [id name value links]} knob]
    {:knob-id id :name name :value value :text (format "%.3f" (double value))
     :columns (mapv (fn [link]
                      (cond-> {:label (:label link) :status (:status link)}
                        (:resolved link) (assoc :text (get-in link [:resolved :text]))
                        (get-in link [:resolved :ratio]) (assoc :ratio (get-in link [:resolved :ratio]))
                        (:diverged? link) (assoc :diverged? true)))
                    (take 8 links))}))

(defn encoder-display
  "Display columns for an encoder surface from the same projection Studio
  shows: the knob value and up to eight links with their resolved text."
  [model knob-id]
  (projected-encoder-display (knob-view model knob-id)))
