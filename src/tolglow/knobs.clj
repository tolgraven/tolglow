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
  "Ops whose committed value fans out to destination writes. A :knob/link that
  replaces a moving link writes the new link's value."
  #{:knob/set :knob/nudge :knob/link})

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
  or falling (release). Zero or absent moves at once; an empty map asks for
  no smoothing on a stepped destination, which otherwise gets
  `stepped-envelope`."
  [envelope]
  (when (some? envelope)
    (when-not (and (map? envelope) (every? #{:attack :release} (keys envelope))
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

(def ^:private source-keys #{:adapter :session-id :device-id :scheduled-event-id})

(defn- motion-entry?
  "A well-formed in-flight move: a 0–1 :from level, finite :start beat and
  :beats, counters that are whole and non-negative, sorted finite crossing
  beats and a source limited to its identifying keys."
  [{:keys [from start beats turn commits segment crossings next source] :as entry}]
  (and (map? entry)
       (every? #{:from :start :beats :turn :commits :segment :crossings :next :written :source :own :mark} (keys entry))
       (or (nil? (:own entry)) (and (vector? (:own entry)) (<= (count (:own entry)) 256) (every? string? (:own entry))))
       (or (nil? (:mark entry)) (and (integer? (:mark entry)) (not (neg? (:mark entry)))))
       (unit-fraction? from) (time/finite-number? start)
       (time/finite-number? beats) (<= 0 beats) (<= beats 4096)
       (or (nil? turn) (and (string? turn) (<= 1 (count turn) 256)))
       (every? #(or (nil? %) (and (integer? %) (not (neg? %)))) [commits segment])
       (or (nil? next) (time/finite-number? next))
       (or (nil? crossings) (and (vector? crossings) (every? time/finite-number? crossings)
                                 (or (empty? crossings) (apply <= crossings))))
       (or (nil? source) (and (map? source) (every? source-keys (keys source))))))

(defn- knob-errors [knob]
  (if-not (map? knob) ["A knob must be a map"]
          (vec (concat
                (when-not (every? #{:id :name :value :links :order :recipe :motion :created-by} (keys knob))
                  ["Unknown knob field"])
                ;; In-flight envelope moves by link ID (see `advance-envelopes`).
                ;; A malformed move is dropped by `restore`, per link, not refused here.
                (when-not (or (nil? (:motion knob)) (and (map? (:motion knob)) (registry/data? (:motion knob))))
                  ["Envelope moves are a data map by link ID"])
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
  catching up, and the move stretches with the tempo. The move's :beats, fixed
  when it started, wins over the link's envelope in the current meter."
  [link meter {:keys [from start] :as entry} target beat]
  (let [d (- (double target) (double from))
        beats (if (contains? entry :beats)
                (:beats entry)
                (get (envelope-beats link meter) (if (pos? d) :attack :release)))
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

(defn default-envelope
  "The link with the typed default envelope: a stepped destination (choice,
  switch, integer, beat ratio, grid) without an :envelope gets
  `stepped-envelope`; one that names an envelope, empty included (no
  smoothing), keeps it. An empty envelope on a continuous destination is
  dropped, so it does not make a backup version 2. The REPL, Studio, agents
  and the generator share it."
  [link schema]
  (cond
    (nil? schema) link
    (and (not (contains? link :envelope)) (stepped? link schema)) (assoc link :envelope stepped-envelope)
    ;; An empty envelope only means something on a stepped destination.
    (and (= {} (:envelope link)) (not (stepped? link schema))) (dissoc link :envelope)
    :else link))

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
    (default-envelope link (:schema view))))

(defn- meter [model] (get-in model [:transport :meter]))

(defn- beat [model] (double (get-in model [:transport :beat] 0)))

(defn enveloped?
  "Whether a link smooths the knob with a non-zero attack or release."
  [{:keys [envelope]}]
  (boolean (some (fn [d] (pos? (val (first d)))) (vals envelope))))

(defn- move-beats
  "Beats for the whole range in the direction of a move of d, in meter."
  [link meter d]
  (double (get (envelope-beats link meter) (if (pos? d) :attack :release))))

(defn- settle-beat [{:keys [from start beats]} target]
  (+ (double start) (* (Math/abs (- (double target) (double from))) (double beats))))

(defn output-level
  "The knob position a link outputs now: its in-flight move's level, or the
  knob value when it is settled."
  [model knob link]
  (if-let [entry (get-in knob [:motion (:id link)])]
    (envelope-level link (meter model) entry (:value knob) (beat model))
    (double (:value knob))))

(defn- move-source [source]
  (when (map? source) (not-empty (select-keys source [:adapter :session-id :device-id :scheduled-event-id]))))

(defn- start-move
  "A move of an enveloped link from level towards target, starting now. The
  rate is fixed in beats when the move starts, so a later meter change does
  not shift it. The crossings are planned by `plan-move` once the destination
  schema is known."
  [model link level target source previous turn]
  (cond-> {:from (double level) :start (beat model)
           :beats (move-beats link (meter model) (- (double target) (double level)))
           ;; The command that started the move names its commits, so each is unique.
           :turn (str (or turn (str "move/" (:revision model) "/" (beat model))))}
    (move-source source) (assoc :source (move-source source))
    (contains? previous :written) (assoc :written (:written previous))))

(defn- crossing-beats
  "Beats at which a move crosses its link's step boundaries, ascending."
  [link schema {:keys [from start beats]} target]
  (let [from (double from) target (double target)]
    (vec (sort (for [b (step-boundaries link schema)
                     :when (if (< from target) (< from b target) (< target b from))]
                 (+ (double start) (* (Math/abs (- b from)) (double beats))))))))

(defn- segment-value
  "Value of segment k of a move: the plain step at the knob target for the
  last segment, otherwise the step at the segment's middle beat."
  [link schema entry target crossings k meter]
  (if (= k (count crossings))
    (resolve-value link schema target)
    (let [lo (if (zero? k) (double (:start entry)) (nth crossings (dec k)))
          mid (/ (+ lo (double (nth crossings k))) 2.0)]
      (resolve-value link schema (envelope-level link meter entry target mid)))))

(defn- plan-move
  "Complete a move once its destination schema is known: its crossing beats,
  the segment the transport is in now and the next beat it must act at. The
  step the destination shows is :written, from the move it took over, or the
  destination's committed value."
  [model link entry target schema current]
  (let [stepped (stepped? link schema)
        crossings (if stepped (crossing-beats link schema entry target) [])
        now (beat model)
        segment (count (filter #(<= % now) crossings))
        settle (settle-beat entry target)]
    (cond-> (assoc entry :crossings crossings :segment segment
                   :next (if stepped (or (first (drop segment crossings)) settle) now))
      (not (contains? entry :written)) (assoc :written current))))

(defn- writes-destination? [command {:keys [kind param variable]} target]
  (case kind
    :param (and (= :param/set (:op command)) (= target (:instance-id command)) (= param (:param command)))
    :variable (and (= :variable/set (:op command)) (= variable (:variable command)))
    false))

(defn- last-writer
  "The latest committed command that wrote a destination target, from the
  shared receipt history (as the scheduler finds a ramp's overrider):
  {:id :revision}, or nil."
  [model destination target]
  (some (fn [id]
          (let [{:keys [command ack]} (get-in model [:control :history id])]
            (when (and command (writes-destination? command destination target))
              {:id id :revision (:revision ack)})))
        (take 64 (rseq (vec (get-in model [:control :history-order] []))))))

(defn- overridden?
  "Whether another writer committed to the link's destination since the move
  last did: a parameter or variable by the receipt history, so an equal-value
  write counts too; a waveform field, which has no receipts, by its value."
  [model link view {:keys [own mark written]}]
  (let [{:keys [kind] :as destination} (:destination link)]
    (if (and (#{:param :variable} kind) (integer? mark))
      (let [own (set own)]
        (boolean (some (fn [target]
                         (when-let [{:keys [id revision]} (last-writer model destination target)]
                           (and (not (own id)) (integer? revision) (> revision mark))))
                       (if (= :variable kind) [nil] (:targets view)))))
      (boolean (some #(not (same-value? written %))
                     (if (= :variable kind) [(current-value model destination nil)]
                         (mapv #(current-value model destination %) (:targets view))))))))

(defn- destination-values
  "The committed values of a link's destination, one per target."
  [model {:keys [destination]} view]
  (if (= :variable (:kind destination))
    [(current-value model destination nil)]
    (mapv #(current-value model destination %) (:targets view))))

(defn- retarget
  "The knob's moves after its value changes to value. Each enveloped link
  takes over from the level it outputs now and heads for the new value at its
  rate, so a running move carries on from where it is, never from its old
  start; it keeps the step it last committed, so no step fires twice. A link
  already at value settles."
  [model knob value source turn]
  (reduce (fn [motion {:keys [id] :as link}]
            (let [level (output-level model knob link)]
              (if (or (not (enveloped? link)) (== level (double value)))
                (dissoc motion id)
                (assoc motion id (start-move model link level value source (get motion id) turn)))))
          (or (:motion knob) {}) (:links knob)))

(defn- link-writes
  "[model outcome writes] for writing value to one link now."
  [model link view value base]
  (let [{:keys [kind param variable]} (:destination link)]
    (case kind
      :variable [model (assoc base :status :sent :value value)
                 [{:link-id (:id link) :op :variable/set :variable variable :value value}]]
      :param (let [open (filter #(open-gesture? model % param) (:targets view))
                   free (remove (set open) (:targets view))]
               [model
                (cond-> (assoc base :status (if (seq free) :sent :skipped) :value value)
                  (seq open) (assoc :held-by-gesture (vec open))
                  (empty? free) (assoc :reason :gesture-open))
                (mapv (fn [id] {:link-id (:id link) :op :param/set :instance-id id :param param :value value}) free)])
      :lfo (try
             [(reduce #(write-lfo %1 (:destination link) %2 value) model (:targets view))
              (assoc base :status :applied :value value) []]
             (catch clojure.lang.ExceptionInfo error
               [model (assoc base :status :rejected :message (.getMessage error)) []])))))

(defn- pending-steps
  "Values of the crossings an earlier move of the same link reached by now
  and had not committed yet, in order."
  [model entry link schema target]
  (when (and entry link (:crossings entry) (number? target))
    (let [now (beat model) crossings (:crossings entry)
          reached (count (filter #(<= % now) crossings))]
      (for [k (range (inc (:segment entry 0)) (inc reached))]
        (segment-value link schema entry target crossings k (meter model))))))

(defn- knob-value-writes
  "Apply waveform-field links in place and return [model outcomes writes knob].
  Parameter and variable writes are returned for the owner's command reducer.
  A link whose envelope starts or carries on moving writes the step at its
  current level if the destination shows another (the knob moved, so a hand
  edit gives way), and its move is planned; `advance-envelopes` commits the
  later crossings."
  ([model knob] (knob-value-writes model knob nil nil))
  ([model knob previous previous-links]
  (reduce
   (fn [[model outcomes writes knob] link]
     (let [view (destination-view model link)
           base {:link-id (:id link)}
           entry (get-in knob [:motion (:id link)])]
       (cond
         (not= :ok (:status view))
         [model (conj outcomes (assoc base :status :skipped :reason (or (:reason view) (:status view)))) writes knob]

         entry
         (let [schema (:schema view)
               now-value (resolve-value link schema (:from entry))
               current (destination-values model link view)
               ;; Crossings the move it takes over reached since the last frame fire
               ;; first, in order, at their musical time, as if a frame had landed there.
               fired (pending-steps model (get previous (:id link)) (get previous-links (:id link)) schema
                                    (:previous-value knob))
               [model outcome more _]
               (reduce (fn [[model outcome more shown] v]
                         (if (and (every? #(same-value? v %) shown)) [model outcome more shown]
                             (let [[model o w] (link-writes model link view v base)] [model o (into more w) [v]])))
                       [model base [] current] (concat fired [now-value]))
               ;; The owner admits these writes as <turn>/<index> (`set-value`); later
               ;; writers to the destination are recognised by those receipt IDs.
               own (mapv #(str (:turn entry) "/" (+ (count writes) %)) (range (count more)))
               planned (plan-move model link (assoc entry :written now-value :own own :mark (long (:revision model 0)))
                                  (:value knob) schema now-value)]
           [model (conj outcomes (assoc outcome :status :moving :target (resolve-value link schema (:value knob))))
            (into writes more) (assoc-in knob [:motion (:id link)] planned)])

         :else
         (let [[model outcome more] (link-writes model link view (resolve-value link (:schema view) (:value knob)) base)]
           [model (conj outcomes outcome) (into writes more) knob]))))
   [model [] [] knob] (:links knob))))

(defn- dissoc-motion [model knob-id link-id]
  (let [motion (dissoc (get-in model [:knobs :knobs knob-id :motion]) link-id)]
    (if (seq motion)
      (assoc-in model [:knobs :knobs knob-id :motion] motion)
      (update-in model [:knobs :knobs knob-id] dissoc :motion))))

(defn- forget-motion
  "Drop a link's in-flight move when the link is removed."
  [model knob-id link-id]
  (if (contains? (get-in model [:knobs :knobs knob-id :motion]) link-id)
    (dissoc-motion model knob-id link-id)
    model))

(defn- agent-owns!
  "An agent removes a knob, unlinks or replaces one of its links only on a
  knob the same agent conversation created; it may still add links to,
  rename and turn the operator's knobs."
  [knob {:keys [source]} what]
  (when (= :agent (:adapter source))
    (let [creator (:created-by knob)]
      (when-not (and (= :agent (:adapter creator)) (= (:session-id creator) (:session-id source)))
        (throw (ex-info (str "An agent can " what " only on knobs it created; this knob belongs to the operator")
                        {:control/error :not-agent-knob :details {:knob-id (:id knob)}}))))))

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
                                                                       ;; An agent's own knob, which only it may delete.
                                                                       (= :agent (get-in command [:source :adapter]))
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
      (do (agent-owns! (knob! model knob-id) command "remove a knob")
          {:model (update-in model [:knobs :knobs] dissoc knob-id)
           :events [{:type :knob/removed :knob-id knob-id}]})

      :knob/link
      ;; Replacing a moving link carries its move on from the level it outputs now
      ;; (with the new link's envelope), or writes the new link's plain value.
      (let [knob (knob! model knob-id)
            link (checked-link model (:link command))
            links (:links knob)
            index (first (keep-indexed #(when (= (:id link) (:id %2)) %1) links))
            _ (when index (agent-owns! knob command "replace a link"))
            previous (get-in knob [:motion (:id link)])
            level (when previous (output-level model knob (nth links index)))
            knob (cond-> (assoc knob :links (if index (assoc links index link) (conj links link)))
                   previous (update :motion dissoc (:id link))
                   (and previous (enveloped? link) (not (== level (double (:value knob)))))
                   (assoc-in [:motion (:id link)] (start-move model link level (:value knob) (:source command) previous
                                                              (:command-id command))))
            knob (cond-> knob (empty? (:motion knob)) (dissoc :motion))
            [model outcomes writes knob]
            (if previous
              (let [[m o w k] (knob-value-writes model (assoc knob :links [link] :previous-value (:value knob))
                                                 {(:id link) previous} {(:id link) (nth links index)})]
                [m o w (cond-> (-> k (assoc :links (:links knob)) (dissoc :previous-value)) (empty? (:motion k)) (dissoc :motion))])
              [model [] [] knob])]
        {:model (assoc-in model [:knobs :knobs knob-id] knob)
         :events [{:type :knob/linked :knob-id knob-id :link-id (:id link)}]
         :result (cond-> {:link-id (:id link) :replaced? (some? index)}
                   previous (assoc :links outcomes :writes writes))})

      :knob/unlink
      (let [knob (knob! model knob-id)
            link-id (link-id (:link-id command))]
        (check (some #(= link-id (:id %)) (:links knob)) "Unknown link" {:link-id link-id})
        (agent-owns! knob command "unlink a link")
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
            motion (retarget model knob value (:source command) (:command-id command))
            previous (:motion knob)
            links-by-id (into {} (map (juxt :id identity)) (:links knob))
            old-value (:value knob)
            knob (cond-> (assoc knob :value value) (seq motion) (assoc :motion motion) (empty? motion) (dissoc :motion))
            [model outcomes writes knob] (knob-value-writes model (assoc knob :previous-value old-value) previous links-by-id)
            knob (dissoc knob :previous-value)]
        {:model (assoc-in model [:knobs :knobs knob-id] knob)
         :events [{:type :knob/changed :knob-id knob-id :value value}]
         :result {:knob-id knob-id :value value :links outcomes :writes writes}}))))

(defn set-value
  "Owner dispatch for :knob/set, :knob/nudge and :knob/link. command is the show's shared
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

(defn- own-ids
  "Receipt IDs `commit-link` gives a commit with id-base, one per target."
  [link view id-base]
  (case (get-in link [:destination :kind])
    :variable [(str id-base "/0")]
    :param (mapv #(str id-base "/" %) (range (count (:targets view))))
    []))

(defn- end-move
  "End a link's move because another writer changed its destination: the
  destination keeps that write, as a committed write cancels a ramp."
  [model knob-id link-id]
  (dissoc-motion model knob-id link-id))

(defn- advance-link
  [model command time-ms knob-id {:keys [id] :as link} entry]
  (let [now (beat model)]
    (if (and (:crossings entry) (< now (double (:next entry))))
      ;; Scheduled: nothing to do until the transport reaches the next crossing.
      model
      (let [knob (get-in model [:knobs :knobs knob-id])
            view (destination-view model link)]
        (cond
          (not (enveloped? link)) (dissoc-motion model knob-id id)
          ;; A cue that is not running holds nothing; the move goes on in time.
          (not= :ok (:status view)) model
          :else
          (let [schema (:schema view)
                target (double (:value knob))
                current (destination-values model link view)
                entry (if (:crossings entry) entry
                          (plan-move model link entry target schema (first current)))
                written (:written entry)
                id-base #(str "knob-envelope/" (:turn entry) "/" id "/" %)]
            (cond
              ;; Someone else committed to the destination: their write stands.
              (overridden? model link view entry) (end-move model knob-id id)

              (stepped? link schema)
              (let [crossings (:crossings entry)
                    m (meter model)
                    settled? (>= now (settle-beat entry target))
                    reached (if settled? (count crossings) (count (filter #(<= % now) crossings)))
                    ;; Each crossing reached fires once, in order, at its musical time.
                    [model written own] (reduce (fn [[model written own] k]
                                                  (let [v (segment-value link schema entry target crossings k m)]
                                                    (if (same-value? v written) [model written own]
                                                        [(commit-link model command time-ms knob link view v (id-base k) (:source entry))
                                                         v (own-ids link view (id-base k))])))
                                                [model written (:own entry)] (range (inc (:segment entry 0)) (inc reached)))
                    final (when settled? (resolve-value link schema target))
                    model (if (and final (not (same-value? final written)))
                            (commit-link model command time-ms knob link view final (id-base "end") (:source entry))
                            model)]
                (if settled?
                  (dissoc-motion model knob-id id)
                  (assoc-in model [:knobs :knobs knob-id :motion id]
                            (cond-> (assoc entry :segment reached :written written
                                           :next (or (first (drop reached crossings)) (settle-beat entry target)))
                              (not= own (:own entry)) (assoc :own own :mark (long (:revision model 0)))))))

              :else
              (let [level (envelope-level link (meter model) entry target now)
                    settled? (== level target)
                    value (resolve-value link schema level)
                    n (long (:commits entry 0))
                    changed? (not (same-value? value written))
                    model (if changed? (commit-link model command time-ms knob link view value (id-base n) (:source entry)) model)]
                (if settled?
                  (dissoc-motion model knob-id id)
                  (assoc-in model [:knobs :knobs knob-id :motion id]
                            (cond-> (assoc entry :written (if changed? value written))
                              changed? (assoc :commits (inc n) :own (own-ids link view (id-base n))
                                              :mark (long (:revision model 0))))))))))))))

(defn advance-envelopes
  "Owner step for every in-flight envelope move, after the frame's transport
  beat is frozen. A stepped link is skipped until the transport reaches its
  next crossing beat; then every crossing reached commits its step once, in
  order, so the committed sequence does not depend on the frame rate. A
  continuous link resolves its level each frame and commits when it changes.
  A move ends when it settles on the plain step at the knob value, or when
  another writer changes its destination. A move that cannot be advanced is
  dropped rather than failing every frame. command is the show's
  [model payload time-ms] reducer."
  [model time-ms command]
  (reduce (fn [model {knob-id :id links :links}]
            (reduce (fn [model link]
                      (if-let [entry (get-in model [:knobs :knobs knob-id :motion (:id link)])]
                        (try (advance-link model command time-ms knob-id link entry)
                             (catch Exception error
                               ;; Dropped, not retried every frame, and recorded so a real bug is seen.
                               (-> (dissoc-motion model knob-id (:id link))
                                   (update-in [:knobs :diagnostics]
                                              #(vec (take-last 32 (conj (vec %) {:code :move-failed :knob-id knob-id
                                                                                  :link-id (:id link) :beat (beat model)
                                                                                  :message (str (.getMessage error))})))))))
                        model))
                    model links))
          model
          (sort-by :order (filter #(seq (:motion %)) (vals (get-in model [:knobs :knobs]))))))

(defn rebase-clock
  "Shift every in-flight move by a transport rebase (seek, loop, source change
  or Link resume), so each keeps its remaining musical time: no step fires
  again and none is caught up."
  [knobs offset]
  (let [shift #(+ (double %) (double offset))]
    (update knobs :knobs update-vals
            (fn [knob]
              (if-let [motion (:motion knob)]
                (assoc knob :motion (update-vals motion (fn [entry]
                                                          (cond-> (update entry :start shift)
                                                            (:next entry) (update :next shift)
                                                            (:crossings entry) (update :crossings #(mapv shift %))))))
                knob)))))

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
    (cond-> {:knobs (mapv (fn [knob]
                            (-> (select-keys knob [:id :name :value :recipe])
                                (assoc :links (mapv #(link-view model knob % shared) (:links knob)))))
                          knobs)}
      ;; Envelope moves dropped after failing, most recent last.
      (seq (get-in model [:knobs :diagnostics])) (assoc :diagnostics (get-in model [:knobs :diagnostics])))))

(defn knob-view
  "Projection of one knob, or nil."
  [model knob-id]
  (some #(when (= knob-id (:id %)) %) (:knobs (projection model))))

;; ---------------------------------------------------------------------------
;; Restore

(defn- restored-motion
  "Moves of a restored knob that still fit it, as fresh moves: entries for
  links that exist and have an envelope, well formed, keeping the level
  they started from, their start beat (no later than the restored transport
  beat, so a move never waits for the future) and rate, and the step last
  committed. Counters, crossings and the recorded source are dropped:
  crossings are planned again against the current schema and commits carry
  the knob's own source. Anything else is dropped, per link."
  [{:keys [motion links]} beat epoch]
  (let [enveloped (set (map :id (filter enveloped? links)))]
    (not-empty (into {} (for [[id entry] motion
                              :when (and (enveloped id) (motion-entry? entry))
                              :let [start (if (and (time/finite-number? beat) (> (:start entry) beat)) (double beat) (:start entry))]]
                          [id (-> (select-keys entry [:from :beats :written])
                                  (assoc :start start :turn (str "restore/" epoch "/" start)))])))))

(defn- sanitized-motion
  "Saved knob state with each knob's malformed moves dropped, per link, so one
  bad entry neither refuses the whole restore nor reaches a frame."
  [knobs]
  (if-not (and (map? knobs) (map? (:knobs knobs)))
    knobs
    (update knobs :knobs update-vals
            (fn [knob]
              (if-not (and (map? knob) (contains? knob :motion))
                knob
                (let [motion (when (map? (:motion knob))
                               (not-empty (into {} (filter (fn [[id entry]] (and (string? id) (motion-entry? entry))))
                                                (:motion knob))))]
                  (if motion (assoc knob :motion motion) (dissoc knob :motion))))))))

(defn restore
  "Rebind saved knobs to restored launch IDs. A link to a launch that did not
  survive keeps its old identity and is marked dangling, so it stays visible
  and editable instead of being dropped or resolving to an unrelated launch.
  Version 1 state is restored as version 2. In-flight envelope moves are kept
  (see `restored-motion`) for links that still have an envelope. beat is the
  restored transport beat."
  ([knobs instance-ids epoch] (restore knobs instance-ids epoch nil))
  ([knobs instance-ids epoch beat]
   (validate-state (sanitized-motion knobs))
   (-> (sanitized-motion knobs)
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
                       motion (restored-motion knob beat epoch)]
                   (if motion (assoc knob :motion motion) (dissoc knob :motion))))))))

(defn saved-state
  "Knob state for a backup: written as version 1 when no knob uses a version 2
  field (an envelope, a grid, an in-flight move or an agent creator), so a
  plain backup still opens in an older build."
  [knobs]
  (let [v2? (some (fn [knob]
                    (or (:motion knob) (:created-by knob)
                        (some #(or (contains? % :envelope) (and (map? (:quantize %)) (contains? (:quantize %) :step)))
                              (:links knob))))
                  (vals (:knobs knobs)))]
    (cond-> knobs (not v2?) (assoc :schema-version 1))))

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
