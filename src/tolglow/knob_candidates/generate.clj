(ns tolglow.knob-candidates.generate
  "Pure generator for playable linked-knob candidates (#667).

  Data in, data out: `eligibility` reads the running show and lists the
  destinations a generated knob may drive, typed by their schemas, with a
  reason for every parameter it leaves out. `generate` turns that list, an
  optional track-feel personality and a seed into one candidate: a character
  (what the knob means), links in the #666 knob link format with ranges,
  directions, curves, knob windows and musical ratio steps, a score and an
  explanation. Randomness comes only from `tolglow.automation.random` and the
  supplied seed, so the same show, personality, seed and options always give
  the same candidate. See docs/generated-knobs.md."
  (:require [afterglow.control.params :as params]
            [clojure.string :as str]
            [tolglow.authoring.cue-variations :as ratios]
            [tolglow.automation.random :as random]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.knobs :as knobs]
            [tolglow.registry :as registry]))

(def generator-version
  "Bump when a change makes a saved recipe resolve to different links.
  2 (#737): link IDs keep the cue's namespace, and a recipe may carry :avoid.
  A build that only knows 1 refuses such a recipe instead of misreplaying it.
  3 (#747): integer destinations get whole-number endpoints and :quantize
  :integer, so they can be linked at all; versions 1 and 2 drafted fractional
  endpoints the knob API refused, and their replays still leave them out.
  4 (#667): enum, boolean and vector destinations are generated too, a
  number with a :step stays on its grid, and stepped links carry the stepped
  default :envelope. Replays of versions 1–3 leave the new types out and draft
  no envelope."
  4)

(def generator-versions
  "Recipe versions this build replays. A version 1 recipe regenerates with
  version 1 link IDs (the cue's plain name), so its recorded edits still match.
  Versions 1 and 2 replay without integer links, as they were made."
  #{1 2 3 4})

(def min-links 2)

(def max-links
  "One knob drives at most eight links, the columns an encoder display shows."
  8)

(def default-size [3 6])

(def ^:private attempts
  "Drafts drawn per generation; the best-scoring valid draft wins."
  4)

(def ^:private reroll-limit 16)

;; ---------------------------------------------------------------------------
;; Roles: what a destination does musically, from its schema and name

(def ^:private role-tokens
  "Name tokens per role, checked in order. A parameter named x-bars is a
  period, not a position, because :period comes first."
  [[:seed #{"seed"}]
   [:period #{"beats" "bars" "period" "cycle" "dur" "step"}]
   [:rate #{"rate" "speed" "frequency" "density" "chance" "decay"}]
   [:duty #{"duty" "width" "fade" "halo" "softness"}]
   [:intensity #{"level" "dimmer" "intensity" "brightness" "gain" "drive" "opacity"}]
   [:saturation #{"saturation" "sat" "chroma"}]
   [:lightness #{"lightness" "whiteness"}]
   [:hue #{"hue" "shift"}]
   [:spread #{"spread" "fan" "stagger" "radius" "reach" "size" "pitch" "wavelength" "bands" "zoom"}]
   [:position #{"pan" "tilt" "x" "y" "z" "center" "depth" "height" "scroll" "focus"}]])

(def ^:private vector-role-tokens
  "Extra name tokens for vector destinations only, so number roles (and the
  replays of earlier recipes) stay as they were."
  [[:spread #{"extent" "scale" "dimensions"}]
   [:position #{"point" "offset" "origin" "target" "centre" "position" "aim"}]])

(def role-names
  {:intensity "intensity" :saturation "saturation" :lightness "lightness" :hue "hue" :colour "colour"
   :rate "rate" :period "period" :duty "pulse width" :spread "spread" :position "position"
   :mode "choice" :toggle "switch"})

(defn- tokens [k] (str/split (str/lower-case (name k)) #"[-_/.]+"))

(defn- named-role [k table]
  (let [words (set (tokens k))]
    (some (fn [[role names]] (when (some names words) role)) table)))

(defn param-role
  "Role of a destination from its schema and name, or nil when none is known.
  An enum is a choice (:mode) and a boolean a switch (:toggle) whatever its
  name; a vector is a position or a spread by its semantic type or name."
  [k schema]
  (case (:type schema)
    :color :colour
    :enum :mode
    :boolean :toggle
    :vector (cond
              (= :position (:semantic/type schema)) :position
              :else (let [role (or (named-role k role-tokens) (named-role k vector-role-tokens))]
                      (when (#{:position :spread} role) role)))
    (cond
      (or (= :beat-ratio (:semantic/type schema)) (#{:beats :bars :phrases} (:unit schema))) :period
      :else (named-role k role-tokens))))

;; ---------------------------------------------------------------------------
;; Characters: what turning the knob up means

(def characters
  "Each character names the roles it moves and the direction each takes as the
  knob rises. An anchor role must be present for the character to be chosen.
  A choice steps up through its declared values and a switch turns on as the
  knob rises; neither anchors a character (version 4)."
  [{:id :energy :name "Energy" :anchors #{:intensity :rate :period}
    :polarity {:intensity :up :rate :up :period :down :duty :down :saturation :up :spread :up :lightness :down
               :toggle :up :mode :up}
    :weights {:intensity 3 :rate 2 :period 2 :duty 2 :saturation 1 :spread 1 :lightness 1 :toggle 1 :mode 1}}
   {:id :colour :name "Colour" :anchors #{:hue :colour :saturation}
    :polarity {:hue :up :colour :up :saturation :up :lightness :down :intensity :up :mode :up}
    :weights {:hue 3 :colour 3 :saturation 2 :lightness 1 :intensity 1 :mode 1}}
   {:id :motion :name "Motion" :anchors #{:position :spread}
    :polarity {:position :up :spread :up :period :down :rate :up :mode :up :toggle :up}
    :weights {:position 3 :spread 3 :period 1 :rate 1 :mode 1 :toggle 1}}])

(def ^:private character-index (into {} (map (juxt :id identity)) characters))

(def correlations
  "Roles on one owner that move together. Both links share a knob window, so
  for example a pulse gets brighter and shorter in the same stretch of the knob."
  [{:roles #{:intensity :duty} :why "brighter but shorter pulses"}
   {:roles #{:intensity :period} :why "brighter and quicker"}
   {:roles #{:intensity :rate} :why "brighter and busier"}
   {:roles #{:spread :period} :why "wider and quicker"}
   {:roles #{:hue :saturation} :why "the colour turns and deepens together"}])

;; ---------------------------------------------------------------------------
;; Eligibility against the running show

(def generated-types
  "Destination types the current generator drafts."
  #{:number :integer :color :enum :boolean :vector})

(def ^:private legacy-types
  "Types versions 1–3 drafted; their replays leave the others out."
  #{:number :integer :color})

(def type-names
  "Names surfaces use for the :types selection."
  {:number "number" :integer "integer" :color "colour" :enum "choice" :boolean "switch" :vector "vector"})

(defn- running-instances [model]
  (->> (vals (:instances model)) (filter #(= :running (:phase %))) (sort-by :order)))

(defn- owner-of [instance]
  (if-let [cue (:cue-id instance)] {:cue-id cue} {:instance-id (:id instance)}))

(defn- id-text
  "Text of an identifier with its namespace, so :a/wash and :b/wash stay apart."
  [id]
  (if (keyword? id) (subs (str id) 1) (str id)))

(defn- owner-name [instance] (id-text (or (:cue-id instance) (:id instance))))

(defn- waveform-inputs
  "Registered inputs of a binding's waveform, such as a pulse's :duty."
  [model binding]
  (some #(when (= (:waveform binding) (:id %)) (:params %)) (parameter-sources/waveform-options model)))

(defn- range-of [schema]
  (or (:recommended schema) (:bounds schema) (when (= :color (:type schema)) [0.0 1.0])))

(defn- grid-step
  "A schema's declared :step on a number, integer or vector, or nil."
  [schema]
  (let [step (:step schema)]
    (when (and (#{:number :integer :vector} (:type schema)) (registry/finite-number? step) (pos? step)) step)))

(defn- bounded? [bounds]
  (and (vector? bounds) (= 2 (count bounds)) (every? registry/finite-number? bounds) (< (first bounds) (second bounds))))

(defn- exclusion [instance param reason message]
  {:owner (owner-name instance) :param param :reason reason :message message
   :label (str (owner-name instance) " · " (name param))})

(defn- schema-problem
  "Why a parameter schema cannot be generated, or nil."
  [k schema]
  (let [type (:type schema) role (param-role k schema) bounds (range-of schema)]
    (cond
      (not (generated-types type))
      [:unsupported-type (str "A " (name (or type :unknown)) " parameter is linked by hand, not generated")]
      (and (#{:number :integer :vector} type) (not (bounded? bounds)))
      [:unbounded "The parameter has no bounds, so no useful range is known"]
      (and (= :enum type) (< (count (or (:values schema) (:options schema))) 2))
      [:too-few-steps "The choice has one value, so a knob cannot change it"]
      (= :seed role) [:seed-parameter "A seed chooses a pattern; it is not a performance control"]
      (nil? role) [:no-role "No musical role is known for this parameter name"]
      :else nil)))

(defn- destination-entry [instance destination schema role label default]
  {:key (pr-str (into (sorted-map) destination)) :destination destination :schema schema :role role
   :group (owner-name instance) :label label :default default})

(defn knob-driven
  "Destinations the show's existing knobs already drive, as destination key →
  sorted knob IDs. A link addressed to a running launch by instance ID also
  counts for that launch's cue, since a generated link addresses the cue."
  [model]
  (let [instances (:instances model)]
    (-> (reduce (fn [acc [knob-id link]]
                  (let [destination (knobs/destination-key link)
                        cue-id (some->> (:instance-id destination) (get instances) :cue-id)
                        addressed (cond-> [destination]
                                    cue-id (conj (-> destination (dissoc :instance-id) (assoc :cue-id cue-id))))]
                    (reduce #(update %1 %2 (fnil conj []) knob-id) acc addressed)))
                {}
                (for [[knob-id knob] (get-in model [:knobs :knobs]) link (:links knob)] [knob-id link]))
        (update-vals #(vec (sort-by id-text (distinct %)))))))

(defn- knob-driven-exclusion [{:keys [group destination label]} knob-ids]
  {:owner group :param (:param destination) :reason :knob-driven :label label
   :destination destination :knob-ids (vec knob-ids)
   :message (if (seq knob-ids)
              (str "Already driven by knob " (str/join ", " (map id-text knob-ids))
                   "; generate with :include-knob-driven? true to share it")
              "Already driven by an existing knob when the recipe was made")})

(defn- param-entries
  "Eligible destinations and exclusions for one running instance."
  [model instance]
  (let [definition (:definition instance)
        owner (owner-of instance)
        bindings (:parameter-bindings instance)]
    (reduce
     (fn [acc [k schema]]
       (let [entry (get bindings k)
             binding (:binding entry)
             label (str (owner-name instance) " · " (name k))]
         (cond
           (namespace k) acc
           ;; Writing the base value would hold the waveform under the manual-edit
           ;; rule, so a generated knob shapes the waveform itself instead.
           entry
           (let [acc (update acc :excluded conj
                             (exclusion instance k :modulated
                                        "A waveform drives this parameter; its waveform fields are offered instead"))]
             (if-not (= :lfo (:type binding))
               acc
               (let [base (merge {:kind :lfo :param k} owner)
                     role (param-role k schema)
                     gain (when (and (#{:number :integer} (:type schema)) role (not= :seed role) (range-of schema))
                            (destination-entry instance (assoc base :field :to) schema role
                                               (str label " LFO gain") (:default schema)))
                     period (destination-entry instance (assoc base :field :period) knobs/period-schema :period
                                               (str label " LFO period") nil)
                     inputs (for [[input s] (sort-by (comp str key) (waveform-inputs model binding))
                                  :when (nil? (schema-problem input s))]
                              (destination-entry instance (assoc base :input input) s (param-role input s)
                                                 (str label " LFO " (name input)) (:default s)))]
                 (update acc :eligible into (remove nil? (concat [gain period] inputs))))))

           :else
           (if-let [[reason message] (schema-problem k schema)]
             (update acc :excluded conj (exclusion instance k reason message))
             (update acc :eligible conj
                     (destination-entry instance (merge {:kind :param :param k} owner) schema
                                        (param-role k schema) label (:default schema)))))))
     {:eligible [] :excluded []}
     (sort-by (comp str key) (:params definition)))))

(defn- id-match
  "The known ID x names: the same ID or its full text (\"show/wash\"), else
  the only known ID with that plain name. Plain names that match cues in two
  namespaces stay unresolved."
  [ids x]
  (or (some #(when (or (= % x) (= (id-text %) (id-text x))) %) ids)
      (let [named (filter #(= (name %) (id-text x)) (filter keyword? ids))]
        (when (= 1 (count named)) (first named)))))

(defn eligibility
  "Destinations a generated knob may drive on the running show: effect
  parameters and the gain, period and inputs of bound waveforms, deduplicated
  by destination, in launch then parameter order. :cue-ids restricts the
  selection. :avoid maps destination keys (see `knob-driven`) to the knob IDs
  that drive them; those destinations are left out with reason :knob-driven.
  :shared is the same map for destinations offered anyway, which then carry
  :driven-by. :types (a set of `generated-types`) keeps only destinations of
  those types; the others are left out with reason :type-not-selected. Every
  parameter left out is listed under :excluded with a reason."
  ([model] (eligibility model {}))
  ([model {:keys [cue-ids avoid shared types]}]
   (let [known (set (keys (get-in model [:registry :cues])))
         wanted (when (seq cue-ids) (mapv #(or (id-match known %) %) cue-ids))
         running (running-instances model)
         selected (if wanted (filter #(some #{(:cue-id %)} wanted) running) running)
         unselected (for [cue wanted
                          :when (not-any? #(= cue (:cue-id %)) running)]
                      (if (known cue)
                        {:owner (name cue) :reason :not-running :message "The cue is not running" :label (name cue)}
                        {:owner (str cue) :reason :cue-missing :message "No such cue" :label (str cue)}))
         {:keys [eligible excluded]}
         (reduce (fn [acc instance]
                   (let [{e :eligible x :excluded} (param-entries model instance)
                         x (if (and (empty? e) (empty? x))
                             [{:owner (owner-name instance) :reason :no-parameters
                               :message "The cue has no parameters" :label (owner-name instance)}]
                             x)]
                     (-> acc (update :eligible into e) (update :excluded into x))))
                 {:eligible [] :excluded (vec unselected)} selected)
         seen (volatile! #{})
         unique (filter #(when-not (@seen (:key %)) (vswap! seen conj (:key %))) eligible)
         unselected-type? #(and (seq types) (not (contains? (set types) (get-in % [:schema :type]))))
         avoided? #(contains? avoid (:destination %))]
     {:eligible (vec (for [entry unique :when (not (or (avoided? entry) (unselected-type? entry)))]
                       (if-let [knob-ids (get shared (:destination entry))]
                         (assoc entry :driven-by (vec knob-ids))
                         entry)))
      :excluded (vec (distinct (concat excluded
                                       (for [entry unique :when (unselected-type? entry)]
                                         {:owner (:group entry) :param (get-in entry [:destination :param])
                                          :reason :type-not-selected :label (:label entry)
                                          :destination (:destination entry)
                                          :message (str "A " (type-names (get-in entry [:schema :type]))
                                                        " destination; the selection asks for "
                                                        (str/join ", " (map type-names (sort types))))})
                                       (for [entry unique :when (and (avoided? entry) (not (unselected-type? entry)))]
                                         (knob-driven-exclusion entry (get avoid (:destination entry)))))))
      :selection (cond-> {:running (count selected)} wanted (assoc :cue-ids wanted) (seq types) (assoc :types (vec (sort types))))})))

;; ---------------------------------------------------------------------------
;; Track feel

(defn- palette-colours [personality]
  (vec (distinct (mapcat (fn [entry]
                           (cond (vector? entry) [entry]
                                 (:gradient entry) (map :color (:gradient entry))
                                 (:split entry) (:split entry)))
                         (:palette personality)))))

(defn feel
  "Motion character and palette that shape a candidate, with neutral values
  when no personality is selected."
  [personality]
  (let [{:keys [speed smoothness jitter symmetry]} (:motion personality)]
    {:speed (double (or speed 0.5)) :smoothness (double (or smoothness 0.5))
     :jitter (double (or jitter 0.2)) :symmetry (double (or symmetry 0.5))
     :palette (palette-colours personality)}))

(defn- character-weight [{:keys [speed smoothness jitter symmetry]} id]
  (case id
    :energy (+ 0.5 speed)
    :colour (+ 0.5 (* (- 1.0 speed) smoothness) (* 0.3 smoothness))
    :motion (+ 0.4 (* 0.5 jitter) (* 0.3 (- 1.0 symmetry)) (* 0.3 speed))))

;; ---------------------------------------------------------------------------
;; Seeded helpers

(defn- draw [seed] (random/next-random seed))

(defn- scramble
  "Spread an operator's seed over the generator's 32-bit state. Consecutive
  seeds start an LCG at nearly the same first value, so seeds 1, 2 and 3
  would otherwise make the same first choices. Murmur3's 64-bit finalizer."
  [seed]
  (let [z (unchecked-long seed)
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 33)) (unchecked-long 0xff51afd7ed558ccd))
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 33)) (unchecked-long 0xc4ceb9fe1a85ec53))]
    (bit-and (bit-xor z (unsigned-bit-shift-right z 33)) 0xffffffff)))

(defn- weighted-pick
  "[seed item] by cumulative weight in the given order. Items with no weight are
  never chosen; nil when none has weight."
  [seed items weight]
  (let [weighted (keep #(let [w (double (weight %))] (when (pos? w) [% w])) items)
        total (reduce + (map second weighted))]
    (if-not (pos? total)
      [seed nil]
      (let [[seed x] (draw seed)
            target (* x total)]
        [seed (loop [[[item w] & more] weighted acc 0.0]
                (if (or (empty? more) (< target (+ acc w))) item (recur more (+ acc w))))]))))

(defn- between [x low high] (+ low (* x (- high low))))

(defn- round3 [x] (/ (Math/round (* 1000.0 (double x))) 1000.0))

;; ---------------------------------------------------------------------------
;; Ranges, curves and windows

(def ^:private preferred-values
  "Exact musical ratios n/d with n and d from 1, 2, 3, 4, 6 and 8."
  (vec (sort (distinct (for [n ratios/preferred-components d ratios/preferred-components] (/ n d))))))

(defn- ratio-schema? [schema]
  (or (= :beat-ratio (:semantic/type schema)) (#{:beats :bars :phrases} (:unit schema))))

(defn- snap
  "Nearest preferred ratio to x within [low high], or nil."
  [x [low high]]
  (let [inside (filter #(<= low % high) preferred-values)]
    (when (seq inside) (apply min-key #(Math/abs (- (double %) (double x))) inside))))

(defn- musical-range
  "[low high] around centre by factor, on preferred ratios within bounds, with
  at least two distinct values, or nil."
  [centre factor bounds]
  (let [low (snap (/ centre factor) bounds) high (snap (* centre factor) bounds)]
    (when (and low high (< low high)) [low high])))

(defn- continuous-range
  "[low high] for a continuous destination. Position and hue centre on the
  default; other roles spread from the default towards both bounds, so the
  authored value always lies inside the range."
  [role [lo hi] default width]
  (let [span (- hi lo)
        d (max lo (min hi (double (or default (+ lo (/ span 2))))))
        centred (fn [half]
                  (let [low (- d half) high (+ d half)
                        shift (cond (< low lo) (- lo low) (> high hi) (- hi high) :else 0.0)]
                    [(max lo (+ low shift)) (min hi (+ high shift))]))]
    (mapv round3
          (case role
            :position (centred (* 0.5 width span))
            :hue (centred (* 0.25 width span))
            [(- d (* width (- d lo))) (+ d (* width (- hi d)))]))))

(defn- integer-range
  "Whole-number [low high] for an integer destination: each endpoint of an
  ascending range rounded to the nearest whole value inside the bounds, one
  step apart at least, or nil when the bounds hold fewer than two whole values."
  [[a b] [lo hi]]
  (let [lo (long (Math/ceil (double lo))) hi (long (Math/floor (double hi)))
        fit #(max lo (min hi (Math/round (double %))))
        a (fit a) b (fit b)]
    (cond
      (>= lo hi) nil
      (< a b) [a b]
      (< a hi) [a (inc a)]
      :else [(dec a) a])))

(def ^:private fallback-colours
  "Targets for a colour link when no track feel supplies a palette."
  [[1.0 0.55 0.1] [0.9 0.15 0.7] [0.1 0.9 0.8] [1.0 0.15 0.1] [0.1 0.3 1.0] [0.3 1.0 0.3]])

(defn- colour-distance [a b]
  (Math/sqrt (reduce + (map #(let [d (- (double %1) (double %2))] (* d d)) a b))))

(defn- colour-range
  "[seed [default target]]: the target is a seeded choice among the palette
  colours (or fallbacks) clearly unlike the default, so the sweep visibly
  changes colour; the farthest one when none is."
  [seed default palette]
  (let [default (mapv double (or default [1.0 1.0 1.0]))
        choices (vec (if (seq palette) palette fallback-colours))
        distinct-enough (filterv #(> (colour-distance default %) 0.5) choices)
        choices (if (seq distinct-enough) distinct-enough [(apply max-key #(colour-distance default %) choices)])
        [seed target] (random/choose seed choices)]
    [seed [default (mapv double target)]]))

(defn- curve-choices [role {:keys [smoothness jitter]}]
  (cond
    (and (= :intensity role) (>= smoothness 0.7)) [:smoothstep :ease-in-out]
    (= :intensity role) [:ease-in :linear]
    (>= smoothness 0.7) [:ease-in-out :sine :smoothstep]
    (or (>= jitter 0.5) (<= smoothness 0.2)) [:linear :ease-in]
    :else [:linear :ease-in-out :ease-out]))

(defn- link-id
  "Stable link ID from the full cue (or launch) ID and the destination, so cues
  that differ only by namespace never share a link ID. :id-group replaces the
  cue part when a version 1 recipe is replayed."
  [{:keys [destination group id-group]}]
  (let [{:keys [param field input]} destination]
    (str (or id-group group) "-" (id-text param) (when (or field input) (str "-" (id-text (or field input)))))))

(defn- too-few-steps?
  "Whether an integer destination's range holds fewer than two whole values,
  or (from version 4) a stepped number's range fewer than two grid values."
  [version {:keys [schema]}]
  (let [[lo hi] (range-of schema)]
    (or (and (= :integer (:type schema))
             (< (- (Math/floor (double hi)) (Math/ceil (double lo))) 1.0))
        (and (>= version 4) (#{:number :integer :vector} (:type schema)) (grid-step schema)
             (< (- (double hi) (double lo)) (double (grid-step schema)))))))

(defn- too-few-steps-exclusion [{:keys [group destination label schema]}]
  {:owner group :param (:param destination) :reason :too-few-steps :label label :destination destination
   :message (if (grid-step schema)
              "The range holds fewer than two grid steps, so the knob link cannot step"
              "The range holds fewer than two whole values, so an integer knob link cannot step")})

(defn- version-1-ids
  "Version 1 named an owner by its plain name: :rig/feel gave link IDs such as
  \"feel-level\", and cues sharing a plain name across namespaces counted as
  one owner for correlated pairs and owner variety. A replay does the same."
  [entry]
  (let [{:keys [cue-id instance-id]} (:destination entry)
        id (or cue-id instance-id)
        plain (if (keyword? id) (name id) (str id))]
    (assoc entry :id-group plain :group plain)))

(defn- grid-anchor [schema] (or (first (:bounds schema)) 0))

(defn- on-grid
  "x on schema's step grid, the nearest grid value inside the hard bounds."
  [schema step x]
  (let [anchor (grid-anchor schema)
        [lo hi] (:bounds schema)
        k (Math/round (/ (- (double x) anchor) (double step)))
        at #(let [v (+ anchor (* % step))] (if (and (rational? anchor) (rational? step)) v (round3 v)))
        k (cond (and lo (< (double (at k)) lo)) (inc k) (and hi (> (double (at k)) hi)) (dec k) :else k)]
    (at k)))

(defn- grid-range
  "[low high] snapped onto the schema's step grid, one step apart at least, or nil."
  [schema step [a b]]
  (let [a (on-grid schema step a) b (on-grid schema step b)
        [_ hi] (:bounds schema)]
    (cond
      (not= a b) [a b]
      (or (nil? hi) (<= (+ (double b) step) hi)) [a (on-grid schema step (+ b step))]
      :else [(on-grid schema step (- a step)) b])))

(defn- enum-range
  "[seed [from to]]: a contiguous span of two to five declared values that
  holds the default where it can, stepped through in declared order."
  [seed {:keys [values options default]}]
  (let [values (vec (or values options))
        n (count values)
        d (max 0 (.indexOf ^java.util.List values default))
        [seed span] (random/choose seed (vec (range 2 (inc (min n 5)))))
        [seed start] (random/choose seed (vec (range (max 0 (- d (dec span))) (inc (min d (- n span))))))]
    [seed [(values start) (values (+ start (dec span)))]]))

(def ^:private switch-windows
  "Knob windows for a switch: it turns on at knob 0.5, 0.75 or 0.625."
  [[0 1] [0.5 1] [0.25 1]])

(defn- vector-range
  "[seed [from to]]: a curve through vector space. A position moves from the
  default along a seeded direction by width of the range on each component;
  a spread grows every component from below the default towards the upper
  bound. Components stay inside the bounds and on the :step grid."
  [seed role schema default width]
  (let [[lo hi] (range-of schema)
        span (- (double hi) (double lo))
        n (or (:size schema) (count default))
        d (mapv #(max (double lo) (min (double hi) (double %))) (or default (repeat n (+ lo (/ span 2)))))
        [seed directions] (reduce (fn [[seed acc] _] (let [[seed x] (draw seed)] [seed (conj acc (- (* 2.0 x) 1.0))]))
                                  [seed []] (range n))
        clamp #(max (double lo) (min (double hi) %))
        [a b] (case role
                :spread [(mapv #(- % (* width (- % lo))) d) (mapv #(+ % (* width (- hi %))) d)]
                (let [offsets (mapv #(* 0.5 width span %) directions)]
                  [(mapv #(clamp (- %1 %2)) d offsets) (mapv #(clamp (+ %1 %2)) d offsets)]))
        step (grid-step schema)
        tidy (fn [v] (mapv #(if step (on-grid schema step %) (round3 %)) v))]
    [seed [(tidy a) (tidy b)]]))

(defn- build-typed-link
  "[seed link-or-nil] for a choice, switch or vector destination (version 4).
  A choice steps through a seeded span of its declared values and a switch
  turns on at a seeded knob position, both on a linear curve so each step has
  an equal share of the knob window; a vector moves along a seeded curve."
  [seed {:keys [role schema default] :as entry} character feel]
  (let [direction (get-in character [:polarity role] :up)
        base {:id (link-id entry) :destination (:destination entry) :direction direction :input [0 1]}]
    (case (:type schema)
      :enum (let [[seed range] (enum-range seed schema)] [seed (assoc base :range range :curve :linear)])
      :boolean (let [[seed window] (random/choose seed switch-windows)]
                 [seed (assoc base :range [false true] :curve :linear :input window)])
      :vector (let [[seed x] (draw seed)
                    width (between x 0.35 0.8)
                    [seed range] (vector-range seed role schema default width)
                    [seed curve] (random/choose seed (curve-choices role feel))]
                [seed (when (not= (first range) (second range))
                        (cond-> (assoc base :range range :curve curve)
                          (grid-step schema) (assoc :quantize {:step (grid-step schema)})))]))))

(defn- build-scalar-link
  "build-link for a number, integer or colour destination."
  [seed {:keys [role schema default] :as entry} character {:keys [speed jitter smoothness palette] :as feel} version]
  (let [whole? (and (>= version 3) (= :integer (:type schema)))
        direction (get-in character [:polarity role] :up)
        [seed x] (draw seed)
        width (min 1.0 (max 0.3 (* (between x 0.45 0.85)
                                   (+ 1.0 (* 0.2 jitter))
                                   (if (and (> smoothness 0.8) (< speed 0.3)) 0.75 1.0))))
        [seed factor] (random/choose seed (if (> speed 0.6) [4 4 2] [2 2 4]))
        bounds (range-of schema)
        centre (cond (> speed 0.6) 1/2 (< speed 0.3) 2 :else 1)
        [seed range quantize]
        (cond
          (= :colour role) (let [[seed r] (colour-range seed default palette)] [seed r nil])
          (and (#{:period :rate} role) (ratio-schema? schema) (not whole?))
          ;; A waveform period centres on a beat, earlier for a fast feel.
          [seed (musical-range (or (when (rational? default) default) centre) factor (:bounds schema))
           {:ratios :preferred}]
          ;; Periods, and rates counted per beat, land on musical values. A rate
          ;; that is a 0–1 amount (density, chance) stays continuous.
          (or (= :period role) (and (= :rate role) (> (double (second bounds)) 1.0)))
          ;; A whole-number period with no positive default centres like a waveform's.
          [seed (some-> (musical-range (if whole? (if (and default (pos? default)) default centre) (or default 1))
                                       factor bounds)
                        (->> (mapv double)))
           nil]
          :else [seed (continuous-range role bounds default width) nil])
        [range quantize] (if whole? [(some-> range (integer-range bounds)) :integer] [range quantize])
        step (when (and (>= version 4) (not (:ratios quantize)) (grid-step schema)
                        (or (not= :integer (:type schema)) (integer? (grid-step schema))))
               (grid-step schema))
        [range quantize] (if step [(some->> range (grid-range schema step)) {:step step}] [range quantize])
        range (if (and (>= version 4) (= :colour role) range (:bounds schema))
                (let [[lo hi] (params/color-bounds schema)] (mapv (fn [c] (mapv #(max lo (min hi %)) c)) range))
                range)
        [seed curve] (if (:ratios quantize) [seed :linear] (random/choose seed (curve-choices role feel)))]
    [seed (when (and range (not= (first range) (second range)))
            (cond-> {:id (link-id entry) :destination (:destination entry) :range range
                     :direction direction :curve curve :input [0 1]}
              quantize (assoc :quantize quantize)))]))

(defn- build-link
  "[seed link-or-nil] for one destination in a character and feel. From
  version 3 an integer destination gets whole-number endpoints and steps
  through them. Its range is drawn like a number destination's, except that
  an integer with a beat unit takes the period path instead of ratio steps,
  centred like a waveform period, and so also draws a curve. From version 4
  choices, switches and vectors take `build-typed-link`, a number or integer
  with a :step stays on its grid, and a colour target is kept inside the
  colour bounds."
  [seed {:keys [role schema default] :as entry} character {:keys [speed jitter smoothness palette] :as feel} version]
  (if (and (>= version 4) (#{:enum :boolean :vector} (:type schema)))
    (build-typed-link seed entry character feel)
    (build-scalar-link seed entry character feel version)))

(defn- correlation [a b]
  (when (and (= (:group a) (:group b)) (not= (:role a) (:role b)))
    (some #(when (= (:roles %) #{(:role a) (:role b)}) %) correlations)))

;; ---------------------------------------------------------------------------
;; Constraints and score

(defn playable?
  "A link is playable when its schema accepts it and the knob's two ends
  resolve to different destination values."
  [link schema]
  (and (empty? (knobs/link-errors link))
       (nil? (knobs/compatibility-error link schema))
       (not= (knobs/resolve-value link schema 0.0) (knobs/resolve-value link schema 1.0))))

(defn constraint-errors
  "Hard constraints a candidate must meet, as a vector of messages. entries
  maps a link ID to its eligible destination."
  [{:keys [links character]} entries]
  (let [character (character-index character)
        roles (map #(:role (entries (:id %))) links)
        destinations (map knobs/destination-key links)]
    (vec (concat
          (when-not (<= min-links (count links) max-links)
            [(str "A knob needs " min-links " to " max-links " links")])
          (when-not (= (count destinations) (count (set destinations))) ["Two links drive the same destination"])
          (when-not (some (:anchors character) roles)
            [(str "The " (:name character) " character needs one of "
                  (str/join ", " (map role-names (sort (:anchors character)))))])
          (for [link links
                :let [entry (entries (:id link))]
                :when (not (and entry (playable? link (:schema entry))))]
            (str "Link " (:id link) " does not change its destination across the knob"))
          (for [link links
                :let [role (:role (entries (:id link)))]
                :when (not= (:direction link) (get-in character [:polarity role]))]
            (str "Link " (:id link) " runs against the " (:name character) " direction"))))))

(defn- width-fraction [link schema]
  (let [[a b] (:range link) [lo hi] (range-of schema)]
    (when (and (number? a) (number? b) lo (< lo hi))
      (/ (Math/abs (- (double b) (double a))) (- (double hi) (double lo))))))

(defn score
  "Score in 0–1 favouring role and owner variety, moderate range widths,
  correlated pairs and three to six links."
  [{:keys [links pairs]} entries pool]
  (let [n (count links)
        chosen (map #(entries (:id %)) links)
        owners-available (count (distinct (map :group pool)))
        widths (keep #(width-fraction % (:schema (entries (:id %)))) links)
        roles (/ (count (distinct (map :role chosen))) (double (max 1 n)))
        owners (/ (count (distinct (map :group chosen))) (double (max 1 (min n owners-available))))
        width (if (seq widths) (- 1.0 (Math/abs (- 0.6 (/ (reduce + widths) (count widths))))) 0.8)
        correlated (min 1.0 (double (count pairs)))
        size (if (<= 3 n 6) 1.0 0.6)]
    {:roles (round3 roles) :owners (round3 owners) :width (round3 width) :correlated (count pairs)
     :size (round3 size)
     :total (round3 (+ (* 0.3 roles) (* 0.3 owners) (* 0.15 width) (* 0.15 correlated) (* 0.1 size)))}))

;; ---------------------------------------------------------------------------
;; Drafting

(defn- pick-destinations [seed pool character k]
  (let [weights (:weights character)
        anchor? #((:anchors character) (:role %))]
    (loop [seed seed chosen [] remaining pool]
      (if (or (= k (count chosen)) (empty? remaining))
        [seed chosen]
        (let [role-count #(count (filter (fn [c] (= (:role %) (:role c))) chosen))
              open (filter #(< (role-count %) 2) remaining)
              open (if (seq open) open remaining)
              candidates (if (some anchor? chosen) open (or (seq (filter anchor? open)) open))
              weight (fn [d] (* (get weights (:role d) 0)
                                (/ 1.0 (inc (role-count d)))
                                (/ 1.0 (inc (* 0.5 (count (filter #(= (:group d) (:group %)) chosen)))))
                                (if (some #(correlation d %) chosen) 3.0 1.0)))
              [seed picked] (weighted-pick seed candidates weight)]
          (if-not picked
            [seed chosen]
            (recur seed (conj chosen picked) (remove #(= (:key %) (:key picked)) remaining))))))))

(defn- correlated-pairs [chosen]
  (vec (for [[i a] (map-indexed vector chosen) b (drop (inc i) chosen)
             :let [c (correlation a b)] :when c]
         {:links [(link-id a) (link-id b)] :why (:why c)})))

(defn- clusters
  "Link ID sets joined by correlated pairs, in first-pair order."
  [pairs]
  (reduce (fn [found {[a b] :links}]
            (let [hit (filter #(or (% a) (% b)) found)]
              (conj (vec (remove (set hit) found)) (into #{a b} cat hit))))
          [] pairs))

(defn- share-windows
  "Give each correlated cluster one knob window: the whole knob, or its upper half."
  [seed links pairs]
  (reduce (fn [[seed links] cluster]
            (let [[seed x] (draw seed)
                  window (if (< x 0.5) [0.5 1.0] [0 1])]
              [seed (mapv #(if (cluster (:id %)) (assoc % :input window) %) links)]))
          [seed links] (clusters pairs)))

(defn- explain-link [link entry character pairs]
  (let [[a b] (:range link)
        role (:role entry)
        partner (some #(when (some #{(:id link)} (:links %)) %) pairs)
        text #(cond (vector? %) (str "[" (str/join " " (map (fn [c] (format "%.2f" (double c))) %)) "]")
                    (ratio? %) (str (numerator %) "/" (denominator %))
                    (boolean? %) (if % "on" "off")
                    (keyword? %) (name %)
                    :else (str %))
        [w0 w1] (:input link [0 1])]
    (cond-> {:link-id (:id link) :role role
             :why (str (str/capitalize (role-names role)) " "
                       (if (= :up (:direction link)) (str (text a) " → " (text b)) (str (text b) " → " (text a)))
                       " as the knob rises (" (:name character) ")"
                       (cond
                         (nil? (:quantize link)) nil
                         (= :integer (:quantize link)) ", in whole steps"
                         (:step (:quantize link)) (str ", in steps of " (text (:step (:quantize link))))
                         :else ", in preferred beat ratios")
                       (cond
                         (= :toggle role) (str ", switching at knob " (text (+ w0 (/ (- w1 w0) 2.0))))
                         (= [0.5 1.0] (:input link)) ", over the knob's upper half")
                       (when (:envelope link)
                         (str "; smoothed over " (text (get-in link [:envelope :attack :beats]))
                              " beat for the whole range, so brief spikes do not change the step")))}
      partner (assoc :correlated-with (first (remove #{(:id link)} (:links partner))) :correlation (:why partner))
      ;; Offered on request although an existing knob drives it: the last committed write wins.
      (:driven-by entry) (assoc :shared-with (:driven-by entry)))))

(defn- with-envelope
  "From version 4 a stepped link smooths the knob with `knobs/stepped-envelope`,
  so a brief spike or a noisy input does not reach the next step; the step is
  read after the envelope. Continuous links follow the knob at once."
  [link schema version]
  (if (and (>= version 4) (knobs/stepped? link schema))
    (assoc link :envelope knobs/stepped-envelope)
    link))

(defn- draft [seed pool character feel [low high] version]
  (let [[seed x] (draw seed)
        k (min max-links (count pool) (+ low (long (Math/floor (* x (inc (- high low)))))))
        [seed chosen] (pick-destinations seed pool character k)
        [seed built] (reduce (fn [[seed acc] entry]
                               (let [[seed link] (build-link seed entry character feel version)]
                                 [seed (if (and link (playable? link (:schema entry))) (conj acc [entry link]) acc)]))
                             [seed []] chosen)
        chosen (mapv first built)
        pairs (correlated-pairs chosen)
        [seed links] (share-windows seed (mapv second built) pairs)
        links (mapv #(with-envelope %2 (:schema %1) version) chosen links)]
    [seed {:links links :pairs pairs :chosen chosen}]))

(defn- signature [{:keys [character links]}]
  [character (sort (map #(pr-str (into (sorted-map) (knobs/destination-key %))) links))])

(defn- refuse!
  "Refuse options with a code the owner's receipt carries, so clients can branch on it."
  [code message details]
  (throw (ex-info message {:control/error code :details details})))

(defn- insufficient [pool excluded selection message]
  {:status :insufficient :eligible (count pool) :needed min-links :message message
   :eligible-labels (mapv :label pool) :excluded excluded :selection selection})

(defn generate
  "One candidate from eligibility data, a personality (or nil) and options:
  :seed (integer, required), :character (pin one of `characters`), :size
  [min max] links, :generator (a version in `generator-versions`, default the
  current one; only a recipe replay passes an older one). Returns
  {:status :ready ...} with :links, :score, :explain and :recipe, or
  {:status :insufficient ...} explaining which parameters were left out and why."
  [{:keys [eligible excluded selection]} personality {:keys [seed character size generator] :as options}]
  (when-not (integer? seed) (refuse! :invalid-seed "A generation seed must be an integer" {:seed seed}))
  (when-not (generator-versions (or generator generator-version))
    (refuse! :unsupported-generator "The recipe was made by a generator version this build does not know"
             {:generator generator :known generator-versions}))
  (when (and character (not (character-index character)))
    (refuse! :unknown-character (str "Unknown character; choose one of "
                                     (str/join ", " (map (comp name :id) characters)))
             {:character character :known (mapv :id characters)}))
  (let [version (or generator generator-version)
        eligible (if (= 1 version) (mapv version-1-ids eligible) eligible)
        ;; Versions 1–3 drafted numbers, integers and colours only; their replays do too.
        eligible (if (< version 4) (filterv #(legacy-types (get-in % [:schema :type])) eligible) eligible)
        ;; Version 3 leaves out integers that cannot step, with a reason. Earlier
        ;; versions kept them in the pool, whose size feeds the draws, so their
        ;; replays keep them too.
        [eligible excluded] (if (>= version 3)
                              [(filterv (complement #(too-few-steps? version %)) eligible)
                               (into (vec excluded) (map too-few-steps-exclusion)
                                     (filter #(too-few-steps? version %) eligible))]
                              [eligible excluded])
        feel (feel personality)
        [low high] (cond (nil? size) default-size (and (sequential? size) (= 2 (count size))) size)
        _ (when-not (and (integer? low) (integer? high) (<= min-links low high max-links))
            (refuse! :invalid-size (str "Size is [min max] links within " min-links "–" max-links) {:size size}))
        viable (filter (fn [c] (let [pool (filter #(get-in c [:polarity (:role %)]) eligible)]
                                 (and (<= min-links (count pool)) (some #((:anchors c) (:role %)) pool))))
                       characters)
        ;; Always draw the character, so pinning one keeps every later draw.
        [seed* drawn] (weighted-pick (scramble seed) viable #(character-weight feel (:id %)))
        chosen (if character (character-index character) drawn)
        pool (when chosen (filterv #(get-in chosen [:polarity (:role %)]) eligible))]
    (cond
      (< (count eligible) min-links)
      (insufficient eligible excluded selection
                    (str (case (count eligible) 0 "No parameter" 1 "Only one parameter" (str (count eligible) " parameters"))
                         " in the selection can be generated; a knob needs at least " min-links
                         ". Add running cues with numeric or colour parameters, or bind a waveform."))

      (or (nil? chosen) (< (count pool) min-links) (not (some #((:anchors chosen) (:role %)) pool)))
      (insufficient (or pool eligible) excluded selection
                    (if character
                      (str "The " (:name (character-index character)) " character needs at least " min-links
                           " parameters including one of "
                           (str/join ", " (map role-names (sort (:anchors (character-index character)))))
                           "; this selection has " (count pool) ".")
                      "No character fits the eligible parameters: none has enough roles to move together."))

      :else
      (let [entries-of (fn [chosen] (into {} (map (juxt link-id identity)) chosen))
            avoided (vec (sort-by pr-str (keep #(when (= :knob-driven (:reason %)) (:destination %)) excluded)))
            [_ drafts] (reduce (fn [[seed acc] _]
                                 (let [[seed d] (draft seed pool chosen feel [low high] version)] [seed (conj acc d)]))
                               [seed* []] (range attempts))
            scored (for [{:keys [links] picked :chosen :as d} drafts
                         :let [entries (entries-of picked)
                               candidate {:links links :character (:id chosen)}]
                         :when (empty? (constraint-errors candidate entries))]
                     (assoc d :score (score d entries pool) :entries entries))
            best (when (seq scored) (reduce #(if (> (get-in %2 [:score :total]) (get-in %1 [:score :total])) %2 %1) scored))]
        (if-not best
          (insufficient pool excluded selection
                        (str "No playable mapping was found for the " (:name chosen) " character with seed " seed
                             "; reroll or widen the selection."))
          {:status :ready :generator version :seed seed :character (:id chosen)
           :character-name (:name chosen) :name (str "Generated " (:name chosen))
           :links (:links best) :score (:score best)
           :explain (mapv #(explain-link % ((:entries best) (:id %)) chosen (:pairs best)) (:links best))
           :roles (into {} (map (fn [[id e]] [id (:role e)])) (:entries best))
           :recipe (cond-> {:generator version :seed seed :character (:id chosen)}
                     size (assoc :size size)
                     (:cue-ids selection) (assoc :cue-ids (:cue-ids selection))
                     (:types selection) (assoc :types (:types selection))
                     ;; Exactly the knob-driven destinations this show left out, so the
                     ;; recipe repeats after this candidate's own knob drives them too.
                     (seq avoided) (assoc :avoid avoided)
                     ;; Rerolls after a repeat of this recipe share knob-driven destinations too.
                     (:include-knob-driven? options) (assoc :include-knob-driven? true)
                     personality (assoc :personality (select-keys personality [:id :version])))})))))

(defn candidate-signature
  "Comparable identity of a candidate: its character and destination set."
  [candidate]
  (signature candidate))

(defn- reroll-from
  "reroll's search over generate-seed, a function of one seed."
  [generate-seed seed seen]
  (loop [s (inc seed) tries 1 first-ready nil]
    (let [candidate (generate-seed s)]
      (cond
        (not= :ready (:status candidate)) (if first-ready (assoc first-ready :repeated? true) candidate)
        (not (contains? seen (signature candidate))) candidate
        (>= tries reroll-limit) (assoc (or first-ready candidate) :repeated? true)
        :else (recur (inc s) (inc tries) (or first-ready candidate))))))

(defn reroll
  "The next candidate after seed whose signature is not in seen, trying at most
  sixteen consecutive seeds. Stops at the first seed with no playable
  mapping. Returns the candidate, with :repeated? true when it repeats an
  earlier one: every tried seed repeated, or the search stopped after a repeat."
  [eligibility personality options seed seen]
  (reroll-from #(generate eligibility personality (assoc options :seed %)) seed seen))
