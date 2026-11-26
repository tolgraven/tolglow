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
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.registry :as registry]))

(def schema-version 1)

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

(def ^:private link-keys #{:id :destination :range :direction :curve :input :quantize :unit :dangling})

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
                (and (map? quantize) (= [:ratios] (keys quantize)) (#{:preferred :free} (:ratios quantize))))
    "Quantize is :integer, {:ratios :preferred} or {:ratios :free}"))

(defn link-errors
  "Structural errors for one link, without consulting the show."
  [{:keys [id destination range direction curve input unit dangling] :as link}]
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
           (when-not (or (nil? unit) (keyword? unit)) ["A unit is a keyword"])
           (when-not (or (nil? dangling) (and (map? dangling) (keyword? (:reason dangling))))
             ["A dangling marker needs a keyword :reason"]))))))

(defn- knob-errors [knob]
  (if-not (map? knob) ["A knob must be a map"]
          (vec (concat
                (when-not (every? #{:id :name :value :links :order} (keys knob)) ["Unknown knob field"])
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
  (when-not (and (map? knobs) (= schema-version (:schema-version knobs)) (map? (:knobs knobs))
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

(defn- discrete-steps
  "Ordered discrete values a link steps through, from its first endpoint to its
  second, or nil for a continuous link."
  [{:keys [range quantize]} schema]
  (let [[a b] range]
    (cond
      (:ratios quantize) (let [steps (ratio-steps (:ratios quantize) schema range)]
                           (if (> (exact a) (exact b)) (vec (rseq steps)) steps))
      (= :enum (:type schema)) (let [values (vec (or (:values schema) (:options schema)))
                                     i (.indexOf ^java.util.List values a) j (.indexOf ^java.util.List values b)]
                                 (when (and (<= 0 i) (<= 0 j))
                                   (if (<= i j) (subvec values i (inc j)) (vec (rseq (subvec values j (inc i)))))))
      (= :boolean (:type schema)) (vec (distinct [a b]))
      :else nil)))

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

(defn resolve-value
  "Destination value for a knob position on one link and destination schema."
  [{:keys [range quantize] :as link} schema value]
  (let [u (shaped link value)
        [a b] range]
    (if-let [steps (discrete-steps link schema)]
      (nth steps (link-step link value (count steps)))
      (let [lerp #(+ % (* u (- %2 %)))
            continuous (if (#{:color :vector} (:type schema)) (mapv lerp a b) (lerp a b))
            continuous (if (and (= :integer quantize) (= :number (:type schema)))
                         (Math/round (double continuous)) continuous)]
        (constrain schema continuous)))))

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
    (nil? (:unit link)) (dissoc :unit)))

(defn- checked-link
  "Validate a link structurally and against the current show. A link may target
  a cue that is not running yet; a missing or incompatible target is refused."
  [model link]
  (let [errors (link-errors link)]
    (check (empty? errors) (first errors) {:link-id (:id link)}))
  (let [link (normalized-link link)
        view (destination-view model link)]
    (check (not= :dangling (:status view)) (:message view) {:reason (:reason view)})
    (when (= :lfo (get-in link [:destination :kind]))
      (check (= :ok (:status view)) "Bind a waveform to that parameter on a running launch first"))
    (when-let [schema (:schema view)]
      (check (nil? (compatibility-error link schema)) (compatibility-error link schema)))
    link))

(defn- knob-value-writes
  "Apply waveform-field links in place and return [model outcomes writes].
  Parameter and variable writes are returned for the owner's command reducer."
  [model knob]
  (reduce
   (fn [[model outcomes writes] link]
     (let [view (destination-view model link)
           base {:link-id (:id link)}]
       (if-not (= :ok (:status view))
         [model (conj outcomes (assoc base :status :skipped :reason (or (:reason view) (:status view)))) writes]
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

(defn apply-operation
  "Pure [model command] operation for afterglow.control.model/apply-domain-command."
  [model {:keys [op knob-id] :as command}]
  (let [knobs (state model)]
    (case op
      :knob/create
      (let [{:keys [name value links]} command
            value (or value 0.0)]
        (check (id? knob-id) "A knob needs a stable ID")
        (check (not (contains? (:knobs knobs) knob-id)) "A knob with that ID already exists" {:knob-id knob-id})
        (check (and (string? name) (<= 1 (count name) 128)) "Give the knob a name")
        (check (unit-fraction? value) "A knob value is between 0 and 1")
        (check (or (nil? links) (vector? links)) "Links are a vector")
        (let [links (mapv #(checked-link model %) (or links []))]
          (check (or (empty? links) (apply distinct? (map :id links))) "Link IDs must be distinct within a knob")
          {:model (assoc model :knobs (-> knobs
                                          (assoc-in [:knobs knob-id] {:id knob-id :name name :value value
                                                                       :links links :order (:next-order knobs)})
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
        {:model (assoc-in model [:knobs :knobs knob-id :links]
                          (if index (assoc links index link) (conj links link)))
         :events [{:type :knob/linked :knob-id knob-id :link-id (:id link)}]
         :result {:link-id (:id link) :replaced? (some? index)}})

      :knob/unlink
      (let [knob (knob! model knob-id)
            link-id (link-id (:link-id command))]
        (check (some #(= link-id (:id %)) (:links knob)) "Unknown link" {:link-id link-id})
        {:model (assoc-in model [:knobs :knobs knob-id :links] (vec (remove #(= link-id (:id %)) (:links knob))))
         :events [{:type :knob/unlinked :knob-id knob-id :link-id link-id}]})

      (:knob/set :knob/nudge)
      (let [knob (knob! model knob-id)
            value (if (= :knob/set op)
                    (:value command)
                    (do (check (time/finite-number? (:delta command)) "A nudge needs a finite delta")
                        (min 1.0 (max 0.0 (+ (double (:value knob)) (double (:delta command)))))))
            _ (check (unit-fraction? value) "A knob value is between 0 and 1")
            knob (assoc knob :value value)
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
  destination's committed value, and status (:ok, :inactive or :dangling)."
  [model knob link shared]
  (let [view (destination-view model link)
        schema (:schema view)
        unit (or (:unit link) (:unit schema))
        resolved (when schema (try (resolve-value link schema (:value knob)) (catch Exception _ nil)))
        targets (:targets view)
        current (cond
                  (= :variable (get-in link [:destination :kind])) (current-value model (:destination link) nil)
                  (seq targets) (current-value model (:destination link) (first targets)))
        diverged? (boolean (and (= :ok (:status view)) (some? resolved)
                                (if (= :variable (get-in link [:destination :kind]))
                                  (not (same-value? current resolved))
                                  (some #(not (same-value? (current-value model (:destination link) %) resolved)) targets))))
        steps (when schema (discrete-steps link schema))]
    (cond-> (merge (select-keys link [:id :destination :range :direction :curve :input :quantize])
                   {:label (target-label model (:destination link)) :status (:status view)
                    :shared-with (vec (sort-by str (disj (get shared (destination-key link) #{}) (:id knob))))})
      (:reason view) (assoc :reason (:reason view))
      (:message view) (assoc :message (:message view))
      schema (assoc :schema (select-keys schema [:type :bounds :unit :values :options :semantic/type :name]))
      (some? resolved) (assoc :resolved (display resolved schema unit))
      (some? current) (assoc :current (display current schema unit))
      (seq targets) (assoc :targets targets)
      diverged? (assoc :diverged? true)
      steps (assoc :steps (count steps) :step (link-step link (:value knob) (count steps))))))

(defn projection
  "Knobs in creation order with every link resolved at the knob's value."
  [model]
  (let [knobs (sort-by :order (vals (:knobs (state model))))
        shared (reduce (fn [index knob]
                         (reduce #(update %1 (destination-key %2) (fnil conj #{}) (:id knob)) index (:links knob)))
                       {} knobs)]
    {:knobs (mapv (fn [knob]
                    (-> (select-keys knob [:id :name :value])
                        (assoc :links (mapv #(link-view model knob % shared) (:links knob)))))
                  knobs)}))

(defn knob-view
  "Projection of one knob, or nil."
  [model knob-id]
  (some #(when (= knob-id (:id %)) %) (:knobs (projection model))))

;; ---------------------------------------------------------------------------
;; Restore

(defn restore
  "Rebind saved knobs to restored launch IDs. A link to a launch that did not
  survive keeps its old identity and is marked dangling, so it stays visible
  and editable instead of being dropped or resolving to an unrelated launch."
  [knobs instance-ids epoch]
  (validate-state knobs)
  (update knobs :knobs update-vals
          (fn [knob]
            (update knob :links
                    (fn [links]
                      (mapv (fn [{:keys [destination] :as link}]
                              (if-let [old (:instance-id destination)]
                                (if-let [new-id (get instance-ids old)]
                                  (assoc-in link [:destination :instance-id] new-id)
                                  (assoc link :dangling {:reason :instance-missing :epoch epoch
                                                         :message (str "Launch " old " did not survive the restore")}))
                                link))
                            links))))))

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
