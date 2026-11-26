(ns tolglow.signals
  "Portable waveform templates, derived channels and bounded previews. Templates
  lower into the existing numeric IR; no parallel signal execution engine exists."
  (:require [clojure.string :as str]
            [afterglow.render.ir :as ir]
            [afterglow.render.blocks :as blocks]
            [tolglow.registry :as registry]
            [tolglow.effects :as fx]))

(def phase-input :signal/phase)

(def units {:phase :cycles :value :amplitude :slope :amplitude-per-cycle
            :angular-velocity :degrees-per-cycle :jagginess :amplitude-per-cycle-squared})

(defn- fail! [path message] (throw (ex-info message {:errors [{:path path :message message}]})))

(defn- identifier? [value] (or (keyword? value) (and (string? value) (not (str/blank? value)))))

(defn- nodes [expression] (tree-seq #(and (map? %) (vector? (:args %))) :args expression))

(defn- portable? [value]
  (and (registry/data? value) (not-any? record? (tree-seq coll? seq value))))

(defn validate-waveform
  "Return [] or structured errors. A waveform is a scalar, clock-free numeric
  template. :signal/phase is its reserved cycle input; other parameter references
  need declared scalar schemas. Spatial/clock expressions bind at sampling time."
  [definition]
  (if-not (map? definition)
    [{:path [] :message "Waveform must be a map"}]
    (let [{:keys [id version params expression range]} definition
          problem (fn [path message] [{:path path :message message}])
          references (filter #(= :param (:op %)) (nodes expression))
          expression-errors (ir/validate-expression expression)]
      (vec
       (concat
        (when-not (identifier? id) (problem [:id] "Waveform needs a stable ID"))
        (when-not (and (integer? version) (pos? version)) (problem [:version] "Waveform version must be positive"))
        (when-not (portable? definition) (problem [] "Waveform must contain portable numeric authoring data"))
        (when-not (map? params) (problem [:params] "Waveform parameters must be a map"))
        (when (and (map? params) (contains? params phase-input))
          (problem [:params phase-input] "Phase is a reserved template input"))
        (mapcat (fn [[key schema]]
                  (concat
                   (when-not (keyword? key) (problem [:params key] "Waveform parameter IDs must be keywords"))
                   (registry/validate-parameter schema [:params key])
                   (when-not (#{:number :integer} (:type schema))
                     (problem [:params key :type] "Waveform inputs must be scalar numeric schemas"))))
                (when (map? params) params))
        (when-not (and (vector? range) (= 2 (count range)) (every? registry/finite-number? range) (apply <= range))
          (problem [:range] "Declared waveform range needs ordered finite endpoints"))
        (mapcat (fn [reference]
                  (when (and (not= phase-input (:key reference))
                             (not (and (map? params) (contains? params (:key reference)))))
                    (problem [:expression] (str "Undeclared waveform input " (:key reference))))) references)
        (when (some #(#{:time :beat :bar :phrase :position} (:op %)) (nodes expression))
          (problem [:expression] "Bind clocks and positions through the phase input, outside the waveform template"))
        (map #(update % :path (partial into [:expression])) expression-errors)
        (when (and (empty? expression-errors) (not= :number (ir/expression-type expression)))
          (problem [:expression] "Waveform expression must return a scalar")))))))

(defn publish
  "Pure monotonic publication. Identical current versions are idempotent;
  history remains available by ID/version for reproducible lowered definitions."
  [state definition]
  (let [errors (validate-waveform definition)]
    (when (seq errors) (throw (ex-info "Invalid waveform" {:errors errors}))))
  (let [id (:id definition) version (:version definition) previous (get-in state [:waveforms id])
        versions (keep (fn [[[entry-id version] _]] (when (= id entry-id) version)) (:versions state))
        maximum (reduce max 0 versions)]
    (cond
      (= previous definition) state
      (<= version maximum) (fail! [:version] "Changed waveform content requires a higher version")
      :else (-> state (assoc-in [:waveforms id] definition)
                (assoc-in [:versions [id version]] definition) (update :revision inc)))))

(defn builtin-waveforms []
  (let [inputs {:phase (fx/param phase-input)}
        from-block (fn [id params]
                     (:expression (blocks/plan {:base 0.0 :stages [{:block id :inputs params}]})))
        sine (fx/clamp (from-block :lfo/sine inputs))
        triangle (from-block :lfo/triangle inputs)
        saw (from-block :lfo/saw inputs)
        pulse (from-block :lfo/pulse (assoc inputs :duty (fx/param :duty)))]
    [{:id :sine :version 1 :name "Sine" :category :smooth :range [0.0 1.0] :params {}
      :expression sine :discontinuities []}
     {:id :triangle :version 1 :name "Triangle" :category :angular :range [0.0 1.0] :params {}
      :expression triangle :discontinuities [{:channel :slope :phase 0.0} {:channel :slope :phase 0.5}]}
     {:id :saw :version 1 :name "Saw" :category :discontinuous :range [0.0 1.0] :params {}
      :expression saw :discontinuities [{:channel :value :phase 0.0}]}
     {:id :pulse :version 1 :name "Pulse" :category :discontinuous :range [0.0 1.0]
      :params {:duty (fx/parameter 0.5 {:bounds [0.0 1.0] :semantic/type :duty-cycle
                                      :semantic/unit :ratio :control/axis :slide})}
      :expression pulse :discontinuities [{:channel :value :phase 0.0} {:channel :value :phase-param :duty}]}]))

(defn new-registry
  "Create an immutable versioned registry. The zero-argument form installs the
  four builtin definitions. Supply a vector to choose a different initial set."
  ([] (new-registry (builtin-waveforms)))
  ([definitions]
   (when-not (vector? definitions) (fail! [] "Initial waveforms must be a vector"))
   (reduce publish {:version 1 :revision 0 :waveforms {} :versions {}} definitions)))

(defn validate-registry
  "Validate an EDN-restored registry without compiling executable saved objects.
  Returns the original data; current entries must equal their latest history."
  [state]
  (when-not (and (map? state) (= #{:version :revision :waveforms :versions} (set (keys state)))
                 (= 1 (:version state)) (integer? (:revision state)) (not (neg? (:revision state)))
                 (map? (:waveforms state)) (map? (:versions state)))
    (fail! [] "Invalid waveform registry"))
  (doseq [[identity definition] (:versions state)]
    (when-not (and (vector? identity) (= 2 (count identity)) (= identity [(:id definition) (:version definition)]))
      (fail! [:versions] "Historical waveform identity disagrees with its definition"))
    (let [errors (validate-waveform definition)]
      (when (seq errors) (throw (ex-info "Invalid waveform history" {:errors errors})))))
  (doseq [[id definition] (:waveforms state)]
    (let [versions (keep (fn [[[entry-id version] _]] (when (= id entry-id) version)) (:versions state))]
      (when-not (and (= id (:id definition)) (= definition (get-in state [:versions [id (:version definition)]]))
                     (= (:version definition) (reduce max 0 versions)))
        (fail! [:waveforms id] "Current waveform must equal its latest historical version"))))
  (when-not (= (set (keys (:waveforms state))) (set (map (comp first key) (:versions state))))
    (fail! [:waveforms] "Every historical waveform needs a current definition"))
  state)

(defn- lookup [state id version]
  (or (if version (get-in state [:versions [id version]]) (get-in state [:waveforms id]))
      (fail! [:waveform-id] (str "Unknown waveform or version " id " " version))))

(defn- bindings [definition options]
  (let [overrides (:params options {})]
    (when-not (map? overrides) (fail! [:params] "Waveform parameter bindings must be a map"))
    (doseq [[key value] overrides]
      (when-not (contains? (:params definition) key) (fail! [:params key] "Unknown waveform parameter"))
      (when (and (number? value) (not (registry/valid-value? (get-in definition [:params key]) value)))
        (fail! [:params key] "Waveform parameter violates its declared bounds")))
    (into {} (for [[key schema] (:params definition)]
               [key (fx/expression (get overrides key (:default schema)))]))))

(defn- bind-template [template replacements]
  (cond
    (= :param (:op template)) (get replacements (:key template))
    (contains? template :args) (update template :args #(mapv (fn [child] (bind-template child replacements)) %))
    :else template))

(defn channels
  "Lower a waveform into :value/:slope/:angular-velocity/:jagginess IR maps.
  Options: :phase expression (default elapsed bar), :version, :params bindings,
  central-difference :window in cycles (default 1/1024), :angle-span-deg (360).
  Derivatives are per cycle, not per second/beat. Discontinuities remain visible."
  [state id options]
  (let [definition (lookup state id (:version options))
        replacements (bindings definition options)
        phase (fx/expression (:phase options (fx/cycles)))
        window (:window options (/ 1.0 1024.0)) span (:angle-span-deg options 360.0)]
    (when-not (and (registry/finite-number? window) (<= 1.0e-6 window 0.25))
      (fail! [:window] "Derivative window must be between 1e-6 and 0.25 cycles"))
    (when-not (and (registry/finite-number? span) (pos? span))
      (fail! [:angle-span-deg] "Angular span must be positive finite degrees"))
    (let [sample #(bind-template (:expression definition) (assoc replacements phase-input %))
          value (sample phase) before (sample (fx/subtract phase window)) after (sample (fx/add phase window))
          slope (fx/divide (fx/subtract after before) (* 2.0 window))
          curvature (fx/divide (fx/add after (fx/multiply -2.0 value) before) (* window window))]
      {:value value :slope slope :angular-velocity (fx/multiply span slope)
       :jagginess {:op :abs :args [curvature]}})))

(defn sample
  "Return one lowered channel expression (default :value) for composition into
  an effect. All template parameters disappear into the supplied bindings."
  [state id options]
  (or (get (channels state id options) (:channel options :value))
      (fail! [:channel] "Unknown signal channel")))

(defn waveform-block
  "Capture a registered waveform/version as a portable prefab. Its phase and
  waveform parameters become stage inputs. Supply :phase-reference and a
  musical :period to set the prefab's phase default. Choose :slope, :jagginess or
  :angular-velocity to drive another effect input from the same signal."
  ([state id] (waveform-block state id {}))
  ([state id options]
   (let [definition (lookup state id (:version options))
         input #(hash-map :op :input :key %)
         defaults (update-vals (:params definition) :default)
         settings (assoc options :params (into {} (map (fn [key] [key (input key)])) (keys defaults))
                         :phase (input phase-input))
         default-phase (if (contains? options :phase-reference)
                         (fx/cycles (:period options {:beats 1}) (select-keys options [:phase-reference :phase :clock]))
                         {:op :beat})]
     {:name (:name definition) :mode :replace
      :waveform {:id id :version (:version definition) :channel (:channel options :value)}
      :inputs (assoc defaults phase-input default-phase)
      :expression (sample state id settings)})))

(defn- choice-definitions [state choices]
  (when-not (and (vector? choices) (<= 1 (count choices) 16))
    (fail! [:choices] "Select between 1 and 16 registered waveforms"))
  (mapv (fn [choice]
          (if (map? choice) (lookup state (:id choice) (:version choice)) (lookup state choice nil))) choices))

(defn selector-schema
  "Numeric selector schema with stable ordered choices for a rich picker/MPE.
  Selection rounds to the nearest index; :step and semantic choices are metadata.
  Options: :default-id, :label, :description, :control/axis (nil opts out)."
  ([state choices] (selector-schema state choices {}))
  ([state choices options]
   (let [definitions (choice-definitions state choices)
         default-id (:default-id options (:id (first definitions)))
         index (first (keep-indexed (fn [index definition] (when (= default-id (:id definition)) index)) definitions))]
     (when-not index (fail! [:default-id] "Default waveform must be in the selector choices"))
     (fx/parameter (double index)
                   (merge {:bounds [0.0 (double (dec (count definitions)))] :step 1.0
                           :semantic/type :waveform-selector :semantic/category :modulation
                           :control/axis :slide :label "Waveform"
                           :choices (mapv (fn [index definition choice]
                                            (merge {:value (double index) :id (:id definition) :version (:version definition)
                                                    :label (:name definition (str (:id definition))) :category (:category definition)}
                                                   (when (map? choice) (select-keys choice [:params]))))
                                          (range) definitions choices)}
                          (select-keys options [:label :description :control/axis]))))))

(defn selected-channels
  "Lower a selector and ordered waveform choices into ordinary IR arithmetic.
  Choices may be IDs or {:id :version :params} maps. Templates are all evaluated;
  custom functions must be defined for all supplied inputs, even when unselected."
  [state selector choices options]
  (let [definitions (choice-definitions state choices)
        selected (fx/clamp {:op :floor :args [(fx/add selector 0.5)]} 0.0 (double (dec (count choices))))
        alternatives (mapv (fn [choice definition]
                             (channels state (:id definition)
                                       (merge options (when (map? choice) (select-keys choice [:params]))
                                              {:version (:version definition)}))) choices definitions)
        mask (fn [index] (fx/subtract 1.0 {:op :min :args [(fx/constant 1.0)
                                                         {:op :abs :args [(fx/subtract selected index)]}]}))]
    (into {} (for [channel [:value :slope :angular-velocity :jagginess]]
               [channel (apply fx/add (map-indexed (fn [index alternative]
                                                    (fx/multiply (mask index) (get alternative channel))) alternatives))]))))

(defn select-waveform [state selector choices options]
  (or (get (selected-channels state selector choices options) (:channel options :value))
      (fail! [:channel] "Unknown signal channel")))

(defn preview
  "Sample the same IR in a bounded cycle-domain chart. Selection is an ID or
  {:selector numeric-value :choices [...]}. Options include waveform bindings,
  :samples (2..2048, default129), :cycles (positive, default1), :start-phase (0).
  Result contains sample rows, explicit units and derivative-window metadata."
  ([state selection] (preview state selection {}))
  ([state selection options]
   (let [count (:samples options 129) cycles (:cycles options 1.0) start (:start-phase options 0.0)
         _ (when-not (and (integer? count) (<= 2 count 2048)) (fail! [:samples] "Preview needs 2..2048 samples"))
         _ (when-not (and (registry/finite-number? cycles) (pos? cycles) (registry/finite-number? start)
                          (registry/finite-number? (+ start cycles)))
             (fail! [:cycles] "Preview phase interval must be finite and positive"))
         options (assoc options :phase (fx/param :preview/phase))
         expressions (if (map? selection)
                       (selected-channels state (:selector selection) (:choices selection) options)
                       (channels state selection options))
         compiled (into {} (map (fn [[channel expression]] [channel (ir/compile-expression expression)])) expressions)
         samples (mapv (fn [index]
                         (let [phase (+ (double start) (* (double cycles) (/ (double index) (dec count))))
                               env {:params {:preview/phase phase}}]
                           (into {:phase phase} (map (fn [[channel evaluate]] [channel (evaluate env)])) compiled))) (range count))]
     {:samples samples :units units :window (:window options (/ 1.0 1024.0))
      :angle-span-deg (:angle-span-deg options 360.0) :selection selection
      :start-phase start :cycles cycles})))

(defn glyph
  "One cycle of a registered waveform for a shape picker: `:samples` values
  (default 49) of its :value channel over phase 0…1, normalised by the declared
  :range and rounded to four decimals. It is sampled from the compiled IR that
  `preview` and the engine run, with the waveform's default parameters unless
  :params binds them, so a glyph is never a hand-drawn lookalike. Returns nil
  when a sample is not finite."
  ([state id] (glyph state id {}))
  ([state id options]
   (let [definition (lookup state id (:version options))
         [low high] (:range definition [0.0 1.0])
         span (- (double high) (double low))
         samples (:samples (preview state id (merge {:samples 49} (select-keys options [:version :params :samples]))))
         values (map :value samples)]
     (when (and (pos? span) (every? registry/finite-number? values))
       (mapv #(/ (Math/round (* 10000.0 (/ (- (double %) low) span))) 10000.0) values)))))
