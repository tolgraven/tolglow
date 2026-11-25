(ns tolglow.compiler
  "Compile declarative effects to pure context middleware, using the shared
  Afterglow numeric IR. Compilation and evaluation never start output."
  (:require [afterglow.render.ir :as ir]
            [tolglow.compiler.blocks :as blocks]
            [tolglow.effects.reactive :as reactive]
            [afterglow.runtime.keyframes :as keyframes]
            [afterglow.runtime.keyframes.values :as keyframe-values]
            [afterglow.time :as time]
            [tolglow.registry :as registry]
            [tolglow.lighting-match :as matching]
            [tolglow.modulation :as modulation]
            [tolglow.composition.tween :as tween]
            [tolglow.composition.envelope :as envelope]))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn- with-path [path f]
  (try (f)
       (catch clojure.lang.ExceptionInfo cause
         (throw (ex-info (.getMessage cause)
                         {:errors (mapv #(update % :path (fn [p] (into path p)))
                                        (or (:errors (ex-data cause))
                                            [{:path [] :message (.getMessage cause)}]))}
                         cause)))))

(defn- parameter-type [schema]
  (case (:type schema)
    (:number :integer) :number
    :color :vec3
    :range :vec2
    :vector (when (<= 2 (:size schema) 4) (keyword (str "vec" (:size schema))))
    nil))

(defn- prepare-expression
  ([expression schemas path] (prepare-expression expression schemas nil path))
  ([expression schemas reactive-inputs path]
   (cond
     (or (number? expression) (vector? expression)) {:op :const :value expression}
     (not (map? expression)) (fail! path "Expression must be a keyword map or numeric constant")
     (= :reactive (:op expression))
     (let [input (:key expression)]
       (when-not (contains? reactive-inputs input)
         (fail! (conj path :key) "Expression references an undeclared reactive input"))
       (when-not (every? #{:op :key :type} (keys expression))
         (fail! path "Use :op, :key and optional :type to read a reactive input"))
       {:op :param :key input :type (:type expression :number)})
     (= :param (:op expression))
     (let [schema (get schemas (:key expression))
           type (when schema (parameter-type schema))]
       (when-not schema (fail! (conj path :key) "Expression references an unknown parameter"))
       (when-not type (fail! path "This parameter type is not supported by the numeric expression IR"))
       (when (and (:type expression) (not= type (:type expression)))
         (fail! (conj path :type) "Expression parameter type disagrees with its schema"))
       (assoc expression :type type))
     (contains? expression :args)
     (if (vector? (:args expression))
       (update expression :args #(mapv (fn [i arg] (prepare-expression arg schemas reactive-inputs (conj path :args i))) (range) %))
       (fail! (conj path :args) "Expression args must be a vector"))
     :else expression)))

(def geometry-attributes
  "World positions and directions accept signed coordinates outside the unit interval."
  #{:aim :direction})

(defn- compile-output [schemas bind-pure prepared? reactive-inputs i output]
  (let [path [:outputs i :value]
        value (:value output)
        tween? (= :tween (:op value))
        sample (when tween? (with-path (conj path :spec) #(tween/compile-tween (:spec value))))
        expression (when-not tween? (if prepared? value (prepare-expression value schemas reactive-inputs path)))
        errors (when expression (ir/validate-expression expression))]
    (when (seq errors)
      (throw (ex-info "Invalid effect expression"
                      {:errors (mapv #(update % :path (fn [p] (into path p))) errors)})))
    (let [type (if tween?
                 (let [value (:value (sample 0))]
                   (cond (number? value) :number
                         (and (vector? value) (= 3 (count value))) :vec3
                         :else :unsupported))
                 (ir/expression-type expression))
          expected (if (#{:color :aim :direction} (:attribute output)) :vec3 :number)]
      (when-not (= expected type) (fail! path (str "Output needs " (name expected) " values"))))
    (when (and (geometry-attributes (:attribute output))
               (not= :replace (:blend output :replace)))
      (fail! [:outputs i :blend] "Geometry assignments require replace blending"))
    (let [calculate (if tween? (fn [env] (:value (sample env))) (ir/compile-expression expression))
          inputs (if tween? [[:elapsed-beats] [:elapsed-ms] [:meter]] (ir/input-paths expression))]
      (when (and reactive-inputs (some #(not= :params (first %)) inputs))
        (fail! path "Reactive outputs read only declared signal values and constants"))
      (assoc output :blend (:blend output :replace) :frame-only? tween? :expression expression
             :inputs inputs
             :evaluate (if bind-pure (bind-pure {:pure? true :inputs inputs} calculate) calculate)))))

(defn hdr-attribute? [attribute]
  (or (= attribute :color) (= attribute :dimmer) (contains? registry/emitter-attributes attribute)))

(defn- normalized-output? [attribute value hdr?]
  (cond
    (geometry-attributes attribute)
    (and (vector? value) (= 3 (count value)) (every? registry/finite-number? value))
    (= attribute :color)
    (and (vector? value) (= 3 (count value))
         (every? #(and (registry/finite-number? %) (<= 0 %) (or hdr? (<= % 1))) value))
    :else (and (registry/finite-number? value) (<= 0 value) (or (and hdr? (hdr-attribute? attribute)) (<= value 1)))))

(defn compile-plan
  "Validate an effect once and expose a reusable runtime compilation plan.
  Prepared output :expression trees have parameter types resolved; tween outputs
  have nil :expression and :frame-only? true. Functions in this plan are runtime
  artifacts, never serializable show state or registry definitions."
  ([definition] (compile-plan definition nil))
  ([definition bind-pure]
  (let [errors (registry/validate-definition :effects definition)]
    (when (seq errors) (throw (ex-info "Invalid effect definition" {:errors errors}))))
  (when (contains? definition :modulation)
    (let [errors (modulation/validate-config (:modulation definition))]
      (when (seq errors)
        (throw (ex-info "Invalid modulation definition"
                        {:errors (mapv #(update % :path (partial into [:modulation])) errors)})))))
  (let [schemas (:params definition {})
        defaults (registry/resolve-params schemas {})
        entries (registry/output-entries (:outputs definition))
        reactive-inputs (get-in definition [:reactive :inputs])
        block-outputs? (some (comp blocks/needs-plan? :value) entries)
        _ (when (and reactive-inputs block-outputs?)
            (fail! [:outputs] "Reactive outputs read only declared signal values and constants"))
        block-plan (when (or (contains? definition :blocks)
                             block-outputs?)
                     (blocks/prepare (merge {:blocks {}} definition) entries prepare-expression))
        outputs (mapv (partial compile-output schemas bind-pure (some? block-plan) reactive-inputs) (range)
                      (or (:outputs block-plan) entries))
        selected (when (contains? definition :targets) (set (:targets definition)))
        envelope-spec (:envelope definition)
        envelope-fn (when (contains? definition :envelope)
                      (with-path [:envelope] #(envelope/compile-envelope envelope-spec)))
        envelope-attributes (:attributes envelope-spec [:dimmer])]
    (when (and envelope-fn
               (not (and (vector? envelope-attributes)
                          (every? (into registry/emitter-attributes [:dimmer :color :strobe :fog])
                                  envelope-attributes))))
      (fail! [:envelope :attributes] "Envelope targets must be intensity/color attributes"))
    {:schemas schemas :defaults defaults :outputs outputs :block-plan block-plan :selected selected
     :envelope-fn envelope-fn :envelope-attributes envelope-attributes})))

(defn- validate-frame-fixtures! [fixtures]
  (when-not (and (vector? fixtures) (every? #(and (map? %) (some? (:id %))) fixtures))
    (fail! [:frame :fixtures] "Frame fixtures must be a vector with stable IDs"))
  fixtures)

(defn- fixture-selector [rig selected]
  (let [select (if selected #(filterv (comp selected :id) %) identity)
        prepared (when rig (select (validate-frame-fixtures! rig)))]
    (fn [fixtures]
      (if (and rig (identical? rig fixtures))
        prepared
        (select (validate-frame-fixtures! fixtures))))))

(defn predictable-outputs? [definition]
  (let [outputs (registry/output-entries (:outputs definition))]
    (and (seq outputs) (every? (comp keyframe-values/continuous :attribute) outputs))))

(defn- compile-reactive-effect [definition outputs rig]
  (let [{:keys [inputs transition]} (:reactive definition)
        definition-targets (:targets definition)
        targets (if rig (filterv (set definition-targets) (map :id (validate-frame-fixtures! rig))) definition-targets)
        calculate (fn [input-values]
                    (let [env {:params input-values}
                          resolved (mapv (fn [output] (assoc output :value ((:evaluate output) env))) outputs)]
                      {:assignments (into []
                                          (mapcat (fn [id]
                                                    (keep (fn [{:keys [attribute blend value targets]}]
                                                            (when (or (nil? targets) (some #{id} targets))
                                                              {:target-id id :attribute attribute :blend blend :value value}))
                                                          resolved)))
                                          targets)}))]
    (keyframes/with-inputs
      (keyframes/dependency-driven {:inputs (vec (keys inputs)) :transition transition} calculate)
      (partial reactive/project-inputs inputs))))

(defn compile-effect
  "Return a context -> context middleware function. :outputs accepts an
  attribute->expression map or ordered output descriptors. Numeric expressions
  use afterglow.render.ir; {:op :tween :spec ...} is an output-level time tween.
  Definition/output :targets select stable fixture IDs, intersected when both
  are present. Optional :envelope scales :attributes (default [:dimmer]).
  A captured rig lets targeted scalar effects reuse their selection while the
  show presents that same immutable fixture vector.
  Scalar outputs and linear RGB components are normalized to [0,1], while
  :aim/:direction use finite 3D world coordinates/vectors;
  invalid computed values throw before any caller can publish a frame."
  ([definition] (compile-effect definition nil))
  ([definition bind-pure] (compile-effect definition bind-pure nil))
  ([definition bind-pure match-plan] (compile-effect definition bind-pure match-plan nil))
  ([definition bind-pure match-plan rig]
  (let [{:keys [schemas defaults outputs selected block-plan envelope-fn envelope-attributes]}
        (compile-plan definition bind-pure)
        fixture-index (when block-plan (blocks/fixture-index rig))
        select-fixtures (fixture-selector rig selected)
        step (fn [{:keys [frame instance state] :as context}]
      (let [now (:time-ms frame)
            start (:started-at-ms instance 0)
            _ (when-not (and (registry/finite-number? now) (registry/finite-number? start))
                (fail! [:frame :time-ms] "Effect frame/start time must be finite milliseconds"))
            params (registry/resolve-params schemas (merge defaults (:params instance)))
            params (if block-plan (blocks/with-variables block-plan params frame) params)
            releasing? (= :releasing (:phase instance))
            clock (time/frame-clock frame)
            envelope-result (when envelope-fn
                              (envelope-fn (cond-> {:started-at-ms start
                                                   :started-at-beat (:started-at-beat instance)
                                                   :timing-meter (:timing-meter instance)}
                                             releasing? (assoc :released-at-ms (:release-at-ms instance now)
                                                               :released-at-beat (:release-at-beat instance))) clock))
            done? (boolean (or (:done? context)
                               (if envelope-fn (:done? envelope-result) releasing?)))
            frame-fixtures (:fixtures frame [])
            fixtures (select-fixtures frame-fixtures)
            env {:time-ms now :elapsed-ms (max 0 (- now start)) :params params :state state
                 :beat (:beat clock)
                 :elapsed-beats (when (and (number? (:beat clock)) (number? (:started-at-beat instance)))
                                  (max 0 (- (:beat clock) (:started-at-beat instance))))
                 :meter (time/normalize-meter (:timing-meter instance))}
            env (if (and block-plan (not done?))
                  (blocks/sample-inputs block-plan env (or (:transport-clock frame) clock)) env)
            ;; A tween depends on the instance clock, not fixture geometry.
            ;; Resolve it once per frame, including captured-meter normalization.
            outputs (if done? outputs
                        (mapv (fn [i output]
                                (if (:frame-only? output)
                                  (assoc output :sampled-value
                                         (with-path [:outputs i :value] #((:evaluate output) env)))
                                  output)) (range) outputs))
            targets (when (matching/active? definition)
                      (or (when (or (nil? rig) (identical? rig frame-fixtures)) match-plan)
                          (matching/prepare frame-fixtures selected)))
            assignments
            (if done? []
              (into []
                    (mapcat
                     (fn [fixture]
                       (let [rows (keep-indexed
                          (fn [i {:keys [attribute blend targets evaluate frame-only? sampled-value]}]
                            (when (or (nil? targets) (some #{(:id fixture)} targets))
                              (let [value (try
                                            (if frame-only? sampled-value
                                                (evaluate (assoc env :fixture fixture :position (:position fixture [0 0 0])
                                                                 :surface-uv (:surface-uv fixture))))
                                            (catch clojure.lang.ExceptionInfo cause
                                              (throw (ex-info "Effect evaluation failed"
                                                              {:definition-id (:id definition) :fixture-id (:id fixture)
                                                               :errors [{:path (into [:outputs i :value] (:path (ex-data cause)))
                                                                         :message (.getMessage cause)}]} cause))))
                                    value (if (and envelope-fn (some #{attribute} envelope-attributes))
                                            (if (vector? value) (mapv #(* (:value envelope-result) %) value)
                                                (* (:value envelope-result) value)) value)]
                                (when-not (normalized-output? attribute value (:hdr? frame))
                                  (fail! [:outputs i :value] "Computed fixture output must be finite and within [0,1]"))
                                {:target-id (:id fixture) :attribute attribute :value value :blend blend}))) outputs)]
                           (if targets
                             (matching/match-rows (get targets (:id fixture)) (get params matching/target-param) rows)
                             rows))))
                    fixtures))]
        (-> context
            (blocks/observed block-plan env frame (when (identical? rig frame-fixtures) fixture-index))
            (assoc :state state :done? done?)
            (update :assignments (fnil into []) assignments)
            (update :intents #(or % [])))))]
    (if (:reactive definition)
      (compile-reactive-effect definition outputs rig)
      (cond-> step
        block-plan (vary-meta blocks/step-info block-plan)
        (predictable-outputs? definition) keyframes/predictable)))))

(defn demo-definitions
  "Portable example effects for the studio, REPL, and deterministic tests."
  []
  (let [level {:type :number :default 0.7 :bounds [0 1] :recommended [0.2 0.8]
               :random {:enabled? true :max-change 0.2}}
        color {:type :color :default [1.0 0.03 0.01] :random {:enabled? false}}
        wave {:op :add :args [0.5 {:op :multiply :args [0.5 {:op :sin :args [{:op :multiply :args [{:op :bar} (* 2 Math/PI)]}]}]}]}]
    [{:id :solid-red :version 1 :name "Solid red" :ui/layout {:template :wash :monitor :color} :capabilities [:emission]
      :params {:level level :color color}
      :outputs {:color {:op :param :key :color} :dimmer {:op :param :key :level}}}
     {:id :breath :version 1 :name "Breathing color" :ui/layout :pulse :capabilities [:emission]
      :params {:level level :color (assoc color :default [0.3 0.02 0.6])}
      :envelope {:attack {:beats 1/2} :release {:beats 1}}
      :outputs {:color {:op :param :key :color}
                :dimmer {:op :multiply :args [{:op :param :key :level} wave]}}}
     {:id :motion :version 1 :name "Moving head sweep" :ui/layout {:template :movement :monitor :level} :capabilities [:movement]
      :requires [:emission] :params {:level level}
      :outputs {:pan wave :tilt 0.5}}]))
