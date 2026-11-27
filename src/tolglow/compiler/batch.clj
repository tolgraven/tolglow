(ns tolglow.compiler.batch
  "Rig-specific declarative compiler producing owned typed contributions.
  Compiler plans are immutable; each evaluation owns its new result arrays."
  (:require [afterglow.render.ir :as ir]
            [afterglow.runtime :as runtime]
            [afterglow.runtime.keyframes :as keyframes]
            [afterglow.time :as time]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.blocks :as blocks]
            [tolglow.compiler.capability :as capability]
            [tolglow.compiler.gradient :as gradient]
            [tolglow.lighting-match :as matching]
            [tolglow.registry :as registry]))

(defn- fail! [path message & [data]]
  (throw (ex-info message (merge {:errors [{:path path :message message}]} data))))

(defn- coordinates? [size point]
  (and (vector? point) (= size (count point)) (every? registry/finite-number? point)))

(defn- valid-fixture? [{:keys [id position surface-uv] :or {position [0 0 0]} :as fixture}]
  (and (map? fixture) (some? id) (coordinates? 3 position)
       (or (nil? surface-uv) (coordinates? 2 surface-uv))))

(defn- validate-rig! [fixtures]
  (when-not (and (vector? fixtures) (every? valid-fixture? fixtures)
                 (= (count fixtures) (count (set (map :id fixtures)))))
    (fail! [:fixtures] "Batch compiler needs distinct fixture IDs, finite xyz positions and optional finite [u v] pairs")))

(defn- select-targets [fixtures selected {:keys [targets attribute] :as output}]
  ;; A head without the output's attribute is skipped, never faulted (#688).
  (let [targets (when (some? targets) (set targets))
        fixtures (filterv #(and (or (nil? selected) (contains? selected (:id %)))
                                (or (nil? targets) (contains? targets (:id %)))
                                (capability/capable? % attribute)) fixtures)]
    (when (seq fixtures)
      (cond-> (assoc output :target-ids (mapv :id fixtures) :positions (mapv #(:position % [0 0 0]) fixtures))
        (some :surface-uv fixtures) (assoc :surface-uvs (mapv :surface-uv fixtures))))))

(defn- output-groups [outputs prepare-group bind-pure]
  ;; Group only consecutive, independent fields. Duplicate attributes force a
  ;; new layer, so replace/add/max order remains the scalar compiler's order.
  (let [groups (reduce (fn [groups {:keys [target-ids positions surface-uvs] :as output}]
                         (let [previous (peek groups)
                               compatible? (and previous (= (:blend previous) (:blend output))
                                                (= (:target-ids previous) target-ids)
                                                (not (contains? (:expressions previous) (:attribute output))))]
                           (if compatible?
                             (conj (pop groups) (assoc-in previous [:expressions (:attribute output)] (:expression output)))
                             (conj groups {:target-ids target-ids :positions positions :surface-uvs surface-uvs :blend (:blend output)
                                           :expressions {(:attribute output) (:expression output)}}))))
                       [] outputs)]
    (mapv (fn [{:keys [expressions positions surface-uvs] :as group}]
            (let [plan (ir/compile-batch expressions)
                  uvs (when (:surface-uv? plan) surface-uvs)
                  evaluate (fn [env] (ir/evaluate-batch plan env positions uvs))]
              (if prepare-group
                (assoc group :prepared (if uvs (prepare-group expressions positions plan uvs)
                                         (prepare-group expressions positions plan)))
                (let [inputs (into [] (comp (mapcat ir/input-paths)
                                            (remove #{[:position] [:surface-uv]}) (distinct)) (vals expressions))]
                  (assoc group :evaluate (if bind-pure
                                           (bind-pure {:pure? true :inputs inputs} evaluate)
                                           evaluate))))))
          groups)))

(defn- finish-fields!
  "Validate and optionally scale exclusively owned IR results before publication."
  [outputs envelope-result envelope-attributes hdr?]
  (doseq [[attribute {:keys [values]}] outputs]
    (let [^doubles values values
          geometric? (contains? compiler/geometry-attributes attribute)
          emission-hdr? (and hdr? (compiler/hdr-attribute? attribute))
          scale (double (if (and envelope-result (contains? envelope-attributes attribute))
                          (:value envelope-result) 1.0))
          scale? (not (== 1.0 scale))]
      (dotimes [index (alength values)]
        (let [value (* scale (aget values index))]
          (when-not (and (Double/isFinite value) (or geometric? (and (<= 0.0 value) (or emission-hdr? (<= value 1.0)))))
            (fail! [:outputs attribute :value]
                   (if geometric? "World coordinates must be finite"
                       "Computed fixture output must be finite and within [0,1]")))
          (when scale? (aset-double values index value))))))
  outputs)

(defn- contribution [{:keys [target-ids evaluate blend prepared expressions]} env envelope-result envelope-attributes hdr?]
  (let [batch {:target-ids target-ids :blend blend}]
    {:kind :batch
     :batch
     (if prepared
       (assoc batch :field-request
              {:group prepared :hdr? hdr? :env (dissoc env :state)
               :scales (when envelope-result
                         (into {} (for [attribute (keys expressions) :when (envelope-attributes attribute)]
                                    [attribute (:value envelope-result)])))})
       (let [result (evaluate env)]
         (finish-fields! (:outputs result) envelope-result envelope-attributes hdr?)
         (merge result batch)))}))

(defn- floating-value? [value]
  (if (vector? value) (every? floating-value? value)
      (or (instance? Double value) (instance? Float value))))

(defn- exact-value? [value]
  (if (vector? value) (some exact-value? value)
      (or (ratio? value) (instance? java.math.BigDecimal value)
          (and (integer? value) (not (<= (- 9007199254740992) value 9007199254740992))))))

(defn- needs-scalar? [value arithmetic?]
  (or (exact-value? value) (and arithmetic? (not (floating-value? value)))))

(defn- coerces-position? [{:keys [op args]}]
  ;; The scalar IR passes these unary functions through double. For binary
  ;; arithmetic the floating literal must be an immediate sibling: a later
  ;; cast cannot repair rounded intermediate integer calculations.
  (or (#{:sin :cos :abs :floor :sqrt :pow} op)
      (and (#{:add :subtract :multiply :divide} op) (= 2 (count args))
           (some #(and (= :const (:op %)) (number? (:value %)) (floating-value? (:value %))) args))))

(defn- exact-coordinates? [op nodes arithmetic? points]
  (let [axis-index {:x 0 :y 1 :z 2 :u 0 :v 1}
        axis #(when (= op (:op %)) (axis-index (:axis %)))
        axes (into #{} (keep axis) nodes)
        exact-axes (when arithmetic?
                     (into #{} (comp (remove coerces-position?) (mapcat :args) (keep axis)) nodes))]
    (and (seq axes)
         (some (fn [p] (some #(needs-scalar? (nth p %) (contains? exact-axes %)) axes)) points))))

(defn- exact-position? [{:keys [nodes arithmetic? positions surface-uvs]}]
  (or (exact-coordinates? :position nodes arithmetic? positions)
      (exact-coordinates? :surface-uv nodes arithmetic?
                          (if surface-uvs (map #(or %1 %2) surface-uvs positions) positions))))

(defn- numeric-contract [outputs]
  (let [expressions (mapv (fn [output]
                            (let [nodes (tree-seq #(and (map? %) (vector? (:args %))) :args (:expression output))]
                              {:nodes nodes :positions (:positions output) :surface-uvs (:surface-uvs output)
                               :arithmetic? (some #(not (#{:const :param :position :surface-uv :time :beat :bar :phrase :vec2 :vec3 :vec4} (:op %))) nodes)})) outputs)
        nodes (mapcat :nodes expressions)
        arithmetic-nodes (mapcat :nodes (filter :arithmetic? expressions))
        params (fn [nodes] (set (keep #(when (= :param (:op %)) (:key %)) nodes)))]
    {:parameter-keys (params nodes)
     :arithmetic-parameter-keys (params arithmetic-nodes)
     :clock-keys (set (keep #(case (:op %)
                             :time (if (= :absolute (:clock % :elapsed)) :time-ms :elapsed-ms)
                             (:beat :bar :phrase) (if (= :absolute (:clock % :elapsed)) :beat :elapsed-beats)
                             nil) nodes))
     :reasons (cond-> []
                (some (fn [{:keys [nodes arithmetic?]}]
                        (some #(needs-scalar? % arithmetic?)
                              (map :value (filter #(= :const (:op %)) nodes)))) expressions)
                (conj :exact-literal)
                (some exact-position? expressions)
                (conj :exact-position))}))

(defn- exact-environment? [{:keys [arithmetic-parameter-keys parameter-keys clock-keys]} env]
  (or (some (fn [key]
              (let [value (get-in env [:params key])]
                (needs-scalar? value (contains? arithmetic-parameter-keys key)))) parameter-keys)
      (some #(not (floating-value? (get env %))) clock-keys)))

(defn- build-effect
  "Compile definition against a captured rig to the ordinary pure step contract.

  Numeric IR outputs, envelopes, stable-ID selectors and duplicate output layers
  use typed fields. World aim points and directions remain signed vec3 fields.
  Tweens use the scalar compiler. Integer-position arithmetic that can remain exact falls back;
  exact parameters or integer clocks select the scalar step for that frame.
  This conservative policy preserves scalar numeric semantics at byte boundaries.
  Unsupported expressions still fail normal validation; they
  are never executed as arbitrary code. Function metadata reports :backend and
  :fallback-reasons. Recompile when the rig or definition changes.

  Responses work unchanged: (comp (compile-effect definition fixtures)
  response-middleware). The canonical model's parameters are not modified."
  ([definition fixtures] (build-effect definition fixtures {}))
  ([definition fixtures {:keys [enabled? prepare-group bind-pure] :or {enabled? true}}]
   (validate-rig! fixtures)
   (when-not (instance? Boolean enabled?) (fail! [:enabled?] "Batch selection must be boolean"))
   (let [definition (or (when (and enabled? (matching/active? definition))
                          (matching/static-definition definition fixtures (registry/output-entries (:outputs definition))))
                        definition)
         {:keys [schemas defaults gradients outputs selected block-plan envelope-fn envelope-attributes] :as plan} (compiler/compile-plan definition)
         selected-outputs (into [] (keep (partial select-targets fixtures selected)) outputs)
         numeric (numeric-contract selected-outputs)
         reasons (cond-> (:reasons numeric)
                   (matching/active? definition) (conj :matched-chroma)
                   (not enabled?) (conj :disabled)
                   (some :frame-only? outputs) (conj :tween)
                   (some :video outputs) (conj :video)
                   (:shader plan) (conj :shader))]
     (if (seq reasons)
       (vary-meta (compiler/compile-effect definition bind-pure
                    (when (matching/active? definition) (matching/prepare fixtures selected))
                    (when enabled? fixtures))
         merge {:backend :ordinary :fallback-reasons reasons})
       (let [groups (output-groups selected-outputs prepare-group bind-pure)
             fixture-index (when block-plan (blocks/fixture-index fixtures))
             scalar-step (compiler/compile-effect definition bind-pure nil fixtures)
             expand-gradients (gradient/expander gradients)
             envelope-attributes (set envelope-attributes)]
         (with-meta
           (fn [{:keys [frame instance state] :as context}]
             (when-not (or (identical? fixtures (:fixtures frame)) (= fixtures (:fixtures frame [])))
               (fail! [:frame :fixtures] "Rig changed; rebuild the batch compiler before rendering"
                      {:type ::stale-rig}))
             (let [now (:time-ms frame), start (:started-at-ms instance 0)
                   _ (when-not (and (registry/finite-number? now) (registry/finite-number? start))
                       (fail! [:frame :time-ms] "Effect frame/start time must be finite milliseconds"))
                   params (expand-gradients (registry/resolve-params schemas (merge defaults (:params instance))))
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
                   env {:time-ms now :elapsed-ms (max 0 (- now start)) :params params :state state
                        :beat (:beat clock)
                        :elapsed-beats (when (and (number? (:beat clock)) (number? (:started-at-beat instance)))
                                         (max 0 (- (:beat clock) (:started-at-beat instance))))
                        :meter (time/normalize-meter (:timing-meter instance))}
                   env (if (and block-plan (not done?))
                         (blocks/sample-inputs block-plan env (or (:transport-clock frame) clock)) env)
                   exact? (exact-environment? numeric env)
                   blocks (if (or done? exact?) []
                              (mapv #(contribution % env envelope-result envelope-attributes (:hdr? frame)) groups))]
               (if exact? (scalar-step context)
                   (-> context
                       (blocks/observed block-plan env frame fixture-index)
                       (assoc :state state :done? done?)
                       (runtime/append-contributions blocks)
                       (update :intents #(or % []))))))
           (blocks/step-info {:backend (if prepare-group :deferred :batch)
                              :fallback-reasons [] :dynamic-fallbacks [:exact-parameters :exact-clock]
                              :groups (count groups)} block-plan)))))))

(defn- normalized-literal? [value]
  (and (instance? Double value)
       ;; Interpolating negative zero can change its sign.
       (not (neg? (Double/compare (double value) 0.0)))
       (<= value 1.0)))

(defn- literal-wash? [{:keys [outputs params] :as definition}]
  ;; Restrict this to plain RGB/white paint with frozen target selection. Other
  ;; definition features, including authored keyframes, keep their sampler.
  (and (every? #{:id :version :params :targets :outputs} (keys definition))
       (empty? params)
       (map? outputs)
       (seq outputs)
       (every? (fn [[attribute value]]
                 (case attribute
                   :color (and (vector? value) (= 3 (count value)) (every? normalized-literal? value))
                   :white (normalized-literal? value)
                   false))
               outputs)))

(defn compile-effect
  "Compile the live backend. Future keyframes retain owned numeric samples. A
  deferred GPU rig uses its equivalent CPU field evaluator for predictive
  endpoints. Plain CPU RGB/white literals already produce the same field at
  every clock, so automatic windows can share one owned sample."
  ([definition fixtures] (compile-effect definition fixtures {}))
  ([definition fixtures options]
   (if (:reactive definition)
     (vary-meta (compiler/compile-effect definition nil nil fixtures)
                assoc :backend :ordinary :fallback-reasons [:reactive-inputs])
     (let [step (build-effect definition fixtures options)
           backend (:backend (meta step))]
       (cond
         (and (= :batch backend) (literal-wash? definition)) (keyframes/time-invariant step)
         (not (compiler/predictable-outputs? definition)) step
         (= :deferred backend)
         (let [cpu (delay (build-effect definition fixtures (dissoc options :prepare-group)))]
           (keyframes/predictable step (fn [ctx] (@cpu ctx))))
         :else (keyframes/predictable step))))))

(defn compiler-for-rig
  "Return definition -> step, suitable for engine/advance and a shell-owned cache.
  The factory starts no threads and owns no mutable cache. Recreate it whenever
  the captured rig changes. Pass {:enabled? false} to select scalar execution."
  ([fixtures] (compiler-for-rig fixtures {}))
  ([fixtures options]
   (validate-rig! fixtures)
   (fn [definition] (compile-effect definition fixtures options))))
