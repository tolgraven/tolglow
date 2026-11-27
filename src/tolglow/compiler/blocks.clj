(ns tolglow.compiler.blocks
  "Resolve effect-local material bindings into the ordinary Afterglow field plan."
  (:require [afterglow.render.blocks :as blocks]
            [afterglow.render.ir :as ir]
            [afterglow.runtime :as runtime]
            [afterglow.runtime.keyframes :as keyframes]
            [afterglow.time :as time]
            [tolglow.compiler.gradient :as gradient]
            [tolglow.composition.tween :as tween]
            [tolglow.oscillator :as oscillator]))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path (into [:blocks] path) :message message}]})))

(defn- path-of [{:keys [op key path]}]
  (case op
    :param [:params key]
    :variable (let [segments (or path [key])]
                (when-not (and (vector? segments) (seq segments)
                               (every? #(or (keyword? %) (string? %) (nat-int? %)) segments))
                  (fail! [:variable] "Use a nonempty show variable path of keys or vector indices"))
                (into [:variables] segments))
    :varying [:varyings key]
    nil))

(defn- valid-path? [path]
  (when (vector? path)
    (let [[kind & parts] path]
      (and (seq parts)
           (case kind
             :variables (every? keyword? parts)
             (:params :varyings) (and (= 1 (count parts)) (keyword? (first parts)))
             :outputs (and (= 1 (count parts)) (or (keyword? (first parts)) (nat-int? (first parts))))
             false)))))

(defn- output-index [entries id]
  (if (nat-int? id) (when (< id (count entries)) id)
      (let [indices (keep-indexed #(when (= id (:attribute %2)) %1) entries)]
        (when (next indices)
          (fail! [:outputs id] "This attribute has several assignments. Use its output index."))
        (first indices))))

(defn- read-variable [{:keys [uniforms bindings]} path explicit-type]
  (let [bound-type (when-let [binding (get bindings path)] (:type binding :number))
        type (or explicit-type bound-type :number)
        known (get @uniforms path)]
    (when-not (#{:number :vec2 :vec3 :vec4} type)
      (fail! path "Read a numeric scalar or a vector with two to four components"))
    (when (or (and bound-type (not= type bound-type)) (and known (not= type (:type known))))
      (fail! path "Uses of the same variable must agree on its numeric type"))
    (or known
        (let [index (count @uniforms)
              input {:op :param :key (keyword "tolglow.block-input" (str index)) :type type}]
          (when (>= index 64)
            (fail! path "A field plan supports up to 64 distinct variable inputs"))
          (vswap! uniforms assoc path input)
          input))))

(defn needs-plan? [expression]
  (and (map? expression) (or (#{:variable :oscillator-phase :phase-accumulator} (:op expression))
                             (some needs-plan? (:args expression)))))

(def accumulator-units #{:beats :bars :phrases})

(defn accumulates?
  "Whether a definition reads a phase accumulator, which keeps its phase in
  instance state and so cannot be forecast between keyframes."
  [definition]
  (boolean (some #(and (map? %) (= :phase-accumulator (:op %)))
                 (tree-seq coll? seq (select-keys definition [:outputs :blocks :prefabs])))))

(defn- raw-value [{:keys [type-param entries] :as compiler} [kind id :as path]]
  (case kind
    :params (type-param {:op :param :key id} path)
    :variables (read-variable compiler path nil)
    :outputs (if-some [i (output-index entries id)] (:value (nth entries i))
                 (fail! path "Unknown output assignment"))
    :varyings nil))

(declare binding-plan)

(defn- lower-input [{:keys [bindings type-param] :as compiler} current trail node path]
  (when (and (= :variable (:op node)) (contains? bindings path) (:type node))
    (read-variable compiler path (:type node)))
  (cond
    (and (= path current) (#{:params :variables} (first path))) (raw-value compiler path)
    (contains? bindings path) (:expression (binding-plan compiler path trail))
    (= :variable (:op node)) (read-variable compiler path (:type node))
    (= :param (:op node)) (type-param node current)
    :else (fail! current (str "Unknown block input " path))))

(defn- tween-input [{:keys [tweens]} {:keys [spec]}]
  (or (:input (get @tweens spec))
      (let [sample (tween/compile-tween spec)
            type (ir/expression-type {:op :const :value (:value (sample 0))})
            id (keyword "tolglow.block-tween" (str (count @tweens)))
            input {:op :param :key id :type type}]
        (vswap! tweens assoc spec {:input input :sample sample})
        input)))

(defn- phase-input [{:keys [oscillators]} path node]
  (when-not (and (every? #{:op :phase-reference :period :phase} (keys node))
                 (oscillator/phase-references (:phase-reference node :launch))
                 (oscillator/musical-period? (:period node))
                 (time/finite-number? (:phase node 0.0)))
    (fail! path "Oscillator phase needs a launch or transport reference, a positive musical period and a finite phase offset"))
  (or (get @oscillators node)
      (let [index (count @oscillators)
            input {:op :param :key (keyword "tolglow.block-phase" (str index)) :type :number}]
        (when (>= index 64) (fail! path "A field plan supports up to 64 distinct oscillator phase inputs"))
        (vswap! oscillators assoc node input)
        input)))

(declare lower)

(defn- accumulator-input
  "Lower `{:op :phase-accumulator :id k :period expr :unit u}` to a frame input.
  The period is a scalar, frame-wide expression in `:unit` (default beats)."
  [{:keys [accumulators] :as compiler} current trail depth {:keys [id period unit] :as node}]
  (when-not (and (every? #{:op :id :period :unit} (keys node)) (keyword? id) (contains? node :period)
                 (accumulator-units (or unit :beats)))
    (fail! current "A phase accumulator needs a keyword :id, a :period expression and a :unit of :beats, :bars or :phrases"))
  (if-let [existing (get @accumulators id)]
    (if (= node (:node existing)) (:input existing)
        (fail! current "Two phase accumulators share one :id"))
    (let [lowered (lower compiler current trail (inc depth) period)
          spatial? (some #(and (map? %) (#{:position :surface-uv} (:op %)))
                         (tree-seq #(and (map? %) (vector? (:args %))) :args lowered))
          _ (when (or spatial? (not= :number (ir/expression-type lowered)))
              (fail! current "A phase accumulator period must be one scalar for the whole frame"))
          index (count @accumulators)
          input {:op :param :key (keyword "tolglow.block-accumulator" (str index)) :type :number}]
      (when (>= index 16) (fail! current "A field plan supports up to 16 phase accumulators"))
      (vswap! accumulators assoc id {:node node :input input :unit (or unit :beats)
                                     :evaluate (ir/compile-expression lowered)})
      input)))

(defn- lower [compiler current trail depth node]
  (when (> depth 64) (fail! current "Expression nesting exceeds 64 levels"))
  (if-let [path (and (map? node) (path-of node))]
    (lower-input compiler current trail node path)
    (cond
      (= :tween (:op node)) (tween-input compiler node)
      (= :oscillator-phase (:op node)) (phase-input compiler current node)
      (= :phase-accumulator (:op node)) (accumulator-input compiler current trail depth node)
      (or (number? node) (vector? node)) {:op :const :value node}
      ;; A gradient samples its own parameter at an already lowered position.
      (gradient/gradient-node? node)
      (let [node (cond-> node
                   (and (vector? (:args node)) (<= (count (:args node)) 64))
                   (update :args #(mapv (partial lower compiler current trail (inc depth)) %)))]
        (try (gradient/lower (:schemas compiler) node [])
             (catch clojure.lang.ExceptionInfo error
               (fail! (into current (:path (first (:errors (ex-data error))))) (ex-message error)))))
      (and (map? node) (contains? node :args))
      (do (when-not (and (vector? (:args node)) (<= (count (:args node)) 64))
            (fail! current "Expected up to 64 expression arguments"))
          (update node :args #(mapv (partial lower compiler current trail (inc depth)) %)))
      :else node)))

(defn- binding-plan [{:keys [bindings catalog prepared] :as compiler} path trail]
  (or (get @prepared path)
      (do
        (when (contains? trail path) (fail! path "Circular block binding"))
        (let [spec (get bindings path)
              raw (raw-value compiler path)
              dry (if (= :variables (first path)) {:op :variable :path (subvec path 1)} raw)
              chain (assoc spec :base (get spec :base dry))
              plan (blocks/plan catalog chain (partial lower compiler path (conj trail path) 0))
              actual (ir/expression-type (:expression plan))
              expected (case (first path) :params (ir/expression-type raw) :variables (:type raw) nil)]
          (when (and expected (not= actual expected))
            (fail! path "Modulation must preserve the numeric type of the binding"))
          (vswap! prepared assoc path plan)
          plan))))

(defn- output-entry [{:keys [prepared] :as compiler} i {:keys [attribute value] :as entry}]
  (let [numbered (get @prepared [:outputs i])
        named (get @prepared [:outputs attribute])]
    (when (and numbered named) (fail! [:outputs i] "Output was bound twice"))
    (assoc entry :value (or (:expression numbered) (:expression named)
                            (lower compiler [:outputs i] #{} 0 value)))))

(defn- binding-outputs [entries paths]
  (into {}
        (keep (fn [[kind id :as path]]
                (when (= kind :outputs)
                  (let [{:keys [attribute targets]} (nth entries (output-index entries id))]
                    [path {:attribute attribute :targets (when targets (set targets))}]))))
        paths))

(defn prepare [{:keys [blocks prefabs imported-prefabs params targets]} entries prepare-expression]
  (when-not (and (map? blocks) (<= (count blocks) 64) (every? valid-path? (keys blocks)))
    (fail! [] "Use up to 64 binding paths in :params, :variables, :varyings or :outputs"))
  (when-not (or (nil? prefabs) (and (map? prefabs) (<= (count prefabs) 64) (every? keyword? (keys prefabs))))
    (fail! [] "Use up to 64 named local prefab definitions"))
  (let [uniforms (volatile! {})
        ;; Identical references share a lowered tree. The cache lives only while
        ;; compiling this definition and never enters the live step or show file.
        prepared (volatile! {})
        tweens (volatile! {})
        oscillators (volatile! {})
        accumulators (volatile! {})
        compiler {:bindings blocks :entries entries :uniforms uniforms :prepared prepared :tweens tweens
                  :oscillators oscillators :accumulators accumulators :schemas params
                  :catalog (merge blocks/library imported-prefabs prefabs)
                  :type-param #(prepare-expression %1 params %2)}]
    (when (some (comp #{"tolglow.block-input" "tolglow.block-tween" "tolglow.block-phase" "tolglow.block-accumulator"} namespace) (keys params))
      (fail! [] "Parameter ID collides with a block input binding"))
    (doseq [path (keys blocks)] (binding-plan compiler path #{}))
    (let [outputs (mapv (partial output-entry compiler) (range) entries)]
      {:outputs outputs
       :tweens (mapv (fn [{:keys [input sample]}] [(:key input) sample]) (vals @tweens))
       :oscillators (mapv (fn [[spec input]] [spec (:key input)]) @oscillators)
       :accumulators (mapv (fn [[id {:keys [input unit evaluate]}]] {:id id :key (:key input) :unit unit :evaluate evaluate})
                           (sort-by (comp :key :input val) @accumulators))
       :transport-clock? (boolean (some #(= :transport (:phase-reference %)) (keys @oscillators)))
       :uniforms @uniforms
       :targets (when (some? targets) (set targets))
       :output-bindings (binding-outputs entries (keys blocks))
       :expressions (update-vals @prepared :expression)
       :traces (update-vals @prepared blocks/compile-trace)})))

(defn step-info [metadata plan]
  (cond-> metadata
    (seq (:uniforms plan)) (assoc ::keyframes/variable-paths (vec (keys (:uniforms plan))))
    (:transport-clock? plan) (assoc ::keyframes/transport-clock? true)))

(defn with-variables [plan params frame]
  (reduce-kv (fn [result path uniform]
               (let [value (get-in frame path)]
                 (if (nil? value) result (assoc result (:key uniform) value))))
             params (:uniforms plan)))

(defn- accumulate
  "Advance each phase accumulator by the cue's elapsed beats since its last
  frame divided by the period now, so editing the period keeps the phase it
  has reached. The first frame starts at elapsed beats / period. A stopped
  transport adds nothing, and play resumes from the held phase. The new
  phases go to `::phases` in env; the step commits them to instance state."
  [plan env]
  (if-let [accumulators (seq (:accumulators plan))]
    (let [elapsed (:elapsed-beats env)
          previous (get-in env [:state ::phases])
          _ (when-not (time/finite-number? elapsed)
              (throw (ex-info "A phase accumulator runs only inside a launched cue" {:path [:phase-accumulator]})))
          phases (into {} (map (fn [{:keys [id unit evaluate]}]
                                 (let [period (* (double (evaluate env)) (time/duration->beats {unit 1} (:meter env)))
                                       {:keys [beat phase]} (get previous id)]
                                   (when-not (and (time/finite-number? period) (pos? period))
                                     (throw (ex-info "A phase accumulator period must be positive" {:path [:phase-accumulator id]})))
                                   [id {:beat (double elapsed)
                                        :phase (if (and (time/finite-number? beat) (time/finite-number? phase))
                                                 (+ phase (/ (- elapsed beat) period))
                                                 (/ elapsed period))}])))
                       accumulators)]
      (assoc (reduce (fn [out {:keys [id key]}] (assoc-in out [:params key] (get-in phases [id :phase]))) env accumulators)
             ::phases phases))
    env))

(defn with-phases
  "Instance state with the phases a frame's inputs advanced."
  [state env]
  (if-let [phases (::phases env)] (assoc (or state {}) ::phases phases) state))

(defn sample-inputs
  ([plan env transport-clock] (sample-inputs plan env transport-clock false))
  ([plan env transport-clock isolate-phases?]
   (let [env (reduce (fn [out [id sample]] (assoc-in out [:params id] (:value (sample env))))
                     env (:tweens plan))
         env (reduce (fn [out [spec id]]
                       (try
                         (assoc-in out [:params id] (oscillator/phase spec transport-clock (:elapsed-beats env) (:meter env)))
                         (catch clojure.lang.ExceptionInfo error
                           (if isolate-phases? out (throw error)))))
                     env (:oscillators plan))]
     (accumulate plan env))))

(defn fixture-index
  "Index this rig on first inspection. Effects that nobody views never build the index."
  [rig]
  (when rig (delay (into {} (map (juxt :id identity)) rig))))

(defn- observation [plan inputs number rig index path target]
  (let [trace (get-in plan [:traces path])
        group (:targets plan)
        output (get-in plan [:output-bindings path])
        selection (:targets output)]
    (when (and trace (or (nil? target) (and (or (nil? group) (contains? group target))
                                           (or (nil? selection) (contains? selection target)))))
      (when-let [fixture (cond (nil? target) {}
                               index (get @index target)
                               :else (some #(when (= target (:id %)) %) rig))]
        (let [position (:position fixture [0.0 0.0 0.0])
              surface-uv (or (:surface-uv fixture) (subvec position 0 2))]
          (assoc (trace (assoc inputs :position position :surface-uv surface-uv))
                 :position position :surface-uv surface-uv :target-id target :attribute (:attribute output)
                 :time-ms (:time-ms inputs) :frame-number number))))))

(defn observed
  "Retain the traced plan and numeric inputs from a successful step. Queries
  run on demand. Exclude opaque effect state from the captured environment."
  [context plan env frame index]
  (if (empty? (:traces plan)) context
    (let [inputs (dissoc env :state) number (:number frame) rig (:fixtures frame)
          read-inputs (::runtime/observe (meta context))]
      (vary-meta context assoc ::runtime/observe
                 (fn [[path target :as query]]
                   (or (when read-inputs (read-inputs query))
                       (observation plan inputs number rig index path target)))))))
