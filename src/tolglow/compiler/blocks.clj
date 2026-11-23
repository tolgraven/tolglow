(ns tolglow.compiler.blocks
  "Resolve effect-local material bindings into the ordinary Afterglow field plan."
  (:require [afterglow.render.blocks :as blocks]
            [afterglow.render.ir :as ir]
            [afterglow.runtime :as runtime]
            [tolglow.composition.tween :as tween]))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path (into [:blocks] path) :message message}]})))

(defn- path-of [{:keys [op key path]}]
  (case op
    :param [:params key]
    :variable (into [:variables] (or path [key]))
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

(defn- var-uniforms [bindings]
  (into {} (map-indexed
             (fn [i path]
               [path {:op :param :key (keyword "tolglow.block-input" (str i))
                      :type (get-in bindings [path :type] :number)}]))
        (sort-by pr-str (filter #(= :variables (first %)) (keys bindings)))))

(defn- raw-value [{:keys [type-param entries uniforms]} [kind id :as path]]
  (case kind
    :params (type-param {:op :param :key id} path)
    :variables (get uniforms path)
    :outputs (if-some [i (output-index entries id)] (:value (nth entries i))
                 (fail! path "Unknown output assignment"))
    :varyings nil))

(declare binding-plan)

(defn- lower-input [{:keys [bindings type-param] :as compiler} current trail node path]
  (cond
    (and (= path current) (#{:params :variables} (first path))) (raw-value compiler path)
    (contains? bindings path) (:expression (binding-plan compiler path trail))
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

(defn- lower [compiler current trail depth node]
  (when (> depth 64) (fail! current "Expression nesting exceeds 64 levels"))
  (if-let [path (and (map? node) (path-of node))]
    (lower-input compiler current trail node path)
    (cond
      (= :tween (:op node)) (tween-input compiler node)
      (or (number? node) (vector? node)) {:op :const :value node}
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

(defn prepare [{:keys [blocks prefabs params targets]} entries prepare-expression]
  (when-not (and (map? blocks) (<= (count blocks) 64) (every? valid-path? (keys blocks)))
    (fail! [] "Use up to 64 binding paths in :params, :variables, :varyings or :outputs"))
  (when-not (or (nil? prefabs) (and (map? prefabs) (<= (count prefabs) 64) (every? keyword? (keys prefabs))))
    (fail! [] "Use up to 64 named local prefab definitions"))
  (let [uniforms (var-uniforms blocks)
        reserved (set (map (comp :key val) uniforms))
        ;; Identical references share a lowered tree. The cache lives only while
        ;; compiling this definition and never enters the live step or show file.
        prepared (volatile! {})
        tweens (volatile! {})
        compiler {:bindings blocks :entries entries :uniforms uniforms :prepared prepared :tweens tweens
                  :catalog (merge blocks/library prefabs)
                  :type-param #(prepare-expression %1 params %2)}]
    (when (or (some reserved (keys params)) (some #(= "tolglow.block-tween" (namespace %)) (keys params))) (fail! [] "Parameter ID collides with a block variable binding"))
    (doseq [path (keys blocks)] (binding-plan compiler path #{}))
    (let [outputs (mapv (partial output-entry compiler) (range) entries)]
      {:outputs outputs
       :tweens (mapv (fn [{:keys [input sample]}] [(:key input) sample]) (vals @tweens))
     :uniforms uniforms
     :targets (when (some? targets) (set targets))
     :output-bindings (into {} (keep (fn [[kind id :as path]]
                                       (when (= kind :outputs)
                                         (let [{:keys [attribute targets]} (nth entries (output-index entries id))]
                                           [path {:attribute attribute :targets (when targets (set targets))}]))))
                            (keys blocks))
     :expressions (update-vals @prepared :expression)
     :traces (update-vals @prepared blocks/compile-trace)})))

(defn with-variables [plan params frame]
  (reduce-kv (fn [result path uniform]
               (let [value (get-in frame path)]
                 (if (nil? value) result (assoc result (:key uniform) value))))
             params (:uniforms plan)))

(defn sample-tweens [plan env]
  (reduce (fn [out [id sample]] (assoc-in out [:params id] (:value (sample env))))
          env (:tweens plan)))

(defn positions
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
  (if-not plan context
    (let [inputs (dissoc env :state) number (:number frame) rig (:fixtures frame)]
      (vary-meta context assoc ::runtime/observe
                 (fn [[path target]] (observation plan inputs number rig index path target))))))
