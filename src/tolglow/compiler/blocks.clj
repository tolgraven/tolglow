(ns tolglow.compiler.blocks
  "Resolve effect-local material bindings into the ordinary Afterglow field plan."
  (:require [afterglow.render.blocks :as blocks]
            [afterglow.render.ir :as ir]
            [afterglow.runtime :as runtime]
            [tolglow.registry :as registry]))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path (into [:blocks] path) :message message}]})))

(defn- path-of [{:keys [op key path]}]
  (case op
    :param [:params key]
    :variable (into [:variables] (or path [key]))
    :varying [:varyings key]
    nil))

(defn- valid-path? [[kind & parts :as path]]
  (and (vector? path) (seq parts)
       (case kind
         :variables (every? keyword? parts)
         (:params :varyings) (and (= 1 (count parts)) (keyword? (first parts)))
         :outputs (and (= 1 (count parts)) (or (keyword? (first parts)) (nat-int? (first parts))))
         false)))

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

(defn- compile-bindings [definition entries prepare-expression]
  (let [bindings (:blocks definition)
        schemas (:params definition {})
        uniforms (var-uniforms bindings)
        reserved (set (map (comp :key val) uniforms))
        catalog (merge blocks/library (:prefabs definition))
        ;; Private only during definition compilation. Shared references point at the
        ;; same lowered tree and form a DAG. No cache survives a definition change.
        prepared (volatile! {})]
    (when (some reserved (keys schemas)) (fail! [] "Parameter ID collides with a block variable binding"))
    (letfn [(raw [path]
              (let [[kind id] path]
                (case kind
                  :params (prepare-expression {:op :param :key id} schemas path)
                  :variables (get uniforms path)
                  :outputs (when-some [i (output-index entries id)] (:value (nth entries i)))
                  :varyings nil)))
            (lower [node current trail depth]
              (when (> depth 64) (fail! current "Expression nesting exceeds 64 levels"))
              (if-let [path (and (map? node) (path-of node))]
                (cond
                  (and (= path current) (#{:params :variables} (first path))) (raw path)
                  (contains? bindings path) (:expression (binding-plan path trail))
                  (= :param (:op node)) (prepare-expression node schemas current)
                  :else (fail! current (str "Unknown block input " path)))
                (cond (or (number? node) (vector? node)) {:op :const :value node}
                      (and (map? node) (contains? node :args))
                      (do (when-not (and (vector? (:args node)) (<= (count (:args node)) 64))
                            (fail! current "Expected up to 64 expression arguments"))
                          (update node :args #(mapv (fn [arg] (lower arg current trail (inc depth))) %)))
                      :else node)))
            (binding-plan [path trail]
              (or (get @prepared path)
                  (do
                    (when (contains? trail path) (fail! path "Circular block binding"))
                    (let [spec (get bindings path)
                          fallback (raw path)
                          _ (when (and (= :outputs (first path)) (nil? fallback))
                              (fail! path "Unknown output assignment"))
                          dry (if (= :variables (first path))
                                {:op :variable :path (subvec path 1)} fallback)
                          chain (assoc spec :base (get spec :base dry))
                          compiled (blocks/plan catalog chain #(lower % path (conj trail path) 0))
                          actual (ir/expression-type (:expression compiled))
                          expected (case (first path)
                                     :params (ir/expression-type fallback)
                                     :variables (:type (uniforms path))
                                     nil)]
                      (when (and expected (not= actual expected))
                        (fail! path "Modulation must preserve the numeric type of the binding"))
                      (vswap! prepared assoc path compiled)
                      compiled))))]
      (doseq [path (keys bindings)] (binding-plan path #{}))
      (let [outputs (mapv (fn [i {:keys [attribute value] :as entry}]
                            (let [numbered (get @prepared [:outputs i])
                                  named (get @prepared [:outputs attribute])]
                              (when (and numbered named) (fail! [:outputs i] "Output was bound twice"))
                              (assoc entry :value (or (:expression numbered) (:expression named)
                                                      (lower value [:outputs i] #{} 0)))))
                          (range) entries)]
        {:outputs outputs
         :uniforms uniforms
         :expressions (update-vals @prepared :expression)
         :traces (update-vals @prepared blocks/compile-trace)}))))

(defn prepare [definition entries prepare-expression]
  (let [bindings (:blocks definition)]
    (when-not (and (map? bindings) (<= (count bindings) 64) (every? valid-path? (keys bindings)))
      (fail! [] "Use up to 64 binding paths in :params, :variables, :varyings or :outputs"))
    (compile-bindings definition entries prepare-expression)))

(defn with-variables [plan params frame]
  (reduce-kv (fn [result path uniform]
               (let [value (get-in frame path)]
                 (if (nil? value) result (assoc result (:key uniform) value))))
             params (:uniforms plan)))

(defn observed
  "Retain only the traced plan and the numeric inputs to the successful step.
  The query executes on demand on the observer. Never close over effect state."
  [context plan env frame]
  (if-not plan context
    (let [traces (:traces plan)
          inputs (dissoc env :state)
          number (:number frame)
          rig (:fixtures frame)]
      (vary-meta context assoc ::runtime/observe
             (fn [[path target]]
               (when-let [trace (get traces path)]
                 (when-let [position (if (nil? target) [0.0 0.0 0.0]
                                       (some #(when (= target (:id %)) (:position % [0.0 0.0 0.0])) rig))]
                   (assoc (trace (assoc inputs :position position)) :time-ms (:time-ms inputs)
                          :frame-number number))))))))
