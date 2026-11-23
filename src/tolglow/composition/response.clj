(ns tolglow.composition.response
  "Pure global/selected response envelopes over running instance parameters.
  Apply before a compiled effect step. Canonical parameter values in the show
  model are never changed: only the per-frame instance copy is modulated."
  (:require [tolglow.registry :as registry]
            [tolglow.composition.envelope :as envelope]
            [tolglow.composition.tween :as tween]))

(def ^:private numeric-types #{:number :integer :color :vector :range})
(def ^:private selectors {:effect-ids :effect-id :cue-ids :cue-id :instance-ids :id})

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn- identifier? [x] (or (keyword? x) (string? x)))

(defn- matches? [selection instance]
  (every? (fn [[selector field]]
            (or (not (contains? selection selector))
                (some #{(get instance field)} (get selection selector)))) selectors))

(defn- constrain [schema value]
  (when-not (if (vector? value) (every? registry/finite-number? value)
                (registry/finite-number? value))
    (fail! [:params] "Response arithmetic produced a non-finite value"))
  (let [bounds (or (:bounds schema) (when (= :color (:type schema)) [0 1]))
        clamp (fn [x] (if bounds (max (first bounds) (min (second bounds) x)) x))
        result (cond
                 (= :integer (:type schema))
                 (let [rounded (Math/round (double value))]
                   (if bounds
                     (long (max (Math/ceil (double (first bounds)))
                                (min (Math/floor (double (second bounds))) rounded))) rounded))
                 (vector? value) (mapv clamp value)
                 :else (clamp value))
        result (if (= :range (:type schema)) (vec (sort result)) result)]
    (when-not (registry/valid-value? schema result)
      (fail! [:params] "Response result does not satisfy its parameter schema"))
    result))

(defn- target-params [schemas params]
  (if (= params :all)
    (sort-by str (keep (fn [[k schema]] (when (numeric-types (:type schema)) k)) schemas))
    (filter #(contains? schemas %) params)))

(defn compile-response
  "Validate and compile one response to context -> context middleware.

  Response: {:id ..., :select {:effect-ids [...] :cue-ids [...] :instance-ids [...]},
  :params [:level] (or explicit :all numeric params), :operation :multiply
  (:replace/:add/:max also supported), :range [0 1], :envelope {...},
  :state {:started-at-ms ... :released-at-ms ...}}.

  Omitted selectors match all running instances; supplied selectors intersect,
  and an empty selector vector matches none. Responses are applied in caller
  order. Hard parameter bounds are respected; integers are rounded. A completed
  release holds its terminal response until the caller removes it; this does
  not end the selected effects or secretly restore their values."
  [response]
  (when-not (and (map? response) (registry/data? response) (identifier? (:id response)))
    (fail! [] "Response needs a stable ID and portable map data"))
  (let [{:keys [select params state] :or {select {} params [:level]}} response
        operation (:operation response :multiply)
        range (:range response [0.0 1.0])]
    (when-not (and (map? select)
                   (every? (set (keys selectors)) (keys select))
                   (every? #(and (vector? %) (every? identifier? %)) (vals select)))
      (fail! [:select] "Response selectors must be vectors of effect, cue or instance IDs"))
    (when-not (or (= params :all)
                  (and (vector? params) (every? keyword? params) (= (count params) (count (distinct params)))))
      (fail! [:params] "Response params must be distinct parameter keys or explicit :all"))
    (when-not (#{:multiply :replace :add :max} operation)
      (fail! [:operation] "Unsupported response operation"))
    (when-not (and (vector? range) (= 2 (count range)) (every? registry/finite-number? range))
      (fail! [:range] "Response range must contain two finite values"))
    (when-not (map? state) (fail! [:state] "Response needs explicit envelope state timestamps"))
    (let [sample (envelope/compile-envelope (:envelope response {}))
          _ (sample state {:time-ms (:started-at-ms state) :beat (:started-at-beat state)})
          combine (case operation :multiply * :replace (fn [_ y] y) :add + :max max)]
      (fn [{:keys [instance frame] :as context}]
        (if-not (matches? select instance) context
          (let [schemas (get-in instance [:definition :params] {})
                values (registry/resolve-params schemas (:params instance {}))
                selected (target-params schemas params)]
            (if (empty? selected) context
              (let [{:keys [value done?]} (sample state frame)
                    amount (tween/interpolate (first range) (second range) value)
                    updated
                    (reduce (fn [values key]
                              (let [schema (get schemas key)
                                    current (get values key)]
                                (when-not (numeric-types (:type schema))
                                  (fail! [:params key] "Response envelopes apply to numeric parameters only"))
                                (assoc values key (constrain schema
                                                            (if (vector? current)
                                                              (mapv #(combine % amount) current)
                                                              (combine current amount)))))) values selected)]
                (-> context
                    (assoc-in [:instance :params] updated)
                    (update :response-values (fnil conj [])
                            {:id (:id response) :value amount :done? done? :params (vec selected)}))))))))))

(defn compile-middleware
  "Compile ordered responses to one pure context transform. Compose as
  (comp compiled-effect (compile-middleware responses))."
  [responses]
  (when-not (vector? responses) (fail! [] "Responses must be an ordered vector"))
  (when-not (= (count responses) (count (distinct (map :id responses))))
    (fail! [] "Response IDs must be unique within one middleware chain"))
  (let [middleware (mapv compile-response responses)]
    (fn [context] (reduce (fn [context f] (f context)) context middleware))))

(defn- diagnose [context response]
  (let [instance (:instance context)]
    (if-not (matches? (:select response {}) instance) context
      (reduce (fn [current param]
                (try
                  ((compile-response (assoc response :params [param])) current)
                  (catch Exception error
                    (update current :response-values (fnil conj [])
                            {:id (:id response) :params [param] :error (ex-message error)}))))
              context (target-params (get-in instance [:definition :params]) (:params response [:level]))))))

(defn monitor-middleware
  "Use the render chain on healthy inputs. For a failed instance, identify the
  affected response and parameter and retain independent inputs for inspection."
  [responses]
  (let [apply-chain (compile-middleware responses)]
    (fn [context]
      (try (apply-chain context)
           (catch Exception _ (reduce diagnose context responses))))))

(defn apply-responses
  "Apply response data in vector order to an effect context. For repeated frames
  use compile-middleware to validate/compile once per response revision."
  [responses context]
  ((compile-middleware responses) context))
