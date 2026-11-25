(ns tolglow.autopilot.config
  "Portable controls shared by the performer, REPL, Studio and renderer."
  (:require [afterglow.time :as time]
            [tolglow.autopilot.signal :as signal]
            [tolglow.autopilot.target-response :as target-response]
            [tolglow.registry :as registry]))

(def hdr-defaults {:gain 1.0 :max-excess 1.0 :white-mix 1.0 :spill 0.0 :spill-white-mix 1.0
                   :radius 1.0 :falloff :linear})

(def defaults
  {:enabled? false :scope :global :targets {:brightness 0.5 :activity 0.4}
   :window {:bars 2} :interval {:beats 1} :duration {:bars 1}
   :strategy :gradual :preference :parameters :step 0.10 :tolerance 0.025
   :standby-level 0.0 :max-gain 2.0 :overshoot 0.0 :rotate? true :seed 1
   :hdr hdr-defaults :signals {} :target-response {}})

(defn enabled? [model]
  (let [{:keys [enabled? enable-cue]} (get-in model [:settings :autopilot])]
    (boolean (or enabled? (and enable-cue
                            (some #(and (= enable-cue (:cue-id %)) (= :running (:phase %)))
                                  (vals (:instances model))))))))

(defn- id? [v] (or (keyword? v) (string? v)))

(defn- scope? [s]
  (or (= :global s)
      (and (map? s) (= 1 (count s))
           (cond (contains? s :fixture-ids) (and (vector? (:fixture-ids s)) (every? id? (:fixture-ids s)))
                 (contains? s :group-id) (id? (:group-id s))
                 (contains? s :page-id) (id? (:page-id s))))))

(defn- between? [spec lo hi]
  (if (number? spec) (and (registry/finite-number? spec) (<= lo spec hi)) (signal/valid? spec)))

(def ^:private hdr-ranges
  {:gain [0.0 Double/MAX_VALUE] :max-excess [0.0 Double/MAX_VALUE]
   :radius [0.0 Double/MAX_VALUE] :white-mix [0.0 1.0] :spill [0.0 1.0] :spill-white-mix [0.0 1.0]})

(defn hdr-settings [opts]
  (let [s (merge hdr-defaults opts)]
    (when-not (and (map? opts) (every? (into (set (keys hdr-defaults))
                                              [:enabled? :scope :cue-gains :source-gains :instance-gains]) (keys opts))
                   (boolean? (:enabled? s false)) (scope? (:scope s :global))
                   (#{:linear :smoothstep :quadratic} (:falloff s))
                   (every? (fn [[k [lo hi]]] (between? (s k) lo hi)) hdr-ranges)
                   (every? (fn [k] (let [g (s k {})]
                                     (and (map? g) (every? id? (keys g))
                                          (every? #(between? % 0.0 Double/MAX_VALUE) (vals g)))))
                           [:cue-gains :source-gains :instance-gains]))
      (throw (ex-info "Invalid highlight gain, whitening, spill or scope" {:config opts})))
    s))

(def ^:private policy-ranges
  {:step [0.0 1.0] :tolerance [0.0 1.0] :standby-level [0.0 1.0]
   :overshoot [0.0 1.0] :max-gain [1.0 Double/MAX_VALUE]})

(defn with-defaults [opts]
  (assoc (merge defaults opts) :targets (merge (:targets defaults) (:targets opts))))

(defn settings [opts]
  (let [s (-> (with-defaults opts)
              (update :hdr hdr-settings)
              (update :target-response target-response/validate!))]
    (when-not (and (every? (conj (set (keys defaults)) :enable-cue) (keys opts))
                   (boolean? (:enabled? s)) (boolean? (:rotate? s))
                   (scope? (:scope s)) (or (nil? (:enable-cue s)) (id? (:enable-cue s)))
                   (integer? (:seed s))
                   (#{:instant :gradual} (:strategy s)) (#{:parameters :cues :generate :author} (:preference s))
                   (signal/valid-bank? (:signals s))
                   (= #{:brightness :activity} (set (keys (:targets s))))
                   (every? #(between? % 0.0 1.0) (vals (:targets s)))
                   (every? (fn [[k [lo hi]]] (between? (s k) lo hi)) policy-ranges)
                   (every? #(pos? (:value (time/normalize-duration (s %)))) [:duration :window :interval]))
      (throw (ex-info "Invalid autopilot scope, target, strategy or timing" {:config opts})))
    s))

(defn resolve-level [model spec context lo hi]
  (max lo (min hi (double (if (number? spec) spec (signal/value model spec context))))))

(defn policy-values [model s context]
  (reduce-kv (fn [m k [lo hi]] (assoc m k (resolve-level model (s k) context lo hi)))
             (select-keys s [:strategy :preference :duration :interval :rotate? :seed]) policy-ranges))

(defn targets [model s context]
  (update-vals (:targets s) #(resolve-level model % context 0.0 1.0)))

(defn hdr-values [model config context]
  (let [bounded (reduce-kv (fn [m k [lo hi]] (assoc m k (resolve-level model (config k) context lo hi)))
                           {:falloff (:falloff config)} hdr-ranges)]
    (reduce-kv
      (fn [m k gains]
        (if (= k :cue-gains)
          (reduce-kv
            (fn [m cue spec]
              (if (number? spec)
                (assoc-in m [:cue-gains cue] spec)
                (reduce (fn [m instance]
                          (if (= cue (:cue-id instance))
                            (update-in m [:instance-gains (:id instance)] (fnil * 1.0)
                                       (resolve-level model spec (assoc context :instance-id (:id instance)) 0.0 Double/MAX_VALUE))
                            m)) m (vals (:instances model)))))
            m gains)
          (reduce-kv (fn [m id spec]
                       (let [ctx (if (= k :instance-gains) (assoc context :instance-id id) context)]
                         (update-in m [k id] (fnil * 1.0) (resolve-level model spec ctx 0.0 Double/MAX_VALUE))))
                     m gains)))
      bounded (select-keys config [:cue-gains :source-gains :instance-gains]))))
