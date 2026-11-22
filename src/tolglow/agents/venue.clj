(ns tolglow.agents.venue
  "Inspect the committed lighting frame and translate provider identifiers to their rig types."
  (:require [tolglow.lighting-scenes :as scenes]))

(defn- wire-id [id]
  (if (keyword? id) (subs (str id) 1) id))

(defn- resolve-id [known value]
  (let [matches (distinct (filter #(= (wire-id %) (wire-id value)) known))]
    (case (count matches)
      0 (keyword value)
      1 (first matches)
      (throw (ex-info "Ambiguous textual identifier in this rig" {:id value})))))

(defn- group-ids [model]
  (into (set (keys (scenes/groups model))) (mapcat :groups) (get-in model [:rig :fixtures])))

(def ^:private built-in-signals
  #{[:audio :features :energy-envelope] [:fog/level] [:fog/observed-level]})

(defn scene-request [model arguments]
  (cond-> (update arguments :group-id (partial resolve-id (group-ids model)))
    (:fixture-ids arguments)
    (update :fixture-ids #(mapv (partial resolve-id (map :id (get-in model [:rig :fixtures]))) %))
    (:effect-ids arguments)
    (update :effect-ids #(mapv (partial resolve-id (keys (get-in model [:registry :effects]))) %))
    (:op arguments) (update :op keyword)
    (:brightness-reference arguments) (update :brightness-reference keyword)
    (:target-var arguments)
    (update :target-var
            (fn [path]
              (or (built-in-signals (mapv keyword path))
                  (loop [node (:variables model) remaining path resolved []]
                    (if-let [k (first remaining)]
                      (when (map? node)
                        (let [id (resolve-id (keys node) k)]
                          (recur (get node id) (next remaining) (conj resolved id))))
                      (when (and (number? node) (<= 0 node 1)) resolved)))
                  (throw (ex-info "Choose a normalized show variable as the lighting target" {})))))))

(defn budget-request [model {:keys [enabled? group-id fixture-order strategy] :as args}]
  (let [groups (group-ids model)
        group (when group-id (resolve-id groups group-id))
        ids (set (map :id (get-in model [:rig :fixtures])))
        order (when fixture-order (mapv (partial resolve-id ids) fixture-order))
        controls (cond-> (select-keys args [:limit :max-active :standby-level])
                   strategy (assoc :strategy (keyword strategy))
                   order (assoc :fixture-order order))]
    (when (or (and group (not (groups group))) (some #(not (ids %)) order))
      (throw (ex-info "Choose an existing group and patched fixtures" {:group group :order order})))
    (when (and (empty? controls) (nil? enabled?))
      (throw (ex-info "Supply a brightness budget or enable setting" {})))
    {:scope :lighting-budget
     :values (cond-> {:enabled? (if (nil? enabled?) true enabled?)}
               (seq controls) (assoc-in (if group [:groups group] [:global]) controls))}))

(defn- energy-subscriber-id [id]
  (keyword "agent.energy" (java.net.URLEncoder/encode (wire-id id) java.nio.charset.StandardCharsets/UTF_8)))

(defn- energy-action [subscriber id]
  (let [[action] (:actions subscriber)]
    (when (and (= 1 (count (:actions subscriber))) (= {:type :audio/energy} (:match subscriber))
               (= :lighting/scene (:op action)) (= id (get-in action [:scene :group-id]))
               (= [:data :energy-envelope] (get-in action [:gate :path])))
      action)))

(defn energy-binding [model {:keys [op group-id target above below]}]
  (let [groups (group-ids model)
        group (resolve-id groups group-id)
        id (energy-subscriber-id group)
        prior (some #(when (= id (:id %)) %) (get-in model [:event-subscribers :subscribers]))]
    (when (and prior (nil? (energy-action prior group)))
      (throw (ex-info "A different subscriber already uses this binding ID" {:id id})))
    (case (keyword op)
      :remove {:op :remove :id id :expected-subscriber prior}
      :save (do (when-not (and (groups group) (every? number? [below above target])
                               (<= 0 below) (< below above) (<= above 1) (<= 0 target 1))
                  (throw (ex-info "Choose an existing group, a target, and lower/upper energy thresholds in [0,1]" {})))
                {:op :save :expected-subscriber prior
                 :subscriber {:id id :match {:type :audio/energy}
                              :actions [{:op :lighting/scene :scene {:op :vary :group-id group :target target}
                                         :gate {:path [:data :energy-envelope] :above above :below below}}]}}))))

(defn- page [items offset limit]
  (let [rows (vec (take limit (drop offset items)))
        next-offset (+ offset (count rows))
        total (count items)]
    (cond-> {:total total :items rows}
      (< next-offset total) (assoc :next-offset next-offset))))

(defn- venue-signals [{:keys [variables time-ms epoch]} output]
  (let [capacity (:lighting-capacity output)
        observation (:fog/observation variables)
        features (get-in variables [:audio :features])]
    {:music (when features
              (select-keys features [:time-ms :energy-envelope :energy-dbfs :energy-class :relative-energy
                                     :rms :breakdown? :confidence :calibrated?]))
     :lighting (when capacity
                 (select-keys capacity [:utilization :relative-capacity :relative-output :rated-watts
                                        :used-rated-watts :headroom :color :target :mode :operator-master]))
     :fog {:estimate (when-let [atmosphere (:atmosphere output)]
                       (select-keys atmosphere [:fog-level :fog-level-change-per-s :clearance-half-life-s
                                                :mixed-density :volume-m3 :sample-time-ms
                                                :last-presented-ms :reference-path-m :extinction-per-m :basis]))
           :controller (when-let [controller (:fog-control output)]
                         (select-keys controller [:enabled? :target :level :zone :lower :upper :fresh? :request?
                                                  :clearance-half-life-s]))
           :observation (when observation
                          (assoc (select-keys observation [:level :level-change-per-s :sample-time-ms :epoch :phase :path-m])
                                 :current-epoch? (= epoch (:epoch observation))))}
     :light-budget (:lighting-budget output)
     :light-activity (get-in output [:lighting-activity :scene])
     :show-time-ms time-ms}))

(defn inspect
  "Return only optical/audio measurements and paged emitter/group discovery.
  Full-resolution fixture frames, PCM, pump commands and provider settings stay out."
  ([state request] (inspect state request (constantly {})))
  ([{:keys [model output]} {:keys [group-id offset fixture-offset limit] :or {offset 0 fixture-offset 0 limit 24}} read-sources]
  (when-not (and (every? #(and (integer? %) (<= 0 % Integer/MAX_VALUE)) [offset fixture-offset])
                 (integer? limit) (<= 1 limit 64))
    (throw (ex-info "Invalid lighting inspection page" {})))
  (let [rig (get-in model [:rig :fixtures])
        eligible (set (scenes/eligible-ids rig))
        controlled (into {} (map (juxt :group-id identity)) (:groups (scenes/snapshot model [])))
        ids (group-ids model)
        selected (when group-id (resolve-id ids group-id))
        _ (when (and selected (not (contains? ids selected))) (throw (ex-info "Unknown lighting group" {:id group-id})))
        member-counts (reduce (fn [counts fixture]
                                (if (eligible (:id fixture))
                                  (reduce #(update %1 %2 (fnil inc 0)) counts (distinct (:groups fixture))) counts))
                              {} rig)
        bindings (into {} (map (juxt :id identity)) (get-in model [:event-subscribers :subscribers]))
        group-page (page (if selected [selected] (sort-by wire-id ids)) offset limit)
        group-rows (mapv (fn [id]
                           (let [g (get controlled id)
                                 subscriber (get bindings (energy-subscriber-id id))
                                 action (energy-action subscriber id)]
                             (cond-> {:group-id (wire-id id) :controlled? (boolean g)
                                      :eligible-count (if g (count (:fixture-ids g)) (get member-counts id 0))}
                               action (assoc :energy-binding (merge (select-keys (:gate action) [:above :below])
                                                                    {:target (get-in action [:scene :target])
                                                                     :enabled? (:enabled? subscriber true)}))
                               g (merge (select-keys g [:target :actual :brightness-reference :status :gain :generation])
                                        {:instance-id (wire-id (:instance-id g)) :source-effect-id (wire-id (:source-effect-id g))
                                         :numeric-inputs (into {} (comp (filter (comp number? val)) (take 32)
                                                                         (map (fn [[k v]] [(wire-id k) v])))
                                                               (:source-params g))}))))
                         (:items group-page))
        members (when-let [g (get controlled selected)] (set (:fixture-ids g)))
        fixtures (filter #(and (eligible (:id %)) (or (nil? selected)
                                                      (if members (members (:id %)) (some #{selected} (:groups %))))) rig)
        fixture-page (page fixtures fixture-offset limit)
        source-readings (read-sources (mapv :id (:items fixture-page)))]
    (merge (select-keys model [:epoch :master :blackout?])
           (venue-signals model output)
           {:groups (assoc group-page :items group-rows)
            :eligible-fixtures (update fixture-page :items
                                       #(mapv (fn [f] (cond-> (assoc (get source-readings (:id f) {}) :id (wire-id (:id f))) (:name f) (assoc :name (:name f)))) %))}))))
