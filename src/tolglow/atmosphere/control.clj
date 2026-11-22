(ns tolglow.atmosphere.control
  "Opt-in mixed-room fog control. Times use the physical presentation clock."
  (:require [afterglow.runtime.events :as staging]
            [afterglow.render.atmosphere :as atmosphere]))

(def defaults {:enabled? false :target 0.2 :hysteresis 0.05 :fog-fixture-ids []})
(def default-duty {:max-on-s 0.4 :min-off-s 6.0 :max-level 0.5})
(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))
(defn- unit? [x] (and (finite? x) (<= 0 x 1)))

(defn settings [values]
  (let [{:keys [enabled? target hysteresis fog-fixture-ids] :as s} (merge defaults values)]
    (when-not (and (boolean? enabled?) (unit? target) (unit? hysteresis) (pos? hysteresis)
                   (vector? fog-fixture-ids)
                   (every? #(or (keyword? %) (string? %)) fog-fixture-ids)
                   (= (count fog-fixture-ids) (count (set fog-fixture-ids))))
      (throw (ex-info "Fog control requires a unit target, a positive unit band and distinct selected fogger IDs" {})))
    s))

(defn configure
  "Pure show edit. An operator can also submit this scope using the show settings API."
  [model values]
  (let [s (settings (merge (get-in model [:settings :fog-control]) values))]
    (cond-> (assoc-in model [:settings :fog-control] s)
      (contains? values :target) (assoc-in [:variables :fog/target] (:target s)))))

(defn disarm-restored [model]
  (if (get-in model [:settings :fog-control :enabled?])
    (assoc-in model [:settings :fog-control :enabled?] false)
    model))

(defn valid-duty? [duty]
  (and (map? duty)
       (let [{:keys [max-on-s min-off-s max-level]} (merge default-duty duty)]
         (and (finite? max-on-s) (pos? max-on-s) (Double/isFinite (* 1000.0 max-on-s))
              (finite? min-off-s) (not (neg? min-off-s)) (Double/isFinite (* 1000.0 min-off-s)) (unit? max-level) (pos? max-level)))))

(defn- recent? [now stamp] (and (finite? stamp) (<= 0 (- now stamp) 500)))

(defn- prior-pump [previous id]
  (let [a (get-in previous [:pumps id])
        b (get-in previous [:music-pumps id])]
    (if (> (:rest-until-ms b 0) (:rest-until-ms a 0)) b a)))

(defn- pump [fixture source previous now request? ready?]
  (let [{:keys [fogger compiled-channels]} fixture
        channel (:fog compiled-channels)
        [off full] (:dmx-range fogger [0 255])
        duty (if (valid-duty? (:duty fogger {})) (merge default-duty (:duty fogger)) default-duty)
        reason (cond (or (nil? source) (nil? channel)) :missing
                     (not (valid-duty? (:duty fogger {}))) :invalid-duty
                     (not (recent? now (:last-presented-ms source))) :no-recent-dmx
                     (not ready?) :suspended)
        flowing? (pos? (:flow-m3-s source 0))
        rest-until (if (and (:flowing? previous) (not flowing?))
                     (max (:rest-until-ms previous 0) (+ now (* 1000 (:min-off-s duty))))
                     (:rest-until-ms previous 0))
        continuing? (and (:active? previous) (< now (:until-ms previous 0)))
        start? (and (not flowing?) (>= now rest-until) (not continuing?))
        active? (boolean (and (nil? reason) request? (or continuing? start?)))
        until (if (and active? (not continuing?)) (+ now (* 1000 (:max-on-s duty))) (:until-ms previous 0))]
    {:active? active? :until-ms until :flowing? flowing?
     :rest-until-ms (if active? (max rest-until (+ until (* 1000 (:min-off-s duty)))) rest-until)
     :mode (cond reason reason active? :injecting (and request? (< now rest-until)) :resting
                 flowing? :external-flow :else :idle)
     :off-bytes (atmosphere/off-bytes fixture)
     :demand (if active? (atmosphere/physical-level channel (+ off (* (:max-level duty) (- full off)))) 0.0)}))

(defn advance
  "Calculate automatic demand from an immutable, delivered DMX observation.
  :fog-level is the calibrated attenuation over the venue reference sightline."
  [model patch observation prior now bypass?]
  (let [s (merge defaults (get-in model [:settings :fog-control]))
        epoch (:epoch model)
        previous (when (= epoch (:epoch prior)) prior)
        target (get-in model [:variables :fog/target] (:target s))
        valid? (unit? target)
        low (when valid? (max 0.0 (- target (:hysteresis s))))
        high (when valid? (min 1.0 (+ target (:hysteresis s))))
        level (:fog-level observation)
        fresh? (and (= epoch (:epoch observation)) (unit? level)
                    (recent? now (:last-presented-ms observation)))
        zone (when (and fresh? valid?) (cond (and (pos? target) (<= level low)) :below (>= level high) :above :else :in-band))
        peak (:coasting-peak observation)
        coasting? (and fresh? valid? (< level high) (>= (:fog-level peak 0.0) high))
        request? (and fresh? valid? (not coasting?) (pos? target) (or (= zone :below)
                     (and (= zone :in-band) (:request? previous) (< level high))))
        enabled? (:enabled? s)
        sources (into {} (map (juxt :fixture-id identity)) (:sources observation))
        pumps (into {} (for [configured (:fog-fixture-ids s)
                             :let [index (:by-id patch)
                                   id (if (contains? index configured) configured
                                        (let [alias (if (keyword? configured) (subs (str configured) 1) (keyword configured))]
                                          (if (contains? index alias) alias configured)))]]
                         [id (pump (get-in patch [:by-id id]) (get sources id) (prior-pump previous id)
                                   now (and enabled? request?) (and enabled? fresh? valid? (not bypass?)))]))]
    {:epoch epoch :enabled? enabled? :target target :level level :lower low :upper high :zone zone
     :level-change-per-s (:fog-level-change-per-s observation)
     :clearance-half-life-s (or (:clearance-half-life-s observation)
                               (get-in model [:rig :room :measured-fog :clearance-half-life-s]))
     :coasting? (boolean coasting?) :coasting-peak peak
     :fresh? (boolean fresh?) :request? (boolean (and request? enabled? (not bypass?))) :pumps pumps}))

(defn publish [model previous status]
  (let [{:keys [target level zone enabled?]} status
        changed (cond-> []
                  (and (number? level) zone (not= zone (:zone previous)))
                  (conj {:type (case zone :below :fog/below-target :above :fog/above-target :fog/in-band)})
                  (not= target (:target previous)) (conj {:type :fog/target-changed})
                  (not= (boolean (:request? previous)) (:request? status))
                  (conj {:type (if (:request? status) :fog/demand-started :fog/demand-ended)}))]
    (-> model
        (update :variables assoc
                :fog/level level :fog/target target
                :fog/level-change-per-s (:level-change-per-s status)
                :fog/clearance-half-life-s (:clearance-half-life-s status))
        (staging/collect :atmosphere {:kind :venue :id :fog-control}
                         (mapv #(assoc % :level level :target target :zone zone :enabled? enabled?) changed)))))

(defn automatic-ids [status]
  (when (:enabled? status) (set (keys (:pumps status)))))

(defn- batch-fog? [id batch]
  (and batch (pos? (:opacity batch 1))
       (or (contains? (:outputs batch) :fog)
           (some #(= :fog (:attribute %)) (get-in batch [:field-request :group :cpu-plan :outputs])))
       (some #{id} (:target-ids batch))))

(defn- manual-fog? [id {:keys [assignments batch native-layer]}]
  (and (pos? (:opacity native-layer 1)) (pos? (:member-opacity native-layer 1))
       (or (some #(and (= :fog (:attribute %)) (= id (:target-id %)) (pos? (:opacity % 1))) assignments)
           (batch-fog? id batch)
           (and (some #(= :fog (:attribute %)) (:transforms native-layer))
                (or (= :previous (:targets native-layer)) (some #{id} (:target-ids native-layer)))))))

(defn arbitrate [status blocks]
  (if-not (:enabled? status) status
    (update status :pumps #(into {} (for [[id pump] %]
                                      [id (if (some (partial manual-fog? id) blocks)
                                            (assoc pump :active? false :mode :manual :demand 0.0) pump)])))))

(defn demands [status]
  (into [] (for [[id {:keys [active? demand until-ms off-bytes]}] (:pumps status)
                 :when active?]
             {:target-id id :attribute :fog :value demand :blend :replace
              :until-ms until-ms :off-bytes off-bytes})))

(defn with-demands [blocks assignments]
  (cond-> blocks (seq assignments) (conj {:kind :assignments :assignments assignments})))

(defn music-pulses
  "Apply each profiled device's duty cycle and leases to music pulses. A
  conventional fog channel without a flow profile keeps the music policy's
  existing pulse. Manual cues take precedence on both kinds of device."
  [patch observation previous control block blocks now bypass?]
  (let [reserved (automatic-ids control)
        venue-hold? (and (seq reserved)
                         (or (:coasting? control) (= :above (:zone control)) (not (:fresh? control))
                             (not (and (unit? (:target control)) (pos? (:target control))))))
        assignments (when-not (or bypass? venue-hold?)
                      (remove #(contains? reserved (:target-id %)) (:assignments block)))
        requested (into {} (map (juxt :target-id identity)) assignments)
        sources (into {} (map (juxt :fixture-id identity)) (:sources observation))
        previous-pumps (:music-pumps previous)
        ids (into (set (keys requested))
                  (keep (fn [[id p]] (when (or (:active? p) (> (:rest-until-ms p 0) now)) id))) previous-pumps)
        pumps (into {} (for [id ids
                             :let [fixture (get-in patch [:by-id id])] :when (:fogger fixture)
                             :let [musical (get requested id)
                                   duty (get-in fixture [:fogger :duty] {})
                                   limited (if (valid-duty? duty)
                                             (assoc-in fixture [:fogger :duty :max-level]
                                                       (min (:value musical 0.5) (:max-level duty 0.5)))
                                             fixture)
                                   p (pump limited (get sources id)
                                           (prior-pump previous id) now
                                           (some? musical) (not bypass?))]]
                         [id (if (some (partial manual-fog? id) blocks)
                               (assoc p :active? false :mode :manual) p)]))
        managed (demands {:pumps pumps})
        ordinary (remove #(or (get-in patch [:by-id (:target-id %) :fogger])
                              (some (partial manual-fog? (:target-id %)) blocks)) assignments)]
    {:pumps pumps :leases managed
     :block (when (or (seq managed) (seq ordinary))
              {:kind :assignments :assignments (into managed ordinary)})}))

(defn with-forecast [plan observation]
  (if (and plan observation)
    (assoc observation :coasting-peak (atmosphere/coasting-peak plan observation))
    observation))

(defn frame [model patch observation previous now bypass? blocks music-block]
  (when (or observation previous music-block (get-in model [:settings :fog-control :enabled?])
            (get-in model [:rig :room :measured-fog]))
    (let [prior (when (= (:epoch model) (:epoch previous)) previous)
          control (arbitrate (advance model patch observation prior now bypass?) blocks)
          feedback (when (:request? control) (demands control))
          music (when (or music-block (seq (:music-pumps prior)))
                  (music-pulses patch observation prior control music-block blocks now bypass?))]
      {:status (cond-> control (seq (:pumps music)) (assoc :music-pumps (:pumps music)))
       :feedback feedback :music (:block music) :leases (into (vec feedback) (:leases music))})))

(defn view [status]
  (when status
    (let [summarize #(update-vals % (fn [p] (select-keys p [:active? :mode :until-ms :rest-until-ms :flowing?])))]
      (cond-> (update status :pumps summarize) (:music-pumps status) (update :music-pumps summarize)))))
