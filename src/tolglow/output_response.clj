(ns tolglow.output-response
  "Group and global attack/release response of composed light, before budget
  allocation and metering. The show owner supplies time, previous history and
  the rendered frame; no clocks, devices or effects are invoked here."
  (:require [afterglow.render.emission :as emission]
            [afterglow.time :as time]
            [tolglow.lighting-scenes :as scenes]))

(def defaults {:enabled? false :global {} :groups {}})
(defn- id? [x] (or (string? x) (keyword? x)))
(defn- magnitude? [x] (and (time/finite-number? x) (not (neg? x))))

(defn- duration-binding? [binding]
  (and (map? binding) (magnitude? (:default binding 0))
       (or (and (every? #{:variable :default} (keys binding))
                (vector? (:variable binding)) (seq (:variable binding)) (every? id? (:variable binding)))
           (and (every? #{:cue-id :param :default} (keys binding))
                (id? (:cue-id binding)) (id? (:param binding))))))

(defn- duration? [d]
  (and (map? d) (= 1 (count d)) (time/units (ffirst d))
       (let [x (val (first d))] (or (magnitude? x) (duration-binding? x)))))

(defn- envelope? [e]
  (and (map? e) (every? #{:attack :release} (keys e)) (every? duration? (vals e))))

(defn settings [opts]
  (let [{:keys [enabled? global groups] :as s} (merge defaults opts)]
    (when-not (and (map? opts) (every? (set (keys defaults)) (keys opts))
                   (boolean? enabled?) (envelope? global)
                   (map? groups) (every? id? (keys groups)) (every? envelope? (vals groups)))
      (throw (ex-info "Output response requires global or named group attack/release durations" {:values opts})))
    s))

(defn- magnitude [model binding]
  (if (number? binding) binding
    (let [v (if-let [path (:variable binding)]
              (get-in (:variables model) path)
              (when-let [instance (last (sort-by :order
                                                 (filter #(and (= (:cue-id binding) (:cue-id %))
                                                               (= :running (:phase %))) (vals (:instances model)))))]
                (get (:params instance) (:param binding)
                     (get-in instance [:definition :params (:param binding) :default]))))]
      (if (magnitude? v) v (:default binding 0)))))

(defn- coefficient [model previous clock [unit source]]
  (let [value (magnitude model source)]
    (if (zero? value) 1.0
      (let [{:keys [unit value]} (time/normalize-duration {unit value} (:meter clock))
            elapsed (max 0.0 (- (time/coordinate clock unit) (time/coordinate previous unit)))]
        (- (Math/expm1 (- (/ elapsed value))))))))

(defn- group-rates [configured model previous clock]
  (let [rates (fn [options] (update-vals options #(coefficient model previous clock (first %))))]
    {:global (rates (:global configured))
     :groups (update-vals (:groups configured) rates)}))

(defn- scope-layout [model patch groups cached]
  (when (seq groups)
    (let [fixtures (:fixtures patch)
          scenes (scenes/groups model)
          members (into {} (map (fn [id] [id (:fixture-ids (get scenes id))])) (keys groups))]
      (if (and (identical? fixtures (:fixtures cached)) (= members (:members cached))) cached
        (let [custom (reduce-kv (fn [m id fixture-ids]
                                  (reduce #(update %1 %2 (fnil conj #{}) id) m fixture-ids))
                                {} members)
              by-scope (reduce-kv
                        (fn [m i fixture]
                          (let [ids (into (get custom (:id fixture) #{}) (filter #(contains? groups %)) (:groups fixture))]
                            (update m ids (fnil conj []) i)))
                        {} fixtures)]
          {:fixtures fixtures :members members :indices (update-vals by-scope int-array)})))))

(defn- stage-pair [rates group-ids]
  ;; A local stage overrides the global stage. If groups overlap, use their
  ;; slowest specified stage independently for attack and release.
  (let [local (reduce (fn [a id] (merge-with min a (get-in rates [:groups id]))) {} group-ids)
        stages (merge (:global rates) local)]
    (if (seq stages) [(:attack stages 1.0) (:release stages 1.0)] [-1.0 -1.0])))

(defn- stage-coefficients [rates layout]
  (let [indices (:indices layout)]
    (if (<= (count indices) 1)
      (stage-pair rates (first (keys indices)))
      (let [n (count (:fixtures layout))
            attack (double-array n)
            release (double-array n)]
        (doseq [[ids ^ints rows] indices]
          (let [[rise fall] (stage-pair rates ids)]
            (dotimes [j (alength rows)]
              (let [i (aget rows j)]
                (aset-double attack i (double rise))
                (aset-double release i (double fall))))))
        [attack release]))))

(defn advance
  "Follow physical emitter output at the supplied show time. prior-frame is
  the previous committed output from the same patch when enabling a new scope
  mid-show. Hard blackout and the effective master take precedence over tails.
  Retain the returned runtime history only for the same rig and epoch."
  ([previous model patch frame bypass?]
   (advance previous model patch frame bypass? (:master model 1.0) nil))
  ([previous model patch frame bypass? ceiling prior-frame]
   (let [opts (get-in model [:settings :output-response])]
     (if (or (not (:enabled? opts)) (and (empty? (:global opts)) (empty? (:groups opts)))
             bypass? (:blackout? model) (zero? ceiling))
       {:state nil :frame frame}
       (let [clock (time/frame-clock model)
             reset? (and previous
                         (or (not= (:epoch previous) (:epoch model))
                             (< (:time-ms clock) (get-in previous [:clock :time-ms]))
                             (< (:beat clock) (get-in previous [:clock :beat]))))
             prior (when-not reset? previous)
             rates (group-rates opts model (:clock prior clock) clock)
             layout (scope-layout model patch (:groups opts) (:layout prior))
             coefficients (if (and (identical? layout (:layout prior)) (= rates (:rates prior)))
                            (:coefficients prior)
                            (stage-coefficients rates layout))
             [attack release] coefficients
             seed (when (and (not reset?) prior-frame (= (:target-order patch) (:target-order prior-frame)))
                    prior-frame)
             result (emission/follow-frame patch frame (:levels prior) seed attack release ceiling)]
         {:frame (:frame result)
          :state {:epoch (:epoch model) :clock clock :levels (:levels result)
                  :layout layout :rates rates :coefficients coefficients}})))))
