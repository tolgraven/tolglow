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

(defn- custom-memberships [model groups]
  (reduce-kv
   (fn [m group-id _]
     (reduce #(update %1 %2 (fnil conj []) group-id) m (get-in (scenes/groups model) [group-id :fixture-ids])))
   {} groups))

(defn- fixture-rates [rates group-ids]
  ;; A local stage overrides the corresponding global stage. When patched
  ;; groups overlap, their slowest specified stage wins independently.
  (let [local (reduce (fn [a id] (merge-with min a (get-in rates [:groups id]))) {} group-ids)]
    (merge (:global rates) local)))

(defn- follow [current target attack release ceiling]
  (reduce-kv
   (fn [m attr goal]
     (let [old (get current attr 0.0)
           a (if (> goal old) attack release)
           x (+ old (* a (- goal old)))
           value (min ceiling (if (< (abs (- x goal)) 1.0e-9) goal x))]
       (assoc m attr value)))
   {} target))

(defn advance
  "Return {:frame ... :state ...}. prior-frame is the previous committed output
  from the same patch when enabling a new scope mid-show. The ceiling is the effective operator
  master. Upper limits and hard blackout win over any remaining release tail.
  Each state contains portable channel drives and the last logical clock only."
  ([previous model patch frame bypass?]
   (advance previous model patch frame bypass? (:master model 1.0) nil))
  ([previous model patch frame bypass? ceiling prior-frame]
   (let [opts (get-in model [:settings :output-response])]
     (if (or (not (:enabled? opts)) bypass? (:blackout? model) (zero? ceiling))
       {:state nil :frame frame}
       (let [clock (time/frame-clock model)
             prior (when (= (:epoch previous) (:epoch model)) previous)
             clock-back? (and prior (or (< (:time-ms clock) (get-in prior [:clock :time-ms]))
                                         (< (:beat clock) (get-in prior [:clock :beat]))))
             prior (when-not clock-back? prior)
             rates (group-rates opts model (:clock prior clock) clock)
             members (custom-memberships model (:groups opts))
             ;; The caller supplies a prior packet only for the same compiled rig.
             seed? (and prior-frame (= (:target-order patch) (:target-order prior-frame)))
             [history changes]
             (reduce
              (fn [[history changes :as result] fixture]
                (let [id (:id fixture)
                      r (fixture-rates rates (concat (:groups fixture) (members id)))]
                  (if (empty? r) result
                    (if-let [target (emission/emitted-levels fixture frame)]
                      (let [old (or (get-in prior [:levels id])
                                    (when seed? (emission/emitted-levels fixture prior-frame)))
                            filtered (follow old target (:attack r 1.0) (:release r 1.0) ceiling)]
                        [(assoc history id filtered) (cond-> changes (not= target filtered) (assoc id filtered))])
                      result))))
              [{} {}] (:fixtures patch))]
         {:frame (emission/with-emitted-levels patch frame changes)
          :state {:epoch (:epoch model) :clock clock :levels history}})))))
