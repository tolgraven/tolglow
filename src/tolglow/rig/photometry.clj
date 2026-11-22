(ns tolglow.rig.photometry
  "One-metre relative white-light calibration. The show owns an expiring output
  override. Only the physical publisher can confirm a phase for measurement."
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [tolglow.rig.calibration :as calibration]))

(def session-path [:extensions ::session])
(defn current [model] (get-in model session-path))
(defn disarm [model]
  (if (current model) (update model :extensions dissoc ::session) model))

(def ^:private visible #{:red :green :blue :white :amber :lime :cyan :warm-white :cold-white :cool-white})
(def ^:private controlled (into visible [:dimmer :uv :strobe]))
(defn- can-light? [{:keys [channels]}]
  (some #(contains? channels %) (conj visible :dimmer)))
(defn- type-key [fixture]
  (if-let [t (:fixture-type-id fixture)] [:type t] [:fixture (:id fixture)]))
(defn fixtures [model]
  (mapv (fn [f] {:id (:id f) :name (:name f) :type (second (type-key f))})
        (filter can-light? (get-in model [:rig :fixtures]))))
(defn- check [ok message] (when-not ok (throw (ex-info message {:component :photometry}))))
(defn- valid-rig? [model session]
  (and (= (:epoch model) (:epoch session)) (= (:rig model) (:base-rig session))))
(defn- usable? [model session now]
  (and session (valid-rig? model session) (< now (:lease-until session))))
(defn- latest-phase? [session now]
  (when-let [t (:presented-at session)] (<= 0 (- now t) 1500)))

(defn advance [model receipts now]
  (if-let [s (current model)]
    (if (usable? model s now)
      (let [next-session
            (reduce (fn [s {:keys [epoch calibration eligible? published-at-ms dropped-before]}]
                      (if (and (= epoch (:epoch s)) (= :photometry (:procedure calibration))
                               (= (:session-id s) (:session-id calibration))
                               (= (:step-index s) (:step-index calibration)))
                        (let [key [(:fixture-id s) (:phase s)]]
                          (if (and eligible? (zero? (or dropped-before 0)))
                            (-> s (assoc :presented-at published-at-ms) (update :confirmed conj key))
                            (-> s (dissoc :presented-at) (update :confirmed disj key))))
                        s)) s receipts)]
        (if (identical? s next-session) model (assoc-in model session-path next-session)))
      (disarm model))
    model))

(defn- phase-output [rig active phase]
  (let [fixtures (filterv can-light? (:fixtures rig))
        selected? #(and (= phase :white) (= active (:id %)))
        assignments (vec (for [f fixtures
                               attr (keys (:channels f)) :when (controlled attr)]
                           {:target-id (:id f) :attribute attr
                            :value (if (and (selected? f) (or (= :dimmer attr) (visible attr))) 1.0 0.0)}))
        patch (frame/compile-patch fixtures)
        reference (frame/resolve-frame patch assignments {:include-values? false})
        slots (for [f (:fixtures patch) [attr spec] (:compiled-channels f)
                    :when (controlled attr) slot (keep spec [:coarse :fine])] slot)]
    {:block {:kind :assignments :assignments assignments}
     :expected-bytes (into {} (map (fn [[u i :as slot]]
                                     [slot (bit-and 255 (aget ^bytes (get-in reference [:universes u]) i))])) slots)}))

(defn marker [session]
  (when (:phase session)
    (assoc (select-keys session [:session-id :step-index :expected-bytes]) :procedure :photometry)))

(defn- start [model {:keys [session-id fixture-ids armed?]} now capable?]
  (let [ids (calibration/resolve-fixture-ids model fixture-ids)
        index (into {} (map (juxt :id identity)) (get-in model [:rig :fixtures]))]
    (check (and capable? (true? armed?) (not (current model)) (not (:armed? (calibration/current model))))
           "Arm physical output explicitly and finish any other calibration first")
    (check (and (string? session-id) (<= 1 (count session-id) 128) (vector? fixture-ids)
                (<= 1 (count ids) 128) (every? #(and (index %) (can-light? (index %))) ids)
                (apply distinct? (map #(type-key (index %)) ids)))
           "Select one light-emitting representative of each fixture type in reference order")
    (check (and (== 1 (:master model)) (not (:blackout? model)))
           "Set the master to full and release blackout before measuring white output")
    (assoc-in model session-path {:session-id session-id :epoch (:epoch model) :base-rig (:rig model)
                                  :ids ids :confirmed #{} :step-index 0 :lease-until (+ now 15000)})))

(defn- level? [x] (and (number? x) (Double/isFinite (double x)) (<= 0 x 1)))
(defn- apply-readings [model session {:keys [distance-m readings]}]
  (let [ids (:ids session)
        fixture-ids (calibration/resolve-fixture-ids model (mapv :fixture-id readings))
        index (into {} (map (juxt :id identity)) (get-in model [:rig :fixtures]))]
    (check (and (number? distance-m) (== distance-m 1) (vector? readings) (= ids fixture-ids)
                (every? (:confirmed session) (for [id ids phase [:dark :white]] [id phase])))
           "A complete set of confirmed dark and white readings at one metre is required in reference order")
    (let [levels (mapv (fn [{:keys [white dark]}]
                         (check (and (level? white) (level? dark) (< dark white 1.0))
                                "Use unsaturated linear readings with a positive background-corrected signal")
                         (- (double white) (double dark))) readings)
          ratios (into {} (map (fn [id signal] [(type-key (index id)) (/ signal (first levels))]) ids levels))
          _ (check (every? #(Double/isFinite (double %)) (vals ratios)) "Out-of-range brightness ratio")
          publish (fn [m [kind id] ratio]
                    (if-let [d (when (= kind :type) (get-in m [:registry :fixtures id]))]
                      (let [d (-> d (update :version inc) (assoc :measured-relative-brightness ratio))
                            result (control/apply-command m {:op :registry/publish :kind :fixtures :definition d
                                                             :show-id (:show-id m) :epoch (:epoch m) :time-ms (:time-ms m)
                                                             :command-id (str "photometry/" (:epoch m) "/" (:revision m) "/" (:session-id session)) :source {:adapter :photometry}})]
                        (check (= :accepted (get-in result [:ack :status])) "Fixture type publication failed")
                        (:model result))
                      m))
          published (reduce-kv publish model ratios)]
      (-> published disarm
          (update-in [:rig :fixtures]
                     #(mapv (fn [f] (if-let [ratio (ratios (type-key f))]
                                      (assoc f :measured-relative-brightness ratio) f)) %))))))

(defn apply-operation [model {:keys [op session-id fixture-id phase] :as payload} now capable?]
  (let [s (current model)
        _ (when-not (= :start op)
            (check (and (usable? model s now) (= (:session-id s) session-id))
                   "Calibration lease, rig or session changed"))
        updated (case op
                  :start (start model payload now capable?)
                  :cancel (disarm model)
                  :heartbeat (assoc-in model (conj session-path :lease-until) (+ now 15000))
                  :step (let [id (first (calibration/resolve-fixture-ids model [fixture-id]))
                              phase (when (or (keyword? phase) (string? phase)) (keyword phase))]
                          (check (and (some #{id} (:ids s)) (#{:dark :white} phase)) "Unknown fixture or phase")
                          (assoc-in model session-path
                                    (-> s (merge (phase-output (:rig model) id phase))
                                        (assoc :fixture-id id :phase phase :lease-until (+ now 15000))
                                        (update :step-index inc) (update :confirmed disj [id phase])
                                        (dissoc :presented-at))))
                  :apply (apply-readings model s payload)
                  (throw (ex-info "Unknown photometry operation" {:op op})))
        state (current updated)]
    {:model (cond-> updated (#{:start :step :apply :cancel} op) (update :revision inc))
     :ack (merge {:status :accepted :session-id session-id :presented? (boolean (latest-phase? state now))}
                 (select-keys state [:fixture-id :phase]))}))
