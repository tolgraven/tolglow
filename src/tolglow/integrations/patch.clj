(ns tolglow.integrations.patch
  "Pure bounded musical signal routing. Observations and clocks are supplied as
  data; the returned commands still pass through the shared control reducer.
  No LiveAPI access, global registrations, clocks, devices or hidden state."
  (:require [afterglow.time :as time]
            [tolglow.composition.envelope :as envelope]
            [tolglow.composition.tween :as tween]))

(defn- fail! [code message] (throw (ex-info message {:patch/error code})))
(defn- id? [x] (or (keyword? x) (and (string? x) (<= 1 (count x) 128))))
(defn- finite? [x] (time/finite-number? x))
(defn- bounds? [x] (and (vector? x) (= 2 (count x)) (every? finite? x) (< (first x) (second x))))
(defn- clamp [x low high] (max low (min high x)))
(defn- only-keys? [m allowed] (and (map? m) (every? allowed (keys m))))
(defn- duration! [duration]
  (when (seq (time/validate-duration duration)) (fail! :invalid-spec "Invalid musical/explicit millisecond duration"))
  duration)

(defn validate-patch
  "Return canonical v1 routing data. Named sources feed routes, then named
  destinations; there are no output-to-input edges or implicit feedback cycles.
  Multiple routes share a destination's explicit combine rule and bounds."
  [input]
  (when-not (and (only-keys? input #{:version :id :revision :sources :destinations :routes})
                 (= 1 (:version input)) (id? (:id input)) (integer? (:revision input)) (pos? (:revision input))
                 (map? (:sources input)) (<= 1 (count (:sources input)) 128)
                 (map? (:destinations input)) (<= 1 (count (:destinations input)) 128)
                 (vector? (:routes input)) (<= 1 (count (:routes input)) 512))
    (fail! :invalid-spec "Patch requires version, identity, revision and bounded source/destination/routes collections"))
  (let [sources
        (into {} (map (fn [[id value]]
                        (when-not (and (id? id) (only-keys? value #{:kind :range :timeout :min-confidence})
                                       (#{:midi :automation :audio :control} (:kind value)) (bounds? (:range value)))
                          (fail! :invalid-spec "Source needs a kind and calibrated finite range"))
                        (let [value (merge {:timeout {:beats 4} :min-confidence 0.0} value)]
                          (duration! (:timeout value))
                          (when-not (and (finite? (:min-confidence value)) (<= 0 (:min-confidence value) 1)
                                         (pos? (val (first (:timeout value)))))
                            (fail! :invalid-spec "Source timeout must be positive and confidence normalized"))
                          [id value])) (:sources input)))
        destinations
        (into {} (map (fn [[id value]]
                        (when-not (and (id? id) (only-keys? value #{:cue-id :param :range :base :combine})
                                       (id? (:cue-id value)) (id? (:param value)) (bounds? (:range value))
                                       (#{:max :min :sum :mean :product :priority} (:combine value)))
                          (fail! :invalid-spec "Destination needs logical cue/parameter, bounds and explicit combination"))
                        (let [[low high] (:range value) value (merge {:base low} value)]
                          (when-not (and (finite? (:base value)) (<= low (:base value) high))
                            (fail! :invalid-spec "Destination base must be inside its bounds"))
                          [id value])) (:destinations input)))
        routes
        (mapv (fn [route]
                (when-not (and (only-keys? route #{:id :source :destination :source-range :range :curve :invert? :steps
                                                  :priority :envelope :smoothing})
                               (id? (:id route)) (contains? sources (:source route)) (contains? destinations (:destination route)))
                  (fail! :invalid-spec "Route needs unique identity and declared source/destination"))
                (let [route (merge {:source-range (get-in sources [(:source route) :range])
                                   :range (get-in destinations [(:destination route) :range]) :curve :linear
                                   :invert? false :priority 0} route)
                      [low high] (get-in destinations [(:destination route) :range]) [rlo rhi] (:range route)]
                  (when-not (and (bounds? (:source-range route)) (bounds? (:range route)) (<= low rlo rhi high)
                                 (boolean? (:invert? route)) (integer? (:priority route))
                                 (or (nil? (:steps route)) (and (integer? (:steps route)) (<= 2 (:steps route) 128))))
                    (fail! :invalid-spec "Invalid mapping range, inversion, priority or quantization"))
                  (tween/curve (:curve route) 0.5)
                  (when (:envelope route)
                    (when-not (only-keys? (:envelope route) #{:attack :decay :sustain :release :curve})
                      (fail! :invalid-spec "Envelope uses attack/decay/release duration maps, sustain and curve"))
                    (envelope/normalize (:envelope route)))
                  (when (:smoothing route) (duration! (:smoothing route)))
                  route)) (:routes input))]
    (when-not (= (count routes) (count (set (map :id routes)))) (fail! :invalid-spec "Route IDs must be unique"))
    (assoc input :sources sources :destinations destinations :routes routes)))

(defn- checked-clock [clock]
  (let [clock (time/frame-clock clock)]
    (when-not (every? #(and (finite? %) (<= 0 %)) ((juxt :time-ms :beat) clock))
      (fail! :invalid-clock "Patch clocks require explicit nonnegative milliseconds and musical beat"))
    (assoc clock :meter (time/normalize-meter (:meter clock)))))

(defn new-state
  "Start a fresh patch at an explicit clock/show epoch. Restore captured shows
  through restore-state, and reconcile transport seeks with rebase-clock."
  [spec clock epoch]
  (let [spec (validate-patch spec) clock (checked-clock clock)]
    (when-not (and (integer? epoch) (<= 0 epoch)) (fail! :invalid-epoch "Patch epoch must be a nonnegative integer"))
    {:version 1 :patch-id (:id spec) :revision (:revision spec) :epoch epoch :frame 0
     :clock clock :sources {} :routes {} :receipts [] :last-values {}}))

(defn- historical-clock? [clock current]
  (and (map? clock) (every? finite? ((juxt :time-ms :beat) clock))
       (<= (:time-ms clock) (:time-ms current)) (<= (:beat clock) (:beat current))
       (try (time/normalize-meter (:meter clock)) true (catch Exception _ false))))
(defn- portable? [value depth]
  (and (< depth 24)
       (cond
         (or (nil? value) (boolean? value) (keyword? value)) true
         (string? value) (<= (count value) 4096)
         (number? value) (finite? value)
         (map? value) (and (<= (count value) 8192) (every? #(and (portable? (key %) (inc depth)) (portable? (val %) (inc depth))) value))
         (vector? value) (and (<= (count value) 8192) (every? #(portable? % (inc depth)) value))
         :else false)))

(defn validate-state
  "Validate portable captured routing state against its exact patch revision.
  Historical coordinates may be negative after a backwards transport rebase."
  [input-spec state]
  (let [spec (validate-patch input-spec) current (checked-clock (:clock state))
        routes (into {} (map (juxt :id identity) (:routes spec)))]
    (when-not (and (portable? state 0) (only-keys? state #{:version :patch-id :revision :epoch :frame :clock :sources :routes :receipts :last-values})
                   (= 1 (:version state)) (= (:id spec) (:patch-id state)) (= (:revision spec) (:revision state))
                   (every? #(and (integer? %) (<= 0 %)) ((juxt :epoch :frame) state))
                   (map? (:sources state)) (every? #(contains? (:sources spec) %) (keys (:sources state)))
                   (map? (:routes state)) (every? #(contains? routes %) (keys (:routes state)))
                   (vector? (:receipts state)) (<= (count (:receipts state)) 512)
                   (= (count (:receipts state)) (count (set (:receipts state))))
                   (every? #(and (vector? %) (= 2 (count %)) (contains? (:sources spec) (first %)) (id? (second %))) (:receipts state))
                   (map? (:last-values state)) (<= (count (:last-values state)) 4096))
      (fail! :invalid-state "Captured patch state does not match the declared patch or bounds"))
    (doseq [[id observation] (:sources state)]
      (when-not (and (= id (:source-id observation)) (id? (:event-id observation))
                     (= (:epoch state) (:epoch observation))
                     (finite? (:value observation)) (boolean? (:gate? observation))
                     (finite? (:confidence observation)) (<= 0 (:confidence observation) 1)
                     (historical-clock? (:clock observation) current))
        (fail! :invalid-state "Invalid captured source observation")))
    (doseq [[id route-state] (:routes state)]
      (let [route (get routes id) captured (:envelope route-state)]
        (when-not (and (only-keys? route-state #{:gate? :held-value :envelope :smoothed :clock :instance-id})
                       (boolean? (:gate? route-state))
                       (every? #(and (finite? %) (<= 0 % 1)) ((juxt :held-value :smoothed) route-state))
                       (or (nil? (:instance-id route-state)) (id? (:instance-id route-state)))
                       (historical-clock? (:clock route-state) current)
                       (or (nil? captured)
                           (and (:envelope route)
                                (only-keys? captured #{:initial-level :started-at-ms :started-at-beat :timing-meter
                                                      :released-at-ms :released-at-beat :release-level})
                                (try (envelope/sample (:envelope route) captured current) true (catch Exception _ false)))))
          (fail! :invalid-state "Invalid captured mapping/envelope state"))))
    (doseq [[key value] (:last-values state)]
      (let [[destination instance] (when (vector? key) key)
            [low high] (get-in spec [:destinations destination :range])]
        (when-not (and (vector? key) (= 2 (count key)) (id? instance) low (finite? value) (<= low value high))
          (fail! :invalid-state "Invalid captured destination value"))))
    state))

(defn restore-state
  "Restore progress without replaying observations/commands. Remap captured
  activation IDs using the host's old->new map; orphan gates remain detached
  until a new gate-on. Receipt identities and smoothing progress are retained."
  [spec state {:keys [epoch instance-ids]}]
  (validate-state spec state)
  (when-not (and (integer? epoch) (<= 0 epoch) (map? instance-ids)
                 (every? #(and (id? (key %)) (id? (val %))) instance-ids))
    (fail! :invalid-restore "Restore needs the new epoch and explicit activation ID mapping"))
  (-> state
      (assoc :epoch epoch)
      (update :sources #(into {} (map (fn [[id observation]] [id (assoc observation :epoch epoch)]) %)))
      (update :routes #(into {} (map (fn [[id captured]]
                                     [id (let [instance (get instance-ids (:instance-id captured))]
                                           (cond-> (assoc captured :instance-id instance)
                                             (nil? instance) (assoc :envelope nil :smoothed 0.0)))]) %)))
      (update :last-values #(into {} (keep (fn [[[destination instance] value]]
                                           (when-let [new-id (get instance-ids instance)] [[destination new-id] value])) %)))))

(defn rebase-clock
  "Shift musical anchors to a supplied clock without advancing beat envelopes,
  smoothing or timeouts. Millisecond anchors and physical observed-at-ms metadata
  are unchanged; wall time remains monotonic during a musical seek."
  [state input-clock]
  (let [next-clock (checked-clock input-clock)
        from-beat (get input-clock :from-beat (get-in state [:clock :beat]))
        _ (when-not (finite? from-beat) (fail! :invalid-clock "Rebase source beat must be finite"))
        beat-delta (- (:beat next-clock) from-beat)
        _ (when (< (:time-ms next-clock) (get-in state [:clock :time-ms]))
            (fail! :clock-rewind "Musical rebasing cannot rewind the wall clock"))
        shift-clock #(update % :beat + beat-delta)
        shift-envelope (fn [captured]
                         (when captured
                           (reduce (fn [m [key delta]] (cond-> m (contains? m key) (update key + delta))) captured
                                   [[:started-at-beat beat-delta] [:released-at-beat beat-delta]])))]
    (-> state (assoc :clock next-clock)
        (update :sources #(into {} (map (fn [[id observation]] [id (update observation :clock shift-clock)]) %)))
        (update :routes #(into {} (map (fn [[id captured]] [id (-> captured (update :clock shift-clock)
                                                                                  (update :envelope shift-envelope))]) %))))))

(defn- observation-error [spec state clock observation]
  (let [source (get-in spec [:sources (:source-id observation)]) c (:clock observation)
        last-clock (get-in state [:sources (:source-id observation) :clock])]
    (cond
      (nil? source) :unknown-source
      (not= (:epoch state) (:epoch observation)) :stale-epoch
      (not (id? (:event-id observation))) :invalid-event-id
      (not (and (finite? (:value observation)) (boolean? (:gate? observation))
                 (finite? (:confidence observation)) (<= 0 (:confidence observation) 1)
                 (finite? (:beat c)) (finite? (:time-ms c)) (<= 0 (:time-ms c)))) :invalid-sample
      (> (:time-ms c) (:time-ms clock)) :future-sample
      (> (:beat c) (:beat clock)) :future-sample
      (and last-clock (< (:time-ms c) (:time-ms last-clock))) :out-of-order
      (#{:patch :tolglow-feedback} (:origin observation)) :feedback-suppressed
      (some #{[(:source-id observation) (:event-id observation)]} (:receipts state)) :duplicate
      :else nil)))

(declare sample-route)
(defn- accept-observations [spec state clock observations bindings active-instances]
  (reduce (fn [{:keys [state diagnostics]} observation]
            (if-let [error (observation-error spec state clock observation)]
              {:state state :diagnostics (conj diagnostics {:code error :source-id (:source-id observation) :event-id (:event-id observation)})}
              (let [observed (assoc (time/frame-clock (:clock observation)) :meter (time/normalize-meter (get-in observation [:clock :meter])))
                    ;; A late callback starts/releases at the next available
                    ;; owner coordinate; it cannot rewind an already sampled
                    ;; envelope. Its original timestamp still governs timeout.
                    effective (-> observed (update :time-ms max (get-in state [:clock :time-ms]))
                                  (update :beat max (get-in state [:clock :beat])))
                    next-state (-> state
                                   (assoc-in [:sources (:source-id observation)] (assoc observation :clock observed))
                                   (update :receipts #(vec (take-last 512 (conj % [(:source-id observation) (:event-id observation)])))))
                    next-state (reduce (fn [state route]
                                         (assoc-in state [:routes (:id route)]
                                                   (:state (sample-route spec state route effective bindings active-instances))))
                                       next-state (filter #(= (:source-id observation) (:source %)) (:routes spec)))]
                {:state next-state
                 :diagnostics (cond-> diagnostics (not= observed effective)
                                (conj {:code :late-sample-clamped :source-id (:source-id observation)
                                       :observed-clock observed :effective-clock effective}))})))
          {:state state :diagnostics []} observations))

(defn- fraction [route value]
  (let [[low high] (:source-range route) value (clamp (/ (- value low) (- high low)) 0.0 1.0)
        value (if (:invert? route) (- 1.0 value) value)
        value (tween/curve (:curve route) value)]
    (if-let [steps (:steps route)] (/ (double (Math/round (* value (dec steps)))) (dec steps)) value)))

(defn- fresh? [source observation clock]
  (and observation (>= (:confidence observation) (:min-confidence source))
       (not (:done? (time/sample (time/start (:timeout source) (:clock observation)) clock)))))

(defn- smooth [route previous target clock old-clock]
  (if (and (:smoothing route) (some? previous))
    (let [{:keys [unit value]} (time/normalize-duration (:smoothing route) (:meter old-clock))
          elapsed (- (time/coordinate clock unit) (time/coordinate old-clock unit))
          alpha (if (zero? value) 1.0 (- 1.0 (Math/exp (- (/ elapsed (double value))))))
          result (+ previous (* alpha (- target previous)))]
      (if (< (Math/abs (double result)) 1.0e-9) 0.0 result))
    target))

(defn- sample-route [spec state route clock bindings active-instances]
  (let [old (get-in state [:routes (:id route)]) observation (get-in state [:sources (:source route)])
        source (get-in spec [:sources (:source route)]) destination (get-in spec [:destinations (:destination route)])
        gate? (boolean (and (fresh? source observation clock) (:gate? observation)))
        rising? (and gate? (not (:gate? old))) falling? (and (not gate?) (:gate? old))
        env (:envelope route)
        captured (cond rising? (envelope/initial-state clock)
                       (and env falling? (:envelope old)) (envelope/release env (:envelope old) clock)
                       :else (:envelope old))
        captured (if env captured nil)
        level (if captured (:value (envelope/sample env captured clock)) (if gate? 1.0 0.0))
        done? (and captured (:done? (envelope/sample env captured clock)))
        held-value (if gate? (fraction route (:value observation)) (get old :held-value 0.0))
        target (* held-value level)
        smoothed (smooth route (:smoothed old) target clock (get old :clock clock))
        binding (get bindings (:cue-id destination))
        instance (if env (if rising? binding (:instance-id old)) (or binding (:instance-id old)))
        active? (and instance (contains? active-instances instance)
                     (or gate? (and captured (not done?)) (> smoothed 1.0e-9)))
        [low high] (:range route)
        value (+ low (* (clamp smoothed 0.0 1.0) (- high low)))
        next-state {:gate? gate? :held-value held-value :envelope captured :smoothed smoothed :clock clock :instance-id instance}
        contribution (when active? {:route-id (:id route) :source-id (:source route) :destination (:destination route)
                                     :instance-id instance :value value :priority (:priority route) :origin (:origin observation)})]
    {:state next-state :contribution contribution
     :diagnostic (when (and instance (not (contains? active-instances instance)) (or gate? captured))
                   {:code :missing-captured-instance :route-id (:id route) :instance-id instance})}))

(defn- combine-values [destination contributions]
  (let [values (map :value contributions) [low high] (:range destination)
        combined (if (empty? contributions) (:base destination)
                     (case (:combine destination)
                       :max (apply max values) :min (apply min values) :sum (reduce + values)
                       :mean (/ (reduce + values) (count values)) :product (reduce * values)
                       :priority (:value (first (sort-by (juxt (comp - :priority) (comp str :route-id)) contributions)))))]
    (clamp combined low high)))

(defn step
  "Advance a patch using captured data and return {:state :commands :values
  :diagnostics}. Inputs: :clock, :epoch, up to 1024 :observations, explicit
  :bindings {cue-id instance-id}, :active-instances set and shared :envelope.
  Observations contain source-id/event-id/epoch/clock/value/gate?/confidence and
  origin. Unknown, stale, future, duplicate and feedback samples are diagnosed.
  Changed values become param/set commands with deterministic child IDs.

  ADSR captures its activation ID at gate-on. A replacement cue never inherits
  an old gate or release tail. Musical progress pauses when beat is unchanged."
  [input-spec state {:keys [clock epoch observations bindings active-instances envelope] :or {observations []} :as frame}]
  (let [spec (validate-patch input-spec) clock (checked-clock clock)]
    (when-not (and (= 1 (:version state)) (= (:id spec) (:patch-id state)) (= (:revision spec) (:revision state))
                   (= epoch (:epoch state)) (map? bindings) (set? active-instances)
                   (vector? observations) (<= (count observations) 1024)
                   (map? envelope) (id? (:command-id envelope)) (= epoch (:epoch envelope)) (:show-id envelope))
      (fail! :invalid-frame "Patch requires matching captured revision/epoch, bindings, bounded observations and control envelope"))
    (when (or (< (:time-ms clock) (get-in state [:clock :time-ms])) (< (:beat clock) (get-in state [:clock :beat])))
      (fail! :clock-rewind "Reset captured routing state explicitly after a backwards clock seek"))
    (let [{state :state diagnostics :diagnostics} (accept-observations spec state clock observations bindings active-instances)
          sampled (mapv (fn [route] [route (sample-route spec state route clock bindings active-instances)]) (:routes spec))
          state (reduce (fn [s [route result]] (assoc-in s [:routes (:id route)] (:state result))) state sampled)
          contributions (keep (comp :contribution second) sampled)
          ;; Different captured activations are never mixed into one destination
          ;; write. A release tail may still address an older active instance.
          groups (group-by (juxt :destination :instance-id) contributions)
          old-keys (keys (:last-values state))
          keys (sort-by pr-str (set (concat old-keys (keys groups))))
          values (into {} (for [[destination-id instance :as key] keys
                               :when (contains? active-instances instance)
                               :let [destination (get-in spec [:destinations destination-id])
                                     value (combine-values destination (get groups key []))]]
                           [key value]))
          commands (mapv (fn [[[destination-id instance :as key] value]]
                           (let [destination (get-in spec [:destinations destination-id]) members (get groups key)]
                             (merge envelope {:command-id (str (:command-id envelope) "/patch/" (:id spec) "/" (:frame state) "/" destination-id "/" instance)
                                              :time-ms (:time-ms clock) :op :param/set :instance-id instance :param (:param destination) :value value
                                              :source {:type :patch :patch-id (:id spec) :origin (if (some #(= :live-playback (:origin %)) members) :live-playback :patch)
                                                       :route-ids (mapv :route-id members)}})))
                         (filter (fn [[key value]] (not= value (get-in state [:last-values key]))) (sort-by (comp pr-str key) values)))]
      {:state (-> state (assoc :clock clock :last-values values) (update :frame inc))
       :commands commands :values values
       :diagnostics (into diagnostics (keep (comp :diagnostic second) sampled))})))
