(ns tolglow.rig.fog-calibration
  "Acquire stationary optical observations without changing fixture output."
  (:require [afterglow.render.atmosphere.calibration :as optical]
            [tolglow.rig.calibration :as calibration]))

(def session-path [:extensions ::session])
(defn current [model] (get-in model session-path))
(defn disarm [model]
  (if (current model) (update model :extensions dissoc ::session) model))

(defn fixtures [model]
  (mapv #(select-keys % [:id :name]) (filter :fogger (get-in model [:rig :fixtures]))))

(defn- check [ok msg] (when-not ok (throw (ex-info msg {:component :fog-calibration}))))
(defn- verify [model session-id]
  (let [s (current model)]
    (check (and s (= session-id (:session-id s)) (= (:epoch model) (:epoch s))
                (identical? (:rig model) (:rig s))) "Fog scan expired or the rig changed")
    s))

(defn expire [model now]
  (if-let [s (current model)]
    (if (and (identical? (:rig s) (:rig model)) (= (:epoch s) (:epoch model))
             (<= now (:expires-at-ms s))) model (disarm model))
    model))

(defn- start [model {:keys [session-id fixture-id path-m]} now]
  (let [id (when fixture-id (first (calibration/resolve-fixture-ids model [fixture-id])))]
    (check (and (nil? (current model)) (string? session-id) (<= 1 (count session-id) 128)
                (number? path-m) (<= 0.1 path-m 1000)
                (or (nil? id) (some #(= id (:id %)) (fixtures model))))
           "Choose a measured sightline and a metered nozzle, or leave the nozzle empty for a manual burst")
    (assoc-in model session-path {:session-id session-id :fixture-id id :path-m path-m
                                  :epoch (:epoch model) :rig (:rig model)
                                  :started-at-ms now :expires-at-ms (+ now 7200000)
                                  :readings (sorted-map)})))

(defn- add-readings [s samples now]
  (check (and (vector? samples) (<= 1 (count samples) 120)) "Send one to 120 fog samples at a time")
  (let [readings (reduce (fn [r sample]
                           (let [sample (-> sample (select-keys [:phase :elapsed-ms :bright :dark])
                                            (update :phase keyword) optical/validate-sample!)
                                 t (:elapsed-ms sample)]
                             (check (<= t (+ 2000 (- now (:started-at-ms s)))) "Sample is ahead of the capture clock")
                             (check (or (nil? (get r t)) (= sample (get r t))) "Conflicting sample at this timestamp")
                             (assoc r t sample))) (:readings s) samples)]
    (check (<= (count readings) 7201) "Fog scan reached its sample limit")
    (if (= readings (:readings s)) s
        (-> s (assoc :readings readings) (dissoc :fit)))))

(defn- observed-level [{:keys [readings started-at-ms path-m session-id epoch]}]
  (let [ordered (vals readings)
        contrast #(- (:bright %) (:dark %))
        clear (sort (map contrast (take-while #(= :baseline (:phase %)) ordered)))
        n (count clear)]
    (when (>= n 4)
      (let [reference (* 0.5 (+ (nth clear (quot n 2)) (nth clear (quot (dec n) 2))))
            [[_ latest] [_ previous]] (take 2 (rseq readings))
            level (fn [sample] (max 0.0 (- 1.0 (min 1.0 (/ (contrast sample) reference)))))]
        (when (and (> reference 0.04) (< (- (last clear) (first clear)) (* 0.1 reference)))
          {:level (level latest)
           :level-change-per-s (/ (- (level latest) (level previous))
                                  (/ (- (:elapsed-ms latest) (:elapsed-ms previous)) 1000.0))
           :sample-time-ms (+ started-at-ms (:elapsed-ms latest))
           :phase (:phase latest) :path-m path-m :session-id session-id :epoch epoch})))))

(defn prepare-fit
  "Called by the submitting thread. No curve fitting takes place on the live frame owner."
  [model session-id trace]
  (let [s (verify model session-id)
        {:keys [width height depth] :or {width 12 height 5 depth 10}} (get-in model [:rig :room])
        match? (and (= session-id (:session-id trace)) (= (:epoch model) (:epoch trace)))]
    (when match?
      (check (not (:overflow? trace)) "The scan exceeded its DMX transition history; start a new scan")
      (check (not (:other-source? trace)) "Additional metered fog machines were active during this scan"))
    {:readings-used (:readings s)
     :fit (optical/fit {:samples (vec (vals (:readings s)))
                        :flows (when match? (:flows trace)) :path-m (:path-m s)
                        :volume-m3 (* width height depth)})}))

(defn- accept-fit [model {:keys [fit fixture-id]}]
  (check fit "Fit the scan before applying it")
  (let [room (select-keys fit [:clearance-half-life-s :extinction-per-m :reference-path-m :method :r-squared])]
    (cond-> (-> model disarm
                (update-in [:rig :room] #(merge {:width 12 :height 5 :depth 10} %))
                (update-in [:rig :room :measured-fog] merge room))
      (:mixing-seconds fit)
      (update-in [:rig :fixtures]
                 (fn [fixtures] (mapv #(if (= fixture-id (:id %))
                                        (assoc-in % [:fogger :mixing-seconds] (:mixing-seconds fit)) %) fixtures))))))

(defn operation [model {:keys [op session-id samples fit readings-used] :as cmd} now]
  (let [s (when-not (= :start op) (verify model session-id))
        result (case op
                 :start (start model cmd now)
                 :samples (let [scan (add-readings s samples now)
                                measurement (observed-level scan)]
                            (cond-> (assoc-in model session-path scan)
                              measurement (assoc-in [:variables :fog/observed-level] (:level measurement))
                              measurement (assoc-in [:variables :fog/observed-change-per-s] (:level-change-per-s measurement))
                              measurement (assoc-in [:variables :fog/observation] measurement)))
                 :fit (do (check (and fit (identical? readings-used (:readings s))) "Scan changed during fitting; fit again")
                          (assoc-in model (conj session-path :fit) fit))
                 :apply (accept-fit model s)
                 :cancel (disarm model)
                 (throw (ex-info "Unknown fog scan operation" {:op op})))]
    {:model (if (= :apply op) (update result :revision inc) result)
     :ack (cond-> {:status :accepted :session-id session-id}
            (= :fit op) (assoc :fit fit))}))
