(ns tolglow.rig.calibration
  "Calibration host transitions. Effects use the shared control/compiler path;
  only the runtime's publisher may produce physical-output evidence."
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as render]
            [tolglow.calibration.session :as session]))

(defn current [model] (get-in model session/artifact-path))

(defn resolve-fixture-ids
  "Resolve JSON keyword IDs against actual rig IDs without changing string IDs."
  [model ids]
  (let [existing (set (map :id (get-in model [:rig :fixtures])))]
    (when ids
      (mapv (fn [id]
              (cond (existing id) id
                    (and (keyword? id) (existing (subs (str id) 1))) (subs (str id) 1)
                    (and (string? id) (existing (keyword id))) (keyword id)
                    :else id)) ids))))

(defn- command [model command time]
  (let [result (control/apply-command model
                 (merge {:command-id (str "calibration/" (:epoch model) "/" (:revision model))
                         :show-id (:show-id model) :source {:adapter :rig-calibration}}
                        command {:time-ms time :epoch (:epoch model)}))]
    (when (= :rejected (get-in result [:ack :status])) (throw (ex-info "Calibration command rejected" (:ack result))))
    (:model result)))
(defn- owned? [scan instance]
  (and (= :rig-calibration (get-in instance [:source :adapter]))
       (= (:session-id scan) (get-in instance [:source :session-id]))))
(defn release
  "Release exactly the session's instances; leave ongoing human effects/state
  alone. Remove its temporary definitions and historical versions as well."
  [model reason time]
  (if-let [scan (current model)]
    (let [model (reduce (fn [m instance] (if (owned? scan instance)
                                          (command m {:op :instance/remove :instance-id (:id instance)} time) m))
                        model (vals (:instances model)))
          id (:definition-id scan)]
      (-> model
          (update-in [:registry :effects] dissoc id) (update-in [:registry :cues] dissoc id)
          (update-in [:registry :versions] (fn [versions] (into {} (remove (fn [[[kind key _] _]] (and (#{:effects :cues} kind) (= key id)))) versions)))
          (assoc-in session/artifact-path (assoc scan :status reason :step-index nil :armed? false)))) model))
(defn disarm-restored [model]
  (if (:armed? (current model)) (release model :suspended (:time-ms model)) model))
(defn advance [model receipts physical-now]
  (if-let [scan (current model)]
    (let [model (assoc-in model session/artifact-path (reduce session/receipt scan receipts))]
      (if (and (:armed? scan) (or (not= (:epoch model) (:epoch scan))
                                 (>= physical-now (:expires-at-ms scan))
                                 (>= physical-now (:lease-until-ms scan 0))))
        (release model :expired (:time-ms model)) model)) model))

(defn- output-values [step]
  (cond-> {:color (:color step) :dimmer (:dimmer step) :strobe 0.0}
    (:pan step) (assoc :pan (:pan step) :tilt (:tilt step))))
(defn marker
  "Small frame metadata. Expected bytes cover only controlled fixture channels,
  including coarse/fine and inversion as compiled by the actual renderer."
  [model]
  (let [scan (current model) step (get (:plan scan) (:step-index scan))]
    (when (and (:armed? scan) step (= (:epoch scan) (:epoch model)))
      (let [fixture (some #(when (= (:fixture-id step) (:id %)) %) (get-in model [:rig :fixtures]))
            patch (render/compile-patch [fixture])
            assignments (mapv (fn [[attribute value]] {:target-id (:id fixture) :attribute attribute :value value}) (output-values step))
            frame (render/resolve-frame patch assignments {:master (:master model) :blackout? (:blackout? model)})
            channels (select-keys (:compiled-channels (first (:fixtures patch))) [:red :green :blue :white :dimmer :pan :tilt :strobe])
            slots (mapcat #(remove nil? [(:coarse %) (:fine %)]) (vals channels))]
        {:session-id (:session-id scan) :step-index (:step-index scan)
         :expected-bytes (into {} (map (fn [[universe index :as slot]] [slot (bit-and 255 (aget ^bytes (get-in frame [:universes universe]) index))])) slots)}))))

(defn apply-operation [model payload time physical-now output-capable?]
  (let [payload (cond-> payload (:fixture-ids payload) (update :fixture-ids #(resolve-fixture-ids model %)))
        scan (current model) operation (:op payload)
        _ (when (and (not= :start operation) (not= (:session-id payload) (:session-id scan)))
            (throw (ex-info "Calibration session changed" {})))
        result
        (case operation
          :start
          (do (when-not (and output-capable? (true? (:armed? payload)) (not (:armed? scan)))
                (throw (ex-info "Start requires explicit arming, a physical sink, independent publisher, and no active scan" {})))
              (let [scan (-> (session/create model payload physical-now)
                             (assoc :status :active :armed? true :definition-id (str "rig-calibration/" (:session-id payload))))]
                (when (or (get-in model [:registry :effects (:definition-id scan)]) (get-in model [:registry :cues (:definition-id scan)]))
                  (throw (ex-info "Calibration definition ID already exists" {})))
                (assoc-in model session/artifact-path scan)))
          :step
          (let [index (:step-index payload) step (get (:plan scan) index)]
            (when-not (and (:armed? scan) (= (:rig model) (:base-rig scan)) (integer? index) step
                           (= index (inc (or (:step-index scan) -1)))
                           (or (nil? (:step-start-beat scan))
                               (>= (- (get-in model [:transport :beat] 0) (:step-start-beat scan)) 2)))
              (throw (ex-info "Advance each step in order after two transport beats; rig and scan must remain active" {})))
            (let [model (release model :active time) id (:definition-id scan) version (inc index)
                  definition {:id id :version version :name "Rig calibration" :params {} :targets [(:fixture-id step)]
                              :outputs (output-values step) :release-duration {:beats 0}}
                  model (command model {:op :registry/publish :kind :effects :definition definition} time)
                  model (command model {:op :cue/put :cue {:id id :version version :effect-id id :priority 1000000000}} time)
                  model (command model {:op :cue/launch :cue-id id :source {:adapter :rig-calibration :session-id (:session-id scan)}} time)]
              (assoc-in model session/artifact-path (assoc scan :lease-until-ms (+ physical-now 15000) :step-index index :step-start-beat (get-in model [:transport :beat] 0)))))
          :heartbeat (do (when-not (:armed? scan) (throw (ex-info "Scan is no longer armed" {})))
                         (assoc-in model (conj session/artifact-path :lease-until-ms) (+ physical-now 15000)))
          :observe (assoc-in model session/artifact-path (session/observe scan (:observations payload)))
          :solved
          (do (when-not (= (:evidence-revision scan) (:expected-evidence-revision payload))
                (throw (ex-info "Evidence changed while fitting; solve again" {})))
              (assoc-in (release model :review time) session/artifact-path (assoc (:result payload) :armed? false :step-index nil)))
          :apply
          (let [proposed (session/proposed-rig scan (:rig model) (:fixture-ids payload))]
            (-> (release model :applied time) (assoc :rig proposed)
                (assoc-in (conj session/artifact-path :base-rig) proposed)))
          :cancel (release model :cancelled time)
          (throw (ex-info "Unknown calibration operation" {:op operation})))]
    {:model (update result :revision inc)
     :ack {:status :accepted :session-id (get-in result (conj session/artifact-path :session-id))
           :calibration-status (get-in result (conj session/artifact-path :status))}}))
