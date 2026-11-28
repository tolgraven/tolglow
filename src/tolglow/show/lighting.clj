(ns tolglow.show.lighting
  "Settings and frame ordering for Tolglow's optical and performance controls."
  (:require [tolglow.autopilot.config :as autopilot]
            [tolglow.autopilot.signal :as signal]
            [tolglow.flash-guard :as flash-guard]
            [tolglow.lighting-activity :as activity]
            [tolglow.lighting-budget :as budget]
            [tolglow.output-response :as response]))

(defn activity-settings [options]
  (when-not (and (map? options) (every? #{:enabled?} (keys options))
                 (boolean? (:enabled? options)))
    (throw (ex-info "Lighting activity requires a boolean :enabled?" {:settings options})))
  options)

(def validators
  {:autopilot autopilot/settings :hdr autopilot/hdr-settings :lighting-budget budget/settings :output-response response/settings :lighting-activity activity-settings
   :flash-guard flash-guard/settings})

(defn validate! [settings]
  (doseq [[scope validate] validators :when (contains? settings scope)]
    (validate (get settings scope))))

(defn- edit-groups [current changes]
  (when-not (map? changes) (throw (ex-info "Group settings require a map" {})))
  (reduce-kv (fn [groups id edit]
               (if (nil? edit) (dissoc groups id)
                 (assoc groups id (merge (get groups id) edit))))
             (or current {}) changes))

(defn edit-settings
  "A group patch edits only the specified controls. A nil group removes its
  override. Individual controls such as a duration or parameter binding are
  replaced as a unit."
  [scope current edit]
  (let [fields (case scope
                 :output-response [:groups :fixtures :cues :compositions]
                 :lighting-budget [:groups] [])
        options (cond-> (merge current edit)
                  (and (seq fields) (map? (:global edit)))
                  (assoc :global (merge (:global current) (:global edit))))
        options (reduce (fn [opts k]
                          (if (contains? edit k) (assoc opts k (edit-groups (get current k) (get edit k))) opts))
                        options fields)
        options (reduce (fn [opts k]
                          (if (and (map? (get edit k)) (map? (get current k)))
                            (update opts k #(merge (get current k) %)) opts))
                        options (case scope :autopilot [:targets :hdr :signals :target-response] :hdr [:cue-gains :instance-gains :source-gains] []))]
    (when-let [validate (validators scope)] (validate options))
    options))

(defn needs-meter? [settings]
  (or (get-in settings [:lighting-capacity :enabled?]) (get-in settings [:lighting-budget :enabled?])
      (get-in settings [:lighting-activity :enabled?])))

(defn guard-active?
  "Whether the flash guard inspects this frame. It needs the meter and the
  frame finish even when no other lighting control is enabled."
  [model before]
  (flash-guard/active? model (get-in before [:lighting-history :flash-guard])))

(defn prepare-budget [before model patch meter-plan calibration? hard-off?]
  (when (and (budget/enabled? model) (not calibration?) (not hard-off?))
    (budget/prepare-frame-context (get-in before [:lighting-history :budget]) model patch meter-plan)))

(defn contributions [standby authored extra]
  (if standby (into (into [standby] authored) extra) (into authored extra)))

(defn- show-time
  "The model at show time: host :time-ms less the transport's held time."
  [model]
  (if (number? (:time-ms model))
    (assoc model :time-ms (:time-ms (signal/show-clock model {:time-ms (:time-ms model)})))
    model))

(defn- ambient-pause? [model]
  (and (false? (get-in model [:transport :playing?]))
       (= :ambient (get-in model [:settings :output :pause-look :mode]))))

(defn finish-frame
  "Apply responses to the resolved look, enforce the shared budget, guard
  against fast flashing and score the actual result. Only committed frames
  carry temporal state forward."
  [before model patch plan budget-context frame blocks time-ms calibration? hard-off? ceiling]
  (let [same-epoch? (= (:epoch model) (get-in before [:model :epoch]))
        rig (:rig model)
        before-rig (get-in before [:model :rig])
        same-rig? (and same-epoch? (identical? rig before-rig))
        previous (when same-rig? (:lighting-history before))
        settings (:settings model)
        response? (get-in settings [:output-response :enabled?])
        shaped (when response?
                 ;; A link edit changes the rig map without changing the patch.
                 ;; Preserve the envelopes of fixtures unaffected by tracking.
                 (let [same-response-rig? (and same-epoch?
                                               (or same-rig?
                                                   (= (dissoc rig :edge-tracking)
                                                      (dissoc before-rig :edge-tracking))))]
                   ;; A transport stop is not a bypass: the response follows show
                   ;; time, so it stands still while stopped and resumes from
                   ;; where it was (#702). Blackout, master zero, calibration
                   ;; and the ambient pause look still bypass it.
                   (response/advance (when same-response-rig? (get-in before [:lighting-history :response]))
                     (show-time model) patch frame (or calibration? hard-off? (ambient-pause? model))
                     ceiling (when same-response-rig? (:output before)) blocks)))
        shaped-frame (or (:frame shaped) frame)
        budgeted (if budget-context (budget/apply-frame budget-context shaped-frame) shaped-frame)
        ;; The flash guard reads the light the fixtures will emit, after
        ;; response and budget, and holds rises on emission channels only.
        ;; It runs on show time, so it stands still with the transport.
        guard-history (:flash-guard previous)
        guarded (when (and plan (flash-guard/active? model guard-history))
                  (let [show-ms (:time-ms (signal/show-clock model {:time-ms time-ms}))]
                    (if (or calibration? hard-off?)
                      {:state (flash-guard/idle-state guard-history show-ms)}
                      (flash-guard/advance guard-history model patch plan (when same-rig? (:output before))
                                           budgeted blocks show-ms (double (get-in model [:transport :beat] 0.0))))))
        delivered (or (:frame guarded) budgeted)
        scoring? (get-in settings [:lighting-activity :enabled?])
        scored (when scoring?
                 (activity/advance-meter (:activity previous) patch plan delivered blocks time-ms))]
    {:frame (cond-> delivered
              scored (assoc :lighting-activity (:report scored))
              (:report guarded) (assoc :flash-guard (:report guarded)))
     :state (when (or budget-context shaped scored (:state guarded))
              {:budget budget-context :response (:state shaped) :activity (:state scored)
               :flash-guard (:state guarded)})}))

(defn publish [model frame]
  (let [usage (:lighting-budget frame)]
    (cond-> (flash-guard/publish (activity/publish model (:lighting-activity frame)) (:flash-guard frame))
      usage (assoc-in [:variables :lighting/budget] usage)
      (and (nil? usage) (contains? (:variables model) :lighting/budget))
      (update :variables dissoc :lighting/budget))))
