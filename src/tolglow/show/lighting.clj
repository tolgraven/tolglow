(ns tolglow.show.lighting
  "Settings and frame ordering for Tolglow's optical and performance controls."
  (:require [tolglow.autopilot.config :as autopilot]
            [tolglow.lighting-activity :as activity]
            [tolglow.lighting-budget :as budget]
            [tolglow.output-response :as response]))

(defn activity-settings [options]
  (when-not (and (map? options) (every? #{:enabled?} (keys options))
                 (boolean? (:enabled? options)))
    (throw (ex-info "Lighting activity requires a boolean :enabled?" {:settings options})))
  options)

(def validators
  {:autopilot autopilot/settings :hdr autopilot/hdr-settings :lighting-budget budget/settings :output-response response/settings :lighting-activity activity-settings})

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
                        options (case scope :autopilot [:targets :hdr :signals] :hdr [:cue-gains :instance-gains :source-gains] []))]
    (when-let [validate (validators scope)] (validate options))
    options))

(defn needs-meter? [settings]
  (or (get-in settings [:lighting-capacity :enabled?]) (get-in settings [:lighting-budget :enabled?])
      (get-in settings [:lighting-activity :enabled?])))

(defn prepare-budget [before model patch meter-plan calibration? hard-off?]
  (when (and (budget/enabled? model) (not calibration?) (not hard-off?))
    (budget/prepare-frame-context (get-in before [:lighting-history :budget]) model patch meter-plan)))

(defn contributions [standby authored extra]
  (if standby (into (into [standby] authored) extra) (into authored extra)))

(defn finish-frame
  "Apply responses to the resolved look, enforce the shared budget and score
  the actual result. Only committed frames carry temporal state forward."
  [before model patch plan budget-context frame blocks time-ms calibration? hard-off? ceiling]
  (let [same-rig? (and (= (:epoch model) (get-in before [:model :epoch]))
                       (identical? (:rig model) (get-in before [:model :rig])))
        previous (when same-rig? (:lighting-history before))
        settings (:settings model)
        response? (get-in settings [:output-response :enabled?])
        shaped (when response? (response/advance (:response previous) model patch frame
                                 (or calibration? hard-off? (false? (get-in model [:transport :playing?])))
                                 ceiling (when same-rig? (:output before)) blocks))
        shaped-frame (or (:frame shaped) frame)
        delivered (if budget-context (budget/apply-frame budget-context shaped-frame) shaped-frame)
        scoring? (get-in settings [:lighting-activity :enabled?])
        scored (when scoring?
                 (activity/advance-meter (:activity previous) patch plan delivered blocks time-ms))]
    {:frame (cond-> delivered scored (assoc :lighting-activity (:report scored)))
     :state (when (or budget-context shaped scored)
              {:budget budget-context :response (:state shaped) :activity (:state scored)})}))

(defn publish [model frame]
  (let [usage (:lighting-budget frame)]
    (cond-> (activity/publish model (:lighting-activity frame))
      usage (assoc-in [:variables :lighting/budget] usage)
      (and (nil? usage) (contains? (:variables model) :lighting/budget))
      (update :variables dissoc :lighting/budget))))
