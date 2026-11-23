(ns tolglow.effects.cadence
  "Bind wall-clock measurements to the release show's adaptive keyframe policy."
  (:require [afterglow.runtime.cadence :as cadence]))

(defn settings [config]
  (when-not (and (map? config) (boolean? (:enabled? config true)))
    (throw (ex-info "Adaptive keyframes require settings with a boolean :enabled?" {:settings config})))
  (assoc (cadence/settings (dissoc config :enabled?)) :enabled? (:enabled? config true)))

(defn- versions [model]
  (into {} (map (fn [[id {:keys [effect-id effect-version]}]]
                  [id [effect-id effect-version (get-in model [:registry :effects effect-id :version])]]))
        (:instances model)))

(defn begin [runtime before model]
  (let [config (get-in model [:settings :adaptive-keyframes] (:adaptive-keyframes runtime))]
    (when (and config (:enabled? config true) (seq (:instances model)))
      (let [prior (:cadence before)
            options (if (identical? config (:config prior)) (:options prior) (settings config))
            state (:state prior)
            ids (versions model)
            period (/ 1000.0 (:frame-rate runtime))
            intervals (when (= (:epoch model) (:epoch state))
                        (into {} (keep (fn [[id {:keys [stride version]}]]
                                         (when (and (> stride 1) (= version (get ids id)))
                                           [id {:ms (* period (min stride (:max-stride options)))}])))
                              (:effects state)))
            observer (cadence/observer)]
        {:config config :options options :previous state :samples (:samples observer)
         :epoch (:epoch model) :period-ms period
         :inputs {:keyframe-intervals intervals :measure-effect (:measure-effect observer)}}))))

(defn finish [session model elapsed-ms]
  (let [{:keys [options epoch period-ms previous samples config]} session
        timings {:epoch epoch :versions (versions model) :samples @samples
                 :period-ms period-ms :elapsed-ms elapsed-ms}]
    {:options options :config config :state (cadence/advance previous timings options)}))

(defn status
  "Read the diagnostic ranking on demand, outside the frame publication loop."
  [runtime]
  (when-let [state (get-in @(:state runtime) [:cadence :state])]
    (assoc (select-keys state [:frame :frame-ms :late-rate :keyframe-intervals])
           :effects (cadence/ranked-costs state))))
