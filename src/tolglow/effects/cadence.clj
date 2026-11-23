(ns tolglow.effects.cadence
  "Bind wall-clock measurements to the release show's adaptive keyframe policy."
  (:require [afterglow.runtime.cadence :as cadence]
            [tolglow.effects.sampling-quality :as quality]))

(defn settings [config]
  (when-not (and (map? config) (boolean? (:enabled? config true)))
    (throw (ex-info "Adaptive keyframes require settings with a boolean :enabled?" {:settings config})))
  (assoc (cadence/settings (dissoc config :enabled?)) :enabled? (:enabled? config true)))

(defn- versions [model]
  (into {} (map (fn [[id {:keys [effect-id effect-version]}]]
                  [id [effect-id effect-version (get-in model [:registry :effects effect-id :version])]]))
        (:instances model)))

(defn- stride-limits [prior definitions transport period global-max]
  (when (seq definitions)
    (let [{:keys [bpm meter]} transport]
      (if (and (identical? definitions (:definitions prior))
               (= [bpm meter period global-max] (:clock prior)))
        prior
        {:definitions definitions :clock [bpm meter period global-max]
         :limits (update-vals definitions #(cadence/maximum-stride % transport period global-max))}))))

(defn- requested-intervals [entries ids period global-max limits]
  (into {} (keep (fn [[id {:keys [stride version]}]]
                   (let [available (min stride global-max (get limits id global-max))]
                     (when (and (> available 1) (= version (get ids id)))
                       [id {:ms (* period available)}]))))
        entries))

(defn begin [runtime before model]
  (let [config (get-in model [:settings :adaptive-keyframes] (:adaptive-keyframes runtime))]
    (when (and config (:enabled? config true) (seq (:instances model)))
      (let [prior (:cadence before)
            options (if (identical? config (:config prior)) (:options prior) (settings config))
            state (:state prior)
            ids (versions model)
            period (/ 1000.0 (:frame-rate runtime))
            ;; A new capture starts at full rate. Only a surviving version can
            ;; reuse a prior stride, so its previous cap declaration suffices.
            quality (:quality prior)
            limits (when (= (:epoch model) (:epoch state))
                     (stride-limits (:ceilings prior) (:ceilings quality) (:transport model) period (:max-stride options)))
            intervals (when (= (:epoch model) (:epoch state))
                        (requested-intervals (:effects state) ids period (:max-stride options) (:limits limits)))
            observer (cadence/observer)]
        {:config config :options options :previous state :quality quality :ceilings limits :samples (:samples observer)
         :epoch (:epoch model) :period-ms period
         :inputs {:keyframe-intervals intervals :measure-effect (:measure-effect observer)}}))))

(defn finish [session model elapsed-ms]
  (let [{:keys [options epoch period-ms previous samples config]} session
        ids (versions model)
        quality (quality/prepare (:quality session) epoch ids (:instances model))
        limits (stride-limits (:ceilings session) (:ceilings quality) (:transport model) period-ms (:max-stride options))
        timings {:defer-live-slowdown? (:defer-live-slowdown? session)
                 :epoch epoch :versions ids :max-strides (:limits limits) :samples (quality/protect @samples (:protected quality))
                 :period-ms period-ms :elapsed-ms elapsed-ms}]
    {:options options :config config :quality quality :ceilings limits :state (cadence/advance previous timings options)}))

(defn status
  "Read the diagnostic ranking on demand, outside the frame publication loop."
  [runtime]
  (when-let [state (get-in @(:state runtime) [:cadence :state])]
    (assoc (select-keys state [:frame :frame-ms :late-rate :keyframe-intervals])
           :effects (cadence/ranked-costs state))))
