(ns tolglow.automation.prediction
  "Hypothetical outputs for choosing a look. Prediction owns no show or device state."
  (:require [tolglow.effects.post :as post-effects]
            [afterglow.control.model :as control]
            [tolglow.spatial :as spatial]
            [afterglow.render.frame :as render]
            [afterglow.runtime.engine :as engine]
            [afterglow.time :as time]
            [tolglow.compiler :as compiler]
            [tolglow.effects.state-migration :as state-migration]))

(defn frame
  "Sample the existing show and proposed launches once using the actual compiler
  and resolver. The supplied patch can be reused across a planning decision."
  ([model patch cues time-ms] (frame model patch cues time-ms nil))
  ([model patch cues time-ms standby-block]
  (let [clock (time/frame-clock (assoc model :time-ms time-ms))
        bpm (:bpm clock 120)
        ;; This is a same-tempo hypothetical sample for dependency planning,
        ;; never conversion of a musical runtime deadline to milliseconds.
        offset (reduce max 1
                       (map #(time/estimate-ms
                              (time/normalize-duration
                               (or (get-in model [:registry :effects (:effect-id %) :envelope :attack])
                                   {:ms (get-in model [:registry :effects (:effect-id %) :envelope :attack-ms] 0)})
                               (:meter clock)) bpm) cues))
        sample-time (+ time-ms offset)
        sample-clock (cond-> (assoc clock :time-ms sample-time)
                       (number? (:beat clock)) (update :beat + (* offset (/ bpm 60000.0))))
        prediction-model
        (reduce (fn [show [i cue]]
                  (let [launch (update (:prediction-launch cue) :params #(merge {} (:params cue) %))
                        {:keys [model ack]}
                        (spatial/apply-command show
                          {:op :cue/launch :show-id (:show-id show) :epoch (:epoch show)
                           :command-id (str "auto-prediction:" (:revision show) ":" i)
                           :source :auto-prediction :time-ms time-ms :cue-id (:id cue) :launch launch})]
                    (when (= :rejected (:status ack))
                      (throw (ex-info "Auto prediction launch rejected" {:cue-id (:id cue) :ack ack})))
                    model))
                (merge (control/new-show {:show-id (:show-id model)}) model {:time-ms time-ms})
                (map-indexed vector cues))
        result (engine/advance (assoc prediction-model :transport (dissoc sample-clock :time-ms))
                               {:time-ms sample-time :fixtures (:fixtures patch)
                                :transport (dissoc sample-clock :time-ms)}
                               ;; Match live :migrate reloads of the copied instances.
                               #(state-migration/attach nil % (compiler/compile-effect %)))
        background (when (and (empty? (:instances model)) (seq (:assignments model)))
                     {:kind :assignments :assignments (:assignments model)})
        blocks (into (cond-> [] standby-block (conj standby-block) background (conj background)) (:contributions result))
        ;; A held group blackout masks its heads in the forecast too (#479).
        frame (render/resolve-contributions patch blocks (cond-> {:master (:master model 1) :blackout? (:blackout? model false)}
                                                           (post-effects/masks? model)
                                                           (assoc :blackout-ids (post-effects/blackout-ids model))))
        proposed (into #{} (map :id) cues)]
    (cond-> frame
      (some #(and (= :effect/failed (:type %)) (proposed (:cue-id %))) (:fault-events result))
      (assoc ::failed? true)))))
