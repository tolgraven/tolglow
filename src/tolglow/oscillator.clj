(ns tolglow.oscillator
  "Phase in cycles from a shared beat or a cue's elapsed beat."
  (:require [afterglow.time :as time]))

(def phase-references #{:launch :transport})

(defn musical-period? [period]
  (and (map? period) (= 1 (count period)) (#{:beats :bars :phrases} (ffirst period))
       (time/finite-number? (val (first period))) (pos? (val (first period)))))

(defn phase
  "Use the captured meter for elapsed beats and the shared meter for transport."
  [{:keys [period phase phase-reference] :or {phase 0.0 phase-reference :launch}}
   transport-clock elapsed-beats launch-meter]
  (let [[beat meter] (case phase-reference
                      :transport [(:beat transport-clock) (:meter transport-clock)]
                      :launch [elapsed-beats launch-meter])]
    (when-not (time/finite-number? beat)
      (throw (ex-info "Waveform phase requires a finite beat coordinate" {:phase-reference phase-reference})))
    (+ phase (/ beat (time/duration->beats period meter)))))
