(ns tolglow.studio.audio-scope
  "Display the bounded capture waveform from the :connections frame lane."
  (:require [re-frame.core :as rf]
            [tolglow.studio.parameter-widgets :as widgets]))

(defn panel []
  (let [{:keys [audio]} @(rf/subscribe [:connections/state])
        ;; The waveform changes on every frame while capture runs; it arrives
        ;; on the :connections frame lane, not in app-db.
        live (:audio-waveform @(rf/subscribe [:connections/live]))
        {:keys [channels input-channel] :as waveform} (or live (:waveform audio))]
    [:section.capture-scope {:aria-label "Live audio capture"}
     [:div.capture-scope-heading [:span.section-label "CAPTURED AUDIO"]
      [:span (cond-> (cond (:running? audio) "Live input" audio "Capture stopped" :else "Awaiting capture status")
               input-channel (str " · Channel " input-channel " of " channels))]]
     [widgets/audio-waveform waveform]]))
