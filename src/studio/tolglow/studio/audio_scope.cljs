(ns tolglow.studio.audio-scope
  "Display the bounded capture waveform from the Connections state subscription."
  (:require [re-frame.core :as rf]
            [tolglow.studio.parameter-widgets :as widgets]))

(defn panel []
  (let [{:keys [audio]} @(rf/subscribe [:connections/state])]
    [:section.capture-scope {:aria-label "Live audio capture"}
     [:div.capture-scope-heading [:span.section-label "CAPTURED AUDIO"]
      [:span (cond (:running? audio) "Live input" audio "Capture stopped" :else "Awaiting capture status")]]
     [widgets/audio-waveform (:waveform audio)]]))
