(ns tolglow.legacy-frame-oracle.scenario
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]))

(def legacy-source
  {:tolglow "01146ece7731ec06aa1ee6409342373096c8e0dc"
   :afterglow "4f7a82eab41862f80e43d91a8c2530b00aa51925"})

(def paired-source
  {:tolglow "40c922664c6c330d9c5eab7f4b2974f5807e2b88"
   :afterglow "d0ee56528aafb6d73cbe86b53d618a23b64c081e"})

(def seed 206)
(def fixture {:id :beam :universe 1 :address 37 :position [0.6 1.5 0.0]})
(def channels {:pan [1 2] :tilt [3 4] :movement-speed 5 :dimmer 6 :shutter 7
               :red 8 :green 9 :blue 10 :white 11 :auto 12 :control 13})
(def calibration {:pan [127 85] :tilt [0 256]})

;; Color spreads feed the original Tolglow color authoring function. The realized
;; RGB values become the shared authored inputs when the legacy trace is captured.
(def palette {:base {:color :orange :hue-spread 0.04}
              :accent {:color :blue :hue-spread 0.04}
              :cover {:color :white :hue-spread 0.0}})

(def cues
  {:raw-red {:kind :channel :attribute :red :dmx 49 :priority -20}
   :aim-front {:kind :aim :aim [1.2 2.75 4.0] :priority -10}
   :aim-back {:kind :aim :aim [0.1 0.75 -4.0] :priority -10}
   :base {:kind :color :blend :replace :priority 0}
   :accent {:kind :color :blend :max :priority 5}
   :cover {:kind :color :blend :replace :priority 5}
   :dimmer {:kind :function :attribute :dimmer :percent 75 :priority 0}
   :shutter {:kind :function :attribute :shutter :percent 0 :priority 0}})

(def operations
  [[:frame 0 :idle]
   [:launch 0 :raw-red]
   [:launch 0 :aim-front]
   [:launch 0 :base]
   [:launch 0 :dimmer]
   [:launch 0 :shutter]
   [:frame 100 :base-with-override]
   [:launch 120 :accent]
   [:end 150 :raw-red]
   [:frame 200 :overlapping-colors]
   [:launch 240 :cover]
   [:frame 300 :equal-priority-cover]
   [:end 310 :cover]
   [:end 310 :aim-front]
   [:launch 310 :aim-back]
   [:frame 400 :back-aim-with-history]
   [:end 410 :accent]
   [:frame 500 :base-revealed]
   [:restart 600]
   [:frame 600 :after-restart]
   [:launch 625 :base]
   [:launch 625 :aim-front]
   [:end-stale 630 :base]
   [:frame 750 :relaunch-survives]])

(defn read-edn [path]
  (with-open [reader (java.io.PushbackReader. (io/reader path))]
    (edn/read reader)))

(defn write-edn! [path data]
  (with-open [writer (io/writer path)]
    (pprint/pprint data writer)))

(defn unsigned-bytes [bytes]
  (mapv #(bit-and % 255) bytes))
