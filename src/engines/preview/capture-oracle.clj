(ns preview.capture-oracle
  (:require [afterglow.render.frame :as frame] [clojure.data.json :as json]))
(let [cases (for [[rotation target] [[[0 0 0] [0 -1 0]]
                                     [[0 0 0] [0 0 1]]
                                     [[0 0 0] [1 0 0]]
                                     [[25 40 -15] [1 -2 3]]
                                     [[-20 -60 30] [-2 -4 -1]]]]
              (let [fixture {:id "oracle-head" :position [0 3 0] :rotation-deg rotation
                             :universe 1 :address 1 :channels {:pan [1 2] :tilt [3 4]}
                             :calibration {:pan {:min-deg -270 :max-deg 270}
                                           :tilt {:min-deg -90 :max-deg 90}}}
                    result (frame/resolve-frame [fixture] [{:target-id "oracle-head" :attribute :direction
                                                           :value target :blend :replace}] {})
                    output (get-in result [:values "oracle-head"])]
                {:fixture fixture :output (select-keys output [:pan :tilt :pan-deg :tilt-deg :direction])
                 :servo-output (select-keys output [:pan :tilt]) :expected (:direction output)}))]
  (spit "src/engines/preview/fixtures/aim-oracle.json"
        (json/write-str {:source "afterglow.render.frame/resolve-frame" :cases (vec cases)})))
