(ns tolglow.capture-preview-oracle
  (:require [afterglow.render.frame :as frame] [clojure.data.json :as json]))

(let [base {:id "oracle-head" :position [0 3 0] :universe 1 :address 1
            :channels {:pan [1 2] :tilt [3 4]}
            :calibration {:pan {:min-deg -270 :max-deg 270}
                          :tilt {:min-deg -90 :max-deg 90}}}
      stopped (assoc base :id "end-stop" :rotation-deg [0 0 180]
                     :calibration {:pan {:min-deg -30 :max-deg 30} :tilt {:min-deg -20 :max-deg 20}})
      cases (conj (mapv (fn [[rotation direction]] [(assoc base :rotation-deg rotation) direction])
                        [[[0 0 0] [0 -1 0]] [[0 0 0] [0 0 1]] [[0 0 0] [1 0 0]]
                         [[25 40 -15] [1 -2 3]] [[-20 -60 30] [-2 -4 -1]]])
                  [stopped [-1 -1 0]])
      capture (fn [[fixture direction]]
                (let [frame (frame/resolve-frame [fixture] [{:target-id (:id fixture) :attribute :direction
                                                             :value direction :blend :replace}])
                      values (get-in frame [:values (:id fixture)])
                      pose (frame/fixture-pose fixture values)]
                  {:fixture fixture :output (select-keys values [:pan :tilt :pan-deg :tilt-deg :direction])
                   :servo-output (select-keys values [:pan :tilt]) :pose pose :expected (:direction pose)}))]
  (spit "test/js/preview/fixtures/aim-oracle.json"
        (json/write-str {:source "afterglow.render.frame/resolve-frame" :cases (mapv capture cases)})))
