(ns tolglow.fixtures.patch-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.render.frame :as render]
            [tolglow.application :as application]
            [tolglow.demo :as demo]
            [tolglow.persistence :as files]
            [tolglow.fixtures.patch :as patch]
            [tolglow.show :as show]))

(def pixel {:id :rgbw :name "RGBW" :version 1 :footprint 4
            :channels {:red 1 :green 2 :blue 3 :white 4}})

(deftest bare-names-builders-and-positioned-entries-cross-universes
  (let [calls (atom 0)
        factory #(do (swap! calls inc) pixel)
        groups {:wash {:universe 7 :offset 510 :type :led
                       :list [:rgbw factory [:rgbw 1 2 3 :y-rotation (/ Math/PI 2)]
                              ['venue/wash {:position [3 0 4]}]]}}
        result (patch/expand {:rgbw pixel} {'venue/wash factory} groups)]
    (is (= 2 @calls))
    (is (= [:wash-1 :wash-2 :wash-3 :wash-4] (mapv :id result)))
    (is (= [[7 510] [8 2] [8 6] [8 10]] (mapv (juxt :universe :address) result)))
    (is (= [[0 0 0] [0 0 0] [1 2 3] [3 0 4]] (mapv :position result)))
    (is (= [0.0 90.0 0.0] (:rotation-deg (nth result 2))))
    (is (every? #(= [:wash :led] (:groups %)) result))
    (is (identical? result (patch/expand {} result)))
    (is (= #{7 8} (set (keys (:universes (render/resolve-frame (render/compile-patch result) [] {}))))))))

(deftest separate-universe-groups-reserve-the-whole-footprint
  (let [strip (assoc pixel :footprint 1020)
        result (patch/expand {:rgbw strip} {:strip {:start-universe 10 :offset 4 :list [:rgbw :rgbw]}})]
    (is (= [[10 4] [12 4]] (mapv (juxt :universe :address) result))))
  (let [at {:universe 1 :offset 1 :list [:rgbw]}]
    (doseq [groups [{:wash at :strip at}
                    {:wash (assoc at :list [:missing])}
                    {:wash (assoc at :start-universe 1)}]]
      (is (thrown? clojure.lang.ExceptionInfo (patch/expand {:rgbw pixel} groups))))))

(deftest normal-startup-and-submitted-rig-use-the-same-stored-full-patch
  (let [runtime (application/start!
                 {:init {:components [:show] :modules [:fixtures]}
                  :show {:workers 1 :workspace-options nil}
                  :fixtures {:definitions [pixel] :patches {:wash {:offset 1 :universe 2 :list [:rgbw :rgbw]}}}})]
    (try
      (is (= [:wash-1 :wash-2] (mapv :id (get-in (show/snapshot runtime) [:rig :fixtures]))))
      (is (= :accepted (:status @(show/submit! runtime :rig {:fixtures {:led {:universe 3 :list [:rgbw]}}}))))
      (let [fixtures (get-in (show/snapshot runtime) [:rig :fixtures])]
        (is (= [:led-1] (mapv :id fixtures)))
        (is (= (get-in fixtures [0 :channels]) (:channels pixel)))
        (is (= :rejected (:status @(show/submit! runtime :rig {:fixtures {:led {:universe 3 :list [:absent]}}}))))
        (is (= fixtures (get-in (show/snapshot runtime) [:rig :fixtures]))))
      (finally (application/stop! runtime)))))

(deftest fixture-optics-survive-ordered-groups-and-head-expansion
  (let [props {:beam-angle 18.5 :measured-relative-brightness 2.0 :max-watts 160 :pseudo-fixture false
               :lens-radius-m 0.04 :zoom-range-deg [30 12]}
        single (assoc pixel :beam-angle 45 :max-watts 40 :pseudo-fixture true)
        split (merge props {:id :strip :version 1 :channels {:dimmer 1}
                            :heads [{:channels {:red 2} :beam-angle 22} {:channels {:red 3}}]})
        fixtures (patch/expand {:one single :two split}
                               {:spots {:universe 1 :list [[:one props] :two]}})
        [lamp master h1 h2] fixtures]
    (is (= props (select-keys lamp (keys props))))
    (let [model (assoc-in (demo/model) [:rig :fixtures] fixtures)
          restored (files/read-string (files/write-string model))]
      (is (= fixtures (get-in restored [:model :rig :fixtures]))))
    (is (= [18.5 22 18.5] (mapv :beam-angle [master h1 h2])))
    (is (= [0.0 0.5 0.5] (mapv :emission-share [master h1 h2])))
    (is (= [2.0 2.0 2.0] (mapv :measured-relative-brightness [master h1 h2])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (patch/expand {:one single} {:spots {:universe 1 :list [[:one {:beam-angle -3}]]}})))))

(deftest grouped-fogger-properties-follow-the-target-that-owns-its-fog-channel
  (let [profile {:flow-m3-min 4.0 :mixing-seconds 8 :direction [0 1 0]}
        fixture {:id :hybrid :version 1 :fogger profile
                 :channels {:fog 1} :heads [{:channels {:red 2}}]}
        [machine led] (patch/expand {:hybrid fixture} {:rig {:universe 1 :list [:hybrid]}})]
    (is (= profile (:fogger machine)))
    (is (not (contains? led :fogger)))))
