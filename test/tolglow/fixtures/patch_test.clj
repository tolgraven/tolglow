(ns tolglow.fixtures.patch-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.render.frame :as render]
            [tolglow.application :as application]
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
