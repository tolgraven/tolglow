(ns tolglow.fixtures.patch-parity-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.application :as app]
            [tolglow.config :as config]
            [tolglow.fixtures :as fixtures]
            [tolglow.fixtures.patch :as patch]
            [afterglow.render.frame :as render]))

(deftest original-mover-and-multi-universe-pixel-builders
  (let [groups (assoc (select-keys config/patches [:moving :moving-mini :strip])
                      :long-strip {:universe 4 :list [[[fixtures/pixel-strip 144 :color-layout :rgbw]]]})
        result (patch/expand config/fixture-types {} groups [:moving :moving-mini :strip])
        by-id (into {} (map (juxt :id identity)) result)
        pixels (filter #(= :strip-1 (:fixture-id %)) result)
        big (filter #(= :long-strip-1 (:fixture-id %)) result)
        output (render/resolve-frame (render/compile-patch result)
                                     [{:target-id :long-strip-1-head-144 :attribute :red :value 1.0}] {})]
    (is (= 3 (count (filter #(= :moving (first (:groups %))) result))))
    (is (= [0.0 180.0 0.0] (:rotation-deg (:moving-3 by-id))))
    (is (= 4 (count (filter #(= :moving-mini (first (:groups %))) result))))
    (is (= 125 (count pixels)))
    (is (= 144 (count big)))
    (is (= :long-strip-1-head-144 (:id (last big))))
    (is (= #{1 2 4 5} (set (keys (:universes output)))))
    (is (= 5 (:universe (last big))))
    (is (= 255 (bit-and 255 (aget ^bytes (get-in output [:universes 5]) 60))))
    (is (apply < (map (comp first :position) pixels)))))

(deftest venue-fixtures-start-as-configured
  (let [runtime (app/start! {:init {:components [:show] :modules [:fixtures]}
                             :show {:workers 1 :workspace-options nil}
                             :fixtures (:fixtures config/data)})]
    (try
      (let [fixtures (get-in @(:state runtime) [:model :rig :fixtures])]
        (is (= 149 (count fixtures)))
        (is (= [:moving :moving-mini :wash :cob :strip :tube]
               (vec (distinct (map (comp first :groups) fixtures)))))
        (is (= 125 (count (filter #(= :strip-1 (:fixture-id %)) fixtures))))
        (is (= 10 (count (filter #(= :tube-1 (:fixture-id %)) fixtures))))
        (is (= #{1 2 10 11} (set (map :universe fixtures)))))
      (finally (app/stop! runtime)))))
