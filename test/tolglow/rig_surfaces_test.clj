(ns tolglow.rig-surfaces-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.rig :as rig]
            [tolglow.rig.surfaces :as surfaces]))
(def tilted {:id "tilted" :point [1 2 3] :normal [0 4 4] :u-axis [2 4 4] :size [4 2]})
(def fixtures
  (mapv (fn [i id] {:id id :universe 1 :address (inc (* i 2)) :channels {:pan 1 :tilt 2}})
        (range 3) [:a :b :c]))
(def rig {:fixtures fixtures :room {:width 8 :height 5 :depth 10} :surfaces [tilted]})
(deftest finite-receivers-project-relative-to-their-own-centers
  (is (= rig (rig/validate-rig rig)))
  (is (= [8.0 10.0] (:size (first (surfaces/receivers rig)))))
  (is (surfaces/on-surface? tilted [3 2 3]))
  (is (not (surfaces/on-surface? tilted [3.01 2 3])))
  (is (not (surfaces/on-surface? tilted [1 2.1 3])))
  (doseq [bad [(assoc tilted :id "floor") (assoc tilted :normal [0 0 0]) (assoc tilted :u-axis [0 1 1])
               (assoc tilted :size [2 -3])]]
    (is (seq (rig/errors (assoc rig :surfaces [bad]))))))
(deftest tracking-has-a-single-parent-and-a-leader-first-order
  (let [ab {:leader :a :follower :b :surface-id :floor :along [1 0 0]}
        bc (assoc ab :leader :b :follower :c)
        valid (assoc rig :edge-tracking [ab bc])]
    (is (= valid (rig/validate-rig valid)))
    (is (= valid (surfaces/resolve-link-ids (assoc-in valid [:edge-tracking 0] (assoc ab :leader "a" :surface-id "floor")))))
    (doseq [bad [[ab ab] [bc ab] [ab (assoc bc :follower :a)] [(assoc ab :follower :unknown)]
                 [(assoc ab :surface-id :outside)] [(assoc ab :along [0 1 0])]]]
      (is (seq (rig/errors (assoc rig :edge-tracking bad)))))))
