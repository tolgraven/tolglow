(ns tolglow.reflected-surfaces-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.demo :as demo]
            [tolglow.rig.reflections :as reflections]
            [tolglow.show :as show]
            [tolglow.studio :as studio]))

(def heads (mapv (fn [id x address angle]
                   {:id id :position [x 3 -1] :pseudo-fixture true
                    :beam-angle angle :lens-radius-m 0.1 :universe 1 :address address
                    :channels {:pan [1 2] :tilt [3 4] :red 5 :green 6 :blue 7}})
                 [:one :two] [-2 1] [1 12] [25 22]))
(defn- edge-x [spot sign]
  (+ (first (:center spot))
     (* sign (Math/hypot (* (:major-radius-m spot) (first (:major-axis spot)))
                         (* (:minor-radius-m spot) (first (:minor-axis spot)))))))

(deftest surface-aim-updates-the-physical-follower-and-publishes-its-reflection
  (doseq [batch? [true false]]
    (let [rig {:fixtures heads :room {:width 25 :depth 25 :height 6}}
          rt (show/create-runtime {:model (assoc (demo/model) :rig rig) :batch? batch?})
          aim! (fn [x t]
                 (let [p (show/submit! rt :surface-aim {:targets [:one :two] :surface-id :floor
                                                        :mode :edge-to-edge :point [x 0 1] :along :u})]
                   (show/tick! rt t)
                   (is (= :accepted (:status @p)) (str @p))
                   (let [out (:output (studio/browser-snapshot rt {}))
                         a (get-in out [:values :one :beam-footprint])
                         b (get-in out [:values :two :beam-footprint])]
                     (is (= :floor (:surface-id a) (:surface-id b)))
                     (is (vector? (get-in out [:edge-tracking 0 :contact])))
                     (when (and a b) (is (< (abs (- (edge-x a 1) (edge-x b -1))) 1e-5)))
                     (get-in out [:values :two :pose :direction]))))]
      (try (is (not= (aim! -2 25) (aim! 1 50)))
           (finally (show/stop! rt))))))

(deftest fixed-light-footprints-use-mount-and-remain-opt-in
  (let [fixture {:id :strip :position [0 3 0] :beam-angle 20 :pseudo-fixture true}
        rig {:fixtures [fixture (assoc fixture :id :ordinary :pseudo-fixture false)]}
        plan (reflections/plan rig)
        rows (reflections/with-footprints plan {:strip {} :ordinary {}})]
    (is (= :floor (get-in rows [:strip :beam-footprint :surface-id])))
    (is (= {} (:ordinary rows)))))
