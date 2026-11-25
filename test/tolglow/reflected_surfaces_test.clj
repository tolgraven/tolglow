(ns tolglow.reflected-surfaces-test
  (:require [afterglow.control.model :as model]
            [clojure.test :refer [deftest is]]
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

(deftest a-rejected-follower-cuts-through-output-response-and-restarts-from-dark
  (let [moving (mapv (fn [i id]
                       (assoc (first heads) :id id :position [i 3 0] :beam-angle 20 :address (+ 1 (* 11 i))))
                     (range) [:one :two :three])
        fixtures (conj moving {:id :outside :universe 1 :address 34 :channels {:dimmer 1}})
        rig {:fixtures fixtures :room {:width 20 :depth 20 :height 6}}
        registry {:effects {:beam {:id :beam :name "Beam" :version 1 :params {}
                                  :targets [:one :two :three] :outputs {:color [1.0 0.4 0.2]}}
                            :lamp {:id :lamp :name "Lamp" :version 1 :params {}
                                   :targets [:outside] :outputs {:dimmer 1.0}}}
                  :cues {:beam {:id :beam :effect-id :beam :version 1}
                         :lamp {:id :lamp :effect-id :lamp :version 1}}}
        envelope {:attack {:mode :linear :ms 100} :release {:mode :linear :ms 400}}]
    (doseq [batch? [true false]
            scope [{:global envelope} {:fixtures {:three envelope :outside envelope}}]]
      (let [model (assoc (model/new-show {:registry registry}) :rig rig
                         :settings {:output-response (assoc scope :enabled? true)})
            packet (atom nil)
            runtime (show/create-runtime {:model model :batch? batch? :workspace-options nil
                                          :sink {:send! (fn [universes _] (reset! packet (vec (get universes 1))))}})
            submit! (fn [op payload t]
                      (let [ack (show/submit! runtime op payload)]
                        (show/tick! runtime t)
                        (is (= :accepted (:status @ack)) (str @ack))
                        @ack))
            aim! (fn [id t]
                   (submit! :surface-aim {:targets [:one id] :surface-id :floor
                                         :mode :edge-to-edge :point [0 0 0] :along :u} t))
            preview (fn [] (:output (studio/browser-snapshot runtime {})))
            level (fn [slot] (bit-and 255 (nth @packet slot)))]
        (try
          (submit! :command {:op :cue/launch :cue-id :beam} 0)
          (let [lamp (:instance-id (submit! :command {:op :cue/launch :cue-id :lamp} 25))]
            (aim! :two 125)
            (is (= [255 255] (mapv level [26 33])))
            (submit! :command {:op :instance/remove :instance-id lamp} 150)
            (is (= 239 (level 33)))
            (aim! :three 200)
            (is (= [nil :overlap] (mapv :reason (:edge-tracking (preview)))))
            (is (= [0.0 0.0 0.0] (get-in (preview) [:values :three :emitted-color])))
            (is (= [255 255 0 207] (mapv level [4 15 26 33])))
            (show/tick! runtime 225)
            (is (= [0 191] (mapv level [26 33])))
            (submit! :surface-aim {:mode :release :targets [:two]} 250)
            (is (every? :contact (:edge-tracking (preview))))
            (is (= [64 175] (mapv level [26 33])))
            (show/tick! runtime 325)
            (is (= 255 (level 26))))
          (finally (show/stop! runtime)))))))
