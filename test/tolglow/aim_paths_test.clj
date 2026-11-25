(ns tolglow.aim-paths-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.render.frame :as frame]
            [tolglow.aim-paths :as paths]
            [tolglow.compiler :as compiler]
            [tolglow.registry :as registry]
            [tolglow.composition.tween :as tween]))

(def model {:rig {:fixtures [{:id :mover :channels {:pan [1 2] :tilt [3 4]}}
                             {:id "rgbw" :channels {:red 1 :green 2 :blue 3 :white 4}}]}
            :registry {:effects {} :versions {}}})

(def path-spec {:id :sweep :targets ["mover"] :points [[0 0 0] [1 0 0] [1 0 3]]
                :duration {:beats 4} :curve :linear :mode :loop})

(deftest musical-distance-spaced-paths-use-the-existing-pure-tween
  (let [effect (paths/definition model path-spec)
        spec (get-in effect [:outputs :aim :spec])
        sample (tween/compile-tween spec)]
    (is (empty? (registry/validate-definition :effects effect)))
    (is (= [:mover] (:targets effect)))
    (is (= [{:beats 0.0} {:beats 1.0} {:beats 4.0}] (mapv :at (:keyframes spec))))
    (is (= [1.0 0.0 1.0] (:value (sample 2))))
    (is (= [0.0 0.0 0.0] (:value (sample 4))))
    (is (nil? (:duration effect)))))

(deftest path-effects-execute-through-the-real-compiler-and-captured-meter
  (let [effect (paths/definition model (assoc path-spec :duration {:bars 1} :mode :once))
        step (compiler/compile-effect effect)
        result (step {:frame {:time-ms 1000 :beat 2 :fixtures (get-in model [:rig :fixtures])}
                      :instance {:started-at-ms 0 :started-at-beat 0 :timing-meter {:beats-per-bar 4 :bars-per-phrase 4} :params {}}
                      :state {} :assignments [] :intents []})]
    (is (= {:bars 1} (:duration effect)))
    (is (= [{:target-id :mover :attribute :aim :value [1.0 0.0 1.0] :blend :replace}] (:assignments result)))))

(deftest closed-and-ping-pong-paths-retain-portable-editor-data
  (let [closed (paths/definition model (assoc path-spec :closed? true))
        reverse (paths/definition model (assoc path-spec :mode :ping-pong))]
    (is (= (:points path-spec) (get-in closed [:editor :points])))
    (is (= (first (:points path-spec)) (:value (peek (get-in closed [:outputs :aim :spec :keyframes])))))
    (is (= [1.0 0.0 1.0] (:value ((tween/compile-tween (get-in reverse [:outputs :aim :spec])) 6))))
    (is (registry/data? closed))))

(deftest maximum-size-closed-path-remains-editable
  (let [spec (assoc path-spec :closed? true :points (mapv #(vector % 0 0) (range 256)))
        effect (paths/definition model spec)]
    (is (= 256 (count (get-in effect [:editor :points]))))
    (is (= 257 (count (get-in effect [:outputs :aim :spec :keyframes]))))
    (is (= effect (paths/definition model (merge spec (:editor effect)))))))

(deftest invalid-paths-and-stale-publication-are-rejected-before-effects-exist
  (doseq [spec [(assoc path-spec :points [[1 1 1] [1 1 1]])
                (assoc path-spec :points [[0 0 0] [##NaN 0 0]])
                (assoc path-spec :targets [:rgbw])
                (assoc path-spec :targets [])
                (assoc path-spec :duration {:beats 0})
                (assoc path-spec :mode :random)]]
    (is (thrown? clojure.lang.ExceptionInfo (paths/definition model spec))))
  (let [first (paths/definition model path-spec)
        updated (assoc-in model [:registry :effects :sweep] first)]
    (is (= 2 (:version (paths/definition updated (assoc path-spec :expected-version 1)))))
    (is (thrown? clojure.lang.ExceptionInfo (paths/definition updated (assoc path-spec :expected-version 0))))
    (is (thrown? clojure.lang.ExceptionInfo (paths/definition (assoc-in updated [:registry :effects :sweep :editor] nil) path-spec)))))

(def paired-heads
  [{:id :far-travel :universe 1 :address 511 :position [0 0 -1]
    :channels {:pan [1 3] :tilt [2 4]}
    :calibration {:motor-rate-deg-per-sec {:pan 90 :tilt 120}}}
   {:id :slow-motor :universe 2 :address 5 :position [0 0 (- (+ 1 (Math/sqrt 2)))]
    :rotation-deg [0 20 0] :channels {:pan [1 2] :tilt [3 4]}
    :calibration {:motor-rate-deg-per-sec {:pan 22.5 :tilt 45}}}])

(def synchronized-spec
  {:id :paired :targets [:far-travel :slow-motor] :points [[-1 0 0] [1 0 0]] :sync-arrival? true})

(defn- near? [expected actual]
  (< (abs (- expected actual)) 1.0e-9))

(defn- axis-endpoints [effect id axis]
  (some (fn [{:keys [attribute targets value]}]
          (when (and (= axis attribute) (= [id] targets))
            (mapv :value (get-in value [:spec :keyframes]))))
        (:outputs effect)))

(deftest calibrated-arrival-uses-angular-travel-and-the-rate-limiting-head
  (let [rig (assoc-in model [:rig :fixtures] paired-heads)
        effect (paths/definition rig synchronized-spec)
        duration (get-in effect [:editor :duration :ms])]
    (is (empty? (registry/validate-definition :effects effect)))
    (is (near? 2000.0 duration))
    (is (nil? (:duration effect)) "The active cue holds the endpoint after one traversal")
    (is (= [[{:ms 0} {:ms duration}]]
           (into [] (comp (map #(mapv :at (get-in % [:value :spec :keyframes]))) (distinct)) (:outputs effect))))
    (doseq [[fixture start end] (map vector paired-heads [-45.0 -42.5] [45.0 2.5])]
      (let [[from to] (axis-endpoints effect (:id fixture) :pan)]
        (is (near? start (:pan-deg (frame/fixture-pose fixture {:pan from :tilt 0.5}))))
        (is (near? end (:pan-deg (frame/fixture-pose fixture {:pan to :tilt 0.5}))))))
    (is (= 3000.0 (get-in (paths/definition rig (assoc synchronized-spec :duration {:ms 3000}))
                          [:editor :duration :ms])))
    (is (near? 2000.0 (get-in (paths/definition rig (assoc synchronized-spec :duration {:ms 250}))
                            [:editor :duration :ms])))
    (let [rig (assoc-in rig [:rig :fixtures 1 :calibration :motor-rate-deg-per-sec :pan] 60)]
      (is (near? 1000.0 (get-in (paths/definition rig synchronized-spec) [:editor :duration :ms]))))))

(deftest calibrated-arrival-uses-the-selected-mechanical-wrap-and-all-axes
  (let [head (-> (first paired-heads)
                 (assoc :position [0 0 0])
                 (assoc-in [:calibration :pan] {:min-deg -270 :max-deg 270})
                 (assoc-in [:calibration :motor-rate-deg-per-sec :tilt] 15))
        rig (assoc-in model [:rig :fixtures] [head])
        point (fn [yaw y] (let [r (Math/toRadians yaw)] [(Math/sin r) y (Math/cos r)]))
        effect (paths/definition rig (assoc synchronized-spec :targets [:far-travel]
                                             :points [(point 170 0) (point -170 1)]))
        [from to] (axis-endpoints effect :far-travel :pan)]
    (is (near? 3000.0 (get-in effect [:editor :duration :ms])) "45 degrees of tilt at 15 degrees/s governs arrival")
    (is (near? 170.0 (:pan-deg (frame/fixture-pose head {:pan from :tilt 0.5}))))
    (is (near? 190.0 (:pan-deg (frame/fixture-pose head {:pan to :tilt 0.5}))))))

(deftest synchronized-aim-path-rejects-incomplete-calibration-and-unsupported-paths
  (let [rig (assoc-in model [:rig :fixtures] paired-heads)]
    (doseq [spec [(assoc synchronized-spec :mode :ping-pong)
                  (assoc synchronized-spec :closed? true)
                  (assoc synchronized-spec :curve :sine)
                  (assoc synchronized-spec :duration {:beats 1})
                  (assoc synchronized-spec :points [[-1 0 0] [0 1 0] [1 0 0]])]]
      (is (thrown? clojure.lang.ExceptionInfo (paths/definition rig spec))))
    (doseq [invalid [nil 0 -1 ##Inf ##NaN]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"positive calibrated pan and tilt rates"
                            (paths/definition (assoc-in rig [:rig :fixtures 1 :calibration :motor-rate-deg-per-sec :pan] invalid)
                                              synchronized-spec))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"cannot reach this aim point"
                          (paths/definition (assoc-in rig [:rig :fixtures 0 :calibration :pan] {:min-deg -50 :max-deg 20})
                                            synchronized-spec)))))
