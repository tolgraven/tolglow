(ns tolglow.aim-paths-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [tolglow.aim-paths :as paths]
            [tolglow.compiler :as compiler]
            [tolglow.composition :as composition]
            [tolglow.event-subscribers :as sub]
            [tolglow.spatial :as spatial]
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
        duration (get-in effect [:editor :arrival-duration :ms])]
    (is (empty? (registry/validate-definition :effects effect)))
    (is (near? 2000.0 duration))
    (is (= {:ms duration} (:duration effect)) "Timed and one-shot slots use the solved interval")
    (is (= [[{:ms 0} {:ms duration}]]
           (into [] (comp (map #(mapv :at (get-in % [:value :spec :keyframes]))) (distinct)) (:outputs effect))))
    (doseq [[fixture start end] (map vector paired-heads [-45.0 -42.5] [45.0 2.5])]
      (let [[from to] (axis-endpoints effect (:id fixture) :pan)]
        (is (near? start (:pan-deg (frame/fixture-pose fixture {:pan from :tilt 0.5}))))
        (is (near? end (:pan-deg (frame/fixture-pose fixture {:pan to :tilt 0.5}))))))
    (is (= 3000.0 (get-in (paths/definition rig (assoc synchronized-spec :duration {:ms 3000}))
                          [:editor :arrival-duration :ms])))
    (is (near? 2000.0 (get-in (paths/definition rig (assoc synchronized-spec :duration {:ms 250}))
                            [:editor :arrival-duration :ms])))
    (let [rig (assoc-in rig [:rig :fixtures 1 :calibration :motor-rate-deg-per-sec :pan] 60)]
      (is (near? 1000.0 (get-in (paths/definition rig synchronized-spec) [:editor :arrival-duration :ms]))))))

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
    (is (near? 3000.0 (get-in effect [:editor :arrival-duration :ms])) "45 degrees of tilt at 15 degrees/s governs arrival")
    (is (near? 170.0 (:pan-deg (frame/fixture-pose head {:pan from :tilt 0.5}))))
    (is (near? 190.0 (:pan-deg (frame/fixture-pose head {:pan to :tilt 0.5}))))))

(deftest reopened-synchronized-path-recomputes-arrival-from-current-calibration
  (let [rig (assoc-in model [:rig :fixtures] paired-heads)
        effect (paths/definition rig synchronized-spec)
        reopened (merge synchronized-spec (:editor effect))
        faster (assoc-in rig [:rig :fixtures 1 :calibration :motor-rate-deg-per-sec :pan] 60)]
    (is (= {:beats 0} (get-in effect [:editor :duration])) "The editor keeps the requested minimum, not the solved interval")
    (is (= effect (paths/definition rig reopened)))
    (is (near? 1000.0 (get-in (paths/definition faster reopened) [:editor :arrival-duration :ms]))
        "Republishing saved editor settings after recalibration does not ratchet the old interval")
    (is (= {:ms 3000} (get-in (paths/definition rig (assoc synchronized-spec :duration {:ms 3000})) [:editor :duration])))))

(deftest synchronized-aim-path-rejects-incomplete-calibration-and-unsupported-paths
  (let [rig (assoc-in model [:rig :fixtures] paired-heads)]
    (doseq [spec [(assoc synchronized-spec :mode :ping-pong)
                  (assoc synchronized-spec :closed? true)
                  (assoc synchronized-spec :curve :sine)
                  (assoc synchronized-spec :points [[-1 0 0] [0 1 0] [1 0 0]])]]
      (is (thrown? clojure.lang.ExceptionInfo (paths/definition rig spec))))
    (doseq [invalid [nil 0 -1 ##Inf ##NaN]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"positive calibrated pan and tilt rates"
                            (paths/definition (assoc-in rig [:rig :fixtures 1 :calibration :motor-rate-deg-per-sec :pan] invalid)
                                              synchronized-spec))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"need mechanical travel"
                          (paths/definition (assoc-in model [:rig :fixtures] [(first paired-heads)])
                                            (assoc synchronized-spec :targets [:far-travel] :duration {:ms 500}
                                                   :points [[0 0 1] [0 0 3]])))
        "A positive minimum does not admit a move in which no head travels")
    (is (near? 2000.0 (get-in (paths/definition rig (assoc synchronized-spec :points [[-1 0 0] [-1 0 0] [1 0 0]]))
                              [:editor :arrival-duration :ms]))
        "Repeated points collapse before the two-point check")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"cannot reach this aim point"
                          (paths/definition (assoc-in rig [:rig :fixtures 0 :calibration :pan] {:min-deg -50 :max-deg 20})
                                            synchronized-spec)))))

(def transport {:bpm 120 :playing? true :beat 0.0 :meter {:beats-per-bar 4 :bars-per-phrase 4}})

(defn- registered [rig effect]
  (-> rig
      (assoc-in [:registry :effects (:id effect)] effect)
      (assoc-in [:registry :cues :paired-cue] {:id :paired-cue :effect-id (:id effect)})))

(def launch {:op :cue/launch :cue-id "paired-cue"})

(deftest musical-minimum-uses-the-show-tempo-and-motor-rates-remain-a-bound
  (let [rig (-> model (assoc-in [:rig :fixtures] paired-heads) (assoc :transport transport))
        bars (paths/definition rig (assoc synchronized-spec :duration {:bars 2}))
        beat (paths/definition rig (assoc synchronized-spec :duration {:beats 1}))]
    (is (= {:bars 2} (get-in bars [:editor :arrival-duration]) (:duration bars))
        "Two bars at 120 BPM outlast the 2 s motor limit, so the move stays musical")
    (is (near? 4000.0 (get-in bars [:editor :arrival-duration-ms])))
    (is (= [[{:bars 0} {:bars 2}]]
           (into [] (comp (map #(mapv :at (get-in % [:value :spec :keyframes]))) (distinct)) (:outputs bars))))
    (is (near? 2000.0 (get-in beat [:editor :arrival-duration :ms])) "One beat is shorter than the motor limit")
    (is (nil? (paths/launch-problem (registered rig bars) launch nil)))
    (is (= :tempo-exceeds-motor-rate
           (first (paths/launch-problem (assoc-in (registered rig bars) [:transport :bpm] 480) launch nil))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"finite, positive show tempo"
                          (paths/definition (assoc-in rig [:transport :bpm] 0) (assoc synchronized-spec :duration {:bars 2}))))))

(deftest launch-refuses-a-path-solved-against-another-mount-or-calibration
  (let [rig (assoc-in model [:rig :fixtures] paired-heads)
        effect (paths/definition rig synchronized-spec)
        model (registered rig effect)]
    (is (= #{"far-travel" "slow-motor"} (set (keys (get-in effect [:editor :rig-fingerprint])))))
    (is (nil? (paths/launch-problem model launch nil)))
    (is (nil? (paths/launch-problem (assoc-in model [:rig :fixtures 0 :calibration :motor-rate-deg-per-sec :pan] 90.0) launch nil))
        "A JSON round trip of the same numbers keeps the fingerprint")
    (is (nil? (paths/launch-problem (assoc-in model [:rig :fixtures 0 :address] 1) launch nil))
        "Repatching addresses does not change the solved angles")
    (doseq [changed [(assoc-in model [:rig :fixtures 1 :calibration :motor-rate-deg-per-sec :pan] 60)
                     (assoc-in model [:rig :fixtures 1 :rotation-deg] [0 25 0])
                     (assoc-in model [:rig :fixtures 0 :position] [0 0 -2])
                     (assoc-in model [:rig :fixtures 0 :calibration :pan] {:min-deg -270 :max-deg 270})
                     (update-in model [:rig :fixtures] (comp vec rest))]]
      (is (= :stale-calibration (first (paths/launch-problem changed launch nil)))))
    (is (nil? (paths/launch-problem (assoc-in model [:registry :effects :paired :editor :type] :aim-path) launch nil)))))

(deftest start-wrap-follows-the-committed-pose-and-launch-refuses-a-mismatch
  (let [head (-> (first paired-heads)
                 (assoc :position [0 0 0])
                 (assoc-in [:calibration :pan] {:min-deg -270 :max-deg 270}))
        rig (assoc-in model [:rig :fixtures] [head])
        point (fn [yaw y] (let [r (Math/toRadians yaw)] [(Math/sin r) y (Math/cos r)]))
        spec (assoc synchronized-spec :targets [:far-travel] :points [(point -170 0) (point -150 0)])
        pan-of (fn [degrees] (/ (+ degrees 270.0) 540.0))
        committed {:far-travel {:pan (pan-of 190.0) :tilt 0.5}}
        start-deg (fn [effect] (:pan-deg (frame/fixture-pose head {:pan (first (axis-endpoints effect :far-travel :pan)) :tilt 0.5})))
        neutral (paths/definition rig spec)
        from-pose (paths/definition rig spec committed)]
    (is (near? -170.0 (start-deg neutral)) "Without history the start wrap is nearest neutral")
    (is (near? 190.0 (start-deg from-pose)) "A committed pose at +190 degrees selects the matching wrap")
    (is (near? 210.0 (:pan-deg (frame/fixture-pose head {:pan (second (axis-endpoints from-pose :far-travel :pan)) :tilt 0.5}))))
    (is (near? (get-in neutral [:editor :arrival-duration :ms]) (get-in from-pose [:editor :arrival-duration :ms])))
    (is (= :start-wrap-mismatch (first (paths/launch-problem (registered rig neutral) launch committed)))
        "Launching the -170 degree start from +190 degrees would command a full-turn jump")
    (is (nil? (paths/launch-problem (registered rig from-pose) launch committed)))
    (is (nil? (paths/launch-problem (registered rig neutral) launch {:far-travel {:pan (pan-of -100.0) :tilt 0.5}}))
        "A head that is merely not yet prepositioned is not a wrap mismatch")
    (is (nil? (paths/launch-problem (registered rig neutral) launch nil)))))

(defn- stale-show
  "A real control model with a published synchronized cue whose slow head was
  recalibrated after publication."
  []
  (let [rig (control/new-show {:rig {:fixtures paired-heads}})
        effect (paths/definition rig synchronized-spec)]
    (-> rig
        (assoc-in [:registry :effects (:id effect)] effect)
        (assoc-in [:registry :cues :paired-cue] {:id :paired-cue :effect-id (:id effect) :version 1})
        (assoc-in [:rig :fixtures 1 :calibration :motor-rate-deg-per-sec :pan] 60))))

(deftest every-spatial-launch-route-refuses-a-stale-synchronized-cue
  (let [model (stale-show)
        result (spatial/apply-command model {:op :cue/launch :cue-id :paired-cue :command-id "direct"
                                             :show-id (:show-id model) :epoch (:epoch model) :time-ms 0
                                             :source {:adapter :test}})]
    (is (= [:rejected :stale-calibration] ((juxt :status :code) (:ack result))))
    (is (empty? (:instances (:model result)))))
  (let [model (composition/save (stale-show) {:id :run :mode :sequence :timebase :ms
                                              :slots [{:cue-id :paired-cue :duration {:ms 500}}]})
        advanced (composition/advance (composition/start model :run 0) 10)
        [fault] (:composition-faults advanced)]
    (is (empty? (:instances advanced)) "The sequence slot does not launch the stale cue")
    (is (= [:composition/launch-refused :stale-calibration :paired-cue] ((juxt :type :code :cue-id) fault)))
    (is (contains? (:composition-runs advanced) :run) "A refused slot faults without stopping the run"))
  (let [model (sub/configure (stale-show)
                             {:op :save :subscriber {:id :kicks :enabled? true :match {:type :audio/kick}
                                                     :actions [{:op :cue/launch :select {:ids [:paired-cue]}}]}})
        admitted (sub/admit model [{:category :audio :type :audio/kick :emitter {:kind :audio :id :analysis}
                                    :data {} :time {:beat 0}}])
        advanced (sub/advance (:model admitted) 0 {})]
    (is (empty? (:instances advanced)))
    (is (re-find #"calibration changed" (str (get-in advanced [:event-subscribers :last-error :message]))))))
