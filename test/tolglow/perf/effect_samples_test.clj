(ns tolglow.perf.effect-samples-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.demo :as demo]
            [tolglow.perf.effect-samples :as samples]
            [tolglow.show :as show]))

(deftest reports-work-done-before-the-next-stride-takes-effect
  (let [row {:slot 142 :ready-at-ms 3500}
        entry {:version [:wash 1 1] :sampled-at 7 :evaluations 10 :stride 4}
        state {:model {:epoch 2}
               :cadence {:state {:epoch 2 :frame 7 :effects {"i" entry}}}}
        window {:unit :ms :span 50 :origin (double-array 12)}
        observed #(get-in (samples/capture row %) [:effect-sampling "i"])
        tween (-> state (assoc-in [:cadence :state :frame] 8)
                  (assoc-in [:keyframes "i"] window))]
    (is (= {:effect-id :wash :outcome :direct :evaluations 10 :next-stride 4} (observed state)))
    (is (= {:effect-id :wash :outcome :interpolated :evaluations 10 :next-stride 4
            :interval {:unit :ms :span 50}} (observed tween)))
    (is (= :endpoint (:outcome (observed (assoc-in state [:keyframes "i"] window)))))
    (is (= :forecast-fallback
           (:outcome (observed (assoc-in tween [:keyframes "i" :forecast-failed?] true)))))
    (is (= row (samples/capture row (assoc-in state [:model :epoch] 3))))
    (is (= {:slot 143} (samples/capture {:slot 143} state)))
    (is (= {:wash {:direct 1 :interpolated 1}}
           (samples/summarize [(samples/capture row state) {:slot 143} (samples/capture row tween)])))))

(deftest real-output-changes-between-observed-endpoints
  (let [effect {:id :ramp :version 1 :keyframes {:interval {:beats 1}}
                :outputs {:color [1 1 1] :dimmer {:op :beat}}}
        model (-> (demo/model)
                  (assoc-in [:registry :effects :ramp] effect)
                  (assoc-in [:registry :cues :ramp] {:id :ramp :version 1 :effect-id :ramp}))
        host (show/create-runtime {:model model :workspace-options nil :batch? true
                                   :adaptive-keyframes {:enabled? true :max-stride 1}})]
    (try
      (let [receipt (show/submit! host {:op :cue/launch :cue-id :ramp})
            observe (fn [at]
                      (show/tick! host at)
                      (let [state @(:state host)
                            id (:instance-id (deref receipt 1000 nil))
                            row (samples/capture {:ready-at-ms at} state)]
                        [(get-in row [:effect-sampling id])
                         (bit-and 255 (aget ^bytes (get-in state [:output :universes 1]) 0))]))
            [start dark] (observe 0)
            [tween brighter] (observe 125)]
        (is (= :endpoint (:outcome start)))
        (is (= :interpolated (:outcome tween)))
        (is (pos? (:evaluations start)))
        (is (= (:evaluations start) (:evaluations tween)))
        (is (< dark brighter)))
      (finally (show/stop! host)))))
