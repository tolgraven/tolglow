(ns tolglow.aim-paths-test
  (:require [clojure.test :refer [deftest is]]
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
    (is (= (first (:points (:editor closed))) (peek (:points (:editor closed)))))
    (is (= [1.0 0.0 1.0] (:value ((tween/compile-tween (get-in reverse [:outputs :aim :spec])) 6))))
    (is (registry/data? closed))))

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
