(ns tolglow.effects.sampling-quality-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.effects.sampling-quality :as quality]
            [tolglow.effects.cadence :as cadence]))

(def dynamic {:op :floor :args [{:op :add :args [0.125 {:op :fract :args [{:op :beat}]}]}]})
(def static {:op :floor :args [{:op :position :axis :x}]})
(defn definition [expression] {:id :pulse :version 1 :outputs {:dimmer expression}})
(defn captured [defn] {:effect-id (:id defn) :effect-version (:version defn) :definition defn})

(deftest changing-values-in-a-separate-subtree-do-not-make-a-static-edge-dynamic
  (is (quality/transient-output? (definition dynamic)))
  (is (quality/transient-output? (definition {:op :fract :args [{:op :time}]})))
  (is (quality/transient-output? (definition {:op :tween :spec {:keyframes [{:at {:beats 0} :value 1 :curve :step}
                                                                              {:at {:beats 1} :value 0}]}})))
  (let [tween (fn [mode a b]
                (definition {:op :tween :spec {:mode mode :keyframes [{:value a} {:value b :curve :step}]}}))]
    (is (false? (quality/transient-output? (tween :once 0 1))) "The final keyframe has no outgoing segment")
    (is (quality/transient-output? (tween :loop 0 1)) "The loop seam jumps")
    (is (false? (quality/transient-output? (tween :ping-pong 0 1))) "A reversal has no value jump"))
  (is (false? (quality/transient-output? (definition {:op :add :args [{:op :beat} static]}))))
  (is (false? (quality/transient-output? (definition {:op :sin :args [{:op :bar}]})))))

(deftest analysis-is-bounded-by-live-definition-revisions
  (let [inspections (atom 0) inspect quality/transient-output?
        pulse (definition dynamic) wash (assoc (definition static) :id :wash)
        active {:a (captured pulse) :b (captured pulse) :c (captured wash)}
        ids (update-vals active (juxt :effect-id :effect-version))]
    (with-redefs [quality/transient-output? (fn [definition] (swap! inspections inc) (inspect definition))]
      (let [cache (quality/prepare nil 0 ids active)]
        (is (= 2 @inspections))
        (is (= #{:a :b} (:protected cache)))
        (is (every? #(identical? cache %) (repeatedly 400 (fn [] (quality/prepare cache 0 ids active)))))
        (is (= 2 @inspections))
        (let [revised (assoc wash :version 2)
              cache (quality/prepare cache 0 {:c [:wash 2]} {:c (captured revised)})]
          (is (= #{[:wash 2]} (set (keys (:definitions cache)))))
          (is (= 3 @inspections)))))))

(deftest pressure-slows-a-smooth-wash-and-keeps-the-costliest-flash-at-source-rate
  (let [runtime {:adaptive-keyframes {:enabled? true :pressure-frames 1 :smoothing 0.8 :min-cost-ms 0.1}
                 :frame-rate 40}
        flash (definition dynamic) wash (assoc (definition static) :id :wash)
        model {:epoch 0 :instances {:a (captured flash) :b (captured wash)}
               :registry {:effects {:pulse flash :wash wash}}}
        next (fn [before frame]
               (let [session (cadence/begin runtime before model)
                     stride (get-in before [:cadence :state :effects :b :stride] 1)]
                 (reset! (:samples session)
                         (cond-> {:a {:calls 1 :total-ms 8 :predictive? true}}
                           (zero? (mod frame stride)) (assoc :b {:calls 1 :total-ms 3 :predictive? true})))
                 {:cadence (cadence/finish session model 29.0)}))
        state (reduce next {} (range 48))
        intervals (get-in (cadence/begin runtime state model) [:inputs :keyframe-intervals])]
    (is (nil? (:a intervals)))
    (is (< 25 (get-in intervals [:b :ms])))))

(deftest tempo-edits-clip-the-next-frame-before-workers-run
  (let [wash (assoc (definition static) :id :wash :keyframes {:max-interval {:beats 1/8}})
        aim (assoc (definition static) :id :aim)
        model {:epoch 0 :transport {:bpm 120}
               :instances {:a (captured wash) :b (captured aim)}
               :registry {:effects {:wash wash :aim aim}}}
        runtime {:frame-rate 40 :adaptive-keyframes {:enabled? true :pressure-frames 1}}
        observe (fn [state _]
                  (let [s (cadence/begin runtime state model)]
                    (reset! (:samples s) {:a {:predictive? true :total-ms 8 :calls 1}
                                          :b {:predictive? true :total-ms 3 :calls 1}})
                    {:cadence (cadence/finish s model 35)}))
        warm (reduce observe {} (range 15))
        steady (cadence/begin runtime warm model)
        changed (cadence/begin runtime warm (assoc-in model [:transport :bpm] 240))]
    (is (identical? (get-in warm [:cadence :ceilings]) (:ceilings steady)))
    (is (= {:a {:ms 50.0} :b {:ms 200.0}} (get-in steady [:inputs :keyframe-intervals])))
    (is (= {:b {:ms 200.0}} (get-in changed [:inputs :keyframe-intervals])))
    (is (= {:a 1} (get-in changed [:ceilings :limits])))))
