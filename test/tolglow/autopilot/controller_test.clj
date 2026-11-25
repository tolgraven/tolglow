(ns tolglow.autopilot.controller-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.autopilot.controller :as control]
            [tolglow.autopilot.scope :as scope]
            [tolglow.autopilot.statistics :as stats]))

(defn- inputs [beat levels brightness activity]
  {:clock {:beat beat :time-ms (* 500.0 beat) :playing? true}
   :epoch 4 :targets {:brightness 0.5 :activity 0.4}
   :sources (mapv (fn [id level] {:id id :level level :capacity 1.0}) (range) levels)
   :statistics {:window {:bars 1} :brightness {:mean brightness :instant brightness}
                :activity {:mean activity :instant activity}}
   :options {:strategy :instant :rotate? false}})

(defn- emitted [sources gains]
  (mapv (fn [{:keys [id level]}] (min 1.0 (* level (get gains id 1.0)))) sources))

(defn- near? [x y] (< (abs (- x y)) 1e-8))

(deftest excess-light-shared-by-physical-sources
  (let [in (inputs 0 [1.0 1.0 0.4 0.4] 0.7 0.61)
        out (control/advance nil in)]
    (is (= [1.0 1.0 0.0 0.0] (emitted (:sources in) (:gains out))))
    (is (near? 0.5 (get-in out [:report :estimated-brightness])))
    (is (= 2 (get-in out [:report :active-count])))
    (is (= [{:op :adjust :domain :activity :direction :down}] (mapv #(select-keys % [:op :domain :direction]) (:actions out))))
    (is (near? 0.21 (get-in out [:actions 0 :amount])))
    (is (= [0 1] (get-in out [:actions 0 :lit-fixture-ids]))))
  (let [in (-> (inputs 0 [1.0 1.0 1.0 1.0] 1.0 0.4)
               (assoc-in [:options :standby-level] 0.04)
               (assoc-in [:sources 0 :capacity] 3.0))
        out (control/advance nil in)
        levels (emitted (:sources in) (:gains out))]
    (is (every? #(<= 0.039999 % 0.041) (rest levels)))
    (is (near? 0.96 (first levels)))
    (is (near? 0.5 (get-in out [:report :estimated-brightness])))))

(deftest overdrive-and-remaining-action-respect-output-capacity
  (let [in (-> (inputs 0 [0.6] 0.6 0.4) (assoc-in [:targets :brightness] 1.0))
        out (control/advance nil in)]
    (is (near? (/ 1.0 0.6) (get (:gains out) 0)))
    (is (= [1.0] (emitted (:sources in) (:gains out))))
    (is (empty? (:actions out)))
    (let [capped (control/advance nil (assoc-in in [:options :max-gain] 1.25))]
      (is (= 1.25 (get (:gains capped) 0)))
      (is (= :up (get-in capped [:actions 0 :direction])))
      (is (near? 0.25 (get-in capped [:actions 0 :amount])))))
  (let [in (assoc-in (inputs 0 [0.0 0.0] 0.0 0.0) [:options :preference] :generate)
        ops (:actions (control/advance nil (assoc in :cue-ids [:wash])))]
    (is (= [:generate] (mapv :op ops)))
    (is (= :brightness (:domain (first ops))))
    (is (= [:wash] (:cue-ids (first ops))))))

(deftest sustained-average-and-lag-aware-adjustments
  (testing "Downward changes stay at the feasible target while its old light fades from a four-beat window."
    (loop [beat 0 state nil]
      (when (<= beat 8)
        (let [avg (+ 0.5 (* 0.2 (max 0.0 (- 1.0 (/ beat 4.0)))))
              in (inputs beat [1.0 1.0 0.4 0.4] avg 0.4)
              out (control/advance state in)]
          (is (near? 0.5 (get-in out [:report :estimated-brightness])))
          (recur (inc beat) (:state out))))))
  (testing "A short, full-output pulse may meet a low averaged target."
    (let [in (-> (inputs 4 [1.0] 0.125 0.4)
                 (assoc-in [:statistics :brightness :instant] 1.0)
                 (assoc-in [:targets :brightness] 0.125))
          out (control/advance nil in)]
      (is (= [1.0] (emitted (:sources in) (:gains out))))
      (is (empty? (:actions out)))))
  (testing "Unreachable demand cannot accumulate fictitious gains."
    (loop [beat 0 state nil]
      (when (< beat 12)
        (let [in (-> (inputs beat [0.1] (if (zero? beat) 0.1 0.2) 0.4)
                     (assoc-in [:targets :brightness] 0.9))
              out (control/advance state in)]
          (is (near? 2.0 (get (:gains out) 0)))
          (is (< (get-in out [:actions 0 :amount]) 1.0))
          (recur (inc beat) (:state out)))))))

(deftest musical-cadence-seeded-rotation-and-operator-changes
  (let [in (-> (inputs 0 [1.0 1.0 1.0 1.0] 1.0 0.4)
               (assoc :options {:strategy :instant :rotate? true :interval {:beats 1/2} :seed 13}))
        step (fn [s beat] (control/advance s (assoc in :clock {:beat beat :time-ms (* beat 500.0)})))
        a (step nil 0) b (step (:state a) 0.4) c (step (:state b) 0.5)
        paused (control/advance (:state c) (assoc-in in [:clock :playing?] false))]
    (is (false? (:due? b)))
    (is (identical? (:state a) (:state b)))
    (is (empty? (:actions b)))
    (is (= (:state a) (:state (step nil 0))))
    (is (not= (get-in a [:state :order]) (get-in c [:state :order])))
    (is (false? (:due? paused)))
    (is (control/due? (:state c) {:beat 0 :time-ms 0} 4 (:options in)))
    (is (control/due? (:state c) {:beat 0.55 :time-ms 275} 5 (:options in)))
    (is (near? 2.5 (get-in (step (:state c) 2.7) [:state :timer :started-at]))))
  (let [in (-> (inputs 0 [1.0 1.0] 1.0 0.4)
               (assoc :options {:step 0.1 :rotate? false}))
        out (control/advance nil in)]
    (is (near? 0.9 (get-in out [:report :estimated-brightness])))
    (is (thrown? Exception (control/settings {:interval {:beats 0}})))))

(deftest page-group-and-explicit-scope-use-patched-targets
  (let [fixture (fn [id g ch] {:id id :groups [g] :channels ch})
        red (fn [id ids] {:id id :outputs [{:attribute :red :targets ids :value 1.0}]})
        model {:rig {:fixtures [(fixture :a :front {:red 1}) (fixture :b :front {:dimmer 1})
                                (fixture :c :back {:red 1}) (fixture :smoke :back {:fog 1})]}
               :pages {:front {:cells {[0 0] :red [0 1] :pinned}} :empty {:cells {}}}
               :registry {:cues {:red {:effect-id :red} :pinned {:effect-id :old :effect-version 1}}
                          :effects {:red (assoc (red :red [:a :b :c]) :targets [:a :b])
                                    :old {:id :old :version 2 :outputs {:dimmer 1.0}}}
                          :versions {[:effects :old 1] (red :old [:c])}}}]
    (is (= [:a :b :c] (control/scope-ids model :global)))
    (is (= [:a :b] (control/scope-ids model {:group-id :front})))
    (is (= [:a :c] (control/scope-ids model {:page-id :front})))
    (is (= [:b] (control/scope-ids model {:fixture-ids [:b :smoke]})))
    (is (empty? (control/scope-ids model {:page-id :empty})))
    (is (thrown? Exception (control/scope-ids model {:page-id :missing})))
    (is (thrown? Exception (control/scope-ids model {:group-id :missing})))
    (is (thrown? Exception (control/scope-ids model {:fixture-ids [:missing]})))
    (is (<= 1 (count (scope/page-cue-ids model :front))))))

(deftest rotating-look-changes-membership-and-zero-step-holds-the-mix
  (let [in (assoc (inputs 0 [1.0 1.0 1.0 1.0] 1.0 0.4) :options {:strategy :instant :seed 17})
        a (control/advance nil in)
        ; The averaging window has filled with the already-corrected level.
        follow (-> in (assoc :clock {:beat 4 :time-ms 2000}) (assoc-in [:statistics :brightness :mean] 0.5))
        b (control/advance (:state a) follow)
        hold (control/advance (:state b) (-> follow (assoc :clock {:beat 5 :time-ms 2500})
                                             (assoc :options {:step 0 :strategy :gradual})
                                             (assoc :targets {:brightness 0.1 :activity 0.9})))]
    (is (not= (get-in a [:state :active]) (get-in b [:state :active])))
    (is (near? 0.5 (get-in b [:report :estimated-brightness])))
    (is (= (:gains b) (:gains hold)))
    (is (empty? (:actions hold)))))

(deftest closes-the-feedback-loop-over-measured-musical-time
  (let [in (inputs 0 [1.0 1.0 0.4 0.4] 0.7 0.61)
        target (:targets in)
        measure (fn [state beat values]
                  (stats/advance state {:beat beat :time-ms (* 500 beat) :epoch 4}
                                 {:bars 1} values target 0.001))
        start {:brightness 0.7 :activity 0.61}
        old (measure nil -4 start)]
    (loop [i 0 s nil history (:state old) values start requests []]
      (let [beat (/ i 8)
            measured (measure history beat values)
            out (control/advance s (assoc in :clock {:beat beat :time-ms (* beat 500)}
                                            :statistics (:report measured)))
            next-values {:brightness (get-in out [:report :estimated-brightness])
                         :activity (reduce (fn [v {:keys [domain direction amount]}]
                                             (if (= domain :activity)
                                               ((if (= :up direction) + -) v amount) v))
                                           (:activity values) (:actions out))}
            fresh (measure (:state measured) beat next-values)
            requests (into requests (:actions out))]
        (is (near? 0.5 (:brightness next-values)))
        (is (near? 0.4 (:activity next-values)))
        (if (< i 40)
          (recur (inc i) (:state out) (:state fresh) next-values requests)
          (do
            (is (near? 0.5 (get-in fresh [:report :brightness :mean])))
            (is (near? 0.4 (get-in fresh [:report :activity :mean])))
            (is (= 1 (count requests)))))))))

(deftest warming-window-uses-its-actual-coverage
  (let [in (inputs 0 [0.75 0.75] 0.75 0.4)
        initial (assoc-in in [:statistics :coverage] 0.0)
        a (control/advance nil initial)
        partial (-> in (assoc :clock {:beat 1 :time-ms 500})
                    (assoc :statistics {:window {:bars 1} :coverage 0.25
                                        :brightness {:mean 0.5} :activity {:mean 0.4}}))
        b (control/advance (:state a) partial)]
    (is (near? 0.5 (get-in a [:report :estimated-brightness])))
    (is (near? 0.5 (get-in b [:report :estimated-brightness])))
    (is (empty? (:actions b)))))

(deftest pending-launch-prevents-firing-again-into-rolling-lag
  (let [in (-> (inputs 0 [0.0 0.0] 0.0 0.0)
               (assoc :targets {:brightness 0.5 :activity 0.0}))
        a (control/advance nil in)
        b (control/advance (:state a) (-> in (assoc :clock {:beat 1 :time-ms 500})
                                         (assoc :sources [{:id 0 :capacity 1 :level 0.5}
                                                          {:id 1 :capacity 1 :level 0.5}])
                                         (assoc-in [:statistics :brightness :mean] 0.125)))]
    (is (= [:launch] (mapv :op (:actions a))))
    (is (empty? (:actions b)))
    (is (near? 0.5 (get-in b [:report :estimated-brightness])))))

(deftest first-frame-matches-existing-light-before-a-window-exists
  (let [in (dissoc (inputs 0 [1.0 1.0 0.4 0.4] 0.7 0.61) :statistics)
        out (control/advance nil in)]
    (is (near? 0.7 (get-in out [:report :forecast :brightness])))
    (is (near? 0.5 (get-in out [:report :estimated-brightness])))
    (is (empty? (:actions out)))))

(deftest page-scope-skips-program-control-cells-and-channels-that-cannot-emit-its-color
  (let [model {:rig {:fixtures [{:id :red :channels {:red 1}}
                                {:id :amber :channels {:amber 1}}
                                {:id :virtual-white :rgbw-policy :extract-min :channels {:white 1}}]}
               :pages {:music {:cells {[0 0] :rgb [0 1] :toggle}}}
               :registry {:cues {:rgb {:effect-id :hue} :toggle {:effect-id :all :role :show-control}}
                          :effects {:hue {:outputs {:color [1 1 1]}}
                                    :all {:outputs {:dimmer 1}}}}}]
    (is (= [:red :virtual-white] (control/scope-ids model {:page-id :music})))))

(deftest configured-overshoot-allows-completing-a-source-only-within-the-headroom
  (let [in (assoc-in (inputs 0 [1.0 1.0] 1.0 0.4) [:targets :brightness] 0.45)
        level #(get-in (control/advance nil (assoc-in in [:options :overshoot] %)) [:report :estimated-brightness])]
    (is (near? 0.45 (level 0.0)))
    (is (near? 0.45 (level 0.04)))
    (is (near? 0.5 (level 0.06)))))

(deftest a-glowing-source-can-be-lit-without-counting-as-active
  (let [in (-> (inputs 0 [] 0.43 0.0)
               (assoc :sources [{:id :quiet :level 1e-12 :capacity 1/3}
                                {:id :bright :level 1/2 :capacity 2M}])
               (assoc-in [:targets :brightness] 1.0))
        out (control/advance nil in)]
    (is (= #{:bright} (get-in out [:state :active])))
    (is (= [:quiet :bright] (get-in out [:actions 0 :lit-fixture-ids])))
    (is (near? (/ (+ (* 2e-12 1/3) 2.0) (+ 1/3 2.0)) (get-in out [:report :estimated-brightness])))))

(deftest calibrated-allocation-preserves-seeded-show-playback
  (let [sources (mapv (fn [id level capacity] {:id id :level level :capacity capacity})
                      [:a :b :c :d :e] [1.0 0.75 0.5 0.9 0.1] [1.0 2.0 1.0 4.0 0.5])
        in (assoc (inputs 3 [] 0.60 0.50)
                  :options {:strategy :instant :seed 42}
                  :targets {:brightness 0.40 :activity 0.50}
                  :sources sources)
        state (:state (control/advance nil in))]
    (is (= 955863294 (:seed state)))
    (is (= [:c :d :e :a :b] (:order state)))
    (is (= #{:a :c :d :e} (:active state)))
    (is (= {:c 1.0 :d 1.0 :e 1.0 :a 0.2833333333333326 :b 0.0} (:gains state)))))
