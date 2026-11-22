(ns tolglow.lighting-budget-test
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [clojure.test :refer [deftest is]]
            [tolglow.automation.policy :as auto]
            [tolglow.automation.prediction :as prediction]
            [tolglow.lighting-budget :as budget]
            [tolglow.lighting-capacity :as meter]
            [tolglow.registry :as registry]))

(defn- lamp [label address watts groups]
  (cond-> {:id label :universe 1 :address address :channels {:dimmer 1} :groups groups}
    watts (assoc :max-watts watts)))

(def lamps (mapv #(lamp (keyword (str "lamp" %)) % (* 10 %) (if (even? %) [:even] [:odd])) (range 1 10)))
(defn- setup [fixtures definitions settings]
  (let [reg (reduce (fn [r e]
                      (let [id (:id e)]
                        (-> r (registry/publish :effects (merge {:version 1} e))
                            (registry/publish :cues {:id id :effect-id id})))) (registry/new-registry) definitions)]
    (assoc (control/new-show {:show-id "lighting-budget" :registry reg})
           :rig {:fixtures fixtures} :fixtures fixtures :settings {:lighting-budget (budget/settings settings)})))

(defn- launch [m cue-id source now]
  (control/apply-command m {:op :cue/launch :show-id (:show-id m) :epoch (:epoch m)
                            :command-id (pr-str [cue-id source now]) :source source :time-ms now
                            :cue-id cue-id :launch {:mode :persistent}}))

(defn- simulate [model cue-ids]
  (let [patch (frame/compile-patch (:fixtures model))
        power (meter/prepare patch)
        cues (mapv #(get-in model [:registry :cues %]) cue-ids)
        ctx (budget/prepare-frame-context nil model patch power)
        raw (prediction/frame model patch cues 0 (:standby-block ctx))]
    {:raw raw :output (budget/apply-frame model patch power raw) :patch patch :meter power}))

(defn- dmx [frame] (mapv #(bit-and % 255) (get-in frame [:universes 1])))

(deftest simultaneous-cues-share-one-budget-with-dim-standby
  (let [definitions [{:id :pair :targets [:lamp1 :lamp2] :outputs {:dimmer 1}}
                     {:id :third :targets [:lamp3] :outputs {:dimmer 1}}
                     {:id :fourth :targets [:lamp4] :outputs {:dimmer 1}}]
        config {:enabled? true :global {:limit 0.1 :max-active 2 :standby-level 0.05
                                        :fixture-order [:lamp4 :lamp2]}}
        m (setup lamps definitions config)
        {raw :raw proportional :output} (simulate m [:pair :third :fourth])
        priority (:output (simulate (assoc-in m [:settings :lighting-budget :global :strategy] :priority)
                                    [:pair :third :fourth]))
        scope (get-in proportional [:lighting-budget :global])]
    (is (= [255 255 255 255 0 0 0 0 0] (subvec (dmx raw) 0 9)))
    (is (= 450.0 (:capacity-watts scope)))
    (is (= 100.0 (:requested-watts scope)))
    (is (<= 44.5 (:used-watts scope) (:limit-watts scope) 45.0))
    (is (= [2 2 9] (mapv scope [:active-count :shed-count :source-count])))
    (is (= [12 182 12 182 0 0 0 0 0] (subvec (dmx proportional) 0 9)))
    (is (= 255 (nth (dmx priority) 3)))
    (is (< 12 (nth (dmx priority) 1) 182))
    (is (= 255 (nth (dmx raw) 1)) "Budgeting does not mutate the original published buffers")))

(deftest overlapping-budget-scopes-and-clipped-standby
  (let [rig [(lamp :a 1 10 [:front :left]) (lamp :b 2 20 [:front]) (lamp :c 3 30 [:left])]
        m (setup rig [{:id :all :outputs {:dimmer 1}}]
                 {:enabled? true :global {:limit 0.15 :standby-level 0.4}
                  :groups {:left {:limit 0.3 :standby-level 0.1}
                           :front {:limit 0.5 :max-active 1 :strategy :priority :fixture-order [:b]}}})
        out (:output (simulate m [:all]))
        reports (:lighting-budget out)]
    (doseq [s [(:global reports) (get-in reports [:groups :left]) (get-in reports [:groups :front])]]
      (is (<= (:used-watts s) (:limit-watts s))))
    (is (zero? (get-in reports [:global :active-count])))
    (is (< 0 (get-in reports [:global :used-watts]) 9.01))
    (is (= [0 0 0] (subvec (dmx (:output (simulate (assoc-in m [:settings :lighting-budget :global :limit] 0) [:all]))) 0 3)))))

(deftest unknown-emitter-watts-use-configured-estimate-and-real-drive
  (let [rgb {:id :strip :universe 1 :address 1 :channels {:red 1 :green 2 :blue 3} :emission-share 0.5}
        fog {:id :fog :universe 1 :address 4 :channels {:fog 1}}
        m (setup [rgb fog] [{:id :red :targets [:strip] :outputs {:red 1}}
                            {:id :pump :targets [:fog] :outputs {:fog 1}}]
                 {:enabled? true :unrated-watts 120.0 :global {:limit 0.25}})
        {out :output} (simulate m [:red :pump])
        s (get-in out [:lighting-budget :global])]
    (is (= [1 1 60.0 20.0] (mapv s [:source-count :unrated-count :capacity-watts :requested-watts])))
    (is (<= (:used-watts s) 15.0))
    (is (= [191 0 0 255] (subvec (dmx out) 0 4)))))

(deftest choosing-and-releasing-auto-looks-respects-live-source-cost
  (let [rig [(lamp :a 1 90 []) (lamp :b 2 10 [])]
        defs [{:id :wide :outputs {:dimmer 1}} {:id :small :targets [:b] :outputs {:dimmer 1}}
              {:id :large :targets [:a] :outputs {:dimmer 1}}]
        m (setup rig defs {:enabled? true :global {:limit 0.9 :max-active 1}})
        source {:adapter :auto :session-id "set"}
        state (auto/initial-state 0 0 "set")
        ranked (budget/rank-candidates m rig
                 (mapv (fn [e] {:effect e :outputs (registry/output-entries (:outputs e))})
                       [(get-in m [:registry :effects :wide]) (get-in m [:registry :effects :small])]))
        chosen (auto/advance m {:scope {:cue-ids [:small :wide]}} state 0)
        {with-old :model ack :ack} (launch m :small source 0)
        id (:instance-id ack)
        owned (auto/record-launch state id)
        rotate (auto/advance with-old {:scope {:cue-ids [:large]}} owned 1)
        manual (:model (launch m :small :human 0))
        keep (auto/advance manual {:scope {:cue-ids [:large]}} owned 1)]
    (is (= [:small :wide] (mapv (comp :id :effect) ranked)))
    (is (= [:small] (mapv :cue-id (:commands chosen))))
    (is (= [:instance/end :cue/launch] (mapv :op (:commands rotate))))
    (is (= id (get-in rotate [:commands 0 :instance-id])))
    (is (empty? (:owned-instance-ids (:state rotate))))
    (is (= [:cue/launch] (mapv :op (:commands keep))))
    (is (pos? (get-in keep [:reasons 0 :unmet-watts])))))

(deftest measured-activity-chooses-the-closest-look-that-fits
  (let [defs [{:id :rapid :outputs {:dimmer 1}} {:id :soft :outputs {:dimmer 0.25}}]
        m (-> (setup [(lamp :spot 1 40 [])] defs {:enabled? false})
              (assoc-in [:variables :lighting/activity :history]
                        {:rapid {:version 1 :score 0.9} :soft {:version 1 :score 0.15}}))
        choose (fn [model target] (get-in (auto/advance model {:activity-target target}
                                          (auto/initial-state 1000 0 :auto) 0) [:commands 0 :cue-id]))]
    (is (= :soft (choose m 0.1)))
    (is (= :rapid (choose m 0.8)))
    (is (= :soft (choose (assoc-in m [:settings :lighting-budget]
                                   (budget/settings {:enabled? true :global {:limit 0.3}})) 0.95)))))

(deftest standby-on-idle-lamps-is-paid-for-and-authored-darkness-wins
  (let [rig [(lamp :a 1 5 [:left]) (lamp :b 2 5 [:left]) (lamp :c 3 10 [])]
        defs [{:id :black :targets [:a] :outputs {:dimmer 0}}
              {:id :on :targets [:c] :outputs {:dimmer 1}}]
        m (setup rig defs {:enabled? true
                           :groups {:left {:standby-mode :all :standby-level 0.2}}
                           :global {:limit 0.3 :standby-mode :all :standby-level 0.1 :max-active 1
                                    :fixture-order [:c]}})
        out (:output (simulate m [:black :on]))
        dark (:output (simulate (assoc m :blackout? true) [:on]))
        low (:output (simulate (assoc m :master 0.5) [:black]))]
    (is (zero? (first (dmx out))))
    (is (pos? (nth (dmx out) 1)) "Idle lamp receives a warm standby look")
    (is (< (nth (dmx out) 1) (nth (dmx out) 2)))
    (is (<= (get-in out [:lighting-budget :global :used-watts]) 6.0))
    (is (= 1 (get-in out [:lighting-budget :global :active-count])))
    (is (<= 0 (nth (dmx low) 1) 26))
    (is (= [0 0 0] (subvec (dmx dark) 0 3)))))

(deftest prepared-budgets-keep-static-patch-work-out-of-frames
  (let [m (setup lamps [] {:enabled? true :groups {:left {:limit 0.5}}})
        patch (frame/compile-patch lamps)
        plan (meter/prepare patch)
        path [:extensions :tolglow.lighting-scenes/control :groups :left]
        custom (assoc-in m path {:fixture-ids [:lamp1 :lamp2] :gain 0.25})
        ctx (budget/prepare-frame-context nil custom patch plan)]
    (is (identical? ctx (budget/prepare-frame-context ctx (assoc-in custom (conj path :gain) 0.8) patch plan)))
    (let [changed (budget/prepare-frame-context ctx (assoc-in custom (conj path :fixture-ids) [:lamp1]) patch plan)]
      (is (= [:lamp1] (get-in changed [:scopes 0 :ids]))))))

(deftest invalid-budget-configuration-is-rejected
  (doseq [settings [{:global {:limit ##NaN}} {:global {:standby-level 1.1}}
                    {:groups {:front {:max-active -1}}} {:global {:strategy :typo}}
                    {:unrated-watts -1} {:global {:fixture-order [:a :a]}} {:groups {42 {}}}
                    {:typo true} {:global {:wrong 1}}]]
    (is (thrown? clojure.lang.ExceptionInfo (budget/settings settings)))))
