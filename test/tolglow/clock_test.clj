(ns tolglow.clock-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.control.model :as control]
            [afterglow.runtime.engine :as engine]
            [afterglow.time :as time]
            [tolglow.clock :as clock]
            [tolglow.compiler :as compiler]
            [tolglow.composition.response :as response]
            [tolglow.composition :as composition]
            [tolglow.persistence :as persistence]
            [tolglow.automation.policy :as policy]
            [tolglow.modulation :as modulation]
            [tolglow.integrations.patch :as patch]))

(def fixture {:id :head :position [0.0 0.0 0.0] :universe 1 :address 1 :channels {:dimmer 1 :focus 2}})

(def effect {:id :wash :version 1 :params {:level {:type :number :default 0.8 :bounds [0.0 1.0]}}
             :duration {:beats 4} :release-duration {:beats 2}
             :envelope {:attack {:beats 2} :release {:beats 2}}
             :outputs {:dimmer {:op :param :key :level}}})

(defn command [model payload]
  (let [result (control/apply-command model (merge {:show-id (:show-id model) :epoch (:epoch model)
                                                    :source :clock-test :command-id (str (:revision model))} payload))]
    (when-not (= :accepted (get-in result [:ack :status])) (throw (ex-info "Test command rejected" result)))
    (:model result)))

(defn show
  ([] (show effect))
  ([definition]
   (-> (control/new-show {:show-id :clock-test :rig {:fixtures [fixture]}
                         :transport {:beat 8.0 :bpm 120 :playing? true}})
       (command {:op :registry/publish :kind :effects :definition definition})
       (command {:op :cue/put :cue {:id :wash :effect-id (:id definition)}}))))

(defn tick [model milliseconds beat]
  (engine/advance (update model :transport assoc :time-ms milliseconds :beat beat)
                  {:time-ms milliseconds :fixtures [fixture]} compiler/compile-effect))

(defn launched [] (command (show) {:op :cue/launch :cue-id :wash :time-ms 1000.0 :launch {:mode :one-shot}}))

(defn dimmer [result] (->> (:assignments result) (filter #(= :dimmer (:attribute %))) first :value))

(deftest musical-one-shot-and-release-survive-loop-and-forward-seek
  (let [active (tick (launched) 1500.0 9.0)
        before (:model active)
        shifted (clock/rebase before {:beat 1.0 :time-ms 1500.0 :source :live :source-epoch 2})
        after (tick shifted 1500.0 1.0)
        release (tick (:model after) 3000.0 4.0)
        tail (tick (:model release) 3250.0 4.5)
        sought (clock/rebase (:model tail) {:beat 100.0 :time-ms 3250.0 :source-epoch 3})
        same-tail (tick sought 3250.0 100.0)
        ended (tick (:model same-tail) 4000.0 101.5)]
    (is (= 0.4 (dimmer active) (dimmer after)))
    (is (= 0.0 (get-in shifted [:instances "0:1" :started-at-beat])))
    (is (= 0.0 (get-in shifted [:instances "0:1" :duration-timer :started-at])))
    (is (= 1.0 (:last-frame-beat shifted)))
    (is (= 1000.0 (get-in shifted [:instances "0:1" :started-at-ms])))
    (is (= (:time-ms before) (:time-ms shifted)))
    (is (= (:revision before) (:revision shifted)))
    (is (= (:epoch before) (:epoch shifted)))
    (is (= :releasing (get-in release [:model :instances "0:1" :phase])))
    (is (= 4.0 (get-in release [:model :instances "0:1" :release-at-beat])))
    (is (= 3000.0 (get-in same-tail [:model :instances "0:1" :release-at-ms])))
    (is (= 99.5 (get-in same-tail [:model :instances "0:1" :release-timer :started-at])))
    (is (= (dimmer tail) (dimmer same-tail)))
    (is (empty? (get-in ended [:model :instances])))
    (is (= {:source :live :source-epoch 3} (select-keys (:transport sought) [:source :source-epoch])))))

(deftest absolute-oscillators-follow-external-beat-and-elapsed-oscillators-stay-continuous
  (let [definition {:id :clocks :version 1 :params {}
                    :outputs {:dimmer {:op :divide :args [{:op :beat} {:op :const :value 10.0}]}
                              :focus {:op :divide :args [{:op :beat :clock :absolute} {:op :const :value 20.0}]}}}
        started (command (show definition) {:op :cue/launch :cue-id :wash :time-ms 1000.0})
        active (tick started 1500.0 9.0)
        result (tick (clock/rebase (:model active) {:beat 1.0}) 1500.0 1.0)
        values (into {} (map (juxt :attribute :value)) (:assignments result))]
    (is (= 0.1 (dimmer active) (:dimmer values)))
    (is (= 0.05 (:focus values)))))

(deftest millisecond-timers-and-arbitrary-instance-state-are-not-reinterpreted
  (let [definition (-> effect (dissoc :duration :release-duration)
                       (assoc :duration-ms 2000.0 :release-duration-ms 500.0
                              :envelope {:attack-ms 1000.0 :release-ms 500.0}))
        started (command (show definition) {:op :cue/launch :cue-id :wash :time-ms 1000.0 :launch {:mode :one-shot}})
        before (-> (:model (tick started 1500.0 9.0))
                   (assoc-in [:instances "0:1" :state] {:seed 123 :beat 999 :started-at-beat 777}))
        rebased (clock/rebase before {:beat 0.0 :time-ms 1500.0})
        result (tick rebased 1500.0 0.0)
        release (tick (:model result) 3250.0 3.5)]
    (is (= 0.4 (dimmer result)))
    (is (= 0.4 (dimmer release)))
    (is (= 3000.0 (get-in release [:model :instances "0:1" :release-at-ms])))
    (is (= (get-in before [:instances "0:1" :state]) (get-in rebased [:instances "0:1" :state])))
    (is (= 2000.0 (get-in rebased [:instances "0:1" :duration-ms])))
    (is (= 500.0 (get-in rebased [:instances "0:1" :release-duration-ms])))))

(deftest musical-response-progress-is-preserved-and-wall-response-progress-is-unchanged
  (let [responses [{:id :beat-gate :params [:level] :envelope {:attack {:beats 2} :release {:beats 2}}
                    :state {:started-at-ms 1000.0 :started-at-beat 8.0 :released-at-ms 1500.0
                            :released-at-beat 9.0 :timing-meter time/default-meter}}
                   {:id :wall-gate :params [:level] :envelope {:attack-ms 1000.0}
                    :state {:started-at-ms 1000.0 :started-at-beat 8.0}}]
        before (assoc (:model (tick (launched) 1750.0 9.5)) :responses responses)
        shifted (clock/rebase before {:beat 0.5})
        context {:instance (get-in before [:instances "0:1"]) :frame {:time-ms 1750.0 :beat 9.5}}
        original (response/apply-responses responses context)
        actual (response/apply-responses (:responses shifted) (assoc-in context [:frame :beat] 0.5))]
    (is (= (get-in original [:instance :params]) (get-in actual [:instance :params])))
    (is (= -1.0 (get-in shifted [:responses 0 :state :started-at-beat])))
    (is (= 0.0 (get-in shifted [:responses 0 :state :released-at-beat])))
    (is (= 1500.0 (get-in shifted [:responses 0 :state :released-at-ms])))
    (is (= 1000.0 (get-in shifted [:responses 1 :state :started-at-ms])))))

(deftest beat-sequence-keeps-phase-token-seed-and-window-while-ms-sequence-is-untouched
  (let [spec {:id :beat-sequence :mode :sequence :loop :shuffle :seed 51
              :slots [{:cue-id :wash :duration {:beats 2}} {:cue-id :wash :duration {:beats 2}}]}
        wall (assoc spec :id :wall-sequence :timebase :ms
                         :slots [{:cue-id :wash :duration {:ms 2000}}])
        started (-> (show) (composition/save spec) (composition/save wall)
                    (composition/start :beat-sequence 1000.0) (composition/start :wall-sequence 1000.0)
                    (composition/advance 1000.0))
        before (-> started (assoc-in [:transport :beat] 9.0) (composition/advance 1500.0))
        rebased (clock/rebase before {:beat 0.0 :time-ms 1500.0})
        after (composition/advance rebased 1500.0)
        next (-> after (assoc-in [:transport :beat] 1.0) (composition/advance 2000.0))]
    (is (= 0.5 (get-in before [:composition-runs :beat-sequence :playback :phase])
                 (get-in after [:composition-runs :beat-sequence :playback :phase])))
    (is (= (:composition-tokens before) (:composition-tokens after)))
    (is (= (get-in before [:composition-runs :beat-sequence :state :seed])
           (get-in after [:composition-runs :beat-sequence :state :seed])))
    (is (= 1.0 (get-in after [:composition-runs :beat-sequence :state :step-end-beat])))
    (is (= (get-in before [:composition-runs :wall-sequence]) (get-in rebased [:composition-runs :wall-sequence])))
    (is (= 1 (get-in next [:composition-runs :beat-sequence :state :visit])))))

(deftest auto-planner-next-beat-and-seed-survive-a-seek-without-triggering-a-burst
  (let [show (show) config {:interval {:beats 2} :intent :darkness}
        first-plan (policy/advance show config (policy/initial-state 71 1000.0 :auto) 1000.0)
        before (-> show (assoc :auto {:enabled? true :config config :state (:state first-plan)})
                   (assoc-in [:transport :beat] 9.0))
        shifted (clock/rebase before {:beat 0.0})
        held (policy/advance shifted config (get-in shifted [:auto :state]) 1500.0)
        next (policy/advance (assoc-in shifted [:transport :beat] 1.0) config (:state held) 2000.0)]
    (is (= 1.0 (get-in shifted [:auto :state :next-at-beat])))
    (is (= -1.0 (get-in shifted [:auto :state :interval-timer :started-at])))
    (is (= (:seed (:state first-plan)) (:seed (:state held))))
    (is (empty? (:commands held)))
    (is (= 1 (count (:commands next))))
    (is (= (inc (get-in first-plan [:state :tick])) (get-in next [:state :tick])))))

(deftest real-show-save-restore-and-seek-preserve-modulation-state-and-negative-history
  (let [before (:model (tick (launched) 1500.0 9.0))
        state (modulation/start (modulation/new-state)
                                 {:id :random :seed 31 :params [:level] :every {:beats 4} :mode :glide}
                                 {:beat 8.0})
        modulated (modulation/advance state before {:time-ms 1500.0 :beat 9.0})
        saved-model (assoc before :modulation (:state modulated))
        restored (persistence/thaw saved-model (persistence/read-string (persistence/write-string saved-model)))
        rebased (clock/rebase restored {:beat 0.0 :time-ms 1500.0})
        after (modulation/advance (:modulation rebased) rebased {:time-ms 1500.0 :beat 0.0})
        id (first (keys (:instances restored)))]
    (is (= (get-in modulated [:overrides "0:1"]) (get-in after [:overrides id])))
    (is (= (get-in saved-model [:modulation :controllers 0 :seed]) (get-in after [:state :controllers 0 :seed])))
    (is (= 3.0 (get-in after [:state :controllers 0 :next-beat])))
    (is (= -1.0 (get-in rebased [:instances id :started-at-beat])))
    (is (= rebased (:model (persistence/freeze rebased))))))

(deftest invalid-clock-inputs-and-overflow-do-not-return-a-partial-model
  (let [model (:model (tick (launched) 1500.0 9.0))]
    (doseq [clock [{:beat Double/NaN} {:beat 1.0 :time-ms 1499.0} {:beat 1.0 :playing? :yes}
                   {:beat 1.0 :bpm 0} {:beat 1.0 :bpm 1001} {:beat 1.0 :meter {:beats-per-bar 0}}]]
      (is (thrown? clojure.lang.ExceptionInfo (clock/rebase model clock))))
    (is (= 9.0 (get-in model [:transport :beat])))
    (is (= 8.0 (get-in model [:instances "0:1" :started-at-beat])))))

(deftest system-private-clock-anchors-shift-but-pause-offsets-and-user-state-do-not
  (let [model (assoc-in (:model (tick (launched) 1500.0 9.0)) [:instances "0:1" :system]
                        {:clock-offset-ms 200.0 :clock-offset-beats 2.0 :paused? true
                         :last-clock {:time-ms 1300.0 :beat 7.0 :bpm 120}
                         :paused-clock {:time-ms 1200.0 :beat 6.8 :bpm 120}
                         :gate {:kind :release :from 0.7 :timer (time/start {:beats 2} {:beat 6.5})}
                         :last-input-state {:beat 999.0}})
        shifted (clock/rebase model {:beat 1.0})
        system (get-in shifted [:instances "0:1" :system])]
    (is (= 200.0 (:clock-offset-ms system)))
    (is (= 2.0 (:clock-offset-beats system)))
    (is (= -1.0 (get-in system [:last-clock :beat])))
    (is (< (Math/abs (+ 1.2 (get-in system [:paused-clock :beat]))) 1.0e-12))
    (is (= -1.5 (get-in system [:gate :timer :started-at])))
    (is (= 1300.0 (get-in system [:last-clock :time-ms])))
    (is (= {:beat 999.0} (:last-input-state system)))))

(deftest signal-routing-rebases-with-the-shared-origin-and-keeps-observation-age
  (let [spec {:version 1 :id :routing :revision 1
              :sources {:sensor {:kind :midi :range [0.0 1.0] :timeout {:beats 8}}}
              :destinations {:wash {:cue-id :wash :param :level :range [0.0 1.0] :combine :max}}
              :routes [{:id :level :source :sensor :destination :wash :envelope {:attack {:beats 2}}}]}
        c (fn [milliseconds beat] {:time-ms milliseconds :beat beat :bpm 120 :playing? true :meter time/default-meter})
        input (fn [clock observations] {:epoch 0 :clock clock :observations observations
                                       :bindings {:wash "0:1"} :active-instances #{"0:1"}
                                       :envelope {:show-id :clock-test :epoch 0 :command-id :route}})
        first-frame (patch/step spec (patch/new-state spec (c 1000.0 8.0) 0)
                                (input (c 1000.0 8.0)
                                       [{:source-id :sensor :event-id :on :epoch 0 :clock (c 1000.0 8.0)
                                         :value 1.0 :gate? true :confidence 1.0 :origin :human :observed-at-ms 1000.0}]))
        before (patch/step spec (:state first-frame) (input (c 1500.0 9.0) []))
        model (assoc (:model (tick (launched) 1500.0 9.0))
                     :routing {:enabled? true :spec spec :state (:state before)})
        shifted (clock/rebase model {:time-ms 1500.0 :beat 0.0})
        state (get-in shifted [:routing :state])
        after (patch/step spec state (input (c 1500.0 0.0) []))]
    (is (= 0.5 (get-in before [:values [:wash "0:1"]]) (get-in after [:values [:wash "0:1"]])))
    (is (= -1.0 (get-in state [:sources :sensor :clock :beat])))
    (is (= 1000.0 (get-in state [:sources :sensor :clock :time-ms])))
    (is (= 1000.0 (get-in state [:sources :sensor :observed-at-ms])))
    (is (= (get-in before [:state :frame]) (:frame state)))
    (is (= (get-in before [:state :receipts]) (:receipts state)))))

(deftest hold-shifts-only-millisecond-show-progress-and-is-pure
  (let [model {:time-ms 5000 :transport {:beat 4.0 :time-ms 5000 :playing? false}
               :instances {"0:1" {:id "0:1" :started-at-ms 0 :system {:clock-offset-ms 250 :clock-offset-beats 1.0}}
                           "0:2" {:id "0:2" :started-at-ms 0 :system {:paused? true :paused-clock {:time-ms 900 :beat 1.0}}}
                           "0:3" {:id "0:3" :started-at-ms 0}}
               :responses [{:id :r :state {:started-at-ms 1000 :started-at-beat 2.0}}
                           {:id :q :state {:started-at-ms 1000 :released-at-ms 3000}}]
               :event-subscribers {:ramps [{:event-id "a" :unit :ms :start 1000 :end 5000}
                                           {:event-id "b" :unit :beats :start 1.0 :end 9.0}]}
               :auto {:state {:interval-timer {:duration {:unit :ms :value 1000} :started-at 4000}
                              :next-at-ms 5000}}}
        held (clock/hold model 400)]
    (is (= model (clock/hold model 0)))
    (is (= [650 nil 400] (mapv #(get-in held [:instances % :system :clock-offset-ms]) ["0:1" "0:2" "0:3"])))
    (is (= 1.0 (get-in held [:instances "0:1" :system :clock-offset-beats])))
    (is (= (assoc-in (get-in model [:instances "0:2"]) [:system :transport-held-ms] 400) (get-in held [:instances "0:2"]))
        "an explicit pause keeps its own clock")
    (is (= [400 400 400] (mapv #(get-in held [:instances % :system :transport-held-ms]) ["0:1" "0:2" "0:3"])))
    (is (= [{:started-at-ms 1400 :started-at-beat 2.0} {:started-at-ms 1400 :released-at-ms 3400}]
           (mapv :state (:responses held))))
    (is (= [[1400 5400] [1.0 9.0]] (mapv (juxt :start :end) (get-in held [:event-subscribers :ramps]))))
    (is (= [4400 5400] [(get-in held [:auto :state :interval-timer :started-at]) (get-in held [:auto :state :next-at-ms])]))
    (is (= [(:time-ms model) (:transport model)] [(:time-ms held) (dissoc (:transport held) :held-ms)])
        "host time is not show progress")
    (is (= 400 (get-in held [:transport :held-ms])) "the show counts its held time")
    (is (= 1000 (get-in (clock/hold held 600) [:transport :held-ms])))
    (is (thrown? clojure.lang.ExceptionInfo (clock/hold model -1)))))

(deftest hold-keeps-routing-envelopes-and-leaves-live-input-on-the-wall-clock
  (let [spec {:version 1 :id :p :revision 1
              :sources {:sensor {:kind :control :range [0 1] :timeout {:beats 64}}}
              :destinations {:wash {:cue-id :wash :param :level :range [0 1] :combine :max}}
              :routes [{:id :ms :source :sensor :destination :wash :envelope {:attack {:ms 1000} :release {:ms 1000}}}
                       {:id :beat :source :sensor :destination :wash :envelope {:attack {:beats 2} :release {:beats 2}}}
                       {:id :plain :source :sensor :destination :wash}]}
        state (-> (patch/new-state spec {:time-ms 0.0 :beat 0.0} 0)
                  (assoc-in [:routes :ms] {:gate? false :held-value 1.0 :smoothed 0.5 :clock {:time-ms 900.0 :beat 1.8}
                                           :envelope {:initial-level 0.0 :started-at-ms 100.0 :started-at-beat 0.2
                                                      :released-at-ms 600.0 :release-level 0.5}})
                  (assoc-in [:routes :beat] {:gate? true :held-value 1.0 :smoothed 0.5 :clock {:time-ms 900.0 :beat 1.8}
                                             :envelope {:initial-level 0.0 :started-at-ms 100.0 :started-at-beat 0.2}})
                  (assoc-in [:routes :plain] {:gate? true :held-value 1.0 :smoothed 1.0 :clock {:time-ms 900.0 :beat 1.8}
                                              :envelope nil}))
        held (patch/hold-clock state 400.0)]
    (is (= {:initial-level 0.0 :started-at-ms 500.0 :started-at-beat 0.2 :released-at-ms 1000.0 :release-level 0.5}
           (get-in held [:routes :ms :envelope])))
    (is (= {:initial-level 0.0 :started-at-ms 500.0 :started-at-beat 0.2} (get-in held [:routes :beat :envelope]))
        "beat envelopes keep their beat anchors; their millisecond anchors are unused")
    (is (= (get-in state [:routes :plain]) (get-in held [:routes :plain])))
    (is (= (mapv #(get-in state [:routes % :clock]) [:ms :beat]) (mapv #(get-in held [:routes % :clock]) [:ms :beat]))
        "the routing clock and smoothing stay on the wall clock")
    (is (= (dissoc state :routes) (dissoc held :routes)))
    (is (= held (get-in (clock/hold {:transport {:beat 1.8 :time-ms 900.0 :playing? false}
                                     :routing {:enabled? true :spec spec :state state}} 400.0)
                        [:routing :state])))))
