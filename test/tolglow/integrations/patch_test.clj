(ns tolglow.integrations.patch-test
  (:require [clojure.test :refer :all]
            [clojure.edn :as edn]
            [tolglow.integrations.patch :as patch]))

(def spec {:version 1 :id :performance :revision 1
           :sources {:drums {:kind :midi :range [0 127] :timeout {:beats 8}}}
           :destinations {:wash {:cue-id :wash :param :level :range [0 1] :combine :max}}
           :routes [{:id :velocity :source :drums :destination :wash}]})

(defn clock [beat] {:time-ms (* 500.0 beat) :beat beat :bpm 120 :playing? true :meter {:beats-per-bar 4 :bars-per-phrase 4}})

(defn sample [id beat value gate?]
  {:source-id :drums :event-id id :epoch 3 :clock (clock beat) :value value :gate? gate? :confidence 1.0 :origin :live-playback})

(defn frame [beat observations]
  {:epoch 3 :clock (clock beat) :observations observations :bindings {:wash :wash-1} :active-instances #{:wash-1}
   :envelope {:show-id :show :epoch 3 :command-id (str "frame-" beat)}})

(defn near? [a b] (< (Math/abs (double (- a b))) 1.0e-8))

(defn output [result] (get-in result [:values [:wash :wash-1]]))

(deftest calibration-curves-inversion-quantization-and-bounds
  (doseq [[route input expected] [[{} 63.5 0.5]
                                 [{:curve :ease-in} 63.5 0.25]
                                 [{:invert? true} 127 0.0]
                                 [{:steps 5} 40 0.25]
                                 [{} 300 1.0] [{} -50 0.0]
                                 [{:source-range [20 100] :range [0.2 0.8]} 60 0.5]]]
    (let [spec (update-in spec [:routes 0] merge route)
          result (patch/step spec (patch/new-state spec (clock 0) 3) (frame 0 [(sample :one 0 input true)]))]
      (is (near? expected (output result)))
      (is (= :param/set (:op (first (:commands result)))))
      (is (= :wash-1 (:instance-id (first (:commands result)))))
      (is (= :live-playback (get-in result [:commands 0 :source :origin]))))))

(deftest many-sources-use-explicit-deterministic-combination-and-destination-clamp
  (doseq [[combine expected] [[:max 0.75] [:min 0.25] [:sum 1.0] [:mean 0.5] [:product 0.1875] [:priority 0.25]]]
    (let [spec (-> spec (assoc-in [:sources :keys] {:kind :automation :range [0 1]})
                   (assoc-in [:destinations :wash :combine] combine)
                   (assoc :routes [{:id :a :source :drums :destination :wash :priority 10}
                                   {:id :b :source :keys :destination :wash :priority 0}]))
          observations [(sample :a 0 (* 127 0.25) true)
                        (assoc (sample :b 0 0.75 true) :source-id :keys)]
          state (patch/new-state spec (clock 0) 3)
          first-run (patch/step spec state (frame 0 observations))
          reversed (patch/step spec state (frame 0 (vec (reverse observations))))]
      (is (near? expected (output first-run)))
      (is (= (:commands first-run) (:commands reversed)))
      (is (= 1 (count (:commands first-run)))))))

(deftest musical-adsr-pauses-and-releases-from-the-actual-level
  (let [spec (assoc-in spec [:routes 0 :envelope] {:attack {:beats 2} :decay {:beats 1} :sustain 0.5 :release {:beats 2}})
        initial (patch/new-state spec (clock 0) 3)
        started (patch/step spec initial (frame 0 [(sample :on 0 127 true)]))
        attack (patch/step spec (:state started) (frame 1 []))
        paused (patch/step spec (:state attack) (assoc-in (frame 1 []) [:clock :time-ms] 10000))
        released (patch/step spec (:state attack) (frame 1 [(sample :off 1 0 false)]))
        tail (patch/step spec (:state released) (frame 2 []))
        done (patch/step spec (:state tail) (frame 3 []))]
    (is (= 0.0 (output started)))
    (is (= 0.5 (output attack)))
    (is (= 0.5 (output paused)))
    (is (= 0.5 (output released)))
    (is (= 0.25 (output tail)))
    (is (= 0 (output done)))
    (is (empty? (:commands (patch/step spec (:state done) (frame 4 [])))))))

(deftest brief-gate-on-and-off-in-one-input-block-preserves-release-tail
  (let [spec (assoc-in spec [:routes 0 :envelope] {:attack {:beats 0} :release {:beats 1}})
        result (patch/step spec (patch/new-state spec (clock 0) 3)
                           (frame 0.5 [(sample :on 0 127 true) (sample :off 0.25 0 false)]))]
    (is (= 0.75 (output result)))
    (is (= 0.25 (get-in result [:state :routes :velocity :envelope :released-at-beat])))))

(deftest envelope-tail-cannot-attach-to-replacement-cue-instance
  (let [spec (assoc-in spec [:routes 0 :envelope] {:release {:beats 2}})
        started (patch/step spec (patch/new-state spec (clock 0) 3) (frame 0 [(sample :on 0 127 true)]))
        replaced (patch/step spec (:state started)
                             (assoc (frame 1 [(sample :off 1 0 false)]) :bindings {:wash :wash-2} :active-instances #{:wash-2}))]
    (is (empty? (:commands replaced)))
    (is (empty? (:values replaced)))
    (is (= :missing-captured-instance (get-in replaced [:diagnostics 0 :code])))
    (let [retrigger (patch/step spec (:state replaced)
                              (assoc (frame 2 [(sample :on-again 2 127 true)]) :bindings {:wash :wash-2} :active-instances #{:wash-2}))]
      (is (= :wash-2 (get-in retrigger [:commands 0 :instance-id]))))))

(deftest different-live-activations-are-never-combined-into-one-write
  (let [spec (-> spec (assoc-in [:sources :keys] {:kind :midi :range [0 127]})
                 (assoc :routes [{:id :a :source :drums :destination :wash :envelope {:release {:beats 4}}}
                                 {:id :b :source :keys :destination :wash :envelope {:release {:beats 4}}}]))
        started (patch/step spec (patch/new-state spec (clock 0) 3) (frame 0 [(sample :a 0 127 true)]))
        next (patch/step spec (:state started)
                         (assoc (frame 1 [(sample :a-off 1 0 false) (assoc (sample :b 1 63.5 true) :source-id :keys)])
                                :bindings {:wash :wash-2} :active-instances #{:wash-1 :wash-2}))]
    (is (= 1.0 (get-in next [:values [:wash :wash-1]])))
    (is (= 0.5 (get-in next [:values [:wash :wash-2]])))
    (is (= #{:wash-2} (set (map :instance-id (:commands next)))))))

(deftest smoothing-is-exact-for-a-held-target-across-frame-cadences
  (let [spec (assoc-in spec [:routes 0 :smoothing] {:beats 1})
        initial (patch/step spec (patch/new-state spec (clock 0) 3) (frame 0 [(sample :zero 0 0 true)]))
        target (patch/step spec (:state initial) (frame 0 [(sample :one 0 127 true)]))
        coarse (patch/step spec (:state target) (frame 1 []))
        fine (reduce (fn [result beat] (patch/step spec (:state result) (frame beat []))) target [0.25 0.5 0.75 1.0])]
    (is (near? (- 1 (Math/exp -1.0)) (output coarse)))
    (is (near? (output coarse) (output fine)))
    (let [paused (patch/step spec (:state target) (assoc-in (frame 0 []) [:clock :time-ms] 1000))]
      (is (= 0.0 (output paused))))))

(deftest timeout-and-low-confidence-release-gates-without-clamping-to-stale-input
  (let [spec (-> spec (assoc-in [:sources :drums :timeout] {:beats 1})
                 (assoc-in [:sources :drums :min-confidence] 0.5)
                 (assoc-in [:routes 0 :envelope] {:release {:beats 1}}))
        started (patch/step spec (patch/new-state spec (clock 0) 3) (frame 0 [(sample :on 0 127 true)]))
        expired (patch/step spec (:state started) (frame 1 []))
        tail (patch/step spec (:state expired) (frame 1.5 []))]
    (is (= 1.0 (output expired)))
    (is (= 0.5 (output tail)))
    (is (false? (get-in expired [:state :routes :velocity :gate?])))
    (let [quiet (patch/step spec (:state started) (frame 0.5 [(assoc (sample :uncertain 0.5 127 true) :confidence 0.1)]))]
      (is (false? (get-in quiet [:state :routes :velocity :gate?]))))))

(deftest feedback-duplicates-old-epochs-and-future-data-are-diagnosed
  (let [state (patch/new-state spec (clock 0) 3)
        result (patch/step spec state (frame 1 [(sample :good 0 127 true)
                                               (sample :good 0 127 true)
                                               (assoc (sample :old 0 10 true) :epoch 2)
                                               (assoc (sample :feedback 0 20 true) :origin :tolglow-feedback)
                                               (assoc (sample :patch 0 20 true) :origin :patch)
                                               (sample :future 2 20 true)]))]
    (is (= 1.0 (output result)))
    (is (= [:duplicate :stale-epoch :feedback-suppressed :feedback-suppressed :future-sample]
           (mapv :code (:diagnostics result))))
    (is (= 1 (count (get-in result [:state :receipts]))))
    (is (= 1 (count (:commands result))))))

(deftest late-callback-is-explicitly-clamped-without-rewinding-an-envelope
  (let [spec (assoc-in spec [:routes 0 :envelope] {:attack {:beats 1} :release {:beats 1}})
        state (patch/new-state spec (clock 2) 3)
        result (patch/step spec state (frame 3 [(sample :late 1 127 true)]))]
    (is (= :late-sample-clamped (get-in result [:diagnostics 0 :code])))
    (is (= 2 (get-in result [:state :routes :velocity :envelope :started-at-beat])))
    (is (= 1.0 (output result)))))

(deftest capture-is-data-and-replay-is-deterministic
  (let [state (patch/new-state spec (clock 0) 3)
        first-frame (frame 0 [(sample :on 0 127 true)])
        result (patch/step spec state first-frame)]
    (is (= result (patch/step spec (edn/read-string (pr-str state)) first-frame)))
    (is (= (:state result) (edn/read-string (pr-str (:state result)))))
    (is (empty? (:commands (patch/step spec (:state result) (frame 1 [])))))
    (is (thrown? clojure.lang.ExceptionInfo (patch/step spec (:state result) (assoc (frame 1 []) :epoch 4))))
    (is (thrown? clojure.lang.ExceptionInfo (patch/step spec (patch/new-state spec (clock 2) 3) (frame 1 []))))))

(deftest malformed-specifications-fail-before-evaluation
  (doseq [bad [(assoc spec :version 2)
               (assoc-in spec [:sources :drums :range] [1 1])
               (assoc-in spec [:sources :drums :timeout] {:beats 0})
               (assoc-in spec [:destinations :wash :combine] :implicit)
               (assoc-in spec [:routes 0 :source] :missing)
               (assoc-in spec [:routes 0 :range] [-1 1])
               (assoc-in spec [:routes 0 :steps] 1)
               (assoc-in spec [:routes 0 :envelope] {:attack {:beats 1} :release {:ms 100}})
               (assoc spec :routes (vec (repeat 2 (first (:routes spec)))))] ]
    (is (thrown? clojure.lang.ExceptionInfo (patch/validate-patch bad)))))

(deftest restore-preserves-envelope-progress-and-remaps-owned-instance
  (let [spec (assoc-in spec [:routes 0 :envelope] {:release {:beats 4}})
        on (patch/step spec (patch/new-state spec (clock 0) 3) (frame 0 [(sample :on 0 127 true)]))
        off (patch/step spec (:state on) (frame 1 [(sample :off 1 0 false)]))
        tail (patch/step spec (:state off) (frame 2 []))
        saved (edn/read-string (pr-str (:state tail)))
        restored (patch/restore-state spec saved {:epoch 4 :instance-ids {:wash-1 :restored}})
        next-frame (-> (frame 3 []) (assoc :epoch 4 :bindings {:wash :restored} :active-instances #{:restored})
                       (assoc-in [:envelope :epoch] 4))
        next (patch/step spec restored next-frame)]
    (is (= saved (patch/validate-state spec saved)))
    (is (= restored (patch/validate-state spec restored)))
    (is (= (:receipts saved) (:receipts restored)))
    (is (= 0.75 (get-in restored [:last-values [:wash :restored]])))
    (is (= 0.5 (get-in next [:values [:wash :restored]])))
    (is (= :restored (get-in next [:commands 0 :instance-id])))
    (is (= 4 (get-in next [:commands 0 :epoch])))))

(deftest orphaned-captured-gate-does-not-rebind-on-restore
  (let [spec (assoc-in spec [:routes 0 :envelope] {:release {:beats 4}})
        on (patch/step spec (patch/new-state spec (clock 0) 3) (frame 0 [(sample :on 0 127 true)]))
        restored (patch/restore-state spec (:state on) {:epoch 4 :instance-ids {}})
        frame (-> (frame 1 []) (assoc :epoch 4 :bindings {:wash :replacement} :active-instances #{:replacement})
                  (assoc-in [:envelope :epoch] 4))
        next (patch/step spec restored frame)]
    (is (true? (get-in restored [:routes :velocity :gate?])))
    (is (nil? (get-in restored [:routes :velocity :instance-id])))
    (is (empty? (:commands next)))
    (is (empty? (:values next)))))

(deftest musical-seek-preserves-progress-and-wall-deadlines
  (let [spec (assoc-in spec [:routes 0 :envelope] {:attack {:beats 4} :release {:beats 4}})
        on (patch/step spec (patch/new-state spec (clock 10) 3) (frame 10 [(sample :on 10 127 true)]))
        half (patch/step spec (:state on) (frame 12 []))
        rebased (patch/rebase-clock (:state half) (assoc (clock 0) :time-ms 6000))
        next (patch/step spec rebased (assoc (frame 1 []) :clock (assoc (clock 1) :time-ms 6500)))]
    (is (= rebased (patch/validate-state spec rebased)))
    (is (= -2 (get-in rebased [:routes :velocity :envelope :started-at-beat])))
    (is (= 5000.0 (get-in rebased [:routes :velocity :envelope :started-at-ms])))
    (is (= -2 (get-in rebased [:sources :drums :clock :beat])))
    (is (= 0.75 (output next)))
    (let [shared (patch/rebase-clock (:state half) {:time-ms 7000 :beat 0 :from-beat 14})]
      (is (= -4 (get-in shared [:routes :velocity :envelope :started-at-beat])))
      (is (= 0 (get-in shared [:clock :beat])))
      (is (= shared (patch/validate-state spec shared)))))
  (let [spec (assoc-in spec [:routes 0 :envelope] {:attack {:ms 1000}})
        on (patch/step spec (patch/new-state spec (clock 0) 3) (frame 0 [(sample :on 0 127 true)]))
        rebased (patch/rebase-clock (:state on) {:time-ms 500 :beat 0 :meter {:beats-per-bar 4 :bars-per-phrase 4}})
        next (patch/step spec rebased (assoc (frame 0 []) :clock {:time-ms 500 :beat 0}))]
    (is (= 0.5 (output next)))))

(deftest captured-state-rejects-invalid-shapes-and-nonportable-progress
  (let [saved (:state (patch/step spec (patch/new-state spec (clock 0) 3) (frame 0 [(sample :on 0 127 true)])))]
    (doseq [bad [(assoc saved :version 2) (assoc saved :revision 999)
                 (assoc-in saved [:routes :velocity :smoothed] Double/NaN)
                 (assoc-in saved [:sources :drums :epoch] 2)
                 (assoc-in saved [:sources :drums :hidden-callback] identity)
                 (assoc saved :receipts [[:unknown :event]])
                 (assoc saved :last-values {:not-a-key 0})
                 (assoc-in saved [:last-values [:wash :wash-1]] 5)]]
      (is (thrown? clojure.lang.ExceptionInfo (patch/validate-state spec bad))))))

(deftest admitted-observation-rebased-before-loop-start-is-not-discarded
  (let [current {:time-ms 1000 :beat 0}
        observation (assoc (sample :queued 0 127 true) :clock {:time-ms 900 :beat -0.2})
        result (patch/step spec (patch/new-state spec current 3)
                           (assoc (frame 0 [observation]) :clock current))]
    (is (= 1.0 (output result)))
    (is (= [:late-sample-clamped] (mapv :code (:diagnostics result))))
    (is (= -0.2 (get-in result [:state :sources :drums :clock :beat])))))
