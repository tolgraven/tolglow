(ns tolglow.automation.policy-test
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.automation.policy :as policy]
            [tolglow.clock :as clock]
            [tolglow.registry :as registry]
            [tolglow.spatial :as spatial]))

(def fixture {:id :head :position [0 0 0] :universe 1 :address 1
              :channels {:red 1 :green 2 :blue 3 :dimmer 4 :pan 5 :tilt 6}})

(def movement {:id :movement :version 1 :outputs {:pan 0.3 :tilt 0.6}})

(def emission {:id :emission :version 1 :outputs {:color [1 0 0] :dimmer 0.5}})

(defn model []
  {:show-id "auto" :epoch 1 :master 1 :blackout? false :fixtures [fixture]
   :transport {:beat 0 :bpm 120 :playing? true}
   :pages {:motion-only {:cells {[0 0] :motion}} :all {:cells {[0 0] :motion [1 0] :red}}}
   :registry (-> (registry/new-registry)
                 (registry/publish :effects movement)
                 (registry/publish :effects emission)
                 (registry/publish :cues {:id :motion :effect-id :movement})
                 (registry/publish :cues {:id :red :effect-id :emission}))})

(deftest movement-adds-real-emission-and-preserves-intentional-darkness
  ;; With two entries seed 1000 keeps motion first in Fisher-Yates ordering.
  (let [state (policy/initial-state 1000 0 "operator-auto")
        result (policy/advance (model) {} state 0)
        dark (policy/advance (model) {:intent :darkness} state 0)]
    (is (= #{:motion :red} (set (map :cue-id (:commands result)))))
    (is (= :added-emission (get-in result [:reasons 0 :reason])))
    (is (= [:motion] (mapv :cue-id (:commands dark))))
    (is (= :intentional-darkness (get-in dark [:reasons 0 :reason])))))

(deftest scope-and-black-output-do-not-create-false-prerequisites
  (let [state (policy/initial-state 1000 0 :auto)
        scoped (policy/advance (model) {:scope {:page-id :motion-only}} state 0)
        black (policy/advance (assoc (model) :master 0) {} state 0)
        existing (assoc (model) :assignments [{:target-id :head :attribute :color :value [1 1 1] :blend :replace}])
        visible (policy/advance existing {:scope {:cue-ids [:motion]}} state 0)]
    (is (empty? (:commands scoped)))
    (is (= :unsatisfied-emission (get-in scoped [:reasons 0 :reason])))
    (is (empty? (:commands black)))
    (is (= [:motion] (mapv :cue-id (:commands visible))))))

(deftest scheduling-replay-restart-and-cleanup-use-owned-instance-ids
  (let [state (policy/initial-state 0 0 :auto)
        result (policy/advance (model) {:interval-ms 100} state 0)
        same (policy/advance (model) {:interval-ms 100} (:state result) 0)
        with-id (policy/record-launch (:state result) "auto-instance")
        ;; Ownership covers instances that still exist; an ended one is forgotten.
        running (assoc (model) :instances {"auto-instance" {:id "auto-instance" :phase :running}})
        stopped (policy/stop-plan running with-id 10)
        _ (is (empty? (:commands (policy/stop-plan (model) with-id 10))))
        restarted (policy/advance (assoc (model) :epoch 2) {} with-id 100)]
    (is (= result (policy/advance (model) {:interval-ms 100} state 0)))
    (is (empty? (:commands same)))
    (is (= ["auto-instance"] (mapv :instance-id (:commands stopped))))
    (is (= :show-restarted (get-in restarted [:reasons 0 :reason])))
    (is (empty? (:commands restarted)))
    (is (empty? (get-in restarted [:state :owned-instance-ids])))))

(deftest current-instance-priority-and-expiry-participate-in-emission-prediction
  (let [black {:id :black :version 1 :outputs {:color [0 0 0]}}
        instance {:id "human-blackout" :cue-id :human :effect-id :black :effect-version 1
                  :definition black :started-at-ms 0 :params {} :state {}
                  :priority 100 :order 0 :phase :running}
        show (assoc (model) :instances {"human-blackout" instance})
        state (policy/initial-state 1000 10 :auto)
        blocked (policy/advance show {} state 10)
        expired (policy/advance (assoc-in show [:instances "human-blackout" :duration-ms] 5) {} state 10)]
    (is (empty? (:commands blocked)))
    (is (= :unsatisfied-emission (get-in blocked [:reasons 0 :reason])))
    (is (= #{:motion :red} (set (map :cue-id (:commands expired)))))))

(deftest musical-intervals-use-beats-pause-and-captured-meter
  (let [show (assoc-in (model) [:transport :meter] {:beats-per-bar 3 :bars-per-phrase 2})
        config {:interval {:bars 1} :intent :darkness}
        state (policy/initial-state 1000 0 :musical)
        initial (policy/advance show config state 0)
        paused (policy/advance (-> show (assoc-in [:transport :beat] 1.5)
                                   (assoc-in [:transport :playing?] false)) config (:state initial) 90000)
        not-yet (policy/advance (-> show (assoc-in [:transport :beat] 2.9)
                                    (assoc-in [:transport :bpm] 240)
                                    (assoc-in [:transport :meter :beats-per-bar] 7)) config (:state paused) 91000)
        next (policy/advance (assoc-in show [:transport :beat] 3) config (:state not-yet) 92000)]
    (is (= {:bars 1} (get-in initial [:commands 0 :launch :duration])))
    (is (= 3 (get-in initial [:state :next-at-beat])))
    (is (empty? (:commands paused)))
    (is (empty? (:commands not-yet)))
    (is (= 1 (count (:commands next))))
    (is (= 6 (get-in next [:state :next-at-beat])))
    (is (empty? (:commands (policy/advance (assoc-in show [:transport :playing?] false) config state 0))))))

(deftest beat-decisions-stay-on-the-grid-when-frames-miss-the-beat
  ;; #670 rehearsal finding: at 124 BPM a 125 ms frame is 0.258 beats, so a
  ;; one-beat decision lands up to a frame after its beat. The next interval
  ;; used to start from that late frame, and every later decision slid
  ;; further off the beat until a section boundary re-anchored it.
  (let [config {:interval {:beats 1} :intent :darkness}
        beats (map #(* % (/ 124.0 60000.0) 125.0) (range 0 200))
        decided (loop [state (policy/initial-state 1000 0 :grid) [beat & more] beats out []]
                  (if-not beat out
                    (let [result (policy/advance (assoc-in (model) [:transport :beat] beat) config state 0)]
                      (recur (:state result) more (cond-> out (seq (:commands result)) (conj beat))))))]
    (is (= (count decided) (inc (long (Math/floor (last beats))))) "one decision per beat, none skipped or doubled")
    (is (every? (fn [[i beat]] (<= i beat (+ i 0.2584))) (map-indexed vector decided))
        (pr-str (take 12 decided))))
  (testing "a decision taken off the grid starts the next interval on the grid"
    (let [state (:state (policy/advance (assoc-in (model) [:transport :beat] 64.07) {:interval {:bars 1} :intent :darkness}
                                        (policy/initial-state 1 0 :grid) 0))]
      (is (== 68 (:next-at-beat state)))))
  (testing "a half-beat interval aligns to half beats and is not due at once"
    (let [state (:state (policy/advance (assoc-in (model) [:transport :beat] 64.6) {:interval {:beats 1/2} :intent :darkness}
                                        (policy/initial-state 1 0 :grid) 0))]
      (is (== 65 (:next-at-beat state))))))

(defn- decisions
  "Beats at which auto decided over frames {:beat b :config c}; frames are
  125 ms at 124 BPM unless given. Returns [decided-beats final-state]."
  ([config frames] (decisions config frames (policy/initial-state 1 0 :grid)))
  ([config frames state]
   (loop [state state [{:keys [beat] :as f} & more] frames out []]
     (if-not beat [out state]
       (let [result (policy/advance (assoc-in (model) [:transport :beat] beat) (:config f config) state 0)]
         (recur (:state result) more (cond-> out (seq (:commands result)) (conj beat))))))))

(def ^:private frame-beats (* 125.0 (/ 124.0 60000.0)))

(defn- frames-from [beat n] (map #(hash-map :beat (+ beat (* % frame-beats))) (range n)))

(deftest a-start-gap-or-change-just-before-a-line-never-decides-twice-in-a-row
  ;; Review of #766: re-anchoring to the last line at or before now made the
  ;; next due point the coming line, one frame later: a catch-up decision.
  (testing "auto enabled just before a bar line waits a full bar-ish interval"
    (let [[decided] (decisions {:interval {:bars 1} :intent :darkness} (frames-from 3.9 30))]
      (is (= [3.9 8] (mapv #(if (< % 4) % (long (Math/floor %))) decided)) (pr-str decided))))
  (testing "a phrase interval enabled just before a phrase line"
    (let [[decided] (decisions {:interval {:phrases 1} :intent :darkness} (frames-from 15.8 80))]
      (is (= 2 (count decided)) (pr-str decided))
      (is (<= 32 (second decided) (+ 32 frame-beats)))))
  (testing "a frame gap that lands just before a line"
    (let [frames (concat (frames-from 0.0 40) (frames-from 20.92 12))
          [decided] (decisions {:interval {:beats 1} :intent :darkness} frames)
          after (filter #(>= % 20.9) decided)]
      (is (= 20.92 (first after)))
      (is (<= 22 (second after) (+ 22 frame-beats)) (pr-str after))))
  (testing "a forward jump inside an interval that lands just before the next line"
    (let [frames (concat (frames-from 0.0 40) (frames-from 15.9 20))
          [decided] (decisions {:interval {:bars 1} :intent :darkness} frames)
          after (filter #(>= % 15.9) decided)]
      (is (= 15.9 (first after)))
      (is (<= 20 (second after) (+ 20 frame-beats)) (pr-str after))))
  (testing "changing the interval mid-run puts the new one on its beat grid"
    (let [frames (map-indexed (fn [i f] (cond-> f (> i 40) (assoc :config {:interval {:beats 2} :intent :darkness})))
                              (frames-from 0.0 160))
          [decided] (decisions {:interval {:beats 1} :intent :darkness} frames)
          later (filter #(> % 12) decided)]
      (is (seq later))
      (is (every? #(< (mod % 1) (+ frame-beats 1e-9)) later) (pr-str (take 6 later)))
      (is (every? #{2} (map - (rest (map #(long (Math/floor %)) later)) (map #(long (Math/floor %)) later))) "every second beat")))
  (testing "a section plan entered just before a bar line"
    (let [plan {:intent :darkness :interval {:bars 1}
                :sections [{:id :only :length {:bars 2} :config {}}]}
          [decided] (decisions plan (frames-from 3.9 60))
          [_ state] (decisions plan (frames-from 3.9 1))]
      (is (= 3.9 (first decided)))
      (is (<= 8 (second decided) (+ 8 frame-beats)) (pr-str decided))
      (is (== 12 (+ (get-in state [:section :timer :started-at]) (get-in state [:section :timer :duration :value])))
          "the first section ends on the first bar line at least 1.5 bars away"))))

(deftest a-rebase-off-the-grid-returns-decisions-to-the-grid
  ;; Review of #766: clock/rebase shifts the interval timer by the seek's
  ;; fractional offset; a due point off the grid is not kept.
  (let [config {:interval {:bars 1} :intent :darkness}
        [_ state] (decisions config (frames-from 0.0 41))
        moved (clock/rebase (-> (model) (assoc-in [:transport :beat] (* 40 frame-beats)) (assoc :auto {:state state}))
                            {:beat 50.0})
        [decided] (decisions config (frames-from (+ 50.0 frame-beats) 120) (get-in moved [:auto :state]))]
    (is (seq decided))
    (is (every? #(< (mod % 4) (+ frame-beats 1e-9)) (rest decided)) (pr-str (take 5 decided)))))

(deftest musical-emission-attacks-participate-in-real-prediction
  (let [show (update (model) :registry registry/publish :effects
                     (assoc emission :version 2 :envelope {:attack {:beats 1/2} :release {:beats 1/4}}))
        result (policy/advance show {:interval {:beats 2}} (policy/initial-state 1000 0 :fades) 0)]
    (is (= #{:motion :red} (set (map :cue-id (:commands result)))))
    (is (= :added-emission (get-in result [:reasons 0 :reason])))))

;; ---------------------------------------------------------------------------
;; #217: semantic launch parameters, performers, categories, quiet sections

(def level-emission
  {:id :level-emission :version 1
   :params {:level {:type :number :default 1.0 :bounds [0 1]}}
   :outputs {:color [1 0 0] :dimmer {:op :param :key :level}}})

(defn- show-model [m] (merge (control/new-show {:show-id (:show-id m)}) m))

(defn- dispatch
  "Apply planner commands through the real reducer and record accepted launches."
  [model state commands]
  (reduce (fn [[model state] c]
            (let [{:keys [model ack]} (spatial/apply-command model c)]
              [model (if (and (= :cue/launch (:op c)) (= :accepted (:status ack)))
                       (policy/record-launch state (:instance-id ack) (get-in model [:instances (:instance-id ack)]))
                       state)]))
          [model state] commands))

(deftest launches-carry-seeded-parameters-from-their-domains
  (let [show (-> (model)
                 (update :registry registry/publish :effects level-emission)
                 (update :registry registry/publish :cues {:id :dim :effect-id :level-emission}))
        config {:scope {:cue-ids [:dim]} :randomize {:ranges {:level [0.6 0.9]}}}
        state (policy/initial-state 9 0 :auto)
        result (policy/advance show config state 0)
        level (get-in result [:commands 0 :launch :params :level])]
    (is (<= 0.6 level 0.9))
    (is (= result (policy/advance show config state 0)) "same seed, same show, same plan")
    (is (= {:level level} (get-in result [:reasons 0 :randomized :dim :params])))
    (testing "a range that leaves the light dark is predicted as dark, so motion needs another source"
      (let [dark (policy/advance show (assoc config :scope {:cue-ids [:motion :dim]}
                                             :randomize {:ranges {:level [0 0]}}) state 0)]
        (is (empty? (:commands dark)))
        (is (= :unsatisfied-emission (get-in dark [:reasons 0 :reason])))))))

(deftest a-performer-edit-takes-an-instance-out-of-auto-ownership
  (let [show (show-model (model))
        config {:scope {:cue-ids [:red]} :launch {:mode :persistent}}
        state (policy/initial-state 1 0 :auto)
        {:keys [commands] :as plan} (policy/advance show config state 0)
        [before state] (dispatch show (:state plan) commands)
        id (first (:owned-instance-ids state))
        _ (is (string? id))
        {show :model} (spatial/apply-command before {:op :instance/opacity-set :instance-id id :value 0.5
                                                   :command-id "push-1" :show-id "auto" :epoch 1
                                                   :source {:adapter :push :session-id "push"} :time-ms 10})
        next (policy/advance show (assoc config :interval {:beats 1}) state 10)]
    (is (some #(= {:reason :performer-took-over :instance-ids [id]} %) (:reasons next)))
    (is (empty? (get-in next [:state :owned-instance-ids])))
    (is (empty? (:commands (policy/stop-plan show (:state next) 20))) "stopping auto leaves the performer's look")
    (is (= [id] (mapv :instance-id (:commands (policy/stop-plan before state 20))))
        "before the edit auto still owned it")))

(deftest auto-yields-cues-a-performer-runs-and-filters-categories
  (let [show (show-model (model))
        {show :model} (spatial/apply-command show {:op :cue/launch :cue-id :red :launch {:mode :persistent}
                                                   :command-id "push-red" :show-id "auto" :epoch 1
                                                   :source {:adapter :push :session-id "push"} :time-ms 0})
        state (policy/initial-state 1000 0 :auto)
        yielded (policy/advance show {} state 0)
        stacked (policy/advance show {:yield-to-performers? false :count 2} state 0)
        no-motion (policy/advance (show-model (model)) {:categories {:exclude [:movement]}} state 0)]
    (is (= [:motion] (mapv :cue-id (:commands yielded))) "the performer's red already lights the head")
    (is (= #{:motion :red} (set (map :cue-id (:commands stacked)))))
    (is (= [:red] (mapv :cue-id (:commands no-motion))))))

(def two-heads
  [fixture (assoc fixture :id :head-2 :address 11 :position [1 0 0])])

(deftest a-quiet-section-limits-lit-fixtures-without-darkening-performers
  (let [one {:id :one :version 1 :targets [:head] :outputs {:color [1 1 1] :dimmer 1}}
        both {:id :both :version 1 :outputs {:color [1 0 0] :dimmer 1}}
        show (-> (model) (assoc :fixtures two-heads)
                 (update :registry #(-> % (registry/publish :effects one) (registry/publish :effects both)
                                        (registry/publish :cues {:id :spot :effect-id :one})
                                        (registry/publish :cues {:id :wash :effect-id :both}))))
        config {:scope {:cue-ids [:spot :wash]} :max-lit 1 :count 2}]
    (doseq [seed (range 6)]
      (let [result (policy/advance show config (policy/initial-state seed 0 :auto) 0)]
        (is (= [:spot] (mapv :cue-id (:commands result))))
        (is (= 1 (get-in result [:reasons 0 :lit])))))
    (let [lit (assoc show :assignments [{:target-id :head-2 :attribute :color :value [1 1 1] :blend :replace}
                                        {:target-id :head-2 :attribute :dimmer :value 1 :blend :replace}])
          result (policy/advance lit config (policy/initial-state 0 0 :auto) 0)]
      (is (empty? (:commands result)) "existing light already fills the limit")
      (is (= :lit-limit (get-in result [:reasons 0 :reason]))))))

(deftest section-plans-follow-the-beat-cut-on-entry-and-replay
  (let [show (show-model (model))
        config {:interval {:beats 1} :launch {:mode :persistent}
                :sections [{:id :glow :weight 3 :length {:bars 1} :config {:scope {:cue-ids [:red]}}}
                           {:id :dark :weight 1 :length {:beats 2} :max-run 1 :transition :cut
                            :config {:scope {:cue-ids [:motion]} :intent :darkness}}]}
        run (fn [seed]
              (loop [beat 0 show show state (policy/initial-state seed 0 :auto) log []]
                (if (> beat 24) log
                  (let [show (-> show (assoc-in [:transport :beat] beat) (assoc :time-ms (* beat 500)))
                        {:keys [commands] :as plan} (policy/advance show config state (* beat 500))
                        [show state] (dispatch show (:state plan) commands)]
                    (recur (inc beat) show state
                           (conj log [beat (get-in state [:section :id]) (mapv (juxt :op :cue-id :force?) commands)]))))))
        log (run 6)
        sections (map second log)]
    (is (= log (run 6)) "seeded replay")
    (is (not= sections (map second (run 7))))
    (is (= #{:glow :dark} (set sections)))
    (is (not-any? (fn [[a b]] (= :dark a b)) (partition 2 1 (map first (partition-by identity sections))))
        "a dark section never follows itself")
    (testing "entering the dark section force-ends the glow it owns, then moves in darkness"
      (let [[_ [_ _ entry]] (first (filter (fn [[[_ a] [_ b]]] (and (= :glow a) (= :dark b))) (partition 2 1 log)))]
        (is (= [:instance/end nil true] (first entry)))
        (is (= [:cue/launch :motion nil] (last entry)))))
    (testing "a stopped transport holds the section"
      (let [state (:state (policy/advance show config (policy/initial-state 6 0 :auto) 0))
            stopped (assoc-in show [:transport :playing?] false)]
        (is (= {:state state :commands [] :reasons []}
               (policy/advance (assoc-in stopped [:transport :beat] 40) config state 99999)))))))

(deftest configs-are-validated-before-planning
  (let [state (policy/initial-state 0 0 :auto)]
    (doseq [bad [{:count 0} {:intent :bright} {:max-lit -1} {:randomize {:params :some}}
                 {:categories {:include :strobe}} {:activity-target 2}
                 {:sections [{:id :a :length {:ms 100}}]}
                 {:sections [{:id :a :length {:bars 1} :config {:sections []}}]}
                 {:surprise true}]]
      (is (seq (policy/validate-config bad)) (pr-str bad))
      (is (thrown? clojure.lang.ExceptionInfo (policy/advance (model) bad state 0)) (pr-str bad)))
    (is (empty? (policy/validate-config {:activity-target {:waveform :sine :period {:bars 4}}})))
    (is (seq (:commands (policy/advance (model) {:activity-target {:waveform :sine :period {:bars 4}}} state 0))))))

(deftest most-of-the-pool-refused-still-plans-and-is-remembered
  ;; Eight cues write a shutter the head does not have; prediction refuses
  ;; each. The planner drops them one by one and still lights the movement.
  (let [bad-ids (mapv #(keyword (str "bad-" %)) (range 8))
        show (reduce (fn [m id]
                       (-> m (update :registry registry/publish :effects {:id id :version 1 :targets [:head]
                                                                         :outputs {:shutter 1.0 :dimmer 1.0}})
                           (update :registry registry/publish :cues {:id id :effect-id id})))
                     ;; The show checks written attributes against the patched rig.
                     (assoc (model) :rig {:fixtures [fixture]}) bad-ids)
        config {:scope {:cue-ids (into [:motion :red] bad-ids)} :interval {:beats 1}}]
    (is (some #(>= % 5)
              (map #(count (some :refused (:reasons (policy/advance show config (policy/initial-state % 0 :auto) 0))))
                   (range 40)))
        "some seed meets most of the bad cues before finding light")
    (doseq [seed (range 12)]
      (let [first-plan (policy/advance show config (policy/initial-state seed 0 :auto) 0)
            refused (set (map :cue-id (some :refused (:reasons first-plan))))
            remembered (set (keys (get-in first-plan [:state :refusals])))
            second-plan (policy/advance (assoc-in show [:transport :beat] 1) config (:state first-plan) 500)]
        (is (seq (:commands first-plan)) (str "seed " seed))
        (is (every? (set bad-ids) refused))
        (is (= refused remembered) "every refusal is remembered for this rig and these versions")
        (is (not-any? #(remembered (:cue-id %)) (:commands second-plan)))
        (is (empty? (filter #(remembered %) (map :cue-id (some :refused (:reasons second-plan)))))
            "a remembered refusal is not tried again")))
    (testing "a new effect version is tried again"
      (let [plan (some #(let [p (policy/advance show config (policy/initial-state % 0 :auto) 0)]
                          (when (seq (get-in p [:state :refusals])) p))
                       (range 50))
            id (first (keys (get-in plan [:state :refusals])))
            edited (update show :registry registry/publish :effects {:id id :version 2 :targets [:head] :outputs {:dimmer 1.0 :color [1 0 0]}})]
        (is (keyword? id))
        (is (not-any? #(= id (:id %)) (#'policy/candidates show config (:state plan))))
        (is (some #(= id (:id %)) (#'policy/candidates edited config (:state plan))))))))

(deftest scopes-launches-and-section-scopes-are-validated-up-front
  (doseq [bad [{:scope :nope} {:scope {:page-id :p :cue-ids [:a]}} {:launch {:mode :held}}
               {:launch {:mode :timed :duration {:bars 0}}} {:launch {:params 3}}
               {:sections [{:id :a :length {:bars 1}} {:id :b :length {:bars 1} :config {:scope :nope}}]}]]
    (is (seq (policy/validate-config bad)) (pr-str bad)))
  (is (= [[:sections 1 :config :scope]]
         (mapv :path (policy/validate-references (model) {:sections [{:id :a :length {:bars 1}}
                                                                     {:id :b :length {:bars 1} :config {:scope {:page-id :gone}}}]}))))
  (is (= [{:config {:count 2 :sections [{:id :a :length {:bars 1} :config {:count 1}}]}
           :dropped [[:note] [:sections 0 :config :legacy]]
           :unrecognized {:config {:note "x"} :sections {:a {:legacy true}}}}]
         [(policy/sanitize-config {:count 2 :note "x" :sections [{:id :a :length {:bars 1} :config {:count 1 :legacy true}}]})])))

(deftest set-aside-settings-go-back-verbatim
  (let [config {:count 2 :note "x" :sections [{:id :a :length {:bars 1} :config {:count 1 :legacy true}}
                                              {:id :b :length {:bars 1}}]}
        {:keys [unrecognized] clean :config} (policy/sanitize-config config)]
    (is (= config (policy/restore-unrecognized clean unrecognized)))))

(deftest only-definition-refusals-are-remembered
  (let [show (assoc (model) :rig {:fixtures [fixture]})
        state (policy/initial-state 0 0 :auto)]
    (is (empty? (:refusals (policy/record-refusal state show :red {:code :start-wrap-mismatch}))))
    (is (empty? (:refusals (policy/record-refusal state show :red {:code :invalid-duration}))))
    (is (= :incapable-target (get-in (policy/record-refusal state show :red {:code :incapable-target}) [:refusals :red :code])))
    (testing "invalid parameters are remembered only when the launch brought none"
      (is (= :invalid-params (get-in (policy/record-refusal state show :red {:code :invalid-params}) [:refusals :red :code])))
      (is (empty? (:refusals (policy/record-refusal state show :red {:code :invalid-params} {:level 2.0})))))))

(deftest keep-plan-keeps-randomize-domains-across-stop-and-start
  (let [repl {:sections [{:id :a :length {:bars 1}}] :randomize {:params :marked :palette [{:color [1 0 0]}] :ranges {:level [0 1]}}}
        stopped (policy/keep-plan repl {} false)
        unvaried (policy/keep-plan stopped {:count 1} true)
        varied (policy/keep-plan unvaried {:randomize {:params "marked"}} true)]
    (is (= (:randomize repl) (:randomize stopped)) "a stop keeps the whole selection")
    (is (= {:params :none :palette [{:color [1 0 0]}] :ranges {:level [0 1]}} (:randomize unvaried)))
    (is (= {:params "marked" :palette [{:color [1 0 0]}] :ranges {:level [0 1]}} (:randomize varied)))))
