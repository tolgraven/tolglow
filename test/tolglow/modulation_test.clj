(ns tolglow.modulation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [afterglow.control.model :as control]
            [afterglow.control.params :as params]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.executor :as executor]
            [afterglow.time :as time]
            [tolglow.automation.random :as random]
            [tolglow.compiler :as compiler]
            [tolglow.modulation :as modulation]))

(def effect {:id :wash :version 1 :category :light
             :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
             :outputs {:dimmer {:op :param :key :level}}})

(defn instance [id definition]
  {:id id :cue-id :wash :effect-id (:id definition) :effect-version (:version definition)
   :definition definition :params (params/resolve-values (:params definition {}) {})
   :state {} :priority 0 :order 0 :phase :running :reload-policy :pinned
   :started-at-ms 0.0 :started-at-beat 0.0 :release-duration-ms 0
   :timing-meter time/default-meter})

(defn model
  ([] (model effect))
  ([definition]
   (control/new-show {:show-id :modulation-test :rig {:fixtures [{:id :head :position [0.0 0.0 0.0]
                                                                :universe 1 :address 1 :channels {:dimmer 1}}]}
                      :registry {:effects {(:id definition) definition}}
                      :instances {"0:1" (instance "0:1" definition)}})))

(defn frame [beat] {:beat beat :time-ms (* beat 500.0)})

(defn start
  ([] (start {}))
  ([config] (modulation/start (modulation/new-state)
                              (merge {:id :random :seed 7 :params [:level]} config) (frame 0))))

(defn level [result] (get-in result [:overrides "0:1" :level]))

(defn controller [result] (first (get-in result [:state :controllers])))

(deftest fractional-musical-steps-catch-up-and-pause-with-identical-seed-state
  (let [state (start {:every {:beats 1/4}}) show (model)
        initial (modulation/advance state show (frame 0))
        [seed value] (random/next-random 7)
        between (modulation/advance (:state initial) show (frame 1/8))
        paused (modulation/advance (:state between) show {:beat 1/8 :time-ms 90000.0})
        jumped (modulation/advance state show (frame 1))
        stepped (reduce (fn [state beat] (:state (modulation/advance state show (frame beat))))
                        state [0 1/4 1/2 3/4 1])]
    (is (= value (level initial)))
    (is (= seed (:seed (controller initial))))
    (is (= 1/4 (:next-beat (controller initial))))
    (is (= value (level between)))
    (is (= seed (:seed (controller between))))
    (is (= (:state between) (:state paused)))
    (is (= (:state jumped) stepped))
    (is (= 5 (:ticks (controller jumped))))
    (is (= 5/4 (:next-beat (controller jumped))))))

(deftest captured-bars-and-phrases-survive-meter-and-tempo-changes
  (doseq [[every expected] [[{:bars 1} 3.5] [{:phrases 1} 7.0]]]
    (let [state (modulation/start (modulation/new-state) {:id :meter :every every :params [:level]}
                                  {:beat 8.0 :meter {:beats-per-bar 3.5 :bars-per-phrase 2}})
          first-frame (modulation/advance state (model) {:beat 8.0 :time-ms 0.0 :meter {:beats-per-bar 4}})
          second-frame (modulation/advance (:state first-frame) (model)
                                            {:beat (+ 8.0 expected) :time-ms 10000.0 :bpm 30})]
      (is (= expected (:interval-beats (controller first-frame))))
      (is (= (+ 8.0 expected) (:next-beat (controller first-frame))))
      (is (= 2 (:ticks (controller second-frame)))))))

(def all-types
  (assoc effect :params
         {:level {:type :number :default 0.5 :bounds [0.0 1.0] :recommended [0.3 0.7]
                  :random {:max-change 0.1}}
          :steps {:type :integer :default 2 :bounds [1 4]}
          :gate {:type :boolean :default false}
          :shape {:type :enum :values [:sine :triangle :square] :default :sine}
          :color {:type :color :default [0.2 0.4 0.8]}
          :position {:type :vector :size 3 :bounds [-2.0 2.0] :default [0.0 0.0 0.0]}
          :spread {:type :range :bounds [0.0 10.0] :default [2.0 7.0]}
          :unbounded {:type :number :default 100.0}}))

(deftest every-schema-type-randomizes-and-missing-domains-are-explicit
  (doseq [mode [:sample-hold :glide]]
    (let [show (model all-types)
          first-frame (modulation/advance (start {:params :all :mode mode}) show (frame 0))
          next-frame (modulation/advance (:state first-frame) show (frame 1/2))
          values (get-in next-frame [:overrides "0:1"])]
      (is (= (disj (set (keys (:params all-types))) :unbounded) (set (keys values))))
      (is (empty? (params/validate-values (:params all-types) values)))
      (is (integer? (:steps values)))
      (is (<= 0.4 (:level values) 0.6))
      (is (= :unbounded (-> first-frame :diagnostics first :param)))
      (is (= :modulation/skipped (-> first-frame :diagnostics first :type)))
      (is (= (get-in show [:instances "0:1" :params]) (params/resolve-values (:params all-types) {})))))
  (let [result (modulation/advance (start {:params [:unbounded] :ranges {:unbounded [80.0 120.0]}})
                                    (model all-types) (frame 0))]
    (is (<= 80.0 (get-in result [:overrides "0:1" :unbounded]) 120.0))
    (is (empty? (:diagnostics result)))))

(deftest glide-curves-interpolate-without-stealing-canonical-values
  (let [state (start {:mode :glide :curve :ease-in}) show (model)
        initial (modulation/advance state show (frame 0))
        target (get-in (controller initial) [:values "0:1" :level :to])
        middle (modulation/advance (:state initial) show (frame 1/2))
        boundary (modulation/advance (:state middle) show (frame 1))]
    (is (= 0.0 (level initial)))
    (is (= (* target 0.25) (level middle)))
    (is (= target (level boundary)))
    (is (= 0.0 (get-in show [:instances "0:1" :params :level])))
    (is (= 2 (:ticks (controller boundary))))))

(deftest selector-intersections-categories-and-overlap-precedence
  (let [show (-> (model) (assoc-in [:instances "0:2"]
                                  (assoc (instance "0:2" (assoc effect :id :movement :category :movement)) :order 1)))
        state (-> (modulation/new-state)
                  (modulation/start {:id :higher :params [:level] :ranges {:level [0.8 0.8]} :priority 9} (frame 0))
                  (modulation/start {:id :lower :params [:level] :ranges {:level [0.2 0.2]} :priority 0} (frame 0)))
        result (modulation/advance state show (frame 0))
        selected (modulation/advance (start {:select {:categories [:movement] :effect-ids [:movement]}
                                             :ranges {:level [0.3 0.3]}}) show (frame 0))
        missed (modulation/advance (start {:select {:categories [:movement] :effect-ids [:wash]}}) show (frame 0))]
    (is (= {"0:1" {:level 0.8} "0:2" {:level 0.8}} (:overrides result)))
    (is (= {"0:2" {:level 0.3}} (:overrides selected)))
    (is (= {} (:overrides missed)))
    (is (= {} (:overrides (modulation/advance (start {:select {:instance-ids []}}) show (frame 0)))))
    (is (= 0.2 (level (modulation/advance (modulation/stop state :higher) show (frame 0)))))))

(deftest output-derived-categories-select-effects-without-manual-tags
  (let [show (model (dissoc effect :category))
        intensity (modulation/advance (start {:select {:categories [:intensity]}}) show (frame 0))
        movement (modulation/advance (start {:select {:categories [:movement]}}) show (frame 0))]
    (is (contains? (:overrides intensity) "0:1"))
    (is (empty? (:overrides movement)))))

(deftest human-holds-win-and-resume-from-the-new-human-value
  (let [show (model) first-frame (modulation/advance (start {:mode :glide}) show (frame 0))
        held (modulation/hold (:state first-frame) {:instance-id "0:1" :params [:level] :until-beat 2.0})
        human (assoc-in show [:instances "0:1" :params :level] 0.9)
        suppressed (modulation/advance held human (frame 1))
        resumed (modulation/advance (:state suppressed) human (frame 2))
        indefinite (modulation/hold (:state resumed) {:instance-id "0:1" :params [:level]})
        stopped (modulation/advance indefinite human (frame 3))
        released (modulation/release-hold (:state stopped) "0:1" [:level])
        running (modulation/advance released human (frame 4))]
    (is (= {} (:overrides suppressed)))
    (is (= (:seed (controller first-frame)) (:seed (controller suppressed))))
    (is (= 0.9 (level resumed)))
    (is (= {} (get-in resumed [:state :holds])))
    (is (= {} (:overrides stopped)))
    (is (= 0.9 (level running)))
    (is (= 0.9 (get-in human [:instances "0:1" :params :level])))))

(defn with-owner [show config]
  (let [definition {:id :modulator :version 1 :modulation config :params {}}
        owner (assoc (instance "0:2" definition) :order 1 :priority 4)]
    (-> show (assoc-in [:registry :effects :modulator] definition) (assoc-in [:instances "0:2"] owner))))

(deftest launchable-cues-reconcile-and-stop-at-the-exact-musical-duration-boundary
  (let [show (with-owner (model) {:params [:level] :ranges {:level [0.7 0.7]} :every {:beats 1/4}})
        show (-> show (assoc-in [:instances "0:2" :duration] {:beats 1/2})
                 (assoc-in [:instances "0:2" :duration-timer] (time/start {:beats 1/2} (frame 0))))
        active (modulation/advance nil show (frame 1/4))
        expired (modulation/advance (:state active) show (frame 1/2))
        released (modulation/advance (:state active) (assoc-in show [:instances "0:2" :phase] :releasing) (frame 1/3))
        removed (modulation/advance (:state active) (update show :instances dissoc "0:2") (frame 1/3))]
    (is (= [:cue "0:2"] (:id (controller active))))
    (is (= 0.7 (level active)))
    (is (= 2 (:ticks (controller active))))
    (is (nil? (get-in active [:overrides "0:2"])))
    (doseq [result [expired released removed]]
      (is (empty? (get-in result [:state :controllers])))
      (is (empty? (:overrides result))))))

(deftest cue-reload-policies-and-restart-before-expiry-match-engine-order
  (let [show (-> (with-owner (model) {:params [:level] :seed 19})
                 (assoc-in [:instances "0:2" :reload-policy] :preserve-state))
        first-frame (modulation/advance nil show (frame 0))
        updated (assoc-in show [:registry :effects :modulator :version] 2)
        preserved (modulation/advance (:state first-frame) updated (frame 1/2))
        restarted-show (-> updated (assoc-in [:instances "0:2" :reload-policy] :restart)
                           (assoc-in [:instances "0:2" :duration-ms] 250.0))
        restarted (modulation/advance (:state first-frame) restarted-show (frame 1/2))
        changed (modulation/advance (:state first-frame)
                                    (assoc-in updated [:registry :effects :modulator :modulation :seed] 23) (frame 1/2))]
    (is (= (:seed (controller first-frame)) (:seed (controller preserved))))
    (is (= 1.0 (double (:next-beat (controller preserved)))))
    (is (= 2 (:owner-version (controller preserved))))
    (is (= 1/2 (:anchor-beat (controller restarted))))
    (is (= 1 (:ticks (controller restarted))))
    (is (= 3/2 (:next-beat (controller restarted))))
    (is (= (first (random/next-random 23)) (:seed (controller changed))))))

(deftest quarantined-cue-owners-stop-overrides-and-rejected-reloads-keep-the-last-good-config
  (let [show (-> (with-owner (model) {:params [:level] :ranges {:level [0.7 0.7]}})
                 (assoc-in [:instances "0:2" :reload-policy] :preserve-state))
        active (modulation/advance nil show (frame 0))
        compiler-for (fn [failed-version]
                       (fn [definition]
                         (if (= :modulator (:id definition))
                           (fn [context]
                             (when (= failed-version (:version definition)) (throw (ex-info "Bad owner" {})))
                             context)
                           (compiler/compile-effect definition))))
        snapshot {:time-ms 0.0 :fixtures (get-in show [:rig :fixtures])}
        failed (:model (engine/advance show snapshot (compiler-for 1)))
        stopped (modulation/advance (:state active) failed (frame 0))
        candidate (-> show (assoc-in [:registry :effects :modulator :version] 2)
                           (assoc-in [:registry :effects :modulator :modulation :ranges :level] [0.3 0.3]))
        rejected (:model (engine/advance candidate snapshot (compiler-for 2)))
        retained (modulation/advance (:state active) rejected (frame 0))]
    (is (get-in failed [:instances "0:2" :fault]))
    (is (empty? (:overrides stopped)))
    (is (empty? (get-in stopped [:state :controllers])))
    (is (= 2 (get-in rejected [:instances "0:2" :reload-fault :effect-version])))
    (is (= 0.7 (level retained)))
    (is (= 1 (:owner-version (controller retained))))))

(deftest save-and-restore-remap-identities-without-resetting-seeds-or-next-beats
  (let [show (with-owner (model) {:seed 41 :params [:level] :select {:instance-ids ["0:1"]}})
        original (modulation/advance nil show (frame 1/4))
        saved (edn/read-string (pr-str (:state original)))
        ids {"0:1" "9:4" "0:2" "9:5"}
        restored (modulation/restore-state saved {:show-id :modulation-test :epoch 9 :instance-ids ids})
        next-show (-> show (assoc :epoch 9)
                      (update :instances #(into {} (map (fn [[id instance]] [(get ids id) (assoc instance :id (get ids id))])) %)))
        same-beat (modulation/advance restored next-show (frame 1/4))
        next-frame (modulation/advance (:state same-beat) next-show (frame 1))
        expected (modulation/advance (:state original) show (frame 1))]
    (is (= [] (modulation/validate-state restored)))
    (is (= (:seed (controller original)) (:seed (controller same-beat))))
    (is (= (:next-beat (controller original)) (:next-beat (controller same-beat))))
    (is (= [:cue "9:5"] (:id (controller same-beat))))
    (is (= ["9:4"] (get-in (controller same-beat) [:config :select :instance-ids])))
    (is (= (get-in expected [:overrides "0:1"]) (get-in next-frame [:overrides "9:4"])))
    (is (= (:seed (controller expected)) (:seed (controller next-frame))))
    (is (= {} (:overrides (modulation/advance (:state original)
                                             (assoc show :epoch 1 :instances {}) (frame 0)))))))

(deftest middleware-feeds-the-real-compiler-and-engine-without-parameter-drift
  (let [show (-> (model) (assoc-in [:instances "0:2"] (assoc (instance "0:2" effect) :order 1)))
        captured (modulation/advance (start {:ranges {:level [0.7 0.7]}}) show (frame 0))
        middleware (modulation/middleware (:overrides captured))
        compile (fn [definition] (comp (compiler/compile-effect definition) middleware))
        frame (assoc (frame 0) :fixtures (get-in show [:rig :fixtures]))
        serial (engine/advance show frame compile)]
    (is (= [0.7 0.7] (mapv :value (:assignments serial))))
    (is (= [0.0 0.0] (mapv :level (map :params (vals (get-in serial [:model :instances]))))))
    (with-open [^java.io.Closeable pool (executor/create {:workers 2 :timeout-ms 1000})]
      (is (= serial (engine/advance show frame compile (executor/mapper pool)))))))

(deftest target-hot-reload-uses-the-schema-the-engine-will-capture
  (let [next-definition (assoc effect :version 2
                              :params {:gain {:type :number :default 0.2 :bounds [0.0 1.0]}}
                              :outputs {:dimmer {:op :param :key :gain}})
        show (-> (model) (assoc-in [:instances "0:1" :reload-policy] :preserve-state)
                 (assoc-in [:registry :effects :wash] next-definition))
        modulated (modulation/advance (start {:params :all :ranges {:gain [0.4 0.4]}}) show (frame 0))
        middleware (modulation/middleware (:overrides modulated))
        rendered (engine/advance show (assoc (frame 0) :fixtures (get-in show [:rig :fixtures]))
                                  #(comp (compiler/compile-effect %) middleware))]
    (is (= {"0:1" {:gain 0.4}} (:overrides modulated)))
    (is (= 0.4 (-> rendered :assignments first :value)))
    (is (= {:gain 0.2} (get-in rendered [:model :instances "0:1" :params])))
    (is (= 2 (get-in rendered [:model :instances "0:1" :effect-version])))))

(deftest real-published-modulator-only-cue-launches-and-expires-through-the-engine
  (let [owner {:id :modulator :version 1 :params {} :duration {:beats 1/2}
               :modulation {:params [:level] :ranges {:level [0.6 0.6]}}}
        commands [{:op :registry/publish :kind :effects :definition effect}
                  {:op :registry/publish :kind :effects :definition owner}
                  {:op :cue/put :cue {:id :wash :effect-id :wash}}
                  {:op :cue/put :cue {:id :modulator :effect-id :modulator}}
                  {:op :cue/launch :cue-id :wash :time-ms 0.0}
                  {:op :cue/launch :cue-id :modulator :time-ms 0.0 :launch {:mode :one-shot}}]
        show (reduce (fn [show command]
                       (let [result (control/apply-command show
                                                           (merge {:show-id (:show-id show) :epoch (:epoch show)
                                                                   :command-id (str (:revision show)) :source :test} command))]
                         (is (= :accepted (get-in result [:ack :status])))
                         (:model result)))
                     (control/new-show {:rig (:rig (model))}) commands)
        tick (fn [show beat]
               (let [frame (assoc (frame beat) :fixtures (get-in show [:rig :fixtures]))
                     result (modulation/advance (:modulation show) show frame)
                     middleware (modulation/middleware (:overrides result))]
                 (engine/advance (assoc show :modulation (:state result)) frame
                                  #(comp (compiler/compile-effect %) middleware))))
        active (tick show 0) expired (tick (:model active) 1/2)]
    (is (= [0.6] (mapv :value (:assignments active))))
    (is (= [0.0] (mapv :value (:assignments expired))))
    (is (= ["0:1"] (vec (keys (get-in expired [:model :instances])))))
    (is (empty? (get-in expired [:model :modulation :controllers])))))

(deftest restored-manual-holds-retain-their-exact-deadline-and-rng
  (let [show (model) initial (modulation/advance (start) show (frame 0))
        held (modulation/hold (:state initial) {:instance-id "0:1" :params [:level] :until-beat 2})
        restored (modulation/restore-state (edn/read-string (pr-str held))
                                            {:show-id :modulation-test :epoch 4 :instance-ids {"0:1" "4:9"}})
        new-show (-> show (assoc :epoch 4 :instances {"4:9" (assoc (get-in show [:instances "0:1"]) :id "4:9")}))
        still-held (modulation/advance restored new-show (frame 1))
        resumed (modulation/advance (:state still-held) new-show (frame 2))]
    (is (= 2 (get-in restored [:holds "4:9" :level :until-beat])))
    (is (= {} (:overrides still-held)))
    (is (= (:seed (controller initial)) (:seed (controller still-held))))
    (is (contains? (:overrides resumed) "4:9"))
    (is (= (first (random/next-random (:seed (controller initial)))) (:seed (controller resumed))))))

(deftest explicit-loop-and-forward-seek-rebase-preserve-glide-phase-and-rng
  (let [show (model (assoc-in effect [:params :other] {:type :number :default 0.4 :bounds [0.0 1.0]}))
        initial (modulation/advance (start {:mode :glide}) show (frame 0))
        middle (modulation/advance (:state initial) show (frame 1/2))
        held (modulation/hold (:state middle) {:instance-id "0:1" :params [:other] :until-beat 2})
        rebased (modulation/rebase-clock held {:beat -7/2})
        looped (modulation/advance rebased show {:time-ms 10000.0 :beat -7/2})
        next-looped (modulation/advance (:state looped) show {:time-ms 10500.0 :beat -3})
        next-original (modulation/advance held show (frame 1))
        seeked (modulation/rebase-clock held {:beat 1000.0})
        after-seek (modulation/advance seeked show {:time-ms 11000.0 :beat 1000.0})]
    (is (= (level middle) (level looped)))
    (is (= (:seed (controller middle)) (:seed (controller looped))))
    (is (= -3 (:next-beat (controller looped))))
    (is (= -2 (get-in rebased [:holds "0:1" :other :until-beat])))
    (is (= (level next-original) (level next-looped)))
    (is (= (:seed (controller next-original)) (:seed (controller next-looped))))
    (is (= (level middle) (level after-seek)))
    (is (= (:ticks (controller middle)) (:ticks (controller after-seek))))
    (is (= 1000.5 (:next-beat (controller after-seek))))
    (is (= [] (modulation/validate-state (edn/read-string (pr-str rebased)))))))

(deftest invalid-config-saved-state-clock-and-catchup-fail-before-returning-partial-output
  (doseq [config [{:every {:ms 100}} {:every {:beats 0}} {:every {:beats Double/NaN}}
                  {:select {:effects [:wash]}} {:mode :unknown} {:curve :unknown}
                  {:ranges {:level [1 0]}} {:seed 0.5} {:max-catch-up 0}]]
    (is (seq (modulation/validate-config config))))
  (let [state (start {:max-catch-up 2}) first-frame (modulation/advance state (model) (frame 0))]
    (is (thrown? clojure.lang.ExceptionInfo (modulation/advance state (model) (frame 2))))
    (is (= 7 (get-in state [:controllers 0 :seed])))
    (is (thrown? clojure.lang.ExceptionInfo (modulation/advance (:state first-frame) (model) (frame -1))))
    (is (seq (modulation/validate-state (assoc-in state [:controllers 0 :seed] (atom 2)))))
    (is (seq (modulation/validate-state (update-in state [:controllers 0 :config] dissoc :max-catch-up))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (modulation/start (modulation/new-state) {:id :overflow :every {:bars 1.0e308}}
                                   {:beat 0.0 :meter {:beats-per-bar 10}})))
    (is (seq (modulation/validate-state (assoc-in state [:holds "0:1" :level] {:until-beat "tomorrow"}))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (modulation/restore-state state {:show-id :x :epoch 2 :instance-ids {"a" "c" "b" "c"}})))))
