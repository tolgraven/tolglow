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

(deftest mixed-instance-priorities-advance-without-cue-controllers
  (let [show (-> (model)
                 (assoc-in [:instances "0:1" :priority] 0.25M)
                 (assoc-in [:instances "0:2"] (assoc (instance "0:2" effect) :order 1 :priority 1/3)))
        result (modulation/advance nil show (frame 0))]
    (is (empty? (get-in result [:state :controllers])))
    (is (= {} (:overrides result)))))

(deftest mixed-cue-controller-priorities-order-sampling-and-projections
  (let [owner (fn [id effect-id order priority value]
                (assoc (instance id {:id effect-id :version 1 :params {}
                                     :modulation {:priority priority :params [:level]
                                                  :ranges {:level [value value]}}})
                       :order order))
        first-id [:cue "0:2"]
        second-id [:cue "0:3"]]
    (doseq [[first-priority second-priority expected-ids expected-value]
            [[1/3 0.333333333333333333M [second-id first-id] 0.8]
             [1/2 0.5M [first-id second-id] 0.2]]]
      (let [show (-> (model)
                     (assoc-in [:instances "0:2"]
                               (assoc (owner "0:2" :first-modulator 1 first-priority 0.8)
                                      :render-priority 99 :render-order 0))
                     (assoc-in [:instances "0:3"] (owner "0:3" :second-modulator 2 second-priority 0.2)))
            result (modulation/advance nil show (frame 0))
            captured (assoc show :modulation (:state result))
            sources (get-in (modulation/source-index captured (:diagnostics result)) ["0:1" :level])]
        (is (= expected-ids (mapv :controller-id (:events result)) (mapv :controller-id sources)))
        (is (= expected-value (level result)))
        (is (= (:overrides result) (modulation/captured-overrides captured)))))))

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

(deftest reload-candidates-are-the-revisions-the-engine-will-attempt
  (let [show (-> (model) (assoc-in [:instances "0:1" :reload-policy] :preserve-state))
        wash (get-in show [:instances "0:1"])
        newer (assoc effect :version 2)
        pending (assoc-in show [:registry :effects :wash] newer)
        candidate #(modulation/reload-candidate %1 (merge wash %2))]
    (is (nil? (candidate show {})) "The registry still holds the captured version")
    (is (= newer (candidate pending {})))
    (is (= newer (modulation/owner-definition pending wash)))
    (is (true? (modulation/reload-pending? pending)))
    (is (false? (modulation/reload-pending? show)))
    (doseq [[label change] [["pinned" {:reload-policy :pinned}]
                            ["paused" {:system {:paused? true}}]
                            ["already rejected" {:reload-fault {:effect-version 2}}]
                            ["already adopted" {:effect-version 2 :definition newer}]
                            ["ahead of the registry" {:effect-version 3}]]]
      (is (nil? (candidate pending change)) label))
    (testing "a schema that rejects a retained parameter is rejected at preparation, so it is no candidate"
      (let [unfit (assoc-in pending [:registry :effects :wash :params :level] {:type :number :default 2.5 :bounds [2.0 3.0]})]
        (is (nil? (candidate unfit {})))
        (is (= effect (modulation/owner-definition unfit wash)))
        (is (= 0.0 (level (modulation/advance (start {:ranges {:level [0.0 0.0]}}) unfit (frame 0))))
            "The frame samples the captured schema instead of failing")
        (is (map? (modulation/source-index (assoc unfit :modulation (start)) [])))))
    (testing "during a ramp a retained parameter must fit its committed value, frame value and target"
      (let [narrowed (fn [bounds] (assoc-in pending [:registry :effects :wash :params :level]
                                            {:type :number :default (first bounds) :bounds bounds}))
            committed (get-in wash [:params :level])
            framed {:level (+ committed 0.2)}
            target {:level (+ committed 0.4)}
            ramped #(modulation/with-frame-params % {"0:1" framed} {"0:1" target})
            preparation #(modulation/ramp-preparation % (merge wash %2) framed target)]
        (is (= pending (modulation/with-frame-params pending {} {})) "No ramp leaves the model as it is")
        (is (= newer (candidate (ramped pending) {})) "A revision that accepts all three values is a candidate")
        (is (nil? (preparation pending {})) "Its engine pass keeps the frame's ramp values")
        (doseq [[label bounds prepared]
                [["the committed value" [(+ committed 0.1) 1.0] {}]
                 ["the frame value" [0.0 (+ committed 0.1)] nil]
                 ["the ramp target" [0.0 (+ committed 0.3)] target]]]
          (testing (str "a revision that refuses " label)
            (let [unfit (narrowed bounds)]
              (is (nil? (candidate (ramped unfit) {})) "is no candidate")
              (is (= prepared (preparation unfit {}))
                  "is prepared on the refused values, or on the frame values the engine already refuses"))))
        (is (nil? (preparation (narrowed [(+ committed 0.1) 1.0]) {:reload-policy :pinned}))
            "An instance the engine does not reload keeps its ramp values")))
    (testing "the last-good view has no candidates and leaves the model data alone"
      (let [view (modulation/last-good-view pending)]
        (is (nil? (modulation/reload-candidate view wash)))
        (is (false? (modulation/reload-pending? view)))
        (is (= effect (modulation/owner-definition view wash)))
        (is (= (:registry pending) (:registry view)))
        (is (= (:instances pending) (:instances view)))))))

(defn- owner-frame
  "One owner frame as the show runs it: sample both reload outcomes from the
  same prior state, let the engine read one of them, commit that one."
  [show beat compile]
  (let [frame (assoc (frame beat) :fixtures (get-in show [:rig :fixtures]))
        candidate (modulation/advance (:modulation show) show frame)
        last-good (when (modulation/reload-pending? show)
                    (modulation/advance (:modulation show) (modulation/last-good-view show) frame))
        overlay (if last-good
                  (modulation/reload-middleware (:overrides candidate) (:overrides last-good))
                  (modulation/middleware (:overrides candidate)))
        result (engine/advance (assoc show :modulation (:state candidate))
                               (cond-> frame last-good (assoc ::engine/reload-fallback modulation/last-good-frame))
                               #(comp (compile %) overlay))]
    (cond-> result
      (= :fallback (:reload-inputs result)) (assoc-in [:model :modulation] (:state last-good)))))

(deftest both-reload-outcomes-are-sampled-from-one-prior-state-and-one-is-committed
  (let [config {:params [:level] :seed 19 :every {:beats 1/4} :mode :glide}
        show (-> (with-owner (model) config) (assoc-in [:instances "0:2" :reload-policy] :preserve-state))
        changed (-> (get-in show [:registry :effects :modulator])
                    (assoc :version 2 :modulation (assoc config :seed 23 :ranges {:level [0.9 1.0]})))
        compile-with (fn [broken-version]
                       (fn [definition]
                         (if (= :modulator (:id definition))
                           (fn [context]
                             (when (= broken-version (:version definition)) (throw (ex-info "Bad owner" {})))
                             context)
                           (compiler/compile-effect definition))))
        run (fn [compile publish-at beats]
              (reductions (fn [result beat]
                            (owner-frame (cond-> (:model result)
                                           (= beat publish-at) (assoc-in [:registry :effects :modulator] changed))
                                         beat compile))
                          {:model show} beats))
        beats [0 1/8 1/4 3/8 1/2 5/8 3/4]
        observed (fn [results] (mapv (juxt :assignments (comp :controllers :modulation :model)) (rest results)))
        twin (run (compile-with nil) nil beats)
        rejected (run (compile-with 2) 3/8 beats)
        accepted (run (compile-with nil) 3/8 beats)]
    (testing "a rejected candidate leaves the output and the controller of a show that never saw it"
      (is (= (observed twin) (observed rejected)))
      (is (= [nil nil nil :fallback nil nil nil] (mapv :reload-inputs (rest rejected))))
      (is (= 2 (get-in (last rejected) [:model :instances "0:2" :reload-fault :effect-version]))))
    (testing "an accepted candidate restarts the controller once, on its takeover frame"
      (is (= (take 3 (observed twin)) (take 3 (observed accepted))))
      (is (= [nil nil nil :frame nil nil nil] (mapv :reload-inputs (rest accepted))))
      (is (= [[1 0 0.0] [1 0 0.0] [1 0 0.0] [2 1 3/8] [2 1 3/8] [2 1 3/8] [2 1 3/8]]
             (mapv (fn [result] ((juxt :owner-version :order :anchor-beat)
                                 (first (get-in result [:model :modulation :controllers]))))
                   (rest accepted))))
      (is (<= 0.9 (get-in (nth accepted 4) [:model :modulation :controllers 0 :values "0:1" :level :to]) 1.0)
          "The takeover frame already samples the new range"))
    (testing "sampling the other outcome consumes nothing: the same prior state gives the same result"
      (let [pending (assoc-in (:model (nth twin 3)) [:registry :effects :modulator] changed)
            frame (frame 3/8)
            prior (:modulation pending)
            first-sample (modulation/advance prior (modulation/last-good-view pending) frame)]
        (is (= first-sample (modulation/advance prior (modulation/last-good-view pending) frame)))
        (is (= (:state first-sample)
               (:state (modulation/advance prior (:model (nth twin 3)) frame)))
            "The last-good view samples exactly what the unpublished show samples")
        (is (not= (:state first-sample) (:state (modulation/advance prior pending frame))))))))

(deftest reload-middleware-reads-the-overrides-of-the-inputs-the-engine-evaluates
  (let [overlay (modulation/reload-middleware {"0:1" {:level 0.9}} {"0:1" {:level 0.2}})
        context {:instance {:id "0:1" :definition effect :params {:level 0.0}} :frame {:time-ms 0.0}}]
    (is (= 0.9 (get-in (overlay context) [:instance :params :level])))
    (is (= 0.2 (get-in (overlay (update context :frame merge modulation/last-good-frame)) [:instance :params :level])))
    (is (thrown? clojure.lang.ExceptionInfo (modulation/reload-middleware {"0:1" {:level 0.9}} nil)))))

(deftest overrides-that-do-not-follow-a-reload-are-restricted-to-the-candidates-schema
  (let [two {:id :wash :version 1 :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}
                                           :gain {:type :number :default 1.0 :bounds [0.0 1.0]}}}
        show (-> (model two)
                 (assoc-in [:instances "0:1" :reload-policy] :preserve-state)
                 (assoc-in [:instances "0:2"] (assoc (instance "0:2" two) :order 1)))
        ramped {"0:1" {:level 0.9 :gain 0.4} "0:2" {:level 0.9 :gain 0.4} "gone" {:level 0.1}}
        publish (fn [params] (assoc-in show [:registry :effects :wash] (assoc two :version 2 :params params)))]
    (is (= ramped (modulation/candidate-overrides show ramped)) "No candidate, nothing restricted")
    (is (= {} (modulation/candidate-overrides show nil)))
    (testing "a removed parameter and a value outside the candidate's bounds are dropped for the candidate only"
      (is (= (assoc ramped "0:1" {:level 0.9})
             (modulation/candidate-overrides (publish {:level (get-in two [:params :level])}) ramped)))
      (is (= (assoc ramped "0:1" {:gain 0.4})
             (modulation/candidate-overrides
               (publish (assoc-in (:params two) [:level :bounds] [0.0 0.5])) ramped))))
    (testing "an instance left with nothing has no entry, and the last-good view restricts nothing"
      (let [emptied (publish {:other {:type :number :default 0.0 :bounds [0.0 1.0]}})]
        (is (= (dissoc ramped "0:1") (modulation/candidate-overrides emptied ramped)))
        (is (= ramped (modulation/candidate-overrides (modulation/last-good-view emptied) ramped)))))))

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

(deftest gesture-holds-rebase-the-displaced-deadline-and-commit-as-permanent
  (let [initial (:state (modulation/advance (start) (model) (frame 0)))
        target {:instance-id "0:1" :params [:level] :until-beat 2}
        owner [:operator :encoder :gesture]
        held (-> initial
                 (modulation/hold target)
                 (modulation/hold-gesture-param "0:1" :level owner))
        rebased (modulation/rebase-clock held {:beat 10})
        canceled (modulation/settle-gesture-hold rebased "0:1" :level owner false)
        committed (modulation/settle-gesture-hold rebased "0:1" :level owner true)
        restored (modulation/restore-state held {:show-id :modulation-test :epoch 1 :instance-ids {"0:1" "1:1"}})
        path [:holds "0:1" :level]]
    (is (= {:until-beat 12} (get-in canceled path)))
    (is (= {:until-beat nil} (get-in committed path)))
    (is (= {:until-beat nil} (get-in restored [:holds "1:1" :level])))
    (is (= (:controllers rebased) (:controllers canceled) (:controllers committed)))))

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

(deftest invalid-config-saved-state-and-clock-fail-before-returning-partial-output
  (doseq [config [{:every {:ms 100}} {:every {:beats 0}} {:every {:beats Double/NaN}}
                  {:select {:effects [:wash]}} {:mode :unknown} {:curve :unknown}
                  {:ranges {:level [1 0]}} {:seed 0.5} {:max-catch-up 0}]]
    (is (seq (modulation/validate-config config))))
  (let [state (start {:max-catch-up 2}) first-frame (modulation/advance state (model) (frame 0))]
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

(deftest a-restart-reload-starts-the-owner-controller-on-the-private-clock
  ;; The engine restarts a :restart owner at its private clock. On the reload
  ;; frame modulation prepares the same instance, so the controller starts
  ;; where the following frame, reading the reloaded owner, would start it.
  (let [owner (assoc effect :id :owner :version 1 :params {} :outputs {})
        modulated (assoc owner :version 2 :modulation {:seed 3 :every {:beats 1} :params [:level]
                                                       :select {:cue-ids [:wash]} :ranges {:level [0.5 0.5]}})
        running (assoc (instance "0:2" owner) :cue-id :owner :order 1 :reload-policy :restart
                       :duration {:ms 3000}
                       :duration-timer (time/start {:ms 3000} {:time-ms 0.0 :beat 0.0})
                       :system {:clock-offset-ms 1000.0 :clock-offset-beats 2.0})
        frame {:time-ms 5000.0 :beat 10.0}
        base (-> (model) (assoc-in [:registry :effects :owner] modulated))
        reload-frame (assoc-in base [:instances "0:2"] running)
        reloaded (assoc-in base [:instances "0:2"]
                           (assoc running :definition modulated :effect-version 2
                                  :started-at-ms 4000.0 :started-at-beat 8.0
                                  :duration-timer (time/start {:ms 3000} {:time-ms 4000.0 :beat 8.0})))
        controller #(first (:controllers (modulation/reconcile-cues nil % frame)))]
    (is (= [:cue "0:2"] (:id (controller reload-frame))))
    (is (= 8.0 (:anchor-beat (controller reload-frame))) "the private beat, not the shared 10.0")
    (is (= (controller reloaded) (controller reload-frame)))))

(def two-to-the-40 1099511627776)

(deftest periods-that-cannot-start-or-schedule-are-refused-with-the-field
  (doseq [[every unit] [[{:bars 1.0e308} :bars] [{:phrases 1.0e307} :phrases] [{:beats 1.0e-300} :beats]
                        [{:beats 1/2048} :beats] [{:beats (inc two-to-the-40)} :beats]
                        [{:bars (/ two-to-the-40 2)} :bars] [{:phrases (/ two-to-the-40 8)} :phrases]]]
    (testing (pr-str every)
      (let [[error :as errors] (modulation/validate-config {:every every})]
        (is (= 1 (count errors)))
        (is (= [:every unit] (:path error)))
        (is (re-find #"^Modulation period :every \{" (:message error)))
        (is (re-find #"between 1/1024 and 2\^40" (:message error))))
      (let [errors (compiler/definition-errors :effects (assoc effect :modulation {:every every})
                                               {:publication? true})]
        (is (= [[:modulation :every unit]] (mapv :path errors)))
        (is (re-find #"^Modulation period :every" (:message (first errors)))))
      (is (= [] (compiler/definition-errors :effects (assoc effect :modulation {:every every})))
          "A saved show still compiles the definition an older owner accepted")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Modulation period :every"
                            (start {:every every})))))
  (doseq [every [{:beats 0} {:beats -1}]]
    (is (= [[:every :beats]] (mapv :path (modulation/validate-config {:every every}))) (pr-str every)))
  (doseq [every [{:beats Double/NaN} {:beats ##Inf}]]
    (is (seq (modulation/validate-config {:every every})) "Non-finite numbers are not portable data"))
  (is (= [[:every]] (mapv :path (modulation/validate-config {:every {:ms 100}}))))
  (doseq [every [{:beats 1/1024} {:beats two-to-the-40} {:bars (/ two-to-the-40 4)} {:phrases (/ two-to-the-40 16)}
                 {:bars 1/4096} {:phrases 1/16384}]]
    (is (= [] (modulation/validate-config {:every every})) (pr-str every))))

(deftest start-refuses-a-period-outside-the-range-in-its-captured-meter
  ;; Publication checks the default meter; the meter a controller captures can
  ;; still take an accepted bar period past the bound.
  (is (= [] (modulation/validate-config {:every {:bars (/ two-to-the-40 4)}})))
  (let [error (try (modulation/start (modulation/new-state) {:id :wide :every {:bars (/ two-to-the-40 4)}}
                                     {:beat 0.0 :meter {:beats-per-bar 8 :bars-per-phrase 4}})
                   nil (catch clojure.lang.ExceptionInfo error error))]
    (is (= ::modulation/invalid-period (:type (ex-data error))))
    (is (= [:every :bars] (:path (ex-data error))))
    (is (re-find #"captured meter \(8 beats per bar" (ex-message error)))))

(deftest borderline-periods-start-and-advance-their-schedule
  (testing "the shortest period catches up within the default budget"
    (let [state (start {:every {:beats 1/1024}})
          first-frame (modulation/advance state (model) (frame 0))
          later (modulation/advance (:state first-frame) (model) (frame 1/2))]
      (is (= [] (:diagnostics later)))
      (is (= 513 (:ticks (controller later))))
      (is (= 513/1024 (:next-beat (controller later))))
      (is (number? (level later)))))
  (testing "the shortest period still advances near the top of the beat range"
    (let [anchor (double (dec (* 2 two-to-the-40)))
          state (modulation/start (modulation/new-state) {:id :fine :seed 7 :every {:beats 1/1024} :params [:level]}
                                  {:beat anchor})
          result (modulation/advance state (model) {:beat (+ anchor 1/256) :time-ms 0.0})]
      (is (= [] (:diagnostics result)))
      (is (= 5 (:ticks (controller result))))
      (is (< anchor (:next-beat (controller result))))))
  (testing "the longest period samples once and waits for its next boundary"
    (let [anchor (double two-to-the-40)
          state (modulation/start (modulation/new-state) {:id :slow :seed 7 :every {:beats two-to-the-40} :params [:level]}
                                  {:beat anchor})
          first-frame (modulation/advance state (model) {:beat anchor :time-ms 0.0})
          later (modulation/advance (:state first-frame) (model) {:beat (+ anchor 1000.0) :time-ms 500000.0})]
      (is (= [] (:diagnostics later)))
      (is (= 1 (:ticks (controller later))))
      (is (= (* 2.0 two-to-the-40) (double (:next-beat (controller later)))))
      (is (= (level first-frame) (level later))))))

(deftest an-owner-whose-period-cannot-start-is-refused-alone
  (let [refused-config {:every {:bars (/ two-to-the-40 4)} :params [:level] :ranges {:level [0.7 0.7]}}
        show (-> (with-owner (model) refused-config)
                 (assoc-in [:instances "0:2" :timing-meter] {:beats-per-bar 8 :bars-per-phrase 4}))
        global (modulation/start (modulation/new-state)
                                 {:id :global :seed 3 :params [:level] :ranges {:level [0.4 0.4]} :priority -1} (frame 0))
        result (modulation/advance global show (frame 0))
        next-frame (modulation/advance (:state result) show (frame 1))
        [diagnostic] (:diagnostics result)]
    (is (= [:global] (mapv :id (get-in result [:state :controllers]))) "Only the refused owner has no controller")
    (is (= 0.4 (level result)) "The other controller still modulates")
    (is (= {:type :modulation/refused :controller-id [:cue "0:2"] :owner-instance-id "0:2"
            :effect-id :modulator :path [:every :bars]}
           (select-keys diagnostic [:type :controller-id :owner-instance-id :effect-id :path])))
    (is (re-find #"Modulation period :every \{:bars 274877906944\}" (:message diagnostic)))
    (is (= [:modulation/refused] (mapv :type (:diagnostics next-frame))) "Every frame reports the refusal")
    (is (= 2 (:ticks (controller next-frame))))
    (testing "a running owner reloaded to such a period stops its controller instead of the frame"
      (let [valid (-> (with-owner (model) (assoc refused-config :every {:beats 1/4}))
                      (assoc-in [:instances "0:2" :timing-meter] {:beats-per-bar 8 :bars-per-phrase 4}))
            running (modulation/advance nil valid (frame 0))
            revised (-> valid
                        (assoc-in [:instances "0:2" :definition :modulation] refused-config)
                        (assoc-in [:registry :effects :modulator :modulation] refused-config))
            ;; A changed config restarts on the frame's meter.
            reloaded (modulation/advance (:state running) revised
                                         (assoc (frame 1/2) :meter {:beats-per-bar 8 :bars-per-phrase 4}))]
        (is (= [[:cue "0:2"]] (mapv :id (get-in running [:state :controllers]))))
        (is (= [] (get-in reloaded [:state :controllers])))
        (is (= {} (:overrides reloaded)))
        (is (= [:modulation/refused] (mapv :type (:diagnostics reloaded))))))))

(deftest a-schedule-that-no-longer-advances-its-beat-coordinate-is-refused-alone
  ;; A double cannot add 1/1024 to beat 2^60, but can add 1024. Only the
  ;; first controller is refused; the frame and the other controller go on.
  (let [beat (Math/pow 2 60)
        state (-> (modulation/new-state)
                  (modulation/start {:id :fine :seed 7 :every {:beats 1/1024} :params [:level]} {:beat beat})
                  (modulation/start {:id :coarse :seed 9 :every {:beats 1024} :params [:level]
                                     :ranges {:level [0.3 0.3]}} {:beat beat}))
        result (modulation/advance state (model) {:beat beat :time-ms 0.0})]
    (is (= [:coarse] (mapv :id (get-in result [:state :controllers]))))
    (is (= 0.3 (level result)))
    (is (= [{:type :modulation/refused :controller-id :fine :every {:beats 1/1024}}]
           (mapv #(select-keys % [:type :controller-id :every]) (:diagnostics result))))
    (is (re-find #"no longer advances its schedule" (:message (first (:diagnostics result)))))
    (is (= [] (modulation/validate-state (:state result))))))

(deftest saved-controllers-outside-the-period-range-load-and-are-refused-alone
  (doseq [interval [1/4096 1.0e-9]]
    (testing (str interval)
      (let [state (-> (modulation/new-state)
                      (modulation/start {:id :random :seed 7 :every {:beats 1/4} :params [:level]} (frame 0))
                      (modulation/start {:id :other :seed 9 :every {:beats 1/4} :params [:level]
                                         :ranges {:level [0.3 0.3]} :priority -1} (frame 0)))
            saved (-> state
                      (assoc-in [:controllers 0 :config :every] {:beats interval})
                      (assoc-in [:controllers 0 :interval-beats] interval))
            frames (reductions (fn [result beat] (modulation/advance (:state result) (model) (frame beat)))
                               {:state saved} [0 1 5 9])]
        (is (= [] (modulation/validate-state saved)) "Backups keep their controllers")
        (is (= [[:modulation/refused] [] [] []] (mapv #(mapv :type (:diagnostics %)) (rest frames)))
            "Refused once, with no frame failure")
        (is (re-find #"^Saved modulation interval of .* beats is outside" (:message (first (:diagnostics (second frames))))))
        (is (every? #(= [:other] (mapv :id (get-in % [:state :controllers]))) (rest frames)))
        (is (every? #(= 0.3 (level %)) (rest frames)))))))

(defn- revise-owner [show config version]
  (-> show
      (assoc-in [:instances "0:2" :definition :modulation] config)
      (assoc-in [:instances "0:2" :definition :version] version)
      (assoc-in [:instances "0:2" :effect-version] version)
      (assoc-in [:registry :effects :modulator :modulation] config)
      (assoc-in [:registry :effects :modulator :version] version)))

(deftest a-valid-revision-after-a-refused-one-starts-at-its-frame-beat
  ;; v1 runs for 400 beats in eight-beat bars, v2 is refused in that meter,
  ;; v3 is valid again. v3 must start like a reload at its own frame beat, not
  ;; like a first launch that catches up over the cue's whole life.
  (let [meter {:beats-per-bar 8 :bars-per-phrase 4}
        at (fn [beat] (assoc (frame beat) :meter meter))
        valid {:every {:beats 1/4} :params [:level] :ranges {:level [0.6 0.6]}}
        refused (assoc valid :every {:bars (/ two-to-the-40 4)})
        base (-> (with-owner (model) valid) (assoc-in [:instances "0:2" :timing-meter] meter))
        v1 (reduce (fn [state beat] (:state (modulation/advance state base (at beat)))) nil (range 0 400))
        v2 (revise-owner base refused 2)
        r2 (modulation/advance v1 v2 (at 400))
        r2b (modulation/advance (:state r2) v2 (at 401))
        r3 (modulation/advance (:state r2b) (revise-owner base valid 3) (at 402))]
    (is (= [] (get-in r2 [:state :controllers])))
    (is (= [:modulation/refused] (mapv :type (:diagnostics r2)) (mapv :type (:diagnostics r2b))))
    (is (identical? (first (:diagnostics r2)) (first (:diagnostics r2b)))
        "An unchanged refused revision reports its recorded diagnostic without another attempt")
    (is (= {:owner-instance-id "0:2" :owner-version 2 :owner-config refused}
           (select-keys (get-in r2 [:state :refused [:cue "0:2"]]) [:owner-instance-id :owner-version :owner-config])))
    (is (= [] (modulation/validate-state (:state r2))))
    (is (= [] (:diagnostics r3)))
    (is (nil? (get-in r3 [:state :refused])) "The record goes once the owner has a controller again")
    (is (= [[[:cue "0:2"] 402 1]] (mapv (juxt :id :anchor-beat :ticks) (get-in r3 [:state :controllers]))))
    (is (= 0.6 (level r3)))
    (testing "the record follows a restore and goes with its owner"
      (let [restored (modulation/restore-state (:state r2) {:show-id :modulation-test :epoch 0
                                                            :instance-ids {"0:1" "0:1" "0:2" "0:7"}})]
        (is (= [[:cue "0:7"]] (keys (:refused restored))))
        (is (= "0:7" (get-in restored [:refused [:cue "0:7"] :diagnostic :owner-instance-id])))
        (is (nil? (:refused (:state (modulation/advance (:state r2) (update base :instances dissoc "0:2") (at 401))))))))))

(deftest a-backlog-beyond-the-catch-up-budget-skips-to-the-current-boundary
  (doseq [[label config beats]
          [["1/1024 beat with a budget of one at 120 BPM and 40 Hz"
            {:every {:beats 1/1024} :max-catch-up 1} (map #(* % 1/20) (range 0 40))]
           ["a 1/64-note shimmer with a budget of one at 180 BPM and 40 Hz"
            {:every {:beats 1/16} :max-catch-up 1} (map #(* % 3/40) (range 0 40))]
           ["1/1024 beat with the default budget across a two-beat frame gap"
            {:every {:beats 1/1024}} [0 1/20 1/10 21/10 43/20 11/5]]]]
    (testing label
      (let [show (with-owner (model) (assoc config :params [:level]))
            frames (rest (reductions (fn [result beat] (modulation/advance (:state result) show (frame beat)))
                                     {:state nil} beats))
            rebased (filter #(some #{:modulation/rebased} (map :type (:diagnostics %))) frames)]
        (is (seq rebased) "The budget was exceeded")
        (is (every? #(number? (level %)) frames) "Every frame is modulated")
        (doseq [[result beat] (map vector frames beats)
                :let [c (controller result)]]
          (is (<= (:next-beat c) (+ beat (:interval-beats c))) "Its schedule is at the current boundary")
          (is (< beat (:next-beat c))))
        (let [{:keys [skipped max-catch-up message]} (first (filter #(= :modulation/rebased (:type %))
                                                                    (mapcat :diagnostics rebased)))]
          (is (pos? skipped))
          (is (= (:max-catch-up config 1024) max-catch-up))
          (is (re-find #"^Skipped \d+ missed modulation steps" message))))))
  (testing "a skip keeps the controller's grid and is deterministic"
    (let [show (with-owner (model) {:every {:beats 1/4} :max-catch-up 2 :params [:level]})
          first-frame (modulation/advance nil show (frame 0))
          jumped (modulation/advance (:state first-frame) show (frame 10))
          again (modulation/advance (:state first-frame) show (frame 10))]
      (is (= jumped again))
      ;; 39 boundaries from 1/4 to 39/4 are skipped and beat 10 is sampled.
      (is (= [41 10.25] ((juxt :ticks :next-beat) (controller jumped))))
      (is (= 39 (:skipped (first (:diagnostics jumped))))))))
