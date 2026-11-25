(ns tolglow.effects.modulation-reload-test
  "Modulation around a hot reload that the engine accepts or rejects.
  A rejected revision must leave captured DMX and controller state exactly as
  they are in a twin show where that revision was never published."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

(def level {:type :number :default 0.5 :bounds [0.0 1.0]})

(defn- param [key] {:op :param :key key})

(def wash
  {:id :wash :version 1 :name "Wash" :targets [:head-2]
   :params {:level level}
   :outputs {:dimmer (param :level)}})

(def owner
  {:id :modulator :version 1 :name "Modulator" :targets [:head-1]
   :params {:level level}
   :outputs {:dimmer (param :level)}
   :modulation {:seed 7 :every {:beats 1/4} :params [:level] :select {:cue-ids [:wash]} :mode :glide}})

(def changed-modulation
  {:seed 23 :every {:beats 1/8} :params [:level] :select {:cue-ids [:wash]} :mode :glide
   :ranges {:level [0.6 1.0]}})

(defn- model [policies effects]
  (reduce (fn [model {:keys [id] :as effect}]
            (-> model
                (assoc-in [:registry :effects id] effect)
                (assoc-in [:registry :cues id] {:id id :version 1 :effect-id id :priority 20
                                               :reload-policy (get policies id :preserve-state)})))
          (demo/model) effects))

(defn- runtime
  ([output show-model] (runtime output show-model {}))
  ([output show-model {:keys [workers failing-compile] :or {workers 2}}]
   (cond-> (show/create-runtime {:model show-model :workspace-options nil :effect-log! (fn [_]) :workers workers
                                 :sink {:send! (fn [universes _]
                                                (swap! output conj (mapv #(bit-and 255 %) (get universes 1))))}})
     ;; The compiler stage is driven at its owned boundary, as the renderer
     ;; rejection test in tolglow.effects.faults-test does.
     failing-compile
     (update :compile (fn [compile-effect]
                        (fn [{:keys [id version] :as definition}]
                          (if (= failing-compile [id version])
                            (throw (ex-info "Compiler rejected this revision" {:effect-id id}))
                            (compile-effect definition))))))))

(defn- apply! [runtime payload t]
  (let [reply (if (contains? payload :op) (show/submit! runtime payload)
                  (show/submit! runtime :definition payload))]
    (show/tick! runtime t)
    (deref reply 2000 ::timeout)))

(defn- instance-id [runtime cue-id]
  (some #(when (= cue-id (:cue-id %)) (:id %)) (vals (get-in @(:state runtime) [:model :instances]))))

(defn- instance [runtime cue-id]
  (get-in @(:state runtime) [:model :instances (instance-id runtime cue-id)]))

(defn- controllers [runtime]
  (get-in @(:state runtime) [:model :modulation :controllers]))

(defn- publish!
  "Publish every definition in one owner cycle and render that frame."
  [runtime definitions t]
  (let [replies (mapv #(show/submit! runtime :definition {:kind :effects :definition %}) definitions)]
    (show/tick! runtime t)
    (mapv #(:status (deref % 2000 ::timeout)) replies)))

(defn- capture!
  "Launch the cues, publish the revisions scheduled in `publications`
  ({time-ms [definition ...]}), and return the captured universe and the
  controller state of every later frame."
  [runtime output cues publications times]
  (doseq [cue cues] (apply! runtime {:op :cue/launch :cue-id cue} 0))
  (reset! output [])
  (mapv (fn [t]
          (let [acks (if-let [definitions (get publications t)]
                       (publish! runtime definitions t)
                       (do (show/tick! runtime t) nil))]
            {:time-ms t :acks acks :dmx (peek @output) :controllers (controllers runtime)
             :error (get-in @(:state runtime) [:status :error])}))
        times))

(def times (vec (range 50 651 50)))

(def head-2-dimmer
  "Index of head-2's dimmer in the captured universe."
  12)

(defn- differing
  "Frame times at which `field` differs between a control run and another run."
  [field control actual]
  (vec (keep (fn [[expected frame]] (when (not= (field expected) (field frame)) (:time-ms frame)))
             (map vector control actual))))

(defn- frames-match [control broken]
  (is (= (count control) (count broken)))
  (is (= [] (differing :dmx control broken)) "Captured DMX differs at these frame times")
  (is (= [] (differing :controllers control broken)) "Controller seed, schedule or cells differ at these frame times")
  (is (every? nil? (map :error broken))))

(defn- twin-run
  "Run a show with `publications` beside a twin that never publishes them, or
  publishes only :control-publications."
  [show-model cues publications options]
  (let [broken-output (atom []) control-output (atom [])
        broken (runtime broken-output show-model options)
        control (runtime control-output show-model (dissoc options :failing-compile))]
    (try
      (let [control-frames (capture! control control-output cues (:control-publications options {}) times)
            broken-frames (capture! broken broken-output cues publications times)]
        {:control control-frames :broken broken-frames
         :owner (instance broken :modulator) :wash (instance broken :wash)
         :events (get-in @(:state broken) [:model :variables :runtime :events])
         :snapshot-error (try (show/snapshot broken) nil (catch Exception error (ex-message error)))})
      (finally (show/stop! broken) (show/stop! control)))))

(def migration {:from {1 []}})

(defn- rejected-revision
  "A version 2 of `effect` that the engine rejects at `stage`."
  [effect stage]
  (let [next (assoc effect :version 2)]
    (case stage
      :reload (assoc-in next [:params :level] {:type :number :default 2.5 :bounds [2.0 3.0]})
      :compile next
      :migrate (assoc next :state-migration {:from {1 [{:op :scale :path [:missing] :by 2}]}})
      :evaluate (-> next
                    (assoc-in [:params :divisor] {:type :number :default 0.0 :bounds [0.0 1.0]})
                    (assoc-in [:outputs :dimmer] {:op :divide :args [(param :level) (param :divisor)]}))
      :output (assoc-in next [:outputs :aim]
                        {:op :const :value (:position (first (filter #(= (first (:targets effect)) (:id %))
                                                                     (demo/fixtures))))}))))

(defn- with-migration [definition policy stage]
  (cond-> definition
    (and (= :migrate policy) (not= :migrate stage)) (assoc :state-migration migration)))

(def owner-cases
  (for [policy [:preserve-state :restart :migrate]
        stage [:reload :compile :migrate :evaluate :output]
        :when (or (not= :migrate stage) (= :migrate policy))]
    [policy stage]))

(deftest control-show-modulation-is-live
  (let [output (atom []) rt (runtime output (model {} [wash owner]))]
    (try
      (let [frames (capture! rt output [:wash :modulator] {} times)
            levels (mapv #(nth (:dmx %) head-2-dimmer) frames)]
        (is (< 4 (count (distinct levels))) "The owned glide moves the wash between frames")
        (is (= [[:cue (instance-id rt :modulator)]] (mapv :id (:controllers (peek frames)))))
        (is (< 1 (:ticks (first (:controllers (peek frames)))))))
      (finally (show/stop! rt)))))

(deftest rejected-owner-reload-leaves-frames-and-controller-as-if-never-published
  (doseq [[policy stage] owner-cases]
    (testing (str policy " rejected at " stage)
      (let [show-model (model {:modulator policy} [wash owner])
            publish (-> (rejected-revision owner stage)
                        (assoc :modulation changed-modulation)
                        (with-migration policy stage))
            {:keys [control broken owner events snapshot-error]}
            (twin-run show-model [:wash :modulator] {300 [publish]}
                      (when (= :compile stage) {:failing-compile [:modulator 2]}))]
        (is (= [:accepted] (some :acks broken)) "Publication accepts the revision; only the running engine rejects it")
        (is (= [1 stage 2] [(:effect-version owner) (get-in owner [:reload-fault :stage])
                            (get-in owner [:reload-fault :effect-version])]))
        (is (nil? (:fault owner)))
        (is (= [:effect/reload-failed] (mapv :type events)) "One bounded event")
        (is (nil? snapshot-error) "The Studio and REPL projection still reads the show")
        (frames-match control broken)))))

(deftest rejected-owner-reload-with-unchanged-modulation-keeps-the-controller
  (doseq [policy [:preserve-state :restart :migrate]]
    (testing (str policy)
      (let [show-model (model {:modulator policy} [wash owner])
            publish (with-migration (rejected-revision owner :evaluate) policy :evaluate)
            {:keys [control broken owner]} (twin-run show-model [:wash :modulator] {300 [publish]} {})]
        (is (= [1 :evaluate] [(:effect-version owner) (get-in owner [:reload-fault :stage])]))
        (frames-match control broken)))))

(deftest a-rejected-reload-defers-the-other-reloads-of-that-frame-by-one-frame
  (let [show-model (model {:modulator :preserve-state :wash :preserve-state} [wash owner])
        rejected (assoc (rejected-revision owner :evaluate) :modulation changed-modulation)
        brighter (assoc wash :version 2 :outputs {:dimmer {:op :min :args [{:op :const :value 1.0}
                                                                        {:op :add :args [(param :level) {:op :const :value 0.1}]}]}})
        ;; The twin never sees the rejected revision and takes the healthy one a frame later.
        {:keys [control broken owner wash]}
        (twin-run show-model [:wash :modulator] {300 [rejected brighter]} {:control-publications {350 [brighter]}})]
    (is (= [:accepted :accepted] (some :acks broken)))
    (is (= [1 :evaluate] [(:effect-version owner) (get-in owner [:reload-fault :stage])]))
    (is (= [2 nil nil] [(:effect-version wash) (:reload-fault wash) (:fault wash)])
        "The healthy revision is deferred, never faulted")
    (frames-match control broken)))

(deftest serial-and-two-worker-evaluation-agree-around-a-rejected-reload
  (let [show-model (model {:modulator :preserve-state} [wash owner])
        publish (assoc (rejected-revision owner :evaluate) :modulation changed-modulation)
        serial (twin-run show-model [:wash :modulator] {300 [publish]} {:workers 1})
        parallel (twin-run show-model [:wash :modulator] {300 [publish]} {:workers 2})]
    (is (= (:broken serial) (:broken parallel)))
    (frames-match (:control serial) (:broken serial))
    (frames-match (:control parallel) (:broken parallel))))

(deftest rejected-target-reload-keeps-the-last-good-revision-modulated
  (doseq [[label revise]
          [["widened bounds" #(assoc-in % [:params :level :bounds] [0.0 10.0])]
           ["removed parameter" #(-> % (update :params dissoc :level)
                                     (assoc-in [:params :gain] level))]]]
    (testing label
      (let [show-model (model {:wash :preserve-state :modulator :pinned}
                              [wash (assoc-in owner [:modulation :mode] :sample-hold)])
            ;; 250 ms is a quarter-beat sample boundary at 120 BPM.
            publish (-> (revise (assoc wash :version 2))
                        (assoc-in [:params :divisor] {:type :number :default 0.0 :bounds [0.0 1.0]})
                        (assoc :outputs {:dimmer {:op :divide :args [{:op :const :value 0.5} (param :divisor)]}}))
            broken-output (atom []) control-output (atom [])
            broken (runtime broken-output show-model) control (runtime control-output show-model)]
        (try
          (let [control-frames (capture! control control-output [:wash :modulator] {} times)
                broken-frames (capture! broken broken-output [:wash :modulator] {250 [publish]} times)
                target (instance broken :wash)]
            (is (= [1 :evaluate] [(:effect-version target) (get-in target [:reload-fault :stage])]))
            (is (nil? (:fault target)) "The last good revision is not quarantined by the candidate's overrides")
            (frames-match control-frames broken-frames))
          (finally (show/stop! broken) (show/stop! control)))))))

(deftest accepted-owner-reload-switches-the-controller-once-at-the-takeover-frame
  (doseq [policy [:preserve-state :restart :migrate]]
    (testing (str policy)
      (let [steady (assoc owner :modulation {:seed 7 :every {:beats 1/4} :params [:level] :select {:cue-ids [:wash]}
                                             :ranges {:level [0.2 0.2]}})
            publish (cond-> (assoc steady :version 2
                                   :modulation (assoc (:modulation steady) :seed 23 :ranges {:level [0.8 0.8]}))
                      (= :migrate policy) (assoc :state-migration migration))
            output (atom []) rt (runtime output (model {:modulator policy} [wash steady]))]
        (try
          (let [frames (capture! rt output [:wash :modulator] {300 [publish]} times)
                at (fn [t] (some #(when (= t (:time-ms %)) %) frames))
                dimmer (fn [t] (nth (:dmx (at t)) head-2-dimmer))
                controller (fn [t] (first (:controllers (at t))))
                reloaded (instance rt :modulator)]
            (is (= [2 nil nil] [(:effect-version reloaded) (:reload-fault reloaded) (:fault reloaded)]))
            (is (= (dimmer 100) (dimmer 250)))
            (is (< (dimmer 250) (dimmer 300)) "The takeover frame already renders the new modulation")
            (is (= (dimmer 300) (dimmer 650)))
            (is (= [1 0] [(:owner-version (controller 250)) (:order (controller 250))]))
            (is (= [2 1 0.6] [(:owner-version (controller 300)) (:order (controller 300))
                              (double (:anchor-beat (controller 300)))])
                "Restarted once, anchored at the takeover beat")
            (is (= #{[2 1 0.6]} (set (map (fn [frame]
                                            (let [c (first (:controllers frame))]
                                              [(:owner-version c) (:order c) (double (:anchor-beat c))]))
                                          (filter #(<= 300 (:time-ms %)) frames))))
                "No second restart on later frames"))
          (finally (show/stop! rt)))))))

(deftest accepted-target-reload-is-not-faulted-by-the-previous-revisions-overrides
  (doseq [[label revise expected-params]
          [["removed parameter" #(-> % (update :params dissoc :level) (assoc-in [:params :gain] level)
                                     (assoc :outputs {:dimmer (param :gain)})) #{:gain}]
           ["narrowed bounds" #(assoc-in % [:params :level :bounds] [0.4 0.6]) #{:level}]
           ["retyped parameter" #(assoc-in % [:params :level] {:type :integer :default 1 :bounds [0 1]}) #{:level}]]]
    (testing label
      (let [modulator (assoc owner :modulation {:seed 7 :every {:beats 1/4} :params :all :select {:cue-ids [:wash]}
                                                :ranges {:level [0.45 0.55]}})
            ;; The canonical level 1 is valid before and after the integer retype;
            ;; the modulated 0.45..0.55 of version 1 is not.
            show-model (cond-> (model {:wash :preserve-state :modulator :pinned} [wash modulator])
                         (= "retyped parameter" label) (assoc-in [:registry :cues :wash :params] {:level 1}))
            output (atom []) rt (runtime output show-model)]
        (try
          (let [frames (capture! rt output [:wash :modulator] {300 [(revise (assoc wash :version 2))]} times)
                target (instance rt :wash)]
            (is (= [2 nil nil] [(:effect-version target) (:reload-fault target) (:fault target)]))
            (is (= expected-params (set (keys (:params target)))))
            (is (every? nil? (map :error frames))))
          (finally (show/stop! rt)))))))

(defn- failing-wash
  "A wash revision that passes publication and fails every evaluation."
  [revise]
  (-> (revise (assoc wash :version 2))
      (assoc-in [:params :divisor] {:type :number :default 0.0 :bounds [0.0 1.0]})
      (assoc :outputs {:dimmer {:op :divide :args [{:op :const :value 0.5} (param :divisor)]}})))

(defn- waveform-run
  "Launch two washes, bind `binding` to `param` on the first, and capture the
  frames. The second wash has equal priority and launched later, so it is on
  top unless the first one's modulated layer priority is above it."
  [runtime output param binding publish times]
  (let [launch (fn [level] (:instance-id (apply! runtime {:op :cue/launch :cue-id :wash :launch {:params {:level level}}} 0)))
        low (launch 0.2)
        _ (when-not (= :layer/priority param) (apply! runtime {:op :instance/end :instance-id (launch 0.8)} 0))
        _ (when (= :layer/priority param) (launch 0.8))
        bound (let [reply (show/submit! runtime :modulation-bind {:instance-id low :param param :binding binding})]
                (show/tick! runtime 0)
                (:status (deref reply 2000 ::timeout)))]
    (reset! output [])
    {:bound bound :low low
     :frames (mapv (fn [t]
                     (let [ack (if (and publish (= t (:at publish)))
                                 (apply! runtime {:kind :effects :definition (:definition publish)} t)
                                 (do (show/tick! runtime t) nil))]
                       {:time-ms t :ack (:status ack) :dmx (peek @output)
                        :priorities (get-in @(:state runtime) [:parameter-samples :priorities])
                        :error (get-in @(:state runtime) [:status :error])}))
                   times)}))

(deftest rejected-reload-leaves-a-bound-waveform-as-if-never-published
  (doseq [[label param binding revise publish-at times]
          [["a parameter the candidate removes" :level
            {:type :lfo :waveform :saw :period {:beats 1} :phase-reference :transport :from 0.1 :to 0.9}
            #(-> % (update :params dissoc :level) (assoc-in [:params :gain] level)) 300 times]
           ["a parameter the candidate narrows" :level
            {:type :lfo :waveform :saw :period {:beats 1} :phase-reference :transport :from 0.1 :to 0.9}
            #(assoc-in % [:params :level] {:type :number :default 0.2 :bounds [0.15 0.25]}) 300 times]
           ;; A saw from tier 20 to 21 over four beats holds 20 until 1500 ms.
           ;; A source that forgets its held tier between 1000 and 1500 ms rounds up to 21.
           ["layer priority hysteresis" :layer/priority
            {:type :lfo :waveform :saw :period {:beats 4} :phase-reference :transport :from 20 :to 21}
            identity 1100 (vec (range 950 1651 50))]]]
    (testing label
      (let [show-model (model {:wash :preserve-state} [wash])
            broken-output (atom []) control-output (atom [])
            broken (runtime broken-output show-model) control (runtime control-output show-model)]
        (try
          (let [expected (waveform-run control control-output param binding nil times)
                actual (waveform-run broken broken-output param binding
                                     {:at publish-at :definition (failing-wash revise)} times)
                target (get-in @(:state broken) [:model :instances (:low actual)])]
            (is (= [:accepted :accepted] [(:bound expected) (:bound actual)]))
            (is (= :accepted (some :ack (:frames actual))))
            (is (< 1 (count (distinct (map :dmx (:frames expected))))) "The bound waveform is live in the twin")
            (is (= [1 :evaluate nil] [(:effect-version target) (get-in target [:reload-fault :stage]) (:fault target)]))
            (is (= [] (differing :dmx (:frames expected) (:frames actual))) "Captured DMX differs at these frame times")
            (is (= [] (differing :priorities (:frames expected) (:frames actual)))
                "Held layer priority tiers differ at these frame times")
            (is (every? nil? (map :error (:frames actual)))))
          (finally (show/stop! broken) (show/stop! control)))))))
