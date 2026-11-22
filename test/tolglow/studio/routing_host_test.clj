(ns tolglow.studio.routing-host-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.control.model :as control]
            [tolglow.clock :as clock]
            [tolglow.show :as show]
            [tolglow.integrations.analysis :as analysis]
            [tolglow.connections :as connections]
            [tolglow.persistence :as persistence]
            [tolglow.routing :as routing]
            [afterglow.sync.session :as session]))

(def head {:id :head :position [0 0 0] :universe 1 :address 1 :channels {:dimmer 1 :red 2 :green 3 :blue 4}})
(def effect {:id :wash :version 1 :params {:level {:type :number :default 0.8 :bounds [0.0 1.0]}}
             :duration {:beats 4} :release-duration {:beats 2}
             :outputs {:dimmer {:op :param :key :level} :color {:op :const :value [1.0 0.0 0.0]}}})
(defn model []
  (control/new-show {:rig {:fixtures [head]}
                     :registry {:effects {:wash effect} :cues {:wash {:id :wash :version 1 :effect-id :wash}}}}))
(def source-id "midi/midi-clock/ch/1/note/60")
(defn spec
  ([] (spec source-id :midi [0 127]))
  ([source kind source-range]
   {:version 1 :id :patch :revision 1
    :sources {source {:kind kind :range source-range :timeout {:beats 16}}}
    :destinations {:level {:cue-id :wash :param :level :range [0.0 1.0] :base 0.0 :combine :max}}
    :routes [{:id :input :source source :destination :level}]}))
(defn apply-at! [runtime type payload time]
  (let [receipt (show/submit! runtime type payload)]
    (show/tick! runtime time)
    (deref receipt 1000 ::missing)))
(defn prepare! [runtime specification]
  (is (= :accepted (:status (apply-at! runtime :patch-save {:source (pr-str specification)} 0))))
  (is (false? (get-in (show/snapshot runtime) [:patch :enabled?])))
  (is (= :accepted (:status (apply-at! runtime :patch-enabled {:enabled? true} 0))))
  (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :wash} 0)))
(defn observation [id value time]
  {:type :sync/observation
   :observation {:source-id source-id :event-id id :value value :gate? (pos? value)
                 :confidence 1.0 :observed-at-ms time :kind :midi :origin :midi-input}})
(defn capture-fixture []
  (let [opened (promise)
        runtime (show/create-runtime
                  {:model (model)
                   :sync-options {:midi-factory (fn [_ _]
                                                  {:open! (fn [receive!] (deliver opened receive!) :captured)
                                                   :close! (fn [_])})}})]
    (show/sync-action! runtime :configure {:midi {:enabled? true :input-index 0}})
    (show/sync-action! runtime :start {})
    {:runtime runtime :receive! (deref opened 2000 nil)
     :manager (connections/sync (:connections runtime))}))

(deftest real-midi-observations-use-owner-clock-and-common-command-reducer
  (let [{:keys [runtime receive! manager]} (capture-fixture)]
    (try
      (let [id (prepare! runtime (spec))]
        (receive! {:type :midi/message :source-id :midi-clock :time-ms 20.0 :bytes [0x90 60 127]})
        (is (session/flush! manager))
        (show/tick! runtime 100.0)
        (let [model (:model @(:state runtime))
              observed (get-in model [:routing :state :sources source-id])
              commands (map #(get-in model [:control :history % :command]) (get-in model [:control :history-order]))]
          (is (= 1.0 (get-in model [:instances id :params :level])))
          (is (= 1.0 (first (get-in (show/snapshot runtime) [:output :values :head :emitted-color]))))
          (is (= 20.0 (:observed-at-ms observed)))
          (is (= 100.0 (get-in observed [:clock :time-ms])))
          (is (= 0.2 (get-in observed [:clock :beat])))
          (is (= (:epoch model) (:epoch observed)))
          (is (some #(= :patch (get-in % [:source :type])) commands))
          (is (= [0 127] (get-in (show/snapshot runtime) [:patch :sources 0 :range]))))
        (receive! {:type :midi/message :source-id :midi-clock :time-ms 120.0 :bytes [0x80 60 0]})
        (is (session/flush! manager))
        (show/tick! runtime 200.0)
        (is (= 0.0 (get-in @(:state runtime) [:model :instances id :params :level])))
        (is (empty? (get-in @(:state runtime) [:model :routing :pending]))))
      (finally (show/stop! runtime)))))

(deftest actual-audio-features-create-ports-without-human-override-holds
  (let [runtime (show/create-runtime {:model (model)})]
    (try
      (let [id (prepare! runtime (spec "audio/rms" :audio [0.0 1.0]))
            callbacks (show/audio-callbacks runtime)]
        (apply-at! runtime :modulation-start {:id :pulse :seed 4 :every {:beats 1}
                                             :select {:instance-ids [id]} :params [:level]
                                             :ranges {:level [0.1 0.2]}} 0)
        ((:features! callbacks) {:time-ms 40.0 :rms 0.65 :peak 0.9 :kick-rms 0.2 :audio/energy 0.3
                                :confidence 0.8 :capture {:run-id "capture-1"}})
        (show/tick! runtime 100.0)
        (let [model (:model @(:state runtime))]
          (is (= 0.65 (get-in model [:instances id :params :level])))
          (is (= 4 (count (get-in (show/snapshot runtime) [:patch :sources]))))
          (is (= 0.3 (:audio/energy (show/variables runtime))
                 (get-in model [:routing :ports "audio/energy" :value])))
          (is (not (contains? (get-in model [:variables :audio :features]) :audio/energy)))
          (is (empty? (get-in model [:modulation :holds])))
          (is (<= 0.1 (get-in (show/snapshot runtime) [:modulation :overrides id :level]) 0.2))
          (is (= 40.0 (get-in model [:routing :state :sources "audio/rms" :observed-at-ms]))))
        ((:features! callbacks) {:time-ms 40.0 :rms 0.1})
        (show/tick! runtime 150.0)
        (is (not (contains? (show/variables runtime) :audio/energy)))
        (is (= 0.65 (get-in @(:state runtime) [:model :instances id :params :level]))))
      (finally (show/stop! runtime)))))

(deftest transient-gate-release-and-show-restore-retain-captured-ownership
  (let [runtime (show/create-runtime {:model (model)})]
    (try
      (let [specification (assoc-in (spec) [:routes 0 :envelope] {:attack {:beats 0} :release {:beats 2}})
            id (prepare! runtime specification)
            callbacks (show/sync-callbacks runtime)]
        ((:events! callbacks) (observation "on" 127 10))
        ((:events! callbacks) (observation "off" 0 20))
        (show/tick! runtime 100.0)
        (is (= 1.0 (get-in @(:state runtime) [:model :instances id :params :level])))
        (show/tick! runtime 350.0)
        (is (= 0.75 (get-in @(:state runtime) [:model :instances id :params :level])))
        (let [saved (show/export-show runtime)
              before (get-in saved [:model :routing :state :routes :input])]
          (is (= :accepted (:status (apply-at! runtime :import {:source (pr-str saved)} 350.0))))
          (let [new-id (:id (first (:instances (show/snapshot runtime))))
                restored (get-in @(:state runtime) [:model :routing :state :routes :input])]
            (is (not= id new-id))
            (is (= new-id (:instance-id restored)))
            (is (= (dissoc before :instance-id) (dissoc restored :instance-id)))
            (is (nil? ((:events! callbacks) (observation "stale" 127 350))))
            (show/tick! runtime 600.0)
            (is (= 0.5 (get-in @(:state runtime) [:model :instances new-id :params :level])))
            (is (= [{:destination-id :level :instance-id new-id :value 0.5}]
                   (get-in (show/snapshot runtime) [:patch :values]))))))
      (finally (show/stop! runtime)))))

(deftest invalid-graphs-are-atomic-and-reenable-requires-fresh-input
  (let [runtime (show/create-runtime {:model (model)})]
    (try
      (let [id (prepare! runtime (spec)) callbacks (show/sync-callbacks runtime)]
        ((:events! callbacks) (observation "on" 127 1))
        (show/tick! runtime 50)
        (let [saved (get-in @(:state runtime) [:model :routing :spec])]
          (doseq [invalid [(assoc-in (spec) [:destinations :level :range] [-1 1])
                           (assoc-in (spec) [:destinations :level :param] :missing)
                           (assoc-in (spec) [:routes 0 :invert?] true)]]
            (is (= :rejected (:status (apply-at! runtime :patch-save {:source (pr-str invalid)} 50))))
            (is (= saved (get-in @(:state runtime) [:model :routing :spec])))))
        (is (= :accepted (:status (apply-at! runtime :patch-enabled {:enabled? false} 50))))
        ((:events! callbacks) (observation "disabled" 10 60))
        (show/tick! runtime 100)
        (apply-at! runtime :patch-enabled {:enabled? true} 100)
        (is (= 1.0 (get-in @(:state runtime) [:model :instances id :params :level])))
        (is (empty? (get-in @(:state runtime) [:model :routing :state :sources])))
        ((:events! callbacks) (observation "fresh" 64 110))
        (show/tick! runtime 150)
        (is (= (/ 64.0 127) (get-in @(:state runtime) [:model :instances id :params :level]))))
      (finally (show/stop! runtime)))))

(deftest rejected-command-retries-and-historical-queued-input-survives-loop
  (let [prepared (-> (model) (routing/save (spec) 0) (routing/set-enabled true 0))
        launched (:model (control/apply-command prepared {:op :cue/launch :cue-id :wash :command-id "launch"
                                                          :show-id (:show-id prepared) :epoch 0 :time-ms 0 :source :test}))
        captured (routing/admit-sync (assoc-in launched [:transport :beat] 7.8) [(observation "pending" 127 10)])
        rebased (clock/rebase (assoc-in captured [:transport :beat] 8.0) {:beat 0.0 :time-ms 1000.0})
        restored (persistence/thaw (model) (persistence/freeze rebased))
        rejected (routing/advance restored 1000.0 (fn [m _ _] {:model m :ack {:status :rejected :code :busy}}))
        retried (routing/advance rejected 1000.0)
        id (first (keys (:instances retried)))]
    (is (< (get-in restored [:routing :pending 0 :clock :beat]) 0))
    (is (= [source-id "pending"] (first (get-in rejected [:routing :state :receipts]))))
    (is (empty? (get-in rejected [:routing :state :last-values])))
    (is (some #(= :command-rejected (:code %)) (get-in rejected [:routing :diagnostics])))
    (is (= 1.0 (get-in retried [:instances id :params :level])))
    (is (= 1.0 (get-in retried [:routing :state :last-values [:level id]])))
    (is (= 10 (get-in retried [:routing :state :sources source-id :observed-at-ms])))))

(deftest learned-ports-are-bounded-without-configured-output
  (let [events (mapv (fn [i] (assoc-in (observation (str i) i i) [:observation :source-id] (str "source/" i)))
                     (range (+ routing/max-ports 10)))
        captured (routing/admit-sync (model) events)]
    (is (= routing/max-ports (count (get-in captured [:routing :ports]))))
    (is (not (contains? (get-in captured [:routing :ports]) "source/0")))
    (is (contains? (get-in captured [:routing :ports]) "source/137"))
    (is (empty? (get-in captured [:routing :pending])))
    (is (empty? (get-in (routing/advance captured 1000) [:control :history])))
    (is (= (:routing captured) (routing/validate-state (:routing captured))))))

(deftest routing-admission-is-bounded-and-saved-queues-rebase-and-remap
  (let [prepared (-> (model) (routing/save (spec) 0) (routing/set-enabled true 0)
                  (assoc :transport {:beat 8.0 :time-ms 1000.0 :bpm 120 :playing? true}))
        observations (mapv #(observation (str %) 127 1) (range (inc routing/max-pending)))
        captured (routing/admit-sync prepared observations)
        rebased (clock/rebase captured {:beat 0.0 :time-ms 1000.0})
        saved (persistence/freeze rebased)
        restored (persistence/thaw (model) saved)]
    (is (= routing/max-pending (count (get-in captured [:routing :pending]))))
    (is (= :input-overflow (get-in captured [:routing :diagnostics 0 :code])))
    (is (= 0.0 (get-in rebased [:routing :pending 0 :clock :beat])))
    (is (= 1 (get-in rebased [:routing :pending 0 :observed-at-ms])))
    (is (= 0.0 (get-in rebased [:routing :ports source-id :clock :beat])))
    (is (= (:epoch restored) (get-in restored [:routing :pending 0 :epoch])))
    (is (= (get-in captured [:routing :pending 0 :event-id]) (get-in restored [:routing :pending 0 :event-id])))
    (doseq [bad [(assoc-in (:routing captured) [:pending 0 :epoch] 99)
                 (assoc-in (:routing captured) [:ports source-id :value] Double/NaN)
                 (assoc-in (:routing captured) [:diagnostics] [(fn [])])]]
      (is (thrown? clojure.lang.ExceptionInfo (routing/validate-state bad))))))

(deftest source-switch-and-repeated-midi-seeks-preserve-local-progress-once
  (let [{:keys [runtime receive! manager]} (capture-fixture)]
    (try
      (let [specification (assoc-in (spec) [:routes 0 :envelope] {:attack {:beats 2} :release {:beats 2}})
            _ (prepare! runtime specification)
            id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :wash
                                                          :launch {:mode :one-shot}} 0))]
        ((:events! (show/sync-callbacks runtime)) (observation "on" 127 10))
        (show/tick! runtime 100.0)
        (show/tick! runtime 600.0)
        (let [before (:model @(:state runtime))
              started (get-in before [:instances id :started-at-beat])
              value (get-in before [:instances id :params :level])]
          (is (= 0.5 value))
          (receive! {:type :midi/message :source-id :midi-clock :time-ms 600.0 :bytes [0xf2 0 0]})
          (is (session/flush! manager))
          (show/sync-action! runtime :source {:source-id :midi-clock})
          (show/tick! runtime 600.0)
          (let [shifted (:model @(:state runtime))]
            (is (= 0.0 (get-in shifted [:transport :beat])))
            (is (= (- started 1.2) (get-in shifted [:instances id :started-at-beat])))
            (is (= value (get-in shifted [:instances id :params :level])))
            (is (nil? (get-in @(:state runtime) [:status :error]))))
          (receive! {:type :midi/message :source-id :midi-clock :time-ms 650.0 :bytes [0xf2 16 0]})
          (is (session/flush! manager))
          (show/tick! runtime 650.0)
          (let [forward (:model @(:state runtime))]
            (is (= 4.0 (get-in forward [:transport :beat])))
            (is (= value (get-in forward [:instances id :params :level])))
            (is (= 2.8 (get-in forward [:instances id :duration-timer :started-at])))
            (show/tick! runtime 700.0)
            (is (= (get-in forward [:instances id :duration-timer])
                   (get-in @(:state runtime) [:model :instances id :duration-timer])))
            (is (= :running (get-in @(:state runtime) [:model :instances id :phase])))
            (is (= 10 (get-in @(:state runtime) [:model :routing :state :sources source-id :observed-at-ms]))))))
      (finally (show/stop! runtime)))))

(deftest measured-bass-pitch-drives-an-active-effect-and-releases-on-silence
  (let [runtime (show/create-runtime {:model (model)})
        source "audio/bass-pitch-hz"]
    (try
      (let [id (prepare! runtime (spec source :audio [20 180]))
            samples (double-array (map #(* 0.5 (Math/sin (/ (* 2.0 Math/PI 65.0 %) 8000))) (range 1600)))
            analyze (fn [state pcm t]
                      (analysis/analyze state {:samples pcm :sample-rate 8000
                                               :started-at-ms t :received-at-ms (+ 200 t)}))
            tone (analyze (analysis/new-analyzer) samples 0)
            publish (:features! (show/audio-callbacks runtime))]
        (publish (:features tone))
        (show/tick! runtime 200.0)
        (let [model (:model @(:state runtime)) port (get-in model [:routing :ports source])]
          (is (= [20.0 180.0] (:range port)))
          (is (= :hz (get-in port [:port :unit])))
          (is (true? (:gate? port)))
          (is (> (:confidence port) 0.9))
          (is (< (Math/abs (- (/ 45.0 160.0) (get-in model [:instances id :params :level]))) 0.02)))
        (publish (:features (analyze (:state tone) (double-array 1600) 200)))
        (show/tick! runtime 400.0)
        (is (false? (get-in @(:state runtime) [:model :routing :ports source :gate?]))))
      (finally (show/stop! runtime)))))

(deftest saved-audio-energy-bindings-follow-the-canonical-signal-after-load
  (let [m (-> (model) (assoc-in [:rig :fixtures 0 :groups] [:wash])
              (assoc-in [:variables :audio :features] {:energy-envelope 0.3 :rms 0.31}))
        rt (show/create-runtime {:model m})
        old-field [:data :energy-envelope]
        action {:op :lighting/scene :scene {:op :vary :group-id :wash :target 0.4}
                :gate {:path old-field :above 0.7 :below 0.2}}]
    (try
      (prepare! rt (spec "audio/energy-envelope" :audio [0.0 1.0]))
      (is (= :accepted (:status (apply-at! rt :lighting-scene
                               {:op :apply :group-id :wash :target-var [:audio :features :energy-envelope]} 1))))
      (doseq [topic [:audio/energy :my/energy]]
        (is (= :accepted (:status (apply-at! rt :event-subscriber
                                  {:op :save :subscriber {:id topic :match {:type topic} :actions [action]}} 2)))))
      (let [saved (persistence/read-string (show/export-show-source rt))
            reloaded (persistence/thaw (model) saved)
            subscribers (into {} (map (juxt :id identity)) (get-in reloaded [:event-subscribers :subscribers]))
            routing (get-in reloaded [:routing :spec])]
        (is (= 0.3 (get-in reloaded [:variables :audio/energy])))
        (is (= {:rms 0.31} (get-in reloaded [:variables :audio :features])))
        (is (= [:audio/energy] (get-in reloaded [:extensions :tolglow.lighting-scenes/control :groups :wash :target-var])))
        (is (= [:data :audio/energy] (get-in subscribers [:audio/energy :actions 0 :gate :path])))
        (is (= old-field (get-in subscribers [:my/energy :actions 0 :gate :path])))
        (is (= ["audio/energy"] (keys (:sources routing))))
        (is (= "audio/energy" (get-in routing [:routes 0 :source]))))
      (finally (show/stop! rt)))))
