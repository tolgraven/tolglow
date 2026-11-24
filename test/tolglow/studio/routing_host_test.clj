(ns tolglow.studio.routing-host-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.control.model :as control]
            [tolglow.audio.capture-test :as pcm]
            [tolglow.clock :as clock]
            [tolglow.show :as show]
            [tolglow.integrations.analysis :as analysis]
            [tolglow.connections :as connections]
            [tolglow.persistence :as persistence]
            [tolglow.routing :as routing]
            [afterglow.sync.session :as session])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

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
(defn capture-fixture
  ([] (capture-fixture {}))
  ([options]
   (let [opened (promise)
        runtime (show/create-runtime
                  (merge {:model (model)
                          :sync-options {:midi-factory (fn [_ _]
                                                         {:open! (fn [receive!] (deliver opened receive!) :captured)
                                                          :close! (fn [_])})}}
                         options))]
    (show/sync-action! runtime :configure {:midi {:enabled? true :input-index 0}})
    (show/sync-action! runtime :start {})
    {:runtime runtime :receive! (deref opened 2000 nil)
     :manager (connections/sync (:connections runtime))})))

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

(deftest four-channel-pcm-drives-the-show-only-from-the-selected-channel
  (let [config {:default? true :channels 4 :input-channel 3}
        neighbors (pcm/pcm (mapcat identity (repeat 480 [24576 -16384 0 32767])) false)
        selected (pcm/pcm (mapcat identity (repeat 480 [0 0 16384 0])) false)
        blocks [neighbors selected neighbors]
        reads (atom -1)
        opened (atom [])
        closed (atom 0)
        dimmer (atom nil)
        waiting (vec (repeatedly 4 #(CountDownLatch. 1)))
        available (vec (repeatedly 4 #(CountDownLatch. 1)))
        runtime (show/create-runtime
                 {:model (-> (model)
                             (assoc :time-ms 100.0)
                             (assoc-in [:registry :effects :wash :params :level :default] 0.0))
                  :sink {:send! (fn [universes _]
                                  (reset! dimmer (bit-and 255 (aget ^bytes (get universes 1) 0))))}
                  :audio-options {:open-source!
                                  (fn [settings]
                                    (swap! opened conj (select-keys settings [:default? :channels :input-channel]))
                                    {:started-at-ms 0.0
                                     :read! (fn [out offset length]
                                              (let [n (swap! reads inc)]
                                                (.countDown (nth waiting n))
                                                (if (and (.await (nth available n) 3 TimeUnit/SECONDS)
                                                         (< n (count blocks)))
                                                  (do (System/arraycopy (nth blocks n) 0 out offset length) length)
                                                  -1)))
                                     :close! #(do (swap! closed inc)
                                                  (doseq [gate available] (.countDown gate)))})}})]
    (try
      (let [id (prepare! runtime (spec "audio/rms" :audio [0.0 1.0]))]
        ;; Prime the host before admitting capture. The test source's 0 ms
        ;; origin precedes the initialized 100 ms clock for all three reads.
        (show/tick! runtime 100.0)
        (is (= config (select-keys (:config (show/audio-action! runtime :start config)) (keys config))))
        (doseq [[n rms] [[0 0.0] [1 0.5] [2 0.0]]]
          (.countDown (nth available n))
          (is (.await (nth waiting (inc n)) 2 TimeUnit/SECONDS))
          (show/tick! runtime (+ 120.0 (* n 20.0)))
          (let [model (:model @(:state runtime))
                waveform (:waveform (show/audio-action! runtime :status {}))]
            (is (= rms (get-in model [:variables :audio :features :rms])))
            (is (= rms (get-in model [:routing :ports "audio/rms" :value])))
            (is (= rms (get-in model [:instances id :params :level])))
            (is (= rms (first (get-in (show/snapshot runtime) [:output :values :head :emitted-color]))))
            (is (= (if (= 1 n) 128 0) @dimmer))
            (is (= {:channels 4 :input-channel 3 :mix :selected-channel :sample-end (* 480 (inc n))}
                   (select-keys waveform [:channels :input-channel :mix :sample-end])))
            (is (pcm/close? (if (zero? n) 0.0 (/ 0.5 (Math/sqrt 2.0)))
                            (:rms (first (:bins waveform)))))
            (when (= 1 n)
              (is (pos? (:audio/energy (show/variables runtime)))))))
        (is (:stopped? (show/audio-action! runtime :stop {})))
        (is (= 1 @closed))
        (let [saved (show/export-show runtime)]
          (is (= {:version 1 :config (:config (show/audio-action! runtime :status {})) :capture-wanted? false}
                 (get-in saved [:model :integrations :audio-capture])))
          (is (= :accepted (:status (apply-at! runtime :import {:source (pr-str saved)} 180.0))))
          (let [status (show/audio-action! runtime :status {})]
            (is (= :idle (:state status)))
            (is (= config (select-keys (:config status) (keys config)))))
          (is (= [config] @opened))))
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

(deftest midi-cue-parameter-shapes-group-emission-and-measured-activity
  (let [bytes (atom nil)
        knob {:id :wash :version 1 :params {:rise {:type :number :default 0.0 :bounds [0.0 2.0]}}
              :targets [:head] :outputs {:dimmer 0.0}}
        hit {:id :light :version 1 :targets [:head] :outputs {:dimmer 1.0}}
        rig (control/new-show {:rig {:fixtures [(assoc head :groups [:front] :channels {:dimmer 1})]}
                               :registry {:effects {:wash knob :light hit}
                                          :cues {:wash {:id :wash :version 1 :effect-id :wash}
                                                 :light {:id :light :version 1 :effect-id :light :priority 10}}}})
        {:keys [runtime receive! manager]} (capture-fixture {:model rig :sink {:send! (fn [us _]
                                                        (reset! bytes (bit-and 255 (aget ^bytes (get us 1) 0))))}})
        midi! (fn [t data]
                (receive! {:type :midi/message :source-id :midi-clock :time-ms t :bytes data})
                (is (session/flush! manager))
                (show/tick! runtime t))
        score #(get-in (show/snapshot runtime) [:output :lighting-activity :scene :score])]
    (try
      (is (= :accepted (:status (apply-at! runtime :settings {:scope :lighting-activity :values {:enabled? true}} 0))))
      (is (= :accepted (:status (apply-at! runtime :settings
                  {:scope :output-response :values {:enabled? true :groups {:front {:attack {:beats {:cue-id :wash :param :rise}}
                                                                                    :release {:beats 1}}}}} 0))))
      (let [id (prepare! runtime (-> (spec) (assoc-in [:destinations :level :param] :rise)
                                     (assoc-in [:destinations :level :range] [0.0 2.0])))]
        (midi! 10 [0x90 60 127])
        (is (= 2.0 (get-in @(:state runtime) [:model :instances id :params :rise])))
        (let [light (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :light} 10))]
          (show/tick! runtime 260)
          (let [soft (score)]
            (is (<= 55 @bytes 57))
            (midi! 285 [0x80 60 0])
            (is (= 255 @bytes))
            (is (< soft (score))))
          (apply-at! runtime :command {:op :instance/remove :instance-id light} 285)
          (show/tick! runtime 785)
          (is (<= 93 @bytes 95))))
      (finally (show/stop! runtime)))))

(deftest projected-route-activity-follows-samples-and-release-tails
  (doseq [with-envelope? [false true]]
    (let [rt (show/create-runtime {:model (model)})
          patch (cond-> (spec) with-envelope? (assoc-in [:routes 0 :envelope] {:release {:ms 100}}))]
      (try
        (let [id (prepare! rt patch)
              read-source #(->> (:instances (show/snapshot rt)) (filter (comp #{id} :id)) first
                                :parameter-sources :level :sources first)
              events! (:events! (show/sync-callbacks rt))]
          (is (true? (:pending? (read-source))))
          (events! (observation "note" 127 10))
          (show/tick! rt 10)
          (is (= {:type :patch :route-id :input :source-id source-id} (read-source)))
          (events! (observation "off" 0 20))
          (show/tick! rt 20)
          (show/tick! rt 70)
          (is (= (not with-envelope?) (boolean (:pending? (read-source)))))
          (show/tick! rt 125)
          (is (true? (:pending? (read-source))))
          (is (zero? (get-in @(:state rt) [:model :instances id :params :level]))))
        (finally (show/stop! rt))))))
