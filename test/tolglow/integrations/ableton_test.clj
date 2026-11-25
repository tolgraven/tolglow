(ns tolglow.integrations.ableton-test
  (:require [clojure.test :refer :all]
            [afterglow.control.model :as model]
            [tolglow.integrations.ableton :as live]))

(def envelope {:show-id :main :epoch 0 :command-id "press" :time-ms 0})

(defn bridge [] (live/new-bridge {:mappings {36 {:cue-id :red :mode :held}}
                                 :parameters {1 {:cue-id :red :instance-id "0:1" :param :level :bounds [0 10]}}}))

(def note-on {:type :note-on :channel 1 :note 36 :velocity 127})

(def note-off (assoc note-on :type :note-off :velocity 0))

(def recording {:track-armed? true :automation-armed? true :recording? true
                :writable? true :destination-id "clip-7" :beat 12.5})

(deftest drum-rack-layout-is-64-stable-pads-with-explicit-colors
  (let [pads (live/pad-page [{:id :red :name "Red" :color [1 0 0]}])]
    (is (= 64 (count pads)))
    (is (= (set (range 36 100)) (set (map :note pads))))
    (is (= {:x 0 :y 0 :note 92 :color [1 0 0] :live-color 16711680 :cue-id :red}
           (select-keys (first pads) [:x :y :note :color :live-color :cue-id])))
    (is (nil? (:cue-id (last pads)))))
  (is (thrown? clojure.lang.ExceptionInfo (live/pad-page [{:color [2 0 0]}]))))

(deftest note-launch-and-release-use-the-real-shared-reducer
  (let [show (model/new-show {:registry {:effects {:fx {:id :fx :version 1 :params {}}}
                                        :cues {:red {:id :red :version 1 :effect-id :fx}}}})
        pressed (live/midi->commands (bridge) note-on envelope)
        launched (model/apply-command show (first (:commands pressed)))
        bound (live/acknowledge-launch (:state pressed) 36 (:ack launched) envelope)
        released (live/midi->commands (:state bound) note-off (assoc envelope :command-id "off" :time-ms 1))
        ended (model/apply-command (:model launched) (first (:commands released)))]
    (is (= :accepted (get-in launched [:ack :status])))
    (is (= "0:1" (:instance-id (first (:commands released)))))
    (is (= :accepted (get-in ended [:ack :status])))
    (is (= :releasing (get-in ended [:model :instances "0:1" :phase])))))

(deftest release-before-ack-and-mode-handoff-cannot-leave-stuck-instances
  (let [pressed (live/midi->commands (bridge) note-on envelope)
        released (live/midi->commands (:state pressed) note-off (assoc envelope :command-id "off"))
        ack {:status :accepted :command-id "press" :instance-id "old-instance"}
        bound (live/acknowledge-launch (:state released) 36 ack (assoc envelope :command-id "late-off"))]
    (is (empty? (:commands released)))
    (is (= "old-instance" (:instance-id (first (:commands bound)))))
    (is (empty? (get-in bound [:state :held])))
    (let [handed (live/handoff (:state pressed) :live (assoc envelope :command-id "mode"))
          late (live/acknowledge-launch (:state handed) 36 ack (assoc envelope :command-id "late"))]
      (is (= :live (get-in handed [:state :mode])))
      (is (= "old-instance" (:instance-id (first (:commands late)))))
      (is (empty? (:commands (live/midi->commands (:state handed) note-on envelope)))))))

(deftest grid-holds-use-the-original-note-and-source-through-handoff
  (let [initial (live/new-bridge {:launch-grid {:beats 1/2}
                                  :mappings {36 {:cue-id :red :mode :held}
                                             37 {:cue-id :ice :mode :persistent}}
                                  :parameters {1 {:cue-id :red :param :level :bounds [0 1]}}})
        play (fn [state event source id]
               (live/midi->commands state event
                                    (assoc envelope :command-id id :source {:source-id source :sync-run-id "run"
                                                                           :note (:note event)})))
        live-press (play initial note-on :live "live-press")
        midi-press (play (:state live-press) note-on :midi "midi-press")
        pending (live/acknowledge-launch (:state midi-press) 36 {:command-id "midi-press" :status :pending} envelope)
        adjusted (play (:state pending) {:type :cc :channel 1 :controller 1 :value 127} :midi "knob")
        fired (live/acknowledge-launch (:state adjusted) 36 {:command-id "midi-press" :status :accepted :instance-id "midi-instance"} envelope)
        released (play (:state fired) note-off :live "release")
        persistent (play (:state released) (assoc note-on :note 37) :live "persistent")
        handoff (live/handoff (:state persistent) :live (assoc envelope :command-id "handoff"))]
    (is (= {:beats 1/2} (-> midi-press :commands first :grid)))
    (is (empty? (:commands adjusted)))
    (is (= {:op :param/set :instance-id "midi-instance" :value 1.0}
           (select-keys (first (:commands fired)) [:op :instance-id :value])))
    (is (= "live-press" (-> released :commands first :launch-command-id)))
    (is (= (-> live-press :commands first :source) (-> released :commands first :source)))
    (is (= [{:op :cue/cancel-launch :launch-command-id "midi-press" :reason :held-release}
            {:op :cue/cancel-launch :launch-command-id "persistent" :reason :surface-handoff}]
           (mapv #(select-keys % [:op :instance-id :launch-command-id :reason]) (:commands handoff))))))

(deftest bridge-suppresses-feedback-and-scales-controller-input
  (is (empty? (:commands (live/midi->commands (bridge) (assoc note-on :origin :tolglow-feedback) envelope))))
  (is (empty? (:commands (live/midi->commands (bridge) (assoc note-on :channel 2) envelope))))
  (is (= 10.0 (:value (first (:commands (live/midi->commands (bridge) {:type :cc :channel 1 :controller 1 :value 127} envelope))))))
  (is (thrown? clojure.lang.ExceptionInfo
               (live/midi->commands (bridge) {:type :cc :channel 1 :controller 1 :value 128} envelope))))

(deftest recording-requires-accepted-armed-events-and-never-recurses
  (let [cmd (merge envelope {:op :cue/launch :cue-id :red :source {:type :push}})
        ack {:command-id "press" :status :accepted :instance-id "0:1"}
        first-write (live/record-command (bridge) cmd ack recording)]
    (is (= :note-on (-> first-write :midi first :type)))
    (is (= "clip-7" (-> first-write :intents first :destination-id)))
    (is (empty? (:intents (live/record-command (:state first-write) cmd ack recording))))
    (is (empty? (:intents (live/record-command (bridge) cmd ack (assoc recording :track-armed? false)))))
    (is (empty? (:intents (live/record-command (bridge) cmd (assoc ack :status :rejected) recording))))
    (is (empty? (:intents (live/record-command (bridge) (assoc cmd :source {:origin :live-playback}) ack recording))))
    (let [param (merge envelope {:command-id "knob" :op :param/set :instance-id "0:1" :param :level :value 5})
          ack {:command-id "knob" :status :accepted}]
      (is (= 64 (-> (live/record-command (:state first-write) param ack recording) :midi first :value)))
      (is (empty? (:intents (live/record-command (:state first-write) param ack (assoc recording :automation-armed? false))))))))

(deftest musical-note-launch-duration-is-preserved-as-data
  (let [state (live/new-bridge {:mappings {36 {:cue-id :pulse :mode :one-shot :duration {:beats 2}}}})
        result (live/midi->commands state {:type :note-on :channel 1 :note 36 :velocity 100}
                                      {:command-id "musical-launch" :show-id :main :epoch 1 :time-ms 0})]
    (is (= {:beats 2} (get-in result [:commands 0 :launch :duration])))
    (is (nil? (get-in result [:commands 0 :launch :duration-ms])))))

(deftest stable-parameter-lanes-respect-manual-mappings-and-pinned-schemas
  (let [schemas {:level {:type :number :bounds [-4 4]}
                 :count {:type :integer :bounds [2 7]}
                 :wave {:type :enum :values [:sine :saw :square]}
                 :stepped? {:type :boolean} :rgb {:type :color}}
        registry {:cues {:red {:effect-id :fx :effect-version 3}}
                  :versions {[:effects :fx 3] {:params schemas}}}
        page {:cells {[0 0] :red}}
        manual {:mappings {42 {:cue-id :red :mode :held}}
                :parameters {7 {:cue-id :red :param :level :bounds [-2 2] :lane-note 12}
                             8 {:cue-id :red :param :stepped? :disabled? true}}}
        mapped (live/map-page manual registry page)
        bridge (live/new-bridge mapped)
        mapping #(some (fn [[_ m]] (when (= % (:param m)) m)) (:parameters mapped))]
    (is (= mapped (live/map-page mapped registry page)))
    (is (= (:mappings manual) (:mappings mapped)))
    (is (= [-2 2] (:bounds (mapping :level))))
    (is (nil? (:lane-note (mapping :stepped?))))
    (is (nil? (mapping :rgb)))
    (doseq [[param velocity expected] [[:count 1 2] [:count 127 7] [:wave 64 :saw] [:level 1 -2.0]]]
      (let [note {:type :note-on :note (:lane-note (mapping param)) :velocity velocity :channel 1}
            early (live/midi->commands bridge note envelope)
            press (live/midi->commands (:state early) (assoc note :note 42) envelope)
            queued (live/midi->commands (:state press) note (assoc envelope :command-id "adjust"))
            ack (live/acknowledge-launch (:state queued) 42 {:command-id "press" :status :accepted :instance-id "i"} envelope)]
        (is (= expected (get-in press [:commands 0 :launch :params param])))
        (is (= expected (get-in ack [:commands 0 :value])))
        (is (= "i" (get-in ack [:commands 0 :instance-id])))))))

(deftest saved-enum-lanes-retain-velocity-order-and-registry-identity
  (let [registry {:cues {"auto/red" {:effect-id :fx}}
                  :effects {:fx {:params {:shape {:type :enum :values [:saw :square :sine]}}}}}
        saved {:mappings {36 {:cue-id :auto/red}}
               :parameters {7 {:cue-id :auto/red :param "shape" :type :enum :values ["sine" "saw" "square"] :lane-note 101}}}
        config (-> (live/bind-registry-ids saved registry) (live/map-page registry {:cells {[0 0] "auto/red"}}))]
    (is (= "auto/red" (get-in config [:mappings 36 :cue-id])))
    (is (= {:cue-id "auto/red" :param :shape :type :enum :values [:sine :saw :square] :lane-note 101}
           (get-in config [:parameters 7])))
    (is (= 0xff0000 (:live-color (first (live/pad-page [{:color (thi.ng.color.core/rgba 1 0 0)}])))))))

(deftest parameter-note-before-ack-targets-each-non-held-launch
  (doseq [mode [:persistent :timed :one-shot]]
    (let [bridge (live/new-bridge {:mappings {36 {:cue-id :red :mode mode :duration {:beats 1}}}
                                   :parameters {1 {:cue-id :red :param :level :bounds [0 1] :lane-note 100}}})
          press (live/midi->commands bridge note-on envelope)
          swept (live/midi->commands (:state press) (assoc note-on :note 100 :velocity 127)
                                     (assoc envelope :command-id "knob") "older-instance")
          released (live/midi->commands (:state swept) note-off (assoc envelope :command-id "off"))
          ack {:status :accepted :command-id (:command-id (first (:commands press))) :instance-id "this-instance"}
          acked (live/acknowledge-launch (:state released) 36 ack envelope)]
      (is (empty? (:commands swept)))
      (is (empty? (:commands released)))
      (is (= [{:op :param/set :instance-id "this-instance" :value 1.0}]
             (mapv #(select-keys % [:op :instance-id :value]) (:commands acked))))
      (is (empty? (:pending-launches (:state acked)))))))
