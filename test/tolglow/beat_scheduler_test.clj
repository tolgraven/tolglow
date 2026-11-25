(ns tolglow.beat-scheduler-test
  "Beat-clock scheduled events and parameter ramps (#645), observed through
  captured ArtDmx packets from the real owner tick."
  (:require [afterglow.control.model :as control]
            [afterglow.output.artnet :as artnet]
            [afterglow.render.easing :as easing]
            [afterglow.sync :as sync]
            [afterglow.sync.session :as session]
            [clojure.test :refer [deftest is testing]]
            [tolglow.connections :as connections]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.persistence :as files]
            [tolglow.show :as show]
            [tolglow.spatial-host-test :as spatial-host]))

(def ^:private effect
  {:id :wash :version 1 :category :light
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}
            :tint {:type :color :default [0.0 0.0 0.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- live-model []
  (control/new-show
   {:show-id :schedule-test :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects {:wash effect}
               :cues {:wash {:id :wash :effect-id :wash :version 1}}}
    :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}}))

(def ^:private agent {:adapter :agent :session-id "agent-run"})

(defn- harness []
  (let [packets (atom [])
        sink {:send! (fn [universes _]
                       ;; ArtDmx data starts at byte 18; capture the wire packet itself.
                       (swap! packets conj (vec (artnet/artdmx-packet 1 1 (get universes 1)))))}]
    {:rt (show/create-runtime {:model (live-model) :sink sink :workspace-options nil})
     :packets packets}))

(defn- dimmer [packet] (bit-and 255 (nth packet 18)))

(defn- at! ([rt time payload] (at! rt time payload nil))
  ([rt time payload authority]
   (let [reply (show/submit! rt :command payload authority)]
     (show/tick! rt time)
     (deref reply 1000 ::missing))))

(defn- beat [rt] (get-in (show/snapshot rt) [:transport :beat]))

(defn- level [rt id] (get-in @(:state rt) [:model :instances id :params :level]))

(defn- recent [rt] (get-in (show/snapshot rt) [:schedule :recent]))

(defn- ramp-scenario
  "Launch a wash, ramp it 0 -> 1 over 8 beats with ease-in-out, halve the tempo
  halfway through and return every frame's [time beat dmx] plus the final state."
  []
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))
            posted (at! rt 250 {:op :param/ramp :event-id "swell" :label "Swell" :instance-id id :param :level
                                :to 1.0 :over {:beats 8} :curve :ease-in-out :source agent})
            frame (fn [t] [t (beat rt) (dimmer (peek @packets))])
            first-frame (frame 250)
            before (mapv (fn [t] (show/tick! rt t) (frame t)) (range 375 2250 125))
            tempo (at! rt 2250 {:op :transport/set :transport {:bpm 60.0} :source {:adapter :repl :session-id "r"}})
            mid (frame 2250)
            committed-mid [(level rt id)]
            projection (show/schedule rt)
            after (mapv (fn [t] (show/tick! rt t) (frame t)) (range 2500 6500 250))]
        {:id id :posted posted :tempo tempo :projection projection :frames-meta {:committed-mid committed-mid}
         :frames (into [first-frame] (concat before [mid] after))
         :level (level rt id) :recent (recent rt) :ramps (get-in (show/snapshot rt) [:schedule :ramps])
         :history (get-in @(:state rt) [:model :control :history])
         :packets @packets})
      (finally (show/stop! rt)))))

(deftest param-ramp-matches-the-easing-curve-across-a-mid-ramp-tempo-change
  (let [{:keys [posted tempo projection frames level recent ramps packets id history frames-meta]} (ramp-scenario)
        expected (fn [b] (* 255.0 (easing/sample :ease-in-out (/ (- b 0.5) 8.0))))]
    (is (= :accepted (:status posted)))
    (is (= ["swell"] (:event-ids posted)))
    (is (= [0.5] (:due-beats posted)) "the receipt shows the resolved due beat")
    (is (= :accepted (:status tempo)))
    (testing "every captured frame matches the curve sampled at that frame's beat within DMX quantization"
      (doseq [[t b value] frames]
        (is (<= (Math/abs (- value (expected b))) 1.0) (pr-str {:time t :beat b :dmx value :expected (expected b)}))))
    (testing "the projection describes the running ramp"
      (let [ramp (first (:ramps projection))]
        (is (= {:event-id "swell" :label "Swell" :category :ramp :from 0.0 :to 1.0 :curve :ease-in-out
                :unit :beats :over {:beats 8} :start 0.5 :end 8.5
                :destination {:target :param :instance-id id :param :level}}
               (select-keys ramp [:event-id :label :category :from :to :curve :unit :over :start :end :destination])))
        (is (= :agent (get-in ramp [:source :adapter])))
        (is (not (contains? ramp :progress)) "live progress is derived from the transport, not published per frame")))
    (testing "mid-ramp values are a frame overlay; the committed parameter keeps its start value"
      (is (every? #(= 0.0 %) (:committed-mid frames-meta))))
    (testing "the remaining four beats take twice as long after the tempo halves"
      (let [by-time (into {} (map (fn [[t b v]] [t [b v]])) frames)]
        (is (= 4.5 (first (by-time 2250))))
        (is (<= 127 (second (by-time 2250)) 128))
        (is (= 8.0 (first (by-time 5750))))
        (is (< (second (by-time 5750)) 255))
        (is (= [8.5 255] (by-time 6250)))))
    (is (= 1.0 level))
    (is (empty? ramps))
    (is (= {:event-id "swell" :status :completed :category :ramp}
           (select-keys (last recent) [:event-id :status :category])))
    (is (some #(= {:op :param/set :instance-id id :param :level :value 1.0}
                  (select-keys (:command %) [:op :instance-id :param :value]))
              (vals history))
        "the finished ramp commits its exact target through the shared reducer")
    (testing "replaying the same command log gives identical ArtDmx output"
      (is (= packets (:packets (ramp-scenario)))))))

(defn- launch-scenario []
  (let [{:keys [rt packets]} (harness)
        manager (connections/sync (:connections rt))
        clock! (fn [position at discontinuity?]
                 (session/apply-clock! manager nil {:type :clock :source-id :midi-clock :time-ms at
                                                    :beat position :bpm 120 :playing? true :confidence 1
                                                    :discontinuity? discontinuity?}))
        step! (fn [position at & [discontinuity?]]
                (clock! position at (boolean discontinuity?))
                (show/tick! rt at)
                {:beat (beat rt) :dmx (dimmer (peek @packets))
                 :instances (count (get-in @(:state rt) [:model :instances]))})]
    (try
      (sync/select-source! manager :midi-clock)
      (clock! 0.0 100 false)
      (let [posted (at! rt 100 {:op :schedule/post :command-id "plan-1" :source agent
                                :events [{:id "drop" :label "Drop" :op :cue/launch :cue-id :wash
                                          :launch {:mode :persistent :params {:level 1.0}} :at {:bar 2}}
                                         {:id "outro" :op :cue/launch :cue-id :wash :at {:bar 4}}]})
            upcoming (get-in (show/snapshot rt) [:schedule :upcoming])
            frames [(step! 7.5 200) (step! 7.9 300) (step! 8.1 400)]
            launched (first (vals (get-in @(:state rt) [:model :instances])))
            seek (step! 20.0 500 true)
            after-seek (show/schedule rt)
            looped [(step! 6.0 600 true) (step! 8.5 700) (step! 16.5 800) (step! 17.0 900)]]
        {:posted posted :upcoming upcoming :frames frames :launched launched :seek seek
         :after-seek after-seek :looped looped :packets @packets})
      (finally (show/stop! rt)))))

(deftest scheduled-launch-fires-on-the-crossing-frame-and-a-seek-reports-the-miss-once
  (let [{:keys [posted upcoming frames launched seek after-seek looped packets]} (launch-scenario)]
    (is (= :accepted (:status posted)))
    (is (= [["drop" 8.0 :absolute :cue "Drop"] ["outro" 16.0 :absolute :cue nil]]
           (mapv (juxt :event-id :due-beat :anchor :category :label) upcoming)))
    (is (= [[7.5 0 0] [7.9 0 0] [8.1 255 1]] (mapv (juxt :beat :dmx :instances) frames))
        "the launch fires on the frame that crosses bar 2, not before")
    (is (= 8.1 (:started-at-beat launched)))
    (is (= :agent (get-in launched [:source :adapter])))
    (is (= "drop" (get-in launched [:source :scheduled-event-id])))
    (is (= 1 (:instances seek)))
    (is (empty? (:upcoming after-seek)))
    (is (= [["drop" :fired nil] ["outro" :missed :clock-discontinuity]]
           (mapv (juxt :event-id :status :reason) (:recent after-seek))))
    (is (= [1 1 1 1] (mapv :instances looped)) "looping back past bar 2 and bar 4 fires neither again")
    (is (= [["drop" :fired nil] ["outro" :missed :clock-discontinuity]]
           (mapv (juxt :event-id :status :reason) (:recent after-seek))))
    (testing "replaying the same command and clock log gives identical ArtDmx output"
      (is (= packets (:packets (launch-scenario)))))))

(deftest superseding-ramp-continues-without-a-jump-and-a-human-gesture-cancels
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))
            _ (at! rt 250 {:op :param/ramp :event-id "up" :instance-id id :param :level :to 1.0
                           :over {:beats 8} :source agent})
            frames (mapv (fn [t] (show/tick! rt t) (dimmer (peek @packets))) (range 500 2250 250))
            superseded (at! rt 2250 {:op :param/ramp :event-id "down" :instance-id id :param :level :to 0.0
                                     :over {:beats 8} :source agent})
            down-start (dimmer (peek @packets))
            down (mapv (fn [t] (show/tick! rt t) (dimmer (peek @packets))) (range 2500 3500 250))
            series (concat frames [down-start] down)
            ramp (first (get-in (show/snapshot rt) [:schedule :ramps]))]
        (is (= :accepted (:status superseded)))
        (is (= {:event-id "up" :status :cancelled :reason :superseded :by "down"}
               (select-keys (last (filter #(= "up" (:event-id %)) (recent rt))) [:event-id :status :reason :by])))
        (is (= 0.5 (:from ramp)) "the new ramp starts from the old ramp's value at its start beat")
        (is (<= 127 down-start 128))
        (is (every? #(<= (Math/abs (long %)) 17) (map - (rest series) series))
            (str "no frame-to-frame jump: " (vec series)))
        (testing "a human gesture on the same parameter cancels the running ramp and records why"
          (let [gesture {:instance-id id :param :level :control-id :encoder-1 :gesture-id :g1
                         :source {:adapter :push-3 :session-id "push"}}
                operator {:operator-id :front-of-house}
                begun (at! rt 3500 (assoc gesture :op :gesture/begin) operator)
                sampled (at! rt 3750 (assoc gesture :op :gesture/sample :sequence 1 :value 0.9) operator)]
            (is (= :accepted (:status begun)))
            (is (= :accepted (:status sampled)))
            (is (empty? (get-in (show/snapshot rt) [:schedule :ramps])))
            (is (= {:event-id "down" :status :cancelled :reason :human-override}
                   (select-keys (last (recent rt)) [:event-id :status :reason])))
            (is (= :gesture/begin (get-in (last (recent rt)) [:by :op])))
            (show/tick! rt 4000)
            (is (= 0.9 (level rt id)) "the human value stays; the ramp no longer writes"))))
      (finally (show/stop! rt)))))

(defn- batch-events [bad-index]
  (vec (for [i (range 16)]
         (let [at {:beat (+ 4 i)}
               event (case (mod i 4)
                       0 {:op "cue/launch" :cue-id "wash" :at at}
                       1 {:op "param/ramp" :cue-id "wash" :param "level" :to 0.5 :over {:beats 2} :curve "sine" :at at}
                       2 {:op "instance/end" :cue-id "wash" :at at}
                       3 {:op "variable/set" :variable "section" :value i :at at})]
           (cond-> (assoc event :id (str "e" i) :label (str "Event " i))
             (= i bad-index) (assoc :curve "wobble"))))))

(deftest a-batch-of-sixteen-agent-events-validates-atomically-and-projects-in-order
  (let [{:keys [rt]} (harness)
        tool! (fn [_ tool arguments command-id]
                (let [result (future (show/dispatch-tool! rt tool arguments {:command-id command-id :run-id "agent-run"}))]
                  ;; dispatch-tool! waits for the owner; drive bounded ticks until it answers.
                  (loop [n 0]
                    (when (and (not (realized? result)) (< n 400))
                      (show/tick! rt (inc (get-in @(:state rt) [:model :time-ms])))
                      (Thread/sleep 5) (recur (inc n))))
                  (deref result 1000 ::timeout)))]
    (try
      (let [rejected (tool! 100 "schedule_events" {:events (batch-events 13)} "batch-bad")]
        (is (= :rejected (:status rejected)))
        (is (= :invalid-schedule (:code rejected)))
        (is (re-find #"Event 13" (:message rejected)))
        (is (empty? (get-in (show/snapshot rt) [:schedule :upcoming])) "one bad event rejects all sixteen"))
      (let [accepted (tool! 200 "schedule_events" {:events (batch-events nil)} "batch-good")
            {:keys [upcoming]} (show/schedule rt)
            compact (show/dispatch-tool! rt "inspect_schedule" {} {:run-id "agent-run"})]
        (is (= :accepted (:status accepted)))
        (is (= (mapv #(str "e" %) (range 16)) (:event-ids accepted)))
        (is (= (mapv #(str "e" %) (range 16)) (mapv :event-id upcoming)))
        (is (= (take 16 (cycle [:cue :ramp :instance :variable])) (map :category upcoming)))
        (is (= (mapv #(double (+ 4 %)) (range 16)) (mapv :due-beat upcoming)))
        (is (every? #(= {:adapter :agent :session-id "agent-run"} (:source %)) upcoming))
        (is (= {:target :param :param :level :to 0.5 :over {:beats 2} :curve :sine
                :select {:ids [:wash] :pick :all}}
               (select-keys (second upcoming) [:target :param :to :over :curve :select])))
        (is (= [4.0 "e0" :cue/launch "Event 0"] (first (:upcoming compact))))
        (is (= 16 (count (:upcoming compact))))
        (testing "REPL and Studio read one projection"
          (is (= (show/schedule rt) (:schedule (show/snapshot rt)))))
        (testing "agents cannot schedule master or blackout, and cannot cancel or move other posters' events"
          (let [master (tool! 250 "schedule_events" {:events [{:id "m" :op "master/set" :value 0.2 :in {:bars 1}}]} "agent-master")
                blackout (tool! 250 "schedule_events" {:events [{:id "b" :op "blackout/set" :value true :in {:bars 1}}]} "agent-blackout")]
            (is (= :rejected (:status master)))
            (is (re-find #"authoritative" (:message master)))
            (is (= :rejected (:status blackout))))
          (at! rt (inc (get-in @(:state rt) [:model :time-ms]))
               {:op :schedule/post :source {:adapter :repl :session-id "fod"}
                :events [{:id "human" :op :cue/launch :cue-id :wash :in {:bars 8}}]})
          (let [denied (tool! 260 "cancel_scheduled" {:event-ids ["human"]} "agent-cancel-other")
                compact (show/dispatch-tool! rt "inspect_schedule" {} {:run-id "agent-run"})
                move (at! rt (inc (get-in @(:state rt) [:model :time-ms]))
                          {:op :schedule/move :event-id "human" :at {:bar 20} :source {:adapter :agent :session-id "agent-run"}})]
            (is (= [] (:event-ids denied)) "an agent's cancel only matches what it posted")
            (is (some #(= "human" (second %)) (:upcoming compact)))
            (is (= :rejected (:status move)))
            (is (re-find #"poster or a human" (:message move)))
            (show/cancel-events! rt {:event-ids ["human"]})
            (show/tick! rt (inc (get-in @(:state rt) [:model :time-ms])))))
        (testing "cancel by source withdraws everything that agent run posted"
          (let [cancelled (tool! 300 "cancel_scheduled" {:mine? true} "cancel-mine")]
            (is (= :accepted (:status cancelled)))
            (is (= 16 (count (:event-ids cancelled))))
            (is (empty? (:upcoming (show/schedule rt))))
            (is (= :cancelled (:status (last (:recent (show/schedule rt)))))))))
      (finally (show/stop! rt)))))

(deftest scheduled-events-and-ramps-survive-a-full-backup
  (let [{:keys [rt]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))
            _ (at! rt 250 {:op :schedule/post :source {:adapter :repl :session-id "fod"}
                           :events [{:id "glow" :op :param/ramp :instance-id id :param :tint
                                     :to [1.0 0.5 0.0] :over {:bars 2} :curve :ease-out}
                                    {:id "dark" :op :master/set :value 0.25 :in {:phrases 1}}
                                    {:id "dim" :op :param/ramp :target :master :to 0.0 :over {:ms 4000}
                                     :at {:bar 8}}]})
            _ (show/tick! rt 1250)
            model (:model @(:state rt))
            restored (files/thaw (live-model) (files/freeze model))
            s (:event-subscribers restored)]
        (is (= ["dark" "dim"] (mapv #(get-in % [:scheduled :event-id]) (:runs s))))
        (is (= [:relative :absolute] (mapv :anchor (:runs s))))
        (is (= ["glow"] (mapv :event-id (:ramps s))))
        (is (= (mapv #(dissoc % :source) (:upcoming (subscribers/schedule-view model)))
               (mapv #(dissoc % :source) (:upcoming (subscribers/schedule-view (assoc restored :transport (:transport model)))))))
        (is (= (map #(select-keys % [:event-id :from :to :curve :start :end]) (:ramps (subscribers/schedule-view model)))
               (map #(select-keys % [:event-id :from :to :curve :start :end]) (:ramps s))))
        (is (= [0.0 0.0 0.0] (get-in model [:instances id :params :tint])) "a backup holds the committed start value")
        (let [tint (get-in (subscribers/frame-overlays model 1250) [:params id :tint])]
          (is (every? #(< 0 % 1) (take 2 tint)) (pr-str tint))))
      (finally (show/stop! rt)))))

(deftest invalid-requests-are-rejected-with-reasons
  (let [{:keys [rt]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))]
        (doseq [[bad message] [[{:op :param/ramp :instance-id id :param :level :to 2.0 :over {:beats 1}} #"satisfy"]
                               [{:op :param/ramp :instance-id id :param :missing :to 0.5 :over {:beats 1}} #"Unknown parameter"]
                               [{:op :param/ramp :instance-id id :param :level :to 0.5 :over {:beats 0}} #"positive"]
                               [{:op :cue/launch :cue-id :nope :at {:bar 1}} #"Unknown cue"]
                               [{:op :master/set :value 0.5 :at {:bar 1} :in {:beats 1}} #"only one"]]]
          (let [ack (at! rt 100 bad)]
            (is (= :rejected (:status ack)) (pr-str bad))
            (is (re-find message (str (:message ack))) (pr-str ack))))
        (is (empty? (:upcoming (show/schedule rt))))
        (is (= :rejected (:status (at! rt 200 {:op :schedule/post :events [{:id "a" :op :master/set :value 0.1 :in {:bars 1}}
                                                                       {:id "a" :op :master/set :value 0.2 :in {:bars 2}}]}))))
        (is (= :accepted (:status (at! rt 300 {:op :master/set :value 0.1 :in {:bars 1} :event-id "a"}))))
        (is (= :rejected (:status (at! rt 400 {:op :master/set :value 0.2 :in {:bars 1} :event-id "a"})))
            "a pending event ID cannot be reused")
        (is (= :accepted (:status (at! rt 500 {:op :schedule/move :event-id "a" :at {:bar 3}}))))
        (is (= 12.0 (:due-beat (first (:upcoming (show/schedule rt))))))
        (testing "a Studio-style cancel by event ID is not narrowed by the canceller's own source"
          (let [ack (at! rt 600 {:op :schedule/cancel :event-ids ["a"] :source {:adapter :web :session-id "s"}})]
            (is (= ["a"] (:event-ids ack)))
            (is (empty? (:upcoming (show/schedule rt))))
            (is (= [:cancelled :operator-cancel] ((juxt :status :reason) (last (recent rt))))))))
      (finally (show/stop! rt)))))

(deftest master-variable-and-subscriber-steps-share-the-clock
  (let [{:keys [rt packets]} (harness)
        energy #(get-in @(:state rt) [:model :variables :energy])]
    (try
      (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent :params {:level 1.0}}})
      (at! rt 100 {:op :schedule/post :source {:adapter :repl :session-id "fod"}
                   :events [{:id "fade" :op :param/ramp :target :master :to 0.0 :over {:ms 1000}}
                            {:id "count" :op :param/ramp :target :variable :variable :energy :from-note "x"}]})
      (is (empty? (:upcoming (show/schedule rt))) "an unknown option rejects the batch")
      (at! rt 100 {:op :schedule/post :source {:adapter :repl :session-id "fod"}
                   :events [{:id "fade" :op :param/ramp :target :master :to 0.0 :over {:ms 1000}}
                            {:id "count" :op :variable/set :variable :energy :value 0.0}
                            {:id "rise" :op :param/ramp :target :variable :variable :energy :to 1.0
                             :over {:bars 1} :in {:beats 1}}]})
      (show/tick! rt 600)
      (is (= 1.0 (:master (:model @(:state rt)))) "the committed master keeps its value mid-ramp")
      (is (<= 126 (dimmer (peek @packets)) 129) "the frame uses the ramped master; ms is an explicit boundary unit")
      (is (= 0.0 (energy)) "the bar-long variable ramp starts at beat 1.2")
      (show/tick! rt 1100)
      (is (= 0.0 (:master (:model @(:state rt)))))
      (is (= 0 (dimmer (peek @packets))))
      (show/tick! rt 1600)
      (is (= 0.0 (energy)) "the committed variable keeps its value mid-ramp")
      (is (< (Math/abs (- 0.5 (get-in (subscribers/frame-overlays (:model @(:state rt)) 1600) [:variables :energy]))) 1e-9)
          "beat 3.2 is halfway through the four-beat ramp")
      (is (= :rejected (:status (at! rt 1700 {:op :variable/set :variable :runtime :value 1})))
          "runtime-owned variables are reserved")
      (testing "subscriber actions accept :at, :in and :grid next to :delay"
        (is (= [4.0 :absolute] (subscribers/step-due {:op :master/set :value 1.0 :grid {:bars 1}} 1.5 1.5 nil)))
        (is (= [4.0 :absolute] (subscribers/step-due {:op :master/set :value 1.0 :grid {:bars 1}} 4.0 4.0 nil)))
        (is (= [24.0 :absolute] (subscribers/step-due {:op :master/set :value 1.0 :at {:bar 6}} 1.5 1.5 nil)))
        (is (= [3.0 :relative] (subscribers/step-due {:op :master/set :value 1.0 :in {:beats 2}} 2.5 1.0 nil)))
        (is (= [4.5 :relative] (subscribers/step-due {:op :master/set :value 1.0 :delay {:beats 2}} 2.5 1.0 nil)))
        (is (= [21.0 :absolute] (subscribers/step-due {:op :master/set :value 1.0 :at {:bar 5 :beat 1}} 0 0
                                                      {:beats-per-bar 4}))))
      (finally (show/stop! rt)))))

(deftest rebase-keeps-relative-waits-and-ramp-progress-but-misses-passed-positions
  (let [run (fn [id event-id at due anchor]
              {:id id :steps [(merge {:op :master/set :value 1.0} at)] :index 0 :due-beat due :anchor anchor
               :event {:type :schedule/posted} :scheduled {:event-id event-id :source agent}})
        model {:transport {:beat 10.0 :meter {:beats-per-bar 4 :bars-per-phrase 4}}
               :event-subscribers {:subscribers [] :next-run 3 :seed 1 :dropped 0
                                   :runs [(run 1 "soon" {:in {:beats 4}} 14.0 :relative)
                                          (run 2 "bar5" {:at {:bar 5}} 20.0 :absolute)
                                          (run 3 "bar10" {:at {:bar 10}} 40.0 :absolute)]
                                   :ramps [{:event-id "r" :destination {:target :master} :from 0.0 :to 1.0
                                            :curve :linear :unit :beats :start 8.0 :end 16.0 :value 0.25 :progress 0.25}]}}
        s (:event-subscribers (subscribers/rebase-clock model 22.0 32.0))]
    (is (= [["soon" 36.0] ["bar10" 40.0]] (mapv (juxt (comp :event-id :scheduled) :due-beat) (:runs s))))
    (is (= [30.0 38.0] ((juxt :start :end) (first (:ramps s)))) "the ramp keeps its elapsed musical progress")
    (is (= [["bar5" :missed :clock-discontinuity]] (mapv (juxt :event-id :status :reason) (:recent s))))
    (is (= s (subscribers/validate-state s)))
    (testing "a small forward phase correction leaves a crossed bar due now instead of missed"
      (let [nudged (subscribers/rebase-clock (assoc-in model [:transport :beat] 20.01) 0.02 20.01)]
        (is (= ["soon" "bar5" "bar10"] (mapv (comp :event-id :scheduled) (get-in nudged [:event-subscribers :runs]))))
        (is (empty? (get-in nudged [:event-subscribers :recent])))))
    (testing ":grid steps re-quantize from the new position instead of freezing"
      (let [grid (assoc model :event-subscribers
                        (assoc (:event-subscribers model) :runs [(run 1 "next-phrase" {:grid {:phrases 1}} 16.0 :absolute)]))
            seeked (subscribers/rebase-clock (assoc-in grid [:transport :beat] 37.0) 27.0 37.0)]
        (is (= [48.0] (mapv :due-beat (get-in seeked [:event-subscribers :runs]))))
        (is (empty? (get-in seeked [:event-subscribers :recent])))))))

(deftest a-position-already-past-is-rejected-at-post
  (let [{:keys [rt]} (harness)]
    (try
      (show/tick! rt 0)
      (show/tick! rt 2500)                                  ; beat 5.0
      (let [past (at! rt 2600 {:op :cue/launch :cue-id :wash :at {:bar 1} :source agent})
            now (at! rt 2700 {:op :cue/launch :cue-id :wash :at {:bar 2} :event-id "b2" :source agent})]
        (is (= :rejected (:status past)))
        (is (re-find #"already passed" (:message past)))
        (is (re-find #"zero-based" (:message past)))
        (is (= :accepted (:status now)))
        (is (= [8.0] (:due-beats now))))
      (finally (show/stop! rt)))))

(deftest any-committed-write-to-a-ramped-destination-cancels-it-at-the-domain-layer
  (let [model (assoc-in (live-model) [:transport :beat] 0.0)
        cmd (fn [m id payload]
              (:model (control/apply-command m (merge {:show-id (:show-id m) :epoch (:epoch m) :time-ms 0.0
                                                       :command-id id} payload))))
        launched (cmd model "launch" {:op :cue/launch :cue-id :wash :source {:adapter :repl}})
        id (first (keys (:instances launched)))
        ramped (-> (:model (control/apply-domain-command
                            launched {:op :schedule/post :show-id (:show-id launched) :epoch (:epoch launched)
                                      :time-ms 0.0 :command-id "post" :source agent
                                      :events [{:id "r" :op :param/ramp :instance-id id :param :level :to 1.0 :over {:beats 8}}]}
                            subscribers/apply-operation))
                   (subscribers/advance 0.0 {}))
        cancelled (fn [source]
                    (-> (cmd (assoc-in ramped [:transport :beat] 2.0) "write"
                             {:op :param/set :instance-id id :param :level :value 0.3 :source source})
                        (subscribers/advance-ramps 1000.0)))]
    (is (= 1 (count (get-in ramped [:event-subscribers :ramps]))))
    (testing "a macro, routing or automation write (any non-human source) supersedes the ramp"
      (let [m (cancelled {:adapter :composition :session-id "macro-1"})]
        (is (empty? (get-in m [:event-subscribers :ramps])))
        (is (= {:event-id "r" :status :cancelled :reason :superseded}
               (select-keys (last (get-in m [:event-subscribers :recent])) [:event-id :status :reason])))
        (is (= {:op :param/set :command-id "write" :source {:adapter :composition :session-id "macro-1"}}
               (:by (last (get-in m [:event-subscribers :recent])))))
        (is (= 0.3 (get-in m [:instances id :params :level])) "the writer's value stays")))
    (testing "a write from a human surface records a human override"
      (is (= :human-override (:reason (last (get-in (cancelled {:adapter :push-3 :session-id "p"})
                                                    [:event-subscribers :recent]))))))
    (testing "device sessions are named by their profile ID: an X-Touch Extender, Seaboard or MPE write is human too"
      (doseq [adapter [:x-touch :x-touch-xt :seaboard-m :mpe :midi :osc]]
        (is (= :human-override (:reason (last (get-in (cancelled {:adapter adapter :session-id "device"})
                                                      [:event-subscribers :recent]))))
            (str adapter))))))

(deftest a-restored-schedule-follows-a-transport-that-starts-elsewhere
  (let [file (java.io.File/createTempFile "schedule-restore" ".tolglow")
        source {:adapter :repl :session-id "fod"}]
    (try
      (let [{:keys [rt]} (harness)]
        (try
          (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))]
            (at! rt 500 {:op :schedule/post :source source
                         :events [{:id "wait" :op :variable/set :variable :section :value 2 :in {:bars 2}}
                                  {:id "bar4" :op :cue/launch :cue-id :wash :at {:bar 4}}
                                  {:id "swell" :op :param/ramp :instance-id id :param :level :to 1.0 :over {:bars 2}}
                                  {:id "fade" :op :param/ramp :target :master :to 0.5 :over {:ms 4000}}]})
            (show/tick! rt 1000)                            ; beat 2.0
            (show/save-show! rt (str file)))
          (finally (show/stop! rt))))
      (let [{:keys [rt]} (harness)
            manager #(connections/sync (:connections rt))   ; a restore replaces the sync session
            clock! (fn [position at] (session/apply-clock! (manager) nil {:type :clock :source-id :midi-clock :time-ms at
                                                                        :beat position :bpm 120 :playing? true
                                                                        :confidence 1 :discontinuity? false}))]
        (try
          (show/tick! rt 50)
          (is (= :accepted (:status (let [reply (show/load-show! rt (str file))]
                                      (show/tick! rt 60) (deref reply 1000 ::missing)))))
          (is (= ["wait" "bar4"] (mapv :event-id (:upcoming (show/schedule rt))))
              "on the restored show clock nothing has moved")
          ;; The restored show now follows a clock that is elsewhere (beat 100).
          (let [restored-ms (get-in @(:state rt) [:model :time-ms])]
            (sync/select-source! (manager) :midi-clock)
            (clock! 100.0 (+ restored-ms 10))
            (show/tick! rt (+ restored-ms 10))
            (let [{:keys [upcoming ramps recent]} (show/schedule rt)
                  by-id (into {} (map (juxt :event-id identity)) ramps)]
              (is (= ["wait"] (mapv :event-id upcoming)))
              (is (= 107.0 (:due-beat (first upcoming))) "the relative wait keeps its remaining 7 beats")
              (is (= ["bar4" :missed :clock-discontinuity] ((juxt :event-id :status :reason) (last recent))))
              (is (= [99.0 107.0] ((juxt :start :end) (get by-id "swell"))) "the beat ramp keeps its elapsed beat")
              (is (= 3500.0 (- (:end (get by-id "fade")) restored-ms)) "the ms ramp keeps its remaining milliseconds")))
          (finally (show/stop! rt))))
      (finally (.delete file)))))

(deftest a-spatial-cues-opacity-ramp-fades-across-captured-frames
  (let [rt (spatial-host/runtime false)
        request {:op :spawn :id :orb :page-id :main :cell [2 1]
                 :volume {:shape :ball :position [0 1 0] :velocity [0 0 0] :gravity [0 0 0]
                          :radius 0.5 :restitution 0.75 :selection #{:position}
                          :falloff {:inner-m 0.5 :outer-m 0.5 :curve :linear}}
                 :paint {:red 1.0 :blue 0.0}}]
    (try
      (spatial-host/submit rt 0 :command {:op :cue/launch :cue-id :blue})
      (let [id (:instance-id (spatial-host/submit rt 0 :spatial-cue request))
            full (spatial-host/output rt)
            _ (spatial-host/submit rt 10 :command {:op :param/ramp :event-id "orb-out" :instance-id id :target :opacity
                                                   :to 0.0 :over {:ms 1000} :source {:adapter :repl :session-id "fod"}})
            frames (mapv (fn [t] (spatial-host/tick rt t) [(first (spatial-host/output rt))
                                                            (get-in @(:state rt) [:model :instances id :system :opacity] 1.0)])
                         [260 510 760])]
        (is (= 255 (first full)))
        (is (apply > (map first frames)) (str "the orb red fades frame by frame: " frames " " (pr-str (:recent (show/schedule rt)))))
        (is (every? #(< 0 % 255) (map first frames)) (str "no jump to either end mid-ramp: " frames))
        (is (every? #(== 1.0 %) (map second frames)) "the committed opacity holds until completion")
        (spatial-host/tick rt 1010)
        (is (= 0 (first (spatial-host/output rt))))
        (is (== 0.0 (get-in @(:state rt) [:model :instances id :system :opacity]))))
      (finally (show/stop! rt)))))

(deftest a-stage-that-reboxes-an-equal-value-does-not-commit-the-overlay
  (let [model (assoc-in (live-model) [:transport :beat] 0.0)
        launched (:model (control/apply-command model {:op :cue/launch :cue-id :wash :source {:adapter :repl}
                                                      :show-id (:show-id model) :epoch (:epoch model)
                                                      :time-ms 0.0 :command-id "launch"}))
        id (first (keys (:instances launched)))
        ramped (-> (:model (control/apply-domain-command
                            launched {:op :schedule/post :show-id (:show-id launched) :epoch (:epoch launched)
                                      :time-ms 0.0 :command-id "post" :source agent
                                      :events [{:id "r" :op :param/ramp :instance-id id :param :level :to 1.0 :over {:beats 8}}]}
                            subscribers/apply-operation))
                   (subscribers/advance 0.0 {})
                   (assoc-in [:transport :beat] 4.0))
        overlays (subscribers/frame-overlays ramped 2000.0)
        staged (subscribers/with-frame-overlays ramped overlays)
        ;; A stage rebuilds params with an equal value in a fresh box.
        reboxed (update-in staged [:instances id :params :level] #(Double/valueOf (double %)))
        restored (subscribers/without-frame-overlays reboxed ramped overlays)]
    (is (== 0.5 (get-in staged [:instances id :params :level])))
    (is (not (identical? (get-in staged [:instances id :params :level]) (get-in reboxed [:instances id :params :level]))))
    (is (== 0.0 (get-in restored [:instances id :params :level])) "the committed start value is restored")
    (is (= ["r"] (mapv :event-id (get-in (subscribers/advance-ramps restored 2000.0) [:event-subscribers :ramps])))
        "the ramp continues")
    (testing "a stage's genuine write is kept and cancels the ramp"
      (let [written (subscribers/without-frame-overlays (assoc-in staged [:instances id :params :level] 0.7) ramped overlays)]
        (is (== 0.7 (get-in written [:instances id :params :level])))
        (is (empty? (get-in (subscribers/advance-ramps written 2000.0) [:event-subscribers :ramps])))))))

(deftest non-human-posters-cannot-target-scene-or-show-control-instances
  (let [rt (spatial-host/runtime false)
        request {:op :spawn :id :orb :page-id :main :cell [2 1]
                 :volume {:shape :ball :position [0 1 0] :velocity [0 0 0] :gravity [0 0 0]
                          :radius 0.5 :restitution 0.75 :selection #{:position}
                          :falloff {:inner-m 0.5 :outer-m 0.5 :curve :linear}}
                 :paint {:red 1.0 :blue 0.0}}
        ramp (fn [id] {:op :param/ramp :instance-id id :target :opacity :to 0.0 :over {:ms 500}})
        post! (fn [now source events]
                (let [reply (show/submit! rt {:op :schedule/post :source source :events events})]
                  (spatial-host/tick rt now)
                  (deref reply 1000 ::missing)))]
    (try
      (let [blue (:instance-id (spatial-host/submit rt 0 :command {:op :cue/launch :cue-id :blue}))
            next-id (str (subs blue 0 (inc (.indexOf ^String blue ":")))
                         (inc (parse-long (subs blue (inc (.indexOf ^String blue ":"))))))
            ;; Posted before the scene instance exists, so only the fire-time check can stop it.
            early (post! 5 agent [(merge (ramp next-id) {:id "early" :in {:beats 1}})])
            orb (:instance-id (spatial-host/submit rt 10 :spatial-cue request))]
        (is (= :accepted (:status early)))
        (is (= next-id orb))
        (testing "rejected at post when the instance exists"
          (doseq [[source events] [[agent [(merge (ramp orb) {:id "a1"})]]
                                   [{:adapter :reactive :session-id "rx"} [{:id "a2" :op :instance/kill :instance-id orb :in {:beats 1}}]]]]
            (let [ack (post! 20 source events)]
              (is (= :rejected (:status ack)) (pr-str source))
              (is (re-find #"stay with the operator" (str (:message ack)))))))
        (testing "rejected again when due"
          (doseq [t [700 1300 1600]] (spatial-host/tick rt t))
          (is (= {:event-id "early" :status :rejected :message "Scene and show-control instances stay with the operator"}
                 (select-keys (last (filter #(= "early" (:event-id %)) (:recent (show/schedule rt))))
                              [:event-id :status :message])))
          (is (= 255 (first (spatial-host/output rt))) "the scene instance is untouched")
          (is (some? (get-in @(:state rt) [:model :instances orb]))))
        (testing "a human operator may address it by exact instance ID"
          (is (= :accepted (:status (post! 1700 {:adapter :repl :session-id "fod"} [(merge (ramp orb) {:id "h1"})]))))))
      (finally (show/stop! rt)))))

(deftest numeric-recoercion-is-not-an-override
  (is (subscribers/same-value? 1 1.0))
  (is (subscribers/same-value? [1 0 0] [1.0 0.0 0.0]))
  (is (subscribers/same-value? [{:position 0 :color [1 0 0]}] [{:position 0.0 :color [1.0 0.0 0.0]}]))
  (is (not (subscribers/same-value? 1 1.01)))
  (is (not (subscribers/same-value? [1 0] [1 0 0]))))

(deftest a-ramp-whose-endpoints-cannot-interpolate-is-refused-and-the-owner-keeps-running
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent :params {:level 1.0}}}))]
        (at! rt 100 {:op :variable/set :variable :energy :value [0.0 0.0 0.0] :source agent})
        (is (= :accepted (:status (at! rt 200 {:op :param/ramp :event-id "bad" :target :variable :variable :energy
                                              :to 1.0 :over {:beats 4} :source agent})))
            "a variable's shape is only known when the ramp fires")
        (is (nil? (get-in @(:state rt) [:status :error])) "the owner frame completes")
        (is (= {:event-id "bad" :status :rejected :message "Ramp endpoints need equal shapes"}
               (select-keys (last (recent rt)) [:event-id :status :message])))
        (is (empty? (:upcoming (show/schedule rt))) "the refused event does not stay due")
        (is (= [0.0 0.0 0.0] (get-in @(:state rt) [:model :variables :energy])))
        (at! rt 400 {:op :param/set :instance-id id :param :level :value 0.5 :source {:adapter :repl :session-id "r"}})
        (is (<= 127 (dimmer (peek @packets)) 128) "later frames still render"))
      (finally (show/stop! rt))))
  (testing "a stored ramp that cannot be sampled is cancelled instead of failing every owner tick"
    (let [model (-> (live-model)
                    (assoc-in [:variables :energy] [0.0 0.0])
                    (assoc :event-subscribers
                           {:subscribers [] :runs [] :next-run 0 :seed 1 :dropped 0
                            :ramps [{:event-id "stored" :destination {:target :variable :variable :energy}
                                     :from [0.0 0.0] :to 1.0 :base [0.0 0.0] :curve :linear
                                     :unit :beats :start 0.0 :end 4.0}]}))
          settled (subscribers/advance-ramps model 0.0)]
      (is (empty? (get-in settled [:event-subscribers :ramps])))
      (is (= [["stored" :cancelled :invalid-endpoints]]
             (mapv (juxt :event-id :status :reason) (get-in settled [:event-subscribers :recent]))))
      (is (= {} (subscribers/frame-overlays settled 0.0))))))

(deftest a-cancelled-ramp-holds-its-current-value
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))
            _ (at! rt 250 {:op :param/ramp :event-id "up" :instance-id id :param :level :to 1.0
                           :over {:beats 8} :source agent})
            rising (mapv (fn [t] (show/tick! rt t) (dimmer (peek @packets))) (range 500 2250 250))
            stopped (at! rt 2250 {:op :schedule/cancel :event-ids ["up"] :source {:adapter :web :session-id "s"}})
            held (into [(dimmer (peek @packets))]
                       (map (fn [t] (show/tick! rt t) (dimmer (peek @packets)))) [2500 2750 3000])
            commits (filter #(= :param/set (get-in % [:command :op]))
                            (vals (get-in @(:state rt) [:model :control :history])))]
        (is (= 112 (peek rising)))
        (is (= ["up"] (:event-ids stopped)))
        (is (every? #(<= 127 % 128) held) (str "the output stays at the stop frame's value: " held))
        (is (== 0.5 (level rt id)) "the stop frame's value is committed, so backups and controls agree with the output")
        (is (= [[0.5 :agent]] (mapv (juxt (comp :value :command) (comp :adapter :source :command)) commits))
            "one commit through the shared reducer, with the ramp's source")
        (is (= {:event-id "up" :status :cancelled :reason :operator-cancel}
               (select-keys (last (recent rt)) [:event-id :status :reason])))
        (is (empty? (:ramps (show/schedule rt))))
        (testing "a JSON caller names a variable destination as text"
          (at! rt 3050 {:op :variable/set :variable :energy :value 0.0 :source agent})
          (at! rt 3100 {:op :param/ramp :event-id "rise" :target :variable :variable :energy :to 1.0
                        :over {:beats 8} :source agent})
          (is (= :rejected (:status (at! rt 3150 {:op :schedule/cancel :destination "energy" :source agent}))))
          (is (= ["rise"] (:event-ids (at! rt 3200 {:op :schedule/cancel :source agent
                                                    :destination {:target "variable" :variable "energy"}})))))
        (testing "a ramp stopped on its first frame has nothing to commit"
          (at! rt 3250 {:op :param/ramp :event-id "again" :instance-id id :param :level :to 1.0
                        :over {:beats 8} :source agent})
          (let [revision (get-in @(:state rt) [:model :revision])]
            (show/cancel-events! rt {:event-ids ["again"]})
            (show/tick! rt 3250)
            (is (== 0.5 (level rt id)))
            (is (= (inc revision) (get-in @(:state rt) [:model :revision])) "only the cancel itself is committed"))))
      (finally (show/stop! rt)))))

(deftest a-rejected-scheduled-set-leaves-the-running-ramp-alone
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))
            _ (at! rt 250 {:op :param/ramp :event-id "up" :instance-id id :param :level :to 1.0
                           :over {:beats 8} :source agent})
            ;; A cue selector is resolved when due, so the bad value is only caught then.
            posted (at! rt 1250 {:op :param/set :event-id "bad-set" :cue-id :wash :param :level :value 7.0
                                 :in {:beats 1} :source agent})
            frames (mapv (fn [t] (show/tick! rt t) (dimmer (peek @packets))) [1500 1750 2000 2250])]
        (is (= :accepted (:status posted)))
        (is (= [:rejected "Invalid parameters"]
               ((juxt :status :message) (last (filter #(= "bad-set" (:event-id %)) (recent rt))))))
        (is (= ["up"] (mapv :event-id (:ramps (show/schedule rt)))) "the ramp keeps running")
        (is (apply < frames) (str "the output keeps rising: " frames))
        (testing "an accepted scheduled set replaces the ramp and its value stays"
          (at! rt 2500 {:op :param/set :event-id "good-set" :cue-id :wash :param :level :value 0.25
                        :in {:beats 1} :source agent})
          (doseq [t [2750 3000 3250]] (show/tick! rt t))
          (is (empty? (:ramps (show/schedule rt))))
          (is (= {:event-id "up" :status :cancelled :reason :superseded :by "good-set"}
                 (select-keys (last (filter #(= "up" (:event-id %)) (recent rt))) [:event-id :status :reason :by])))
          (is (== 0.25 (level rt id)))
          (is (= 64 (dimmer (peek @packets))))))
      (finally (show/stop! rt)))))

(deftest same-beat-events-fire-in-admission-order-after-a-reschedule
  (let [{:keys [rt]} (harness)
        operator {:adapter :repl :session-id "fod"}]
    (try
      (show/tick! rt 0)
      (at! rt 100 {:op :schedule/post :source operator
                   :events [{:id "first" :op :variable/set :variable :section :value "first" :at {:beat 8}}
                            {:id "second" :op :variable/set :variable :section :value "second" :at {:beat 4}}]})
      (show/tick! rt 200)                                   ; the queue is now kept in due order
      (is (= :accepted (:status (at! rt 300 {:op :schedule/move :event-id "second" :at {:beat 8} :source operator}))))
      (is (= ["first" "second"] (mapv :event-id (:upcoming (show/schedule rt)))))
      (show/tick! rt 4100)
      (is (= ["first" "second"] (mapv :event-id (take-last 2 (recent rt))))
          "events fire in the order the projection lists them")
      (is (= "second" (get-in @(:state rt) [:model :variables :section])))
      (finally (show/stop! rt)))))

(deftest a-requester-without-an-identity-owns-no-scheduled-work
  (let [{:keys [rt]} (harness)
        pending #(mapv :event-id (:upcoming (show/schedule rt)))
        now #(inc (get-in @(:state rt) [:model :time-ms]))]
    (try
      (show/tick! rt 0)
      (at! rt 100 {:op :schedule/post :source {:adapter :repl :session-id "fod"}
                   :events [{:id "human" :op :cue/launch :cue-id :wash :in {:bars 8}}]})
      (at! rt 200 {:op :schedule/post :source agent
                   :events [{:id "planned" :op :cue/launch :cue-id :wash :in {:bars 8}}]})
      (doseq [requester [{:type :patch} {:session-id "agent-run"} {:adapter :agent} {:adapter :agent :session-id "other-run"}]]
        (let [moved (at! rt (now) {:op :schedule/move :event-id "planned" :in {:bars 1} :source requester})
              cancelled (at! rt (now) {:op :schedule/cancel :event-ids ["human" "planned"] :source requester})]
          (is (= :rejected (:status moved)) (pr-str requester))
          (is (= [] (:event-ids cancelled)) (pr-str requester))))
      (is (= ["human" "planned"] (pending)))
      (testing "a non-human requester naming another poster matches nothing, not its own events"
        (is (= [] (:event-ids (at! rt (now) {:op :schedule/cancel :posted-by {:adapter :repl} :source agent}))))
        (is (= ["human" "planned"] (pending))))
      (testing "the poster still reaches its own event, and an operator surface any event"
        (is (= ["planned"] (:event-ids (at! rt (now) {:op :schedule/cancel :event-ids ["human" "planned"] :source agent}))))
        (is (= ["human"] (:event-ids (at! rt (now) {:op :schedule/cancel :event-ids ["human"]
                                                    :source {:adapter :x-touch-xt :session-id "extender"}})))))
      (finally (show/stop! rt)))))

(deftest only-a-group-opacity-action-overrides-an-opacity-ramp
  (let [{:keys [rt packets]} (harness)
        operator {:adapter :repl :session-id "fod"}]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent :params {:level 1.0}}}))
            group (fn [time action] (at! rt time {:op :group/apply :selector {:instance-ids [id]} :action action
                                                  :source operator}))]
        (at! rt 250 {:op :param/ramp :event-id "fade" :instance-id id :target :opacity :to 0.0
                     :over {:beats 8} :source agent})
        (show/tick! rt 1000)
        (is (= 207 (dimmer (peek @packets))))
        (is (= :accepted (:status (group 1250 {:op :instance/gain-set :value 1.0}))))
        (is (= ["fade"] (mapv :event-id (:ramps (show/schedule rt)))) "a group gain change does not write opacity")
        (is (= 191 (dimmer (peek @packets))) "the fade continues")
        (is (= :accepted (:status (group 1500 {:op :instance/opacity-set :value 0.8}))))
        (is (empty? (:ramps (show/schedule rt))))
        (is (= {:event-id "fade" :status :cancelled :reason :human-override}
               (select-keys (last (recent rt)) [:event-id :status :reason])))
        (is (= 204 (dimmer (peek @packets))) "the operator's group opacity stays"))
      (finally (show/stop! rt)))))

(deftest a-gesture-takes-over-a-ramped-parameter-from-its-current-value
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))
            _ (at! rt 250 {:op :param/ramp :event-id "up" :instance-id id :param :level :to 1.0
                           :over {:beats 8} :source agent})
            rising (mapv (fn [t] (show/tick! rt t) (dimmer (peek @packets))) (range 500 2000 250))
            gesture {:instance-id id :param :level :control-id :encoder-1 :gesture-id :g1
                     :source {:adapter :push-3 :session-id "push"}}
            operator {:operator-id :front-of-house}
            revision #(get-in @(:state rt) [:model :revision])]
        (is (= 96 (peek rising)))
        (testing "a gesture refused for lack of an operator leaves the ramp and the parameter alone"
          (let [before (revision)
                refused (at! rt 2000 (assoc gesture :op :gesture/begin))]
            (is (= :rejected (:status refused)))
            (is (= before (revision)))
            (is (== 0.0 (level rt id)))
            (is (= ["up"] (mapv :event-id (:ramps (show/schedule rt)))))
            (is (= 112 (dimmer (peek @packets))))))
        (let [begun (at! rt 2250 (assoc gesture :op :gesture/begin) operator)
              touched (into [(dimmer (peek @packets))]
                            (map (fn [t] (show/tick! rt t) (dimmer (peek @packets)))) [2500 2750])]
          (is (= :accepted (:status begun)))
          (is (every? #(<= 127 % 128) touched)
              (str "touching the control does not return the output to the ramp's start value: " touched))
          (is (== 0.5 (level rt id)) "the controller reads the value the ramp had reached")
          (is (empty? (:ramps (show/schedule rt))))
          (is (= [:cancelled :human-override :gesture/begin]
                 ((juxt :status :reason (comp :op :by)) (last (recent rt)))))
          (is (= :accepted (:status (at! rt 3000 (assoc gesture :op :gesture/sample :sequence 1 :value 0.6) operator))))
          (is (= 153 (dimmer (peek @packets))))
          (testing "cancelling the gesture returns to the value it took over, not the ramp's start"
            (is (= :accepted (:status (at! rt 3250 (assoc gesture :op :gesture/cancel) operator))))
            (is (== 0.5 (level rt id))))))
      (finally (show/stop! rt)))))
