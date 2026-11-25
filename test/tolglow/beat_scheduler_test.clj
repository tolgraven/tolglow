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
            [tolglow.show :as show]))

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
            projection (show/schedule rt)
            after (mapv (fn [t] (show/tick! rt t) (frame t)) (range 2500 6500 250))]
        {:id id :posted posted :tempo tempo :projection projection
         :frames (into [first-frame] (concat before [mid] after))
         :level (level rt id) :recent (recent rt) :ramps (get-in (show/snapshot rt) [:schedule :ramps])
         :history (get-in @(:state rt) [:model :control :history])
         :packets @packets})
      (finally (show/stop! rt)))))

(deftest param-ramp-matches-the-easing-curve-across-a-mid-ramp-tempo-change
  (let [{:keys [posted tempo projection frames level recent ramps packets id history]} (ramp-scenario)
        expected (fn [b] (* 255.0 (easing/sample :ease-in-out (/ (- b 0.5) 8.0))))]
    (is (= :accepted (:status posted)))
    (is (= ["swell"] (:event-ids posted)))
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
        (is (= 0.5 (:progress ramp)))))
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
                       2 {:op "master/set" :value 0.8 :at at}
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
        (is (= (take 16 (cycle [:cue :ramp :master :variable])) (map :category upcoming)))
        (is (= (mapv #(double (+ 4 %)) (range 16)) (mapv :due-beat upcoming)))
        (is (every? #(= {:adapter :agent :session-id "agent-run"} (:source %)) upcoming))
        (is (= {:target :param :param :level :to 0.5 :over {:beats 2} :curve :sine
                :select {:ids [:wash] :pick :first}}
               (select-keys (second upcoming) [:target :param :to :over :curve :select])))
        (is (= [4.0 "e0" :cue/launch "Event 0"] (first (:upcoming compact))))
        (is (= 16 (count (:upcoming compact))))
        (testing "REPL and Studio read one projection"
          (is (= (show/schedule rt) (:schedule (show/snapshot rt)))))
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
            _ (at! rt 250 {:op :schedule/post :source agent
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
        (is (= (map #(select-keys % [:event-id :from :to :curve :start :end :progress]) (:ramps (subscribers/schedule-view model)))
               (map #(select-keys % [:event-id :from :to :curve :start :end :progress]) (:ramps s))))
        (let [tint (get-in model [:instances id :params :tint])]
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
      (at! rt 100 {:op :schedule/post :source agent
                   :events [{:id "fade" :op :param/ramp :target :master :to 0.0 :over {:ms 1000}}
                            {:id "count" :op :param/ramp :target :variable :variable :energy :from-note "x"}]})
      (is (empty? (:upcoming (show/schedule rt))) "an unknown option rejects the batch")
      (at! rt 100 {:op :schedule/post :source agent
                   :events [{:id "fade" :op :param/ramp :target :master :to 0.0 :over {:ms 1000}}
                            {:id "count" :op :variable/set :variable :energy :value 0.0}
                            {:id "rise" :op :param/ramp :target :variable :variable :energy :to 1.0
                             :over {:bars 1} :in {:beats 1}}]})
      (show/tick! rt 600)
      (is (= 0.5 (:master (:model @(:state rt)))) "a millisecond ramp is an explicit boundary unit")
      (is (<= 126 (dimmer (peek @packets)) 129))
      (is (= 0.0 (energy)) "the bar-long variable ramp starts at beat 1.2")
      (show/tick! rt 1100)
      (is (= 0.0 (:master (:model @(:state rt)))))
      (is (= 0 (dimmer (peek @packets))))
      (show/tick! rt 1600)
      (is (< (Math/abs (- 0.5 (energy))) 1e-9) "beat 3.2 is halfway through the four-beat ramp")
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
    (is (= s (subscribers/validate-state s)))))
