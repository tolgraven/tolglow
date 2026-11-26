(ns tolglow.studio.stem-routing-host-test
  "Named stems through the real show owner (#675): a multichannel capture,
  per-stem onsets launching a cue through an event subscriber, and a stem
  level driving a cue parameter through the signal patch, with a transport
  stop part-way. Injected PCM only; no device is opened."
  (:require [clojure.test :refer [deftest is testing]]
            [afterglow.control.model :as control]
            [tolglow.audio.stem-fixtures :as fixtures]
            [tolglow.connections :as connections]
            [tolglow.show :as show]
            [tolglow.stems :as stems]))

(def head {:id :head :position [0 0 0] :universe 1 :address 1 :channels {:dimmer 1 :red 2 :green 3 :blue 4}})

(def effects
  {:hit {:id :hit :version 1 :params {} :duration {:beats 1/4}
         :outputs {:dimmer {:op :const :value 1.0} :color {:op :const :value [1.0 1.0 1.0]}}}
   :wash {:id :wash :version 1 :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
          :outputs {:dimmer {:op :param :key :level} :color {:op :const :value [0.0 0.0 1.0]}}}})

(defn- model []
  (-> (control/new-show {:rig {:fixtures [head]}
                         :registry {:effects effects
                                    :cues {:hit {:id :hit :version 1 :effect-id :hit}
                                           :wash {:id :wash :version 1 :effect-id :wash}}}})
      (assoc :time-ms 0.0)
      (update :transport assoc :time-ms 0.0 :beat 0.0 :bpm 120.0 :playing? true)))

(defn- apply-at! [runtime type payload time]
  (let [receipt (show/submit! runtime type payload)]
    (show/tick! runtime time)
    (deref receipt 1000 ::missing)))

(defn- run-stems!
  "Play the four-stem render through capture, ticking the owner at every
  block start. transport! receives each block's start time first."
  [rendered transport!]
  (let [runtime-ref (atom nil) launches (atom []) levels (atom [])
        bytes (fixtures/pcm16 rendered)
        open! (fn [_] (fixtures/memory-source bytes 4 480
                                              (fn [frame]
                                                (let [t (/ frame 48.0) runtime @runtime-ref]
                                                  (when (pos? t)
                                                    (transport! runtime t)
                                                    (show/tick! runtime t)
                                                    (let [model (:model @(:state runtime))
                                                          wash (some #(when (= :wash (:cue-id %)) %) (vals (:instances model)))]
                                                      (swap! levels conj [t (get-in model [:transport :playing?])
                                                                          (get-in wash [:params :level])])))))))
        runtime (show/create-runtime {:model (model) :workspace-options nil :audio-options {:open-source! open!}})]
    (reset! runtime-ref runtime)
    (try
      (show/attach-observer! runtime :stem-launches
        (fn [] {:events! (fn [_ events _ _]
                          (swap! launches into (keep #(when (and (= :instance/launched (:type %))
                                                                 (= :hit (get-in % [:data :cue-id])))
                                                        (get-in % [:time :beat])) events)))
                :frame! (fn [_]) :tracing? (constantly false) :close! (fn [])}))
      ;; Route through the same REPL operations Studio's stem panel submits.
      (let [{:keys [receipt]} (stems/route-onset! runtime :kick :hit)
            {:keys [saved enabled]} (stems/route-level! runtime :guitar :wash :level)]
        (show/tick! runtime 0.0)
        (is (= [:accepted :accepted :accepted] (mapv #(:status (deref % 1000 ::missing)) [receipt saved enabled]))))
      (is (= :accepted (:status (apply-at! runtime :command {:op :cue/launch :cue-id :wash} 0.0))))
      ;; The injected stream reads faster than real time; keep the capture
      ;; clock past every sample it reads.
      (swap! (:clock runtime) assoc :time-ms 60000.0)
      (let [handle (connections/start-audio! (:connections runtime) (fixtures/capture-config rendered)
                                             (show/audio-callbacks runtime))
            result (deref (:result handle) 20000 ::timeout)]
        (is (= :completed (:status result)) (pr-str result)))
      (let [end (/ (:frames rendered) 48.0)]
        (doseq [t [(+ end 1) (+ end 2)]] (transport! runtime t) (show/tick! runtime t)))
      {:launches @launches :levels @levels :model (:model @(:state runtime)) :saved (show/export-show runtime)
       :view (stems/view runtime)
       :status (show/audio-action! runtime :status {})}
      (finally (show/stop! runtime)))))

(deftest kick-onsets-launch-a-cue-on-each-beat-and-a-guitar-level-drives-a-parameter
  (let [rendered (fixtures/render {:bpm 120.0 :bars 4})
        {:keys [launches levels model status saved view]} (run-stems! rendered (fn [_ _]))
        beats launches
        kicks (mapv :beat (get-in rendered [:onsets :kick]))]
    (is (= (count kicks) (count beats)) (pr-str beats))
    (testing "each launch follows its kick by less than an eighth of a beat (detection, one block and one frame)"
      (doseq [[kick beat] (map vector kicks beats)]
        (is (<= 0.0 (- beat kick) 0.125) [kick beat])))
    (testing "the stems are reported per channel, with counts matching the fixture"
      (is (= {:kick 16 :snare 8 :bass 16 :guitar 8}
             (update-vals (get-in status [:features :stems]) :onset-count)))
      (is (= [1 2 3 4] (mapv #(get-in status [:features :stems % :channel]) [:kick :snare :bass :guitar]))))
    (testing "a saved show keeps the named stems and the routes that read them"
      (is (= [[:kick 1 :kick] [:snare 2 :snare] [:bass 3 :bass] [:guitar 4 :guitar]]
             (mapv (juxt :id :channel :preset) (get-in saved [:model :integrations :audio-capture :config :stems]))))
      (is (= #{"stem/guitar/level"} (set (keys (get-in saved [:model :routing :spec :sources])))))
      (is (= :stem/kick (get-in (first (get-in saved [:model :event-subscribers :subscribers])) [:match :emitter-id]))))
    (testing "the REPL view lists each stem with its live counts and routes"
      (is (= [:kick :snare :bass :guitar] (mapv :id view)))
      (is (= 16 (:onset-count (first view))))
      (is (= [{:subscriber-id :stem-kick-hit :enabled? true
               :actions [{:op :cue/launch :select {:ids [:hit] :pick :first}}]}]
             (get-in (first view) [:routes :onsets])))
      (is (= [{:route-id :stem-guitar-level-wash-level :feature :level :cue-id :wash :param :level}]
             (get-in (last view) [:routes :ports]))))
    (testing "stem ports are learned by the patch bay"
      (is (every? #(contains? (get-in model [:routing :ports]) %)
                  ["stem/kick/level" "stem/kick/onset" "stem/guitar/level" "stem/bass/onset"])))
    (testing "the guitar phrase moves the wash level up and releases it in the silent bar"
      (let [during (keep (fn [[t _ level]] (when (< 600 t 2400) level)) levels)
            silent (keep (fn [[t _ level]] (when (< 3700 t 4400) level)) levels)]
        (is (< 0.5 (reduce max during)))
        ;; Neighbour bleed at -24 dB is all that remains in the silent bar.
        (is (< (reduce max silent) (* 0.5 (reduce max during))))))))

(deftest a-stopped-transport-drops-stem-onsets-holds-stem-routes-and-resumes-without-catch-up
  (let [rendered (fixtures/render {:bpm 120.0 :bars 4})
        stopped (atom false) played (atom false)
        transport! (fn [runtime t]
                     (when (and (>= t 2250.0) (not @stopped))
                       (reset! stopped true)
                       (show/submit! runtime :command {:op :transport/set :transport {:playing? false}}))
                     (when (and (>= t 4250.0) (not @played))
                       (reset! played true)
                       (show/submit! runtime :command {:op :transport/set :transport {:playing? true}})))
        {:keys [launches levels model]} (run-stems! rendered transport!)
        beats launches]
    ;; 16 kicks play; the four inside the 2 s stop launch nothing, and the
    ;; transport resumes at beat 4.5, so the rest land on beats 5–12.
    (is (= 12 (count beats)) (pr-str beats))
    (doseq [[expected beat] (map vector (range 1 13) beats)]
      (is (<= 0.0 (- beat expected) 0.125) [expected beat]))
    ;; Kick 4, snare 2 and bass 4 onsets fell inside the stop.
    (is (= 10 (get-in model [:variables :audio :suppressed-onsets])))
    (testing "the guitar-driven level holds through the stop and moves again after play"
      (let [held (keep (fn [[t playing? level]] (when (and (false? playing?) (< 2300 t 4200)) level)) levels)
            after (keep (fn [[t _ level]] (when (< 4300 t 5600) level)) levels)]
        (is (seq held))
        (is (apply = held) (pr-str (distinct held)))
        (is (< 0.0 (first held)))
        (is (not= (first held) (reduce max after)))))))
