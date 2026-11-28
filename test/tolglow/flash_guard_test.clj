(ns tolglow.flash-guard-test
  "The flash guard (#836) on the real show owner with captured DMX. The
  showcase colour strobe at four flashes a beat is 12 Hz at 180 BPM. Flashes
  are counted from the captured packets with an independent decoder: the
  mean lit level of the strobe's heads crossing half of its peak."
  (:require [afterglow.render.frame :as render]
            [clojure.test :refer [deftest is testing]]
            [tolglow.flash-guard :as guard]
            [tolglow.registry :as registry]
            [tolglow.show :as show]
            [tolglow.showcase :as showcase]
            [tolglow.showcase.rig :as rig]))

(def ^:private agent-source {:adapter :agent :session-id "conversation-1" :run-id "run-1"})

(def ^:private push-source {:adapter :push-2 :session-id "push"})

(def ^:private frame-ms 25)

(defn- capture-sink [frames]
  {:description {:type :capture :network false}
   :send! (fn [universes _]
            (swap! frames conj (into (sorted-map) (map (fn [[universe data]] [universe (mapv #(bit-and 255 %) data)]))
                                     universes)))
   :close! (fn [])})

(defn- showcase-model [settings bpm extra-effects]
  (cond-> (-> (showcase/model)
              (assoc-in [:transport :bpm] (double bpm))
              (update :registry (fn [registry]
                                  (reduce (fn [r effect]
                                            (-> r (assoc-in [:effects (:id effect)] effect)
                                                (assoc-in [:cues (:id effect)] {:id (:id effect) :version 1 :effect-id (:id effect)
                                                                                :priority 80})))
                                          registry extra-effects))))
    settings (assoc-in [:settings :flash-guard] settings)))

(defn- session
  "Tick a showcase show every 25 ms until `end-ms`. `launches` are submitted
  before the first tick, a vector as [type payload]; `commands` maps a tick time to payloads submitted
  just before it. Returns captured frames, one guard report per tick, the
  transport beat per tick and every event the show emitted."
  [{:keys [settings launches commands end-ms bpm extra-effects tools]
    :or {end-ms 4000 bpm 180 extra-effects []}}]
  (let [frames (atom [])
        seen (atom [])
        runtime (show/create-runtime {:model (showcase-model settings bpm extra-effects) :workspace-options nil
                                      :worker-timeout-ms 60000 :sink (capture-sink frames)
                                      :event-admit (fn [m xs] (swap! seen into xs) {:model m :deliveries []})})]
    (try
      (let [acks (mapv #(if (vector? %) (apply show/submit! runtime %) (show/submit! runtime %)) launches)
            ;; Agent tool calls enter through the same boundary Studio chat
            ;; and rehearsals use. Each waits for the owner, which the ticks
            ;; below drive. A call queued after the first tick lands a frame later.
            tool-results (mapv (fn [[tool arguments]]
                                 (future (show/dispatch-tool! runtime tool arguments
                                                              {:command-id (str (random-uuid)) :run-id "agent-run"})))
                               tools)
            _ (Thread/sleep (if (seq tools) 200 0))
            rows (mapv (fn [t]
                         (doseq [payload (get commands t)] (show/submit! runtime payload))
                         (show/tick! runtime t)
                         (let [snapshot (show/snapshot runtime)]
                           (when-let [fault (some :fault (:instances snapshot))]
                             (throw (ex-info "A cue faulted" {:fault fault})))
                           {:t t :beat (get-in snapshot [:transport :beat])
                            :guard (get-in snapshot [:output :flash-guard])}))
                       (range 0 end-ms frame-ms))]
        {:frames @frames :rows rows :events @seen :acks (mapv #(deref % 1000 :timeout) acks) :tool-results (mapv #(deref % 5000 :timeout) tool-results)
         :instances (get-in @(:state runtime) [:model :instances])})
      (finally (show/stop! runtime)))))

(def ^:private patch (delay (render/compile-patch (get-in (showcase/model) [:rig :fixtures]))))

(def ^:private emission-attributes (conj registry/emitter-attributes :dimmer))

(defn- byte-at [frame [u i]] (nth (get frame u) i 0))

(defn- lit-level
  "Mean over `heads` of dimmer times brightest emitter, from DMX bytes only."
  [heads frame]
  (/ (reduce + (for [id heads
                     :let [channels (get-in @patch [:by-id id :compiled-channels])
                           level #(/ (byte-at frame (:coarse (get channels %))) 255.0)
                           emitters (filter channels (disj emission-attributes :dimmer))]]
                 (* (if (:dimmer channels) (level :dimmer) 1.0)
                    (reduce max 0.0 (map level emitters)))))
     (double (count heads))))

(defn- rise-times
  "Times (ms, frame index * 25) at which the lit level crosses `threshold` upward."
  [levels threshold]
  (keep (fn [[i [a b]]] (when (and (<= a threshold) (> b threshold)) (* frame-ms (inc i))))
        (map-indexed vector (partition 2 1 levels))))

(defn- peak-rate
  "Most rises inside any one-second window (t - 1000, t]."
  [times]
  (reduce max 0 (map (fn [t] (count (filter #(< (- t 1000) % (inc t)) times))) times)))

(defn- strobe-rises [frames heads]
  (let [levels (mapv #(lit-level heads %) frames)
        peak (reduce max 0.0 levels)]
    (rise-times levels (* 0.5 peak))))

(defn- guard-events
  "Data of each :lighting/flash-guard event the show emitted."
  [events]
  (keep #(when (= :lighting/flash-guard (:type %)) (:data %)) events))

(defn- strobe-launch [source params]
  {:op :cue/launch :cue-id :strobe :source source
   :launch {:mode :persistent :params (merge {:rate 4.0} params)}})

(defn- emission-addresses []
  (into #{} (for [fixture (:fixtures @patch)
                  [attribute channel] (:compiled-channels fixture)
                  :when (emission-attributes attribute)
                  address [(:coarse channel) (:fine channel)]
                  :when address]
              address)))

(defn- differing-addresses [as bs]
  (into #{} (for [[a b] (map vector as bs)
                  u (keys a)
                  i (range (count (get a u)))
                  :when (not= (nth (get a u) i) (nth (get b u) i))]
              [u i])))

;; ------------------------------------------------------------------ strobe

(def ^:private human-run (delay (session {:launches [(strobe-launch push-source {})]})))

(def ^:private agent-run (delay (session {:launches [(strobe-launch agent-source {})]})))

(deftest a-12-hz-agent-strobe-is-held-to-the-club-limit
  (let [{:keys [frames rows events instances]} @agent-run
        rises (strobe-rises frames rig/coloured)
        instance-id (key (first instances))]
    (is (= :agent (get-in (val (first instances)) [:source :adapter])) "the launch keeps its agent source")
    (is (<= (peak-rate rises) 4) "no one-second window of captured output holds more than four flashes")
    (is (>= (count rises) 10) "the strobe still flashes, at the limit")
    (testing "the guard says so in the projection and as an event"
      (is (some #(get-in % [:guard :limiting?]) rows))
      (is (every? #(= 4.0 (get-in % [:guard :limit-hz])) (filter :guard rows)))
      (let [limiting (filter #(= :limiting (:status %)) (guard-events events))]
        (is (seq limiting))
        (is (every? #(= :flash-guard (:reason %)) limiting))
        (is (every? #(= [instance-id] (:instance-ids %)) limiting))
        (is (= [:luminance] (:reasons (first limiting))))))))

(deftest the-same-strobe-from-push-is-delivered-unchanged
  (let [human (:frames @human-run)
        off (:frames (session {:settings {:enabled? false} :launches [(strobe-launch push-source {})]}))]
    (is (>= (peak-rate (strobe-rises human rig/coloured)) 11) "a human strobe flashes at 12 Hz")
    (is (= off human) "every captured byte matches a show without the guard")
    (is (not-any? #(get-in % [:guard :limiting?]) (:rows @human-run)))))

(deftest the-whole-show-switch-limits-human-strobe-too
  (let [{:keys [frames rows]} (session {:settings {:scope :show} :launches [(strobe-launch push-source {})]})]
    (is (<= (peak-rate (strobe-rises frames rig/coloured)) 4))
    (is (some #(get-in % [:guard :limiting?]) rows))
    (is (= :show (some #(get-in % [:guard :scope]) rows)))))

(deftest the-broadcast-preset-holds-three-flashes-a-second
  (let [{:keys [frames]} (session {:settings {:preset :broadcast} :launches [(strobe-launch agent-source {})]})]
    (is (<= (peak-rate (strobe-rises frames rig/coloured)) 3))))

(deftest only-emission-changes-and-motion-passes-unclamped
  (testing "an agent strobe with a fast agent aim sweep differs from the human run on emission channels only"
    (let [aim {:op :cue/launch :cue-id :aim-lfo :launch {:mode :persistent}}
          human (:frames (session {:end-ms 2000 :launches [(strobe-launch push-source {}) (assoc aim :source push-source)]}))
          agent (:frames (session {:end-ms 2000 :launches [(strobe-launch agent-source {}) (assoc aim :source agent-source)]}))
          changed (differing-addresses human agent)]
      (is (seq changed) "the guard held something")
      (is (every? (emission-addresses) changed)
          "pan, tilt, shutter, motor speed and every other non-emission byte matches"))))

(deftest guarded-output-is-deterministic
  (is (= (:frames @agent-run) (:frames (session {:launches [(strobe-launch agent-source {})]})))))

;; Enforcement sits in the owner's frame path, keyed on the instance's
;; recorded source, not in an orchestrator or one tool path (#879 review).
(deftest a-direct-agent-tool-call-is-limited-by-the-owner
  (testing "launch_cue through dispatch-tool!, the boundary Studio chat and rehearsals use"
    (let [{:keys [frames instances tool-results rows]}
          (session {:tools [["launch_cue" {:cue-id "strobe" :launch {:mode "persistent" :params {:rate 4.0}}}]]})]
      (is (= :accepted (:status (first tool-results))))
      (is (= :agent (get-in (val (first instances)) [:source :adapter])))
      (is (<= (peak-rate (strobe-rises frames rig/coloured)) 4))
      (is (some #(get-in % [:guard :limiting?]) rows))))
  (testing "a cue launch the agent scheduled on the beat clock keeps its agent source"
    (let [{:keys [frames instances tool-results]}
          (session {:tools [["schedule_events" {:events [{:id "hit" :op "cue/launch" :cue-id "strobe" :at {:beat 1}}]}]]})]
      (is (= :accepted (:status (first tool-results))))
      (is (= :agent (get-in (val (first instances)) [:source :adapter])))
      (is (<= (peak-rate (strobe-rises frames rig/coloured)) 4)))))

;; ------------------------------------------------------------------ grant

(deftest an-operator-grant-allows-a-bounded-faster-burst
  (let [settings {:grant {:limit-hz 12.0 :burst-beats 6}}
        ;; Granted from Studio, so the show stamps it with the running epoch.
        {:keys [frames rows acks]} (session {:end-ms 6000
                                             :launches [[:settings {:scope :flash-guard :values settings
                                                                    :source {:adapter :studio :session-id "studio"}}]
                                                        (strobe-launch agent-source {})]})
        rises (strobe-rises frames rig/coloured)
        ;; At 180 BPM six beats are two seconds; the cooldown is as long again.
        early (filter #(< % 2000) rises)
        cooling (filter #(< 3200 % 4000) rises)]
    (is (= :accepted (:status (first acks))))
    (is (>= (peak-rate early) 11) "inside the burst the strobe runs at the granted rate")
    (is (<= (count cooling) 4) "after the burst it is held to the base limit")
    (is (some #(= 12.0 (get-in % [:guard :limit-hz])) rows))
    (is (some #(= 4.0 (get-in % [:guard :limit-hz])) (filter #(< 3000 (:t %)) rows)))
    (testing "a grant lapses when the show restarts"
      (let [model (assoc-in (showcase/model) [:settings :flash-guard]
                            (guard/edit-settings nil settings 3))]
        (is (= 3 (get-in model [:settings :flash-guard :grant :epoch])))
        (is (some? (:grant (guard/config (assoc model :epoch 3)))))
        (is (nil? (:grant (guard/config (assoc model :epoch 4)))))))))

;; ------------------------------------------------------------------ area, colour, strobe channel

(deftest a-small-area-pixel-chase-above-the-limit-passes
  (let [strobe (get-in (showcase/model) [:registry :effects :strobe])
        tiny (-> strobe
                 (assoc :id :tiny-strobe :name "Tiny strobe" :targets (vec (take 8 rig/tubes)))
                 (update :outputs #(filterv (comp #{:color} :attribute) %)))
        launch {:op :cue/launch :cue-id :tiny-strobe :launch {:mode :persistent :params {:rate 4.0}}}
        agent (session {:extra-effects [tiny] :launches [(assoc launch :source agent-source)]})
        human (session {:extra-effects [tiny] :launches [(assoc launch :source push-source)]})]
    (is (>= (peak-rate (strobe-rises (:frames agent) (take 8 rig/tubes))) 11))
    (is (= (:frames human) (:frames agent)) "eight pixels are well under a quarter of the rig's light")))

(deftest saturated-red-is-held-to-three-flashes-a-second
  (let [params {:rate 1.25 :tint [1.0 0.0 0.0] :lightness 0.0}
        red (session {:launches [(strobe-launch agent-source params)]})
        white (session {:launches [(strobe-launch agent-source (assoc params :tint [1.0 1.0 1.0]))]})]
    (testing "3.75 Hz is under the 4 Hz luminance limit"
      (is (>= (peak-rate (strobe-rises (:frames white) rig/coloured)) 4))
      (is (not-any? #(get-in % [:guard :limiting?]) (:rows white))))
    (testing "but over the 3 Hz saturated red limit"
      (is (<= (peak-rate (strobe-rises (:frames red) rig/coloured)) 3))
      (is (some #(= [:saturated-red] (:reasons %)) (guard-events (:events red)))))))

(deftest a-fast-strobe-channel-loses-its-light-but-keeps-its-channel
  (let [heads (into rig/washes rig/movers)
        effect {:id :channel-strobe :version 1 :name "Channel strobe" :targets heads :duration {:bars 64}
                :outputs {:dimmer 1.0 :color [1.0 1.0 1.0] :white 1.0 :strobe 0.5}}
        launch {:op :cue/launch :cue-id :channel-strobe :launch {:mode :persistent}}
        ;; Washes and movers hold about a tenth of the showcase rig's light, and
        ;; their RGBW policy turns white into the white emitter alone, about a
        ;; quarter of each head's full output: lower the area to cover them.
        settings {:area 0.02}
        agent (session {:settings settings :end-ms 500 :extra-effects [effect] :launches [(assoc launch :source agent-source)]})
        human (session {:settings settings :end-ms 500 :extra-effects [effect] :launches [(assoc launch :source push-source)]})
        last-frame (comp last :frames)
        channel (fn [run id attribute] (byte-at (last-frame run) (get-in @patch [:by-id id :compiled-channels attribute :coarse])))]
    (doseq [id heads]
      (is (= 128 (channel agent id :strobe) (channel human id :strobe)) "the strobe channel itself is untouched")
      (is (pos? (channel human id :dimmer)))
      (is (zero? (channel agent id :dimmer)) "an undeclared strobe rate counts as faster than the limit"))
    (is (some #(= [:strobe-channel] (:reasons %)) (guard-events (:events agent))))))

(deftest strobe-rates-follow-declared-spans
  (let [shutter (get-in @patch [:by-id :beam-1 :compiled-channels :shutter])
        frame (fn [value] {(first (:coarse shutter)) (doto (byte-array 512) (aset-byte (second (:coarse shutter)) (unchecked-byte value)))})]
    (is (nil? (guard/strobe-rate shutter (frame 10))) "shutter open")
    (is (< 0.19 (guard/strobe-rate shutter (frame 16)) 0.21) "slowest strobe step")
    (is (< 9.9 (guard/strobe-rate shutter (frame 131)) 10.1) "fastest strobe step")
    (is (= ##Inf (guard/strobe-rate shutter (frame 150))) "a pulse strobe without a declared rate")))

;; ------------------------------------------------------------------ transport and settings

(deftest a-stopped-transport-holds-the-window
  (let [{:keys [frames rows]} (session {:end-ms 4000 :launches [(strobe-launch agent-source {})]
                                        :commands {1000 [{:op :transport/set :playing? false}]
                                                   3000 [{:op :transport/set :playing? true}]}})
        levels (mapv #(lit-level rig/coloured %) frames)
        peak (reduce max levels)
        ;; Show time stands still while stopped: measure windows on it.
        show-times (reductions + 0 (map (fn [[a b]] (if (= (:beat a) (:beat b)) 0 frame-ms)) (partition 2 1 rows)))
        rises (keep (fn [[t [a b]]] (when (and (<= a (* 0.5 peak)) (> b (* 0.5 peak))) t))
                    (map vector (rest show-times) (partition 2 1 levels)))]
    (is (<= (peak-rate rises) 4) "resuming does not start a fresh window of flashes")))

(deftest only-an-operator-can-change-the-guard
  (let [runtime (show/create-runtime {:model (showcase-model nil 128 []) :workspace-options nil})]
    (try
      (let [settle! (fn [ack] (show/tick! runtime 0.0) (deref ack 2000 {:status :timeout}))
            agent (settle! (show/submit! runtime :settings {:scope :flash-guard :values {:enabled? false} :source agent-source}))
            studio (settle! (show/submit! runtime :settings {:scope :flash-guard :values {:scope :show}
                                                              :source {:adapter :studio :session-id "studio"}}))
            bad (settle! (show/submit! runtime :settings {:scope :flash-guard :values {:limit-hz 400}}))]
        (is (= :rejected (:status agent)))
        (is (= :accepted (:status studio)))
        (is (= :rejected (:status bad)))
        (is (= {:scope :show} (get-in (show/snapshot runtime) [:settings :flash-guard]))))
      (finally (show/stop! runtime)))))

(deftest settings-validate-and-resolve-presets
  (is (= {:limit-hz 4.0 :red-limit-hz 3.0} (select-keys (guard/config {}) [:limit-hz :red-limit-hz])))
  (is (= 3.0 (:limit-hz (guard/config {:settings {:flash-guard {:preset :broadcast}}}))))
  (is (= 5.0 (:limit-hz (guard/config {:settings {:flash-guard {:limit-hz 5.0}}}))))
  (is (nil? (guard/config {:settings {:flash-guard {:enabled? false}}})))
  (is (= #{:agent} (:sources (guard/config {}))))
  (doseq [bad [{:scope :room} {:preset :festival} {:area 0} {:limit-hz 0} {:grant {:limit-hz 10}}
               {:sources #{:agent}} {:unknown true}]]
    (is (thrown? clojure.lang.ExceptionInfo (guard/settings bad)) (pr-str bad))))
