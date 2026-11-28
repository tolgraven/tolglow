(ns tolglow.flash-guard-test
  "The flash guard (#836) on the real show owner with captured DMX. The
  showcase colour strobe at four flashes a beat is 12 Hz at 180 BPM. Flashes
  are counted from the captured packets by two decoders written here, apart
  from the guard: the mean lit level of the strobe's heads crossing half its
  peak, and Harding-style transitions of the whole rig's area-weighted level."
  (:require [afterglow.control.model :as control-model]
            [afterglow.render.frame :as render]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.flash-guard :as guard]
            [tolglow.flash-guard.influence :as influence]
            [tolglow.lighting-capacity :as capacity]
            [tolglow.registry :as registry]
            [tolglow.show :as show]
            [tolglow.showcase :as showcase]
            [tolglow.showcase.rig :as rig]))

(def ^:private agent-source {:adapter :agent :session-id "conversation-1" :run-id "run-1"})

(def ^:private push-source {:adapter :push-2 :session-id "push"})

(def ^:private studio-source {:adapter :studio :session-id "studio"})

(def ^:private frame-ms 25)

(defn- capture-sink [frames]
  {:description {:type :capture :network false}
   :send! (fn [universes _]
            (swap! frames conj (into (sorted-map) (map (fn [[universe data]] [universe (mapv #(bit-and 255 %) data)]))
                                     universes)))
   :close! (fn [])})

(defn- with-effects [model effects]
  (update model :registry
          (fn [registry]
            (reduce (fn [r effect]
                      (-> r (assoc-in [:effects (:id effect)] effect)
                          (assoc-in [:cues (:id effect)] {:id (:id effect) :version 1 :effect-id (:id effect) :priority 80})))
                    registry effects))))

(defn- showcase-model [bpm extra-effects]
  (-> (showcase/model) (assoc-in [:transport :bpm] (double bpm)) (with-effects extra-effects)))

(defn- submit-item!
  "A command map, a [type payload] vector, {:tool name :args (fn [instance-ids])}
  for an agent tool call through dispatch-tool!, or (fn [runtime])."
  [runtime item]
  (cond
    (fn? item) (item runtime)
    (vector? item) (apply show/submit! runtime item)
    (:tool item) (let [ids (sort (keys (get-in @(:state runtime) [:model :instances])))]
                   (future (show/dispatch-tool! runtime (:tool item) ((:args item) ids)
                                                {:command-id (str (random-uuid)) :run-id "agent-run"})))
    :else (show/submit! runtime item)))

(defn- session
  "Tick a show every 25 ms until `end-ms`. `settings` go in as an operator
  command before anything else; `launches` are submitted before the first
  tick and `commands` maps a tick time to items submitted just before it.
  Returns captured frames, one guard report and beat per tick, every event
  and the replies."
  [{:keys [settings launches commands end-ms bpm extra-effects model]
    :or {end-ms 4000 bpm 180 extra-effects []}}]
  (let [frames (atom [])
        seen (atom [])
        runtime (show/create-runtime {:model (or model (showcase-model bpm extra-effects)) :workspace-options nil
                                      :worker-timeout-ms 60000 :sink (capture-sink frames)
                                      :event-admit (fn [m xs] (swap! seen into xs) {:model m :deliveries []})})
        replies (atom [])]
    (try
      (when settings
        (swap! replies conj (show/submit! runtime :settings {:scope :flash-guard :values settings :source studio-source})))
      (doseq [item launches] (swap! replies conj (submit-item! runtime item)))
      (when (some :tool launches) (Thread/sleep 200))
      (let [rows (mapv (fn [t]
                         (let [items (get commands t)]
                           (doseq [item items] (swap! replies conj (submit-item! runtime item)))
                           (when (some :tool items) (Thread/sleep 100)))
                         (show/tick! runtime t)
                         (let [snapshot (show/snapshot runtime)]
                           (when-let [fault (some :fault (:instances snapshot))]
                             (throw (ex-info "A cue faulted" {:fault fault})))
                           {:t t :beat (get-in snapshot [:transport :beat])
                            :guard (get-in snapshot [:output :flash-guard])}))
                       (range 0 end-ms frame-ms))]
        {:frames @frames :rows rows :events @seen
         :replies (mapv #(if (instance? clojure.lang.IDeref %) (deref % 5000 :timeout) %) @replies)
         :model (:model @(:state runtime))
         :instances (get-in @(:state runtime) [:model :instances])})
      (finally (show/stop! runtime)))))

(def ^:private patch (delay (render/compile-patch (get-in (showcase/model) [:rig :fixtures]))))

(def ^:private emission-attributes (conj registry/emitter-attributes :dimmer))

(defn- byte-at [frame [u i]] (nth (get frame u) i 0))

(defn- head-level
  "Dimmer times brightest emitter, from DMX bytes only."
  [the-patch frame id]
  (let [channels (get-in the-patch [:by-id id :compiled-channels])
        level #(/ (byte-at frame (:coarse (get channels %))) 255.0)
        emitters (filter channels (disj emission-attributes :dimmer :uv))]
    (* (if (:dimmer channels) (level :dimmer) 1.0) (reduce max 0.0 (map level emitters)))))

(defn- lit-level [heads frame]
  (/ (reduce + (map #(head-level @patch frame %) heads)) (double (count heads))))

(defn- rise-times
  "Times (ms, frame index * 25) at which the level crosses `threshold` upward."
  [levels threshold]
  (keep (fn [[i [a b]]] (when (and (<= a threshold) (> b threshold)) (* frame-ms (inc i))))
        (map-indexed vector (partition 2 1 levels))))

(defn- peak-rate
  "Most events inside any one-second window (t - 1000, t]."
  [times]
  (reduce max 0 (map (fn [t] (count (filter #(< (- t 1000) % (inc t)) times))) times)))

(defn- strobe-rises [frames heads]
  (let [levels (mapv #(lit-level heads %) frames)
        peak (reduce max 0.0 levels)]
    (if (pos? peak) (rise-times levels (* 0.5 peak)) [])))

(defn- area-weights
  "Each head's share of the rig, by the rule the guard documents, computed
  here independently: the larger of luminous share and visual share (one
  unit per fixture or strip, two for strobe-capable fixtures and beams)."
  [the-patch]
  (let [meter (capacity/prepare the-patch)
        total (:relative-capacity meter)
        heads (keys (:writers-by-head meter))
        lum #(/ (get-in meter [:source-capacities % :relative-capacity] 0.0) total)
        group-of #(let [f (get-in the-patch [:by-id %])] (or (:pixel-layout-id f) (:fixture-id f) %))
        strobe? (fn [id] (some (fn [[attribute channel]]
                                 (or (some #(str/includes? (name (:type %)) "strobe") (:functions channel))
                                     (and (= :strobe attribute) (empty? (:functions channel)))))
                               (get-in the-patch [:by-id id :compiled-channels])))
        lit (filter #(pos? (lum %)) heads)
        groups (group-by group-of lit)
        units (into {} (for [[g ids] groups]
                         (let [f (get-in the-patch [:by-id (first ids)])]
                           [g (if (or (some strobe? ids) (some-> (:beam-angle f) (<= 15))) 2.0 1.0)])))
        unit-total (reduce + (vals units))]
    (into {} (for [[g ids] groups id ids]
               [id (max (lum id) (/ (units g) unit-total (count ids)))]))))

(defn- harding-flashes
  "Times of Harding-style flashes in the whole rig's area-weighted level: a
  rise of at least `area` since the last trough, after a fall of at least
  half of it."
  ([frames] (harding-flashes frames @patch 0.25))
  ([frames the-patch area]
   (let [weights (area-weights the-patch)
         levels (mapv (fn [f] (reduce + (map (fn [[id w]] (* w (head-level the-patch f id))) weights))) frames)]
     (loop [i 0 trough (first levels) peak (first levels) armed? true out []]
       (if (= i (count levels)) out
         (let [l (nth levels i)]
           (if armed?
             (if (>= (- l trough) area)
               (recur (inc i) trough l false (conj out (* frame-ms i)))
               (recur (inc i) (min trough l) peak true out))
             (let [peak (max peak l)]
               (if (>= (- peak l) (/ area 2))
                 (recur (inc i) l peak true out)
                 (recur (inc i) trough peak false out))))))))))

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

(defn- head-addresses [ids]
  (into #{} (for [id ids
                  [_ channel] (get-in @patch [:by-id id :compiled-channels])
                  address [(:coarse channel) (:fine channel)]
                  :when address]
              address)))

(defn- differing-addresses [as bs]
  (into #{} (for [[a b] (map vector as bs)
                  u (keys a)
                  i (range (count (get a u)))
                  :when (not= (nth (get a u) i) (nth (get b u) i))]
              [u i])))

(defn- after [run ms] (update run :frames #(vec (drop (quot ms frame-ms) %))))

(def ^:private strobe-data (delay (get-in (showcase/model) [:registry :effects :strobe])))

(defn- sawtooth
  "The showcase strobe with its gate replaced by a ramp up that snaps off
  once a period, or the reverse."
  [id reversed?]
  (let [s @strobe-data
        color (first (filter #(= :color (:attribute %)) (:outputs s)))
        phase {:op :fract :args [{:op :multiply :args [{:op :beat :clock :absolute} {:op :param :key :rate}]}]}
        ramp (if reversed? {:op :subtract :args [{:op :const :value 1.0} phase]} phase)
        value {:op :clamp :args [{:op :multiply :args [{:op :mix :args [{:op :param :key :tint} {:op :const :value [1.0 1.0 1.0]}
                                                                        {:op :param :key :lightness}]}
                                                    ramp {:op :param :key :level}]}
                                 {:op :const :value 0.0} {:op :const :value 1.0}]}]
    (assoc s :id id :name "Sawtooth" :outputs (mapv #(if (= :color (:attribute %)) (assoc color :value value) %) (:outputs s)))))

;; ------------------------------------------------------------------ strobe

(def ^:private human-run (delay (session {:launches [(strobe-launch push-source {})]})))

(def ^:private agent-run (delay (session {:launches [(strobe-launch agent-source {})]})))

(deftest a-12-hz-agent-strobe-is-held-to-three-flashes-a-second
  (let [{:keys [frames rows events instances]} @agent-run
        rises (strobe-rises frames rig/coloured)
        instance-id (key (first instances))]
    (is (= :agent (get-in (val (first instances)) [:source :adapter])) "the launch keeps its agent source")
    (is (<= (peak-rate rises) 3) "no one-second window of captured output holds more than three flashes")
    (is (<= (peak-rate (harding-flashes frames)) 3) "nor by the area-weighted transition count")
    (is (>= (count rises) 8) "the strobe still flashes, at the limit")
    (testing "the guard says so in the projection and as an event"
      (is (some #(get-in % [:guard :limiting?]) rows))
      (is (every? #(= 3.0 (get-in % [:guard :limit-hz])) (filter :guard rows)))
      (let [limiting (filter #(= :limiting (:status %)) (guard-events events))]
        (is (seq limiting))
        (is (every? #(= :flash-guard (:reason %)) limiting))
        (is (every? #(= [instance-id] (:instance-ids %)) limiting))
        (is (= [:luminance] (:reasons (first limiting))))))))

(deftest the-same-strobe-from-push-is-delivered-unchanged
  (let [human (:frames @human-run)
        off (:frames (session {:settings {:enabled? false} :launches [(strobe-launch push-source {})]}))]
    (is (>= (peak-rate (strobe-rises human rig/coloured)) 11) "a human strobe flashes at 12 Hz")
    (is (= off human) "every captured byte matches a show with the guard off")
    (is (not-any? #(get-in % [:guard :limiting?]) (:rows @human-run)))))

(deftest the-whole-show-switch-limits-human-strobe-too
  (let [{:keys [frames rows]} (session {:settings {:scope :show} :launches [(strobe-launch push-source {})]})]
    (is (<= (peak-rate (strobe-rises frames rig/coloured)) 3))
    (is (some #(get-in % [:guard :limiting?]) rows))
    (is (= :show (some #(get-in % [:guard :scope]) rows)))))

(deftest the-club-preset-is-an-opt-in-four-hertz
  (let [{:keys [frames rows]} (session {:settings {:preset :club} :launches [(strobe-launch agent-source {})]})]
    (is (<= (peak-rate (strobe-rises frames rig/coloured)) 4))
    (is (every? #(= 4.0 (get-in % [:guard :limit-hz])) (filter :guard rows)))))

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

;; ------------------------------------------------------------------ evasion

(deftest ramps-are-counted-like-steps
  (doseq [reversed? [false true] level [0.45 0.6 1.0]]
    (testing (str (if reversed? "reversed " "") "sawtooth at 6 Hz, level " level)
      (let [{:keys [frames rows]} (session {:extra-effects [(sawtooth :saw reversed?)]
                                           :launches [{:op :cue/launch :cue-id :saw :source agent-source
                                                       :launch {:mode :persistent :params {:rate 2.0 :level level :lightness 1.0}}}]})]
        (is (<= (peak-rate (harding-flashes frames)) 3))
        (is (some #(get-in % [:guard :limiting?]) rows))))))

(def ^:private slow-human-strobe (strobe-launch push-source {:rate 1.0}))

(defn- influenced
  "A human strobe at 3 Hz, then at 1 s an agent item. Measured from 1.7 s on."
  [item]
  (after (session {:launches [slow-human-strobe] :commands {1000 [item]}}) 1700))

(deftest light-an-agent-influenced-is-guarded
  (testing "set_parameter on a human instance"
    (let [run (influenced {:tool "set_parameter" :args (fn [ids] {:instance-id (first ids) :param "rate" :value 4.0})})]
      (is (= :accepted (:status (last (:replies run)))))
      (is (<= (peak-rate (strobe-rises (:frames run) rig/coloured)) 3))))
  (testing "a scheduled param/set by cue-id"
    (let [run (influenced {:tool "schedule_events"
                           :args (fn [_] {:events [{:id "faster" :op "param/set" :cue-id "strobe" :param "rate" :value 4.0
                                                    :at {:beat 4}}]})})]
      (is (<= (peak-rate (strobe-rises (:frames run) rig/coloured)) 3))))
  (testing "a scheduled param/ramp by cue-id"
    (let [run (influenced {:tool "schedule_events"
                           :args (fn [_] {:events [{:id "ramp" :op "param/ramp" :cue-id "strobe" :param "rate" :to 4.0
                                                    :over {:beats 1} :at {:beat 4}}]})})]
      (is (<= (peak-rate (strobe-rises (:frames run) rig/coloured)) 3))))
  (testing "a scheduled opacity change on a human strobe"
    (let [run (after (session {:launches [(strobe-launch push-source {})]
                               :commands {1000 [{:tool "schedule_events"
                                                 :args (fn [_] {:events [{:id "fade" :op "instance/opacity" :cue-id "strobe"
                                                                          :value 0.9 :at {:beat 4}}]})}]}})
                     1700)]
      (is (<= (peak-rate (strobe-rises (:frames run) rig/coloured)) 3))))
  (testing "publish_effect revising the effect a human instance runs"
    (let [faster (-> @strobe-data (update :version inc)
                     (update :outputs (fn [outputs]
                                        (mapv (fn [o] (if (= :color (:attribute o))
                                                        (update o :value (fn walk [v]
                                                                           (cond (= v {:op :param :key :rate})
                                                                                 {:op :multiply :args [{:op :const :value 4.0} v]}
                                                                                 (map? v) (update-vals v walk)
                                                                                 (vector? v) (mapv walk v)
                                                                                 :else v)))
                                                        o)) outputs))))
          run (influenced {:tool "publish_effect" :args (fn [_] {:definition faster})})]
      (is (= :accepted (:status (last (:replies run)))))
      (is (<= (peak-rate (strobe-rises (:frames run) rig/coloured)) 3))))
  (testing "configure_auto launching the strobe"
    (let [{:keys [frames instances]} (session {:end-ms 5000
                                               :launches [{:tool "configure_auto"
                                                           :args (fn [_] {:configuration {:enabled? true :interval {:beats 1} :count 1
                                                                                          :scope {:cue-ids ["strobe"]}
                                                                                          :launch {:mode "persistent"} :seed 42}})}]})]
      (is (some #(= :auto (get-in % [:source :adapter])) (vals instances)))
      (is (<= (peak-rate (strobe-rises frames rig/coloured)) 3))))
  (testing "an agent variable write covers the whole show"
    (let [{:keys [frames rows]} (after (session {:launches [(strobe-launch push-source {})]
                                                 :commands {1000 [{:tool "schedule_events"
                                                                   :args (fn [_] {:events [{:id "v" :op "variable/set" :variable "section"
                                                                                            :value 1 :at {:beat 4}}]})}]}})
                                       1700)]
      (is (<= (peak-rate (strobe-rises frames rig/coloured)) 3))
      (is (= :show (:scope (:guard (last rows))))))))

(deftest policies-an-agent-configured-launch-agent-light
  (let [{:keys [model replies]} (session {:end-ms 200
                                          :launches [{:tool "configure_energy_autopilot" :args (fn [_] {:enabled? true})}
                                                     {:tool "set_lighting_scene" :args (fn [_] {:op "off"})}]})
        inf (influence/summary model)]
    (is (= :accepted (:status (first replies))))
    (is (contains? (:policies inf) :autopilot))
    (is (influence/attributed? #{:agent} inf {:id "x" :source {:adapter :autopilot}}))
    (is (not (influence/attributed? #{:agent} inf {:id "x" :source {:adapter :push-2}}))))
  (testing "an operator reconfiguring the policy ends the agent's claim"
    (let [model (-> (influence/note-policy {} :auto true false) (influence/note-policy :auto false true))]
      (is (nil? (influence/summary model))))))

;; ------------------------------------------------------------------ area and strobe channels

(deftest moving-heads-carry-their-visual-area
  (testing "a 12 Hz white strobe on every wash and mover"
    (let [heads (into rig/washes rig/movers)
          white (assoc @strobe-data :id :ww :targets heads
                       :outputs (mapv #(dissoc % :targets) (filterv (comp #{:color :dimmer} :attribute) (:outputs @strobe-data))))
          {:keys [frames]} (session {:extra-effects [white]
                                     :launches [{:op :cue/launch :cue-id :ww :source agent-source
                                                 :launch {:mode :persistent :params {:rate 4.0 :lightness 1.0}}}]})]
      (is (<= (peak-rate (strobe-rises frames heads)) 3))))
  (testing "a 10 Hz fixture shutter strobe on all six beams"
    (let [effect {:id :beam-strobe :version 1 :name "Beam strobe" :targets rig/beams :duration {:bars 64}
                  :outputs {:dimmer 1.0 :color [1.0 1.0 1.0] :shutter (/ 131.0 255.0)}}
          launch {:op :cue/launch :cue-id :beam-strobe :launch {:mode :persistent}}
          agent (session {:end-ms 500 :extra-effects [effect] :launches [(assoc launch :source agent-source)]})
          human (session {:end-ms 500 :extra-effects [effect] :launches [(assoc launch :source push-source)]})
          channel (fn [run id attribute] (byte-at (last (:frames run)) (get-in @patch [:by-id id :compiled-channels attribute :coarse])))]
      (doseq [id rig/beams]
        (is (= 131 (channel agent id :shutter) (channel human id :shutter)) "the shutter channel itself is untouched")
        (is (pos? (channel human id :dimmer))))
      (is (>= (count (filter #(zero? (channel agent % :dimmer)) rig/beams)) 5)
          "share times rate stays within area times limit; the rest lose their light")
      (is (some #(= [:strobe-channel] (:reasons %)) (guard-events (:events agent)))))))

(deftest slower-fixture-strobes-add-up-with-worst-case-phase
  ;; Byte 59 on the beams' shutter is about 3.9 Hz: each is under the limit.
  (let [effect {:id :beam-strobe :version 1 :name "Beam strobe" :targets rig/beams :duration {:bars 64}
                :outputs {:dimmer 1.0 :color [1.0 1.0 1.0] :shutter (/ 59.0 255.0)}}
        {:keys [frames]} (session {:end-ms 300 :extra-effects [effect]
                                   :launches [{:op :cue/launch :cue-id :beam-strobe :launch {:mode :persistent} :source agent-source}]})
        shutter (get-in @patch [:by-id :beam-1 :compiled-channels :shutter])
        hz (guard/strobe-rate shutter {(first (:coarse shutter)) (doto (byte-array 512) (aset-byte (second (:coarse shutter)) 59))})
        weights (area-weights @patch)
        lit (remove #(zero? (byte-at (last frames) (get-in @patch [:by-id % :compiled-channels :dimmer :coarse]))) rig/beams)]
    (is (< 3.5 hz 4.0))
    (is (< (count lit) 6) "six together would exceed the budget")
    (is (<= (reduce + (map #(* (weights %) hz) lit)) (* 0.25 3.0)))))

(deftest a-small-area-pixel-chase-above-the-limit-passes
  (let [tiny (-> @strobe-data
                 (assoc :id :tiny-strobe :name "Tiny strobe" :targets (vec (take 8 rig/tubes)))
                 (update :outputs #(filterv (comp #{:color} :attribute) %)))
        launch {:op :cue/launch :cue-id :tiny-strobe :launch {:mode :persistent :params {:rate 4.0}}}
        agent (session {:extra-effects [tiny] :launches [(assoc launch :source agent-source)]})
        human (session {:extra-effects [tiny] :launches [(assoc launch :source push-source)]})]
    (is (>= (peak-rate (strobe-rises (:frames agent) (take 8 rig/tubes))) 11))
    (is (= (:frames human) (:frames agent)) "eight pixels are well under a quarter of the rig")))

(deftest saturated-red-is-held-to-three-flashes-a-second-under-any-grant
  (let [params {:rate 1.25 :tint [1.0 0.0 0.0] :lightness 0.0}
        settings {:preset :club :grant {:limit-hz 12.0 :burst-beats 8}}
        red (session {:settings settings :launches [(strobe-launch agent-source params)]})
        white (session {:settings settings :launches [(strobe-launch agent-source (assoc params :tint [1.0 1.0 1.0]))]})]
    (testing "3.75 Hz white passes under a 4 Hz limit"
      (is (>= (peak-rate (strobe-rises (:frames white) rig/coloured)) 4))
      (is (not-any? #(get-in % [:guard :limiting?]) (:rows white))))
    (testing "the same saturated red is held to 3 Hz, a grant notwithstanding"
      (is (<= (peak-rate (strobe-rises (:frames red) rig/coloured)) 3))
      (is (some #(some #{:saturated-red} (:reasons %)) (guard-events (:events red)))))))

(deftest strobe-rates-follow-declared-spans
  (let [shutter (get-in @patch [:by-id :beam-1 :compiled-channels :shutter])
        frame (fn [value] {(first (:coarse shutter)) (doto (byte-array 512) (aset-byte (second (:coarse shutter)) (unchecked-byte value)))})]
    (is (nil? (guard/strobe-rate shutter (frame 10))) "shutter open")
    (is (< 0.19 (guard/strobe-rate shutter (frame 16)) 0.21) "slowest strobe step")
    (is (< 9.9 (guard/strobe-rate shutter (frame 131)) 10.1) "fastest strobe step")
    (is (= ##Inf (guard/strobe-rate shutter (frame 150))) "a pulse strobe without a declared rate")
    (is (= ##Inf (guard/strobe-rate {:coarse [1 0] :attribute :dimmer :functions [{:start 0 :end 99 :type :dim}
                                                                                    {:start 100 :end 255 :type :strobe}]}
                                    {1 (doto (byte-array 512) (aset-byte 0 (unchecked-byte 200)))}))
        "a strobe range in a dimmer channel counts")))

;; ------------------------------------------------------------------ grant

(deftest an-operator-grant-allows-a-bounded-faster-burst
  (let [settings {:grant {:limit-hz 12.0 :burst-beats 6}}
        {:keys [frames rows replies]} (session {:settings settings :end-ms 6000 :launches [(strobe-launch agent-source {})]})
        rises (strobe-rises frames rig/coloured)
        ;; At 180 BPM six beats are two seconds; the cooldown is as long again.
        early (filter #(< % 2000) rises)
        cooling (filter #(< 3200 % 4000) rises)]
    (is (= :accepted (:status (first replies))))
    (is (>= (peak-rate early) 11) "inside the burst the strobe runs at the granted rate")
    (is (<= (count cooling) 3) "after the burst it is held to the base limit")
    (is (some #(= 12.0 (get-in % [:guard :limit-hz])) rows))
    (is (some #(= 3.0 (get-in % [:guard :limit-hz])) (filter #(< 3000 (:t %)) rows)))))

(deftest a-relaunch-does-not-restart-a-burst
  (let [settings {:grant {:limit-hz 12.0 :burst-beats 4}}
        end! (fn [runtime]
               (doseq [id (keys (get-in @(:state runtime) [:model :instances]))]
                 (show/submit! runtime {:op :instance/kill :instance-id id :source agent-source})))
        ;; Four beats of burst end at 1.33 s; the cooldown lasts four more.
        {:keys [frames]} (session {:settings settings :end-ms 4000 :launches [(strobe-launch agent-source {})]
                                   :commands {1500 [end!] 1600 [(strobe-launch agent-source {})]}})
        cooling (filter #(< 1600 % 2600) (strobe-rises frames rig/coloured))]
    (is (<= (count cooling) 3) "the relaunched strobe is still in the cooldown")))

(deftest a-burst-counts-forward-beats-and-a-rewind-ends-it
  (let [next-burst @#'guard/next-burst
        cfg {:limit-hz 3.0 :grant {:limit-hz 12.0 :burst-beats 8}}
        start (next-burst cfg {:last-beat 10.0} 10.0 true)
        running (next-burst cfg start 12.0 true)
        rewound (next-burst cfg running 4.0 true)]
    (is (= 0.0 (:beats start)))
    (is (= 2.0 (:beats running)))
    (is (nil? (:beats rewound)) "a backward jump ends the burst")
    (is (= 8.0 (:cooldown rewound)) "and the cooldown is the full burst length")
    (is (= 3.0 (@#'guard/burst-limit cfg rewound)) "the base limit applies")
    (is (= 6.0 (:cooldown (next-burst cfg rewound 6.0 true))) "only forward beats drain it")
    (testing "a burst whose flashing pauses still runs to its cap"
      (is (= 2.0 (:beats (next-burst cfg running 12.0 false)))))))

(deftest grants-are-capped-and-lapse-on-restart
  (is (thrown? clojure.lang.ExceptionInfo (guard/settings {:grant {:limit-hz 20.0 :burst-beats 4}})))
  (is (thrown? clojure.lang.ExceptionInfo (guard/settings {:grant {:limit-hz 10.0 :burst-beats 16}})))
  (let [model (assoc-in (showcase/model) [:settings :flash-guard]
                        (guard/edit-settings nil {:grant {:limit-hz 12.0 :burst-beats 8}} 3))]
    (is (= 3 (get-in model [:settings :flash-guard :grant :epoch])))
    (is (some? (:grant (guard/config (assoc model :epoch 3)))))
    (is (nil? (:grant (guard/config (assoc model :epoch 4)))))))

;; ------------------------------------------------------------------ window, rig, mixed heads

(deftest a-rig-edit-keeps-the-window
  (let [edit! (fn [runtime]
                (let [rig (get-in @(:state runtime) [:model :rig])
                      moved (update rig :fixtures (fn [fs] (mapv #(if (= :wash-1 (:id %)) (update % :position (fn [[x y z]] [x y (+ z 0.1)])) %) fs)))]
                  (show/submit! runtime :rig moved)))
        {:keys [frames model replies]} (session {:launches [(strobe-launch agent-source {})] :commands {1000 [edit!]}})]
    (is (= :accepted (:status (last replies))))
    (is (not= (get-in (showcase/model) [:rig :fixtures]) (get-in model [:rig :fixtures])) "the rig changed")
    (is (<= (peak-rate (strobe-rises frames rig/coloured)) 3))))

(deftest mixed-heads-hold-and-name-the-human-cues
  (let [heads (into rig/washes rig/movers)
        layer {:id :agent-layer :version 1 :name "Agent layer" :targets heads :duration {:bars 64}
               :outputs {:dimmer 1.0}}
        launches (fn [agent?] (cond-> [(strobe-launch push-source {})]
                                agent? (conj {:op :cue/launch :cue-id :agent-layer :source agent-source
                                              :launch {:mode :persistent}})))
        mixed (session {:extra-effects [layer] :launches (launches true)})
        human (session {:extra-effects [layer] :launches (launches false)})
        human-id (some (fn [[id i]] (when (= :push-2 (get-in i [:source :adapter])) id)) (:instances mixed))
        agent-id (some (fn [[id i]] (when (= :agent (get-in i [:source :adapter])) id)) (:instances mixed))
        limiting (filter #(= :limiting (:status %)) (guard-events (:events mixed)))
        others (head-addresses (remove (set heads) rig/coloured))]
    (is (seq limiting) "safety wins on heads an agent writes")
    (is (some #(and (= [agent-id] (:instance-ids %)) (= [human-id] (:human-instance-ids %))) limiting))
    (is (empty? (filter others (differing-addresses (:frames human) (:frames mixed))))
        "heads only the human lights are byte-identical")))

(deftest the-window-drains-on-wall-time-while-the-transport-is-stopped
  (let [{:keys [rows]} (session {:end-ms 3000 :launches [(strobe-launch agent-source {})]
                                 :commands {1000 [{:op :transport/set :transport {:playing? false}}]
                                            2600 [{:op :master/set :value 0.5}]}})
        at (fn [t] (:guard (first (filter #(= t (:t %)) rows))))]
    (is (pos? (:rate-hz (at 975))))
    (is (zero? (:rate-hz (at 2500))) "a second of wall time after the stop the window is empty")
    (is (not (:limiting? (at 2900))) "a master change on a stopped show is not held")))

;; ------------------------------------------------------------------ coarse and fine

(deftest a-sixteen-bit-dimmer-stays-coherent-under-hold
  (let [fixtures [{:id :d16 :universe 1 :address 1 :channels {:dimmer [1 2] :red 3 :green 4 :blue 5}}]
        ramp {:op :fract :args [{:op :multiply :args [{:op :beat :clock :absolute} 2.0]}]}
        effect {:id :ramp16 :version 1 :name "Ramp" :targets [:d16] :duration {:bars 64}
                :outputs {:dimmer ramp :color [1.0 1.0 1.0]}}
        model (-> (control-model/new-show {:show-id :d16 :rig {:fixtures fixtures} :transport {:bpm 180.0 :playing? true :beat 0.0}})
                  (with-effects [effect]))
        run (fn [source] (:frames (session {:model model :end-ms 3000
                                            :launches [{:op :cue/launch :cue-id :ramp16 :source source :launch {:mode :persistent}}]})))
        level (fn [frame] (+ (* 256 (nth (get frame 1) 0)) (nth (get frame 1) 1)))
        guarded (mapv level (run agent-source))
        free (mapv level (run push-source))
        d16-patch (render/compile-patch fixtures)]
    (is (every? true? (map <= guarded free)) "a hold only ever lowers the 16-bit level")
    (is (some true? (map < guarded free)) "and it held")
    (is (<= (peak-rate (harding-flashes (run agent-source) d16-patch 0.25)) 3))
    (testing "held frames keep a fine byte: the level is scaled as one 16-bit value"
      (is (some (fn [[g f]] (and (< g f) (pos? (mod g 256)))) (map vector guarded free))))))

;; ------------------------------------------------------------------ settings

(deftest only-an-operator-can-change-the-guard
  (let [runtime (show/create-runtime {:model (showcase-model 128 []) :workspace-options nil})]
    (try
      (let [settle! (fn [ack] (show/tick! runtime 0.0) (deref ack 2000 {:status :timeout}))
            change (fn [values source] (settle! (show/submit! runtime :settings (cond-> {:scope :flash-guard :values values}
                                                                                   source (assoc :source source)))))]
        (is (= :rejected (:status (change {:enabled? false} agent-source))) "an agent")
        (is (= :rejected (:status (change {:enabled? false} nil))) "no source")
        (is (= :rejected (:status (change {:enabled? false} {:adapter :sensor :session-id "s"}))) "a sensor")
        (is (= :rejected (:status (change {:limit-hz 400} studio-source))) "an invalid limit")
        (is (= :accepted (:status (change {:scope :show} studio-source))))
        (is (= :accepted (:status (settle! (show/configure-flash-guard! runtime {:preset :club})))) "the REPL helper")
        (is (= {:scope :show :preset :club} (get-in (show/snapshot runtime) [:settings :flash-guard]))))
      (finally (show/stop! runtime)))))

(deftest saved-settings-may-only-tighten
  (let [saved {:enabled? false :preset :club :area 0.9 :sources [] :grant {:limit-hz 12.0 :burst-beats 8 :epoch 0}}
        loaded (guard/tighten-loaded saved)]
    (is (= 3.0 (:limit-hz loaded)))
    (is (= 0.25 (:area loaded)))
    (is (= [:agent] (:sources loaded)))
    (is (nil? (:grant loaded)))
    (is (some? (guard/config {:settings {:flash-guard loaded}})) "the guard stays on")
    (is (= {:scope :show} (select-keys (guard/tighten-loaded {:scope :show :limit-hz 2.0}) [:scope])))
    (is (= 2.0 (:limit-hz (guard/tighten-loaded {:limit-hz 2.0}))) "a stricter saved limit is kept")
    (testing "a show created from a saved model starts tightened"
      (let [runtime (show/create-runtime {:model (assoc-in (showcase-model 128 []) [:settings :flash-guard] saved)
                                          :workspace-options nil})]
        (try
          (is (= loaded (get-in (show/snapshot runtime) [:settings :flash-guard])))
          (finally (show/stop! runtime)))))))

(deftest settings-validate-and-resolve-presets
  (is (= {:limit-hz 3.0 :red-limit-hz 3.0} (select-keys (guard/config {}) [:limit-hz :red-limit-hz])))
  (is (= 4.0 (:limit-hz (guard/config {:settings {:flash-guard {:preset :club}}}))))
  (is (= 5.0 (:limit-hz (guard/config {:settings {:flash-guard {:limit-hz 5.0}}}))))
  (is (nil? (guard/config {:settings {:flash-guard {:enabled? false}}})))
  (is (= #{:agent} (:sources (guard/config {}))))
  (doseq [bad [{:scope :room} {:preset :festival} {:area 0} {:limit-hz 0} {:grant {:limit-hz 10}}
               {:sources #{:agent}} {:unknown true} {:min-change 2}]]
    (is (thrown? clojure.lang.ExceptionInfo (guard/settings bad)) (pr-str bad))))
