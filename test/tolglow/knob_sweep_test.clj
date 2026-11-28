(ns tolglow.knob-sweep-test
  "Linked knob demonstration (#666) on captured ArtDmx from the real owner tick:
  one knob sweep raises a colour's saturation, pans a head, steps an LFO period
  through the preferred beat ratios in 4/4, and over the knob's upper half
  raises the gain and shortens the duty cycle of a square LFO on the dimmer."
  (:require [afterglow.control.model :as control]
            [afterglow.output.artnet :as artnet]
            [afterglow.render.easing :as easing]
            [clojure.test :refer [deftest is testing]]
            [tolglow.knobs :as knobs]
            [tolglow.persistence :as files]
            [tolglow.show :as show]))

(def ^:private effects
  {:tint {:id :tint :version 1 :category :color
          :params {:saturation {:type :number :default 0.0 :bounds [0.0 1.0]}
                   :hue {:type :color :default [1.0 0.0 0.0]}}
          :outputs {:color {:op :mix :args [{:op :vec3 :args [{:op :const :value 1.0} {:op :const :value 1.0}
                                                                {:op :const :value 1.0}]}
                                              {:op :param :key :hue :type :vec3}
                                              {:op :param :key :saturation}]}}}
   :aim {:id :aim :version 1 :category :movement
         :params {:pan {:type :number :default 0.5 :bounds [0.0 1.0]}}
         :outputs {:pan {:op :param :key :pan}}}
   :pulse {:id :pulse :version 1 :category :light
           :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
           :outputs {:dimmer {:op :param :key :level}}}})

(defn- live-model []
  (control/new-show
   {:show-id :knob-test :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects effects
               :cues (into {} (for [id (keys effects)] [id {:id id :effect-id id :version 1}]))}
    :rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:red 1 :green 2 :blue 3 :dimmer 4 :pan 5}}]}}))

(defn- harness
  ([] (harness nil))
  ([options]
   (let [packets (atom [])
         sink {:send! (fn [universes _]
                        (swap! packets conj (vec (artnet/artdmx-packet 1 1 (get universes 1)))))}]
     {:rt (show/create-runtime (merge {:model (live-model) :sink sink :workspace-options nil} options))
      :packets packets})))

;; ArtDmx data starts at byte 18.
(defn- channel [packet n] (bit-and 255 (nth packet (+ 17 n))))

(def ^:private studio {:adapter :studio :session-id "knob-test"})

(defn- submit! ([rt time payload] (submit! rt time payload nil))
  ([rt time payload authority]
   (let [reply (show/submit! rt :command (merge {:source studio} payload) authority)]
     (show/tick! rt time)
     (deref reply 2000 ::missing))))

(defn- message! [rt time type payload]
  (let [reply (show/submit! rt type payload)]
    (show/tick! rt time)
    (deref reply 2000 ::missing)))

(def links
  [{:id "sat" :destination {:kind :param :cue-id :tint :param :saturation} :range [0.2 1.0]}
   {:id "pan" :destination {:kind :param :cue-id :aim :param :pan} :range [0.25 0.75] :curve :ease-in-out}
   {:id "rate" :destination {:kind :lfo :cue-id :pulse :param :level :field :period}
    :range [2 1/2] :quantize {:ratios :preferred}
    ;; #666 steps at once on a turn; #667 would give a stepped link its envelope by default.
    :envelope {}}
   {:id "gain" :destination {:kind :lfo :cue-id :pulse :param :level :field :to}
    :range [0.5 1.0] :input [0.5 1.0]}
   {:id "duty" :destination {:kind :lfo :cue-id :pulse :param :level :input :duty}
    :range [0.125 0.5] :direction :down :input [0.5 1.0]}])

(def preferred-steps [2 3/2 4/3 1 3/4 2/3 1/2])

(defn- expected [knob]
  (let [upper (max 0.0 (min 1.0 (/ (- knob 0.5) 0.5)))]
    {:saturation (+ 0.2 (* 0.8 knob))
     :pan (+ 0.25 (* 0.5 (easing/sample :ease-in-out knob)))
     :period (nth preferred-steps (min 6 (long (Math/floor (+ (* knob 7) 1e-9)))))
     :gain (+ 0.5 (* 0.5 upper))
     :duty (+ 0.125 (* 0.375 (- 1.0 upper)))}))

(defn- setup! [rt]
  (let [launch #(:instance-id (submit! rt %2 {:op :cue/launch :cue-id % :launch {:mode :persistent}}))
        ids {:tint (launch :tint 0) :aim (launch :aim 10) :pulse (launch :pulse 20)}
        bound (message! rt 30 :modulation-bind
                        {:instance-id (:pulse ids) :param :level
                         :binding {:type :lfo :waveform :pulse :period {:beats 1} :phase-reference :transport
                                   :inputs {:duty 0.5} :from 0.0 :to 0.5}})
        created (submit! rt 40 {:op :knob/create :knob-id :look :name "Look" :links links})]
    (assert (= :accepted (:status bound)) (pr-str bound))
    (assert (= :accepted (:status created)) (pr-str created))
    ids))

(defn- pulse-high? [beat period duty]
  (let [x (/ beat (double period))]
    (< (+ (- x (Math/floor x)) (- 1.0 duty)) 1.0)))

(defn- sweep
  "Set each knob value, render one frame and read DMX plus the shared projection."
  [rt packets t0 values]
  (loop [t t0 [value & more] values out []]
    (if-not value
      [t out]
      (let [ack (submit! rt t {:op :knob/set :knob-id :look :value value})
            packet (peek @packets)
            beat (get-in (show/snapshot rt) [:transport :beat])
            view (knobs/knob-view (:model @(:state rt)) :look)]
        (recur (+ t 20) more
               (conj out {:knob value :ack ack :beat beat :view view
                          :green (channel packet 2) :dimmer (channel packet 4) :pan (channel packet 5)}))))))

(defn- link [view id] (some #(when (= id (:id %)) %) (:links view)))

(def ^:private boundaries (mapv #(/ % 7.0) (range 1 7)))

(def ^:private sweep-values
  (vec (sort (distinct (concat (map #(/ % 16.0) (range 17)) boundaries
                               (map #(- % 1e-6) boundaries))))))

(deftest one-sweep-drives-colour-aim-ratio-and-lfo-on-captured-dmx
  (let [{:keys [rt packets]} (harness)]
    (try
      (setup! rt)
      (let [[t up] (sweep rt packets 100 sweep-values)
            [_ down] (sweep rt packets t (rseq sweep-values))]
        (doseq [[direction frames] [[:up up] [:down down]]
                {:keys [knob ack beat view green dimmer pan]} frames
                :let [e (expected knob)
                      context (pr-str {:direction direction :knob knob :beat beat})]]
          (is (= :accepted (:status ack)) context)
          (is (every? #(= :applied (:status %)) (:links ack)) (str context (pr-str (:links ack))))
          (testing "saturation: green falls from 0.8 to 0 as saturation rises"
            (is (<= (Math/abs (- green (* 255 (- 1.0 (:saturation e))))) 1.0) (str context " green " green)))
          (testing "pan follows the ease-in-out curve between 0.25 and 0.75"
            (is (<= (Math/abs (- pan (* 255 (:pan e)))) 1.0) (str context " pan " pan)))
          (testing "the projection shows the exact ratio, gain and duty"
            (is (= (:period e) (get-in (link view "rate") [:resolved :value])) context)
            (is (= (let [r (:period e)] (if (integer? r) [r 1] [(numerator r) (denominator r)]))
                   (get-in (link view "rate") [:resolved :ratio]))
                context)
            (is (< (Math/abs (- (:gain e) (get-in (link view "gain") [:resolved :value]))) 1e-9) context)
            (is (< (Math/abs (- (:duty e) (get-in (link view "duty") [:resolved :value]))) 1e-9) context))
          (testing "the dimmer is the square LFO with the knob's period, duty and gain at this frame's beat"
            (let [high? (pulse-high? beat (:period e) (:duty e))]
              (is (<= (Math/abs (- dimmer (if high? (* 255 (:gain e)) 0))) 1.0)
                  (str context " dimmer " dimmer " high? " high?)))))
        (testing "ratio boundaries select the upper step in both directions; just below selects the lower step"
          (doseq [frames [up down]
                  :let [by-knob (into {} (map (juxt :knob identity)) frames)]
                  [k boundary] (map-indexed vector boundaries)]
            (is (= (nth preferred-steps (inc k))
                   (get-in (by-knob boundary) [:view :links 2 :resolved :value])))
            (is (= (nth preferred-steps k)
                   (get-in (by-knob (- boundary 1e-6)) [:view :links 2 :resolved :value])))))
        (testing "endpoints in both directions"
          (doseq [frames [up down]
                  :let [by-knob (into {} (map (juxt :knob identity)) frames)]]
            (is (= ["0.200" "0.250" "2 beats" "0.500" "0.500"]
                   (mapv #(get-in (by-knob 0.0) [:view :links % :resolved :text]) (range 5))))
            (is (= ["1.000" "0.750" "1/2 beats" "1.000" "0.125"]
                   (mapv #(get-in (by-knob 1.0) [:view :links % :resolved :text]) (range 5))))
            (is (= "1/2 beats" (get-in (by-knob 1.0) [:view :links 2 :resolved :text])))
            (is (= [2 1] (get-in (by-knob 0.0) [:view :links 2 :resolved :ratio])))
            (is (= [1 2] (get-in (by-knob 1.0) [:view :links 2 :resolved :ratio]))))))
      (finally (show/stop! rt)))))

(defn- longest-high-run
  "Longest run of consecutive frames with the dimmer above zero, in beats."
  [frames]
  (let [runs (->> frames (partition-by #(pos? (:dimmer %))) (filter #(pos? (:dimmer (first %)))))]
    (apply max 0 (map #(- (:beat (last %)) (:beat (first %))) runs))))

(defn- frames-over [rt packets t0 n]
  (mapv (fn [i] (let [t (+ t0 (* 5 i))]
                  (show/tick! rt t)
                  {:beat (get-in (show/snapshot rt) [:transport :beat]) :dimmer (channel (peek @packets) 4)}))
        (range n)))

(deftest at-the-bright-end-the-pulse-is-shorter-and-brighter
  (let [{:keys [rt packets]} (harness)]
    (try
      (setup! rt)
      (submit! rt 100 {:op :knob/set :knob-id :look :value 0.5})
      (let [middle (frames-over rt packets 105 400)
            _ (submit! rt 2200 {:op :knob/set :knob-id :look :value 1.0})
            bright (frames-over rt packets 2205 400)]
        (testing "half way: one-beat period, half duty, half gain"
          (is (= #{0 128} (set (map :dimmer middle))))
          (is (<= 0.45 (longest-high-run middle) 0.5) (pr-str (longest-high-run middle))))
        (testing "bright end: half-beat period, eighth duty, full gain: a 1/16-beat pulse"
          (is (= #{0 255} (set (map :dimmer bright))))
          (is (<= 0.04 (longest-high-run bright) 0.0625) (pr-str (longest-high-run bright))))
        (is (< (longest-high-run bright) (longest-high-run middle))))
      (finally (show/stop! rt)))))

(defn- params [rt id] (get-in @(:state rt) [:model :instances id :params]))

(deftest manual-edits-gestures-ramps-and-a-second-knob-resolve-deterministically
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [{:keys [tint aim pulse]} (setup! rt)
            view #(knobs/knob-view (:model @(:state rt)) %)]
        (submit! rt 100 {:op :knob/set :knob-id :look :value 0.5})
        (testing "a manual edit on a linked destination wins until the knob moves again"
          (is (= :accepted (:status (submit! rt 120 {:op :param/set :instance-id tint :param :saturation :value 0.1}))))
          (is (= 0.1 (:saturation (params rt tint))))
          (is (:diverged? (link (view :look) "sat")) "the projection flags the destination as moved away from the knob")
          (is (= 0.5 (:value (view :look))) "the knob keeps its own position")
          (submit! rt 140 {:op :knob/set :knob-id :look :value 0.5})
          (is (< (Math/abs (- 0.6 (:saturation (params rt tint)))) 1e-9) "moving the knob writes the resolved value again")
          (is (not (:diverged? (link (view :look) "sat")))))
        (testing "an open human gesture on a destination holds it; other links still follow the knob"
          (let [operator {:operator-id :front-of-house}
                gesture {:instance-id aim :param :pan :control-id :encoder-2 :gesture-id :g1
                         :source {:adapter :push-3 :session-id "push"}}]
            (is (= :accepted (:status (submit! rt 160 (assoc gesture :op :gesture/begin) operator))))
            (is (= :accepted (:status (submit! rt 180 (assoc gesture :op :gesture/sample :sequence 1 :value 0.9) operator))))
            (let [ack (submit! rt 200 {:op :knob/set :knob-id :look :value 0.75})
                  by-id (into {} (map (juxt :link-id identity)) (:links ack))]
              (is (= {:status :skipped :reason :gesture-open} (select-keys (by-id "pan") [:status :reason])))
              (is (= :applied (:status (by-id "sat"))))
              (is (= 0.9 (:pan (params rt aim))) "the gesture's value stays"))
            (is (= :accepted (:status (submit! rt 220 (assoc gesture :op :gesture/end :last-sequence 1) operator))))
            (submit! rt 240 {:op :knob/set :knob-id :look :value 0.75})
            (is (< (Math/abs (- (+ 0.25 (* 0.5 (easing/sample :ease-in-out 0.75))) (:pan (params rt aim)))) 1e-9)
                "after the gesture ends the next knob move writes the destination")))
        (testing "a knob write cancels a running scheduler ramp on the same destination"
          (let [posted (submit! rt 260 {:op :param/ramp :event-id "fade" :instance-id tint :param :saturation
                                        :to 0.0 :over {:beats 8} :source {:adapter :agent :session-id "a"}})]
            (is (= :accepted (:status posted)))
            (show/tick! rt 500)
            (is (= 1 (count (get-in (show/snapshot rt) [:schedule :ramps]))))
            (submit! rt 520 {:op :knob/set :knob-id :look :value 0.25})
            (is (empty? (get-in (show/snapshot rt) [:schedule :ramps])))
            (is (= {:event-id "fade" :status :cancelled :reason :human-override}
                   (select-keys (last (get-in (show/snapshot rt) [:schedule :recent])) [:event-id :status :reason])))
            (show/tick! rt 600)
            (is (< (Math/abs (- 0.4 (:saturation (params rt tint)))) 1e-9) "the knob value stays after the ramp is gone")
            (is (<= (Math/abs (- (channel (peek @packets) 2) (* 255 0.6))) 1.0))))
        (testing "two knobs on one destination: both list the share and the last committed write wins"
          (is (= :accepted (:status (submit! rt 700 {:op :knob/create :knob-id :wash :name "Wash"
                                                     :links [{:id "s" :range [0.0 0.3]
                                                              :destination {:kind :param :cue-id :tint :param :saturation}}]}))))
          (is (= [:wash] (:shared-with (link (view :look) "sat"))))
          (is (= [:look] (:shared-with (link (view :wash) "s"))))
          (submit! rt 720 {:op :knob/set :knob-id :wash :value 1.0})
          (is (< (Math/abs (- 0.3 (:saturation (params rt tint)))) 1e-9))
          (is (:diverged? (link (view :look) "sat")))
          (submit! rt 740 {:op :knob/nudge :knob-id :look :delta 0.25})
          (is (= 0.5 (:value (view :look))))
          (is (< (Math/abs (- 0.6 (:saturation (params rt tint)))) 1e-9))
          (is (:diverged? (link (view :wash) "s"))))
        (testing "removing a knob leaves its destinations and their current values intact"
          (let [before (params rt tint)
                binding (get-in @(:state rt) [:model :instances pulse :parameter-bindings :level :binding])]
            (is (= :accepted (:status (submit! rt 800 {:op :knob/remove :knob-id :look}))))
            (is (nil? (view :look)))
            (is (= before (params rt tint)))
            (is (= binding (get-in @(:state rt) [:model :instances pulse :parameter-bindings :level :binding]))
                "the LFO keeps its last knob-driven period, duty and gain")
            (is (= {:period {:beats 1} :inputs {:duty 0.5} :to 0.5} (select-keys binding [:period :inputs :to])))
            (is (= [] (:shared-with (link (view :wash) "s")))))))
      (finally (show/stop! rt)))))

(deftest knobs-survive-a-full-backup-and-report-dangling-targets-after-reload
  (let [{:keys [rt]} (harness)]
    (try
      (let [{:keys [aim tint]} (setup! rt)
            created (submit! rt 50 {:op :knob/create :knob-id "solo" :name "Solo"
                                    :links [{:id "aim" :destination {:kind :param :instance-id aim :param :pan}
                                             :range [0.0 1.0]}
                                            {:id "energy" :destination {:kind :variable :variable :look/energy}
                                             :range [0 10] :quantize :integer :unit :percent :envelope {}}]})
            _ (submit! rt 60 {:op :knob/set :knob-id :look :value 3/7})
            _ (submit! rt 70 {:op :knob/set :knob-id "solo" :value 0.42})
            model (:model @(:state rt))
            restored (files/thaw (live-model) (files/freeze model))
            before (knobs/projection model)
            after (knobs/projection restored)
            new-aim (some #(when (= :aim (:cue-id %)) (:id %)) (vals (:instances restored)))]
        (is (= :accepted (:status created)))
        (testing "a full backup keeps every knob, value and link; launch IDs are rebound"
          (is (= (mapv #(select-keys % [:id :name :value]) (:knobs before))
                 (mapv #(select-keys % [:id :name :value]) (:knobs after))))
          (is (not= aim new-aim))
          (is (= new-aim (get-in restored [:knobs :knobs "solo" :links 0 :destination :instance-id])))
          (is (= (for [k (:knobs before) l (:links k)] [(:id l) (:status l) (:resolved l)])
                 (for [k (:knobs after) l (:links k)] [(:id l) (:status l) (:resolved l)])))
          (is (= {:value 4 :text "4 percent" :unit :percent}
                 (:resolved (link (second (:knobs after)) "energy"))))
          (is (= [1 1] (get-in (link (first (:knobs after)) "rate") [:resolved :ratio]))
              "3/7 sits exactly on a boundary and stays on the upper step (1 beat) after reload"))
        (testing "a removed launch and an incompatible effect revision stay visible"
          (is (= :accepted (:status (submit! rt 80 {:op :instance/remove :instance-id aim}))))
          (is (= :accepted (:status (submit! rt 90 {:op :registry/publish :kind :effects
                                                    :definition (-> (:aim effects) (assoc :version 2)
                                                                    (assoc-in [:params :pan] {:type :number :default 0.75
                                                                                              :bounds [0.5 1.0]}))}))))
          (let [model (:model @(:state rt))
                live (knobs/projection model)
                reloaded (files/thaw (live-model) (files/freeze model))
                again (knobs/projection reloaded)
                status #(select-keys (link (some (fn [k] (when (= %2 (:id k)) k)) (:knobs %1)) %3) [:status :reason])]
            (is (= {:status :dangling :reason :instance-missing} (status live "solo" "aim")))
            (is (= {:status :dangling :reason :incompatible} (status live :look "pan")))
            (is (= {:status :ok} (status live :look "sat")))
            (is (= {:status :dangling :reason :instance-missing} (status again "solo" "aim")))
            (is (= :instance-missing (get-in reloaded [:knobs :knobs "solo" :links 0 :dangling :reason]))
                "the restore keeps the unmatched link with a reason instead of dropping it")
            (is (= aim (get-in reloaded [:knobs :knobs "solo" :links 0 :destination :instance-id])))
            (is (= {:status :dangling :reason :incompatible} (status again :look "pan")))
            (testing "a knob with dangling links still sets its live links"
              (let [ack (submit! rt 100 {:op :knob/set :knob-id :look :value 1.0})
                    by-id (into {} (map (juxt :link-id identity)) (:links ack))]
                (is (= :accepted (:status ack)))
                (is (= {:status :skipped :reason :incompatible} (select-keys (by-id "pan") [:status :reason])))
                (is (= :applied (:status (by-id "sat"))))
                (is (= 1.0 (:saturation (params rt tint)))))))))
      (finally (show/stop! rt)))))

(deftest a-repeated-knob-command-replays-its-receipt-without-writing-twice
  (let [{:keys [rt]} (harness)]
    (try
      (let [{:keys [tint]} (setup! rt)
            payload {:op :knob/set :knob-id :look :value 0.5 :command-id "turn-1"}
            first-ack (submit! rt 100 payload)
            revision (:revision (show/snapshot rt))
            _ (submit! rt 120 {:op :param/set :instance-id tint :param :saturation :value 0.05})
            replay (submit! rt 140 payload)]
        (is (= :accepted (:status first-ack)))
        (is (= (:links first-ack) (:links replay)))
        (is (= (:revision first-ack) (:revision replay)))
        (is (= (inc revision) (:revision (show/snapshot rt))) "only the manual edit changed the show after the first turn")
        (is (= 0.05 (:saturation (params rt tint))) "the replay does not overwrite a later edit")
        (is (= :rejected (:status (submit! rt 160 (assoc payload :value 0.7)))) "a reused ID with new data is refused"))
      (finally (show/stop! rt)))))

(deftest an-encoder-surface-turns-the-knob-and-shows-the-studio-values
  ;; The command a bound encoder sends, with a Push source, and the display it
  ;; reads. tolglow.controllers.push-knobs-test drives it through captured Push
  ;; 2/3 MIDI and the Afterglow adapters.
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [{:keys [tint]} (setup! rt)
            push {:adapter :push-3 :session-id "push"}
            turn! (fn [t delta fine?] (submit! rt t (assoc (knobs/encoder-command :look delta fine?) :source push)))
            model #(:model @(:state rt))
            studio-texts #(mapv (comp :text :resolved) (:links (knobs/knob-view (model) :look)))]
        (submit! rt 50 {:op :param/ramp :event-id "fade" :instance-id tint :param :saturation :to 0.0 :over {:beats 8}
                        :source {:adapter :agent :session-id "a"}})
        (let [acks (mapv #(turn! (+ 100 (* 10 %)) 1 false) (range 50))
              display (knobs/encoder-display (model) :look)]
          (is (every? #(= :accepted (:status %)) acks))
          (is (< (Math/abs (- 0.5 (:value display))) 1e-9) "fifty coarse detents move half the range")
          (is (= "0.500" (:text display)))
          (is (= (studio-texts) (mapv :text (:columns display))) "the encoder display reads the Studio projection")
          (is (= [1 1] (:ratio (nth (:columns display) 2))))
          (is (= {:event-id "fade" :status :cancelled :reason :human-override}
                 (select-keys (last (get-in (show/snapshot rt) [:schedule :recent])) [:event-id :status :reason]))
              "a Push turn is a human write and takes over the ramped destination")
          (is (<= (Math/abs (- (channel (peek @packets) 2) (* 255 (- 1.0 0.6)))) 1.0)))
        (turn! 700 -5 true)
        (is (< (Math/abs (- 0.495 (:value (knobs/encoder-display (model) :look)))) 1e-9) "Shift turns are ten times finer")
        (turn! 720 200 false)
        (is (= 1.0 (:value (knobs/encoder-display (model) :look))) "turning past the end clamps")
        (is (= ["1.000" "0.750" "1/2 beats" "1.000" "0.125"] (mapv :text (:columns (knobs/encoder-display (model) :look))))))
      (finally (show/stop! rt)))))

(deftest a-rejected-linked-write-is-reported-on-its-link-and-the-rest-still-apply
  ;; Writes use the derived IDs <command-id>/<n>. A client that already used
  ;; one of those IDs for another command makes that one write conflict; the
  ;; receipt says so on the link instead of claiming the write.
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [{:keys [tint aim]} (setup! rt)
            _ (submit! rt 100 {:op :param/set :instance-id tint :param :saturation :value 0.05 :command-id "turn-9/0"})
            ack (submit! rt 120 {:op :knob/set :knob-id :look :value 0.5 :command-id "turn-9"})
            by-id (into {} (map (juxt :link-id identity)) (:links ack))]
        (is (= :accepted (:status ack)) "the knob value itself committed")
        (is (= {:status :rejected :code :command-id-conflict}
               (select-keys (by-id "sat") [:status :code])))
        (is (= #{:applied} (set (map :status (vals (dissoc by-id "sat"))))))
        (is (= 0.5 (:value (knobs/knob-view (:model @(:state rt)) :look))))
        (is (= 0.05 (:saturation (params rt tint))) "the refused destination keeps its value")
        (is (< (Math/abs (- (+ 0.25 (* 0.5 (easing/sample :ease-in-out 0.5))) (:pan (params rt aim)))) 1e-9))
        (is (:diverged? (link (knobs/knob-view (:model @(:state rt)) :look) "sat")))
        (is (<= (Math/abs (- (channel (peek @packets) 2) (* 255 0.95))) 1.0) "DMX shows the refused value")
        (is (= (:links ack) (:links (submit! rt 140 {:op :knob/set :knob-id :look :value 0.5 :command-id "turn-9"})))
            "a replay reports the same partial outcome"))
      (finally (show/stop! rt)))))

(deftest a-room-audition-never-commits-knob-or-destination-state
  ;; A Room draft cannot stage knob commands, and its private model carries no
  ;; knobs. A knob turned during an audition is a live edit after staging: it
  ;; writes the live show, and the Room keeps the values it was staged with.
  (let [{:keys [rt packets]} (harness {:audition {:max-live-share ##Inf}})
        draft #(deref (:scene-draft rt))
        tick! (fn [t] (show/tick! rt t) (is (show/await-audition rt 5000)))]
    (try
      (let [{:keys [tint pulse]} (setup! rt)
            _ (submit! rt 100 {:op :knob/set :knob-id :look :value 0.5})
            refused (submit! rt 110 {:op :scene-draft/stage :draft-id "scene"
                                     :operations [{:op :knob/set :knob-id :look :value 1.0}]})
            staged (submit! rt 120 {:op :scene-draft/stage :draft-id "scene"
                                    :operations [{:op :instance/end :instance-id pulse}]})
            before (:model @(:state rt))]
        (is (= :rejected (:status refused)) "a knob command cannot be staged")
        (is (= :accepted (:status staged)))
        (run! tick! (range 140 400 20))
        (testing "audition frames leave the knob, its links and every destination as they were"
          (is (= (:knobs before) (:knobs (:model @(:state rt)))))
          (is (= (params rt tint) (get-in before [:instances tint :params])))
          (is (= (get-in before [:instances pulse :parameter-bindings])
                 (get-in @(:state rt) [:model :instances pulse :parameter-bindings])))
          (is (not (contains? (get-in (draft) [:frame :model]) :knobs)) "the private model has no knobs"))
        (testing "a knob turned during the audition writes the live show only"
          (let [ack (submit! rt 400 {:op :knob/set :knob-id :look :value 1.0})]
            (tick! 420)
            (is (= #{:applied} (set (map :status (:links ack)))))
            (is (= 1.0 (:saturation (params rt tint))))
            (is (<= (channel (peek @packets) 2) 1) "live DMX follows the knob")
            (is (< (Math/abs (- 0.6 (get-in (draft) [:frame :model :instances tint :params :saturation]))) 1e-9)
                "the Room keeps the staged saturation")
            (is (= :auditioning (:status (show/scene-draft rt))) "a knob turn does not stale the draft")))
        (testing "applying the draft keeps the knob and its live values"
          (let [applied (submit! rt 440 {:op :scene-draft/apply :draft-id "scene" :expected-draft-revision 0})
                view (knobs/knob-view (:model @(:state rt)) :look)]
            (is (= :accepted (:status applied)))
            (is (= 1.0 (:value view)))
            (is (= 1.0 (:saturation (params rt tint))))
            (is (= #{:inactive} (set (map :status (filter #(#{"rate" "gain" "duty"} (:id %)) (:links view)))))
                "links to the ended pulse cue wait for its next launch"))))
      (finally (show/stop! rt)))))

(deftest every-dangling-reason-survives-a-reload
  (let [{:keys [rt]} (harness)]
    (try
      (let [{:keys [tint pulse]} (setup! rt)
            _ (message! rt 50 :modulation-unbind {:instance-id pulse :param :level})
            _ (submit! rt 60 {:op :instance/end :instance-id tint})
            _ (show/tick! rt 80)
            _ (submit! rt 100 {:op :registry/publish :kind :effects
                               :definition (-> (:tint effects) (assoc :version 2) (update :params dissoc :saturation)
                                               (assoc :outputs {:color {:op :param :key :hue :type :vec3}}))})
            _ (submit! rt 110 {:op :cue/put :cue {:id :tint :effect-id :tint :version 2}})
            _ (submit! rt 120 {:op :cue/delete :cue-id :aim})
            model (:model @(:state rt))
            reasons (fn [model] (into {} (map (juxt :id (juxt :status :reason)))
                                      (:links (knobs/knob-view model :look))))
            expected {"sat" [:dangling :param-missing] "pan" [:dangling :cue-missing]
                      "rate" [:dangling :lfo-missing] "gain" [:dangling :lfo-missing] "duty" [:dangling :lfo-missing]}]
        (is (= expected (reasons model)))
        (is (= expected (reasons (files/thaw (live-model) (files/freeze model)))))
        (is (= 5 (count (get-in (files/thaw (live-model) (files/freeze model)) [:knobs :knobs :look :links])))
            "nothing is dropped")
        (let [ack (submit! rt 140 {:op :knob/set :knob-id :look :value 1.0})]
          (is (= :accepted (:status ack)))
          (is (= (update-vals expected (fn [[_ reason]] [:skipped reason]))
                 (into {} (map (juxt :link-id (juxt :status :reason))) (:links ack))))))
      (finally (show/stop! rt)))))
