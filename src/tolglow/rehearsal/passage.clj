(ns tolglow.rehearsal.passage
  "Recorded rehearsal passages as data (#670). A passage is a music section
  map (what Live locators or AbleSet song sections would supply), a tempo and
  the inputs a performance would have produced, each placed on a transport
  beat: stem onsets and levels, Push-sourced commands and transport stops.
  Everything here is pure and seeded; tolglow.rehearsal plays a passage
  through the real show owner.

    (require '[tolglow.rehearsal.passage :as passage])
    (passage/club 670)            ; unattended: groove, breakdown, return, quiet
    (passage/concert 670 :a)      ; Push-led take A of the same song as take B
    (passage/concert 670 :b)

  The show model is the 2023 gig rig with the R1o red-show cues
  (tolglow.automation.red-show) plus two section anchors and two stem hits.
  Bars and beats count from zero, as the scheduler does."
  (:require [afterglow.time :as time]
            [tolglow.automation.random :as random]
            [tolglow.automation.red-show :as red-show]
            [tolglow.registry :as registry]))

(def meter time/default-meter)

(def beats-per-bar (:beats-per-bar meter))

(def beats-per-phrase (* beats-per-bar (:bars-per-phrase meter)))

(def kick-washes [:wash-1 :wash-3 :wash-5 :wash-7])

(def rehearsal-effects
  "Section anchors the agent schedules for a concert, and the hits stem onsets
  launch. Anchors light the washes; fills and hits sit on top of them."
  [{:id :verse-wash :version 1 :targets red-show/washes
    :params {:level {:type :number :default 0.45 :bounds [0 1]}}
    :outputs {:color red-show/deep-red :dimmer {:op :param :key :level}}}
   {:id :chorus-wash :version 1 :targets red-show/washes
    :params {:level {:type :number :default 0.6 :bounds [0 1]}}
    :outputs {:color red-show/red :dimmer {:op :param :key :level}}}
   {:id :kick-hit :version 1 :targets kick-washes :duration {:beats 1/4}
    :outputs {:color [1.0 0.85 0.8] :dimmer 1.0}}
   {:id :snare-hit :version 1 :targets red-show/movers :duration {:beats 1/4}
    :outputs {:color [1.0 1.0 1.0] :dimmer 1.0}}])

(def anchor-cue-ids #{:verse-wash :chorus-wash})

(defn model
  "The red-show gig rig with the rehearsal cues, its transport at bpm and beat 0."
  [bpm]
  (-> (reduce (fn [m effect]
                (-> m (update :registry registry/publish :effects effect)
                    (update :registry registry/publish :cues
                            ;; Anchors and hits are played by the agent's schedule
                            ;; and the stems, never picked by auto mode.
                            {:id (:id effect) :effect-id (:id effect) :auto? false})))
              (red-show/model) rehearsal-effects)
      (assoc :transport {:playing? true :beat 0.0 :bpm (double bpm)})))

(defn- with-starts
  "Give each section its zero-based :start-bar."
  [sections]
  (second (reduce (fn [[bar out] section] [(+ bar (:bars section)) (conj out (assoc section :start-bar bar))])
                  [0 []] sections)))

(defn section-at
  "The music section playing at beat, or nil past the end."
  [passage beat]
  (some #(when (and (<= (* beats-per-bar (:start-bar %)) beat)
                    (< beat (* beats-per-bar (+ (:start-bar %) (:bars %))))) %)
        (:sections passage)))

(defn length-beats [passage]
  (* beats-per-bar (reduce + (map :bars (:sections passage)))))

(defn- seeded-unit [seed & parts]
  (second (random/next-random (apply random/derive-seed seed parts))))

(defn- drum-onsets
  "Kick on every beat, snare on beats 1 and 3 of each bar, and a sixteenth
  snare fill over the last two beats of a section whose density the seed
  chooses. Sections of kind :quiet and :breakdown keep the kick only on the
  bar line, and the kick drops out for the last bar of a breakdown."
  [seed sections]
  (vec
   (sort-by (juxt :beat (comp str :stem))
            (mapcat
             (fn [{:keys [id kind start-bar bars]}]
               (let [first-beat (* beats-per-bar start-bar)
                     last-beat (* beats-per-bar (+ start-bar bars))
                     sparse? (#{:quiet :breakdown} kind)]
                 (concat
                  (for [beat (range first-beat last-beat)
                        :when (if sparse?
                                (and (zero? (mod beat beats-per-bar))
                                     (not (and (= :breakdown kind) (>= beat (- last-beat beats-per-bar)))))
                                true)]
                    {:stem :kick :beat (double beat)})
                  (when-not sparse?
                    (for [beat (range first-beat (- last-beat 2)) :when (odd? (long (mod beat beats-per-bar)))]
                      {:stem :snare :beat (double beat)}))
                  (when-not sparse?
                    (let [density (+ 0.35 (* 0.6 (seeded-unit seed :fill id)))]
                      (for [step (range 8)
                            :let [beat (+ (- last-beat 2) (/ step 4.0))]
                            :when (or (zero? step) (< (seeded-unit seed :fill id step) density))]
                        {:stem :snare :beat beat}))))))
             sections))))

(defn- bass-levels
  "Bass envelope sampled every eighth note: a pump that peaks on the beat,
  louder in groove-like sections, silent in quiet ones, and one bar the seed
  picks where the bassist lays out."
  [seed sections]
  (vec
   (mapcat
    (fn [{:keys [id kind start-bar bars]}]
      (let [rest-bar (+ start-bar (long (* bars (seeded-unit seed :bass-rest id))))
            peak (case kind (:quiet) 0.0 (:breakdown :verse) 0.45 0.85)]
        (for [step (range (* 2 beats-per-bar bars))
              :let [beat (+ (* beats-per-bar start-bar) (/ step 2.0))
                    bar (long (quot beat beats-per-bar))
                    on-beat? (even? step)]]
          {:stem :bass :beat beat
           :level (if (= bar rest-bar) 0.0 (if on-beat? peak (* 0.4 peak)))})))
    sections)))

(def club-sections
  [{:id :groove :kind :groove :bars 16}
   {:id :breakdown :kind :breakdown :bars 8}
   {:id :return :kind :return :bars 16}
   {:id :quiet :kind :quiet :bars 8}])

(defn club
  "An unattended club passage at 124 BPM: groove, breakdown, return and an
  intentional quiet passage, with one transport stop in the return. Nobody
  plays; stems drive nothing here, the agent alone chooses. Options: :bpm,
  :sections, :stop {:beat b :hold-ms ms}, and :agent-lag-beats to make the
  agent answer each section line that many beats late (negative: early)."
  ([seed] (club seed {}))
  ([seed {:keys [bpm sections stop agent-lag-beats] :or {bpm 124.0 sections club-sections agent-lag-beats 0}}]
   (let [sections (with-starts sections)
         return (some #(when (= :return (:kind %)) %) sections)
         stop (or stop (when return {:beat (double (* beats-per-bar (+ (:start-bar return) 4 1/2))) :hold-ms 4000.0}))]
     {:id :club :mode :club :seed seed :bpm (double bpm) :take nil :agent-lag-beats agent-lag-beats
      :sections sections
      :stops (if stop [stop] [])
      :onsets (drum-onsets seed sections)
      :levels []
      :routes {}
      :gestures []})))

(def concert-sections
  [{:id :verse-1 :kind :verse :bars 8 :anchor :verse-wash}
   {:id :chorus-1 :kind :chorus :bars 8 :anchor :chorus-wash}
   {:id :verse-2 :kind :verse :bars 8 :anchor :verse-wash}
   {:id :chorus-2 :kind :chorus :bars 8 :anchor :chorus-wash}])

(def push
  "The source a Push 3 device session gives its commands
  (afterglow.control.devices.session uses the profile ID as the adapter)."
  {:adapter :push-3 :session-id "push-3-rehearsal"})

(defn- at-bar [bar & [beat]] (double (+ (* beats-per-bar bar) (or beat 0))))

(def take-gestures
  "Push captures for each take, as shared commands with a Push source. A
  :select names the running instance a pad or encoder was bound to when the
  gesture was recorded; the harness resolves it on replay."
  {:a [{:beat (at-bar 4) :label "Performer spotlight on"
        :command {:op :cue/launch :cue-id :spot :launch {:mode :persistent}}}
       {:beat (at-bar 6) :label "Performer spotlight off"
        :command {:op :instance/end} :select {:cue-id :spot :adapter :push-3}}
       {:beat (at-bar 16 2) :label "Take the auto sweep's span"
        :command {:op :param/set :param :span :value 0.1} :select {:cue-id :sweep :adapter :auto}}
       {:beat (at-bar 17 2) :label "Release the taken sweep"
        :command {:op :instance/end} :select {:cue-id :sweep :taken? true}}]
   :b [{:beat (at-bar 7 2) :label "Strobe into the chorus"
        :command {:op :cue/launch :cue-id :strobe-hit :launch {:mode :one-shot :duration {:beats 2}}}}
       {:beat (at-bar 10 2) :label "Grab the chorus level mid-ramp"
        :command {:op :param/set :param :level :value 0.35} :select {:cue-id :chorus-wash}}
       {:beat (at-bar 20 1) :label "Take the auto sweep's span"
        :command {:op :param/set :param :span :value 0.4} :select {:cue-id :sweep :adapter :auto}}
       {:beat (at-bar 21 3) :label "Release the taken sweep"
        :command {:op :instance/end} :select {:cue-id :sweep :taken? true}}]})

(defn concert
  "A Push-led concert passage at 120 BPM, verse/chorus twice, as take :a or
  :b. Both takes share the song, its section anchors and the agent; the take
  seed changes the drummer's fills, the bass rests and the auto fills, and
  each take has its own fixed table of Push gestures (take-gestures). Kick
  onsets launch :kick-hit, snare onsets :snare-hit, and the bass level
  drives the verse anchor's level."
  ([seed take] (concert seed take {}))
  ([seed take {:keys [bpm sections] :or {bpm 120.0 sections concert-sections}}]
   (when-not (contains? take-gestures take)
     (throw (ex-info "A concert take is :a or :b" {:take take})))
   (let [take-seed (random/derive-seed seed :take take)
         sections (with-starts sections)]
     {:id :concert :mode :concert :seed seed :take take :take-seed take-seed :bpm (double bpm)
      :sections sections
      :stops []
      :onsets (drum-onsets take-seed sections)
      :levels (bass-levels take-seed sections)
      :routes {:onsets {:kick :kick-hit :snare :snare-hit}
               :levels [{:stem :bass :cue-id :verse-wash :param :level}]}
      :gestures (get take-gestures take)})))
