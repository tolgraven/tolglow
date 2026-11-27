(ns tolglow.showcase
  "The Tolglow showcase show: a venue rig, a cue grid of ported and new looks,
  two macros and a sequence from the operator's saved compound cues, a
  recorded 32-bar timeline at 128 BPM that plays on the beat-clock scheduler,
  and the 16-bar `after-hours` timeline for the cues mined from tolglow-web.

  Run it with `lein run --config tolglow.showcase/config`. Output stays in the
  preview unless the command line selects a physical sink. Loading this
  namespace builds data and opens nothing.

  From a REPL:

      (require '[tolglow.showcase :as showcase] '[tolglow.show :as show])
      (def runtime (show/start! {:model (showcase/model)}))
      (def player (showcase/play! runtime))
      (showcase/status player)
      (showcase/stop! runtime player)
      (show/stop! runtime)"
  (:require [afterglow.control.model :as control-model]
            [clojure.java.io :as io]
            [tolglow.aim-paths :as aim-paths]
            [tolglow.audio-lfo :as audio-lfo]
            [tolglow.composition :as composition]
            [tolglow.data :as data]
            [tolglow.registry :as registry]
            [tolglow.show :as show]
            [tolglow.showcase.looks :as looks]
            [tolglow.showcase.rig :as rig]
            [tolglow.showcase.timeline :as timeline]
            [tolglow.signals :as signals]
            [tolglow.spatial :as spatial]
            [tolglow.spatial.cues :as spatial-cues]
            [tolglow.venue-fog :as venue-fog]))

(def default-seed
  "Every seeded draw in the show derives from this one integer."
  656)

(def meter {:beats-per-bar 4 :bars-per-phrase 4})

(def bpm 128)

;; ---------------------------------------------------------------------- model

(def hdr-settings
  "Highlight treatment for the two cues with a `:drive` gain. Excess above full
  scale whitens the source, and up to 80% of it spreads to heads within 0.4 m,
  which is three tube pixels either side of a hot one."
  {:enabled? true :scope :global
   :cue-gains {:blinder-hit {:cue-id :blinder-hit :param :drive :default 1}
               :riser-chase {:cue-id :riser-chase :param :drive :default 1}}
   :max-excess 2.0 :white-mix 0.8 :spill 0.8 :spill-white-mix 0.5
   :radius 0.4 :falloff [[0 1] [0.3 1] [0.65 0.45] [1 0]]})

(def spatial-settings
  "The rolling volume stays in a slab in front of the tube wall."
  {:enabled? true :run-when :enabled :gravity [0 0 0]
   :bounds {:min [-5.2 0.1 -4.6] :max [5.2 2.6 -2.2]}})

(def gather-path
  "A closed loop on the audience side of the stage. Every beam follows the same
  point, so the six shafts converge and travel together."
  {:id :gather-path :name "Gather path" :targets rig/beams :closed? true :mode :loop
   :duration {:bars 2} :curve :linear
   :points [[-2.4 0.4 1.2] [-0.8 1.6 2.6] [1.0 0.5 3.0] [2.5 1.4 1.4] [0.8 2.0 0.2] [-1.0 0.9 0.4]]})

(def rolling-volume
  "A soft warm ball that drifts along the tube wall and paints the pixels it
  passes through. Its centre is solid and its edge feathers over 0.6 m."
  {:op :spawn :id :rolling-volume :name "Rolling volume" :page-id :showcase :cell [1 3] :priority 40
   :volume {:shape :ball :position [-4.0 1.25 -3.4] :velocity [2.4 0.0 0.0] :gravity [0 0 0]
            :radius 1.1 :restitution 1.0 :selection #{:position}
            :falloff {:inner-m 0.35 :outer-m 0.6 :curve :smoothstep}}
   :paint {:color [0.3 0.2 0.12]}})

(def macros
  "Compound cues restated from the operator's saved macros.clj, with the
  parameter values of the originals where the port keeps the parameter."
  [{:id :mixed-movement :mode :macro :name "Mixed movement"
    :legacy "macros.clj \"mixed movement ex\" (38 11)"
    :slots [{:id :stripes :cue-id :pinstripes
             :params {:color-1 [0.36 0.64 0.09] :color-2 [0.03 0.07 0.56] :beats 1.0 :fade 0.76}}
            {:id :kick :cue-id :can-can
             :params {:bars 2.0 :stagger 1.94 :spread 21.0 :pan-min -42.0 :pan-max 23.0
                      :tilt-min -60.0 :tilt-max 80.0}}]}
   {:id :heavy-colour :mode :macro :name "Heavy colour" :opacity 0.9
    :legacy "macros.clj \"heavy color load\" (3 22)"
    :slots [{:id :sparks :cue-id :tube-sparkle
             :params {:tint [0.62 0.03 0.43] :chance 0.385 :fade 1.5 :level 0.3}}
            {:id :bloom :cue-id :bloom-halo
             :params {:tint [0.73 0.18 0.0] :width 0.445 :halo 0.845 :beats 4.0 :level 0.2}}
            {:id :flakes :cue-id :confetti
             :params {:min-hue 210.0 :max-hue 260.0 :min-sat 0.3 :max-sat 0.7
                      :min-dur 1.0 :max-dur 4.0 :step-beats 1.0}}]}
   {:id :strobe-chase :mode :sequence :name "Strobe chase" :loop :ping-pong :timebase :beats
    :legacy "tolglow.page \"Chase Test 2\": strobe one fixture group per step, bouncing"
    :slots (vec (mapcat (fn [[id targets]]
                          [{:id id :cue-id :strobe :duration {:beats 1/2}
                            :launch {:targets targets} :params {:rate 4.0 :duty 0.5 :lightness 0.85}}
                           {:id (keyword (str (name id) "-gap")) :kind :empty :duration {:beats 1/2}}])
                        [[:beams rig/beams] [:movers rig/movers] [:washes rig/washes] [:tubes rig/tubes]]))}])

(def phrase-accents
  "An event subscriber: every phrase boundary fires a blinder hit, and three
  bars later a one-bar dimmer sweep or halo bloom. The show's seed decides
  which."
  {:id :phrase-accents :enabled? true
   :match {:type :clock/phrase :category :clock}
   :actions [{:op :cue/launch :select {:ids [:blinder-hit] :pick :first}
              :launch {:mode :one-shot :duration {:beats 2}}}
             {:op :cue/launch :select {:ids [:dimmer-sweep :bloom-halo] :pick :random}
              :delay {:bars 3} :launch {:mode :one-shot :duration {:bars 1}}}]})

(defn- setup-command [model payload now]
  (spatial/apply-command
   model (merge {:command-id (str "showcase/setup/" (:revision model)) :show-id (:show-id model)
                 :epoch (:epoch model) :source {:adapter :showcase :session-id "setup"} :time-ms now}
                payload)))

(defn- publish [model kind definition]
  (update model :registry registry/publish kind definition))

(defn- with-rolling-volume
  "Register the volume cue on the grid. Spawning also launches it, so that one
  instance is removed again and the cue waits for the timeline."
  [model]
  (let [{:keys [model ack]} (spatial-cues/spawn model rolling-volume 0 setup-command)]
    (:model (setup-command model {:op :instance/remove :instance-id (:instance-id ack)} 0))))

(defn- seeded [seed effect]
  (cond-> effect (get-in effect [:params :seed]) (assoc-in [:params :seed :default] (double seed))))

(defn- page
  "Six columns in grid order. The rolling volume's own cell stays free for it."
  [cue-ids]
  {:id :showcase :name "Showcase"
   :cells (zipmap (remove #{(:cell rolling-volume)} (for [row (range) column (range 6)] [column row]))
                  cue-ids)})

(defn model
  "The complete show model. `:seed` changes every seeded pattern together:
  sparkle and confetti draws, the searchlight wander, the colour drift, the
  strobe chase order and the subscriber's random picks."
  ([] (model {}))
  ([{:keys [seed] :or {seed default-seed}}]
   (when-not (and (integer? seed) (<= 0 seed 9999))
     (throw (ex-info "The showcase seed is an integer from 0 to 9999" {:seed seed})))
   (let [effects (-> (mapv (comp (partial seeded seed) :effect) looks/catalogue)
                     (conj (looks/colour-drift seed)))
         cues (conj (mapv looks/cue looks/catalogue)
                    {:id :colour-drift :version 1 :name "Colour drift" :effect-id :colour-drift
                     :swatch "#b46ad8" :tags [:modulation] :params {} :priority 0
                     :reload-policy :preserve-state})
         base (control-model/new-show
               {:show-id :showcase :rig (rig/rig)
                :transport {:bpm bpm :playing? true :beat 0.0 :meter meter}
                :waveforms (signals/publish (signals/new-registry) looks/heartbeat)
                :settings {:hdr hdr-settings :spatial spatial-settings}
                :event-subscribers {:subscribers [] :runs [] :next-run 0 :seed seed :dropped 0}})
         heard (audio-lfo/configure base (select-keys looks/bass-lfo [:lfo-id :name :config]))
         typed (reduce #(publish %1 :fixtures %2) heard (vals rig/fixture-types))
         lit (reduce #(publish %1 :cues %2) (reduce #(publish %1 :effects %2) typed effects) cues)
         path (aim-paths/definition lit gather-path)
         moving (-> (publish lit :effects path)
                    (publish :cues {:id :gather-path :version 1 :name (:name path) :effect-id :gather-path
                                    :swatch "#3fb8c4" :tags [:movement] :params {} :priority 14
                                    :reload-policy :preserve-state}))
         paged (assoc-in moving [:pages :showcase]
                         ;; The tolglow-web cues follow every earlier cue, so those
                         ;; keep the cells they had before the after-hours cues.
                         (page (let [ids (mapv :id looks/catalogue)]
                                 (concat (remove looks/after-hours-cues ids) [:gather-path :colour-drift]
                                         (filter looks/after-hours-cues ids)))))
         composed (reduce (fn [m definition]
                            (composition/save m (cond-> (dissoc definition :legacy)
                                                  (= :sequence (:mode definition)) (assoc :seed seed))))
                          (with-rolling-volume paged) macros)]
     ;; Start in a hazed room. Preview playback then looks the same from the
     ;; first frame without a minute of pump time.
     (assoc-in composed venue-fog/saved-path {:mixed-m3 62.0 :sources []}))))

;; ------------------------------------------------------------------- timeline

(defn- launch
  ([bar cue-id] (launch bar cue-id {}))
  ([bar cue-id {:keys [params sources? mode duration beat] :or {beat 0}}]
   (cond-> {:at {:bar bar :beat beat} :op :cue/launch :cue-id cue-id
            :launch (cond-> {} params (assoc :params params)
                      mode (assoc :mode mode) duration (assoc :duration duration))}
     sources? (assoc :sources (get looks/sources cue-id)))))

(defn- end [bar & cue-ids]
  (mapv (fn [cue-id] {:at {:bar bar} :op :instance/end :cue-id cue-id}) cue-ids))

(defn- ramp [bar cue-id param to over curve]
  {:at {:bar bar} :op :param/ramp :cue-id cue-id :param param :to to :over over :curve curve})

(def timeline
  "Eight four-bar phrases. Bars are zero-based transport positions."
  {:id :first-light :name "First light" :bpm bpm :meter meter :length {:bar 32}
   :events
   (vec
    (flatten
     [;; Phrase 1, bars 0-3: the room wakes. Haze, a slow ribbon, a low wash.
      (launch 0 :haze {:sources? true})
      (launch 0 :aurora {:sources? true})
      (launch 0 :wash-tide {:sources? true})
      (launch 2 :tube-sparkle {:params {:chance 0.1 :fade 1.4}})
      ;; Phrase 2, bars 4-7: beams arrive on a heartbeat and start to wander.
      (launch 4 :beam-shafts {:sources? true})
      (launch 4 :aim-lfo)
      (launch 4 :bloom-halo {:params {:beats 8.0 :width 0.2 :halo 0.9}})
      (ramp 6 :tube-sparkle :chance 0.45 {:bars 2} :ease-in)
      ;; Phrase 3, bars 8-11: groove. Floor heads orbit, tubes climb, the wash
      ;; weaves between two palettes and the beam hue starts to drift.
      (end 8 :bloom-halo)
      (launch 8 :mover-orbit {:sources? true})
      (launch 8 :wash-ember {:sources? true})
      (launch 8 :riser-chase {:sources? true :params {:beats 2.0}})
      (launch 8 :colour-drift)
      (launch 10 :bloom-keyhole {:params {:beats 4.0 :width 0.45 :keyhole-opacity 0.85}})
      ;; Phrase 4, bars 12-15: the operator's "mixed movement" macro takes the beams.
      (end 12 :aim-lfo :beam-shafts :colour-drift)
      {:at {:bar 12} :op :macro/run :macro-id :mixed-movement}
      (ramp 12 :can-can :spread 40.0 {:bars 3} :ease-in-out)
      (launch 14 :dimmer-sweep {:params {:beats 2.0 :width 0.6 :fade 0.6}})
      (ramp 14 :dimmer-sweep :width 0.25 {:beats 6} :linear)
      ;; Phrase 5, bars 16-19: breakdown and build. The beams gather on one
      ;; travelling point, a warm volume rolls along the tubes and a strobe
      ;; accelerates into a half-bar of darkness.
      {:at {:bar 16} :op :macro/stop :macro-id :mixed-movement}
      (end 16 :dimmer-sweep :bloom-keyhole :riser-chase :wash-ember :mover-orbit)
      (launch 16 :beam-shafts {:params {:hue 0.58 :saturation 0.35 :level 0.7}})
      (launch 16 :gather-path)
      {:at {:bar 16} :op :cue/launch :cue-id :rolling-volume :launch {:targets rig/tubes}}
      (ramp 16 :tube-sparkle :chance 0.08 {:bars 1} :ease-out)
      (ramp 16 :beam-shafts :saturation 0.95 {:bars 3} :ease-in)
      (launch 18 :strobe {:params {:rate 1.0 :duty 0.3 :level 0.25 :lightness 0.9}
                          :mode :one-shot :duration {:bars 2}})
      (ramp 18 :strobe :rate 8.0 {:beats 6} :ease-in)
      (ramp 18 :strobe :level 1.0 {:beats 6} :ease-in)
      {:at {:bar 19 :beat 2} :op :master/set :value 0.0}
      ;; Phrase 6, bars 20-23: the drop. Blinders past full scale, the "heavy
      ;; colour" macro on the pixels, a fast can-can and overdriven risers.
      ;; The aurora steps aside so its cool gradient does not mix the ribbon
      ;; toward white under the macro's warm layers.
      {:at {:bar 20} :op :master/set :value 1.0}
      (end 20 :gather-path :rolling-volume :beam-shafts :tube-sparkle :aurora)
      (launch 20 :blinder-hit {:params {:drive 3.0 :decay 1.2} :mode :one-shot :duration {:beats 3}})
      {:at {:bar 20} :op :macro/run :macro-id :heavy-colour}
      (launch 20 :pinstripes {:params {:color-1 [1.0 0.06 0.02] :color-2 [1.0 0.5 0.04] :beats 1.0 :fade 0.3}})
      (launch 20 :can-can {:params {:bars 1.0 :stagger 2.4 :spread 34.0 :tilt-min -35.0 :tilt-max 72.0}})
      (launch 20 :riser-chase {:sources? true :params {:beats 0.5 :bands 1.5 :drive 1.9}})
      (launch 20 :wash-ember {:params {:level 0.9}})
      (launch 20 :mover-orbit {:sources? true :params {:tint [1.0 0.16 0.04] :bars 2.0}})
      {:at {:bar 20} :op :subscriber/save :subscriber phrase-accents}
      ;; Phrase 7, bars 24-27: the drop opens out, then the strobe chase bounces.
      (end 24 :can-can)
      (launch 24 :aim-lfo {:params {:x-bars 1.0 :y-bars 0.75 :z-bars 2.0 :fan 1.0 :x-min -3.0 :x-max 3.0}})
      {:at {:bar 24} :op :modulation/bind :cue-id :wash-ember :param :layer/priority
       :binding (get-in looks/sources [:wash-ember :layer/priority])}
      (ramp 24 :riser-chase :drive 1.2 {:bars 3} :ease-out)
      {:at {:bar 26} :op :macro/run :macro-id :strobe-chase}
      ;; Phrase 8, bars 28-31: everything lets go, down to ribbon and haze.
      {:at {:bar 28} :op :subscriber/remove :id :phrase-accents}
      {:at {:bar 28} :op :macro/stop :macro-id :strobe-chase}
      {:at {:bar 28} :op :macro/stop :macro-id :heavy-colour}
      (end 28 :pinstripes :aim-lfo :riser-chase :mover-orbit :wash-ember)
      (launch 28 :beam-shafts {:sources? true :params {:hue 0.62 :saturation 0.6}})
      (launch 28 :bloom-halo {:params {:beats 16.0 :width 0.35 :halo 1.2 :tint [0.3 0.1 0.9]}})
      (launch 28 :aurora {:sources? true})
      (ramp 28 :aurora :wavelength 14.0 {:bars 2} :ease-in-out)
      (end 30 :beam-shafts :bloom-halo)
      (end 31 :aurora :wash-tide :haze)]))})

(def after-hours
  "Four phrases for the cues mined from tolglow-web: searchlights wander over
  a matrix chase, the floor cones pick up the bass, and one control drives the
  beams and washes from a dim wash to a white strobe while the beams keep
  wandering, before the blinders take the hit."
  {:id :after-hours :name "After hours" :bpm bpm :meter meter :length {:bar 16}
   :events
   (vec
    (flatten
     [;; Phrase 1, bars 0-3: a hazed room, beams searching, columns ticking by.
      (launch 0 :haze {:sources? true})
      (launch 0 :wash-tide {:params {:level 0.3}})
      (launch 0 :searchlight-drift)
      (launch 0 :matrix-chase {:params {:step 0.5 :tail 3.0}})
      (launch 2 :twin-cones {:sources? true})
      ;; Phrase 2, bars 4-7: the chase turns from columns through diagonals to
      ;; rows, and the run starts its build under everything.
      (ramp 4 :matrix-chase :angle 90.0 {:bars 4} :ease-in-out)
      (launch 4 :run {:params {:drive 0.0}})
      (ramp 4 :run :drive 1.0 {:bars 8} :ease-in)
      ;; Phrase 3, bars 8-11: tighter steps, the searchlights narrow their box.
      (end 8 :wash-tide)
      ;; The bands keep their phase through the edit and double their speed.
      {:at {:bar 8} :op :param/set :cue-id :matrix-chase :param :step :value 0.25}
      (ramp 8 :searchlight-drift :extent [2.0 0.4 1.0] {:bars 4} :ease-in)
      ;; Phrase 4, bars 12-15: the hit. The run lets go, the blinders fire
      ;; past full scale and the chase runs diagonals on its own.
      (end 12 :run)
      (launch 12 :blinder-hit {:params {:drive 2.5 :decay 1.0} :mode :one-shot :duration {:beats 3}})
      {:at {:bar 12} :op :param/set :cue-id :matrix-chase :param :angle :value 45.0}
      (end 14 :twin-cones :searchlight-drift)
      (end 15 :matrix-chase :haze)]))})

(def timelines
  "Timelines by ID, for the `:showcase-timeline` module's `:timeline` option."
  {:first-light timeline :after-hours after-hours})

;; --------------------------------------------------------------------- player

(defn- origin-for
  "The next bar line at least two beats away. The owner applies a post on its
  next cycle, and every posted position must still be ahead of the transport."
  [beat]
  (let [bar (:beats-per-bar meter)]
    (* bar (Math/ceil (/ (+ beat 2.0) bar)))))

(defn- post-pass!
  "Hand one pass of scheduler events to the show owner."
  [runtime player origin pass]
  (let [receipts (mapv #(show/submit! runtime %) (timeline/posts (:prepared @player) origin pass))]
    (swap! player #(-> % (assoc :posted-pass pass) (update :receipts into receipts)))))

(defn- unhappy-outcomes
  "Scheduled events of this timeline that did not simply fire or complete."
  [prepared model]
  (let [prefix (str (name (:id prepared)) "/")]
    (into #{}
          (comp (filter #(and (string? (:event-id %)) (.startsWith ^String (:event-id %) prefix)
                              (not (#{:fired :completed} (:status %)))))
                (map #(select-keys % [:event-id :status :reason :message])))
          (get-in model [:event-subscribers :recent]))))

(defn- on-frame!
  "Runs under the show owner after each committed frame. It reads the model,
  submits the direct commands the next frame needs and returns. The next owner
  cycle applies them in order. A looping player posts the following pass one
  bar before the current one ends."
  [runtime player committed]
  (let [{:keys [prepared loop? state previous-beat posted-pass]} @player
        model (:model committed)
        beat (get-in model [:transport :beat])
        horizon (+ beat (if previous-beat (max 0.0 (- beat previous-beat)) 0.0))
        {:keys [state operations]} (timeline/advance prepared state model horizon)
        receipts (mapv (fn [[type payload]] (show/submit! runtime type payload)) operations)
        following (+ (:origin state) (:length prepared))
        post-next? (and loop? (= posted-pass (:pass state))
                        (>= (- horizon (:origin state)) (- (:length prepared) (:beats-per-bar meter))))
        state (if (and loop? (:done? state)) (timeline/start following (inc (:pass state))) state)]
    (swap! player (fn [current]
                    (-> current
                        (assoc :state state :previous-beat beat)
                        (update :receipts into receipts)
                        (update :outcomes into (unhappy-outcomes prepared model)))))
    (when post-next? (post-pass! runtime player following (inc posted-pass)))))

(defn- settle-receipts [player]
  (swap! player
         (fn [{:keys [receipts] :as current}]
           (let [{done true pending false} (group-by realized? receipts)
                 rejected (remove #(= :accepted (:status @%)) done)]
             (-> current
                 (assoc :receipts (vec pending))
                 (update :accepted + (- (count done) (count rejected)))
                 (update :rejected into (map deref) rejected))))))

(defn play!
  "Play the timeline on a running or manually ticked show. Its scheduler events
  are posted at once and its direct commands follow frame by frame. It starts
  on the next bar line at least two beats ahead. Options: `:timeline` (default
  `timeline`), `:loop?` (default false) and `:origin`, an explicit transport
  beat for timeline beat 0 that must not be behind the transport when the
  owner next runs. Returns the player. Starting twice returns the first player."
  ([runtime] (play! runtime {}))
  ([runtime {:keys [loop?] :as options}]
   (let [model (:model @(:state runtime))
         prepared (timeline/prepare (:timeline options timeline) model)
         origin (double (:origin options (origin-for (get-in model [:transport :beat]))))
         created (atom {:prepared prepared :loop? (boolean loop?) :state (timeline/start origin 0)
                        :previous-beat nil :posted-pass nil :receipts [] :accepted 0 :rejected []
                        :outcomes #{}})
         player (:player (show/attach-observer!
                          runtime ::timeline
                          (fn [] {:player created
                                  :events! (fn [& _])
                                  :tracing? (constantly false)
                                  :frame! (fn [committed]
                                            (settle-receipts created)
                                            (on-frame! runtime created committed))
                                  :close! (fn [])})))]
     (when (identical? player created) (post-pass! runtime player origin 0))
     player)))

(defn status
  "Where the player is: timeline beat and bar, pass, the operations it has had
  accepted or rejected, the direct commands that never found their cue, and
  any scheduled event that was rejected, missed or cancelled."
  [player]
  (let [{:keys [state previous-beat accepted rejected receipts prepared outcomes]} (settle-receipts player)
        local (when previous-beat (- previous-beat (:origin state)))]
    {:timeline (:id prepared) :pass (:pass state) :done? (boolean (:done? state))
     :beat local :bar (when local (/ local (:beats-per-bar meter)))
     :accepted accepted :pending (count receipts) :rejected rejected
     :missed (:missed state []) :outcomes outcomes}))

(defn stop!
  "Detach the player, withdraw what it still has scheduled and release every
  instance the timeline launched. Returns the final status."
  [runtime player]
  (show/detach-observer! runtime ::timeline)
  (let [model (:model @(:state runtime))
        source (timeline/source (get-in @player [:prepared :id]))]
    (when (seq (filter :scheduled (get-in model [:event-subscribers :runs])))
      (show/cancel-events! runtime {:posted-by source}))
    (doseq [id (keys (:composition-runs model))]
      (show/submit! runtime :composition-stop {:id id}))
    (doseq [{:keys [id source]} (vals (:instances model))
            :when (#{:timeline :composition} (:adapter source))]
      (show/submit! runtime {:op :instance/end :instance-id id})))
  (status player))

;; --------------------------------------------------------------------- replay

(defn- capture-sink [frames]
  {:description {:type :capture :network false}
   :send! (fn [universes frame-number]
            (swap! frames conj
                   {:frame frame-number
                    :universes (into (sorted-map)
                                     (map (fn [[universe data]] [universe (vec (map #(bit-and 255 %) data))]))
                                     universes)}))
   :close! (fn [])})

(defn replay
  "Play the timeline on a private show with a manual clock and a capture sink.
  Nothing leaves the process. Returns the captured DMX frames, the player's
  final status and, when `:snapshot!` is given, whatever it returned for each
  frame's committed snapshot.

  Options: `:seed`, `:frame-ms` (default 25, the show's 40 Hz), `:from-bar` and
  `:to-bar` (default the whole timeline; frames before `:from-bar` still run
  but are not kept), `:snapshot!` and `:timeline`."
  ([] (replay {}))
  ([{:keys [seed frame-ms from-bar to-bar snapshot!] :or {seed default-seed frame-ms 25 from-bar 0} :as options}]
   (let [source (:timeline options timeline)
         beats-per-ms (/ (:bpm source) 60000.0)
         bar-ms (/ (:beats-per-bar (:meter source)) beats-per-ms)
         last-bar (or to-bar (/ (timeline/position->beats (:length source) (:meter source))
                                (:beats-per-bar (:meter source))))
         frames (atom [])
         runtime (show/create-runtime {:model (model {:seed seed}) :workspace-options nil
                                       :worker-timeout-ms 60000 :sink (capture-sink frames)})]
     (try
       (let [player (play! runtime {:timeline source :origin 0.0})
             kept (loop [t 0 kept []]
                    (if (> t (+ (* last-bar bar-ms) frame-ms))
                      kept
                      (do (show/tick! runtime t)
                          (recur (+ t frame-ms)
                                 (if (and snapshot! (>= t (* from-bar bar-ms)))
                                   (conj kept (snapshot! (show/snapshot runtime)))
                                   kept)))))
             from-frame (long (Math/ceil (/ (* from-bar bar-ms) frame-ms)))]
         {:dmx (subvec @frames (min from-frame (count @frames)))
          :status (status player)
          :snapshots kept})
       (finally (show/stop! runtime))))))

;; --------------------------------------------------------------------- config

(defn showcase-timeline
  "Application module. With `:autoplay` it starts the timeline when the show
  starts, and `:loop` keeps it cycling. `:timeline` names one of `timelines`
  (default `:first-light`)."
  [context {:keys [autoplay loop timeline] :or {autoplay true loop true timeline :first-light}}]
  (let [runtime (or (get-in context [:modules :show])
                    (throw (ex-info "Place :show before :showcase-timeline" {})))
        source (or (get timelines timeline)
                   (throw (ex-info "Unknown showcase timeline" {:timeline timeline :known (set (keys timelines))})))
        player (when autoplay (play! runtime {:timeline source :loop? loop}))]
    {:value {:player player}
     :stop! (fn [] (show/detach-observer! runtime ::timeline))}))

(def config
  "Application config for `lein run --config tolglow.showcase/config`. The
  shipped defaults stay in force: a 40 Hz preview show with Studio on loopback
  port 8500 and no physical output."
  (-> (data/read-edn (slurp (io/resource "tolglow.edn")))
      (assoc :description "Tolglow showcase"
             :show {:model (model)}
             :showcase-timeline {:enabled true :autoplay true :loop true})
      (assoc-in [:init :search-in-namespaces] ['tolglow.showcase])
      (update-in [:init :post] conj :showcase-timeline)))
