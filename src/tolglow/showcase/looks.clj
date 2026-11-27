(ns tolglow.showcase.looks
  "The showcase cue library as portable effect and cue definitions.

  Every look is plain numeric IR built with `tolglow.effects`. There are no
  clocks, atoms or random draws in here. Patterns that used a random source in
  the original show hash the head position, a musical step and a `:seed`
  parameter, so one seed always gives the same frames.

  `catalogue` lists each cue with its category and where it came from. The
  ported looks name the original effect they restate."
  (:require [tolglow.audio-lfo :as audio-lfo]
            [tolglow.effects :as fx]
            [tolglow.registry :as registry]
            [tolglow.showcase.rig :as rig]
            [tolglow.signals :as signals]))

(def ^:private tau (* 2.0 Math/PI))

(defn- op [operator & args] {:op operator :args (mapv fx/expression args)})

(def ^:private beat
  "Transport beat. Patterns stay locked to the bar whenever a cue is launched."
  {:op :beat :clock :absolute})

(def ^:private bar {:op :bar :clock :absolute})

(def ^:private elapsed-beats {:op :beat})

(def ^:private x (fx/position :x))

(def ^:private y (fx/position :y))

(def ^:private z (fx/position :z))

(def ^:private p fx/param)

(defn- fract [value] (op :fract value))

(defn- floor [value] (op :floor value))

(defn- absolute [value] (op :abs value))

(defn- sine
  "Unit sine over `cycles`: 0.5 rising at zero, 1 at a quarter cycle."
  [cycles]
  (fx/add 0.5 (fx/multiply 0.5 (op :sin (fx/multiply tau cycles)))))

(defn- triangle
  "Unit triangle over `cycles`: 0 at zero, 1 at half a cycle."
  [cycles]
  (fx/subtract 1.0 (absolute (fx/subtract (fx/multiply 2.0 (fract cycles)) 1.0))))

(defn- at-least
  "1 when value >= edge, otherwise 0."
  [value edge]
  (floor (fx/clamp (fx/add 1.0 (fx/subtract value edge)) 0.0 1.0)))

(defn- noise
  "A repeatable value in [0,1) from the sum of `terms`."
  [& terms]
  (fract (fx/multiply 43758.5453 (op :sin (apply fx/add terms)))))

(def ^:private head-hash
  (fx/add (fx/multiply x 12.9898) (fx/multiply y 78.233) (fx/multiply z 37.719)))

(defn- degrees [value] (fx/multiply value (/ Math/PI 180.0)))

(defn hue->rgb
  "Fully saturated linear RGB for a hue in turns. Hues wrap."
  [hue]
  (let [six (fx/multiply 6.0 (fract hue))]
    (fx/rgb (fx/clamp (fx/subtract (absolute (fx/subtract six 3.0)) 1.0))
            (fx/clamp (fx/subtract 2.0 (absolute (fx/subtract six 2.0))))
            (fx/clamp (fx/subtract 2.0 (absolute (fx/subtract six 4.0)))))))

(defn- tinted
  "Mix white toward the pure hue by `saturation`."
  [hue saturation]
  (fx/mix [1.0 1.0 1.0] (hue->rgb hue) saturation))

(def ^:private gradient-schema {:type :gradient :default [{:position 0.0 :color [0.0 0.0 0.0]}]})

(defn gradient
  "Lower typed gradient stops into a piecewise-linear colour over `t` in [0,1].

  `stops` use the typed gradient parameter shape: ordered
  `{:position p :color [r g b]}` maps with strictly increasing positions. The
  stops are baked into the expression when the effect is authored; use
  `fx/gradient` to sample an editable `:gradient` parameter instead."
  [stops t]
  (when-not (registry/valid-value? gradient-schema stops)
    (throw (ex-info "Gradient stops must be ordered {:position :color} maps" {:stops stops})))
  (reduce (fn [colour [{from :position} {to :position target :color}]]
            (fx/mix colour target (fx/clamp (fx/divide (fx/subtract t from) (- to from)))))
          (fx/constant (:color (first stops)))
          (partition 2 1 stops)))

(def ^:private shutter-open
  "DMX 12 sits in the beam's 8..15 open band. Its shutter rests closed."
  (fx/output :shutter (/ 12.0 255.0) {:targets rig/beams}))

(defn- look [id options & layers]
  (apply fx/effect id (merge {:duration {:bars 4}} options) layers))

(def ^:private soft {:attack {:beats 1} :release {:beats 2}})

(defn- soft-colour [attack release]
  {:attack {:beats attack} :release {:beats release} :attributes [:color]})

;; ---------------------------------------------------------------- atmosphere

(def haze
  (look :haze {:name "Stage haze" :targets [rig/hazer] :params {:level [0.55 0.0 1.0]}}
        (fx/output :fog (fx/clamp (p :level)))))

;; -------------------------------------------------------------------- colour

(def beam-shafts
  (look :beam-shafts
        {:name "Beam shafts" :targets rig/beams :envelope soft
         :params {:level [0.85 0.0 1.0] :hue [0.72 0.0 1.0] :saturation [0.92 0.0 1.0]}}
        (fx/color (tinted (p :hue) (p :saturation)))
        (fx/intensity (p :level))
        shutter-open))

(def tide-stops
  [{:position 0.0 :color [0.02 0.05 0.55]}
   {:position 0.5 :color [0.28 0.03 0.75]}
   {:position 1.0 :color [0.75 0.02 0.42]}])

(def wash-tide
  (look :wash-tide
        {:name "Wash tide" :targets rig/washes :envelope soft
         :params {:level [0.5 0.0 1.0] :shift [0.0 -0.5 0.5]}}
        (fx/color (gradient tide-stops (fx/clamp (fx/add 0.5 (fx/divide x 10.4) (p :shift)))))
        (fx/intensity (p :level))))

(def ember-stops
  [{:position 0.0 :color [1.0 0.32 0.02]}
   {:position 0.6 :color [0.95 0.08 0.01]}
   {:position 1.0 :color [0.6 0.0 0.05]}])

(def wash-ember
  (look :wash-ember
        {:name "Ember weave" :targets rig/washes :envelope soft
         :params {:level [0.55 0.0 1.0]}}
        (fx/color (gradient ember-stops (fx/clamp (fx/divide (absolute x) 5.0))))
        (fx/intensity (p :level))))

(def pinstripes
  "Port of `tolglow.fx/pinstripes` and the `tolglow.cue/pinstripe` cues: heads
  fall into columns by X, alternate two colours and rotate one column per
  step. `:fade` is the fraction of each step spent crossing to the next colour."
  (let [column (floor (fx/add 0.5 (fx/divide x (p :pitch))))
        step (fx/divide beat (p :beats))
        moved (fx/add (floor step)
                      (fx/clamp (fx/divide (fx/subtract (fract step) (fx/subtract 1.0 (p :fade)))
                                           (fx/add (p :fade) 1.0e-6))))
        phase (fx/multiply 2.0 (fract (fx/multiply 0.5 (fx/add column moved))))
        amount (fx/subtract 1.0 (absolute (fx/subtract phase 1.0)))]
    (look :pinstripes
          {:name "Pinstripes" :targets rig/movement :envelope soft
           :params {:color-1 (fx/parameter [0.03 0.28 0.98]) :color-2 (fx/parameter [0.76 0.12 0.07])
                    :beats [4.0 0.25 16.0] :fade [0.0 0.0 1.0] :pitch [1.6 0.1 6.0]
                    :level [0.9 0.0 1.0]}}
          (fx/color (fx/mix (p :color-1) (p :color-2) amount))
          (fx/intensity (p :level))
          shutter-open)))

(defn- bloom-band
  "The legacy bloom measure: distance from a centre in the X/Y plane, as a
  fraction of `:reach`. The band sits at an oscillating fraction of that
  distance, is `:width` wide and falls off linearly over `:halo` widths."
  []
  (let [dx (fx/subtract x (p :center-x))
        dy (fx/subtract y (p :center-y))
        distance (fx/divide (op :sqrt (fx/add (fx/multiply dx dx) (fx/multiply dy dy))) (p :reach))
        fraction (fx/mix (p :min) (p :max) (sine (fx/subtract (fx/divide beat (p :beats)) 0.25)))
        outside (fx/subtract (absolute (fx/subtract distance fraction)) (fx/multiply 0.5 (p :width)))]
    (fx/clamp (fx/subtract 1.0 (fx/divide outside (fx/add (fx/multiply (p :halo) (p :width)) 1.0e-4))))))

(def ^:private bloom-params
  {:beats [8.0 0.25 64.0] :min [0.0 0.0 1.5] :max [1.0 0.0 1.5] :width [0.25 0.0 1.0] :halo [0.5 0.0 2.0]
   :center-x [0.0 -5.5 5.5] :center-y [0.3 0.0 5.0] :reach [4.8 0.5 12.0]})

(def bloom-halo
  "Port of `tolglow.fx/bloom` with a halo, as on the `tolglow.cue/bloom` pads.
  The band takes the bloom colour and the halo fades to whatever is beneath."
  (look :bloom-halo
        {:name "Halo bloom" :targets rig/pixels :envelope (soft-colour 1/2 1)
         :params (merge bloom-params {:tint (fx/parameter [0.87 0.46 0.02]) :level [0.2 0.0 1.0]})}
        (fx/color (fx/multiply (p :tint) (bloom-band) (p :level)) {:blend :max})))

(def bloom-keyhole
  "Port of the `:keyhole? true` bloom and the `:bloom-keyhole` cue: the band
  is a window onto the lighting beneath, and everything outside it is dimmed
  to `1 - :keyhole-opacity`. Authored as a native layer modifier, so it masks
  whichever cues are running under it."
  (let [mask (fx/mix (fx/subtract 1.0 (p :keyhole-opacity)) 1.0 (bloom-band))]
    (look :bloom-keyhole
          {:name "Keyhole bloom" :targets rig/coloured
           :params (assoc bloom-params :width [0.3 0.0 1.0] :keyhole-opacity [0.8 0.0 1.0])
           :layer {:transforms [{:attribute :color
                                 :value {:op :multiply
                                         :args [{:op :previous :attribute :color :default [0.0 0.0 0.0]}
                                                {:op :incoming :attribute :color}]}}]}}
          (fx/color (fx/rgb mask mask mask)))))

;; ----------------------------------------------------------------- intensity

(def dimmer-sweep
  "Port of `tolglow.fx/sweep` and the `tolglow.cue/sweep-column` pads: a bar of
  light travels across the heads by X and holds everything else dark."
  (let [low (first rig/beam-xs)
        span (- (peek rig/beam-xs) low)
        position (fx/add low (fx/multiply span (sine (fx/subtract (fx/divide beat (p :beats)) 0.25))))
        reach (fx/add (fx/multiply 0.5 span (p :width)) 1.0e-4)
        inside (fx/subtract reach (absolute (fx/subtract position x)))]
    (look :dimmer-sweep
          {:name "Dimmer sweep" :targets rig/movement
           :params {:beats [2.0 0.25 32.0] :width [0.3 0.0 1.0] :level [1.0 0.0 1.0] :fade [1.0 0.0 1.0]}}
          (fx/intensity (fx/multiply (p :level) (at-least inside 0.0)
                                     (fx/mix 1.0 (fx/divide inside reach) (p :fade)))))))

(def strobe
  "Port of `tolglow.fx/color-strobe` and the `tolglow.cue/strobe` pads. The
  original drove each fixture's free-running strobe channel. This one gates
  emission on the beat clock, `:rate` flashes per beat, so it stays in time
  and replays exactly."
  (let [gate (at-least (p :duty) (fract (fx/multiply beat (p :rate))))]
    (look :strobe
          {:name "Colour strobe" :targets rig/coloured :duration {:bars 1}
           :params {:tint (fx/parameter [0.42 0.27 0.79]) :lightness [0.6 0.0 1.0]
                    :rate [4.0 0.5 8.0] :duty [0.35 0.05 1.0] :level [1.0 0.0 1.0]}}
          (fx/color (fx/multiply (fx/mix (p :tint) [1.0 1.0 1.0] (p :lightness)) gate (p :level)))
          (fx/intensity 1.0 {:targets (-> rig/beams (into rig/movers) (into rig/washes))})
          shutter-open)))

(def blinder-hit
  "A warm audience blinder that decays from the launch. `:drive` is read by the
  show's HDR settings as an emission gain, so the hit runs past full scale and
  the excess whitens the source."
  (let [decay (op :exp (fx/multiply -1.0 (p :decay) elapsed-beats))]
    (look :blinder-hit
          {:name "Blinder hit" :targets rig/blinders :duration {:beats 2}
           :params {:level [1.0 0.0 1.0] :warmth [0.45 0.0 1.0] :decay [1.6 0.0 8.0] :drive [2.0 1.0 4.0]}}
          (fx/output :white (fx/clamp decay))
          (fx/output :amber (fx/clamp (fx/multiply (p :warmth) decay)))
          (fx/intensity (p :level)))))

;; ------------------------------------------------------------------ movement

(def aim-lfo
  "Port of `tolglow.cue/pointing-lfo` as saved in the \"all aim lfo\" macro:
  three independent sine oscillators on the aim point's X, Y and Z with bar
  periods of 4, 3 and 8. `:fan` spreads the heads by their own X so the beams
  open into a fan around the moving point."
  (let [axis (fn [low high bars] (fx/mix (p low) (p high) (sine (fx/divide bar (p bars)))))]
    (look :aim-lfo
          {:name "Aim LFO" :targets rig/beams
           :params {:x-min [-2.0 -6.0 6.0] :x-max [2.0 -6.0 6.0] :x-bars [4.0 0.25 32.0]
                    :y-min [0.0 0.0 5.0] :y-max [1.6 0.0 5.0] :y-bars [3.0 0.25 32.0]
                    :z-min [0.5 -4.0 6.0] :z-max [3.4 -4.0 6.0] :z-bars [8.0 0.25 32.0]
                    :fan [0.6 0.0 2.0]}}
          (fx/aim (fx/xyz (fx/add (axis :x-min :x-max :x-bars) (fx/multiply (p :fan) x))
                          (axis :y-min :y-max :y-bars)
                          (axis :z-min :z-max :z-bars))))))

(def can-can
  "Port of `tolglow.fx/can-can` (the `:move-all` pads): pan and tilt sines with
  a per-head phase `:stagger` and a pan `:spread` across the row. Tilt runs four
  times slower than pan, as in the original. Angles are degrees from straight
  down, positive tilt toward the audience."
  (let [row (fx/divide (fx/subtract x (first rig/beam-xs)) 9.6)
        phase (fx/multiply (p :stagger) row)
        pan (degrees (fx/add (fx/mix (p :pan-min) (p :pan-max) (sine (fx/add (fx/divide bar (p :bars)) phase)))
                             (fx/multiply (p :spread) (fx/divide x (peek rig/beam-xs)))))
        tilt (degrees (fx/mix (p :tilt-min) (p :tilt-max)
                              (sine (fx/add (fx/divide bar (fx/multiply 4.0 (p :bars))) phase))))]
    (look :can-can
          {:name "Can-can" :targets rig/beams
           :params {:bars [2.0 0.25 16.0] :stagger [1.94 0.0 4.0] :spread [21.0 -45.0 45.0]
                    :pan-min [-42.0 -180.0 180.0] :pan-max [23.0 -180.0 180.0]
                    :tilt-min [-60.0 -90.0 90.0] :tilt-max [80.0 -90.0 90.0]}}
          (fx/output :direction (fx/xyz (fx/multiply (op :sin tilt) (op :sin pan))
                                        (fx/multiply -1.0 (op :cos tilt))
                                        (fx/multiply (op :sin tilt) (op :cos pan)))))))

(def mover-orbit
  "The floor heads chase one another around a circle above the stage."
  (let [phase (fx/multiply tau (fx/add (fx/divide bar (p :bars)) (fx/divide (fx/subtract x (first rig/mover-xs)) 9.6)))]
    (look :mover-orbit
          {:name "Mover orbit" :targets rig/movers :envelope soft
           :params {:tint (fx/parameter [0.1 0.55 0.95]) :level [0.8 0.0 1.0]
                    :radius [1.6 0.0 4.0] :height [3.4 0.5 5.0] :depth [-0.2 -4.0 4.0] :bars [4.0 0.25 32.0]}}
          (fx/color (p :tint))
          (fx/intensity (p :level))
          (fx/aim (fx/xyz (fx/multiply (p :radius) (op :cos phase))
                          (p :height)
                          (fx/add (p :depth) (fx/multiply (p :radius) (op :sin phase))))))))

;; --------------------------------------------------------------------- pixel

(def tube-sparkle
  "Port of `tolglow.fx/sparkle` and the `tolglow.cue/sparkle` pads. Each pixel
  has `:chance` of lighting in every sixteenth and fades over `:fade` beats.
  The original drew from `rand` each frame and kept the sparks in an atom. This
  one hashes the pixel, the sixteenth and `:seed`. The original also nudged each
  spark's hue up to 35 degrees either way. Here each pixel shades between
  `:tint` and `:accent`, a saturated neighbouring hue, so sparks stay coloured
  rather than fading toward white."
  (let [slots-per-beat 4.0
        time (fx/multiply beat slots-per-beat)
        slot (floor time)
        within (fract time)
        spark (fn [age]
                (let [drawn (noise head-hash (fx/multiply (fx/subtract slot age) 4.1414)
                                   (fx/multiply (p :seed) 1.618))
                      lit (at-least (fx/multiply 0.4 (p :chance)) drawn)
                      beats (fx/divide (fx/add within age) slots-per-beat)]
                  (fx/multiply lit (fx/clamp (fx/subtract 1.0 (fx/divide beats (p :fade)))))))
        level (apply op :max (map (comp spark double) (range 6)))
        shade (noise (fx/multiply head-hash 1.37) (floor (fx/multiply 0.25 beat)) (p :seed))]
    (look :tube-sparkle
          {:name "Sparkle" :targets rig/pixels :envelope (soft-colour 1/4 1)
           :params {:tint (fx/parameter [0.81 0.19 0.69]) :accent (fx/parameter [0.95 0.08 0.3])
                    :chance [0.385 0.0 1.0] :fade [0.75 0.1 1.5] :level [0.3 0.0 1.0]
                    :seed [656.0 0.0 9999.0]}}
          (fx/color (fx/multiply (fx/mix (p :tint) (p :accent) shade) level (p :level)) {:blend :max}))))

(def confetti
  "Port of `tolglow.fx/confetti`: on each step some pixel clusters take a
  random hue and saturation and hold it for one to four steps. Hues are degrees
  and saturation a fraction, as in the saved macros. Newer flakes cover older
  ones. The draw is a hash of cluster, step and `:seed`."
  (let [step (floor (fx/divide beat (p :step-beats)))
        cluster (fx/add (floor (fx/multiply x 3.0)) (fx/multiply 31.0 (floor (fx/multiply y 3.0))))
        flake (fn [age]
                (let [born (fx/subtract step age)
                      draw (fn [salt] (noise (fx/multiply cluster 7.31) (fx/multiply born 3.17)
                                             (fx/multiply (p :seed) 0.618) salt))
                      steps (fx/add (p :min-dur)
                                    (floor (fx/multiply (draw 1.1) (fx/add 0.999 (fx/subtract (p :max-dur) (p :min-dur))))))
                      alive (fx/multiply (at-least (p :density) (draw 0.0)) (at-least steps (+ age 1.0)))
                      hue (fx/divide (fx/mix (p :min-hue) (p :max-hue) (draw 2.2)) 360.0)]
                  [alive (tinted hue (fx/mix (p :min-sat) (p :max-sat) (draw 3.3)))]))
        colour (reduce (fn [beneath age]
                         (let [[alive flake-colour] (flake (double age))]
                           (fx/mix beneath flake-colour alive)))
                       (fx/constant [0.0 0.0 0.0]) [3 2 1 0])]
    (look :confetti
          {:name "Confetti" :targets rig/ribbon :envelope (soft-colour 1/4 1)
           :params {:step-beats [1.0 0.25 4.0] :density [0.3 0.0 1.0]
                    :min-dur [1.0 1.0 4.0] :max-dur [4.0 1.0 4.0]
                    :min-hue [210.0 0.0 360.0] :max-hue [260.0 0.0 360.0]
                    :min-sat [0.3 0.0 1.0] :max-sat [0.7 0.0 1.0]
                    :level [0.2 0.0 1.0] :seed [656.0 0.0 9999.0]}}
          (fx/color (fx/multiply colour (p :level)) {:blend :max}))))

(def riser-stops
  [{:position 0.0 :color [0.85 0.03 0.02]}
   {:position 0.55 :color [1.0 0.42 0.04]}
   {:position 1.0 :color [1.0 0.88 0.68]}])

(def riser-chase
  "Bands climb the tubes, offset from tube to tube so the row reads as a
  diagonal. Colour follows a gradient up each tube. `:drive` is an HDR emission
  gain: past 1 the band cores whiten and bleed into the neighbouring pixels."
  (let [height (fx/divide (fx/subtract y rig/tube-base) rig/tube-length)
        phase (fract (fx/add (fx/multiply height (p :bands))
                             (fx/multiply -1.0 (fx/divide beat (p :beats)))
                             (fx/multiply x 0.119)))
        band (fx/clamp (fx/subtract 1.0 (fx/divide (absolute (fx/subtract phase 0.5))
                                                    (fx/add (fx/multiply 0.5 (p :width)) 1.0e-4))))]
    (look :riser-chase
          {:name "Riser chase" :targets rig/tubes :envelope (soft-colour 1/2 1)
           :params {:beats [1.0 0.25 16.0] :bands [1.0 0.5 4.0] :width [0.3 0.02 1.0]
                    :level [0.2 0.0 1.0] :drive [1.0 1.0 4.0]}}
          (fx/color (fx/multiply (gradient riser-stops (fx/clamp height)) band (p :level)) {:blend :max}))))

(def aurora-stops
  [{:position 0.0 :color [0.0 0.35 0.45]}
   {:position 0.35 :color [0.02 0.8 0.35]}
   {:position 0.7 :color [0.35 0.1 0.85]}
   {:position 1.0 :color [0.9 0.05 0.55]}])

(def aurora
  "A four-stop gradient drifts along the ribbon while a slower swell breathes
  through it. `:scroll` is one full cycle of drift."
  (let [drift (fx/add (fx/multiply 0.7 (sine (fx/add (fx/divide x (p :wavelength)) (p :scroll))))
                      (fx/multiply 0.3 (sine (fx/subtract (fx/divide x 2.3) (fx/multiply 2.0 (p :scroll))))))
        swell (fx/add 0.55 (fx/multiply 0.45 (sine (fx/add (fx/divide beat 8.0) (fx/divide y 4.0)))))]
    (look :aurora
          {:name "Aurora ribbon" :targets rig/ribbon :envelope (soft-colour 1 2)
           :params {:scroll [0.0 0.0 1.0] :level [0.22 0.0 1.0] :wavelength [6.0 1.0 20.0]}}
          (fx/color (fx/multiply (gradient aurora-stops drift) swell (p :level))))))

;; ------------------------------------------------------- after-hours cues
;;
;; Ideas from the operator's 2020 tolglow-web frontend (#656): the searchlight
;; wander and the twin audio cones of its Three.js scene, and the "Run" and
;; matrix concepts of its placeholder grid. Each reads only the transport beat,
;; the head position, its parameters and a seed, so a stopped transport holds
;; every frame and play resumes where it stopped, with no catch-up.

(defn- signed [unit] (fx/subtract (fx/multiply 2.0 unit) 1.0))

(defn- ease-out-quad
  "Quadratic-out easing of a unit fraction: fast away, settling on arrival."
  [t]
  (let [rest (fx/subtract 1.0 t)] (fx/subtract 1.0 (fx/multiply rest rest))))

(def searchlight-drift
  "Port of the tolglow-web searchlight wander (`three.cljs`, random tweens of
  each spot's aim over 2-7 s with quadratic-out easing). Each beam lunges to a
  new seeded point in the box `:center` +/- `:extent`, settles there for the
  `:hold` fraction of its move and picks the next. Moves last `:move-beats`
  on average; every head's are 0.6 to 1.4 times that, by seed, with its own
  phase, so the six beams never move in step. The moves count from the launch
  in a phase accumulator, so turning `:move-beats` changes the pace from the
  move each beam is in rather than sending it to another one. A move is a
  stretch of beats, so the drift follows tempo and freezes with the
  transport. Nothing limits how fast a head is asked to travel. The original
  also tweened beam angle and penumbra; these heads have no zoom, so the
  brightness wanders instead, between `:level-min` and `:level`."
  (let [salt (fx/multiply (p :seed) 0.618)
        pace (fx/add 0.6 (fx/multiply 0.8 (noise head-hash salt 0.37)))
        moves (fx/phase-accumulator :moves (p :move-beats))
        clock (fx/add (fx/divide moves pace) (noise (fx/multiply head-hash 1.91) salt 5.3))
        move (floor clock)
        eased (ease-out-quad (fx/clamp (fx/divide (fract clock) (fx/subtract 1.0 (p :hold)))))
        draw (fn [index salt-2] (noise head-hash (fx/multiply index 3.17) salt salt-2))
        spot (fn [index] (fx/add (p :center)
                                 (fx/multiply (p :extent) (fx/xyz (signed (draw index 1.1))
                                                                  (signed (draw index 2.2))
                                                                  (signed (draw index 3.3))))))
        level (fn [index] (fx/mix (p :level-min) (p :level) (draw index 4.4)))
        previous (fx/subtract move 1.0)]
    (look :searchlight-drift
          {:name "Searchlight drift" :targets rig/beams :envelope soft
           :params {:center (fx/parameter [0.0 0.8 1.6] {:type :vector :size 3 :bounds [-6.0 6.0]
                                                         :semantic/type :position :unit :m :name "Centre"})
                    :extent (fx/parameter [4.2 0.8 2.2] {:type :vector :size 3 :bounds [0.0 6.0]
                                                         :unit :m :name "Extent"})
                    :move-beats (fx/parameter 8.0 {:bounds [1.0 32.0] :unit :beats :name "Move length"})
                    :hold (fx/parameter 0.3 {:bounds [0.0 0.9] :unit :fraction :name "Settle"})
                    :level (fx/parameter 0.9 {:bounds [0.0 1.0] :name "Brightest"})
                    :level-min (fx/parameter 0.45 {:bounds [0.0 1.0] :name "Dimmest"})
                    :tint (fx/parameter [0.78 0.86 1.0] {:name "Tint"})
                    :seed (fx/parameter 656.0 {:bounds [0.0 9999.0] :name "Seed"})}}
          (fx/color (p :tint))
          (fx/intensity (fx/mix (level previous) (level move) eased))
          (fx/aim (fx/mix (spot previous) (spot move) eased))
          shutter-open)))

(def twin-cones
  "Port of the tolglow-web twin audio cones (`crazy-spotlight-shader.glsl`).
  The floor heads trace circles above the stage, the stage-left pair turning
  one way and the stage-right pair the other. Their tints sway between
  `:tint-a` and `:tint-b` in opposite phase. The turn and the sway count from
  the launch in phase accumulators, so turning `:bars` or `:sway-bars` changes
  the speed from where the cones are. Brightness follows a mix of a
  kick that decays from every beat and `:energy`. The show binds `:energy` to
  an audio-shaped LFO on the bass band, so with a capture running each bar of
  bass sets the next bar's brightness. Without audio that LFO rests on a sine
  and the cones still breathe once a bar."
  (let [side (signed (at-least x 0.0))
        kick (op :exp (fx/multiply -1.0 (p :punch) (fract beat)))
        drive (fx/mix kick (fx/clamp (p :energy)) (p :follow))
        turns (fx/phase-accumulator :turn (p :bars) {:unit :bars})
        angle (fx/multiply tau (fx/add (fx/multiply side turns) (fx/divide x 4.8)))
        sway (sine (fx/add (fx/phase-accumulator :sway (p :sway-bars) {:unit :bars}) (fx/multiply 0.25 side)))]
    (look :twin-cones
          {:name "Twin cones" :targets rig/movers :envelope soft
           :params {:energy (fx/parameter 0.5 {:bounds [0.0 1.0] :unit :fraction :name "Energy"
                                               :control/axis :pressure})
                    :follow (fx/parameter 0.6 {:bounds [0.0 1.0] :unit :fraction :name "Audio follow"})
                    :punch (fx/parameter 3.0 {:bounds [0.0 12.0] :unit :per-beat :name "Kick decay"})
                    :floor (fx/parameter 0.15 {:bounds [0.0 1.0] :name "Floor"})
                    :level (fx/parameter 1.0 {:bounds [0.0 1.0] :name "Level"})
                    :bars (fx/parameter 2.0 {:bounds [0.25 16.0] :unit :bars :name "Turn"})
                    :sway-bars (fx/parameter 4.0 {:bounds [0.25 32.0] :unit :bars :name "Sway"})
                    :spread (fx/parameter 1.8 {:bounds [0.0 5.0] :unit :m :name "Spread"})
                    :radius (fx/parameter 1.1 {:bounds [0.0 4.0] :unit :m :name "Radius"})
                    :height (fx/parameter 3.6 {:bounds [0.5 5.0] :unit :m :name "Height"})
                    :depth (fx/parameter 0.4 {:bounds [-4.0 4.0] :unit :m :name "Depth"})
                    :tint-a (fx/parameter [0.05 0.35 1.0] {:name "Tint A"})
                    :tint-b (fx/parameter [0.95 0.1 0.55] {:name "Tint B"})}}
          (fx/color (fx/mix (p :tint-a) (p :tint-b) sway))
          (fx/intensity (fx/multiply (p :level) (fx/mix (p :floor) 1.0 drive)))
          (fx/aim (fx/xyz (fx/add (fx/multiply side (p :spread)) (fx/multiply (p :radius) (op :cos angle)))
                          (p :height)
                          (fx/add (p :depth) (fx/multiply (p :radius) (op :sin angle))))))))

(def run
  "The \"Run\" concept from the tolglow-web grid (strobe, level, lightness and
  colour) as a macro cue: `:drive` alone takes the beams and washes from a
  dim single colour to a white-hot strobe. It writes no aim, so whatever moves
  the beams keeps moving them, and it leaves the floor heads to the cones. As
  it rises the level
  climbs from `:floor`, the hue fans out across the room by up to
  `:hue-spread` turns, the colour whitens by up to `:whiten`, and past
  `:strobe-from` a beat-locked strobe starts and doubles its rate in steps,
  up to 2^`:rate-steps` flashes per beat, while its duty shortens. Two
  doublings, four flashes a beat, is 12 Hz at 180 BPM, the most the show's
  40 Hz frames draw as distinct flashes; eight a beat would alias. At full
  drive each flash lasts 0.35 of its slot, long enough for a 25 ms frame. The other
  parameters shape the ramp; a performer only needs the one control."
  (let [drive (fx/clamp (p :drive))
        strobing (at-least drive (p :strobe-from))
        rate (op :pow 2.0 (floor (fx/multiply drive (fx/add (p :rate-steps) 0.999))))
        ;; At full drive a flash lasts 0.35 of its slot: 29 ms at four a beat
        ;; and 180 BPM, so a 25 ms frame always lands in it.
        flash (at-least (fx/mix 0.6 0.35 drive) (fract (fx/multiply beat rate)))
        gate (fx/mix 1.0 flash strobing)
        hue (fx/add (p :hue) (fx/multiply drive (p :hue-spread) (fx/divide x 10.0)))
        colour (fx/mix (hue->rgb hue) [1.0 1.0 1.0] (fx/multiply (p :whiten) drive drive))]
    (look :run
          {:name "Run" :targets (into rig/beams rig/washes) :envelope soft
           :params {:drive (fx/parameter 0.0 {:bounds [0.0 1.0] :unit :fraction :name "Drive"
                                              :control/axis :pressure})
                    :floor (fx/parameter 0.2 {:bounds [0.0 1.0] :name "Floor"})
                    :hue (fx/parameter 0.62 {:bounds [0.0 1.0] :unit :turns :name "Hue"})
                    :hue-spread (fx/parameter 0.35 {:bounds [0.0 1.0] :unit :turns :name "Hue spread"})
                    :whiten (fx/parameter 0.8 {:bounds [0.0 1.0] :unit :fraction :name "Whiten"})
                    :strobe-from (fx/parameter 0.55 {:bounds [0.0 1.0] :unit :fraction :name "Strobe from"})
                    :rate-steps (fx/parameter 2 {:type :integer :bounds [0 2] :unit :doublings :name "Strobe doublings"})}}
          (fx/color (fx/multiply colour gate))
          (fx/intensity (fx/mix (p :floor) 1.0 (op :sqrt drive)))
          shutter-open)))

(def matrix-stops
  [{:position 0.0 :color [0.95 0.05 0.25]}
   {:position 0.5 :color [0.4 0.08 0.95]}
   {:position 1.0 :color [0.02 0.7 0.95]}])

(def matrix-chase
  "A grid chase on the tube wall, read as eight columns of ten cells. Bands of
  light every `:spacing` cells step one cell per `:step` beats across the grid
  at `:angle` degrees: 0 sweeps the columns left to right, 90 climbs the rows,
  anything between runs diagonals. Each band fades over `:tail` cells behind
  it. Rows take their colour from the `:palette` gradient, bottom to top. The
  bands count from the launch in a phase accumulator, one band cycle per
  `:step` x `:spacing` beats, so editing either changes the pace or the gaps
  from where the bands are."
  (let [column (floor (fx/add 0.5 (fx/divide (fx/subtract x (first rig/tube-xs)) 1.2)))
        row (floor (fx/multiply 10.0 (fx/clamp (fx/divide (fx/subtract y rig/tube-base) rig/tube-length) 0.0 0.999)))
        angle (degrees (p :angle))
        along (fx/add (fx/multiply column (op :cos angle)) (fx/multiply row (op :sin angle)))
        cycle (fx/phase-accumulator :bands (fx/multiply (p :step) (p :spacing)))
        head (floor (fx/multiply (fract cycle) (p :spacing)))
        ;; The nudge keeps a cell exactly on a band from rounding to the far
        ;; end of the gap when cos or sin of the angle is not exactly zero.
        behind (fx/multiply (p :spacing) (fract (fx/add (fx/divide (fx/subtract head along) (p :spacing)) 1.0e-6)))
        lit (fx/clamp (fx/subtract 1.0 (fx/divide behind (p :tail))))]
    (look :matrix-chase
          {:name "Matrix chase" :targets rig/tubes :envelope (soft-colour 1/4 1)
           :params {:angle (fx/parameter 0.0 {:bounds [-180.0 180.0] :unit :deg :name "Angle"})
                    :step (fx/parameter 0.5 {:bounds [0.125 4.0] :unit :beats :semantic/type :beat-ratio
                                             :name "Step"})
                    :spacing (fx/parameter 4 {:type :integer :bounds [1 12] :unit :cells :name "Spacing"})
                    :tail (fx/parameter 2.0 {:bounds [0.5 8.0] :unit :cells :name "Tail"})
                    :level (fx/parameter 0.6 {:bounds [0.0 1.0] :name "Level"})
                    :palette (fx/parameter matrix-stops {:name "Palette"})}}
          (fx/color (fx/multiply (fx/gradient :palette (fx/divide row 9.0)) lit (p :level)) {:blend :max}))))

;; ----------------------------------------------------------------- modulator

(defn colour-drift
  "A launchable modulator cue. Each bar it glides the beam hue and the floor
  heads' tint to a new seeded random value. It writes no fixture output itself."
  [seed]
  {:id :colour-drift :version 1 :name "Colour drift" :params {} :outputs {}
   :categories [:modulation] :tags [:modulation] :duration {:bars 4}
   :modulation {:seed seed :every {:bars 1} :mode :glide :curve :smoothstep
                :select {:cue-ids [:beam-shafts]} :params [:hue] :ranges {:hue [0.52 0.98]}}})

;; -------------------------------------------------------------------- signals

(def heartbeat
  "A registered waveform: two decaying thumps per cycle, the second softer."
  (let [phase (fx/param signals/phase-input)
        thump (fn [offset gain]
                (fx/multiply gain (op :exp (fx/multiply -9.0 (fract (fx/subtract phase offset))))))]
    {:id :heartbeat :version 1 :name "Heartbeat" :category :composite :range [0.0 1.0] :params {}
     :expression (fx/clamp (op :max (thump 0.0 1.0) (thump 0.28 0.62)))}))

(def bass-lfo
  "The audio-shaped LFO the show configures (`tolglow.audio-lfo`, #676): the
  RMS envelope of each whole bar of the bass band, morphing over two beats.
  Until a capture supplies a bar it rests on a sine, and through silence it
  holds the last bar it heard."
  {:lfo-id :showcase-bass :name "Showcase bass"
   :config {:band :bass :extract :envelope :window {:bars 1} :points 16
            :morph {:beats 2} :curve :smoothstep :rest-shape :sine :on-silence :hold}})

(def bass-waveform (audio-lfo/waveform-id (:lfo-id bass-lfo)))

(defn- lfo [waveform period from to & {:as options}]
  (merge {:type :lfo :waveform waveform :period period :phase-reference :transport :from from :to to}
         options))

(def sources
  "Cue ID -> {parameter waveform-binding}. A launch that asks for its sources
  binds these to the new instance. They are ordinary `:modulation-bind`
  payloads, so Studio shows and edits them like any other attached waveform."
  {:haze {:level (lfo :pulse {:bars 2} 0.0 0.7 :inputs {:duty 0.3})}
   :beam-shafts {:level (lfo :heartbeat {:beats 2} 0.3 0.95)}
   :wash-tide {:level (lfo :sine {:bars 2} 0.28 0.72)
               :shift (lfo :triangle {:bars 4} -0.3 0.3)}
   :wash-ember {:layer/priority (lfo :pulse {:bars 2} -1 1 :inputs {:duty 0.5} :hysteresis {:band 1/4})}
   :mover-orbit {:radius (lfo :triangle {:beats 8} 0.6 2.6)
                 :height (lfo :sine {:bars 6} 2.6 4.6)}
   :riser-chase {:width (lfo :triangle {:bars 2} 0.12 0.6)}
   :aurora {:scroll (lfo :saw {:bars 4} 0.0 1.0)
            :level (lfo :sine {:bars 3} 0.12 0.3)}
   :twin-cones {:energy (lfo bass-waveform {:bars 1} 0.0 1.0)}})

;; ------------------------------------------------------------------ catalogue

(def catalogue
  "Grid order. :origin is :ported or :new; :legacy names what a port restates."
  [{:id :haze :effect haze :category :atmosphere :origin :new :swatch "#8fa3ad" :priority 0}
   {:id :wash-tide :effect wash-tide :category :colour :origin :new :swatch "#5a3fd0" :priority 0}
   {:id :wash-ember :effect wash-ember :category :colour :origin :new :swatch "#e2581c" :priority 0}
   {:id :beam-shafts :effect beam-shafts :category :colour :origin :new :swatch "#6c4df0" :priority 5}
   {:id :pinstripes :effect pinstripes :category :colour :origin :ported :swatch "#2d4bc5" :priority 10
    :legacy "tolglow.fx/pinstripes, tolglow.cue/pinstripe"}
   {:id :bloom-halo :effect bloom-halo :category :colour :origin :ported :swatch "#de7504" :priority 20
    :legacy "tolglow.fx/bloom with :halo, tolglow.cue/bloom"}
   {:id :bloom-keyhole :effect bloom-keyhole :category :intensity :origin :ported :swatch "#1f2a44" :priority 50
    :legacy "tolglow.fx/bloom with :keyhole? true, the :bloom-keyhole cue in tolglow.page/effects"}
   {:id :dimmer-sweep :effect dimmer-sweep :category :intensity :origin :ported :swatch "#d8d2c4" :priority 60
    :legacy "tolglow.fx/sweep and dimmer-sweep, tolglow.cue/sweep-column"}
   {:id :strobe :effect strobe :category :intensity :origin :ported :swatch "#ac8ce6" :priority 80
    :legacy "tolglow.fx/color-strobe, tolglow.cue/strobe"}
   {:id :blinder-hit :effect blinder-hit :category :intensity :origin :new :swatch "#ffd9a0" :priority 100}
   {:id :aim-lfo :effect aim-lfo :category :movement :origin :ported :swatch "#1e90ff" :priority 10
    :legacy "tolglow.cue/pointing-lfo, the \"all aim lfo\" macro in macros.clj"}
   {:id :can-can :effect can-can :category :movement :origin :ported :swatch "#e6c229" :priority 12
    :legacy "tolglow.fx/can-can, the :move-all cues in tolglow.page/movement"}
   {:id :mover-orbit :effect mover-orbit :category :movement :origin :new :swatch "#27a6e5" :priority 10}
   {:id :tube-sparkle :effect tube-sparkle :category :pixel :origin :ported :swatch "#ce31af" :priority 30
    :legacy "tolglow.fx/sparkle, tolglow.cue/sparkle"}
   {:id :confetti :effect confetti :category :pixel :origin :ported :swatch "#7a6ae0" :priority 25
    :legacy "tolglow.fx/confetti, tolglow.cue/confetti"}
   {:id :riser-chase :effect riser-chase :category :pixel :origin :new :swatch "#ff7a1a" :priority 15}
   {:id :aurora :effect aurora :category :pixel :origin :new :swatch "#19c37d" :priority 5}
   {:id :searchlight-drift :effect searchlight-drift :category :movement :origin :ported :swatch "#c8d6ff" :priority 11
    :legacy "tolglow-web three.cljs searchlight wander (random aim and beam tweens, quadratic-out)"}
   {:id :twin-cones :effect twin-cones :category :intensity :origin :ported :swatch "#3a5cff" :priority 13
    :legacy "tolglow-web crazy-spotlight-shader.glsl two-spot audio cones"}
   {:id :run :effect run :category :intensity :origin :ported :swatch "#ff3d6e" :priority 70
    :legacy "tolglow-web placeholder grid concept \"Run\" (strobe, level, lightness, colour)"}
   {:id :matrix-chase :effect matrix-chase :category :pixel :origin :new :swatch "#9a2cf0" :priority 20}])

(def after-hours-cues
  "The cues the `after-hours` timeline was written for."
  #{:searchlight-drift :twin-cones :run :matrix-chase})

(defn cue [{:keys [id effect category swatch priority]}]
  {:id id :version 1 :name (:name effect) :effect-id id :swatch swatch :tags [category]
   :params {} :priority priority :reload-policy :preserve-state})
