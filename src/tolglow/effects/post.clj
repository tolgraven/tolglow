(ns tolglow.effects.post
  "Post-effects: cues that filter, shade or black out the lighting composed
  beneath their priority, instead of adding light of their own (#479).

  The colour guard, shading bloom and darkening sparkle are native layer
  modifiers. Each computes three coefficients per head and frame from typed
  parameters, the head position and the transport beat, writes them as its
  colour output, and its layer program hands them with the previous colour to
  an Afterglow colour filter (`:color-guard` or `:color-grade`) that writes the
  filtered colour in their place. The cue's priority is the layer position:
  cues above it, and cues at the same priority launched after it, are not
  filtered. Coefficients of zero leave the colour unchanged. Fade a filter
  through its instance opacity or attack and release, never by scaling its
  colour output: a colour envelope would scale the hue settings, so these
  recipes refuse one, and the energy autopilot never picks a post-effect for a
  brightness gain. Nothing here keeps state, reads a clock or draws a random
  number; the sparkle hashes the pixel, the sixteenth and a `:seed` parameter
  with IEEE arithmetic only, so a stopped transport holds every frame and one
  seed replays the same bytes on any JVM.

  The group blackout masks the heads its cue's `:selector` chooses after
  composition and HDR, as the global blackout masks every head, so no cue of
  any priority and no spilled light can relight them. `blackout-cue` makes it
  a held, operator-owned cue: hold the pad to black the group out, release to
  restore whatever is running beneath, which never stopped or advanced on its
  account."
  (:require [tolglow.effects :as fx]
            [tolglow.registry :as registry]
            [tolglow.spatial.targets :as targets]))

(def ^:private p fx/param)

(defn- op [operator & args] {:op operator :args (mapv fx/expression args)})

(defn- fract [value] (op :fract value))

(defn- floor [value] (op :floor value))

(def ^:private beat
  "Transport beat: a stopped transport holds the pattern, and play resumes it."
  {:op :beat :clock :absolute})

(def ^:private previous-color {:op :previous :attribute :color :default [0.0 0.0 0.0]})

(defn- filter-layer
  "Replace the colour beneath with `operator` of that colour and this member's
  colour output, which carries the coefficients."
  [operator]
  {:transforms [{:attribute :color
                 :value {:op operator :args [previous-color {:op :incoming :attribute :color}]}}]})

(defn- with-defaults
  "Parameter schemas with operator-chosen defaults, checked against their bounds."
  [schemas defaults]
  (when-not (and (map? defaults) (every? #(contains? schemas %) (keys defaults)))
    (throw (ex-info "Defaults name parameters this post-effect declares"
                    {:errors [{:path [:defaults] :message "Unknown parameter"}]})))
  (reduce-kv (fn [result key value]
               (let [schema (assoc (get result key) :default value)]
                 (when-not (registry/valid-value? schema value)
                   (throw (ex-info "Default is outside the parameter bounds"
                                   {:errors [{:path [:defaults key] :message "Out of bounds"}]})))
                 (assoc result key schema)))
             schemas defaults))

(defn- post-effect [id options schemas operator coefficients]
  (let [{:keys [defaults envelope]} options]
    (when (some #{:color} (:attributes envelope))
      (throw (ex-info "A post-effect fades through its instance opacity: a colour envelope would scale its settings"
                      {:errors [{:path [:envelope :attributes] :message "Colour envelope on a filter"}]})))
    (fx/effect id (merge {:duration {:bars 4}}
                         (dissoc options :defaults)
                         {:params (with-defaults schemas (or defaults {}))
                          :layer (filter-layer operator)
                          ;; Marks the colour output as filter settings, not light.
                          :post-effect? true})
               (fx/color coefficients))))

;; -------------------------------------------------------------- colour guard

(def guard-params
  {:min-saturation (fx/parameter 0.55 {:bounds [0.0 1.0] :unit :fraction :name "Saturation floor"})
   :hue-center (fx/parameter 0.0 {:bounds [0.0 1.0] :unit :turns :name "Palette hue"})
   :hue-restriction (fx/parameter 0.0 {:bounds [0.0 1.0] :unit :turns :name "Hue restriction"})
   :amount (fx/parameter 1.0 {:bounds [0.0 1.0] :unit :fraction :name "Amount" :control/axis :pressure})})

(defn color-guard
  "Keep the colour beneath inside a palette: never a muddy mix, never a hue the
  show does not use. Below `:min-saturation` a colour's saturation rises toward
  it, smoothly, so white and faint tints stay near white while greyish mixes
  of two primaries reach the floor. Hues outside the arc
  `1 - :hue-restriction` turns wide around `:hue-center` move to its nearest
  edge: 0 allows every hue, 1 locks every colour to the palette hue. Studio's
  Compose preset uses the same names. `:amount` scales both from no change (0) to
  as set (1). Value (the brightest component) is kept, including HDR overrange.

  Options are ordinary effect options plus `:defaults`, a map of parameter
  defaults. Put it on a cue at the priority where the filter belongs, with a
  `:selector` for one group, or over the whole rig."
  ([id] (color-guard id {}))
  ([id options]
   (post-effect id (merge {:name "Colour guard"} options) guard-params :color-guard
                (fx/rgb (fx/multiply (p :min-saturation) (p :amount))
                        (fx/multiply (p :hue-restriction) (p :amount))
                        (p :hue-center)))))

;; ------------------------------------------------------------- shading looks

(def ^:private x (fx/position :x))

(def ^:private y (fx/position :y))

(def ^:private z (fx/position :z))

(def shade-params
  {:dim (fx/parameter 0.6 {:bounds [0.0 1.0] :unit :fraction :name "Dampen"})
   :desaturate (fx/parameter 0.0 {:bounds [0.0 1.0] :unit :fraction :name "Desaturate"})
   :hue-shift (fx/parameter 0.0 {:bounds [-0.5 0.5] :unit :turns :name "Hue shift"})})

(defn- grade
  "Coefficients for `:color-grade` at a per-head shading level in [0,1]. The
  hue turn wraps into [0,1), so a negative shift fades through small turns."
  [level]
  (fx/rgb (fx/multiply (p :dim) level)
          (fx/multiply (p :desaturate) level)
          (fract (fx/multiply (p :hue-shift) level))))

(defn- sine [cycles]
  (fx/add 0.5 (fx/multiply 0.5 (op :sin (fx/multiply (* 2.0 Math/PI) cycles)))))

(defn bloom-band
  "The legacy bloom measure (`tolglow.fx/bloom`): distance from a centre in the
  X/Y plane as a fraction of `:reach`. A band `:width` wide sits at a fraction
  that swings between `:min` and `:max` every `:beats`, and falls off linearly
  over `:halo` widths. 1 on the band, 0 away from it."
  []
  (let [dx (fx/subtract x (p :center-x))
        dy (fx/subtract y (p :center-y))
        distance (fx/divide (op :sqrt (fx/add (fx/multiply dx dx) (fx/multiply dy dy))) (p :reach))
        fraction (fx/mix (p :min) (p :max) (sine (fx/subtract (fx/divide beat (p :beats)) 0.25)))
        outside (fx/subtract (op :abs (fx/subtract distance fraction)) (fx/multiply 0.5 (p :width)))]
    (fx/clamp (fx/subtract 1.0 (fx/divide outside (fx/add (fx/multiply (p :halo) (p :width)) 1.0e-4))))))

(def bloom-params
  {:beats (fx/parameter 8.0 {:bounds [0.25 64.0] :unit :beats :name "Swing"})
   :min (fx/parameter 0.0 {:bounds [0.0 1.5] :unit :fraction :name "Nearest"})
   :max (fx/parameter 1.0 {:bounds [0.0 1.5] :unit :fraction :name "Farthest"})
   :width (fx/parameter 0.25 {:bounds [0.0 1.0] :unit :fraction :name "Width"})
   :halo (fx/parameter 0.5 {:bounds [0.0 2.0] :unit :widths :name "Halo"})
   :center-x (fx/parameter 0.0 {:bounds [-20.0 20.0] :unit :m :name "Centre X"})
   :center-y (fx/parameter 0.3 {:bounds [-20.0 20.0] :unit :m :name "Centre Y"})
   :reach (fx/parameter 4.8 {:bounds [0.5 40.0] :unit :m :name "Reach"})})

(defn bloom-shade
  "Inverse bloom. The bloom band passes over the lighting beneath and shades it
  rather than lighting it: in the band the colour dims by `:dim`, loses
  `:desaturate` of its saturation toward its own luminance and turns
  `:hue-shift` turns, all falling to nothing across the halo. Use it with
  `:dim` 0 to drain or turn the colour without darkening it."
  ([id] (bloom-shade id {}))
  ([id options]
   (post-effect id (merge {:name "Shading bloom"} options) (merge bloom-params shade-params)
                :color-grade (grade (bloom-band)))))

(defn- hash3
  "A repeatable value in [0,1) from three inputs, without a transcendental
  function: only IEEE-rounded add, multiply and fract, so every JVM and
  architecture draws the same value (after Dave Hoskins' hash without sine)."
  [a b c]
  (let [p (fract (fx/add (fx/multiply a 0.1031) (fx/multiply b 0.11369) (fx/multiply c 0.13787)))]
    ;; Each further use of p copies the input tree, so two uses is the budget
    ;; that keeps six sparks within the compiler's node limit.
    (fract (fx/multiply 7.13 p (fx/add p 33.33)))))

(def ^:private sixteenth-period
  "Sparkle draws repeat every 4096 sixteenths (1024 beats), so the hashed slot
  stays small and exact however long the show runs."
  4096.0)

(def ^:private head-hash
  (fx/add (fx/multiply x 12.9898) (fx/multiply y 78.233) (fx/multiply z 37.719)))

(defn sparkle-level
  "A seeded sparkle like `tolglow.showcase.looks/tube-sparkle`: each pixel has
  probability `:chance` of a spark in every sixteenth, which fades over
  `:fade` beats. The draw hashes the pixel position, the sixteenth (modulo
  4096) and `:seed`. 0 to 1 per head."
  []
  (let [slots-per-beat 4.0
        time (fx/multiply beat slots-per-beat)
        slot (floor time)
        within (fract time)
        spark (fn [age]
                (let [cell (fx/subtract slot age)
                      cell (fx/subtract cell (fx/multiply sixteenth-period (floor (fx/divide cell sixteenth-period))))
                      drawn (hash3 head-hash (fx/add cell 0.5) (fx/add (p :seed) 0.25))
                      lit (floor (fx/clamp (fx/add 1.0 (fx/subtract (p :chance) drawn)) 0.0 1.0))
                      beats (fx/divide (fx/add within age) slots-per-beat)]
                  (fx/multiply lit (fx/clamp (fx/subtract 1.0 (fx/divide beats (p :fade)))))))]
    (apply op :max (map (comp spark double) (range 6)))))

(def sparkle-params
  {:chance (fx/parameter 0.15 {:bounds [0.0 1.0] :unit :fraction :name "Chance per sixteenth"})
   :fade (fx/parameter 0.75 {:bounds [0.1 1.5] :unit :beats :name "Fade"})
   :seed (fx/parameter 656.0 {:bounds [0.0 9999.0] :name "Seed"})})

(defn sparkle-shade
  "Darkening sparkle. Sparks are holes in the lighting beneath instead of
  points of light: where one strikes, the colour dims by `:dim` (1 is black),
  loses `:desaturate` of its saturation and turns `:hue-shift` turns, and
  recovers over `:fade` beats. The legacy sparkle could only add light (it
  blended its sparks by maximum, from an atom of random draws), which is why
  the original notes found an off-sparkle impossible. A native layer modifier
  reads the colour composed beneath it, so it can take light away."
  ([id] (sparkle-shade id {}))
  ([id options]
   (post-effect id (merge {:name "Darkening sparkle"} options)
                (merge sparkle-params (assoc-in shade-params [:dim :default] 0.85))
                :color-grade (grade (sparkle-level)))))

;; ------------------------------------------------------------ group blackout

(def blackout-priority
  "Above every authored look. The mask, not the priority, keeps the heads dark."
  1000)

(defn group-blackout
  "Black out each head the cue selects. By default the effect is a mask: while
  an instance runs, the show resolves its cue's selector against the live
  membership every frame and passes those heads to Afterglow, which zeros
  their dimmer and every emitter drive (colour, white, amber, UV and the rest)
  after composition and HDR. No cue of any priority and no spilled light can
  relight them, and every fixture goes dark however its dimmer is wired. The
  mask writes no output of its own: it is a show-control effect, so a group, a
  volume and an explicit fixture list all admit it and nothing stays dark
  where the selection has moved on. With `:emitters? false` it is instead an
  ordinary zero-dimmer write at the cue's priority, over the heads selected at
  launch. Discrete channels such as shutter and strobe keep the values beneath.
  The effect keeps no state."
  ([id] (group-blackout id {}))
  ([id {:keys [emitters?] :or {emitters? true} :as options}]
   (let [options (merge {:name "Group blackout" :duration {:beats 1}} (dissoc options :emitters?))]
     (if emitters?
       ;; It has no outputs to derive a category from; it acts on intensity.
       (assoc (fx/effect id (assoc options :blackout-mask? true :show-control? true :operator-only? true))
              :categories [:intensity])
       (fx/effect id options (fx/intensity 0.0))))))

(defn- masking? [{:keys [definition phase] :as instance}]
  (and (:blackout-mask? definition) (= :running phase)
       (pos? (double (get-in instance [:system :opacity] 1.0)))))

(defn masks?
  "Whether any running group blackout masks heads this frame."
  [model]
  (boolean (some masking? (vals (:instances model)))))

(def ^:private resolved
  "Selector resolutions, keyed on the selector and the identity of the rig,
  the lighting-scene groups and the spatial membership they read. Resolving
  is O(fixtures) and the inputs change rarely, so a blackout held over many
  frames resolves once. Bounded: past 64 entries the cache starts over."
  (atom {}))

(def ^:private resolved
  "Selector resolutions, keyed on the selector and the identity of the rig,
  the lighting-scene groups and the spatial membership they read. Resolving
  is O(fixtures) and the inputs change rarely, so a blackout held over many
  frames resolves once. Bounded: past 64 entries the cache starts over."
  (atom {}))

(defn- resolve-selection [model selector]
  (let [k [selector (get-in model [:rig :fixtures])
           (get-in model [:extensions :tolglow.lighting-scenes/control :groups])
           (get-in model [:variables :spatial :members]) (get-in model [:settings :spatial :volumes])]]
    (or (get @resolved k)
        (let [ids (set (targets/resolve-ids model selector))]
          (swap! resolved #(assoc (if (> (count %) 64) {} %) k ids))
          ids))))

(defn- masked-heads [model instance]
  (let [selector (:selector instance)]
    (cond
      ;; The selector the launch resolved, followed against live membership.
      ;; A launch that named its heads instead has no :selector and keeps them.
      selector (try (resolve-selection model selector)
                    (catch clojure.lang.ExceptionInfo _ (set (:targets instance))))
      (seq (:targets instance)) (:targets instance)
      (seq (get-in instance [:definition :targets])) (get-in instance [:definition :targets])
      ;; Never the whole rig by default: only an explicit :all selector masks it.
      :else nil)))

(defn blackout-ids
  "The heads every running group blackout masks this frame: the live members
  of the selector its launch resolved (a group, volume, fixture list or
  explicit :all), else the heads its launch named, else its definition
  targets.
  An instance with no selection masks nothing, so a stray launch cannot black
  out the whole rig."
  [model]
  (let [sets (into [] (comp (filter masking?) (map #(set (masked-heads model %)))) (vals (:instances model)))]
    ;; One held blackout reuses its cached resolution without copying it.
    (case (count sets) 0 #{} 1 (first sets) (reduce into sets))))

(defn blackout-cue
  "A held, operator-owned cue for `effect-id` (a `group-blackout`) over the
  heads `selector` chooses, such as `{:group-id :tubes}`. A surface pressing
  its pad launches it with `{:launch {:mode :held}}` and its release ends the
  instance. `:show-control? true`, `:role :show-control` and
  `:operator-only? true` put it under the show owner's operator-only rule:
  only a human surface or a named system owner, named as the command's
  source, may launch, edit, republish or end it. A selector is required; pass
  :all to black out the whole rig."
  [id effect-id selector & {:as options}]
  (when-not (targets/selector? selector)
    (throw (ex-info "A group blackout needs a selector: a group, volume, fixture list or :all"
                    {:errors [{:path [:selector] :message "Missing or invalid selector"}]})))
  (merge {:id id :version 1 :name "Group blackout" :effect-id effect-id :selector selector
          :priority blackout-priority :launch-mode :held :show-control? true :operator-only? true
          :role :show-control
          :tags [:intensity] :swatch "#000000" :params {}}
         options))
