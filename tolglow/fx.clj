(ns tolglow.fx "Big Joe's effect tweaks" {:author "Joen Tolgraven / James Elliott"}
  (:require [afterglow
             [channels :as chan]
             [effects :as fx :refer [chase scene]]
             [rhythm :as rhythm :refer [metro-snapshot metro-start metronome snapshot-bar-phase snapshot-beat-phase snapshot-beat-within-bar snapshot-down-beat?]]
             [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]
             [show-context :refer [*show* set-default-show! with-show]]
             [transform :as tf :refer [degrees]]
             [util :as autil]]
            [afterglow.effects
             [channel :as chan-fx]
             [color :as color-fx :refer [color-effect transform-colors]]
             [cues :as cues :refer [apply-merging-var-map code-cue color-fn-from-cue-var cue function-cue]]
             [dimmer :as dimmer :refer [dimmer-effect master-set-level]]
             [fun :as fun]
             [movement :as move]
             [oscillators :as lfo :refer [build-oscillated-param sawtooth sine square triangle]]
             [params :as params :refer [bind-keyword-param build-aim-param build-direction-param-from-pan-tilt build-pan-tilt-param build-param-formula build-spatial-param build-step-param frame-dynamic-param? param? resolve-param validate-param-type]]
             [show-variable :as var-fx :refer [variable-effect]]]
            [amalloy.ring-buffer :refer [ring-buffer]]
            [clojure.math.numeric-tower :as math]
            [com.evocomputing.colors :as colors :refer [color-name]]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.profiling :refer [pspy]]
            [tolglow
             [color :as color :refer []]
             [param :as param :refer []]
             [debug :as debug :refer [det]]
             [util :as util :refer []]])
  (:import [afterglow.effects Effect IEffect]
           [afterglow.effects.params IParam Param]
           afterglow.effects.dimmer.Master
           afterglow.effects.oscillators.IOscillator
           afterglow.rhythm.Metronome
           javax.media.j3d.Transform3D
           [javax.vecmath Point3d Vector3d]))

; REMEMBER XXX FIXME LOLS
; pseudo-FX not directly implementing Effect. need to make sure any params bound if
; attempting to resolve as usually bail on that in cue fn. So middle man fucking with params
; need to fucking take note.
; CLEAR ERROR MESSAGES WHEN THID HAPPENS

(def or-spec "Map of fx defaults. Should contain keys :dynamic, :static, :constant ?
              Currently :vars (dynamic params to expose to cue-vars), :opts (non-exposed args)
              which is dumb, allow make choice downstream...

              Prob could roll all into one sorta and just use resolve-unless-frame-dynamic to
              sep types? Just need some sorta tagging to weed out ones not relevant for cue-vars.
              Maybe simply by type?")

(def fx-data {:bloom {:name "Bloom color"
                      :arg-spec #({:vars {:color (color/create :white), :fraction 0, :width 1
                                          :halo 0.1  ;part, as percent of width, where bloom has ended but some bleeds through/fades out
                                          :keyhole? false, :keyhole-opacity 0.8 ; allow some through even outside keyhole
                                          :hue-mod 30 :lightness-mod -5 :saturation-mod -15}
                                   :opts {:measure (tf/build-distance-measure 0 0 0) ;; :keyhole-target "lightness"; adjustable destination (curr simply black outside bounds, should rather mod was)
                                          }})
                      :main-param :fraction
 #_:protocol #_{:name "Bloom color" :active fx/always-active :gen :bloom, :end fx/end-immediately}}
                      :active fx/always-active
                      :gen :bloom
                      :end fx/end-immediately})
#_(defn get-fx-maker "Effect generator FIXME" ;wait til work on bloom etc shows best way forward
 [fx-name
  arg-spec-fn ;returns map of var args (resolved at execution/inside main-fn) and their defaults   opts ;map of args to be resolved at effect creation
  inters-fn ;other intermediate values (calculated at creation) used in later fns (how?)
  gen-fn ;main calc fn, called by assigners-fn. takes [show snapshot head was map-of-all-above]
  assigners-fn ;takes main-fn (+possibly other stuff?)
  & {:keys [active-fn end-fn effect-def-fn] :or {active-fn fx/always-active, end-fn fx/end-immediately}}]
 {:pre [(some? *show*)]}
 (fn [fixtures & args]
  (let [p (param/assemble args arg-spec-fn)
       inters (inters-fn)
       f (fn [show snap head was]
          (let [r (apply merge (map (param/auto-resolve) [p inters]))]
           (gen-fn show snap head was r)))]
  (if effect-def-fn
   (effect-def-fn #_pass-shit-somehow) ;if creating scene or whatever
   (Effect. fx-name
            active-fn
            (fn [show snapshot] (assigners-fn gen-fn))
            end-fn)))))

(defn make-effect "Create an Effect. with name, gen-fn, and optional overridden active and end-fns (defaulting to always-active and end-immediately)"
 [fx-name gen-fn & {:keys [active-fn end-fn] :or {active-fn fx/always-active, end-fn fx/end-immediately}}]
 (Effect. fx-name active-fn gen-fn end-fn))

(defn color "Make a color effect which affects all lights, or :fixtures"
  [color & {:keys [fixtures effect-name include-color-wheels? htp? priority]
            :or {fixtures (all-fixtures)}}]
  (try
    (let [[c desc] (cond (= (type color) ::colors/color) [color (color-name color)]
                         (keyword? color)                [(color/like (name color)) (name color)]
                         (and (param? color) ;XXX helper (param/results-in ::color/color)
                              (= (params/result-type color)
                                 ::colors/color))        [color "variable"]
                         :else                           [(color/like (name color)) color])]
                         ;; :else [(color/create color) color])]
      (color-fx/color-effect (or effect-name (str "Global " desc)) c fixtures
                             :include-color-wheels? include-color-wheels?))
    (catch Exception e (throw (Exception. (str "Can't figure out how to create color from " color) e)))))

(defn global-dimmer-effect "Return an effect that sets all dimmers. It can vary in response to a MIDI mapped show variable, an oscillator, or the location of the fixture. You can override the default name by passing in a value with :effect-name"
  [level & {:keys [effect-name add-virtual-dimmers?]}]
  (let [htp? (not add-virtual-dimmers?)]
    (dimmer-effect level (all-fixtures) :effect-name effect-name :htp? htp? :add-virtual-dimmers? add-virtual-dimmers?)))
#_(defn rgb-dimmer "moderate rgb pixels using a virtual dimmer with high priority, scaling other effects." []
  (add-effect! :master (global-dimmer-effect 255 :htp? false :add-virtual-dimmers? true) :priority 100000))

(defn sat-transformer "Creates a color transformation for use with [[transform-colors]] which changes the saturation based on a variable parameter. If no parameter is supplied, the default is to use an oscillated parameter based on [[sawtooth]] with `:down?` set to `true` so the color is fully saturated at the start of the beat, and fully desaturated by the end. A different pattern can be created by supplying a different parameter with the `:param` optional keyword argument." {:doc/format :markdown}
  [& {:keys [param] :or {param (build-oscillated-param (sawtooth :down? true) :min -20 :max 20)}}]
  (fn [color show snapshot head]
    (let [s (colors/clamp-percent-float (resolve-param param show snapshot head))]
      (build-color-param :color color :adjust-saturation s)))); }}}

(defn color-transformer "generalized color transform"
  [& {:keys [h s l]}]
  (fn [color show snapshot head]
    (let [args (into [:color color] ;there is no need to resolve params at this point, so just pass them on...
                     ;; (when-not (= color (color/create :black)) ;avoid lighting up black, maybe some performance gains too
                     ;; (when-not (color/black? color) ;avoid lighting up black, maybe some performance gains too
                     (when (< (color/l color) 10) ;avoid lighting up black/very dark maybe some performance gains too
                      [:adjust-hue h :adjust-saturation s :adjust-lightness l]))]
      (apply build-color-param args))))

(defn color-mod-effect "Wraps transform-colors and the baddest transform-fn to get some fucking order in this place"
 [param fixtures & {:keys [hue saturation lightness] :or {hue 0.0 saturation 1.0 lightness 0.0}}] ;value scales param for that key
 (fn [show snapshot head]
  (let [res (resolve-param param show snapshot head)
        transformer (color-transformer :h (* res hue) :s (* res saturation) :l (* res lightness))] ;would be better to simply avoid if scale 0
   (transform-colors fixtures :transform-fn transformer))))

;; (defn compressor "compress value towards upper bounds without smashing it. SHOULD destructure color params to HSL with individual targets.
;;                   COULD hue-comp work? gravitating towards middle val rather than upper bound"
;;  [param level ratio knee & {:keys [extract-param]}] ;or makes more sense for sender to handle split/repack hmm
;;  (let [resolved (fn [show snapshot head] (resolve-param param show snapshot head))
;;        f (fn [_] )]))
;;
;; (defn saturation-comp "compress saturation"
;;  [])

;      SPARKLE
(defn- clean-sparkles "Filters out any sparkles that were created longer ago than the fade time. `sparkles` is a map from head to the timestamp at which the sparkle was created."
  [sparkles show snapshot fade-time]
  (pspy :clean-sparkles
        (let [now (:instant snapshot)
              fade-time (resolve-param fade-time show snapshot)
              updater (fn [result [where creation-time]]
                       (if (< (- now creation-time) fade-time)
                        (assoc result where creation-time)
                        result))]
          (reduce updater {} sparkles))))

(defn or-sparkle []
 {:vars {:color (color/like :white), :chance 0.001, :fade-time 500}})
;; joe's take: eh just do brunch's ideas in actual cue or even further down (live/max)
;; but DO sort larger sparkles, and vary intensity for new ones, + work with lightness...
(defn sparkle "A random sparkling effect like a particle generator over the supplied fixture heads.
  As each frame of DMX values generated, each participating fixture head has a chance of being assigned a sparkle (this chance is
  controlled by the optional keyword parameter `:chance`). Once a sparkle has been created, it will fade out over the number of
  milliseconds specified by the optional keyword parameter `:fade-time`. The initial color of each sparkle can be changed with
  the optional keyword parameter `:color`. All parameters may be dynamic, including show variables with the standard shorthand of
  passing the variable name as a keyword."
  [fixtures & {:keys [color chance fade-time] :as all}] ; :or {color (color/create :white) chance 0.001 fade-time 500}}]
  {:pre [(some? *show*)]}
  (let [p (param/assemble all or-sparkle :resolve-vars true)
        heads (chan/find-rgb-heads fixtures) ;; TODO: shit should be per-head in case stuff spatial
        [running sparkles] (map atom [true {}]) ; whether active, and a map from head to creation timestamp for active sparkles
        active-fn (fn [show snapshot]
                  (swap! sparkles clean-sparkles show snapshot (:fade-time p)) ;; Continue running until all existing sparkles fade
                  (or @running (seq @sparkles)))
        gen-fn (fn [show snapshot]
                  (pspy :sparkle
                        (when @running ;; See if we create any new sparkles (unless ending)
                         (doseq [head heads] ;XXX either make sure to utilize per-head chance, or resolve just once...
                          (when (< (rand) (* 0.1 (resolve-param (:chance p) show snapshot head))) ; also hype likelihood that next head will also sparkle?
                           (swap! sparkles assoc head (:instant snapshot)))))
                        (let [now (:instant snapshot)]
                         (for [[head creation-time] @sparkles] ;; Build assigners for all active sparkles.
                          (let [r (param/auto-resolve p :show show :snapshot snapshot :head head) ;XXX either make sure to utilize spatial color and fade, or resolve just once...
                                fraction (/ (- now creation-time) (max 10 (:fade-time r)))
                                colormod (->> (:color r)
                                              (color/h (- (rand 70) 35)) ;XXX dont vary every fucking update...
                                              (color/l (* fraction (color/l (:color r)))))]
                           (fx/build-head-assigner
                            :color head
                            (fn [show snapshot target was] ;; can get black/darker sparkles by changing merge mode from htp
                             (color-fx/fade-colors colormod was fraction show snapshot target))))))))
        end-fn (fn [_ _] (reset! running false))]
    (Effect. "Sparkle" active-fn gen-fn end-fn))) ;; Arrange to shut down once all existing sparkles fade out.

;      BLOOMS
(defn bloom-dimmer "Bloom that doesn't set a color, only affecting dimmer"
  [fixtures & {:keys [fraction measure]
               :or { :fraction 0, :measure (tf/build-distance-measure 0 0 0)}}]
  (let [fraction (bind-keyword-param fraction Number 0)
        measure (resolve-param (bind-keyword-param measure :tf/distance-measure
                                                                 (tf/build-distance-measure 0 0 0))
                                      *show* (metro-snapshot (:metronome *show*)))
        heads (chan/find-rgb-heads fixtures)
        furthest (tf/max-distance measure heads)
        f (fn [show snapshot target was]
            (let [fraction (resolve-param fraction show snapshot target)
                  distance (measure target)]
              ;; (if (<= distance (* furthest fraction))
                was))
        assigners (fx/build-head-assigners :color heads f)]
    (apply fx/scene "Bloom"
           (concat [(Effect. "Bloom dimmer"
                             fx/always-active
                             (fn [show snapshot] assigners)
                             fx/end-immediately)]
                   (for [fixture fixtures]
                     (let [distance (measure fixture)
                           level (build-param-formula Number
                                                      (fn [fraction]
                                                        (if (<= distance (* furthest fraction))
                                                          255
                                                          0))
                                  fraction)]
                       (dimmer/dimmer-effect level [fixture])))))))

;      BLOOMS
(defn or-bloom []
 {:vars {:color (color/create :white), :fraction 0, :width 0.5
         :halo 0.2  ;part, as fraction of width, where bloom has ended but some bleeds through/fades out
         :keyhole? false, :keyhole-opacity 0.8 ; allow some through even outside keyhole
         :hue-mod 0 :lightness-mod 0 :saturation-mod 0}
 :opts {:measure (tf/build-distance-measure 0 0 0 :ignore-z true) ;; :keyhole-target "lightness"; adjustable destination (curr simply black outside bounds, should rather mod was)
        }
 :main-param :fraction
 #_:protocol #_{:name "Bloom color" :active fx/always-active :gen :bloom, :end fx/end-immediately}}) ;default value to send level arg to if none specified
; obviously :active and :end could be skipped for those effects using defaults
; ^ tho splitting fully like that seems prob foolish as most(?) fx not using always-active/end-immediately
; have some interdependency

(defn bloom "yo" ;FIXME: doesnt handle fixtures spanning across 0?
 [fixtures & {:keys [measure color fraction width halo keyhole? keyhole-opacity keyhole-target
                     hue-mod lightness-mod saturation-mod] :as all}] ;XXX move to [vars]
 (let [p (param/assemble all or-bloom :resolve-vars true)
       heads (chan/find-rgb-heads fixtures)
       furthest (tf/max-distance (:measure p) heads)
       fx (fn [show snap head was]
           (let [r (param/auto-resolve p) ;ev rename to p so all same. not sure if resolving already-resolved opt has impact here? shouldn't normally, but with measure which is a fn?
                 distance (/ ((:measure p) head) furthest)
                 [h s l] (map #(* (% r) distance) [:hue-mod :saturation-mod :lightness-mod])
                 color-fn (fn [color]
                           (when color (apply build-color-param :color color
                                          (flatten (map #(when (not= 0 %2) [%1 %2])
                                                        [:adjust-hue :adjust-saturation :adjust-lightness]
                                                        [h s l]))))) ;; XXX per-head virtual dimmer (not the same as lightness) (how not the same?)
                 [<bound bound>] (map #(% (:fraction r) (/ (:width r) 2)) [- +])
                 [<halo halo>] (map #(%1 %2 (* (:halo r) (:width r))) [- +] [<bound bound>])
                 ;; bounded (fn [point near far] (<= near point far))
                 bounds (map #(% (:fraction r) (/ (:width r) 2)) [- +])
                 halos (for [[b op] [bounds [- +]]] (op b (* (:halo r) (:width r))))]
             (if (<= <bound distance bound>) ;within bounds = bloom color normal, was color (modded) keyhole
             ;; (if (apply bounded distance bounds) ;within bounds = bloom color normal, was color (modded) keyhole
              (color-fn (if-not (:keyhole? r) (:color r) was))
              (let [level (cond (<= <halo distance <bound) (/ (- distance <halo) (- <bound <halo))
                                (<= bound> distance halo>) (/ (- halo> distance) (- halo> bound>))
              ;; (let [[near far] (map #(map % [halos bounds]) [first second])
              ;;       level (cond (bounded distance near) (/ (- distance <halo) (apply - (reverse near)))
              ;;                   (bounded distance (reverse far)) (/ (- halo> distance) (apply - far))
                                :else 0.0)] ;zero outside halo
               (if-not (:keyhole? r)
                (color-fx/fade-colors was (:color r) level show snap head) ;halo should fade out linearly...
                (color-fx/fade-colors was nil (* level (- 1.0 (:keyhole-opacity r))) show snap head))))))]
  (make-effect "Bloom color" (fn [show snapshot] (fx/build-head-assigners :color heads fx)))))

;; (when aim?  ;from confetti. Here we are talking about a bounds tho, not specific point... but resolve only one axis and track?
;;  (filter identity  ;but guess heads check whether selves in/out, either find nearest head that got lit, or nearest point that would be?
;;          (for [[head [_ _ point]] @flakes]
;;            (when (seq (move/find-moving-heads [head]))
;;              (fx/build-head-assigner :aim head (fn [_ _ _ _] point))))))
;;
;; XXX also todo: mod or clone bloom to only affect dimmers (a la sweep), not put color...


;     CONFETTI
(defn- clean-flakes "Filters out any flakes that were created longer ago than the configured duration. `flakes` is a map from head to a tuple containing the step value after which the flake will end, followed by color and potentially aim information."
  [flakes show snapshot step]
  (pspy :clean-flakes
        (let [now (math/round (resolve-param step show snapshot))
              result-fn (fn [result [where info]]
                         (let [final-step (first info)]
                          (if (<= now final-step)
                           (assoc result where info)
                           result)))]
          (reduce result-fn {} flakes))))

(defn- get-min-max "Get resolved, correct, positive params for making randem babbys"
 [show snapshot min-param max-param]
 (let [min-res (max 0 (math/round (resolve-param min-param show snapshot)))
       max-res (max min-res (math/round (resolve-param max-param show snapshot)))]
 [min-res max-res]))

(defn- add-flakes "Create new flakes of a shared random color and individual random durations and saturations for each of the supplied heads."
  [heads show snapshot current-step pm & {:keys [colors] :or {}}]
  (let [hue (+ (:min-hue pm) (rand (apply - (map pm [:max-hue :min-hue])))) ] ;XXX pass hue-range or vec of specific colors
    (into {}
          (map (fn [head]
                 (let [[min-dur max-dur] (apply get-min-max show snapshot (map pm [:min-dur :max-dur]))
                       duration (+ min-dur (rand-int (inc (- max-dur min-dur))))
                       [min-sat max-sat] (map #(colors/clamp-percent-float (resolve-param % show snapshot head)) (map pm [:min-sat :max-sat]))
                       saturation (colors/clamp-percent-float (- max-sat (rand (- max-sat min-sat))))]
                   [head [(+ current-step duration) (color/create (or hue 350) (or saturation 50) (+ (rand-int 25) 38))]]))
               heads))))

(defn- aim-flakes "Chooses a random point at which the newly-created flakes shoule be aimed."
  [flakes show snapshot pm]
  (let [get-resolved (fn [ks] (map #(resolve-param % show snapshot) (map pm ks)))
        [x y z] (apply map #(+ %1 (rand (- %2 %1))) (map get-resolved [[:min-x :min-y :min-z] [:max-x :max-y :max-z]]))
        point (Point3d. x y z)]
    (into {} (for [[head info] flakes] [head (conj info point)]))))

(defn or-confetti
 []
 {:whatever "make work w systim from can-can"
  :vars {:step (build-step-param) :min-add 1 :max-add 4 :min-dur 2 :max-dur 4
         :min-sat 40.0 :max-sat 80.0 :min-hue 0.0 :max-hue 360.0}
  :opts {:aim? false :min-x -5.0 :max-x 5.0 :min-y 0.0 :max-y 2.0 :min-z 0.5 :max-z 5.0}})
;; XXX tol, flakes as bounds in space instead of heads. So could map over multiple heads, change size to osc n shit
(defn confetti "Mod confetti so can set max saturation, and limit colors to specific range or selection"
  [fixtures & {:keys [step min-add max-add min-dur max-dur min-sat max-sat min-hue max-hue
                      aim? min-x max-x min-y max-y min-z max-z] :as all}]
  {:pre [(some? *show*)]}
  (let [p (param/assemble all or-confetti)
        heads (chan/find-rgb-heads fixtures true)
        [running current-step flakes] (map ref [true nil {}]) ; A map from head to [creation-step color] for active flakes
        active-fn (fn [show snapshot] ;; Continue running until all existing flakes fade
                   (dosync
                    (alter flakes clean-flakes show snapshot (param/auto-resolve (:step p)))
                    (or @running (seq @flakes))))
        gen-fn (fn [show snapshot]
                  (pspy :confetti ;; See how many flakes to create (unless we've been asked to end).
                        (dosync
                         (when @running
                          (let [now (math/round (param/auto-resolve (:step p) :show show :snapshot snapshot))] ;; (let [now (math/round (param/auto-resolve (:step p) show snapshot))] ;<-- wrong callll
                           (when (not= now @current-step)
                            (ref-set current-step now)
                            (let [p (update p :min-add #(max 0 (math/round (resolve-param % show snapshot))))
                                  p (update p :max-add #(max (:min-add p) (math/round (resolve-param % show snapshot))))
                                  p (reduce (fn [m k] (update m k #(resolve-param % show snapshot))) p [:min-hue :max-hue])
                                  add (+ (:min-add p) (rand-int (inc (- (:max-add p) (:min-add p)))))
                                  new-flakes (add-flakes (take add (shuffle heads)) show snapshot now p)
                                  aimed-flakes (if aim? (aim-flakes new-flakes show snapshot p) new-flakes)]
                             (alter flakes merge aimed-flakes)))))
                         (concat (for [[head [_ color]] @flakes] ;; Build assigners for all active flakes.
                                  (fx/build-head-assigner :color head (fn [_ _ _ _] color)))
                                 (when aim?
                                  (filter identity
                                          (for [[head [_ _ point]] @flakes]
                                           (when (seq (move/find-moving-heads [head]))
                                            (fx/build-head-assigner :aim head (fn [_ _ _ _] point))))))))))
        end-fn (fn [_ _] (dosync (ref-set running false)))] ;; Stop making new sparkles and arrange to shut down once all existing ones fade out.
        (Effect. "Confetti" active-fn gen-fn end-fn)))

;          CAN-CAN
(defn or-can-can []
 {:vars {:bars 1 :cycles 1 :stagger 0 :spread 0 :pan-min -30 :pan-max 30 :tilt-min -100 :tilt-max 100}})
(def heads [{:key :moving-1      :phase 0.0 :pan-offset  0 :tilt-offset 0}
            {:key :moving-mini-1 :phase 0.2 :pan-offset  0 :tilt-offset 0}
            {:key :moving-mini-2 :phase 0.4 :pan-offset -0 :tilt-offset 0}
            {:key :moving-mini-3 :phase 0.6 :pan-offset -0 :tilt-offset 0}
            {:key :moving-2      :phase 0.8 :pan-offset  0 :tilt-offset 0}])
(defn can-can-heads "Return fixture data map for [[can-can]]"
 [fixtures & [pan-offsets tilt-offsets]]
 (map-indexed (fn [i k]
               {:key k :phase (* i (/ 1 (count fixtures)))
                :pan-offset (pan-offsets i 0) :tilt-offsets (tilt-offsets i 0)}) ;or w/e nomenclature is...
              fixtures))

(defn can-can "Fix something reasonable of this... then apply to xyz, color etc"
  [#_fixture-cfg & {:keys [bars cycles stagger spread pan-min pan-max tilt-min tilt-max] :as all}]
  (let #_tolglow.debug/det
       [km (util/ks-show-ks-defaults all (:vars (or-can-can))) ;align fallback map
        p (param/bind-vars km)
        ;; builder (fn [kind f & arg]
        ;;           (map (fn [side & arg]
        ;;                 (apply build-param-formula Number f side arg))
        ;;                (reverse (param/extract kind p)))) ;figure out here how spec so fns get sent right args (:spread for pan)
        ratio (map #(apply build-param-formula Number % (param/extract :ratio p)) [#(/ %1 %2) #(/ (* 4 %1) %2)])
        fx (for [h heads]
             (let #_tolglow.debug/det [head-phase (param/number-formula #(* % (:phase h) (:stagger p))) ;(build-param-formula Number #(* % (:phase h)) (p :stagger))
                   lfos (map #(sine :interval :bar, :interval-ratio %, :phase head-phase) ratio)
                   ;; [head-pan head-tilt] (map #(builder %1 %2 %3)
                   ;;                           [(:pan-offset h 0) (:tilt-offset h 0)]
                   ;;                           [#(+ %1 (* (:pan-offset h 0) %2))
                   ;;                            #(+ % (:tilt-offset h 0))] [(:spread p) nil])
                   ;; head-pan (map (fn [side] (build-param-formula Number #(+ %1 (* (:pan-offset h 0) %2)) side (p :spread)))
                   head-pan (map (fn [side] (param/number-formula #(+ %1 (* (:pan-offset h 0) %2)) side (p :spread)))
                                                    (reverse (param/extract :pan p))) ;just rev now cause get -max -min order... fix sturdier
                   head-tilt (map (fn [side] (build-param-formula Number #(+ % (:tilt-offset h 0)) side))
                                                     (reverse (param/extract :tilt p)))
                   pt-params (for [[lfo [lo hi]] (zipmap lfos [head-pan head-tilt])]
                                               (build-oscillated-param lfo :min lo :max hi))
                   direction (apply build-pan-tilt-param (interleave [:pan :tilt] pt-params))]
               (move/pan-tilt-effect "Can Can Element" direction (fixtures-named (:key h)))))]
    (apply fx/scene "Can Can" fx)))
;
;  PINSTRIPE
(defn- gather-stripes "Gathers heads into the groups that will be assigned particular colors by the pinstripes effect."
  [heads group-fn num-colors]
  (let [head-groups (partition-all num-colors (sort-by #(:x (first %))
                                                       (vals (group-by group-fn (sort-by :x heads)))))
        stripe-groups (for [i (range num-colors)] [])]
    (loop [remaining head-groups, result stripe-groups]
      (let [[current remaining] (map #(% remaining) [first rest])
            result (map concat result (concat current (repeat [])))]
        (if (empty? remaining)
          result
          (recur remaining result))))))

(defn or-pinstripes "Default map for [[pinstripes]]" []
 {:vars {:colors [(color/like :royalblue) (color/like :orangered4)], :step (build-step-param)}
  :opts {:tolerance 0}})
(defn pinstripes "Divides heads into alternating columns by their _x_ positions (with configurable tolerance, default
  exact equality), assigning a color to each column, rotating by `:step` parameter. Fades between colors can be achieved by passing a step parameter that fades.
                  The tolerance used when creating the stripes can be controlled by `:tolerance`, default 0,
  meaning heads must have the exact same _x_ value to get assigned to the same stripe. The assignment of heads into stripes is
  done when the effect is created, so if the tolerance changes after that, it will have no effect.

  The colors themselves are passed as a sequence with `:colors` and default to red and white. The list of colors you supply can be of
  any length. Although the colors within the list themselves can be dynamic parameters, the content of the list is evaluated when the
  effect is created, so the number of colors and the color parameters themselves are fixed at that time.

  The `:step`, `:tolerance`, and `:colors` parameters may be dynamic, (and may be bound to show variables using the standard shorthand of
  passing the variable name as a keyword). Since `:step` and `:tolerance` are not associated with a specific head, they cannot be
  spatial parameters. The colors can be, however, so for example saturations can vary over the rig."
  [fixtures & {:keys [step tolerance colors] :as all}]
  {:pre [(some? *show*)]}
  (let [p (param/assemble all or-pinstripes)
        heads (chan/find-rgb-heads fixtures)
        group-fn (if (< (:tolerance p) 0.00001) :x #(math/round (/ (:x %) (:tolerance p))))
        stripes (gather-stripes heads group-fn (count (:colors p)))
        chases (map (fn [i stripe-heads]
                     (let [effects (map #(color-fx/color-effect "pin color" % stripe-heads) (:colors p))
                           pin-step (build-param-formula Number #(- % i) (:step p))]
                      (chase "Pinstripe" effects pin-step :beyond :loop)))
                    (range) stripes)]
  (apply fx/scene "Pinstripes" chases)))


(defn or-stripes "Default map for [[stripes]]" []
 {:vars {:step (build-step-param), :values [(color/like :royalblue) (color/like :orangered4)]}
  :opts {:tolerance 0}})
(defn stripes "Pinstripes for generic params" ;works already. for color anyways...
 [fixtures target-fn & {:keys [step tolerance values] :as all}] ;prob want to also have multiple target fns? like every other strobes and colors, etc
(let [p (param/assemble all or-stripes)
      ;; heads (chan/extract-heads-with-some-matching-channel fixtures #(= % target-fn)) ;eh tricky, pass as extra arg?
      heads fixtures
      group-fn (if (< (:tolerance p) 0.00001) :x #(math/round (/ (:x %) (:tolerance p))))
      stripes (gather-stripes heads group-fn (count (:values p)))
      chases (map (fn [i stripe-heads]
                   (let [effects (map #(target-fn stripe-heads %) (:values p)) ;just wrap fx-fn if not [fixtures level]
                         stripe-step (build-param-formula Number #(- % i) (:step p))]
                    (chase "Stripe" effects stripe-step :beyond :loop)))
                  (range) stripes)]
  (apply fx/scene "Stripes" chases)))


;  METRONOME EFFECT
(defn or-metronome-effect []
 {:vars {:down-color (color/like :red) :other-color (color/like :purple)}
  :opts {:metronome (:metronome *show*)}})
(defn metronome-effect "Flashes the supplied fixtures to the beats of the show metronome, emphasizing the down beat, and colored by `:down-color` /
 `:other-color` optional keyword arguments. If desired, a Metronome other than the default show one kan be used."
 [fixtures & {:keys [down-color other-color metronome] :as all}]
 {:pre [(some? *show*)]}
  (let [p (param/assemble all or-metronome-effect)
        heads (chan/find-rgb-heads fixtures)
        [running local-snapshot] (map atom [true nil]) ; Need to set up a snapshot at start of each run for all assigners
        snapshot (metro-snapshot (:metronome p)) ;; Need to use the show metronome as a snapshot to resolve our metronome parameter first
        [down-color other-color] (map #(params/resolve-unless-frame-dynamic % *show* snapshot) (map p [:down-color :other-color]))
        f (fn [show snapshot target was]
           (pspy :metronome-effect
                 (let [raw-intensity (* 2 (- (/ 1 2) (snapshot-beat-phase @local-snapshot 1)))
                       intensity (if (neg? raw-intensity) 0 raw-intensity)
                       base-color (if (snapshot-down-beat? @local-snapshot)
                                   (resolve-param down-color show @local-snapshot)
                                   (resolve-param other-color show @local-snapshot))]
                  (color/create (color/h base-color) (color/s base-color) (* (color/l base-color) intensity)))))
        assigners (fx/build-head-assigners :color heads f)]
   (Effect. "Metronome"
            (fn [show snapshot]  ;; Continue running until the end of a measure Also need to set up the local snapshot based on our private metronome for the assigners to use.
             (reset! local-snapshot (metro-snapshot (:metronome p)))
             (or @running (< (snapshot-bar-phase @local-snapshot) 0.9)))
            (fn [_ _] assigners)
            (fn [_ _] (reset! running false)))))  ;; Arrange to shut down at the end of a measure


;           DIMMER SWEEP
; XXX dimmer sweep with per-head virtual dimming.
(defn dimmer-sweep "An effect which uses an oscillator to move a bar of light across a group of fixtures. The width of the bar, maximum dimmer level, and whether the level should fade from the center of the bar to the edge, can be controlled with optional keyword arguments."
  [fixtures osci & {:keys [width level fade] :or {width 0.1 level 255 fade false}}]
  (let [width (bind-keyword-param width Number 0.1)
        level (bind-keyword-param level Number 255)
        fade (bind-keyword-param fade Boolean false)
        min-x (apply min (map :x fixtures))
        max-x (apply max (map :x fixtures))
        position (build-oscillated-param osci :min min-x :max max-x)]
    (apply fx/scene "Dimmer Sweep"
           (for [fixture fixtures]
             (let [fixture-level (build-param-formula Number
                                  (fn [position width level fade]
                                    (let [range (/ width 2)
                                          distance (math/abs (- position (:x fixture)))]
                                      (if (> distance range)
                                        0
                                        (if fade
                                          (* level (/ (- range distance) range))
                                          level))))
                                  position width level fade)]
               (dimmer-effect fixture-level [fixture])))))) ;:htp? false, :add-virtual-dimmers? true)))))) ;;add virtual dimmers so works for ledstrips...

(defn or-sweep []
 {:vars {:width 0.1 :level 255 :fade true :scale 1.0}})
(defn sweep "Dimmer sweep for any param... and adjustable positions"
  ;; [fixtures position & {:keys [width level fade effect-fn effect-keys scale name] :as args}]
  [fixtures position & {:keys [vars effect-fn effect-keys name] #_:as #_args
                        :or {effect-fn dimmer-effect}}]
  (let [p (param/assemble vars or-sweep)
        bounds (map #(apply % (map :x fixtures)) [min max])
        minmax (map (fn [b] (build-param-formula Number #(* %1 %2) b (:scale p))) bounds) ;use min-x max-x, or scale + "middle" for proper control of both ends?
        position (cond (or (number? position) (params/param? position)) position
                       (instance? IOscillator position) (build-oscillated-param position :min 0.0 :max 1.0)
                       :else (println (type position)))
        position (apply build-param-formula Number
                                      (fn [v lo hi] ; (param/clamped) or something, further... just send any 0-1 param or value, for pos...
                                       (let [hi (max lo hi)]
                                        (+ lo (* (- hi lo) v))))
                                      position minmax)
        f (fn [fixture position width level fade]
           (let [range (/ width 2)
                 distance (math/abs (- position (:x fixture)))]
            (if (> distance range)
             0
             (if fade (* level (/ (- range distance) range)) level))))]
    (apply fx/scene #_"Effect Sweep" (str #_(pretty-demunge effect-fn) (or name "Some") " Sweep")
           (for [fixture fixtures]
             (let [fixture-level (build-param-formula Number f fixture position (:width p) (:level p) (:fade p))]
               (apply effect-fn fixture-level [fixture] effect-keys)))))) ;why fixture in lone vec?

(defn color-strobe "Compound strobe/color effect, sets dimmers by show variable `:strobe-dimmers` (default 255), assigns color from show variable `:strobe-color` (default purple), and specified `lightness` (which may be dynamic), with a default of 100, whiting out the hue unless lowered.  Then sets fixtures' strobe channel to `level`, which may also be a dynamic parameter.
Designed to be run as a high priority queue, ideally held and with aftertouch adjusting a cue-introduced `lightness` or `level` var (which is used to control the strobe function of the affected fixtures, setting the strobe speed, and defaults to a middle value). The global strobe color can be adjusted via the show variables, either by aftertouch or by another effect with no assigners, like [[adjust-strobe]]."
  [level fixtures & {:keys [fx-name lightness]
                     :or {fx-name "Color Strobe"}}]
  (let [[level-p lightness-p dimmer-p]
         (map #(params/bind-keyword-param %1 Number %2)
              [level lightness :strobe-dimmers] [70 0.80 255])
        color-p (params/bind-keyword-param :strobe-color color/used-type (color/create "purple")) ;XXX fix conversion fn to all 0.0
        dimmer (dimmer-effect dimmer-p fixtures)
        color (color-fx/color-effect
               "Strobe Color" (param/color :color color-p :l lightness-p)
               fixtures :include-color-wheels? true)  ; :htp true tends to wash out right away
        function (chan-fx/function-effect "Strobe Level" :strobe level-p fixtures)]
   (fx/scene fx-name dimmer color function)))
;; (param/build-color-param :color (color/create "purple") :l 0.5)

(defn strobe "wraps strobe-2 with [level fixtures] standard ting"
 [level fixtures & {:keys [lightness name] :or {lightness 100 name "Strobe"}}]
 (let [level (resolve-param level *show* (metro-snapshot (:metronome *show*)))]
  (if (< 0 level) ;seems merely triggering sends 1 to strobe ch
   (fun/strobe-2 name fixtures level lightness)
   (fx/blank))))


(defn strobe-effect "Basic strobe effect that just strobes"
  [level fixtures & {:keys [lightness effect-name] :or {lightness 100 effect-name "Strobe"}}]
  {:pre [(some? *show*)]}
  (let [level-param (bind-keyword-param level Number 50)
        function (chan-fx/function-effect "strobe level" :strobe level-param fixtures)]
    (Effect. effect-name
             fx/always-active
             (fn [show snapshot] (fx/generate function show snapshot))
             (fn [show snapshot] (fx/end function show snapshot)))))

;           CROSSOVER CHASE
(defn shift [coll] (->> coll cycle (drop 1) (take (count coll))))

(defn build-cross-scene "Create scene setting color of a light while aiming it just below and in front of another."
  [move-key reference-key color]
  (fx/scene "Cross scene"
            (move/aim-effect "Cross" (Point3d. (:x (first (fixtures-named reference-key))) 0.0 0.2)
                             (fixtures-named move-key))
            (color-fx/color-effect "Cross color" color (fixtures-named move-key) :include-color-wheels? true)))

(defn or-crossover-chase []
 {:vars {:beats 1, :fade-fraction 0.25, :cross-color (color/like :orangered), :end-color (color/like :orange)}})
(defn crossover-chase "Create a sequential chase which gradually takes over all the moving heads from whatever they were doing, changes their colors, and makes
  them cross in an interesting pattern. By default, stages of the chase advance on every beat, but you can adjust that by passing in a
  different value for with the optional keyword argument `:beats`. To add a fade between stages, pass a non-zero value (up to 1, which
  means continually fade) with `:fade-fraction`.
  The color used during the crossover stages defaults to red, but you can pass a different color object to use with `:cross-color`."
  [fixture-keys & {:keys [beats fade-fraction cross-color end-color] :as all}]
  (let [p (param/assemble all or-crossover-chase :resolve-vars true)
        cross-elements (map #(build-cross-scene %1 %2 (:cross-color p)) fixture-keys (shift fixture-keys))
        fixtures (mapcat fixtures-named fixture-keys)
        step (build-step-param :interval-ratio (:beats p) :fade-fraction (:fade-fraction p))]
    (chase "Crossover"
           (concat (for [i (range 1 (inc (count cross-elements)))]
                     (apply fx/scene (str "Crossover Scene " i) (take i cross-elements)))
                   [(fx/scene "Crossover End"
                              (move/aim-effect "Cross End Point" (Point3d. 0.0 0.0 2.5) fixtures)
                              (color-fx/color-effect "Cross End color" (:end-color p) fixtures :include-color-wheels? true))
                    (fx/blank)])
           step :beyond :loop)))
;
 ;          CIRCLE CHAIN
(defn or-circle-chain []
 {:vars {:bars 2 :radius 1.0 :stagger 0.0}
  :opts {:right-wall -5 :left-wall 5 :rear-wall 10 :stage-wall -2 :ceiling 4}}) ;temp, shouldnt be in hurrr
(defn circle-chain "Create a chase that generates a series of circles on either the floor or the ceiling, causing a single head to trace out each, and passing them along from head to head.
  The number of bars taken to trace out each circle defaults to 2 and can be adjusted by passing a different value with the optional keyword argument `:bars`. The radius of each circle defaults to one
  meter, and can be adjusted with `:radius`. If you want each head to be tracing a different position in its circle, you can pass a value between zero and one with `:stagger`."
  [fixtures ceiling? & {:keys [bars radius stagger, right-wall left-wall rear-wall stage-wall ceiling] :as all}]
  (let [p (param/assemble all or-circle-chain :resolve-vars true)
        step (build-step-param :interval :bar :interval-ratio (:bars p))
        phase-osc (sawtooth :interval :bar :interval-ratio (:bars p))
        width (- (:right-wall p) (:left-wall p))
        front (if ceiling? 0.5 (:stage-wall p))
        depth (- (:rear-wall p) front)
        y (if ceiling? (:ceiling p) 0.0)
        heads (sort-by :x (move/find-moving-heads fixtures))
        [points running current-step] (map ref [(ring-buffer (count heads)) true nil])
        active-fn (fn [_ _] (dosync (or @running (seq @points)))) ;; Continue running until all circles are finished
        gen-fn (fn [show snapshot]
               (dosync
                (let [now (math/round (resolve-param step show snapshot))
                      phase (lfo/evaluate phase-osc show snapshot nil)
                      stagger (resolve-param (:stagger p) show snapshot)
                      head-phases (map #(* stagger %) (range))] ;well no need i guess since lazy but
                  (when-not (= now @current-step)
                    (ref-set current-step now)
                    (if @running  ;; Either add a new circle, or just drop the oldest
                      (alter points conj (Point3d. (+ (:left-wall p) (rand width)) y (+ front (rand depth))))
                      (alter points pop)))
                  (map (fn [head point head-phase]
                         (let [radius (resolve-param (:radius p) show snapshot head)
                               theta (* 2.0 Math/PI (+ phase head-phase))
                               head-point (Point3d. (+ (.x point) (* radius (Math/cos theta)))
                                                    (.y point)
                                                    (+ (.z point) (* radius (Math/sin theta))))]
                           (fx/build-head-parameter-assigner :aim head head-point show snapshot)))
                       heads @points head-phases))))
        end-fn (fn [_ _] (dosync (ref-set running false)))]
    (Effect. "Circle Chain" active-fn gen-fn end-fn))) ;; Stop making new circles, and shut down once all exiting ones have ended.


(defn build-aim-fan-point "Create a dynamic parameter which computes the aim point for a fixture in an [[aim-fan]] effect."
  [x y z x-scale y-scale]
  (let [params [x y z x-scale y-scale]
        dyn (boolean (some params/frame-dynamic-param? params))
        resolve-fn (fn [show snapshot head]
                     (let [params (map #(params/resolve-unless-frame-dynamic % show snapshot head) params)]
                       (apply build-aim-fan-point params)))
        eval-fn (fn [show snapshot head]
                  (let [[x y z x-scale y-scale]
                        (map #(params/resolve-unless-frame-dynamic % show snapshot head) params)
                        [dx dy] (map (fn [k sym] (- (k head) sym)) [:x :y] [x y])]
                  (Point3d. (+ (:x head) (* dx x-scale)), (+ (:y head) (* dy y-scale)), z)))]
   (Param. "Aim fan param" dyn Point3d resolve-fn eval-fn))) ;feels like a much smaller wrapper around a general aim param creator oughta do??

(defn aim-fan "Creates an aim effect which aims the lights outward around a reference point specified by `:x`, `:y`, and `:z`, which defaults to `(0, 0, 5)`, by first aiming the lights at the reference point, and then adding on the distance within the x-y plane from the fixture to that point, multiplied by `:x-scale` and `:y-scale`, which each default to `1`. A scale of `0` would aim the lights straight forward. Positive values fan them out, while negative values overshoot the reference point in the other direction, fanning them in. All parameters other than `fixtures` can be dynamic or keywords, which will be bound to show variables.  "
  [fixtures & {:keys [x y z x-scale y-scale effect-name]
               :or {x 0.0 y 0.0 z 5.0 x-scale 1.0 y-scale 1.0 effect-name "Aim Fan"}}]
  (let [params (map #(params/bind-keyword-param %1 Number %2)
                    [x y z x-scale y-scale] [0.0 0.0 5.0 1.0 1.0])]
    (move/aim-effect effect-name (apply build-aim-fan-point params) fixtures)))

(defn- build-twirl-vector "Create a dynamic parameter which computes the direction vector for a fixture in a [[twirl]] effect."
  [x y z radius osc]
  (letfn [(eval-fn [show snapshot head]
           (let [[x y z radius] (map #(params/resolve-param % show snapshot head) [x y z radius])
                 reference-point (Point3d. x y z)
                 head-point (Point3d. (:x head) (:y head) (:z head))
                 displacement (Point3d. 0.0 radius 0.0)
                 angle (* tf/two-pi (lfo/evaluate osc show snapshot head))
                 rotation (Transform3D.)]
            (.rotZ rotation angle)
            (.transform rotation displacement)
            (.add head-point displacement)
            (.sub head-point reference-point)
            (Vector3d. head-point)))
          (resolve-fn [show snapshot head]
           (let [params (map #(params/resolve-unless-frame-dynamic
                               % show snapshot head) [x y z radius osc])] ;osc wrong here no? not yet param...
            (apply build-twirl-vector params)))]
   (Param. "Twirl vector" true Vector3d eval-fn resolve-fn)))


(defn or-twirl []
 {:vars {:x 0.0 :y 0.0 :z -2.0 :radius 0.25 :beats 4 :cycles 1}})
(defn twirl "Direction movement effect fanning lights out from set point and then making rotations around this new direction, with phase offset by fixture x pos"
  [fixtures & {:keys [x y z radius beats cycles effect-name]
               :or {x 0.0 y 0.0 z -2.0 radius 0.25 beats 4 cycles 1 effect-name "Twirl"}}]
  (let [[x y z radius beats cycles] (map #(params/bind-keyword-param %1 Number %2)
                                         [x y z radius beats cycles] [0.0 0.0 -2.0, 0.25 4 1])
        osc (lfo/sawtooth :interval-ratio (params/build-param-formula Number #(/ %1 %2) beats cycles)
                          :phase (params/build-spatial-param fixtures :x :max 1.0))]
    (move/direction-effect effect-name (build-twirl-vector x y z radius osc) fixtures)))


;           CHANNEL / FUNCTION effects
;XXX maybe flip channels and channel-type??
(defn channel "Fixed to [level targets & effect-name] standard format... Returns an effect which assigns a dynamic value to all the supplied channels. If `level is a keyword, it will be looked up as a show variable. If `htp?` is true, applies highest-takes-precedence (i.e.  compares the value to the previous assignment for the channel, and lets the highest value remain)."
  [level channels & {:keys [htp? effect-name fixtures channel-type]
                     :or {fixtures (all-fixtures), effect-name "Channel effect"}}]
  {:pre [(some? effect-name) (some? *show*) (sequential? channels)]}
  (let [level (params/bind-keyword-param level Number 0)
        f (if htp?  ;; We need to resolve any dynamic parameters at this point so we can apply the highest-take-precedence rule.
            (fn [show snapshot target was]
              (max (colors/clamp-rgb-int (resolve-param level show snapshot (:head target)))
                   (or (colors/clamp-rgb-int (resolve-param was
                                                            show snapshot (:head target))) 0)))
            (fn [show snapshot target was] ;; We can defer resolution to the final DMX calculation stage.
              level))
        assigners (chan-fx/build-raw-channel-assigners channels f)]
    (Effect. effect-name
             fx/always-active
             (fn [show snapshot] assigners)
             fx/end-immediately)))


(defn function "Fixed to [level target &]. Returns an effect which assigns a dynamic value to all channels of the supplied fixtures or heads which have a range that implements the specified function. (Functions are a way for fixtures to use the same DMX channel to do multiple things, allocating ranges of values to get more dense use from a smaller number of channel allocations.) The `function` argument is the keyword by which the function information will be found for the supplied `fixtures`. The actual value sent for the channel associated with `function` for each fixture will be calculated by treating `level` as a percentage of the way between the lowest and highest DMX values assigned to that named function for the fixture. The name displayed for the effect in user interfaces is determined by `effect-name`.  If `:htp?` is passed with a `true` value, applies highest-takes-precedence (i.e. compares the value to the previous assignment for the channels implementing the function, and lets the highest value remain).  If you have multiple effects trying to control different functions which use the same channel, you are unlikely to get the results you want. Hopefully the fixture designers chose how to share channels wisely, avoiding this pitfall."
  [level function-type & {:keys [effect-name htp? fixtures] :or {fixtures (all-fixtures)}}]
  {:pre [(some? *show*) (some? function-type) #_(sequential? fixtures)]}
  (let [level (params/bind-keyword-param level Number 50)
        effect-name (or effect-name (str (name function-type) " effect"))
        function-type (keyword function-type) ;why, when passed as such?
        heads (chan-fx/find-heads-with-function function-type fixtures)
        f (if htp?  ;; We need to resolve any dynamic parameters at this point so we can apply the highest-take-precedence rule.
            (fn [show snapshot target was]
              (max (resolve-param level show snapshot target)
                   (or (resolve-param was show snapshot target) 0)))
            (fn [_ _ _ _] level)) ;; We can defer parameter resolution until the final DMX level assignment stage.
        get-assigners (fn [_ _] (chan-fx/build-head-function-assigners function-type heads f))]
    (Effect. effect-name fx/always-active get-assigners fx/end-immediately)))

(defn or-trigger [] {:vars {} :opts {}})
(defn trigger "Trigger code a la code-cue, whenever level hits 1.0 (or arbitrary threshold?). Or when level changes?"
 [level f & {:keys [args effect-name trigger-check-fn throttle-fn]
             :or {effect-name "Code trigger", trigger-check-fn #(>= % 1.0)
                  throttle-fn (fn [triggered-at now & {:keys [interval] :or {interval :bar}}] (= triggered-at (interval now)))}}]
 (let [level (params/bind-keyword-param level Number 0.0)
       gen-fn (fn [show snapshot]
               (when (trigger-check-fn (param/auto-resolve level))
                ()
                (apply f args)))]
  (Effect. effect-name fx/always-active gen-fn fx/end-immediately)))
;; ^^ this looks a lot like conditional-effect... am I going for a wrapper or low down?


(defn code "Calls f, once or per frame, (or timed/throttled?) for code to be run from cue grid,
   or some live coding coolness.  `f` must be a fn of 2 args, apart from your own.
   It will be called with those, and show / snapshot..
   Must return immediately, as is run in rendering pipeline, so any lengthy ops
   must be performed on another thread.  And on-end-fn can be provided, that will
            (hopefully) be called when effect ends, before shutdown. ;/"
  [f arg-fn & {:keys [kind on-end-fn] :or {kind :loop}}]
  {:pre [(ifn? f)]}
  (let [end-fn (if on-end-fn (fn [show snapshot]
                              (on-end-fn show snapshot) true) fx/end-immediately)]
    (Effect. "Code"
             (fn [show snapshot] true) ;here is where he ran f. better why?
             (fn [show snapshot] (or (f show snapshot) [])) ;If no assigners to return
             end-fn)))


(defmacro pass-live-fn "Guard fn in var so stays hot-reloadable inside fx, chases etc..."
 [v]
 (if (symbol? v)
  `(if (fn? ~v) (var ~v) ~v)
  v))

(defmacro call "Fetch and call :gen-fn for Effect while passing show and snapshot eh uselessss"
;;  [effect showsnap] ;oh yeah head as well ugh
 [effect show snap] ;oh yeah head as well ugh
 `(let [gen# (:gen-fn ~effect)]
   ;; (gen# ~@showsnap)))
   (gen# ~show ~snap)))


(defn live "Something more like quil where you can have a draw fn and changes reflect instantly, no relaunching bs"
 [draw-fn & {:keys [fx-name inputs starting-state state-var get-update-fn]
             :or {fx-name "Live" state-var :live-effect}}]
 (let [gen-fn (fn [show snapshot]
               ;; ((get-update-fn) state-var show snapshot)
               (if-let [assigners (draw-fn (show/get-variable state-var) show snapshot)]
                assigners
                []))]
  (Effect. fx-name fx/always-active gen-fn fx/end-immediately)))


(def generate-fade #'afterglow.effects/generate-fade) ;not public so gotta alias like this

(defn ms-elapsed ;shouldnt there already be a fn like this somewhere??
 [earlier & show]
 (if earlier
  (let [show (or (first show) *show*)
       now (metro-snapshot (:metronome show))]
  (apply - (map :instant [now earlier])))
  0)) ; 0 elapsed since right now if earlier is nil, makes sense
(def conditions [:default :mute :solo]) ;condition for mute and that? so dont rely on alpha 0 for it would then have to store sep anyways ; nil and :default would be same...
;; :mute easy enough. but :solo another fucking thing... guess it'd iterate over all effects pre-rendering and if any has :solo collect and only run those
(defn effect "Wraps other effect-fns in a fade to blank, and possibly other shit later. Like prio adjustment/autorestart?"
  [effect alpha & {:keys [timeout timeout-interval envelope condition] ;XXX alpha/condition should be ev integrated into Effect protocol?
                   :or {envelope {:a 600 :r 1000}
                        timeout-interval :ms ;oh yeah ms could be a thing with :beats :bars etc.
                        ;; timeout 1000000000}}] ; should be built in. but maybe more at the cue level...
                        timeout :global-effect-timeout-ms}}] ; should be built in. but maybe more at the cue level...
  {:pre [(some? *show*) #_(instance? Effect effect)]}
  (map #(validate-param-type % Number) [alpha timeout])
  (let [[alpha condition] (param/auto-resolve [alpha condition] :dynamic false)
        timeout (param/auto-resolve (bind-keyword-param timeout Number 3000 (str (:name effect) " timeout")))
        launched (metro-snapshot (:metronome *show*))
        ended (atom nil)
        end-fn (if (pos? (:r envelope))
                (fn [show snapshot]
                 (if (not @ended) (reset! ended snapshot))
                 nil)
                (:end-fn effect))
        active-fn (if (or timeout (pos? (:r envelope))) ;this makes more sense than like wrapping everything in chases right?
                   (fn [show snapshot]
                    (let [[ms-start ms-end] (map #(ms-elapsed % show) [launched @ended])]
                     (if (not @ended)
                      (if (< ms-start timeout)
                       (:active-fn effect)
                       (reset! ended snapshot)) ;since cant call end-fn on ourselves yet. no idea why wouldnt work when called to f above in let tho,..
                      (if (< ms-end (:r envelope))
                       (:active-fn effect)))))
                   (:active-fn effect))
        f (fn [show snapshot]
            (let [[alpha condition] (map #(resolve-param % show snapshot) [alpha condition])
                  [ms-start ms-end] (map #(ms-elapsed % show) [launched @ended])
                   fade (min 1.0 (/ ms-start (:a envelope 1))
                            (if (pos? (:r envelope)) (- 1.0 (/ ms-end (:r envelope))) 1))
                   alpha (* fade alpha)]
             (cond (autil/float>= alpha 1.0) ((:gen-fn effect) show snapshot)
                   (autil/float<= alpha 0.0) ((:gen-fn (fx/blank)) show snapshot)
                   :else (generate-fade (fx/blank) effect alpha show snapshot))))] ;could we do spatial param = fade effect per head/in space? or smooth fades out. or mixing bunch of lfos prob funky
    (Effect. (:name effect) active-fn f end-fn)))

