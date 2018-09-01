
(ns afterglow-tol.core "testing stuff, based on examples file by James Elliott" {:author "Joen Tolgraven"} ;; {{{
  (:require
   ;; [afterglow.channels               :as chan      :refer [extract-channels expand-heads find-rgb-heads]]
   ;; [afterglow.controllers            :as ct]
   ;; [afterglow.controllers.tempo]
   ;; [afterglow.midi                   :as midi      :refer [sync-to-midi-clock]]
   [afterglow.core                   :as core]
   [afterglow.effects                :as fx        :refer [chase scene]]
   [afterglow.effects.channel        :as chan-fx] ;:refer [function-effect] ;channel-effect
   [afterglow.effects.color          :as color-fx  :refer [color-effect transform-colors]]
   [afterglow.effects.show-variable  :as var-fx    :refer [variable-effect]]
   [afterglow.effects.cues           :as cues      :refer [cue code-cue function-cue color-fn-from-cue-var apply-merging-var-map]]
   [afterglow.effects.fun            :as fun]
   [afterglow.effects.movement       :as move]
   [afterglow.effects.oscillators    :as lfo       :refer [build-oscillated-param sine square sawtooth triangle]]
   [afterglow.effects.params         :as params    :refer [bind-keyword-param resolve-param param? frame-dynamic-param? validate-param-type
                                                           build-step-param build-aim-param build-pan-tilt-param build-spatial-param build-color-param
                                                           build-direction-param-from-pan-tilt build-param-formula]]
   [afterglow.effects.dimmer         :as dimmer    :refer [dimmer-effect master-set-level]]
   [afterglow.rhythm                 :as rhythm    :refer [metronome metro-snapshot metro-start snapshot-beat-phase snapshot-bar-phase snapshot-beat-within-bar snapshot-down-beat?]]
   [afterglow.show                   :as show      :refer [patch-fixture! all-fixtures fixtures-named set-cue! set-variable! get-variable
                                                           add-effect! end-effect! start! stop! sync-to-external-clock]]
   [afterglow.show-context                         :refer [*show* with-show set-default-show!]]
   [afterglow.transform              :as tf        :refer [degrees]]
   [afterglow.util                   :as util]
   [amalloy.ring-buffer                            :refer [ring-buffer]]
   [clojure.math.numeric-tower       :as math]
   [com.evocomputing.colors          :as colors    :refer [color-name create-color hue saturation lightness adjust-hue lighten darken saturate desaturate]]
   [com.evocomputing.colors.palettes.core :as palette]
   [overtone.osc                     :as osc  :refer [osc-handle osc-listen osc-send osc-close]]
   [taoensso.timbre                  :as timbre]
   [taoensso.timbre.profiling                      :refer [pspy]]
   [ola-clojure.ola-client           :as ola]
   [clojure.string                   :as string    :refer [capitalize upper-case]]
   ;; [clojure.core]
   ;; [clojure.pprint                                 :refer [pprint]]
   #_[debugger.core                    :as debug     :refer [break break-catch dbg dbg-defn]]
   ;; [afterglow.max.core]
   ;; [afterglow.max.init]
   [afterglow-tol.fixtures           :as tol]
   [afterglow.examples               :as example])

  (:import [afterglow.effects Effect]
           (afterglow.rhythm Metronome)
           [java.lang Boolean Long Double]
           [javax.media.j3d Transform3D]
           [javax.vecmath Point3d Vector3d]))
; }}}
; {{{ ALIASES
(def #^{:macro true} do-vm #'apply-merging-var-map) ;; give macro another name
; }}}
; {{{         VARIABLES
(defonce ^{:doc "Lets effects set variables in the running show."}  var-binder (atom nil))
(defonce ^{:doc "Holds show, so can unregister if re-creating."}    tol-show (atom nil))
(defonce ^{:doc "Holds Push 2 binding"}                             push (atom nil))
(defonce ^{:doc "Holds a per-beat resettable step-param"}           step-beat (atom nil))
(defonce ^{:doc "Holds a per-bar resettable step-param"}            step-bar (atom nil))
(defonce ^{:doc "Holds a per-phrase resettable step-param"}         step-phrase (atom nil))

(def rig-height       "Height of horizontal truss of main rig"      3.00)
(def stage-wall       "Wall behind the rig on the show Z axis."    -2.00)
(def house-rear-wall  "Wall behind audience on the show Z axis."    9.00)
(def left-wall        "House left wall on the show X axis."        -4.50)
(def right-wall       "House right wall on the show X axis."        4.50)
(def ceiling          "Ceiling on the show Y axis."                 5.00)

(def macro-record-file  "macros.clj") ;test, def this var supposed to enable macro-writing
(def midi-clock-bus   "IAC Bus") ;"to Max 1")
(def light-groups     "Named groups of lights for the cue grid."
 [:moving :moving-mini :cob :wash :strip :tube]) ; :milight :moving-beam
(def moving-heads     "Named individual moving heads" ;XXX quoted statement to grab everything starting with :moving auto, eval after patching. check (move/find-moving-heads)
 [:moving-1 :moving-2 :moving-3 :moving-mini-1 :moving-mini-2 :moving-mini-3])
(def pixtol-channels  "DMX control channels for pixel strips (minus dimmer, strobe/shutter), 8 a page"
 [:attack :release :dimmer-attack :dimmer-release :bleed :noise :rotate-back :rotate-fwd]) ;:rotate-hue later. just one rotate later for float/osc tho
(def global-channels-functions  "Other fixture channels/functions where global control is needed"
 [:fog :movement-speed :focus])

(def default-lightness 70)
(def default-saturation 70)
(def default-measure (tf/build-distance-measure 0 rig-height 0 :ignore-z true)) ; :ignore-z true

(def universes        "Universes used for show"       [1 2 3 4 10])
(def default-color-like-spread {:h 50 :s 30 :l 15})
(defn color-like "Return a random color similar to one passed in, with adjustable rng range per h s l"
 [color & {:keys [h s l ] :or {h 50 s 30 l 15}}]

 (let [color (create-color #_params/interpret-color (or color "black")) ;XXX nil, should result in black, or a random color?
       [h s l] (map (fn [k]
                     (or (k :keys)
                      (try (get-variable (keyword (str "color-like-spread-" (name k)))) (catch Exception e))
                      (k default-color-like-spread)))
                    [:h :s :l])
       [hr sr lr] (map rand [h s l])
       [hw sw lw] (map #(- %1 (* %1 %2)) [h s l] [0.5 0.25 0.25]) ;weight, where is (inverse) center? 0.25 = 75% below
       [h s l] (map - [hr sr lr] [hw sw lw])
       s (if (< 70 (saturation color))
          (- s (- (saturation color) 80)) ;lol
          s)] ;fuck overly saturated shit
  (create-color :h (colors/clamp-hue (+ (hue color) h))
                :s (colors/clamp-percent-float (+ (saturation color) s))
                :l (colors/clamp-percent-float (+ (lightness color) l)))))

(defn patch []
 (patch-fixture! :moving-1 (tol/rgbw-36-moving)           10 200 :y 1.7 :x -1.5
                       :y-rotation (tf/degrees 0))
  (patch-fixture! :wash-1 (tol/rgb-54-3-par)     10   1 :y 2.2 :x -4.2) ; :z -0.3) ;not as far out but points to corner so
  (patch-fixture! :tube-1 (tol/rgbw-pixel-strip :pixels 125  :x-start 1.0 :x-end 0.0 :y-start 0.0 :y-end 1.0 :z 0.0) 2 1)  ;; L RGBW 120strip
 )

(defn activate-show [] ;{{{
  (core/init-logging)
  (set-default-show!
   (swap! tol-show (fn [s]
                    (when s (with-show s (show/stop!) (show/blackout-show))
                            (show/unregister-show s))
                    (show/show :description "tol show"
                               :refresh-interval 25 ; DMX 29 to 44 hz. <=25 (40) sometimes wdt's ESP.. 20 (50) makes cpu go bananas. 25/40 default
                               :universes universes))))
  ;; (ct/auto-bind *show* :device-filter "Ableton Push")                   ;; bind show to any compatible grid controllers connected now or in the future. CoreMidiSource.java GOES NUTS IF WAVETICK SENDS PHASE!!!
  ;; (reset! push (ct/bind-to-show *show* "User Port"))
  ;; (show/register-grid-controller) ;see if can get quicker bind if use this straight instead of autobind?
  ;; (when (nil? @core/osc-server) (core/start-osc-server osc-port-receive)) (clear-osc-var-bindings) (clear-osc-cue-bindings)  ;; Activate OSC server, clear any previous bindings
  (reset! var-binder (var-fx/create-for-show *show*)) ;; Enable cues whose purpose is to set show variable values while they run.
  (try (core/start-web-server 16000 false) (catch Exception e))     ;; not using webserver normally but need it for macros etc...
  (core/start-nrepl 5000) ;; already using parent repl
  ;; (try (sync-to-external-clock (sync-to-midi-clock #_midi-clock-bus)) (catch Exception e))
  '*show*)



(defn value "shortcut resolve param to val"
 [param]
 (resolve-param param *show* (metro-snapshot (:metronome *show*))))
(def s (build-oscillated-param (sine)))
(value s)
(value (build-oscillated-param (sine)))
(resolve-param (bind-keyword-param 0 Number 255) *show* (metro-snapshot (:metronome *show*)))
(resolve-param s *show* (metro-snapshot (:metronome *show*)))
