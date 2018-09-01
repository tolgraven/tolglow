
(ns afterglow-tol.core "testing stuff, based on examples file by James Elliott" {:author "Joen Tolgraven"} ;; {{{
  (:require
   [afterglow.channels               :as chan      :refer [extract-channels expand-heads find-rgb-heads]]
   [afterglow.controllers            :as ct]
   [afterglow.controllers.tempo]
   [afterglow.midi                   :as midi      :refer [sync-to-midi-clock]]
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
   [clojure.core]
   [clojure.pprint                                 :refer [pprint]]
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
; make fun to generate these...
;; (def lfo-p build-oscillated-param)
;; (def step-p build-step-param)
;; (def color-p build-color-param)
;; (def spatial-p build-spatial-param)
;; (def aim-p build-aim-param)
;; (def dir-from-pt-p build-direction-param-from-pan-tilt)
;; (def pt-p build-pan-tilt-param)
;; (def saw sawtooth)
;; (def do-vm apply-merging-var-map) ;cant take value of a macro, redefine it...
(def #^{:macro true} do-vm #'apply-merging-var-map) ;; give macro another name
; }}}
; {{{         VARIABLES
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

(def tol-ns 'afterglow-tol.core) ; should be conditionally set so afterglow.max.init also works...{{{
(def is-max false)
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
;}}}}}}
; {{{         VAR-MAP
;XXX introduce additional key like :unprotected, if true opens var to "non-action" modification, eg randomizer functions
(defn make-var "Create variable for cue from minimum required information"
 [[var-key var-start var-min var-max] & {:keys [var-type var-name velocity vel-min vel-max centered resolution]}] ;put start before min-max i guess so can skip them for bools
  (let [var-name (or var-name (capitalize (name var-key)))
        var-start (if (= (type var-key) ::colors/color) var-key ;just starting color obj for color var
                    (if (nil? var-start) (+ var-min (rand (- var-max var-min))) ;make something up. can't get here tho since (type nil) throws, right?
                      (condp = (type var-start)
                      java.lang.String (params/interpret-color var-start)
                      java.lang.Boolean (if-not var-min var-start) ;bool and no var-min/max means is bool, else bool start means velocity, so nil
                      var-start)))
        var-key (if (= (type var-key) ::colors/color)
                 (str "color" (when-not (nil? var-start) (str "-" var-start)))
                 var-key) ;easy color-1 color-2 etc. or could use min-max as bounds
        ;; var-start (cond (= (type var-key) ::colors/color) var-key ;just starting color obj for color var
        ;;                 (nil? var-start) (+ var-min (rand (- var-max var-min))) ;make it random between min and max
        ;;                 (string? var-start) (color-like var-start)
        ;;                 (instance? Boolean var-start) (if-not var-min var-start) ;bool and no var-min/max means is bool, else bool start means velocity, so nil
        ;;                 :else var-start)
        ;;
        ;;               ;; (condp = (type var-start)
        ;;               ;; java.lang.String (params/interpret-color var-start)
        ;;               ;; java.lang.Boolean (if-not var-min var-start) ;bool and no var-min/max means is bool, else bool start means velocity, so nil
        ;; ;;               var-start)))
        ;; var-key (if (= (type var-key) ::colors/color)
        ;;          (str "color" #_(when-not (nil? var-start) (str "-" var-start)))
        ;;          var-key) ;easy color-1 color-2 would be cool
        var-type (or var-type (condp = (type var-start)
                               java.lang.Boolean :boolean
                               java.lang.Long    :integer
                               ::colors/color    :color
                               ;; clojure.lang.Keyword (type (get-variable var-start))
                               nil)) ;add, if type keyword try to look up and auto bind to show var?
        velocity (or velocity (when (and var-min (not var-start)) true))
        segments {:key var-key :name var-name :min var-min :max var-max :start var-start :type var-type
                  :velocity velocity :velocity-min vel-min :velocity-max vel-max
                  :centered centered :resoution resolution}]
   (into {} (filter (fn [[k v]] (not (nil? v))) segments)))) ;nuke any nil vals

(defn make-vars [& lots]
 (map make-var lots))

;TODO more robustly defined types of vars (not in the color/bool/number sense)
;and ways of easily generating them / interfacing out
;these "types"/groups should have additional properties like color
(def var-data ; or have scaler to create -dmx versions but nah moving toward all floats anyways
 {:beats         ["beats"      4   1  32]
  :bars          ["bars"       2   1  16]
 :phrases        ["phrases"    1   1   8]
  :cycles        ["cycles"     1   1   8]
  :max-num       ["max"      1.0 0.0 1.0],    :min-num       ["min"     0.0  0.0  1.0]
  :max-dmx       ["max"      255 0   255],    :min-dmx       ["min"       0    0  255]
  :phase         ["phase"    0.0 0.0 1.0]
  :width         ["width"   0.25 0.0 1.0],    :width-denom   ["width"     4    1   16]
  :alpha         ["alpha"   1.00 0.0 1.0]
  :down          ["down"    true]

  :lfo-gain      ["lfo-gain"   1.0  0.0  5.0]
  :lfo-offset    ["lfo-offset" 0.0 -3.0  3.0]
  :lfo-noise     ["lfo-noise"  0.0  0.0  1.0]
  :lfo-picker    ["lfo-picker" 0    0    3]

  :min-change    ["min-change" 0.15  0.0  0.33]

  :hue           ["hue"        0 -360  360] :hue-mod       ["hue-mod"          30 -180  180]
  :lightness     ["lightness"  0  -50   50] :lightness-mod ["lightness-mod"   -7   -30   30]
  :saturation    ["saturation" 0  -50   50] :saturation-mod ["saturation-mod" -10  -30   30]

  :pause         ["pause"  false]
  :htp           ["htp"     true]
  :fade          ["fade"    true]
  :level         ["level"    0.0  0.0  1.0],  :level-vel     ["level"   true  0.0  1.0],
  :level-dmx     ["level"   200    0  255],   :held-dmx      ["level"   true    0  255] ;flag for velocity at var-start
  :fraction      ["fraction" 0.0  0.0  1.0],  :fraction-vel  ["fraction" true  0.0  1.0]
  :fade-fraction ["fade-fraction" 0.0  0.0  1.0]
  :fade-time     ["fade-time" 200  1  2500]
  :chance        ["chance"  0.0001 0.0  1.0]
  :color         ["color" "antiquewhite2"]
  ;; :color         ["color" (color-like "antiquewhite2")] ;nice if :color type could also take min/max, as colors, bounds per hsl (:min :black :max :white = full range)
  ;; :color-rand    ["color"   (color-like nil)]
  ;nicer yet if color picker overlay reflected this XXX first fix overlay less rigid using (color-like)
  })

(def default-vars (into {} (for [[k v] var-data] {k (make-var v)})))

(defn use-vars "Get variables from default-vars map. SHOULD handle any combination. SHOULD easily mod or extend templates"
 [variables & special] ;special is full defs added onto others
 (let [found (into [] (map default-vars variables))
       all (if special (into found (flatten special)) found)]
  (into [] all))) ;varmap is vector of maps

; per-var can merge in overloads for specific keys. so think eg grab general lfo stuff, adjust starting phase without making a var from scratch?
(defn cue-vars "resolve loose keywords through use-vars, merge complete (vector) var-maps and single-map vars"
 [& more]
 (let [more (map (fn [element]
                  (condp = (type element)
                   clojure.lang.Keyword            (use-vars [element])
                   clojure.lang.PersistentArrayMap (conj [] element)
                   ::colors/color                  (conj [] (make-var [element]))
                   java.lang.String                (conj [] (make-var [(color-like element)]))
                   element))
                 more)]
  (reduce #(into %1 %2) [] more)))

(defn cue-var "no vector" [k] (k default-vars))
(defn cv [k start] (assoc (cue-var k) :start start))

(defn get-map-with "extract single map from vector of vars, by string matched to value of some key, default :key..."
 [string coll & {:keys [by] :or {by :key}}]
 (into {} (filter #(= (by %) string) coll)))

(def lfo-common (cue-vars :beats :cycles :min-dmx :max-dmx :phase))
(def lfo-scale  (cue-vars :lfo-gain :lfo-offset))
(def lfo-all (cue-vars lfo-common :down :width-denom lfo-scale))
;; (def sparkle-common (cue-vars :chance :fade-time (make-var ["color" (create-)]))
(defn interval [start]
 (let [[b c] (cue-vars :beats :cycles)]
  (conj [(assoc b :start start)] c)))


(def lfo-types        "Types of oscillators and associated colors"
 (let [common (cue-vars :beats :cycles :min-dmx :max-dmx :phase :alpha)
       scale  (cue-vars :lfo-gain :lfo-offset)
       most   (cue-vars common scale)] ;XXX fucking up bloom since they go 0-1, how fix til proper scaling sorted?
     [{:type "held"     :color "antiquewhite2" :variables (cue-vars :held-dmx)} ;could also do with scale/gain...
      {:type "level"    :color "white"         :variables (cue-vars :level-dmx)}
      {:type "random"   :color "lightskyblue3" :variables (cue-vars common :min-change)} ;first try rng-param then fix better.
      {:type "sawtooth" :color "salmon3"       :variables (cue-vars common :down scale)}
      {:type "triangle" :color "orangered1"    :variables most}
      {:type "sine"     :color "navajowhite2"  :variables most}
      {:type "square"   :color "darkseagreen4" :variables (cue-vars common :width-denom)}]))
      #_{:type "multi"    :color "pink"          :variables (cue-vars most (use-vars [:lfo-picker :down :width-denom]))}
     ;{:type "external" :color "whatever"      :variables (some shit bound to show vars auto bound to max)}

(def get-map-for-lfo #(get-map-with % lfo-types :by :type))
(defn lfo-starting-vm "Lookup starting/default values for lfo-type, for visualizing before actually running. Only supports number values"
 [lfo-type]
 (into {} (map (fn [v]
                    [(keyword (:key v)) (or (:start v) 0)])
                  (:variables (get-map-for-lfo lfo-type)))))

(defn setup-global-test-defs "So can eval most stuff in place wherever without calling fns Proper way prob defonce and atom, and then fns to update/bind these
                              Which also helps, having dynamic = bind to relevant for current work"
 []
 (def show *show*)
 (def snapshot (metro-snapshot (:metronome show)))
 (def lfo-type "sawtooth")
 (def vm (lfo-starting-vm "sine") #_(:variables (get-map-with lfo-type lfo-types :by :type)))
 (def sin (sine))
 #_(value sin))

; }}}
   ;; [clojure.tools.namespace.repl                   :refer [refresh]] ;put in profiles...

; {{{ OSC
; {{{ VARIABLES
(def osc-address      "IP for OSC messages"   "127.0.0.1")
(def osc-port-receive "Port for incoming OSC control"  16010)
(def osc-port-send    "Port for outgoing OSC state"    16011)
(defonce ^{:doc "Holds the OSC client"} osc-client (atom nil))
(defonce ^{:doc "Holds OSC cue bindings, to clear out before re-creating the show."} osc-cue-bindings (atom #{}))
(defonce ^{:doc "Holds OSC cue bindings, to clear out before re-creating the show."} osc-var-bindings (atom #{})) ;}}}
; {{{ OSC-CUE
(defn add-osc-cue-binding "Set up bind so state of a cue gets sent per OSC. Record that fact, so can clean up later. Then set up so that incoming OSC messages can start and end that cue."
  [x y path]
  (let [bind (fn [state _ _]
                 ;; (osc-send @osc-client path (if (= state :started) 1 0))
                  (case state
                    :started (osc-send @osc-client path 1)
                    :ended (osc-send @osc-client path 0)
                    nil))]
    (swap! osc-cue-bindings conj [x y bind path])
    (ct/add-cue-fn! (:cue-grid *show*) x y bind))

  (osc-handle @core/osc-server path
    (fn [{[vel two] :args :as msg}] ; XXX set up so msg can set state of cue var(s?) so dont have to manually expose. like max.Cue
      (let [[cue active] (show/find-cue-grid-active-effect *show* x y)
            vel (if (integer? vel) vel (* 127 vel))]
        (when cue
          (if (pos? vel)
            (if (and active (not (:held cue)))
              (end-effect! (:key cue))
              (show/add-effect-from-cue-grid! x y :velocity vel)) ;XXX get var taking velocity and conform to its spec (0-1, 0-255...) but really go all 0-1 everything always yo
            (when (and active (:held cue)) ;kill if vel 0, since is held cue
              (end-effect! (:key cue)))))))))

(defn- clear-osc-cue-bindings "Clear out any OSC cue bindings which have been established." []
  (doseq [[x y f path] @osc-cue-bindings]
    (ct/clear-cue-fn! (:cue-grid *show*) x y f)
    (osc-handle @core/osc-server path (fn [msg] :done)))
  (reset! osc-cue-bindings #{}))
; }}}
; {{{ OSC-VAR
(defn add-osc-var-binding "Arrange to send an OSC message whenever the value of a show variable changes, and record that we did that so it can be cleaned up later. Then set things up so incoming OSC messages update the value of that variable.
  If you need to do anything more complicated than send a message with the raw value of the variable, or update the variable with the raw first value from the incoming OSC message, you can pass your own functions with the optional keyword arguments `:send-fn` and `:receive-fn`. `:send-fn` will be called with the keyword identifying the variable that has changed, and its new value.  `:receive-fn` will be called with the incoming OSC message.
  If you want this binding to not affect reception of messages on the OSC path (for example because you have another variable binding set up which processes these messages, since they contain values for multiple show variables), then pass `:none` as the value for `:receive-fn`."
  [var-key path & {:keys [send-fn receive-fn]}]
  (let [have-receiver (not= receive-fn :none)
        bind (or send-fn
                 (fn [_ v] (osc-send @osc-client path v)))]
    (swap! osc-var-bindings conj [var-key bind path have-receiver])
    (show/add-variable-set-fn! var-key bind)

    (when have-receiver
      (osc-handle @core/osc-server path
                  (or receive-fn
                      (fn [{[vel two] :args :as msg}]
                       (set-variable! var-key vel)))))))

(defn- clear-osc-var-bindings "Clear out any OSC var bindings which have been established." []
  (doseq [[k f path have-receiver] @osc-var-bindings]
    (show/clear-variable-set-fn! k f)
    (when have-receiver
     (osc-handle @core/osc-server path #(:done) #_(fn [msg] :done))))
  (reset! osc-var-bindings #{})) ; }}}
;; (defn off [msg] :done)
; {{{ OSC START/STOP
(defn osc-demo "Early experiments with using OSC to control shows. This should grow into a well-defined API, with integration to show variables, cue grids, and the like."
 []
  (when (nil? @core/osc-server) (core/start-osc-server osc-port-receive))
  (let [width (- right-wall left-wall)
        depth (- house-rear-wall stage-wall)]
    (osc-handle @core/osc-server "/1/aim-a" (fn [{[one two] :args :as msg}] ; & res :as msg
                      (set-variable! :aim-group-a-x (+ left-wall (* width one)))
                      (set-variable! :aim-group-a-z (+ stage-wall (* depth two)))
                      #_(timbre/info msg)))
    (osc-handle @core/osc-server "/1/aim-a-y" (fn [msg]
                      (set-variable! :aim-group-a-y (* ceiling (first (:args msg))))
                      #_(timbre/info msg)))
    (osc-handle @core/osc-server "/1/aim-b" (fn [msg]
                      (set-variable! :aim-group-b-x (+ left-wall (* width (first (:args msg)))))
                      (set-variable! :aim-group-b-z (+ stage-wall (* depth (second (:args msg)))))
                      #_(timbre/info msg)))
    (osc-handle @core/osc-server "/1/aim-b-y" (fn [msg]
                      (set-variable! :aim-group-b-y (* ceiling (first (:args msg))))
                      #_(timbre/info msg))))
  (osc-handle
    @core/osc-server "/1/sparkle" (fn [msg]
                                    (if (pos? (first (:args msg)))
                                      (show/add-effect!
                                       :sparkle (fun/sparkle (all-fixtures)
                                                             :chance (/ (first (:args msg)) 256.0)
                                                             :fade-time 20))
                                      (show/end-effect! :sparkle))))
  (osc-listen @core/osc-server (fn [msg] (timbre/info msg)) :debug)
  (osc/zero-conf-on)

  (doseq [x (range 32) y (range 32)] (add-osc-cue-binding x y (str "/" x "/" y)))
  (print "osc up"))


(defn osc-shutdown "Shut down osc server and clean up." []
  (clear-osc-var-bindings)
  (clear-osc-cue-bindings)
  (core/stop-osc-server)
  (swap! osc-client #(when % (osc-close %) nil)))
; }}}
; }}}
; {{{         CONTROLLERS
;; XXX XXX yoyo XXX solution to controlling color lightness from push: touch both encoders = touchstrip becomes lightness (curr controls both hue and sat instead heh)
;; send DMX ctrl chs with flags for pixels in stream, esp for when multiple streams overdubbed. Then can switch htp/lowtp/blend modes etc per pixel instead of stream - and barely takes up data!!!
;; ^^ same flag map for the ctrl chs, for easy override dimmer, cancel out unwanted rotation or whatnot.  fuck yeah. FUCK YEAH.
(defn push-toggle "Take control of Push2 from Ableton Live, or give it back"
 []
 ;; needs to create/destroy a virtual port Live is pointed to. Find shell thing to do it until I figure out CoreMidi4J?
 ;; using copperlan for now..
 (if (nil? @push)
  (reset! push (ct/bind-to-show *show* "User Port"))
  (do
   (ct/deactivate push)
   (reset! push nil))))
; }}}

; {{{ PATCHING
(defn patch-moving []; {{{2
  (patch-fixture! :moving-1 (tol/rgbw-36-moving)           10 200 :y 1.7 :x -1.5
                       ;; :relative-rotations [ ; [:x-rotation (tf/degrees 180)]]
                       ;;                      [:y-rotation (tf/degrees 180)]])
                       ;; :x-rotation (tf/degrees 180) ; display facing right, panREV off (others on) and this works when it's standing. but dumb because resting position becomes opposite and shit
                       :y-rotation (tf/degrees 0))
  (patch-fixture! :moving-2 (tol/rgbw-12-12-moving)        10 156 :y 1.75 :x 1.32 :z 0.0
                       :x-rotation (tf/degrees 0)
                       :y-rotation (tf/degrees 0))
  (patch-fixture! :moving-3 (tol/rgbw-60-moving-beam) 10 180 :y 4.4 :x 0.0 :z 0.4 ;; XXX wtf even tho it's high up etc the second I aim higher than 0 it starts moving up. Why?
                       ;; :z-rotation (tf/degrees 180)
                       ;; :x-rotation (tf/degrees 180)
                       :y-rotation (tf/degrees 180))
  (patch-fixture! :moving-mini-1  (tol/rgbw-7-12-moving)   10 100 :y 4.00 :x -2.0
                       :y-rotation (tf/degrees 0))
  (patch-fixture! :moving-mini-2  (tol/rgbw-7-12-moving)   10 114 :y 4.00 :x 0.0
                       :y-rotation (tf/degrees 0))
  (patch-fixture! :moving-mini-3  (tol/rgbw-7-12-moving)   10 128 :y 4.00 :x 2.00 ;:z -0.2
                       :y-rotation (tf/degrees 0))) ; }}}
(defn patch-static []; {{{2
  (patch-fixture! :wash-1 (tol/rgb-54-3-par)     10   1 :y 2.2 :x -4.2) ; :z -0.3) ;not as far out but points to corner so
  (patch-fixture! :wash-2 (tol/rgb-54-3-par)     10   8 :y 2.2 :x  4.2) ; :z -0.3)
  (patch-fixture! :wash-3 (tol/rgbw-7-12-par)    10  15 :y 4.2 :x -1.8) ; this one seems fucked FYI, stuck on white/no response to dmx...
  (patch-fixture! :wash-4 (tol/rgbw-7-12-par)    10  23 :y 4.2 :x  1.8) ;5 :z -0.2)

  ;; (patch-fixture! :wash-5 (tol/adj-pro-led-tol)  10  31 :y 0.0 :x -1.5)
  #_(patch-fixture! :wash-6 (tol/rgbw-mirror)      10  37 :y 2.2 :x  0.0) ; :z 0.7)
  (patch-fixture! :wash-7 (tol/rgbw-18-12-par)   10  60 :y 1.7 :x -2.1)
  ;; (patch-fixture! :wash-8 (tol/rgbwauv-5-18-par) 10  70 :y 1.6 :x  2.1)

  (patch-fixture! :cob-1 (tol/wa-100-cob-par)    10  50 :y 3.1 :x  0.1)

  (patch-fixture! :fogger-1 (tol/af-250-fogger)  10 511 :y 3.0 :x 0.1)
  ); }}}
(defn patch-strips []; {{{2
  (patch-fixture! :strip-1 (tol/rgbw-pixel-strip :pixels 120 :x-start -0.2 :x-end -1.2 :y-start 0.0 :y-end 0.0 :z 0.0) 1 1)  ;; L RGBW 120strip
  ;; (patch-fixture! :strip-1 (tol/rgbw-pixel-strip :pixels 120 :x-start -0.5 :x-end -2.5 :y-start 2.0 :y-end 2.0 :z -0.0) 1 1)  ;; L RGBW 120strip
  (patch-fixture! :tube-1 (tol/rgbw-pixel-strip :pixels 125  :x-start 1.0 :x-end 0.0 :y-start 0.0 :y-end 1.0 :z 0.0) 2 1)  ;; L RGBW 120strip
  ;; (patch-fixture! :strip-2 (tol/rgbw-pixel-strip :pixels 125  :x-start -1.0 :x-end  -0.01 :y-start 0.0 :y-end 0.0) 3 1) ;; R RGBW 144
  ;; (patch-fixture! :strip-4 (tol/rgbw-pixel-strip :pixels 120 :x-start  0.0 :x-end  0.0 :y-start 0.0 :y-end -2.0 :z 0.0) 4 1) ;; R RGBW 144
  ;; (patch-fixture! :tube-1 (tol/rgb-pixel-strip   :pixels 144 :x-start -0.5 :x-end  0.5 :y-start 2.0 :y-end 2.0 :z -0.0) 2 1)   ;;   RGB  144, midpiece (2x 2m folded over = 576 leds)
  ;; (patch-fixture! :strip-3 (tol/rgbw-pixel-strip :pixels 60  :x-start 2.0 :x-end  2.0 :y-start 1.5 :y-end 1.5 :z-start -0.1 :z-end -1.1) 3 1) ;; R RGBW 144
  ;; (patch-fixture! :strip-3 (tol/rgbw-pixel-strip :pixels 60  :x-start -2.0 :x-end  0.0 :y-start 1.5 :y-end 1.5 :z-start -0.1 :z-end -0.1) 3 1) ;; R RGBW 144
  ;; (patch-fixture! :strip-3 (tol/rgbw-pixel-strip :pixels 125  :x-start -1.0 :x-end  -0.01 :y-start 0.5 :y-end 1.5 :z-start -0.1 :z-end -0.1) 3 1) ;; R RGBW 144
  ;; (patch-fixture! :strip-5 (tol/opc-strip :pixels 170  :x-start 0.0 :x-end 2.0 :y-start 0.0 :y-end 0.0 :z 0.0) 5 1)  ;; L RGBW 120strip
  ;; (patch-fixture! :strip-6 (tol/rgbw-pixel-strip :pixels 60  :x-start -1.0 :x-end  1.0 :y-start 0.0 :y-end 0.0 :z -0.0) 6 1) ;; R RGBW 144
  ;; (patch-fixture! :strip-2 (tol/rgbw-pixel-strip :pixels 120 :x-start  0.5 :x-end  2.5 :y-start 2.0 :y-end 2.0 :z -0.0) 4 1) ;; R RGBW 120

  ;; (patch-fixture! :tube-2 (tol/rgbw-pixel-strip :pixels 60  :x-start -0.5 :x-end  0.5  :y-start 0.0 :y-end 0.0 :z -0.9) 4 1)  ;; M RGBW 60
); }}}
(defn patch-all []
  (patch-moving) (patch-static) (patch-strips)
  ;; (reset! strips-tubes (concat (fixtures-named :strip) (fixtures-named :tube))))
)
; }}}

(defn activate-show [] ;{{{
  (core/init-logging)
  (set-default-show!
   (swap! tol-show (fn [s]
                    (when s (with-show s (show/stop!) (show/blackout-show))
                            (show/unregister-show s))
                    (show/show :description "tol show"
                               :refresh-interval 25 ; DMX 29 to 44 hz. <=25 (40) sometimes wdt's ESP.. 20 (50) makes cpu go bananas. 25/40 default
                               :universes universes))))
  (ct/auto-bind *show* :device-filter "Ableton Push")                   ;; bind show to any compatible grid controllers connected now or in the future. CoreMidiSource.java GOES NUTS IF WAVETICK SENDS PHASE!!!
  (reset! push (ct/bind-to-show *show* "User Port"))
  ;; (show/register-grid-controller) ;see if can get quicker bind if use this straight instead of autobind?
  (when (nil? @core/osc-server) (core/start-osc-server osc-port-receive)) (clear-osc-var-bindings) (clear-osc-cue-bindings)  ;; Activate OSC server, clear any previous bindings
  (reset! var-binder (var-fx/create-for-show *show*)) ;; Enable cues whose purpose is to set show variable values while they run.
  (try (core/start-web-server 16000 false) (catch Exception e))     ;; not using webserver normally but need it for macros etc...
  (core/start-nrepl 5000) ;; already using parent repl
  (try (sync-to-external-clock (sync-to-midi-clock #_midi-clock-bus)) (catch Exception e))
  '*show*)  ;; Return the symbol *show*, rather than value itself - avoid crazy spewing REPL }}}

; {{{ HANDY
;; BOOTSTRAP WHEN USING REPL: basically these below, for some reason if initing while booting repl it gets confused about which object is holding the reference. So stay manual for now.{{{
(defn start "bootstrap errthang" []
  (activate-show)
  (patch-all)
  (osc-demo)
  ;; (clojure.pprint/pprint (midi/scan-midi-environment))

  (declare make-cues-tol) (try (make-cues-tol) (catch Exception e))
  (add-effect! :beam-shutter (chan-fx/function-effect "Shutter Open" :shutter-open 8 (fixtures-named "moving-3")))
  )
; }}}


(defn shortcuts "not for running, just easy to eval shortcuts" [] ; {{{
  (osc-shutdown)
  (osc-demo)

  (show/address-map)

  (show/add-effect-from-cue-grid! #_x #_y :var-overrides #_(get vm key, min/max, then randomize)) ;easy way to switch up running effects - just retrigger them and randomize overrides...

  (ct/deactivate-all)
  (doseq [x (range 31) y (range 31)] #_(print x y ".") (show/clear-cue! x y))
  (show/blackout-universe 2)
  (show/unregister-show *show*)
  (show/blackout-show)
  (show/remove-fixture! :cob-1) ; how fetch fixtures for this? since (all-fixtures) doesnt give names but spews everything

  (pprint (:meta @(:active-effects show)))
  (eval [(show/stop!) (show/profile-show)])
  (eval [(show/stop!) (show/profile-show :serial? true)])
  (chan/extract-heads-with-some-matching-channel)) ;; good for using capabilities without manual grouping }}}}}}

(defn afx [] (show/active-effect-keys *show*))
;; (pprint (afx))
(defn profile [& serial] (do (show/stop!) (show/profile-show :serial? (or (first serial) false))))
(defn clear-cues! [] (doseq [x (range 32) y (range 32)] (show/clear-cue! x y)))

; {{{ GLOBAL BASIC EFFECTS
(defn global-color-effect "Make a color effect which affects all lights, or :fixtures"; {{{
  [color & {:keys [include-color-wheels? fixtures effect-name] :or {fixtures (all-fixtures)}}]
  (try
    (let [[c desc] (cond (= (type color) ::colors/color)          [color (color-name color)]
                         (= (type color) clojure.lang.Keyword)    [(color-like (name color))#_(create-color color) (name color)]
                         (and (param? color) (= (params/result-type color)
                                                ::colors/color))  [color "variable"]
                         ;; :else [(create-color color) color])]
                         :else [(color-like (name color)) color])]
      (color-fx/color-effect (or effect-name (str "Global " desc)) c fixtures
                             :include-color-wheels? include-color-wheels?))
    (catch Exception e (throw (Exception. (str "Can't figure out how to create color from " color) e))))); }}}

(defn global-dimmer-effect "Return an effect that sets all dimmers. It can vary in response to a MIDI mapped show variable, an oscillator, or the location of the fixture. You can override the default name by passing in a value with :effect-name"; {{{
  [level & {:keys [effect-name add-virtual-dimmers?]}]
  (let [htp? (not add-virtual-dimmers?)]
    (dimmer-effect level (all-fixtures) :effect-name effect-name :htp? htp? :add-virtual-dimmers? add-virtual-dimmers?))); }}}

(defn rgb-dimmer "moderate rgb pixels using a virtual dimmer with high priority, scaling other effects." []
  (add-effect! :master (global-dimmer-effect 255 :htp? false :add-virtual-dimmers? true) :priority 100000))
; }}}

; {{{  TRANSFORMATIONS
(defn build-saturation-transformation "Deprecated. Creates a color transformation for use with [[transform-colors]] which changes the saturation based on a variable parameter. If no parameter is supplied, the default is to use an oscillated parameter based on [[sawtooth]] with `:down?` set to `true` so the color is fully saturated at the start of the beat, and fully desaturated by the end. A different pattern can be created by supplying a different parameter with the `:param` optional keyword argument." {:doc/format :markdown}; {{{
  [& {:keys [param] :or {param (build-oscillated-param (sawtooth :down? true) :max 90)}}]
  (fn [color show snapshot head]
    (let [s (colors/clamp-percent-float (resolve-param param show snapshot head))]
      (create-color {:h (hue color) :s s :l (lightness color)}))))

(defn sat-transformer "Creates a color transformation for use with [[transform-colors]] which changes the saturation based on a variable parameter. If no parameter is supplied, the default is to use an oscillated parameter based on [[sawtooth]] with `:down?` set to `true` so the color is fully saturated at the start of the beat, and fully desaturated by the end. A different pattern can be created by supplying a different parameter with the `:param` optional keyword argument." {:doc/format :markdown}
  [& {:keys [param] :or {param (build-oscillated-param (sawtooth :down? true) :min -20 :max 20)}}]
  (fn [color show snapshot head]
    (let [s (colors/clamp-percent-float (resolve-param param show snapshot head))]
      (build-color-param :color color :adjust-saturation s)))); }}}

(defn color-transformer "generalized color transform"
  [& {:keys [h s l]}]
  (fn [color show snapshot head]
    (let [args (into [:color color] ;there is no need to resolve params at this point, so just pass them on...
                     (when-not (= color (create-color :black)) ;avoid lighting up black, maybe some performance gains too
                      [:adjust-hue h :adjust-saturation s :adjust-lightness l]))]
      (apply build-color-param args))))

(defn color-mod-effect "Wraps transform-colors and the baddest transform-fn to get some fucking order in this place"
 [param fixtures & {:keys [hue saturation lightness] :or {hue 0.0 saturation 1.0 lightness 0.0}}] ;value scales param for that key
 (fn [show snapshot head]
  (let [res (resolve-param param show snapshot head)
        transformer (color-transformer :h (* res hue) :s (* res saturation) :l (* res lightness))] ;would be better to simply avoid if scale 0
   (transform-colors fixtures :transform-fn transformer))))

(defn compressor "compress value towards upper bounds without smashing it. SHOULD destructure color params to HSL with individual targets.
                  COULD hue-comp work? gravitating towards middle val rather than upper bound"
 [param level ratio knee & {:keys [extract-param]}] ;or makes more sense for sender to handle split/repack hmm
 (let [resolved (fn [show snapshot head] (resolve-param param show snapshot head))
       f (fn [_] )])
 )

(defn saturation-comp "compress saturation"
 [])
; }}}

; {{{     EFFECTS
; {{{     SPARKLE
(defn- remove-finished-sparkles "Filters out any sparkles that were created longer ago than the fade time. `sparkles` is a map from head to the timestamp at which the sparkle was created."
  [sparkles show snapshot fade-time]
  (pspy :remove-finished-sparkles
        (let [now (:instant snapshot)
              fade-time (resolve-param fade-time show snapshot)]
          (reduce
           (fn [result [where creation-time]]
             (if (< (- now creation-time) fade-time)
               (assoc result where creation-time)
               result))
           {}
           sparkles))))

;; brunch's todo: add off-beat-penalty that slopes the chance downwards as the beat passes,
;; same for off-bar-penalty, so can prioritize beats and bars, perhaps pass an oscillator
;; so they can be scaled in time too. Eventually allow randomization of fade time and perhaps
;; hue and peak brightness, with control over how much they vary?
;; joe's take: eh just do that in the actual cue or even further down (live/max)
;; but do sort larger sparkles, and vary intensity for new ones, + work with lightness...
(defn sparkle-tol "A random sparkling effect like a particle generator over the supplied fixture heads.
  As each frame of DMX values generated, each participating fixture head has a chance of being assigned a sparkle (this chance is
  controlled by the optional keyword parameter `:chance`). Once a sparkle has been created, it will fade out over the number of
  milliseconds specified by the optional keyword parameter `:fade-time`. The initial color of each sparkle can be changed with
  the optional keyword parameter `:color`. All parameters may be dynamic, including show variables with the standard shorthand of
  passing the variable name as a keyword."
  [fixtures & {:keys [color chance fade-time] :or {color (create-color :white) chance 0.001 fade-time 500}}]
  {:pre [(some? *show*)]}
  (let [color (bind-keyword-param color ::colors/color (create-color :white))
        chance (bind-keyword-param chance Number 0.001)
        fade-time (bind-keyword-param fade-time Number 500)]
    (let [heads (chan/find-rgb-heads fixtures)
          running (atom true)
          sparkles (atom {})  ; A map from head to creation timestamp for active sparkles
          snapshot (metro-snapshot (:metronome *show*))
          ;; TODO: These should be per-head in case they are spatial.
          [color chance fade-time] (map #(params/resolve-unless-frame-dynamic % *show* snapshot) [color chance fade-time])]
      (Effect. "Sparkle"
              (fn [show snapshot]
                (swap! sparkles remove-finished-sparkles show snapshot fade-time) ;; Continue running until all existing sparkles fade
                (or @running (seq @sparkles)))
              (fn [show snapshot]
                (pspy :sparkle
                      (when @running ;; See if we create any new sparkles (unless we've been asked to end).
                        (doseq [head heads]
                          (let [chance (resolve-param chance show snapshot head)]
                            (when (< (rand) (/ chance 10)) ; also hype likelihood that next head will also sparkle?
                              (swap! sparkles assoc head (:instant snapshot))))))
                      (let [now (:instant snapshot)]
                        (for [[head creation-time] @sparkles] ;; Build assigners for all active sparkles.
                          (let [color (resolve-param color show snapshot head)
                                fade-time (max 10 (resolve-param fade-time show snapshot head))
                                fraction (/ (- now creation-time) fade-time)
                                colormod (-> (adjust-hue color (rand 10))
                                             (darken (* fraction (lightness color))))]
                            (fx/build-head-assigner
                             :color head
                             (fn [show snapshot target previous-assignment] ;; can get black/darker sparkles by changing merge mode from htp
                               (color-fx/fade-colors colormod
                                                     previous-assignment
                                                     fraction
                                                     show snapshot target))))))))
              (fn [show snapshot]
                (reset! running false)))))) ;; Arrange to shut down once all existing sparkles fade out.
; }}}
; {{{     BLOOMS
(defn bloom-dimmer "Bloom that doesn't set a color, only affecting dimmer"  ; {{{
  [fixtures & {:keys [fraction measure]
               :or { :fraction 0, :measure (tf/build-distance-measure 0 0 0)}}]
  (let [fraction (bind-keyword-param fraction Number 0)
        measure (resolve-param (bind-keyword-param measure :tf/distance-measure
                                                                 (tf/build-distance-measure 0 0 0))
                                      *show* (metro-snapshot (:metronome *show*)))
        heads (chan/find-rgb-heads fixtures)
        furthest (tf/max-distance measure heads)
        f (fn [show snapshot target previous-assignment]
            (let [fraction (resolve-param fraction show snapshot target)
                  distance (measure target)]
              ;; (if (<= distance (* furthest fraction))
                previous-assignment))
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
                                                          ;; (- 255 (* 10 distance)) ; test vary dimmer by distance, bit silly since no go per head. But nice across rig?
                                                          ;; (- 100 (* 10 distance))))
                                  fraction)]
                       (dimmer/dimmer-effect level [fixture]))
                     )))))
; }}}

     ;FIXME: doesnt handle fixtures spanning across 0...
(defn bloom-tol "variable width, no dimmer action, adjustable spatial HSL" ; {{{
 [fixtures & {:keys [measure color fraction width halo keyhole?
                     keyhole-opacity keyhole-target hue-mod lightness-mod saturation-mod]
              :or {:measure (tf/build-distance-measure 0 0 0)
                   :color (create-color :white), :fraction 0, :width 1
                   :halo 0.1  ; fraction where bloom has ended but some bleeds through/fades out
                   :keyhole? false ;needed because false != nil down the line
                   :keyhole-opacity 1.0 ; allow some through even outside keyhole
                   :keyhole-target "lightness"; adjustable destination (curr simply black outside bounds, should rather mod previous-assignment)
                   :hue-mod 50 :lightness-mod -5 :saturation-mod -20}}]
 (let [measure (resolve-param (bind-keyword-param measure ::tf/distance-measure (tf/build-distance-measure 0 0 0))
                              *show* (metro-snapshot (:metronome *show*)))
       color (bind-keyword-param color ::colors/color (create-color :white))
       [num-params num-defaults] [[fraction width halo hue-mod lightness-mod saturation-mod] [0 1.0 0.1 50 -5 -20]]
       [fraction width halo hue-mod lightness-mod saturation-mod]
         (map #(bind-keyword-param %1 Number %2) num-params num-defaults)
       keyhole? (bind-keyword-param keyhole? Boolean false)
       heads (chan/find-rgb-heads fixtures)
       furthest (tf/max-distance measure heads)
       f (fn [show snapshot target previous-assignment]
           (let [[fraction width keyhole? hue-mod lightness-mod saturation-mod]
                 (map #(resolve-param % show snapshot target) [fraction width keyhole? hue-mod lightness-mod saturation-mod])
                 hue-mod (or hue-mod 50) ;why is this needed when we have defaults both from :or, and in bind-keyword-param?
                 lightness-mod (or lightness-mod -5)
                 saturation-mod (or saturation-mod -20)
                 distance (measure target)
                 [near-bound far-bound] (map #(* furthest (% fraction (/ width 2))) [- +])]
             (if (<= near-bound distance far-bound)
              (if-not keyhole?
                      (build-color-param :color color ;; XXX per-head virtual dimmer (not the same as lightness) (how not the same?)
                                         :adjust-lightness (* lightness-mod distance)
                                         :adjust-hue (* hue-mod distance)
                                         :adjust-saturation (* saturation-mod fraction))
                      ;; previous-assignment)
               (build-color-param :color previous-assignment ;(resolve-param previous-assignment show snapshot) ;; test use whatever color already set but adjust it
                                  :adjust-lightness (* lightness-mod distance) :adjust-hue (* hue-mod distance) :adjust-saturation (* saturation-mod fraction)))
              (if-not keyhole?
               previous-assignment
               ;; (create-color :black)))))
               (color-fx/fade-colors color (resolve-param previous-assignment show snapshot)
                                     (- fraction (/ furthest distance))
                                     show snapshot target))))) ; blend new and prev here ideally. just get a weird purple color now hmm
       assigners (fx/build-head-assigners :color heads f)]
   (apply fx/scene "Bloom"
          (concat [(Effect. "Bloom color"
                            fx/always-active
                            (fn [show snapshot] assigners)
                            fx/end-immediately)]))))
;; (when aim?  ;from confetti. Here we are talking about a bounds tho, not specific point...
;;  (filter identity  ;but guess heads check whether selves in/out, either find nearest head that got lit, or nearest point that would be?
;;          (for [[head [_ _ point]] @flakes]
;;            (when (seq (move/find-moving-heads [head]))
;;              (fx/build-head-assigner :aim head (fn [_ _ _ _] point))))))
; }}}
; }}}
; {{{    CONFETTI
(defn- remove-finished-flakes "Filters out any flakes that were created longer ago than the configured duration. `flakes` is a map from head to a tuple containing the step value after which the flake will end, followed by color and potentially aim information."
  [flakes show snapshot step]
  (pspy :remove-finished-flakes
        (let [now (math/round (resolve-param step show snapshot))]
          (reduce
           (fn [result [where info]]
             (let [final-step (first info)]
               (if (<= now final-step)
                 (assoc result where info)
                 result)))
           {}
           flakes))))

(defn- add-flakes "Create new flakes of a shared random color and individual random durations for each of the supplied heads."
  [heads show snapshot current-step min-dur max-dur min-sat max-sat min-hue max-hue
   & {:keys [colors] :or {colors nil}}]
  (let [hue (+ min-hue (rand (- max-hue min-hue)))] ;XXX pass hue-range or vec of specific colors
    (into {}
          (map (fn [head]
                 (let [min-dur (max 0 (math/round (resolve-param min-dur show snapshot head)))
                       max-dur (max min-dur (math/round (resolve-param max-dur show snapshot head)))
                       duration (+ min-dur (rand-int (inc (- max-dur min-dur))))
                       min-sat (colors/clamp-percent-float (resolve-param min-sat show snapshot head))
                       max-sat (colors/clamp-percent-float (resolve-param max-sat show snapshot head))
                       saturation (colors/clamp-percent-float (- max-sat (rand (- max-sat min-sat))))]
                   [head [(+ current-step duration) (create-color :h hue :s saturation :l 50)]]))
               heads))))

(defn- aim-flakes "Chooses a random point at which the newly-created flakes shoule be aimed."
  [flakes show snapshot min-x max-x min-y max-y min-z max-z]
  (let [[min-x max-x min-y max-y min-z max-z] (map #(resolve-param % show snapshot) [min-x max-x min-y max-y min-z max-z])
        [x y z] (map #(+ %1 (rand (- %2 %1))) [min-x min-y min-z] [max-x max-y max-z])
        point (Point3d. x y z)]
    (into {} (for [[head info] flakes]
               [head (conj info point)]))))

;; XXX tol, flakes as bounds in space instead of heads. So could map over multiple heads, change size to osc n shit
(defn confetti-tol "Mod confetti so can set max saturation, and limit colors to specific range or selection"
  [fixtures & {:keys [step min-add max-add min-dur max-dur min-sat max-sat min-hue max-hue
                      aim? min-x max-x min-y max-y min-z max-z]
               :or {step (build-step-param)
                    min-add 1 max-add 4 min-dur 2 max-dur 4 min-sat 40.0 max-sat 80.0 min-hue 0.0 max-hue 360.0
                    aim? false min-x -5.0 max-x 5.0 min-y 0.0 max-y 0.0 min-z 0.5 max-z 5.0}}]
  {:pre [(some? *show*)]}
  (let [step (bind-keyword-param step Number (build-step-param))
        self-binds [min-add max-add min-dur max-dur min-sat max-sat min-hue max-hue min-x max-x min-y max-y min-z max-z]
        starting   [1       4       2       4       40.0    80.0    0.0     360.0   -5.0  5.0   0.0   0.0   0.5   5.0]
        [min-add max-add min-dur max-dur min-sat max-sat min-hue max-hue min-x max-x min-y max-y min-z max-z]
        (map #(bind-keyword-param %1 Number %2) self-binds starting)]
    (let [heads (chan/find-rgb-heads fixtures true)
          running (ref true)
          current-step (ref nil)
          flakes (ref {}) ; A map from head to [creation-step color] for active flakes
          snapshot (metro-snapshot (:metronome *show*))
          [min-add max-add min-x max-x min-y max-y min-z max-z]
            (map #(params/resolve-unless-frame-dynamic % *show* snapshot)
                   [min-add max-add min-x max-x min-y max-y min-z max-z])]
      (Effect. "Confetti"
               (fn [show snapshot] ;; Continue running until all existing flakes fade
                 (dosync
                  (alter flakes remove-finished-flakes show snapshot step)
                  (or @running (seq @flakes))))
               (fn [show snapshot]
                 (pspy :confetti ;; See how many flakes to create (unless we've been asked to end).
                       (dosync
                        (when @running
                          (let [now (math/round (resolve-param step show snapshot))]
                            (when (not= now @current-step)
                              (ref-set current-step now)
                              (let [min-add (max 0 (math/round (resolve-param min-add show snapshot)))
                                    max-add (max min-add (math/round (resolve-param max-add show snapshot)))
                                    [min-hue max-hue] (map #(resolve-param % show snapshot) [min-hue max-hue])
                                    add (+ min-add (rand-int (inc (- max-add min-add))))
                                    new-flakes (add-flakes (take add (shuffle heads)) show snapshot now
                                                           min-dur max-dur min-sat max-sat min-hue max-hue)
                                    aimed-flakes (if aim?
                                                   (aim-flakes new-flakes show snapshot
                                                               min-x max-x min-y max-y min-z max-z)
                                                   new-flakes)]
                                (alter flakes merge aimed-flakes)))))
                        (concat (for [[head [_ color]] @flakes] ;; Build assigners for all active flakes.
                                  (fx/build-head-assigner :color head (fn [_ _ _ _] color)))
                                (when aim?
                                  (filter identity
                                          (for [[head [_ _ point]] @flakes]
                                            (when (seq (move/find-moving-heads [head]))
                                              (fx/build-head-assigner :aim head (fn [_ _ _ _] point))))))))))
               (fn [show snapshot] ;; Stop making new sparkles and arrange to shut down once all existing ones fade out.
                 (dosync (ref-set running false)))))))

; }}}
; {{{         CAN-CAN
(defn can-can "Moves moving head like in a kick line."
  [& {:keys [bars cycles stagger spread pan-min pan-max tilt-min tilt-max]
      :or {bars 1 cycles 1 stagger 0 spread 0 pan-min 0 pan-max 0 tilt-min -100 tilt-max 100}}]
  (let [bars (bind-keyword-param bars Number 1)
        cycles (bind-keyword-param cycles Number 1)
        stagger (bind-keyword-param stagger Number 0)
        spread (bind-keyword-param spread Number 0)
        pan-min (bind-keyword-param pan-min Number 0)
        pan-max (bind-keyword-param pan-max Number 0)
        tilt-min (bind-keyword-param tilt-min Number -30)
        tilt-max (bind-keyword-param tilt-max Number 30)
        pan-ratio (build-param-formula Number #(/ (* 4 %1) %2) bars cycles)
        tilt-ratio (build-param-formula Number #(/ %1 %2) bars cycles)
        fx (for [head [{:key :moving-1      :phase 0.0 :pan-offset 0}
                       {:key :moving-mini-1 :phase 0.2 :pan-offset 0}
                       {:key :moving-mini-2 :phase 0.4 :pan-offset -0}
                       {:key :moving-mini-3 :phase 0.6 :pan-offset -0}
                       {:key :moving-2      :phase 0.8 :tilt-offset 0}]]
             (let [head-phase (build-param-formula Number #(* % (:phase head 0)) stagger)
                   tilt-osc (sine :interval :bar :interval-ratio tilt-ratio :phase head-phase)
                   head-tilt-min (build-param-formula Number #(+ % (:tilt-offset head 0)) tilt-min)
                   head-tilt-max (build-param-formula Number #(+ % (:tilt-offset head 0)) tilt-max)
                   tilt-param (build-oscillated-param tilt-osc :min head-tilt-min :max head-tilt-max)
                   pan-osc (sine :interval :bar :interval-ratio pan-ratio :phase head-phase)
                   head-pan-min (build-param-formula Number #(+ %1 (* (:pan-offset head 0) %2))
                                                            pan-min spread)
                   head-pan-max (build-param-formula Number #(+ %1 (* (:pan-offset head 0) %2))
                                                            pan-max spread)
                   pan-param (build-oscillated-param pan-osc :min head-pan-min :max head-pan-max)
                   direction (build-pan-tilt-param :pan pan-param :tilt tilt-param)]
               (move/pan-tilt-effect "can can element" direction (fixtures-named (:key head)))))]
    (apply fx/scene "Can Can" fx)))
; }}}
; {{{  PINSTRIPE
(def default-pinstripe-colors "The set of colors that will be used by a pinstripe effect if no `:colors` parameter is supplied."
  ;; [(create-color :orangered) (colors/create-color :orangered4)])
  [(color-like "royalblue") (color-like "orangered4")])

(defn- gather-stripes "Gathers heads into the groups that will be assigned particular colors by the pinstripes effect."
  [heads group-fn num-colors]
  (let [head-groups (partition-all num-colors (sort-by #(:x (first %)) (vals (group-by group-fn (sort-by :x heads)))))
        stripe-groups (for [i (range num-colors)] [])]
    (loop [remaining-groups head-groups
           result stripe-groups]
      (let [current-group (first remaining-groups)
            remaining-groups (rest remaining-groups)
            result (map concat result (concat current-group (repeat [])))]
        (if (empty? remaining-groups)
          result
          (recur remaining-groups result))))))

(defn pinstripes "A color effect which divides the lights into alternating columns by their _x_ positions (with configurable tolerance, defaulting to
  requiring exact equality), and assigns a color to each column. The colors rotate according to a step parameter.
  The step parameter defaults to one which changes abruptly on each beat, but you can supply your own with the optional keyword argument
  `:step`. Fades between colors can be achieved by passing a step parameter that fades.
The tolerance used when grouping the heads into stripes is controlled by the optional parameter `:tolerance`, which defaults to
  zero, meaning the heads must have the exact same _x_ value to get assigned to the same stripe. The assignment of heads into stripes is
  done when the effect is created, so if the tolerance changes after that, it will have no effect.

  The colors themselves are passed as a sequence with `:colors` and default to red and white. The list of colors you supply can be of
  any length. Although the colors within the list themselves can be dynamic parameters, the conntent of the list is evaluated when the
  effect is created, so the number of colors and the color parameters themselves are fixed at that time.

  The `:step`, `:tolerance`, and `:colors` parameters may be dynamic, (and may be bound to show variables using the standard shorthand of
  passing the variable name as a keyword). Since `:step` and `:tolerance` are not associated with a specific head, they cannot be
  a spatial parameters. The colors can be, however, so for example saturations can vary over the rig."
  [fixtures & {:keys [step tolerance colors]
               :or {step (build-step-param)
                    tolerance 0
                    colors default-pinstripe-colors}}]
  {:pre [(some? *show*)]}
  (let [step (bind-keyword-param step Number (build-step-param))
        tolerance (bind-keyword-param tolerance Number 0)
        colors (bind-keyword-param colors java.util.List default-pinstripe-colors)]
    (let [heads (chan/find-rgb-heads fixtures false)
          snapshot (metro-snapshot (:metronome *show*))
          tolerance (resolve-param tolerance *show* snapshot)
          colors (map #(bind-keyword-param % ::colors/color (create-color :white))
                      (resolve-param colors *show* snapshot))
          group-fn (if (< tolerance 0.00001) :x #(math/round (/ (:x %) tolerance)))
          stripes (gather-stripes heads group-fn (count colors))
          chases (map (fn [i stripe-heads]
                        (let [effects (map (fn [color]
                                             (color-fx/color-effect "pin color" color stripe-heads))
                                           colors)
                              pin-step (build-param-formula Number #(- % i) step)]
                          (chase "Pinstripe" effects pin-step :beyond :loop)))
                      (range) stripes)]
      (apply fx/scene "Pinstripes" chases))))

; }}}
; {{{ METRONOME EFFECT
(def default-down-beat-color "The default color for [[metronome-effect]] to flash on the down beats."
 (color-like "red"))

(def default-other-beat-color "The default color for [[metronome-effect]] to flash on beats that are not down beats."
 (color-like "yellow"))

(defn metronome-effect "Returns an effect which flashes the supplied fixtures to the beats of the show metronome, emphasizing the down beat, which is a great
 way to test and understand metronome synchronization. The color of the flashes can be controlled by the `:down-beat-color` and
 `:other-beat-color` optional keyword arguments (defaulting to red with lightness 70, and yellow with lightness 20, respectively).

 This is no longer as useful as it used to be before there were metronome adjustment interfaces in the Ableton Push and then web
 interfaces, but is still an example of how to write a metronome driven effect, and can be synchronized to metronomes other than the
 default show metronome by passing them in with optional keyword argument `:metronome`."
 [fixtures & {:keys [down-beat-color other-beat-color metronome]
              :or {down-beat-color default-down-beat-color
                   other-beat-color default-other-beat-color
                   metronome (:metronome *show*)}}]
 {:pre [(some? *show*)]}
 (let [down-beat-color (bind-keyword-param
                        down-beat-color ::colors/color default-down-beat-color)
       other-beat-color (bind-keyword-param
                         other-beat-color ::colors/color default-other-beat-color)
       metronome (bind-keyword-param metronome Metronome (:metronome *show*))]
   (validate-param-type down-beat-color ::colors/color)
   (validate-param-type other-beat-color ::colors/color)
   (validate-param-type metronome Metronome)
   (let [heads (chan/find-rgb-heads fixtures)
         running (atom true)
         ;; Need to use the show metronome as a snapshot to resolve our metronome parameter first
         metronome (resolve-param metronome *show* (metro-snapshot (:metronome *show*)))
         snapshot (metro-snapshot metronome)
         down-beat-color (params/resolve-unless-frame-dynamic down-beat-color *show* snapshot)
         other-beat-color (params/resolve-unless-frame-dynamic other-beat-color *show* snapshot)
         local-snapshot (atom nil)  ; Need to set up a snapshot at start of each run for all assigners
         f (fn [show snapshot target previous-assignment]
             (pspy :metronome-effect
                   (let [raw-intensity (* 2 (- (/ 1 2) (snapshot-beat-phase @local-snapshot 1)))
                         intensity (if (neg? raw-intensity) 0 raw-intensity)
                         base-color (if (snapshot-down-beat? @local-snapshot)
                                      (resolve-param down-beat-color show @local-snapshot)
                                      (resolve-param other-beat-color show @local-snapshot))]
                     (create-color {:h (colors/hue base-color)
                                           :s (colors/saturation base-color)
                                           :l (* (colors/lightness base-color) intensity)}))))
         assigners (fx/build-head-assigners :color heads f)]
     (Effect. "Metronome"
              (fn [show snapshot]  ;; Continue running until the end of a measure
                ;; Also need to set up the local snapshot based on our private metronome for the assigners to use.
                (reset! local-snapshot (metro-snapshot metronome))
                (or @running (< (snapshot-bar-phase @local-snapshot) 0.9)))
              (fn [show snapshot] assigners)
              (fn [snow snapshot]  ;; Arrange to shut down at the end of a measure
                (reset! running false))))))

; }}}
; {{{          DIMMER SWEEP
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

;; (def effect-fn dimmer-effect)
;; (symbol? '~effect-fn)
;; (namespace effect-fn)
;; (name 'effect-fn)
;; (str (name effect-fn) " Sweep")
;; (str (str effect-fn) " Sweep")
;; (defn- pretty-demunge
;;   [fn-object]
;;   (let [dem-fn (clojure.repl/demunge (str fn-object))
;;         pretty (second (re-find #"(.*?\/.*?)[\-\-|@].*" dem-fn))]
;;     (if pretty pretty dem-fn)))
;; (pretty-demunge effect-fn)

(defn effect-sweep "Dimmer sweep for any param... and adjustable positions"
  [fixtures position & {:keys [width level fade effect-fn effect-keys scale name]
                        :or {width 0.1 level 255 fade false effect-fn dimmer-effect scale 1.0}}]
  (let [[width level scale] (map #(bind-keyword-param %1 Number %2) [width level scale] [0.1 255 1.0])
        fade (bind-keyword-param fade Boolean false)
        [min-bound max-bound] (map #(apply % (map :x fixtures)) [min max])
        [min-x max-x] (map (fn [b] (build-param-formula Number #(* %1 %2) b scale)) ;use min-x max-x, or scale + "middle" for proper control of both ends?
                           [min-bound max-bound])
        position (build-oscillated-param position :min min-x :max max-x)
        f (fn [position width level fade fixture]
           (let [range (/ width 2)
                 distance (math/abs (- position (:x fixture)))]
            (if (> distance range)
             0
             (if fade (* level (/ (- range distance) range)) level))))]
    (apply fx/scene (str #_(pretty-demunge effect-fn) (or name "Some") " Sweep")
    ;; (apply fx/scene "Effect Sweep"
           (for [fixture fixtures]
             (let [fixture-level (build-param-formula Number f position width level fade fixture)]
               (apply effect-fn fixture-level [fixture] effect-keys))))))
; }}}


(defn strobe-tol "wraps strobe-2 with [level fixtures] standard ting"
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

;; (def stro (strobe-effect 50 (all-fixtures)))
;; ((:end-fn stro))
;; (record? stro)
; {{{          CROSSOVER CHASE
(defn build-cross-scene
  "Create a scene which sets the color of one light, and aims it just below and in front of another."
  [move-key reference-key color]
  (fx/scene "Cross scene"
            (move/aim-effect "Cross" (javax.vecmath.Point3d. (:x (first (fixtures-named reference-key))) 0.0 0.2)
                             (fixtures-named move-key))
            (color-fx/color-effect "Cross color" color (fixtures-named move-key) :include-color-wheels? true)))

(defn crossover-chase ;{{{
  "Create a sequential chase which gradually takes over all the moving heads from whatever they were doing, changes their colors, and makes
  them cross in an interesting pattern. By default, stages of the chase advance on every beat, but you can adjust that by passing in a
  different value for with the optional keyword argument `:beats`. To add a fade between stages, pass a non-zero value (up to 1, which
  means continually fade) with `:fade-fraction`.
  The color used during the crossover stages defaults to red, but you can pass a different color object to use with `:cross-color`."
  [& {:keys [beats fade-fraction cross-color end-color]
      :or {beats 1 fade-fraction 0 cross-color (create-color :orangered) end-color (create-color :orange)}}]
  (let [beats (bind-keyword-param beats Number 1)
        fade-fraction (bind-keyword-param fade-fraction Number 0)
        cross-color (bind-keyword-param cross-color ::colors/color (create-color :orangered))
        end-color (bind-keyword-param end-color ::colors/color (create-color :yellow))
        cross-elements [(build-cross-scene :moving-1 :moving-mini-1 cross-color)
                        (build-cross-scene :moving-mini-1 :moving-1 cross-color)
                        (build-cross-scene :moving-mini-3 :moving-2 cross-color)
                        (build-cross-scene :moving-2 :moving-mini-3 cross-color)]]
    (chase "Crossover"
              (concat (for [i (range 1 (inc (count cross-elements)))]
                        (apply fx/scene (str "Crossover Scene " i) (take i cross-elements)))
                      [(fx/scene "Crossover End"
                                 (move/aim-effect "Cross End Point" (Point3d. 0.0 0.0 2.5)
                                                  (concat (fixtures-named "moving")
                                                          (fixtures-named "moving-mini")))
                                 (color-fx/color-effect "Cross End color" end-color
                                                        (concat (fixtures-named "moving")
                                                                (fixtures-named "moving-mini"))
                                                        :include-color-wheels? true))
                       (fx/blank)])
              (build-step-param :interval-ratio beats :fade-fraction fade-fraction) :beyond :loop))) ;}}}
; }}}
 ;{{{          CIRCLE CHAIN
(defn circle-chain
  "Create a chase that generates a series of circles on either the floor or the ceiling, causing a single head to trace out each, and passing them along from head to head.
  The number of bars taken to trace out each circle defaults to 2 and can be adjusted by passing a different value with the optional keyword argument `:bars`. The radius of each circle defaults to one
  meter, and can be adjusted with `:radius`. If you want each head to be tracing a different position in its circle, you can pass a value between zero and one with `:stagger`."
  [fixtures ceiling? & {:keys [bars radius stagger] :or {bars 2 radius 1.0 stagger 0.0}}]
  (let [bars (bind-keyword-param bars Number 2)
        radius (bind-keyword-param radius Number 1.0)
        stagger (bind-keyword-param stagger Number 0.0)
        snapshot (metro-snapshot (:metronome *show*))
        bars (params/resolve-unless-frame-dynamic bars *show* snapshot)
        radius (params/resolve-unless-frame-dynamic radius *show* snapshot)
        stagger (params/resolve-unless-frame-dynamic stagger *show* snapshot)
        step (build-step-param :interval :bar :interval-ratio bars)
        phase-osc (sawtooth :interval :bar :interval-ratio bars)
        width (- right-wall left-wall)
        front (if ceiling? 0.5 stage-wall)  ; The blades can't reach behind the rig
        depth (- house-rear-wall front)
        y (if ceiling? ceiling 0.0)
        heads (sort-by :x (move/find-moving-heads fixtures))
        points (ref (ring-buffer (count heads)))
        running (ref true)
        current-step (ref nil)]
    (Effect. "Circle Chain"
             (fn [show snapshot] ;; Continue running until all circles are finished
               (dosync (or @running (seq @points))))
             (fn [show snapshot]
               (dosync
                (let [now (math/round (resolve-param step show snapshot))
                      phase (lfo/evaluate phase-osc show snapshot nil)
                      stagger (resolve-param stagger show show snapshot)
                      head-phases (map #(* stagger %) (range))]
                  (when (not= now @current-step)
                    (ref-set current-step now)
                    (if @running  ;; Either add a new circle, or just drop the oldest
                      (alter points conj (Point3d. (+ left-wall (rand width)) y (+ front (rand depth))))
                      (alter points pop)))
                  (map (fn [head point head-phase]
                         (let [radius (resolve-param radius show snapshot head)
                               theta (* 2.0 Math/PI (+ phase head-phase))
                               head-point (Point3d. (+ (.x point) (* radius (Math/cos theta)))
                                                    (.y point)
                                                    (+ (.z point) (* radius (Math/sin theta))))]
                           (fx/build-head-parameter-assigner :aim head head-point show snapshot)))
                       heads @points head-phases))))
             (fn [show snapshot] ;; Stop making new circles, and shut down once all exiting ones have ended.
                 (dosync (ref-set running false))))))
; }}}

; {{{          CHANNEL / FUNCTION effects
;XXX maybe flip channels and channel-type??
(defn channel-effect "Fixed to [level targets & effect-name] standard format... Returns an effect which assigns a dynamic value to all the supplied channels. If `level is a keyword, it will be looked up as a show variable. If `htp?` is true, applies highest-takes-precedence (i.e.  compares the value to the previous assignment for the channel, and lets the highest value remain)."
  [level channels & {:keys [htp? effect-name fixtures channel-type] :or {fixtures (all-fixtures) effect-name "Channel effect"}}]
  {:pre [(some? effect-name) (some? *show*) (sequential? channels)]}
  (let [level (params/bind-keyword-param level Number 0)
        f (if htp?  ;; We need to resolve any dynamic parameters at this point so we can apply the highest-take-precedence rule.
            (fn [show snapshot target previous-assignment]
              (max (colors/clamp-rgb-int (params/resolve-param level show snapshot (:head target)))
                   (or (colors/clamp-rgb-int (params/resolve-param previous-assignment
                                                                   show snapshot (:head target))) 0)))
            (fn [show snapshot target previous-assignment] ;; We can defer resolution to the final DMX calculation stage.
              level))
        assigners (chan-fx/build-raw-channel-assigners channels f)]
    (Effect. effect-name
             fx/always-active
             (fn [show snapshot] assigners)
             fx/end-immediately)))

(defn get-channels "wrap extract-channels"
  [channel-type fixtures]
  (let [fixtures (or fixtures (all-fixtures))
        channel-type (if (keyword? channel-type) channel-type (keyword channel-type))]
  (when channel-type (extract-channels fixtures #(= (:type %) channel-type)) )))


(defn function-effect-std "Fixed to [level target &]. Returns an effect which assigns a dynamic value to all channels of the supplied fixtures or heads which have a range that implements the specified function. (Functions are a way for fixtures to use the same DMX channel to do multiple things, allocating ranges of values to get more dense use from a smaller number of channel allocations.) The `function` argument is the keyword by which the function information will be found for the supplied `fixtures`. The actual value sent for the channel associated with `function` for each fixture will be calculated by treating `level` as a percentage of the way between the lowest and highest DMX values assigned to that named function for the fixture. The name displayed for the effect in user interfaces is determined by `effect-name`.  If `:htp?` is passed with a `true` value, applies highest-takes-precedence (i.e. compares the value to the previous assignment for the channels implementing the function, and lets the highest value remain).  If you have multiple effects trying to control different functions which use the same channel, you are unlikely to get the results you want. Hopefully the fixture designers chose how to share channels wisely, avoiding this pitfall."
  [level function-type & {:keys [effect-name htp? fixtures] :or {fixtures (all-fixtures)}}]
  {:pre [(some? *show*) (some? function-type) #_(sequential? fixtures)]}
  (let [level (params/bind-keyword-param level Number 50)
        effect-name (or effect-name (str (name function-type) " effect"))
        function-type (keyword function-type) ;why, when passed as such?
        heads (chan-fx/find-heads-with-function function-type fixtures)
        f (if htp?  ;; We need to resolve any dynamic parameters at this point so we can apply the highest-take-precedence rule.
            (fn [show snapshot target previous-assignment]
              (max (resolve-param level show snapshot target)
                   (or (resolve-param previous-assignment show snapshot target) 0)))
            (fn [show snapshot target previous-assignment] ;; We can defer parameter resolution until the final DMX level assignment stage.
              level))
        assigners (chan-fx/build-head-function-assigners function-type heads f)]
    (Effect. effect-name
             fx/always-active
             (fn [show snapshot] assigners)
             fx/end-immediately)))



;; (defn channel-or-function-effect "Try merging channel-effect and function-effect"
;;   [level target-key & {:keys [htp? effect-name fixtures ] :or {fixtures (all-fixtures)}}]
;;   {:pre [(some? *show*) (some? target-key)]}
;;   (let [level (params/bind-keyword-param level Number 0)
;;         effect-name (or effect-name (str (name target-key) " effect"))
;;         fn-heads (chan-fx/find-heads-with-function target-key fixtures)
;;         channels (get-channels (target-key fixtures))
;;         f (if htp?  ;; We need to resolve any dynamic parameters at this point so we can apply the highest-take-precedence rule.
;;            (fn [show snapshot target previous-assignment]
;;              (if fn-heads
;;               (max (colors/clamp-rgb-int (params/resolve-param level show snapshot (:head target)))
;;                    (or (colors/clamp-rgb-int (params/resolve-param previous-assignment show snapshot (:head target))) 0)))
;;               (max (resolve-param level show snapshot target) ;
;;                     (or (resolve-param previous-assignment show snapshot target) 0)))
;;
;;             (fn [show snapshot target previous-assignment] level))
;;
;;         assigners-ch (chan-fx/build-raw-channel-assigners channels f)
;;         assigners-fn (chan-fx/build-head-function-assigners target-key fn-heads f)]
;;     (scene "control shiet" [ (map #(Effect. effect-name
;;                                       fx/always-active
;;                                       (fn [show snapshot] [assigners-ch assigners-fn])
;;                                       fx/end-immediately) )
;;                             (Effect. effect-name
;;                                      fx/always-active
;;                                      (fn [show snapshot] assigners)
;;                                      fx/end-immediately)
;;             ])))
; }}}

;GOTTA WORK ON
;conditional-effect
;spatial shit...
;others?
;making new effects!!!


;; (defn effect
;;  [effect-fn level]
;;  ;will have to get name of fx-fn now i guess for name, since that is generally hardcoded ugh.
;;  ;else dummy-run and grab off metadata?
;;  )
(defonce ^{:doc "One blank effect ought to be enough for everyone."}
  blank-effect (Effect. "Blank" fx/always-active (fn [show snapshot] []) fx/end-immediately))
(def generate-fade #'afterglow.effects/generate-fade)

(defn effect "Wraps other effect-fns in a fade to blank, and possibly other shit later. Like prio adjustment/autorestart?"
  [effect phase]
  {:pre [(some? *show*) #_(satisfies? Effect effect)]}
  (validate-param-type phase Number)
  (let [snapshot (metro-snapshot (:metronome *show*))
        phase (params/resolve-unless-frame-dynamic phase *show* snapshot) ;could we do spatial param = fade effect per head/in space? surely
        ;; active (atom true) ;; (atom [true true])                          ;so pinstripes type but with full effects
        effect-name (:name effect)]                                       ;or smooth fades out. or mixing bunch of lfos prob funky
    (Effect. effect-name
             (:active-fn effect)
             (fn [show snapshot]
               (let [v (resolve-param phase show snapshot)]
                     ;; active (if @active effect blank-effect)]
                 (cond
                   (util/float>= v 1.0) ((:gen-fn effect) show snapshot)
                   (util/float<= v 0.0) ((:gen-fn blank-effect) show snapshot)
                   :else (generate-fade blank-effect effect v show snapshot))))
             (:end-fn effect)
             #_(fn [show snapshot]  ; Ask any remaining active effects to end, record and report results
               (every? false? (swap! active end-still-active [from-effect blank-effect] show snapshot))))))

;XXX maybe interesting: bilinear fade should be just wrapping two fades no?

; XXX modulating overlay effects modeled after "actual" stuff - ripples, calm water, crowd walkingby brightlights, whatever
; }}}

; {{{  HELPER FUNCTIONS
; {{{ GROUPS
;XXX expand or build replacement that can handle random collection of fixtures
(defn group-end-keys "Helper function to produce a vector of effect keywords to end all effects running on light groups with a given suffix."
  [effect-suffix]
  (mapv #(keyword (str (name %) "-" effect-suffix)) light-groups))

(defn group-cue-parts "Helper function for common variables when creating a cue to run on either all, or a named group of lights."
  [group effect-suffix name-suffix & {:keys [channel]}]
  (let [effect-key (or (when group   (keyword (str (name group) "-" effect-suffix)))
                       (when channel (keyword (str (name channel) "-" effect-suffix )))
                       (keyword effect-suffix))
                       ;; (keyword (str effect-suffix (and "-" (try (name channel) (catch Exception e)))))))
        fixtures   (or (when group (fixtures-named group))
                       (all-fixtures))
        end-keys   (or (when group [(keyword effect-suffix)])
                       (group-end-keys effect-suffix))
        effect-name (str (case group
                          nil (if channel (capitalize (name channel)) "All")
                          (capitalize (name group)))
                         " " (capitalize name-suffix))]
    [effect-key fixtures effect-name end-keys])) ;changed order because end-keys often ignored

;; (keyword (str effect-suffix (and "-" (try (name channel) (catch Exception e)))))
;; (keyword (str "strobe" (and "-" (try (name "fog") (catch Exception e)))))
; }}}

; {{{  SPATIAL
(defn space-phase "Return position of head, in dimension, with show bounds as 0-1 - generally left to right for x, low to high for y, front to back for z."
  [head show & {:keys [dim] :or {dim :x}}]
  (let [dimensions @(:dimensions *show*)
        min-dim (keyword (str "min-" (str dim)))
        max-dim (keyword (str "max-" (str dim))) ]
    (/ (- (:dim head) (min-dim dimensions)) (- (max-dim dimensions) (min-dim dimensions)))))

(defn x-phase "Return a value that ranges from zero for the leftmost fixture in a show to 1 for the rightmost, for staggering the phase of an oscillator in making a can-can chase."
  [head show]
  (let [dimensions @(:dimensions *show*)]
    (/ (- (:x head) (:min-x dimensions)) (- (:max-x dimensions) (:min-x dimensions)))))
; }}}

; {{{          COLOR


(defn color-compliment "Return a different color matching provided one, according to science or whatever"
 [color & {:keys [looseness taste] :or {looseness "monet", taste :yourmom}}]
 )

(defn bound-palette "Give n colors puck with above. Create ranges around these values, that color-params can be clipped to
                     XXX including post global hue rotation, lightening effects etc.
                     Colors too far from appropriate points get darkened and desaturated..."
 [])


; }}}

(defn print-var-on-change
  [key value]
  (print key "-" value ". "))
;; (show/add-variable-set-fn! :thevar print-var-on-change)

(defn fx-from-cue!  "Find cue at x/y, run it respecting key, priority, :end-keys etc. Returns id of new effect, or nil if no cue found. Velocity-responsive vars will start at `:velocity`, or 127 if not specified.  A map of variable keywords to values can be supplied with `:var-overrides`, replacing the `:start` value used normally.  Example usage: compound-cues, afterglow-max. If a `:var-override` is specified for a variable which is also configured as velocity sensitive, the override will win."
  [x y & {:keys [velocity var-overrides page-x page-y] :or {velocity 127 page-x 0 page-y 0}}]
  {:pre [(some? *show*) (number? velocity) (<= 0 velocity 127)]}
  (let [x (+ x (* page-x 8))
        y (+ y (* page-y 8))]
   (when-let [cue (ct/cue-at (:cue-grid *show*) x y)]
    (doseq [k (:end-keys cue)] (end-effect! k))
    (let [saved-vars (ct/cue-vars-saved-at (:cue-grid *show*) x y)
          velocity-vars (ct/starting-vars-for-velocity cue velocity)
          var-map (#'show/introduce-cue-variables (:variables cue) x y
                                                  (merge saved-vars velocity-vars var-overrides))
          id (add-effect! (:key cue) ((:effect cue) var-map)
                          :priority (:priority cue) :from-cue cue :x x :y y :var-map var-map)]
      (ct/activate-cue! (:cue-grid *show*) x y id)
      id))))


; }}}

#_(defn scaled-metronome "not needed for what I thought, since scaling time in ratio-param... but might still be good to finish."
 ([]) ; update
 ([multiplier divisor] ;setup
   (set-variable! :metro-scale (/ multiplier divisor))
   (set-variable! :scaled-master-metro (metronome (/ (:bpm (metro-snapshot (:metronome *show*)))
                                                     (get-variable :metro-scale)))))
 ([scale-param])) ;setup dynamic

(defn avar "get/set show variable shortcut"
 ([key]
  (show/get-variable key))
 ([key value]
  (show/set-variable! key value)))

(defmacro apply-vm ;XXX include some common actions so eg beats+cycles auto-resolves through rato-param etc...
  "Call fn, merging k/v from supplied cue var-map to end of arg list, so optional effect params can be cue-controlled without manually specifying each."
  [var-map f & args] `(apply ~f ~@args (flatten (seq ~var-map))))

(defmacro make-fn [m] ;EVIL, just gotta test...
 `(fn [& args#]
    (eval
      (cons '~m args#))))


(defn find-nested "Find key at any level of nested maps"
  [m k]
  (->> (tree-seq map? vals m)
       (filter map?)
       (some k)))

; {{{   RANDOM ACTIONS

(defn value "shortcut resolve param to val"
 [param]
 (resolve-param param *show* (metro-snapshot (:metronome *show*))))

(defn clamp-number "Clamp a number within min-max range"
  [number min max]
  (let [number (or number 0)]
    (cond (>= number max) max
          (<= number min) min
          (<= min number max) number)))

(defn randomize-colors "Set new colors for all, or specified, cues/color show vars"
;;  [& {:keys [hue-min hue-max sat-min sat-max] :or {sat-min 20, sat-max 70}}]
 [& {:keys [hue sat lightness] :or {hue (rand 360) sat {:min 30 :max 70} lightness {:min 35 :max 65}}}] ;or like, pass a single value and optionally range around (so instead :min 30 :max 70, :sat 50 :sat-range 40)
 ) ;might have to make compatible by setting such color vars globally through a helper

;; (pprint effects)
;; (def show-keys (map #(into [] (vals %)) (map :variables effects)))
;; (def show-keys (reduce #(conj (vals %)) (map :variables effects)))
;; (def show-keys (reduce into [] #(conj (vals %)) (map :variables effects)))
;; (get-in effects [:cue :variables :start])
;; (get-in effects [:variables])
;
;STUFF:
;(snapshot-cue-variables cue when-id)
;(set-cue-variable! cue var-spec value)
;(get-cue-variable cue var-spec :with-id)
(defn randomize-effect-vars
 [& {:keys [effects cues] :or {effects (:meta @(:active-effects show))}}]
;;  (let [[var-type show-key] (reduce #(into %1 (vals %2)) [] (map :variables effects))
  (def effects (:meta @(:active-effects show)))
  (def var-maps (map :variables effects))
  (def show-keys (reduce #(into %1 (vals %2)) [] var-maps))
  (def show-keys (reduce #(into %1 (vals %2)) (mapcat :variables effects)))
 (let [cues (map :start (flatten (map :variables (map :cue effects))))
       [vms varspecs] (map :variables [effects cues])
       show-key (reduce #(into %1 (vals %2)) [] vms)
       var-type (reduce #(into %1 (keys %2)) [] vms)
       ;; show-vals (map get-variable show-keys)
       ]
  #_(println varspecs)
  #_(println show-key)
  #_(println var-type)))
;; (randomize-effect-vars)

(defn do-random-action
 []
 ) ;runs, kills random registered(?)/cue-grid shit, updates random show var off list, etc

(defn get-random-fixture
 [& {:keys [with-channel multiple? group? stop-after allow-latest? prefer-latest?]}]
 )

(defn get-random-head "calls get-random-fixture, then iterates over heads? which is good since so many more ledstrip heads like..."
 []
 )

(defn kill-random-cue
 [save-vars? cue-filter]
 ((vec (show/active-effect-keys *show*)) 3)
      )
(defn run-random-cue ;would this like set up a one-step chase or how else control duration of arbitrary cue?
 [randomize-vars? cue-filter duration duration-type]
 )

;; bind position to volume instead of straight step param = slo-mo between beats, speedup during
;;high fade-fraction + pause step-param similar
; }}}
; {{{     PARAM HELPERS
(defn ratio-param "Create dynamic param setting the beat ratio for LFO-containing cues. Expects var-map to contain keys `:beats` and `:cycles`, defaulting to 1 if missing."; {{{
  [var-map] ;XXX should be a display overlay (add-control-held-feedback-overlay) touching "beats" brings up cycles...
  ; AND rows of 12345678 beats, 12345678 cycles, mult, div, maybe presets? instant 8/3 etc
  (let [[beats cycles] (map #(or (%1 var-map) %2) [:beats :cycles] [2 1])
        [beats cycles mul div] (map #(bind-keyword-param %1 Number %2)
                                    [beats cycles :lfo-metro-mul :lfo-metro-div] [2 1 1 1])
        f (fn [beats cycles mul div] (* mul (/ beats cycles div)))] ;XXX clamp all four params to 1,2,3,4,6,8 etc
    (build-param-formula Number f beats cycles mul div))); }}}

(defn fraction-param "Create dynamic param representing a fraction as either 0-1 or 1/divisor with optional offset"
  [vm & more]

  (let [target-key (or (first more) (some #{:width :fraction} (flatten (seq vm))))] ;(contains? vm :width)
   (if-let [target (target-key vm)] ;no idea continuing if we don't get something here...
   (let [offset-key (keyword (str (name target-key) "-offset"))
         offset-global (keyword (str (name offset-key) "-global"))
         offset-bind (or (second more) (offset-key vm) (get-variable offset-global)) ;manual offset can go in fn call
         fraction-param (bind-keyword-param target Number 1)
         offset-param  (bind-keyword-param offset-bind Number 0)
         f (fn [fraction offset]
           (if (< 1 fraction)
            (let [offset (or offset 0 #_(/ 1 fraction 8))] ;always leave a little glitch?
             (- (/ 1 fraction) offset))
            fraction))]
    (build-param-formula Number f fraction-param offset-param))
   (bind-keyword-param 1 Number 1))))

; to smooth without keeping track of earlier values, just use rolling average as output? would also work for others
; but we still gotta count invocations to hmm wait... create a unique var to hold vector of last x values?
(defn noise-param "Random noise param, with options for smoothing (rolling average) and min/max volatility"
 [vm & {:keys [smoothing min-jump-fraction max-jump-fraction]}]
 )

(defn jitter-param "Hold specified value, but keep it jiggly"
 [])

(defn drift-param "Drift from specified value, unsteadily, towards target"
 [])

(defn color-breathing-param "Color param, but moves around given color, within bounds, never staying static"
 []
 )

(defn random-wrapper-param "Generates new random number (within range) each time wrapped square lfo hits max. Either lfo 1 frame wide, or have some flag get reset once reaching min..."
 [vm]
 (let [min (bind-keyword-param (:min vm) Number 0)
       max (bind-keyword-param (:max vm) Number 255)
       last-val (ref nil)
       already-triggered (ref nil)
       square (build-oscillated-param (lfo/square :interval-ratio (ratio-param vm)
                                                  :phase (:phase vm) :width 0.01)
                                      :min min :max max)
       f ()]
  (build-param-formula Number f square :last-random-wrap-test)))

(defn trigger-param "Trigger some shit when reaching a specific range. Differs from whatever-param driving a chase by running actual code.
                     Meaning shit can evolve more better hopefully"
 [var-map])

(declare rng-param)
(def lfo-type "sawtooth")

(defn lfo-param "Creates the oscillated parameter used by `lfo-cue` for both the actual effect and its visualizer/color-fn"
 [vm lfo-type & {:keys [min max phase down width lfo-gain lfo-offset level smoothing noise min-change lfos] :as all}] ;so can set these without them being present in var-map

   (let [vm (or vm (lfo-starting-vm lfo-type))
         p '[min max phase down width lfo-gain lfo-offset level min-change]
         ks (map keyword p)
         [min max phase down width lfo-gain lfo-offset level min-change] (map #(or (% :keys) (% vm)) ks)

         params [min max phase lfo-gain lfo-offset level min-change]
         [min max phase lfo-gain lfo-offset level min-change]
         (map #(if %1 (bind-keyword-param %1 Number %2) %2) params [0 255 0 1 0 0 0.1]) ;actually: grab the standard :start val...

         [noise smoothing] (map (fn [k] ;utility fn for this, have map of globals and defaults duh
                                 (when-not (= (type (get-variable k)) Number)
                                  (set-variable! k 0.0))
                                 (bind-keyword-param k Number 0))
                                [:lfo-noise-global :lfo-smooth-global])  ;this one for all. best opt for smoothing?-basic
         lfo (ns-resolve tol-ns (symbol "lfo" lfo-type)) ;max binding workaround...

         raw (case lfo-type
              ("level" "held") (bind-keyword-param (:level vm level) #_(:level vm 0) Number 255) ;straight level as "lfo", later incorporate ext control etc and rename function... ;level, just held
               "random" (rng-param :min min :max max :min-change min-change)
              (build-oscillated-param (apply lfo :interval-ratio (ratio-param vm) :phase phase
                                       (cond (= lfo-type "sawtooth") [:down? down]
                                             (= lfo-type "square")   [:width (fraction-param vm :width)]))
                                      :min min :max max))
         ;; raw-future (resolve-param raw *show* (metro-snapshot (:metronome *show*)))
         final {:type lfo-type :param raw :min min :max max :gain lfo-gain :off lfo-offset :noise noise :vm vm}

         f (fn [param min max gain offset noise] ;;calculate actual resultant value, incorporating gain, offset and noise... XXX and any additional mixed-in params...
            (let [offset (* max offset)
                  noise-base (* max (rand noise)) ;XXX main thing noise has got to be its own bound param we can just resolve and add in. w/ ctrl params for smoothing etc
                  noise (if (< 0 noise-base) (- noise-base (/ (* max noise) 2)) 0)
                  result (try (+ offset noise (* gain param)) (catch Exception e) (finally (pprint final)))]
                  (clamp-number result min max))) ;clamp final
         #_butt #_(pprint final)]
    ;; (map #(when (nil? %) (pprint [vm lfo-type]) #_params) final)
    (build-param-formula Number f raw min max lfo-gain lfo-offset noise))
   #_0)

(defn lfo-color-fn-param
 "Stripped down version. But only reasonable way I think is for all cues
  showing same thing (inactive) to share params and either way, should ease up
 if detecting high load?"
 [vm lfo-type & {:keys [min max phase down? width] :as all}] ;so can set these without them being present in vm
 (let [[min max phase] (map #(or (% all) (% vm)) [:min :max :phase])
       [min max phase] (map #(if %1 (bind-keyword-param %1 Number %2) %2) [min max phase] [0 255 0])
       ;; [min max phase] (map #(if (or (%1 :keys) (%1 vm))
       ;;                        (bind-keyword-param %2 Number %3)
       ;;                        %3)
       ;;                      [:min :max :phase] [min max phase] [0 255 0])
       lfo (ns-resolve tol-ns (symbol "lfo" lfo-type)) ;max binding workaround...
       raw (case lfo-type
             ("level" "held") (bind-keyword-param (:level vm) Number 255) ;straight level as "lfo", later incorporate ext control etc and rename function... ;level, just held
             ("random" rng-param :min min :max max :min-change (:min-change vm 0.1)) ;#_(build-param-formula Number #(* %1 %2 1/3) phase max)) ;phase filling in, fix proper later, +interval-ratio
             (build-oscillated-param (lfo :interval-ratio (ratio-param vm) :phase phase
                                             :down? (:down vm) :width (fraction-param vm :width)) ;maybe clear out the nil ones tho. but no real harm either way
                                        :min min :max max))]
  raw))

(defn #_defmacro def-base-lfo-color-fns
 [& lfo-names]
 (eval (cons 'do
       (map (fn [lfo-type]
           (let [lfo-sym (symbol (str "base-" lfo-type "-color-param"))
                 vm (:variables (get-map-with lfo-type lfo-types :by :type))]
             `(def ~lfo-sym ~(lfo-color-fn-param vm lfo-type))))
          lfo-names)))) ;defonce not actually necessary I guess since got all info from start
;and won't mod... or maybe should scale these with global metro scale tho...
;surely they should just be stuck in a map btw lol

;; (macroexpand-1 `(def-base-lfo-color-fns "square"))
;; (eval (def-base-lfo-color-fns "random"))
;; (value base-sine-color-param)
;; (value base-random-color-param)

(def #_once base-lfo-color-params
 (for [lfo-type (map :type lfo-types)]
  (lfo-color-fn-param vm lfo-type)))

(defn lfo-viz "Create visualizer for lfo-cues"
 [var-map show lfo-type scale]
  (let [p (lfo-param var-map lfo-type)] ;should only create it twice right. Probably in lfo-cue, bind to show var
   ;; could have :lfo entry just as now has :effect :color-fn etc. Inactive cues can often share same instances, just for color-fn
   ;; Then once var-map created, build own to be shared by effect/viz/color, and "pointed" towards varmap targets...
   ;; To change lfo just link new show var...
  (fn [snapshot] (/ (params/evaluate p show snapshot nil) scale))))

(defn quick-lfo "quick wrapper around lfo-param, to make lots"
 []
 )

(defn lfo-chooser-param "run lfo params into a picker whoosing which one to use. SHOULD also allow mixing. XY thing with them in the corners?"; {{{
 [var-map lfo-names & {:keys [picker-param [lfo-params]]}] ;picker could be in var-map as well...
 (let [[min max] (map #(bind-keyword-param (%1 var-map) Number %2) [:min :max] [0 255])
       lfo-syms (map #(ns-resolve tol-ns (symbol "lfo" %)) lfo-names)
       [lfo-sine lfo-saw lfo-tri lfo-square]
       (map (fn [lfo-type]
             ;; (println (name lfo-type))
             (build-oscillated-param
                      ;; (lfo-type :interval-ratio (ratio-param var-map)
                      ;;           :phase (:phase var-map) :down? (:down var-map)
                      ;;           :width (fraction-param var-map :width)) ;maybe clear out the nil ones tho. but no real harm either way
                      (lfo-type :interval-ratio 4 :phase 0.5
                                :down? true
                                :width 0.25) ;maybe clear out the nil ones tho. but no real harm either way
                      :min 0 :max 255))
                 lfo-syms)
       picker (bind-keyword-param (or picker-param (:lfo-picker var-map)) Number 0)
       ;; held-param  (bind-keyword-param (:level var-map) Number 255) ;keep held in this somehow for quick modulation between lfos and M4L incoming - just merge it on

       ;; f (fn [picker & params]
       f (fn [picker lfo-sine lfo-saw lfo-tri lfo-square]
          (let [params [lfo-sine lfo-saw lfo-tri lfo-square]]
           (params picker)))]
 (build-param-formula Number f picker lfo-sine lfo-saw lfo-tri lfo-square))) ; }}}
 ;; (apply build-param-formula Number f picker params))); }}}


(defn var-param "Create variable parameter from var-map"
 [var-map]
 (params/build-variable-param (show/get-variable (:variable var-map))))

(defn step-param "Create step parameter from var-map"; {{{
 [var-map & {:keys [interval] :or {interval :beat}}]
 (build-step-param :interval interval :interval-ratio (ratio-param var-map)
                   :fade-fraction (:fade var-map) :fade-curve (if (:sine-curve var-map)
                                                               :sine :linear)))

(defn lfo-step-param "Create step parameter, from var-map, actual speed of which, as a fraction of set :interval-ratio, is run off LFO (would need high speed/fade-fraction no?)"
 [var-map & {:keys [interval lfo-type] :or {interval :beat, lfo-type "sawtooth"}}]
  (build-step-param :interval interval :interval-ratio (lfo-param var-map lfo-type)
                   :fade-fraction (:fade var-map) :fade-curve (if (:sine-curve var-map)
                                                               :sine :linear)))

(defn lfo-offset-step-param "Create step parameter, from var-map, which actual position is offset by LFO mapped from 0-1 to +-0.5 (at least by default), making it rock ahead unsteadily"
 ;; seems easiest achieved by making :starting dynamic and shifting that to achieve proper offset?
 [var-map & {:keys [interval lfo-type] :or {interval :beat, lfo-type "sawtooth"}}]
 (build-step-param :interval interval :interval-ratio (ratio-param var-map)
                   :fade-fraction (:fade var-map) :fade-curve (if (:sine-curve var-map)
                                                               :sine :linear)
                   :starting )); }}}

(defn multi-lfo "run multiple lfos (through oscillated-params) and combine them in various ways to generate a resultant end value"
;;  [lfo-first lfo-second phase & {:keys [htp? lowtp? average add subtract multiply]}]) ;divide would get hectic...
 [lfo-1 lfo-2 & {:keys [phase htp? lowtp? math-fn]}] ;average add subtract multiply]}]
 (let [f (fn [one two] (/ (+ one two) 2))]
  (build-param-formula Number f lfo-1 lfo-2))) ;divide would get hectic...
;XXX need for this sorta avoided if got to modulating previous values properly. So a mod-level effect of 1.0 followed by 0.5 no-htp doesn't set from 255 to 127, but half of earlier. Above-1.0 also possible to gain-up
 ;{{{
#_(defn graph-skip-sawtooth ;use as base/ex for merging oscs
  "Draw a graph of a chase which skips every other instance of a double-time sawtooth beat oscillator, as an example of how to compose oscillators with chases; a square wave oscillator is used to pick between the chase elements."
  []

  (let [metro (:metronome *show*)
        position-param (osc/build-oscillated-param (osc/square) :min 1 :max 2)
        saw-param (osc/build-oscillated-param (osc/sawtooth :beat-ratio (/ 2)))
        chase (fx/chase "Gap Saw"
                        [(fx/blank)
                         (afterglow.effects.dimmer/dimmer-effect saw-param (show/all-fixtures))]
                        position-param)
        f (fn [x] (let [snapshot (build-beat-snapshot metro x)
                        assigners (fx/generate chase a-show snapshot)]
                   (if-let [assigner (first assigners)]
                    (params/resolve-param (fx/assign assigner a-show snapshot nil nil) a-show snapshot)
                    0)))
        plot (function-plot f 0 4 :x-label "beat" :y-label "dimmer value"
                            :title "Chase including every other sawtooth wave")]
   (view plot))) ; }}}

; {{{ RANDOM NUMBER PARAM
(defn- pick-new-value "Helper for random-params, pick new value with min difference from last value."
 [old-value min range min-change]
 (loop [new-value (+ min (rand range))]
   (if (or (nil? old-value)
           (>= (math/abs (- new-value old-value)) min-change))
     new-value
     (recur (+ min (rand range))))))

;; XXX this should be implemented as an osc? picks random new values ahead, then interpolates towards...  Or just add settable interval-ratio...
(defn rng-param "Returns a dynamic number parameter which gets a new random value on each beat."
 [& {:keys [min max min-change interval] :or {min 0 max 255 min-change 0 interval :beat}}]
 {:pre [(some? *show*)]}
 (let [[min max min-change] (map #(bind-keyword-param %1 Number %2) [min max min-change] [0 255 0])
       [last-beat last-value] (map ref [nil nil])]
   (if-not (some params/param? [min max min-change])

     (let [range (- max min) ;; Optimize the simple case of all constant parameters
           eval-fn (fn [_ snapshot]
                     (dosync (when (not= @last-beat (:beat snapshot)) ;should be fn
                               (ref-set last-beat (:beat snapshot))
                               (alter last-value pick-new-value min range min-change))
                             @last-value))]
       (when-not (pos? range) (throw (IllegalArgumentException. "min must be less than max")))
       (when-not (< min-change (/ range 3)) (throw (IllegalArgumentException. "min-change must be less 1/3 the range")))
       (reify params/IParam
         (params/evaluate [this show snapshot _] (eval-fn show snapshot))
         (params/frame-dynamic? [this] true)
         (params/result-type [this] Number)
         (params/resolve-non-frame-dynamic-elements [this _ _ _] this)))  ; Nothing to resolve, return self

     (let [eval-fn (fn [show snapshot] ;; Support the general case where we have an incoming variable parameter
                     (let [[min max min-change] (map #(resolve-param %1 show snapshot) [min max min-change])
                           range (- max min)]
                       (if (neg? range)
                         (do (timbre/error "Random beat number parameters min > max, returning max.") max)
                         (if (< min-change (/ range 3))
                             (dosync (when (not= @last-beat (:beat snapshot))
                                       (ref-set last-beat (:beat snapshot))
                                       (alter last-value pick-new-value min range min-change))
                                     @last-value)
                             (do (timbre/error "Random beat number min-change > 1/3 range, returning max.") max)))))]
       (reify params/IParam
         (params/evaluate [this show snapshot _] (eval-fn show snapshot))
         (params/frame-dynamic? [this] true)
         (params/result-type [this] Number)
         (params/resolve-non-frame-dynamic-elements [this show snapshot head]
           (with-show show
            (let [[min max min-change] (map #(params/resolve-unless-frame-dynamic % show snapshot head)
                                            [min max min-change])]
             (rng-param :min min :max max :min-change min-change)))))))))


; }}}

; }}}

; {{{     CUE HELPERS

(defn group-cue "Base helper for fixture-group cue builders"
 [group x y & {:keys [held? priority effect-name extra-params]}] ; XXX rename(?) group and make it handle anything thrown at it - group name, vector of fixture keywords, output of (fixtures-named) etc
 (let [[effect-key fixtures effect-name] (group-cue-parts group "color" "color")]
  )
 )

(defn color-cue "Create a cue-grid entry which establishes a global color effect, given a named color. Also set up a cue color parameter so the color can be tweaked in the Web UI or on the Ableton Push, and changes can be saved to persist between invocations."; {{{
  [group x y color-name & {:keys [include-color-wheels? held? fixtures priority color]
                           :or   {include-color-wheels? true, priority 0}}]
  (let [color (or color (color-like color-name))
        [effect-key fixtures effect-name] (group-cue-parts group "color" "color")
        color-var {:key "color", :type :color, :start color, :name "Color"}
        cue (cue effect-key
                 (fn [var-map]
                  (global-color-effect (bind-keyword-param (:color var-map) ::colors/color color)
                                       :effect-name effect-name :fixtures fixtures :include-color-wheels? include-color-wheels?))
                 :priority priority :held held? :color color
                 :color-fn (cues/color-fn-from-cue-var color-var x y)
                 :variables [color-var])]
   (set-cue! x y cue))); }}}

(defn dimmer-cue "Create cue adjusting dimmer level of a group of fixtures, being one of the values in [[light-groups]], or `nil` if the cue should affect all lights."; {{{
  [group x y color & {:keys [priority end-rest? htp? add-virtual-dimmers? held? velocity?] :or {priority 5, htp? true}}]
  (let [[effect-key fixtures effect-name end-keys] (group-cue-parts group "dimmers" "dimmers")]
    (set-cue! x y
      (cue effect-key
           (fn [var-map] (dimmer-effect (bind-keyword-param (:level var-map 255) Number 255) fixtures
                                        :effect-name effect-name, :htp? htp?, :add-virtual-dimmers? add-virtual-dimmers?))
           :variables [{:key "level", :min 0, :max 255, :start 255, :velocity velocity? :velocity-min 10 :name "Level"}]
           :held held?, :priority priority, :color color, :end-keys (if end-rest? end-keys))))); }}}

(defn strobe-cue "tweaks, incl uniform [group x y] params inline with other make-cues, dynamic color-var and velocity-target, evt externalize other vars (level etc), not just color"; {{{
  [group x y & {:keys [strobe-color fixtures velocity-target]
                :or   {strobe-color :strobe-color, velocity-target "level"}}]
  (when-not (= (type (get-variable strobe-color)) ::colors/color)
    (set-variable! strobe-color (color-like "mediumpurple2")))
  (let [[effect-key fixtures effect-name] (group-cue-parts group "strobe" "strobe") ; XXX instead concat fixture names if passed
        color-var {:key strobe-color, :type :color, :name (name strobe-color)}
        [level-var lightness-var] (map #(make-var [%1 (if (= velocity-target %1) true %2) 0 100])
                                  ["level" "lightness"] [70 100])]
    (set-cue! x y
      (cue effect-key
           (fn [vm] #_(pprint vm #_(type (:level vm)))
            (fun/strobe-2 effect-name fixtures (bind-keyword-param (:level vm) Number 70) (:lightness vm)))
           :held true, :priority 11000, :color (get-variable strobe-color)
           :color-fn (fn [cue active show snapshot]
                       (if (> (snapshot-bar-phase snapshot 0.5) 0.9)
                         (create-color :black)
                         (or (get-variable strobe-color) (:color cue))))
           ;; :variables (cue-vars (map #(make-var [%1 (if (= velocity-target %1) true %2) 0 100])
                        :variables [level-var lightness-var color-var])))); }}}

(defn channel-cue "control channel in group/fixture. TODO: optionally pass multiple fixtures, and control multiple channels from one cue"; {{{
 [group x y channel-key & {:keys [color held? priority effect-name]
                          :or {color :white, priority 0}}] ; fixtures (all-fixtures),
 (let [[effect-key fixtures effect-name end-keys]
       (group-cue-parts group (name channel-key) (name channel-key))]
   (set-cue! x y (cue effect-key
      (fn [vm] #_(pprint vm) (channel-effect (:level vm)
                                    (extract-channels fixtures #(= (:type %) channel-key)) ;; (extract-channels fixtures #(= (:type %) (keyword channel)))))
                                    :effect-name effect-name))
      :short-name effect-name, :held held?, :color color, :priority priority
      :variables [{:key "level" :name effect-name :min 0 :max 255 :start 0 :type :integer :velocity held?}])))); }}}


;; (defn lfo-color-fn
;;  [])
;; (defn lfo-color-param
;;  [])

(defn starting-vm "Generate var-map substitute off :variables starting values"
 []
 )
;; (avar :cue-temp-4-3-beats)

(defn lfo-cue "Create cue which applies an lfo of named type to target effect, for specified group/fixtures, with cue variables to adjust the oscillator parameters. Target effect must take two keys, value and fixtures. Additional keys can be passed" ; {{{
;; WISH-LIST: pass which key is to be controlled so complex effects don't have to be wrapped, + keys for var-map for full control.
;; later: dont actually couple to effect-function at all, but start osc-cue, then pick another cue to be controlled, then pick which var to control.
  ;; could pass like "ext" as "lfo" type for <> ctrl straight over libmapper etc, through common interface of simply using show var
 [group x y lfo-type fx-fn & ;use macro so can pass lfo-type not as string but symbol
  {:keys [priority end-rest? color scale held? extra-keys extra-vars
          channels channel-type heads head-type] ;channels/heads for eg channel-effect requiring that - handle automatically later...
   :or {priority 10}}]
 (let [lfo-data (into {} (filter #(= (:type %) lfo-type) lfo-types))
       [fx-key fixtures fx-name end-keys] (group-cue-parts group (str fx-fn) lfo-type
                                                           :channel channel-type)
       color (or color (:color lfo-data))
       lfo-vars (:variables lfo-data) ;XXX switch to float min-max and use scale for eventual value. express width as 1/cycles (+- offset for fine adj?), same concept as reg beats/cycles
       variables (if-not extra-vars lfo-vars (into lfo-vars (flatten extra-vars)))
       scale (or scale (:max (get-map-with "level" variables)) (:max (get-map-with "max" variables)) 255) ;get scale from max level if not otherwise specified
       channels (or channels (get-channels channel-type fixtures))
       targets (or channels heads fixtures)
       ;; param '(lfo-param vm ~lfo-type)
       held? (if (nil? held?) (:velocity (get-map-with "level" variables)) held?)] ;should scan all vars rather  ;velocity implies held unless specified. (if not or) so handles non-nil falsey
   (set-cue! x y
     (cue fx-key
          (fn [vm] (effect (fx-fn (#_lfo-color-fn-param lfo-param vm lfo-type) targets :effect-name fx-name)
                           (fraction-param vm :alpha))) ;; (fn [vm] (apply (make-fn apply-vm) vm fx-fn (lfo-param vm lfo-type) targets :effect-name fx-name, extra-keys #_(if extra-keys (flatten extra-keys))) )
          :priority priority, :color color, :variables variables ;XXX pack resolution into all doubles for better control? shift adjust obvs necessary later
          ;; :visualizer (fn [vm show] (lfo-viz vm show lfo-type scale))
          ;; :color-fn (cues/color-fn-from-param color-param) ; NOTE: lightness has no effect, only hue/sat
          ;; :color-fn (fn [cue active show snapshot] ;this needs to reuse, not recreate every frame...
          ;;            ;(:variables active) is vm. but is nil until started
          ;;            ;(:variables cue) is same as passed, vector of var-defs. could construct similar from :key and :start
          ;;            ;but that'd go in sep func and reuse those values
          ;;            ;; (pprint cue)
          ;;            ;; (if active) ;should try to reuse visualizer param if available, esp since that's normalized etc
          ;;              ;; (let [l (lfo-color-fn-param (:variables active) lfo-type :min -30 :max 10)
          ;;            ;; (let [l (lfo-param (or (:variables active) (lfo-starting-vm lfo-type)) lfo-type :min -30 :max 10)
          ;;            (let [l (lfo-param (:variables active) lfo-type :min -30 :max 10)
          ;;                  value (resolve-param l show snapshot)]
          ;;             ;; (print value " ")
          ;;             (build-color-param :color (:color cue) :adjust-saturation value :adjust-hue (* 0.7 value))))
          :held held? ;also maybe default if any vars have velocity auto-held? bit tricky automating otherwise
          :end-keys (if end-rest? end-keys))))) ; }}}


(defn multi-lfo-cue "Create cue which applies an lfo of named type to target effect, for specified group/fixtures, with cue variables to adjust the oscillator parameters. Target effect must take two keys, value and fixtures. Additional keys can be passed" ; {{{
 [group x y lfo-names fx-fn &
  {:keys [priority end-rest? color scale held? extra-keys extra-vars
          channels channel-type heads head-type]
   :or {priority 10}}]
 (let [lfo-data (into {} (filter #(= (:type %) lfo-names) lfo-types))
       [fx-key fixtures fx-name end-keys] (group-cue-parts group (str fx-fn) "multi" :channel channel-type) ;FIX might not be getting channel-type
       color :white #_(or color #_(:color lfo-data))
       lfo-vars (:variables lfo-data)
       variables (if-not extra-vars lfo-vars (into lfo-vars (flatten extra-vars)))
       scale (or scale (:max (get-map-with "level" variables)) (:max (get-map-with "max" variables)) 255)
       ;; channels (or channels (get-channels channel-type fixtures))
       targets (or channels heads fixtures)
       held? (if (nil? held?) (:velocity (get-map-with "level" variables)) held?)] ;should scan all vars rather  ;velocity implies held unless specified. (if not or) so handles non-nil falsey
   (set-cue! x y
     (cue fx-key
          (fn [vm] (fx-fn (lfo-chooser-param vm lfo-types :picker-param (rng-param 1 4))
                          targets :effect-name fx-name))
          :priority priority, :color color, :variables variables
          ;; :visualizer (fn [vm show] (lfo-viz vm show lfo-type scale))
          ;; :color-fn (fn [cue active show snapshot]
          ;;            (let [l (lfo-color-fn-param (:variables active) lfo-type :min -35 :max 10)]
          ;;             (build-color-param :color (:color cue)
          ;;                                :adjust-saturation (resolve-param l show snapshot) ;(* -20))
          ;;                                :adjust-hue (resolve-param l show snapshot)))) ;(* 40) (- 20)))))
          :held held?
          :end-keys (if end-rest? end-keys))))) ; }}}


;; (defn what []
;;  (def vm (:variables (show/find-effect :sweep-dimmers)))
;;  (def p (lfo-param vm "sine"))
;;  (def b (:beats vm))
;;  (def si (sine))
;; ;;  (let [vm (:variables (show/find-effect :sweep-dimmers))
;; ;;        b (:beats vm)
;; ;;        p (lfo-param vm "sine")]
;;   (avar b)
;;  (value p))

;; (apply cue-vars :beats :cycles (when true [:hue-mod]))
 ;(rng-param 0 0.5)

(defn bloom-cue "build cue for bloom effect"
 [group x y color & {:keys [mods? keyhole? measure fraction-param lfo-type priority]
                     :or {:measure default-measure :lfo-type "sine"}}]
  (let [[fx-key fixtures fx-name end-keys] (group-cue-parts group "bloom" "bloom")
        color (or color (create-color color))]
   (set-cue! x y
      (cue fx-key
           (fn [vm]
             (let [fraction (or fraction-param (lfo-param vm lfo-type))]
              (apply-vm vm bloom-tol (all-fixtures) :measure measure, :fraction fraction)))
           :variables (apply cue-vars :beats :cycles :min-num :max-num :width
                                      (make-var ["color" color])
                                      (when mods? [:hue-mod :lightness-mod :saturation-mod]))
           :visualizer (fn [vm show] (lfo-viz vm show lfo-type 1.0))
           :short-name fx-name, :priority priority, :color color))) )


(defn blank-cue "cue that runs a scene, including taking input from operator, but also running code to replace itself, with a new cue, taking more input, eventually spawning an effect cue"
 []
 (code-cue)
 )


(defn lfo-cue-row "Create a row of lfo-cues, from passed lfo-types"
 [group x yb effect-fn & {:keys [args lfos cue-fn colormap] :or {lfos lfo-types, cue-fn lfo-cue}}]
 (doall (map-indexed
     (fn [y lfo-type] ;possible somehow transforming input params in-place?
      (let [y (+ yb y) ;[x y] (map + [xb yb] [x y])
            type (:type lfo-type)
            #_color #_(:color (or colormap lfo-type))]
       ;XXX flag to put "group" into eg :channel-type
       ;needed to make truly generic wrt effect-fn...
       (apply cue-fn group x y type effect-fn args))) ;, :color (:color lfo-type) ;not needed handled in lfo-cue?
     lfos)))


(defn lfo-cue-page "Create a page of lfo-cues, from passed groups and lfo-types"
 [groups xb yb effect-fn & {:keys [args lfos cue-fn colormap] :or {lfos lfo-types, cue-fn lfo-cue}}]

 (doall (map-indexed
   (fn [x group]
    (let [group group]
     (lfo-cue-row group (+ xb x) yb effect-fn :args args)))
   groups)))

; }}}

;j}}}

; {{{  INDIVIDUAL CUE PAGES

; {{{     MAIN COLOR/DIMMER/STROBE CUES

(defn make-main-color-dimmer-cues-tol "Tol make cues."; {{{
 [& {:keys [page-x page-y groups globals color] :or {page-x 0, page-y 0, globals true}}]
 (let [xb (* page-x 8), yb (* page-y 8)
       groups (or groups (concat [nil] light-groups))] ; nil resolves to all fixtures when passed to helper functions

  (doall (map-indexed ; Strobe cues
            (fn [i group]
              (strobe-cue group (+ xb i) (+ yb 0) :velocity-target "level"))
            groups))

  (let [color-var {:key :strobe-color, :type :color, :name "Strobe Color"}]
        ;; #_level-var #_(do (set-variable! :strobe-level 80.0)
        ;;            (make-var [:strobe-level 80.0 0.0 100.0])) #_{:key :strobe-level, :min 0.0, :max 100.0, :start 80.0, :name "Strobe Level"}]
   (set-cue! (+ xb 7) (+ yb 0)
             (cue :strobe-color (fn [_] (fx/blank "Strobe Color"))
                  :color :purple, :color-fn (cues/color-fn-from-cue-var color-var)
                  ;; :variables [color-var #_level-var])))
                  :variables [color-var])))

  (lfo-cue-page groups xb (+ yb 1) dimmer-effect)

  (when color (doall (map-indexed
           (fn [i group] (color-cue group (+ xb i) (+ yb 7) nil, :color (color-like "royalblue")
                                    :priority (if group 1 0))) groups)))

  (when globals
   (dimmer-cue nil (+ xb 7) (+ yb 1) :white, :priority 11000) ; ALL OVERRIDE PRIO
   (lfo-cue-row nil (+ xb 7) (+ yb 1) dimmer-effect :args [:priority 11000])))) ; }}}

; }}}

; {{{          DYNAMIC CUES
(defn make-dynamic-cues "make cues to select which fixture to point an effect to, and what prio to run it at"
  [& {:keys [page-x page-y] :or {page-x 0 page-y 0}}] ; {{{
  (let [xb (* page-x 8)
        yb (* page-y 8)
        fixtures [:strip-1 :strip-2 :strip-3 :strip-4 :moving-1 :moving-2 :wash-1 :wash-2]
        rig-width (- right-wall left-wall)]


 ));}}}
; }}}

; {{{  EFFECT CUES

(defn make-effect-cues
  [& {:keys [page-x page-y] :or {page-x 0 page-y 0}}] ; {{{
  (let [xb (* page-x 8)
        yb (* page-y 8)
        rig-width (- right-wall left-wall)
        strips-tubes (concat (fixtures-named :strip) (fixtures-named :tube))]
         ; }}}

  (channel-cue :fogger (+ xb 0) (+ yb 0) :fog, :held? true, :color :antiquewhite3)
  (lfo-cue     :fogger (+ xb 1) (+ yb 0) "square" channel-effect, :channel-type :fog, :color :antiquewhite3)
   ;; (set-cue! (+ xb 0) (+ yb 0)
   ;;   (cue :fog-active
   ;;    (fn [_] (channel-effect "Make fog spew" :fog-level
   ;;                                    (extract-channels (fixtures-named :fogger) #(= (:type %) :fog))))
   ;;            :short-name "Fog", :held true, :color :antiquewhite3
   ;;            :variables [{:key :fog-level :name "Fog Level" :min 0 :max 255 :start 30 :type :integer :velocity true}]))
  (set-cue! (+ xb 2) (+ yb 0)
    (code-cue (fn [show snapshot] (metro-start (:metronome *show*) 1)) "Reset Metronome"))
  (set-cue! (+ xb 3) (+ yb 0)
    (code-cue (fn [show snapshot] (ns-resolve (symbol tol-ns) make-cues-tol)) "Reload Cues"))

  (when strips-tubes ;XXX should extract all funky ctrl chs and automatically map out

   ;; (set-cue! (+ xb 2) (+ yb 7)
   ;;   (cue :strip-settings ;XXX how do this? stack multiple channel effects in one cues...
   ;;     (fn [var-map] (scene "Strip control"
   ;;                    ;; (into [] (map #(channel-effect
   ;;                    ;; (keyword (str "strip-" (name %))) (get-channels strips-tubes %),
   ;;                    ;; :effect-name (str (capitalize (name %)) " strip"))
   ;;                    ;; [:noise :bleed :attack :release])) ))
   ;;                    [(channel-effect  (var-map :strip-noise) (extract-channels strips-tubes #(= (:type %) :noise))   :effect-name "Noise strip"  )
   ;;                     (channel-effect  (var-map :strip-bleed) (extract-channels strips-tubes #(= (:type %) :bleed))   :effect-name "Bleed strip"  )
   ;;                     (channel-effect  (var-map :strip-attack) (extract-channels strips-tubes #(= (:type %) :attack))  :effect-name "Attack strip" )
   ;;                     (channel-effect  (var-map :strip-release) (extract-channels strips-tubes #(= (:type %) :release)) :effect-name "Release strip") ]))
   ;;     :short-name "Strip settings", :color :darkolivegreen1
   ;;     :variables [{:key :strip-noise :name "Noise" :min 0 :max 255 :start 0 :type :integer}
   ;;                 {:key :strip-bleed :name "Bleed" :min 0 :max 255 :start 0 :type :integer}
   ;;                 {:key :strip-attack :name "Attack" :min 0 :max 255 :start 0 :type :integer}
   ;;                 {:key :strip-release :name "Release" :min 0 :max 255 :start 0 :type :integer}]))
   )

   ;; (set-cue! x y (parse-cue z w :+held) etc would be plenty nice as well tho
   ;; PLUS: a very good foundation for spawning cues straight from push, when needed. also writing out to save-file
   ;; would also allow straight cloning to spawn multiple copies of one cue-effect.  look through macro code and finish that shit up hey. function cues is prob where it's at.
   ;; REMEMBER: a :held with same key is awesome way to change value of param in running cue rhytmically...

    ;; {{{ SPARKLE
    (let [color (make-var ["color" (-> (create-color :purple) (desaturate 30) (lighten 10))])
          [min max] (map #(make-var %) [["min" 0.0 0.0 0.1] ["max" 0.1 0.0 0.2]])
          variables (cue-vars :fade-time color :beats :cycles min max :lfo-gain :lfo-offset)]
     (set-cue!  (+ xb 7) (+ yb 7)
      (cue :sparkle-tol (fn [var-map]
                          (let [chance (lfo-param var-map "sawtooth")]
                          (apply-vm var-map sparkle-tol (all-fixtures) :chance chance)))
           :held false, :priority 400, :color (:start color), :color-fn (color-fn-from-cue-var color)
           :visualizer #(lfo-viz %1 %2 "sawtooth" (:max max))
           :variables variables)))

    (let [color-var  (make-var ["color" (-> (create-color :purple) (desaturate 30) (lighten 20))])]
     (set-cue!  (+ xb 7) (+ yb 6)
      (cue :sparkle (fn [var-map] (apply-vm var-map fun/sparkle (all-fixtures)))
           :held true, :priority 100, :color "white", :color-fn (color-fn-from-cue-var color-var)
           :variables [{:key "chance",    :min 0.0, :max 0.2,  :start 0.0000, :velocity false}
                       {:key "fade-time", :min 1,   :max 2000, :start 20, :type :integer, :velocity true, :name "Fade"}
                       color-var])))

    (let [color-var {:key "color", :type :color, :start (desaturate (create-color :red) 40), :name "Color"}]
     (set-cue! (+ xb 7) (+ yb 5) (cue :sparkle-2   (fn [var-map] (fun/sparkle (all-fixtures) :chance (rng-param :min 0 :max 0.2) :color (:color var-map) :fade-time (:fade-time var-map)))
                                               :held false, :priority 2000, :color "red", :color-fn (color-fn-from-cue-var color-var)
                                               :variables [{:key "chance",    :min 0.0, :max 0.15,  :start 0.0001, :velocity false}
                                                           {:key "fade-time", :min 1,   :max 2500, :start 200, :type :integer, :velocity false, :name "Fade"}
                                                           color-var])))
    (let [color-var {:key "color", :type :color, :start (darken (create-color :blue) 30), :name "Color"}]
     (set-cue! (+ xb 7) (+ yb 4) (cue :sparkle-3   (fn [var-map] (apply-vm var-map fun/sparkle (all-fixtures)))
                                               :held true, :priority 20000, :color "blue", :color-fn (color-fn-from-cue-var color-var)
                                               :variables [{:key "chance",    :min 0.0, :max 0.15,  :start 0.0001, :type nil, :velocity true}
                                                           {:key "fade-time", :min 1,   :max 200, :start 10, :type :integer, :velocity false, :name "Fade"}
                                                           color-var])))
;; }}}
; {{{  BLOOMS
    ;; BLOOM
    (set-cue! (+ xb 6) (+ yb 7)
      (cue :bloom
        (fn [vm] (apply-vm vm bloom-tol (all-fixtures) :measure default-measure :keyhole? true))
        :variables (cue-vars :color :width :fraction-vel) #_[{:key "color", :type :color, :start (create-color :white), :name "Color"}
                    {:key "width", :min 0.0, :max 1.0, :start 0.2, :name "Width"}
                    {:key "fraction", :min 0, :max 1, :start 0, :velocity true}]
        :held true, :priority 1000, :color :purple))
    ;; linked
    ;; (set-cue! (+ xb 6) (+ yb 6)
    ;;   (cue :bloom-link2
    ;;     (fn [vm]
    ;;       (let [fraction (build-oscillated-param (triangle :interval-ratio (ratio-param vm)) :max (:max vm), :min (:min vm))]
    ;;        ;; (let [fraction (lfo-param vm "triangle")]
    ;;        (fun/bloom (all-fixtures) :measure default-measure :fraction fraction :color (:color vm))))
    ;;     :variables  (use-vars [:beats :cycles :min-num :max-num] (make-var ["color" "orangered"]))
    ;;     :held false, :priority 1001, :color :red))

    (set-cue! (+ xb 6) (+ yb 5)
      (cue :bloom-strip
        (fn [vm] (apply-vm vm fun/bloom strips-tubes :measure default-measure))
                :variables (use-vars [:color :fraction-vel]) ;{:key "fraction", :min 0, :max 1, :start 0, :velocity true})
                :held true, :priority 1000, :color :purple))
    ;; linked
    (set-cue! (+ xb 6) (+ yb 4)
      (cue :bloom-link2-strip
        (fn [vm]
          (let [fraction (build-oscillated-param (triangle :interval-ratio (ratio-param vm)) :max (:max vm), :min (:min vm))] ;(lfo-param-2 vm triangle)]
           (apply-vm vm fun/bloom strips-tubes :measure default-measure :fraction fraction)))
        :variables (use-vars [:beats :cycles :min-num :max-num]
                              (make-var ["color" (lighten (color-like "orange") 10)]))
          :held false, :priority 1001, :color :orange))


    (set-cue! (+ xb 6) (+ yb 0)
      (cue :bloom-tol ;XXX velocity cue, not held but one-off, not continous effect. slim bloom slipping across strips, diff speed/hue? from velocity
           (fn [vm]
             (let [fraction (lfo-param vm "sine")] ;(build-oscillated-param (sine :interval-ratio (ratio-param vm)) :max (:max vm), :min (:min vm))] ;
               (apply-vm vm bloom-tol (all-fixtures) :measure default-measure :fraction (rng-param 0 0.5) :width 0.2)))
           :variables (cue-vars :beats :cycles :min-num :max-num
                                (make-vars ["color" "mediumpurple"] ["hue-mod" 50 -180 180]
                                           ["lightness-mod" -5 -50 50] ["saturation-mod" 10 -50 50])
           :visualizer (fn [vm show] ;XXX make generic build-visualizer function
                           (let [p (build-oscillated-param (sine :interval-ratio (ratio-param vm)) :max (:max vm), :min (:min vm))]
                           ;; (let [p (lfo-param vm "sine")]
                             (fn [snapshot] (params/evaluate p show snapshot nil) )))
                         :short-name "Bloom mod", :priority 2001, :color :seagreen))

    (set-cue! (+ xb 6) (+ yb 1)
      (cue :bloom-stripe
                (fn [vm] (let [fraction (lfo-param vm "sine")]
                    (apply-vm vm bloom-tol strips-tubes :measure default-measure :fraction fraction)))
                :variables (cue-vars (make-var ["color" "navajowhite3"]) :fraction-vel (cv :width 0.1)
                                     :hue-mod :lightness-mod :saturation-mod :beats :cycles :min-num :max-num)
                #_[{:key "color", :type :color, :start (create-color :mediumpurple), :name "Color"}
                            {:key "width", :min 0.0, :max 1.0, :start 0.1, :name "Width"}
                            {:key "hue-mod" :min -180 :max 180 :start 30 :type :integer :name "Hue mod"}
                            {:key "lightness-mod" :min -50 :max 50 :start -8 :type :integer :name "Lightness mod"}
                            {:key "saturation-mod" :min -50 :max 50 :start -15 :type :integer :name "Saturation mod"}
                            {:key "beats", :min 1, :max 32, :start 8, :type :integer, :name "Beats"}
                            {:key "cycles", :min 1, :max 8, :start 1, :type :integer, :name "Cycles"}
                            {:key "max", :min 0.0, :max 1.0, :start 1.0, :name "Max"}
                            {:key "min", :min 0.0, :max 1.0, :start 0.0, :name "Min"}]
                :short-name "Bloom stripe", :priority 1002, :color :orangered))

    (set-cue! (+ xb 6) (+ yb 2)
      (cue :bloom-stripe-2
                 (fn [vm]
                    ;; (let [fraction (lfo-param vm "sine")]
                   (let [fraction (build-oscillated-param (sine :interval-ratio (ratio-param vm)) :max (:max vm), :min (:min vm))] ;(lfo-param vm "sine")]
                    (effect (apply-vm vm bloom-tol (all-fixtures) :measure default-measure :fraction fraction
                                           :width (rng-param :min 0.02 :max 0.2 :min-change 0.05)) fraction)))
                :variables (cue-vars :beats :cycles :min-num :max-num :hue-mod :lightness-mod :saturation-mod
                                     (make-var ["color" "seagreen"]))
                :short-name "Bloom stripe 2", :priority 1012, :color :orange))

    (set-cue! (+ xb 7) (+ yb 0)
      (cue :bloom-stripe-held
                 (fn [vm] (apply-vm vm bloom-tol strips-tubes :measure default-measure))
                :variables (cue-vars (make-var ["color" "mediumpurple"]) :fraction-vel (cv :width 0.15)
                                     :hue-mod :lightness-mod :saturation-mod))
                #_[{:key "color", :type :color, :start (create-color :mediumpurple), :name "Color"}
                            {:key "fraction", :min 0, :max 1, :start 0, :velocity true, :velocity-min 0.1 :velocity-max 0.9}
                            {:key "width", :min 0.0, :max 1.0, :start 0.15, :name "Width"}
                            {:key "hue-mod" :min -180 :max 180 :start -20 :type :integer :name "Hue mod"}
                            {:key "lightness-mod" :min -50 :max 50 :start -7 :type :integer :name "Lightness mod"}
                            {:key "saturation-mod" :min -50 :max 50 :start -20 :type :integer :name "Saturation mod"}]
                :short-name "Bloom stripe held", :held true :priority 3012, :color :yellow))

    (set-cue! (+ xb 7) (+ yb 1)
      (cue :bloom-stripe-held-2
                 (fn [vm] (apply-vm vm bloom-tol strips-tubes :measure default-measure))
                :variables (cue-vars (make-var ["color" "navajowhite3"]) :fraction-vel (cv :width 0.10)
                                     :hue-mod :lightness-mod :saturation-mod)
                #_[{:key "color", :type :color, :start (create-color :navajowhite3), :name "Color"}
                            {:key "fraction", :min 0, :max 1, :start 0, :velocity true, :velocity-min 0.1 :velocity-max 0.9}
                            {:key "width", :min 0.0, :max 1.0, :start 0.15, :name "Width"}
                            {:key "hue-mod" :min -180 :max 180 :start 30 :type :integer :name "Hue mod"}
                            {:key "lightness-mod" :min -50 :max 50 :start -7 :type :integer :name "Lightness mod"}
                            {:key "saturation-mod" :min -50 :max 50 :start -20 :type :integer :name "Saturation mod"}]
                :short-name "Bloom stripe held 2", :held true :priority 3012, :color :yellow))

    (set-cue! (+ xb 7) (+ yb 2)
      (cue :bloom-keyhole
           (fn [vm]
             (let [fraction (build-oscillated-param (sine :interval-ratio (ratio-param vm)) :max (:max vm), :min (:min vm)) #_(lfo-param vm "sine")]
              (apply-vm vm bloom-tol  (all-fixtures) :measure default-measure :fraction fraction :keyhole? true)))
        :variables (cue-vars :beats :cycles :max-num :min-num (make-var ["width" 0.3 0.0 1.0]))
        :short-name "Bloom keyhole", :priority 2001, :color :green))

    (set-cue! (+ xb 7) (+ yb 3)
      (cue :bloom-keyhole-held
                 (fn [vm] (apply-vm vm bloom-tol  strips-tubes :measure default-measure :keyhole? true))
        :variables [{:key "fraction", :min 0, :max 1, :start 0, :velocity true}
                    {:key "width", :min 0.0, :max 1.0, :start 0.3, :velocity true, :velocity-max 0.5, :name "Width"}]
        :short-name "Bloom keyhole", :held true, :priority 2001, :color :lightgreen))

    ;; reverse
    (set-cue! (+ xb 5) (+ yb 7)
      (cue :bloom-reverse
           (fn [vm] (apply-vm vm fun/bloom (all-fixtures) :color (create-color :black) :measure default-measure))
           :variables (cue-vars :fraction-vel)
           :held true, :priority 1002, :color :black))

    ;; auto, reverse
    (set-cue! (+ xb 5) (+ yb 6)
              (cue :bloom-link
              (fn [vm]
                (let [fraction (build-oscillated-param (sine :interval-ratio (ratio-param vm)) :max (:max vm) :min (:min vm))] ;(lfo-param vm "sine")
                ;; (let [fraction (lfo-param vm "sine")] ;
                 (apply-vm vm fun/bloom (all-fixtures) :fraction fraction :color (create-color :black) :measure default-measure)))
              :variables (cue-vars :beats :cycles :max-num :min-num :phase)
     :priority 19999, :color :black))

    (set-cue! (+ 0 5) (+ 0 5)
              (cue :bloom-reverse-strip
                   (fn [vm] (apply-vm vm fun/bloom (all-fixtures) :color (create-color :black) :measure default-measure))
                   ;; (fn [vm] (let [fraction (bind-keyword-param (:fraction vm) Number 0)]
                    ;; (fun/bloom strips-tubes :fraction fraction :color (create-color :black)
       :variables [{:key "fraction", :min 0, :max 1, :start 0.0, :velocity true}]
       :held true, :priority 1002, :color :black))

    (set-cue! (+ xb 5) (+ yb 4)
              (cue :bloom-link-strip
                (fn [vm]
                  (let [fraction (lfo-param vm "sawtooth")]
                   (apply-vm vm fun/bloom strips-tubes :fraction fraction :color (create-color :black) :measure default-measure)))
                   :variables (cue-vars :beats :cycles :max-num :min-num :down)
       :priority 1999, :color :black))

    (set-cue! (+ xb 5) (+ yb 0)
       (cue :bloom-reverse-tol
                 (fn [vm]
                   (apply-vm vm bloom-tol (all-fixtures) :color (create-color :black) :measure default-measure))
                   :variables (cue-vars :fraction-vel :width) #_[{:key "fraction", :min 0, :max 1, :start 0, :velocity true}
                               {:key "width", :min 0, :max 1, :start 0.2}]
                   :held true, :priority 1002, :color :black))

    (set-cue! (+ xb 5) (+ yb 1)
     (cue :bloom-link-strip-tol
              (fn [vm]
                (let [fraction (lfo-param vm "sawtooth")]
                 (apply-vm vm bloom-tol strips-tubes :fraction fraction :color (create-color :black)  :measure default-measure)))
              :variables (cue-vars :beats :cycles :max-num :min-num :down)
     :priority 1999, :color :black))

; }}}
; CONFETTI {{{
    (set-cue!
      (+ xb 5) (+ yb 2)
      (cue :confetti
                (fn [vm]
                  (let [step-ratio (ratio-param vm)
                        step (build-step-param :interval-ratio step-ratio)]
                    (apply-vm vm confetti-tol (all-fixtures) :step step :aim? true)))
                :variables [{:key "beats" :min 1 :max 8 :start 2 :type :integer :name "Beats"}
                            {:key "cycles" :min 1 :max 8 :start 1 :type :integer :name "Cycles"}
                            {:key "min-added" :min 1 :max 2 :start 1 :type :integer :name "Min Add"}
                            {:key "max-added" :min 1 :max 2 :start 4 :type :integer :name "Max Add"}
                            {:key "min-dur" :min 1 :max 16 :start 1 :type :integer :name "Min Last"}
                            {:key "max-dur" :min 1 :max 16 :start 4 :type :integer :name "Max Last"}
                            {:key "min-sat" :min 0 :max 100 :start 30 :name "Min Sat"}
                            {:key "max-sat" :min 0 :max 100 :start 70 :name "Max Sat"}
                            {:key "min-hue" :min 0 :max 360 :start 210 :name "Min Hue"}
                            {:key "max-hue" :min 0 :max 360 :start 260 :name "Max Hue"}]
                :color :red :priority 10))
; }}}
; {{{           PINSTRIPES

    (let [[color-1 color-2] (map #(make-var [(str "color-" %1) (color-like %2)])
                                 [1 2] ["coral1" "aquamarine"])]
     (set-cue!  (+ xb 4) yb
     (cue :pinstripes
               (fn [vm]
                 (let [colors [(:color-1 vm) (:color-2 vm)]]
                   (effect (fun/pinstripes (all-fixtures) :step (step-param vm) :colors colors) (fraction-param vm :alpha))))
               :variables (cue-vars :beats :cycles (make-var ["fade" 0.0 0.0 1.0])
                                    (make-var ["tolerance" 0.1 0 1])
                                    color-1 color-2 :alpha)
               :color-fn (fn [cue active show snapshot]
                     (let [vm (:variables active)]
                      (if (> (snapshot-bar-phase snapshot 0.5) 0.5)
                       (:start color-1) #_(get-variable (:color-1 vm))
                       (:start color-2) #_(get-variable (:color-2 vm))))))))
                         ;;
                         ;; ))))))

    (set-cue! (+ xb 4) (inc yb)
              (cue :pinstripes
                   (fn [vm]
                          (let [fixtures (clojure.set/difference (set (all-fixtures)) (set (fixtures-named :moving)))
                                colors [(:color-1 vm) (:color-2 vm) (:color-3 vm)]]
                            (fun/pinstripes fixtures :step (step-param vm) :colors colors)))
                        :variables (cue-vars :beats :cycles
                                             (map #(make-var [(str "color-" %1) (color-like %2)])
                                                  [1 2 3] ["indianred" "khaki" "cadetblue"])
                                             (make-var ["fade" 0.0 0 1]))
                        :color :orange :short-name "Pin 3"))

    (set-cue! (+ xb 4) (+ 2 yb)
              (cue :pinstripes
                             (fn [vm]
                               (let [fixtures (clojure.set/difference (set (all-fixtures)) (set (fixtures-named :strip)))
                                     colors [(:color-1 vm) (:color-2 vm) (:color-3 vm) (:color-4 vm)]]
                                 (fun/pinstripes fixtures :step (step-param vm) :colors colors)))
                             :variables (cue-vars :beats :cycles
                                                  (map #(make-var [(str "color-" %1) (color-like %2)])
                                                       [1 2 3 4] ["lightpink1" "orchid2" "white" "lightsalmon1"])
                                                  (make-var ["fade" 0.0 0 1]))
                             :color :orange :short-name "Pin 4"))))
; }}}
   ; }}}

(defn chase-fn
 [vm & {:keys [lfo-type pad-between?]}]
 (chase "Chase osc"
  [(fx/blank)
   (global-color-effect :black)
   ;; (fun/strobe-2 :strobe (fixtures-named "moving-mini") 80 50)
   ;; (fun/bloom (all-fixtures)) ; :fraction (rand 1.0))
   (fx/blank)
   (global-dimmer-effect :black)
   (global-color-effect :seagreen :fixtures (fixtures-named "wash"))
   (global-color-effect :black)
   (fx/blank)
   (global-color-effect :black)]
  (lfo-param vm "sine")
 #_(build-oscillated-param (sine :interval-ratio (ratio-param var-map) :phase (:phase var-map)) :min (:min var-map) :max (:max var-map))))


;; {{{ EFFECT CUES 2
(defn make-effect-cues-2
  [& {:keys [page-x page-y] :or {page-x 0 page-y 0}}]
  (let [xb (* page-x 8) ; {{{
        yb (* page-y 8)
        rig-width (- right-wall left-wall)

        blank (fx/blank)
        blackout (global-color-effect :black)
        desat-beat  (build-oscillated-param  (sawtooth :down? true) :max 100)  ; Desaturate a color as a beat progresses
        desat-bar   (build-oscillated-param  (sawtooth :down? true) :max 80)  ; Desaturate a color as a bar progresses
        hue-bar     (build-oscillated-param  (sawtooth :interval :bar) :min 200 :max 260)  ; Spread a gradient across a bar of music
        hue-grad    (build-spatial-param  (all-fixtures) (fn [head] (- (:x head) (:min-x @(:dimensions *show*)))) :min 200 :max 260) ; Spread a gradient across the light grid
        rig-hue-gradient (build-spatial-param (all-fixtures) ; Spread a rainbow across just the main rig, repeating beyond that, irrespective of other lights' positions.
                                              (fn [head] (colors/clamp-hue (* 360 (/ (- (:x head) left-wall) rig-width)))))]
        ;; rig-sat-gradient (build-spatial-param (all-fixtures)
        ;;                                       (fn [head] (colors/darken (:color head) (/ (- (:x head) left-wall) rig-width))))]
         ; }}}

  ;; rainbow bullshit{{{
   (set-variable! :rainbow-sat 30)
   (set-cue! (+ xb 2) (+ yb 0)
                  (let [color-param (build-color-param :s :rainbow-sat, :l 50, :h hue-bar)]
                    (cue :all-color (fn [_] (global-color-effect color-param))
                              :color-fn (cues/color-fn-from-param color-param)
                              :short-name "Rainbow Bar Fade"
                              :variables [{:key :rainbow-sat, :name "Saturation", :min 0, :max 100, :start 30, :type :integer}])))
   (set-cue! (+ xb 2) (+ yb 1)
                   (cue :all-color (fn [_] (global-color-effect
                                                  (build-color-param :s :rainbow-sat, :l 50, :h rig-hue-gradient) :include-color-wheels? true))
                             :short-name "Rainbow Rig"
                             :variables [{:key :rainbow-sat, :name "Saturation", :min 0, :max 100, :start 30, :type :integer}]))
   (set-cue! (+ xb 2) (+ yb 2)
                   (let [color-param (build-color-param :s :rainbow-sat, :l 50, :h hue-grad, :adjust-hue hue-bar)]
                     (cue :all-color (fn [_] (global-color-effect color-param))
                               :color-fn (cues/color-fn-from-param color-param)
                               :short-name "Rainbow Grid+Bar"
                               :variables [{:key :rainbow-sat, :name "Saturation", :min 0, :max 100, :start 30, :type :integer}])))
   (set-cue! (+ xb 2) (+ yb 3)
                  (let [color-param (build-color-param :s desat-bar, :l 80, :h hue-grad, :adjust-hue hue-bar)]
                    (cue :all-color (fn [_] (global-color-effect color-param))
                               :priority 0, :color-fn (cues/color-fn-from-param color-param), :short-name "Rainbow Pulse")))
; }}}

  ; {{{   CHASES
  ;; expose step-param fade-fraction in mapping to can adjust curve on fly?
   (reset! step-beat (build-step-param :fade-fraction 0.20, :fade-curve :sine))  ;; Set up an initial value for our step parameter
   (reset! step-bar (build-step-param :interval :bar :fade-fraction 0.125, :fade-curve :sine))  ;; Set up an initial value for our step parameter
   (reset! step-phrase (build-step-param :interval :phrase :fade-fraction 0.03125, :fade-curve :sine))  ;; Set up an initial value for our step parameter
   ; XXX test using fun/random-beat-number-param as step param, would jump to a random pos in chase each beat?
  ;; (set-cue! (+ xb 4) yb (code-cue :chase-control))

   (set-cue! (+ xb 4) (+ yb 1)
     (cue :chase (fn [var-map]
                   (let [step (build-step-param :interval-ratio (ratio-param var-map) :fade-fraction (:fade var-map) :fade-curve :sine)]
                     (chase "Chase Test 1" ;;XXX other step param whose index looks up which effect fn to call from map, so evolves without needing 64 lines long chase...
                                           ; instead many chases of 1-4 indexes continously triggering same effect with diff values, others updating what will be called etc
                               ;;also wrapper "(get-random-effect :min-intensity i :max-intensity j)"
                               ;;later, weight it so on start of bar, phrase, etc, higher chanse of crazy fx, midpoint/low energy reported by analyzer, likelier blank or black or kill running shit...
                               [(fun/strobe-2        :strobe (fixtures-named "moving-mini") 87 60) ;; XXX random numbers (or global vars, with offsets) for fx fn params
                                (global-color-effect :black)
                                (fun/strobe-2        :strobe (fixtures-named "wash") 97 80)
                                (global-color-effect :royalblue :fixtures (fixtures-named "moving-mini")) ;;XXX static
                               ;; (global-color-effect :black)
                                (fun/strobe-2        :strobe (fixtures-named "moving-beam") 99 100)
                                (global-color-effect :lightgoldenrodyellow :fixtures (fixtures-named "wash"))
                               ;; (global-color-effect :black)
                                (fun/strobe-2        :strobe (fixtures-named "moving") 70 100)
                                (fx/blank)]
                                ;; (scene "black'n'reset" [(global-color-effect :black)
                                    ;; (reset! step-beat (build-step-param :fade-curve :sine :fade-fraction (rand 0.5)))])
                               step :beyond :loop)))
                               ;; step)))
          :color :magenta :priority 1000
          :variables [{:key "beats" :name "Beats" :min 1 :max 32 :type :integer :start 16}
                      {:key "cycles" :name "Cycles" :min 1 :max 16 :type :integer :start 3}
                      {:key "fade", :min 0.0, :max 1.0, :start 0.3}]))


   (set-cue! (+ xb 4) (+ yb 2)
                  (cue :chase (fn [var-map]
                                     (chase "Chase Test 2"
                                               [(fun/strobe-2        :strobe (fixtures-named "moving") 77 70)
                                                blackout
                                                (fun/strobe-2        :strobe (fixtures-named "moving-mini") 82 50)
                                                blackout
                                                (fun/strobe-2        :strobe (fixtures-named "wash") 97 70)
                                                blackout
                                                (fun/strobe-2        :strobe (fixtures-named "moving") 60 99)
                                                (global-color-effect :black :fixtures (fixtures-named "moving"))]
                                               @step-beat :beyond :bounce))
                            :color :magenta :priority 10000))

   (set-cue! (+ xb 4) (+ yb 3); {{{
                  (cue :chase (fn [var-map]
                                     (chase "Chase Test 3"
                                            [(fun/strobe-2        :strobe (fixtures-named "moving") 80 50)
                                             blackout
                                             blank
                                             blackout

                                             (fun/strobe-2        :strobe (fixtures-named "wash") 97 100)
                                             (global-color-effect :royalblue :fixtures (fixtures-named "moving-mini"))
                                             (fun/strobe-2        :strobe (fixtures-named "moving-beam") 47 100)
                                             blackout

                                             (global-color-effect (create-color :h 270 :s 30 :l 70) :fixtures (fixtures-named "moving-mini"))
                                             blackout
                                             (global-color-effect :seagreen :fixtures (fixtures-named "wash"))
                                             blackout

                                             (fun/strobe-2        :strobe (fixtures-named "moving-mini") 70 100)
                                             blackout
                                             (fun/strobe-2        :strobe (fixtures-named "moving") 90 100)
                                             blackout]
                                             @step-bar :beyond :blank))
                       :color :magenta :priority 20000)) ; }}}

   ;XXX set beats to global var, use on-var-update fn to actually modify var to closest 12346816. Might cause issues with knobs unless defer tho
   (let [strobe-osc (build-oscillated-param (sawtooth :interval :beat) :min 0 :max 100)]
    (set-cue! (+ xb 4) (+ yb 4)
     (cue :chase-osc
            (fn [var-map] (chase "Chase osc"
                                 [blank
                                  (global-color-effect (color-like "lightgreen"  :h 100))
                                  ;; (fun/strobe-2 :strobe (fixtures-named "tube") (resolve-param strobe-osc *show* (metro-snapshot (:metronome *show*))) 100) ;(+ 60 (* 30 (rand)))
                                  (fun/strobe-2 :strobe (fixtures-named "tube") strobe-osc 100) ;(+ 60 (* 30 (rand)))
                                  blackout
                                  (fun/bloom (all-fixtures) :measure default-measure :color (color-like "orange" :h 200) :fraction (+ 0.15 (rand 0.7)))
                                  blank
                                  blank
                                  (fun/sparkle (all-fixtures))]
                            (lfo-param var-map "sine")))
            :color :yellow :priority 10000
            :variables (use-vars [:beats :cycles :phase] (make-var ["min" 1.0 0.0 8.0]), (make-var ["max" 8.0 0.0 8.0])))))

    (set-cue! (+ xb 0) (+ yb 0)
         (cue :chase-random
                (fn [var-map] (chase "Chase rand"
                                     [blank
                                      (do (avar :glo-green (color-like "lightgreen")) (global-color-effect (avar :glo-green)))
                                      (fun/strobe-2 :strobe (fixtures-named "tube") (+ 60 (* 30 (rand))) 100)
                                      blackout
                                      (fun/bloom (all-fixtures) :measure default-measure :color (color-like "orange") :fraction (rand 1.0))
                                      blank
                                      blank
                                      (fun/sparkle (all-fixtures))]
                                (lfo-param var-map "sine")))
                :color :yellow :priority 10000
                :variables (use-vars [:beats :cycles :phase] (make-var ["min" 1.0 0.0 8.0]), (make-var ["max" 8.0 0.0 8.0]))))

   (set-cue! (+ xb 4) (+ yb 5)
     (cue :chase-osc chase-fn
            :color :orange :priority 20000
            ;; :variables [{:key "min", :min 0.0, :max 8.0, :start 0.0, :name "Min"}
            :variables (use-vars [:beats :cycles :phase] (make-var ["min" 0.0 0.0 8.0]), (make-var ["max" 8.0 0.0 8.0]))))

   (set-cue! (+ xb 4) (+ yb 6)
     (cue :chase-fade (fn [var-map] (let [step (step-param var-map)]
        (chase "blanker-blacker"
         [blank (global-dimmer-effect 0 :add-virtual-dimmers? true)
          blank blackout
          blank blackout
          blank blackout]
         step :beyond :loop)))
          :color :orange :priority 30000
          :variables [{:key "beats", :min 1, :max 32, :type :integer, :start 4, :name "Beats"},
                      {:key "cycles", :min 1, :max 8, :type :integer, :start 1, :name "Cycles"}
                      {:key "sine-curve", :start true, :type :boolean :name "Sine-fade?"}
                      {:key "fade", :min 0 :max 1 :start 0.5, :name "Fade"}]))

   ;; (set-cue! (+ xb 4) (+ yb 5)
   ;;                (cue :chase (fn [var-map]
   ;;                                   (chase "Chase Test cues"
   ;;                                             []
   ;;                                             ;; [(cues/run-cue-or-something yo yeah)]
   ;;                                             @step-beat :beyond :loop))
   ;;                     :color :purple, :priority 300)))); }}}
                    ))
; }}}

; {{{ MOVEMENT CUES
;
(defn dimmer-sweep-cue "Create cue for dimmer sweep effect"
 [group x y lfo-type ]
 )


(defn make-movement-cues "Create a page of with some large scale and layered movement effects. And miscellany which I'm not totally sure what to do with yet." ; {{{
  [& {:keys [page-x page-y] :or {page-x 0 page-y 0}}]
  (let [xb (* page-x 8)
        yb (* page-y 8)]

    ;; (let [dimmer-sweep-fixtures (map fixtures-named [:moving :moving-mini :moving-beam])]
    (set-cue! (+ xb 0) (+ yb 0)
              (cue :sweep-dimmers
                   (fn [var-map] (apply-vm
                                  var-map dimmer-sweep (all-fixtures)
                                  (sawtooth :down? (:down var-map) :interval-ratio (ratio-param var-map))))
                   :color :red :short-name "Sawtooth Sweep"
                   :variables [{:key "beats" :min 1 :max 32 :type :integer :start 2 :name "Beats"}
                               {:key "cycles" :min 1 :max 8 :type :integer :start 1 :name "Cycles"}
                               {:key "down" :type :boolean :start true :name "Down?"}
                               {:key "width" :min 0 :max 1 :start 0.1 :name "Width"}
                               {:key "level" :min 0 :max 255 :start 255 :name "Level"}
                               {:key "fade" :type :boolean :start false :name "Fade?"}]))

    (set-cue! (+ xb 0) (+ yb 1)
              (cue :sweep-dimmers
                   (fn [var-map] (apply-vm var-map dimmer-sweep (all-fixtures)
                                  (triangle :interval-ratio (ratio-param var-map))))
                   :color :red :short-name "Triangle Sweep"
                   :variables [{:key "beats" :min 1 :max 32 :type :integer :start 2 :name "Beats"}
                               {:key "cycles" :min 1 :max 8 :type :integer :start 1 :name "Cycles"}
                               {:key "width" :min 0 :max 1 :start 0.25 :name "Width"}
                               {:key "level" :min 0 :max 255 :start 255 :name "Level"}
                               {:key "fade" :type :boolean :start false :name "Fade?"}]))

    (set-cue! (+ xb 1) (+ yb 0)
              (cue :sweep-moving-dimmers
                   (fn [var-map] (apply-vm var-map dimmer-sweep (fixtures-named :moving)
                                  (sawtooth :down? (:down var-map) :interval-ratio (ratio-param var-map))))
                   :color :red :short-name "Moing Saw Sweep"
                   :variables [{:key "beats" :min 1 :max 32 :type :integer :start 2 :name "Beats"}
                               {:key "down" :type :boolean :start true :name "Down?"}
                               {:key "cycles" :min 1 :max 10 :type :integer :start 1 :name "Cycles"}
                               {:key "width" :min 0 :max 1 :start 0.1 :name "Width"}
                               {:key "level" :min 0 :max 255 :start 255 :name "Level"}
                               {:key "fade" :type :boolean :start false :name "Fade?"}]))

    (set-cue! (+ xb 1) (+ yb 1)
                   (cue :sweep-moving-dimmers
                             (fn [var-map] (apply-vm
                                            var-map dimmer-sweep (fixtures-named :moving)
                                            (triangle :interval-ratio (ratio-param var-map))))
                               :color :red :short-name "Moving Triangle Sweep"
                               :variables [{:key "beats" :min 1 :max 32 :type :integer :start 4 :name "Beats"}
                                           {:key "cycles" :min 1 :max 8 :type :integer :start 1 :name "Cycles"}
                                           {:key "width" :min 0 :max 1 :start 0.25 :name "Width"}
                                           {:key "level" :min 0 :max 255 :start 255 :name "Level"}
                                           {:key "fade" :type :boolean :start false :name "Fade?"}]))

    (set-cue! (+ xb 2) (+ yb 0)
                   (cue :sweep-moving-mini-dimmers
                             (fn [var-map] (apply-vm var-map dimmer-sweep  (fixtures-named :moving-mini)
                                            (sawtooth :down? (:down var-map) :interval-ratio (ratio-param var-map))))
                             :color :red :short-name "Moing-Mini Saw Sweep"
                             :variables [{:key "beats" :min 1 :max 32 :type :integer :start 2 :name "Beats"}
                                         {:key "down" :type :boolean :start true :name "Down?"}
                                         {:key "cycles" :min 1 :max 8 :type :integer :start 1 :name "Cycles"}
                                         {:key "width" :min 0 :max 1 :start 0.1 :name "Width"}
                                         {:key "level" :min 0 :max 255 :start 255 :name "Level"}
                                         {:key "fade" :type :boolean :start false :name "Fade?"}]))

    (set-cue! (+ xb 2) (+ yb 1)
                   (cue :sweep-moving-mini-dimmers
                             (fn [var-map] (apply-vm
                                            var-map dimmer-sweep  (fixtures-named :moving-mini)
                                            (triangle :interval-ratio (ratio-param var-map))))
                               :color :red :short-name "Moving-mini Triangle Sweep"
                               :variables [{:key "beats" :min 1 :max 32 :type :integer :start 4 :name "Beats"}
                                           {:key "cycles" :min 1 :max 8 :type :integer :start 1 :name "Cycles"}
                                           {:key "width" :min 0 :max 1 :start 0.25 :name "Width"}
                                           {:key "level" :min 0 :max 255 :start 255 :name "Level"}
                                           {:key "fade" :type :boolean :start false :name "Fade?"}]))



    (set-cue! (+ xb 1) (+ 2 yb)
              (cue :moving-mini-dimmers
                (fn [var-map]
                  (let [step (build-step-param :interval-ratio (ratio-param var-map) :fade-fraction (:fade-fraction var-map))]
                    (chase "Moving Mini Cross"
                              (map #(dimmer-effect (:level var-map) (fixtures-named %))
                                   [:moving-mini-1 :moving-mini-2 :moving-mini-3])
                              step :beyond :loop)))
                  :color :red :short-name "Moving Cross"
                  :variables [{:key "beats" :min 1 :max 32 :type :integer :start 1 :name "Beats"}
                              {:key "cycles" :min 1 :max 10 :type :integer :start 4 :name "Cycles"}
                              {:key "level" :min 0 :max 255 :start 255 :name "Level"}
                              {:key "fade-fraction" :min 0 :max 1 :start 0 :name "Fade"}]))

    (set-cue! (+ xb 3) (inc yb)
      (cue :movement (fn [var-map]
                            (apply-vm var-map fun/aim-fan
                                                        (concat (fixtures-named :moving) (fixtures-named :moving-mini)))) ;(map fixtures-named :moving :moving-mini)))
                                                        ;; (map #(fixtures-named %) ["moving" "moving-mini"]))) ;(map fixtures-named :moving :moving-mini)))
                :variables [{:key "x-scale" :min -5 :max 5 :start 1 :name "X Scale"}
                            {:key "y-scale" :min -10 :max 10 :start 5 :name "Y Scale"}
                            {:key "z" :min 0 :max 20 :start 4}
                            {:key "y" :min -10 :max 10 :start rig-height}
                            {:key "x" :min -10 :max 10 :start 0.0}]
                :color :blue, :end-keys [:move-moving]))

    (set-cue! (+ xb 3) (+ yb 2)
                   (cue :movement (fn [var-map]
                                         (apply-vm var-map fun/twirl
                                                                     (concat (fixtures-named :moving) (fixtures-named :moving-mini)))) ;(map fixtures-named :moving :moving-mini)))
                             :variables [{:key "beats" :min 1 :max 32 :type :integer :start 8 :name "Beats"}
                                         {:key "cycles" :min 1 :max 10 :type :integer :start 1 :name "Cycles"}
                                         {:key "radius" :min 0 :max 10 :start 0.25 :name "Radius"}
                                         {:key "z" :min -10 :max 10 :start -1.0}
                                         {:key "y" :min -10 :max 10 :start rig-height}
                                         {:key "x" :min -10 :max 10 :start 0.0}]
                             :color :green, :end-keys [:move-moving]))

    (set-cue! (+ xb 3) (+ yb 3)
                   (cue :move-all
                             (fn [var-map] (apply-vm var-map can-can))
                             :variables [{:key "bars" :name "Bars" :min 1 :max 8 :type :integer :start 1}
                                         {:key "cycles" :name "Cycles" :min 1 :max 8 :type :integer :start 1}
                                         {:key "stagger" :name "Stagger" :min 0 :max 4 :start 0.5}
                                         {:key "spread" :name "Spread" :min -45 :max 45 :centered true :resolution 0.25 :start 0}
                                         {:key "pan-min" :name "Pan min" :min -180 :max 180 :centered true :resolution 0.5 :start 0}
                                         {:key "pan-max" :name "Pan max" :min -180 :max 180 :centered true :resolution 0.5 :start 0}
                                         {:key "tilt-min" :name "Tilt min" :min -180 :max 180 :centered true :resolution 0.5 :start -60}
                                         {:key "tilt-max" :name "Tilt max" :min -180 :max 180 :centered true :resolution 0.5 :start 100}]
                             :color :yellow :end-keys [:movement]))

    (set-cue! (+ xb 3) (+ yb 4)
                   (cue :moving-circles
                             (fn [var-map] (apply-vm
                                            ;; var-map circle-chain (concat (fixtures-named :strip) (fixtures-named :tube)) true))
                                            var-map circle-chain (concat (fixtures-named :moving) (fixtures-named :moving-mini)) true))
                                            ;; var-map circle-chain moving-heads false))
                             :variables [{:key "bars" :name "Bars" :min 1 :max 8 :type :integer :start 2}
                                         {:key "radius" :name "Radius" :min 0.1 :max 2 :resolution 0.1 :start 1.0}
                                         {:key "stagger" :name "Stagger" :min 0 :max 2 :start 0 :resolution 0.1}]
                             :short-name "Moving Circles" :color :green :priority 4))

    ;; ;; A chase which overlays on other movement cues, gradually taking over the lights
    (set-cue! (+ xb 3) (+ yb 5)
                   (cue :crossover (fn [var-map] (apply-vm var-map crossover-chase))
                             :variables (cue-vars :beats :fade-fraction (make-vars ["cross-color" "orangered"] ["end-color" "seagreen"])) #_[ {:key "beats" :min 1 :max 8 :start 2 :type :integer :name "Beats"}
                                         {:key "fade-fraction" :min 0 :max 1 :start 0 :name "Fade"}
                                         {:key "cross-color" :type :color :start (create-color :orangered) :name "X Color"}
                                         {:key "end-color" :type :color :start (create-color :seagreen) :name "End Color"}]
                             :color :cyan :priority 5))

    (let [triangle-phrase (build-oscillated-param (triangle :interval :phrase) :min -90 :max 90) ; Move back and forth over a phrase
          staggered-triangle-bar (build-spatial-param (all-fixtures) ; Bounce over a bar, staggered across grid x
                                  (fn [head]
                                   (build-oscillated-param (triangle :interval :bar :phase (x-phase head *show*)) :min -90 :max 0)))
                                   ;; (build-oscillated-param (triangle :interval :bar :phase (space-phase head *show*)) :min -90 :max 0)))
        can-can-dir (params/build-direction-param-from-pan-tilt :pan triangle-phrase :tilt staggered-triangle-bar)
        can-can-p-t (params/build-pan-tilt-param :pan triangle-phrase :tilt staggered-triangle-bar)]
    (set-cue! (+ xb 4) (+ yb 0) (cue :movement (fn [_] (move/direction-effect "Can Can" can-can-dir (all-fixtures)))))
    (set-cue! (+ xb 4) (+ yb 1) (cue :movement (fn [_] (move/pan-tilt-effect "P/T Can Can" can-can-p-t (all-fixtures))))))

  (set-cue! (+ xb 5) (+ yb 0) (function-cue :moving-speed :movement-speed (concat (fixtures-named "moving") (fixtures-named "moving-mini"))
                                        :color :purple :effect-name "Slow Moving Heads"))
  (set-cue! (+ xb 5) (+ yb 1) (function-cue :moving-speed :movement-speed (concat (fixtures-named "moving") (fixtures-named "moving-mini"))
                                        :color :purple :effect-name "Moving head motor speed osc")) ;; HAJHATT XXX osc (sawtooth?)



    (def color-cycle (map create-color ["mediumpurple" "seagreen" "darkblue" "black"]))
    ;; Some color cycle chases
    (let [color-cycle (map create-color ["mediumpurple" "seagreen" "darkblue" "black"])]
     (set-cue! xb (+ yb 7)
       (cue :all-color (fn [_] (fun/iris-out-color-cycle-chase
                                      (all-fixtures), :color-cycle color-cycle
                                      :color-index-function snapshot-beat-within-bar))))
     (set-cue! (inc xb) (+ yb 7)
       (cue :all-color (fn [_] (fun/wipe-right-color-cycle-chase
                                      (all-fixtures), :color-cycle color-cycle
                                     :transition-phase-function snapshot-bar-phase))))
     (set-cue! (+ xb 2) (+ yb 7)
       (cue :all-color (fn [_] (fun/wipe-right-color-cycle-chase
                                     (all-fixtures), :color-cycle color-cycle
                                      :color-index-function snapshot-beat-phase
                                      :transition-phase-function snapshot-beat-phase
                                      :effect-name "Wipe Right Beat")))))))
; }}}
; }}}

 ; {{{  AIM CUES
; {{{ HELPERS
(defn- aim-cue-var-key "Determine the cue variable key value to use for a variable being created for an aim cue page cue. `base-name` is the name that will be used for the variable if it is not part of a group of cues sharing variables; if `shared-prefix` is not blank then the variable key will refer to a show variable with that prefix identifying which group it belongs to."
  [base-name shared-prefix]
  (if (clojure.string/blank? shared-prefix)
    (name base-name)
    (keyword (str "aim-group-" shared-prefix "-" (name base-name)))))

(defn- build-aim-cue "Build an aim cue for the mutiplexable fixture aiming page."
  [fixture-key shared-prefix transform? color]
  (let [isolated? (clojure.string/blank? shared-prefix)]
    (cue (keyword (str "aim-" (name fixture-key)))
              (fn [var-map]
                (let [base-aim (if isolated?
                                 (apply-merging-var-map var-map build-aim-param)
                                 (build-aim-param :x (aim-cue-var-key "x" shared-prefix)
                                                  :y (aim-cue-var-key "y" shared-prefix)
                                                  :z (aim-cue-var-key "z" shared-prefix)))
                      aim-param (if transform?
                                  (params/build-aim-transformer base-aim (keyword (str "aim-group-" shared-prefix "-transform")))
                                  base-aim)]
                  (move/aim-effect (str "Aim " (name fixture-key)
                                        (when-not isolated?
                                          (str " (Group " (clojure.string/upper-case shared-prefix)
                                               (when transform? " flip") ")")))
                                   aim-param (fixtures-named fixture-key))))
              :variables [(merge {:key (aim-cue-var-key "x" shared-prefix) :name "X" :min -10.0 :max 10.0
                                  :centered true :resolution 0.05} (when isolated? {:start 0.0}))
                          (merge {:key (aim-cue-var-key "y" shared-prefix) :name "Y" :min -10.0 :max 10.0
                                  :centered true :resolution 0.05} (when isolated? {:start 0.0}))
                          (merge {:key (aim-cue-var-key "z" shared-prefix) :name "Z" :min -10.0 :max 10.0
                                  :centered true :resolution 0.05} (when isolated? {:start 2.0}))]
              :color color :priority 1))); }}}

(defn- make-main-aim-cues "Create a page of cues for aiming lights in particular points, individually and in groups."
  [& {:keys [page-x page-y fixtures] :or {page-x 0 page-y 0 fixtures nil}}]
  (let [xb (* page-x 8)
        yb (* page-y 8)
        fixtures (or fixtures moving-heads)
        transform (Transform3D.)
        width (- right-wall left-wall)
        depth (- house-rear-wall stage-wall)]

    ;; Set up default shared aiming coordinates
    (set-variable! :aim-group-a-x 0.0)
    (set-variable! :aim-group-a-y 0.0)
    (set-variable! :aim-group-a-z 2.0)
    (set-variable! :aim-group-b-x 0.0)
    (set-variable! :aim-group-b-y 0.0)
    (set-variable! :aim-group-b-z 2.0)

    ;; Set up OSC bindings so Touch OSC can control these cues in a powerful way with XY pads.
    (add-osc-var-binding
     :aim-group-a-x "/1/aim-a"
     :send-fn (fn [_ v] (osc-send @osc-client "/1/aim-a"
                                           (/ (- v left-wall) width)
                                           (/ (- (get-variable :aim-group-a-z) stage-wall) depth)))
     :receive-fn (fn [msg]
                   (set-variable! :aim-group-a-x (+ left-wall (* width (first (:args msg)))))
                   (set-variable! :aim-group-a-z (+ stage-wall (* depth (second (:args msg)))))))
    (add-osc-var-binding
     :aim-group-a-z "/1/aim-a"
     :send-fn (fn [_ v] (osc-send @osc-client "/1/aim-a"
                                           (/ (- (get-variable :aim-group-a-x) left-wall) width)
                                           (/ (- v stage-wall) depth)))
     :receive-fn :none)

    (add-osc-var-binding
     :aim-group-a-y "/1/aim-a-y"
     :send-fn (fn [_ v] (osc-send @osc-client "/1/aim-a-y" (/ v ceiling)))
     :receive-fn (fn [msg] (set-variable! :aim-group-a-y (* ceiling (first (:args msg))))))

    (add-osc-var-binding
     :aim-group-b-x "/1/aim-b"
     :send-fn (fn [_ v] (osc-send @osc-client "/1/aim-b"
                                           (/ (- v left-wall) width)
                                           (/ (- (get-variable :aim-group-b-z) stage-wall) depth)))
     :receive-fn (fn [msg] (set-variable! :aim-group-b-x (+ left-wall (* width (first (:args msg)))))
                           (set-variable! :aim-group-b-z (+ stage-wall (* depth (second (:args msg)))))))
    (add-osc-var-binding
     :aim-group-b-z "/1/aim-b"
     :send-fn (fn [_ v] (osc-send @osc-client "/1/aim-b"
                                           (/ (- (get-variable :aim-group-b-x) left-wall) width)
                                           (/ (- v stage-wall) depth)))
     :receive-fn :none)

    (add-osc-var-binding
     :aim-group-b-y "/1/aim-b-y"
     :send-fn (fn [_ v] (osc-send @osc-client "/1/aim-b-y" (/ v ceiling)))
     :receive-fn (fn [msg] (set-variable! :aim-group-b-y (* ceiling (first (:args msg))))))


    (.setScale transform (Vector3d. -1.0 1.0 1.0)) ;; Set up default transformation of a reflection over the Y axis
    (set-variable! :aim-group-a-transform transform) (set-variable! :aim-group-b-transform transform)

    (loop [fixtures fixtures
           index 0]
      (when (seq fixtures)
        (let [fixture (first fixtures)]
          ;; Disconnected individual aim cues
          (set-cue! (+ xb index) yb (build-aim-cue fixture nil false :white))
          ;; Group A untransformed aim cues
          (set-cue! (+ xb index) (inc yb) (build-aim-cue fixture "a" false :red))
          (add-osc-cue-binding (+ xb index) (inc yb) (str "/1/aim-" (name fixture) "-a"))
          ;; Group A transformed aim cues
          (set-cue! (+ xb index) (+ yb 2) (build-aim-cue fixture "a" true :orange))
          (add-osc-cue-binding (+ xb index) (+ yb 2) (str "/1/flip-" (name fixture) "-a"))

          ;; Group B untransformed aim cues
          (set-cue! (+ xb index) (+ yb 3) (build-aim-cue fixture "b" false :blue))
          (add-osc-cue-binding (+ xb index) (+ yb 3) (str "/1/aim-" (name fixture) "-b"))
          ;; Group B transformed aim cues
          (set-cue! (+ xb index) (+ yb 4) (build-aim-cue fixture "b" true :cyan))
          (add-osc-cue-binding (+ xb index) (+ yb 4) (str "/1/flip-" (name fixture) "-b")))
        (recur (rest fixtures) (inc index))))

    ;; Transformation modifiers for group A
    (set-cue! (+ xb 7) (inc yb)
     (cue :aim-group-a-transform
               (fn [_]
                 (let [transform (Transform3D.)]
                   (.setScale transform (Vector3d. 1.0 -1.0 1.0))
                   (variable-effect @var-binder :aim-group-a-transform transform)))
               :color :cyan :short-name "Group A flip Y"))
    (add-osc-cue-binding (+ xb 7) (inc yb) "/1/flip-a-y")
    (set-cue! (+ xb 7) (+ yb 2)
                   (cue :aim-group-a-transform
                             (fn [_]
                               (let [transform (Transform3D.)]
                                 (.setScale transform (Vector3d. -1.0 -1.0 1.0))
                                 (variable-effect @var-binder :aim-group-a-transform transform)))
                             :color :cyan :short-name "Group A flip XY"))
    (add-osc-cue-binding (+ xb 7) (+ yb 2) "/1/flip-a-xy")

    ;; Transformation modifiers for group B
    (set-cue! (+ xb 7) (+ yb 3)
                   (cue :aim-group-b-transform
                             (fn [_]
                               (let [transform (Transform3D.)]
                                 (.setScale transform (Vector3d. 1.0 -1.0 1.0))
                                 (variable-effect @var-binder :aim-group-b-transform transform)))
                             :color :orange :short-name "Group B flip Y"))
    (add-osc-cue-binding (+ xb 7) (+ yb 3) "/1/flip-b-y")
    (set-cue! (+ xb 7) (+ yb 4)
                   (cue :aim-group-b-transform
                             (fn [_]
                               (let [transform (Transform3D.)]
                                 (.setScale transform (Vector3d. -1.0 -1.0 1.0))
                                 (variable-effect @var-binder :aim-group-b-transform transform)))
                             :color :orange :short-name "Group B flip XY"))
    (add-osc-cue-binding (+ xb 7) (+ yb 4) "/1/flip-b-xy")))
; }}}

 ; {{{  DIRECTION CUES
; {{{   HELPERS
(defn- direction-cue-var-key "Determine the cue variable key value to use for a variable being created for an direction cue page cue. `base-name` is the name that will be used for the variable if it is not part of a group of cues sharing variables; if `shared-prefix` is not blank then the variable key will refer to a show variable with that prefix identifying which group it belongs to."
  [base-name shared-prefix]
  (if (clojure.string/blank? shared-prefix)
    (name base-name)
    (keyword (str "direction-group-" shared-prefix "-" (name base-name)))))

(defn- build-direction-cue "Build a direction cue for the mutiplexable fixture direction page."
  [fixture-key shared-prefix transform? color]
  (let [isolated? (clojure.string/blank? shared-prefix)]
    (cue (keyword (str "dir-" (name fixture-key)))
              (fn [var-map]
                (let [base-direction (if isolated?
                                       (apply-merging-var-map var-map build-direction-param-from-pan-tilt)
                                       (build-direction-param-from-pan-tilt
                                        :pan (direction-cue-var-key "pan" shared-prefix)
                                        :tilt (direction-cue-var-key "tilt" shared-prefix)))
                      direction-param (if transform?
                                        (params/build-direction-transformer
                                         base-direction (keyword (str "direction-group-" shared-prefix "-transform")))
                                        base-direction)]
                  (move/direction-effect (str "P/T " (name fixture-key)
                                              (when-not isolated?
                                                (str " (Group " (clojure.string/upper-case shared-prefix)
                                                     (when transform? " flip") ")")))
                                         direction-param (fixtures-named fixture-key))))
              :variables [(merge {:key (direction-cue-var-key "pan" shared-prefix) :name "Pan" :min -180.0 :max 180.0
                                  :centered true :resolution 0.5}
                                 (when isolated? {:start 0.0}))
                          (merge {:key (direction-cue-var-key "tilt" shared-prefix) :name "Tilt" :min -180.0 :max 180.0
                                  :centered true :resolution 0.5}
                                 (when isolated? {:start 0.0}))]
              :color color :priority 1)))

(defn- build-pan-tilt-osc-cue "Build a raw pan/tilt oscillator cue."  ; {{{
  [fixture-key]
  (cue (keyword (str "p-t-" (name fixture-key)))
            (fn [var-map]
              (let [pan-osc (sine :interval :bar :interval-ratio (:pan-bars var-map) :phase (:pan-phase var-map))
                    pan-param (build-oscillated-param pan-osc :min (:pan-min var-map) :max (:pan-max var-map))
                    tilt-osc (sine :interval :bar :interval-ratio (:tilt-bars var-map) :phase (:tilt-phase var-map))
                    tilt-param (build-oscillated-param tilt-osc :min (:tilt-min var-map) :max (:tilt-max var-map))]
                (fx/scene (str "P/T " (name fixture-key))
                          (chan-fx/channel-effect "pan" pan-param
                           (extract-channels (expand-heads (fixtures-named fixture-key))
                                                  #(= (:type %) :pan)))
                          (chan-fx/channel-effect "tilt" tilt-param
                           (extract-channels (expand-heads (fixtures-named fixture-key))
                                                  #(= (:type %) :tilt))))))
            :variables [{:key "pan-min" :name "Pan min" :min 0 :max 255 :centered true :resolution 0.25 :start 64}
                        {:key "pan-max" :name "Pan max" :min 0 :max 255 :centered true :resolution 0.25 :start 191}
                        {:key "pan-bars" :name "Pan bars" :min 1 :max 16 :type :integer :start 4}
                        {:key "pan-phase" :name "Pan phase" :min 0.0 :max 1.0 :start 0.0}
                        {:key "tilt-min" :name "Tilt min" :min 0 :max 255 :centered true :resolution 0.25 :start 0}
                       {:key "tilt-max" :name "Tilt max" :min 0 :max 255 :centered true :resolution 0.25 :start 127}
                        {:key "tilt-bars" :name "Tilt bars" :min 1 :max 16 :type :integer :start 2}
                        {:key "tilt-phase" :name "Tilt phase" :min 0.0 :max 1.0 :start 0.0}]
            :color :dodgerblue :priority 1))
; }}}

;; (def osc (build-oscillated-param (sine :interval-ratio (ratio-param vm) :phase (:phase vm)) :min (:min vm) :max (:max vm)))
(def sin (sine :interval-ratio (ratio-param vm) :phase (:phase vm)))
;; (value (value osc))
;; (value sin)

;; (chan-fx/function-effect "focus" :focus (value osc) (all-fixtures))
(defn- build-focus-oscillator "Returns a cue which oscillates a fixture's focus between a minimum and minimum value using a sine oscillator with cue variables to adjust the range and the oscillator's parameters."  ; {{{
  [effect-key effect-name fixtures]
  (cue effect-key
       (fn [vm]
        (let [;osc2 (lfo-param vm "sine")
              osc (build-oscillated-param (sine :interval-ratio (ratio-param vm) :phase (:phase vm)) :min (:min vm) :max (:max vm))]
            (chan-fx/function-effect effect-name :focus osc fixtures)))
            ;; (function-effect-std osc :focus :effect-name effect-name :fixtures fixtures)))
            :color :yellow
            :variables (cue-vars (make-vars ["min" 0 0 100] ["max" 100 0 100]) :beats :cycles (cv :phase 0.5))))
; }}}
;}}}

(defn- make-main-direction-cues "Create a page of cues for aiming lights in particular directions, individually and in groups."  ; {{{
  [& {:keys [page-x page-y fixtures] :or {page-x 0 page-y 0 fixtures nil}}]
  (let [xb (* page-x 8)
        yb (* page-y 8)
        fixtures (or fixtures moving-heads)
        transform (Transform3D.)]

    ;; Set up default shared direction coordinates
    (set-variable! :direction-group-a-pan 0.0)
    (set-variable! :direction-group-a-tilt 0.0)
    (set-variable! :direction-group-b-pan 0.0)
    (set-variable! :direction-group-b-tilt 0.0)

    ;; Set up default transformation of a reflection over the Y axis
    (.setScale transform (Vector3d. -1.0 1.0 1.0))
    (set-variable! :direction-group-a-transform transform)
    (set-variable! :direction-group-b-transform transform)

    (loop [fixtures fixtures
           index 0]
      (when (seq fixtures)
        (let [fixture (first fixtures)] ;; XXX diuble loop with y index and lookup map
          (set-cue! (+ xb index) yb (build-direction-cue fixture nil false :white));; Disconnected individual direction cues

          (set-cue! (+ xb index) (inc yb) (build-direction-cue fixture "a" false :blue)) ;; Group A untransformed direction cues
          (set-cue! (+ xb index) (+ yb 2) (build-direction-cue fixture "a" true :cyan)) ;; Group A transformed direction cues

          (set-cue! (+ xb index) (+ yb 3) (build-direction-cue fixture "b" false :red)) ;; Group B untransformed direction cues
          (set-cue! (+ xb index) (+ yb 4) (build-direction-cue fixture "b" true :orange)) ;; Group B transformed direction cues

          (set-cue! (+ xb index) (+ yb 7) (build-pan-tilt-osc-cue fixture))) ;; Raw pan/tilt oscillated cues
        (recur (rest fixtures) (inc index))))

    ;; XXX generalize these ffs
    ;; Transformation modifiers for group A
    (set-cue! (+ xb 7) (inc yb)
                   (cue :direction-group-a-transform
                             (fn [_]
                               (let [transform (Transform3D.)]
                                 (.setScale transform (Vector3d. 1.0 -1.0 1.0))
                                 (variable-effect @var-binder :direction-group-a-transform transform)))
                             :color :cyan :short-name "Group B flip Y"))
    (set-cue! (+ xb 7) (+ yb 2)
                   (cue :direction-group-a-transform
                             (fn [_]
                               (let [transform (Transform3D.)]
                                 (.setScale transform (Vector3d. -1.0 -1.0 1.0))
                                 (variable-effect @var-binder :direction-group-a-transform transform)))
                             :color :cyan :short-name "Group B flip XY"))

    ;; Transformation modifiers for group B
    (set-cue! (+ xb 7) (+ yb 3)
                   (cue :direction-group-b-transform
                             (fn [_]
                               (let [transform (Transform3D.)]
                                 (.setScale transform (Vector3d. 1.0 -1.0 1.0))
                                 (variable-effect @var-binder :direction-group-b-transform transform)))
                             :color :orange :short-name "Group B flip Y"))
    (set-cue! (+ xb 7) (+ yb 4)
                   (cue :direction-group-b-transform
                             (fn [_]
                               (let [transform (Transform3D.)]
                                 (.setScale transform (Vector3d. -1.0 -1.0 1.0))
                                 (variable-effect @var-binder :direction-group-b-transform transform)))
                             :color :orange :short-name "Group B flip XY"))))
; }}}
; }}}


(defn- make-main-modulator-cues "Cue page modulating and transforming existing cues - changing colors by global HSL etc"
 [& {:keys [page-x page-y groups] :or {page-x 0 page-y 0}}]
  (let [xb (* page-x 8)
        yb (* page-y 8)
        groups (if groups groups (concat [nil] light-groups))] ; nil resolves to all fixtures when passed to helper functions

  ;; (set-cue! (+ xb 0) (+ yb 0) (build-focus-oscillator :moving-1-focus "Moving Focus Sine" (show/fixtures-named "moving")))

; {{{ TRANSFORM COLORS
   (set-cue!  (+ xb 3) (+ yb 7) (cue :transform-saturation
              (fn [_] (transform-colors (all-fixtures)
                       :transform-fn (color-fx/build-saturation-transformation
                                      :param (build-oscillated-param (sine :interval :phrase) :max 70 :min 30))))
              :priority 10000))
   (set-cue! (+ xb 3) (+ yb 4) (cue :transform-saturation-saw
                                   (fn [var-map] (transform-colors (all-fixtures)
                                              :transform-fn (sat-transformer :param (lfo-param var-map "sawtooth"))))
                                   :variables (cue-vars :beats :cycles :min-num :max-num)
                                   :priority 10000))

   (set-cue! (+ xb 3) (+ yb 2)
               (cue :transform-saturation-square
                                   (fn [var-map] (transform-colors (all-fixtures)
                                             :transform-fn (sat-transformer
                                                            :param (build-oscillated-param (square :interval-ratio (ratio-param var-map))
                                                                                               :max (:max var-map), :min (:min var-map), :phase (:phase var-map), :width (:width var-map)))))
                                   :variables (cue-vars :beats :cycles
                                                        (make-var ["min" -30 -100 50])
                                                        (make-var ["max"  10  -50 100])
                                                        :width :phase)
                                   :priority 10000))
   (set-cue! (+ xb 3) (+ yb 1)
             (cue :transform-hsl
                  (fn [var-map]
                   (transform-colors (all-fixtures) :transform-fn (color-transformer :h (:hue var-map) :s (:saturation var-map) :l (:lightness var-map))))
                  :variables (cue-vars :hue :saturation :lightness)
                             ;; {:key "blend", :min 0.0, :max 1.0, :start 1.0, :type :double, :name "Blend"}]
                  :priority 30000))
   (set-cue! (+ xb 3) (+ yb 0)
             (cue :transform-hue
                  (fn [var-map]
                   (do #_(println var-map)
                    (transform-colors (all-fixtures) :transform-fn (color-transformer
                                                                   :h (lfo-param var-map "sine")))))
                  :variables (cue-vars :beats :cycles (map #(make-var [%1 %2 -720 720]) ["min" "max"] [-30 30]) :phase)
                  :priority 30000, :color :mediumpurple4))))
;; }}}

(defn- make-test-cues "fuck around"
 [& {:keys [page-x page-y groups] :or {page-x 0 page-y 0}}]
  (let [xb (* page-x 8)
        yb (* page-y 8)
        groups (if groups groups (concat [nil] light-groups))]

    (set-cue! (+ xb 0) (+ yb 0)
              (cue :sweep-dimmers
                   (fn [vm] (apply-vm vm effect-sweep (all-fixtures)
                                  (sawtooth :down? (:down vm) :interval-ratio (ratio-param vm))
                                  :effect-fn dimmer-effect))
                   :color :red :short-name "Sawtooth Sweep"
                  :variables (cue-vars :beats :cycles :down :width :level :fade)))
    (set-cue! (+ xb 1) (+ yb 0)
             (cue :sweep-strobe
                  (fn [vm] (apply-vm vm effect-sweep (all-fixtures)
                                  ;; (lfo-param vm "sawtooth")
                                 (sawtooth :down? (:down vm) :interval-ratio (ratio-param vm))
                                 :effect-fn strobe-effect))
                  :color :red :short-name "Sawtooth Sweep"
                  :variables (cue-vars :beats :cycles :down :width :level :fade)))))


; {{{ VELOCITY CUES
(defn make-velocity-cues "live playing yo"
  [& {:keys [page-x page-y] :or [page-x 0 page-y 0]}]
  (let [xb (* page-x 8)
        yb (* page-y 8) ]

    (color-cue "royalblue" xb yb, :fixtures (all-fixtures), :effect-key :all-color, :effect-name "Color all")
    (doall (map-indexed (fn [i group]
                         (color-cue group "royalblue" (+ xb (inc i)) yb, :include-color-wheels? true
                                         :effect-key (keyword (str (name group) "-color")) :priority 1, :held true
                                         :effect-name (str "Color " (name group))))
                        light-groups)) ))
; }}}

; {{{ COLOR CUES
(defn make-color-cues "colors all over the place"
  [& {:keys [page-x page-y] :or [page-x 0 page-y 0]}]
  (let [xb (* page-x 8)
        yb (* page-y 8) ]

   ;TODO: show var cues setting color (mostly for hue info), held cues with velocity adjusting sat/lightness
   ;some mechanism for fading on/off color over attack/release time, a la strips but within afterglow...
    (color-cue "royalblue" xb yb, :fixtures (all-fixtures), :effect-key :all-color, :effect-name "Color all")
    ;TODO: bind most colorvar-using cues (bloom, sparkle etcetc) to show vars containing functions looking up actual show var to use
    ;then can route by index incl step param, easily randomize, update each beat/bar/phrase etc
    ))
; }}}

(defn- make-channel-control-cues "LFO channel cues, eg fog or LED-strip control channel cues for pixtol"
 [& {:keys [page-x page-y groups lfos] :or {page-x 0 page-y 0}}]
  (let [xb (* page-x 8)
        yb (* page-y 8)
        lfos (or lfos lfo-types)]
   ;; XXX support function-cues in same fn

   (doall (map-indexed
           (fn [x channel]
             (doall (map-indexed
                     (fn [y lfo-type]
                       (lfo-cue nil (+ xb x) (+ yb y) (:type lfo-type) channel-effect ;function-effect
                                :channel-type channel :end-rest? false))
                     lfos)))
           groups))))

#_(defn- make-effect-lfo-cues "LFO effect cues"
 [& {:keys [page-x page-y groups lfos] :or {page-x 0 page-y 0}}]
  (let [xb (* page-x 8)
        yb (* page-y 8)
        groups (or groups (concat [nil] light-groups))
        lfos (or lfos lfo-types)]

   (doall (map-indexed
           (fn [x group]
             (doall (map-indexed
                     (fn [y lfo-type]
                       (lfo-cue group (+ xb x) (+ yb y) (:type lfo-type) strobe-tol #_strobe-effect
                                :end-rest? false))
                     lfos)))
           groups))))

;}}} END INDIVIDUAL CUE PAGE MAKERS

(defn make-cues-tol []
  (when (nil? @osc-client) (reset! osc-client (osc/osc-client osc-address osc-port-send))) ;osc client outputs state of queues
  (clear-osc-cue-bindings)

  ; XXX prob have main page/startup at like 2 2 so plenty space in all directions...
  (make-main-color-dimmer-cues-tol :page-x 0 :page-y 0)
  (make-main-color-dimmer-cues-tol :page-x 1 :page-y 0 :globals false
                                   :groups [:moving-1 :moving-mini-1 :moving-mini-2 :moving-mini-3
                                            :strip-1 :strip-2 :tube-1 :tube-2])
  (make-main-modulator-cues        :page-x 2 :page-y 0)

  (make-effect-cues                :page-x 0 :page-y 1)
  (make-effect-cues-2              :page-x 0 :page-y 3)

  (make-movement-cues              :page-x 1 :page-y 1)
  (make-main-aim-cues              :page-x 2 :page-y 1)
  (make-main-direction-cues        :page-x 3 :page-y 1)
  ;; (make-velocity-cues              :page-x 0 :page-y 2)
  (make-channel-control-cues       :page-x 0 :page-y 2 :groups pixtol-channels)
  (make-channel-control-cues       :page-x 1 :page-y 2 :groups [:fog])
  ;; (make-effect-lfo-cues             :page-x 2 :page-y 2)
  ;; (make-dimmer-sweep-cues) ;just yeh. then width n level as lfo.
  (make-test-cues                  :page-x 0 :page-y 4))

