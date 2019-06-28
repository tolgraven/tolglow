(ns tolglow.config "Settings and functions to apply them" {:author "Joen Tolgraven"}
  (:require [afterglow
             [core :as core]
             [transform :as tf :refer [degrees]]]
            [tolglow
            [fixtures :as fixtures]]))

(def venue "Main truss and ceiling height (Y), distance from it to stage/rear (Z) and left/right (X) walls"
 {:rig 2.00 :ceiling 3.00 :wall {:stage -1.00 :rear 6.00 :left -2.50 :right 2.50}})

(defonce binds ;dirty way to separate, should be auto-generated somehow...
 {:show (atom nil) ;? id or wha?
  :vars (atom nil)
  :ns (atom nil)
  :controller {:push (atom nil) :launchpad (atom nil)}
  :step (reduce #(into %1 {%2 (atom nil)}) {} [:beat :bar :phrase])
  :fixture-type {:moving (atom []), :strip (atom [])}
  :osc {:server core/osc-server, :client (atom nil)
        :vars (atom #{}) :cues (atom #{})}
  :exceptions (atom [])
  :loaded-components (atom [])})

 (def pixtol-chs
  {:dimmer 1
   :strobe [2 :hz [0.5 7.0]] :strobe-curve 3
   :attack 4 :release 5
   :bleed 6, :noise 7
   :rotate-back 8, :rotate-fwd 9
   :dimmer-attack 10 :dimmer-release 11
   :control 12})
(def fixture-data "Data for fixture definition generation"
 {:strip {:pixtol
          {:name "pixTol RGBW"
           :mode :rgbw
           :pixels 125
           :channels pixtol-chs}
          :pixtol-bulb
          {:name "pixTol RGB Bulbs"
           :mode :rgb
           :pixels 40
           :channels pixtol-chs}

          :opc
          {:name "Fadecandy/OPC RGB" :mode :rgb}}

  :moving {:rgbw-36
           {:name "RGBW 108/36 moving head"
            :channels
            {:pan [1 13] :tilt [2 14] :movement-speed 3
             :dimmer 4 :red 5 :green 6 :blue 7 :white 8 ;; :rgbw [5 6 7 8] ;XXX impl support
             :strobe [9 :start 10 :hz [1.0 30.0]]
             :focus 10 :auto 11 :speed 12}
            :calibration {:pan [127 85] :tilt [35 188]} ;-center, -half-circle
            :size 3.14 ;XXX do, one of diameter or length/height?
            :strength 360 ;XXX watts, lm, approximation?
            :beam 15} ;XXX angle

           :rgbw-7-12-moving
           {:name "LED MOVING HEAD 7x12W"
            :channels
            {:pan [1 2] :tilt [3 4] :movement-speed 5
             :dimmer 6 :strobe [7 :start 10]
             :auto 8 :speed 9
             :red 10 :green 11 :blue 12 :white 13
             :reset 14}
            :calibration {:pan [127 85] :tilt [0 256]}} ;fix so can fancy map here as well, eg reset offset 255...

           :rgbw-12-12-moving
           {:name "HY12x12 MOVING HEAD BEAM"
            :channels
            {:pan [1 14] :tilt [2 15] :movement-speed 9
             :dimmer 3 :red 4 :green 5 :blue 6 :white 7
             :strobe [8 :start 10]
             :auto 10 :speed 11 :control 12 :sound-sensitivity 13 :reset 16} ;16 150
            :calibration {:pan [102 73] :tilt [0 256]}}

           :robe-1200
           {:name "Robe 1200"
            :channels
            {:pan [1 2] :tilt [3 4] :movement-speed 5
             :bs 6 :color-wheel 7 :color-wheel-2 8
             ;; :cyan [9 :hue 180] :magenta [10 :hue 300] :yellow [11 :hue 55]
             :blue 9 :red 10 :green 11 ;temp, dunno why no work above?
             :color-filter 12 :effect-speed 13 :fresnels 14
             :zoom 15 :strobe [16 :open 50 :start 64]
             :dimmer 17}
            :calibration {:pan [127 85] :tilt [35 188]} ;guessing
            }}

  :wash {:random-wash
         {:name "bs"
          :channels {:dimmer 1 :red 2 :green 3 :blue 4 :amber [5 :hue 45]
                     :strobe 6 :bs 7 :bs2 8 :bs3 9 :bs4 10}}
         :rgb-washy
         {:name "uganda"
          :channels {:dimmer 1 :red 2 :green 3 :blue 4 :strobe 5}}
         :rgbw-mini
         {:name "Tiny RGBW split like"
          :channels {:dimmer 1 :red 2 :green 3 :blue 4 :white 5 :auto 6 :strobe 7}}}

  :other {:capture-camera
          {:name "Capture viewport"
           :channels {:x [1 2] :y [3 4] :z {5 6}
                      :pan [7 8] :tilt [9 10] :pitch [11 12]
                      :ambient 14 :lighting 15 :atmosphere 16
                      :layers 17 :scene 18}};layers: total of 64 slots each occupying 4 DMX steps, ie. 0-3, 4-7, 8-11 and so on.
          :processing-camera
          {:name "Processing viewport"
           :channels {:x [1 2] :y [3 4] :z {5 6}
                      :pan [7 8] :tilt [9 10] :pitch [11 12]}}
          :processing-color-1
          {:name "Processing dominant color"
           :channels {:red 1 :green 2 :blue 3}}

          :milight
          {:name "Milight RGB +W"
           :channels {:color-wheel-hue 1, :dimmer 2, :white 3}}}})

(defn make-group-patcher "Generic of below"
 [group-key f]
 (fn [group-key f]
  (into {} (for [fix (try (group-key fixture-data))]
            {(key fix) (f (val fix))}))))
(def moving ;[]
 (into {} (for [fix (:moving fixture-data)]
             {(key fix) (tolglow.fixtures/moving-head (val fix))})))
(def wash ;[]
 (into {} (for [fix (:wash fixture-data)]
             {(key fix) (tolglow.fixtures/create (val fix))})))
(def strip ;[]
 (into {} (for [fix (:strip fixture-data)]
             {(key fix) (tolglow.fixtures/strip-from-data (val fix))})))
;; (def strips (make-group-patcher ))
(def fixture-types (merge moving wash #_strip)) ;add other defs later

(def test-patches
   {:moving
    {:universe 4, :offset 66, :type :moving
    :list [(mapv #(flatten [(:robe-1200 moving) %])
                 [[-2.0 3.0 0.0], [-1.0 3.0 0.0], [1.0 3.0 0.0], [2.0 3.0 0.0]])]} ; :rotation {:y 180} ideal

   :camera
   {:universe 4, :offset 490, :type :moving
    :list `[[capture-camera 0.0 0.0 0.0]]}
   :wash
   {:universe 4, :offset 1, :type :wash
    :list '[[random-wash -1.0 2.0 0.0]
            [random-wash -0.3 2.0 0.0]
            [random-wash  0.3 2.0 0.0]
            [random-wash  1.0 2.0 0.0]]}})

(def pixtol-chs (-> fixture-data :strip :pixtol :channels))

(def patches #_old-patches
  {:moving
   {:universe 4, :offset 66, :type :moving
    ;; :list [(mapv #(flatten [(:robe-1200 moving) %])
    ;;              [[-2.0 3.0 0.0], [-1.0 3.0 0.0], [1.0 3.0 0.0], [2.0 3.0 0.0]])]} ; :rotation {:y 180} ideal
    :list [[:robe-1200 -1.0 2.0 0.0]
           [:robe-1200 1.0 2.0 0.0]]} ; :rotation {:y 180} ideal
   ;; {:universe 10, :offset 156, :type :moving
    ;; :list '[[rgbw-36-moving -1.5 1.7 0.0]
    ;;         [rgbw-12-12-moving 1.32 1.75 0.0]
    ;;         [rgbw-60-moving-beam 0.0 4.4 0.4
    ;;          :y-rotation (afterglow.transform/degrees 180)]
   :moving-mini
   {:universe 10, :offset 100, :type :moving
    :list [[:rgbw-7-12-moving -2.0 2.0 0.0]
           [:rgbw-7-12-moving  2.0 2.0 0.0]]}
   ;; :camera
   ;; {:universe 4, :offset 490, :type :moving
   ;;  :list [[:capture-camera 0.0 0.0 0.0]]}
   :cob
   {:universe 10, :offset 50
    :list [[:wa-100-cob-par 0.5 1.1 0.0]]}
   :wash
   {:universe 10, :offset 65, :type :wash
    :list [[:rgbw-mini 0.0 0.0 0.0]
           [:rgb-washy -0.0 2.0 0.0]
           [:random-wash -1.0 2.0 0.0]
           [:random-wash  1.0 2.0 0.0]]}
   :strip
   {:start-universe 2, :type :strip
    :list `[[#(tolglow.fixtures/pixel-strip
               20 :mode :rgb, :x [-3.0 -0.0] :y [2.0 2.0]
               :channels ~pixtol-chs)]
            [#(tolglow.fixtures/pixel-strip
               20 :mode :rgb, :x [-2.0 -0.0] :y [2.0 2.0]
               :channels ~pixtol-chs)]
            [#(tolglow.fixtures/pixel-strip
               20 :mode :rgb, :x [-1.0 -0.0] :y [2.0 2.0]
               :channels ~pixtol-chs)]
            [#(tolglow.fixtures/pixel-strip
               20 :mode :rgb, :x [-0.0 -0.0] :y [2.0 2.0]
               :channels ~pixtol-chs)]
            [#(tolglow.fixtures/pixel-strip
               20 :mode :rgb, :x [-1.0 -0.0] :y [2.0 2.0]
               :channels ~pixtol-chs)]
            [#(tolglow.fixtures/pixel-strip
               20 :mode :rgb, :x [-2.0 -0.0] :y [2.0 2.0]
               :channels ~pixtol-chs)]
            [#(tolglow.fixtures/pixel-strip
               20 :mode :rgb, :x [-3.0 -0.0] :y [2.0 2.0]
               :channels ~pixtol-chs)]
            [#(tolglow.fixtures/pixel-strip
               20 :mode :rgb, :x [-4.0 -0.0] :y [2.0 2.0]
               :channels ~pixtol-chs)]]}
   :tube
   {:start-universe 10, :type :strip
    :list `[[#(tolglow.fixtures/pixel-strip
               20, :mode :rgbw, :x [-1.0 3.0] :y [0.0 3.0]
               :channels ~pixtol-chs)]
            [#(tolglow.fixtures/pixel-strip
               20, :mode :rgbw, :x [ 0.0 3.0] :y [0.0 3.0]
               :channels ~pixtol-chs)]
            [#(tolglow.fixtures/pixel-strip
               20, :mode :rgbw, :x [ 1.0 3.0] :y [0.0 3.0]
               :channels ~pixtol-chs)]]}
   :fogger
   {:universe 10, :offset 511
    :list '[[af-250-fogger]]}})


(def effects ; replace hardcoded defaults with settings. LATER when enough helpers allow complete defs
 {:can-can {:vars {:bars 1 :cycles 1 :stagger 0 :spread 0 :pan-min -30 :pan-max 30 :tilt-min -100 :tilt-max 100}
            :whatever "else"}})
;;  {{:can-can {:chain {:pre ["lala"] :transform ["mm"] :optional {:wawa "no" :hehe 'disting}}
;;              :vars {:bars 1 :cycles 1 :stagger 0 :spread 0 :pan-min -30 :pan-max 30 :tilt-min -100 :tilt-max 100}
;;                  :whatever "else"}}})
;;                  ^ bs but like. ks + p are out of fx def.
;;                  spec chain of :targets :transform :inputs
;;                  since each param aware of its parts easy to automap pan/tilt <-> xyz <-> h s l
;;                  if input p/t -> vector -> xyz/hsl
;;                     single Number param -> single, or first of available multi

(def cue-data "defaults n stuff for various cue creation fns"
 {:bloom {}})
;; (def cues "Individual cues"
;;  {:code
;;   [[[1 0] (fn [show _] (tolglow.random/ize-cue-vars! :show *show*)) "Randomize Cue Vars"]
;;    [[1 1] (fn [show _] (tolglow.util/hook-var :wavetick-bars tolglow.random/ize-cue-vars!)) "Randomize each bar"]
;;    [[2 0] (fn [_ _] (afterglow.rhythm/metro-start (:metronome *show*) 1)) "Reset Metronome"]]})
;;
(def pixtol-data
 [:attack :release :dimmer-attack :dimmer-release
  :bleed :noise :rotate-back :rotate-fwd])

(def pages
 {:main-dimmer #{[0 0]
                 [1 0 :globals false
                  :groups [:moving-1 :moving-2 :moving-mini-1 :moving-mini-2
                           :wash-1 :wash-2 :wash-3 :wash-4]]
                 #_[2 0 :globals false
                  :groups [:strip-1 :strip-2 :tube-1 :tube-2]]}
  ;XXX above, also have by type (:moving :wash :strip), "zone" (left, right, more fine-grained?)
  ;direction (simple: patched direction, cooler: conditional assigners so eg moving heads
  ;           only get applied color effect while facing certain direction...)
  :effects   [0 1]
  :effects-2 [0 2]
  :modulator [0 3]
  :color     [1 2]
  :channel-control [0 4 :groups pixtol-data]
  :pointing #{[3 0 :aim]
              [4 0 :dir]
              [5 0 :pt]}
  :movement  [1 1]
  :testing   [2 1]})

(def pointing-data
 {:groups {:none {:color "white", :index 0, :setup-mods false}
           :a {:color "orangered", :index 1, :setup-mods true}
           :b {:color "royalblue", :index 2, :setup-mods true}}
  :fixture-mods [{:flip false, :color-mod `tolglow.color/create} ;#(identity %) ;thinking here go actual transform fns...
                 {:flip true,  :color-mod `#(tolglow.color/s -35 %)}]
  :group-mods [{:flip "x" :color-mod `#(tolglow.color/h 20 %)} ;:transform #(rescale % :x true)}
               {:flip "xy" :color-mod `#(tolglow.color/h 10 %)}] ;:transform #(rescale % :x true :y true)}]
  :type
  {:aim {:vars [:x :y :z] :name-prefix "Aim" :color-mod `tolglow.color/create
         :fns `[afterglow.effects.params/build-aim-param
                afterglow.effects.params/build-aim-transformer
                afterglow.effects.movement/aim-effect]}
   :dir {:vars [:x :y :z] :name-prefix "Dir" :color-mod `#(tolglow.color/h -30 %)
         :fns `[afterglow.effects.params/build-direction-param
                afterglow.effects.params/build-direction-transformer
                afterglow.effects.movement/direction-effect]}
   :pt {:vars [:pan :tilt] :name-prefix "PT" :color-mod `#(tolglow.color/h 30 %)
        :fns `[afterglow.effects.params/build-direction-param-from-pan-tilt
               afterglow.effects.params/build-direction-transformer
               afterglow.effects.movement/direction-effect]}}
  :vars {:x {:start 0.0 :min -10.0 :max 10.0 #_(cfg :venue :wall :left)} ;could do with colors etc for var display... also later split cue boxes showing var colors? or alternating
         :y {:start 0.0 :min -2.0 :max 10.0}
         :z {:start 2.0 :min -5.0 :max 10.0}
         :pan {:start 0.0 :min -180.0 :max 180.0} ;XXX handle :bounds 180.0}
         :tilt {:start 0.0 :min -180.0 :max 180.0} }
  :movement-mods {:motor-speed "dont forget"
                  :focus "sorta takes nearer-further..." }})


(def data
 {:description "tol show" :universes [1 2 3 4 10] :hz 40 ;universes should come from actual patch cfg...
  :fixture-patches true, :cue-pages true
  :debug {:enabled true
          :force-init true #_false
          :force-cue-pages #_true false
          :auto-print-trace true} ;XXX how both catch, print, and pass to eg pst?
  :macro {:save-file "macros.clj"
          :save-enabled true, :restore-enabled false} ;first gotta make it work hehe
  :auto-save-cue-vars true ;implement, then create proper toggle. and reset button :P

  :web-server {:enabled true :port 16000}
  :nrepl {:enabled true :port 5000} ;alredy runs in lein...
  :terminal-repl {:enabled true}
  :logging {:enabled true}
  :ns {:base 'tolglow.core}
  :max {:enabled false, :ns 'afterglow.max.init
        :require ['afterglow.max.core 'afterglow.max.init]}
  :color {}
  :venue venue
  :measure {:default #(afterglow.transform/build-distance-measure 0 (:rig venue) 0 :ignore-z true)}
  :osc {:enabled true :debug true
        :address "127.0.0.1" :port-in 16010 :port-out 16011}
  :controllers {:enabled true
                :push {:enabled true :name "Ableton Push" }
                :launchpad {:enabled false :name "Launchpad" }}
  :midi {}
  :clock-sync {:enabled true :device "IAC Driver" :type :midi}
  :wavetick {:enabled true :device "IAC Driver" :channel 0 ;0 is midi ch 1
             :maps {:bar {:note 64 :ticker :wavetick-bars} ;change :fn -> :fns (atom []), then add watchers to that?
                    :beat {:note 65 :ticker :wavetick-beats}
                    :tatum {:note 66 :ticker :wavetick-tatums}}}
  :show-state {:enabled false ;XXX below not ready
               :actions `[(afterglow.show/add-effect! :beam-on (afterglow.effects.channels/function-effect "Beam on" :shutter-open 100 (fixtures-named :moving)))]}
  :channels {:global-control [:fog :movement-speed :focus]
             :pixtol [:attack :release :dimmer-attack :dimmer-release
                      :bleed :noise :rotate-back :rotate-fwd]}
  :fixtures
   {:groups [:moving :moving-mini :cob :wash :strip :tube]
    :types {:moving {:groups [:moving :moving-mini] ;generate...
                     :names [:moving-1 :moving-2 :moving-3 ;generate
                             :moving-mini-1 :moving-mini-2 :moving-mini-3]}
            :wash {}
            :strip {:groups [:strip :tube]
                    :names [:strip-1 :tube-1]}}
    :defs patches}
  :pointing pointing-data
  :pages pages
  ; or like  :cues {:pages pages
  ;                 :auto-save-vars true}
  :init {:components [:show :vars :set-ns :set-macro-path] ;looks in setup.clj (and later optionally elsewhere? for fns with same name)
         :modules [:nrepl :web-server :controllers
                   :max-msp :fixture-patches :cue-pages
                   :osc :clock-sync :wavetick :extra-for-show
                   :show-state] ;like :beam-on, but future: auto load state from last session...
         :post [:terminal-repl]}})

(defn get-getter [m] (fn [& path] (reduce #(-> %1 %2) m path)))

(def cfg (get-getter data))
(def at (get-getter binds))
(def ptr-cfg (get-getter pointing-data))

;; (def values-data
;;  [{:type :min :kinds [:fraction :percent :dmx :free]}
;;   {:type :unit :kinds }])
(def values-data
 {:min #{:fraction :percent :dmx :free}
  :time-unit #{:ticks :beats :bars :phrases}
  :time-div #{:cycles}
  :fraction #{:width :fraction}})

;; (def var-types ;TODO more robustly defined types of vars (not in the color/bool/number sense
;;  {:time {:unit {:integer } :divisor :width};  touch time knob to get :unit :cycles :base-unit[:beats :bars :phrases] overlay. not adding :bars :cycles but (time :interval-ratio :bars),
;;   :range [:min :max] ;  touch range knob to get :min :max overlay. not :min-dmx :max-dmx but (range :scale dmx)
;;   :size [:min :max :fraction]
;;   :blending [:alpha :keyhole] })
;but like time[beats, cycles, width] / range[min, max] / size[min, max, fraction] / blend[alpha, keyhole?] etc)
;this way time is simply [mul, div, width[mul, div, frac]], range is [min[mul, div, frac]*scale, max[mul, div, frac]*scale]
;these "types"/groups should have additional properties like color, be grouped for creation and handling
;basically namespaces/classes to reuse groupings of data structures
;; XXX additional min/max _for_ each rel property (beginning with min/max), so :min-min :min-max etc.
;;  By default this set off min/max so no need specify. Effective min/max set off this.
;;  Used for when updating these automatically eg by wavetick binding
(def var-data
 {:beats   ["beats"  4  1 32], :bars  ["bars"  2  1 16], :phrases  ["phrases" 1  1  8]
  :cycles  ["cycles" 1  1  8]
  :max-num ["max"      1.0 0.0 1.0]  :min-num      ["min"   0.0  0.0  1.0]
  :max-dmx ["max"      255 0   255]  :min-dmx      ["min"     0    0  255]
  :phase   ["phase"    0.0 0.0 1.0]
  :width   ["width"   0.25 0.0 1.0]  :width-denom  ["width"   4    1   16]
  :halo    ["halo"    0.15 0.0 1.0]
  :alpha   ["alpha"   1.00 0.0 1.0]
  :down    ["down"    true]

  :lfo-gain      ["lfo-gain"   1.0  0.0  5.0], :lfo-offset    ["lfo-offset" 0.0 -3.0  3.0]
  :lfo-noise     ["lfo-noise"  0.0  0.0  1.0], :lfo-picker    ["lfo-picker" 0    0    3]

  :min-change    ["min-change" 0.15  0.0  0.33]

  :hue           ["hue"        0 -360  360] :hue-mod       ["hue-mod"          30 -180  180]
  :lightness     ["lightness"  0  -50   50] :lightness-mod ["lightness-mod"   -7   -30   30]
  :saturation    ["saturation" 0  -50   50] :saturation-mod ["saturation-mod" -10  -30   30]

  :keyhole?      ["keyhole?" true]
  :pause         ["pause"  false]
  :htp           ["htp"     true]
  :fade          ["fade"    true]
  :level         ["level"    0.0  0.0  1.0],  :level-vel     ["level"   true  0.0  1.0]
  :level-dmx     ["level"    200    0  255],  :held-dmx      ["level"   true    0  255] ;flag for velocity at var-start
  :level-percent ["level"   true    0  100] ;flag for velocity at var-start
  :lightness-percent ["lightness"   80    0  100] ;flag for velocity at var-start
  :fraction      ["fraction" 0.0  0.0  1.0],  :fraction-vel  ["fraction" true  0.0  1.0]
  :fade-fraction ["fade-fraction" 0.0  0.0  1.0]
  :fade-time     ["fade-time" 200  1  2500]
  :chance        ["chance"  0.0001 0.0  1.0]
  :color         ["color" "antiquewhite2"]}) ;nice if :color type could also take min/max, as colors, bounds per hsl (:min :black :max :white = full range)
  ;; :color-rand    ["color"   (color-like nil)]   ;nicer yet if color picker overlay reflected this XXX first fix overlay less rigid using (color-like)

;; (defrecord CueParam) ;makes sense no? not from IParam maybe but in some sense...
(def param-data "Types of oscillators and associated data - colors, cue-vars etc" ;XXX put fns for building here as well...
 (let [common [:beats :cycles :min-dmx :max-dmx :phase]
       scale  [:lfo-gain :lfo-offset]
       most   (into common scale)] ;XXX fix so just add however then flatten on build...
  [{:type "held"     :color "antiquewhite2" :variables [:held-dmx]
    :fn '#(tolglow.param/bind-keys % 255)} ;could also do with scale/gain tho
   {:type "level"    :color "white"         :variables [:level-dmx]
    :fn '#(tolglow.param/bind-keys % 255)} ;or more likely a wrapper so plain fn here.
   {:type "random"   :color "lightskyblue3" :variables (into common [:min-change])
    :fn 'tolglow.param/rng} ;; then should just apply with m of :type keys (:min min, :max max, :min-change min-change)
   {:type "sawtooth" :color "salmon3"       :variables (into common (into [:down] scale))
    :fn 'afterglow.effects.oscillators/sawtooth}
   {:type "triangle" :color "orangered1"    :variables most
    :fn 'afterglow.effects.oscillators/triangle}
   {:type "sine"     :color "navajowhite2"  :variables most
    :fn 'afterglow.effects.oscillators/sine}
   {:type "square"   :color "darkseagreen4" :variables (into common [:width-denom])
    :fn 'afterglow.effects.oscillators/square}]))
   #_{:type "multi"    :color "pink"          :variables (cue-vars most :lfo-picker :down :width-denom)}
  ;{:type "external" :color "whatever"      :variables (some shit bound to show vars auto bound to max)}

(def color-data "Construct a palette of named colors, define acceptable color ranges"
 {:default {:lightness 55 :saturation 70}
  :max {:saturation 90}
  :min {}
  :map {:blue "lightskyblue3" :red "orangered1" #_and-so-on-etc}}) ;first start with terminal-like 16 colors, dark-light


(defn wall[side] (side (cfg :venue :wall)))
(defn wall-x [side] [(wall side) (cfg :venue :rig) 0 :ignore-z true])

(def measure-data {:rig-center [0 (cfg :venue :rig) 0 :ignore-z true]
                   :left-wall (wall-x :left) :right-wall (wall-x :right)})
