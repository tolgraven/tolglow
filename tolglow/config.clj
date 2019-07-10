(ns tolglow.config "Settings and functions to apply them" {:author "Joen Tolgraven"}
  (:require [afterglow
             [core :as core]
             [transform :as tf :refer [degrees]]]
            [tolglow
            [fixtures :as fixtures]]))

(def venue "Main truss and ceiling height (Y), distance from it to stage/rear (Z) and left/right (X) walls"
 {:rig 2.00 :ceiling 3.00 :wall {:stage -2.00 :rear 6.00 :left -2.50 :right 2.50}})

(defonce binds ;dirty way to separate, should be auto-generated somehow...
 {:show (atom nil) ;? id or wha?
  :vars (atom nil)
  :ns (atom nil)
  :auto-cue-grid (atom {:x 0 :y 0 :page-x 0 :page-y 0}) ;thinking largerly skip xy and have a macro calling an incrementor...
  :controller {:push (atom nil) :launchpad (atom nil)}
  :step (reduce #(into %1 {%2 (atom nil)}) {} [:beat :bar :phrase])
  :fixture-type {:moving (atom []), :strip (atom [])}
  :osc {:server core/osc-server, :client (atom nil)
        :vars (atom #{}) :cues (atom #{})}
  :exceptions (atom [])
  :loaded-components (atom [])})

 (def pixtol-chs "Channel spec for pixTol standard"
  {:dimmer 1
   :strobe [2 :hz [0.5 7.0]] :strobe-curve 3
   :attack 4 :release 5
   :bleed 6, :noise 7
   :rotate-back 8, :rotate-fwd 9
   :dimmer-attack 10 :dimmer-release 11
   :control 12})

(def fixture-data "Data for fixture definition generation"
 {:strip ;; If the fixture or head has an  explicit :visualizer-visible boolean, that is honored
  {:pixtol-144-1m ;so can cfg that way
   {:name "pixTol RGBW 144" :channels pixtol-chs
    :strip {:color-layout :rgbw :pixels 125 :density 144}}
   :pixtol-60-2m
   {:name "pixTol RGBW 60" :channels pixtol-chs
    :strip {:color-layout :rgbw :pixels 120 :density 60}}
   :pixtol-bulb
   {:name "pixTol RGB Bulbs" :channels pixtol-chs
    :strip {:color-layout :rgb :pixels 1 :density 1}}
   :pixtol-144-10
   {:name "pixTol RGBW 144 10" :channels pixtol-chs
    :strip {:color-layout :rgbw :pixels 10 :density 144}}
   :opc {:name "Fadecandy/OPC RGB"
         :strip {:color-layout :rgb :pixels 120 :density 60}}} ;; XXX afterglow needs to support fixtures spanning multiple unis btw

  :strip-test
  {:pixtol-test
   {:name "pixTol test"
    :channels pixtol-chs
    :strip {:color-layout :rgbw :pixels 5 :density 10}}}

  :moving
  {:rgbw-36-moving
   {:name "RGBW 108/36 moving head" :calibration {:pan [127 85] :tilt [35 188]}
    :channels
    {:pan [1 13] :tilt [2 14] :movement-speed 3 :dimmer 4 :rgbw 5 ;[5 6 7 8]
     :strobe [9 :start 10 :hz [1.0 30.0]] :focus 10 :auto 11 :speed 12}
    ;; :size 3.14 ;XXX do, one of diameter or length/height?
    ;; :strength 360 ;XXX watts, lm, approximation?
    #_:beam #_15} ;XXX angle

   :rgbw-7-12-moving
   {:name "LED MOVING HEAD 7x12W" :calibration {:pan [127 85] :tilt [0 256]};only 180 deg. fix so can fancy map here as well, eg reset offset 255...
    :channels
    {:pan [1 2] :tilt [3 4] :movement-speed 5 :dimmer 6 :strobe [7 :start 10 :hz [1 10]]
     :auto 8 :speed 9 :rgbw 10 :reset 14}}  ;; :red 10 :green 11 :blue 12 :white 13;14 255...

   :rgbw-12-12-moving
   {:name "HY12x12 MOVING HEAD" :calibration {:pan [102 73] :tilt [0 256]}
    :channels
    {:pan [1 14] :tilt [2 15] :movement-speed 9 :dimmer 3 :rgbw [4 5 6 7] ;; :dimmer 3 :red 4 :green 5 :blue 6 :white 7
     :strobe [8 :start 10] :auto 10 :speed 11 :control 12 :sound-sensitivity 13 :reset 16}} ;ch16 val150 actually

   :rgbw-60-moving-beam
   {:name "60w Beam MOVING HEAD" :calibration {:pan [127 85] :tilt [0 256]}
    :channels
    {:pan [1 2] :tilt [3 4] :movement-speed 5
     :dimmer 6 :strobe [7 :open 8 :start 16 :hz [0.2 10]] ;here needs better shutter-builder...
     :rgbw 8 :auto 12 :control [13 :reset 200 :sound-active 240]}}

   :robe-1200
   {:name "Robe 1200" :calibration {:pan [127 85] :tilt [35 188]}
    :channels
    {:pan [1 2] :tilt [3 4] :movement-speed 5 :bs 6 :color-wheel 7 :color-wheel-2 8
     ;; :cyan [9 :hue 180] :magenta [10 :hue 300] :yellow [11 :hue 55]
     :rgb [10 11 9] ;; :blue 9 :red 10 :green 11 ;temp, dunno why no work above?
     :color-filter 12 :effect-speed 13 :fresnels 14 :zoom 15
     :strobe [16 :open 50 :start 64] :dimmer 17}}} ;guessing

  :wash
  {:rgb-54-3-par {:name "54 LED RGB PAR"
    :channels {:dimmer 1 :rgb 2 :strobe [5 :start 10 :hz [0.2 10]] :control 6 :speed 7}}
   :rgbw-7-12-par {:name "LED RGBW Par Light"
    :channels {:dimmer 1 :rgbw 2 :strobe [6 :start 10 :hz [0.5 10]] :control 7 :speed 8}}
   :rgbw-18-12-par {:name "18x12 RGBW, 200+ W"
    :channels {:dimmer 1 :rgbw 2 :strobe [6 :start 10 #_:hz #_[0.5 10]] :control 7}}
   :rgbwauv-5-18-par {:name "5x18 RGBWA+UV, ? W"
    :channels {:dimmer 1 :rgbwa 2 :uv [7 :hue 270] ;:rgbw 2 :amber [6 :hue 45] :uv [7 :hue 270]
               :strobe [8 :start 10 #_:hz #_[0.5 10]] :control 9 :speed 10}}
   ;; :wa-100-cob-par {:name "100Watt W/A LED COB PAR Light"
   ;;  :channels {:dimmer 1 :white 2 :amber [3 :hue 48] :strobe [4 :start 10 :hz [0.45 10]]}}

   :adj-pro-led-tol {:name "ADJ 64B LED PRO"
    :channels {:rgb 1 :color-macros 4 :strobe [5 :start 16 :hz [1.12 32]] :speed 6}}
   :rgbw-mirror {:name "RGBW churchy window thing"
    :channels {:dimmer 1 :rgbw 2 :strobe [6 :start 16 :hz [0.2 10]] :movement-speed 7}}

   :rgbw-mini {:name "Tiny RGBW split like"
    :channels {:dimmer 1 :rgbw 2 :auto 6 :strobe 7}}
   :hollywood-par {:name "Hollywood PAR"
    :channels {:dimmer 1 :strobe 2 :auto 3 :whatever 4 :rgb 5}}}

  :cob
  {:wa-100-cob-par {:name "100Watt W/A LED COB PAR Light"
    :channels {:dimmer 1 :white 2 :amber [3 :hue 48] :strobe [4 :start 10 :hz [0.45 10]]}}}

  :other
  {:capture-camera
   {:name "Capture viewport"
    :channels {:x [1 2] :y [3 4] :z [5 6] :pan [7 8] :tilt [9 10] :pitch [11 12]
               :ambient 14 :lighting 15 :atmosphere 16 :layers 17 :scene 18}};layers: total of 64 slots each occupying 4 DMX steps, ie. 0-3, 4-7, 8-11 and so on.
   :processing-camera
   {:name "Processing viewport"
    :channels {:x [1 2] :y [3 4] :z [5 6] :pan [7 8] :tilt [9 10] :pitch [11 12]}}
   :processing-color-1 {:name "Processing dominant color" :channels {:rgb 1}}

   :milight {:name "Milight RGB +W" :channels {:color-wheel-hue 1, :dimmer 2, :white 3}}
   :milight-new {:name "Milight RGBW"
      :channels {:color-wheel-hue 1, :dimmer 2, :white 3, :amber 4 :saturation 5}}

   :af-250-fogger {:name "Fogger" :channels {:fog 1}}}})

(defn make-group-data "Make patchable fixture definitions from raw data, for fixtures of specific type and optionally passing a different parser than the default tolglow.fixtures/create"
 [group-key & {:keys [builder-fn fixture-info]
               :or {builder-fn tolglow.fixtures/create, fixture-info fixture-data}}]
  (into {} (for [[fixture data] (try (group-key fixture-info))]
           {fixture (builder-fn data)})))

(def fixture-categories [:moving :wash :strip :cob #_:other])
(def fixture-types (apply merge (map make-group-data fixture-categories)))


; should def be able to patch, and update patches, in web ui!
(def patches #_old-patches
  {:moving ;;XXX parser should handle just pushing new ones onto and offset auto calcs and tells you
   {:universe 1, :offset 400, :type :moving
    ;; :list [(mapv #(flatten [(:robe-1200 moving) %])
    ;;              [[-2.0 3.0 0.0], [-1.0 3.0 0.0], [1.0 3.0 0.0], [2.0 3.0 0.0]])]} ; :rotation {:y 180} ideal
    :list [[:rgbw-36-moving     -1.5 1.2 -0.5]
           [:rgbw-12-12-moving   1.5 1.2 -0.5 #_:y-rotation #_(degrees 180)]
           `[rgbw-60-moving-beam 0.0 0.2 -1.0 :y-rotation (degrees 180)]]} ; :rotation {:y 180} ideal
   :moving-mini
   {:universe 1, :offset 150, :type :moving
    :list [[:rgbw-7-12-moving -2.0 3.0 0.0]
           [:rgbw-7-12-moving -1.0 2.0 0.0]
           [:rgbw-7-12-moving  1.0 2.0 0.0]
           [:rgbw-7-12-moving  2.0 3.0 0.0]]}
   ;; :camera
   ;; {:universe 4, :offset 490, :type :moving
   ;;  :list [[:capture-camera 0.0 0.0 0.0]]}
   :cob
   {:universe 10, :offset 50
    :list [[:wa-100-cob-par 0.0 4.0 -1.0]]}
   :wash
   {:universe 1, :offset 292, :type :wash
    :list  [[:rgbw-18-12-par -2.5 2.5 0.0]
            [:rgbw-7-12-par  -1.5 2.0 0.0]
            [:hollywood-par   1.5 2.0 0.0]
            [:hollywood-par   2.5 2.5 0.0]]}
   :strip
   {:start-universe 2, :type :strip
    :list [[:pixtol-144-10 -0.0 0.1 0.0]
           #_[:pixtol-60-2m  -1.0 2.0 0.0 :y-rotation (degrees 90)]]}
   :tube
   {:start-universe 11, :type :strip
    :list `[[#(tolglow.fixtures/pixel-strip
               4, :color-layout :rgbw, :x [-1.0 1.0] :y [4.0 4.0]
               :channels ~pixtol-chs)]]}
   #_:fogger
   #_{:universe 1, :offset 511
    :list [[:af-250-fogger 0.0 0.0 0.0]]}})

(def channel-pages
 {:pixtol [:attack :release :dimmer-attack :dimmer-release :bleed :noise :rotate-back :rotate-fwd]
  :global [:fog :movement-speed :pan :tilt :white :strobe :focus]
  :color [:red :green :blue :white :amber :uv]})

(def main-fixture-units
 [:moving-1 :moving-2 :moving-3 :moving-4
  :moving-mini-1 :moving-mini-2 :moving-mini-3 :moving-mini-4])
(def static-fixture-units
 [:wash-1 :wash-2 :wash-3 :wash-4
  :strip-1 :strip-2 :tube-1 :cob-1])

(def pages
[:main-dimmer #{[0 0]
                [1 0 :globals false :groups main-fixture-units];XXX also have by type (:moving :wash :strip), "zone" (left, right, more fine-grained?)
                #_[2 0 :globals false :groups static-fixture-units]} ;direction (simple: patched direction, cooler: conditional assigners so eg moving heads
;                                                                 only get applied color effect while facing certain direction...)
 :effects   [0 2]
 :effects-2 [1 2]
 :modulator [2 2]
 :color   #{[0 1]
            [1 1 :groups main-fixture-units]}
 :channel-control #{[0 3 :groups (channel-pages :pixtol)]
                    [3 1 :groups (channel-pages :global)]
                    [2 1 :groups (channel-pages :color)]}
 :pointing #{[2 0 :aim]
             [3 0 :dir]
             [4 0 :pt]
             [5 0 :pt-raw]}
 :movement   [4 1]
 :testing    [3 2]])

(def pointing-data
 {:groups {:none {:color "white", :index 0, :setup-mods false}
           :a {:color "orangered", :index 1, :setup-mods true}
           :b {:color "royalblue", :index 2, :setup-mods true}}
  :fixture-mods [{:flip false, :color-mod `tolglow.color/create} ;these should be other way ;#(identity %) ;thinking here go actual transform fns...
                 {:flip true,  :color-mod `#(tolglow.color/s -0.35 %)}] ;but when i flip it errors about lack of transform3d object heh?? guess cause tf cue = keyword var param set not yet happened...
  :group-mods [{:flip "x" :color-mod `#(tolglow.color/h 0.05 #_0.20 %)} ;:transform #(rescale % :x true)}
               {:flip "xy" :color-mod `#(tolglow.color/h 0.025 %)}] ;:transform #(rescale % :x true :y true)}]
  :type
  {:aim {:vars [:x :y :z] :name-prefix "Aim" :color-mod `tolglow.color/create
         :fns `[afterglow.effects.params/build-aim-param
                afterglow.effects.params/build-aim-transformer
                afterglow.effects.movement/aim-effect]}
   :dir {:vars [:x :y :z] :name-prefix "Dir" :color-mod `#(tolglow.color/h -0.30 %)
         :fns `[afterglow.effects.params/build-direction-param
                afterglow.effects.params/build-direction-transformer
                afterglow.effects.movement/direction-effect]}
    :pt {:vars [:pan :tilt] :name-prefix "PT" :color-mod `#(tolglow.color/h 0.30 %)
         :fns `[afterglow.effects.params/build-direction-param-from-pan-tilt
                afterglow.effects.params/build-direction-transformer
                afterglow.effects.movement/direction-effect]}
:pt-raw {:vars [:pan :tilt] :name-prefix "PT-raw" :color-mod `#(tolglow.color/h 0.50 %)
         :fns `[afterglow.effects.params/build-pan-tilt-param
                nil ;(fn [& this] this #_nil) ;cant transform. create Vector2d param maybe? could always do flip transforms...
                afterglow.effects.movement/pan-tilt-effect]}}
  :vars {:x {:start 0.0 :min -10.0 :max 10.0 #_(cfg :venue :wall :left)} ;could do with colors etc for var display... also later split cue boxes showing var colors? or alternating
         :y {:start 1.0 :min -2.0 :max 10.0} ;should be -1.0-1.0 vals based on show dimensions btw...
         :z {:start 2.0 :min -5.0 :max 10.0} ;-1.0 x = fully right, 1.0 z = fully front etc...
         :pan {:start 0.0 :min -180.0 :max 180.0} ;XXX handle :bounds 180.0}
         :tilt {:start 0.0 :min -180.0 :max 180.0} } ;180 degrees is actually 360 for some reason
  :lfo {:x [-2.0 2.0 4] :y [-2.0 2.0 3] :z [2.0 3.0 8] ;min max bars, due to vars/prefixed-lfos. but should be using keys and stuff i guess for clarity
        :pan [-90 90 4] :tilt [0 90 3]}
  :movement-mods {:motor-speed "dont forget"
                  :focus "sorta takes nearer-further..." }})

; should have sep cfg files:
; main
; fixture defs, fixture patches
; (cue defs), cue page patches
; only parsers and data-not-really-settings (pointing, cue var defaults)
; could stay here
(def data
 {:description "tol show ffs" :universes [1 2 3 10 11] :hz 40 ;universes should come from actual patch cfg...
  ;;XXX BUG still with reading top-level true/false vals :fixture-patches true, :cue-pages true
  ;; :fixture-patches false, ;:cue-pages false ;seems get loaded anyways?
  :debug {:enabled true
          :force-init false
          :force-cue-pages false #_false
          :auto-print-trace true} ;XXX how both catch, print, and pass to eg pst?
  :macro {:save-file "macros.clj" ;relative to project home
          :save true, :load true} ;first gotta make it work hehe
  :load-macros true ;XXX mechanism for fn lookup from nested stuff. fix when rework entire cfg structure
  :auto-save-cue-vars true ;implement, then create proper toggle. and reset button :P
  ; also a second button to persistently save cue state as new default. and maybe 4 saveable slots for each cue of different var values

  :web-server {:enabled true :port 16000}
  :nrepl {:enabled true :port 5000} ;alredy runs in lein...
  :terminal-repl {:enabled true}
  :logging {:enabled true}
  :ns {:base 'tolglow.core}
  :max-msp {:enabled false, :ns 'afterglow.max.init
            :require `[afterglow.max.core afterglow.max.init]}
  :visualizer {:enabled true
               :max-lights 16}
  :color {}
  :venue venue
  :measure {:default #(afterglow.transform/build-distance-measure 0 (:rig venue) 0 :ignore-z true)}
  ;; ^^ says the type is like, tol cfg fn even when wrapped, hmm.
  ;; :measure {:default (afterglow.transform/build-distance-measure 0 (:rig venue) 0 :ignore-z true)}
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
               :actions `[(afterglow.show/add-effect!
                           :beam-on (afterglow.effects.channels/function-effect
                                     "Beam on" :shutter-open 100 (afterglow.show/fixtures-named :moving)))]}
  :pointing pointing-data
  :fixtures
   {:groups [:moving :moving-mini :wash :cob :strip :tube]
    :types {:moving {:groups [:moving :moving-mini] ;generate...
                     :names [:moving-1 :moving-2 :moving-3
                             :moving-mini-1 :moving-mini-2 :moving-mini-3 :moving-mini-4]}
            :wash {}
            :strip {:groups [:strip :tube]
                    :names [:strip-1 :tube-1]}}
    :patches patches :enabled true}
  ;; :universes (extract-auto-from-patches :universe and :starting-universe etc...)
  :channels channel-pages
  :cue-pages {:definitions pages
              :enabled true}
  ; or like  :cues {:pages pages
  ;                 :auto-save-vars true}
  :show-vars {:global-effect-timeout-ms 3000000000
              :those-global-metro-dividers :whatkey-again?} ;examples.
  ;; init components/modules are looked up to fns in tolglow.setup. XXX support loading external modules
  :init {:search-in-namespaces `[tolglow.setup] ;;not in use yet, hardcoded
         :components [:show :vars :set-ns :set-macro-path] ;looks in setup.clj (and later optionally elsewhere? for fns with same name)
         :modules [:nrepl :web-server :controllers :osc :max-msp
                   :fixtures :cue-pages :clock-sync :wavetick
                   :extra-for-show :show-state :load-macros] ;like :beam-on, but future: auto load state from last session...
         :post [:terminal-repl]}})

(defmacro get-getter "Create a -> getter for data maps
Arguments: name of fn to define, docstring, map where to search."
 [name docstring m]
  `(defn ~name ~docstring [& path#]
     (reduce #(-> %1 %2) ~m path#)))
(get-getter cfg "Look in cfg map" data)
(get-getter at "Look in binds map" binds)
(get-getter ptr-cfg "Look in pointer cfg map" pointing-data)

(def fixture-keys #(cfg :fixtures :types % :names))

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
  :gain      ["gain"   1.0  0.0  5.0], :offset    ["offset" 0.0 -1.0  1.0] ;if offset is multiplied by max then anything outside -1/1 useless no??
  :noise     ["noise"  0.0  0.0  1.0], :picker    ["picker" 0    0    3]

  :min-change    ["min-change" 0.15  0.0  0.33]

  :hue           ["hue"        0 -1.0  1.0] :hue-mod       ["hue-mod"          0.30 -0.180  0.180] ;XXX fix ::colors/
  :lightness     ["lightness"  0  -0.50   0.50] :lightness-mod ["lightness-mod"   -0.7   -0.30   0.30]
  :saturation    ["saturation" 0  -0.50   0.50] :saturation-mod ["saturation-mod" -0.10  -0.30   0.30]

  :keyhole?      ["keyhole?" true]
  :pause         ["pause"  false]
  :htp           ["htp"     true]
  :fade          ["fade"    true]
  :level         ["level"    0.0  0.0  1.0],  :level-vel     ["level"   true  0.0  1.0]
  :level-dmx     ["level"    200    0  255],  :held-dmx      ["level"   true    0  255] ;flag for velocity at var-start
  :level-percent ["level"   true    0  100] ;flag for velocity at var-start
  :lightness-percent ["lightness"   80    0  100] ;flag for velocity at var-start
  :fraction      ["fraction" 0.0  0.0  1.0],  :fraction-vel  ["fraction" true  0.0  1.0]
  :fade-fraction ["fade-fraction" 0.25  0.0  1.0]
  :fade-time     ["fade-time" 200  1  2500]
  :chance        ["chance"  0.0001 0.0  1.0]
  :color         ["color" "antiquewhite2"]}) ;nice if :color type could also take min/max, as colors, bounds per hsl (:min :black :max :white = full range)
  ;; :color-rand    ["color"   (color-like nil)]   ;nicer yet if color picker overlay reflected this XXX first fix overlay less rigid using (color-like)

;; (defrecord CueParam) ;makes sense no? not from IParam maybe but in some sense...
(def param-data "Types of oscillators and associated data - colors, cue-vars etc" ;XXX put fns for building here as well...
 (let [common [:beats :cycles :min-dmx :max-dmx :phase]
       ;; scale  [:lfo-gain :lfo-offset]
       scale  [:gain :offset]
       most   (into common scale)] ;XXX fix so just add however then flatten on build...
  [{:type "held"     :color "antiquewhite2" :variables [:held-dmx]
    :fn '#(tolglow.param/bind-keys % 255)} ;could also do with scale/gain tho
   {:type "level"    :color "white"         :variables [:level-dmx]
    :fn '#(tolglow.param/bind-keys % 255)} ;or more likely a wrapper so plain fn here.
   {:type "random"   :color "lightskyblue3" :variables (into common [:min-change :fade-fraction])
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
 {:default {:lightness 0.55 :saturation 0.70}
  :max {:saturation 0.90}
  :min {}
  :map {:blue "lightskyblue3" :red "orangered1" #_and-so-on-etc}}) ;first start with terminal-like 16 colors, dark-light


(defn wall   [side] (side (cfg :venue :wall)))
(defn wall-x [side] [(wall side) (cfg :venue :rig) 0 :ignore-z true])

(def measure-data {:rig-center [0 (cfg :venue :rig) 0 :ignore-z true]
                   :left-wall (wall-x :left) :right-wall (wall-x :right)})



(def attributes-and-models ; some bs for below to use
 {:capabilities
  {:types {:movement {:raw [:pan :tilt]
                      :calibration [:angle :howfarcanturnlikeyo]
                      :models {:aim :vector ;[:x :y :z] ;like, this is ways to model control of this capability
                               :direction [:x :y :z]
                               :pan-tilt [:pan :tilt]} ;in afterglow thats pan-tilt in show space, so not same as raw
                      :modulators [:motor-speed]} ;could also inc some strip stuff how moves across
           ;;^ then obviously some fx/cues will make this :pan-min :pan-max :tilt-min :tilt-max etc
           :color {:raw {:rgb [:red :green :blue]
                         :rgbw [:red :green :blue :white]
                         :rgbwa [:red :green :blue :white :amber]
                         :cmy [:cyan :magenta :yellow]
                         :single :specify}
                   :calibration [:brightness :hue]
                   :models {:hsl [:hue :saturation :lightness]
                            :hsla [:hue :saturation :lightness :alpha]}} ;alpha should be a sep component that can go in colors, layers, fxs, etc...
           :shutter {:raw [:strobe]
                     :models {:color-strobe [:strobe :color]}} ;with lightness
           :power {:raw [:dimmer]
                   :calibration [:lumen :cuttoff-like-maybegrosscolorsatlowdimmervalues]
                   :models {:scale-like-strobe-hehu :maybe}}

           :function {}
           }}})
(def effects ; replace hardcoded defaults with settings. LATER when enough helpers allow complete defs
 {:can-can
  {:type :movement #_"ie what capabilities does it use, fixtures need to be easily discoverable by ability"
  ;; :controls {:time-ratio :bars, :pan [-30 30], :tilt [-100 100], :stagger 0, :spread 0}
   ;; :controls {:time-ratio :bars, :movement :pan-tilt, :vars {:stagger 0, :spread 0}} ;eh tricky should rather attach ratio to it
   :controls {:movement {:model :pan-tilt, :type :range} ;should create :pan-min :pan-max :tilt-min...
              :time-ratio :bars ;get :bars :cycles
              :special-vars {:stagger Number, :spread Number}} ;eh tricky should rather attach ratio to it
   ;;^ could parse down to below...
   ;; :vars {:bars 1 :cycles 1 :stagger 0 :spread 0
   ;;        :pan-min -30 :pan-max 30 :tilt-min -100 :tilt-max 100}
   ;;          :whatever "else"
   :effect {:pre ["lala"]
          :transform ["mm"]
          :optional {:wawa "no" :hehe 'disting}
          :function (fn [& args]
                     (prn "do the shit yo"))}}})
;;  {{:can-can }})
;;  ^ bs but like. ks + p are out of fx def.
;;  spec chain of :targets :transform :inputs
;;  since each param aware of its parts easy to automap pan/tilt <-> xyz <-> h s l
;;  if input p/t -> vector -> xyz/hsl
;;     single Number param -> single, or first of available multi

(def cue-data "defaults n stuff for various cue creation fns"
 {:bloom {}})
(def cues "Individual cues"
 {:code
  [[[1 0] "Randomize Cue Vars"
    ;; (fn [s _] (tolglow.random/ize-cue-vars! :show s))]
    (fn [_ _] `(tolglow.random/ize-cue-vars!))]
   [[1 1] "Randomize each bar"
    (fn [s _] `(tolglow.util/hook-var :wavetick-bars tolglow.random/ize-cue-vars!))]
   [[2 0] "Reset Metronome"
    (fn [s _] `(afterglow.rhythm/metro-start (:metronome s) 1))]]})

