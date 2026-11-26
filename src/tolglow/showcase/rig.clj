(ns tolglow.showcase.rig
  "The showcase venue: an 11 x 5 x 8 m black box with a back truss, two side
  booms and a floor package. Positions are metres, Y is up and +Z faces the
  audience. Loading this namespace does no I/O.

  The rig is sized to what the Room preview draws at once: sixteen beams and
  128 emitting targets in all. Sixteen conventional fixtures carry light, and
  80 tube pixels plus a 32-pixel ribbon make up the rest.

  The fixture types restate the operator's own venue definitions as portable
  type data, so the show loads without the original dynamic-show namespaces.
  `tolglow.showcase-parity-test` compares every channel map with what
  `tolglow.config/fixture-types` and `tolglow.fixtures/rgbw-60-moving-beam`
  expand to."
  (:require [tolglow.fixtures.patch :as patch]
            [tolglow.rig.pixel-layout :as pixel-layout]))

(def ^:private strobe-from-10
  [{:start 0 :end 9 :type :no-function} {:start 10 :end 255 :type :strobe}])

(def fixture-types
  "Type ID -> portable fixture type. :source names the original definition.
  Beam angle and lens radius are not in the originals. They are estimates for
  the preview and do not change DMX output."
  {:rgbw-60-moving-beam
   {:id :rgbw-60-moving-beam :version 1 :name "60w Beam MOVING HEAD LIGHT" :footprint 13
    :source "tolglow.fixtures/rgbw-60-moving-beam"
    :channels {:pan [1 2] :tilt [3 4] :movement-speed 5 :dimmer 6
               :shutter {:offset 7
                         :functions [{:start 0 :end 7 :type :shutter-closed}
                                     {:start 8 :end 15 :type :shutter-open}
                                     {:start 16 :end 131 :type :strobe :hz [0.2 10]}
                                     {:start 132 :end 139 :type :shutter-open-2}
                                     {:start 140 :end 181 :type :pulse-strobe}
                                     {:start 182 :end 189 :type :shutter-open-3}
                                     {:start 190 :end 231 :type :pulse-strobe-rev}
                                     {:start 232 :end 239 :type :shutter-open-4}
                                     {:start 240 :end 247 :type :random-strobe}
                                     {:start 248 :end 255 :type :shutter-open-5}]}
               :red 8 :green 9 :blue 10 :white 11 :auto 12 :control 13}
    :calibration {:pan [127 85] :tilt [0 256]}
    :rgbw-policy :extract-min :beam-angle 9 :lens-radius-m 0.06 :max-watts 60}

   :rgbw-7-12-moving
   {:id :rgbw-7-12-moving :version 1 :name "LED MOVING HEAD 7x12W" :footprint 14
    :source "tolglow.config/fixture-data :moving :rgbw-7-12-moving"
    :channels {:pan [1 2] :tilt [3 4] :movement-speed 5 :dimmer 6
               :strobe {:offset 7 :functions strobe-from-10}
               :auto 8 :speed 9 :red 10 :green 11 :blue 12 :white 13 :reset 14}
    :calibration {:pan [127 85] :tilt [0 256]}
    :rgbw-policy :extract-min :beam-angle 22 :lens-radius-m 0.085 :max-watts 84}

   :rgbw-18-12-par
   {:id :rgbw-18-12-par :version 1 :name "18x12 RGBW, 200+ W" :footprint 7
    :source "tolglow.config/fixture-data :wash :rgbw-18-12-par"
    :channels {:dimmer 1 :red 2 :green 3 :blue 4 :white 5
               :strobe {:offset 6 :functions strobe-from-10} :control 7}
    :rgbw-policy :extract-min :beam-angle 44 :lens-radius-m 0.13 :max-watts 216}

   :wa-100-cob-par
   {:id :wa-100-cob-par :version 1 :name "100Watt W/A LED COB PAR Light" :footprint 4
    :source "tolglow.config/fixture-data :cob :wa-100-cob-par"
    :channels {:dimmer 1 :white 2 :amber 3 :strobe {:offset 4 :functions strobe-from-10}}
    :beam-angle 64 :lens-radius-m 0.11 :max-watts 100}

   :af-250-fogger
   {:id :af-250-fogger :version 1 :name "Fogger" :footprint 1
    :source "tolglow.config/fixture-data :other :af-250-fogger"
    :channels {:fog 1}}})

(def room {:width 11 :height 5 :depth 8})

(def trusses [{:id :back-truss :position [0 4.2 -2.0] :length 9.4}])

(def beam-xs [-4.0 -2.4 -0.8 0.8 2.4 4.0])

(def mover-xs [-3.6 -1.2 1.2 3.6])

(def tube-xs (mapv #(+ -4.2 (* % 1.2)) (range 8)))

(def tube-base 0.35)

(def tube-length 1.8)

(def tube-z -3.4)

(def patches
  "Ordered patch groups. Universe 1 carries every conventional fixture."
  {;; The 0..180 degree tilt of these heads only reaches below the horizon
   ;; when the yoke hangs upside down, so the truss heads are rolled over.
   :beam {:universe 1 :offset 1 :type :moving
          :list (mapv (fn [x] [:rgbw-60-moving-beam x 3.95 -2.0 {:rotation-deg [0 0 180]}]) beam-xs)}
   :mover {:universe 1 :offset 101 :type :moving
           :list (mapv (fn [x] [:rgbw-7-12-moving x 0.45 -2.7]) mover-xs)}
   ;; A par points straight down until rotated. Rotation is degrees about X,
   ;; then Y, then Z. The boom pars lean in from the wings and the floor pars
   ;; graze up the back wall.
   :wash {:universe 1 :offset 201 :type :wash
          :list [[:rgbw-18-12-par -5.0 2.7 -0.9 {:rotation-deg [12 0 52]}]
                 [:rgbw-18-12-par 5.0 2.7 -0.9 {:rotation-deg [12 0 -52]}]
                 [:rgbw-18-12-par -2.4 0.22 -3.75 {:rotation-deg [162 0 0]}]
                 [:rgbw-18-12-par 2.4 0.22 -3.75 {:rotation-deg [162 0 0]}]]}
   ;; The blinders face the audience and tip a little below the horizon.
   :blinder {:universe 1 :offset 301 :type :blinder
             :list (mapv (fn [x] [:wa-100-cob-par x 3.75 -2.0 {:rotation-deg [-68 0 0]}]) [-3.2 3.2])}
   :hazer {:universe 1 :offset 401 :type :atmosphere
           :list [[:af-250-fogger -4.9 0.35 -3.4
                   {:fogger {:flow-m3-min 78 :mixing-seconds 3.5 :direction [1 0.15 0.35]}}]]}})

(def group-order [:beam :mover :wash :blinder :hazer])

(def tube-layouts
  "Eight frosted vertical RGBW tubes along the upstage wall, ten pixels each,
  like the operator's ten-pixel pixTol tubes."
  (vec (map-indexed
        (fn [i x]
          {:id (keyword (str "tube-" (inc i))) :shape :path :group :tubes
           :universe 2 :address (inc (* i 40)) :pixels 10 :color-layout :rgbw
           :diffuser-radius-m 0.03 :points [[x tube-base tube-z] [x (+ tube-base tube-length) tube-z]]})
        tube-xs)))

(def ribbon-layout
  "One 32-pixel RGBW ribbon sculpted into a proscenium arch that leans
  upstage at its crown."
  {:id :ribbon :shape :path :group :ribbon
   :universe 3 :address 1 :pixels 32 :color-layout :rgbw :diffuser-radius-m 0.024
   :points (mapv (fn [step]
                   (let [angle (* Math/PI (- 1.0 (/ step 16.0)))
                         rise (Math/sin angle)]
                     [(* 4.7 (Math/cos angle)) (+ 0.3 (* 3.3 rise)) (- -0.5 (* 0.5 rise))]))
                 (range 17))})

(def pixel-layouts (conj tube-layouts ribbon-layout))

(defn fixtures
  "The expanded, validated patch: conventional fixtures, then tubes, then ribbon."
  []
  (into (patch/expand fixture-types {} patches group-order)
        (mapcat pixel-layout/fixtures)
        pixel-layouts))

(defn rig []
  {:room room :trusses trusses :fixtures (fixtures) :pixel-layouts pixel-layouts})

(defn- ids [group n] (mapv #(keyword (str (name group) "-" (inc %))) (range n)))

(def beams (ids :beam (count beam-xs)))

(def movers (ids :mover (count mover-xs)))

(def washes (ids :wash 4))

(def blinders (ids :blinder 2))

(def hazer :hazer-1)

(defn- heads [layout]
  (mapv #(keyword (str (name (:id layout)) "-head-" (inc %))) (range (:pixels layout))))

(def tubes (into [] (mapcat heads) tube-layouts))

(def ribbon (heads ribbon-layout))

(def pixels (into tubes ribbon))

(def movement (into beams movers))

(def coloured
  "Every head with red, green and blue emitters."
  (-> beams (into movers) (into washes) (into pixels)))
