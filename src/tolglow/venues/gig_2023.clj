(ns tolglow.venues.gig-2023
  "The rig from the operator's last real gig (August 2023, re-edited July
  2024), restated from the uncommitted legacy `tolglow/config.clj` edits kept in
  issue #720. Positions are metres, Y is up and +Z faces the audience. Loading
  this namespace builds data and opens nothing.

  Legacy coordinates were measured from the main truss: the stage wall stood
  2 m behind it and the rear wall 6 m in front. The Room preview centres the
  room on the origin, so every legacy Z is moved 2 m upstage here. The other
  corrections to the legacy data are listed in `docs/gig-2023-venue.md`.

  `tolglow.venues.gig-2023-parity-test` checks the fixture types against the
  legacy channel maps and every patched channel against the DMX address the
  legacy group patcher gave it.

      (require '[tolglow.venues.gig-2023 :as gig] '[tolglow.show :as show])
      (def runtime (show/start! {:model (gig/model)}))
      (show/stop! runtime)"
  (:require [afterglow.control.model :as control-model]
            [clojure.java.io :as io]
            [tolglow.data :as data]
            [tolglow.fixtures.patch :as patch]
            [tolglow.registry :as registry]
            [tolglow.showcase.rig :as showcase-rig]))

(defn- thunder-wash-strobe
  "Strobe channel of the Thunder Wash 600 in its 6- and 7-channel 2 modes.
  The manuals rate the strobe 0-30 Hz (RGB) and 1-30 Hz (RGBW)."
  [hz]
  {:offset 2
   :functions [{:start 0 :end 5 :type :shutter-open}
               {:start 6 :end 10 :type :shutter-closed}
               {:start 11 :end 250 :type :strobe :hz hz}
               {:start 251 :end 255 :type :shutter-open-2}]})

(def fixture-types
  "Type ID -> portable fixture type. :source names where each channel map
  comes from. Beam angles and wattage come from the manuals where cited; lens
  radii and the HY12x12 optics are estimates for the preview and do not change
  DMX output."
  {:cameo-thunder-wash-600-rgb
   {:id :cameo-thunder-wash-600-rgb :version 1 :footprint 6
    :name "Cameo Thunder Wash 600 RGB (6-channel 2)"
    :source (str "Cameo CLTW600RGB manual (CLTW100W/CLTW100RGB/CLTW600W/CLTW600RGB, "
                 "DMX control, 6-CH 2 Mode) and OFL cameo/thunder-wash-600-rgb mode 6ch2; "
                 "legacy tolglow.config :cameo-thunder-wash")
    :channels {:dimmer 1 :strobe (thunder-wash-strobe [0 30]) :strobe-duration 3
               :red 4 :green 5 :blue 6}
    :beam-angle 75 :lens-radius-m 0.13 :max-watts 110}

   :cameo-thunder-wash-600-rgbw
   {:id :cameo-thunder-wash-600-rgbw :version 1 :footprint 7
    :name "Cameo Thunder Wash 600 RGBW (7-channel 2)"
    :source (str "Cameo CLTW600RGBW manual REV 04 (DMX control, 7-CH Mode_2) and OFL "
                 "cameo/thunder-wash-600-rgbw mode 7ch2; legacy tolglow.config "
                 ":cameo-thunder-wash-rgbw")
    :channels {:dimmer 1 :strobe (thunder-wash-strobe [1 30]) :strobe-duration 3
               :red 4 :green 5 :blue 6 :white 7}
    :rgbw-policy :extract-min :beam-angle 104 :lens-radius-m 0.13 :max-watts 160}

   ;; Not verified against a manufacturer table: no manual or OFL/QLC+
   ;; definition of this head's 15-channel mode was found. The map restates
   ;; the operator's legacy :rgbw-60-mh exactly. Channels 5 and 12-15 were
   ;; declared as unused dummies there, so the footprint reserves them and
   ;; nothing writes to them.
   :rgbw-60-moving-beam-15ch
   {:id :rgbw-60-moving-beam-15ch :version 1 :footprint 15
    :name "60w Beam MOVING HEAD LIGHT (15-channel, unverified)"
    :source (str "legacy tolglow.config :rgbw-60-mh (unverified: no manufacturer "
                 "15-channel table found; pan, tilt, dimmer, shutter and RGBW match the "
                 "13-channel tolglow.fixtures/rgbw-60-moving-beam)")
    :channels {:pan [1 2] :tilt [3 4] :dimmer 6
               :shutter {:offset 7
                         :functions [{:start 0 :end 7 :type :shutter-closed}
                                     {:start 8 :end 15 :type :shutter-open}
                                     {:start 16 :end 255 :type :strobe :hz [0.2 10]}]}
               :red 8 :green 9 :blue 10 :white 11}
    :calibration {:pan [127 85] :tilt [0 256]}
    :rgbw-policy :extract-min :beam-angle 9 :lens-radius-m 0.06 :max-watts 60}

   :rgbw-12-12-moving
   {:id :rgbw-12-12-moving :version 1 :name "HY12x12 MOVING HEAD" :footprint 16
    :source "tolglow.config/fixture-data :moving :rgbw-12-12-moving"
    :channels {:pan [1 14] :tilt [2 15] :dimmer 3 :red 4 :green 5 :blue 6 :white 7
               :strobe {:offset 8 :functions [{:start 0 :end 9 :type :no-function}
                                              {:start 10 :end 255 :type :strobe}]}
               :movement-speed 9 :auto 10 :speed 11 :control 12 :sound-sensitivity 13
               :reset 16}
    :calibration {:pan [102 73] :tilt [0 256]}
    :rgbw-policy :extract-min :beam-angle 25 :lens-radius-m 0.09 :max-watts 144}

   :rgbw-60-moving-beam (:rgbw-60-moving-beam showcase-rig/fixture-types)

   :rgbw-7-12-moving (:rgbw-7-12-moving showcase-rig/fixture-types)})

(def room
  "Width from the gig's +-5 m pointing range (its walls were never updated),
  height to the ceiling and depth between the stage and rear walls."
  {:width 10 :height 4 :depth 8})

(def stage-shift
  "Metres added to every legacy Z so the legacy room [-2, 6] spans [-4, 4]."
  -2.0)

(defn- at [x y z] [x y (+ z stage-shift)])

(def trusses
  "The legacy main truss: 2.5 m high at legacy Z 0."
  [{:id :main-truss :position (at 0 2.5 0) :length 7.0}])

(def wash-xs
  "The eight RGBW washes, evenly spread across the room in legacy patch order
  (the legacy X positions -6 to 10 span 16 m)."
  [-4.2 -3.0 -1.8 -0.6 0.6 1.8 3.0 4.2])

(def patches
  "Ordered patch groups, all on universe 1. Group names and list order are the
  legacy ones, so fixture IDs match the legacy show."
  {:moving {:universe 1 :offset 1 :type :moving
            :list [[:rgbw-12-12-moving (at 1.5 1.2 -0.5) {:rotation-deg [0 180 0]}]
                   [:rgbw-60-moving-beam (at 0.2 0.2 0.0)]]}
   :moving-mini {:universe 1 :offset 100 :type :moving
                 :list [[:rgbw-7-12-moving (at -3.0 3.0 0.0)]
                        [:rgbw-7-12-moving (at 0.0 3.0 0.0)]
                        [:rgbw-7-12-moving (at 3.0 3.0 0.0)]
                        [:rgbw-7-12-moving (at 0.2 0.0 -2.0) {:rotation-deg [0 180 0]}]]}
   :wash {:universe 1 :offset 301 :type :wash
          :list (mapv (fn [x] [:cameo-thunder-wash-600-rgbw (at x 4.0 0.0)]) wash-xs)}
   :wash-2 {:universe 1 :offset 357 :type :wash
            :list [[:cameo-thunder-wash-600-rgb (at 0.0 4.0 0.0)]
                   [:cameo-thunder-wash-600-rgb (at -0.5 4.0 0.5)]]}})

(def group-order [:moving :moving-mini :wash :wash-2])

(defn- positioned
  "Turn [type [x y z] options] into the patch form [type x y z options]."
  [patches]
  (update-vals patches
               (fn [group]
                 (update group :list
                         (partial mapv (fn [[type [x y z] options]]
                                         (cond-> [type x y z] options (conj options))))))))

(def fixture-patches (positioned patches))

(defn fixtures
  "The expanded, validated patch."
  []
  (patch/expand fixture-types {} fixture-patches group-order))

(defn rig []
  {:room room :trusses trusses :fixtures (fixtures)})

(def fixtures-config
  "The `:fixtures` module config: publishes the types, then patches the groups.
  It does not set the room; start with `model` or `config` for that."
  {:definitions (vec (vals fixture-types)) :patches fixture-patches :groups group-order})

(def output-response
  "The faster launch and release used at the gig (legacy defaults 600 and
  1000 ms). See docs/output-response.md."
  {:global {:attack {:mode :linear :ms 150} :release {:ms 200}}})

(defn model
  "A show model with the gig rig and its fixture types published."
  []
  (reduce #(update %1 :registry registry/publish :fixtures %2)
          (control-model/new-show {:show-id :gig-2023 :rig (rig)})
          (vals fixture-types)))

(def config
  "Application config for `lein run --config tolglow.venues.gig-2023/config`:
  the shipped defaults (a 40 Hz preview show, Studio on loopback port 8500, no
  physical output) with this rig and the gig's output response."
  (-> (data/read-edn (slurp (io/resource "tolglow.edn")))
      (assoc :description "Tolglow 2023 gig rig"
             :show {:model (model)}
             :output-response (assoc output-response :enabled true))))
