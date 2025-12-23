(ns tolglow.graph)
; (ns tolglow.graph
;   (:require [afterglow.effects :as fx]
;             [afterglow.effects.params :as params]
;             [afterglow.effects.oscillators :as lfo]
;             [afterglow.show :as show]
;             [afterglow.show-context :refer [with-show *show*]]
;             [afterglow.rhythm :as rhythm]
;             [afterglow.transform :as tf]
;             [afterglow.util :as util]

;             [incanter.core :refer :all]
;             [incanter.stats :refer :all]
;             [incanter.charts :refer :all]
;             [incanter.io :refer :all]
;             [incanter.svg :refer [save-svg]])
;   (:import [afterglow.rhythm MetronomeSnapshot]))

; (defn snapshot-metro-ms "Creates a metronome snapshot representing the specified number of milliseconds after the supplied metronome was started."
;   [metro offset]
;   (rhythm/metro-snapshot metro (+ offset (rhythm/metro-start metro))))

; (defn snapshot-metro-timescale "Create a snapshot that represents the specified number of beats after the creation of the supplied metronome."
;   [metro beats]
;   (snapshot-metro-ms metro (* beats (rhythm/metro-tick metro))))

; (defn param "Graff a param"
;   [title param & {:keys [metro steps unit] :or {metro (:metronome *show*), steps 4, unit :beats}}]
;   (when *show*
;     (let [f (fn [x]
;               (let [snapshot (snapshot-metro-timescale metro x)]
;                 (params/resolve-param param *show* snapshot)))
;           plot (function-plot f 0 steps :x-label (name unit) :y-label "value" :title title)]
;       (view plot))))
