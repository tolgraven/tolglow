(ns tolglow.live
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
             [dimmer :as dimmer :refer [dimmer-effect master-set-level]]
             [fun :as fun]
             [movement :as move :refer [pan-tilt-effect]]
             [oscillators :as lfo :refer [build-oscillated-param sawtooth sine square triangle]]
             [params :as params :refer [bind-keyword-param build-aim-param build-color-param build-direction-param-from-pan-tilt build-pan-tilt-param build-param-formula build-spatial-param build-step-param frame-dynamic-param? param? resolve-param validate-param-type]]
             [show-variable :as var-fx :refer [variable-effect]]]
            [tolglow
             [fx :as tolfx :refer :all]
             [color :as color :refer []]
             [param :as param :refer [quick-lfo mix sum sub div mul avg average]]
             [util :as util :refer [value avar]]]))


(defn live-fn-2 "test"
 [s show snap]
 (let [all (all-fixtures)
       as [(confetti all :aim? true)
           (color :blue :htp? true)
           (sparkle all :chance 0.2)]]
  (mapcat #(call % show snap) as)))

(when *show*
(def st (build-step-param :interval-ratio 4 :fade-fraction 0.5))
(def st2  (build-step-param :interval-ratio 8 :fade-fraction 0.25))
(def sin (quick-lfo "sine"))
(def sinmul (mix [sin (quick-lfo "triangle" :beats 3)] sum :min -40 :max 40 :normalize? true))
(def sinmo (mix [sinmul (quick-lfo "sine" :beats 5)] sub :min -10 :max 10 :normalize? true))
(def pur (build-color-param
          :color (color/like :purple)
          :adjust-lightness (quick-lfo "triangle" :beats 2 :low -10 :high 20)))
(def ptp1 (build-pan-tilt-param :tilt sinmo :pan sinmul #_sin))
(def ptp2 (build-pan-tilt-param :tilt sinmul :pan #_sinmo sin))
(defn live-fn "test" ;one of many issues here obviously: we're recreating each frame haha.
 [s show snap] ;;  [s & showsnap]    ;but concept should still be alright for fx dev work, having fn always running
 (let [all (all-fixtures)            ;shouldve always been method tho, (add-effect :dev (fn [])) etcetc. But timing aspects interesting.
       lgs (mapv #(fixtures-named %) [:moving :moving-mini :wash :strip]) ;XXX duh, diff live loops/sequencers per fixtures/groups
       as [(color pur :fixtures (lgs 2) #_:purple)
           (move/pan-tilt-effect "eh" (params/build-pan-tilt-param :tilt sinmo :pan sinmul #_sin) all)
           ;; (sparkle all :chance 0.1)
           (chase "split" [;(sparkle all :chance (rand) :fade 5000)
                           (color :black)
                           ;; (color :purple)
                           ;; (color :orangered2 :fixtures (lgs (int (rand 4)))) ;triggers each frame so bit crazy :/ state + draw sep it is then...
                           ;; (show/add-effect-from-cue-grid! (int (rand 1)) (int (rand 8))) ;filter by held. also make a *GET* effect from grid! not add straight
                           (pan-tilt-effect "eh" ptp1 (lgs 0))
                           (pan-tilt-effect "ah" ptp2 (lgs 1))
                           ]
                  st :beyond :loop)
           ]]
  (mapcat #(call % show snap) as))
;;  ((:gen-fn (color :green)) show snapshot)
 ;; so in env like this want easy shortcuts to lots of stuff
 ;; idea is what, put into chase etc, have a bunch, keep and running and changing code?
 ;; issue is how to get raw assigners...
 )


(add-effect! :dynch (chase "Chase Test Dynamic"
                                 [(tolfx/live (tolfx/pass-live-fn live-fn))
                                  #_(tolfx/live (tolfx/pass-live-fn live-fn-2))]
                                st2 :beyond :loop))
             ;; :color :green :priority 10000))
)
