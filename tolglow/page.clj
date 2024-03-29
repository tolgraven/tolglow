(ns tolglow.page "Build cue pages of various types" {:author "Joen Tolgraven"}
  (:require
   [afterglow
     [channels :as chan :refer [expand-heads extract-channels find-rgb-heads]]
     [effects :as fx :refer [chase scene]]
     [rhythm :as rhythm :refer [metro-snapshot metro-start metronome snapshot-bar-phase snapshot-beat-phase snapshot-beat-within-bar snapshot-down-beat?]]
     [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]
     [show-context :refer [*show* set-default-show! with-show]]]
    [afterglow.effects
     [color :as color-fx :refer [color-effect transform-colors]]
     [cues :as cues :refer [apply-merging-var-map code-cue color-fn-from-cue-var cue function-cue]]
     [dimmer :as dimmer :refer [dimmer-effect master-set-level]]
     [fun :as fun]
     [movement :as move]
     [oscillators :as lfo :refer [build-oscillated-param sawtooth sine square triangle]]
     [params :as params :refer [bind-keyword-param build-aim-param build-color-param build-direction-param-from-pan-tilt build-pan-tilt-param build-param-formula build-spatial-param build-step-param frame-dynamic-param? param? resolve-param validate-param-type]]
     [show-variable :as var-fx :refer [variable-effect]]]
    [com.evocomputing.colors :as colors :refer [adjust-hue color-name create-color darken desaturate hue lighten lightness saturate saturation]]
    [tolglow
     [color :as color]
     [config :as config :refer [at cfg ptr-cfg venue wall wall-x]]
     [cue :as cue :refer [group-parts]]
     [fx :as tolfx :refer [color-transformer sweep]]
     [param :as param :refer []]
     [random :as random]
     [util :as util :refer [add-midi-callback apply-vm avar clamp-number get-channels get-map-with key-str random-in-range space-phase value x-phase]]
     [vars :as vars]])
  (:import javax.media.j3d.Transform3D
           [javax.vecmath Point3d Vector3d]))

(def ledstrips (at :fixture-type :strip))
(def moving-heads (at :fixture-type :moving))
(def all-groups (cfg :fixtures :groups))

(defn create "Wrap builder functions"
 [fn-key page-x page-y & args]
 (let [x (* page-x 8)
       y (* page-y 8)
       f (resolve (symbol "tolglow.page" (name fn-key)))
       run #(apply f x y args)] ;XXX first try straight then page I guess?
  (if (cfg :debug :force-cue-pages) #_true
   ;; (util/catchall run #(println "Exception creating cue page" (name %1) "\n" %2))
   (util/catchall run)
   (run))))

(defn reload
 [& {:keys [origin-page page-data] :or {origin-page 0, page-data (cfg :pages)}}] ; prob have main page/startup at like 2 2 so plenty space in all directions...
 (doseq [[fn-key pages] page-data
         [xb yb & opts] (if (set? pages) pages #{pages})] ;XXX or use whatever :while alternative binding thing...
   (let [[x y] (map #(+ origin-page %) [xb yb])]
    (print "\n" (name fn-key) "\t" x y (or opts "")) ;log
    (apply create (name fn-key) x y opts))) )

(defn far "Top/far end of page"
 [yb]
 (+ yb 7))

(defn dummy
 [xb yb & {:keys [groups]}]
 (println xb yb groups))



 (defn main-dimmer "Strobe cues at bottom, dimmer param cues above"
  [xb yb & {:keys [groups globals? color?] :or {globals? true}}]
  (let [groups (or groups (into [nil] (cfg :fixtures :groups)))] ; nil resolves to all fixtures when passed to helper functions

   (doall (map-indexed ; Strobe cues
           (fn [i group]
             (cue/strobe group (+ xb i) (+ yb 0) :velocity-target :level))
           groups))

   (let [color-var {:key :strobe-color, :type :color, :name "Strobe Color"} ;]
         ;; level-var (do (set-variable! :strobe-level 80.0)
                    #_(vars/cue-map [:strobe-level 80.0 0.0 100.0]);)
         ]
    (set-cue! (+ xb 7) (+ yb 0)
              (cue :strobe-color (fn [_] (fx/blank "Strobe Color"))
                   :color :purple, :color-fn (cues/color-fn-from-cue-var color-var)
                   :variables [color-var #_level-var])))

   (cue/param-page groups xb (+ yb 1) dimmer-effect)

   (when color?
    (doall (map-indexed
            (fn [i group]
             (cue/color group (+ xb i) (+ yb 7) nil, :color (color/like :royalblue)
                        :priority (if group 1 0)))
            groups)))

   (when globals?
    (cue/dimmer nil (+ xb 7) (+ yb 1) :white, :priority 11000) ; ALL OVERRIDE PRIO
    (cue/param-column nil (+ xb 7) (+ yb 1) dimmer-effect :args [:priority 11000]))))



(defn effects
 [xb yb]
 (let [rig-width (- (wall :right) (wall :left))
       ledstrips (or @ledstrips (mapcat fixtures-named [:strip :tube]))]

 (cue/code (+ xb 1) (+ yb 0) (fn [show _] (tolglow.random/ize-cue-vars! :show *show*)) "Randomize Cue Vars")
 (cue/code (+ xb 1) (+ yb 1) (fn [show _] (tolglow.util/hook-var :wavetick-bars tolglow.random/ize-cue-vars!)) "Randomize each bar")
;;  (cue/code (+ xb 1) (+ yb 1) (fn [show _] (tolglow.util/add-midi-callback try launching straight too))) "Randomize each bar")
 (cue/code (+ xb 2) (+ yb 0) (fn [_ _] (afterglow.rhythm/metro-start (:metronome *show*) 1)) "Reset Metronome")
 (cue/code (+ xb 3) (+ yb 0) (fn [_ _] (reload)) "Reload Cues")
 (cue/code (+ xb 3) (+ yb 1) (fn [_ _] (afterglow.show/clear-effects!)) "Clear FX")
 (cue/code (+ xb 3) (+ yb 2) (fn [_ _] (tolglow.util/clear!)) "Clear errthin")

 (when ledstrips ;XXX should extract all funky ctrl chs and automatically map out
  ;; (set-cue! (+ xb 2) (+ yb 7)
  ;;   (cue :strip-settings ;XXX how do this? stack multiple channel effects in one cue...
  ;;     (fn [var-map] (apply scene "Strip control"
  ;;                    (into [] (map #(channel-effect % (get-channels ledstrips %),
  ;;                    :effect-name (str (capitalize (name %)) " strip"))
  ;;                                  [:noise :bleed :attack :release]))))
  ;;     :variables (map #(vars/cue-map [%1 0 0 255]) [:noise :bleed :attack :release])))

  ;; (let [color (vars/cue-map ["color" (color/s -30 (color/l 10 :purple))])
  ;; (let [color (vars/cue-map ["color" "blue"])
  ;;       variables (vars/auto :fade-time color :beats :cycles (vars/min-max 0.0 0.2 0.0 0.1) :lfo-gain :lfo-offset)]
  ;;  (set-cue! (+ xb 7) (+ yb 7)
  ;;   (cue :tol-sparkle (fn [vm] (apply-vm vm tolfx/sparkle (all-fixtures) :chance (param/auto-vm vm "sawtooth")))
  ;;        :held false, :priority 400, :color (:start color), :color-fn (color-fn-from-cue-var color)
  ;;        :visualizer #(param/lfo-viz %1 %2 "sawtooth" (:max max))
  ;;        :variables variables)))
  ;;
  ;; (let [color (vars/cue-map ["color" (color/s -20 (color/h 30 :purple))])]
  ;;  (set-cue! (+ xb 7) (+ yb 6)
  ;;   (cue :sparkle (fn [vm] (apply-vm vm fun/sparkle (all-fixtures)))
  ;;        :held true, :priority 100, :color (:start color), :color-fn (color-fn-from-cue-var color)
  ;;        :variables [{:key "chance",    :min 0.0, :max 0.2,  :start 0.0000}
  ;;                    {:key "fade-time", :min 1,   :max 2000, :start 20, :type :integer, :velocity true, :name "Fade"}
  ;;                    color])))
  ;;
  ;; (let [color (vars/cue-map ["color", (color/s (color/like :red) -30)])]
  ;;  (set-cue! (+ xb 7) (+ yb 5)
  ;;   (cue :sparkle-2 (fn [vm] (apply-vm vm fun/sparkle (all-fixtures) :chance (param/rng :min 0 :max 0.2)))
  ;;        :held false, :priority 2000, :color "red", :color-fn (color-fn-from-cue-var color)
  ;;        :variables [{:key "chance",    :min 0.0, :max 0.15,  :start 0.0001, :velocity false}
  ;;                    {:key "fade-time", :min 1,   :max 2500, :start 200, :type :integer, :name "Fade"}
  ;;                    color])))
  ;;
  ;; (let [color (vars/cue-map ["color", (color/l (color/like :blue) -20)])]
  ;;  (set-cue! (+ xb 7) (+ yb 4)
  ;;   (cue :sparkle-3 (fn [vm] (apply-vm vm fun/sparkle (all-fixtures)))
  ;;        :held true, :priority 20000, :color "blue", :color-fn (color-fn-from-cue-var color)
  ;;        :variables [{:key "chance",    :min 0.0, :max 0.15,  :start 0.0001, :velocity true}
  ;;                    {:key "fade-time", :min 1,   :max 200, :start 10, :type :integer, :name "Fade"}
  ;;                    color])))
  ;; BLOOM

  ;; (cue/sparkle nil (+ xb 7) (+ yb 6) (color/like :green))
  (cue/sparkle nil (+ xb 7) (+ yb 6) (color/like (color/s -20 (color/h 30 :purple))))
  (cue/sparkle nil (+ xb 7) (+ yb 5) (color/like (color/s -30 (color/like :red))))
  (cue/sparkle nil (+ xb 7) (+ yb 4) (color/like (color/l -20 (color/like :blue))))

  ;; BLOOM
  ;; XXX mixing lfos def the shit for blooms etc if nothing else...
  (cue/bloom nil (+ xb 0) (+ yb 7) (color/like :green))
  (cue/bloom nil (+ xb 0) (+ yb 6) (color/like :blue) :keyhole? true)
  (cue/bloom :tube (+ xb 0) (+ yb 5) (color/like :blue) :param-type "sawtooth")
  (cue/bloom nil (+ xb 0) (+ yb 4) (color/like :orange) :halo? true)
  (cue/bloom nil (+ xb 0) (+ yb 3) (color/like :yellow) :mods? true)
  ;; (cue/bloom nil (+ xb 0) (+ yb 2) (color/like :orange) :mods? true :keyhole? true)
  (cue/bloom nil (+ xb 0) (+ yb 1) (color/like :purple) :halo? true :param-type "held")
  (cue/bloom nil (+ xb 0) (+ yb 0) (color/like :black))

  (def ugh true)
  (when ugh (let [m ((cfg :measure :default))]
   (set-cue! (+ xb 6) (+ yb 7)
             (cue :bloom
                  (fn [vm] (apply-vm vm tolfx/bloom (all-fixtures) :measure m :keyhole? true))
                  :variables (vars/auto :color :width :fraction-vel)
                  :held true, :priority 1000, :color :purple))
   ;; linked
   ;; (set-cue! (+ xb 6) (+ yb 6)
   ;;           (cue :bloom-link2
   ;;                (fn [vm]
   ;;                 (fun/bloom (all-fixtures) :measure m :fraction (param/auto-vm vm "triangle") :color (:color vm)))
   ;;                :variables (vars/auto :beats :cycles :min-num :max-num (vars/cue-map ["color" "orangered"]))
   ;;                :held false, :priority 1001, :color :red))

   (set-cue! (+ xb 6) (+ yb 5)
             (cue :bloom-strip
                  (fn [vm] (apply-vm vm tolfx/bloom (all-fixtures) :measure m))
                  :variables (vars/auto :color :fraction-vel)
                  :held true, :priority 1000, :color :purple))
   ;; linked
   (set-cue! (+ xb 6) (+ yb 4)
             (cue :bloom-link2-strip
                  (fn [vm]
                   (let [fraction (param/auto-vm vm "triangle")] ;(build-oscillated-param (triangle :interval-ratio (ratio-param vm)) :max (:max vm), :min (:min vm))]
                    (apply-vm vm tolfx/bloom ledstrips :measure m :fraction fraction)))
                  :variables (vars/auto :beats :cycles :min-num :max-num
                                        (vars/cue-map ["color" (color/l 10 (color/like "orange"))]))
                  :held false, :priority 1001, :color :orange))


   (set-cue! (+ xb 6) (+ yb 0)
             (cue :bloom-tol ;XXX velocity cue, not held but one-off, not continous effect. slim bloom slipping across strips, diff speed/hue? from velocity
                  (fn [vm]
                   (let [fraction (param/auto-vm vm "sine")] ;(build-oscillated-param (sine :interval-ratio (ratio-param vm)) :max (:max vm), :min (:min vm))] ;
                    (apply-vm vm tolfx/bloom (all-fixtures) :measure m :fraction (param/rng 0 0.5) :width 0.2)))
                  :variables (vars/auto :beats :cycles :min-num :max-num
                                        (vars/cue-maps [["color" "mediumpurple"] ["hue-mod" 50 -180 180]
                                                        ["lightness-mod" -5 -50 50] ["saturation-mod" 10 -50 50]]))
                  ;; :visualizer (fn [vm show] ;XXX make generic build-visualizer function
                  ;;                 ;; (let [p (build-oscillated-param (sine :interval-ratio (param/ratio vm)) :max (:max vm), :min (:min vm))]
                  ;;                 (let [p (param/auto-vm vm "sine")]
                  ;;                   (fn [snapshot] (params/evaluate p show snapshot nil) )))
                  :short-name "Bloom mod", :priority 2001, :color :seagreen))

   (set-cue! (+ xb 6) (+ yb 1)
             (cue :bloom-stripe
                  (fn [vm] (let [fraction (param/auto-vm vm "sine")]
                            (apply-vm vm tolfx/bloom ledstrips :measure m :fraction fraction)))
                  :variables (vars/auto (vars/cue-map ["color" "navajowhite3"]) :fraction-vel (vars/alt-start :width 0.1)
                                        :hue-mod :lightness-mod :saturation-mod :beats :cycles :min-num :max-num)
                  :short-name "Bloom stripe", :priority 1002, :color :orangered))

   ;; (set-cue! (+ xb 6) (+ yb 2)
   ;;   (cue :bloom-stripe-2
   ;;              (fn [vm]
   ;;                 (let [fraction (param/auto-vm vm "sine")]
   ;;                 (tolfx/effect (apply-vm vm tolfx/bloom (all-fixtures) :measure m :fraction fraction
   ;;                                        :width (param/rng :min 0.02 :max 0.2 :min-change 0.05)) fraction)))
   ;;             :variables (vars/auto :beats :cycles :min-num :max-num :hue-mod :lightness-mod :saturation-mod
   ;;                                  (vars/cue-map ["color" "seagreen"]))
   ;;             :short-name "Bloom stripe 2", :priority 1012, :color :orange))

   ;; (set-cue! (+ xb 7) (+ yb 0)
   ;;   (cue :bloom-stripe-held
   ;;              (fn [vm] (apply-vm vm tolfx/bloom ledstrips :measure m))
   ;;             :variables (vars/auto (vars/cue-map ["color" "mediumpurple"]) :fraction-vel (vars/alt-start :width 0.15)
   ;;                                  :hue-mod :lightness-mod :saturation-mod)
  ;;             :short-name "Bloom stripe held", :held true :priority 3012, :color :yellow))
  ;;
  ;; (set-cue! (+ xb 7) (+ yb 1)
  ;;   (cue :bloom-stripe-held-2
  ;;              (fn [vm] (apply-vm vm tolfx/bloom ledstrips :measure m))
  ;;             :variables (vars/auto (vars/cue-map ["color" "navajowhite3"]) :fraction-vel (vars/alt-start :width 0.10)
  ;;                                  :hue-mod :lightness-mod :saturation-mod)
  ;;             :short-name "Bloom stripe held 2", :held true :priority 3012, :color :yellow))
  ;;
  (set-cue! (+ xb 7) (+ yb 2)
    (cue :bloom-keyhole
         (fn [vm]
           (let [fraction (build-oscillated-param (sine :interval-ratio (param/ratio vm)) :max (:max vm), :min (:min vm)) #_(param/auto-vm vm "sine")]
            (apply-vm vm tolfx/bloom  (all-fixtures) :measure m :fraction fraction :keyhole? true)))
      :variables (vars/auto :beats :cycles :max-num :min-num (vars/cue-map ["width" 0.3 0.0 1.0]))
      :short-name "Bloom keyhole", :priority 2001, :color :green))

  ;; (set-cue! (+ xb 7) (+ yb 3)
  ;;   (cue :bloom-keyhole-held
  ;;              (fn [vm] (apply-vm vm tolfx/bloom  ledstrips :measure m :keyhole? true))
  ;;     :variables [{:key "fraction", :min 0, :max 1, :start 0, :velocity true}
  ;;                 {:key "width", :min 0.0, :max 1.0, :start 0.3, :velocity true, :velocity-max 0.5, :name "Width"}]
  ;;     :short-name "Bloom keyhole", :held true, :priority 2001, :color :lightgreen))
  ;;
  ;; reverse
  ;; (set-cue! (+ xb 5) (+ yb 7)
  ;;   (cue :bloom-reverse
  ;;        (fn [vm] (apply-vm vm fun/bloom (all-fixtures) :color (color/create :black) :measure m))
  ;;        :variables (vars/auto :fraction-vel)
  ;;        :held true, :priority 1002, :color :black))
  ;;
  ;; ;; auto, reverse
  (set-cue! (+ xb 5) (+ yb 6)
            (cue :bloom-link
            (fn [vm]
              (let [fraction (build-oscillated-param (sine :interval-ratio (param/ratio vm)) :max (:max vm) :min (:min vm))] ;(param/auto-vm vm "sine")
              ;; (let [fraction (param/auto-vm vm "sine")] ;
               (apply-vm vm fun/bloom (all-fixtures) :fraction fraction :color (color/create :black) :measure m)))
            :variables (vars/auto :beats :cycles :max-num :min-num :phase)
   :priority 19999, :color :black))

  ;; (set-cue! (+ 0 5) (+ 0 5)
  ;;           (cue :bloom-reverse-strip
  ;;                (fn [vm] (apply-vm vm fun/bloom (all-fixtures) :color (color/create :black) :measure m))
  ;;    :variables [{:key "fraction", :min 0, :max 1, :start 0.0, :velocity true}]
  ;;    :held true, :priority 1002, :color :black))
  ;;
  ;; (set-cue! (+ xb 5) (+ yb 4)
  ;;           (cue :bloom-link-strip
  ;;             (fn [vm]
  ;;               (let [fraction (param/auto-vm vm "sawtooth")]
  ;;                (apply-vm vm fun/bloom ledstrips :fraction fraction :color (color/create :black) :measure m)))
  ;;                :variables (vars/auto :beats :cycles :max-num :min-num :down)
  ;;    :priority 1999, :color :black))

  (set-cue! (+ xb 5) (+ yb 0)
     (cue :bloom-reverse-tol
               (fn [vm]
                 (apply-vm vm tolfx/bloom (all-fixtures) :color (color/create :black) :measure m))
                 :variables (vars/auto :fraction-vel :width)
                 :held true, :priority 1002, :color :black))

  (set-cue! (+ xb 5) (+ yb 1)
   (cue :bloom-link-strip-tol
            (fn [vm]
              (let [fraction (param/auto-vm vm "sawtooth")]
               (apply-vm vm tolfx/bloom ledstrips :fraction fraction :color (color/create :black)  :measure m)))
            :variables (vars/auto :beats :cycles :max-num :min-num :down)
   :priority 1999, :color :black))))


  (cue/confetti nil (+ xb 5) (+ yb 2) :red)

  (cue/pinstripe nil (+ xb 4) (+ yb 3) ["blue" "red"])
  (cue/pinstripe nil (+ xb 4) (+ yb 4) ["indianred" "khaki" "cadetblue"])
  (cue/pinstripe nil (+ xb 4) (+ yb 5) ["lightpink1" "orchid2" "white" "lightsalmon1"]))))


(defn chase-fn
 [vm & {:keys [param-type pad-between?]}]
 (chase "Chase osc"
        [(fx/blank)
         (tolfx/color :black)
   ;; (fun/bloom (all-fixtures)) ; :fraction (rand 1.0))
         (fx/blank)
         (tolfx/global-dimmer-effect :black)
         (tolfx/color :seagreen :fixtures (fixtures-named "wash"))
         (tolfx/color :black)
         (fx/blank)
         (tolfx/color :black)]
        (param/auto-vm vm "sine")
        #_(build-oscillated-param (sine :interval-ratio (param/ratio var-map) :phase (:phase var-map)) :min (:min var-map) :max (:max var-map))))

;; {{{ EFFECT CUES 2
(defn effects-2
 [xb yb]
 (let [rig-width (- (wall :right) (wall :left))
       steps (at :step)
       blank (fx/blank)
       blackout (tolfx/color :black :priority 10000) ;XXX actually lacks prio key hahah, fix

       desat-beat  (param/auto-vm nil "sawtooth" :max 80)  ; Desaturate a color as a beat progresses
       desat-bar   (build-oscillated-param  (sawtooth :down? true :interval :bar) :max 80)  ; Desaturate a color as a bar progresses
       hue-bar     (build-oscillated-param  (sawtooth :interval :bar) :min 200 :max 260)  ; Spread a gradient across a bar of music
       hue-grad    (build-spatial-param  (all-fixtures) (fn [head] (- (:x head) (:min-x @(:dimensions *show*)))) :min 200 :max 260) ; Spread a gradient across the light grid
       rig-hue-gradient (build-spatial-param ; Spread a rainbow across just the main rig, repeating beyond that, irrespective of other lights' positions.
                         (all-fixtures) (fn [head] (colors/clamp-hue
                                                    (* 360 (/ (- (:x head) (wall :left)) rig-width)))))
       rainbow-sat (vars/cue-map [:rainbow-sat 40 10 85] :var-name "Saturation")]

   (set-variable! :rainbow-sat 30)
   (set-cue! (+ xb 2) (+ yb 0)
             (let [color-param (build-color-param :s :rainbow-sat, :l 55, :h hue-bar)]
               (cue :all-color (fn [_] (tolfx/color color-param))
                    :color-fn (cues/color-fn-from-param color-param)
                    :short-name "Rainbow Bar Fade"
                    :variables [rainbow-sat])))
   (set-cue! (+ xb 2) (+ yb 1)
             (let [color-param (build-color-param :s :rainbow-sat, :l 55, :h hue-bar #_rig-hue-gradient)]
              (cue :all-color (fn [_] (tolfx/color color-param :include-color-wheels? true))
                   :short-name "Rainbow Rig"
                   :variables [rainbow-sat])))
   (set-cue! (+ xb 2) (+ yb 2)
                   (let [color-param (build-color-param :s :rainbow-sat, :l 60, :h hue-grad, :adjust-hue hue-bar)]
                     (cue :all-color (fn [_] (tolfx/color color-param))
                               :color-fn (cues/color-fn-from-param color-param)
                               :short-name "Rainbow Grid+Bar"
                               :variables [rainbow-sat])))
   (set-cue! (+ xb 2) (+ yb 3)
                  (let [color-param (build-color-param :s desat-bar, :l 80, :h hue-grad, :adjust-hue hue-bar)]
                    (cue :all-color (fn [_] (tolfx/color color-param))
                               :priority 0, :color-fn (cues/color-fn-from-param color-param)
                              :short-name "Rainbow Pulse")))

  (let [color-cycle (map #(color/s -18 %) ["mediumpurple" "seagreen" "darkblue" "black"])]
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
                                      :effect-name "Wipe Right Beat")))))

  ;    CHASES
   (reset! (:beat steps) (build-step-param :fade-fraction 0.20, :fade-curve :sine))  ;; Set up an initial value for our step parameter
   (reset! (:beat steps) (build-step-param :interval :bar :fade-fraction 0.125, :fade-curve :sine))  ;; Set up an initial value for our step parameter
   (reset! (:phrase steps) (build-step-param :interval :phrase :fade-fraction 0.03125, :fade-curve :sine))  ;; Set up an initial value for our step parameter
   ; XXX test using fun/random-beat-number-param as step param, would jump to a random pos in chase each beat?

  #_(let [strobe #(fun/strobe-2 :strobe (fixtures-named %1) %2 %3)
        set-color (fn [c & name] (tolfx/color (color/like c) #_c :fixtures (apply fixtures-named name)))]
        ;; set-color (fn [c & name] (apply tolfx/color (color/like c) :fixtures
        ;;                                       (if (first name)
        ;;                                        (apply fixtures-named name)
        ;;                                        (all-fixtures))))]
   (set-cue! (+ xb 4) (+ yb 1)
             (cue :chase (fn [vm]
                          (let [step (param/step vm)]
                           (chase "Chase Test 1" ;;XXX other step param whose index looks up which effect fn to call from map, so evolves without needing 64 lines long chase...
                                  ; instead many chases of 1-4 indexes continously triggering same effect with diff values, others updating what will be called etc
                                  ;;also wrapper "(get-random-effect :min-intensity i :max-intensity j)"
                                  ;;later, weight it so on start of bar, phrase, etc, higher chanse of crazy fx, midpoint/low energy reported by analyzer, likelier blank or black or kill running shit...
                                  [(strobe :moving-mini 87 60) ;; XXX random numbers (or global vars, with offsets) for fx fn params
                                   (set-color "black")
                                   (strobe :wash 97 80)
                                   (set-color "royalblue" :moving-mini)
                                   (strobe :moving-beam 99 100)
                                   (set-color "lightgoldenrodyellow" :wash)
                                   (strobe :moving 70 100)
                                   (fx/blank)]
                                  (param/step vm) :beyond :loop)))
                  :color :magenta :priority 1000
                  :variables (vars/cue-maps [["beats" 16 1 32] ["cycles" 3 1 16] ["fade" 0.3 0.0 1.0]])))


   (set-cue! (+ xb 4) (+ yb 2)
             (cue :chase (fn [vm]
                          (chase "Chase Test 2"
                                 [(strobe :moving 77 70) ;XXX be able define progression, 50->77, over the "step" (which would actually consist of two steps with smooth fade)
                                  blackout
                                  (strobe :moving-mini 82 50)
                                  blackout
                                  (strobe :wash 97 70)
                                  blackout
                                  (strobe :moving 60 99)
                                  (set-color "black" :moving)]
                                 @(:beat steps) :beyond :bounce))
                  :color :magenta :priority 10000))

   (set-cue! (+ xb 4) (+ yb 3)
             (cue :chase (fn [vm]
                          (chase "Chase Test 3"
                                 [(strobe :moving 80 50)
                                  blackout blank blackout

                                  (strobe :wash 97 100)
                                  (set-color "royalblue" :moving-mini)
                                  (strobe :moving-beam 47 100)
                                  blackout

                                  (set-color (color/like [:h 270 :s 30 :l 70]) :moving-mini)
                                  (set-color [:h 270 :s 30 :l 70] :moving-mini)
                                  blackout
                                  (set-color "seagreen" :wash)
                                  blackout

                                  (strobe :moving-mini 70 100)
                                  blackout
                                  (strobe :moving 90 100)
                                  blackout]
                                 @(:beat steps) :beyond :blank))
                  :color :magenta :priority 20000))

   ;XXX set beats to global var, use on-var-update fn to actually modify var to closest 12346816. Might cause issues with knobs unless defer tho
   (let [strobe-osc (build-oscillated-param (sawtooth :interval :beat) :min 0 :max 100)]
    (set-cue! (+ xb 4) (+ yb 4)
     (cue :chase-osc
            (fn [vm] (chase "Chase osc"
                            [blank
                             (set-color (color/like "lightgreen" :h 100))
                             (strobe "tube" strobe-osc 100) ;(+ 60 (* 30 (rand)))
                             blackout
                             (fun/bloom (all-fixtures) :measure (cfg :measure :default)
                                        :color (color/like "orange" :h 200) :fraction (+ 0.15 (rand 0.7)))
                             blank blank
                             (fun/sparkle (all-fixtures))]
                            (param/auto-vm vm "sine")))
            :color :yellow :priority 10000
            :variables (vars/auto :beats :cycles :phase (vars/min-max 0.0 8.0 1.0)))))

    (set-cue! (+ xb 0) (+ yb 0)
         (cue :chase-random
                (fn [vm] (chase "Chase rand"
                                [blank
                                 (do (avar :glo-green (color/like "lightgreen")) (set-color (avar :glo-green)))
                                 (strobe "tube" (+ 60 (* 30 (rand))) 100)
                                 blackout
                                 (fun/bloom (all-fixtures) :measure (cfg :measure :default)
                                            :color (color/like "orange") :fraction (rand 1.0))
                                 blank blank
                                 (fun/sparkle (all-fixtures))]
                                (param/auto-vm vm "sine")))
                :color :yellow :priority 10000
                :variables (vars/auto :beats :cycles :phase (vars/min-max 0.0 8.0 1.0))))

   (set-cue! (+ xb 4) (+ yb 5)
     (cue :chase-osc chase-fn
            :color :orange :priority 20000
            :variables (vars/auto :beats :cycles :phase (vars/min-max 0.0 8.0))))

   (set-cue! (+ xb 4) (+ yb 6)
             (cue :chase-fade
                  (fn [vm] (chase "blanker-blacker"
                                  (into [blank (tolfx/global-dimmer-effect 0)]
                                        (take 6 (cycle [blank blackout])))
                                  (param/step vm) :beyond :loop))
          :color :orange :priority 30000
          :variables (vars/auto :beats :cycles (vars/cue-maps [["sine-curve" true] ["fade" 0.5 0 1]])))))))


(defn movement "Create a page of with some large scale and layered movement effects. And miscellany which I'm not totally sure what to do with yet."
  [xb yb]
  (let [moving-heads (or @moving-heads (mapcat fixtures-named [:strip :tube]))] ;#_concat (fixtures-named "strip") (fixtures-named "tube")))]

    #_(set-cue! (+ xb 0) (+ yb 0)
              (cue :sweep-dimmers
                   (fn [vm] (apply-vm vm tolfx/dimmer-sweep (all-fixtures)
                                  (sawtooth :down? (:down vm) :interval-ratio (param/ratio vm))))
                   :color :red :short-name "Sawtooth Sweep"
                   :variables (vars/auto :beats :cycles :down :width :level-dmx :fade)))

    (set-cue! (+ xb 1) (+ 2 yb)
      (cue :moving-dimmers
        (fn [vm]
          (chase "Moving Cross"
                 (map #(dimmer-effect (:level vm) (fixtures-named %)) [:moving-1 :moving-2 :moving-3 :moving-4])
                 (param/step vm) :beyond :loop))
          :color :red :short-name "Moving Cross"
          :variables (vars/auto :beats :cycles :level-dmx :fade-fraction)))

    (set-cue! (+ xb 3) (+ yb 1)
      (cue :movement (fn [vm] (apply-vm vm fun/aim-fan moving-heads))
           :variables (vars/auto (vars/cue-maps [["x-scale" 1 -5 5] ["y-scale" 5 -10 10]
                                                 ["z" 4 0 20] ["y" (venue :rig) -10 10] ["x" 0.0 -10 10]]))
                :color :blue, :end-keys [:move-moving]))

    #_(set-cue! (+ xb 3) (+ yb 2)
      (cue :movement (fn [vm] (apply-vm vm fun/twirl moving-heads))
           :variables (vars/auto :beats :cycles
                                 (vars/cue-maps [["radius" 0.25 0 10] ["z" -1.0 -10 10]
                                                 ["y" (venue :rig) -10 10] ["x" 0.0 -10 10.0]]))
                :color :green, :end-keys [:move-moving]))

    (set-cue! (+ xb 3) (+ yb 3)
              (cue :move-all
                   (fn [vm] (apply-vm vm tolfx/can-can))
                   :variables (vars/auto :bars :cycles (vars/cue-map ["stagger" 0.5 0 4])
                                        (vars/cue-map ["spread" 0 -45 45] :centered true :resolution 0.25)
                                        (map #(vars/cue-map [%1 %2 %3 %4] :centered true :resolution 0.5)
                                             (for [t ["pan" "tilt"] a ["min" "max"]] (str t "-" a))
                                             [0 0 -60 100] (repeat 4 -180) (repeat 4 180)))
                   :color :yellow :end-keys [:movement]))

    (set-cue! (+ xb 4) (+ yb 3)
              (cue :move-all
                   (fn [vm] (apply-vm vm tolfx/can-can))
                   :variables (vars/auto :bars :cycles
                                         (vars/cue-map ["stagger" 0.5 0 4])
                                         (vars/cue-map ["spread" 0 -45 45] :centered true :resolution 0.25)
                                         #_(map #(vars/cue-map [%1 %2 %3 %4] :centered true :resolution 0.5)
                                             (for [t ["pan" "tilt"] a ["min" "max"]] (str t "-" a))
                                             [0 0 -60 100] (repeat 4 -180) (repeat 4 180)))
                   :color :yellow :end-keys [:movement]))

    (set-cue! (+ xb 3) (+ yb 4)
      (cue :moving-circles
           (fn [vm] (apply-vm vm tolfx/circle-chain moving-heads true
                              :right-wall (wall :right) :left-wall (wall :left)
                              :stage-wall (wall :stage) :rear-wall (wall :rear)))
           :variables (vars/auto :bars (vars/cue-maps [["radius" 1.0 0.1 2.0] ["stagger" 0.0 0.0 2.0]] :resolution 0.1))
           :short-name "Moving Circles" :color :green :priority 4))

    ;; (set-cue! (+ xb 3) (+ yb 5)
    ;;           (cue :crossover (fn [vm] (apply-vm vm tolfx/crossover-chase))
    ;;                :variables (vars/auto :beats :fade-fraction (vars/cue-maps [["cross-color" "orangered"] ["end-color" "seagreen"]]))
    ;;                :color :cyan :priority 5))

    (let [triangle-phrase (build-oscillated-param (triangle :interval :phrase) :min -90 :max 90) ; Move back and forth over a phrase
          staggered-triangle-bar (build-spatial-param (all-fixtures) ; Bounce over a bar, staggered across grid x
                                  (fn [head]
                                   (build-oscillated-param (triangle :interval :bar :phase (x-phase head *show*)) :min -90 :max 0)))
                                   ;; (build-oscillated-param (triangle :interval :bar :phase (space-phase head *show*)) :min -90 :max 0)))
        can-can-dir (params/build-direction-param-from-pan-tilt :pan triangle-phrase :tilt staggered-triangle-bar)
        can-can-p-t (params/build-pan-tilt-param :pan triangle-phrase :tilt staggered-triangle-bar)]
    (set-cue! (+ xb 4) (+ yb 0) (cue :movement (fn [_] (move/direction-effect "Can Can" can-can-dir (all-fixtures)))))
    (set-cue! (+ xb 4) (+ yb 1) (cue :movement (fn [_] (move/pan-tilt-effect "P/T Can Can" can-can-p-t (all-fixtures))))))

    (set-cue! (+ xb 5) (+ yb 1) (function-cue :moving-speed :movement-speed moving-heads
                                              :color :purple :effect-name "XXX Moving head motor speed osc"))))


(defn pointing "Create a page of cues pointing lights, either aiming at a point or directionally by pan/tilt"
 [xb yb id & {:keys [groups fixtures]
              :or {groups (keys (ptr-cfg :groups))
                   fixtures (cfg :fixtures :types :moving :names)}}]
 {:pre [(some? *show*)]}
 (doall (map-indexed
   (fn [index fixture] ;over fixtures, rows
    (let [x (+ xb index)
          step (count (ptr-cfg :fixture-mods))]

     (loop [base (ptr-cfg :groups (first groups) :index), groups groups] ;starting index
      (let [group (first groups)
            step (if (ptr-cfg :groups group :setup-mods) step 1)]

       (doseq [offset (range step)] ; straight/flip/any other mode specified for group
        (let [at-index #((ptr-cfg %2) %1)
              [group-mods fixture-mods] (map #(at-index offset %) [:group-mods :fixture-mods])
              f? (:flip fixture-mods)
              [cmod-id cmod-group cmod-fixture] (map eval (map :color-mod [(ptr-cfg :type id) group-mods fixture-mods]))
              color (cmod-id (ptr-cfg :groups group :color))
              y (+ yb base offset)]

        (set-cue! x y (cue/pointing fixture (if (not= :none group) group) f?
                                    (cmod-fixture color) :id id)) ;XXX also have color-mods for fixture groupsjk
        #_(osc/cue-binding x y (str "/1/" % (name fixture) "-" group))

        (when-not (= :none group) ;:none is "group" for disconnected direct control
         (cue/pointing-transform group (far xb) y (:flip group-mods) id
                                 (cmod-group color)))))

       (set-cue! x (far yb) (cue/pointing-lfo fixture id)) ;lfo up top

      (when (next groups)
       (recur (+ base step) (next groups)))))))
   fixtures)))


(defn modulator "Cue page modulating and transforming existing cues - changing colors by global HSL etc"
 [xb yb & {:keys [groups] :or {}}]
  (let [groups (if groups groups (into [nil] (cfg :fixtures :groups)))] ; nil resolves to all fixtures when passed to helper functions

  ;; (set-cue! (+ xb 0) (+ yb 0) (cue/focus-oscillator :moving-1-focus "Moving Focus Sine" (show/fixtures-named "moving")))

   (set-cue! (+ xb 3) (+ yb 4)
             (cue :transform-saturation-saw
                  (fn [vm] (transform-colors (all-fixtures)
                             :transform-fn (tolfx/sat-transformer :param (param/auto-vm vm "sawtooth"))))
                  :variables (vars/auto :beats :cycles :min-num :max-num)
                  :priority 10000))

   (set-cue! (+ xb 3) (+ yb 2)
               (cue :transform-saturation-square
                    (fn [vm] (transform-colors (all-fixtures)
                              :transform-fn (tolfx/sat-transformer
                                             :param (param/auto-vm vm "square") #_(build-oscillated-param (square :interval-ratio (param/ratio vm), :phase (:phase vm), :width (:width vm))
                                                                                :max (:max vm), :min (:min vm)))))
                    :variables (vars/auto :beats :cycles (vars/min-max -100 100 -30 10) :width :phase)
                    :priority 10000))
   (set-cue! (+ xb 3) (+ yb 1)
             (cue :transform-hsl
                  (fn [vm]
                   (transform-colors (all-fixtures)
                                     :transform-fn (tolfx/color-transformer
                                                    :h (:hue vm) :s (:saturation vm) :l (:lightness vm))))
                  :variables (vars/auto :hue :saturation :lightness)
                  :priority 30000))
   (set-cue! (+ xb 3) (+ yb 0)
             (cue :transform-hue
                  (fn [vm]
                   (transform-colors (all-fixtures) :transform-fn (tolfx/color-transformer :h (param/auto-vm vm "sine"))))
                  :variables (vars/auto :beats :cycles (vars/min-max -720 720 -30 30) :phase)
                  :priority 30000, :color :mediumpurple4))))

(defn testing "fuck around"
 [xb yb & {:keys [groups] :or {groups (into [nil] all-groups)}}]

 (set-cue! (+ xb 0) (+ yb 0)
           (cue :sweep-dimmers
                (fn [vm] (apply-vm vm tolfx/sweep (all-fixtures)
                                   (sawtooth :down? (:down vm) :interval-ratio (param/ratio vm))
                                   :effect-fn dimmer-effect))
                :color :red :short-name "Sawtooth Sweep"
                :variables (vars/auto :beats :cycles :down :width :level :fade)))
 (set-cue! (+ xb 1) (+ yb 0)
           (cue :sweep-strobe
                (fn [vm] (apply-vm vm tolfx/sweep (all-fixtures)
                                   ;; (param/auto-vm vm "sawtooth") ;nah go
                                   (sawtooth :down? (:down vm) :interval-ratio (param/ratio vm))
                                   :effect-fn tolfx/strobe-effect))
                :color :red :short-name "Sawtooth Sweep"
                :variables (vars/auto :beats :cycles :down :width :level :fade))))


(defn velocity "live playing yo"
  [xb yb]
  (let []
    (cue/color "royalblue" xb yb, :fixtures (all-fixtures), :effect-key :all-color, :effect-name "Color all")
    (doall (map-indexed (fn [i group]
                         (cue/color group "royalblue" (+ xb (inc i)) yb, :include-color-wheels? true
                                         :effect-key (keyword (str (name group) "-color")) :priority 1, :held true
                                         :effect-name (str "Color " (name group))))
                        all-groups)) ))

(defn color "colors all over the place"
  [xb yb & {:keys [groups colors]
            :or {groups (into [nil] all-groups)
                 colors [:royalblue :pink :orange :forestgreen :purple :lavender :black :white]}}]
  (let [colors (map color/like colors)]
   (doall (map-indexed
           (fn [j color]
     (doall (map-indexed
             (fn [i group] ;needs alpha wrapper for blending and shit. + USE ALPHA COMPONENT GHRRRdamnit
              ;; (println i j color group)
              (cue/color group (+ xb i) (+ yb j) (color/like color)
                         :priority (* i j)))
             groups)))
           colors))
   ;TODO: show var cues setting color (mostly for hue info), held cues with velocity adjusting sat/lightness
   ;some mechanism for fading on/off color over attack/release time, a la strips but within afterglow...
    ;; (cue/color "royalblue" xb yb, :fixtures (all-fixtures), :effect-key :all-color, :effect-name "Color all")
    ;TODO: bind most colorvar-using cues (bloom, sparkle etcetc) to show vars containing functions looking up actual show var to use
    ;then can route by index incl step param, easily randomize, update each beat/bar/phrase etc
    ))

(defn channel-control "LFO channel cues, eg fog or LED-strip control channel cues for pixtol"
 [xb yb & {:keys [groups params] :or {}}]
  (let [params (or params param/types)]
   ;; XXX support function-cues in same fn
   (doall (map-indexed
           (fn [x channel]
            (doall (map-indexed
                    (fn [y param]
                     (cue/auto nil (+ xb x) (+ yb y) (:type param) tolfx/channel ;function-effect
                              :ch-type channel :end-rest? false))
                    params)))
           groups))))


 (defn dynamic "make cues to select which fixture to point an effect to, and what prio to run it at"
   [xb yb]
   (let [fixtures [:strip-1 :strip-2 :strip-3 :strip-4 :moving-1 :moving-2 :wash-1 :wash-2]
         rig-width (- (wall :right) (wall :left))]))

