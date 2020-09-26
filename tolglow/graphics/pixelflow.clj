(ns tolglow.graphics.pixelflow
(:require [quil.core :as q]
          [quil.applet :as a]
          [afterglow.effects.params]
          [afterglow.show-context :refer [*show*]]
          [afterglow.rhythm]
          [thi.ng.color.core :as clr]
          [thi.ng.math.core :as cmath]
          [tolglow.param :as param])
(:import [com.thomasdiewald.pixelflow.java        DwPixelFlow]
         [com.thomasdiewald.pixelflow.java.dwgl   DwGLSLProgram]
         [com.thomasdiewald.pixelflow.java.fluid  DwFluid2D
                                                  DwFluid2D$FluidData
                                                  DwFluidParticleSystem2D]
         [com.thomasdiewald.pixelflow.java.imageprocessing     DwOpticalFlow
                                                               DwFlowField]
         [com.thomasdiewald.pixelflow.java.imageprocessing.filter  DwFilter DwLiquidFX Merge]
         [com.thomasdiewald.pixelflow.java.flowfieldparticles  DwFlowFieldParticles
                                                               DwFlowFieldParticles$SpawnRadial]
         [Dw.filter.DwFilter]))

(defmacro pass-live-fn "Guard fn in var so stays hot-reloadable inside fx, chases etc..."
 [v]
 (if (symbol? v)
  `(if (fn? ~v) (var ~v) ~v)
  v))

(defonce ^{:kind processing.opengl.PGraphics2D} pg (atom {}))
; in devdemo: checker, canvas, obstacles, spheres, particles, trails, trails_tmp, sprite, gravity, impulse, luminance...

(defonce ^{:kind processing.opengl.PGraphics2D} pg-canvas (atom nil))
(defonce ^{:kind processing.opengl.PGraphics2D} pg-world (atom nil)) ;physical objexts must be rendered seperately first so particles/flow can adhere
(defonce ^{:kind processing.opengl.PGraphics2D} pg-img (atom nil)) ;physical objexts must be rendered seperately first so particles/flow can adhere
(defonce ^{:kind DwPixelFlow} context (atom nil))
(defonce ^{:kind DwFluid2D} fluid (atom nil))
(defonce ^{:kind DwFlowFieldParticles} particles (atom nil))
(defonce ^{:kind DwFlowField} gravity (atom nil))
(defonce ^{:kind DwLiquidFX} liquidfx (atom nil))

(def obstacle-color [40 40 40 255]) ;color of texture used as solid matter

(defn lfo [id l h b] (param/quick-lfo id :low l :high h :beats b))

(defn fuck-around-defs []
 (def sine (lfo "sine"  1 255 13))
 (def sine2 (lfo "sine" 2 255 4))
 (def sin-1 (lfo "sine"  -1 1 8))
 (def sin-2 (lfo "sine"  -1 1 4))
 (def saw  (lfo "sawtooth" 3 80 23))
 (def saw2 (lfo "sawtooth" 0.05 0.8 3))
 (def mix  (param/mix [sine saw] param/sum :min 0.99 :max 20.5))
 (def sinsin (param/mix [sin-1 sin-2] param/sum :min -1 :max 1))
 (def sinsin-2 (param/mix [sin-1 sin-2] param/div :min -1 :max 10))
 ;; (def mix (param/mix [sine saw] param/sum :mode :bounce))
 (def sub  (param/mix [sine saw] param/sub :min 2.99 :max 40.5))
 (def div  (param/mix [saw saw2] param/div :min 2.99 :max 20.5))
 (def params [sine sine2 saw saw2 mix sub div])
 )

(defn pp+ [ps]
 (apply + (param/auto-resolve ps)))
(defn pp+ [& ps]
 (apply + (param/auto-resolve ps)))
(defmacro p+ [& ps]
 `(apply + (param/auto-resolve [~@ps])))

(defn p-mathf [f & ps]
 (apply f (param/auto-resolve ps)))
(defmacro p-math [f & ps]
 `(apply ~f (param/auto-resolve [~@ps])))

;; (macroexpand-1 (with-r-p sine))
;; (with-r-p sine)
;; (defn with-r-p*
;;  [f & args]
;;  (for [part args]
;;     ;; (if (afterglow.effects.params/param? part)
;;     (if (afterglow.effects.params/param? part)
;;        (tolglow.param/auto-resolve part)
;;        part)))
(def not-param 1)

;; (with-r-p 5)
;; (with-r-p (+ 5 5))
;; (wrp (+ 5 5))
;; (wrp* (+ 5 5))
;; (with-r-p + 5 5 5 5)
;; (with-r-p (+ 5 (+ 9 sine)))
;; (with-r-p (+ 9 sine))
;; (wrp (+ 9 sine))
;; (with-r-p (+ 5 (+ 9 not-param)))

(declare inline-2-helper)
(defn clean-arg [arg]
  (if (seq? arg)
    (inline-2-helper arg)
    arg))

(defn apply-arg
  "Given args [x (+ y)], return (+ x y)"
  [val [op arg]]
  (list op val (clean-arg arg)))

(defn inline-2-helper
  [[arg1 & ops-and-args]]
  (let [ops (partition 2 ops-and-args)]
    (reduce apply-arg (clean-arg arg1) ops)))

(defmacro inline-2 [form]
  (inline-2-helper form))

(inline-2-helper '(a + (b - 2) - (c * 5)))
(inline-2 (4 * 3 / (2 + (3 / 1) + 8)))
(param/auto-resolve (list sine sine2))

(declare wrp-helper)
(defn expand-seq [arg]
 (if (seq? arg) (wrp-helper arg) arg))

(defn wrp-helper
 [[f & args]]
;;  (vec (tolglow.param/auto-resolve args))
;;  (let [res (tolglow.param/auto-resolve args)]
 (let [res (afterglow.effects.params/resolve-param args *show* (afterglow.rhythm/metro-snapshot (:metronome *show*)))]
  ;; (list apply f res)
  `(apply ~f ~@res)
  ;; [f res]
  ;; (flatten [f res])
  ;; (vec f res)
  ;; res
  ;; (f res)
  #_(apply f res)))

;; (require '(clojure.core [reducers :as r]))
;; (defmacro wrp [form] (wrp-helper form))
;; ;; (count (wrp (+ 9 5)))
;; (wrp (+ 9 5))
;; (wrp (+ 9 sine))
;; (macroexpand-1 (macroexpand-1 '(wrp (+ 9 sine))))
;; (def ran (vec (range 1 1000000)))
;; ;; (time (reduce + (mapv inc ran)))
;; (time (reduce + (map inc (range 1 100000000))))
;; (time (reduce + (mapv inc (range 1 10000000))))
;; (time (apply + (map inc (range 1 100000000))))
;; (time (r/fold + (r/map inc (range 1 10000000))))

;; (def data (into [] (take 10000000 (repeatedly #(rand-int 1000)))))
;; (defn frequencies-v1 [coll]
;;   (reduce
;;     (fn [counts x]
;;       (merge-with + counts {x 1}))
;;   {} coll))
;; (defn frequencies-v2 [coll]
;;   (r/fold
;;     (fn combinef
;;       ([] {})
;;       ([x y] (merge-with + x y)))
;;     (fn reducef
;;       ([counts x] (merge-with + counts {x 1})))
;;     coll))
;; (time (frequencies-v1 data))
;; (time (frequencies-v2 data))

;; (defmacro wrp
;;  ([[f & args]]
;;   `(let [args# (for [arg# ~args]
;;                 (tolglow.param/auto-resolve arg#))]
;;     (apply ~f args#))))

;; (clojure.core/let
;;  [res (clojure.core/for [arg__80622__auto__ (5 5)] (tolglow.param/auto-resolve arg__80622__auto__))]
;;  (clojure.core/apply + res))

(defn wrp*
 ([[f & args]]
  (let [args (for [arg args]
                (tolglow.param/auto-resolve arg))]
    (apply f args))))

;; (defmacro with-r-p
;;  ([body]
;;   (if (list? body)
;;      (with-r-p (first body) (rest body))
;;      body))
;;   ;; `(if (list? ~body)  ;cant do this, if eg (+ 5 sine) unquoting obvs tries to eval the faulty code
;;   ;;    (with-r-p (first body) (rest body))
;;   ;;    ~body))
;;  ([f & args]
;;   ;; `(f args)
;;   `(~f ~@args))
;;   ;; `(for [part# [~@args]]
;;   #_`(if (seq ~args)
;;     (for [part# ~args]
;;   ;; `(doseq [part# [~@args]]
;;     ;; (println ~f part#)
;;       (if (afterglow.effects.params/param? part#)
;;          (tolglow.param/auto-resolve part#)
;;          part#))
;;     #_(let [f# ~f]
;;      (with-r-p (first f#) (rest f#)))))
  ;; `(let [args# ~@args]
  ;;  (if args#
  ;;   (for [part# args#]
  ;; ;; `(doseq [part# [~@args]]
  ;;   ;; (println ~f part#)
  ;;   (if (afterglow.effects.params/param? part#)
  ;;      (tolglow.param/auto-resolve part#)
  ;;      part#))
  ;;   (with-r-p (first f) (rest f)))))
;; )

;; (defmacro with-r-p
;;  ([body]
;;   `(let [f# ~'(first body)
;;          args# ~'(next body)]
;;     (if args#
;;      (with-r-p* f# arg#)
;;      ;; (with-r-p* (first body#) (rest body#))
;;      ;; (with-r-p (first body#) (rest body#))
;;      f#)))
;;  #_([f & args]
;;   ;; `(~f ~@args))
;;   ;; `(for [part# [~@args]]
;;   `(for [part# ~args]
;;   ;; `(doseq [part# [~@args]]
;;     ;; (println ~f part#)
;;     (if ~(afterglow.effects.params/param? part#)
;;        (tolglow.param/auto-resolve part#)
;;        part#)))
;;   )

(defmacro with-resolved-params [body]
;;  `(for [part# ~body]
;;  (let []
  `(for [part# [~@body]]
   ;; (if-let [is-param# ~(afterglow.effects.params/param? part#)]
   ;;  (tolglow.param/auto-resolve part#)
   ;;  part#)))
   (println part#)
   #_(if ~(afterglow.effects.params/param? part#)
    (tolglow.param/auto-resolve part#)
    part#))
  )
;; )

;;  `(let [sorted# (for [part ~body]
;;                  (if (params/param? part)
;;                   (param/auto-resolve part)
;;                   part))]
;;  sorted#))
;; (defmacro or
;;   ([x] x)
;;   ([x & next]
;;       `(let [or# ~x]
;;          (if or# or# (or ~@next)))))
;; (with-resolved-params (+ sine sine2))
;; (with-resolved-params sine)
;; (with-resolved-params (+ 5))

;; (apply + (param/auto-resolve [sine sine2]))
;; (param/auto-resolve [sine sine2])
;; (p+ sine sine2)
;; (pp+ sine sine2)
;; (p-math - sine sine2)
;; (clojure.core/apply clojure.core/+ (tolglow.param/auto-resolve (sine sine2)))


(defn update-fluid [obj] ;fix this so not inlined a la quil then can update anytime
 (let [[sine sine2 saw saw2 mix sub div] (mapv #(param/auto-resolve %) params)
       x (q/mouse-x)
       y (- (q/height) 200 (q/mouse-y))
       radius 50]

  (if (q/mouse-pressed?)
    (let [radius 70
          vel-x 30
          vel-y 30]
     (.addVelocity obj x y radius vel-x vel-y))
    (let [x 0
          radius 900
          vel-x 5
          vel-y 3]
     (.addVelocity obj x y radius vel-x vel-y)))

  (let [x (+ x 2 (* 0.3 mix))
        radius (+ 30 (* mix 0.2))
        r (+ 0.2 (/ sine 600))
        g (+ 0.2 (/ sine2 790))
        ;; g 0.9
        b (+ 0.4 (/ (- 255 sine2) 500))
        scale 50]
   (.addDensity obj x y radius r g b scale))

  (let [x (* saw 1.00)
        y (* sine 2.20)
        one (* saw 0.14)
        two (* mix 1.50)]
   (.addTemperature obj x y one two))))


(defn set-particle-params [#_obj] ;can get fancy later. live reload is #1
 (let [p (.-param @particles #_obj)]
  (set! (.col_A p) (float-array [0.10 0.50 0.80 1.00])) ;inner color
  (set! (.col_B p) (float-array [0.25 0.15 0.50 0.00])) ;outer color - but 0.00 sure as hell aint alpha??
  ;; (set! (.col_B p) (float-array [0.85 0.15 0.10 0.10])) ;outer color - but 0.00 sure as hell aint alpha??
  ;; (set! (.col_B p) (float-array [0.05 0.25 0.70 0.0 #_0.09])) ;outer color - but 0.00 sure as hell aint alpha??
  ;; (set! (.col_B p) (float-array [0.0 0.0 0.0 0.0 #_0.09])) ;outer color - but 0.00 sure as hell aint alpha??
  (set! (.shader_collision_mult p) 0.2)
  (set! (.steps p) 1) ;right. stes 2 and we get, not blend but less
  (set! (.velocity_damping p) 0.97) ;0.95 - 1.0
  (set! (.size_display p) 13)
  (set! (.size_collision p) 4)
  (set! (.size_cohesion p) 3)
  (set! (.wh_scale_coh p) 4)
  (set! (.wh_scale_col p) 1)
  (set! (.wh_scale_obs p) 0)

  (set! (.mul_coh p) 0.5)
  (set! (.mul_col p) 1.0)
  (set! (.mul_obs p) 1.0)))

; decent goo starting point:
; steps 1
; size: 50 display 11 colission 34 cohesion
; mul: 3.3 colission 4.0 cohesion 4.0 obstacles
; wh scale: 0 col 1 coh 0 obs
; sprite: size 32 exp1 0.38 exp2 0.52 mult 0.04 (not even showing on preview)
; bloom + liquid fx (which with user settings looks like 90s photoshop lol)

; glowing cubes:
; size display and colission very large, cohesion vary
; sprite: 0.00 exp2 (square)
; spawn few
;
; bloomy:
; use display dist debug mode as starting point for an actual
;

;XXX eh fix a script to go from java to clj automatically
(defn set-liquidfx-params [obj]
 (.base_LoD            (.-param @liquidfx) 1   )
 (.base_blur_radius    (.-param @liquidfx) 1   )
 (.base_threshold      (.-param @liquidfx) 0.6)
 (.base_threshold_pow  (.-param @liquidfx) 25  )
 (.highlight_enabled   (.-param @liquidfx) true)
 (.highlight_LoD       (.-param @liquidfx) 1   )
 (.highlight_decay     (.-param @liquidfx) 0.6)
 (.sss_enabled         (.-param @liquidfx) true)
 (.sss_LoD             (.-param @liquidfx) 3   )
 (.sss_decay           (.-param @liquidfx) 0.8)
;;  (.apply @liquidfx pg-particles)
 )

(defn set-fluid-params [obj] ;can also be called whenever to update (so def try lfoing them)
(let [p (.-param @fluid #_obj)]
  (set! (.dissipation_velocity p) 0.98)
  (set! (.dissipation_density p) 0.89)
  (set! (.dissipation_temperature p) 0.79)
  (set! (.vorticity p) 0.29)))

(defn init-fluid [context obj callback] ;ok guess no work now, callback gets inlined...
 (let [cb (proxy [DwFluid2D$FluidData] []
           (update [this] (pass-live-fn (callback this))))]
  (reset! obj (DwFluid2D. @context (q/width) (q/height) 1.0))
  (.addCallback_FluiData @obj cb)
  (set-fluid-params @obj)))

(defn add-update-fn [obj]
  (let [cb (proxy [DwFluid2D$FluidData] []
         (update [this] (pass-live-fn (update-fluid this))))] ;old, working...
    (.addCallback_FluiData obj cb))) ;; // adding data to the fluid simulation

(defn setup []
 (let [[w h] [(q/width) (q/height)]]
  (reset! context (DwPixelFlow. (quil.applet/current-applet)))
  (.printGL @context)

  ;; (init-fluid context fluid update-fluid) ;uh dont have the atom reset/deref in the fn
  (init-fluid context fluid (pass-live-fn update-fluid)) ;uh dont have the atom reset/deref in the fn

  (reset! particles (DwFlowFieldParticles. @context 10000))
  (set-particle-params #_@particles)
  ;; (.createSpriteTexture @particles 50 0.5 0.8 8.9) ;dunno what it does haha

  (reset! gravity (DwFlowField. @context))
  (.resize @gravity w h)

  ;; (let particle-system (DwFluidParticleSystem2D.)
  ;;  (.resize particle-system context, viewport_w/3, viewport_h/3));

  (reset! pg-canvas (q/create-graphics w h :p2d))
  (.smooth @pg-canvas 0)
  (reset! pg-world (q/create-graphics w h :p2d))
  (.smooth @pg-world 8)

  (q/frame-rate 60)
  (q/color-mode :rgb 255)))



(defn update-state [& s]
 (set! (.timestep (.-param @particles)) (/ 1 (q/current-frame-rate)))
 )

(defn render-world [obj]
 (let [[w h] [(.width obj) (.height obj)]
       dim (* 2 (/ h 3.0))]

  (q/with-graphics obj
    (q/clear)
    (q/no-stroke)
    (q/blend-mode :replace)

    (q/rect-mode :corner) ;; MAKE PHYSICAL BORDER
    (apply q/fill obstacle-color) (q/rect 0 0 w h) ;fill entire
    (q/fill 0 0)   (q/rect 10 10 (- w 20) (- h 20)) ;cut a hole

    (q/rect-mode :center) ;obstacles
    (q/push-matrix)
    (let [rot (/ (q/frame-count) 360.0 #_90.0 #_180.0)]
      (q/translate (/ w 2) (- h (/ dim 2)))
      (q/rotate rot)
      (apply q/fill obstacle-color) ;40 40 40 255 being set as obstacle color
      (q/rect 0 0 dim,  30)
      (q/rect 0 0 30,  dim))
    (q/pop-matrix))))

(defn spawn-particles
 [obj sr [px py :as pos] [vx vy :as vel] radius amount] ;similar order to flow
  (.num sr amount)
  (.dim sr radius radius)
  (.pos sr px (- (q/height) 1 py))
  (.vel sr vx vy)
  (.spawn obj (q/width) (q/height) sr))

(defn add-particles [obj & [amount]]
 (let [[w h] [(q/width) (q/height)]
       sinsin (* 50 (param/auto-resolve sinsin))
       ;; position [(/ w 2.0) (/ h 4.0)]
       position [(+ sinsin (/ w 2.0)) (+ sinsin (/ h 4.0))]
       velocity [0 4]
       [amount radius] [((fnil + 0) amount) 10]
       sr (DwFlowFieldParticles$SpawnRadial.)] ;DwFlowFieldParticles.SpawnRadial sr = new DwFlowFieldParticles.SpawnRadial();

  (if (q/mouse-pressed?)
   (let [pr (* 0.5 (.getCollisionSize @particles))
         amount (Math/ceil (* 0.01 (.getCount @particles)))
         amount (min 100 (max 1 amount)) ;guess it gets pissy if passed 0?
         radius (Math/ceil (Math/sqrt (* amount pr pr)))
         [px py :as position] [(max 0 (- (q/mouse-x) 100))
                               (q/mouse-y)]
         [vx vy :as velocity] [(* -0.5 (- px (q/pmouse-x)))
                               (* -5 (- py (q/pmouse-y)))]]
    (spawn-particles obj sr position velocity radius amount))

   (spawn-particles obj sr position velocity radius amount))))


(defn render-gui []
  (q/stroke-weight 1)
  (q/fill 10 120) (q/stroke 70) (q/stroke-weight 2)
  (q/rect 15 15  140 60)

  (q/fill 245)
  (q/text-size 13)
  ;; (q/text (str (cmath/roundto (q/current-frame-rate) 1) " fps") 20 33)
  ;; (q/text (str (.getCount @particles) " particles") 20 50)
  (q/text (with-out-str (printf "%.0f fps\n%,d particles"
                        (q/current-frame-rate)
                        (.getCount @particles)))
          20 33))


(def reset-trigger (lfo "sawtooth" 0 80 27))
(def run-trigger (lfo "sawtooth" 0 80 6))

(defn draw []
 (let [[reset run] (map #(param/auto-resolve %) [reset-trigger run-trigger])]
  (update-state)
  (render-world @pg-world) ;obstacles
  (when (or (> 5 run) (q/mouse-pressed?))
   (add-particles @particles (int (* 2 run))))
  ;; (add-particles @particles 1)
  ;; (when (< 75 reset) (.reset @particles))
  ; (.reset @particles) ;dont call from repl btw, crashhh

  (.addObstacles @fluid @pg-world)
  (.update @fluid)
  (.resizeWorld @particles (q/width) (q/height))
  (.createObstacleFlowField @particles @pg-world (int-array obstacle-color) false) ;last fg (obstacle def), fg-invert
  (.update @particles @gravity)

  (q/with-graphics @pg-canvas
   (q/background 25)
   (q/image @pg-world 0 0))
  (.displayParticles @particles @pg-canvas)
  (.renderFluidTextures @fluid @pg-canvas 0)


  ;; ; bloom - pretty so sort soon
  ;; DwFilter filter = DwFilter.get(context);
  ;; filter.luminance_threshold.param.threshold = 0.7f; // when 0, all colors are used
  ;; filter.luminance_threshold.param.exponent  = 7;
  ;; filter.luminance_threshold.apply(pg_aa, pg_luminance);
  ;;
  ;; filter.bloom.setBlurLayers(10);
  ;; ;filter.bloom.gaussianpyramid.setBlurLayers(10);
  ;; filter.bloom.param.blur_radius = 1;
  ;; filter.bloom.param.mult   = 0.5f; //map(mouseX, 0, width, 0, 2);
  ;; filter.bloom.param.radius = 0.7f; //map(mouseY, 0, height, 0, 1);
  ;; filter.bloom.apply(pg_luminance, null, pg_aa);

  (q/blend-mode :replace)
  (q/image @pg-canvas 0 0)
  (q/blend-mode :blend)

  (render-gui)))


(defn init []
  (q/defsketch fluids
    :title "fluids"
    :features [:resizable]
    :setup setup  :draw draw ;; :update update-state
    :size [600 600]
    ;; :middleware [m/fun-mode m/navigation-3d]
    :renderer :p2d)
 (alter-var-root #'a/*applet* (constantly fluids))) ;like i dont get why is this bad/not done by default?
