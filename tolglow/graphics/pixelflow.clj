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
         [com.thomasdiewald.pixelflow.java.flowfieldparticles  DwFlowFieldParticles
                                                               DwFlowFieldParticles$SpawnRadial]
         [Dw.filter.DwFilter]))
;
(defmacro pass-live-fn "Guard fn in var so stays hot-reloadable inside fx, chases etc..."
 [v]
 (if (symbol? v)
  `(if (fn? ~v) (var ~v) ~v)
  v))

(defonce ^{:kind processing.opengl.PGraphics2D} pg-canvas (atom nil))
(defonce ^{:kind processing.opengl.PGraphics2D} pg-world (atom nil)) ;physical objexts must be rendered seperately first so particles/flow can adhere
(defonce ^{:kind DwPixelFlow} context (atom nil))
(defonce ^{:kind DwFluid2D} fluid (atom nil))
(defonce ^{:kind DwFlowFieldParticles} particles (atom nil))
(defonce ^{:kind DwFlowField} gravity (atom nil))

(def obstacle-color [40 40 40 255]) ;color of texture used as solid matter

(defn lfo [lfo l h b] (param/quick-lfo lfo :lo l :high h :beats b))

(def sine (lfo "sine"  1 255 13))
(def sine2 (lfo "sine" 2 255 4))
(def saw  (lfo "sawtooth" 3 80 23))
(def saw2 (lfo "sawtooth" 0.05 0.8 3))
(def mix  (param/mix [sine saw] param/sum :min 0.99 :max 20.5))
;; (def mix (param/mix [sine saw] param/sum :mode :bounce))
(def sub  (param/mix [sine saw] param/sub :min 2.99 :max 40.5))
(def div  (param/mix [saw saw2] param/div :min 2.99 :max 20.5))
(def params [sine sine2 saw saw2 mix sub div])

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
       y (- (q/height) (q/mouse-y))
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
        radius (+ 80 (* mix 0.2))
        r (+ 0.1 (/ sine 500))
        g (+ 0.1 (/ sine2 590))
        b (+ 0.4 (/ (- 255 sine2) 500))
        scale 500]
   (.addDensity obj x y radius r g b scale))

  (let [x (* saw 1.00)
        y (* sine 2.20)
        one (* saw 0.14)
        two (* mix 1.50)]
   (.addTemperature obj x y one two))))


(defmacro set-param!
[obj paramm value]
`(set! (. (. @obj ))))

(defn set-particle-params [obj]
 (let [p (.-param obj)]
  (set! (.col_A p) (float-array [0.10 0.50 0.80 5.00])) ;particles.param.col_A = new float[]{0.10f, 0.50f, 0.80f, 5};
  (set! (.col_B p) (float-array [0.05 0.25 0.40 0.00]))
  (set! (.shader_collision_mult p) 0.2)
  (set! (.steps p) 1)
  (set! (.velocity_damping p) 0.995)
  (set! (.size_display p) 10)
  (set! (.size_collision p) 10)
  (set! (.size_cohesion p) 4)
  (set! (.mul_coh p) 0.5)
  (set! (.mul_col p) 1.0)
  (set! (.mul_obs p) 2.0)))


(defn set-fluid-params [obj] ;can also be called whenever to update (so def try lfoing them)
(let [p (.-param obj)]
  (set! (.dissipation_velocity p) 0.98)
  (set! (.dissipation_density p) 0.89)
  (set! (.dissipation_temperature p) 0.79)
  (set! (.vorticity p) 0.59)))

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

  (init-fluid context fluid update-fluid) ;uh dont have the atom reset/deref in the fn

  (reset! particles (DwFlowFieldParticles. @context 10000))
  (set-particle-params @particles)
  ;; (.createSpriteTexture @particles 50 0.5 0.8 8.9) ;dunno what it does haha

  (reset! gravity (DwFlowField. @context))
  (.resize @gravity w h)

  ;; (let particle-system (DwFluidParticleSystem2D.)
  ;;  (.resize particle-system context, viewport_w/3, viewport_h/3));

  (reset! pg-canvas (q/create-graphics w h :p2d))
  ;; (.smooth @pg-canvas 0)
  (reset! pg-world (q/create-graphics w h :p2d))
  ;; (.smooth @pg-world 8)

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

(defn add-particles [obj]
 (let [[w h] [(q/width) (q/height)]
       position [(/ w 2.0) (/ h 4.0)]
       velocity [0 4]
       [amount radius] [1 30]
       sr (DwFlowFieldParticles$SpawnRadial.)] ;DwFlowFieldParticles.SpawnRadial sr = new DwFlowFieldParticles.SpawnRadial();
  (spawn-particles obj sr position velocity radius amount)

  (if (q/mouse-pressed?)
   (let [pr (* 0.5 (.getCollisionSize @particles))
         amount (Math/ceil (* 0.01 (.getCount @particles)))
         amount (min 500 amount)
         radius (Math/ceil (Math/sqrt (* amount pr pr)))
         [px py :as position] [(q/mouse-x) (q/mouse-y)]
         [vx vy :as velocity] [(*  5 (- px (q/pmouse-x)))
                               (* -5 (- py (q/pmouse-y)))]]
    (spawn-particles obj sr position velocity radius amount)))))


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


(def reset-trigger (lfo "sawtooth" 0 80 53))

(defn draw []
  (update-state)
  (render-world @pg-world) ;obstacles
  (add-particles @particles)
  (when (> 2 (param/auto-resolve reset-trigger))
   (.reset @particles))
   (.reset @particles) ;dont call from repl btw, crashhh

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

  (q/blend-mode :replace)
  (q/image @pg-canvas 0 0)
  (q/blend-mode :blend)

  (render-gui))


(defn init []
  (q/defsketch fluids
    :title "fluids"
    :features [:resizable]
    :setup setup  :draw draw ;; :update update-state
    :size [600 600]
    ;; :middleware [m/fun-mode m/navigation-3d]
    :renderer :p2d)
 (alter-var-root #'a/*applet* (constantly fluids))) ;like i dont get why is this bad/not done by default?
