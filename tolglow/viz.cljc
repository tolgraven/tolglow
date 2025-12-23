(ns tolglow.viz "Attempt at a workable visualizer using quil and fragments of afterglow's"
 (:require ;[quil.core :as q]
           [quil.core :as q :include-macros true]
           ; [quil.sketch :as a]
           [quil.middleware :as m]
           [taoensso.timbre :as timbre]

           [thi.ng.color.core :as clr]
           [thi.ng.math.core :as cmath]
           #_[thi.ng.geom.vector :as v :refer [vec2 vec3]]))
           ; [clojure.pprint :as pprint]))

(def max-lights 32)

            ; [ola-clojure.ola-service :as ola])
  ; (:import [javax.media.j3d Transform3D]
           ; [javax.vecmath Matrix3d Vector3d]
           ; [com.google.protobuf ByteString]))
; (def buf1 (byte-array 512))
; ;; (ola/GetDmx {:universe 1} #(printer/cprint (-> % :response :data)))
; (defn get-dmx-testy [] ;argh this was causing me tons of startup issues cause it was loose in the code. badbadbad
;  (ola/GetDmx {:universe 1} #(.copyTo (-> % :response :data) buf1 0)))
; ;; (ola/GetUniverseInfo #(clojure.pprint/pprint %))
; (def ran (range 1 513))

; (defn dump-dmx []
;   (doseq [[i b] (partition 2 (interleave ran buf1))]
;   (printf "%4d" (unsign b))
;   (if (= 0 (mod i 32))
;     (println)))) ;; ((vec buf1) 0);; (bytes buf1)
; ;; (dump-dmx)

;; REMEMBER: Processing has Y inverted compared to openGL / afterglow / rest of world
;; (defn show-span "Determine degree to which show spreads over an axis. For X and Z axes this is simply the difference in bounding box
;;   coordinates. For Y, we want to preserve height from the floor, so we use zero as a lower bound on the minimum coordinate."
;;   [show axis]
;;   (let [dim @(:dimensions show)
;;         min-val ((keyword (str "min-" axis)) dim)
;;         lower-bound (if (= axis "y")
;;                       (min 0 min-val)
;;                       min-val)]
;;     (- ((keyword (str "max-" axis)) dim) lower-bound)))

;XXX we need some version of this right? adjust for Processing
;; (defn adjusted-rotations "Get the current orientations of the active spotlights for the visualizer.
;;   Return as a series of columns of the rotation matrices, since it looks like WebGL or THREE.js is a lot happier passing vectors than matrices as uniforms."
;;   ;; [show] ;prob this fn that wouldve taken up all the time.... see below
;;   ;; (apply concat (for [[_ head] (:visualizer-visible @(:dimensions show))]
;;   [lights show]
;;   (apply concat (for [[_ head] lights]
;;                   (let [[rot adjust] [(Matrix3d.) (Matrix3d.)]]
;;                     (.rotX adjust (/ Math/PI 2)) ;; Transform from show orientation to shader orientation
;;                     (.get (:rotation head) rot) ;oh yeah this only ever grabs origin rotation right?
;;                     (.mulNormalize rot adjust)
;;                     [[(.-m00 rot) (.-m10 rot) (.-m20 rot)]
;;                      [(.-m01 rot) (.-m11 rot) (.-m21 rot)]
;;                      [(.-m02 rot) (.-m12 rot) (.-m22 rot)]]))))

(defn correct-heading "Flip y axis to match Processing's weirdo coordinate space"
  [lights]
  (for [[_ {:keys [x y z]}] lights]
   [x (* -1 y) z]))

(defn fx-on-fixture [id fx previous-movement]
 (get previous-movement (keyword (str fx "-" id)) [0 0]))

(defn color? "Whether given fixture (represented as tuple of [id spec], of show's :visualizer-visible map) is potentially emitting light and should be rendered"
  [previous-movement [id fixture-or-head]]
  (when-let [color (fx-on-fixture id "color" previous-movement)] ;; XXX need to take into account dimmers, and someday be based on raw DMX values rather than the current higher-level abstractions.
    (pos? (clr/luminance color))))

(defn active-fixtures "Return fixtures to render, those emitting light."
  []
  :get-visualizer-visible-however...) ;bugs out when viz started with show stopped. fix how?

#_(defn included-fixtures "Should only use fixtures, not heads (strips) considering only 8 lights supported. So can put spheres where they are patched" [show])
(defn colored-fixtures "change to lit asap"
 [previous-movement visualizer-visible]
 (filter (partial color? previous-movement) visualizer-visible))

;; XXX just save the original current transforms per head and use those instead, gets too weird trying to solve back something with multiple solutions...
;; also, everything should be stored and retrievable anyways (post assigners), also so can do more interesting stuff
;; than just htp on raw buffer like. per-light & attribute smoothing over frames etc
;; and it should be automatic, not like this :movement stuff where it's done manually in assigner
; (defn current-rotations [lights previous-movement] ;sorta ok right now except not properly transforming(?) rotated fixtures, plus they hang "backwards" - regular fixtures point up, flipped ones down...
;  (for [[id head] lights]
;   (let [[pan tilt] (fx-on-fixture id "pan-tilt" previous-movement)
;         ; rot (movement/current-rotation head pan tilt)
;         ;; visualizer-perspective (Transform3D.)
;         ;; half-transform (Transform3D.) ;dont get why get 2Pi/360 rot from Pi...
;         dir (Vector3d. 0 0 1)] ;makes lights point at stage, z -1 is actually correct but no good without volumetric/back-illuminated "fog"
;    ;; (.rotY half-transform (* 0.5)) (.rotX half-transform (* 0.5)) (.mul rot half-transform)
;    (.transform rot dir)
;    [(.x dir) (.y dir) (.z dir)]

;    ;; (.rotX visualizer-perspective (/ Math/PI 2)) ;; Add a rotation so we are seeing the rotation from the default perspectve of the visualizer.
;    ;; (.mul rot-half visualizer-perspective)
;    ;; (.transform rot-half dir)
;    ;; [(.x dir) (.y dir) (.z dir)]

;    #_(let [rot-y (Math/atan2 (.x dir) (.z dir)) ;; Get pan
;    new-direction (Vector3d. dir)] ;; Determine aiming vector after pan
;    (.rotY visualizer-perspective (- rot-y))
;    (.transform visualizer-perspective new-direction)
;    [rot-y (- (Math/atan2 (.y dir) (.z dir)))]))))


#_(defn current-rotations [lights previous-movement]
 (for [[id head] lights]
  (let [[pan tilt] (fx-on-fixture id "pan-tilt" previous-movement)
        rot (:calculated-values :yada..)
        dir (vec3 0 0 1)] ;makes lights point at stage, z -1 is actually correct but no good without volumetric/back-illuminated "fog"
   #_(v/as-vec (v/transform rot dir)) ;(.transform rot dir)
   #_[(.x dir) (.y dir) (.z dir)] )))


(defn current-colors "Get the current color values of the active spotlights for the visualizer.  Return as a series of four-element vectors of red, green, blue, and alpha."
  [lights previous-movement]
  (for [[id head] lights]
    (if-let [color (get previous-movement (keyword (str "color-" id)))] ;; (mapv #(double (/ (% color) 255)) [colors/red colors/green colors/blue colors/alpha]))))
     @(clr/as-rgba color)
     [0.0 0.0 0.0])))

(defn fixtures-to-render "Run all update stuff"
 [lights current-values]
 ; (println lights current-values)
 (for [{:keys [id x y z] :as fixture} lights
       :let [{:keys [color] :as state} (get current-values id)]]
  (assoc (merge fixture state)
         :color (if color
                 (-> color clr/as-rgba deref)
                 [0.0 0.0 0.0])
         :y (* -1 y) ;correct heading... dont need to do every frame tho lol (if sort)
         ; :rotation (:rotation state) ;get straight rotation instead of calc from p/t...
         )))

;; PARTICLE / FOG SYSTEM
(defn divv [v n]
 (if (zero? n) v (mapv #(/ % n) v)))

(defn create-particle [location & {:keys [velocity lifespan]}]
  {:location location
   :velocity (or velocity
                 [          (* (q/random-gaussian)  0.5)
                  (- (q/abs (* (q/random-gaussian)  0.3)))
                  (- (q/abs (* (q/random-gaussian)  0.1)))])
   :acceleration [0 0 0]
   :lifespan (or lifespan 255)
   :mass 8000})

(defn add-particle [ps & n]
 (update ps :particles conj (create-particle (:origin ps))))

; (defn split-particle
;  [ps {:keys [location velocity] :as particle}]
;  (update ps :particles
;          conj (create-particle location
;                                {:velocity (divv velocity -2)})))
 ; (update ps :particles
 ;         (fn [p] (conj p (create-particle
 ;                          location :velocity (divv (mapv #(/ (- %1) 2) velocity)))))))
(defn is-dead? [{:keys [lifespan]}] (<= lifespan 0.0))
(defn get-wind "Get wind. Now from mouse location but would have other sources like fog machine origin"
 [& source]
 [(- (q/map-range (q/mouse-x)  0.0 (q/width)   -0.05 0.05))
  (- (q/map-range (q/mouse-y)  0.0 (q/height)  -0.05 0.05)  0.1)
  -0.02])

(defn update-particle [{:keys [mass acceleration velocity location lifespan] :as particle} wind]
  (assoc particle
         :velocity  (mapv + (map / velocity [1.005 1.055 0.99]) acceleration)  ;add vectors, divide by drag
         :location  (mapv + velocity location)
         :lifespan  (- lifespan 1.8)
         :mass      (/ (q/pow (- 255 lifespan) 2.5) 100) ;actually want faster speed early, then slow... guess change back to high mass early but make that keep insertia better (less drag) tho wind low impact
         :acceleration (divv wind mass)))

(defn update-particle-system [{:keys [particles confetti] :as ps}]
   (let [wind (get-wind)]
    ; (println (count particles)) ;stops growing at 284
    (-> ps
        (update :particles (partial map #(update-particle % wind)))  #_(if (and (< 150 (:lifespan p) (>= 0.99 (rand)))) (split-particle ps p));; XXX need some way to split a particle in two by random chance;something tells me wont work lol...
        ; (update :particles #(map (fn [p] #_(if (and (< 150 (:lifespan p) (>= 0.99 (rand)))) (split-particle ps p));; XXX need some way to split a particle in two by random chance
        ;                           (update-particle p wind)) %)) ;something tells me wont work lol...
        (update :particles (partial remove is-dead?) ))))


;; STATE / VARS
(defn start-state "return starting state map. fn in case something isnt valid at load"
 [{:keys [venue] :as setup-cfg}]
 (println setup-cfg)
 ; {:fixtures @((:get-fixtures setup-cfg)) ;eh just have heads with keys instead of weirdo sep lists
 {:fixtures (:get-fixtures setup-cfg) ;eh just have heads with keys instead of weirdo sep lists
  :count 0
  :get-current-values     (:get-current-values     setup-cfg)
  :fog {:particles () :image nil
        :origin [    0   -40    80]
        :drag   [1.005  1.025  0.99]
        :mist {:zoff 0.00 :zincrement 0.07 :increment 0.01}}
  :stage {:w (*  100 6)
          :h (*  100 3)
          :d (*  100 6)}})
  ; :stage {:w (*  100 (apply - (map #(get-in venue [:wall %]) [:right :left])))
  ;         :h (*  100 (get-in venue [:ceiling]))
  ;         :d (* -100 (get-in venue [:wall :stage]))}})
  ; :stage {:w (* #_1   100 (apply - (map #(cfg :venue :wall %) [:right :left])))
  ;         :h (* #_1   100 (cfg :venue :ceiling))
  ;         :d (* #_-1 -100 (cfg :venue :wall :stage))}}); use scale instead if can work for everything...

(defonce lightscounter  (atom 1)) ;cant go in s cause its during draw. unless make decision during update which to use but then much more complex than just counting...
(defonce shader         (atom nil))
(defonce print-ok       (atom false)) ;flip this so can have a fn eg dumping s just for coming frame...
(defonce state-pusher   (atom nil))

(defn print-if-requested [s]
 (when @print-ok
  (reset! print-ok false)
  ; (pprint/pprint s)))
  (print s)))

(defn push-to-state "Put some stuff in state map _once_ not each loop. Avoid relaunching etc"
 [m] (when (map? m) (reset! state-pusher m)))
(defn check-for-new-data "Take any new available data" [s]
 (if-let [m @state-pusher]
  (do (reset! state-pusher nil) ;; (merge s m)) ;tho we possibly want a merging of actual submaps hmm?
      (for [[k v] m] (update s k into v))) ;tho we possibly want a merging of actual submaps hmm?
  s))

(defn trans-points "Like, translate but 0-100 (or 0.0-1.0?) so no div by window size"
 [x y & z]
 (let [x (* x (q/width))
       y (- (* y (q/height)))
       z (when (seq z) (* 100 (first z)))]
  (if z
   (q/translate x y z)
   (q/translate x y))))

(defn begin-gui [{:keys [gui] :as s}]
  ;; (q/with-graphics (:graphics gui)) ;;alternative...
  #?(:clj
     (do (q/no-lights)
         (q/hint :disable-depth-test)))
  (q/push-matrix)
  (q/reset-matrix)
  (q/ortho   0 (q/width)  0 (q/height)) ;left right BOTTOM top hence yeah....  g.ortho(0, viewport[2], -viewport[3], 0, -Float.MAX_VALUE, +Float.MAX_VALUE)
  (q/push-style))

(defn end-gui [s]
  (q/pop-matrix)
  (q/perspective) ;the default perspective (incl in quil) is actually quite fucked, not ortho+restore's fault
  #?(:clj (q/hint :enable-depth-test))
  (q/pop-style))

(defn draw-gui "like what?" [s]
 (begin-gui s)
 (q/color-mode :rgb 255)
;;  (q/translate (* 0.01 (q/width)) (- (* 0.95 (q/height)))) ; upper left of window as baseline...
 (trans-points 0.01 0.95)
 (q/fill 255)
 (q/text-size 14)
;;  (q/rect-mode)
 (q/stroke 255)
 (q/stroke-weight 5)
 (q/text (str (cmath/roundto (q/current-frame-rate) 1)) 0 0)
 (end-gui s))


; (defn draw-mist "overlay fog" [{{{:keys [zoff increment]} :mist} :fog}]
;   (q/noise-detail 6 0.45); Optional: adjust noise detail here
;   (let [pixels (q/pixels)] ;calls loadpixels itself so no need

;   (doseq [x (range (q/width)) y (range (q/height))] ;; For every x,y coordinate in a 2D space, calculate a noise value and produce a brightness value
;      ;; (aset pixels (* x y) (* 255 (q/noise (* x increment) (* y increment) zoff)))) ; // Calculate noise and scale by 255
;      (let [noise (* 255 (q/noise (* x increment) (* y increment) zoff))]
;       ;; (aset pixels (* (q/width) (+ x y)) (int noise)))) ; // Calculate noise and scale by 255
;       (aset pixels (+ x (* (q/width) y)) (int noise)))) ; // Calculate noise and scale by 255
;      ;; (aset pixels (+ x (* (q/width) y)) (q/random 0 255)) // Try using this line instead
;   (q/update-pixels))) ;; zoff += zincrement; XXX Increment zoff in state


(defn draw-particle [{:keys [lifespan] [x y z] :location :as particle} & image]
  (let [d (q/map-range lifespan  0 255  1 (/ (q/width) 4))
        img (first image)]
   ;; (q/with-translation [0 0 z] ;images and ellipses arent 3d so gotta translate...
   (q/with-translation [x y z] ;images and ellipses arent 3d so gotta translate...
    (if (and img (q/loaded? img)) ;might wanna wrap in try? checking nil for loaded no good
     (do (q/tint 255 (* 0.45 lifespan))
         ;; (q/image img x y d d)) ;(q/texture )
         (q/image img 0 0 d d)) ;uh fucker still moves up and down when width of window changes...
     (do (q/fill 140 120 130 (* 0.45 lifespan))
         (q/ellipse x y d d))))))

(defn draw-fog [{{:keys [particles image #_confetti] :as ps} :fog}]
 (q/push-style)
 (q/color-mode :rgb 255) ;;XXX should be 0-1
 (q/stroke 0.2 0.3)
 (q/stroke-weight 0.1)
 #?(:clj (q/hint :disable-depth-mask)) ;transparent part of sprite gets all fucked by blend otherwise
 (doseq [particle particles]
  (draw-particle particle image))
 #?(:clj (q/hint :enable-depth-mask))
 (q/pop-style))


; (defn light-from-fixtures "Lighting shit from (up to 7) fixtures"
;  [{:keys [fixture-render] :as s}]
;  (q/color-mode :rgb 1.0)
;   (when fixture-render
;    (doseq [{:keys [id color x y z]} fixture-render]
;   (when-not (zero? (apply + color)) ;only light with light lol. should be a flag in map, incl prio of what to use etc
;    (when (< @lightscounter 8)
;        (let [[r g b] (map #(* % 255) color)
;              [x y z] (map #(* % 100) [x y z])]
;        #?(:cljs (q/point-light r g b x y z) ;dunno if translation works or need to *100 xyz
;           :clj  (q/spot-light color [x y z] rot (/ q/HALF-PI 2.5) 10)) ;;emitting the fixture's light
;        ; p5js master has spotlight support!
;        (swap! lightscounter inc))
;    #_(q/with-translation (mapv #(* % 100) [x y z]) ;pos
;      (q/with-translation [0 0 -170] ;move light out of the later drawn sphere so can actually see it...
;       (when (< @lightscounter 8)
;        (let [[r g b] (map #(* % 255) color)
;              [x y z] (map #(* % 100) [x y z])]
;         (q/point-light r g b x y z)) ;dunno if translation works or need to *100 xyz
;        ; (q/spot-light color pos rot (/ q/HALF-PI 2.5) 10) ;;emitting the fixture's light
;        (swap! lightscounter inc)))))))))

(defn draw-fixtures "Should split into one drawing (all) fixtures, one setting up lights..."
 [{:keys [fixture-render] :as s}]

  (q/color-mode :rgb #_:hsb 1.00) ;whoops :hsl isnt in clj, use rgb conversion...
  (when fixture-render
   (doseq [{:keys [id color x y z]} fixture-render]
    (q/with-translation (mapv #(* % 100) [x y z]) ;pos
      (q/no-stroke) ;(q/stroke 0.2 0.2) (q/stroke-weight 0.1)
      (apply q/fill color)
      #?(:clj (q/sphere-detail 15))
      (q/sphere 20) ;;symbolizing the fixture itself

      #?(:clj (q/sphere-detail 8))
      (q/with-rotation [0] ;(cons q/PI rot)
       ;; (apply q/stroke (conj (mapv #(- % 0.20) (take 3 color)) 0.8))
       (apply q/stroke color)
       (q/stroke-weight 4) (q/line 0 0 -25,  0  0 75)
       (q/stroke-weight 2) (q/line 0 0   0,  0 35  0)
       (q/with-translation [0 0  77]  (q/sphere 3)) ;also need like a transparent cone to more properly extend from fixture to show beam
       (q/with-translation [0 0 -23]  (q/sphere 4))))))) ;) ;should all be relative to fixture/line size...

(defn draw-stage [{{:keys [w h d] :as stage} :stage :as s}]
  ;we need a texture...
  (q/color-mode :rgb 1.0) ;(q/fill 0.3 0.6 0.5)
  (q/stroke 0.35 0.6)
  (q/stroke-weight 5)
  (q/fill   0.3  0.3 0.3)
  (q/box   w  10  d) ;stage floor
  (q/with-translation [0  15  (* d -2)] ;rest of floor
    (q/box w  10  (* 3 d)))
  (q/with-translation [0  (/ h -2)  (/ d 2)] ;wall behind stage
    (q/box w   h  10)))

(defn base-lights [s]
  (q/color-mode :rgb 1.0) ;(q/fill 0.3 0.6 0.5)
  (q/specular   1.0 0.3 0.5) ;; (q/light-specular)
  ; (q/ambient    0.7 0.3 0.4)
  ; (q/shininess 20.0) ;does this need setting every frame or only once?
  ; (q/directional-light 0.9 0.5 0.7  0.3 0.6 0.4) ;; (q/point-light 0.2 0.2 0.2, 0 -0 0)
  (reset! lightscounter 1))

(defn draw-state "Main loop drawing fn" [s]
  (q/background 0.07)
  (q/stroke 0.2 0.3)
  (q/stroke-weight 0.2)
  ; (q/point-light 250, 250, 250, 200, 300, 50);
  ; (q/no-stroke);
  ; (q/sphere 40);
  ; (q/shader @shader)
   ; (q/reset-shader)
  (let [fs {:base-lights base-lights #_:light-from-fixtures #_light-from-fixtures
            :draw-fixtures draw-fixtures
            :draw-stage draw-stage :draw-fog draw-fog}]
   (doseq [[id f] fs]
    (if (-> s :time id)
     (time (f s))
     (f s)))) ;could also like wrap push/pop style?
#_(draw-gui s)) ;gui breaks it atm...

(defn update-state "Main loop state update fn" [s]
 (print-if-requested (-> s (update :fog dissoc :particles)))
 (let [current-values     @((:get-current-values s))
       fixtures @((:fixtures s))
       lights (take max-lights (vals fixtures))]
  ; (println current-values)
  (-> s
      check-for-new-data ;fixtures should be structured normal tho...
      (assoc-in  [:fixture-render]  (fixtures-to-render lights current-values)) ;use transient for something like this? or just wrap these in one call?
      (update-in [:fog]             (comp add-particle add-particle update-particle-system))
      ; (update-in [:fog :mist :zoff]      +  (-> s :fog :mist :zincrement))
      #_(assoc-in  [:navigation-3d :up]    [0 1 0])))) ;boggles the mind this isnt default... we're not in the fucking ocean

(defn update-state-timed [s]
 (if (:time-update s) (time (update-state s)) (update-state s)))


(defn get-setup [setup-cfg]
 (fn []
  (q/frame-rate    20)
  (q/color-mode    :rgb 1.0)
  #?(:clj (do (q/sphere-detail 24)
              (q/hint :enable-optimized-stroke)))
  ;;  (q/hint :disable-stroke-perspective) ;just testing
  ;;  (q/scale 100) ;useful if actually works on translations etc as well... so auto m -> cm
  ;;  (reset! shader (q/load-shader "pixlightfrag.glsl", "pixlightvert.glsl")) ;;  (reset! shader (q/load-shader "pixlightexfrag.glsl", "pixlightexvert.glsl"))
  ; (let [s (-> (start-state setup-cfg)
  (let [s (start-state setup-cfg)]
   (println s)
   s))) ; setup function returns initial state.

(defn mouse-scroll "Respond to scroll"   [s & what] s)
(defn key-pressed  "Respond to keypress" [s pressed-key]
 (print (:key pressed-key) " ") s)

; (defn settings [] (q/smooth 2)) ;no-smooth errors but smooth 0 works heh
(def starting-camera {:position [  100  200  -300]
                      :straight [-0.05  0.08   0.5]
                      :up       [0 1 0]}) ; XXX should depend on show dimensions etc...

(defn init [& [setup-cfg]] ;so at least currently need :venue from cfg, and getters for previous-movement and visualizer-visible...
 (let [setup (get-setup setup-cfg)]
  (q/defsketch     quil
  :title          "quil visualizer for tolglow"
  :host           "quil"
  :size           [600 400]
  :renderer       :p3d #_:opengl ;java2d default... p2d, opengl, pdf.
  ; :features       [:keep-on-top  :resizable]
  ; :middleware     [m/pause-on-error  m/fun-mode  m/navigation-3d]
  :middleware     [m/fun-mode m/navigation-3d]
  :navigation-3d  (merge {:step-size 60} starting-camera)
  ; :setup          setup
  :setup          (get-setup setup-cfg)
  ; :settings       settings
  :update         update-state-timed
  :draw           draw-state
  :mouse-wheel    mouse-scroll
  :key-pressed    key-pressed
  :on-close       #(println "CLOSING DIz" #_(dissoc % :show :fog))))
  #_(alter-var-root  #'a/*applet* (constantly quil))) ;like i dont get why is this bad/not done by default?
; ;; (defmacro wa [f] `(with-applet quil ~f)) ;not in need anymore

; (defn set-sketch [sketch])
; (defn set-camera [m-pos] (push-to-state {:navigation-3d m-pos}))
; (defn request-print [] (reset! print-ok true))

; (defn shortcuts []
;  (q/debug "something" 100) ;<- prints then pauses sketch [ms] so can check per-frame stuff without wall of fuck
;  (init)
;  (q/random-gaussian)
;  (q/print-camera)
;  (reset! print-ok true)
;  ; (draw-mist s)
;  (q/current-frame-rate))
