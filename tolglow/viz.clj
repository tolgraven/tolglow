(ns tolglow.viz "Attempt at a workable visualizer using quil and fragments of afterglow's"
  (:require [quil
             [core :as q #_:include-macros #_true]
             [middleware :as m]
             [applet :as a :refer [with-applet]]]
            [quil.helpers
             [calc :as calc :refer [mul-add]]
             [drawing :as drawing :refer [line-join-points]]]
            [afterglow
             [show-context :as show-context :refer [*show*]]
             [fixtures :as fixtures]
             [show :as show]
             [util :as autil :refer [ubyte unsign]]]
            [afterglow.effects.movement :as movement]
            [afterglow.web.layout :as layout]
            [tolglow
             [config :as config :refer [cfg]]
             [color :as color]]
            [ola-clojure.ola-service :as ola]
            [ola-clojure.ola-client :as olac]
            [flatland.protobuf.core :refer [protobuf protobuf-load protobuf-dump]]

            [puget.printer :as printer]
            [com.evocomputing.colors :as colors]
            [thi.ng.color.core :as clr]
            [thi.ng.math.core :as cmath])
  (:import [javax.media.j3d Transform3D]
           [javax.vecmath Matrix3d Vector3d]
           [com.google.protobuf ByteString]))

(def max-lights "fuckoff asap, throttle yourself" 32)
(def show *show*)

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

(defn adjusted-positions "Flip y axis to match Processing's weirdo coordinate space"
  [lights _]
  (for [[_ {:keys [x y z]}] lights] [x (* -1 y) z]))

(defn fx-on-fixture [id fx show]
 ((keyword (str fx "-" id)) (:previous @(:movement show))))

(defn color? "Whether given fixture (represented as tuple of [id spec], of show's :visualizer-visible map) is potentially emitting light and should be rendered"
  [show [id fixture-or-head]]
  (when-let [color (fx-on-fixture id "color" show)] ;; XXX need to take into account dimmers, and someday be based on raw DMX values rather than the current higher-level abstractions.
    (pos? (clr/luminance color))))

(defn active-fixtures "Return fixtures to render, those emitting light."
  [show]
  (when show
   ;; (filter (partial color? show) (:visualizer-visible @(:dimensions show) [])))) ;bugs out when viz started with show stopped. fix how?
    (:visualizer-visible @(:dimensions show) []))) ;bugs out when viz started with show stopped. fix how?
#_(defn included-fixtures "Should only use fixtures, not heads (strips) considering only 8 lights supported. So can put spheres where they are patched"
 [show])
(defn colored-fixtures "change to lit asap"
 [show]
 (filter (partial color? show) (active-fixtures show)))

;; XXX just save the original current transforms per head and use those instead, gets too weird trying to solve back something with multiple solutions...
;; also, everything should be stored and retrievable anyways (post assigners), also so can do more interesting stuff
;; than just htp on raw buffer like. per-light & attribute smoothing over frames etc
;; and it should be automatic, not like this :movement stuff where it's done manually in assigner
(defn current-rotations [lights show] ;sorta ok right now except not properly transforming(?) rotated fixtures, plus they hang "backwards" - regular fixtures point up, flipped ones down...
 (for [[id head] lights]
    (let [[pan tilt] (or (fx-on-fixture id "pan-tilt" show) [0 0])
          rot (movement/current-rotation head pan tilt)
          visualizer-perspective (Transform3D.)
          dir (Vector3d. 0 0 1)] ;makes lights point at stage, -1 actually correct but no good without "fog" that can be illuminated from opposite side

    ;; (.rotX visualizer-perspective (/ Math/PI 2)) ;; Add a rotation so we are seeing the rotation from the default perspectve of the visualizer.
    ;; (.mul rot visualizer-perspective)
    ;; (.transform rot dir)
    ;; (let [rot-y (Math/atan2 (.x dir) (.z dir)) ;; Get pan
    ;;       new-direction (Vector3d. dir)] ;; Determine aiming vector after pan
    ;;   (.rotY visualizer-perspective (- rot-y))
    ;;   (.transform visualizer-perspective new-direction)
    ;;   [rot-y (- (Math/atan2 (.y dir) (.z dir)))]

     (.transform rot dir)
     (let [rotvec [(.x dir) (.y dir) (.z dir)]]
      rotvec))))

; for stuff like this, dimmer, strobe etc actually might be easier (and better for testing!) to loop back
; dmx from OLA and (since we know where all the channels are) just read from there...
(defn current-colors "Get the current color values of the active spotlights for the visualizer.  Return as a series of four-element vectors of red, green, blue, and alpha."
  [lights show]
  (for [[id head] lights]
    (if-let [color ((keyword (str "color-" id)) (:previous @(:movement show)))] ;; (mapv #(double (/ (% color) 255)) [colors/red colors/green colors/blue colors/alpha]))))
     @(clr/as-rgba color)
     [0.0 0.0 0.0])))



;; PARTICLE / FOG SYSTEM
(defn divv [v n] (if (zero? n) v (mapv #(/ % n) v)))

(defn create-particle [location & {:keys [velocity lifespan]}]
  {:location location
   :velocity (or velocity
                 [(* (q/random-gaussian) 1.8)
                  (- (q/abs (* (q/random-gaussian) 1.0)))
                  (- (q/abs (* (q/random-gaussian) 0.2)))])
   :acceleration [0 0 0], :lifespan (or lifespan 255), :mass 1000})

(defn add-particle [ps & n] (update ps :particles #(conj % (create-particle (:origin ps)))))
(defn split-particle
 [ps {:keys [location velocity] :as particle}]
 (update ps :particles (fn [p] (conj p (create-particle location
                                 :velocity (divv (mapv #(/ (- %1) 2) velocity)))))))
(defn is-dead? [{:keys [lifespan]}] (<= lifespan 0.0))
(defn get-wind "Get wind. Now from mouse location but would have other sources like fog machine origin"
 [& source]
 [(- (q/map-range (q/mouse-x), 0.0 (q/width), -0.3 0.3))
  (- (q/map-range (q/mouse-y), 0.0 (q/height), -0.3 0.3) 0.8)
  -0.09])

(defn update-particle [{:keys [mass acceleration velocity location lifespan] :as particle} wind]
  (assoc particle
         :velocity (mapv #(+ %1 %2) (map #(/ %1 %2) velocity [1.005 1.025 0.99]) acceleration)  ;add vectors, divide by drag
         :location (mapv #(+ %1 %2) velocity location)
         :lifespan (- lifespan 1.8)
         ;; :mass (/ (q/pow lifespan 1.5) 100)
         :mass (/ (q/pow (- 260 lifespan) 2.5) 100) ;actually want faster speed early, then slow... guess change back to high mass early but make that keep insertia better (less drag) tho wind low impact
         :acceleration (divv wind mass)))

(defn update-particle-system [{:keys [particles confetti] :as ps}]
  (let [wind (get-wind)]
    (-> ps
        (update :particles #(map (fn [p]
                                  #_(if (and (< 150 (:lifespan p) (>= 0.99 (rand))))
                                   (split-particle ps p));; XXX need some way to split a particle in two by random chance
                                  (update-particle p wind)) %)) ;something tells me wont work lol...
        (update :particles #(remove is-dead? %)))))

;; STATE / VARS
(defn start-state "return starting state map. fn in case something isnt valid at load" []
 {:positions [], :colors [], :rotations []
  :fixtures {:positions [], :colors [], :rotations []}
  :count 0
  :show *show* ;no the whole thing is here prob slowing down each update loads. change to quoted and eval to access, dunno?
  :fog {:particles () :origin [0 -140 80] :drag [1.005 1.025 0.99] :image nil
        :mist {:zoff 0.00 :zincrement 0.07 :increment 0.01}}
  :stage {:w (* #_1   100 (apply - (map #(cfg :venue :wall %) [:right :left])))
          :h (* #_1   100 (cfg :venue :ceiling))
          :d (* #_-1 -100 (cfg :venue :wall :stage))}}); use scale instead if can work for everything...

(defonce lightscounter (atom 1)) ;cant go in s cause its during draw. unless make decision during update which to use but then much more complex than just counting...
(defonce shader (atom nil))
(defonce print-ok (atom false)) ;flip this so can have a fn eg dumping s just for coming frame...
(defonce state-pusher (atom nil))

(defn print-if-requested [s] (when @print-ok (reset! print-ok false) (printer/cprint s)))

(defn push-to-state "Put some stuff in state map _once_ not each loop. Avoid relaunching etc"
 [m] (when (map? m) (reset! state-pusher m)))
(defn check-for-new-data "Take any new available data" [s]
 (if-let [m @state-pusher]
  (do (reset! state-pusher nil) (merge s m))
  s))


(defn update-state [s] "Main loop state update fn"
 (print-if-requested (-> s (dissoc :show) (update :fog dissoc :particles)))
 (let [lights (take max-lights (active-fixtures (:show s)))]
  (-> s
      check-for-new-data ;; not sure how much will actually need but
      (assoc-in [:fixtures :positions] (adjusted-positions lights (:show s)))
      (assoc-in [:fixtures :colors] (current-colors lights (:show s)))
      (assoc-in [:fixtures :rotations] (current-rotations lights (:show s)))
      (update :fog (comp add-particle add-particle add-particle add-particle update-particle-system)))))


(defn set-stroke "like if got those settings in state and could set automatically..." [s])


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
 (q/hint :disable-depth-mask) ;transparent part of sprite gets all fucked by blend otherwise
 (doseq [particle particles] (draw-particle particle image))
 (q/hint :enable-depth-mask)
 (q/pop-style))


(defn light-from-fixtures "Lighting shit from (up to 7) fixtures"
 [{{:keys [positions colors rotations] :as s} :fixtures}]
 (q/color-mode :rgb 1.0)
  (doseq [[pos color rot] (partition 3 (interleave positions colors rotations))]
  (when-not (zero? (apply + color)) ;only light with light lol. should be a flag in map, incl prio of what to use etc
   (q/with-translation (mapv #(* % 100) pos) ;pos
     (q/with-translation [0 0 -170 #_-1] ;move light out of the later drawn sphere so can actually see it...
      (when (< @lightscounter 8)
       (q/spot-light color pos rot (/ q/HALF-PI 2.5) 10) ;;emitting the fixture's light
       (swap! lightscounter inc)))))))

(defn draw-fixtures "Should split into one drawing (all) fixtures, one setting up lights..."
 [{{:keys [positions colors rotations] :as s} :fixtures}]
 (when (seq positions)
  (q/color-mode :rgb #_:hsb 1.00) ;whoops :hsl isnt in clj, use rgb conversion...
  (doseq [[pos color rot] (partition 3 (interleave positions colors rotations))]
    (q/with-translation (mapv #(* % 100) pos) ;pos
      (q/no-stroke) ;(q/stroke 0.2 0.2) (q/stroke-weight 0.1)
      (apply q/fill color)
      (q/sphere-detail 15)
      (q/sphere 20) ;;symbolizing the fixture itself

      (q/sphere-detail 8)
      (q/with-rotation (cons q/PI rot)
       ;; (apply q/stroke (conj (mapv #(- % 0.20) (take 3 color)) 0.8))
       (apply q/stroke color)
       (q/stroke-weight 4) (q/line 0 0 -25, 0 0 75)
       (q/stroke-weight 2) (q/line 0 0 0, 0 35 0)
       (q/with-translation [0 0 77] (q/sphere 3))
       (q/with-translation [0 0 -23] (q/sphere 4))
       ;also need like a transparent cone to more properly extend from fixture to show beam
       ))))) ;) ;should all be relative to fixture/line size...

(defn draw-stage [{{:keys [w h d] :as stage} :stage :as s}]
  (q/fill 0.3 0.6 0.5)
  (q/stroke 0.35 0.6)
  (q/stroke-weight 5)
   (q/fill 0.4 0.4 0.4)
   (q/box w 10 d) ;stage floor
   (q/with-translation [0 15 (- (* 2 d))] ;rest of floor
    (q/box w 10 (* 3 d)))
   (q/with-translation [0 (- (/ h 2)) (/ d 2)] ;wall behind stage
    (q/box w h 10)))

(defn base-lights [s]
  (q/specular 1.0) ;; (q/light-specular)
  (q/ambient 0.5 0.5 0.5)
  (q/shininess 20.0) ;does this need setting every frame or only once?

  (q/directional-light 0.7 0.7 0.7  0.3 0.6 0.4) ;; (q/point-light 0.2 0.2 0.2, 0 -0 0)
  (reset! lightscounter 1))

(defn draw-state "Main loop drawing fn" [s]
  (q/background 0.07) ; Clear sketch by filling bg
  (q/stroke 0.2 0.3) (q/stroke-weight 0.2)
  ;; (q/shader @shader)
  ;; (q/reset-shader)
  (let [fs {:base-lights base-lights :light-from-fixtures light-from-fixtures
            :draw-fixtures draw-fixtures
            :draw-stage draw-stage :draw-fog draw-fog}]
   (doseq [[id f] fs]
    (if (-> s :time id)
     (time (f s))
     (f s)))) ;could also like wrap push/pop style?
(draw-gui s))

  (let [draw-fns [base-lights draw-fixtures draw-stage draw-fog]]
   (doseq [f draw-fns] (f s)))

  #_(q/no-lights))

(defn setup []
 (q/frame-rate 40)
 (q/color-mode :rgb 1.0)
 (q/sphere-detail 40)
;;  (q/scale 100) ;useful if actually works on translations etc as well... so auto m -> cm
 (reset! shader (q/load-shader "pixlightfrag.glsl", "pixlightvert.glsl")) ;;  (reset! shader (q/load-shader "pixlightexfrag.glsl", "pixlightexvert.glsl"))
;;  (q/hint :disable-stroke-perspective) ;just testing
;;  (q/current-frame-rate)
 (let [ss (assoc-in (start-state) [:fog :image] (q/load-image "fog.png"))]
 ss)) ; setup function returns initial state.

(defn mouse-scroll "Respond to scroll" [s & what] s)
(defn key-pressed "Respond to keypress" [s pressed-key]
 (print (:key pressed-key) " ")
 s)

(defn settings [] (q/smooth 2)) ;no-smooth errors but smooth 0 works heh

(def starting-camera {:position [100 -200 -300] :straight [-0.05 0.08 0.5] :up [0 1 0]}) ; XXX should depend on show dimensions etc...

(defn init [& args]
 (q/defsketch quil
  :title "quil visualizer for tolglow", :size [600 400]
  :renderer :opengl ;java2d default... p2d, opengl, pdf.
  :features [:keep-on-top :resizable]
  :middleware [m/pause-on-error m/fun-mode m/navigation-3d]
  :navigation-3d (merge {:step-size 60} starting-camera)
  :setup setup, :settings settings, :update update-state-timed, :draw draw-state
  :mouse-wheel mouse-scroll, :key-pressed key-pressed
  :on-close #(println "CLOSING DIz" (puget.printer/cprint (dissoc % :show :fog))))
 (alter-var-root #'a/*applet* (constantly quil))) ;like i dont get why is this bad/not done by default?


(defn set-camera [m-pos] (push-to-state {:navigation-3d m-pos}))
(defn request-print [] (reset! print-ok true))

(defmacro wa [f] `(with-applet tolglow-viz ~f))
(defn shortcuts []
 (wa (q/width))
 (wa (q/random-gaussian))
 (wa (q/print-camera))
 (wa (q/no-loop))
 (wa (q/start-loop))
 (puget.printer/cprint ))
