(ns tolglow.viz "Attempt at a workable visualizer using quil and fragments of afterglow's"
  (:require [quil
             [core :as q :include-macros true]
             [middleware :as m]
             [applet :as a :refer [with-applet]]]
            [quil.helpers
             [calc :as calc :refer [mul-add]]
             [drawing :as drawing :refer [line-join-points]]]
            [afterglow
             [show-context :as show-context :refer [*show*]]
             [fixtures :as fixtures]
             [show :as show]]
            [afterglow.effects.movement :as movement]
            [afterglow.web.layout :as layout]
            [tolglow
             [config :as config :refer [cfg]]]
            [puget.printer :as printer]
            [com.evocomputing.colors :as colors])
  (:import [javax.media.j3d Transform3D]
           [javax.vecmath Matrix3d Vector3d]))

(def max-lights "fuckoff asap, throttle yourself" 32)

(defn show-span "Determine degree to which show spreads over an axis. For X and Z axes this is simply the difference in bounding box
  coordinates. For Y, we want to preserve height from the floor, so we use zero as a lower bound on the minimum coordinate."
  [show axis]
  (let [dim @(:dimensions show)
        min-val ((keyword (str "min-" axis)) dim)
        lower-bound (if (= axis "y")
                      (min 0 min-val)
                      min-val)]
    (- ((keyword (str "max-" axis)) dim) lower-bound)))

(defn adjusted-positions "Move the spotlights so they all fit within the shader's bounding cube, which extends from [-0.5, 0.25, 0.5] to [0.5, -0.25, 0]."
  [lights show scale]
  ;; (partition 3 (for [[_ {:keys [x y z] :as head}] lights]
  ;;               [x y z])))
    (partition 3 (for [[_ head] lights
                       [axis flip] [["x" 1] ["y" -1] ["z" 1]]]
                   (* flip (+ (* ((keyword axis) head) scale))))))

(defn adjusted-rotations "Get the current orientations of the active spotlights for the visualizer.
  Return as a series of columns of the rotation matrices, since it looks like WebGL or THREE.js is a lot happier passing vectors than matrices as uniforms."
  ;; [show] ;prob this fn that wouldve taken up all the time....
  ;; (apply concat (for [[_ head] (:visualizer-visible @(:dimensions show))]
  [lights show] ;prob this fn that wouldve taken up all the time....
  (apply concat (for [[_ head] lights]
                  (let [rot (Matrix3d.)
                        adjust (Matrix3d.)]
                    ;; Transform from show orientation to shader orientation
                    (.rotX adjust (/ Math/PI 2))
                    (.get (:rotation head) rot) ;oh yeah this only ever grabs origin rotation right?
                    (.mulNormalize rot adjust)
                    [[(.-m00 rot) (.-m10 rot) (.-m20 rot)]
                     [(.-m01 rot) (.-m11 rot) (.-m21 rot)]
                     [(.-m02 rot) (.-m12 rot) (.-m22 rot)]]))))

;; TODO: Need to take into account dimmers, and someday be based on raw DMX values rather than the current higher-level abstractions.
(defn active?  "Check whether the given fixture (represented as a tuple of [id spec], as found in a show's :visualizer-visible map) should be
  included in the current visualizer frame, because it is emitting light."
  [show [id fixture-or-head]]
  (when-let [color ((keyword (str "color-" id)) (:previous @(:movement show)))]
    (pos? (colors/lightness color))))

(defn active-fixtures "Return the fixtures which should currently be rendered, because they are emitting light."
  [show] (filter (partial active? show) (:visualizer-visible @(:dimensions show))))

(defn current-rotations
 [lights show]
 (for [[id head] lights]
    (let [[pan tilt] ((keyword (str "pan-tilt-" id)) (:previous @(:movement show)) [0 0])
          rot (movement/current-rotation head pan tilt)
          dir (Vector3d. 0 0 1)] ;makes lights point at stage
          ;; dir (Vector3d. 0 0 -1)] ;no good without fog that can be illuminated from opposite side
     (.transform rot dir)
     (let [rotvec [(.x dir) (.y dir) (.z dir)]]
      rotvec))))

(defn byte-to-double "Convert a one-byte color component, as used in Afterglow, to a floating point color component as used in OpenGL, where 255 becomes 1.0."
  [val] (double (/ val 255)))

(defn current-colors "Get the current color values of the active spotlights for the visualizer.  Return as a series of four-element vectors of red, green, blue, and alpha."
  [lights show]
  (for [[id head] lights]
    (let [color ((keyword (str "color-" id)) (:previous @(:movement show)))]
     (mapv #(byte-to-double (% color)) [colors/red colors/green colors/blue colors/alpha]))))



(defonce particle-system (atom {:particles () :origin []}))

(defn add [[v11 v12] [v21 v22]] [(+ v11 v21) (+ v12 v22)])
(defn add3 [[v11 v12 v13] [v21 v22 v23]] [(+ v11 v21) (+ v12 v22) (+ v13 v23)])
(defn div [[x y] n]     (if (zero? n)  [x y]  [(/ x n) (/ y n)]))
(defn div3 [[x y z] n]  (if (zero? n)  [x y z]  (mapv #(/ % n) [x y z])))

(defn create-particle [location]
  {:location location
   :velocity [ (* (q/random-gaussian) 1.0) (- (* (q/random-gaussian) 1.0) 1.0) (- (q/abs (* (q/random-gaussian) 5.2)))]
   :acceleration [0 0 0]
   :lifespan 255
   :mass 10})

(defn add-particle [ps & n] (update ps :particles #(conj % (create-particle (:origin ps)))))
(defn is-dead? [{:keys [lifespan]}] (< lifespan 0.0))
(defn get-wind "Get wind. Now from mouse location but would have other sources like fog machine origin"
 []
 [(q/map-range (q/mouse-x), 0.0 (q/width), -0.3 0.3)
  (q/map-range (q/mouse-y), 0.0 (q/height), -0.3 0.3)
  1.1
  #_(- (rand 1) 0.5)])

(defn update-particle [{:keys [mass acceleration velocity location lifespan] :as particle} wind]
  (assoc particle
         :velocity (add3 velocity acceleration)
         :location (add3 velocity location)
         :lifespan (- lifespan 1.5)
         :mass (/ (q/pow lifespan 1.5) 100)
         :acceleration (div3 wind mass)))

(declare draw-particle)
(defn run-particle-system [{:keys [particles confetti] :as ps}]
  (doseq [particle particles] (draw-particle particle))
  (let [wind (get-wind)]
    (-> ps
        (update :particles #(map (fn [p] (update-particle p wind)) %))
        (update :particles #(remove is-dead? %)))))


(def start-state
 {;:lights {}
  :positions []
  :colors []
  :rotations []
  :count 0
  :stage {:w (* 100 (apply - (map #(cfg :venue :wall %) [:right :left])))
          :h (* 100 (cfg :venue :ceiling))
          :d (* -100 (cfg :venue :wall :stage))}})

(def camera-set (atom false))
(def camera-start {:position [100 -200 -500] :straight [-0.05 0.08 0.5] :up [0 1 0]})
(defn set-camera-once [s m]
 (if-not @camera-set
  (do (reset! camera-set true)
      (assoc s :navigation-3d camera-start))
  s))

(def print-ok (atom false)) ;flip this so can have a fn eg dumping s just for coming frame...
(def lightscounter (atom 1))
(defn print-if-requested [s]
 (when @print-ok
  (reset! print-ok false)
  (printer/cprint s)))
(defn request-print []
 (reset! print-ok true))

(defmacro wa [f] `(with-applet tolglow-viz ~f))
(defn shortcuts []
 (wa (q/width))
 (wa (q/random-gaussian))
 (wa (q/color 255 0 10 #_lifespan))
 (wa (q/camera 100 -200 -500, 0 -100 0, 0 1 0))
 (wa (q/print-camera))
 (puget.printer/cprint @particle-system)
 )

(defn update-state [s]
 (let [scale 1.0 ;(shader-scale (:show s))
       lights (take max-lights (active-fixtures (:show s)))
       s (assoc s
         :positions (adjusted-positions lights (:show s) scale)
         :colors (current-colors lights (:show s))
         :rotations (current-rotations lights (:show s)))
       s (set-camera-once s camera-start)]
  (print-if-requested (dissoc s :show)) ;bugs out everything if empty...
  s))


(defn draw-particle [{:keys [lifespan] [x y] :location :as particle}]
  (q/color-mode :rgb 255)
  (q/fill 150 110 150 (* 0.45 lifespan))
  (q/tint 255 (* 0.75 lifespan))
  (let [w (q/map-range lifespan  0 255  1 (/ (q/width) 10))
        h w
        img (:image @particle-system)]
   (q/with-translation [-0 -0 80] ;actually fix origin instead...
    (if (q/loaded? img)
    #_(q/image img x y w h) ;(q/texture )
    (q/ellipse x y w h)))))


(defn draw-fixtures [s]
 (when (seq (:positions s))
  (doseq [i (range (count (:positions s)))]
   (let [pos ((vec (:positions s)) i)
         color ((vec (:colors s)) i)
         rot ((vec (:rotations s)) i)
         rot2 [(rot 0) (rot 1) -1]]

    (q/with-translation (mapv #(* % 100) pos)
      (q/with-translation [0 0 -100]
       (swap! lightscounter inc)
       (if (< @lightscounter 9)
        (q/spot-light color pos rot (/ 1.57 2.5) 7 #_0.1 #_600)))
      ;; (apply q/fill color)
      (q/fill (color 0) (color 1) (color 2) 0.7)
      ;; (q/with-translation [(rand 0.3) (rand 0.5) (rand 0.1)]
      (q/with-translation [0 0 0]
       (q/sphere 30))))))) ;size should depend on fixture. big moving heads small leds...

(defn draw-stage [{:keys [stage] :as s}]
  (q/fill 0.3 0.6 0.5)
  (let [{:keys [w h d]} stage]
   (q/with-translation [0 0 0] ;floor
     (q/sphere 10)
     (q/fill 0.4 0.4 0.4)
     (q/box w 10 d)
     (q/with-translation [0 0 (- (+ (/ d 2) 300))] ;wall behind stage
      (q/box w 10 600))
   (q/with-translation [0 (- (/ h 2)) (/ d 2)] ;wall behind stage
    (q/box w h 10)))))


(defn draw-state [s]

  (q/color-mode :rgb 1.0)
  (q/background 0.10) ; Clear sketch by filling bg
  (q/stroke 0.2 0.3)
  (q/stroke-weight 0.5)
  (q/sphere 30)

  (q/specular 1.0)
  ;; (q/light-specular)
  (q/ambient 0.5 0.5 0.5)
  (q/shininess 30.0)
  (q/directional-light 0.7 0.7 0.7  0.3 0.6 0.4)
  (reset! lightscounter 1)

  (draw-fixtures s)
  (draw-stage s)

  ;; (q/no-lights)
    ;; (q/blend-mode :add)
    ;; (q/blend-mode :replace)
  (swap! particle-system (comp add-particle add-particle add-particle add-particle add-particle run-particle-system)))


(defn setup []
 (q/frame-rate 40)
 (q/color-mode :rgb 1.0)
 (q/sphere-detail 40)
 (reset! camera-set false)
;;  (q/current-frame-rate)
 (swap! particle-system assoc
        :origin [0 0 0]
        :particles ()
        :image (q/load-image "fog.png"))

 (merge {:show *show*} start-state)) ; setup function returns initial state.

(defn mouse-scroll "Respond to scroll" [& what])

(defn init [& args]
 (q/defsketch tolglow-viz
  :title "quil vizualiser for tolglow", :size [500 350]
  :renderer :opengl #_:p3d ;java2d default... p2d, opengl, pdf.
  :features [:keep-on-top :resizable]
  :middleware [m/fun-mode m/navigation-3d]
  :setup setup, :update update-state, :draw draw-state
  :mouse-wheel mouse-scroll
  :on-close #(println "CLOSING DIz" (puget.printer/cprint (dissoc % :show)))))
