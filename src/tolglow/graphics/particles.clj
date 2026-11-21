(ns tolglow.graphics.particles
  "Put particle-system thing (newtonian app) here.
   Use to improve mine / for visualizer / for param animations (web) but also
   try to incorporate with Params. etc and see what afterglow can do controlling 3d animations..."
  (:require [newtonian.utils :as utils]
            [tolglow.graphics.particle-system :as ps]
            [tolglow.param :as param]
            [quil.core :as q]
            [quil.middleware :as m]
            [quil.applet :as a :refer [with-applet]]
            [thi.ng.color.core :as clr]
            [thi.ng.math.core :as cmath])
  ;; (:import (newtonian.corporum ParticleEmitter Particle Field)
  (:import [tolglow.graphics.particle_system ParticleEmitter Particle Field]
           [newtonian.utils Vector2D]))

(defn init-state []
 {:particles []
  :emitters [(ps/build-particle-emitter
              (Vector2D. 330.0 280.0) (Vector2D. 1.5 1.0))]
  #_:fields #_[(add-field (Vector2D. 380.0 285.0) 3000.0)]})
; plan: take this, (or just take the good bits) and adapt for web viz.
; maybe some for main viz/use 3d stuff for actual visuals, but mainly thinking
; small system for cue visualizer, crazy futuristic and not much cpu...

; ALSO flatten it out (yes 1d) and push straight to leds ey!!
; physicsy stuff def good for organic strip movement, but could also work for moving heads etc?
; add fields as here then put some bounce on eg an aim point...
; then modulate force so more powerfull on beat
(defonce shader (atom nil))
(defonce max-particles (atom 5000))
(defonce fields (atom []))
(def state (atom {})) ;fun-mode so can't mod outside pipeline but still useful for observation
(def queue (atom {})) ;shit waiting to be put in


(defn add-field [#^Vector2D coord #^double mass]
  (swap! fields conj (Field. coord 8 mass)))

(defn create-random-emitter []
 (ps/build-particle-emitter
          (Vector2D. (- (rand 330.0) (/ 330 2))   (rand 280.0))
          (Vector2D. (- (rand 2.5) 3.5)      (rand 2.0))
          :size (+ 5 (rand 8)) :spread 3.14 #_(/ 3.1415 (/ 128 (rand 8)))
          :emission-rate (rand-int 6)
          :life (rand 555)))

(defn apply-gravity [p fields]
  (reduce ps/influence
          (assoc p :accel (Vector2D. 0.0 0.0)) ;why? we already have acc yeah?
          fields))


(defn add-new-particles [{:keys [particles emitters] :as s}]
  (if (< (count particles) @max-particles)
    (let [new-particles (for [emitter emitters ;here we would first resolve params when have dynamic emitters, etc
                              _ (range (:emission-rate emitter))]
                          (ps/emit emitter))]
      (update s :particles into new-particles))
    s))

(defn out-of-bounds? [p x-bounds y-bounds]
  (let [{:keys [x y]} (:position p)]
    (or (neg? x) (> x x-bounds)
        (neg? y) (> y y-bounds))))

(defn add-movement [obj]
 (-> (apply-gravity obj @fields) ps/move))


(defn update-particles* "Updates each particle new position, and removes 'dead' and/or out of bounds particles"
  [particles x-bounds y-bounds]
  (let [updated (for [p particles :let [p (update p :age inc)]
                      :when (or (<= (:death p) 0)
                                (< (:age p) (:death p)))]
                  (add-movement p))]
    (remove #(out-of-bounds? % x-bounds y-bounds) updated)))

(defn update-particles [s x-bounds y-bounds]
  (update s :particles update-particles* x-bounds y-bounds))



(defn do-if-out-of-bounds? [p x-bounds y-bounds action]
  (let [{:keys [x y]} (:position p)]
    (if (or (neg? x) (> x x-bounds)
            (neg? y) (> y y-bounds))
     (action p (Vector2D. 0.0 0.0)) ;reset pos
     p)))

(defn update-objs "Updates each obj new position, and removes 'dead' and/or out of bounds particles"
  [objs x-bounds y-bounds actions] ;eg check for death would be an action, same check oob
  ;also custom solution eg moving escaped particles to 0 0...
  (let [updated (for [o objs #_:let #_[o (update o :age inc)]
                      #_:when #_(or (<= (:death o) 0)
                                (< (:age o) (:death o)))]
                  (-> o ;then would nil to get it pruned
                      add-movement
                      (do-if-out-of-bounds? x-bounds y-bounds actions)
                      ;; (cond-> (out-of-bounds? x-bounds y-bounds))
                      #_check-death #_out-of-bounds? #_etc))]
    (filter identity updated)))



(defn import-to-state! [kind obj]
 (swap! queue assoc kind obj)) ;bit overkill when empties every frame tho...

(defn restore-state! []
 (reset! queue (init-state))) ;this will fuck the middleware tho...

(defn pull-from-queue "Take any new available data" [s]
 (if (seq @queue)
  (let [m @queue]
   (reset! queue {})
   (into {}
         (for [[k v] m]
          (update s k concat v)))) ;corr?
  s))

(def saw (param/quick-lfo "sawtooth" :lo 0.01 :high 0.9 :beats 21))
(defn only-when [s f]
 (if (< 0.1 (param/auto-resolve saw) 0.3)
           f #_(update s :emitters update-objs 500 500 ps/move)
       s))

(defn update-state [s]
 (-> s
     (pull-from-queue)
     (add-new-particles)
     (update-particles 2000 3000)
     ;; (update s :emitters update-objs 500 500 ps/move)
     ;; (only-when)
     (only-when #(update s :emitters update-objs 500 500 ps/move)) ;should work?
     ))


(defn draw-mass [{:keys [position] :as obj}] ;change to just like, draw-entity for both field, emitter, whatever. use properties and that
  (let [{:keys [x y]} position
        mass (:mass obj 1)
        size (:size obj 10)]
        (when size (q/ellipse x y size size))
        (when mass (q/sphere (cmath/abs* (/ mass 50))))))

(defn draw-particle [{:keys [position velocity accel] :as p}]
  (let [{:keys [x y]} position
        a (utils/mag accel)
        s (utils/mag velocity)]
    #_(q/fill (+ 166 a)
            (/ (* 220 0.5) s)
            (* 76 s)
            255) ;; (max (* a 95) 255))) ;should modulate alpha too
        ;; (min 85 (max 255 (- (+ 50 (:death p)) (:age p)))))) ;should modulate alpha too
    ;; (q/ellipse x y (int (* a 20)) 5)))
    (q/ellipse x y (max 6 (* s a 9)) (max 3 (* s s 6)))))

(def sine (param/quick-lfo "sine" :lo 0.5 :high 1.5 :beats 13))

(defn draw [{:keys [particles emitters] :as s}]
  (q/background (* 11 (param/auto-resolve sine)) )
  (q/lights)
  ;; (q/filter-shader @shader) (q/shader @shader)
  (q/no-stroke)
  (q/fill (+ 166 1)
          (min 255 (* 0.9 (mod (count particles) (cmath/roundto (q/current-frame-rate) 1))))
          (* 76 0.7 (param/auto-resolve sine))
          (* 255 0.9))
  (doseq [p particles] (draw-particle p))

  (q/no-stroke)
  (q/fill 200 94 64)
  (doseq [field @fields] (draw-mass field))

  (q/fill 94 64 200)
  (doseq [pe emitters] (draw-mass pe))

  ;; (q/reset-shader)
  (q/fill 215)
  (q/text-size 14)
  (q/stroke 155) (q/stroke-weight 5)
  (q/text (str (cmath/roundto (q/current-frame-rate) 1)) 20 50)
  (q/text (str (cmath/roundto (count particles) 1)) 20 70)
  (q/text (str (count emitters)) 20 90))


(defn setup []
  (q/frame-rate 40)
  (q/background 8)
  (q/color-mode :rgb 255)
  (q/no-stroke)
  (q/hint :enable-optimized-stroke)
  (reset! fields [])
  (add-field (Vector2D. 380.0 285.0) 3000.0)
  ;; (reset! shader (q/load-shader "blur.glsl"))
  (init-state))
  ;; (reset! state init-state))

(defn settings [] (q/smooth 2)) ;no-smooth errors but smooth 0 works heh

(defn init []
  (q/defsketch particles
    :title "Particles"
    :features [:keep-on-top :resizable]
    :setup setup :settings settings
    :update update-state :draw draw
    :size [700 800]
    :middleware [m/fun-mode m/navigation-3d]
    :renderer :opengl #_:java2d #_:p3d #_:p2d)
 #_(alter-var-root #'a/*applet* (constantly particles))) ;like i dont get why is this bad/not done by default?

(defn add-random-emitter []
  (import-to-state! :emitters [(create-random-emitter)]))

(defn add-random-field []
  (add-field (Vector2D. (- (rand 330.0) (/ 330 2))
                        (- (rand 200.0)))
             (- (rand 3000.0) 700)))



(defn stash []
  (add-random-emitter)
  (add-random-field) ;apart from making emitters updatable like particles, maybe abstract it completely so can spawn an emitter emitter?
  (reset! max-particles 5000)
  (swap! fields pop)
  (restore-state!))

(defn lall []
(init))
