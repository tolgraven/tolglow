(ns tolglow.quil
 (:require [quil.core :as q]
           [quil.middleware :as m]))

(defn setup []
 (q/frame-rate 60) ; Set frame rate to 30 frames per second.
 (q/color-mode :hsb) ; Set color mode to HSB (HSV) instead of default RGB.
 {:color 0 :angle 0}) ; setup function returns initial state. It contains circle color and position.

(defn update-state [state]
 {:color (mod (+ (:color state) 0.7) 150) ; Update sketch state by changing circle color and position.
  :angle (+ (:angle state) 0.01)})

(defn draw-state [state]
  (q/background 80) ; Clear the sketch by filling it with light-grey color.
  (q/fill (:color state) 155 200) ; Set circle color.
  (let [angle (:angle state) ; Calculate x and y coordinates of the circle.
        x (* 100 (q/cos angle))
        y (* 150 (q/sin angle))]
    (q/with-translation [(/ (q/width) 2) ; Move origin point to the center of the sketch.
                         (/ (q/height) 2)]
      (q/ellipse x y 100 180)))) ; Draw the circle.

(defn init [& args]
;;  (q/defsketch testquil
 (q/sketch
  :host "host" ;huh
  :title "glo quil"
  :size [900 700]
  :setup setup ; setup called once during sketch initialization.
  :update update-state ;update-state called on each iteration before draw-state.
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])) ; This sketch uses functional-mode middleware.  Check quil wiki for more info about middlewares and particularly fun-mode.
