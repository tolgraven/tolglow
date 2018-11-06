(ns tolglow.quil
  (:require [quil
             [core :as q]
             [middleware :as m]]
            [quil.helpers
             [calc :refer [mul-add]]
             [drawing :refer [line-join-points]]]
            [afterglow
             [show-context :as show-context :refer [*show*]]]))

(defonce start-state
 {:color 0 :hue 0.3
  :angle 0 :x 0})

(defn setup []
 (q/frame-rate 40)
 (q/color-mode :hsb 1.0)
 (q/sphere-detail 40)
 start-state) ; setup function returns initial state.

(defn update-state [s]
 (println )
 (merge s ;gotta update s, not replace, since contains more data than just ours...
        {:color (mod (+ (:color s) 0.7) 0.01) ; Update sketch state by changing circle color and position.
         :hue (+ 0.001 (:hue s))
         :angle (+ (:angle s) 0.01)
         :x (+ (:x s) 0.01)}))

(defn draw-state [s]
  (q/background 0.13) ; Clear sketch by filling bg
  (q/fill 0.3 #_(:hue s) 0.6 0.5) ; Set circle color.
  (q/with-translation [(:x s) 0]
   (q/sphere 10)))

(defn init [& args]
 (q/defsketch tolglow-quil
  :title "quil vizualiser for tolglow", :size [700 500]
  :renderer :opengl #_:p3d ;java2d default... p2d, opengl, pdf.
  :features [#_:keep-on-top :resizable]
  :middleware [m/fun-mode m/navigation-3d]
  :setup setup, :update update-state, :draw draw-state
  ;; :mouse-wheel drag
  :on-close #(println "CLOSING DIz" %)))
