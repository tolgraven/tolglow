(ns tolglow.qa-video-surfaces
  "A preview-only show for browser QA of video on 3D surfaces (#674).

  - :sheet, a 6.4 × 3.2 m plane showing the labelled grid behind eight
    horizontal 16-pixel tubes. The tubes stand 0.05–0.4 m in front of it and the
    contact distance is 0.2 m, so the top four rows take the grid and the
    bottom four keep the dim blue wash below.
  - :ball, a sphere showing the gradient source, crossing the sheet's tubes
    at a higher priority than the sheet: two feeds overlapping.
  - :globe, a sphere showing the grid with a 24-pixel ring around its
    equator, turned so its seam faces the audience.

  The pixel counts keep every lit head inside the Room's 128 projected
  output values.

    (require 'tolglow.qa-video-surfaces)
    (tolglow.qa-video-surfaces/start! 18660)   ; returns the origin
    (tolglow.qa-video-surfaces/stop!)

  No output sink is opened."
  (:require [tolglow.demo :as demo]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.studio :as studio]))

(defonce ^:private running (atom nil))

(def sheet-z -3.2)

(def row-distances [0.05 0.1 0.15 0.19 0.21 0.25 0.3 0.4])

(def tubes
  (into (vec (map-indexed (fn [r d]
                            (let [y (- 3.4 (* 0.4 r)) z (+ sheet-z d)]
                              {:id (keyword (str "row-" (inc r))) :shape :path :color-layout :rgb :universe (inc r)
                               :address 1 :pixels 16 :points [[-3.1 y z] [3.1 y z]]}))
                          row-distances))
        [{:id :ring :shape :path :color-layout :rgb :universe 9 :address 1 :pixels 24
          :points (vec (for [i (range 25)]
                         (let [a (* 2 Math/PI (/ i 24.0))]
                           [(+ 4.6 (* 0.95 (Math/sin a))) 2.0 (+ -2.0 (* 0.95 (Math/cos a)))])))}]))

(def surfaces
  {:sheet {:id :sheet :source :grid :shape :plane :position [0 2 sheet-z] :size-m [6.4 3.2]
           :contact {:distance-m 0.2}}
   :ball {:id :ball :source :ramp :shape :sphere :position [-1.6 2.2 (- sheet-z 0.7)] :radius-m 0.9
          :contact {:distance-m 0.15}}
   :globe {:id :globe :source :grid :shape :sphere :position [4.6 2.0 -2.0] :radius-m 0.9
           :rotation-deg [0 180 0] :contact {:distance-m 0.1}}})

(defn- model []
  (-> (demo/model)
      (assoc :rig (layout/materialize {:fixtures [] :pixel-layouts tubes}))
      (assoc-in [:settings :video]
                {:sources {:grid {:kind :test-pattern :width 640 :height 320}
                           :ramp {:kind :test-pattern :width 640 :height 320 :pattern :gradient}}
                 :surfaces surfaces})))

(defn start! [port]
  (let [runtime (show/start! {:model (model) :workspace-options nil :recovery false})
        reply (fn [type p] (deref (show/submit! runtime type p) 2000 nil))]
    (reply :definition {:kind :effects :definition {:id :wash :version 1 :name "Dim wash" :outputs {:color [0.0 0.02 0.08]}}})
    (reply :definition {:kind :cues :definition {:id :wash :effect-id :wash :priority 0}})
    (reply :command {:op :cue/launch :cue-id :wash})
    (doseq [[surface priority] [[:sheet 10] [:ball 20] [:globe 10]]]
      (reply :video-mapping {:op :surface-cue :surface surface :priority priority}))
    (let [{:keys [port stop!]} (studio/listen! runtime {:host "127.0.0.1" :port port})]
      (reset! running {:runtime runtime :stop! stop!})
      (str "http://127.0.0.1:" port))))

(defn stop! []
  (when-let [{:keys [runtime stop!]} @running]
    (stop!)
    (show/stop! runtime)
    (reset! running nil)
    :stopped))
