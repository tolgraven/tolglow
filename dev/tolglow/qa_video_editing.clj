(ns tolglow.qa-video-editing
  "A preview-only show for browser QA of Studio video editing (#672, #673,
  #674): the labelled grid with two 16-pixel tubes mapped on it, the demo
  rig's eight moving heads for a fixture group, a plane surface in front of
  the tubes, and a 2D plasma shader cue (running; without the +native-gpu
  profile its producer reports that no GPU renderer is available, which
  does not affect editing its definition).

    (require 'tolglow.qa-video-editing)
    (tolglow.qa-video-editing/start! 19780)   ; returns the origin
    (tolglow.qa-video-editing/stop!)

  No output sink is opened."
  (:require [tolglow.demo :as demo]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.video.shader-examples :as ex]))

(defonce ^:private running (atom nil))

(def tubes
  [{:id :tube-1 :name "Tube 1" :shape :path :color-layout :rgb :universe 3 :address 1 :pixels 16 :points [[-3 1.2 -3] [3 1.2 -3]]}
   {:id :tube-2 :name "Tube 2" :shape :path :color-layout :rgb :universe 4 :address 1 :pixels 16 :points [[-3 0.8 -3] [3 0.8 -3]]}])

(defn- model []
  (-> (demo/model)
      (update :rig #(layout/materialize (assoc % :pixel-layouts tubes)))
      (assoc-in [:settings :video]
                {:sources {:grid {:kind :test-pattern :width 640 :height 360}}
                 :mappings {:video/grid {:id :video/grid :source :grid
                                         :strips [{:id :tube-1 :layout :tube-1 :points [[0.1 0.3] [0.9 0.3]] :height 0.05}
                                                  {:id :tube-2 :layout :tube-2 :points [[0.1 0.7] [0.9 0.7]] :height 0.05}]}}
                 :surfaces {:sheet {:id :sheet :source :grid :shape :plane :position [0 1 -3.1] :size-m [6.4 3.6]
                                    :contact {:distance-m 0.2}}}})))

(defn start! [port]
  (let [runtime (show/start! {:model (model) :workspace-options nil :recovery false})
        reply (fn [& args] (deref (apply show/submit! runtime args) 5000 {:status :timeout}))]
    (reply :definition {:kind :effects :definition {:id :wall :version 1 :name "Video wall"
                                                    :outputs {:color {:op :video :mapping :video/grid}}}})
    (reply :definition {:kind :cues :definition {:id :wall :effect-id :wall :priority 10}})
    (reply {:op :cue/launch :cue-id :wall})
    (let [plasma (ex/plasma)]
      (reply :definition {:kind :effects :definition plasma})
      (reply :definition {:kind :cues :definition {:id (:id plasma) :effect-id (:id plasma) :name (:name plasma) :priority 5
                                                   :reload-policy :preserve-state}})
      (reply {:op :cue/launch :cue-id (:id plasma)}))
    (let [{:keys [port stop!]} (studio/listen! runtime {:host "127.0.0.1" :port port})]
      (reset! running {:runtime runtime :stop! stop!})
      (str "http://127.0.0.1:" port))))

(defn stop! []
  (when-let [{:keys [runtime stop!]} @running]
    (stop!)
    (show/stop! runtime)
    (reset! running nil)
    :stopped))
