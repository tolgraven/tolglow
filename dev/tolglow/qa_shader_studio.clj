(ns tolglow.qa-shader-studio
  "A preview-only show for the Studio shader cue QA (#673). Run in a JVM with
  the +native-gpu profile; no physical output is opened.

    (require 'tolglow.qa-shader-studio)
    (tolglow.qa-shader-studio/start! 18640)   ; then dev/studio/shader-cues-browser-qa.mjs
    (tolglow.qa-shader-studio/stop!)

  It publishes three shader cues: the plasma at 640 × 360 (running), the plasma
  at 200 × 1 on a 200-pixel strip (running) and the ray-marched 3D scene with
  a viewport inset in its target (stopped, for the browser to launch)."
  (:require [tolglow.demo :as demo]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.video.shader-examples :as ex]))

(defonce running (atom nil))

(def strip {:id :strip-200 :name "Strip 200" :shape :path :color-layout :rgb :universe 1 :address 1 :pixels 200
            :points [[-2 2.5 0] [2 2.5 0]]})

(defn model []
  (-> (demo/model)
      (assoc :rig (layout/materialize {:fixtures [] :pixel-layouts [strip]}))
      (assoc-in [:settings :video :mappings]
                {:shader-strip (ex/strip-mapping {:source :shader/shader-plasma-strip :layout :strip-200})})))

(defn start! [port]
  (let [runtime (show/start! {:model (model) :workspace-options nil :recovery false})
        reply! (fn [& args] (deref (apply show/submit! runtime args) 5000 {:status :timeout}))]
    (doseq [d [(ex/plasma) (ex/plasma-strip) (ex/scene {:fps 25 :viewport [80 45 480 270]})]]
      (reply! :definition {:kind :effects :definition d})
      (reply! :definition {:kind :cues :definition {:id (:id d) :effect-id (:id d) :name (:name d) :priority 10
                                                   ;; A saved GLSL edit reaches the running instance.
                                                   :reload-policy :preserve-state}}))
    (reply! {:op :cue/launch :cue-id :shader-plasma})
    (reply! {:op :cue/launch :cue-id :shader-plasma-strip})
    (let [{:keys [port stop!]} (studio/listen! runtime {:host "127.0.0.1" :port port})]
      (reset! running {:runtime runtime :stop! stop!})
      (str "http://127.0.0.1:" port))))

(defn stop! []
  (when-let [{:keys [runtime stop!]} @running]
    (stop!)
    (show/stop! runtime)
    (reset! running nil)))
