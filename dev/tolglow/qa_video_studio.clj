(ns tolglow.qa-video-studio
  "A preview-only show for browser QA of the Studio video strip editor (#672):
  four RGB pixel tubes, a deterministic test grid and, when the native bridge
  is built, a Syphon source published by `TolglowVideoBridge serve`. A video
  cue is running so the Room preview shows the sampled colours too.

    (require 'tolglow.qa-video-studio)
    (tolglow.qa-video-studio/start! 4580)   ; returns the origin
    (tolglow.qa-video-studio/stop!)

  No output sink is opened."
  (:require [clojure.java.io :as io]
            [tolglow.demo :as demo]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.studio :as studio]))

(defonce ^:private running (atom nil))

(def tubes
  (mapv (fn [i] {:id (keyword (str "tube-" (inc i))) :shape :path :color-layout :rgb :universe 1
                 :address (inc (* i 96)) :pixels 32 :points [[(- (* 1.2 i) 1.8) 0.4 -3] [(- (* 1.2 i) 1.8) 3.2 -3]]})
        (range 4)))

(defn- model [syphon?]
  (-> (demo/model)
      (assoc :rig (layout/materialize {:fixtures [] :pixel-layouts tubes}))
      (assoc-in [:settings :video]
                {:sources (cond-> {:test-grid {:kind :test-pattern :width 640 :height 360}}
                            syphon? (assoc :syphon/qa-grid {:kind :syphon :app "TolglowVideoBridge" :name "Tolglow QA Grid"}))
                 :mappings {:video/test-grid
                            {:id :video/test-grid :source :test-grid
                             :strips [{:id :tube-1 :layout :tube-1 :points [[0.08 0.9] [0.3 0.1]] :height 0.06}
                                      {:id :tube-2 :layout :tube-2 :points [[0.35 0.12] [0.65 0.12]] :height 0.08}
                                      {:id :tube-3 :layout :tube-3 :points [[0.4 0.5] [0.6 0.62] [0.9 0.4]] :height 0.05}
                                      {:id :tube-4 :layout :tube-4 :points [[0.95 0.92] [0.55 0.86]] :height 0.03 :filter :nearest}]}}})))

(defn start!
  ([port] (start! port (System/getenv "TOLGLOW_VIDEO_BRIDGE")))
  ([port bridge]
   (let [syphon? (and bridge (.canExecute (io/file bridge)))
         server (when syphon?
                  (.start (ProcessBuilder. ^java.util.List [bridge "serve" "--name" "Tolglow QA Grid" "--width" "1280" "--height" "720"])))
         runtime (show/start! {:model (model syphon?) :workspace-options nil :recovery false
                               :video-options {:bridge bridge}})
         reply (fn [p] (let [r (show/submit! runtime (if (map? p) p {:op p}))] (deref r 2000 nil)))]
     (show/submit! runtime :definition {:kind :effects :definition {:id :video-wall :version 1 :name "Video wall"
                                                                     :outputs {:color {:op :video :mapping :video/test-grid}}}})
     (show/submit! runtime :definition {:kind :cues :definition {:id :video-wall :effect-id :video-wall :priority 10}})
     (Thread/sleep 200)
     (reply {:op :cue/launch :cue-id :video-wall})
     (let [{:keys [port stop!]} (studio/listen! runtime {:host "127.0.0.1" :port port})]
       (reset! running {:runtime runtime :stop! stop! :server server})
       (str "http://127.0.0.1:" port)))))

(defn stop! []
  (when-let [{:keys [runtime stop! ^Process server]} @running]
    (stop!)
    (show/stop! runtime)
    (when server (.close (.getOutputStream server)) (.waitFor server 3 java.util.concurrent.TimeUnit/SECONDS)
      (when (.isAlive server) (.destroy server)))
    (reset! running nil)
    :stopped))
