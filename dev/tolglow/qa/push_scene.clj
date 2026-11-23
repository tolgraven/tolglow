(ns tolglow.qa.push-scene
  "Capture the actual Ring page and device worker with a deterministic, disconnected show."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [tolglow.aim-paths :as paths]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.show.push-scene :as preview]
            [tolglow.studio :as studio]))

(defn- looks [model]
  (map-indexed
   (fn [i {:keys [id position]}]
     (let [x (first position)
           path (paths/definition model {:id (keyword (str "path-" i)) :targets [id]
                                         :points [[(* x -0.5) 0 1] [(* x 1.1) 0 3.4] [(* x -0.3) 0 3.2]]
                                         :duration {:beats 4} :mode :loop :closed? true})]
       (update path :outputs merge {:color (demo/constant (if (even? i) [0.88 0.21 0.035] [0.08 0.43 0.96]))
                                    :dimmer (demo/constant 0.93) :zoom (demo/constant 0.28)})))
   (get-in model [:rig :fixtures])))

(defn- scenic-show []
  (let [base (-> (demo/model)
                 (assoc-in [:rig :room :width] 19)
                 (assoc-in [:rig :trusses 0 :length] 15)
                 (update-in [:rig :fixtures] #(mapv (fn [fixture] (update-in fixture [:position 0] * 1.8)) %)))
        jet {:id :jet :universe 1 :address 200 :position [-7.3 0.45 -0.7] :channels {:fog 1}
             :fogger {:flow-m3-min 140 :mixing-seconds 4}}
        effects (cons {:id :haze :version 1 :name "Stage haze" :targets [:jet] :params {} :outputs {:fog 1}} (looks base))]
    (reduce (fn [model {:keys [id] :as effect}]
              (-> model
                  (assoc-in [:registry :effects id] effect)
                  (assoc-in [:registry :cues id] {:id id :version 1 :effect-id id})))
            (update-in base [:rig :fixtures] conj jet) effects)))

(defn- frames! [runtime]
  (let [cache (preview/create-cache)
        ids (cons :haze (map #(keyword (str "path-" %)) (range 8)))
        starts (mapv #(show/submit! runtime {:op :cue/launch :cue-id %}) ids)]
    (show/tick! runtime 0)
    (assert (every? #(= :accepted (:status @%)) starts))
    (doseq [t (range 250 18000 250)] (show/tick! runtime t))
    (mapv (fn [i]
            (show/tick! runtime (+ 18000 (* i 120)))
            (preview/project-display cache @(:state runtime)
                                     {:display-mode (if (< i 16) :simulator :rig)
                                      :camera (cond (< i 8) :fit (< i 16) :panorama :else :audience)
                                      :fixture-offset (mod i 8)}))
          (range 24))))

(defn -main [directory chromium]
  (let [root (io/file directory)
        _ (.mkdirs root)
        runtime (show/create-runtime {:model (scenic-show)})]
    (try
      (let [{:keys [port stop!]} (studio/listen! runtime {:port 0})]
        (try
          (spit (io/file root "frames.json") (json/write-str (frames! runtime)))
          (let [command ["node" "dev/push-scene-capture.cjs" (str "http://127.0.0.1:" port "/push-display")
                         (.getAbsolutePath root) chromium]
                worker (.start (.inheritIO (ProcessBuilder. ^java.util.List command)))]
            (when-not (zero? (.waitFor worker)) (throw (ex-info "Device capture failed" {}))))
          (finally (stop!))))
      (finally (show/stop! runtime) (shutdown-agents)))))
