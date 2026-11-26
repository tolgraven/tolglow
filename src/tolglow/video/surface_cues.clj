(ns tolglow.video.surface-cues
  "Spawn a cue that projects video onto a configured surface (#674). The cue
  is placed and launched through `tolglow.cue-spawn`, like a spawned spatial
  volume cue (#407): publication, a free grid cell and an ordinary launch,
  committed together or not at all."
  (:require [tolglow.cue-spawn :as cue-spawn]
            [tolglow.video.surface :as surface]))

(defn- id? [v] (or (keyword? v) (and (string? v) (seq v))))

(defn spawn
  "{:op :surface-cue :surface id} plus optional :id, :name, :page-id, :cell,
  :priority, :channel and :launch. Refuses an unknown surface or an existing
  effect or cue id. The receipt carries :cue-id, :page-id and :cell."
  [model {:keys [surface id name page-id cell priority channel launch]} now command]
  (when-not (get-in model [:settings :video :surfaces surface])
    (throw (ex-info "Choose a configured video surface" {:errors [{:path [:surface] :message "Unknown video surface"}]})))
  (let [id (or id (keyword (str "video-surface-" (if (keyword? surface) (subs (str surface) 1) surface))))
        page-id (or page-id :video)
        effect (surface/cue-effect id surface {:name name :channel (or channel :color)})
        cue (cond-> {:id id :effect-id id :name (:name effect) :version 1}
              priority (assoc :priority priority))]
    (when-not (and (id? id) (id? page-id)
                   (not (get-in model [:registry :effects id])) (not (get-in model [:registry :cues id])))
      (throw (ex-info "Choose a new cue id" {:errors [{:path [:id] :message "That effect or cue id is taken"}]})))
    (cue-spawn/place-and-launch model {:effect effect :cue cue :page-id page-id :page-name "Video" :cell cell
                                       :launch (if (map? launch) launch (when (false? launch) false))
                                       :label "Video surface cue"}
                                now command)))
