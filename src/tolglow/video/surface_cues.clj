(ns tolglow.video.surface-cues
  "Spawn a cue that projects video onto a configured surface (#674). The cue
  is placed and launched like a spawned spatial volume cue (#407): one
  validated publication of an effect and a cue, a free grid cell on a page
  and an ordinary launch, all applied to a private model value and committed
  together or not at all."
  (:require [tolglow.spatial.cues :as spatial-cues]
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
        page (get-in model [:pages page-id] {:id page-id :name "Video" :cells {}})
        pos (spatial-cues/available-cell page cell)
        effect (surface/cue-effect id surface {:name name :channel (or channel :color)})
        cue (cond-> {:id id :effect-id id :name (:name effect) :version 1}
              priority (assoc :priority priority))]
    (when-not (and (id? id) (id? page-id)
                   (not (get-in model [:registry :effects id])) (not (get-in model [:registry :cues id])))
      (throw (ex-info "Choose a new cue id" {:errors [{:path [:id] :message "That effect or cue id is taken"}]})))
    (let [model (:model (spatial-cues/accept model command {:op :registry/publish :kind :effects :definition effect} now))
          model (:model (spatial-cues/accept model command {:op :cue/put :cue cue} now))
          model (:model (spatial-cues/accept model command {:op :page/put :page (assoc-in page [:cells pos] id)} now))
          launched (if (false? launch)
                     {:model model :ack {:status :accepted}}
                     (spatial-cues/accept model command {:op :cue/launch :cue-id id :launch (if (map? launch) launch {})} now))]
      (update launched :ack assoc :cue-id id :page-id page-id :cell pos))))
