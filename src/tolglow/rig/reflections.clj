(ns tolglow.rig.reflections
  "Expose the lit surfaces of a committed rig in world coordinates."
  (:require [afterglow.render.frame :as frame]
            [afterglow.render.optics :as optics]
            [tolglow.rig.surfaces :as surfaces]))

(defn plan
  "Prepare bounded receivers, reflection-enabled fixtures and ordered follow
  constraints when the rig changes. This data never enters an output universe."
  [rig]
  (let [receivers (surfaces/receivers rig)
        by-id (into {} (map (juxt :id identity)) receivers)]
    {:rig rig :receivers receivers
     :reflectors (into {} (comp (filter :pseudo-fixture) (map (juxt :id identity))) (:fixtures rig))
     :links (mapv (fn [{:keys [surface-id] :as link}]
                    (-> link (dissoc :surface-id) (assoc :surface (get by-id surface-id))))
                  (:edge-tracking rig))}))

(defn plan-for [cache rig]
  (let [cached @cache]
    (if (identical? (:rig cached) rig) cached (reset! cache (plan rig)))))

(defn footprint
  "Compute one fixture's footprint on its nearest receiver from a committed
  target row. A fixed fixture uses its mounted optical axis."
  [fixture row receivers]
  (let [pose (or (:pose row) (frame/fixture-pose fixture row))
        position (:position fixture [0 0 0])
        direction (or (:direction pose) (:direction row)
                      (when-let [aim (:aim row)] (mapv - aim position))
                      (:direction (frame/fixture-pose fixture {:pan 0.0 :tilt 0.0})))]
    (when (and direction (some #(not (zero? %)) direction))
      (optics/nearest-footprint (optics/fixture-beam fixture (assoc row :direction direction)) receivers))))

(defn with-footprints
  "Attach :beam-footprint to eligible projected head values. Only fixtures whose
  rows are requested are intersected; callers can select any committed head."
  [{:keys [receivers reflectors]} values]
  (if (and (seq reflectors) (seq values))
    (reduce-kv (fn [result id row]
                 (if-let [fixture (get reflectors id)]
                   (if-let [spot (footprint fixture row receivers)]
                     (assoc result id (assoc row :beam-footprint spot))
                     result)
                   result))
               values values)
    values))
