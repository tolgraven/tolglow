(ns tolglow.studio.spatial-preview
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [tolglow.studio.interop :as interop]
            [tolglow.studio.volume-sculpt :as sculpt]
            [tolglow.studio.volume-geometry :as geometry]
            [tolglow.studio.volume-optics :as optics]))

(defn- shaped? [spec]
  (or (:scale spec) (:translation spec) (:geometry spec)
      (#{:rounded-box :union :intersection :difference} (:shape spec))))

(rf/reg-sub ::volumes (fn [db _] (get-in db [:server-projection :view :settings :spatial :volumes])))
(rf/reg-sub ::fields :<- [::volumes]
  (fn [volumes _]
    (into {} (keep (fn [[id spec]]
                     (when (shaped? spec)
                       (let [moving? (= :ball (:shape spec))
                             local (cond-> spec moving? (assoc :position [0 0 0]))]
                         (when-let [guide (geometry/guides local)]
                           [(pr-str id) (assoc guide :key (hash local) :moving? moving? :handles? false)])))))
          volumes)))

(defn- bodies [config signals]
  (mapv (fn [[id spec]]
          (assoc (select-keys spec [:shape :min :max :position :radius :height :parts :planes])
                 :id (pr-str id) :signal (get signals id)))
        (:volumes config)))

(defn- with-moving-bodies [signals config]
  (reduce-kv (fn [result id spec]
               (if (and (= :ball (:shape spec)) (get-in signals [id :position]))
                 (assoc-in result [id :volume] spec) result))
             signals (:volumes config)))

(defn- optical-frame! [cache signals config moving?]
  (let [old @cache]
    (if (and (identical? signals (:signals old)) (identical? config (:config old)))
      old
      (let [signals-with-volumes (if moving? (with-moving-bodies signals config) signals)
            next (optics/prepare old signals-with-volumes)]
        (vreset! cache (assoc next :signals signals :config config))))))

(defn scene
  ([show config] (scene show config :rig))
  ([show config owner]
   (r/with-let [cache (volatile! nil) visible (r/atom false) changed #(reset! visible %)]
     (let [enabled? (:enabled? config)
           saved? (and (= owner :rig) enabled? (seq (:volumes config)))
           moving? (and enabled? (some #(= :ball (:shape %)) (vals (:volumes config))))
           live? (some #(get-in % [:definition :spatial-volume]) (:instances show))
           signals (when (and @visible (or saved? moving? live?))
                     @(rf/subscribe [:studio/remote [:spatial/scene]]))
           optics (optical-frame! cache signals config moving?)]
       [interop/preview (cond-> show
                          (and @visible (= "sculpt-volume" (get-in show [:editor :tool])))
                          (assoc :sculpt-guide @(rf/subscribe [::sculpt/render-guide]))
                          (and @visible saved?)
                          (assoc :spatial-volumes (bodies config signals) :volume-guides @(rf/subscribe [::fields]))
                          (:limited? optics) (assoc :visible-spatial-limit optics/max-visible)
                          (seq (:objects optics))
                          (assoc :glass-meshes (:meshes optics) :glass-volumes (:objects optics))) owner changed]))))
