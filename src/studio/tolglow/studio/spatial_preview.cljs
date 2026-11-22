(ns tolglow.studio.spatial-preview
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [tolglow.studio.interop :as interop]))

(defn watch-visibility
  "Render f with true only while the element intersects the viewport. The
  caller decides which reactions exist; scrolling off releases their leases."
  [f]
  (r/with-let [seen (r/atom false) observer (atom nil) node (atom nil)
               attach (fn [el]
                        (when-not (identical? el @node)
                          (reset! node el)
                          (when @observer (.disconnect @observer) (reset! observer nil))
                          (when el
                            (if (exists? js/IntersectionObserver)
                              (let [watch (js/IntersectionObserver.
                                            (fn [entries] (reset! seen (.-isIntersecting (aget entries 0)))))]
                                (reset! observer watch) (.observe watch el))
                              (reset! seen true)))))]
    [:div {:ref attach} (f @seen)]
    (finally (when @observer (.disconnect @observer)))))

(defn- bodies [config]
  (let [signals @(rf/subscribe [:studio/remote [:spatial/scene]])]
    (mapv (fn [[id spec]]
            (assoc (select-keys spec [:shape :min :max :position :radius :height :parts :planes])
                   :id (pr-str id) :signal (get signals id)))
          (:volumes config))))

(defn scene [show config]
  [watch-visibility
   (fn [visible?]
     [interop/preview (cond-> show
                       (and visible? (:enabled? config) (seq (:volumes config)))
                       (assoc :spatial-volumes (bodies config)))])])
