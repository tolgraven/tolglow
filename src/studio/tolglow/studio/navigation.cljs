(ns tolglow.studio.navigation
  "Browser history for the Studio shell. Separate display and REPL windows each
  initialize their own document and resources."
  (:require [re-frame.core :as rf]
            [reitit.frontend :as frontend]
            [reitit.frontend.history :as history]
            [tolglow.studio.page-routes :as pages]))

(def router (frontend/router pages/routes))
(defonce active-history (atom nil))

(defn match-path [path] (frontend/match-by-path router path))
(defn href
  ([page] (href page nil))
  ([page query] (frontend/match->path (frontend/match-by-name router page) query)))

(defn current-match []
  (match-path (str (.-pathname js/location) (.-search js/location))))

(defn stop! [] (history/stop! @active-history) (reset! active-history nil))

(defn start! []
  (stop!)
  (reset! active-history
          (history/start! router
            (fn [match _]
              (when (= :studio (get-in match [:data :surface]))
                (rf/dispatch [:studio/route-matched (get-in match [:data :name])])))
            {:use-fragment false
             :ignore-anchor-click?
             (fn [router event anchor uri]
               (and (.hasAttribute anchor "data-view")
                    (= (.-origin anchor) (.-origin js/location))
                    (history/ignore-anchor-click? router event anchor uri)))})))

(rf/reg-fx :studio/navigate
  (fn [[operation page]]
    (when-let [owner @active-history]
      (when (and (pages/views page) (not= (href page) (.-pathname js/location)))
        (case operation
          :push (history/push-state owner page)
          :replace (history/replace-state owner page))))))
