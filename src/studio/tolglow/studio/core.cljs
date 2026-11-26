(ns tolglow.studio.core
  (:require [re-frame.core :as rf]
            [reagent.dom.client :as rdom]
            [tolglow.studio.events]
            [tolglow.studio.navigation :as nav]
            [tolglow.studio.panel-layout :as panel-layout]
            [tolglow.studio.page-routes :as pages]
            [tolglow.studio.subs]
            [tolglow.studio.interop]
            [tolglow.studio.views :as views]
            [tolglow.studio.push-display :as push-display]
            [tolglow.studio.repl :as repl]))

(defonce root (atom nil))

(defonce listeners (atom []))

(defn mount! []
  (when-not @root (reset! root (rdom/create-root (.getElementById js/document "app"))))
  (rdom/render @root
               (case (get-in (nav/current-match) [:data :surface])
                 :push-display [push-display/component]
                 :repl [repl/console-view]
                 :studio [views/studio]
                 [:main [:h1 "Page not found"] [:a {:href (nav/href :home)} "Open Studio"]])))

(defn- listen! [target event handler]
  (.addEventListener target event handler)
  (swap! listeners conj [target event handler]))

(defn- install-listeners! []
  (doseq [[target event handler] @listeners] (.removeEventListener target event handler))
  (reset! listeners [])
  (listen! js/window "blur" #(rf/dispatch [:studio/release-pointers]))
  (when (fn? (aget js/window "matchMedia"))
    (doseq [query (vals panel-layout/media-queries)]
      (listen! (js/matchMedia query) "change" #(rf/dispatch [:panel-layout/class (panel-layout/media-class)]))))
  (listen! js/window "pagehide" #(do (rf/dispatch-sync [:studio/release-pointers true]) (rf/dispatch-sync [:chat/cancel]) (rf/dispatch-sync [:repl/close])))
  (listen! js/document "visibilitychange" #(when (.-hidden js/document) (rf/dispatch [:studio/release-pointers true])))
  (listen! js/window "keyup" (fn [event]
                               (when (contains? #{" " "Enter"} (.-key event))
                                 (rf/dispatch [:studio/gesture-cancel "keyboard"]))))
  (listen! js/document "keydown"
           (fn [event]
             (if (and (not (.-defaultPrevented event)) (or (.-ctrlKey event) (.-metaKey event)) (= "`" (.-key event)))
               (do (.preventDefault event) (rf/dispatch [:repl/toggle]))
             (when-not (or (.-defaultPrevented event) (.-repeat event) (.-ctrlKey event) (.-metaKey event) (.-altKey event)
                           (.closest (.-target event) "input,textarea,select,[contenteditable=true],.cue-cell,#neovim-host")
                           (.querySelector js/document "dialog[open]"))
               (case (.-key event)
                 ("b" "B") (rf/dispatch [:studio/blackout-toggle])
                 " " (when-not (.closest (.-target event) "button,[role=button]")
                       (.preventDefault event)
                       (rf/dispatch [:studio/transport-toggle]))
                 (when-let [route (pages/shortcut-routes (.-key event))]
                   (rf/dispatch [:studio/set-view route]))))))))

(defn ^:export init! []
  (let [match (nav/current-match)]
    (case (get-in match [:data :surface])
      :push-display (do (.add (.-classList (.-body js/document)) "push-display-page")
                        (push-display/install-api!)
                        (rf/dispatch-sync [:push-display/update {}]))
      :repl (let [session (get-in match [:query-params :session-id])]
              (rf/dispatch-sync [:repl/open true (if (string? session) session "main")]))
      :studio (do (rf/dispatch-sync [:studio/initialize (str (random-uuid))])
                  (rf/dispatch-sync [:panel-layout/initialize])
                  (nav/start!)
                  (install-listeners!))
      nil))
  (mount!))
