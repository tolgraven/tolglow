(ns tolglow.studio.core
  (:require [re-frame.core :as rf]
            [reagent.dom.client :as rdom]
            [tolglow.studio.events]
            [tolglow.studio.subs]
            [tolglow.studio.interop]
            [tolglow.studio.views :as views]
            [tolglow.studio.push-display :as push-display]))

(defonce root (atom nil))
(defonce listeners (atom []))

(defn device-display? [] (= "/push-display" (.-pathname js/location)))

(defn mount! []
  (when-not @root (reset! root (rdom/create-root (.getElementById js/document "app"))))
  (rdom/render @root (if (device-display?) [push-display/component] [views/studio])))

(defn- listen! [target event handler]
  (.addEventListener target event handler)
  (swap! listeners conj [target event handler]))

(defn- install-listeners! []
  (doseq [[target event handler] @listeners] (.removeEventListener target event handler))
  (reset! listeners [])
  (listen! js/window "blur" #(rf/dispatch [:studio/release-pointers]))
  (listen! js/window "pagehide" #(do (rf/dispatch-sync [:studio/release-pointers true]) (rf/dispatch-sync [:chat/cancel])))
  (listen! js/document "visibilitychange" #(when (.-hidden js/document) (rf/dispatch [:studio/release-pointers true])))
  (listen! js/window "keyup" (fn [event]
                               (when (contains? #{" " "Enter"} (.-key event))
                                 (rf/dispatch [:studio/gesture-cancel "keyboard"]))))
  (listen! js/document "keydown"
           (fn [event]
             (when-not (or (.-defaultPrevented event) (.-repeat event) (.-ctrlKey event) (.-metaKey event) (.-altKey event)
                           (.closest (.-target event) "input,textarea,select,[contenteditable=true],.cue-cell,#neovim-host")
                           (.querySelector js/document "dialog[open]"))
               (case (.-key event)
                 ("b" "B") (rf/dispatch [:studio/blackout-toggle])
                 " " (do (.preventDefault event) (rf/dispatch [:studio/transport-toggle]))
                 "1" (rf/dispatch [:studio/set-view :perform])
                 "2" (rf/dispatch [:studio/set-view :compose])
                 "3" (rf/dispatch [:studio/set-view :rig])
                 "4" (rf/dispatch [:studio/set-view :library])
                 "5" (rf/dispatch [:studio/set-view :assistant])
                 "6" (rf/dispatch [:studio/set-view :connections])
                 nil)))))

(defn ^:export init! []
  (if (device-display?)
    (do (.add (.-classList (.-body js/document)) "push-display-page")
        (push-display/install-api!)
        (rf/dispatch-sync [:push-display/update {}]))
    (do (rf/dispatch-sync [:studio/initialize (str (random-uuid))])
        (install-listeners!)))
  (mount!))
