(ns tolglow.studio.camera-controls
  "Re-frame camera controls. Camera motion never sends lighting commands."
  (:require [re-frame.core :as rf]))

(def defaults {:mode "orbit" :speed 2 :interacting false :reduced-motion false})
(defn state [db] (merge defaults (:camera-navigation db)))
(rf/reg-sub :camera-navigation/state (fn [db _] (state db)))
(rf/reg-event-db :camera-navigation/observed
  (fn [db [_ incoming]]
    (assoc db :camera-navigation (select-keys incoming [:mode :speed :interacting :reduced-motion :tour-fixture :tour-waiting]))))
(rf/reg-event-fx :camera-navigation/mode
  (fn [_ [_ mode]] (when (contains? #{"orbit" "walk" "tour"} mode) {:studio/preview [:navigation-mode mode]})))
(rf/reg-event-fx :camera-navigation/speed
  (fn [_ [_ speed]] (when (and (number? speed) (js/Number.isFinite speed) (<= 0.25 speed 8))
                     {:studio/preview [:navigation-speed speed]})))
(rf/reg-event-fx :camera-navigation/move
  (fn [_ [_ direction pressed?]]
    (when (and (contains? #{"forward" "backward" "left" "right" "up" "down"} direction) (boolean? pressed?))
      {:studio/preview [:navigation-move [direction pressed?]]})))

(defn- move! [direction active?] (rf/dispatch-sync [:camera-navigation/move direction active?]))
(defn- move-button [direction label glyph]
  [:button.camera-move.button
   {:type "button" :aria-label label :title label
    :on-pointer-down (fn [event]
                       (.preventDefault event) (.setPointerCapture (.-currentTarget event) (.-pointerId event))
                       (move! direction true))
    :on-pointer-up #(move! direction false)
    :on-pointer-cancel #(move! direction false)
    :on-lost-pointer-capture #(move! direction false)
    :on-blur #(move! direction false)
    :on-key-down #(when (contains? #{" " "Enter"} (.-key %)) (.preventDefault %) (move! direction true))
    :on-key-up #(when (contains? #{" " "Enter"} (.-key %)) (.preventDefault %) (move! direction false))}
   glyph])

(defn controls []
  (let [{:keys [mode speed interacting reduced-motion tour-fixture tour-waiting]} @(rf/subscribe [:camera-navigation/state])]
    [:section.camera-navigation {:aria-label "Preview camera navigation"}
     [:div.camera-navigation-row
      [:div.camera-modes {:role "group" :aria-label "Camera movement mode"}
       (for [[value label] [["orbit" "Orbit"] ["walk" "Explore"] ["tour" "Auto tour"]]]
         ^{:key value} [:button.button {:type "button" :aria-pressed (= mode value) :disabled interacting
                                       :on-click #(rf/dispatch [:camera-navigation/mode value])} label])]
      [:label.camera-speed "Speed"
       [:input {:type "range" :aria-label "Camera speed in metres per second" :min 0.25 :max 8 :step 0.25
                :value speed :on-change #(rf/dispatch [:camera-navigation/speed (js/Number (.. % -target -value))])}]
       [:span (str speed " m/s")]]]
     (cond
       interacting [:p.small.muted "Camera paused while editing the scene."]
       (= mode "tour") [:p.small.muted {:role "status"}
                         (cond reduced-motion "Auto tour paused · reduced motion is enabled."
                               tour-fixture (str "Following " tour-fixture " · use the camera to take over")
                               tour-waiting "No lit fixtures · room overview"
                               :else "Finding active fixtures…")]
       (= mode "walk") [:div.camera-explore
                         [:p.small.muted "Drag to look · WASD, HJKL, arrows or two fingers to move · Q/E down/up · Shift to go faster · Pinch to zoom"]
                         [:div.camera-movement {:role "group" :aria-label "Move camera"}
                          [move-button "left" "Move camera left" "←"]
                          [move-button "forward" "Move camera forward" "↑"]
                          [move-button "backward" "Move camera backward" "↓"]
                          [move-button "right" "Move camera right" "→"]
                          [move-button "up" "Raise camera" "Rise"]
                          [move-button "down" "Lower camera" "Lower"]]]
       :else [:p.small.muted "Drag to orbit · Pinch to zoom · WASD, HJKL, arrows or two fingers start Explore"])]))
