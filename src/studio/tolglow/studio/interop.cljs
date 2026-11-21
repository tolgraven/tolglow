(ns tolglow.studio.interop
  "Lifecycle-owned graphics/editor engines. Show state stays in re-frame."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]))

(defonce engines (atom {}))
(defn- message [error] (or (.-message error) (str error)))

(rf/reg-fx :studio/preview
  (fn [[action value]]
    (when-let [engine (:preview @engines)]
      (case action
        :fog (js-invoke engine "setFog" value)
        :burst (js-invoke engine "burst")
        :reset (js-invoke engine "resetCamera")
        :resize (js-invoke engine "resize")
        :worklight (when (fn? (aget engine "setWorkLight")) (js-invoke engine "setWorkLight" value))
        :lighting-mode (js-invoke engine "setLightingMode" value)
        :quality (when (fn? (aget engine "setQuality")) (js-invoke engine "setQuality" value))
        :camera (when (fn? (aget engine "setCamera")) (js-invoke engine "setCamera" value))
        :focus (when (fn? (aget engine "focusFixture")) (js-invoke engine "focusFixture" value))
        :navigation-mode (when (fn? (aget engine "setNavigationMode")) (js-invoke engine "setNavigationMode" value))
        :navigation-speed (when (fn? (aget engine "setNavigationSpeed")) (js-invoke engine "setNavigationSpeed" value))
        :navigation-move (when (fn? (aget engine "moveCamera")) (js-invoke engine "moveCamera" (first value) (second value)))
        nil))))
(rf/reg-fx :studio/preview-fullscreen
  (fn [_]
    (when-let [panel (.getElementById js/document "preview-panel")]
      (let [active (or (.-fullscreenElement js/document) (aget js/document "webkitFullscreenElement"))
            target (if (= active panel) js/document panel)
            method (if (= active panel)
                     (if (fn? (aget js/document "exitFullscreen")) "exitFullscreen" "webkitExitFullscreen")
                     (if (fn? (aget panel "requestFullscreen")) "requestFullscreen" "webkitRequestFullscreen"))]
        (if (fn? (aget target method))
          (-> (js/Promise.resolve (js-invoke target method))
              (.then #(rf/dispatch [:studio/preview-resize]))
              (.catch #(rf/dispatch [:studio/toast (message %) true])))
          (rf/dispatch [:studio/toast "Fullscreen is unavailable in this browser. Expand the preview instead." true]))))))
(rf/reg-fx :studio/nvim-source
  (fn [source]
    (when-let [engine (:neovim @engines)]
      (-> (js-invoke engine "setText" source)
          (.catch #(rf/dispatch [:studio/editor-error (message %)]))))))
(rf/reg-fx :studio/nvim-read
  (fn [{:keys [on-success]}]
    (if-let [engine (:neovim @engines)]
      (-> (js-invoke engine "text")
          (.then #(rf/dispatch (conj on-success %)))
          (.catch #(rf/dispatch [:studio/editor-error (message %)])))
      (rf/dispatch [:studio/editor-error "Neovim is not ready."]))))
(rf/reg-fx :studio/nvim-input
  (fn [keys]
    (when-let [engine (:neovim @engines)]
      (js-invoke engine "send" keys)
      (when-let [input (aget engine "input")] (.focus input)))))

(defn- update-preview! [this engine show]
  ;; Preserve the shared rig across frame updates, including worker transfers.
  (let [rig (:rig show)
        previous-rig (aget this "rig")
        js-rig (if (identical? rig previous-rig) (aget this "jsRig") (clj->js rig))
        frame (clj->js (select-keys show [:runtime-id :show-id :epoch :revision :frame-number :time-ms
                                         :blackout? :master :editor]))
        intents (get-in show [:status :last-intents])]
    (aset this "rig" rig)
    (aset this "jsRig" js-rig)
    (aset frame "rig" js-rig)
    (aset frame "output" #js {:values (clj->js (get-in show [:output :values]))})
    (aset frame "cut?" (boolean (or (:cut? show)
                                    (some #(#{"effect/reloaded" "instance/finished" :effect/reloaded :instance/finished}
                                            (:type %)) intents))))
    (js-invoke engine "update" frame)))

(def preview
  (r/create-class
    {:display-name "room-preview"
     :component-did-mount
     (fn [this]
       (let [root (aget this "root")]
         (aset this "alive" true)
         (let [handler #(rf/dispatch-sync [:rig-editor/interaction (js->clj (.-detail %) :keywordize-keys true)])]
           (aset this "rig-editor-handler" handler)
           (.addEventListener (.querySelector root "canvas") "tolglow-editor" handler))
         (let [handler #(rf/dispatch [:studio/preview-resize])]
           (aset this "fullscreen-handler" handler)
           (.addEventListener js/document "fullscreenchange" handler)
           (.addEventListener js/document "webkitfullscreenchange" handler))
         (-> (js-invoke js/globalThis "tolglowLoadEngine" "preview")
             (.then (fn [module]
                      (when (aget this "alive")
                        (let [engine (js/Reflect.construct (aget module "RoomPreview")
                                                          #js [(.querySelector root "canvas")
                                                               (.querySelector root "#preview-status")])]
                          (aset this "engine" engine)
                          (swap! engines assoc :preview engine)
                          (when (fn? (aget engine "onNavigationChange"))
                            (aset this "navigation-unsubscribe"
                                  (js-invoke engine "onNavigationChange"
                                             #(rf/dispatch [:camera-navigation/observed (js->clj % :keywordize-keys true)]))))
                          (when-let [show (second (r/argv this))] (update-preview! this engine show))
                          (rf/dispatch [:studio/preview-ready (every? #(fn? (aget engine %)) ["setWorkLight" "setQuality" "setCamera"])])))))
             (.catch #(rf/dispatch [:studio/toast (message %) true])))))
     :component-did-update
     (fn [this old-argv]
       (when-let [engine (aget this "engine")]
         (let [show (second (r/argv this))]
           (when (and show (not (identical? show (second old-argv))))
             (update-preview! this engine show)))))
     :component-will-unmount
     (fn [this]
       (aset this "alive" false)
       (when-let [unsubscribe (aget this "navigation-unsubscribe")] (unsubscribe))
       (when-let [handler (aget this "fullscreen-handler")]
         (.removeEventListener js/document "fullscreenchange" handler)
         (.removeEventListener js/document "webkitfullscreenchange" handler))
       (when-let [engine (aget this "engine")]
         (js-invoke engine "dispose")
         (when (identical? engine (:preview @engines)) (swap! engines dissoc :preview)))
       (when-let [handler (aget this "rig-editor-handler")]
         (.removeEventListener (.querySelector (aget this "root") "canvas") "tolglow-editor" handler)))
     :reagent-render
     (fn [show]
       (let [this (r/current-component)]
       [:div.preview-stage {:ref #(aset this "root" %)}
        [:canvas#preview {:tab-index 0 :aria-label "Three dimensional lighting preview. WASD to explore; drag to look in Explore mode. Arrow keys orbit; Home resets."}]
        [:div.preview-overlay
         [:span#preview-status "Preparing preview"]
         [:span#frame-time (str (.toFixed (or (get-in show [:status :frame-ms]) 0) 1) " ms")]]
        [:div.preview-caption "LIGHT, GIVEN SPACE."]]))}))

(def neovim
  (r/create-class
    {:display-name "neovim-wasm"
     :component-did-mount
     (fn [this]
       (let [root (aget this "root") definition (second (r/argv this))]
         (aset this "alive" true)
         (-> (js-invoke js/globalThis "tolglowLoadEngine" "neovim")
             (.then
               (fn [module]
                 (when (aget this "alive")
                   (let [engine (js/Reflect.construct (aget module "NeovimEngine")
                                                     #js [(.querySelector root "#neovim-host")
                                                          (js-obj "onChange" #(rf/dispatch [:studio/definition-source %])
                                                                  "onStatus" #(rf/dispatch [:studio/editor-status %])
                                                                  "onError" #(rf/dispatch [:studio/editor-error %]))])]
                     (aset this "engine" engine)
                     (swap! engines assoc :neovim engine)
                     (-> (js-invoke engine "start" (:source definition))
                         (.catch #(rf/dispatch [:studio/editor-unavailable (message %)])))))))
             (.catch #(rf/dispatch [:studio/editor-unavailable (message %)])))))
     :component-will-unmount
     (fn [this]
       (aset this "alive" false)
       (when-let [engine (aget this "engine")]
         (js-invoke engine "stop")
         (when (identical? engine (:neovim @engines)) (swap! engines dissoc :neovim))))
     :reagent-render
     (fn [definition]
       (let [this (r/current-component)]
       [:div {:ref #(aset this "root" %)}
        [:div#neovim-host]
        [:div.nvim-toolbar
         (for [[caption keys] [["Esc" "<Esc>"] ["Tab" "<Tab>"] ["Undo" "<Esc>u"] ["Insert" "i"]]]
           ^{:key caption} [:button.button {:type "button" :on-click #(rf/dispatch [:studio/nvim-input keys])} caption])]
        [:p#neovim-status.small.muted (:editor-status definition)]]))}))
