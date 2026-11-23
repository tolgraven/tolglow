(ns tolglow.studio.interop
  "Lifecycle-owned graphics/editor engines. Show state stays in re-frame."
  (:require [afterglow.render.photometry :as photometry]
            [tolglow.studio.observations :as observations]
            [reagent.core :as r]
            [re-frame.core :as rf]))

(defonce engines (atom {}))
(defn- message [error] (or (.-message error) (str error)))

(defn preview-engine [registered owner]
  (let [previews (:previews registered)]
    (if owner (get previews owner) (or (:rig previews) (:perform previews)))))

(defn preview-recipients [registered action owner]
  ;; Both views display the same saved light/camera settings. Momentary actions belong to the visible view.
  (if (and (nil? owner) (#{:fog :worklight :bloom :grain :exposure :lighting-mode :quality :camera :reset} action))
    (vals (:previews registered))
    (when-let [engine (preview-engine registered owner)] [engine])))

(rf/reg-fx :studio/preview
  (fn [[action value owner]]
    (doseq [engine (preview-recipients @engines action owner)]
      (case action
        :fog (js-invoke engine "setFog" value)
        :burst (js-invoke engine "burst")
        :reset (js-invoke engine "resetCamera")
        :resize (js-invoke engine "resize")
        :worklight (when (fn? (aget engine "setWorkLight")) (js-invoke engine "setWorkLight" value))
        :lighting-mode (js-invoke engine "setLightingMode" value)
        (:bloom :grain :exposure) (js-invoke engine "setFilm" (clj->js {action value}))
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
  (fn [{:keys [on-success on-failure] :or {on-failure [:studio/editor-error]}}]
    (if-let [engine (:neovim @engines)]
      (-> (js-invoke engine "text")
          (.then #(rf/dispatch (conj on-success %)))
          (.catch #(rf/dispatch (conj on-failure (message %)))))
      (rf/dispatch (conj on-failure "Neovim is not ready.")))))
(rf/reg-fx :studio/nvim-input
  (fn [keys]
    (when-let [engine (:neovim @engines)]
      (js-invoke engine "send" keys)
      (when-let [input (aget engine "input")] (.focus input)))))

(defn preview-data [data]
  ;; Maps of fixture IDs and the IDs in the rig must use the same full name.
  (clj->js data :keyword-fn #(subs (str %) 1)))

(defn preview-output [show]
  (preview-data (select-keys (:output show) [:values :atmosphere])))

(defn preview-rig [rig]
  (preview-data
   (update (dissoc rig :pixel-layouts) :fixtures
           (fn [fixtures]
             (mapv (fn [fixture]
                     (if (or (:max-watts fixture) (:measured-relative-brightness fixture))
                       (assoc fixture :relative-brightness (photometry/relative-brightness fixture))
                       fixture))
                   fixtures)))))

(defn- update-preview! [this engine show]
  ;; Preserve the shared rig across frame updates, including worker transfers.
  (let [rig (:rig show)
        previous-rig (aget this "rig")
        js-rig (if (or (identical? rig previous-rig)
                       (= (dissoc rig :pixel-layouts) (dissoc previous-rig :pixel-layouts)))
                 (aget this "jsRig") (preview-rig rig))
        editor (:editor show)
        js-editor (if (= editor (aget this "editor")) (aget this "jsEditor") (preview-data editor))
        frame (preview-data (select-keys show [:runtime-id :show-id :epoch :revision :frame-number :time-ms
                                         :blackout? :master :receiver-surfaces :spatial-volumes]))
        intents (get-in show [:status :last-intents])]
    (aset this "rig" rig)
    (aset this "jsRig" js-rig)
    (doseq [[k js-key cache] [[:sculpt-guide "sculpt-guide" "sculptGuide"] [:volume-guides "volume-guides" "volumeGuides"]
                              [:glass-meshes "glass-meshes" "glassMeshes"] [:glass-volumes "glass-volumes" "glassVolumes"]]]
      (let [guide (get show k)]
        (when-not (identical? guide (aget this js-key))
          (aset this js-key guide)
          (aset this cache (preview-data guide)))
        (aset frame js-key (aget this cache))))
    (aset this "editor" editor)
    (aset this "jsEditor" js-editor)
    (aset frame "editor" js-editor)
    (aset frame "rig" js-rig)
    (aset frame "output" (preview-output show))
    (aset frame "cut?" (boolean (or (:cut? show)
                                    (some #(#{"effect/reloaded" "instance/finished" :effect/reloaded :instance/finished}
                                            (:type %)) intents))))
    (js-invoke engine "update" frame)))

(def preview
  (r/create-class
    {:display-name "room-preview"
     :component-did-mount
     (fn [this]
       (let [root (aget this "root")
             owner (get (r/argv this) 2 :perform)]
         (aset this "alive" true)
         (when-let [changed (get (r/argv this) 3)]
           (let [handler #(changed (.-detail %))]
             (aset this "activity-handler" handler)
             (.addEventListener (.querySelector root "canvas") "tolglow-preview-activity" handler)))
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
                                                               (.querySelector root ".preview-status")])]
                          (aset this "engine" engine)
                          (swap! engines assoc-in [:previews owner] engine)
                          (when (fn? (aget engine "onNavigationChange"))
                            (aset this "navigation-unsubscribe"
                                  (js-invoke engine "onNavigationChange"
                                             #(when (identical? engine (preview-engine @engines nil))
                                                (rf/dispatch [:camera-navigation/observed (js->clj % :keywordize-keys true)])))))
                          (observations/bind! this engine (nth (r/argv this) 4 nil))
                          (when-let [show (second (r/argv this))] (update-preview! this engine show))
                          (rf/dispatch [:studio/preview-ready (every? #(fn? (aget engine %)) ["setWorkLight" "setQuality" "setCamera"]) owner])))))
             (.catch #(rf/dispatch [:studio/toast (message %) true])))))
     :component-did-update
     (fn [this old-argv]
       (when-let [engine (aget this "engine")]
         (when-not (= (nth (r/argv this) 4 nil) (nth old-argv 4 nil))
           (observations/bind! this engine (nth (r/argv this) 4 nil)))
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
         (observations/bind! this engine nil)
         (js-invoke engine "dispose")
         (let [owner (get (r/argv this) 2 :perform)]
           (when (identical? engine (preview-engine @engines owner)) (swap! engines update :previews dissoc owner))))
       (when-let [handler (aget this "activity-handler")]
         (.removeEventListener (.querySelector (aget this "root") "canvas") "tolglow-preview-activity" handler))
       (when-let [handler (aget this "rig-editor-handler")]
         (.removeEventListener (.querySelector (aget this "root") "canvas") "tolglow-editor" handler)))
     :reagent-render
     (fn [show & [owner _activity-changed]]
       (let [this (r/current-component)]
       [:div.preview-stage {:ref #(aset this "root" %)}
        [:canvas {:id (if (= :rig owner) "rig-preview" "preview") :tab-index 0 :aria-label "Three dimensional lighting preview. WASD to explore; drag to look in Explore mode. Arrow keys orbit; Home resets."}]
        [:div.preview-overlay
         [:span.preview-status {:id (if (= :rig owner) "rig-preview-status" "preview-status")} "Preparing preview"]
         (when-let [limit (:visible-spatial-limit show)] [:span (str "Showing " limit " spatial volumes")])
         [:span {:id (if (= :rig owner) "rig-frame-time" "frame-time")} (str (.toFixed (or (get-in show [:status :frame-ms]) 0) 1) " ms")]]
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
