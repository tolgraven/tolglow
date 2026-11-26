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
(defn- preview-document
  "The document showing the Perform Room, which may be its own window."
  []
  (or (some-> (preview-engine @engines :perform) (aget "canvas") .-ownerDocument) js/document))

(rf/reg-fx :studio/preview-layout
  (fn [expanded?]
    (r/after-render
      (fn []
        (when-let [panel (.getElementById (preview-document) "preview-panel")]
          (when (= expanded? (.contains (.-classList panel) "expanded"))
            (when-let [canvas (.querySelector panel "#preview")]
              (.scrollIntoView canvas #js {:block "nearest" :inline "nearest"}))
            (when-let [engine (preview-engine @engines :perform)]
              (js-invoke engine "resize"))))))))
(rf/reg-fx :studio/preview-fullscreen
  (fn [_]
    (when-let [panel (.getElementById (preview-document) "preview-panel")]
      (let [doc (.-ownerDocument panel)
            active (or (.-fullscreenElement doc) (aget doc "webkitFullscreenElement"))
            target (if (= active panel) doc panel)
            method (if (= active panel)
                     (if (fn? (aget doc "exitFullscreen")) "exitFullscreen" "webkitExitFullscreen")
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
                              [:glass-meshes "glass-meshes" "glassMeshes"] [:glass-volumes "glass-volumes" "glassVolumes"]
                              [:video-surfaces "video-surfaces" "videoSurfaces"]]]
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

(defn bitmap-cadence [now report!]
  (let [sample (volatile! nil)]
    {:clear! #(do (vreset! sample nil) (report! nil))
     :present! (fn [show-id epoch]
                 (let [t (now)
                       previous @sample
                       reset-window! #(vreset! sample {:show-id show-id :epoch epoch :start t :intervals 0})]
                   (if (and previous (= show-id (:show-id previous)) (= epoch (:epoch previous)))
                     (let [elapsed (- t (:start previous))
                           intervals (inc (:intervals previous))]
                       (if (>= elapsed 1000)
                         (do (report! (/ (* 1000 intervals) elapsed)) (reset-window!))
                         (vswap! sample assoc :intervals intervals)))
                     (do (report! nil) (reset-window!)))))}))

(defn- canvas-rate [component]
  (or (aget component "canvasRate") (aset component "canvasRate" (r/atom nil))))

(defn- observe-preview! [component engine request]
  (let [cadence (aget component "bitmapCadence")
        count-frame (fn [show-id epoch]
                      (if (and (aget engine "worker") (not (aget engine "fallback")))
                        ((:present! cadence) show-id epoch)
                        (when (some? @(canvas-rate component)) ((:clear! cadence)))))]
    (observations/bind! component engine request count-frame)))

(defn- local-data
  "Values from another window's realm fail js->clj's plain-object test. Copy
  them into this realm first."
  [this value]
  (if (and (aget this "foreign") (some? value)) (js/structuredClone value) value))

(defn- external-window!
  "A Room in its own window keeps rendering while that window is visible,
  although Studio, not the Room window, has keyboard focus."
  [this canvas]
  (let [doc (.-ownerDocument canvas)
        publish! #(.dispatchEvent canvas (js/CustomEvent. "tolglow-preview-external"
                                                          #js {:detail #js {:owner "room-window" :active (not (.-hidden doc))}}))]
    (publish!)
    (.addEventListener doc "visibilitychange" publish!)
    (aset this "external-release" #(.removeEventListener doc "visibilitychange" publish!))))

(def preview
  (r/create-class
    {:display-name "room-preview"
     :component-did-mount
     (fn [this]
       (let [root (aget this "root")
             owner (get (r/argv this) 2 :perform)
             ;; Load the engine in the window that shows this canvas.
             host (.. root -ownerDocument -defaultView)
             load-engine (or (aget host "tolglowLoadEngine") (aget js/globalThis "tolglowLoadEngine"))]
         (aset this "alive" true)
         (aset this "foreign" (not (identical? host js/window)))
         (let [rate (canvas-rate this)
               cadence (bitmap-cadence #(.now js/performance) #(reset! rate %))
               changed (get (r/argv this) 3)
               handler (fn [event]
                         (when-not (.-detail event) ((:clear! cadence)))
                         (when changed (changed (.-detail event))))]
           (aset this "bitmapCadence" cadence)
           (aset this "activity-handler" handler)
           (.addEventListener (.querySelector root "canvas") "tolglow-preview-activity" handler))
         (let [handler #(rf/dispatch-sync [:rig-editor/interaction (js->clj (local-data this (.-detail %)) :keywordize-keys true)])]
           (aset this "rig-editor-handler" handler)
           (.addEventListener (.querySelector root "canvas") "tolglow-editor" handler))
         (let [handler #(rf/dispatch [:studio/preview-resize])]
           (aset this "fullscreen-handler" handler)
           (.addEventListener js/document "fullscreenchange" handler)
           (.addEventListener js/document "webkitfullscreenchange" handler))
         (-> (load-engine "preview")
             (.then (fn [module]
                      (when (aget this "alive")
                        (let [engine (js/Reflect.construct (aget module "RoomPreview")
                                                          #js [(.querySelector root "canvas")
                                                               (.querySelector root ".preview-status")])]
                          (aset this "engine" engine)
                          (when (aget this "foreign") (external-window! this (.querySelector root "canvas")))
                          (swap! engines assoc-in [:previews owner] engine)
                          (when (fn? (aget engine "onNavigationChange"))
                            (aset this "navigation-unsubscribe"
                                  (js-invoke engine "onNavigationChange"
                                             #(when (identical? engine (preview-engine @engines nil))
                                                (rf/dispatch [:camera-navigation/observed (js->clj (local-data this %) :keywordize-keys true)])))))
                          (observe-preview! this engine (nth (r/argv this) 4 nil))
                          (when-let [show (second (r/argv this))] (update-preview! this engine show))
                          (rf/dispatch [:studio/preview-ready (every? #(fn? (aget engine %)) ["setWorkLight" "setQuality" "setCamera"]) owner])))))
             (.catch #(rf/dispatch [:studio/toast (message %) true])))))
     :component-did-update
     (fn [this old-argv]
       (when-let [engine (aget this "engine")]
         (when-not (= (nth (r/argv this) 4 nil) (nth old-argv 4 nil))
           (observe-preview! this engine (nth (r/argv this) 4 nil)))
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
       (when-let [release (aget this "external-release")] (release))
       (when-let [engine (aget this "engine")]
         (observations/bind! this engine nil)
         ;; A closed Room window may already have torn down its realm.
         (try (js-invoke engine "dispose") (catch :default _ nil))
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
        [:canvas {:id (if (= :rig owner) "rig-preview" "preview") :tab-index 0 :aria-label "Three dimensional lighting preview. Move with WASD, HJKL or arrow keys. Drag to orbit or look. Home resets the view."}]
        [:div.preview-overlay
         [:span.preview-status {:id (if (= :rig owner) "rig-preview-status" "preview-status")} "Preparing preview"]
         (when-let [limit (:visible-spatial-limit show)] [:span (str "Showing " limit " spatial volumes")])
         (when-some [ms (get-in show [:status :frame-ms])]
           [:span {:id (if (= :rig owner) "rig-frame-time" "frame-time")
                   :title "Time the show host spent producing its latest lighting frame"}
            (str "Show compute · " (.toFixed ms 1) " ms")])
         (when-let [hz @(canvas-rate this)]
           [:span {:title "Accepted worker images transferred to this canvas per second"}
            (str "Canvas · " (.toFixed hz 1) " fps")])]
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
