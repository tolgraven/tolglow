(ns tolglow.studio.interop
  "Lifecycle-owned graphics/editor engines. Show state stays in re-frame."
  (:require [afterglow.render.photometry :as photometry]
            [tolglow.studio.frame-stream :as frame-stream]
            [tolglow.studio.observations :as observations]
            [tolglow.studio.projection :as projection]
            [tolglow.transit :as transit]
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

(def ^:private room-keys
  [:runtime-id :show-id :epoch :revision :receiver-surfaces :editor :rig :instances])

(defn room-input
  "The structural part of a show that a Room renders. Frame clocks, fixture
  and audition values, master, blackout, cut intents and atmosphere arrive on
  the :room frame lane at their exact token instead, so a show frame does not
  change this value or render the Room component."
  [show]
  (when show (select-keys show room-keys)))

(defn consistent?
  "Whether a lane frame belongs to the same server show and revision as the
  Room input. A frame that arrives ahead of its structural change waits for
  the input; a frame with no lane yet (nil) never blocks."
  [input frame]
  (or (nil? frame)
      (and (= (projection/presentation (:identity frame)) (select-keys input [:runtime-id :show-id :epoch]))
           (= (:revision frame) (:revision input)))))

(defn room-values
  "The typed fixture values owner's Room shows from a lane frame. A staged
  draft's audition replaces the live values in the Perform Room only."
  [owner lane-frame]
  (let [{:keys [values audition]} (:value lane-frame)]
    (if (and audition (= :perform owner)) audition values)))

(defn room-feed
  "Own one Room's consumer of the :room frame lane. (deliver! input lane-frame)
  receives each consistent pair: at most one lane frame per (schedule! f)
  callback, plus one delivery when the structural input changes. The feed
  starts paused; start! delivers the installed frame once."
  [owner deliver! schedule!]
  {:owner owner :deliver! deliver! :schedule! schedule!
   :state (volatile! {:input nil :frame nil :active? false :release nil :alive? true})})

(defn- feed-deliver! [{:keys [deliver! state]}]
  (let [{:keys [input frame alive?]} @state]
    (when (and alive? input (consistent? input frame))
      (deliver! input frame))))

(defn feed-start!
  "Attach to the lane with input, and deliver the installed frame once."
  [{:keys [owner schedule! state] :as feed} input]
  (vswap! state assoc :input input :frame (frame-stream/baseline :room))
  (vswap! state assoc :release
          (frame-stream/attach! :room owner
                                (fn [lane-frame]
                                  (when (:alive? @state)
                                    (vswap! state assoc :frame lane-frame)
                                    (feed-deliver! feed)))
                                :active? (:active? @state) :schedule schedule!))
  (feed-deliver! feed))

(defn feed-input!
  "Install a changed structural input and deliver it with the latest frame."
  [{:keys [state] :as feed} input]
  (when-not (= input (:input @state))
    (vswap! state assoc :input input)
    (feed-deliver! feed)))

(defn feed-activity!
  "Pause or resume lane delivery with the engine's activity gate."
  [{:keys [owner state]} active?]
  (vswap! state assoc :active? (boolean active?))
  (when (:release @state) (frame-stream/set-active! :room owner active?)))

(defn feed-stop! [{:keys [state]}]
  (when-let [release (:release @state)] (release))
  (vswap! state assoc :alive? false :release nil))

(defn preview-rig [rig]
  (preview-data
   (update (dissoc rig :pixel-layouts) :fixtures
           (fn [fixtures]
             (mapv (fn [fixture]
                     (if (or (:max-watts fixture) (:measured-relative-brightness fixture))
                       (assoc fixture :relative-brightness (photometry/relative-brightness fixture))
                       fixture))
                   fixtures)))))

(defn- js-key [k]
  (cond (keyword? k) (subs (str k) 1)
        (string? k) k
        (or (number? k) (symbol? k)) (str k)
        :else (pr-str k)))

(defn- typed->js
  "The JavaScript value preview-data gives the presented form of a typed
  value, in one pass: fractions become numbers and enums become text."
  [value]
  (cond
    (transit/fraction? value) (transit/fraction-value value)
    (keyword? value) (subs (str value) 1)
    (symbol? value) (str value)
    (map? value) (let [out #js {}]
                   (reduce-kv (fn [_ k v] (unchecked-set out (js-key k) (typed->js v))) nil value)
                   out)
    (coll? value) (let [out #js []]
                    (doseq [v value] (.push out (typed->js v)))
                    out)
    :else value))

(defn values->js
  "Convert typed lane values to the engine's fixture value object. A fixture
  whose typed value is identical to the previous frame's reuses that frame's
  converted object, which is never mutated after creation."
  [values previous previous-js]
  (when values
    (let [out #js {}]
      (reduce-kv (fn [_ id fixture]
                   (let [k (js-key id)]
                     (unchecked-set out k (if (and previous-js (identical? fixture (get previous id)))
                                            (unchecked-get previous-js k)
                                            (typed->js fixture)))))
                 nil values)
      out)))

(defn- lane-values
  "Convert typed lane values once per distinct value."
  [this value]
  (if (identical? value (aget this "laneValue"))
    (aget this "laneValues")
    (let [converted (values->js value (aget this "laneValue") (aget this "laneValues"))]
      (aset this "laneValue" value)
      (aset this "laneValues" converted)
      converted)))

(defn- update-preview! [this engine show lane-frame owner]
  ;; Preserve the shared rig across frame updates, including worker transfers.
  (let [rig (:rig show)
        previous-rig (aget this "rig")
        js-rig (if (or (identical? rig previous-rig)
                       (= (dissoc rig :pixel-layouts) (dissoc previous-rig :pixel-layouts)))
                 (aget this "jsRig") (preview-rig rig))
        editor (:editor show)
        js-editor (if (= editor (aget this "editor")) (aget this "jsEditor") (preview-data editor))
        frame (preview-data (merge (select-keys show [:runtime-id :show-id :epoch :revision
                                                      :receiver-surfaces :spatial-volumes])
                                   (cond-> (select-keys lane-frame [:frame-number :time-ms :blackout? :master])
                                     (:master lane-frame) (update :master transit/fraction-value))))
        values (lane-values this (room-values owner lane-frame))
        intents (:last-intents lane-frame)]
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
    (let [output (preview-output {:output (cond-> {} (some? (:atmosphere lane-frame)) (assoc :atmosphere (:atmosphere lane-frame)))})]
      (when values (aset output "values" values))
      (aset frame "output" output))
    (aset frame "cut?" (boolean (or (:cut? lane-frame)
                                    (some #(#{"effect/reloaded" "instance/finished" :effect/reloaded :instance/finished}
                                            (:type %)) intents))))
    (js-invoke engine "update" frame)))

(rf/reg-sub ::frame-ms (fn [db _] (get-in db [:show :status :frame-ms])))

(defn- frame-time
  "The show host's frame compute time. Only this readout follows each frame."
  [owner]
  (when-some [ms @(rf/subscribe [::frame-ms])]
    [:span {:id (if (= :rig owner) "rig-frame-time" "frame-time")
            :title "Time the show host spent producing its latest lighting frame"}
     (str "Show compute · " (.toFixed ms 1) " ms")]))

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
         ;; Lane frames reach the engine on this window's animation frames,
         ;; after the ACK, newest frame first; the feed starts with the engine.
         (aset this "feed" (room-feed owner
                                      (fn [input lane-frame]
                                        (when-let [engine (aget this "engine")]
                                          (update-preview! this engine input lane-frame owner)))
                                      #(.requestAnimationFrame host %)))
         (let [rate (canvas-rate this)
               cadence (bitmap-cadence #(.now js/performance) #(reset! rate %))
               changed (get (r/argv this) 3)
               handler (fn [event]
                         (let [active? (boolean (.-detail event))]
                           (when-not active? ((:clear! cadence)))
                           ;; Frames pause on the same gates as the GPU.
                           (feed-activity! (aget this "feed") active?)
                           (when changed (changed active?))))]
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
                          (feed-start! (aget this "feed") (second (r/argv this)))
                          (rf/dispatch [:studio/preview-ready (every? #(fn? (aget engine %)) ["setWorkLight" "setQuality" "setCamera"]) owner])))))
             (.catch #(rf/dispatch [:studio/toast (message %) true])))))
     :component-did-update
     (fn [this old-argv]
       (when-let [engine (aget this "engine")]
         (when-not (= (nth (r/argv this) 4 nil) (nth old-argv 4 nil))
           (observe-preview! this engine (nth (r/argv this) 4 nil)))
         (let [show (second (r/argv this))]
           (when (and show (not (identical? show (second old-argv))))
             (feed-input! (aget this "feed") show)))))
     :component-will-unmount
     (fn [this]
       (aset this "alive" false)
       (feed-stop! (aget this "feed"))
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
         [frame-time owner]
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
