(ns tolglow.studio.panel-window
  "Detach any Perform panel into its own window.

  This generalises the Room's pop-out (`tolglow.studio.room-window`, #665) to
  every panel and reuses its window state machine. Studio stays the only owner
  of the state socket, the show projection and every command: a detached panel
  is the same Reagent content portalled into one same-origin `window.open`
  window, so it reads the same app-db and sends the same events. While a panel
  is detached its layout tile holds a placeholder, and the content exists in
  exactly one document. The Room keeps its own owner because its preview
  engine must be loaded in the window that shows it.

  A whole Studio view detaches the same way (`tolglow.studio.view-tabs`,
  #669), under a `:view/<name>` ID. Its window loads the engine loader first,
  so a Room or Rig preview inside the view binds its engine to that window, as
  the Room's own window does."
  (:require ["react-dom" :as react-dom]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.room-window :as room-window]
            [tolglow.studio.state-socket :as socket]))

;; ---------------------------------------------------------------------------
;; State (pure). Lives in app-db at [:ui :panel-windows <panel id>] and steps
;; through room-window/step, so late events from a closed window are ignored.

(defn state [db id] (merge room-window/docked (get-in db [:ui :panel-windows id])))

(defn detached? [db id] (not= :docked (:phase (state db id))))

(defn view-id? [id] (= "view" (namespace id)))

(defn detached-ids
  "Perform panels whose window is open or opening. Detached views
  (`:view/<name>`) are not panels of a layout, so a layout never redocks them."
  [db]
  (vec (keep (fn [[id s]] (when (and (not= :docked (:phase s)) (not (view-id? id))) id)) (get-in db [:ui :panel-windows]))))

(defn window-key
  "The text naming panel or view `id` to the window system and the Mac app."
  [id]
  (subs (str id) 1))

(def failure-notices
  {:blocked "The browser blocked the panel window. Allow pop-ups for Studio, then press Detach again."
   :unsupported "This browser cannot open a panel in its own window."})

;; ---------------------------------------------------------------------------
;; Window owners (effects). Window handles never enter app-db.

(defonce ^:private owners (atom {}))

(defonce ^:private token (str (random-uuid)))

(defn- current? [id generation] (= generation (get-in @owners [id :generation])))

(defn- release! [id]
  (when-let [{:keys [^js abort timer frame ^js window engine?]} (get @owners id)]
    (swap! owners dissoc id)
    (.abort abort)
    (js/clearInterval timer)
    (when frame (try (.cancelAnimationFrame window frame) (catch :default _ nil)))
    (when engine? (socket/present-preview! (keyword "panel-window" (window-key id)) false))))

(defn open-ids
  "IDs whose window this page owns now: a test reads it to prove teardown."
  []
  (vec (keys @owners)))

(defn- prepare-document!
  "Give the empty same-origin window Studio's stylesheets and a root for the
  portalled panel. Perform styles are scoped to #view-perform; a whole view
  brings its own section, so its root is a plain #main."
  [^js popup title {:keys [root-id body-class] :or {root-id "view-perform" body-class "panel-window"}}]
  (let [doc (.-document popup)
        head (.-head doc)
        root (.createElement doc "main")]
    (set! (.. doc -documentElement -lang) (.. js/document -documentElement -lang))
    (set! (.. doc -documentElement -className) (.. js/document -documentElement -className))
    (.replaceChildren head)
    ;; After clearing the head, which holds the <title> that setting one creates.
    (set! (.-title doc) (str "Tolglow · " title))
    (let [viewport (.createElement doc "meta")]
      (set! (.-name viewport) "viewport")
      (set! (.-content viewport) "width=device-width, initial-scale=1")
      (.append head viewport))
    (doseq [sheet (array-seq (.querySelectorAll js/document "link[rel=stylesheet]"))]
      (let [link (.createElement doc "link")]
        (set! (.-rel link) "stylesheet")
        (set! (.-href link) (.-href sheet))
        (.append head link)))
    (set! (.-id root) root-id)
    (set! (.-className root) "panel-window-root")
    (set! (.. doc -body -className) body-class)
    (.replaceChildren (.-body doc) root)
    root))

(defn- keep-rendering!
  "Reagent renders on the Studio window's animation frames, which stop while
  Studio is hidden. Flush from the panel window's own frames meanwhile."
  [id generation ^js popup]
  (letfn [(tick [_]
            (when (current? id generation)
              (when (.-hidden js/document) (r/flush))
              (swap! owners assoc-in [id :frame] (.requestAnimationFrame popup tick))))]
    (swap! owners assoc-in [id :frame] (.requestAnimationFrame popup tick))))

(defn report-size!
  "Tell the layout how large panel `id`'s window is, so its content follows the
  window (:panel-layout/window-size)."
  [id ^js popup]
  (rf/dispatch [:panel-layout/window-size id (.-innerWidth popup) (.-innerHeight popup)]))

(defn- open! [{:keys [id generation title width height document engine?]}]
  (let [fail! #(rf/dispatch [:panel-window/failed id generation %])
        ^js popup (when (fn? (aget js/window "open"))
                    (try (.open js/window "" (str "tolglow-panel-" (window-key id) "-" token "-" generation)
                                (str "popup,width=" width ",height=" height))
                         (catch :default _ nil)))]
    (if-not popup
      (fail! (if (or (not (fn? (aget js/window "open"))) (aget js/globalThis "webkit")) :unsupported :blocked))
      (try
        (let [root (prepare-document! popup title document)
              abort (js/AbortController.)
              options #js {:signal (.-signal abort)}
              closed! #(when (current? id generation)
                         (release! id)
                         (js/setTimeout (fn [] (try (.close popup) (catch :default _ nil))))
                         (rf/dispatch [:panel-window/closed id generation]))]
          (swap! owners assoc id {:generation generation :window popup :root root :abort abort :engine? engine?
                                  :timer (js/setInterval #(when (.-closed popup) (closed!)) 500)})
          (.addEventListener popup "pagehide" closed! options)
          ;; The packaged Mac app hosts this window natively and reports its closing.
          (room-window/identify-window! popup "panel" (window-key id))
          (.addEventListener js/window "tolglow-room-window"
                             (fn [^js event]
                               (let [^js detail (.-detail event)]
                                 (when (and detail (= "closed" (.-status detail)) (= "panel" (.-role detail))
                                            (= (window-key id) (.-id detail)))
                                   (closed!))))
                             options)
          (report-size! id popup)
          (.addEventListener popup "resize" #(report-size! id popup) options)
          ;; A panel window never outlives the Studio page that owns its state.
          (.addEventListener js/window "pagehide" #(.close popup) options)
          (if-not engine?
            (do (keep-rendering! id generation popup)
                (rf/dispatch [:panel-window/opened id generation]))
            ;; The window's own module realm, as the Room window has: a preview
            ;; in this view loads its engine there (interop/preview), and the
            ;; socket keeps the scene query while only this window is visible.
            (let [loader (.createElement (.-document popup) "script")]
              (socket/present-preview! (keyword "panel-window" (window-key id)) true)
              (set! (.-onload loader)
                    #(when (current? id generation)
                       (keep-rendering! id generation popup)
                       (rf/dispatch [:panel-window/opened id generation])))
              (set! (.-onerror loader)
                    #(when (current? id generation)
                       (release! id) (.close popup) (fail! :unsupported)))
              (set! (.-src loader) "/studio/engine-loader.js")
              (.append (.. popup -document -head) loader))))
        (catch :default _
          (release! id)
          (try (.close popup) (catch :default _ nil))
          (fail! :unsupported))))))

(defn- close! [[id generation]]
  (when-let [{:keys [^js window]} (when (current? id generation) (get @owners id))]
    (release! id)
    ;; Unmount the portalled content while its document still exists.
    (r/flush)
    (try (.close window) (catch :default _ nil)))
  (rf/dispatch [:panel-window/closed id generation]))

(defn show-window! [id]
  (when-let [^js w (get-in @owners [id :window])] (.focus w)))

(rf/reg-fx :panel-window/open open!)

(rf/reg-fx :panel-window/close close!)

;; ---------------------------------------------------------------------------
;; Events

(defn- advance [db id event] (assoc-in db [:ui :panel-windows id] (room-window/step (state db id) event)))

;; Dispatched synchronously from the click, so the window opens inside the
;; user's gesture and is not taken for an unsolicited pop-up.
(rf/reg-event-fx :panel-window/pop-out
  (fn [{:keys [db]} [_ id title [width height] options]]
    (let [before (state db id) after (room-window/step before [:pop-out false])]
      (when-not (= before after)
        {:db (assoc-in db [:ui :panel-windows id] after)
         :panel-window/open (merge {:id id :generation (:generation after) :title title
                                    :width (or width 720) :height (or height 640)}
                                   (select-keys options [:document :engine?]))}))))

(rf/reg-event-db :panel-window/opened
  (fn [db [_ id generation]] (advance db id [:opened generation])))

(rf/reg-event-fx :panel-window/failed
  (fn [{:keys [db]} [_ id generation kind]]
    {:db (advance db id [:failed generation kind])
     :dispatch [:studio/toast (failure-notices kind) true]}))

(rf/reg-event-fx :panel-window/redock
  (fn [{:keys [db]} [_ id]]
    (let [before (state db id) after (room-window/step before [:redock])]
      (when-not (= before after)
        {:db (assoc-in db [:ui :panel-windows id] after) :panel-window/close [id (:generation after)]}))))

;; The docked state keeps its generation, so a late event from this window
;; cannot touch the next one.
(rf/reg-event-db :panel-window/closed
  (fn [db [_ id generation]] (advance db id [:closed generation])))

;; ---------------------------------------------------------------------------
;; Views

(defn- placeholder [db id title]
  (let [{:keys [phase]} (state db id)]
    [:section.panel.panel-window-placeholder {:aria-label (str title " is in its own window") :data-detached (name id)}
     [:div.panel-top
      [:span.section-label (str title " · own window")]
      [:div.preview-size-controls
       [:button.text-button {:type "button" :on-click #(show-window! id)} "Show window"]
       (if (= :popped phase)
         [:button.text-button {:type "button" :on-click #(rf/dispatch [:panel-window/redock id])} "Redock"]
         [:span.small.muted {:role "status"} (if (= :redocking phase) "Redocking…" "Opening…")])]]
     [:p.small.muted.panel-window-note
      (str "Changes here and in the " title " window are the same show. Close that window or press Redock to bring it back.")]]))

(defn portal
  "`content` portalled into `id`'s window while it is popping or popped."
  [db id content tier]
  (when-let [{:keys [root engine?]} (get @owners id)]
    ;; A window that loads the engine shows content once its loader ran, so a
    ;; preview inside binds to that window's realm, as the Room's does.
    (when ((if engine? #{:popped} #{:popping :popped}) (:phase (state db id)))
      ;; data-panel and data-tier select the same tier rules as a layout tile.
      (react-dom/createPortal
       (r/as-element [:div.panel-window-content {:data-panel (name id) :data-tier (name (or tier :full))} content])
       root))))

(defn region
  "A panel's content, docked in its tile or portalled into its own window, where
  it shows at `tier`, the window's own."
  [db id title content & [tier]]
  (if (detached? db id)
    [:<> [placeholder db id title] [portal db id content tier]]
    content))
