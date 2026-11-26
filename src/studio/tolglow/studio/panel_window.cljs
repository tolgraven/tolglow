(ns tolglow.studio.panel-window
  "Detach any Perform panel into its own window.

  This generalises the Room's pop-out (`tolglow.studio.room-window`, #665) to
  every panel and reuses its window state machine. Studio stays the only owner
  of the state socket, the show projection and every command: a detached panel
  is the same Reagent content portalled into one same-origin `window.open`
  window, so it reads the same app-db and sends the same events. While a panel
  is detached its layout tile holds a placeholder, and the content exists in
  exactly one document. The Room keeps its own owner because its preview
  engine must be loaded in the window that shows it."
  (:require ["react-dom" :as react-dom]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.room-window :as room-window]))

;; ---------------------------------------------------------------------------
;; State (pure). Lives in app-db at [:ui :panel-windows <panel id>] and steps
;; through room-window/step, so late events from a closed window are ignored.

(defn state [db id] (merge room-window/docked (get-in db [:ui :panel-windows id])))

(defn detached? [db id] (not= :docked (:phase (state db id))))

(defn detached-ids
  "Panels whose window is open or opening."
  [db]
  (vec (keep (fn [[id s]] (when (not= :docked (:phase s)) id)) (get-in db [:ui :panel-windows]))))

(def failure-notices
  {:blocked "The browser blocked the panel window. Allow pop-ups for Studio, then press Detach again."
   :unsupported "This browser cannot open a panel in its own window."})

;; ---------------------------------------------------------------------------
;; Window owners (effects). Window handles never enter app-db.

(defonce ^:private owners (atom {}))

(defonce ^:private token (str (random-uuid)))

(defn- current? [id generation] (= generation (get-in @owners [id :generation])))

(defn- release! [id]
  (when-let [{:keys [^js abort timer frame ^js window]} (get @owners id)]
    (swap! owners dissoc id)
    (.abort abort)
    (js/clearInterval timer)
    (when frame (try (.cancelAnimationFrame window frame) (catch :default _ nil)))))

(defn- prepare-document!
  "Give the empty same-origin window Studio's stylesheets and a root for the
  portalled panel. Perform styles are scoped to #view-perform."
  [^js popup title]
  (let [doc (.-document popup)
        head (.-head doc)
        root (.createElement doc "main")]
    (set! (.-title doc) (str "Tolglow · " title))
    (set! (.. doc -documentElement -lang) (.. js/document -documentElement -lang))
    (set! (.. doc -documentElement -className) (.. js/document -documentElement -className))
    (.replaceChildren head)
    (let [viewport (.createElement doc "meta")]
      (set! (.-name viewport) "viewport")
      (set! (.-content viewport) "width=device-width, initial-scale=1")
      (.append head viewport))
    (doseq [sheet (array-seq (.querySelectorAll js/document "link[rel=stylesheet]"))]
      (let [link (.createElement doc "link")]
        (set! (.-rel link) "stylesheet")
        (set! (.-href link) (.-href sheet))
        (.append head link)))
    (set! (.-id root) "view-perform")
    (set! (.-className root) "panel-window-root")
    (set! (.. doc -body -className) "panel-window")
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

(defn- open! [{:keys [id generation title width height]}]
  (let [fail! #(rf/dispatch [:panel-window/failed id generation %])
        ^js popup (when (fn? (aget js/window "open"))
                    (try (.open js/window "" (str "tolglow-panel-" (name id) "-" token "-" generation)
                                (str "popup,width=" width ",height=" height))
                         (catch :default _ nil)))]
    (if-not popup
      (fail! (if (or (not (fn? (aget js/window "open"))) (aget js/globalThis "webkit")) :unsupported :blocked))
      (try
        (let [root (prepare-document! popup title)
              abort (js/AbortController.)
              options #js {:signal (.-signal abort)}
              closed! #(when (current? id generation)
                         (release! id)
                         (js/setTimeout (fn [] (try (.close popup) (catch :default _ nil))))
                         (rf/dispatch [:panel-window/closed id generation]))]
          (swap! owners assoc id {:generation generation :window popup :root root :abort abort
                                  :timer (js/setInterval #(when (.-closed popup) (closed!)) 500)})
          (.addEventListener popup "pagehide" closed! options)
          ;; A panel window never outlives the Studio page that owns its state.
          (.addEventListener js/window "pagehide" #(.close popup) options)
          (keep-rendering! id generation popup)
          (rf/dispatch [:panel-window/opened id generation]))
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
  (fn [{:keys [db]} [_ id title [width height]]]
    (let [before (state db id) after (room-window/step before [:pop-out false])]
      (when-not (= before after)
        {:db (assoc-in db [:ui :panel-windows id] after)
         :panel-window/open {:id id :generation (:generation after) :title title
                             :width (or width 720) :height (or height 640)}}))))

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

(defn- portal [db id content]
  (when-let [root (get-in @owners [id :root])]
    (when (#{:popping :popped} (:phase (state db id)))
      (react-dom/createPortal (r/as-element content) root))))

(defn region
  "A panel's content, docked in its tile or portalled into its own window."
  [db id title content]
  (if (detached? db id)
    [:<> [placeholder db id title] [portal db id content]]
    content))
