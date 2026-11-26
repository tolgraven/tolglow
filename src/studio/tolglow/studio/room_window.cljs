(ns tolglow.studio.room-window
  "Pop the live Room into its own window and send that window to another display.

  Studio stays the only show and preview owner. The Room panel is portalled
  from the main Reagent tree into one same-origin window opened with
  `window.open`. That window loads its own copy of the preview module, so the
  single preview engine, its worker, observers and display clock bind to the
  window that shows them. A docked and a popped-out Room never coexist: the
  docked engine is disposed in the React commit that mounts the popped-out one,
  and the reverse on redock. Each document creates its own canvas; a canvas is
  never moved between documents, which keeps pop-out valid once the worker
  takes control of the visible canvas."
  (:require ["react-dom" :as react-dom]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.state-socket :as socket]))

;; ---------------------------------------------------------------------------
;; State machine (pure). Lives in app-db at [:ui :room-window].

(def docked {:phase :docked :generation 0})

(defn state [db] (merge docked (get-in db [:ui :room-window])))

(defn popped? [db] (= :popped (:phase (state db))))

(defn step
  "Advance the window state. Window events carry the generation of the window
  they belong to, so a late event from a closed window changes nothing."
  [{:keys [phase generation] :as current} [event arg detail]]
  (let [live? (and (not= :docked phase) (= arg generation))]
    (case event
      :pop-out (if (= :docked phase)
                 {:phase :popping :generation (inc generation) :place? (boolean arg)}
                 current)
      :opened (if (and live? (= :popping phase)) (assoc current :phase :popped) current)
      :failed (if (and live? (= :popping phase))
                {:phase :docked :generation generation :failure detail}
                current)
      :redock (if (#{:popping :popped} phase) (assoc current :phase :redocking) current)
      :closed (if live? {:phase :docked :generation generation} current)
      :placing (if live? (assoc current :place? false :display {:status :asking}) current)
      :placed (if live? (assoc current :display {:status :placed :label detail}) current)
      :place-failed (if live? (assoc current :place? false :display {:status detail}) current)
      :screens (if live?
                 (let [status (get-in current [:display :status])]
                   (cond
                     (and (< detail 2) (= :placed status)) (assoc current :display {:status :lost})
                     (and (>= detail 2) (#{:lost :single} status)) (assoc current :display {:status :available})
                     (and (< detail 2) (= :available status)) (assoc current :display {:status :single})
                     :else current))
                 current)
      current)))

(def failure-notices
  {:blocked "The browser blocked the Room window. Allow pop-ups for Studio, then press Pop out again."
   :unsupported "This browser cannot open the Room in its own window. Use Fullscreen or Picture in Picture instead."})

(def display-notices
  {:asking "Asking for access to your displays…"
   :denied "Display access was denied. Drag the Room window to the other display, then press Fullscreen there. Allow window management for Studio in the site settings to move it in one step."
   :single "Only one display is connected. Connect another display, or keep the Room window beside Studio."
   :manual "This browser cannot place windows on another display. Drag the Room window there, then press Fullscreen."
   :lost "The display showing the Room was disconnected. Reconnect it, then press Move to display."
   :available "Another display is connected. Press Move to display to send the Room there."})

(defn notice [{:keys [failure display]}]
  (or (failure-notices failure)
      (let [{:keys [status label]} display]
        (if (= :placed status)
          (str "The Room is on " label ". Press Fullscreen there to fill it.")
          (display-notices status)))))

(defn screen-at
  "Index of the screen rectangle containing point x, y, if any."
  [screens x y]
  (first (keep-indexed (fn [i {:keys [left top width height]}]
                         (when (and (<= left x) (< x (+ left width)) (<= top y) (< y (+ top height))) i))
                       screens)))

(defn target-screen
  "Index of the display to send the Room to. Never Studio's own display; when
  the Room already sits on a free display, the next free one, so repeated
  presses cycle through three or more displays."
  [screens studio room]
  (let [free (vec (remove #(= studio %) (range (count screens))))]
    (when (seq free)
      (if-let [at (first (keep-indexed #(when (= room %2) %1) free))]
        (nth free (mod (inc at) (count free)))
        (first free)))))

;; ---------------------------------------------------------------------------
;; Window owner (effects). The JS window handle never enters app-db.

(defonce ^:private owner (atom nil))

(defonce ^:private token (str (random-uuid)))

(defn- current? [generation] (= generation (:generation @owner)))

(defn- native-host []
  ;; The packaged Mac app hosts popped-out windows natively (native/macOS).
  (some-> js/globalThis (aget "webkit") (aget "messageHandlers") (aget "tolglowRoomWindow")))

(defn- release! []
  (when-let [{:keys [abort timer frame ^js window]} @owner]
    (reset! owner nil)
    (.abort abort)
    (js/clearInterval timer)
    (when frame (try (.cancelAnimationFrame window frame) (catch :default _ nil)))
    (socket/present-preview! :room-window false)))

(defn- prepare-document!
  "Give the empty same-origin window Studio's stylesheets and a root for the
  portalled Room panel."
  [^js popup]
  (let [doc (.-document popup)
        head (.-head doc)
        html (.-documentElement doc)
        main-html (.-documentElement js/document)
        root (.createElement doc "main")]
    (set! (.-title doc) "Tolglow · Room")
    (set! (.-lang html) (.-lang main-html))
    (set! (.-className html) (.-className main-html))
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
    ;; Studio's Room rules are scoped to the Perform view.
    (set! (.-id root) "view-perform")
    (set! (.. doc -body -className) "room-window")
    (.replaceChildren (.-body doc) root)
    root))

(defn- keep-rendering!
  "Reagent schedules renders on the Studio window's animation frames, which
  stop while that window is hidden. While the Room window is visible, flush
  pending renders from its own frames so the Room keeps following the show."
  [generation ^js popup]
  (letfn [(tick [_]
            (when (current? generation)
              (when (.-hidden js/document) (r/flush))
              (swap! owner assoc :frame (.requestAnimationFrame popup tick))))]
    (swap! owner assoc :frame (.requestAnimationFrame popup tick))))

(defn- native-reply! [generation ^js event]
  ;; The packaged Mac app answers placement requests with a window event.
  (let [^js detail (.-detail event)
        status (some-> detail .-status keyword)]
    (when (current? generation)
      (case status
        :placed (rf/dispatch [:room-window/placed generation (.-label detail)])
        (:single :manual) (rf/dispatch [:room-window/place-failed generation status])
        :screens (rf/dispatch [:room-window/screens generation (.-count detail)])
        :closed (do (release!) (rf/dispatch [:room-window/closed generation]))
        nil))))

(defn- open! [generation]
  (let [fail! #(rf/dispatch [:room-window/failed generation %])
        ^js popup (when (fn? (aget js/window "open"))
                (try (.open js/window "" (str "tolglow-room-" token "-" generation) "popup,width=960,height=640")
                     (catch :default _ nil)))]
    (if-not popup
      (fail! (if (or (not (fn? (aget js/window "open"))) (aget js/globalThis "webkit")) :unsupported :blocked))
      (try
        (let [root (prepare-document! popup)
              abort (js/AbortController.)
              options #js {:signal (.-signal abort)}
              closed! #(when (current? generation)
                         (release!)
                         ;; A reload or navigation of the Room window ends it
                         ;; too: close what would otherwise stay an empty page.
                         (js/setTimeout (fn [] (try (.close popup) (catch :default _ nil))))
                         (rf/dispatch [:room-window/closed generation]))
              loader (.createElement (.-document popup) "script")]
          (reset! owner {:generation generation :window popup :root root :abort abort
                         :timer (js/setInterval #(when (.-closed popup) (closed!)) 500)})
          (socket/present-preview! :room-window true)
          (.addEventListener popup "pagehide" closed! options)
          ;; The Room window never outlives the Studio page that owns its engine.
          (.addEventListener js/window "pagehide" #(.close popup) options)
          (.addEventListener js/window "tolglow-room-window" (partial native-reply! generation) options)
          (when-let [^js screen (.-screen js/window)]
            (when (fn? (aget screen "addEventListener"))
              (.addEventListener screen "change"
                                 #(rf/dispatch [:room-window/screens generation (if (.-isExtended screen) 2 1)])
                                 options)))
          ;; The window's own module realm: its engine binds to its globals.
          (set! (.-onload loader)
                #(when (current? generation)
                   (keep-rendering! generation popup)
                   (rf/dispatch [:room-window/opened generation])))
          (set! (.-onerror loader)
                #(when (current? generation)
                   (release!) (.close popup) (fail! :unsupported)))
          (set! (.-src loader) "/studio/engine-loader.js")
          (.append (.. popup -document -head) loader))
        (catch :default _
          (release!)
          (try (.close popup) (catch :default _ nil))
          (fail! :unsupported))))))

(defn- close! [generation]
  (when-let [{:keys [^js window]} (when (current? generation) @owner)]
    (release!)
    ;; Unmount the portalled engine while its document still exists.
    (r/flush)
    (try (.close window) (catch :default _ nil)))
  (rf/dispatch [:room-window/closed generation]))

(defn- screen-rects [^js details]
  (vec (map-indexed (fn [i ^js screen]
                      {:left (.-availLeft screen) :top (.-availTop screen)
                       :width (.-availWidth screen) :height (.-availHeight screen)
                       :label (or (not-empty (.-label screen)) (str "display " (inc i)))})
                    (array-seq (.-screens details)))))

(defn- window-centre [^js w]
  [(+ (.-screenX w) (/ (.-outerWidth w) 2)) (+ (.-screenY w) (/ (.-outerHeight w) 2))])

(defn- move! [generation ^js details]
  (let [{:keys [^js window]} @owner
        failed! #(rf/dispatch [:room-window/place-failed generation %])
        screens (screen-rects details)
        studio (.indexOf (.-screens details) (.-currentScreen details))
        room (apply screen-at screens (window-centre window))
        target (target-screen screens studio room)]
    (if-not target
      (failed! :single)
      (let [{:keys [left top width height label]} (nth screens target)]
        (.moveTo window left top)
        (.resizeTo window width height)
        ;; Browsers can ignore placement silently; check where the window landed.
        (js/setTimeout
         #(when (current? generation)
            (if (= target (apply screen-at screens (window-centre window)))
              (rf/dispatch [:room-window/placed generation label])
              (failed! :manual)))
         500)))))

(defn- watch-screens! [generation ^js details]
  (when-not (identical? details (:screens @owner))
    (swap! owner assoc :screens details)
    (.addEventListener details "screenschange"
                       #(when (current? generation)
                          (rf/dispatch [:room-window/screens generation (.-length (.-screens details))]))
                       #js {:signal (.-signal (:abort @owner))})))

(defn- place! [generation]
  (when (current? generation)
    (let [failed! #(rf/dispatch [:room-window/place-failed generation %])
          extended (some-> js/window .-screen .-isExtended)]
      (cond
        (native-host)
        (do (rf/dispatch [:room-window/placing generation])
            (.postMessage ^js (native-host) #js {:action "move-to-display" :name (.-name ^js (:window @owner))}))

        (not (fn? (aget js/window "getScreenDetails")))
        (failed! (if (false? extended) :single :manual))

        ;; No prompt for a permission that could not help.
        (false? extended) (failed! :single)

        :else
        (do (rf/dispatch [:room-window/placing generation])
            (-> (.getScreenDetails js/window)
                (.then (fn [^js details]
                         (when (current? generation)
                           (watch-screens! generation details)
                           (move! generation details))))
                (.catch (fn [^js error]
                          (failed! (if (= "NotAllowedError" (.-name error)) :denied :manual))))))))))

(defn- show-window! []
  (when-let [^js w (:window @owner)] (.focus w)))

(rf/reg-fx :room-window/open open!)
(rf/reg-fx :room-window/close close!)
(rf/reg-fx :room-window/place place!)

;; ---------------------------------------------------------------------------
;; Events

(defn- advance [db event] (assoc-in db [:ui :room-window] (step (state db) event)))

(defn- pop-out [db place?]
  (let [before (state db) after (step before [:pop-out place?])]
    (when-not (= before after)
      {:db (assoc-in db [:ui :room-window] after) :room-window/open (:generation after)})))

;; Both are dispatched synchronously from the click, so the window opens
;; inside the user's gesture and is not taken for an unsolicited pop-up.
(rf/reg-event-fx :room-window/pop-out (fn [{:keys [db]} _] (pop-out db false)))

(rf/reg-event-fx :room-window/move-to-display
  (fn [{:keys [db]} _]
    (let [{:keys [phase generation]} (state db)]
      (case phase
        :docked (pop-out db true)
        :popped {:room-window/place generation}
        nil))))

(rf/reg-event-fx :room-window/opened
  (fn [{:keys [db]} [_ generation]]
    (let [before (state db)]
      (cond-> {:db (advance db [:opened generation])}
        (and (:place? before) (= generation (:generation before)))
        (assoc :room-window/place generation)))))

(rf/reg-event-fx :room-window/failed
  (fn [{:keys [db]} [_ generation kind]]
    {:db (advance db [:failed generation kind])
     :dispatch [:studio/toast (failure-notices kind) true]}))

(rf/reg-event-fx :room-window/redock
  (fn [{:keys [db]} _]
    (let [before (state db) after (step before [:redock])]
      (when-not (= before after)
        {:db (assoc-in db [:ui :room-window] after) :room-window/close (:generation after)}))))

(doseq [event [:closed :placing :placed :place-failed :screens]]
  (rf/reg-event-db (keyword "room-window" (name event))
    (fn [db [_ & args]] (advance db (into [event] args)))))

;; ---------------------------------------------------------------------------
;; Views

(defn- action [label title on-click]
  [:button.text-button {:type "button" :title title :aria-label title :on-click on-click} label])

(defn controls
  "Pop out or Redock, and Move to display, beside the PiP and sharing actions."
  [db]
  (let [{:keys [phase]} (state db)]
    [:<>
     (case phase
       :docked [action "Pop out" "Open the Room in its own window" #(rf/dispatch-sync [:room-window/pop-out])]
       :popping [:span.small.muted {:role "status"} "Opening…"]
       :popped [action "Redock" "Return the Room to Studio" #(rf/dispatch [:room-window/redock])]
       :redocking [:span.small.muted {:role "status"} "Redocking…"]
       nil)
     (when (#{:docked :popped} phase)
       [action "Move to display" "Send the Room window to another display" #(rf/dispatch-sync [:room-window/move-to-display])])]))

(defn- notice-line [db]
  (when-let [text (notice (state db))]
    [:p.room-window-notice {:role "status"} text]))

(defn- placeholder [db]
  [:section#room-window-placeholder.panel.room-window-placeholder {:aria-label "The Room is in its own window"}
   [:div.panel-top
    [:div [:span.section-label "THE ROOM"]]
    [:div.preview-size-controls
     [action "Show window" "Bring the Room window to the front" show-window!]
     [controls db]]]
   [:div.room-window-body
    [notice-line db]
    [:p.small.muted "Cue changes here update the Room window. Close that window or press Redock to bring the Room back."]]])

(defn- portal [db panel]
  (when-let [root (:root @owner)]
    (react-dom/createPortal (r/as-element [:<> [panel db] [notice-line db]]) root)))

(defn room
  "The Room panel, docked in Studio or portalled into its own window."
  [db panel]
  (if (popped? db)
    [:<> [placeholder db] [portal db panel]]
    [panel db]))
