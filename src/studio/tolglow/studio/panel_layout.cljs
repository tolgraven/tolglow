(ns tolglow.studio.panel-layout
  "Arrange the Perform view's panels in an explicit layout edit mode.

  Each screen size class (phone, tablet, desktop) holds any number of named
  layouts and remembers which one is active. A layout is an ordered list of
  panels with a width in grid columns and a height in grid rows. With no
  active layout the Perform view keeps its built-in responsive CSS layout.
  Layouts belong to this browser and screen, so they live in local storage,
  never in the show document or the portable settings API.

  Besides the four Perform panels a layout can hold a parameter visualization
  panel and any number of parameter tiles, each showing one chosen parameter.
  Every panel's content adapts to its tile size (`tier`).

  Panels are keyed children of one grid. Reordering moves their DOM nodes and
  resizing changes their grid spans, so a panel is never remounted: a running
  cue's controls, the Room's preview engine and every subscription stay the
  same objects. Any panel can be detached into its own window: the Room through
  `tolglow.studio.room-window`, the others through `tolglow.studio.panel-window`."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [reagent.core :as r]
            [tolglow.studio.panel-window :as panel-window]
            [tolglow.studio.room-window :as room-window]))

;; ---------------------------------------------------------------------------
;; Model (pure)

(def panels
  "The Perform panels every layout holds."
  [{:id :cues :title "Cues"}
   {:id :live :title "Live"}
   {:id :room :title "Room"}
   {:id :tools :title "Tools"}])

(def panel-ids (mapv :id panels))

(def ^:private titles (assoc (into {} (map (juxt :id :title)) panels) :viz "Visualization"))

(defn param-id? [id] (boolean (and (keyword? id) (re-matches #"param-\d{1,9}" (name id)))))

(defn extra? [id] (or (= :viz id) (param-id? id)))

(defn title [id]
  (cond (titles id) (titles id)
        (param-id? id) (str "Parameter " (subs (name id) 6))
        :else (some-> id name)))

(def classes
  "Grid metrics per screen size class. The breakpoints match the Perform
  view's built-in CSS layout."
  {:phone {:columns 2 :gap 12 :label "phone"}
   :tablet {:columns 6 :gap 16 :label "tablet"}
   :desktop {:columns 12 :gap 16 :label "desktop"}})

(def row-px 20)

(def min-rows 6)

(def max-rows 80)

(def media-queries
  {:phone "(max-width: 600px)" :tablet "(max-width: 1199px)"})

(defn size-class
  "Size class from the two media query results (or a width in CSS pixels)."
  ([width] (size-class (<= width 600) (< width 1200)))
  ([phone? tablet?] (cond phone? :phone tablet? :tablet :else :desktop)))

(defn columns [class] (get-in classes [class :columns] 12))

(defn gap [class] (get-in classes [class :gap] 16))

(def ^:private default-sizes
  {:desktop {:cues [5 30] :live [7 60] :room [5 26] :tools [5 12] :viz [6 24] :param [2 6]}
   :tablet {:cues [6 28] :live [6 36] :room [6 30] :tools [6 12] :viz [6 20] :param [2 6]}
   :phone {:cues [2 26] :live [2 30] :room [2 34] :tools [2 12] :viz [2 20] :param [1 6]}})

(defn- clamp [low high n] (max low (min high n)))

(defn- whole [value fallback]
  (if (and (number? value) (js/Number.isFinite value)) (js/Math.round value) fallback))

(defn default-entry [class id]
  (let [[w h] (get-in default-sizes [class (if (param-id? id) :param id)] [(columns class) 18])]
    {:id id :w w :h h}))

(defn default-layout [class] (mapv #(default-entry class %) panel-ids))

(defn- entry-id [id]
  (let [id (cond (keyword? id) id (string? id) (keyword id))]
    (when (or (some #{id} panel-ids) (extra? id)) id)))

(defn- text [value] (when (string? value) (not-empty (str/trim value))))

(defn binding-of
  "A parameter tile's binding: a cue ID and a parameter name, both strings."
  [value]
  (when (map? value)
    (let [cue (text (:cue value)) param (text (:param value))]
      (when (and cue param) {:cue cue :param param}))))

(defn normalize
  "Repair a stored layout for this size class: drop unknown and repeated
  panels, clamp sizes to the grid, and append Perform panels it does not
  mention. Parameter tiles keep a valid binding."
  [class layout]
  (let [entries (reduce (fn [out item]
                          (let [id (when (map? item) (entry-id (:id item)))]
                            (if (or (nil? id) (some #(= id (:id %)) out))
                              out
                              (let [{:keys [w h]} (default-entry class id)
                                    binding (when (param-id? id) (binding-of (:binding item)))]
                                (conj out (cond-> {:id id
                                                   :w (clamp 1 (columns class) (whole (:w item) w))
                                                   :h (clamp min-rows max-rows (whole (:h item) h))}
                                            binding (assoc :binding binding)))))))
                        [] (when (sequential? layout) layout))
        present (set (map :id entries))]
    (into entries (comp (remove present) (map #(default-entry class %))) panel-ids)))

(defn tier
  "How much a panel shows at its size: :full (controls and plots), :compact
  (numeric readouts) or :single (one reading, the 1×1 tile)."
  [class {:keys [w h]}]
  (let [n (columns class)]
    (cond (or (nil? w) (nil? h)) :full
          (or (<= w (max 1 (quot n 6))) (<= h 8)) :single
          (or (<= w (quot n 3)) (<= h 14)) :compact
          :else :full)))

(defn window-tier
  "The tier of a panel filling a window `width` by `height` CSS pixels: the full
  width of that window's size class, and its height in grid rows."
  [width height]
  (let [class (size-class width)]
    (tier class {:w (columns class) :h (js/Math.floor (/ height row-px))})))

(defn add-panel
  "Add the visualization panel (once) or a new parameter tile."
  [class layout kind]
  (case kind
    :viz (if (some #(= :viz (:id %)) layout) layout (conj layout (default-entry class :viz)))
    :param (let [n (inc (reduce max 0 (keep #(when (param-id? (:id %)) (js/parseInt (subs (name (:id %)) 6) 10)) layout)))]
             (conj layout (default-entry class (keyword (str "param-" n)))))
    layout))

(defn remove-panel [layout id]
  (if (extra? id) (filterv #(not= id (:id %)) layout) layout))

(defn bind [layout id binding]
  (mapv #(if (= id (:id %))
           (if-let [b (binding-of binding)] (assoc % :binding b) (dissoc % :binding))
           %)
        layout))

(defn- index-of [layout id] (first (keep-indexed #(when (= id (:id %2)) %1) layout)))

(defn move
  "Move a panel by delta positions, staying within the list."
  [layout id delta]
  (if-let [from (index-of layout id)]
    (let [to (clamp 0 (dec (count layout)) (+ from delta))
          without (into (subvec layout 0 from) (subvec layout (inc from)))]
      (into (conj (subvec without 0 to) (nth layout from)) (subvec without to)))
    layout))

(defn move-to
  "Put a dragged panel where the target panel is. Dropping on a later panel
  lands after it and on an earlier panel before it."
  [layout id target]
  (if-let [to (index-of layout target)]
    (move layout id (- to (index-of layout id)))
    layout))

(defn set-size [class layout id w h]
  (mapv #(if (= id (:id %))
           (assoc % :w (clamp 1 (columns class) w) :h (clamp min-rows max-rows h))
           %)
        layout))

(defn resize [class layout id dw dh]
  (if-let [{:keys [w h]} (some #(when (= id (:id %)) %) layout)]
    (set-size class layout id (+ w dw) (+ h dh))
    layout))

(defn column-width [class container-width]
  (let [n (columns class)]
    (/ (- container-width (* (gap class) (dec n))) n)))

(defn span-for
  "Grid width and height closest to a box measured in CSS pixels. A region's
  box includes the gap below it, so heights are whole rows."
  [class container-width width height]
  (let [step (+ (column-width class container-width) (gap class))]
    [(clamp 1 (columns class) (js/Math.round (/ (+ width (gap class)) step)))
     (clamp min-rows max-rows (js/Math.round (/ height row-px)))]))

(defn seed
  "First layout for a size class, measured from the built-in layout the
  operator is looking at. Panels hidden on this screen get default sizes."
  [class {:keys [width regions]}]
  (normalize class
             (for [{:keys [id] :as region} regions
                   :let [visible? (and (pos? (:width region 0)) (pos? (:height region 0)) (pos? (or width 0)))]]
               (if visible?
                 (let [[w h] (span-for class width (:width region) (+ (:height region) (gap class)))]
                   {:id id :w w :h h})
                 (default-entry class id)))))

(defn target-at
  "The panel under point x, y among measured rectangles, other than the one
  being dragged."
  [rects dragged x y]
  (some (fn [{:keys [id left top width height]}]
          (when (and (not= id dragged) (<= left x) (< x (+ left width)) (<= top y) (< y (+ top height)))
            id))
        rects))

(defn drag-size
  "Width and height while a corner is dragged dx, dy pixels from its start."
  [class {:keys [container-width width height]} dx dy]
  (span-for class container-width (+ width dx) (+ height dy)))

(def ^:private moved-px 6)

(defn- moved? [{:keys [dx dy]}] (> (js/Math.hypot (or dx 0) (or dy 0)) moved-px))

;; ---------------------------------------------------------------------------
;; State in app-db at [:ui :panel-layout]:
;;   :class  current size class
;;   :saved  {class layout} as stored on this device
;;   :edit   nil, or {:class :layout :gesture :announce} while editing

(defn state [db] (get-in db [:ui :panel-layout]))

(defn current-class [db] (:class (state db) :desktop))

(defn editing? [db]
  (let [{:keys [edit class]} (state db)]
    (and edit (= class (:class edit)))))

;; Saved layouts: {class {:active name-or-nil :layouts [{:name :panels}]}}

(defn named [saved class] (get-in saved [class :layouts] []))

(defn active-name [saved class]
  (let [name (get-in saved [class :active])]
    (when (some #(= name (:name %)) (named saved class)) name)))

(defn find-panels [saved class name]
  (some #(when (= name (:name %)) (:panels %)) (named saved class)))

(defn unique-name
  "The wanted name, or the wanted name with a number when another layout on
  this screen (other than `except`) already has it."
  [saved class wanted except]
  (let [taken (set (remove #{except} (map :name (named saved class))))]
    (first (remove taken (cons wanted (map #(str wanted " " %) (iterate inc 2)))))))

(defn next-name [saved class] (unique-name saved class (str "Layout " (inc (count (named saved class)))) nil))

(defn save-named
  "Store panels under name, renaming `original` in place when given."
  [saved class original name panels]
  (let [layouts (named saved class)
        entry {:name name :panels panels}]
    (-> saved
        (assoc-in [class :layouts]
                  (if (and original (some #(= original (:name %)) layouts))
                    (mapv #(if (= original (:name %)) entry %) layouts)
                    (conj layouts entry)))
        (assoc-in [class :active] name))))

(defn drop-named [saved class name]
  (let [saved (update-in saved [class :layouts] #(filterv (fn [l] (not= name (:name l))) (or % [])))]
    (cond-> saved (= name (get-in saved [class :active])) (assoc-in [class :active] nil))))

(defn active-layout
  "The layout to render now, or nil for the built-in responsive layout."
  [db]
  (let [{:keys [class saved edit]} (state db)]
    (if (and edit (= class (:class edit)))
      (:layout edit)
      (some->> (active-name saved class) (find-panels saved class) (normalize class)))))

(defn- describe [layout id]
  (if-let [{:keys [w h]} (some #(when (= id (:id %)) %) layout)]
    (str (title id) " · position " (inc (index-of layout id)) " of " (count layout)
         " · " w " columns by " h " rows")
    (str (title id) " removed.")))

(defn- edit-step [db f id]
  (let [{:keys [class layout]} (get-in db [:ui :panel-layout :edit])
        next (f class layout)]
    (update-in db [:ui :panel-layout :edit] assoc :layout next :announce (describe next id))))

(defn- decode-panels [class panels]
  (normalize class (map #(cond-> % (:binding %) (update :binding binding-of)) panels)))

(defn decode
  "Saved layouts from their stored JSON text; anything unreadable is empty.
  Version 1 held one unnamed layout per screen; it becomes \"Layout 1\"."
  [stored]
  (try
    (let [{:keys [version layouts] :as data} (js->clj (js/JSON.parse stored) :keywordize-keys true)
          by-class (:classes data)
          known (fn [m] (filter (fn [[class _]] (contains? classes class)) m))]
      (cond
        (not (map? data)) {}
        (and (= 1 version) (map? layouts))
        (into {} (for [[class panels] (known layouts)]
                   [class {:active "Layout 1" :layouts [{:name "Layout 1" :panels (decode-panels class panels)}]}]))
        (and (= 2 version) (map? by-class))
        (into {} (for [[class {:keys [active layouts]}] (known by-class)
                       :let [entries (reduce (fn [out {:keys [name panels]}]
                                               (let [name (text name)]
                                                 (if (or (nil? name) (some #(= name (:name %)) out))
                                                   out
                                                   (conj out {:name name :panels (decode-panels class panels)}))))
                                             [] (when (sequential? layouts) (filter map? layouts)))]
                       :when (seq entries)]
                   [class {:active (some #(when (= active (:name %)) active) entries) :layouts entries}]))
        :else {}))
    (catch :default _ {})))

(defn encode
  "Stored JSON for saved layouts. `saved-at` (epoch ms) lets a reader prefer
  the newer of two copies."
  [saved & [saved-at]]
  (js/JSON.stringify
   (clj->js {:version 2
             :savedAt (or saved-at 0)
             :classes (into {} (for [[class {:keys [active layouts]}] saved :when (seq layouts)]
                                 [(name class) {:active active
                                                :layouts (mapv (fn [{:keys [name panels]}]
                                                                 {:name name :panels (mapv #(update % :id cljs.core/name) panels)})
                                                               layouts)}]))})))

;; ---------------------------------------------------------------------------
;; Effects. Storage and DOM measurement stay here, outside app-db.

(def storage-key "tolglow.studio.panel-layouts")

(defn- storage []
  (try (.-localStorage js/window) (catch :default _ nil)))

(defn- stored-text []
  (try (some-> (storage) (.getItem storage-key)) (catch :default _ nil)))

;; The packaged Mac app serves each launch on a new loopback port, so page
;; storage does not survive a relaunch there. When the app's window binding
;; (`window.tolglowStudioWindow`, #669) carries a `layouts` copy and accepts
;; `{type: "layouts", layouts, request}` messages on its `tolglowStudioWindow`
;; handler, layouts are kept by the app per show folder as well. The app answers
;; each message with a `tolglow-studio-window` event on the page whose detail is
;; `{type: "layouts", request, kept}`; it refuses a copy over its size limit. In a
;; browser, or until the app provides that copy, only local storage is used.

(defn- host-text []
  (try (let [binding (aget js/window "tolglowStudioWindow")
             text (when (some? binding) (aget binding "layouts"))]
         (when (string? text) text))
       (catch :default _ nil)))

(defn- host-handler []
  (some-> (aget js/window "webkit") (aget "messageHandlers") (aget "tolglowStudioWindow")))

(defonce ^:private host-requests (atom {:next 0 :pending {}}))

(def host-reply-ms
  "How long to wait for the app to say whether it kept a copy."
  3000)

(defn- host-reply! [^js event]
  (let [^js detail (.-detail event)]
    (when (and detail (= "layouts" (.-type detail)))
      (let [request (.-request detail)
            done (get-in @host-requests [:pending request])]
        (when done
          (swap! host-requests update :pending dissoc request)
          (done (if (true? (.-kept detail)) :kept :refused)))))))

(defonce ^:private host-listener
  (delay (.addEventListener js/window "tolglow-studio-window" host-reply!)))

(defn- post-host!
  "Post a copy to the app; `done` later receives :kept, :refused or :silent (no
  answer in time). False when there is no app or the post failed."
  [text done]
  (when-let [^js handler (host-handler)]
    @host-listener
    (let [request (:next (swap! host-requests update :next inc))]
      (swap! host-requests assoc-in [:pending request] done)
      (js/setTimeout #(when (get-in @host-requests [:pending request])
                        (swap! host-requests update :pending dissoc request)
                        (done :silent))
                     host-reply-ms)
      (try (.postMessage handler #js {:type "layouts" :layouts text :request request})
           true
           (catch :default _
             (swap! host-requests update :pending dissoc request)
             false)))))

(defn- saved-at [text]
  (try (let [v (some-> text js/JSON.parse (aget "savedAt"))] (if (number? v) v 0)) (catch :default _ -1)))

(defn newest-text
  "Of the app's copy and the page's copy, the one saved last (the page's on a
  tie, since a reload keeps the app's copy from when the window opened)."
  [host local]
  (cond (nil? host) local
        (nil? local) host
        (> (saved-at host) (saved-at local)) host
        :else local))

(defn media-class []
  (if (fn? (aget js/window "matchMedia"))
    (size-class (.-matches (js/matchMedia (:phone media-queries)))
                (.-matches (js/matchMedia (:tablet media-queries))))
    (size-class (.-innerWidth js/window))))

(rf/reg-cofx :panel-layout/environment
  (fn [cofx _]
    (assoc cofx :panel-layout/environment
           {:class (media-class)
            :saved (if-let [text (newest-text (host-text) (stored-text))] (decode text) {})})))

(defn- write-local!
  "Keep saved layouts in local storage. True when it accepted them."
  [saved text]
  (try (if-let [s (storage)]
         (do (if (seq saved) (.setItem s storage-key text) (.removeItem s storage-key)) true)
         false)
       (catch :default _ false)))

(def failed-notice
  "This browser did not keep the layout (storage is blocked or full). It stays in use until Studio reloads.")

(def host-refused-notice
  (str "The Mac app did not keep a copy of this layout (it keeps up to 64 KB of layouts for each show folder), "
       "so it will be gone after the app relaunches. It stays in use until then."))

(defn store-toast
  "The toast after a save as [text error?], or nil. `local?` says whether local
  storage kept it; `host` is nil outside the Mac app, else what the app said:
  :kept, :refused or :silent (no answer). Inside the app only the app's copy
  survives a relaunch."
  [local? host notice]
  (cond
    (or (= :kept host) (and local? (nil? host))) (when notice [notice false])
    local? [host-refused-notice true]
    :else [failed-notice true]))

(defn- toast! [[text error?]]
  (rf/dispatch [:studio/toast text error?]))

;; The confirmation toast is shown only once the write succeeded: inside the Mac
;; app, once the app said it kept its copy.
(rf/reg-fx :panel-layout/store
  (fn [{:keys [saved notice]}]
    (let [text (encode saved (js/Date.now))
          local? (write-local! saved text)]
      (when-not (post-host! text #(some-> (store-toast local? % notice) toast!))
        (some-> (store-toast local? nil notice) toast!)))))

(defn- layout-element [] (.querySelector js/document "#view-perform .performance-layout"))

(defn- rect [^js element]
  (let [r (.getBoundingClientRect element)]
    {:left (.-left r) :top (.-top r) :width (.-width r) :height (.-height r)}))

(defn measure-built-in
  "Measure the built-in layout: the container width and each panel's content."
  []
  (when-let [container (layout-element)]
    {:width (:width (rect container))
     :regions (vec (for [id panel-ids
                         :let [content (.querySelector container (str "[data-panel=\"" (name id) "\"] > .layout-content > *"))]]
                     (merge {:id id} (when content (select-keys (rect content) [:width :height])))))}))

(defn- measure-regions []
  (when-let [container (layout-element)]
    ;; Page coordinates, so the rectangles stay valid while the page scrolls
    ;; during a drag (the layout itself does not change until the drop).
    {:container-width (:width (rect container))
     :rects (vec (for [element (array-seq (.querySelectorAll container ":scope > .layout-region"))]
                   (-> (rect element)
                       (update :left + (.-scrollX js/window))
                       (update :top + (.-scrollY js/window))
                       (assoc :id (keyword (.getAttribute element "data-panel"))))))}))

(rf/reg-fx :panel-layout/measure
  (fn [_]
    (rf/dispatch [:panel-layout/began (measure-built-in)])))

(rf/reg-fx :panel-layout/focus
  (fn [[id selector]]
    (r/after-render
     #(some-> (.querySelector js/document (str "#view-perform [data-panel=\"" (name id) "\"] " selector)) (.focus)))))

;; Entering or leaving edit mode replaces the focused toolbar button, so put
;; focus on the first panel's title, or back on Edit layout.
(rf/reg-fx :panel-layout/focus-control
  (fn [selector]
    (r/after-render #(some-> (.querySelector js/document (str "#view-perform " selector)) (.focus)))))

(def ^:private first-handle ".layout-handle")

(def ^:private edit-button "#edit-layout")

;; ---------------------------------------------------------------------------
;; Events

;; A detached panel (tolglow.studio.panel-window, tolglow.studio.room-window)
;; adapts to its window, not to the tile it left: the window reports its size.
(rf/reg-event-db :panel-layout/window-size
  (fn [db [_ id width height]]
    (if (and (number? width) (number? height) (pos? width) (pos? height))
      (assoc-in db [:ui :window-tiers id] (window-tier width height))
      db)))

(defn detached-tier
  "The tier of panel `id` in its own window, :full until the window reports."
  [db id]
  (get-in db [:ui :window-tiers id] :full))

(rf/reg-event-fx :panel-layout/initialize
  [(rf/inject-cofx :panel-layout/environment)]
  (fn [{:keys [db] :panel-layout/keys [environment]} _]
    {:db (update-in db [:ui :panel-layout] merge environment)}))

(defn- settle-fx
  "Redock windows of panels the new layout lacks. Leaving edit mode puts
  focus back on Edit layout."
  [db left-editing?]
  (let [present (set (map :id (active-layout db)))
        orphans (remove #(or (some #{%} panel-ids) (present %)) (panel-window/detached-ids db))]
    (cond-> {:db db}
      left-editing? (assoc :panel-layout/focus-control edit-button)
      (seq orphans) (assoc :dispatch-n (mapv (fn [id] [:panel-window/redock id]) orphans)))))

(defn- store-fx
  "Persist saved layouts, then settle windows and focus as settle-fx does."
  [db & [left-editing? notice]]
  (assoc (settle-fx db left-editing?)
         :panel-layout/store (cond-> {:saved (:saved (state db))} notice (assoc :notice notice))))

(defn- label [class] (get-in classes [class :label]))

;; Another size class has its own layouts, so a window whose panel that
;; layout lacks is redocked, as on a layout switch.
(rf/reg-event-fx :panel-layout/class
  (fn [{:keys [db]} [_ class]]
    (let [{:keys [edit] :as before} (state db)]
      (when (and (contains? classes class) (not= class (:class before)))
        (cond-> (settle-fx (cond-> (assoc-in db [:ui :panel-layout :class] class)
                             edit (assoc-in [:ui :panel-layout :edit] nil))
                           false)
          edit (assoc :dispatch [:studio/toast (str "Layout editing ended: the window changed to the "
                                                    (get-in classes [class :label]) " size. Nothing was saved.")]))))))

(rf/reg-event-fx :panel-layout/begin
  (fn [{:keys [db]} _]
    (when-not (:edit (state db))
      (let [{:keys [saved]} (state db) class (current-class db)
            name (active-name saved class)]
        (if-let [layout (active-layout db)]
          {:db (assoc-in db [:ui :panel-layout :edit] {:class class :layout layout :name name :original name
                                                       :announce (str "Editing " name ".")})
           :panel-layout/focus-control first-handle}
          {:panel-layout/measure true})))))

(rf/reg-event-fx :panel-layout/began
  (fn [{:keys [db]} [_ measured]]
    (when-not (:edit (state db))
      (let [class (current-class db)]
        {:db (assoc-in db [:ui :panel-layout :edit]
                       {:class class :layout (if measured (seed class measured) (default-layout class))
                        :name (next-name (:saved (state db)) class) :original nil
                        :announce "Editing a new layout."})
         :panel-layout/focus-control first-handle}))))

(rf/reg-event-db :panel-layout/name
  (fn [db [_ value]]
    (cond-> db (:edit (state db)) (assoc-in [:ui :panel-layout :edit :name] (str value)))))

(defn- finish [db as-new?]
  (let [{:keys [class layout original] :as edit} (:edit (state db))
        saved (:saved (state db))
        wanted (or (text (:name edit)) original (next-name saved class))
        name (unique-name saved class wanted (when-not as-new? original))
        db (update-in db [:ui :panel-layout] assoc :edit nil
                      :saved (save-named saved class (when-not as-new? original) name layout))]
    (store-fx db true (str "Saved “" name "” for this " (label class) " screen."))))

(rf/reg-event-fx :panel-layout/done
  (fn [{:keys [db]} _] (when (:edit (state db)) (finish db false))))

(rf/reg-event-fx :panel-layout/save-as
  (fn [{:keys [db]} _] (when (:edit (state db)) (finish db true))))

(rf/reg-event-fx :panel-layout/cancel
  (fn [{:keys [db]} _] (settle-fx (assoc-in db [:ui :panel-layout :edit] nil) true)))

(rf/reg-event-fx :panel-layout/delete
  (fn [{:keys [db]} _]
    (when-let [{:keys [class original]} (:edit (state db))]
      (let [db (cond-> (assoc-in db [:ui :panel-layout :edit] nil)
                 original (update-in [:ui :panel-layout :saved] drop-named class original))]
        (store-fx db true (when original (str "Deleted “" original "”. This screen uses the built-in layout.")))))))

(rf/reg-event-fx :panel-layout/reset
  (fn [{:keys [db]} _]
    (let [class (or (:class (:edit (state db))) (current-class db))
          db (-> db (assoc-in [:ui :panel-layout :edit] nil)
                 (assoc-in [:ui :panel-layout :saved class :active] nil))]
      (store-fx db true (str "Built-in layout restored for this " (label class) " screen."
                             (when (seq (named (:saved (state db)) class)) " Saved layouts stay in the menu."))))))

(rf/reg-event-fx :panel-layout/select
  (fn [{:keys [db]} [_ name]]
    (let [{:keys [saved edit]} (state db) class (current-class db)]
      (when (and (not edit) (or (nil? name) (find-panels saved class name)))
        (store-fx (assoc-in db [:ui :panel-layout :saved class :active] name))))))

(rf/reg-event-fx :panel-layout/add
  (fn [{:keys [db]} [_ kind]]
    (when-let [{:keys [class layout]} (:edit (state db))]
      (let [next (add-panel class layout kind)
            id (:id (peek next))]
        (when-not (= next layout)
          {:db (update-in db [:ui :panel-layout :edit] assoc :layout next :announce (str (title id) " added at the end."))
           :panel-layout/focus [id ".layout-handle"]})))))

;; A removed panel's window closes now rather than staying empty until Done or
;; Cancel. Cancel brings the panel back docked.
(rf/reg-event-fx :panel-layout/remove
  (fn [{:keys [db]} [_ id]]
    (when (and (:edit (state db)) (extra? id))
      (cond-> {:db (edit-step db (fn [_ layout] (remove-panel layout id)) id)}
        (panel-window/detached? db id) (assoc :dispatch [:panel-window/redock id])))))

(rf/reg-event-fx :panel-layout/bind
  (fn [{:keys [db]} [_ id binding]]
    (let [{:keys [edit saved]} (state db) class (current-class db)]
      (cond
        edit {:db (update-in db [:ui :panel-layout :edit :layout] bind id binding)}
        (active-name saved class)
        (let [name (active-name saved class)]
          (store-fx (assoc-in db [:ui :panel-layout :saved]
                              (save-named saved class name name (bind (find-panels saved class name) id binding)))))))))

(rf/reg-event-fx :panel-layout/move
  (fn [{:keys [db]} [_ id delta focus]]
    (when (:edit (state db))
      (cond-> {:db (edit-step db (fn [_ layout] (move layout id delta)) id)}
        focus (assoc :panel-layout/focus [id focus])))))

(rf/reg-event-fx :panel-layout/resize
  (fn [{:keys [db]} [_ id dw dh focus]]
    (when (:edit (state db))
      (cond-> {:db (edit-step db (fn [class layout] (resize class layout id dw dh)) id)}
        focus (assoc :panel-layout/focus [id focus])))))

(rf/reg-event-db :panel-layout/gesture-start
  (fn [db [_ gesture]]
    (cond-> db
      (and (:edit (state db)) (not (get-in db [:ui :panel-layout :edit :gesture])))
      (assoc-in [:ui :panel-layout :edit :gesture] (assoc gesture :dx 0 :dy 0)))))

(defn- gesture-update [db {:keys [kind id] :as gesture} x y]
  (let [moved (assoc gesture :dx (- x (:x gesture)) :dy (- y (:y gesture)))
        class (get-in db [:ui :panel-layout :edit :class])]
    (case kind
      :move (assoc moved :target (when (moved? moved) (target-at (:rects gesture) id x y)))
      :resize (let [[w h] (drag-size class gesture (:dx moved) (:dy moved))]
                (assoc moved :size [w h])))))

(rf/reg-event-db :panel-layout/gesture-move
  (fn [db [_ pointer-id x y]]
    (let [gesture (get-in db [:ui :panel-layout :edit :gesture])]
      (if (= pointer-id (:pointer-id gesture))
        (assoc-in db [:ui :panel-layout :edit :gesture] (gesture-update db gesture x y))
        db))))

(rf/reg-event-db :panel-layout/gesture-end
  (fn [db [_ pointer-id]]
    (let [{:keys [kind id target size] :as gesture} (get-in db [:ui :panel-layout :edit :gesture])]
      (if (= pointer-id (:pointer-id gesture))
        (let [db (assoc-in db [:ui :panel-layout :edit :gesture] nil)]
          (cond
            (and (= :move kind) target) (edit-step db (fn [_ layout] (move-to layout id target)) id)
            (and (= :resize kind) size (moved? gesture))
            (edit-step db (fn [class layout] (apply set-size class layout id size)) id)
            :else db))
        db))))

(defn- cancel-gesture [db pointer-id]
  (let [gesture (get-in db [:ui :panel-layout :edit :gesture])]
    (if (and gesture (or (nil? pointer-id) (= pointer-id (:pointer-id gesture))))
      (update-in db [:ui :panel-layout :edit] assoc :gesture nil :announce "Change cancelled.")
      db)))

(rf/reg-event-db :panel-layout/gesture-cancel
  (fn [db [_ pointer-id]] (cancel-gesture db pointer-id)))

;; Escape acts at once: a queued follow-up event could land after the
;; pointer release of the gesture it was meant to cancel.
(rf/reg-event-fx :panel-layout/escape
  (fn [{:keys [db]} _]
    (if (get-in db [:ui :panel-layout :edit :gesture])
      {:db (cancel-gesture db nil)}
      (settle-fx (assoc-in db [:ui :panel-layout :edit] nil) true))))

;; ---------------------------------------------------------------------------
;; Views

(defn- primary-pointer? [^js event]
  (or (= 0 (.-button event)) (#{"touch" "pen"} (.-pointerType event))))

;; Gestures work in page coordinates (client position plus scroll).
(defn- page-x [client-x] (+ client-x (.-scrollX js/window)))

(defn- page-y [client-y] (+ client-y (.-scrollY js/window)))

(def ^:private edge-px 64)

(def ^:private max-scroll-px 18)

(defn edge-speed
  "Pixels per frame to scroll while a drag is within edge-px of the top or
  bottom of a viewport `height` tall: negative upwards, faster nearer the edge."
  [client-y height]
  (cond (< client-y edge-px) (- (js/Math.ceil (* max-scroll-px (/ (- edge-px (max 0 client-y)) edge-px))))
        (> client-y (- height edge-px)) (js/Math.ceil (* max-scroll-px (/ (- (min height client-y) (- height edge-px)) edge-px)))
        :else 0))

;; Auto-scroll owner for the gesture in progress: the last pointer position and
;; the handle that captured it. The loop ends with the gesture, when the
;; handle leaves the document, or when the pointer leaves the edges.
(defonce ^:private autoscroll (atom nil))

(defn- stop-autoscroll!
  "End auto-scroll: for every pointer, or only when `pointer-id` owns it. Another
  pointer lifting must not stop the drag in progress."
  ([] (reset! autoscroll nil))
  ([pointer-id] (swap! autoscroll #(when-not (= pointer-id (:pointer-id %)) %))))

(defn claim-autoscroll
  "The auto-scroll owner after `pointer` presses a handle. A gesture already in
  progress keeps it, as it keeps the gesture (:panel-layout/gesture-start)."
  [owner gesture pointer]
  (if (and gesture (not= (:pointer-id gesture) (:pointer-id pointer)))
    (when (= (:pointer-id owner) (:pointer-id gesture)) owner)
    pointer))

(defn- visible-speed
  "Edge speed against what is visible: the visual viewport when the browser
  has one (pinch zoom, on-screen keyboards), else the window."
  [client-y]
  (if-let [^js vv (.-visualViewport js/window)]
    (edge-speed (- client-y (.-offsetTop vv)) (.-height vv))
    (edge-speed client-y (.-innerHeight js/window))))

(defn- scroll-limit
  "The furthest the page scrolls now. Measured when a drag starts, because the
  dragged panel's own translation lengthens the document as the page scrolls:
  scrolling to the live end would chase it forever."
  []
  (max 0 (- (.. js/document -documentElement -scrollHeight) (.-innerHeight js/window))))

(defn- autoscroll-frame [_]
  (when-let [{:keys [pointer-id client-x client-y limit ^js element]} @autoscroll]
    (let [before (.-scrollY js/window)
          speed (let [s (visible-speed client-y)] (if (pos? s) (max 0 (min s (- limit before))) s))]
      (if (or (zero? speed) (not (.-isConnected element)))
        (swap! autoscroll assoc :running? false)
        (do
          (.scrollBy js/window 0 speed)
          (when (not= before (.-scrollY js/window))
            (rf/dispatch [:panel-layout/gesture-move pointer-id (page-x client-x) (page-y client-y)]))
          (js/requestAnimationFrame autoscroll-frame))))))

(defn- track-pointer! [^js event]
  (let [pointer-id (.-pointerId event) client-x (.-clientX event) client-y (.-clientY event)]
    (when (= pointer-id (:pointer-id @autoscroll))
      (swap! autoscroll assoc :client-x client-x :client-y client-y)
      (when (and (not (:running? @autoscroll)) (not (zero? (visible-speed client-y))))
        (swap! autoscroll assoc :running? true)
        (js/requestAnimationFrame autoscroll-frame)))
    (rf/dispatch [:panel-layout/gesture-move pointer-id (page-x client-x) (page-y client-y)])))

(defn- pointer-start [kind id]
  (fn [^js event]
    (when (primary-pointer? event)
      (.preventDefault event)
      (.setPointerCapture (.-currentTarget event) (.-pointerId event))
      (swap! autoscroll claim-autoscroll (get-in @rf-db/app-db [:ui :panel-layout :edit :gesture])
             {:pointer-id (.-pointerId event) :element (.-currentTarget event)
              :client-x (.-clientX event) :client-y (.-clientY event) :limit (scroll-limit)})
      (let [{:keys [container-width rects]} (measure-regions)
            own (some #(when (= id (:id %)) %) rects)]
        (rf/dispatch [:panel-layout/gesture-start
                      {:kind kind :id id :pointer-id (.-pointerId event)
                       :x (page-x (.-clientX event)) :y (page-y (.-clientY event))
                       :rects rects :container-width container-width
                       :width (:width own) :height (:height own)}])))))

(defn- pointer-handlers [kind id]
  {:on-pointer-down (pointer-start kind id)
   :on-pointer-move track-pointer!
   :on-pointer-up #(do (stop-autoscroll! (.-pointerId %)) (rf/dispatch [:panel-layout/gesture-end (.-pointerId %)]))
   :on-pointer-cancel #(do (stop-autoscroll! (.-pointerId %)) (rf/dispatch [:panel-layout/gesture-cancel (.-pointerId %)]))
   :on-lost-pointer-capture #(do (stop-autoscroll! (.-pointerId %)) (rf/dispatch [:panel-layout/gesture-end (.-pointerId %)]))})

(def ^:private move-keys {"ArrowLeft" -1 "ArrowUp" -1 "ArrowRight" 1 "ArrowDown" 1 "Home" -99 "End" 99})

(def ^:private resize-keys {"ArrowLeft" [-1 0] "ArrowRight" [1 0] "ArrowUp" [0 -1] "ArrowDown" [0 1]})

(defn- handle-keys [id]
  (fn [^js event]
    (when-let [delta (move-keys (.-key event))]
      (.preventDefault event)
      (rf/dispatch [:panel-layout/move id delta ".layout-handle"]))))

(defn- resize-keys-handler [id]
  (fn [^js event]
    (when-let [[dw dh] (resize-keys (.-key event))]
      (.preventDefault event)
      (let [n (if (.-shiftKey event) 4 1)]
        (rf/dispatch [:panel-layout/resize id (* n dw) (* n dh) ".layout-resize"])))))

(defn- step-button [step label text panel event]
  ;; Moving a panel moves its DOM node; keep focus on the pressed button. The
  ;; visible word starts the accessible name (WCAG 2.5.3); the panel follows.
  [:button.layout-step {:type "button" :title label :data-step step
                        :on-click #(rf/dispatch (conj event (str "[data-step=\"" step "\"]")))}
   text [:span.sr-only (str " " panel)]])

(def ^:private window-sizes {:cues [760 560] :live [720 760] :tools [560 640] :viz [720 640]})

(defn- detach-control [db id]
  (let [room? (= :room id)
        {:keys [phase]} (if room? (room-window/state db) (panel-window/state db id))
        name (title id)]
    (case phase
      :docked [:button.layout-detach {:type "button" :title (str "Open " name " in its own window")
                                      :on-click (if room?
                                                  #(rf/dispatch-sync [:room-window/pop-out])
                                                  #(rf/dispatch-sync [:panel-window/pop-out id name
                                                                      (window-sizes id (if (param-id? id) [380 300] [720 640]))]))}
               "Detach" [:span.sr-only (str " " name)]]
      :popped [:button.layout-detach {:type "button" :title (str "Return " name " to Studio")
                                      :on-click #(rf/dispatch (if room? [:room-window/redock] [:panel-window/redock id]))}
               "Redock" [:span.sr-only (str " " name)]]
      [:span.small.muted {:role "status"} (if (= :redocking phase) "Redocking…" "Opening…")])))

(defn- chrome [db {:keys [id w h] :as entry} index total gesture chrome-extra]
  (let [name (title id)
        class (current-class db)
        [w h] (if (and (= id (:id gesture)) (:size gesture)) (:size gesture) [w h])]
    [:div.layout-chrome {:role "group" :aria-label (str name " panel layout")}
     [:div.layout-chrome-bar
      [:button.layout-handle (merge {:type "button"
                                     :aria-label (str "Move " name " panel, position " (inc index) " of " total
                                                      ". Drag, or use the arrow keys.")
                                     :on-key-down (handle-keys id)}
                                    (pointer-handlers :move id))
       [:span.layout-grip {:aria-hidden true}] name]
      [:span.layout-size {:aria-hidden true} (str w " × " h)]
      [:div.layout-steps
       [step-button "earlier" (str "Move " name " earlier") "Earlier" name [:panel-layout/move id -1]]
       [step-button "later" (str "Move " name " later") "Later" name [:panel-layout/move id 1]]
       [step-button "narrower" (str "Make " name " narrower") "Narrower" name [:panel-layout/resize id -1 0]]
       [step-button "wider" (str "Make " name " wider") "Wider" name [:panel-layout/resize id 1 0]]
       [step-button "shorter" (str "Make " name " shorter") "Shorter" name [:panel-layout/resize id 0 -2]]
       [step-button "taller" (str "Make " name " taller") "Taller" name [:panel-layout/resize id 0 2]]
       [detach-control db id]
       (when (extra? id)
         [:button.layout-remove {:type "button" :title (str "Remove " name " from this layout")
                                 :on-click #(rf/dispatch [:panel-layout/remove id])}
          "Remove" [:span.sr-only (str " " name)]])]
      (when chrome-extra [chrome-extra entry])]
     [:button.layout-resize (merge {:type "button"
                                    :aria-label (str "Resize " name ", " w " of " (columns class) " columns by " h
                                                     " rows. Drag, or use the arrow keys; Shift steps by four.")
                                    :on-key-down (resize-keys-handler id)}
                                   (pointer-handlers :resize id))]]))

(defn- escape-listener []
  (r/with-let [handler (fn [^js event]
                         (when (and (= "Escape" (.-key event)) (not (.-defaultPrevented event))
                                    (not (.querySelector js/document "dialog[open]")))
                           (.preventDefault event)
                           (stop-autoscroll!)
                           (rf/dispatch [:panel-layout/escape])))
               _ (.addEventListener js/document "keydown" handler)]
    nil
    (finally (.removeEventListener js/document "keydown" handler))))

(defn- delete-button [original]
  (r/with-let [confirming (r/atom false)]
    (if @confirming
      [:span.layout-confirm
       [:button.text-button.layout-danger {:id "layout-delete-confirm" :type "button"
                                           :on-click #(rf/dispatch [:panel-layout/delete])}
        (str "Delete “" original "”")]
       [:button.text-button {:type "button" :on-click #(reset! confirming false)} "Keep"]]
      [:button.text-button {:id "layout-delete" :type "button" :on-click #(reset! confirming true)} "Delete layout"])))

(defn- switcher [db]
  (let [{:keys [saved]} (state db) class (current-class db) layouts (named saved class)]
    (when (seq layouts)
      [:label.layout-switch
       [:span.sr-only "Layout for this screen"]
       [:select {:id "layout-select" :value (or (active-name saved class) "")
                 :on-change #(rf/dispatch [:panel-layout/select (not-empty (.. % -target -value))])}
        [:option {:value ""} "Built-in layout"]
        (for [{:keys [name]} layouts] ^{:key name} [:option {:value name} name])]])))

(defn toolbar
  "Switch and edit layouts; while editing, name, add panels, save or discard."
  [db]
  ;; The live region stays mounted and starts empty, so the first message
  ;; after entering edit mode is announced.
  [:<>
   [:p#layout-announce.sr-only {:aria-live "polite"} (when (editing? db) (:announce (:edit (state db))))]
   (if (editing? db)
    (let [{:keys [name original layout]} (:edit (state db))
          class (current-class db)]
      [:div.layout-toolbar {:role "group" :aria-label "Layout editing"}
       [escape-listener]
       [:span.layout-hint (str "Arranging the " (label class) " layout. "
                               "Drag a title to move a panel and its corner to resize it.")]
       [:label.layout-name [:span "Name"]
        [:input {:id "layout-name" :type "text" :value (or name "") :max-length 40
                 :on-change #(rf/dispatch-sync [:panel-layout/name (.. % -target -value)])}]]
       [:div.layout-add {:role "group" :aria-label "Add a panel"}
        [:button.text-button {:id "layout-add-param" :type "button" :on-click #(rf/dispatch [:panel-layout/add :param])}
         "+ Parameter tile"]
        [:button.text-button {:id "layout-add-viz" :type "button" :disabled (boolean (some #(= :viz (:id %)) layout))
                              :on-click #(rf/dispatch [:panel-layout/add :viz])} "+ Visualization"]]
       [:button.button.primary {:id "layout-done" :type "button" :on-click #(rf/dispatch [:panel-layout/done])} "Done"]
       (when original
         [:button.button {:id "layout-save-as" :type "button" :on-click #(rf/dispatch [:panel-layout/save-as])} "Save as new"])
       [:button.button {:id "layout-cancel" :type "button" :on-click #(rf/dispatch [:panel-layout/cancel])} "Cancel"]
       (when original [delete-button original])
       [:button.text-button {:id "layout-reset" :type "button" :on-click #(rf/dispatch [:panel-layout/reset])}
        "Reset to default"]])
    [:div.layout-toolbar.layout-idle
     [switcher db]
     [:button.text-button.layout-edit {:id "edit-layout" :type "button"
                                       :title "Rearrange and resize the Perform panels for this screen"
                                       :on-click #(rf/dispatch [:panel-layout/begin])}
      "Edit layout"]])])

(defn- region-style [{:keys [w h]} gesture id]
  (let [[w h] (if (and (= id (:id gesture)) (:size gesture)) (:size gesture) [w h])]
    (cond-> {:grid-column (str "span " w) :grid-row (str "span " h)}
      (and (= id (:id gesture)) (= :move (:kind gesture)))
      (assoc :transform (str "translate(" (:dx gesture) "px," (:dy gesture) "px)")))))

(defn arrangement
  "The Perform panels in the active layout. `regions` maps a Perform panel ID
  to its content. `extra` renders added panels: `(:render extra)` takes an
  entry and its tier, and `(:chrome extra)`, if given, adds edit controls for
  an entry. `attrs` are the container's own attributes."
  [db attrs regions & [extra]]
  (let [layout (active-layout db)
        editing (editing? db)
        gesture (get-in (state db) [:edit :gesture])
        class (current-class db)
        entries (or layout (mapv (fn [id] {:id id}) panel-ids))
        total (count entries)]
    [:div.performance-layout
     (cond-> (assoc attrs :data-layout-class (name class))
       layout (-> (update :class #(str/join " " (remove str/blank? [% "custom-layout" (when editing "editing-layout")])))
                  (assoc :style {"--layout-columns" (columns class) "--layout-gap" (str (gap class) "px")
                                 "--layout-row" (str row-px "px")})))
     (for [[index {:keys [id] :as entry}] (map-indexed vector entries)
           :let [size (when layout
                        (tier class (if (and (= id (:id gesture)) (:size gesture))
                                      (zipmap [:w :h] (:size gesture)) entry)))
                 ;; Content in its own window follows that window's size.
                 detached (when (and (not= :room id) (panel-window/detached? db id)) (detached-tier db id))
                 content (if (extra? id)
                           (when-let [render (:render extra)] (render entry (or detached size :full)))
                           (regions id))]]
       ^{:key id}
       [:div.layout-region
        (cond-> {:data-panel (name id)}
          layout (assoc :style (region-style entry gesture id) :data-tier (name size))
          (and editing (= id (:id gesture))) (assoc :data-gesture (name (:kind gesture)))
          (and editing gesture (= id (:target gesture))) (assoc :data-drop-target true))
        [:div.layout-content {:inert (boolean editing)}
         (if (= :room id) content (panel-window/region db id (title id) content detached))]
        (when editing [chrome db entry index total gesture (when (extra? id) (:chrome extra))])])]))
