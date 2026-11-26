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

(defn param-id? [id] (boolean (and (keyword? id) (re-matches #"param-\d+" (name id)))))

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

(defn encode [saved]
  (js/JSON.stringify
   (clj->js {:version 2
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

(defn media-class []
  (if (fn? (aget js/window "matchMedia"))
    (size-class (.-matches (js/matchMedia (:phone media-queries)))
                (.-matches (js/matchMedia (:tablet media-queries))))
    (size-class (.-innerWidth js/window))))

(rf/reg-cofx :panel-layout/environment
  (fn [cofx _]
    (assoc cofx :panel-layout/environment
           {:class (media-class)
            :saved (if-let [text (some-> (storage) (.getItem storage-key))] (decode text) {})})))

(rf/reg-fx :panel-layout/store
  (fn [saved]
    (try (when-let [s (storage)]
           (if (seq saved) (.setItem s storage-key (encode saved)) (.removeItem s storage-key)))
         (catch :default _ nil))))

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
    {:container-width (:width (rect container))
     :rects (vec (for [element (array-seq (.querySelectorAll container ":scope > .layout-region"))]
                   (assoc (rect element) :id (keyword (.getAttribute element "data-panel")))))}))

(rf/reg-fx :panel-layout/measure
  (fn [_]
    (rf/dispatch [:panel-layout/began (measure-built-in)])))

(rf/reg-fx :panel-layout/focus
  (fn [[id selector]]
    (r/after-render
     #(some-> (.querySelector js/document (str "#view-perform [data-panel=\"" (name id) "\"] " selector)) (.focus)))))

;; ---------------------------------------------------------------------------
;; Events

(rf/reg-event-fx :panel-layout/initialize
  [(rf/inject-cofx :panel-layout/environment)]
  (fn [{:keys [db] :panel-layout/keys [environment]} _]
    {:db (update-in db [:ui :panel-layout] merge environment)}))

(rf/reg-event-fx :panel-layout/class
  (fn [{:keys [db]} [_ class]]
    (let [{:keys [edit] :as before} (state db)]
      (when (and (contains? classes class) (not= class (:class before)))
        (cond-> {:db (assoc-in db [:ui :panel-layout :class] class)}
          edit (-> (update :db assoc-in [:ui :panel-layout :edit] nil)
                   (assoc :dispatch [:studio/toast (str "Layout editing ended: the window changed to the "
                                                        (get-in classes [class :label]) " size. Nothing was saved.")])))))))

(defn- store-fx
  "Persist saved layouts, and redock windows of panels the new layout lacks."
  [db]
  (let [present (set (map :id (active-layout db)))
        orphans (remove #(or (some #{%} panel-ids) (present %)) (panel-window/detached-ids db))]
    (cond-> {:db db :panel-layout/store (:saved (state db))}
      (seq orphans) (assoc :dispatch-n (mapv (fn [id] [:panel-window/redock id]) orphans)))))

(defn- label [class] (get-in classes [class :label]))

(rf/reg-event-fx :panel-layout/begin
  (fn [{:keys [db]} _]
    (when-not (:edit (state db))
      (let [{:keys [saved]} (state db) class (current-class db)
            name (active-name saved class)]
        (if-let [layout (active-layout db)]
          {:db (assoc-in db [:ui :panel-layout :edit] {:class class :layout layout :name name :original name
                                                       :announce (str "Editing " name ".")})}
          {:panel-layout/measure true})))))

(rf/reg-event-db :panel-layout/began
  (fn [db [_ measured]]
    (if (:edit (state db))
      db
      (let [class (current-class db)]
        (assoc-in db [:ui :panel-layout :edit]
                  {:class class :layout (if measured (seed class measured) (default-layout class))
                   :name (next-name (:saved (state db)) class) :original nil
                   :announce "Editing a new layout."})))))

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
    (assoc (store-fx db) :dispatch [:studio/toast (str "Saved “" name "” for this " (label class) " screen.")])))

(rf/reg-event-fx :panel-layout/done
  (fn [{:keys [db]} _] (when (:edit (state db)) (finish db false))))

(rf/reg-event-fx :panel-layout/save-as
  (fn [{:keys [db]} _] (when (:edit (state db)) (finish db true))))

(rf/reg-event-fx :panel-layout/cancel
  (fn [{:keys [db]} _] (store-fx (assoc-in db [:ui :panel-layout :edit] nil))))

(rf/reg-event-fx :panel-layout/delete
  (fn [{:keys [db]} _]
    (when-let [{:keys [class original]} (:edit (state db))]
      (let [db (cond-> (assoc-in db [:ui :panel-layout :edit] nil)
                 original (update-in [:ui :panel-layout :saved] drop-named class original))]
        (cond-> (store-fx db)
          original (assoc :dispatch [:studio/toast (str "Deleted “" original "”. This screen uses the built-in layout.")]))))))

(rf/reg-event-fx :panel-layout/reset
  (fn [{:keys [db]} _]
    (let [class (or (:class (:edit (state db))) (current-class db))
          db (-> db (assoc-in [:ui :panel-layout :edit] nil)
                 (assoc-in [:ui :panel-layout :saved class :active] nil))]
      (assoc (store-fx db) :dispatch
             [:studio/toast (str "Built-in layout restored for this " (label class) " screen."
                                 (when (seq (named (:saved (state db)) class)) " Saved layouts stay in the menu."))]))))

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

(rf/reg-event-db :panel-layout/remove
  (fn [db [_ id]]
    (cond-> db
      (and (:edit (state db)) (extra? id))
      (edit-step (fn [_ layout] (remove-panel layout id)) id))))

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
      (store-fx (assoc-in db [:ui :panel-layout :edit] nil)))))

;; ---------------------------------------------------------------------------
;; Views

(defn- primary-pointer? [^js event]
  (or (= 0 (.-button event)) (#{"touch" "pen"} (.-pointerType event))))

(defn- pointer-start [kind id]
  (fn [^js event]
    (when (primary-pointer? event)
      (.preventDefault event)
      (.setPointerCapture (.-currentTarget event) (.-pointerId event))
      (let [{:keys [container-width rects]} (measure-regions)
            own (some #(when (= id (:id %)) %) rects)]
        (rf/dispatch [:panel-layout/gesture-start
                      {:kind kind :id id :pointer-id (.-pointerId event)
                       :x (.-clientX event) :y (.-clientY event)
                       :rects rects :container-width container-width
                       :width (:width own) :height (:height own)}])))))

(defn- pointer-handlers [kind id]
  {:on-pointer-down (pointer-start kind id)
   :on-pointer-move #(rf/dispatch [:panel-layout/gesture-move (.-pointerId %) (.-clientX %) (.-clientY %)])
   :on-pointer-up #(rf/dispatch [:panel-layout/gesture-end (.-pointerId %)])
   :on-pointer-cancel #(rf/dispatch [:panel-layout/gesture-cancel (.-pointerId %)])
   :on-lost-pointer-capture #(rf/dispatch [:panel-layout/gesture-end (.-pointerId %)])})

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

(defn- step-button [step label text event]
  ;; Moving a panel moves its DOM node; keep focus on the pressed button.
  [:button.layout-step {:type "button" :aria-label label :title label :data-step step
                        :on-click #(rf/dispatch (conj event (str "[data-step=\"" step "\"]")))}
   text])

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
               "Detach"]
      :popped [:button.layout-detach {:type "button" :title (str "Return " name " to Studio")
                                      :on-click #(rf/dispatch (if room? [:room-window/redock] [:panel-window/redock id]))}
               "Redock"]
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
       [step-button "earlier" (str "Move " name " earlier") "←" [:panel-layout/move id -1]]
       [step-button "later" (str "Move " name " later") "→" [:panel-layout/move id 1]]
       [step-button "narrower" (str "Make " name " narrower") "−W" [:panel-layout/resize id -1 0]]
       [step-button "wider" (str "Make " name " wider") "+W" [:panel-layout/resize id 1 0]]
       [step-button "shorter" (str "Make " name " shorter") "−H" [:panel-layout/resize id 0 -2]]
       [step-button "taller" (str "Make " name " taller") "+H" [:panel-layout/resize id 0 2]]
       [detach-control db id]
       (when (extra? id)
         [:button.layout-remove {:type "button" :title (str "Remove " name " from this layout")
                                               :on-click #(rf/dispatch [:panel-layout/remove id])} "Remove"])]
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
  (if (editing? db)
    (let [{:keys [announce name original layout]} (:edit (state db))
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
        "Reset to default"]
       [:p.sr-only {:aria-live "polite"} announce]])
    [:div.layout-toolbar.layout-idle
     [switcher db]
     [:button.text-button.layout-edit {:id "edit-layout" :type "button" :aria-pressed false
                                       :title "Rearrange and resize the Perform panels for this screen"
                                       :on-click #(rf/dispatch [:panel-layout/begin])}
      "Edit layout"]]))

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
                 content (if (extra? id)
                           (when-let [render (:render extra)] (render entry (or size :full)))
                           (regions id))]]
       ^{:key id}
       [:div.layout-region
        (cond-> {:data-panel (name id)}
          layout (assoc :style (region-style entry gesture id) :data-tier (name size))
          (and editing (= id (:id gesture))) (assoc :data-gesture (name (:kind gesture)))
          (and editing gesture (= id (:target gesture))) (assoc :data-drop-target true))
        [:div.layout-content {:inert (boolean editing)}
         (if (= :room id) content (panel-window/region db id (title id) content))]
        (when editing [chrome db entry index total gesture (when (extra? id) (:chrome extra))])])]))
