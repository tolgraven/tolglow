(ns tolglow.surface.link
  "Attached control surfaces, links and committed viewports (#705, #706 slice 1).

  A surface is one attached controller or Studio component, described by a
  tolglow.surface.descriptor. Each of its viewports (the X-Touch :strips, a
  Push :grid or :encoders) has a fixed geometry, declares a content axis in its
  descriptor, and looks through a window onto a content space such as the
  mixer strip list. A link joins viewports of two or more surfaces into one
  logical surface. Each member names its own viewport; members link when their
  descriptors declare the same axis, so an X-Touch :strips and a Push
  :encoders can share a channel window.

  Modes:
  - :mirror  every member shows the leader's window.
  - :extend  members sit side by side in :direction (:right :left :down :up),
             packed by their actual widths, and the window pages by the whole
             span. Unavailable members are left out of the packing, so the
             remaining ones page through every cell; the layout returns when
             they reattach.
  - :follow  each member after the leader starts :offset leader pages after
             the leader's window (start + offset x leader width) and shows its
             own width of cells from there. The window pages by the leader's
             width, so a follower never makes the leader skip pages.
  The first member is the leader and its content is what every member shows.

  Surfaces, links and windows are show data. They change only through owner
  commands and are read from the snapshot projection. A detached surface keeps
  its entry, so its links and windows survive reattaching under the same ID,
  and a saved show keeps links and windows with every surface detached.
  Device ports, sessions and per-member gestures stay with the transport.
  Content spaces are a registry (content-cells, content-size, content-axis),
  so a cue grid can be added without changing this namespace.

  Each logical surface also carries a committed layout state (#706 slice 2):
  the encoder mode, its page, the selected cell and flip. It lives beside the
  window (on the link, or on an unlinked surface viewport) and moves with it,
  so every linked member shows the same mode and selection. Layouts such as
  tolglow.surface.mixer read it; :control-surface/layout changes it."
  (:require [tolglow.effects.group-channels :as group-channels]
            [tolglow.surface.descriptor :as descriptor]))

(def ^:private state-path [:extensions ::state])

(def ^:private settings-path [:settings :controllers :surfaces])

(def operations
  #{:control-surface/attach :control-surface/detach :control-surface/remove
    :control-surface/link :control-surface/unlink :control-surface/bank
    :control-surface/layout})

(def modes #{:mirror :extend :follow})

(def directions #{:right :left :down :up})

(def encoder-modes
  "Encoder assignments a layout can show (see tolglow.surface.mixer)."
  #{:pan :send :plug-in :user})

(def layout-defaults {:encoder-mode :pan :page 0 :selected nil :flip? false})

(defn- cell-key? [k]
  (and (vector? k) (= 2 (count k)) (keyword? (first k))
       (let [id (second k)] (or (keyword? id) (and (string? id) (seq id))
                                (and (vector? id) (= 2 (count id)))))))

(defn layout-errors
  "Problems with a layout patch, or nil. Keys are optional; nil :selected
  clears the selection."
  [patch]
  (cond
    (not (map? patch)) "A layout is a map"
    (not (every? (set (keys layout-defaults)) (keys patch))) "A layout takes :encoder-mode :page :selected :flip?"
    (and (contains? patch :encoder-mode) (not (contains? encoder-modes (:encoder-mode patch)))) "Unknown encoder mode"
    (and (contains? patch :page) (not (and (nat-int? (:page patch)) (<= (:page patch) 1024)))) "A page is 0..1024"
    (and (some? (:selected patch)) (not (cell-key? (:selected patch)))) "Select a cell key such as [:cue :ember]"
    (and (contains? patch :flip?) (not (boolean? (:flip? patch)))) "Flip is boolean"))

(defn- reject! [code message details]
  (throw (ex-info message {:control/error code :details details})))

(defn- id? [x] (or (keyword? x) (and (string? x) (seq x))))

(defn- state [model] (get-in model state-path {:surfaces {} :links {}}))

;; Content registry

(defmulti content-cells
  "Every cell of a content space: {:x :y :key :label ...}, keyed by :type."
  (fn [_model content] (:type content)))

(defmulti content-size
  "{:columns :rows} of a content space. Override when cells are costly."
  (fn [_model content] (:type content)))

(defmulti content-axis
  "The axis a content space spreads over, matched against viewport axes."
  :type)

(defmulti content-valid?
  "Type-specific checks on a content map beyond the shared ones; true by default."
  :type)

(defmethod content-valid? :default [_] true)

(defn content-type? [type] (contains? (dissoc (methods content-cells) :default) type))

(defn- cells-size [cells]
  {:columns (if (seq cells) (inc (apply max (map :x cells))) 0)
   :rows (if (seq cells) (inc (apply max (map :y cells))) 0)})

(defmethod content-cells :default [_ content]
  (throw (ex-info "Unknown content type" {:control/error :invalid-surface :details {:type (:type content)}})))

(defmethod content-axis :default [_] ::unregistered)

(defmethod content-size :default [model content] (cells-size (content-cells model content)))

(defmethod content-cells :none [_ _] [])

(defmethod content-axis :none [_] nil)

(def ^:private last-projection (atom nil))

(defn group-projection
  "group-channels/projection, remembered for the last model value seen. Device
  refreshes read strips, the window and meters from one committed model, and
  the projection is a pure function of it, so one model costs one projection."
  [model]
  (let [[seen projection] @last-projection]
    (if (identical? seen model)
      projection
      (let [projection (group-channels/projection model)]
        (reset! last-projection [model projection])
        projection))))

(defn group-strips
  "Group strips for an :effect-group-view (:manual or :overlay), in bank order.
  Manual groups list children after parents."
  [view model]
  (let [{:keys [manual] :as mixer} (group-projection model)
        expanded (when (= :manual view)
                   (into #{} (map :id) (mapcat #(tree-seq :groups :groups %) manual)))]
    (into []
          (comp (filter #(= :group (:type %)))
                (map (fn [{:keys [id name level depth instance-ids]}]
                       {:group-id id :label name :level level :active? (boolean (seq instance-ids))
                        :color (cond (= :overlay view) [1 0 1]
                                     (pos? depth) [0 0 1]
                                     :else [0 1 1])})))
          (group-channels/channel-rows mixer view expanded))))

(defn- latest-instance-id [model cue-id]
  (:id (last (sort-by :order (filter #(= cue-id (:cue-id %)) (vals (:instances model)))))))

(defn- mixer-channels
  "Group strips, then configured cue channels: the order the X-Touch banks."
  [model {:keys [effect-group-view cue-strips]}]
  (into (if effect-group-view (group-strips effect-group-view model) []) cue-strips))

(defmethod content-cells :mixer-strips [model content]
  (vec (map-indexed
        (fn [index {:keys [group-id cue-id label]}]
          (cond-> {:x index :y 0 :label (or label (when cue-id (get-in model [:registry :cues cue-id :name]))
                                                 (some-> cue-id name) "")}
            group-id (assoc :key [:group group-id] :group-id group-id)
            cue-id (assoc :key [:cue cue-id] :cue-id cue-id :instance-id (latest-instance-id model cue-id))))
        (mixer-channels model content))))

(defmethod content-size :mixer-strips [model content]
  ;; Counting strips needs no labels or instance lookups (device hot path).
  {:columns (+ (if-let [view (:effect-group-view content)] (count (group-strips view model)) 0)
               (count (:cue-strips content)))
   :rows 1})

(defmethod content-axis :mixer-strips [_] :channels)

;; Logical surfaces

(defn- default-viewport [state surface-id]
  (first (sort (keys (get-in state [:surfaces surface-id :viewports])))))

(defn- link-of [{:keys [links]} surface-id viewport]
  (some (fn [[_ link]]
          (when (some #(and (= surface-id (:surface-id %)) (= viewport (:viewport %))) (:members link)) link))
        links))

(defn- member-info [state {:keys [surface-id viewport offset]}]
  (let [[columns rows] (get-in state [:surfaces surface-id :viewports viewport :geometry])]
    {:surface-id surface-id :viewport viewport :offset (or offset {:x 0 :y 0})
     :shape {:columns columns :rows rows}
     :attached? (boolean (get-in state [:surfaces surface-id :attached?]))}))

(defn- pack
  "Cell offsets of extend members in :direction, by actual widths, top-left at
  0. Only attached members are packed while any is attached."
  [direction members]
  (let [packed (or (seq (filter :attached? members)) members)
        [axis size sign] (case direction :right [:x :columns 1] :left [:x :columns -1]
                           :down [:y :rows 1] :up [:y :rows -1])
        placed (loop [[m & more] packed at 0 out []]
                 (if-not m
                   out
                   (let [extent (get-in m [:shape size])
                         start (if (pos? sign) at (- at extent))]
                     (recur more (+ at (* sign extent)) (conj out (assoc m :x 0 :y 0 axis start))))))
        low (apply min (map axis placed))]
    (into {} (map (fn [m] [(:surface-id m) (update m axis - low)])) placed)))

(defn- logical
  "The logical surface for a link ID, or for one surface viewport (its link,
  or the surface alone)."
  [state target viewport]
  (let [link (or (get-in state [:links target])
                 (link-of state target (or viewport (default-viewport state target))))
        viewport (or viewport (default-viewport state target))
        members (cond link (mapv #(member-info state %) (:members link))
                      (get-in state [:surfaces target :viewports viewport])
                      [(member-info state {:surface-id target :viewport viewport})])]
    (when (seq members)
      (let [leader (first members)
            mode (if link (:mode link) :single)]
        {:id (if link (:id link) target) :link? (boolean link) :mode mode
         :direction (:direction link :right)
         :viewport (:viewport leader) :members members :leader leader
         :packed (when (= :extend mode) (pack (:direction link :right) members))
         :content (get-in state [:surfaces (:surface-id leader) :viewports (:viewport leader) :content])
         :window (if link (:window link) (get-in state [:surfaces target :viewports viewport :window]))
         :layout (merge layout-defaults
                        (if link (:layout link) (get-in state [:surfaces target :viewports viewport :layout])))}))))

(defn- span
  "Cells one page of the logical surface covers: the packed extent for extend,
  the leader's viewport otherwise."
  [{:keys [mode packed leader]}]
  (if (= :extend mode)
    (let [members (vals packed)
          extent (fn [axis size] (- (apply max (map #(+ (axis %) (size (:shape %))) members))
                                    (apply min (map axis members))))]
      {:columns (extent :x :columns) :rows (extent :y :rows)})
    (:shape leader)))

(defn- limit [cells span-size page]
  (if (or (<= cells span-size) (zero? page)) 0 (* page (quot (dec cells) page))))

(defn- limits
  "Last window. Everything that fits starts at 0; otherwise windows stay on
  multiples of the leader's viewport, so a remainder that does not fill the
  span starts at the window's top-left edge."
  [{:keys [leader] :as view} size]
  (let [{:keys [columns rows]} (span view) shape (:shape leader)]
    {:x (limit (:columns size) columns (:columns shape)) :y (limit (:rows size) rows (:rows shape))}))

(defn- clamp-window [view size window]
  (let [{:keys [x y]} (limits view size)]
    {:x (max 0 (min x (:x window 0))) :y (max 0 (min y (:y window 0)))}))

(defn- resolved
  "Logical surface with its content size, clamped window and, unless
  cells? is false, its cells."
  ([model target viewport] (resolved model target viewport true))
  ([model target viewport cells?]
   (when-let [view (logical (state model) target viewport)]
     (let [cells (when cells? (content-cells model (:content view)))
           size (if cells? (cells-size cells) (content-size model (:content view)))]
       (assoc view :cells cells :size size :window (clamp-window view size (:window view)))))))

(defn- member-window
  "Window of one member, or nil when an extend link leaves it unpacked."
  [{:keys [mode window members packed leader]} surface-id]
  (when-let [{:keys [offset shape]} (some #(when (= surface-id (:surface-id %)) %) members)]
    (case mode
      (:single :mirror) window
      :extend (when-let [{:keys [x y]} (get packed surface-id)] {:x (+ (:x window) x) :y (+ (:y window) y)})
      ;; Offsets count in leader pages; the leader's window is never scaled.
      :follow (if (= surface-id (:surface-id leader))
                window
                {:x (max 0 (+ (:x window) (* (:x offset 0) (get-in leader [:shape :columns]))))
                 :y (max 0 (+ (:y window) (* (:y offset 0) (get-in leader [:shape :rows]))))}))))

(defn logical-position
  "Remap a member-local [x y] in one viewport to the logical surface's content
  coordinates. Gesture identity stays with the member; only coordinates move."
  [model surface-id viewport [x y]]
  (when-let [view (resolved model surface-id viewport false)]
    (when-let [origin (member-window view surface-id)]
      {:logical-id (:id view) :at [(+ x (:x origin)) (+ y (:y origin))]})))

;; Device reads (X-Touch profile options)

(defn- attached? [model surface-id]
  (get-in (state model) [:surfaces surface-id :attached?]))

(defn strip-channels
  "Channels an X-Touch surface banks over: its :strips logical surface's mixer
  strips (the leader's, when linked). Cue channels keep their bindings.
  Until its attach commits, a device shows its own configured content."
  ([model surface-id] (strip-channels model surface-id nil))
  ([model surface-id own-content]
   (if-not (attached? model surface-id)
     (when own-content (mixer-channels model own-content))
     (when-let [view (logical (state model) surface-id :strips)]
       (when (= :mixer-strips (get-in view [:content :type]))
         (mixer-channels model (:content view)))))))

(defn strip-offset
  "First strip this surface shows: the committed logical window plus the
  surface's place in its link. The X-Touch profile reads it as its bank.
  Until its attach commits, a device keeps its own last window."
  [model surface-id]
  (if-not (attached? model surface-id)
    (get-in (state model) [:surfaces surface-id :viewports :strips :window :x] 0)
    (or (some-> (resolved model surface-id :strips false) (member-window surface-id) :x) 0)))

(defn strip-placement
  "Where a strip surface sits in its logical surface, for layouts:
  :first   the first strip it shows (strip-offset);
  :index   its first strip's place within one logical page (0 for the leader,
           a lone unit or a mirror; 8 for an extender to the right of an
           X-Touch; offset x leader width for a follower);
  :span    the strips one logical page covers (16 for main plus extender);
  :logical-id and the committed :layout (layout-defaults when unset).
  Until its attach commits, a device is a lone unit at its own last window."
  [model surface-id]
  (let [view (when (attached? model surface-id) (resolved model surface-id :strips false))
        origin (some-> view (member-window surface-id))]
    (if (and view origin)
      {:logical-id (:id view) :first (:x origin) :index (- (:x origin) (get-in view [:window :x]))
       :span (:columns (span view)) :layout (:layout view)}
      {:logical-id surface-id :first (strip-offset model surface-id) :index 0
       :span (or (get-in (state model) [:surfaces surface-id :viewports :strips :geometry 0]) 8)
       :layout (merge layout-defaults (get-in (state model) [:surfaces surface-id :viewports :strips :layout]))})))

;; Settings and projection

(defn settings
  "The surface settings home, [:settings :controllers :surfaces], with defaults."
  [model]
  (merge {:follow-selection? false} (get-in model settings-path)))

(defn validate-settings!
  "Validate the :controllers settings scope. Only :surfaces is checked here."
  [controllers]
  (when-let [surfaces (:surfaces controllers)]
    (when-not (and (map? surfaces) (every? #{:follow-selection?} (keys surfaces))
                   (every? boolean? (vals surfaces)))
      (throw (ex-info "Surface settings accept only a boolean :follow-selection?" {:settings surfaces}))))
  controllers)

(defn window
  "Project one logical surface (a link, or one surface viewport): mode,
  content type, window, span, content size and cell count, last window, and
  per member its viewport, offset, window and the cells it shows with
  member-local :position."
  [model target viewport]
  (when-let [{:keys [link? members cells] :as view} (resolved model target viewport)]
    {:id (:id view) :link? link? :mode (:mode view) :direction (when (= :extend (:mode view)) (:direction view))
     :viewport (:viewport view) :content-type (get-in view [:content :type]) :count (count cells)
     :window (:window view) :span (span view) :size (:size view)
     :limit (limits view (:size view)) :layout (:layout view)
     :members (mapv (fn [{:keys [surface-id attached? shape] :as member}]
                      (let [{:keys [x y] :as origin} (member-window view surface-id)]
                        (assoc (select-keys member [:surface-id :viewport :offset :attached?])
                               :window origin
                               :cells (if-not (and attached? origin) []
                                        (into [] (keep (fn [cell]
                                                         (let [column (- (:x cell) x) row (- (:y cell) y)]
                                                           (when (and (< -1 column (:columns shape)) (< -1 row (:rows shape)))
                                                             (assoc cell :position {:x column :y row})))))
                                              cells)))))
                    members)}))

(defn with-refusals
  "Add host-side refusals ({surface-id {:op :status :code :message}}) to a
  projection: :refusals for all, and :last-refusal on each known surface."
  [projection refusals]
  (if (empty? refusals)
    projection
    (-> projection
        (assoc :refusals refusals)
        (update :surfaces (fn [surfaces]
                            (mapv #(if-let [refusal (get refusals (:id %))] (assoc % :last-refusal refusal) %) surfaces))))))

(defn projection
  "Snapshot view: attached and detached surfaces, every logical surface
  (links first, then unlinked surface viewports) and the settings home."
  [model]
  (let [{:keys [surfaces links] :as current} (state model)
        unlinked (for [[id surface] (sort-by (comp str key) surfaces)
                       viewport (sort (keys (:viewports surface)))
                       :when (not (link-of current id viewport))]
                   [id viewport])]
    {:settings (settings model)
     :surfaces (mapv (fn [[id {:keys [descriptor role attached? viewports]}]]
                       {:id id :descriptor descriptor :role role :attached? (boolean attached?)
                        :viewports (vec (sort (keys viewports)))})
                     (sort-by (comp str key) surfaces))
     :logical (into (mapv #(window model % nil) (sort-by str (keys links)))
                    (map (fn [[id viewport]] (window model id viewport)))
                    unlinked)}))

;; Commands

(defn- transition [before after event result]
  {:model after :result result
   :events (if (= before after) [] [(assoc result :type event)])})

(defn- position? [{:keys [x y] :as at}]
  (and (map? at) (integer? x) (integer? y) (<= -16 x 16) (<= -16 y 16)))

(defn- window? [{:keys [x y] :as window}]
  (and (map? window) (nat-int? x) (nat-int? y)))

(defn- content? [content]
  (and (map? content) (content-type? (:type content))
       (contains? #{nil :manual :overlay} (:effect-group-view content))
       (vector? (:cue-strips content []))
       (every? #(and (map? %) (id? (:cue-id %))) (:cue-strips content []))
       (content-valid? content)))

(defn- update-state [model f & args]
  (assoc-in model state-path (apply f (state model) args)))

(defn- dissolve
  "Remove a link, leaving each member at the window it showed, with the
  link's layout state."
  [model link-id]
  (let [view (resolved model link-id nil false)
        layout (get-in (state model) [:links link-id :layout])
        model (update-state model update :links dissoc link-id)]
    (if-not view
      model
      (update-state model
                    (fn [current]
                      (reduce (fn [current {:keys [surface-id viewport]}]
                                (let [shown (member-window view surface-id)
                                      path [:surfaces surface-id :viewports viewport]]
                                  (if (and shown (get-in current path))
                                    (cond-> (assoc-in current (conj path :window) shown)
                                      layout (assoc-in (conj path :layout) layout))
                                    current)))
                              current (:members view)))))))

(defn- keep-window
  "Choose a link window so that keep-id's member window stays as it was."
  [model link-id keep-id before]
  (let [probe (logical (state model) link-id nil)
        offset (member-window (assoc probe :window {:x 0 :y 0}) keep-id)]
    (if (and before offset)
      (update-state model assoc-in [:links link-id :window]
                    {:x (max 0 (- (:x before) (:x offset))) :y (max 0 (- (:y before) (:y offset)))})
      model)))

(defn- bank-target [view {:keys [move to reveal] :as command}]
  (let [{:keys [window size cells]} view
        {:keys [columns rows]} (span view)
        clamp #(clamp-window view size %)]
    (cond
      (contains? command :to)
      (do (when-not (and (map? to) (every? nat-int? (vals (select-keys to [:x :y]))))
            (reject! :invalid-value "Move to a nonnegative {:x :y} cell" {:to to}))
          (clamp (merge window (select-keys to [:x :y]))))

      (contains? command :reveal)
      (let [cell (some #(when (= reveal (:key %)) %) cells)]
        (when-not cell
          (reject! :unknown-cell "That cell is not on this surface" {:reveal reveal}))
        (if (some (fn [{:keys [surface-id attached? shape]}]
                    (when-let [{:keys [x y]} (when attached? (member-window view surface-id))]
                      (and (<= x (:x cell) (+ x (:columns shape) -1))
                           (<= y (:y cell) (+ y (:rows shape) -1)))))
                  (:members view))
          window
          (let [{:keys [leader members mode]} view
                ;; In follow mode with the leader away, page an attached follower onto the cell.
                shower (when (and (= :follow mode) (not (:attached? leader)))
                         (first (filter :attached? members)))]
            (if-let [{:keys [surface-id shape]} shower]
              (let [offset (member-window (assoc view :window {:x 0 :y 0}) surface-id)
                    width (:columns shape) height (:rows shape)]
                (clamp {:x (- (* width (quot (:x cell) width)) (:x offset))
                        :y (- (* height (quot (:y cell) height)) (:y offset))}))
              (clamp {:x (* columns (quot (:x cell) columns)) :y (* rows (quot (:y cell) rows))})))))

      :else
      (let [{limit-x :x limit-y :y} (limits view size)]
        (clamp (case move
                 :previous (update window :x - columns)
                 :next (update window :x + columns)
                 :channel-previous (update window :x dec)
                 :channel-next (update window :x inc)
                 :first (assoc window :x 0)
                 :last (assoc window :x limit-x)
                 :up (update window :y - rows)
                 :down (update window :y + rows)
                 :top (assoc window :y 0)
                 :bottom (assoc window :y limit-y)
                 (reject! :invalid-value "Unknown surface move" {:move move})))))))

(defn- own-source? [{:keys [source surface-id]}]
  (= source {:adapter :surface :session-id surface-id}))

(defn- attach [model current {:keys [surface-id role viewports] descriptor-id :descriptor :as command}]
  (let [{groups :viewports :as described} (descriptor/descriptor descriptor-id)
        previous (get-in current [:surfaces surface-id])]
    (when-not (and (id? surface-id) described (keyword? role) (map? viewports)
                   (= (set (keys viewports)) (set (keys groups)))
                   (every? content? (vals viewports))
                   (not (contains? (:links current) surface-id)))
      (reject! :invalid-surface "Attach a surface ID, a known descriptor, a role and content for each viewport"
               {:surface-id surface-id :descriptor descriptor-id}))
    (when (and (:attached? previous) (not (own-source? command)))
      (reject! :surface-owned "That surface's device is connected; only its own connection can replace it"
               {:surface-id surface-id}))
    (transition model
                (update-state model assoc-in [:surfaces surface-id]
                              {:id surface-id :descriptor descriptor-id :role role :attached? true
                               :viewports (into {} (map (fn [[viewport content]]
                                                          (let [layout (get-in previous [:viewports viewport :layout])]
                                                            [viewport (cond-> {:geometry (get groups viewport) :content content
                                                                               :window (get-in previous [:viewports viewport :window] {:x 0 :y 0})}
                                                                        layout (assoc :layout layout))])))
                                                viewports)})
                :control-surface/attached {:surface-id surface-id})))

(defn- viewport-axis
  "The content axis a member's descriptor declares for its viewport."
  [current {:keys [surface-id viewport]}]
  (get-in (descriptor/descriptor (get-in current [:surfaces surface-id :descriptor])) [:viewport-axes viewport]))

(defn- axes-compatible?
  "Members' viewports declare one axis, and the leader's content spreads over it."
  [current members]
  (let [axes (map #(viewport-axis current %) members)
        {:keys [surface-id viewport]} (first members)
        content (get-in current [:surfaces surface-id :viewports viewport :content])]
    (and (some? (first axes)) (apply = axes)
         (contains? #{nil (first axes)} (some-> content content-axis)))))

(defn- link [model current {:keys [link-id mode direction members] :or {mode :extend direction :right}}]
  (when-not (and (vector? members) (every? map? members))
    (reject! :invalid-link "Link members are a vector of {:surface-id :viewport :offset}" {:link-id link-id}))
  (let [members (mapv (fn [{:keys [surface-id viewport offset]}]
                        (cond-> {:surface-id surface-id :viewport (or viewport (default-viewport current surface-id))}
                          (and offset (not= offset {:x 0 :y 0})) (assoc :offset offset)))
                      members)
        ids (map :surface-id members)]
    (when-not (and (id? link-id) (not (contains? (:surfaces current) link-id))
                   (contains? modes mode) (contains? directions direction)
                   (<= 2 (count members) 16) (apply distinct? ids)
                   (every? #(get-in current [:surfaces (:surface-id %) :viewports (:viewport %)]) members)
                   (every? #(position? (:offset % {:x 0 :y 0})) members)
                   (axes-compatible? current members))
      (reject! :invalid-link "Link two or more attached surface viewports that declare the same content axis"
               {:link-id link-id :mode mode :members members}))
    (when-let [taken (some #(when-let [other (link-of current (:surface-id %) (:viewport %))]
                              (when (not= link-id (:id other)) (:surface-id %)))
                           members)]
      (reject! :surface-linked "A surface viewport belongs to one link" {:surface-id taken}))
    (let [model (if (get-in current [:links link-id]) (dissolve model link-id) model)
          leader (first members)
          leader-view (resolved model (:surface-id leader) (:viewport leader) false)
          before (member-window leader-view (:surface-id leader))
          ;; The leader's mode and selection carry over, as its window does.
          layout (when (not= layout-defaults (:layout leader-view)) (:layout leader-view))
          linked (update-state model assoc-in [:links link-id]
                               (cond-> {:id link-id :mode mode :members members :window {:x 0 :y 0}}
                                 (= :extend mode) (assoc :direction direction)
                                 layout (assoc :layout layout)))]
      (transition model (keep-window linked link-id (:surface-id leader) (or before {:x 0 :y 0}))
                  :control-surface/linked {:link-id link-id :mode mode :surface-ids (vec ids)}))))

(defn- remove-surface
  "Forget a surface. Links keep their other members where they were; a link
  left with one member dissolves into that surface at its current window."
  [model current surface-id]
  (let [affected (filter (fn [[_ l]] (some #(= surface-id (:surface-id %)) (:members l))) (:links current))
        model (reduce (fn [model [link-id l]]
                        (let [members (filterv #(not= surface-id (:surface-id %)) (:members l))]
                          (if (< (count members) 2)
                            (dissolve model link-id)
                            (let [keep-id (:surface-id (first members))
                                  before (member-window (resolved model link-id nil false) keep-id)]
                              (keep-window (update-state model assoc-in [:links link-id :members] members)
                                           link-id keep-id before)))))
                      model affected)]
    (transition model (update-state model update :surfaces dissoc surface-id)
                :control-surface/removed {:surface-id surface-id})))

(defn- known! [current surface-id]
  (or (get-in current [:surfaces surface-id])
      (reject! :unknown-surface "No surface with that ID" {:surface-id surface-id})))

(defn apply-operation
  "Apply a surface command after show-owner admission.
  :control-surface/attach {:surface-id :descriptor :role :viewports {viewport content}}
    A connected surface can be replaced only by its own connection.
  :control-surface/detach {:surface-id}: marks it unavailable; links stay.
  :control-surface/remove {:surface-id}: forgets it; two-member links dissolve.
    Detach, attach-over and remove of a connected surface are accepted only
    from that device's own connection; others unlink instead.
  :control-surface/link {:link-id :mode (:extend :mirror :follow) :direction
    (:right :left :down :up, extend only) :members [{:surface-id :viewport
    :offset {:x :y}}]}; :viewport defaults to the surface's first viewport and
    :offset (pages, follow only) to 0.
  :control-surface/unlink {:link-id}
  :control-surface/bank {:target link-or-surface-id :viewport?} with :move
    (:previous :next :channel-previous :channel-next :first :last :up :down
    :top :bottom), :to {:x :y}, or :reveal a cell key such as [:cue :ember].
  :control-surface/layout {:target link-or-surface-id :viewport? :set patch}
    merges :encoder-mode (:pan :send :plug-in :user), :page, :selected (a
    cell key or nil) and :flip? into the logical surface's layout state."
  [model {:keys [op surface-id link-id target viewport] :as command}]
  (let [current (state model)]
    (case op
      :control-surface/attach (attach model current command)

      :control-surface/detach
      (do (when (and (:attached? (known! current surface-id)) (not (own-source? command)))
            (reject! :surface-owned "That surface's device is connected; unlink it instead" {:surface-id surface-id}))
          (transition model (update-state model assoc-in [:surfaces surface-id :attached?] false)
                      :control-surface/detached {:surface-id surface-id}))

      :control-surface/remove
      (do (when (:attached? (known! current surface-id))
            (when-not (own-source? command)
              (reject! :surface-owned "That surface's device is connected; disconnect it first" {:surface-id surface-id})))
          (remove-surface model current surface-id))

      :control-surface/link (link model current command)

      :control-surface/unlink
      (if (get-in current [:links link-id])
        (transition model (dissolve model link-id) :control-surface/unlinked {:link-id link-id})
        (reject! :unknown-link "No link with that ID" {:link-id link-id}))

      :control-surface/bank
      (let [view (resolved model target viewport)]
        (when-not view
          (reject! :unknown-surface "No surface viewport or link with that ID" {:target target :viewport viewport}))
        (let [next-window (bank-target view command)
              path (if (:link? view) [:links (:id view) :window]
                       [:surfaces (:id view) :viewports (:viewport view) :window])]
          (transition model (update-state model assoc-in path next-window)
                      :control-surface/banked {:target (:id view) :viewport (:viewport view) :window next-window})))

      :control-surface/layout
      (let [view (logical (state model) target viewport)
            patch (:set command)]
        (when-not view
          (reject! :unknown-surface "No surface viewport or link with that ID" {:target target :viewport viewport}))
        (when-let [error (layout-errors patch)]
          (reject! :invalid-value error {:set patch}))
        (let [path (if (:link? view) [:links (:id view) :layout]
                       [:surfaces (:id view) :viewports (:viewport view) :layout])
              layout (merge (:layout view) patch)
              stored (into {} (remove (fn [[k v]] (= v (get layout-defaults k)))) layout)]
          (transition model (update-state model (fn [current]
                                                  (if (seq stored) (assoc-in current path stored)
                                                      (update-in current (pop path) dissoc :layout))))
                      :control-surface/layout-changed {:target (:id view) :viewport (:viewport view) :layout layout})))

      (reject! :unknown-operation "Unknown surface operation" {:op op}))))

(defn link-command
  "Link payload for surface IDs (or {:surface-id :viewport :offset} maps):
  {:mode :extend|:mirror|:follow :direction :right|:left|:down|:up
   :offset {:x :y} :viewport v}. In :follow mode the nth follower gets n times
  :offset, so {:mode :follow :offset {:x 1}} keeps a member one page right."
  [surfaces {:keys [mode direction offset viewport] :or {mode :extend direction :right}}]
  (cond-> {:mode mode
           :members (vec (map-indexed
                          (fn [index member]
                            (let [member (if (map? member) member {:surface-id member})]
                              (cond-> member
                                (and viewport (not (:viewport member))) (assoc :viewport viewport)
                                (and offset (= :follow mode) (pos? index) (not (:offset member)))
                                (assoc :offset {:x (* index (:x offset 0)) :y (* index (:y offset 0))}))))
                          surfaces))}
    (= :extend mode) (assoc :direction direction)))

(defn detach-all
  "Mark every surface detached: device sessions do not survive a show
  restart, restore or save. Links and windows stay."
  [model]
  (if (seq (:surfaces (state model)))
    (update-state model update :surfaces #(update-vals % (fn [s] (assoc s :attached? false))))
    model))

(defn- saved-surface? [id {:keys [viewports role attached?] :as s}]
  ;; Geometry and viewport names must match the registered descriptor: the
  ;; projection and every device read do arithmetic on them.
  (let [{groups :viewports} (descriptor/descriptor (:descriptor s))]
    (and (map? s) (id? id) (= id (:id s)) groups (keyword? role) (contains? #{nil true false} attached?)
         (map? viewports) (= (set (keys viewports)) (set (keys groups)))
         (every? (fn [[viewport v]]
                   (and (= (get groups viewport) (:geometry v)) (content? (:content v)) (window? (:window v))
                        (nil? (layout-errors (:layout v {})))))
                 viewports))))

(defn- saved-link? [surfaces id {:keys [mode direction members] :as l}]
  (and (map? l) (id? id) (= id (:id l)) (window? (:window l)) (not (contains? surfaces id))
       (nil? (layout-errors (:layout l {})))
       (contains? modes mode) (or (nil? direction) (contains? directions direction))
       (vector? members) (<= 2 (count members) 16) (apply distinct? (map :surface-id members))
       (every? #(and (map? %) (keyword? (:viewport %)) (get-in surfaces [(:surface-id %) :viewports (:viewport %)])
                     (position? (:offset % {:x 0 :y 0})))
               members)
       (axes-compatible? {:surfaces surfaces} members)))

(defn validate-state!
  "Validate saved surfaces and links before restoring them."
  [model]
  (let [{:keys [surfaces links]} (state model)]
    (when-not (and (map? surfaces) (map? links)
                   (every? (fn [[id s]] (saved-surface? id s)) surfaces)
                   (every? (fn [[id l]] (saved-link? surfaces id l)) links)
                   (let [pairs (mapcat (fn [l] (map (juxt :surface-id :viewport) (:members l))) (vals links))]
                     (or (empty? pairs) (apply distinct? pairs))))
      (throw (ex-info "Invalid control surfaces or links" {:path state-path}))))
  model)
