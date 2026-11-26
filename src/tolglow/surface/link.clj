(ns tolglow.surface.link
  "Attached control surfaces, links and committed viewports (#705, #706 slice 1).

  A surface is one attached controller or Studio component, described by a
  tolglow.surface.descriptor. Each of its viewports (for example the X-Touch
  :strips, a Push :grid) has a fixed geometry and looks through a window onto
  a content space, such as the mixer strip list. A link joins the same
  viewport of two or more surfaces into one logical surface: each member sits
  at :at {:x :y}, measured in its own viewport size (banks or pages). Equal
  positions mirror one window; adjacent positions extend it in any direction.
  The window (the session box) belongs to the logical surface, so a bank move
  from any member, Studio or the REPL moves every member.

  Surfaces, links and windows are show data. They change only through owner
  commands and are read from the snapshot projection. A detached surface keeps
  its entry, so its links and windows survive reattaching under the same ID,
  and a saved show keeps links and windows with every surface detached.
  Device ports, sessions and per-member gestures stay with the transport."
  (:require [tolglow.effects.group-channels :as group-channels]
            [tolglow.surface.descriptor :as descriptor]))

(def ^:private state-path [:extensions ::state])

(def ^:private settings-path [:settings :controllers :surfaces])

(def operations
  #{:control-surface/attach :control-surface/detach :control-surface/remove
    :control-surface/link :control-surface/unlink :control-surface/bank})

(def content-types
  "Content spaces a viewport can show. Linked members share one type."
  #{:mixer-strips :none})

(defn- reject! [code message details]
  (throw (ex-info message {:control/error code :details details})))

(defn- id? [x] (or (keyword? x) (and (string? x) (seq x))))

(defn- state [model] (get-in model state-path {:surfaces {} :links {}}))

;; Content spaces

(defn group-strips
  "Group strips for an :effect-group-view (:manual or :overlay), in bank order.
  Manual groups list children after parents."
  [view model]
  (let [{:keys [manual] :as mixer} (group-channels/projection model)
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

(defn- content-cells
  "Every cell of a content space, with a stable :key, :label and [:x :y]."
  [model {:keys [type] :as content}]
  (case type
    :mixer-strips
    (vec (map-indexed
          (fn [index {:keys [group-id cue-id label]}]
            (cond-> {:x index :y 0 :label (or label (when cue-id (get-in model [:registry :cues cue-id :name]))
                                                   (some-> cue-id name) "")}
              group-id (assoc :key [:group group-id] :group-id group-id)
              cue-id (assoc :key [:cue cue-id] :cue-id cue-id :instance-id (latest-instance-id model cue-id))))
          (mixer-channels model content)))
    []))

(defn- content-size [cells]
  {:columns (if (seq cells) (inc (apply max (map :x cells))) 0)
   :rows (if (seq cells) (inc (apply max (map :y cells))) 0)})

;; Logical surfaces

(defn- default-viewport [state surface-id]
  (first (sort (keys (get-in state [:surfaces surface-id :viewports])))))

(defn- link-of [{:keys [links]} surface-id viewport]
  (some (fn [[_ link]]
          (when (and (= viewport (:viewport link)) (some #(= surface-id (:surface-id %)) (:members link))) link))
        links))

(defn- viewport-size [state surface-id viewport]
  (let [[columns rows] (get-in state [:surfaces surface-id :viewports viewport :geometry])]
    {:columns columns :rows rows}))

(defn- logical
  "The logical surface for a link ID, or for a surface's viewport (its link, or
  the surface alone). Member offsets are in cells, normalised so the top-left
  member starts at 0. The anchor (first member) supplies the content."
  [state target viewport]
  (let [link (or (get-in state [:links target])
                 (let [viewport (or viewport (default-viewport state target))]
                   (link-of state target viewport)))
        viewport (if link (:viewport link) (or viewport (default-viewport state target)))
        members (if link (:members link)
                    (when (get-in state [:surfaces target :viewports viewport])
                      [{:surface-id target :at {:x 0 :y 0}}]))]
    (when (seq members)
      (let [placed (mapv (fn [{:keys [surface-id at]}]
                           (let [{:keys [columns rows] :as size} (viewport-size state surface-id viewport)]
                             {:surface-id surface-id :at at :shape size
                              :attached? (boolean (get-in state [:surfaces surface-id :attached?]))
                              :x (* columns (:x at)) :y (* rows (:y at))}))
                         members)
            min-x (apply min (map :x placed)) min-y (apply min (map :y placed))
            anchor (:surface-id (first members))]
        {:id (if link (:id link) target) :link? (boolean link) :viewport viewport
         :members (mapv #(-> % (update :x - min-x) (update :y - min-y)) placed)
         :anchor-id anchor
         :content (get-in state [:surfaces anchor :viewports viewport :content])
         :window (if link (:window link) (get-in state [:surfaces target :viewports viewport :window]))}))))

(defn- span
  "Cells covered by the attached members (all members when none is attached)."
  [{:keys [members]}]
  (let [members (or (seq (filter :attached? members)) members)
        extent (fn [axis size] (- (apply max (map #(+ (axis %) (size (:shape %))) members))
                                  (apply min (map axis members))))]
    {:columns (extent :x :columns) :rows (extent :y :rows)}))

(defn- limit [cells span-size page]
  (if (or (<= cells span-size) (zero? page)) 0 (* page (quot (dec cells) page))))

(defn- limits
  "Last window: everything that fits starts at 0, otherwise pages stay on
  multiples of the anchor's viewport, so a remainder starts on the anchor."
  [{:keys [members] :as view} size]
  (let [{:keys [columns rows]} (span view) shape (:shape (first members))]
    {:x (limit (:columns size) columns (:columns shape)) :y (limit (:rows size) rows (:rows shape))}))

(defn- clamp-window [view size window]
  (let [{:keys [x y]} (limits view size)]
    {:x (max 0 (min x (:x window 0))) :y (max 0 (min y (:y window 0)))}))

(defn- resolved
  "Logical surface with its content cells, size and clamped window."
  [model target viewport]
  (when-let [view (logical (state model) target viewport)]
    (let [cells (content-cells model (:content view))
          size (content-size cells)]
      (assoc view :cells cells :size size :window (clamp-window view size (:window view))))))

(defn- member-window [{:keys [window members]} surface-id]
  (when-let [{:keys [x y]} (some #(when (= surface-id (:surface-id %)) %) members)]
    {:x (+ (:x window) x) :y (+ (:y window) y)}))

(defn logical-position
  "Remap a member-local [x y] in one viewport to the logical surface's content
  coordinates. Gesture identity stays with the member; only coordinates move."
  [model surface-id viewport [x y]]
  (when-let [view (resolved model surface-id viewport)]
    (let [origin (member-window view surface-id)]
      {:logical-id (:id view) :at [(+ x (:x origin)) (+ y (:y origin))]})))

;; Device reads (X-Touch profile options)

(defn- attached? [model surface-id]
  (get-in (state model) [:surfaces surface-id :attached?]))

(defn strip-channels
  "Channels an X-Touch surface banks over: its :strips logical surface's mixer
  strips (the link anchor's, when linked). Cue channels keep their bindings.
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
    (or (some-> (resolved model surface-id :strips) (member-window surface-id) :x) 0)))

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

(defn- mode [members]
  (if (apply = (map :at members)) :mirror :extend))

(defn window
  "Project one logical surface (a link, or one surface's viewport): window,
  span, content size, last window, members and the cells each attached
  member shows, with member-local :position."
  [model target viewport]
  (when-let [{:keys [link? members cells] :as view} (resolved model target viewport)]
    {:id (:id view) :link? link? :viewport (:viewport view) :mode (if link? (mode members) :single)
     :window (:window view) :span (span view) :size (:size view) :limit (limits view (:size view))
     :members (mapv (fn [{:keys [surface-id attached? shape] :as member}]
                      (let [{:keys [x y] :as origin} (member-window view surface-id)]
                        (assoc (select-keys member [:surface-id :at :attached?])
                               :window origin
                               :cells (if-not attached? []
                                        (into [] (keep (fn [cell]
                                                         (let [column (- (:x cell) x) row (- (:y cell) y)]
                                                           (when (and (< -1 column (:columns shape)) (< -1 row (:rows shape)))
                                                             (assoc cell :position {:x column :y row})))))
                                              cells)))))
                    members)}))

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
  (and (map? content) (contains? content-types (:type content))
       (contains? #{nil :manual :overlay} (:effect-group-view content))
       (vector? (:cue-strips content []))
       (every? #(and (map? %) (id? (:cue-id %))) (:cue-strips content []))))

(defn- update-state [model f & args]
  (assoc-in model state-path (apply f (state model) args)))

(defn- dissolve
  "Remove a link, leaving each remaining member at the window it showed."
  [model link-id]
  (let [view (resolved model link-id nil)
        model (update-state model update :links dissoc link-id)]
    (if-not view
      model
      (update-state model
                    (fn [current]
                      (reduce (fn [current {:keys [surface-id]}]
                                (if (get-in current [:surfaces surface-id :viewports (:viewport view)])
                                  (assoc-in current [:surfaces surface-id :viewports (:viewport view) :window]
                                            (member-window view surface-id))
                                  current))
                              current (:members view)))))))

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
                    (let [{:keys [x y]} (member-window view surface-id)]
                      (and attached? (<= x (:x cell) (+ x (:columns shape) -1))
                           (<= y (:y cell) (+ y (:rows shape) -1)))))
                  (:members view))
          window
          (clamp {:x (* columns (quot (:x cell) columns)) :y (* rows (quot (:y cell) rows))})))

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

(defn- attach [model current {:keys [surface-id role viewports] descriptor-id :descriptor}]
  (let [{groups :viewports :as described} (descriptor/descriptor descriptor-id)]
    (when-not (and (id? surface-id) described (keyword? role) (map? viewports)
                   (= (set (keys viewports)) (set (keys groups)))
                   (every? content? (vals viewports))
                   (not (contains? (:links current) surface-id)))
      (reject! :invalid-surface "Attach a surface ID, a known descriptor, a role and content for each viewport"
               {:surface-id surface-id :descriptor descriptor-id}))
    (let [previous (get-in current [:surfaces surface-id])]
      (transition model
                  (update-state model assoc-in [:surfaces surface-id]
                                {:id surface-id :descriptor descriptor-id :role role :attached? true
                                 :viewports (into {} (map (fn [[viewport content]]
                                                            [viewport {:geometry (get groups viewport) :content content
                                                                       :window (get-in previous [:viewports viewport :window] {:x 0 :y 0})}]))
                                                  viewports)})
                  :control-surface/attached {:surface-id surface-id}))))

(defn- link [model current {:keys [link-id viewport members]}]
  (let [ids (map :surface-id members)
        surfaces (map #(get-in current [:surfaces %]) ids)
        viewport (or viewport (default-viewport current (first ids)))
        content-type #(get-in % [:viewports viewport :content :type])]
    (when-not (and (id? link-id) (not (contains? (:surfaces current) link-id))
                   (vector? members) (<= 2 (count members) 16) (apply distinct? ids)
                   (every? some? surfaces) (every? #(get-in % [:viewports viewport]) surfaces)
                   (every? #(position? (:at %)) members) (apply = (map content-type surfaces)))
      (reject! :invalid-link "Link the same viewport of two or more attached surfaces with one content type"
               {:link-id link-id :viewport viewport :members (vec ids)}))
    (when-let [taken (some #(when-let [other (link-of current % viewport)]
                              (when (not= link-id (:id other)) %)) ids)]
      (reject! :surface-linked "A surface viewport belongs to one link" {:surface-id taken :viewport viewport}))
    (let [model (if (get-in current [:links link-id]) (dissolve model link-id) model)
          window (or (member-window (resolved model (first ids) viewport) (first ids)) {:x 0 :y 0})]
      (transition model (update-state model assoc-in [:links link-id]
                                      {:id link-id :viewport viewport
                                       :members (mapv #(select-keys % [:surface-id :at]) members) :window window})
                  :control-surface/linked {:link-id link-id :viewport viewport :surface-ids (vec ids)}))))

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
                                  before (member-window (resolved model link-id nil) keep-id)
                                  model (update-state model assoc-in [:links link-id :members] members)
                                  offset (some #(when (= keep-id (:surface-id %)) %)
                                               (:members (logical (state model) link-id nil)))]
                              (update-state model assoc-in [:links link-id :window]
                                            {:x (max 0 (- (:x before) (:x offset)))
                                             :y (max 0 (- (:y before) (:y offset)))})))))
                      model affected)]
    (transition model (update-state model update :surfaces dissoc surface-id)
                :control-surface/removed {:surface-id surface-id})))

(defn apply-operation
  "Apply a surface command after show-owner admission.
  :control-surface/attach {:surface-id :descriptor :role :viewports {viewport content}}
  :control-surface/detach {:surface-id}: marks it unavailable; links stay.
  :control-surface/remove {:surface-id}: forgets it; two-member links dissolve.
  :control-surface/link {:link-id :viewport :members [{:surface-id :at {:x :y}}]}
  :control-surface/unlink {:link-id}
  :control-surface/bank {:target link-or-surface-id :viewport?} with :move
  (:previous :next :channel-previous :channel-next :first :last :up :down
  :top :bottom), :to {:x :y}, or :reveal a cell key such as [:cue :ember]."
  [model {:keys [op surface-id link-id target viewport] :as command}]
  (let [current (state model)]
    (case op
      :control-surface/attach (attach model current command)

      :control-surface/detach
      (transition model (if (get-in current [:surfaces surface-id])
                          (update-state model assoc-in [:surfaces surface-id :attached?] false)
                          model)
                  :control-surface/detached {:surface-id surface-id})

      :control-surface/remove (remove-surface model current surface-id)

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

      (reject! :unknown-operation "Unknown surface operation" {:op op}))))

(defn link-members
  "Member placements: :mirror puts every surface at the same window; :extend
  places each next surface one viewport further :right, :left, :down or :up
  (default :right). :offset shifts every member after the first by whole
  viewports, for example {:mode :mirror :offset {:x 1 :y 0}} to follow one
  page to the right."
  [surface-ids {:keys [mode direction offset] :or {mode :extend direction :right offset {:x 0 :y 0}}}]
  (let [[dx dy] (case direction :right [1 0] :left [-1 0] :down [0 1] :up [0 -1]
                  (throw (ex-info "Direction must be :right, :left, :down or :up" {:direction direction})))]
    (vec (map-indexed (fn [index id]
                        (let [shift (if (zero? index) {:x 0 :y 0} offset)]
                          {:surface-id id
                           :at (case mode
                                 :mirror shift
                                 :extend {:x (+ (* index dx) (:x shift 0)) :y (+ (* index dy) (:y shift 0))}
                                 (throw (ex-info "Mode must be :mirror or :extend" {:mode mode})))}))
                      surface-ids))))

(defn detach-all
  "Mark every surface detached: device sessions do not survive a show
  restart, restore or save. Links and windows stay."
  [model]
  (if (seq (:surfaces (state model)))
    (update-state model update :surfaces #(update-vals % (fn [s] (assoc s :attached? false))))
    model))

(defn validate-state!
  "Validate saved surfaces and links before restoring them."
  [model]
  (let [{:keys [surfaces links]} (state model)]
    (when-not (and (map? surfaces) (map? links)
                   (every? (fn [[id s]]
                             (and (= id (:id s)) (descriptor/descriptor (:descriptor s))
                                  (every? (fn [[_ v]] (and (content? (:content v)) (window? (:window v))))
                                          (:viewports s))))
                           surfaces)
                   (every? (fn [[id l]]
                             (and (= id (:id l)) (window? (:window l)) (not (contains? surfaces id))
                                  (every? #(and (get-in surfaces [(:surface-id %) :viewports (:viewport l)])
                                                (position? (:at %)))
                                          (:members l))))
                           links)
                   (every? (fn [[_ ids]] (apply distinct? ids))
                           (group-by first (mapcat (fn [l] (map #(vector (:viewport l) (:surface-id %)) (:members l)))
                                                   (vals links)))))
      (throw (ex-info "Invalid control surfaces or links" {:path state-path}))))
  model)
