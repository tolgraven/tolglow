(ns tolglow.surface.cue-grid
  "Running launches on a pad grid: Select and Reorder modes (#545).

  Content: :active-cues is a link content space on the :cue-grid axis. Its
  cells are the running launches in committed render input order (the order
  Studio's Reorder shows, :active-instance-order), filled left to right and
  top to bottom, :columns (8) to a row, one cell per launch, keyed
  [:instance id]. A launch of the same cue twice is two cells. Pages are the
  logical surface's committed window, so a grid pages with
  :control-surface/bank (:up/:down), and two grids can mirror, extend or
  follow each other like any linked viewports.

  Modes are per device member, like a held pad: a Push opens Select or
  Reorder on its own pads while a linked member keeps launching. What is
  shared is committed: the window (page) and the selection.
  - Select: a pad commits :control-surface/layout {:selected [:instance id]}
    on the logical surface, the same command and field the REPL
    (show/surface-layout!) and Studio's Follow selection use. It never
    launches or stops a cue.
  - Reorder: the first pad chooses a launch (its pad blinks); a second pad
    sends :instance/move-after {:instance-id chosen :after-instance-id
    target}, Studio's Place after, with :expected-revision. The same pad
    again cancels. Holding the first pad while pressing the target does the
    same. The chosen launch is kept by identity across pages and competing
    reorders, and dropped once it is no longer running.
  A press acts only on the launch its pad showed in the last feedback: if
  the pad now shows another launch (a competing reorder, a release or a
  relaunch moved it), the press is consumed without a command.

  This namespace knows nothing about MIDI. Devices give it member-local
  viewport positions ([column row], row 0 at the top) and gesture identities,
  and turn its pad intents into their own addresses (tolglow.surface.push-3)."
  (:require [afterglow.control.instance-order :as instance-order]
            [afterglow.control.views :as views]
            [tolglow.surface.link :as link]))

(def default-columns 8)

(def modes #{:select :reorder})

(def actions
  "Device-independent grid actions a device maps its buttons to."
  #{:cue-grid/select :cue-grid/reorder :cue-grid/page-up :cue-grid/page-down})

;; ---------------------------------------------------------------------------
;; Content

(defn running
  "Running launches in committed render input order."
  [model]
  (filterv #(= :running (:phase %)) (instance-order/ordered model)))

(defn- rgb? [c] (and (vector? c) (= 3 (count c)) (every? #(and (number? %) (<= 0 % 1)) c)))

(defn- launch-color
  "The launch's pad colour, as the cue pads show it (afterglow views/pad-view),
  then its definition's colour or swatch."
  [model {:keys [cue-id params definition] :as instance}]
  (let [pad (when cue-id (views/pad-view model cue-id {cue-id [instance]}))]
    (or (some #(when (rgb? %) %) [(:color pad) (:color params) (:color definition)
                                  (views/swatch-rgb (:swatch definition))])
        [0.35 0.35 0.35])))

(def ^:private cells-cache
  "Last [instances registry pad-colours columns] -> cells. A Push refreshes
  every 10 ms; cells change only with committed instances or definitions."
  (atom nil))

(defn- columns-of [content] (:columns content default-columns))

(defn cells
  "Cells of an :active-cues content space for a model."
  [model content]
  (let [columns (columns-of content)
        k [(:instances model) (:registry model) (get model :afterglow.control.views/pad-colors) columns]
        [seen cached] @cells-cache]
    (if (and seen (every? true? (map identical? (pop seen) (pop k))) (= (peek seen) columns))
      cached
      (let [result (vec (map-indexed
                         (fn [index {:keys [id cue-id effect-id definition] :as instance}]
                           {:x (mod index columns) :y (quot index columns) :index index
                            :key [:instance id] :instance-id id :cue-id cue-id
                            :label (or (get-in model [:registry :cues cue-id :name]) (:name definition)
                                       (some-> (or cue-id effect-id) name) "")
                            :color (launch-color model instance)
                            :priority (instance-order/effective-priority instance)})
                         (running model)))]
        (reset! cells-cache [k result])
        result))))

(defmethod link/content-cells :active-cues [model content] (cells model content))

(defmethod link/content-size :active-cues [model content]
  ;; The same extent link/cells-size would compute, without colours or labels.
  (let [n (count (running model)) columns (columns-of content)]
    {:columns (min n columns) :rows (quot (+ n (dec columns)) columns)}))

(defmethod link/content-axis :active-cues [_] :cue-grid)

(defmethod link/content-valid? :active-cues [{:keys [columns]}]
  (or (nil? columns) (and (pos-int? columns) (<= columns 64))))

;; ---------------------------------------------------------------------------
;; What one member shows

(defn member-view
  "What one member viewport of [columns rows] geometry shows:
  {:logical-id :window :limit :count :selected :cells {[column row] cell}}.
  Until its attach commits, a device shows own-content from the first cell."
  [model surface-id viewport [columns rows] own-content]
  (let [projected (link/window model surface-id viewport)
        member (some #(when (= surface-id (:surface-id %)) %) (:members projected))]
    (if (and member (:attached? member))
      {:logical-id (:id projected) :window (:window projected) :limit (:limit projected)
       :count (:count projected) :selected (get-in projected [:layout :selected])
       :cells (into {} (map (fn [{:keys [position] :as cell}] [[(:x position) (:y position)] cell])) (:cells member))}
      (let [all (link/content-cells model own-content)]
        {:logical-id surface-id :window {:x 0 :y 0} :limit {:x 0 :y 0} :count (count all) :selected nil
         :cells (into {} (keep (fn [{:keys [x y] :as cell}]
                                 (when (and (< x columns) (< y rows)) [[x y] (assoc cell :position {:x x :y y})])))
                      all)}))))

;; ---------------------------------------------------------------------------
;; Local state and input

(def initial-state
  "Per-member state: :mode, gesture identities of pads this grid owns until
  release, the chosen :source launch, and :shown, the launch per position in
  the last feedback with its show identity."
  {:mode nil :pads #{} :source nil :shown {} :shown-identity nil})

(defn reset-model
  "After a show restart: keep the mode and the pads still held (their release
  must reach this grid), forget launches of the old show."
  [state]
  (merge initial-state (select-keys state [:mode :pads])))

(defn open? [state] (some? (:mode state)))

(defn toggle-mode
  "Open mode, or close the grid when mode is already open. Changing mode
  drops a chosen launch."
  [state mode]
  (when-not (contains? modes mode)
    (throw (ex-info "Unknown cue grid mode" {:mode mode})))
  (assoc state :mode (when-not (= mode (:mode state)) mode) :source nil))

(defn- show-identity [model] [(:show-id model) (:epoch model)])

(defn- running-ids [model]
  (into #{} (comp (filter #(= :running (:phase (val %)))) (map key)) (:instances model)))

(defn bank-command
  "Page the logical surface that holds this member."
  [surface-id viewport move]
  {:op :control-surface/bank :target surface-id :viewport viewport :move move})

(defn action
  "A grid button action on press. Paging only while the grid is open."
  [state surface-id viewport action-id]
  (case action-id
    :cue-grid/select {:state (toggle-mode state :select) :commands []}
    :cue-grid/reorder {:state (toggle-mode state :reorder) :commands []}
    :cue-grid/page-up {:state state :commands (if (open? state) [(bank-command surface-id viewport :up)] [])}
    :cue-grid/page-down {:state state :commands (if (open? state) [(bank-command surface-id viewport :down)] [])}
    (throw (ex-info "Unknown cue grid action" {:action action-id}))))

(defn press
  "A pad down at a member-local position with a gesture identity.
  Returns {:state :commands :handled?}; :handled? false leaves the press to
  the device's normal pad mode. Commands are owner command templates the
  device fences with its show identity."
  [state model view surface-id viewport {:keys [at identity]}]
  (cond
    (contains? (:pads state) identity) {:state state :commands [] :handled? true}
    (not (open? state)) {:state state :commands [] :handled? false}
    :else
    (let [state (update state :pads conj identity)
          consumed {:state state :commands [] :handled? true}
          cell (get-in view [:cells at])
          id (:instance-id cell)]
      (if (or (nil? id) (not= id (get-in state [:shown at]))
              (not= (show-identity model) (:shown-identity state)))
        consumed
        (case (:mode state)
          :select
          (if (= (:key cell) (:selected view))
            consumed
            (assoc consumed :commands [{:op :control-surface/layout :target surface-id :viewport viewport
                                        :set {:selected (:key cell)}}]))

          :reorder
          (let [source (:source state)]
            (cond
              (= source id) (assoc-in consumed [:state :source] nil)
              (not (contains? (running-ids model) source)) (assoc-in consumed [:state :source] id)
              :else (assoc consumed
                           :state (assoc state :source nil)
                           :commands [{:op :instance/move-after :instance-id source :after-instance-id id
                                       :expected-revision (:revision model)}]))))))))

(defn release
  "A pad up. :handled? true when this grid owned the press."
  [state identity]
  {:state (update state :pads disj identity) :commands [] :handled? (contains? (:pads state) identity)})

(defn owns? [state identity] (contains? (:pads state) identity))

;; ---------------------------------------------------------------------------
;; Feedback

(defn project
  "Pad intents for one member and the state that records what they show.
  Intents: {[column row] {:color [r g b] :instance-id :selected? :source?}}
  for every occupied position; empty positions are absent. A closed grid
  shows nothing and remembers nothing."
  [state model view]
  (if-not (open? state)
    {:state (assoc state :shown {} :shown-identity nil) :pads {}}
    (let [running (running-ids model)
          source (when (contains? running (:source state)) (:source state))
          pads (into {} (map (fn [[at {:keys [instance-id color key]}]]
                               [at {:color color :instance-id instance-id
                                    :selected? (= key (:selected view))
                                    :source? (= instance-id source)}]))
                     (:cells view))]
      {:state (assoc state :source source :shown-identity (show-identity model)
                     :shown (into {} (map (fn [[at {:keys [instance-id]}]] [at instance-id])) pads))
       :pads pads})))

(defn paging
  "Whether the window can move up or down from here."
  [view]
  {:up? (pos? (get-in view [:window :y] 0))
   :down? (< (get-in view [:window :y] 0) (get-in view [:limit :y] 0))})
