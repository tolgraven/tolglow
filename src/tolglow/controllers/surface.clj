(ns tolglow.controllers.surface
  "One linked X-Touch surface (main unit plus extenders) and its session box:
  the window of strips the hardware shows. The window is committed show state,
  so the device, Studio and the REPL move it with the same command and read the
  same projection. Device ports and sessions stay in tolglow.show (#705)."
  (:require [afterglow.control.devices.x-touch :as x-touch]
            [tolglow.effects.group-channels :as group-channels]))

(def ^:private state-path [:extensions ::surfaces])

(def ^:private settings-path [:settings :controllers :x-touch])

(def operations
  #{:control-surface/attach :control-surface/detach :control-surface/units :control-surface/bank})

(def ^:private strip-width 8)

(defn- reject! [code message details]
  (throw (ex-info message {:control/error code :details details})))

(defn- id? [x] (or (keyword? x) (and (string? x) (seq x))))

(defn group-strips
  "Group strips for an :effect-group-view (:manual or :overlay), in the order
  the X-Touch profile banks them. Manual groups list children after parents."
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

(defn settings
  "The surface settings home, [:settings :controllers :x-touch], with defaults."
  [model]
  (merge {:follow-selection? false} (get-in model settings-path)))

(defn validate-settings!
  "Validate the :controllers settings scope. Only :x-touch is checked here."
  [controllers]
  (when-let [x-touch (:x-touch controllers)]
    (when-not (and (map? x-touch) (every? #{:follow-selection?} (keys x-touch))
                   (every? boolean? (vals x-touch)))
      (throw (ex-info "X-Touch settings accept only a boolean :follow-selection?" {:settings x-touch}))))
  controllers)

(defn- latest-instance-id [model cue-id]
  (:id (last (sort-by :order (filter #(= cue-id (:cue-id %)) (vals (:instances model)))))))

(defn strips
  "Every strip of a surface in bank order: group strips, then configured cues."
  [surface model]
  (into (mapv (fn [{:keys [group-id label]}] {:key [:group group-id] :group-id group-id :label label})
              (when-let [view (:effect-group-view surface)] (group-strips view model)))
        (map (fn [{:keys [cue-id label]}]
               {:key [:cue cue-id] :cue-id cue-id
                :label (or label (get-in model [:registry :cues cue-id :name]) (name cue-id))}))
        (:cue-strips surface)))

(defn- width [surface]
  (* strip-width (count (filter :connected? (:units surface)))))

(defn- clamp [surface strip-count offset]
  (max 0 (min (x-touch/bank-limit strip-count (max strip-width (width surface))) offset)))

(defn surface [model surface-id] (get-in model (conj state-path surface-id)))

(defn offset
  "The committed first strip, clamped to the current strips. The X-Touch
  profile reads this as its host-owned bank."
  [model surface-id]
  (if-let [entry (surface model surface-id)]
    (clamp entry (count (strips entry model)) (:offset entry 0))
    0))

(defn window
  "Session box projection: offset, width, units and the strips each connected
  unit shows. Cue strips name the instance their fader addresses."
  [model {:keys [id kind units] :as entry}]
  (let [all (strips entry model)
        start (clamp entry (count all) (:offset entry 0))
        visible (vec (for [{:keys [session-id connected?] unit-offset :offset} units
                           :when connected?
                           position (range strip-width)
                           :let [index (+ start unit-offset position)]
                           :when (< index (count all))]
                       (cond-> (assoc (nth all index) :index index :position (+ unit-offset position)
                                      :session-id session-id)
                         (:cue-id (nth all index))
                         (assoc :instance-id (latest-instance-id model (:cue-id (nth all index)))))))]
    {:id id :kind kind :offset start :width (width entry) :strip-count (count all)
     :limit (x-touch/bank-limit (count all) (max strip-width (width entry)))
     :units (mapv #(select-keys % [:session-id :extender? :offset :connected?]) units)
     :strips visible}))

(defn projection
  "Every attached surface's window plus the settings home, for the snapshot."
  [model]
  {:settings (settings model)
   :surfaces (mapv #(window model %) (sort-by (comp str :id) (vals (get-in model state-path))))})

(defn- transition [before after event result]
  {:model after :result result
   :events (if (= before after) [] [(assoc result :type event)])})

(defn- existing! [model surface-id]
  (or (surface model surface-id)
      (reject! :unknown-surface "No connected surface with that ID" {:surface-id surface-id})))

(defn- valid-units? [units]
  (and (vector? units) (<= 1 (count units) 16)
       (= (count units) (count (set (map :session-id units))))
       (every? #(and (map? %) (id? (:session-id %)) (boolean? (:extender? %))
                     (integer? (:offset %)) (<= 0 (:offset %))) units)))

(defn- valid-cue-strips? [strips]
  (and (vector? strips)
       (every? #(and (map? %) (id? (:cue-id %)) (or (nil? (:label %)) (string? (:label %)))) strips)))

(defn- move-target [model entry {:keys [move reveal] :as command}]
  (let [all (strips entry model)
        current (clamp entry (count all) (:offset entry 0))
        step (max strip-width (width entry))]
    (cond
      (contains? command :offset)
      (let [requested (:offset command)]
        (when-not (and (integer? requested) (<= 0 requested))
          (reject! :invalid-value "Offset must be a nonnegative strip index" {:offset requested}))
        (clamp entry (count all) requested))

      (contains? command :reveal)
      (let [index (first (keep-indexed #(when (= reveal (:key %2)) %1) all))]
        (when-not index
          (reject! :unknown-strip "That strip is not on this surface" {:reveal reveal}))
        (if (<= current index (dec (+ current step)))
          current
          (clamp entry (count all) (* step (quot index step)))))

      :else
      (do (when-not (#{:previous :next :channel-previous :channel-next :first :last} move)
            (reject! :invalid-value "Unknown bank move" {:move move}))
          (x-touch/move-bank (count all) step current move)))))

(defn apply-operation
  "Apply a surface command after show-owner admission. :control-surface/bank
  takes one of :move (:previous :next :channel-previous :channel-next :first
  :last), :offset (a strip index) or :reveal (a strip key such as
  [:group [:manual :mix]] or [:cue :ember]); reveal moves only when the strip
  is outside the window, to the page that holds it."
  [model {:keys [op surface-id] :as command}]
  (when-not (id? surface-id)
    (reject! :invalid-value "Surface commands need a surface ID" {:surface-id surface-id}))
  (case op
    :control-surface/attach
    (let [{:keys [kind units cue-strips effect-group-view]} command]
      (when-not (and (= :x-touch kind) (valid-units? units) (valid-cue-strips? (or cue-strips []))
                     (contains? #{nil :manual :overlay} effect-group-view))
        (reject! :invalid-surface "Attach needs X-Touch units, cue strips and an optional group view" {}))
      (let [previous (surface model surface-id)
            entry {:id surface-id :kind kind :effect-group-view effect-group-view
                   :cue-strips (vec (or cue-strips []))
                   :units (mapv #(assoc (select-keys % [:session-id :extender? :offset]) :connected? true) units)
                   :offset (:offset previous 0)}]
        (transition model (assoc-in model (conj state-path surface-id) entry)
                    :control-surface/attached {:surface-id surface-id})))

    :control-surface/detach
    (transition model (if (surface model surface-id) (update-in model state-path dissoc surface-id) model)
                :control-surface/detached {:surface-id surface-id})

    :control-surface/units
    (let [entry (existing! model surface-id)
          connected (:connected command)
          known (set (map :session-id (:units entry)))]
      (when-not (and (map? connected) (seq connected) (every? known (keys connected))
                     (every? boolean? (vals connected)))
        (reject! :invalid-value "Mark known units connected or disconnected" {:connected connected}))
      (let [updated (update entry :units (fn [units]
                                           (mapv #(if (contains? connected (:session-id %))
                                                    (assoc % :connected? (get connected (:session-id %))) %)
                                                 units)))]
        (transition model (assoc-in model (conj state-path surface-id) updated)
                    :control-surface/units-changed {:surface-id surface-id :connected connected})))

    :control-surface/bank
    (let [entry (existing! model surface-id)
          target (move-target model entry command)]
      (transition model (assoc-in model (conj state-path surface-id :offset) target)
                  :control-surface/banked {:surface-id surface-id :offset target}))

    (reject! :unknown-operation "Unknown surface operation" {:op op})))

(defn clear
  "Drop every attached surface. Device sessions do not survive a show restart
  or restore, and a saved show never carries a live surface."
  [model]
  (if (get-in model state-path)
    (update model :extensions dissoc ::surfaces)
    model))
