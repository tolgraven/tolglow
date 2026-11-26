(ns tolglow.show.position-hold
  "Hold a moving head at its last committed pose when nothing writes it.

  Afterglow packs an unwritten pan or tilt channel at 0. When the last effect
  positioning a head ends, that would command a full-travel move on the next
  frame. The frame owner instead adds one block at the end of the contribution
  stream that writes the previous frame's normalized pan/tilt for each axis no
  effect writes this frame. An axis any effect writes is not held, so that
  effect composes against the packed default as before. A head that was never
  moved packs 0 either way, so this block exists only after a positioning
  effect has released.")

(def ^:private writes
  "Attributes that set each position axis. Geometry solves both."
  {:pan #{:pan :aim :direction} :tilt #{:tilt :aim :direction}})

(def ^:private position-attributes #{:pan :tilt :aim :direction})

(defn- zero-opacity? [x] (and (number? x) (zero? x)))

(def ^:private head-cache
  "The last patch seen and the indices of its position heads. A compiled
  patch is immutable, so an identical patch has the same heads."
  (atom nil))

(defn- head-indices [patch]
  (let [[cached indices] @head-cache]
    (if (identical? cached patch)
      indices
      (let [indices (into [] (keep-indexed (fn [n {:keys [channels]}] (when (or (:pan channels) (:tilt channels)) n)))
                          (:fixtures patch))]
        (reset! head-cache [patch indices])
        indices))))

(defn- previous-pose
  "Return id -> {axis value} for each patched head whose last committed pan or
  tilt is above its packed default of 0. Rich values are read first, then
  compact fields. Edge-tracking followers are skipped."
  [patch {:keys [values compact]} followers]
  (let [fixtures (:fixtures patch)
        order (:target-order compact)
        index (when (and compact (not (identical? order (:target-order patch))) (not= order (:target-order patch)))
                (delay (zipmap order (range))))
        committed (fn [row i axis]
                    (let [v (or (when row (get row axis (get-in row [:channels axis])))
                                (when i
                                  (let [{:keys [values valid]} (get-in compact [:fields axis])]
                                    (when (and values (aget ^booleans valid (int i)))
                                      (aget ^doubles values (int i))))))]
                      (when (and (number? v) (< 0.0 (double v)) (<= (double v) 1.0)) (double v))))]
    (reduce (fn [pose n]
              (let [{:keys [id channels]} (nth fixtures n)]
                (if (contains? followers id)
                  pose
                  (let [row (get values id)
                        i (when compact (if index (get @index id) n))
                        held (cond-> {}
                               (:pan channels) (as-> m (if-let [v (committed row i :pan)] (assoc m :pan v) m))
                               (:tilt channels) (as-> m (if-let [v (committed row i :tilt)] (assoc m :tilt v) m)))]
                    (if (seq held) (assoc pose id held) pose)))))
            {} (head-indices patch))))

(defn- block-targets [{:keys [assignments batch native-layer]}]
  (concat (:target-ids batch) (map :target-id assignments) (:target-ids native-layer)))

(defn- batch-attributes
  "Attributes a batch may write, or nil when they cannot be determined here."
  [{:keys [outputs field-request columns]}]
  (let [known (concat (keys outputs) (map :attribute columns)
                      (map :attribute (get-in field-request [:group :cpu-plan :outputs])))]
    (when (or (seq known) (nil? field-request)) (set known))))

(defn- remove-written!
  "Drop every [id axis] this block may write from the open candidate set.
  Unknown shapes count as writing, so a hold is never placed under a writer."
  [open block]
  (let [{:keys [assignments batch native-layer]} block
        weight (* (:opacity native-layer 1) (:member-opacity native-layer 1))
        clear (fn [open id attributes]
                (if-let [axes (get open id)]
                  (let [left (reduce-kv (fn [axes axis _]
                                          (if (some (writes axis) attributes) (dissoc axes axis) axes))
                                        axes axes)]
                    (if (seq left) (assoc open id left) (dissoc open id)))
                  open))]
    (if (zero-opacity? weight)
      open
      (as-> open open
        ;; Pixel blocks can hold thousands of rows; test the attribute first.
        (reduce (fn [open assignment]
                  (let [attribute (:attribute assignment)]
                    (case attribute
                      (:pan :tilt :aim :direction)
                      (if (zero-opacity? (:opacity assignment)) open
                          (clear open (:target-id assignment) #{attribute}))
                      open)))
                open assignments)
        (if (and batch (not (zero-opacity? (:opacity batch))))
          (let [attributes (or (batch-attributes batch) position-attributes)]
            (if (some position-attributes attributes)
              (reduce #(clear %1 %2 attributes) open (:target-ids batch))
              open))
          open)
        (let [changed (into #{} (keep :attribute) (:transforms native-layer))]
          (cond
            (not (some position-attributes changed)) open
            ;; A modifier over earlier members can reach any head.
            (= :previous (:targets native-layer)) (reduce-kv (fn [open id _] (clear open id changed)) open open)
            :else (reduce #(clear %1 %2 changed) open (block-targets block))))))))

(defn with-hold
  "Append a hold block to the ordered contribution stream when a head moved
  away from its packed default and no block writes that axis this frame.
  `previous` is the last committed frame output. `followers` are edge-tracking
  followers, which the renderer positions itself. Returns `blocks` unchanged
  when there is nothing to hold.

  No other block writes a held axis, so the block's place in the stream does
  not change its result. It goes last so that no native layer reads it: a
  standalone modifier over `:targets :previous` takes every head in the
  backdrop below it, and a hold placed there would add released heads."
  [patch previous blocks followers]
  (let [pose (when previous (previous-pose patch previous followers))]
    (if (empty? pose)
      blocks
      (let [open (reduce (fn [open block] (if (empty? open) (reduced open) (remove-written! open block)))
                         pose blocks)]
        (if (empty? open)
          blocks
          (conj (vec blocks)
                {:kind :assignments
                 :assignments (into [] (for [{:keys [id]} (:fixtures patch)
                                             :let [axes (get open id)]
                                             :when axes
                                             axis [:pan :tilt]
                                             :let [value (get axes axis)]
                                             :when value]
                                         {:target-id id :attribute axis :value value}))}))))))
