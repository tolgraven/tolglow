(ns tolglow.video.edit-history
  "Undo and redo for video editing (#672, #673, #674), shared by Studio and
  the REPL.

  Video settings edits (sources, strip mappings, strips and surfaces) arrive
  as `:video-mapping` messages. Each accepted edit records the one unit of
  `[:settings :video]` it changed, before and after: a source, a mapping, a
  mapping's strips or a surface. `{:op :undo}` and `{:op :redo}` are
  messages of the same type, so they pass through the same owner queue,
  epoch guard and validation as the edit itself. Restoring a unit
  re-validates the whole video settings; a refused restore leaves both the
  settings and the history as they were.

  Shader cue edits (#673) publish new effect versions. The history keeps,
  per effect, the versions an undo or redo restores the `:shader` block
  from. Undo publishes that older block as the next version, through the
  ordinary definition command, so the registry keeps every version and
  running instances reload as for any save.

  The history lives in the model under `[:extensions state-key]`, so a full
  backup keeps it. Each list holds at most `max-entries`."
  (:require [tolglow.video.mapping :as mapping]))

(def state-key :tolglow.video/edit-history)

(def max-entries 32)

(defn history [model] (get-in model [:extensions state-key] {}))

(defn- bounded [entries] (vec (take-last max-entries entries)))

(defn- wire [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn unit-path
  "The unit of [:settings :video] an edit operation changes."
  [{:keys [op] :as operation}]
  (case op
    (:put-source :delete-source) [:sources (:id operation)]
    :put-mapping [:mappings (get-in operation [:mapping :id])]
    :delete-mapping [:mappings (:id operation)]
    (:put-strip :delete-strip) [:mappings (:mapping-id operation) :strips]
    :put-surface [:surfaces (get-in operation [:surface :id])]
    :delete-surface [:surfaces (:id operation)]
    nil))

(defn label
  "A short description of an edit for the Undo and Redo buttons."
  [{:keys [op] :as operation}]
  (case op
    :put-source (str "source " (wire (:id operation)))
    :delete-source (str "removing source " (wire (:id operation)))
    :put-mapping (str "mapping " (wire (get-in operation [:mapping :id])))
    :delete-mapping (str "removing mapping " (wire (:id operation)))
    :put-strip (str "strip " (wire (get-in operation [:strip :id])))
    :delete-strip (str "removing strip " (wire (:strip-id operation)))
    :put-surface (str "surface " (wire (get-in operation [:surface :id])))
    :delete-surface (str "removing surface " (wire (:id operation)))
    (str (wire op))))

(defn- slot [settings path]
  (let [parent (get-in settings (pop path))]
    (if (and (map? parent) (contains? parent (peek path)))
      {:present? true :value (get parent (peek path))}
      {:present? false})))

(defn- restore [settings path {:keys [present? value]}]
  (if present?
    (assoc-in settings path value)
    (let [parent (pop path)]
      (if (seq parent)
        (update-in settings parent #(when % (dissoc % (peek path))))
        (dissoc settings (peek path))))))

(defn- clean
  "Drop the empty maps a restore leaves behind, as an unedited show has none."
  [settings]
  (reduce (fn [s k] (if (and (contains? s k) (empty? (get s k))) (dissoc s k) s)) settings [:sources :mappings :surfaces]))

(defn apply-edit
  "Apply one `tolglow.video.mapping/apply-operation` edit to the model and
  record it. An edit that changes nothing is not recorded. Throws like
  apply-operation when the result is invalid."
  [model operation]
  (let [before (get-in model [:settings :video])
        after (mapping/apply-operation before operation)
        path (unit-path operation)
        group (:coalesce operation)
        entry (cond-> {:label (label operation) :path path :before (slot before path) :after (slot after path)}
                (string? group) (assoc :coalesce group))]
    (cond-> (assoc-in model [:settings :video] after)
      (and path (not= (:before entry) (:after entry)))
      (update-in [:extensions state-key]
                 (fn [h]
                   (let [undo (or (:undo h) []) top (peek undo)]
                     (-> (or h {})
                         ;; The edits of one knob turn or drag (same :coalesce
                         ;; token, same unit) are one undo step.
                         (assoc :undo (if (and (string? group) (= group (:coalesce top)) (= path (:path top)))
                                        (conj (pop undo) (assoc entry :before (:before top)))
                                        (bounded (conj undo entry))))
                         (assoc :redo []))))))))

(defn step
  "Undo (direction :undo) or redo (:redo) the newest video settings edit.
  Throws ex-info with :code :video/nothing-to-undo or :video/nothing-to-redo
  when the list is empty, or like apply-operation when the restored settings
  are invalid."
  [model direction]
  (let [h (history model)
        [from to side] (if (= :undo direction) [:undo :redo :before] [:redo :undo :after])
        entry (peek (get h from))]
    (when-not entry
      (throw (ex-info (str "Nothing to " (name direction))
                      {:code (keyword "video" (str "nothing-to-" (name direction)))
                       :errors [{:path [:op] :message (str "Nothing to " (name direction))}]})))
    (let [settings (-> (or (get-in model [:settings :video]) {})
                       (restore (:path entry) (get entry side))
                       clean
                       mapping/validate!)]
      (-> model
          (assoc-in [:settings :video] settings)
          (assoc-in [:extensions state-key]
                    (-> h (update from pop) (update to #(bounded (conj (or % []) entry)))))))))

;; Shader cue versions ---------------------------------------------------------

(defn shader-history [model effect-id] (get-in (history model) [:shaders effect-id] {:undo [] :redo []}))

(defn shader-restore
  "The version whose :shader block an undo or redo of effect-id restores,
  or nil when that list is empty."
  [model effect-id direction]
  (peek (get (shader-history model effect-id) direction)))

(defn record-shader
  "Record an accepted shader publication. edit is {:effect-id :op :from}:
  :from is the version that was current before; :op :edit starts a new
  branch, :undo and :redo move one entry from one list to the other."
  [model {:keys [effect-id op from]}]
  (update-in model [:extensions state-key :shaders effect-id]
             (fn [h]
               (let [{:keys [undo redo]} (merge {:undo [] :redo []} h)]
                 (case op
                   :edit {:undo (bounded (conj undo from)) :redo []}
                   :undo {:undo (pop undo) :redo (bounded (conj redo from))}
                   :redo {:undo (bounded (conj undo from)) :redo (pop redo)})))))

(defn check-shader-edit!
  "Refuse a shader edit marker that does not match the model: :from must be
  the effect's current version, and an undo or redo must restore the version
  at the head of its list."
  [model {:keys [effect-id op from restore]}]
  (let [current (get-in model [:registry :effects effect-id :version])]
    (when-not (and (#{:edit :undo :redo} op) (= from current)
                   (or (= :edit op) (= restore (shader-restore model effect-id op))))
      (throw (ex-info "The shader's edit history changed; try again"
                      {:code :shader/history-moved :effect-id effect-id})))))

(defn summary
  "What Studio shows on its Undo and Redo buttons."
  [model]
  (let [h (history model)]
    {:undo (count (:undo h)) :redo (count (:redo h))
     :undo-label (:label (peek (:undo h))) :redo-label (:label (peek (:redo h)))
     :shaders (into {} (for [[id {:keys [undo redo]}] (:shaders h)]
                         [id {:undo (count undo) :redo (count redo)}]))}))
