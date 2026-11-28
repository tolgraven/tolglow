(ns tolglow.video.edit-history
  "Undo and redo for video editing (#672, #673, #674), shared by Studio and
  the REPL.

  Video settings edits (sources, strip mappings, strips and surfaces) arrive
  as `:video-mapping` messages, the only way to write `[:settings :video]`
  (the generic `:settings` message refuses that scope). Each accepted edit
  records the one unit of the settings it changed, before and after: a
  source, a mapping, a mapping's strips or a surface. Every entry has an
  id. `{:op :undo}` and `{:op :redo}` are messages of the same type, so they
  pass through the same owner queue, epoch guard and validation as the edit
  itself, and they are refused rather than applied when they would revert
  something the person did not see:
  - `:expected-entry`, when given (Studio always sends the id its button
    showed), must be the id at the head of that list, else
    `:video/history-moved`;
  - the unit must still hold the value the entry left (its `:after` for an
    undo, its `:before` for a redo), else `:video/history-conflict`.
  A refused or invalid restore leaves both the settings and the history as
  they were.

  Shader cue edits (#673) publish new effect versions. Per effect, each
  entry is {:restore v :to w}: the version whose `:shader` block and params
  the step brings back, and the version the step produced. An undo or redo
  is refused unless the effect is still at the head entry's `:to`, and the
  owner checks that the published block is that version's. A publication
  of the effect without a history step (a plain `:definition`) drops that
  effect's history, since it can no longer be undone safely.

  The history lives in the model under `[:extensions state-key]`, so a full
  backup keeps it and `valid-saved-state?` checks it on load. A sharing
  export must leave it out: it keeps earlier values, such as a removed
  source's file path. Each list holds at most `max-entries`."
  (:require [tolglow.video.mapping :as mapping]))

(def state-key :tolglow.video/edit-history)

(def max-entries 32)

(defn history [model] (get-in model [:extensions state-key] {}))

(defn- bounded [entries] (vec (take-last max-entries entries)))

(defn- wire [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn- refuse! [code message]
  (throw (ex-info message {:code code :errors [{:path [:op] :message message}]})))

(defn unit-path
  "The unit of [:settings :video] an edit operation changes."
  [{:keys [op] :as operation}]
  (case op
    (:put-source :delete-source) [:sources (:id operation)]
    :put-mapping [:mappings (get-in operation [:mapping :id])]
    :delete-mapping [:mappings (:id operation)]
    (:put-strip :delete-strip) [:mappings (:mapping-id operation) :strips]
    :put-surface [:surfaces (get-in operation [:surface :id])]
    (:patch-surface :delete-surface) [:surfaces (:id operation)]
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
    :patch-surface (str "surface " (wire (:id operation)))
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
  record it. An edit that changes nothing is not recorded. The edits of one
  knob turn or drag (the same `:coalesce` token on the same unit, while
  nothing has been undone since) are one step, and a turn that ends where
  it began leaves no step. Throws like apply-operation when the result is
  invalid."
  [model operation]
  (let [before (get-in model [:settings :video])
        after (mapping/apply-operation before operation)
        path (unit-path operation)
        group (:coalesce operation)
        h (history model)
        id (inc (long (:last-id h 0)))
        entry (cond-> {:id id :label (label operation) :path path :before (slot before path) :after (slot after path)}
                (string? group) (assoc :coalesce group))
        undo (vec (:undo h))
        top (peek undo)
        merge? (and (string? group) (= group (:coalesce top)) (= path (:path top)) (empty? (:redo h)))]
    (cond-> (assoc-in model [:settings :video] after)
      (and path (or merge? (not= (:before entry) (:after entry))))
      (assoc-in [:extensions state-key]
                (let [merged (assoc entry :before (:before top))]
                  (-> h
                      (assoc :last-id id)
                      (assoc :undo (cond
                                     (not merge?) (bounded (conj undo entry))
                                     ;; Turned back to where it began: no step.
                                     (= (:before merged) (:after merged)) (pop undo)
                                     :else (conj (pop undo) merged)))
                      (assoc :redo [])))))))

(defn step
  "Undo (direction :undo) or redo (:redo) the newest video settings edit.
  expected-entry, when not nil, is the id of the entry the caller means.
  Throws ex-info with :code :video/nothing-to-undo, :video/nothing-to-redo,
  :video/history-moved or :video/history-conflict, or like apply-operation
  when the restored settings are invalid."
  ([model direction] (step model direction nil))
  ([model direction expected-entry]
   (let [h (history model)
         [from to side current-side] (if (= :undo direction) [:undo :redo :before :after] [:redo :undo :after :before])
         entry (peek (get h from))
         settings (or (get-in model [:settings :video]) {})]
     (when-not entry
       (refuse! (keyword "video" (str "nothing-to-" (name direction))) (str "Nothing to " (name direction))))
     (when (and (some? expected-entry) (not= expected-entry (:id entry)))
       (refuse! :video/history-moved
                (str "Another edit came first: the newest step is now " (:label entry) ". Check it before you " (name direction) ".")))
     (when-not (= (get entry current-side) (slot settings (:path entry)))
       (refuse! :video/history-conflict
                (str "Cannot " (name direction) " " (:label entry) ": it was changed since by an edit outside this history.")))
     (let [settings (-> settings (restore (:path entry) (get entry side)) clean mapping/validate!)]
       (-> model
           (assoc-in [:settings :video] settings)
           (assoc-in [:extensions state-key]
                     (-> h (update from pop) (update to #(bounded (conj (or % []) entry))))))))))

;; Shader cue versions ---------------------------------------------------------

(defn shader-history [model effect-id] (get-in (history model) [:shaders effect-id] {:undo [] :redo []}))

(defn shader-head
  "The entry an undo or redo of effect-id would use: {:restore :to}, or nil."
  [model effect-id direction]
  (peek (get (shader-history model effect-id) direction)))

(defn shader-restore
  "The version whose :shader block an undo or redo of effect-id restores,
  or nil when that list is empty."
  [model effect-id direction]
  (:restore (shader-head model effect-id direction)))

(defn record-shader
  "Record an accepted shader publication. edit is {:effect-id :op :from}:
  :from is the version that was current before and to the version
  published. :op :edit starts a new branch; :undo and :redo move the head
  of one list to the other. The opposite list gains {:restore from :to to}."
  [model {:keys [effect-id op from]} to]
  (update-in model [:extensions state-key :shaders effect-id]
             (fn [h]
               (let [{:keys [undo redo]} (merge {:undo [] :redo []} h)
                     entry {:restore from :to to}]
                 (case op
                   :edit {:undo (bounded (conj undo entry)) :redo []}
                   :undo {:undo (pop undo) :redo (bounded (conj redo entry))}
                   :redo {:undo (bounded (conj undo entry)) :redo (pop redo)})))))

(defn forget-shader
  "Drop effect-id's shader history: it was published without a step."
  [model effect-id]
  (if (contains? (:shaders (history model)) effect-id)
    (update-in model [:extensions state-key :shaders] dissoc effect-id)
    model))

(defn check-shader-edit!
  "Refuse a shader edit step that does not match the model. :from must be
  the effect's current version. An undo or redo must name the head entry's
  version to restore, the effect must still be at that entry's :to, and the
  published definition must carry exactly that version's :shader and
  :params."
  [model {:keys [effect-id op from restore]} definition]
  (let [current (get-in model [:registry :effects effect-id :version])
        head (when (#{:undo :redo} op) (shader-head model effect-id op))
        older (when head (get-in model [:registry :versions [:effects effect-id (:restore head)]]))]
    (when-not (and (#{:edit :undo :redo} op) (= from current)
                   (or (= :edit op)
                       (and head (= restore (:restore head)) (= current (:to head)) older
                            (= (:shader definition) (:shader older)) (= (:params definition) (:params older)))))
      (throw (ex-info "The shader's edit history changed; try again"
                      {:code :shader/history-moved :effect-id effect-id})))))

;; Saved state -------------------------------------------------------------------

(defn- valid-path? [path]
  (and (vector? path)
       (or (and (= 2 (count path)) (#{:sources :mappings :surfaces} (first path)) (mapping/identifier? (second path)))
           (and (= 3 (count path)) (= :mappings (first path)) (mapping/identifier? (second path)) (= :strips (nth path 2))))))

(defn- valid-slot? [s]
  (and (map? s) (boolean? (:present? s)) (or (:present? s) (not (contains? s :value)))))

(defn- valid-entry? [e]
  (and (map? e) (nat-int? (:id e)) (string? (:label e)) (valid-path? (:path e))
       (valid-slot? (:before e)) (valid-slot? (:after e))
       (or (nil? (:coalesce e)) (string? (:coalesce e)))))

(defn- valid-shader-entry? [e]
  (and (map? e) (nat-int? (:restore e)) (nat-int? (:to e))))

(defn- bounded-list? [pred xs]
  (and (vector? xs) (<= (count xs) max-entries) (every? pred xs)))

(defn valid-saved-state?
  "Whether a saved history (the value under `state-key`) is well formed."
  [h]
  (and (map? h)
       (or (nil? (:last-id h)) (nat-int? (:last-id h)))
       (bounded-list? valid-entry? (:undo h []))
       (bounded-list? valid-entry? (:redo h []))
       (or (nil? (:shaders h))
           (and (map? (:shaders h))
                (every? (fn [[id s]] (and (keyword? id) (map? s)
                                          (bounded-list? valid-shader-entry? (:undo s []))
                                          (bounded-list? valid-shader-entry? (:redo s []))))
                        (:shaders h))))))

(defn summary
  "What Studio shows on its Undo and Redo buttons: counts, labels and the
  entry ids it sends back as :expected-entry."
  [model]
  (let [h (history model)]
    {:undo (count (:undo h)) :redo (count (:redo h))
     :undo-label (:label (peek (:undo h))) :redo-label (:label (peek (:redo h)))
     :undo-id (:id (peek (:undo h))) :redo-id (:id (peek (:redo h)))
     :shaders (into {} (for [[id {:keys [undo redo]}] (:shaders h)]
                         [id {:undo (count undo) :redo (count redo)}]))}))
