(ns tolglow.controllers.push-knobs
  "Linked knobs on Push (#666). The Afterglow adapters only route input: Push 3
  host encoder bindings and the Push 2 encoder overlay. This namespace owns the
  Push 2 knob page (toggle, slots, touches), the step policy through
  knobs/encoder-command, and the display data from the projection Studio reads."
  (:require [tolglow.knobs :as knobs]))

(defn- knob-id? [id]
  (or (keyword? id) (and (string? id) (boolean (seq id)))))

(defn- step? [step]
  (or (nil? step) (and (number? step) (Double/isFinite (double step)) (pos? step))))

(defn- turn-command
  "The knob command for detents on an encoder: the shared encoder steps, or a
  per-binding step (a tenth of it with Shift)."
  [knob-id step detents fine?]
  (if step
    {:op :knob/nudge :knob-id knob-id :delta (* detents (if fine? (/ step 10) step))}
    (knobs/encoder-command knob-id detents fine?)))

;; ---------------------------------------------------------------------------
;; Push 3: captured encoders bound to knobs

(defn push-3-options
  "Turn Push 3 :encoder-bindings of {:target :knob :knob-id k :step n} into
  Afterglow host bindings whose turns send :knob/nudge. The binding's control
  is what the display view reports as :touched-control while it is touched."
  [{:keys [encoder-bindings] :as options}]
  (let [knob-bindings (filter #(= :knob (:target (val %))) encoder-bindings)]
    (doseq [[id {:keys [knob-id step] :as binding}] knob-bindings]
      (when-not (and (every? #{:target :knob-id :step} (keys binding)) (knob-id? knob-id) (step? step))
        (throw (ex-info "Knob encoder bindings need a knob ID and a positive step" {:id id :binding binding}))))
    (if (empty? knob-bindings)
      options
      (assoc options
             :encoder-bindings (into encoder-bindings
                                     (map (fn [[id {:keys [knob-id step]}]]
                                            [id {:target :host
                                                 :control (cond-> {:target :knob :knob-id knob-id :editable? true}
                                                            step (assoc :step step))}]))
                                     knob-bindings)
             :host-encoder (fn [{:keys [knob-id step]} detents fine? _model]
                             [(turn-command knob-id step detents fine?)])))))

;; ---------------------------------------------------------------------------
;; Push 2: a knob page on the track encoders

(def ^:private push-2-reserved-ccs
  "Push 2 buttons and encoders the Afterglow profile already interprets."
  (into #{15 31 48 49 51 57 59 62 63 85 86 88 110 111 112 113}
        (concat (range 20 28) (range 44 48) (range 71 80))))

(def ^:private cue-variation-ccs
  "Note and Scale, used by the Push 2 cue-variation overlay."
  #{50 58})

(defn- modifier-cc [{:keys [temporary-encoder-modifier]}]
  (if (integer? temporary-encoder-modifier) temporary-encoder-modifier 57))

(defn- page-input
  "Claim the page button, and while the page is shown the track encoders and
  their touches. The page does not change while any track encoder is touched."
  [{:keys [button knob-ids step]} overlay {:keys [type note controller raw-value]} _model {:keys [shift? touched]}]
  (let [shown? (some? (:knob-ids overlay))
        release #(-> % (update :touches dissoc note) (update :order (fn [order] (vec (remove #{note} order)))))]
    (cond
      (and (= :control-change type) (= button controller))
      {:state (cond-> overlay
                (and (pos? raw-value) (empty? touched) (empty? (:touches overlay)))
                (assoc :knob-ids (when-not shown? knob-ids)))}

      (not shown?) nil

      (and (= :note-on type) (<= 0 note 7))
      {:state (if-let [id (get knob-ids note)]
                (-> (release overlay) (assoc-in [:touches note] id) (update :order (fnil conj []) note))
                overlay)}

      (and (= :note-off type) (<= 0 note 7))
      {:state (release overlay)}

      (and (= :control-change type) (<= 71 controller 78))
      (let [id (get knob-ids (- controller 71))
            detents (if (< raw-value 64) raw-value (- raw-value 128))]
        {:state overlay
         :commands (if (and id (not (zero? detents))) [(turn-command id step detents shift?)] [])}))))

(defn- page-projection [overlay _model]
  (when-let [ids (:knob-ids overlay)]
    (let [touched (get-in overlay [:touches (peek (:order overlay))])]
      (cond-> {:knob-page {:knob-ids ids}}
        touched (assoc :touched-control {:target :knob :knob-id touched :editable? true})))))

(defn push-2-encoder-overlay
  "Validate :knob-page {:button cc :knob-ids [id ...] :step n} against the other
  connection options and return the Afterglow :encoder-overlay for it."
  [{:keys [button knob-ids step] :as page} options]
  (when-not (and (map? page) (every? #{:button :knob-ids :step} (keys page))
                 (integer? button) (<= 0 button 127) (not (push-2-reserved-ccs button))
                 (not= button (modifier-cc options))
                 (vector? knob-ids) (<= 1 (count knob-ids) 8) (every? knob-id? knob-ids)
                 (step? step))
    (throw (ex-info "Knob page needs a free button CC, one to eight knob IDs and a positive step" {:knob-page page})))
  (when (and (true? (:cue-variations? options)) (cue-variation-ccs button))
    (throw (ex-info "The knob page button is taken by cue variations (Note and Scale)" {:button button})))
  {:input (partial page-input page)
   :project page-projection
   :reset #(dissoc % :touches :order)})

;; ---------------------------------------------------------------------------
;; Display

(defn- page-entry [displays knob-id]
  (if-let [{:keys [name value text columns]} (displays knob-id)]
    {:knob-id knob-id :name name :value value :text text :links (count columns)}
    {:knob-id knob-id :missing? true}))

(defn project-display
  "Add :knob for a touched knob encoder and the knobs of a shown Push 2 knob
  page to a device view. A knob that no longer exists is marked, not dropped.
  Every link is resolved once per view, however many knobs it shows."
  [model view]
  (let [{:keys [target knob-id]} (:touched-control view)
        touched? (= :knob target)
        page (get-in view [:knob-page :knob-ids])]
    (if-not (or touched? (seq page))
      view
      (let [displays (into {} (map (juxt :id knobs/projected-encoder-display)) (:knobs (knobs/projection model)))]
        (cond-> view
          touched?
          (assoc :knob (or (displays knob-id) {:knob-id knob-id :missing? true}))
          (seq page)
          (assoc-in [:knob-page :knobs] (mapv #(page-entry displays %) page)))))))
