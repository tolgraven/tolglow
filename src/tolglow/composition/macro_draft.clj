(ns tolglow.composition.macro-draft
  "An immutable macro authoring overlay over a committed show snapshot."
  (:require [afterglow.control.pages :as pages]
            [afterglow.control.views :as views]
            [clojure.string :as str]
            [tolglow.composition :as composition]
            [tolglow.registry :as registry]))

(defn- fail! [code message details]
  (throw (ex-info message (assoc details :code code))))

(defn- show-basis [model]
  (select-keys model [:show-id :epoch]))

(defn begin
  "Open an empty draft with a caller-supplied session ID."
  [model id]
  (when-not (or (keyword? id) (and (string? id) (not (str/blank? id))))
    (fail! :invalid-draft-id "Provide a draft ID" {}))
  (merge (show-basis model) {:id id :revision 0 :status :open :next-slot 0 :slots []}))

(defn- check-open! [draft]
  (when-not (= :open (:status draft))
    (fail! :closed-draft "The macro draft is closed" {:draft-id (:id draft)})))

(defn- check-revision! [draft expected]
  (check-open! draft)
  (when-not (= expected (:revision draft))
    (fail! :stale-draft "The macro draft changed" {:draft-revision (:revision draft)})))

(defn- check-show! [model draft]
  (check-open! draft)
  (when-not (= (show-basis model) (show-basis draft))
    (fail! :stale-show "The draft belongs to a different show session" (show-basis model))))

(defn- cue-stamp [model cue-id]
  (let [registry (:registry model) cue (get-in registry [:cues cue-id])]
    (when-let [effect (registry/cue-effect registry cue)]
      {:cue-id cue-id :cue-version (:version cue)
       :effect-id (:id effect) :effect-version (:version effect)})))

(def ^:private cue-fields [:cue-id :cue-version :effect-id :effect-version])

(defn project-placement
  "Return one bottom-origin pad token for an occupied cell on any page. The
  token identifies the displayed cue and the overlay revision that received it."
  [model draft page-id coordinate]
  (check-show! model draft)
  (when (and (vector? coordinate) (= 2 (count coordinate))
             (every? #(and (integer? %) (not (neg? %))) coordinate))
    (when-let [cue (some->> (pages/cue-id-at model page-id coordinate) (cue-stamp model))]
      (let [placement {:page-id page-id :coordinate coordinate}
            selected (some #(when (and (= placement (:placement %)) (= (:cue-id cue) (:cue-id %)))
                              (:id %)) (:slots draft))]
        (merge (show-basis draft) {:draft-id (:id draft) :draft-revision (:revision draft)
                                   :placement placement :cue cue :slot-id selected})))))

(defn- append-slot [draft slot]
  (-> draft
      (update :slots conj (assoc slot :id (str "slot-" (:next-slot draft))))
      (update :next-slot inc)))

(defn- without-slot [slots index]
  (into (subvec slots 0 index) (subvec slots (inc index))))

(defn- slot-index! [slots slot-id]
  (or (first (keep-indexed #(when (= slot-id (:id %2)) %1) slots))
      (fail! :unknown-draft-slot "No draft slot has this ID" {:slot-id slot-id})))

(defn remove-slot
  "Remove a slot by ID, including a slot captured from a running instance."
  [draft expected-revision slot-id]
  (check-revision! draft expected-revision)
  (let [slots (:slots draft)]
    (-> draft (assoc :slots (without-slot slots (slot-index! slots slot-id))) (update :revision inc))))

(defn toggle-placement
  "Toggle the projected placement. Copies of the same cue on different pads
  have independent slots. A pad move, republish or draft edit invalidates its token."
  [model draft {:keys [draft-revision placement slot-id cue] :as projected}]
  (check-revision! draft draft-revision)
  (let [current (project-placement model draft (:page-id placement) (:coordinate placement))]
    (when-not (and current (= projected current))
      (fail! :stale-placement "The displayed cue placement changed" {:placement placement}))
    (if slot-id
      (remove-slot draft draft-revision slot-id)
      (-> draft (append-slot (assoc cue :placement placement :params {})) (update :revision inc)))))

(defn move-slot
  "Move the specified slot to a zero-based index in the final slot vector."
  [draft expected-revision slot-id index]
  (check-revision! draft expected-revision)
  (let [slots (:slots draft) from (slot-index! slots slot-id)]
    (when-not (and (integer? index) (<= 0 index) (< index (count slots)))
      (fail! :invalid-slot-index "The destination is outside the draft" {:index index}))
    (if (= index from)
      draft
      (let [remaining (without-slot slots from)]
        (-> draft
            (assoc :slots (into (conj (subvec remaining 0 index) (nth slots from))
                                (subvec remaining index)))
            (update :revision inc))))))

(defn capture-running
  "Replace the draft slots with a capture of running cue instances from one
  model snapshot, in committed composition order. Freeze the authored parameter
  values. A cue whose current reference cannot replay its running effect rejects
  the capture as a whole."
  [model draft]
  (check-show! model draft)
  (let [running (filter #(and (:cue-id %) (= :running (:phase %))) (views/active-instances model))
        capture (fn [draft {:keys [id cue-id params] :as instance}]
                  (let [stamp (cue-stamp model cue-id)
                        effect-fields [:effect-id :effect-version]]
                    (when-not (and stamp (= (select-keys stamp effect-fields)
                                            (select-keys instance effect-fields)))
                      (fail! :stale-instance-cue "The registered cue resolves to a different effect"
                             {:instance-id id :cue-id cue-id}))
                    (when-not (and (map? params) (registry/data? params))
                      (fail! :non-portable-params "The running cue has non-portable parameter values" {:instance-id id}))
                    (append-slot draft (assoc stamp :instance-id id :params params))))]
    (update (reduce capture (assoc draft :slots []) running) :revision inc)))

(defn cancel
  "Close the draft and discard its slots."
  [draft]
  (check-open! draft)
  (-> draft (assoc :status :cancelled :slots []) (update :revision inc)))

(defn prepare-save
  "Validate a new named macro and return its normal composition definition.
  Check dependencies against the current model at the owner save boundary."
  [model draft {:keys [expected-draft-revision id name]}]
  (check-revision! draft expected-draft-revision)
  (check-show! model draft)
  (when-not (and (string? name) (not (str/blank? name)))
    (fail! :invalid-macro-name "Provide a macro name" {}))
  (when (contains? (:compositions model) id)
    (fail! :composition-exists "A composition already has this ID" {:id id}))
  (doseq [{:keys [cue-id] :as slot} (:slots draft)]
    (when-not (= (select-keys slot cue-fields) (cue-stamp model cue-id))
      (fail! :stale-draft-cue "A draft slot's cue or effect changed"
             {:slot-id (:id slot) :cue-id cue-id})))
  (composition/prepare-definition model {:id id :name name :mode :macro
                                         :slots (mapv #(select-keys % [:id :cue-id :params]) (:slots draft))}))
