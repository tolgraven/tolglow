(ns tolglow.knob-presets
  "Knob looks (#547): eight saved sets of linked-knob positions, recalled from
  the top Push pad row, Studio or the REPL. A slot stores knob positions only,
  never destination values, so recall resolves every link through its current
  range, curve, direction and steps, exactly as a knob turn does. Recall
  commits all stored knobs in one owner command; parameters and knobs the slot
  does not name are not written. See docs/linked-knobs.md#knob-looks."
  (:require [afterglow.time :as time]
            [tolglow.knobs :as knobs]))

(def state-key
  "Saved under the show's :extensions, so full backups and show saves keep it."
  ::bank)

(def slot-count 8)

(def operations #{:knob-preset/capture :knob-preset/recall :knob-preset/clear})

(def value-operations
  "Ops whose committed knob values fan out to destination writes."
  #{:knob-preset/recall})

(defn- fail! [code message & [details]]
  (throw (ex-info message {:control/error code :details (or details {})})))

(defn- id? [x] (or (keyword? x) (and (string? x) (<= 1 (count x) 128))))

(defn- unit-fraction? [x] (and (time/finite-number? x) (<= 0 x 1)))

(defn- slot? [x] (and (integer? x) (<= 0 x (dec slot-count))))

(defn bank
  "{:slots {slot {:values {knob-id position}}} :selected slot-or-nil}."
  [model]
  (get-in model [:extensions state-key] {:slots {}}))

(defn valid-saved-state?
  "Portable structure only. Knobs a slot names may have been removed since;
  recall skips and reports them."
  [{:keys [slots selected] :as state}]
  (boolean
   (and (map? state) (every? #{:slots :selected} (keys state)) (map? slots)
        (every? (fn [[slot {:keys [values] :as entry}]]
                  (and (slot? slot) (map? entry) (= [:values] (keys entry)) (map? values) (seq values)
                       (every? (fn [[id value]] (and (id? id) (unit-fraction? value))) values)))
                slots)
        (or (nil? selected) (contains? slots selected)))))

(defn- ordered-knobs
  "Knobs in creation order, as every surface lists them."
  [model]
  (sort-by :order (vals (get-in model [:knobs :knobs]))))

(defn- checked-slot [slot]
  (when-not (slot? slot) (fail! :invalid-knob-preset "A knob look slot is 0–7" {:slot slot}))
  slot)

(defn- wire-key [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn- same-contents?
  "Whether shown slot contents match the stored ones. A browser may return knob
  IDs as keywords or text and whole positions as integers. Text comparison
  applies only when it keeps every ID distinct, so a knob :a and a knob \"a\"
  cannot stand in for each other."
  [expected stored]
  (cond
    (or (nil? expected) (nil? stored)) (and (nil? expected) (nil? stored))
    (not (map? expected)) false
    (= expected stored) true
    :else
    (let [shown (update-keys expected wire-key)
          saved (update-keys stored wire-key)]
      (and (= (count expected) (count shown) (count stored) (count saved))
           (every? (fn [[id value]]
                     (let [v (get shown id)]
                       (and (number? v) (== v value))))
                   saved)))))

(defn- check-expected!
  "A surface that presented a slot sends what it showed as :expected (nil for
  an empty pad); a slot changed elsewhere since is refused, not overwritten."
  [command stored]
  (when (and (contains? command :expected) (not (same-contents? (:expected command) stored)))
    (fail! :stale-knob-preset "That knob look changed since it was shown. Look again and retry"
           {:slot (:slot command)})))

(defn- recall-writes
  "Set every stored knob that still exists, in creation order, through the
  linked-knob operation. Return [model events outcomes writes missing]. Link
  IDs in outcomes and writes are [knob-id link-id], unique across knobs."
  [model values]
  (let [present (filter #(contains? values (:id %)) (ordered-knobs model))
        missing (vec (sort-by str (remove (set (map :id present)) (keys values))))]
    (conj (reduce (fn [[model events outcomes writes] {:keys [id]}]
                    (let [{next-model :model knob-events :events {:keys [links] knob-writes :writes} :result}
                          (knobs/apply-operation model {:op :knob/set :knob-id id :value (get values id)})
                          composite #(update % :link-id (fn [link-id] [id link-id]))]
                      [next-model (into events knob-events)
                       (into outcomes (map #(assoc (composite %) :knob-id id)) links)
                       (into writes (map composite) knob-writes)]))
                  [model [] [] []]
                  present)
          missing)))

(defn apply-operation
  "Pure [model command] operation for afterglow.control.model/apply-domain-command."
  [model {:keys [op] :as command}]
  (let [{:keys [slots selected] :as state} (bank model)
        slot (checked-slot (:slot command))
        stored (get-in slots [slot :values])
        save #(assoc-in model [:extensions state-key] %)]
    (check-expected! command stored)
    (case op
      :knob-preset/capture
      (let [values (into {} (map (juxt :id :value)) (ordered-knobs model))]
        (when (empty? values) (fail! :invalid-knob-preset "Create a knob before saving a look"))
        (when (and stored (not (true? (:overwrite? command))))
          (fail! :knob-preset-occupied "That look is already saved. Overwrite it to replace it" {:slot slot}))
        {:model (save (-> state (assoc-in [:slots slot] {:values values}) (assoc :selected slot)))
         :events [{:type :knob-preset/captured :slot slot}]
         :result {:slot slot :values values :replaced? (some? stored)}})

      :knob-preset/recall
      (do
        (when-not stored (fail! :empty-knob-preset "That look is empty" {:slot slot}))
        (let [[next-model events outcomes writes missing] (recall-writes model stored)]
          (when (= (count missing) (count stored))
            (fail! :knob-preset-unavailable "None of that look's knobs exist any more" {:slot slot :missing missing}))
          {:model (assoc-in next-model [:extensions state-key :selected] slot)
           :events (conj events {:type :knob-preset/recalled :slot slot})
           :result (cond-> {:slot slot :links outcomes :writes writes}
                     (seq missing) (assoc :missing missing))}))

      :knob-preset/clear
      (do
        (when-not stored (fail! :empty-knob-preset "That look is empty" {:slot slot}))
        {:model (save (cond-> (update state :slots dissoc slot) (= slot selected) (dissoc :selected)))
         :events [{:type :knob-preset/cleared :slot slot}]
         :result {:slot slot}}))))

(defn- same-position? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(defn projection
  "Eight slots for every surface. :state is :empty, :stored or :selected (the
  look last saved or recalled). :matches? says whether every knob it names
  still sits at its saved position. :knobs lists stored positions in creation
  order with the knob's current position; removed knobs are :missing? true."
  [model]
  (let [{:keys [slots selected]} (bank model)
        knobs (ordered-knobs model)
        by-id (into {} (map (juxt :id identity)) knobs)
        order (into {} (map-indexed (fn [i k] [(:id k) i])) knobs)]
    {:selected selected
     :slots (mapv (fn [slot]
                    (if-let [values (get-in slots [slot :values])]
                      (let [rows (->> values
                                      (sort-by (fn [[id]] [(get order id Long/MAX_VALUE) (str id)]))
                                      (mapv (fn [[id value]]
                                              (if-let [knob (by-id id)]
                                                {:knob-id id :name (:name knob) :value value :current (:value knob)
                                                 :text (format "%.3f" (double value))}
                                                {:knob-id id :value value :missing? true
                                                 :text (format "%.3f" (double value))}))))]
                        {:slot slot :state (if (= slot selected) :selected :stored)
                         :matches? (every? #(and (not (:missing? %)) (same-position? (:value %) (:current %))) rows)
                         :values values :knobs rows})
                      {:slot slot :state :empty}))
                  (range slot-count))}))
