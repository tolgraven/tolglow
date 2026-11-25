(ns tolglow.composition.sequence-edit
  "Pure edits of the same sequence definitions accepted by Composition Save."
  (:require [afterglow.time :as time]
            [clojure.string :as str]
            [tolglow.composition.sequence :as sequence]
            [tolglow.registry :as registry]))

(defn- fail! [code details message]
  (throw (ex-info message (assoc details :code code))))

(defn- slots! [definition]
  (let [slots (:slots definition)]
    (when-not (and (= :sequence (:mode definition)) (vector? slots) (every? map? slots))
      (fail! :invalid-sequence {} "Open a sequence definition with a vector of steps"))
    slots))

(defn- slot-index! [slots step-id]
  (let [[index other] (keep-indexed #(when (= (sequence/step-key step-id) (sequence/step-key (:id %2))) %1) slots)]
    (when (or (nil? step-id) (nil? index) (some? other))
      (fail! :unknown-step {:step-id step-id} "Choose an existing step by its unique ID"))
    index))

(defn select-step
  "Return the step at a stable ID, independent of its current vector index."
  [definition step-id]
  (let [slots (slots! definition)]
    (nth slots (slot-index! slots step-id))))

(defn- sequence-spec [definition slots]
  {:steps slots :timebase (:timebase definition)
   :mode (:loop definition :once) :seed (:seed definition 0)})

(defn- with-slots [definition slots]
  (when (seq slots)
    (when-let [errors (seq (sequence/validate-sequence (sequence-spec definition slots)))]
      (fail! :invalid-sequence {:errors (vec errors)} "Invalid sequence steps")))
  (assoc definition :slots slots))

(defn add-step
  "Insert after a stable step ID, or append when after-id is nil. The caller
  supplies a fresh keyword or string ID for the new step."
  [definition after-id step]
  (let [slots (slots! definition)
        id (:id step)
        index (if (nil? after-id) (count slots) (inc (slot-index! slots after-id)))]
    (when-not (and (map? step) (or (keyword? id) (and (string? id) (not (str/blank? id)))))
      (fail! :invalid-step-id {:step-id id} "Provide a new step ID"))
    (when (some #(= (sequence/step-key id) (sequence/step-key (:id %))) slots)
      (fail! :duplicate-step-id {:step-id id} "A step already uses this ID"))
    (let [default-timebase (:timebase (sequence/normalize-spec (sequence-spec definition slots)))
          step (cond-> (merge {:params {}} step)
                 (not-any? #(contains? step %) [:duration :duration-ms :duration-beats])
                 (assoc :duration (if (= :ms default-timebase) {:ms 1000} {:beats 1})))]
      (with-slots definition (into (conj (subvec slots 0 index) step) (subvec slots index))))))

(defn add-empty-step
  "Insert an empty step that occupies time without launching a cue."
  [definition after-id step-id]
  (add-step definition after-id {:id step-id :kind :empty}))

(defn copy-step
  "Return a portable clipboard value preserving the step's authored content."
  [definition step-id]
  (let [content (dissoc (select-step definition step-id) :id)]
    (when-not (registry/data? content)
      (fail! :invalid-step-clipboard {:step-id step-id} "Step clipboard requires portable data"))
    content))

(defn paste-step
  "Insert clipboard content with a fresh step ID."
  [definition after-id new-id clipboard]
  (when-not (map? clipboard)
    (fail! :invalid-step-clipboard {} "Copy a step before pasting"))
  (add-step definition after-id (assoc clipboard :id new-id)))

(defn duplicate-step [definition step-id new-id]
  (paste-step definition step-id new-id (copy-step definition step-id)))

(defn delete-step
  "Remove a step by ID. A draft can be empty until the next addition."
  [definition step-id]
  (let [slots (slots! definition)
        index (slot-index! slots step-id)]
    (with-slots definition (into (subvec slots 0 index) (subvec slots (inc index))))))

(defn- edit-step [definition step-id f]
  (let [slots (slots! definition)]
    (with-slots definition (update slots (slot-index! slots step-id) f))))

(defn toggle-muted
  "Toggle cue playback for this step while preserving its authored time and content."
  [definition step-id]
  (edit-step definition step-id #(if (:muted? %) (dissoc % :muted?) (assoc % :muted? true))))

(defn set-duration
  "Set a positive duration in the sequence's clock domain. Preserve authored units."
  [definition step-id duration]
  (let [timebase (:timebase (sequence/normalize-spec (sequence-spec definition (slots! definition))))
        {:keys [unit value]} (time/normalize-duration duration)]
    (when-not (and (= timebase unit) (pos? value))
      (fail! :invalid-duration {:step-id step-id} "Use a positive duration from this sequence's clock domain"))
    (edit-step definition step-id #(assoc (dissoc % :duration-ms :duration-beats) :duration duration))))

(defn extend-duration
  "Add a positive musical duration. Preserve matching units and combine
  different musical units in beats using the supplied transport meter."
  [definition step-id increment meter]
  (let [slots (slots! definition)
        index (slot-index! slots step-id)
        {:keys [timebase steps]} (sequence/normalize-spec (sequence-spec definition slots) meter)
        {:keys [unit value]} (time/normalize-duration increment meter)
        authored (:duration (nth slots index))
        [increment-unit increment-value] (first increment)]
    (when-not (and (= :beats timebase unit) (pos? value))
      (fail! :invalid-extension {:step-id step-id} "Extend a musical sequence by positive beats, bars or phrases"))
    (set-duration definition step-id
                  (if (= increment-unit (ffirst authored))
                    {increment-unit (+' (val (first authored)) increment-value)}
                    {:beats (+' (:duration-beats (nth steps index)) value)}))))
