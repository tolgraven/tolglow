(ns tolglow.composition.macro
  "Simultaneous cue slots with explicit ownership, without sequence timing."
  (:require [tolglow.registry :as registry]))

(defn launch-plan
  "Create simultaneous launch intents. Slots have :id, :cue-id and optional
  :params/:launch. owner-id must be unique per execution. No cue is launched by
  editing the slot data; the shell executes these intents and captures IDs."
  [owner-id slots]
  (when-not (and (some? owner-id) (registry/data? owner-id)
                 (vector? slots) (registry/data? slots)
                 (every? #(and (some? (:id %)) (some? (:cue-id %))) slots)
                 (= (count slots) (count (distinct (map :id slots)))))
    (throw (ex-info "Macro needs portable slots with distinct IDs and cue references" {})))
  {:owner-id owner-id
   :tokens (mapv #(vector owner-id (:id %)) slots)
   :intents (mapv (fn [index slot]
                    {:op :composition/launch :owner-id owner-id :member index
                     :token [owner-id (:id slot)] :slot-id (:id slot) :cue-id (:cue-id slot)
                     :launch (merge {:mode :persistent :params (:params slot {})} (:launch slot))}) (range) slots)})

(defn end-plan
  "End only instances captured for this macro's tokens. Instances are the
  token->instance-id acknowledgements from the shell; missing launches are safe."
  [plan instances]
  (into []
        (keep (fn [token]
                (when-let [id (get instances token)]
                  {:op :instance/end :instance-id id :owner-id (:owner-id plan)})))
        (:tokens plan)))
