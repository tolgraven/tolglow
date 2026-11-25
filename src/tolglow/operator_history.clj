(ns tolglow.operator-history
  "Read-only operator history shared by the runtime snapshot and REPL."
  (:require [afterglow.control.edit-history :as history]
            [afterglow.control.model :as control]
            [afterglow.control.params :as params]
            [clojure.string :as str]))

(def ^:private outcome-limit 256)

(defn committed-outcomes
  "Retain terminal Studio history results across coalesced frames. The owner
  supplies the original queue entry and its exact acknowledgement."
  [previous epoch message-receipts]
  (let [completed (keep (fn [[{:keys [authority expected-epoch payload]} [_ ack]]]
                          (let [{:keys [command-id]} payload
                                {:keys [status message]} ack]
                            (when (and (::stream-outcome? authority) (= epoch expected-epoch)
                                       (#{:accepted :rejected} status)
                                       (or (string? command-id) (keyword? command-id))
                                       (<= 1 (count (str command-id)) 128))
                              (cond-> (assoc (select-keys ack [:status :revision :code :reason :history-cursor :history-position])
                                             :command-id command-id :epoch epoch)
                                (string? message) (assoc :message (subs message 0 (min (count message) 256)))))))
                        message-receipts)]
    (if (seq completed)
      (vec (take-last outcome-limit (into previous completed)))
      previous)))

(defn- actor-label [actor]
  (let [channel (cond
                  (keyword? actor) (or (namespace actor) (name actor))
                  (string? actor) (first (str/split actor #"/" 2)))]
    (case channel
      "push-2" "Push 2"
      "studio" "Studio"
      "repl" "REPL"
      "Operator")))

(defn- target-unavailability [instance parameter]
  (let [schema (get-in instance [:definition :params parameter])]
    (cond
      (not= :running (:phase instance)) "The edited instance is no longer running."
      (not (and (#{:number :integer} (:type schema))
                (params/valid-value? schema (get-in instance [:params parameter]))))
      "The edited parameter is no longer available.")))

(defn- node-view [model [id {:keys [parent target actor before after target-display]}]]
  (let [node {:id id :parent-id parent :sequence id}]
    (if (nil? target)
      node
      (let [[target-kind instance-id parameter] target
            instance (when (= :instance-param target-kind) (get-in model [:instances instance-id]))
            target-unavailable (target-unavailability instance parameter)]
        (cond-> (merge (assoc node :actor (actor-label actor) :parameter parameter :before before :after after)
                       (select-keys (or target-display (control/edit-target-display model target))
                                    [:cue-id :cue-label :effect-id :effect-label]))
          target-unavailable (assoc :target-unavailable-reason target-unavailable))))))

(defn- blocked-destinations [nodes cursor]
  (let [by-id (into {} (map (juxt :id identity)) nodes)
        children (group-by :parent-id nodes)]
    ;; A node describes the edge from its parent. A dead edge blocks travel
    ;; across it in either direction, including when selecting later edits.
    (loop [pending [[cursor nil nil]], blocked {}]
      (if-let [[id previous reason] (peek pending)]
        (let [{:keys [parent-id target-unavailable-reason]} (get by-id id)
              neighbors (cons [parent-id target-unavailable-reason]
                              (map (juxt :id :target-unavailable-reason) (get children id)))
              pending (into (pop pending)
                            (keep (fn [[next-id edge-reason]]
                                    (when (and (some? next-id) (not= next-id previous))
                                      [next-id id (or reason edge-reason)])))
                            neighbors)]
          (recur pending (cond-> blocked reason (assoc id reason))))
        blocked))))

(defn project
  "Show only completed node data and availability from the committed model.
  :unavailable-reason describes a dead edge between the cursor and destination.
  Current value/generation checks still belong to the authoritative command."
  [runtime-id model]
  (let [{:keys [nodes cursor limit next-id] :as history} (get-in model [:control :edit-history])
        retained (count nodes)
        rows (mapv #(node-view model %) (sort-by key nodes))
        blocked (when (some :target-unavailable-reason rows) (blocked-destinations rows cursor))]
    {:identity [runtime-id (:show-id model) (:epoch model)]
     :current-id cursor
     :position (history/position history)
     :nodes (if (seq blocked)
              (mapv (fn [{:keys [id] :as row}]
                      (if-let [reason (get blocked id)] (assoc row :unavailable-reason reason) row)) rows)
              rows)
     :retention {:limit (or limit control/edit-history-limit)
                 :retained retained
                 :pruned? (< retained (or next-id retained))}}))
