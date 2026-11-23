(ns tolglow.sensors.audience.zones
  "Operator selection and public zone status for the nearby audience mix."
  (:require [tolglow.sensors.audience :as audience]))

(defn- gateway? [source]
  (let [channels (into #{} (map :id) (get-in source [:config :channels]))]
    (every? channels [:audience-count :dance-effort])))

(defn select-zone
  "Change one zone in the ordered selection. Concurrent edits to other zones survive."
  [model id included?]
  (when-not (and (string? id) (<= 1 (count id) 128) (boolean? included?))
    (throw (ex-info "Select a zone by its enrolled identifier and a boolean" {})))
  (let [ids (get-in model [:variables :audience :gateways] [])
        included (set ids)]
    (when (and included? (not (gateway? (get-in model [:sensors :sources id]))))
      (throw (ex-info "Enroll and configure this audience gateway before including it" {})))
    (cond
      (= included? (contains? included id)) model
      (and included? (>= (count ids) 16)) (throw (ex-info "The audience mix supports 16 zones" {}))
      :else (-> model
                (assoc-in [:variables :audience :gateways]
                          (if included? (conj ids id) (filterv #(not= id %) ids)))
                audience/project))))

(defn- zone [now devices [id source]]
  (let [{:keys [session-id last-time-ms armed?]} (:state source)
        device (get devices id)
        joined? (and (some? last-time-ms) (= session-id (:session-id device)))
        status (cond
                 (not joined?) :waiting
                 (> (- now last-time-ms) (get-in source [:config :timeout-ms] 500)) :expired
                 (and armed? (:armed? device)) :armed
                 :else :ready)]
    (cond-> {:id id :label (or (get-in source [:pairing :label]) id) :status status}
      (#{:ready :armed} status) (assoc :session-id session-id))))

(defn operator-view
  "Project only the operator's enrolled gateways and admitted aggregate readings.
  The session token here identifies the gateway session observed when pressing Arm."
  [model devices]
  (let [configured (get-in model [:variables :audience])
        ids (vec (:gateways configured))
        sources (get-in model [:sensors :sources])
        zones (into (sorted-map)
                    (keep (fn [[id source :as entry]]
                            (when (gateway? source)
                              [id (zone (:time-ms model) devices entry)]))) sources)
        current (filter #(= :armed (:status (get zones %))) ids)
        committed (filter #(get-in sources [% :state :armed?]) ids)
        levels (if (and (:levels configured) (= (seq current) (seq committed)) (seq current))
                 (:levels configured)
                 (audience/levels sources current))]
    {:gateways ids
     :levels (select-keys levels [:audience-count :dance-effort :pulse-count :pulse-bpm])
     :zones (into (vec (vals zones))
                  (keep #(when-not (contains? zones %) {:id % :label % :status :unavailable})) ids)}))
