(ns tolglow.rig.venue-scan
  "Commit a reviewed metric plane as a named receiving surface in the show rig."
  (:require [tolglow.rig.surfaces :as surfaces]))

(def ^:private geometry-keys [:point :normal :u-axis :size])

(defn- revision [surface]
  ;; Send exactly the geometry and evidence being replaced as an opaque string.
  ;; Manual edits to unrelated surface metadata can commute with a measurement.
  (pr-str [(mapv surface geometry-keys) (mapv (or (:measurement surface) {}) [:name :world-id :anchor-id :method])]))

(defn saved [rig]
  (into [] (keep (fn [{:keys [measurement] :as surface}]
                   (when (= "arkit-plane" (:method measurement))
                     {:name (:name measurement) :revision (revision surface)})))
        (:surfaces rig)))

(defn- identifier? [s] (and (string? s) (some? (parse-uuid s))))

(defn admit [model {:keys [surface-name expected-surface-revision world-id anchor-id] :as payload}]
  (when-not (and (string? surface-name) (re-matches #"[a-z0-9][a-z0-9-]{0,47}" surface-name)
                 (identifier? world-id) (identifier? anchor-id))
    (throw (ex-info "Supply a surface name, AR world ID and tracked plane ID" {})))
  (let [id (str "scan-" surface-name)
        planes (vec (get-in model [:rig :surfaces]))
        index (first (keep-indexed #(when (= id (surfaces/wire-id (:id %2))) %1) planes))
        previous (get planes index)
        measurement {:method "arkit-plane" :name surface-name :world-id world-id :anchor-id anchor-id}]
    (when-not (and (= expected-surface-revision (some-> previous revision))
                   (or (nil? previous) (= "arkit-plane" (get-in previous [:measurement :method]))))
      (throw (ex-info "Surface changed or its name is in use. Reconnect and review the measurement again." {})))
    (when-not (every? #(contains? payload %) geometry-keys)
      (throw (ex-info "Supply center, normal, width axis and receiver dimensions" {})))
    (let [surface (merge previous (select-keys payload geometry-keys)
                         {:id (:id previous id) :measurement measurement})
          updated (if (nil? index) (conj planes surface) (assoc planes index surface))
          rig (assoc (:rig model) :surfaces updated)
          errors (concat (surfaces/surface-errors rig) (surfaces/tracking-errors rig))]
      (when (seq errors) (throw (ex-info "Invalid receiver geometry or tracking link" {:errors (vec errors)})))
      {:model (-> model (assoc :rig rig) (update :revision inc))
       :ack {:status :accepted :surface-id id}})))
