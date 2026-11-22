(ns tolglow.rig.surface-aim
  "Direct aim and edge-follow controls shared by Studio and headless operators."
  (:require [tolglow.rig.surfaces :as surfaces]))

(defn- reject! [message] (throw (ex-info message {})))

(defn- order-links [links]
  ;; Preserve peer order; an existing chain can acquire a new parent when re-aimed.
  (loop [pending links ordered []]
    (let [followers (set (map :follower pending))
          ready (filterv #(not (followers (:leader %))) pending)]
      (cond
        (empty? pending) ordered
        (empty? ready) (reject! "A moving head cannot track a loop of followers")
        :else (recur (filterv #(followers (:leader %)) pending) (into ordered ready))))))

(defn apply-operation [model {:keys [mode targets point surface-id along]}]
  (let [rig (:rig model) fixtures (:fixtures rig)
        heads (into {} (keep (fn [{:keys [id channels]}]
                              (when (and (:pan channels) (:tilt channels)) [(surfaces/wire-id id) id]))) fixtures)
        ids (mapv #(get heads (surfaces/wire-id %)) targets)
        selected (set ids)
        surface (some #(when (= (surfaces/wire-id surface-id) (surfaces/wire-id (:id %))) %) (surfaces/receivers rig))
        tracking? (#{:edge-to-edge :follow} mode)
        along (keyword (or along :u))]
    (when-not (and (vector? targets) (seq ids) (every? some? ids) (= (count selected) (count ids)))
      (reject! "Select distinct patched pan/tilt fixtures"))
    (when-not (#{:release :together :edge-to-edge :follow} mode) (reject! "Unknown surface aim mode"))
    (when-not (case mode
                :release true
                :follow surface
                :together (if (nil? surface-id)
                            (and (surfaces/xyz? point) (every? #(<= (abs %) 10000) point))
                            (and surface (surfaces/on-surface? surface point)))
                :edge-to-edge (and surface (surfaces/on-surface? surface point)))
      (reject! "Choose a valid world point or a point inside the selected receiver"))
    (when (and tracking? (not (#{:u :negative-u :v :negative-v} along))) (reject! "Choose a receiver tangent axis"))
    (when (and tracking? (< (count ids) 2)) (reject! "Select a leader and at least one follower"))
    (let [retained (filterv #(not (or (selected (:follower %)) (and (= mode :release) (selected (:leader %))))) (:edge-tracking rig))
          axis (get (surfaces/surface-axes surface) (case along :v :v :negative-v :v :u))
          tangent (if (#{:negative-u :negative-v} along) (mapv - axis) axis)
          links (if tracking?
                  (into retained (map (fn [[leader follower]] {:leader leader :follower follower :surface-id (:id surface) :along tangent})
                                      (partition 2 1 ids))) retained)
          links (order-links links)
          problems (surfaces/tracking-errors (assoc rig :edge-tracking links))
          points (apply dissoc (get-in model [:variables :surface-aim] {}) ids)
          points (case mode :together (merge points (zipmap ids (repeat point)))
                       :edge-to-edge (assoc points (first ids) point) points)]
      (when (seq problems) (throw (ex-info (:message (first problems)) {:errors problems})))
      {:model (-> model
                  (assoc-in [:rig :edge-tracking] links)
                  (assoc-in [:variables :surface-aim] points)
                  (update :revision inc))
       :ack {:status :accepted :mode mode :targets ids :surface-id (:id surface)}})))

(defn contribution [model]
  (when-let [points (not-empty (get-in model [:variables :surface-aim]))]
    (let [assignments (into [] (keep (fn [{:keys [id channels]}]
                                       (when-let [point (and (:pan channels) (:tilt channels) (get points id))]
                                         {:target-id id :attribute :aim :value point})))
                            (get-in model [:rig :fixtures]))]
      (when (seq assignments) {:kind :assignments :assignments assignments}))))
