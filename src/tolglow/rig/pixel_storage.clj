(ns tolglow.rig.pixel-storage
  "Compact generated geometry at the Transit file boundary. An edited fixture
  ordering or independent calibration of a generated field still round-trips."
  (:require [tolglow.rig.pixel-layout :as layout]))

(defn- attachments [reference actual]
  (let [extras (apply dissoc actual (keys reference))]
    (when (seq extras)
      (merge (select-keys actual [:pixel-layout-id :pixel-index]) extras))))

(defn compact [rig]
  (if-not (seq (:pixel-layouts rig))
    rig
    (let [{:keys [fixtures pixel-layouts]} rig
          manual (into [] (remove :pixel-layout-id) fixtures)
          reference (mapcat layout/fixtures pixel-layouts)
          heads (drop (count manual) fixtures)]
      (if (and (= manual (take (count manual) fixtures))
               (= (count heads) (reduce + (map layout/pixel-count pixel-layouts)))
               (every? true? (map (fn [ref head] (= ref (select-keys head (keys ref)))) reference heads)))
        (assoc rig :fixtures manual ::head-attributes (into [] (keep identity) (map attachments reference heads)))
        rig))))

(defn expand [rig]
  (if-not (contains? rig ::head-attributes)
    rig
    (let [attrs (::head-attributes rig)
          sizes (into {} (map (juxt :id layout/pixel-count)) (:pixel-layouts rig))
          index-of (juxt :pixel-layout-id :pixel-index)
          valid-index? (fn [{:keys [pixel-layout-id pixel-index]}]
                         (and (integer? pixel-index) (<= 0 pixel-index)
                              (< pixel-index (get sizes pixel-layout-id 0))))]
      (when-not (and (vector? attrs) (every? valid-index? attrs)
                     (= (count attrs) (count (distinct (map index-of attrs)))))
        (throw (ex-info "Invalid saved pixel attributes" {:path [:rig ::head-attributes]})))
      (-> (dissoc rig ::head-attributes)
          (update :fixtures into attrs)
          layout/materialize))))
