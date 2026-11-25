(ns tolglow.rig.surfaces
  "Finite, world-space receiving planes shared by interactive aim and the preview."
  (:require [clojure.string :as str]))

(defn wire-id [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn- finite? [n] (and (number? n) (Double/isFinite (double n))))

(defn xyz? [v] (and (vector? v) (= 3 (count v)) (every? finite? v)))

(defn- dot [a b] (reduce + (map * a b)))

(defn- cross [[a b c] [x y z]] [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))])

(defn- unit [v]
  (when (xyz? v)
    (let [m (Math/hypot (Math/hypot (double (v 0)) (double (v 1))) (double (v 2)))]
      (when (and (Double/isFinite m) (pos? m)) (mapv #(/ % m) v)))))

(defn surface-axes [{:keys [normal u-axis]}]
  (when-let [n (unit normal)]
    (when-let [horizontal (unit u-axis)]
      (when-let [v (unit (cross n horizontal))]
        {:n n :u (cross v n) :v v}))))

(defn on-surface? [{:keys [point size] :as surface} world]
  (when (xyz? world)
    (let [{:keys [n u v]} (surface-axes surface) diff (mapv - world point)
          [w h] size tol 1.0e-6]
      (and (<= (abs (dot n diff)) tol)
           (<= (abs (dot u diff)) (+ tol (/ w 2)))
           (<= (abs (dot v diff)) (+ tol (/ h 2)))))))

(defn receivers [{:keys [room surfaces]}]
  (let [[w h d] (mapv double ((juxt :width :height :depth) (merge {:width 12 :height 5 :depth 10} room)))
        stage-depth (min 2.3 (* d 0.27))]
    (into [{:id :floor :point [0 0 0] :normal [0 1 0] :u-axis [1 0 0] :size [w d]}
           {:id :back :point [0 (/ h 2) (/ d -2)] :normal [0 0 1] :u-axis [1 0 0] :size [w h]}
           {:id :left :point [(/ w -2) (/ h 2) 0] :normal [1 0 0] :u-axis [0 0 -1] :size [d h]}
           {:id :right :point [(/ w 2) (/ h 2) 0] :normal [-1 0 0] :u-axis [0 0 1] :size [d h]}
           {:id :stage :point [0 0.42 (+ (/ d -2) (/ stage-depth 2) 0.16)] :normal [0 1 0]
            :u-axis [1 0 0] :size [(* w 0.68) stage-depth]}] surfaces)))

(defn- problem [path text] {:path path :code :invalid-receiver :message text})

(defn surface-errors [rig]
  (when (contains? rig :surfaces)
    (let [ss (:surfaces rig)]
      (if-not (vector? ss)
        [(problem [:surfaces] "Receiver surfaces must be a vector")]
        (concat
          (when-not (= (+ 5 (count ss)) (count (set (map (comp wire-id :id) (receivers {:surfaces ss})))))
            [(problem [:surfaces] "Receiver IDs must be unique, including the room surfaces")])
          (keep-indexed
           (fn [i {:keys [id point size] :as s}]
             (when-not (and (or (keyword? id) (and (string? id) (not (str/blank? id))))
                            (xyz? point) (surface-axes s)
                            (vector? size) (= 2 (count size)) (every? #(and (finite? %) (pos? %)) size))
               (problem [:surfaces i] "Supply a receiver ID, finite center, independent normal and u-axis, and positive width and height")))
           ss))))))

(defn resolve-link-ids
  "Restore stored fixture and surface identities when an edited rig returns from a textual UI."
  [rig]
  (if (vector? (:edge-tracking rig))
    (let [index (fn [entries] (into {} (map (juxt (comp wire-id :id) :id)) entries))
          heads (index (:fixtures rig)) planes (index (receivers (dissoc rig :room)))
          resolve-id (fn [ids id] (get ids (wire-id id) id))]
      (update rig :edge-tracking
              #(mapv (fn [link] (if-not (map? link) link
                                  (-> link (update :leader (partial resolve-id heads))
                                      (update :follower (partial resolve-id heads))
                                      (update :surface-id (partial resolve-id planes))))) %))) rig))

(defn tracking-errors [{:keys [fixtures edge-tracking] :as rig}]
  (when (contains? rig :edge-tracking)
    (if-not (vector? edge-tracking)
      [(problem [:edge-tracking] "Edge tracking must be an ordered vector")]
      (let [heads (into #{} (keep #(when (and (get-in % [:channels :pan]) (get-in % [:channels :tilt])) (:id %))) fixtures)
            planes (into {} (map (juxt :id identity)) (receivers rig))
            followers (mapv :follower edge-tracking)]
        (concat
          (when-not (= (count followers) (count (distinct followers)))
            [(problem [:edge-tracking] "A follower can track only one leader")])
          (keep-indexed
           (fn [i {:keys [leader follower surface-id along]}]
             (let [surface (get planes surface-id) a (unit along) n (some-> surface surface-axes :n)]
               (when-not (and (heads leader) (heads follower) (not= leader follower)
                              (not (some #{leader} (subvec followers (inc i))))
                              a n (< (abs (dot a n)) 1.0e-6))
                 (problem [:edge-tracking i] "Order links leader-first and choose distinct moving heads, an existing receiver and a nonzero tangent"))))
           edge-tracking))))))
