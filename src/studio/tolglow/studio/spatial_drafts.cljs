(ns tolglow.studio.spatial-drafts
  "Spatial authoring state scoped to one show, runtime and epoch."
  (:require [tolglow.studio.projection :as projection]))

(defn owner [db] (projection/identity-of (get-in db [:server-projection :view])))

(defn state [db]
  (let [ui (::ui db) id (owner db)]
    (if (= id (:owner ui)) ui {:owner id})))

(defn update-ui [db f & args]
  (assoc db ::ui (apply f (state db) args)))

(defn settle [db]
  (let [ui (::ui db)
        {:keys [path submitted revision]} (:acknowledged ui)
        projected (get-in db [:server-projection :view :revision])]
    (cond
      (nil? ui) db
      (not= (:owner ui) (owner db)) (dissoc db ::ui)
      (and revision (number? projected) (>= projected revision))
      (assoc db ::ui (cond-> (dissoc ui :pending :acknowledged)
                       (= submitted (get-in ui path))
                       (update-in (pop path) dissoc (peek path))))
      :else db)))

(defn acknowledge [db [_ request-owner token message path submitted selected receipt]]
  (let [ui (state db)
        revision (:revision receipt)]
    (cond
      (or (not= request-owner (:owner ui)) (not= token (:pending ui))) db
      (not (integer? revision))
      (update-ui db assoc :pending nil :message (or (:message receipt) "Waiting for save confirmation."))
      :else
      (-> db
          (assoc ::ui (cond-> (assoc ui :message message)
                        selected (assoc :chosen selected)
                        path (assoc :acknowledged {:path path :submitted submitted :revision revision})
                        (nil? path) (dissoc :pending)))
          settle))))
