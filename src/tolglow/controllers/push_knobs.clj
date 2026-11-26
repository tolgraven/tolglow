(ns tolglow.controllers.push-knobs
  "Push display data for linked knobs (#666). The Afterglow adapters send
  :knob/nudge and name a touched knob or the shown knob page in the view; this
  adds the knob's resolved values from the same projection Studio reads."
  (:require [tolglow.knobs :as knobs]))

(defn- page-entry [model knob-id]
  (if-let [{:keys [name value text columns]} (knobs/encoder-display model knob-id)]
    {:knob-id knob-id :name name :value value :text text :links (count columns)}
    {:knob-id knob-id :missing? true}))

(defn project-display
  "Add :knob for a touched knob encoder and the knobs of a shown Push 2 knob
  page to a device view. A knob that no longer exists is marked, not dropped."
  [model view]
  (let [{:keys [target knob-id]} (:touched-control view)
        page (get-in view [:knob-page :knob-ids])]
    (cond-> view
      (= :knob target)
      (assoc :knob (or (knobs/encoder-display model knob-id) {:knob-id knob-id :missing? true}))
      (seq page)
      (assoc-in [:knob-page :knobs] (mapv #(page-entry model %) page)))))
