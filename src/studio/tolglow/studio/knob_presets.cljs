(ns tolglow.studio.knob-presets
  "Knob looks strip (#547). Reads the shared :knob-presets projection and sends
  the same :knob-preset/* owner commands as the REPL and the Push top pad row.
  See docs/linked-knobs.md#knob-looks."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]))

(def ^:private ui-path [:ui :knob-presets])

(defn slot-state
  "Wire-safe slot state keyword: :empty, :stored or :selected."
  [slot]
  (keyword (events/wire-id (:state slot "empty"))))

(defn slot-summary
  "One line naming each stored knob and its saved position, for the slot title."
  [slot]
  (if (= :empty (slot-state slot))
    "Empty"
    (str/join " · " (map (fn [{:keys [name knob-id text missing?]}]
                           (str (or name (events/wire-id knob-id)) " " text (when missing? " (removed)")))
                         (:knobs slot)))))

(defn slot-actions
  "Commands offered for a slot. Each carries the shown contents as :expected,
  so a slot changed elsewhere meanwhile is refused instead of replaced."
  [slot]
  (let [slot-index (:slot slot)
        values (:values slot)]
    (if (= :empty (slot-state slot))
      [[:save "Save" {:op :knob-preset/capture :slot slot-index :expected nil}]]
      [[:recall "Recall" {:op :knob-preset/recall :slot slot-index :expected values}]
       [:overwrite "Overwrite" {:op :knob-preset/capture :slot slot-index :overwrite? true :expected values}]
       [:clear "Clear" {:op :knob-preset/clear :slot slot-index :expected values}]])))

(rf/reg-event-db ::failed
  (fn [db [_ message]] (assoc-in db (conj ui-path :error) (str message))))

(rf/reg-event-fx ::send
  (fn [{:keys [db]} [_ {:keys [op] :as command}]]
    (let [[effect request] (events/command db op (dissoc command :op) nil nil)]
      {:db (update-in db ui-path dissoc :error)
       :fx [[effect (assoc request :on-failure [::failed])]]})))

(defn- slot-card [slot connected?]
  (let [state (slot-state slot)
        moved? (and (= :selected state) (false? (:matches? slot)))
        label (str "Look " (inc (:slot slot)))]
    [:li.knob-look {:data-state (name state) :data-moved (when moved? "true")
                    :aria-label (str label ", " (name state) (when moved? ", knobs moved since"))}
     [:div.knob-look-head [:strong label]
      [:span.knob-look-state (case state :empty "empty" :stored "saved" :selected (if moved? "selected · moved" "selected"))]]
     [:p.knob-look-summary.small (slot-summary slot)]
     [:div.knob-look-actions
      (for [[id title command] (slot-actions slot)]
        ^{:key id} [:button.text-button {:type "button" :disabled (not connected?)
                                         :aria-label (str title " " label)
                                         :on-click #(rf/dispatch [::send command])} title])]]))

(defn panel
  "Eight knob looks beside the Knobs panel, matching the Push top pad row."
  [db]
  (let [slots (get-in db [:show :knob-presets :slots])
        connected? (get-in db [:connection :connected?])
        error (get-in db (conj ui-path :error))]
    (when (or (seq (get-in db [:show :knobs :knobs])) (some #(not= :empty (slot-state %)) slots))
      [:section.knob-looks-panel.panel {:aria-label "Knob looks"}
       [:div.panel-top [:strong "Knob looks"] [:span.small.muted "Saved knob positions · Push top pad row"]]
       [:ol.knob-looks
        (for [slot slots] ^{:key (:slot slot)} [slot-card slot connected?])]
       (when error [:p.knob-error {:role "alert"} error])])))
