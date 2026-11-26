(ns tolglow.studio.effect-selection
  "Perform controls for the committed effect selection projection."
  (:require [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.projection :as projection]))

(def ^:private controls [[:solo "Solo" :soloed?] [:mute "Mute" :muted?]])

(def ^:private interaction-path [:ui :effect-selection])

(defn- selection-view [db]
  (let [show (get-in db [:server-projection :view])
        identity (projection/identity-of show)
        interaction (get-in db interaction-path)
        owned (when (= identity (:identity interaction)) interaction)
        pending (:pending owned)]
    {:identity identity :selection (:effect-selection show) :interaction owned
     :pending? (boolean (and pending (or (nil? (:revision pending)) (< (:revision show) (:revision pending)))))}))

(rf/reg-sub :effect-selection/view (fn [db _] (selection-view db)))

(defn- available? [{:keys [selection pending? identity]} expected]
  (and selection (not pending?) (= identity expected)))

(def once-modes
  "One-action overrides of the configured mode, for touchscreens without a
  modifier key: [mode label title]."
  [[:additive "Add once" "Use additive mode for the next Solo or Mute"]
   [:exclusive "Exclusive once" "Make the next Solo or Mute replace the selection"]])

(rf/reg-event-db :effect-selection/once
  (fn [db [_ expected mode]]
    (let [{:keys [identity interaction] :as view} (selection-view db)]
      (if (and (available? view expected) (some #(= mode (first %)) once-modes))
        (assoc-in db interaction-path (assoc interaction :identity identity
                                             :once (when-not (= mode (:once interaction)) mode)))
        db))))

(defn- send-selection [db {:keys [identity interaction]} op fields]
  (let [command-id (str (random-uuid))
        result [:effect-selection/result identity command-id]
        [effect request] (events/command db op (assoc fields :command-id command-id) (conj result true))]
    {:db (assoc-in db interaction-path (-> interaction (dissoc :error)
                                          (assoc :identity identity :pending {:id command-id})))
     :fx [[effect (assoc request :on-failure (conj result false))]]}))

(defn click-mode
  "The one-action mode a click's modifier keys ask for: Shift adds, Alt
  (Option) makes it exclusive, otherwise nil."
  [event]
  (cond (.-shiftKey event) :additive (.-altKey event) :exclusive))

(rf/reg-event-fx :effect-selection/toggle
  (fn [{:keys [db]} [_ expected control selector clicked-mode]]
    (let [{:keys [interaction] :as view} (selection-view db)
          mode (or clicked-mode (:once interaction))]
      (when (available? view expected)
        ;; Without a mode the owner applies the configured one when the command lands.
        (send-selection db (update view :interaction dissoc :once) :effect-selection/toggle
                        (cond-> {:control control :selector selector} mode (assoc :mode mode)))))))

(rf/reg-event-fx :effect-selection/configure
  (fn [{:keys [db]} [_ expected control mode]]
    (let [view (selection-view db)]
      (when (available? view expected)
        (send-selection db view :effect-selection/configure {:modes {control mode}})))))

(rf/reg-event-db :effect-selection/result
  (fn [db [_ identity command-id success? result]]
    (let [view (selection-view db)
          interaction (:interaction view)]
      (if (and (= identity (:identity view)) (= command-id (get-in interaction [:pending :id])))
        (assoc-in db interaction-path
                  (cond
                    (and success? (= "accepted" (:status result)) (integer? (:revision result)))
                    (assoc-in interaction [:pending :revision] (:revision result))
                    success? (assoc (dissoc interaction :pending) :error "Waiting for the show. Check the displayed selection before retrying.")
                    :else (assoc (dissoc interaction :pending) :error (events/error-message result))))
        db))))

(defn by-instance [selection]
  (into {} (map (juxt :id identity)) (:instances selection)))

(defn group-flags [rows]
  (into {} (map (fn [[control _ flag]]
                  (let [selected (count (filter flag rows))]
                    [control (cond (zero? selected) :none (= selected (count rows)) :all :else :partial)])))
        controls))

(defn action-buttons [{:keys [identity pending?]} selector flags]
  (for [[control label] controls
        :let [flag (get flags control)]]
    ^{:key control}
    [:button.button.effect-selection-action
     {:type "button" :data-selection-control (name control) :disabled pending?
      :aria-pressed (case flag :partial "mixed" :all true false)
      :title (str label " · Shift-click to add, Alt-click to replace the selection")
      :on-click #(rf/dispatch [:effect-selection/toggle identity control selector (click-mode %)])}
     label]))

(defn instance-flags [row]
  (into {} (map (fn [[control _ flag]] [control (if (flag row) :all :none)])) controls))

(defn toolbar [{:keys [identity selection interaction pending?]}]
  [:div.effect-selection-tools {:role "group" :aria-label "Solo and Mute settings"}
   [:details.effect-selection-defaults
    [:summary.button {:aria-label "Solo and Mute defaults"} "Defaults"]
    [:div.effect-selection-modes
     (for [[control label] controls]
       ^{:key control}
       [:label label
        [:select {:aria-label (str label " default") :value (name (get-in selection [:modes control]))
                  :disabled pending?
                  :on-change #(rf/dispatch [:effect-selection/configure identity control (keyword (.. % -target -value))])}
         [:option {:value "exclusive"} "Exclusive"]
         [:option {:value "additive"} "Additive"]]])]]
   (for [[mode label title] once-modes]
     ^{:key mode}
     [:button.button.effect-selection-once
      {:type "button" :data-selection-once (name mode) :aria-pressed (= mode (:once interaction))
       :disabled pending? :title title
       :on-click #(rf/dispatch [:effect-selection/once identity mode])}
      label])])
