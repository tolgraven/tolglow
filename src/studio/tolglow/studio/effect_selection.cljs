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

(rf/reg-event-db :effect-selection/add-once
  (fn [db [_ expected]]
    (let [{:keys [identity interaction] :as view} (selection-view db)]
      (if (available? view expected)
        (assoc-in db interaction-path (assoc interaction :identity identity :add-once? (not (:add-once? interaction))))
        db))))

(defn- send-selection [db {:keys [identity interaction]} op fields]
  (let [command-id (str (random-uuid))
        result [:effect-selection/result identity command-id]
        [effect request] (events/command db op (assoc fields :command-id command-id) (conj result true))]
    {:db (assoc-in db interaction-path (-> interaction (dissoc :error)
                                          (assoc :identity identity :pending {:id command-id})))
     :fx [[effect (assoc request :on-failure (conj result false))]]}))

(rf/reg-event-fx :effect-selection/toggle
  (fn [{:keys [db]} [_ expected control selector shifted?]]
    (let [{:keys [interaction] :as view} (selection-view db)]
      (when (available? view expected)
        (send-selection db (update view :interaction dissoc :add-once?) :effect-selection/toggle
                        {:control control :selector selector :additive? (boolean (or shifted? (:add-once? interaction)))})))))

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
      :title (str label " · Shift-click to add")
      :on-click #(rf/dispatch [:effect-selection/toggle identity control selector (.-shiftKey %)])}
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
   [:button.button.effect-selection-add
    {:type "button" :aria-pressed (boolean (:add-once? interaction)) :disabled pending?
     :title "Use additive mode for the next Solo or Mute"
     :on-click #(rf/dispatch [:effect-selection/add-once identity])}
    "Add once"]])
