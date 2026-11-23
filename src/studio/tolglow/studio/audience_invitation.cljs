(ns tolglow.studio.audience-invitation
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]))

(defn- session [db]
  [(get-in db [:show :runtime-id]) (get-in db [:show :epoch])])

(defn- state [db]
  (let [remote (get-in db [:show :connection-status :stage-invitation])
        form (get-in db [:connections :stage-form])
        sources (set (:sources remote))
        selected (:device-id form)]
    (merge remote {:stage-name "Dance floor" :origin ""} form
           {:device-id (cond (sources selected) selected
                             (sources "audience") "audience"
                             :else (first (:sources remote)))})))

(rf/reg-sub ::state (fn [db _] (state db)))
(rf/reg-event-db ::edit
  (fn [db [_ field value]] (assoc-in db [:connections :stage-form field] value)))

(rf/reg-event-fx ::operate
  (fn [{:keys [db]} [_ op]]
    (let [stage (state db)
          body (if (= op :start) (select-keys stage [:device-id :stage-name :origin]) (select-keys stage [:id]))]
      {:db (update-in db [:connections :stage-form] #(-> % (dissoc :error) (assoc :pending? true)))
       :studio/http {:path (str "/api/sensors/stage/" (name op)) :body body
                     :on-success [::complete (session db)] :on-failure [::failed (session db)]}})))

(rf/reg-event-db ::complete
  (fn [db [_ expected _]]
    (if (= expected (session db)) (update-in db [:connections :stage-form] dissoc :pending?) db)))
(rf/reg-event-db ::failed
  (fn [db [_ expected error]]
    (if (= expected (session db))
      (update-in db [:connections :stage-form] #(-> % (dissoc :pending?) (assoc :error (events/error-message error))))
      db)))

(def phase-label {"idle" "Closed" "starting" "Starting" "discoverable" "Discoverable"
                  "bluetooth-unavailable" "Bluetooth unavailable" "advertising-failed" "Advertising failed"
                  "exited" "Bridge exited"})

(defn- edit [field event]
  (rf/dispatch-sync [::edit field (.. event -target -value)]))

(defn- setup [{:keys [stage-name origin device-id sources receiver? pending?]}]
  [:div
   [:div.connection-fields
    [:label "Stage name" [:input {:value stage-name :max-length 48 :on-change #(edit :stage-name %)}]]
    [:label "Invocation origin" [:input {:type "url" :value origin :placeholder "https://lights.example.org"
                                         :on-change #(edit :origin %)}]]
    [:label "Enrolled sensor"
     [:select {:value (or device-id "") :on-change #(edit :device-id %)}
      (when (empty? sources) [:option {:value ""} "Pair a sensor"])
      (for [id sources] ^{:key id} [:option {:value id} id])]]]
   (when-not receiver? [:p.small "Start an OSC receiver on loopback or all interfaces to host this stage."])
   [:button.button.primary {:type "button" :disabled (or pending? (not receiver?) (nil? device-id)
                                                         (str/blank? origin) (str/blank? stage-name))
                            :on-click #(rf/dispatch [::operate :start])} "Start stage"]])

(defn- running [{:keys [url id stage-name device-id pending?]}]
  (let [png (str "/api/sensors/stage/qr?id=" id)]
    [:div
     [:p (str stage-name " · " device-id)]
     (when url [:div.stage-invitation-qr
                [:img {:src png :width 160 :height 160 :alt "Scan to join this stage"}]
                [:div [:p "This QR expires when the stage stops or the show reloads."]
                 [:p [:a.button {:href png :download "tolglow-stage.png"} "Save QR image"]]]])
     [:button.button {:type "button" :disabled pending? :on-click #(rf/dispatch [::operate :stop])} "Stop stage"]]))

(defn panel []
  (let [{:keys [available? phase id error] :as stage} @(rf/subscribe [::state])]
    (when available?
      [:section.panel.stage-invitation
       [:div.view-heading [:h2 "Stage invitation"] [:span.badge {:role "status"} (get phase-label phase phase)]]
       (when error [:p.connection-error {:role "alert"} error])
       (if id [running stage] [setup stage])])))
