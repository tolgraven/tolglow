(ns tolglow.studio.checkpoints
  "File actions for the current owner's saved show checkpoints."
  (:require [re-frame.core :as rf]
            [tolglow.studio.projection :as projection]))

(def ^:private checkpoints
  (array-map :recovery "Working autosave" :named "Named save" :prior "Previous session"))

(defn- checkpoint-name [kind] (get checkpoints kind))
(defn- identity-of [db] (projection/identity-of (get-in db [:server-projection :view])))
(defn- connected? [db] (and (get-in db [:connection :connected?]) (not (:awaiting-full? db))))
(defn- catalog [db]
  (let [catalog (get-in db [:show-checkpoints :catalog])]
    (when (= (identity-of db) (projection/identity-of catalog)) catalog)))

(defn- update-panel [db f & args]
  (apply update db :show-checkpoints f args))

(defn- finish [db message error?]
  (if (get-in db [:show-checkpoints :open?])
    {:db (update-panel db #(-> % (dissoc :operation) (assoc :feedback {:message message :error? error?})))}
    {:db (dissoc db :show-checkpoints) :dispatch [:studio/toast message error?]}))

(defn- restored [db kind]
  (-> db
      (assoc-in [:ui :rig-dirty?] false)
      (assoc-in [:ui :rig-draft] (get-in db [:show :rig]))
      (finish (str "Restored " (checkpoint-name kind) ".") false)))

(defn- same-show? [a b]
  (= (dissoc a :epoch) (dissoc b :epoch)))

(rf/reg-event-fx :checkpoints/open
  (fn [{:keys [db]} _]
    {:db (update-panel db #(-> % (dissoc :confirmation :feedback) (assoc :open? true)))
     :dispatch [:checkpoints/refresh]}))

(rf/reg-event-db :checkpoints/close
  (fn [db _]
    (if-let [operation (get-in db [:show-checkpoints :operation])]
      (assoc db :show-checkpoints {:operation operation})
      (dissoc db :show-checkpoints))))

(rf/reg-event-fx :checkpoints/refresh [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    (when (and (get-in db [:show-checkpoints :open?]) (connected? db))
      (let [identity (identity-of db)]
        {:db (update-panel db #(-> % (dissoc :catalog :confirmation :catalog-error)
                                  (assoc :catalog-request {:id unique :identity identity})))
         :studio/http {:path "/api/show/checkpoints" :raw-response? true
                       :on-success [:checkpoints/catalog unique]
                       :on-failure [:checkpoints/catalog-failed unique]}}))))

(rf/reg-event-db :checkpoints/catalog
  (fn [db [_ id response]]
    (let [{request-id :id identity :identity} (get-in db [:show-checkpoints :catalog-request])]
      (if (= id request-id)
        (update-panel db
                      #(cond-> (dissoc % :catalog-request)
                         (= identity (identity-of db))
                         (merge (if (and (= identity (projection/identity-of response))
                                         (map? (:checkpoints response)))
                                  {:catalog response}
                                  {:catalog-error "Show checkpoints are unavailable. Refresh to try again."}))))
        db))))

(rf/reg-event-db :checkpoints/catalog-failed
  (fn [db [_ id message]]
    (let [{request-id :id identity :identity} (get-in db [:show-checkpoints :catalog-request])]
      (if (= id request-id)
        (update-panel db
                      #(cond-> (dissoc % :catalog-request)
                         (= identity (identity-of db))
                         (assoc :catalog-error message)))
        db))))

(defn- actionable? [db action kind]
  (and (get-in db [:show-checkpoints :open?]) (connected? db)
       (nil? (get-in db [:show-checkpoints :operation]))
       (or (and (= :restore action) (checkpoint-name kind))
           (and (= :discard action) (= :prior kind)))
       (get-in (catalog db) [:checkpoints kind])))

(rf/reg-event-db :checkpoints/confirm
  (fn [db [_ action kind]]
    (if (actionable? db action kind)
      (assoc-in db [:show-checkpoints :confirmation] {:action action :kind kind :identity (identity-of db)})
      db)))

(rf/reg-event-db :checkpoints/cancel
  (fn [db _] (update-panel db dissoc :confirmation)))

(rf/reg-event-fx :checkpoints/submit [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    (let [{:keys [action kind identity] :as confirmation} (get-in db [:show-checkpoints :confirmation])]
      (when (and (= identity (identity-of db)) (actionable? db action kind))
        {:db (update-panel db #(-> % (dissoc :confirmation :feedback)
                                  (assoc :operation (assoc confirmation :id unique :phase :sending))))
         :studio/http {:path (if (= :restore action) "/api/show/checkpoints/restore"
                                 "/api/show/checkpoints/discard-prior")
                       :typed? true :raw-response? true :timeout-ms 12000
                       :body (cond-> identity (= :restore action) (assoc :kind kind))
                       :on-success [:checkpoints/result unique]
                       :on-failure [:checkpoints/failed unique]}}))))

(defn- with-refresh [effect]
  (if (get-in effect [:db :show-checkpoints :open?])
    (assoc effect :dispatch [:checkpoints/refresh])
    effect))

(rf/reg-event-fx :checkpoints/result
  (fn [{:keys [db]} [_ id {:keys [status epoch message]}]]
    (let [{request-id :id :keys [action kind identity]} (get-in db [:show-checkpoints :operation])
          current (identity-of db)]
      (when (= id request-id)
        (cond
          (and (= :restore action) (#{:accepted "accepted"} status) (integer? epoch)
               (< (:epoch identity) epoch))
          (cond
            (= current (assoc identity :epoch epoch)) (with-refresh (restored db kind))
            (= current identity) {:db (update-in db [:show-checkpoints :operation]
                                                 assoc :phase :awaiting-view :epoch epoch)}
            :else (finish db "The show changed before Studio could confirm the restored view." true))

          (not= identity current)
          (finish db "The show changed before Studio received the checkpoint result." true)

          (and (= :discard action) (#{:accepted "accepted"} status))
          (with-refresh (finish db "Previous session checkpoint discarded." false))

          (and (= :restore action) (#{:pending "pending"} status))
          {:db (assoc-in db [:show-checkpoints :operation :phase] :queued)}

          :else (finish db (or message "The show did not acknowledge the checkpoint action.") true))))))

(rf/reg-event-fx :checkpoints/failed
  (fn [{:keys [db]} [_ id message]]
    (let [{request-id :id identity :identity} (get-in db [:show-checkpoints :operation])]
      (when (= id request-id)
        (finish db (if (= identity (identity-of db)) message
                       "The show changed before Studio received the checkpoint result.") true)))))

(rf/reg-event-fx :checkpoints/show-changed
  (fn [{:keys [db]} _]
    (when-let [{:keys [open? operation]} (:show-checkpoints db)]
      (let [{:keys [identity action kind phase epoch]} operation
            current (identity-of db)
            db (update-panel db dissoc :confirmation :catalog :catalog-request)
            effect (cond
                     (or (nil? operation) (= identity current)) {:db db}
                     (and (= :awaiting-view phase) (= current (assoc identity :epoch epoch))) (restored db kind)
                     (and (= :sending phase) (= :restore action) (same-show? identity current)) {:db db}
                     (= :queued phase) (finish db "Studio received a newer show while the restore was queued." false)
                     :else (finish db "The show changed while the checkpoint action was in progress." true))]
        (if open? (with-refresh effect) effect)))))

(defn- open-dialog! [node]
  (when (and node (not (.-open node))) (.showModal node)))

(defn- close-dialog! [event]
  (.close (.closest (.-currentTarget event) "dialog")))

(defn- timestamp [value]
  (when (and (number? value) (js/Number.isFinite value))
    (let [date (js/Date. value)]
      (when (js/Number.isFinite (.getTime date))
        [:time {:date-time (.toISOString date)} (.toLocaleString date)]))))

(defn view-props [db]
  {:panel (:show-checkpoints db) :identity (identity-of db) :connected? (connected? db)})

(defn panel [{:keys [panel identity connected?]}]
  (when-let [{:keys [open? operation confirmation catalog-request catalog-error catalog feedback]} panel]
    (when open?
      (let [{:keys [enabled?] :as current} (when (= identity (projection/identity-of catalog)) catalog)
            confirmation (when (= (:identity confirmation) identity) confirmation)
            {:keys [action kind]} confirmation
            discard? (= :discard action)
            busy? (or operation catalog-request)]
        [:dialog.show-checkpoint-dialog {:ref open-dialog! :aria-labelledby "show-checkpoint-title"
                                        :on-close #(rf/dispatch [:checkpoints/close])}
         [:div.dialog-header
          [:h2#show-checkpoint-title "Show checkpoints"]
          [:button.icon-button {:type "button" :aria-label "Close show checkpoints" :on-click close-dialog!} "×"]]
         [:p.small.checkpoint-status {:role "status" :class (when (or catalog-error (:error? feedback)) "checkpoint-error")}
          (cond
            (= :sending (:phase operation)) (if (= :restore (:action operation)) "Restoring checkpoint…" "Discarding checkpoint…")
            (= :queued (:phase operation)) "Restore is queued."
            (= :awaiting-view (:phase operation)) "Restore accepted. Waiting for Studio to receive the restored show."
            (not connected?) "Connect to the show to view its checkpoints."
            catalog-request "Loading checkpoints…"
            catalog-error catalog-error
            feedback (:message feedback)
            (and current (false? enabled?)) "Automatic recovery is off.")]
         (if confirmation
           [:div.checkpoint-confirm {:aria-live "polite"}
            [:p (if discard?
                  "Discard the previous session checkpoint? This cannot be undone."
                  (str "Restore " (checkpoint-name kind) "? This replaces the active show, including unsaved changes."))]
            [:div.button-row
             [:button.button {:type "button" :auto-focus true :on-click #(rf/dispatch [:checkpoints/cancel])} "Cancel"]
             [:button.button {:type "button" :class (if discard? "danger" "primary")
                              :disabled (or (boolean busy?) (not connected?) (not (get-in current [:checkpoints kind])))
                              :on-click #(rf/dispatch [:checkpoints/submit])}
              (if discard? "Discard checkpoint" "Restore checkpoint")]]]
           [:<>
            (when current
              [:ul.checkpoint-list
               (for [[kind label] checkpoints
                     :let [entry (get-in current [:checkpoints kind])]]
                 ^{:key kind}
                 [:li.checkpoint-row
                  [:div [:strong label]
                   [:div.small.muted (if entry (or (timestamp (:modified-at-ms entry)) "Time unavailable") "Unavailable")]]
                  [:div.checkpoint-actions
                   [:button.button {:type "button" :aria-label (str "Restore " label)
                                    :disabled (or (boolean busy?) (not entry) (not connected?))
                                    :on-click #(rf/dispatch [:checkpoints/confirm :restore kind])} "Restore"]
                   (when (and entry (= :prior kind))
                     [:button.button.danger {:type "button" :aria-label "Discard Previous session checkpoint"
                                             :disabled (or (boolean busy?) (not connected?))
                                             :on-click #(rf/dispatch [:checkpoints/confirm :discard kind])} "Discard"])]])])
            [:div.button-row
             [:button.button {:type "button" :disabled (or (some? catalog-request) (not connected?))
                              :on-click #(rf/dispatch [:checkpoints/refresh])} "Refresh"]]])]))))
