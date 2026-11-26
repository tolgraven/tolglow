(ns tolglow.studio.checkpoints
  "File actions for the current owner's saved show checkpoints."
  (:require [re-frame.core :as rf]
            [tolglow.studio.projection :as projection]))

(def ^:private checkpoints
  (array-map :recovery "Working autosave" :named "Named save" :prior "Previous session"
             :legacy "Checkpoint from before the update"))

(def ^:private dismissible
  "Checkpoints an operator can drop, and the route that drops each."
  {:prior "/api/show/checkpoints/discard-prior" :legacy "/api/show/checkpoints/dismiss-legacy"})

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

(defn- rig-draft [db] (get-in db [:ui :rig-draft]))

(defn- original-rig-draft? [db]
  (let [operation (get-in db [:show-checkpoints :operation])]
    (and (get-in db [:ui :rig-dirty?])
         (contains? operation :draft-before-restore)
         (identical? (:draft-before-restore operation) (rig-draft db)))))

(defn carry-restore-draft [before updated]
  ;; A streamed view can update measured fog in the draft without editing it.
  (cond-> updated
    (and (original-rig-draft? before) (get-in updated [:ui :rig-dirty?]))
    (assoc-in [:show-checkpoints :operation :draft-before-restore] (rig-draft updated))))

(defn- restored [db kind]
  (let [db (cond-> db
             (original-rig-draft? db)
             (update :ui assoc :rig-dirty? false :rig-draft (get-in db [:show :rig])))]
    (finish db (str "Restored " (checkpoint-name kind) ".") false)))

(defn- same-show? [a b]
  (= (dissoc a :epoch) (dissoc b :epoch)))

(defn queued-result [db]
  (let [{:keys [identity phase receipt-id]} (get-in db [:show-checkpoints :operation])]
    (when (and (= :queued phase) (same-show? identity (identity-of db)))
      (some #(when (and (= receipt-id (:id %)) (= identity (:identity %))) %)
            (get-in db [:server-projection :view :checkpoint-results])))))

(defn replaces-rig-draft? [db view]
  (let [{:keys [action identity phase epoch]} (get-in db [:show-checkpoints :operation])]
    (and (= :restore action) (= :awaiting-view phase) (= identity (identity-of db))
         (original-rig-draft? db)
         (= (assoc identity :epoch epoch) (projection/identity-of view)))))

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
           (and (= :discard action) (contains? dismissible kind)))
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
    (let [{:keys [action kind identity] :as confirmation} (get-in db [:show-checkpoints :confirmation])
          operation (cond-> (assoc confirmation :id unique :phase :sending)
                      (and (= :restore action) (get-in db [:ui :rig-dirty?]))
                      (assoc :draft-before-restore (rig-draft db)))]
      (when (and (= identity (identity-of db)) (actionable? db action kind))
        {:db (update-panel db #(-> % (dissoc :confirmation :feedback)
                                  (assoc :operation operation)))
         :dispatch-later [{:ms 60000 :dispatch [:checkpoints/expire unique]}]
         :studio/http {:path (if (= :restore action) "/api/show/checkpoints/restore" (dismissible kind))
                       :typed? true :raw-response? true :timeout-ms 12000
                       :body (cond-> identity (= :restore action) (assoc :kind kind))
                       :on-success [:checkpoints/result unique]
                       :on-failure [:checkpoints/failed unique]}}))))

(defn- with-refresh [effect]
  (if (get-in effect [:db :show-checkpoints :open?])
    (assoc effect :dispatch [:checkpoints/refresh])
    effect))

(defn- restore-result [db {:keys [status epoch message reason]}]
  (let [{:keys [kind identity]} (get-in db [:show-checkpoints :operation])
        current (identity-of db)]
    (if (and (#{:accepted "accepted"} status) (integer? epoch) (< (:epoch identity) epoch))
      (cond
        (= current (assoc identity :epoch epoch)) (with-refresh (restored db kind))
        (= current identity) {:db (update-in db [:show-checkpoints :operation]
                                             assoc :phase :awaiting-view :epoch epoch)}
        :else (finish db "The show changed before Studio could confirm the restored view." true))
      (finish db (cond
                   (= :stale-show-session reason) "The show changed before the queued restore could run."
                   (not= identity current) "The show changed before Studio received the checkpoint result."
                   :else (or message "The show rejected the checkpoint restore.")) true))))

(rf/reg-event-fx :checkpoints/result
  (fn [{:keys [db]} [_ id {:keys [status receipt-id message] :as result}]]
    (let [{request-id :id :keys [action identity]} (get-in db [:show-checkpoints :operation])
          current (identity-of db)]
      (when (= id request-id)
        (cond
          (= :restore action)
          (if (and (#{:pending "pending"} status) (string? receipt-id) (same-show? identity current))
            (let [queued (update-in db [:show-checkpoints :operation] assoc :phase :queued :receipt-id receipt-id)]
              (if-let [receipt (queued-result queued)] (restore-result queued receipt) {:db queued}))
            (restore-result db result))

          (not= identity current)
          (finish db "The show changed before Studio received the checkpoint result." true)

          (and (= :discard action) (#{:accepted "accepted"} status))
          (with-refresh (finish db (if (= :legacy (get-in db [:show-checkpoints :operation :kind]))
                                     "Checkpoint from before the update dismissed."
                                     "Previous session checkpoint discarded.") false))

          :else (finish db (or message "The show did not acknowledge the checkpoint action.") true))))))

(rf/reg-event-fx :checkpoints/receipt-pushed
  (fn [{:keys [db]} _]
    (when-let [receipt (queued-result db)] (restore-result db receipt))))

(rf/reg-event-fx :checkpoints/expire
  (fn [{:keys [db]} [_ id]]
    (when (= id (get-in db [:show-checkpoints :operation :id]))
      (finish db "Studio could not confirm the checkpoint action. Check the active show before retrying." true))))

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
            receipt (queued-result db)
            effect (cond
                     receipt (restore-result db receipt)
                     (or (nil? operation) (= identity current)) {:db db}
                     (and (= :awaiting-view phase) (= current (assoc identity :epoch epoch))) (restored db kind)
                     (and (#{:sending :queued} phase) (= :restore action) (same-show? identity current)) {:db db}
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

(def ^:private legacy-note
  (str "Tolglow autosaved here before each show folder had its own recovery. Restoring it applies it to "
       "this folder only, and other folders stop offering it. Its file stays in the shared recovery folder."))

(defn- confirmation-text [action kind]
  (case [action kind]
    [:discard :prior] "Discard the previous session checkpoint? This cannot be undone."
    [:discard :legacy] (str "Dismiss the checkpoint from before the update? No show folder will offer it again. "
                            "Its file stays in the shared recovery folder.")
    [:restore :legacy] (str "Restore the checkpoint from before the update into this show folder? This replaces "
                            "the active show, including unsaved changes, and other show folders stop offering it.")
    (str "Restore " (checkpoint-name kind) "? This replaces the active show, including unsaved changes.")))

(defn folder-explanation
  "What `.tolglow-folder-id` is, for the first-open notice and Show checkpoints."
  [{:keys [file copy-of]}]
  (let [file (or file ".tolglow-folder-id")]
    [:div.show-folder-explanation
     [:p "Tolglow keeps a small file, " [:code file] ", in this show folder. It names the folder's own "
      "recovery checkpoints, so they stay apart from other show folders and follow the folder when you "
      "rename or move it."]
     (when copy-of
       [:p "This folder is a copy of " [:code copy-of] ", so it was given its own identity. The copy and "
        "the original keep separate checkpoints."])
     [:p "Copies are detected and safe: a duplicated, backed-up or cloned folder gets its own identity the "
      "first time it opens, so neither overwrites the other's recovery. In a git repository you can ignore "
      "the file or commit it."]]))

(defn notice-props
  "What the show folder notice needs: the folder this show runs from and the
  notices already closed on this page."
  [db]
  {:folder (get-in db [:show :show-folder]) :closed (get-in db [:ui :show-folder-notices] #{})})

(rf/reg-event-db :show-folder/close-notice
  (fn [db [_ notice]] (update-in db [:ui :show-folder-notices] (fnil conj #{}) notice)))

(defn- notice-key [folder kind] [(:id folder) kind])

(defn folder-notice
  "The once-per-folder note about `.tolglow-folder-id` on the folder's first
  open (or when a copy was detected), and the offer of the checkpoint from
  before the update while one is waiting. Neither applies anything."
  [{:keys [folder closed]}]
  (when (map? folder)
    (let [identity? (and (:new? folder) (not (contains? closed (notice-key folder :identity))))
          legacy? (and (:legacy-checkpoint folder) (not (contains? closed (notice-key folder :legacy))))]
      (when (or identity? legacy?)
        [:section.show-folder-notice {:role "region" :aria-label "Show folder"}
         (when identity?
           [:div.show-folder-notice-part {:data-notice "identity"}
            [:strong (if (:copy-of folder) "This copied show folder has its own recovery"
                         "This show folder now has its own recovery")]
            [folder-explanation folder]
            [:p.small.muted "Read this again under Restore show, in Show folder."]
            [:div.button-row
             [:button.button {:type "button" :on-click #(rf/dispatch [:show-folder/close-notice (notice-key folder :identity)])}
              "OK"]]])
         (when legacy?
           [:div.show-folder-notice-part {:data-notice "legacy"}
            [:strong "A checkpoint from before the update is waiting"]
            [:p (str "Tolglow saved it before each show folder had its own recovery. Nothing has been applied. "
                     "Restore it into this folder or dismiss it under Restore show.")]
            [:div.button-row
             [:button.button.primary {:type "button" :on-click #(rf/dispatch [:checkpoints/open])} "Review checkpoint"]
             [:button.button {:type "button" :on-click #(rf/dispatch [:show-folder/close-notice (notice-key folder :legacy)])}
              "Later"]]])]))))

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
            (not connected?) "Connect to the show to view its checkpoints."
            (= :sending (:phase operation)) (cond (= :restore (:action operation)) "Restoring checkpoint…"
                                                  (= :legacy (:kind operation)) "Dismissing checkpoint…"
                                                  :else "Discarding checkpoint…")
            (= :queued (:phase operation)) "Restore is queued."
            (= :awaiting-view (:phase operation)) "Restore accepted. Waiting for Studio to receive the restored show."
            catalog-request "Loading checkpoints…"
            catalog-error catalog-error
            feedback (:message feedback)
            (and current (false? enabled?)) "Automatic recovery is off.")]
         (if confirmation
           [:div.checkpoint-confirm {:aria-live "polite"}
            [:p (confirmation-text action kind)]
            [:div.button-row
             [:button.button {:type "button" :auto-focus true :on-click #(rf/dispatch [:checkpoints/cancel])} "Cancel"]
             [:button.button {:type "button" :class (if discard? "danger" "primary")
                              :disabled (or (boolean busy?) (not connected?) (not (get-in current [:checkpoints kind])))
                              :on-click #(rf/dispatch [:checkpoints/submit])}
              (cond (not discard?) "Restore checkpoint" (= :legacy kind) "Dismiss checkpoint" :else "Discard checkpoint")]]]
           [:<>
            (when current
              [:ul.checkpoint-list
               (for [[kind label] checkpoints
                     :let [entry (get-in current [:checkpoints kind])]
                     ;; Only a Mac show folder can offer the shared checkpoint.
                     :when (or entry (not= :legacy kind))]
                 ^{:key kind}
                 [:li.checkpoint-row {:data-checkpoint (name kind)}
                  [:div [:strong label]
                   [:div.small.muted (if entry (or (timestamp (:modified-at-ms entry)) "Time unavailable") "Unavailable")]
                   (when (= :legacy kind) [:p.small.muted.checkpoint-note legacy-note])]
                  [:div.checkpoint-actions
                   [:button.button {:type "button" :aria-label (str "Restore " label)
                                    :disabled (or (boolean busy?) (not entry) (not connected?))
                                    :on-click #(rf/dispatch [:checkpoints/confirm :restore kind])} "Restore"]
                   (when (and entry (contains? dismissible kind))
                     (let [verb (if (= :legacy kind) "Dismiss" "Discard")]
                       [:button.button.danger {:type "button" :aria-label (str verb " " label)
                                               :disabled (or (boolean busy?) (not connected?))
                                               :on-click #(rf/dispatch [:checkpoints/confirm :discard kind])} verb]))]])])
            (when-let [folder (:folder current)]
              [:section.show-folder-info {:aria-labelledby "show-folder-title"}
               [:h3#show-folder-title "Show folder"]
               [folder-explanation folder]])
            [:div.button-row
             [:button.button {:type "button" :disabled (or (some? catalog-request) (not connected?))
                              :on-click #(rf/dispatch [:checkpoints/refresh])} "Refresh"]]])]))))
