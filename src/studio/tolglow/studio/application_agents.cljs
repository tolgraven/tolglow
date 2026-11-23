(ns tolglow.studio.application-agents
  "Application editing lives in fresh paired workspaces, separate from show chat."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]))

(def defaults {:open? false :generation 0 :serial 0 :pending {} :errors {} :tasks []
               :selected nil :events [] :items {} :order [] :cursor 0
               :form {:provider "codex" :repo "tolglow" :policy "manual" :prompt ""}})
(defn state [db] (merge defaults (:application-agents db)))
(defn token [db] [(get-in db [:show :runtime-id]) (:generation (state db))])
(defn current? [db t] (= t (token db)))
(defn finished? [status] (contains? #{"completed" "failed" "cancelled"} status))
(defn task [s] (some #(when (= (:selected s) (:id %)) %) (:tasks s)))
(defn- store [db s] (assoc db :application-agents s))
(def ^:private command-keys #{:start :cancel :configure})

(defn- request [db key operation params body]
  (let [s (state db)
        serial (inc (:serial s))
        t (token db)
        pending (cond-> {:token t :serial serial}
                  (= key :start) (assoc :prompt (:prompt body)))
        query (when (seq params)
                (str "?" (str/join "&" (map (fn [[k v]] (str (name k) "=" (js/encodeURIComponent (str v)))) params))))]
    {:db (store db (-> s (assoc :serial serial) (assoc-in [:pending key] pending) (update :errors dissoc key)))
     :studio/http {:path (str "/api/application-agents" (when operation (str "/" (name operation))) query)
                   :body (when body (assoc body :runtime-id (get-in db [:show :runtime-id])))
                   :timeout-ms (if (= operation :start) 15000 8000)
                   :on-success [:application-agents/received t key serial operation]
                   :on-failure [:application-agents/failed t key serial]}}))

(defn- owns-request? [db pending t serial]
  (and (= (first t) (get-in db [:show :runtime-id]))
       (= t (:token pending)) (= serial (:serial pending))))

(defn- release-view-requests [s]
  (update s :pending select-keys command-keys))

(defn- update-task [s data]
  (let [tasks (if (some #(= (:id data) (:id %)) (:tasks s))
                (mapv #(if (= (:id data) (:id %)) (merge % data) %) (:tasks s))
                (conj (:tasks s) data))]
    (assoc s :tasks (vec (take-last 32 tasks)))))
(defn consume-events [s data]
  (reduce (fn [s event]
            (case (:type event)
              "agent/item"
              (let [id (if (seq (:item-id event)) (:item-id event) (str "event-" (:cursor event)))
                    order (vec (take-last 100 (if (contains? (:items s) id) (:order s) (conj (:order s) id))))]
                (-> s (assoc :order order) (update :items #(select-keys (assoc % id event) order))))
              "workspace/changes" (update-task s {:id (:selected s) :summary (:summary event)})
              "workspace/check" (update-task s {:id (:selected s) :checks (assoc (:checks (task s) {}) (:profile event) (:result event))})
              "task/status" (update-task s (assoc (select-keys event [:status :error :checks :candidate]) :id (:selected s)))
              (update s :events #(vec (take-last 24 (conj (if (vector? %) % []) event))))))
          (-> s (assoc :cursor (:cursor data) :dropped? (:dropped? data))
              (update-task {:id (:selected s) :status (:status data)})) (:events data)))

(rf/reg-sub :application-agents/state (fn [db _] (state db)))
(rf/reg-event-fx :application-agents/open
  (fn [{:keys [db]} _]
    {:db (store db (assoc (state db) :open? true)) :dispatch [:application-agents/list]}))
(rf/reg-event-db :application-agents/close
  (fn [db _] (store db (-> (state db) release-view-requests (assoc :open? false) (update :generation inc)))))
(rf/reg-event-fx :application-agents/runtime-change
  (fn [{:keys [db]} _]
    (let [s (state db)]
      {:db (store db (assoc defaults :form (:form s) :open? (:open? s) :generation (inc (:generation s))))
       :dispatch (when (:open? s) [:application-agents/list])})))
(rf/reg-event-db :application-agents/edit
  (fn [db [_ key value]] (assoc-in db [:application-agents :form key] value)))
(rf/reg-event-fx :application-agents/list
  (fn [{:keys [db]} _]
    (when (and (:open? (state db)) (not (get-in (state db) [:pending :list])))
      (if (get-in db [:show :runtime-id]) (request db :list nil {} nil)
        {:dispatch-later [{:ms 250 :dispatch [:application-agents/list]}]}))))
(rf/reg-event-fx :application-agents/select
  (fn [{:keys [db]} [_ id]]
    (let [s (release-view-requests (state db))]
      (cond-> {:db (store db (assoc s :selected id :cursor 0 :items {} :order [] :events [] :dropped? false))}
        (:open? s) (assoc :fx [[:dispatch [:application-agents/status id]] [:dispatch [:application-agents/poll id]]])))))
(rf/reg-event-fx :application-agents/status
  (fn [{:keys [db]} [_ id]]
    (when (and (:open? (state db)) (= id (:selected (state db))) (not (get-in (state db) [:pending [:status id]])))
      (request db [:status id] :status {:id id} nil))))
(rf/reg-event-fx :application-agents/poll
  (fn [{:keys [db]} [_ id expected-token]]
    (let [s (state db)]
      (when (and (:open? s) (= id (:selected s)) (not (get-in s [:pending [:events id]]))
                 (or (nil? expected-token) (current? db expected-token)))
        (request db [:events id] :events {:id id :cursor (:cursor s)} nil)))))
(defn start-payload [form]
  (when-not (and (contains? #{"codex"} (:provider form)) (contains? #{"tolglow" "afterglow"} (:repo form))
                 (contains? #{"manual" "after-turn"} (:policy form))
                 (string? (:prompt form)) (<= 1 (count (str/trim (:prompt form))) 32768))
    (throw (js/Error. "Describe an application change and choose a supported repository and review policy.")))
  (select-keys form [:provider :repo :policy :prompt]))
(rf/reg-event-fx :application-agents/start
  (fn [{:keys [db]} _]
    (when-not (get-in db [:application-agents :pending :start])
      (try (request db :start :start {} (start-payload (:form (state db))))
           (catch :default error {:db (assoc-in db [:application-agents :errors :start] (events/error-message error))})))))
(rf/reg-event-fx :application-agents/configure
  (fn [{:keys [db]} [_ fields]]
    ;; Credential text is only in this transient HTTP effect, never app-db.
    (request db :configure :configure {} (select-keys fields [:provider :api-key :credential-ref :model]))))
(rf/reg-event-fx :application-agents/cancel
  (fn [{:keys [db]} _]
    (when-let [id (:selected (state db))] (request db :cancel :cancel {} {:id id}))))
(rf/reg-event-fx :application-agents/received
  (fn [{:keys [db]} [_ t key serial operation data]]
    (let [s (state db)]
      (when (owns-request? db (get-in s [:pending key]) t serial)
        (let [submitted (get-in s [:pending key :prompt])
              s (update s :pending dissoc key)]
          (case operation
            nil {:db (store db (merge s (select-keys data [:available? :runner-ready? :execution-configured? :providers :tasks
                                                          :preview-available? :activation-available? :configuration-persisted?])))
                 :dispatch (cond
                             (:selected s) [:application-agents/poll (:selected s)]
                             (seq (:tasks data)) [:application-agents/select (:id (last (:tasks data)))])}
            :configure {:db (store db (merge s (select-keys data [:providers :configuration-persisted?])))
                        :dispatch [:application-agents/list]}
            :start {:db (store db (cond-> (update-task s data)
                                   (= submitted (get-in s [:form :prompt])) (assoc-in [:form :prompt] "")))
                    :fx [[:dispatch [:application-agents/select (:id data)]] [:dispatch [:workspace/list]]]}
            :status {:db (store db (update-task s data))}
            :cancel {:db (store db (update-task s data))
                     :dispatch (when (and (:open? s) (= (:id data) (:selected s)))
                                 [:application-agents/status (:id data)])}
            :events (if (not= (:selected s) (:task-id data)) {:db (store db s)}
                      {:db (store db (consume-events s data))
                       :dispatch (when (or (:dropped? data) (finished? (:status data))) [:application-agents/status (:selected s)])
                       :dispatch-later (when-not (finished? (:status data))
                                         [{:ms 400 :dispatch [:application-agents/poll (:selected s) t]}])})
            {:db (store db s)}))))))
(rf/reg-event-fx :application-agents/failed
  (fn [{:keys [db]} [_ t key serial error]]
    (let [s (state db)]
      (when (owns-request? db (get-in s [:pending key]) t serial)
        {:db (store db (-> s (update :pending dissoc key) (assoc-in [:errors key] error)))
         :dispatch-later (when (and (:open? s) (vector? key) (= :events (first key)) (= (:selected s) (second key)))
                           [{:ms 1200 :dispatch [:application-agents/poll (:selected s) t]}])}))))

(defn- item-view [item]
  (case (:item-type item)
    "agent_message" [:article.application-agent-message [:span.application-agent-speaker "Codex"] [:p (:text item)]]
    "reasoning" [:details.application-agent-reasoning [:summary "Agent progress"] [:p (:text item)]]
    "command_execution" [:details.application-agent-command [:summary [:code (:command item)] [:span (:status item)]]
                         [:pre (:output item)]]
    "file_change" [:article.application-agent-files [:span.application-agent-speaker "Files updated"]
                   [:ul (for [[index change] (map-indexed vector (:changes item))]
                          ^{:key index} [:li [:span (:kind change)] [:code (:path change)]])]]
    "todo_list" [:ul.application-agent-plan
                 (for [[index entry] (map-indexed vector (:items item))]
                   ^{:key index} [:li {:class (when (:completed? entry) "completed")} [:span (if (:completed? entry) "✓" "○")] (:text entry)])]
    "mcp_tool_call" [:p.application-agent-tool (str (:server item) " / " (:tool item) " · " (:status item))]
    "web_search" [:p.application-agent-tool (str "Search · " (:query item))]
    "error" [:p.connection-error (:message item)]
    [:p.application-agent-tool (str "Activity · " (:item-type item))]))

(defn- credentials-panel []
  (let [api-key (r/atom "") model (r/atom "")]
    (fn []
      [:details.application-agent-credentials
       [:summary "Codex connection"]
       [:div.form-row
        [:label "API key" [:input {:type "password" :auto-complete "off" :value @api-key :aria-label "Application agent API key"
                                    :on-change #(reset! api-key (.. % -target -value))}]]
        [:label "Model · optional" [:input {:type "text" :value @model :on-change #(reset! model (.. % -target -value))}]]]
       [:button.button {:type "button" :disabled (str/blank? @api-key)
                         :on-click #(do (rf/dispatch [:application-agents/configure
                                                      (cond-> {:provider "codex" :api-key @api-key} (seq @model) (assoc :model @model))])
                                        (reset! api-key ""))} "Save connection"]
       [:p.small.muted "The host stores the key in its credential vault. This connection edits application workspaces; show-authoring chat uses its own configuration."]])))

(defn check-summary [selected]
  {:checks (mapv (fn [[profile result]] (assoc result :profile (name profile))) (sort-by (comp str key) (:checks selected)))
   :candidate-ready? (= "ready" (get-in selected [:candidate :status]))})

(defn- checks-view [selected]
  (let [{:keys [checks candidate-ready?]} (check-summary selected)]
    (when (or (seq checks) candidate-ready?)
      [:section.application-agent-checks {:aria-label "Application task checks"}
       [:h3 "Checks"]
       (for [check checks]
         ^{:key (:profile check)} [:div.application-agent-check {:data-status (:status check)}
                                  [:span (:profile check)] [:strong (:status check)]
                                  (when-let [error (:error check)] [:p.connection-error (events/error-message error)])])
       (when candidate-ready? [:p.application-agent-candidate "Candidate ready for review · not loaded into the running application"])])))

(defn- view []
  (let [s @(rf/subscribe [:application-agents/state]) selected (task s) form (:form s)
        configured? (some #(and (= "codex" (:id %)) (:configured? %)) (:providers s))
        busy? (contains? (:pending s) :start)]
    [:section.panel.application-agents {:aria-label "Application coding agent"}
     [:div.panel-top [:span.section-label "APPLICATION AGENT"]
      [:span.small.muted (cond (not (:available? s)) "Loading or unavailable" (not (:runner-ready? s)) "Runner unavailable"
                               configured? "Codex connected" :else "Connect Codex")]]
     [:div.application-agent-layout
      [:div.application-agent-composer
       [:h2 "Describe the next change."]
       [:p.small.muted "Every task gets new paired branches. Review its changes here before loading application code."]
       [credentials-panel]
       [:label "Application change"
        [:textarea#application-agent-prompt {:rows 5 :value (:prompt form) :placeholder "Improve an interaction, add a control, or fix a problem…"
                                             :on-change #(rf/dispatch-sync [:application-agents/edit :prompt (.. % -target -value)])}]]
       [:div.form-row
        [:label "Start in" [:select {:aria-label "Application repository" :value (:repo form) :on-change #(rf/dispatch-sync [:application-agents/edit :repo (.. % -target -value)])}
                             [:option {:value "tolglow"} "Tolglow"] [:option {:value "afterglow"} "Afterglow"]]]
        [:label "After the agent finishes" [:select {:aria-label "After the agent finishes" :value (:policy form) :on-change #(rf/dispatch-sync [:application-agents/edit :policy (.. % -target -value)])}
                                            [:option {:value "manual"} "Review manually"] [:option {:value "after-turn"} "Run checks and prepare review"]]]]
       [:button#application-agent-start.button.primary {:type "button" :disabled (or busy? (not configured?) (not (:runner-ready? s)) (str/blank? (:prompt form)))
                                                        :on-click #(rf/dispatch [:application-agents/start])}
        (if busy? "Creating fresh workspace…" "Start a new task")]
       (when (false? (:preview-available? s)) [:p.small.muted "Preview activation and automatic live reload are not available yet."])
       (for [[key error] (:errors s)] ^{:key (str key)} [:p.connection-error {:role "status"} error])
       [:div.application-agent-task-list
        [:div.application-agent-list-heading [:h3 "Tasks"] [:button.text-button {:type "button" :on-click #(rf/dispatch [:application-agents/list])} "Refresh"]]
        (if (seq (:tasks s))
          (for [task (reverse (:tasks s))]
            ^{:key (:id task)} [:button.application-agent-task {:type "button" :aria-pressed (= (:selected s) (:id task))
                                                               :on-click #(rf/dispatch [:application-agents/select (:id task)])}
                              [:span (or (:name task) (:id task))] [:small (:status task)]])
          [:p.small.muted "No application tasks in this server session."])]]
      [:div.application-agent-progress
       (if selected
         [:<>
          [:div.application-agent-progress-heading [:div [:h3 "Live progress"] [:span.small.muted (:status selected)]]
           (when-not (finished? (:status selected))
             [:button.button {:type "button" :disabled (contains? (:pending s) :cancel) :on-click #(rf/dispatch [:application-agents/cancel])} "Cancel task"])]
          (when-let [workspace-id (:workspace-id selected)]
            [:div.application-agent-workspace
             [:code workspace-id]
             [:button.button {:type "button" :on-click #(rf/dispatch [:workspace/navigate [:workspace/review-task workspace-id]])} "Review workspace changes"]])
          (when-let [summary (:summary selected)]
            [:div.application-agent-observer {:aria-label "Workspace change summary"}
             [:strong (str (:file-count summary 0) " changed files")]
             [:span.small.muted "Observed from Git"]
             [:ul (for [[repo changes] (:repositories summary) change (take 24 changes)]
                    ^{:key (str repo (:path change))} [:li [:span (name repo)] [:code (:path change)]])]])
          (when (:dropped? s) [:p.small.muted "Older progress was trimmed. The current task state and full workspace diff remain available."])
          [checks-view selected]
          [:div.application-agent-transcript {:aria-label "Application agent progress stream" :aria-live "polite" :aria-relevant "additions text"}
           (for [id (:order s)] ^{:key id} [item-view (get-in s [:items id])])
           (when (empty? (:order s)) [:p.small.muted "Waiting for agent output…"])]
          (when-let [error (:error selected)] [:p.connection-error (events/error-message error)])]
         [:div.application-agent-empty [:span "⌘"] [:h3 "A separate place to change the app"]
          [:p "Task messages, commands and file changes appear here as they arrive. Your running show's authoring chat stays separate."]])]]]))

(def panel
  (r/create-class {:display-name "Application coding agent"
                   :component-did-mount (fn [_] (rf/dispatch [:application-agents/open]))
                   :component-will-unmount (fn [_] (rf/dispatch [:application-agents/close]))
                   :reagent-render view}))
