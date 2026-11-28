(ns tolglow.studio.device-retirement
  "Device sessions a restore, import or restart retired (#907). Reads
  :status :device-retirement, the same map the REPL reads from show/snapshot,
  and sends Reconnect through POST /api/devices/reconnect, the route for
  show/reconnect-device!. A refusal shows the REPL's message and ex-data
  :code. The status changes only when a session retires, closes or is
  refused, never per frame, and this panel subscribes to that slice alone."
  (:require [re-frame.core :as rf]
            [tolglow.studio.events :as events]))

(def ^:private ui-path [:ui :device-retirement])

(def counters
  "Counter key, label and whether a non-zero value is a problem."
  [[:pending "Closing" false] [:retired "Closed" false] [:failed "Close failed" true]
   [:reopen-failed "Reopen failed" true] [:abandoned "Abandoned at stop" true]
   [:late "Closed after stop" false] [:refused "Refused reconnects" false]])

(defn- runtime-id [db] (get-in db [:show :runtime-id]))

(defn- owned
  "This panel's request state for the current runtime only."
  [db]
  (let [ui (get-in db ui-path)]
    (when (= (:runtime-id ui) (runtime-id db)) ui)))

(defn sessions
  "One row per session ID that is closing or can be reconnected, sorted."
  [{:keys [closing reconnectable]}]
  (let [closing (set closing) reconnectable (set reconnectable)]
    (mapv (fn [id] {:id id :closing? (contains? closing id) :reconnectable? (contains? reconnectable id)})
          (sort (into closing reconnectable)))))

(defn refusal
  "A rejection body or status refusal as {:message :code :session-id}."
  [data]
  (if (map? data)
    {:message (:message data)
     :code (or (:code data) (get-in data [:details :code]))
     :session-id (or (:session-id data) (get-in data [:details :session-id]))}
    {:message (str data)}))

(defn code-text
  "A :code as the REPL prints it, e.g. :device/closing."
  [code]
  (when (some? code) (str ":" (if (keyword? code) (subs (str code) 1) code))))

(rf/reg-sub ::status (fn [db _] (get-in db [:show :status :device-retirement])))

(rf/reg-sub ::ui (fn [db _] (owned db)))

(rf/reg-sub ::connected? (fn [db _] (true? (get-in db [:connection :connected?]))))

(rf/reg-event-fx ::reconnect
  (fn [{:keys [db]} [_ session-id]]
    (let [runtime (runtime-id db)
          ui (or (owned db) {:runtime-id runtime})]
      (when (and runtime (get-in db [:connection :connected?]) (not (get-in ui [:pending session-id])))
        {:db (assoc-in db ui-path (cond-> (assoc-in ui [:pending session-id] true)
                                    (:results ui) (update :results dissoc session-id)))
         :studio/http {:path "/api/devices/reconnect" :body {:session-id session-id}
                       :failure-data? true :timeout-ms 8000
                       :on-success [::finished runtime session-id true]
                       :on-failure [::finished runtime session-id false]}}))))

(rf/reg-event-db ::finished
  (fn [db [_ runtime session-id accepted? data]]
    (if-let [ui (when (= runtime (runtime-id db)) (owned db))]
      (assoc-in db ui-path (-> ui (update :pending dissoc session-id)
                               (assoc-in [:results session-id]
                                         (if accepted? {:accepted? true} (assoc (refusal data) :accepted? false)))))
      db)))

(defn- refusal-line [{:keys [message code session-id]}]
  [:span.retirement-refusal-text
   [:span message]
   (when code [:code.retirement-code (code-text code)])
   (when session-id [:span.retirement-id session-id])])

(defn- session-row [{:keys [id closing? reconnectable?]} ui connected?]
  (let [pending? (get-in ui [:pending id])
        result (get-in ui [:results id])]
    [:li.retirement-session {:class (when closing? "closing")}
     [:div.retirement-session-line
      [:span.retirement-id id]
      [:span.retirement-state (if closing? "Closing" "Closed")]
      (when reconnectable?
        [:button.button.retirement-reconnect
         {:type "button" :disabled (or pending? (not connected?))
          :aria-label (str "Reconnect device session " id)
          :on-click #(rf/dispatch [::reconnect id])}
         (if pending? "Reconnecting…" "Reconnect")])]
     (cond
       (nil? result) nil
       (:accepted? result) [:p.small.retirement-ok {:role "status"} "Reconnected."]
       :else [:p.retirement-refusal {:role "alert"} [refusal-line result]])]))

(defn panel
  "Connections panel for retired device sessions."
  []
  (let [status @(rf/subscribe [::status])
        ui @(rf/subscribe [::ui])
        connected? @(rf/subscribe [::connected?])
        skin (some-> @(rf/subscribe [:ui/knob-skin]) name)
        rows (sessions status)
        closing (count (:closing status))
        {:keys [last-error last-refusal]} status]
    [:section.panel.device-retirement {:aria-label "Device sessions" :data-skin skin}
     [:div.panel-top [:span.section-label "DEVICE SESSIONS"]
      [:span.connection-chip {:class (when (zero? closing) "active")} [:i]
       (cond (nil? status) "Waiting for the show"
             (pos? closing) (str closing " still closing")
             :else "None closing")]]
     [:div.form-body
      [:p.small.muted "A restore, import or restart retires every connected device session. Each closes on its own lane, so a stuck display or MIDI write holds only that session."]
      (when status
        (into [:dl.retirement-counts]
              (for [[key label problem?] counters
                    :let [value (get status key 0)]]
                ^{:key key}
                [:div.retirement-count {:class (when (and problem? (pos? value)) "problem")}
                 [:dt label] [:dd value]])))
      (if (seq rows)
        (into [:ul.retirement-sessions {:aria-label "Retired device sessions"}]
              (for [row rows] ^{:key (:id row)} [session-row row ui connected?]))
        (when status [:p.small.muted "No retired sessions."]))
      (when last-error
        [:p.connection-error {:role "status"}
         (str (if (= "reopen" (events/wire-id (:stage last-error))) "Last reopen failed: " "Last close failed: ")
              (:message last-error) (when-let [c (:class last-error)] (str " (" c ")")))])
      (when last-refusal
        [:p.retirement-refusal.last {:role "status"} [:span.small.muted "Last refused reconnect · "]
         [refusal-line (refusal last-refusal)]])
      [:p.small.muted "At the REPL: (get-in (show/snapshot runtime) [:status :device-retirement]) and (show/reconnect-device! runtime id)."]]]))
