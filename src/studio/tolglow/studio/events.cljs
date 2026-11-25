(ns tolglow.studio.events
  "The studio application is data + re-frame transitions. I/O lives in effects."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.fog-fields :as fog]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.page-routes :as pages]
            [tolglow.studio.state-socket]
            [tolglow.studio.spatial-drafts :as spatial-drafts]
            [tolglow.transit :as transit]
            [tolglow.studio.parameter-widgets :as widgets]))

(def initial-source
  "{:id :after-hours\n :version 1\n :name \"After hours\"\n :swatch \"#b981a3\"\n :duration {:beats 4}\n :envelope {:attack {:beats 0.125} :release {:beats 1}}\n :params {:level {:type :number :default 0.7 :bounds [0.0 1.0]}}\n :outputs {:color {:op :const :value [0.725 0.506 0.639]}\n           :dimmer {:op :param :key :level}}}")

(defn initial-db [session]
  {:session session :show nil :pointers {} :pending-params {} :drafts {} :stream-id nil
   :connection {:connected? false :error "Connecting to the studio…"}
   :ui {:view :perform :page-id nil :selected-instance nil :launch-mode "toggle" :edit? false
        :preview-settings {:worklight 0 :quality "auto" :camera "audience"} :preview-expanded? false :preview-controls? false
        :definition-search "" :assign-cue "" :toast nil :action-cell nil :clipboard nil :prompt nil
        :composition {:id "evening-drift" :name "Evening drift" :mode "sequence" :loop "once" :slots []}
        :definition {:kind "effects" :source initial-source :version 1 :engine :textarea :mode :source
                     :auto? false :status "Ready to create" :result "" :error? false}
        :builder {:id "after-hours" :name "After hours" :color "#b981a3" :level 0.7
                  :duration-value 4 :duration-unit "beats" :release-value 1 :release-unit "beats" :cue? true}
        :response-form {:id "performance-envelope" :params "level" :scope "all"
                        :attack-value 0.5 :release-value 1 :unit "beats" :curve "smoothstep"}
        :auto-form {:interval-value 8 :interval-unit "beats" :count 1 :scope "page" :mode "timed" :duration-value 4 :duration-unit "beats"}
        :rig-draft nil :rig-dirty? false}})

(defn error-message [error]
  (or (when (map? error) (or (:message error) (:error error) (:reason error)))
      (.-message error) (str error)))
(defn- dispatch-result [event result] (when event (rf/dispatch (conj event result))))

(def guarded-paths
  #{"/api/definition" "/api/materials" "/api/rig" "/api/composition" "/api/composition/run"
    "/api/composition/stop" "/api/import" "/api/auto" "/api/response"
    "/api/response/release" "/api/response/delete" "/api/page/generate" "/api/settings"
    "/api/fixtures/publish" "/api/fixtures/patch"
    "/api/sync/configure" "/api/sync/start" "/api/sync/stop" "/api/sync/source" "/api/sync/handoff" "/api/sync/arrange" "/api/sync/learn-parameter" "/api/sync/forget-parameter"
    "/api/audio/start" "/api/audio/stop"
    "/api/sensors/stage/start" "/api/sensors/stage/stop"
    "/api/sensors/audience-zone" "/api/sensors/pair" "/api/sensors/configure" "/api/sensors/arm" "/api/sensors/calibrate" "/api/sensors/osc/start" "/api/sensors/osc/stop"
    "/api/repl/eval" "/api/repl/interrupt" "/api/repl/scratch" "/api/repl/session"
    "/api/patch/save" "/api/patch/enabled"
    "/api/modulation/start" "/api/modulation/stop" "/api/modulation/hold" "/api/modulation/resume" "/api/modulation/cue"})
(defn guard-request [db request]
  (if (and (:body request) (guarded-paths (:path request)))
    (update request :body #(merge {:expected-epoch (get-in db [:show :epoch])}
                                  (when-let [runtime (get-in db [:show :runtime-id])] {:runtime-id runtime}) %))
    request))
(rf/reg-global-interceptor
  (rf/->interceptor
    :id :studio/session-guard
    :after (fn [context]
             (let [db (get-in context [:coeffects :db])]
               (update context :effects
                 (fn [effects]
                   (cond-> (reduce (fn [out key] (if (nil? (get out key)) (dissoc out key) out))
                                  effects [:fx :dispatch :dispatch-n :dispatch-later])
                     (:studio/http effects) (update :studio/http #(guard-request db %))
                     (:fx effects) (update :fx #(mapv (fn [[kind value]]
                                                        [kind (if (= kind :studio/http) (guard-request db value) value)]) %)))))))))

(defn http! [{:keys [path body typed? raw-response? on-success on-failure timeout-ms]}]
  (let [controller (js/AbortController.)
        timeout (js/setTimeout #(.abort controller) (or timeout-ms 5000))]
    (-> (js/fetch path
                 (clj->js (cond-> {:method (if (some? body) "POST" "GET") :cache "no-store"
                                  :signal (.-signal controller)
                                  :headers (cond-> {"Accept" "application/transit+json"}
                                             (some? body) (assoc "Content-Type" "application/transit+json")
                                             (and (some? body) (not typed?)) (assoc "X-Tolglow-Form" "studio-v1"))}
                           (some? body) (assoc :body (transit/encode body)))))
        (.then (fn [response]
                 (-> (.text response)
                     (.then (fn [text]
                              (let [type (or (.get (.-headers response) "content-type") "")
                                    data (cond
                                           (str/includes? type "application/transit+json")
                                           (let [v (transit/decode text)] (if raw-response? v (projection/presentation v)))
                                           (str/includes? type "application/json")
                                           (js->clj (js/JSON.parse text) :keywordize-keys true)
                                           :else {:message text})]
                                (if (or (not (.-ok response)) (contains? #{:rejected "rejected"} (:status data)) (:error data))
                                  (throw (js/Error. (str (error-message data))))
                                  data)))))))
        (.then #(dispatch-result on-success %))
        (.catch #(dispatch-result (or on-failure [:studio/failure]) (error-message %)))
        (.finally #(js/clearTimeout timeout)))))
(rf/reg-fx :studio/http http!)
(rf/reg-fx :studio/scroll-top (fn [_] (.scrollTo js/window #js {:top 0 :behavior "auto"})))
(rf/reg-fx :studio/send-command
  (fn [{:keys [op fields epoch session runtime-id on-success on-failure beacon?]}]
    (let [payload (merge {:op op :epoch epoch :command-id (str (random-uuid))
                          :source {:adapter "web" :session-id session}} fields
                         (when runtime-id {:runtime-id runtime-id}))]
      (if (and beacon? (.-sendBeacon js/navigator))
        (.sendBeacon js/navigator "/api/command"
                     (js/Blob. #js [(js/JSON.stringify (clj->js payload))] #js {:type "application/json"}))
        (http! {:path "/api/command" :body payload :on-success on-success :on-failure on-failure})))))
(rf/reg-fx :studio/download
  (fn [_]
    (-> (js/fetch "/api/export")
        (.then (fn [response] (if (.-ok response) (.blob response) (throw (js/Error. "Show export failed.")))))
        (.then (fn [blob]
                 (let [url (.createObjectURL js/URL blob) link (.createElement js/document "a")]
                   (set! (.-href link) url) (set! (.-download link) "tolglow-show.transit") (.click link)
                   (js/setTimeout #(.revokeObjectURL js/URL url) 1000)
                   (rf/dispatch [:studio/toast "Show exported."]))))
        (.catch #(rf/dispatch [:studio/failure (error-message %)])))))
(rf/reg-fx :studio/read-file
  (fn [file]
    (if (> (.-size file) (* 16 1024 1024))
      (rf/dispatch [:studio/failure "Choose a show file no larger than 16 MiB."])
      (-> (.text file) (.then #(rf/dispatch [:studio/import-source %]))
          (.catch #(rf/dispatch [:studio/failure (error-message %)]))))))

(defn cue [db id] (some #(when (= id (:id %)) %) (get-in db [:show :cues])))
(defn effect [db id] (some #(when (= id (:id %)) %) (get-in db [:show :effects])))
(defn recent-page? [db id]
  (and (some? id) (= id (get-in db [:show :recent-page :id]))))
(defn selected-recent-page [db]
  (when (get-in db [:ui :recent-page?]) (get-in db [:show :recent-page])))
(defn wire-id [value] (if (keyword? value) (subs (str value) 1) (str value)))
(defn active [db cue-id]
  (filterv #(and (= cue-id (:cue-id %)) (not= "releasing" (:phase %)))
           (get-in db [:show :instances])))
(defn command [db op fields & [on-success epoch beacon? runtime-id]]
  [:studio/send-command {:op op :fields fields :epoch (or epoch (get-in db [:show :epoch]))
                        :session (:session db) :runtime-id (or runtime-id (get-in db [:show :runtime-id]))
                        :on-success on-success :beacon? beacon?}])
(defn notice [message] [:dispatch [:studio/toast message]])
(defn slug [value]
  (let [s (-> value str/lower-case str/trim (str/replace #"[^a-z0-9._/-]+" "-") (str/replace #"^-+|-+$" ""))]
    (if (seq s) s "untitled")))

(defn snapshot-order [show] (mapv #(get show % 0) [:epoch :revision :frame-number]))
(defn outstanding-drafts [drafts show]
  (into {} (filter (fn [[_ draft]]
                    (and (= (:epoch draft) (:epoch show))
                         (= (:runtime-id draft) (:runtime-id show))
                         (or (nil? (:revision draft)) (< (:revision show) (:revision draft)))
                         (or (= "master/set" (:op draft))
                             (some #(= (:id %) (get-in draft [:fields :instance-id])) (:instances show)))))) drafts))
(defn apply-drafts [show drafts]
  (reduce (fn [show [_ {:keys [op fields]}]]
            (case op
              "master/set" (assoc show :master (:value fields))
              "param/set" (update show :instances
                                  (fn [instances] (mapv #(if (= (:id %) (:instance-id fields))
                                                          (assoc-in % [:params (keyword (:param fields))] (:value fields)) %) instances)))
              show)) show drafts))
(defn draft [db key op fields]
  (assoc-in db [:drafts key] {:op op :fields fields :epoch (get-in db [:show :epoch]) :runtime-id (get-in db [:show :runtime-id])}))

(rf/reg-event-fx :studio/initialize
  (fn [_ [_ session]] {:db (initial-db session) :studio/open-stream session}))

(defn- settle-connection-drafts [db old-presentation old-typed]
  (let [show (:show db) owner ((juxt :runtime-id :epoch) show)
        old-pause (get-in old-presentation [:settings :output :pause-look])
        new-pause (get-in show [:settings :output :pause-look])
        old-routing (get-in old-typed [:settings :audio-returns])
        new-routing (get-in db [:server-projection :view :settings :audio-returns])]
    (cond-> db
      (and (get-in db [:connections :pause-save]) (not= old-pause new-pause))
      (update-in [:connections :pause-save] dissoc :form)
      (and (get-in db [:audio-returns owner]) (not= old-routing new-routing))
      (update-in [:audio-returns owner] dissoc :baseline :submitted))))

(defn- install-show [db server]
  (let [presentation (projection/presentation (:view server)
                                              (get-in db [:server-projection :view]) (:server-presentation db))
        runtime-changed? (and (:show db) (not= (:runtime-id presentation) (get-in db [:show :runtime-id])))
        drafts (if runtime-changed? {} (outstanding-drafts (:drafts db) presentation))
        order (snapshot-order presentation)
        show (apply-drafts presentation drafts)
        page-id (get-in db [:ui :page-id])
        selected (get-in db [:ui :selected-instance])
        epoch-changed? (or runtime-changed? (and (:show db) (not= (get-in db [:show :epoch]) (:epoch show))))
        updated (-> db (assoc :show show :snapshot-order order :drafts drafts :connection {:connected? true}
                             :server-projection server :server-presentation presentation)
                    spatial-drafts/settle
                    (assoc-in [:ui :page-id] (if (some #(= page-id (:id %)) (:pages show)) page-id (:id (first (:pages show))))))
        updated (settle-connection-drafts updated (:server-presentation db) (get-in db [:server-projection :view]))
        updated (cond-> updated
                  epoch-changed? (dissoc :ending-holds)
                  epoch-changed? (update-in [:ui :definition] dissoc :publishing? :publication)
                  epoch-changed? (update-in [:ui :auto-form] dissoc :target-activity? :activity-target))
        updated (if (some #(= selected (:id %)) (:instances show)) updated
                  (assoc-in updated [:ui :selected-instance] (:id (last (:instances show)))))
        updated (if (get-in updated [:ui :rig-dirty?])
                  (let [previous (get-in db [:show :rig :room :measured-fog])
                        current (get-in show [:rig :room :measured-fog])]
                    (cond
                      (= previous current) updated
                      current (update-in updated [:ui :rig-draft :room]
                                         #(assoc (or % (get-in show [:rig :room])) :measured-fog current))
                      :else (update-in updated [:ui :rig-draft :room] dissoc :measured-fog)))
                  (assoc-in updated [:ui :rig-draft] (:rig show)))]
    {:db updated
     :fx (cond-> []
           (and (or (:recover-held? updated) (and epoch-changed? (seq (:pointers db))))
                (or (:awaiting-full? db) epoch-changed? (not= (:revision show) (get-in db [:show :revision]))))
           (conj [:dispatch [:studio/reconcile-held]])
           epoch-changed? (conj [:dispatch [:studio/release-pointers]] [:dispatch [:chat/runtime-change]]
                                        [:dispatch [:fixtures/runtime-change]] [:dispatch [:connections/runtime-change]] [:dispatch [:sensors/runtime-change]] [:dispatch [:modulation/runtime-change]] [:dispatch [:patchbay/runtime-change]] [:dispatch [:waveform/runtime-change]] [:dispatch [:repl/runtime-change]] [:dispatch [:performance/runtime-change]] [:dispatch [:workspace/runtime-change]] [:dispatch [:subscribers/runtime-change]]
                                        (notice "Show restarted; controls follow the new epoch."))
           (and (get-in db [:connections :visible?])
                (or epoch-changed?
                    (not (identical? (:connection-status presentation)
                                     (get-in db [:server-presentation :connection-status])))))
           (conj [:dispatch [:connections/pushed]])
           runtime-changed? (conj [:dispatch [:terminal/runtime-change]] [:dispatch [:application-agents/runtime-change]])
           (and (nil? (:show db)) (get-in updated [:application-agents :open?]))
           (conj [:dispatch [:application-agents/list]])
           (and (nil? (:show db)) (get-in updated [:ui :workspace :open?]))
           (conj [:dispatch [:workspace/list]])
           (or epoch-changed? (not (:settings-loaded? db)))
           (conj [:dispatch [:studio/settings-received (or (:settings show) {})]]))}))

(rf/reg-event-db :studio/stream-opening
  (fn [db [_ session id]]
    (if (= session (:session db))
      (assoc db :stream-id id :awaiting-full? true)
      db)))

(defn- current-stream? [db session id]
  (and (= session (:session db)) (some? id) (= id (:stream-id db))))

(rf/reg-event-fx :studio/stream-frame
  (fn [{:keys [db]} [_ session id envelope]]
    (when (current-stream? db session id)
      (let [current (:server-projection db)
            received (if (and (:awaiting-full? db) (not= :full (:kind envelope)))
                       {:status :resync}
                       (projection/receive current envelope))]
        (case (:status received)
          :accepted (-> (install-show db (:state received))
                        (update :db dissoc :awaiting-full?)
                        (assoc :studio/stream-reply [id {:op :ack :token (:token envelope)}]))
          :duplicate (cond-> {:db (-> db (dissoc :awaiting-full?) (assoc :connection {:connected? true}))
                              :studio/stream-reply [id {:op :ack :token (:token envelope)}]}
                       (and (:awaiting-full? db) (:recover-held? db))
                       (assoc :fx [[:dispatch [:studio/reconcile-held]]])
                       (and (:awaiting-full? db) (get-in db [:connections :visible?]))
                       (assoc :dispatch [:connections/pushed]))
          {:db (assoc db :awaiting-full? true)
           :studio/stream-reply [id {:op :resync}]})))))

(rf/reg-event-fx :studio/stream-closed
  (fn [{:keys [db]} [_ session id reason]]
    (when (current-stream? db session id)
      {:db (assoc db :stream-id nil
                  :connection {:connected? false :error (str "Reconnecting: " reason)}
                  :connections (assoc (:connections db) :audio nil :sync nil))
       :dispatch [:studio/release-pointers]})))

(rf/reg-event-fx :studio/toast
  (fn [{:keys [db]} [_ message error?]]
    {:db (assoc-in db [:ui :toast] {:message message :error? error?})
     :dispatch-later [{:ms (if error? 6500 3200) :dispatch [:studio/clear-toast message]}]}))
(rf/reg-event-db :studio/clear-toast
  (fn [db [_ message]] (if (= message (get-in db [:ui :toast :message])) (assoc-in db [:ui :toast] nil) db)))
(rf/reg-event-fx :studio/failure (fn [_ [_ error]] {:dispatch [:studio/toast (str error) true]}))
(rf/reg-event-db :studio/ui-set (fn [db [_ path value]] (assoc-in db (into [:ui] path) value)))
(rf/reg-event-db :studio/numeric-edit
  (fn [db [_ field-id text]] (assoc-in db [:ui :numeric-edits field-id] text)))
(rf/reg-event-db :studio/numeric-blur
  (fn [db [_ field-id]] (update-in db [:ui :numeric-edits] dissoc field-id)))
(rf/reg-event-fx :studio/settings-load
  (fn [_ _] {:studio/http {:path "/api/settings" :on-success [:studio/settings-received] :on-failure [:studio/settings-unavailable]}}))
(rf/reg-event-db :studio/settings-unavailable (fn [db _] (assoc db :settings-supported? false)))
(defn apply-ui-preferences [db settings]
  (let [prefs (:ui settings) raw-view (:view prefs)
        view (when (or (string? raw-view) (keyword? raw-view)) (keyword raw-view))]
    (cond-> (assoc db :settings-loaded? true :settings-supported? true)
      (and (contains? #{nil :home} (:route-name db)) (pages/views view)) (assoc-in [:ui :view] view)
      (string? (:page-id prefs)) (assoc-in [:ui :page-id] (:page-id prefs))
      (contains? #{"toggle" "held" "one-shot"} (:launch-mode prefs)) (assoc-in [:ui :launch-mode] (:launch-mode prefs)))))
(rf/reg-event-fx :studio/settings-received
  (fn [{:keys [db]} [_ settings]]
    (let [updated (apply-ui-preferences db settings)]
      (cond-> {:db updated}
        (= :home (:route-name updated)) (assoc :studio/navigate [:replace (get-in updated [:ui :view])])))))
(rf/reg-event-fx :studio/persist-ui
  (fn [{:keys [db]} _]
    (when (:settings-supported? db)
      {:studio/http {:path "/api/settings"
                     :body {:scope "ui" :values {:view (name (get-in db [:ui :view]))
                                                :page-id (get-in db [:ui :page-id])
                                                :launch-mode (get-in db [:ui :launch-mode])}}}})))
(rf/reg-event-fx :studio/set-view
  (fn [_ [_ view]]
    (when (pages/views view) {:studio/navigate [:push view]})))

(rf/reg-event-fx :studio/route-matched
  (fn [{:keys [db]} [_ page]]
    (cond
      (= page :home)
      (cond-> {:db (assoc db :route-name :home)}
        (:settings-loaded? db) (assoc :studio/navigate [:replace (get-in db [:ui :view])]))

      (and (pages/views page) (not= page (:route-name db)))
      {:db (-> db (assoc :route-name page) (assoc-in [:ui :view] page) (assoc-in [:ui :rig-drag] nil))
       :fx (cond-> [[:dispatch [:studio/release-pointers]] [:studio/scroll-top true]]
             ;; Initial deep links and the home preference restore are read-only.
             (pages/views (:route-name db)) (conj [:dispatch [:studio/persist-ui]])
             (and (= page :assistant) (not (get-in db [:chat :provider]))) (conj [:dispatch [:chat/load]]))
       :dispatch-later (when (= page :perform) [{:ms 30 :dispatch [:studio/preview-resize]}])})))

(rf/reg-event-fx :studio/set-page
  (fn [{:keys [db]} [_ id]]
    (let [recent? (recent-page? db id)]
      {:db (cond-> (assoc-in db [:ui :recent-page?] recent?)
             recent? (assoc-in [:ui :action-cell] nil)
             (not recent?) (assoc-in [:ui :page-id] id))
       :fx (cond-> [[:dispatch [:studio/release-pointers]]]
             (not recent?) (conj [:dispatch [:studio/persist-ui]]))})))
(rf/reg-event-fx :studio/set-launch
  (fn [{:keys [db]} [_ mode]] {:db (assoc-in db [:ui :launch-mode] mode) :fx [[:dispatch [:studio/release-pointers]] [:dispatch [:studio/persist-ui]]]}))
(rf/reg-event-fx :studio/toggle-edit
  (fn [{:keys [db]} _]
    (when-not (selected-recent-page db)
      {:db (update-in db [:ui :edit?] not) :dispatch [:studio/release-pointers]})))
(rf/reg-event-fx :studio/command
  (fn [{:keys [db]} [_ op fields]]
    (if (:show db) {:fx [(command db op fields)]} {:dispatch [:studio/failure "Wait for the studio to connect."]})))
(rf/reg-event-fx :studio/blackout-toggle
  (fn [{:keys [db]} _] {:dispatch [:studio/command "blackout/set" {:value (not (get-in db [:show :blackout?]))}]}))
(rf/reg-event-fx :studio/transport-toggle
  (fn [{:keys [db]} _] {:dispatch [:studio/command "transport/set" {:transport {:playing? (not (get-in db [:show :transport :playing?] true))}}]}))

(defn launch-fx [db cue-id mode on-success]
  (let [cue (cue db cue-id)]
    (command db "cue/launch" {:cue-id cue-id
                             :launch (cond-> {:mode (if (= mode "toggle") "persistent" mode)}
                                       (and (= mode "one-shot") (:duration cue)) (assoc :duration (:duration cue))
                                       (and (= mode "one-shot") (not (:duration cue))) (assoc :duration-ms (or (:duration-ms cue) 1000)))} on-success)))
(rf/reg-event-fx :studio/launch
  (fn [{:keys [db]} [_ cue-id mode]]
    (let [mode (or mode (get-in db [:ui :launch-mode])) instances (active db cue-id)]
      (cond
        (not (get-in db [:connection :connected?])) {:dispatch [:studio/failure "Wait for the show to reconnect before launching."]}
        (and (= mode "toggle") (seq instances)) {:fx (mapv #(command db "instance/end" {:instance-id (:id %)}) instances)}
        :else {:fx [(launch-fx db cue-id mode [:studio/launched])]}))))
(rf/reg-event-db :studio/launched
  (fn [db [_ ack]] (assoc-in db [:ui :selected-instance] (:instance-id ack))))
(rf/reg-event-fx :studio/end-all
  (fn [{:keys [db]} _] {:fx (mapv #(command db "instance/end" {:instance-id (:id %)}) (get-in db [:show :instances]))}))

(rf/reg-cofx :studio/unique (fn [cofx _] (assoc cofx :unique (str (random-uuid)))))

(defn- launch-held [db {:keys [cue-id press-id] :as pointer}]
  ;; The command's origin survives pointer release, reuse and show replacement.
  (let [owner (select-keys pointer [:pointer-id :press-id :epoch :runtime-id])]
    (-> (launch-fx db cue-id "held" [:studio/held-launched owner])
        (assoc-in [1 :fields :gesture-id] press-id)
        (assoc-in [1 :on-failure] [:studio/held-failed]))))

(rf/reg-event-fx :studio/gesture-down [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ {:keys [pointer-id cue-id page-id] :as input}]]
    (let [recent? (recent-page? db page-id)]
      (when-not (or (get-in db [:pointers pointer-id]) (and recent? (nil? cue-id)))
        (let [mode (get-in db [:ui :launch-mode])
              held? (and cue-id (= mode "held") (or recent? (not (get-in db [:ui :edit?]))))
              pointer (merge input (select-keys (:show db) [:epoch :runtime-id])
                             {:press-id unique :mode mode :held? held? :recent-page? recent?})]
          (cond-> {:db (assoc-in db [:pointers pointer-id] pointer)}
            held? (assoc :fx [(launch-held db pointer)])
            (and (not recent?) (not held?) (not= pointer-id "keyboard"))
            (assoc :dispatch-later [{:ms 500 :dispatch [:studio/gesture-long pointer-id unique]}])))))))

(rf/reg-event-fx :studio/held-failed
  (fn [{:keys [db]} [_ error]]
    ;; A transport failure cannot tell whether Ring accepted the launch.
    {:db (assoc db :recover-held? true)
     :fx [[:dispatch [:studio/failure error]] [:dispatch [:studio/reconcile-held]]]}))
(rf/reg-event-db :studio/gesture-move
  (fn [db [_ {:keys [pointer-id client-x client-y]}]]
    (if-let [pointer (get-in db [:pointers pointer-id])]
      (if (> (js/Math.hypot (- client-x (:client-x pointer)) (- client-y (:client-y pointer))) 12)
        (assoc-in db [:pointers pointer-id :moved?] true) db) db)))
(rf/reg-event-fx :studio/gesture-long
  (fn [{:keys [db]} [_ id press-id]]
    (let [pointer (get-in db [:pointers id])]
      (when (and press-id (= press-id (:press-id pointer)) (not-any? pointer [:moved? :recent-page?]))
        {:db (assoc-in db [:pointers id :long?] true)
         :dispatch [:studio/open-actions (select-keys pointer [:page-id :cue-id :x :y])]}))))

(defn- end-held [db {:keys [press-id epoch runtime-id] :as owner} id beacon?]
  (let [fields (cond-> {:instance-id id} press-id (assoc :command-id (str press-id "-end")))]
    (assoc-in (command db "instance/end" fields nil epoch beacon? runtime-id)
              [1 :on-failure] [:studio/held-end-failed owner])))

(defn- live-web-hold? [session {:keys [gesture-id source launch-mode phase]}]
  (and gesture-id (= "held" launch-mode) (= "running" phase)
       (= "web" (:adapter source)) (= session (:session-id source))))

(defn- current-show? [show owner]
  (and (= (:epoch owner) (:epoch show)) (= (:runtime-id owner) (:runtime-id show))))

(rf/reg-event-fx :studio/reconcile-held
  (fn [{:keys [db]} _]
    (let [show (:show db)
          holds (filter #(live-web-hold? (:session db) %) (:instances show))
          pressed (into {} (keep (fn [[id pointer]]
                                   (when (and (:held? pointer) (current-show? show pointer))
                                     [(:press-id pointer) id]))) (:pointers db))
          orphaned (remove #(contains? pressed (:gesture-id %)) holds)
          pending (set (map :gesture-id orphaned))
          ending (or (:ending-holds db) #{})
          attach (fn [state {:keys [id gesture-id]}]
                   (if-let [pointer (get pressed gesture-id)]
                     (assoc-in state [:pointers pointer :instance-id] id)
                     state))
          release (fn [{:keys [id gesture-id]}]
                    (end-held db (assoc (select-keys show [:epoch :runtime-id]) :press-id gesture-id) id false))]
      (when (get-in db [:connection :connected?])
        {:db (assoc (reduce attach db holds) :ending-holds pending)
         :fx (mapv release (remove #(ending (:gesture-id %)) orphaned))}))))

(rf/reg-event-fx :studio/held-end-failed
  (fn [{:keys [db]} [_ owner _error]]
    (when (current-show? (:show db) owner)
      {:db (assoc db :recover-held? true)
       :dispatch-later [{:ms 1000 :dispatch [:studio/retry-held-end owner]}]})))

(rf/reg-event-fx :studio/retry-held-end
  (fn [{:keys [db]} [_ owner]]
    (when (current-show? (:show db) owner)
      {:db (update db :ending-holds disj (:press-id owner))
       :dispatch [:studio/reconcile-held]})))

(rf/reg-event-fx :studio/held-launched
  (fn [{:keys [db]} [_ {:keys [pointer-id press-id] :as owner} {:keys [instance-id]}]]
    (cond
      (nil? instance-id) {:db (assoc db :recover-held? true) :dispatch [:studio/reconcile-held]}
      (and (current-show? (:show db) owner) (= press-id (get-in db [:pointers pointer-id :press-id])))
      {:db (-> db (assoc-in [:pointers pointer-id :instance-id] instance-id)
               (assoc-in [:ui :selected-instance] instance-id))}
      :else {:fx [(end-held db owner instance-id false)]})))

(defn release-pointer [db id beacon?]
  (if-let [pointer (get-in db [:pointers id])]
    (cond-> {:db (update db :pointers dissoc id)}
      (:instance-id pointer) (assoc :fx [(end-held db pointer (:instance-id pointer) beacon?)]))
    {}))
(rf/reg-event-fx :studio/gesture-up
  (fn [{:keys [db]} [_ id]]
    (let [pointer (get-in db [:pointers id]) result (release-pointer db id false)]
      (cond-> result
        (and pointer (not-any? pointer [:held? :moved? :long?]))
        (assoc :dispatch (if (and (not (:recent-page? pointer))
                                  (or (get-in db [:ui :edit?]) (nil? (:cue-id pointer))))
                           [:studio/open-actions (select-keys pointer [:cue-id :x :y])]
                           [:studio/launch (:cue-id pointer) (:mode pointer)]))))))
(rf/reg-event-fx :studio/gesture-cancel
  (fn [{:keys [db]} [_ id beacon?]] (release-pointer db id beacon?)))
(rf/reg-event-fx :studio/release-pointers
  (fn [{:keys [db]} [_ beacon?]]
    (reduce (fn [result id]
              (let [released (release-pointer (:db result) id beacon?)]
                {:db (or (:db released) (:db result)) :fx (into (:fx result) (:fx released))}))
            {:db db :fx []} (keys (:pointers db)))))

(rf/reg-event-fx :studio/master
  (fn [{:keys [db]} [_ value]] {:db (-> db (assoc-in [:show :master] value) (draft :master "master/set" {:value value}))
                               :dispatch [:studio/queue-param :master "master/set" {:value value}]}))
(defn set-param-fx [{:keys [db]} [_ instance-id param value]]
    {:db (-> db
             (update-in [:show :instances]
                        (fn [instances] (mapv #(if (= instance-id (:id %)) (assoc-in % [:params param] value) %) instances)))
             (draft [instance-id param] "param/set" {:instance-id instance-id :param (wire-id param) :value value}))
     :dispatch [:studio/queue-param [instance-id param] "param/set" {:instance-id instance-id :param (wire-id param) :value value}]})
(rf/reg-event-fx :studio/param-set set-param-fx)
(rf/reg-event-fx :studio/param-key
  (fn [{:keys [db] :as cofx} [_ instance-id param schema key]]
    (let [current (some #(when (= instance-id (:id %)) (get-in % [:params param])) (get-in db [:show :instances]))]
      (when-let [next (and (number? current) (widgets/keyboard-value schema current key))]
        (set-param-fx cofx [:studio/param-set instance-id param next])))))
(rf/reg-event-fx :studio/param-component
  (fn [{:keys [db] :as cofx} [_ instance-id param index value]]
    (let [instance (some #(when (= (:id %) instance-id) %) (get-in db [:show :instances]))
          current (get-in instance [:params param] [])]
      (set-param-fx cofx [:studio/param-set instance-id param (assoc (vec current) index value)]))))
(rf/reg-event-fx :studio/queue-param
  (fn [{:keys [db]} [_ key op fields]]
    {:db (assoc-in db [:pending-params key] {:op op :fields fields :epoch (get-in db [:show :epoch]) :runtime-id (get-in db [:show :runtime-id])})
     :dispatch-later (when (empty? (:pending-params db)) [{:ms 65 :dispatch [:studio/flush-params]}])}))
(rf/reg-event-fx :studio/flush-params
  (fn [{:keys [db]} _]
    {:db (assoc db :pending-params {})
     :fx (mapv (fn [[key {:keys [op fields epoch runtime-id]}]]
                 (let [[effect payload] (command db op fields [:studio/param-ack key (:value fields) epoch runtime-id] epoch false runtime-id)]
                   [effect (assoc payload :on-failure [:studio/param-failed key (:value fields) epoch runtime-id])])) (:pending-params db))}))
(rf/reg-event-db :studio/param-ack
  (fn [db [_ key value epoch runtime-id ack]]
    (if (and (= value (get-in db [:drafts key :fields :value])) (= epoch (get-in db [:drafts key :epoch]))
             (= runtime-id (get-in db [:drafts key :runtime-id])))
      (assoc-in db [:drafts key :revision] (:revision ack)) db)))
(rf/reg-event-fx :studio/param-failed
  (fn [{:keys [db]} [_ key value epoch runtime-id error]]
    {:db (if (and (= value (get-in db [:drafts key :fields :value])) (= epoch (get-in db [:drafts key :epoch]))
                  (= runtime-id (get-in db [:drafts key :runtime-id])))
           (update db :drafts dissoc key) db)
     :dispatch [:studio/failure error]}))

(rf/reg-event-db :studio/open-actions
  (fn [db [_ cell]]
    (let [page-id (or (:page-id cell) (:id (selected-recent-page db)) (get-in db [:ui :page-id]))]
      (if (recent-page? db page-id) db
        (-> db (assoc-in [:ui :action-cell] (assoc cell :page-id page-id))
            (assoc-in [:ui :assign-cue] (:id (first (get-in db [:show :cues])))))))))
(rf/reg-event-db :studio/close-actions (fn [db _] (assoc-in db [:ui :action-cell] nil)))
(defn put-cell [db {:keys [page-id x y]} cue-id]
  (let [page (some #(when (= page-id (:id %)) %) (get-in db [:show :pages]))
        cells (filterv #(not (and (= x (:x %)) (= y (:y %)))) (:cells page))
        cells (cond-> cells cue-id (conj {:x x :y y :cue-id cue-id}))]
    (command db "page/put" {:page (assoc page :cells cells)})))
(rf/reg-fx :studio/focus-instance
  (fn [id]
    (js/requestAnimationFrame
      (fn [] (js/requestAnimationFrame
               (fn []
                 (when-let [card (.querySelector js/document (str "[data-live-instance=\"" (.escape js/CSS (str id)) "\"]"))]
                   (.scrollIntoView card #js {:behavior "smooth" :block "nearest"})
                   (when-let [control (.querySelector card "button,input,select")]
                     (.focus control #js {:preventScroll true})))))))))
(rf/reg-event-fx :studio/cue-action [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ action]]
    (let [cell (get-in db [:ui :action-cell]) cue (cue db (:cue-id cell))
          closed (assoc-in db [:ui :action-cell] nil)]
      (case action
        :launch {:db closed :dispatch [:studio/launch (:id cue) "one-shot"]}
        :inspect (if-let [instance (first (active db (:id cue)))]
                   {:db (-> closed (assoc-in [:ui :selected-instance] (:id instance))
                            (assoc-in [:ui :performance-section] "live")
                            (assoc-in [:ui :performance :expanded (:id instance)] true))
                    :studio/focus-instance (:id instance)}
                   {:db closed :dispatch [:studio/launch (:id cue) "toggle"]})
        :copy {:db (assoc-in closed [:ui :clipboard] cue) :fx [(notice "Cue copied. Choose an empty cell to paste.")]}
        :paste (let [copied (assoc (get-in db [:ui :clipboard]) :id (str (get-in db [:ui :clipboard :id]) "-copy-" unique)
                                  :name (str (get-in db [:ui :clipboard :name]) " copy") :version 1)]
                 {:db closed :fx [(command db "cue/put" {:cue copied} [:studio/cue-copied cell (:id copied)])]})
        :assign {:db closed :fx [(put-cell db cell (get-in db [:ui :assign-cue])) (notice "Cue placed on the page.")]}
        :compose {:db closed :dispatch [:studio/composition-add (:id cue)]}
        :edit {:db closed :studio/navigate [:push :library] :dispatch [:studio/definition-load "effects" (:effect-id cue)]}
        :remove {:db closed :fx [(put-cell db cell nil) (notice "Cell cleared; its definition remains in the library.")]}
        {:db closed}))))
(rf/reg-event-fx :studio/cue-copied
  (fn [{:keys [db]} [_ cell id _]] {:fx [(put-cell db cell id) (notice "Cue pasted.")]}))
(rf/reg-event-db :studio/add-page
  (fn [db _] (assoc-in db [:ui :prompt] {:title "A new canvas" :label "Page name" :value "" :action :add-page})))
(rf/reg-event-db :studio/prompt-cancel (fn [db _] (assoc-in db [:ui :prompt] nil)))
(rf/reg-event-fx :studio/prompt-submit [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    (let [title (str/trim (get-in db [:ui :prompt :value] "")) id (str (slug title) "-" (subs unique 0 8))]
      (when (seq title)
        {:db (-> db (assoc-in [:ui :prompt] nil) (assoc-in [:ui :page-id] id))
         :fx [(command db "page/put" {:page {:id id :name title :cells []}} [:studio/page-created id])]}))))
(rf/reg-event-fx :studio/page-created
  (fn [{:keys [db]} [_ id _]]
    {:db (update db :ui assoc :page-id id :recent-page? false) :dispatch [:studio/persist-ui]}))

(rf/reg-event-db :studio/composition-add
  (fn [db [_ id]]
    (let [id (or id (get-in db [:ui :slot-cue]) (:id (first (get-in db [:show :cues]))))]
      (if-let [cue (cue db id)]
        (update-in db [:ui :composition :slots] conj {:cue-id id :params {} :duration {:beats 1}}) db))))
(rf/reg-event-db :studio/slot-duration
  (fn [db [_ index value unit]]
    (update-in db [:ui :composition :slots index]
      (fn [slot]
        (let [[old-unit old-value] (or (first (:duration slot)) (when-let [ms (:duration-ms slot)] [:ms ms]) [:beats 1])]
          (-> slot (dissoc :duration-ms :duration-beats)
              (assoc :duration {(if unit (keyword unit) old-unit) (if (some? value) value old-value)})))))))
(defn slot-parameters [db index]
  (let [slot (get-in db [:ui :composition :slots index]) cue (cue db (:cue-id slot))
        definition (effect db (:effect-id cue))]
    (merge (into {} (map (fn [[key schema]] [key (:default schema)]) (:params definition)))
           (:params cue) (:saved-params cue) (:params slot))))
(rf/reg-event-db :studio/slot-param
  (fn [db [_ index parameter value]] (assoc-in db [:ui :composition :slots index :params parameter] value)))
(rf/reg-event-db :studio/slot-param-key
  (fn [db [_ index parameter schema key]]
    (if-let [next (widgets/keyboard-value schema (get (slot-parameters db index) parameter) key)]
      (assoc-in db [:ui :composition :slots index :params parameter] next) db)))
(rf/reg-event-db :studio/slot-param-component
  (fn [db [_ index parameter component value]]
    (assoc-in db [:ui :composition :slots index :params parameter]
              (assoc (vec (get (slot-parameters db index) parameter)) component value))))
(rf/reg-event-db :studio/slot-params-reset
  (fn [db [_ index]] (assoc-in db [:ui :composition :slots index :params] {})))
(rf/reg-event-db :studio/composition-remove
  (fn [db [_ index]] (update-in db [:ui :composition :slots] #(vec (concat (subvec % 0 index) (subvec % (inc index)))))))
(rf/reg-event-db :studio/composition-move
  (fn [db [_ index offset]]
    (update-in db [:ui :composition :slots]
      (fn [slots] (let [target (+ index offset)] (if (<= 0 target (dec (count slots))) (assoc slots index (nth slots target) target (nth slots index)) slots))))))
(rf/reg-event-db :studio/composition-open
  (fn [db [_ composition]]
    ;; Presentation converts beat ratios to JS numbers. Keep the typed program
    ;; when opening a composition so editing its title or slots preserves ratios.
    (let [typed (some #(when (= (wire-id (:id %)) (wire-id (:id composition))) %)
                      (get-in db [:server-projection :view :compositions]))
          slots (mapv (fn [i slot]
                        (cond-> slot
                          (= "previous" (:targets slot)) (assoc :targets :previous)
                          (some? (get-in typed [:slots i :transforms]))
                          (assoc :transforms (get-in typed [:slots i :transforms]))))
                      (range) (:slots composition))]
      (assoc-in db [:ui :composition] (assoc (dissoc composition :running? :playback) :slots slots)))))
(rf/reg-event-db :studio/composition-new [(rf/inject-cofx :studio/unique)]
  (fn [db _] (assoc-in db [:ui :composition] {:id "new-composition" :name "New composition" :mode "sequence" :loop "once" :slots []})))
(rf/reg-event-fx :studio/composition-save
  (fn [{:keys [db]} [_ run?]]
    (let [composition (get-in db [:ui :composition])]
      (if (seq (:slots composition))
        {:studio/http {:path "/api/composition" :body composition :on-success [:studio/composition-saved (:id composition) run?]}}
        {:dispatch [:studio/failure "Add at least one cue to the composition."]}))))
(rf/reg-event-fx :studio/composition-saved
  (fn [_ [_ id run? _]]
    (if run? {:studio/http {:path "/api/composition/run" :body {:id id} :on-success [:studio/success "Composition playing."]}}
      {:dispatch [:studio/toast "Composition saved."]})))
(rf/reg-event-fx :studio/composition-stop
  (fn [{:keys [db]} _] {:studio/http {:path "/api/composition/stop" :body {:id (get-in db [:ui :composition :id])}
                                    :on-success [:studio/success "Composition stopped."]}}))
(rf/reg-event-fx :studio/success (fn [_ [_ text _]] {:dispatch [:studio/toast text]}))

(rf/reg-event-db :studio/rig-atmosphere
  (fn [db [_ field value]] (fog/set-room-value db field value)))
(rf/reg-event-db :studio/rig-set
  (fn [db [_ path value]] (-> db (assoc-in (into [:ui :rig-draft] path) value) (assoc-in [:ui :rig-dirty?] true))))
(rf/reg-event-db :studio/rig-remove
  (fn [db [_ index]] (-> db (update-in [:ui :rig-draft :fixtures] #(vec (concat (subvec % 0 index) (subvec % (inc index))))) (assoc-in [:ui :rig-dirty?] true))))
(rf/reg-event-fx :studio/truss-add [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    {:db (-> db (update-in [:ui :rig-draft :trusses] (fnil conj [])
                          {:id (str "truss-" (subs unique 0 8)) :name "New truss"
                           :position [0 3 0] :rotation-deg [0 0 0] :length 4})
                (assoc-in [:ui :rig-dirty?] true))}))
(rf/reg-event-db :studio/truss-remove
  (fn [db [_ index]]
    (-> db (update-in [:ui :rig-draft :trusses] #(vec (concat (subvec % 0 index) (subvec % (inc index)))))
        (assoc-in [:ui :rig-dirty?] true) (assoc-in [:ui :rig-drag] nil))))
(rf/reg-event-db :studio/truss-set
  (fn [db [_ index path value]]
    (-> db (update-in [:ui :rig-draft :trusses index]
                      (fn [truss] (assoc-in (cond-> truss
                                            (= :rotation-deg (first path)) (update :rotation-deg #(or % [0 0 0]))
                                            (= :position (first path)) (update :position #(or % [0 3 0]))) path value)))
        (assoc-in [:ui :rig-dirty?] true))))
(defn move-rig-item [db kind index x z]
  (if (and (#{:fixtures :trusses} kind) (number? x) (number? z)
           (js/Number.isFinite x) (js/Number.isFinite z) (get-in db [:ui :rig-draft kind index]))
    (let [path [:ui :rig-draft kind index :position] position (get-in db path [0 3 0])
          round #(/ (js/Math.round (* 1000000 %)) 1000000)]
      (-> db (assoc-in path [(round x) (nth position 1 3) (round z)]) (assoc-in [:ui :rig-dirty?] true))) db))
(rf/reg-event-db :studio/rig-drag-start
  (fn [db [_ kind index pointer-id x z]]
    (if (get-in db [:ui :rig-drag]) db
      (let [[px _ pz] (get-in db [:ui :rig-draft kind index :position] [0 3 0])]
        (assoc-in db [:ui :rig-drag] {:kind kind :index index :pointer-id pointer-id :offset [(- px x) (- pz z)]})))))
(rf/reg-event-db :studio/rig-drag-move
  (fn [db [_ pointer-id x z]]
    (let [{:keys [kind index offset] :as drag} (get-in db [:ui :rig-drag])
          snap #(/ (js/Math.round (* 10 %)) 10)]
      (if (= pointer-id (:pointer-id drag)) (move-rig-item db kind index (snap (+ x (first offset))) (snap (+ z (second offset)))) db))))
(rf/reg-event-db :studio/rig-drag-end
  (fn [db [_ pointer-id]]
    (if (= pointer-id (get-in db [:ui :rig-drag :pointer-id])) (assoc-in db [:ui :rig-drag] nil) db)))
(rf/reg-event-db :studio/rig-nudge
  (fn [db [_ kind index dx dz]]
    (let [[x _ z] (get-in db [:ui :rig-draft kind index :position] [0 3 0])]
      (move-rig-item db kind index (+ x dx) (+ z dz)))))
(rf/reg-event-fx :studio/rig-add [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    (let [fixtures (get-in db [:ui :rig-draft :fixtures]) universe (apply max 1 (map :universe fixtures))
          occupied (filter #(= universe (:universe %)) fixtures)
          end (apply max 0 (map #(+ (:address %) (apply max 1 (flatten (vals (:channels %)))) -1) occupied))
          [universe address] (if (> (+ end 12) 512) [(inc universe) 1] [universe (inc end)])
          fixture {:id (str "head-" (subs unique 0 8)) :name (str "Beam " (inc (count fixtures))) :position [0 3.5 0]
                   :universe universe :address address :channels {:dimmer 1 :red 2 :green 3 :blue 4 :white 5 :pan [6 7] :tilt [8 9] :focus 10 :zoom 11 :strobe 12}
                   :rgbw-policy "extract-min" :capabilities ["color" "dimmer" "movement" "focus" "strobe"]}]
      {:db (-> db (update-in [:ui :rig-draft :fixtures] conj fixture) (assoc-in [:ui :rig-dirty?] true))})))
(rf/reg-event-fx :studio/rig-save
  (fn [{:keys [db]} _]
    (let [rig (get-in db [:ui :rig-draft])
          path [:room :measured-fog]
          typed (get-in db (into [:server-projection :view :rig] path))
          ;; Preserve the phone's typed method and metadata on unrelated layout edits.
          body (cond-> (projection/rig-request rig (get-in db [:server-projection :view :rig]))
                 (and typed (= (get-in rig path) (projection/presentation typed))) (assoc-in path typed))]
      (if-let [message (fog/room-error (:room rig))]
        {:dispatch [:studio/toast message true]}
        {:studio/http {:path "/api/rig" :body body
                       :on-success [:studio/rig-saved rig (get-in db [:show :epoch]) (get-in db [:show :runtime-id])]}}))))
(rf/reg-event-fx :studio/rig-saved
  (fn [{:keys [db]} [_ rig epoch runtime-id _]]
    (when (and (= epoch (get-in db [:show :epoch])) (= runtime-id (get-in db [:show :runtime-id])))
      (let [unchanged? (= rig (get-in db [:ui :rig-draft]))]
        {:db (assoc-in db [:ui :rig-dirty?] (not unchanged?))
         :dispatch [:studio/toast (if unchanged? "Rig saved. Preview and output use this layout."
                                    "Rig saved. Your newer edits remain unsaved.")]}))))

(rf/reg-event-fx :studio/definition-load
  (fn [_ [_ kind id]] {:studio/http {:path (str "/api/definition?kind=" kind "&id=" (js/encodeURIComponent id))
                                    :on-success [:studio/definition-loaded kind id]}}))
(rf/reg-event-fx :studio/definition-loaded
  (fn [{:keys [db]} [_ kind id {:keys [source]}]]
    (let [version (js/parseInt (or (second (re-find #":version\s+(\d+)" source)) "1") 10)]
      {:db (update-in db [:ui :definition] merge {:kind kind :source source :loaded-id id :version version
                                                :mode :source :auto? false :status (str id " · v" version)
                                                :result "Loaded from the running show." :error? false})
       :studio/nvim-source source})))
(rf/reg-event-fx :studio/definition-new
  (fn [{:keys [db]} _] {:db (update-in db [:ui :definition] merge {:kind "effects" :source initial-source :loaded-id nil :version 1 :auto? false :status "New definition" :result ""})
                        :studio/nvim-source initial-source}))
(defn- edit-definition-source [db source]
  (let [revision (inc (get-in db [:ui :definition :edit-revision] 0))]
    {:db (update-in db [:ui :definition] merge {:source source :status "Unpublished changes" :edit-revision revision
                                               :error? false :result ""})
     :dispatch-later (when (get-in db [:ui :definition :auto?]) [{:ms 850 :dispatch [:studio/auto-publish revision]}])}))

(rf/reg-event-fx :studio/definition-source
  (fn [{:keys [db]} [_ source]] (edit-definition-source db source)))
(rf/reg-event-fx :studio/auto-publish
  (fn [{:keys [db]} [_ revision]]
    (when (and (get-in db [:ui :definition :auto?]) (= revision (get-in db [:ui :definition :edit-revision])))
      {:dispatch [:studio/definition-publish true]})))
(rf/reg-event-fx :studio/editor-engine
  (fn [{:keys [db]} [_ engine]]
    (cond
      (and (= engine :textarea) (= :neovim (get-in db [:ui :definition :engine])))
      {:studio/nvim-read {:on-success [:studio/editor-to-text]}}
      (and (= engine :neovim) (not (and (.-crossOriginIsolated js/window) (exists? js/SharedArrayBuffer))))
      {:dispatch [:studio/editor-error "Neovim requires localhost or HTTPS with COOP/COEP. The accessible editor remains available."]}
      :else {:db (assoc-in db [:ui :definition :engine] engine)})))
(rf/reg-event-db :studio/editor-to-text
  (fn [db [_ source]] (update-in db [:ui :definition] merge {:source source :engine :textarea})))
(rf/reg-event-db :studio/editor-status
  (fn [db [_ status]] (assoc-in db [:ui :definition :editor-status] status)))
(rf/reg-event-db :studio/editor-error
  (fn [db [_ error]] (update-in db [:ui :definition] merge {:result (str error) :error? true :publishing? false})))
(rf/reg-event-db :studio/editor-unavailable
  (fn [db [_ error]] (update-in db [:ui :definition] merge {:engine :textarea :result (str error) :error? true :publishing? false})))
(rf/reg-event-fx :studio/nvim-input (fn [_ [_ keys]] {:studio/nvim-input keys}))

(defn musical-duration [value unit]
  (when-not (and (number? value) (js/Number.isFinite value) (<= 0 value) (contains? #{"beats" "bars" "phrases" "ms"} unit))
    (throw (js/Error. "Choose a finite musical duration and a supported unit.")))
  {(keyword unit) value})
(defn builder-definition [{:keys [id name color level duration-value duration-unit release-value release-unit keyframes blocks]} version]
  (when-not (re-matches #"[a-zA-Z][a-zA-Z0-9._/-]*" id) (throw (js/Error. "Use an identifier beginning with a letter.")))
  (when-not (and (number? level) (<= 0 level 1) (pos? duration-value) (<= 0 release-value))
    (throw (js/Error. "Intensity must be 0–1, duration positive, and release zero or positive.")))
  (cond-> {:id (keyword id) :version version :name name :swatch color :duration (musical-duration duration-value duration-unit)
   :envelope {:attack (if (= release-unit "ms") {:ms 100} {:beats 0.125}) :release (musical-duration release-value release-unit)}
   :params {:level {:type :number :default level :bounds [0.0 1.0]}}
   :outputs {:color {:op :const :value (mapv #(/ (js/parseInt (subs color % (+ % 2)) 16) 255) [1 3 5])}
             :dimmer {:op :param :key :level}}}
    keyframes (assoc :keyframes keyframes)
    (seq blocks) (assoc :blocks blocks)))

(rf/reg-event-fx :studio/apply-block-source
  (fn [{:keys [db]} [_ baseline updated]]
    (if (= baseline (get-in db [:ui :definition :source]))
      (assoc (edit-definition-source db updated) :studio/nvim-source updated)
      {:dispatch [:studio/editor-error "The source changed. Reopen the block editor."]})))

(rf/reg-event-fx :studio/builder-generate
  (fn [{:keys [db]} _]
    (try
      (let [source (pr-str (builder-definition (get-in db [:ui :builder]) (get-in db [:ui :definition :version] 1)))]
        {:db (update-in db [:ui :definition] merge {:source source :kind "effects" :mode :source :status "Generated EDN"})
         :studio/nvim-source source})
      (catch :default error {:dispatch [:studio/editor-error (error-message error)]}))))
(defn- publication-current? [db token]
  (and (some? token) (= token (get-in db [:ui :definition :publication]))
       (= (:epoch token) (get-in db [:show :epoch])) (= (:runtime-id token) (get-in db [:show :runtime-id]))))

(rf/reg-event-fx :studio/publication-error
  (fn [{:keys [db]} [_ token error]]
    (when (publication-current? db token)
      {:db (update-in db [:ui :definition] merge {:publishing? false :error? true :status "Publication failed"
                                                  :result (error-message error)})})))

(rf/reg-event-fx :studio/definition-publish
  (fn [{:keys [db]} [_ automatic?]]
    (when-not (get-in db [:ui :definition :publishing?])
      (try
        (let [definition (get-in db [:ui :definition]) built? (= :form (:mode definition))
              token (assoc (select-keys (:show db) [:runtime-id :epoch]) :id (str (random-uuid)))
              pending (update-in db [:ui :definition] merge {:publication token :publishing? true})
              source (if built? (pr-str (builder-definition (get-in db [:ui :builder]) (:version definition))) (:source definition))]
          (if (and (= :neovim (:engine definition)) (not built?))
            {:db pending :studio/nvim-read {:on-success [:studio/publish-source token automatic? nil]
                                           :on-failure [:studio/publication-error token]}}
            {:db pending :dispatch [:studio/publish-source token automatic? (when built? (get-in db [:ui :builder])) source]}))
        (catch :default error {:dispatch [:studio/editor-error (error-message error)]})))))
(rf/reg-event-fx :studio/publish-source
  (fn [{:keys [db]} [_ token automatic? built source]]
    (when (publication-current? db token)
      (let [{:keys [kind loaded-id version]} (get-in db [:ui :definition])
          kind (if built "effects" kind)
          existing (when built (effect db (:id built)))
          expected (if built (:version existing) (when loaded-id version))]
      {:db (update-in db [:ui :definition] merge {:publishing? true :source source :kind kind})
       :studio/http {:path "/api/definition" :body (cond-> {:kind kind :source source :expected-epoch (:epoch token) :runtime-id (:runtime-id token)} expected (assoc :expected-version expected))
                     :on-success [:studio/published token automatic? built kind expected]
                     :on-failure [:studio/publication-error token]}}))))
(rf/reg-event-fx :studio/published
  (fn [{:keys [db]} [_ token automatic? built kind expected ack]]
    (when (publication-current? db token)
      (let [id (or (:id ack) (:definition-id ack) (:id built) (get-in db [:ui :definition :loaded-id]))
          version (or (:version ack) (when expected (inc expected)) 1)
          existing-cue (when built (cue db (:id built)))
          cue-source (when built (pr-str {:id (keyword (:id built)) :version 1 :name (:name built)
                                         :effect-id (keyword (:id built)) :swatch (:color built)
                                         :params {} :duration (musical-duration (:duration-value built) (:duration-unit built)) :reload-policy :preserve-state}))]
      {:db (update-in db [:ui :definition] merge {:publishing? false :loaded-id id :version version :status "Published" :error? false
                                                :result "Validated and published. The running show now has this definition."})
       :fx (cond-> [] (not automatic?) (conj (notice "Definition validated and published."))
             (and built (:cue? built)) (conj [:studio/http {:path "/api/definition"
                                                          :body (cond-> {:kind "cues" :source cue-source :runtime-id (:runtime-id token) :expected-epoch (:epoch token)}
                                                                  existing-cue (assoc :expected-version (:version existing-cue)))
                                                          :on-success [:studio/publication-complete token]
                                                          :on-failure [:studio/publication-error token]}]))}))))

(rf/reg-event-fx :studio/publication-complete
  (fn [{:keys [db]} [_ token _]]
    (when (publication-current? db token) {:fx [(notice "Effect and matching cue published.")]})))

(defn auto-activity [db]
  (let [form (get-in db [:ui :auto-form])
        saved (get-in db [:show :auto :config :activity-target])]
    {:enabled? (get form :target-activity? (some? saved))
     :target (or (:activity-target form) saved 0.5)}))

(rf/reg-event-fx :studio/auto-start
  (fn [{:keys [db]} _]
    (let [{:keys [interval-value interval-unit count scope mode duration-value duration-unit seed]} (get-in db [:ui :auto-form])
          {:keys [enabled? target]} (auto-activity db)]
      {:studio/http {:path "/api/auto"
                     :body (cond-> {:enabled? true :interval (musical-duration interval-value interval-unit) :count count
                                    :scope (if (= scope "all") "all" {:page-id (get-in db [:ui :page-id])})
                                    :launch {:mode mode :duration (musical-duration duration-value duration-unit)}
                                    :seed (or seed (get-in db [:show :auto :config :seed]) 42)}
                             enabled? (assoc :activity-target target))
                     :on-success [:studio/success "Auto mode started. You can keep playing alongside it."]}})))
(rf/reg-event-fx :studio/auto-stop
  (fn [_ _] {:studio/http {:path "/api/auto" :body {:enabled? false} :on-success [:studio/success "Auto mode stopped."]}}))

(defn response-definition [db]
  (let [{:keys [id params scope attack-value release-value unit curve]} (get-in db [:ui :response-form])
        instance (some #(when (= (:id %) (get-in db [:ui :selected-instance])) %) (get-in db [:show :instances]))
        params (->> (str/split (or params "") #"[,\s]+") (remove str/blank?) distinct vec)]
    (when (or (str/blank? id) (empty? params)) (throw (js/Error. "Name the response and choose at least one numeric parameter.")))
    (when (and (not= scope "all") (nil? instance)) (throw (js/Error. "Select a running effect before targeting this response.")))
    (cond-> {:id id :params params :operation "multiply" :range [0 1]
             :envelope {:attack (musical-duration attack-value unit) :release (musical-duration release-value unit) :curve curve}}
      (= scope "instance") (assoc :select {:instance-ids [(:id instance)]})
      (= scope "effect") (assoc :select {:effect-ids [(:effect-id instance)]}))))
(rf/reg-event-fx :studio/response-start
  (fn [{:keys [db]} _]
    (try {:studio/http {:path "/api/response" :body (response-definition db)
                       :on-success [:studio/success "Response started. Release fades down; remove restores the original parameters."]}}
         (catch :default error {:dispatch [:studio/failure (error-message error)]}))))
(rf/reg-event-fx :studio/response-action
  (fn [_ [_ action id]]
    {:studio/http {:path (str "/api/response/" (if (= action :release) "release" "delete")) :body {:id id}
                   :on-success [:studio/success (if (= action :release) "Response release started." "Response removed; original parameters restored.")]}}))
(rf/reg-event-fx :studio/generate-page
  (fn [_ _] {:studio/http {:path "/api/page/generate" :body {:page-id "registry" :options {:columns 8 :rows 8}}
                          :on-success [:studio/page-generated]}}))
(rf/reg-event-fx :studio/page-generated
  (fn [{:keys [db]} [_ result]]
    {:db (update db :ui assoc :page-id (:page-id result) :recent-page? false)
     :fx [[:studio/navigate [:push :perform]] [:dispatch [:studio/persist-ui]]
          (notice (str "Registry page ready: " (count (:placements result)) " new cues, "
                       (count (:collisions result)) " conflicts, " (count (:overflow result)) " without room."))]}))
(rf/reg-event-fx :studio/export (fn [_ _] {:studio/download true}))
(rf/reg-event-fx :studio/import-file (fn [_ [_ file]] {:studio/read-file file}))
(rf/reg-event-fx :studio/import-source
  (fn [_ [_ source]] {:studio/http {:path "/api/import" :body {:source source} :on-success [:studio/imported]}}))
(rf/reg-event-fx :studio/imported
  (fn [{:keys [db]} _] {:db (assoc-in db [:ui :rig-dirty?] false) :dispatch [:studio/toast "Show imported."]}))
(rf/reg-event-fx :studio/preview-ready
  (fn [{:keys [db]} [_ supported? owner]]
    {:db (assoc-in db [:ui :preview-controls?] (boolean supported?))
     :fx (into [[:studio/preview (cond-> [:fog (get-in db [:ui :fog])] owner (conj owner))]]
               (when supported? (mapv (fn [[key value]] [:studio/preview (cond-> [key value] owner (conj owner))])
                                      (merge {:worklight 0 :quality "auto" :camera "audience"} (get-in db [:ui :preview-settings])))))}))
(rf/reg-event-fx :studio/preview-setting
  (fn [{:keys [db]} [_ key value]]
    (let [valid? (case key :worklight (and (number? value) (js/Number.isFinite value) (<= 0 value 1))
                           :bloom (and (number? value) (<= 0 value 2))
                           :grain (and (number? value) (<= 0 value 0.08))
                           :exposure (and (number? value) (<= 0 value 3))
                           :quality (contains? #{"auto" "mobile" "desktop" "high"} value)
                           :lighting-mode (#{"hold" "interpolate"} value)
                           :camera (contains? #{"audience" "overview"} value) false)]
      (when valid? {:db (assoc-in db [:ui :preview-settings key] value) :studio/preview [key value]}))))
(rf/reg-event-fx :studio/preview-expand
  (fn [{:keys [db]} _] {:db (update-in db [:ui :preview-expanded?] not)
                        :dispatch-later [{:ms 30 :dispatch [:studio/preview-resize]}]}))
(rf/reg-event-fx :studio/preview-fullscreen
  (fn [{:keys [db]} _]
    {:db (assoc-in db [:ui :fullscreen-preview-controls?] false)
     :studio/preview-fullscreen true}))
(rf/reg-event-fx :studio/preview-fog
  (fn [{:keys [db]} [_ value]] {:db (assoc-in db [:ui :fog] value) :studio/preview [:fog value]}))
(rf/reg-event-fx :studio/preview-reset
  (fn [{:keys [db]} _]
    {:db (assoc-in db [:ui :preview-settings :camera] "audience") :studio/preview [:reset nil]}))
(doseq [[event action] [[:studio/preview-burst :burst] [:studio/preview-resize :resize]]]
  (rf/reg-event-fx event (fn [_ [_ value]] {:studio/preview [action value]})))
