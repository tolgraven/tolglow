(ns tolglow.studio.events
  "The studio application is data + re-frame transitions. I/O lives in effects."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.checkpoints :as checkpoints]
            [tolglow.studio.fog-fields :as fog]
            [tolglow.studio.frame-stream :as frame-stream]
            [tolglow.studio.operator-history :as history]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.page-routes :as pages]
            [tolglow.studio.pretty :as pretty]
            [tolglow.studio.reactive-editor :as reactive-editor]
            [tolglow.studio.state-socket]
            [tolglow.studio.spatial-drafts :as spatial-drafts]
            [tolglow.studio.show-window :as show-window]
            [tolglow.studio.view-tabs :as view-tabs]
            [tolglow.transit :as transit]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.visualization.control-geometry :as control-geometry]))

(def initial-source
  "{:id :after-hours\n :version 1\n :name \"After hours\"\n :swatch \"#b981a3\"\n :duration {:beats 4}\n :envelope {:attack {:beats 0.125} :release {:beats 1}}\n :params {:level {:type :number :default 0.7 :bounds [0.0 1.0]}}\n :outputs {:color {:op :const :value [0.725 0.506 0.639]}\n           :dimmer {:op :param :key :level}}}")

(def launch-grids
  [["off" "Off" nil] ["quarter-beat" "¼ beat" {:beats 0.25}] ["half-beat" "½ beat" {:beats 0.5}]
   ["beat" "Beat" {:beats 1}] ["bar" "Bar" {:bars 1}] ["phrase" "Phrase" {:phrases 1}]])

(defn- selected-grid [db]
  (some (fn [[choice _ grid]] (when (= choice (get-in db [:ui :launch-grid])) grid)) launch-grids))

(def default-preview-settings
  {:worklight 0.12 :quality "auto" :light-budget "auto" :camera "audience" :lighting-mode "interpolate"})

(def light-budgets
  "Room full-light budget choices: Auto keeps the quality tier's default."
  ["auto" "4" "8" "12" "16"])

(defn preview-setting-valid? [key value]
  (case key
    :worklight (and (number? value) (js/Number.isFinite value) (<= 0 value 1))
    :bloom (and (number? value) (<= 0 value 2))
    :grain (and (number? value) (<= 0 value 0.08))
    :exposure (and (number? value) (<= 0 value 3))
    :quality (contains? #{"auto" "mobile" "desktop" "high"} value)
    :light-budget (contains? (set light-budgets) value)
    :lighting-mode (contains? #{"hold" "interpolate"} value)
    :camera (contains? #{"audience" "overview"} value)
    false))

;; Quality and the full-light budget follow this viewer's GPU, so this browser
;; keeps them. The Mac app's loopback origin changes on every launch, so it
;; keeps one copy per machine (not per show folder, which travels between
;; machines) and hands it back in `window.tolglowStudioWindow.roomView`.
;; Other Room settings last for the page.
(def room-view-keys [:quality :light-budget])

(def room-view-storage-key "tolglow.studio.room-view")

(defn restore-room-view
  "Install this viewer's saved Room quality and light budget, dropping invalid values."
  [db saved]
  (reduce (fn [db key]
            (let [value (get saved key)]
              (cond-> db (preview-setting-valid? key value) (assoc-in [:ui :preview-settings key] value))))
          db room-view-keys))

(defn- room-view-storage []
  (try (.-localStorage js/window) (catch :default _ nil)))

(defn decode-room-view
  "Saved Room view settings from their JSON text, or nil unless it is an object."
  [text]
  (try (let [value (some-> text js/JSON.parse (js->clj :keywordize-keys true))]
         (when (map? value) value))
       (catch :default _ nil)))

(rf/reg-cofx :studio/room-view-saved
  (fn [cofx _]
    (assoc cofx :studio/room-view-saved
           (decode-room-view
            (show-window/newest-text (try (:room-view (show-window/host-config)) (catch :default _ nil))
                                     (try (some-> (room-view-storage) (.getItem room-view-storage-key))
                                          (catch :default _ nil)))))))

(rf/reg-fx :studio/room-view-store
  (fn [settings]
    ;; Both copies carry `savedAt`, so a reload prefers the newer one.
    (let [text (js/JSON.stringify (clj->js (assoc settings :savedAt (.now js/Date))))]
      (try (some-> (room-view-storage) (.setItem room-view-storage-key text)) (catch :default _ nil))
      (show-window/post! {:type "room-view" :room-view text}))))

(defn effective-lights
  "The full-light budget a Room engine reports (Perform first), or nil before one does."
  [db]
  (let [reported (get-in db [:ui :preview-lights])]
    (or (get reported :perform) (some val reported))))

(defn light-budget-label
  "Option text. The chosen option names the engine's effective budget, short
  enough for the narrow Room panel: \"Auto (8)\", \"16 (8 now)\"."
  [choice chosen? effective]
  (let [base (if (= "auto" choice) "Auto full lights" (str choice " full lights"))]
    (cond
      (not (and chosen? (number? effective))) base
      (= "auto" choice) (str "Auto (" effective ")")
      (not= effective (js/parseInt choice 10)) (str choice " (" effective " now)")
      :else base)))

(rf/reg-event-db :studio/preview-lights
  (fn [db [_ owner budget]]
    (if (and (int? budget) (<= 0 budget 16))
      (assoc-in db [:ui :preview-lights owner] budget)
      (cond-> db (contains? (get-in db [:ui :preview-lights]) owner) (update-in [:ui :preview-lights] dissoc owner)))))

(def default-builder
  {:id "after-hours" :name "After hours" :color "#b981a3" :level 0.7
   :duration-value 4 :duration-unit "beats" :release-value 1 :release-unit "beats" :cue? true})

(defn initial-db [session]
  {:session session :show nil :pointers {} :pending-params {} :drafts {} :stream-id nil
   :connection {:connected? false :error "Connecting to the studio…"}
   :ui {:view :perform :page-id nil :selected-instance nil :launch-mode "toggle" :launch-grid "off" :edit? false
        :preview-settings default-preview-settings :preview-expanded? false :preview-controls? false
        :definition-search "" :assign-cue "" :toast nil :action-cell nil :clipboard nil :prompt nil
        :composition {:id "evening-drift" :name "Evening drift" :mode "sequence" :loop "once" :slots []}
        :definition {:kind "effects" :source initial-source :version 1 :engine :textarea :mode :source
                     :auto? false :status "Ready to create" :result "" :error? false}
        :builder default-builder
        :response-form {:id "performance-envelope" :params "level" :scope "all"
                        :attack-value 0.5 :decay-value 0 :sustain 1 :release-value 1
                        :unit "beats" :curve "smoothstep"}
        :auto-form {:interval-value 8 :interval-unit "beats" :count 1 :scope "page" :mode "timed" :duration-value 4 :duration-unit "beats"}
        :rig-draft nil :rig-dirty? false}})

(defn error-message [error]
  (or (when (map? error) (or (:message error) (:error error) (:reason error)))
      (.-message error) (str error)))

(defn- dispatch-result [event result] (when event (rf/dispatch (conj event result))))

(def guarded-paths
  #{"/api/definition" "/api/materials" "/api/rig" "/api/composition" "/api/composition/append" "/api/composition/cards" "/api/composition/run"
    "/api/composition/stop" "/api/import" "/api/auto" "/api/response"
    "/api/response/release" "/api/response/delete" "/api/page/generate" "/api/settings"
    "/api/fixtures/publish" "/api/fixtures/patch"
    "/api/sync/configure" "/api/sync/start" "/api/sync/stop" "/api/sync/source" "/api/sync/handoff" "/api/sync/arrange" "/api/sync/learn-parameter" "/api/sync/forget-parameter"
    "/api/audio/start" "/api/audio/stop"
    "/api/sensors/stage/start" "/api/sensors/stage/stop"
    "/api/sensors/audience-zone" "/api/sensors/pair" "/api/sensors/configure" "/api/sensors/arm" "/api/sensors/calibrate" "/api/sensors/osc/start" "/api/sensors/osc/stop"
    "/api/repl/eval" "/api/repl/interrupt" "/api/repl/scratch" "/api/repl/session"
    "/api/patch/save" "/api/patch/enabled" "/api/video/shader/save"
    "/api/video/shader/edit" "/api/video/shader/undo" "/api/video/shader/redo"
    "/api/modulation/start" "/api/modulation/stop" "/api/modulation/hold" "/api/modulation/resume" "/api/modulation/cue"})

(defn guard-request [db request]
  (if (and (:body request) (guarded-paths (:path request)))
    (update request :body #(merge {:expected-epoch (get-in db [:show :epoch])}
                                  (when-let [runtime (or (get-in db [:show :runtime-id]) (show-window/runtime-id db))]
                                    {:runtime-id runtime}) %))
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

(defn http! [{:keys [path body typed? raw-response? on-success on-failure on-uncertain timeout-ms]}]
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
                                    status (.-status response)
                                    data (cond
                                           (str/includes? type "application/transit+json")
                                           (let [v (transit/decode text)] (if raw-response? v (projection/presentation v)))
                                           (str/includes? type "application/json")
                                           (js->clj (js/JSON.parse text) :keywordize-keys true)
                                           :else {:message text})
                                    rejected? (contains? #{:rejected "rejected"} (:status data))]
                                (if (or (not (.-ok response)) rejected? (:error data))
                                  (throw (if (and on-uncertain (or rejected? (and (<= 400 status 499) (not= 408 status))))
                                           (ex-info (str (error-message data)) {::definite-http-failure true})
                                           (js/Error. (str (error-message data)))))
                                  data)))))))
        (.then #(dispatch-result on-success %))
        (.catch (fn [error]
                  (if (and on-uncertain (not (::definite-http-failure (ex-data error))))
                    (rf/dispatch on-uncertain)
                    (dispatch-result (or on-failure [:studio/failure]) (error-message error)))))
        (.finally #(js/clearTimeout timeout)))))
(rf/reg-fx :studio/http http!)
(rf/reg-fx :studio/scroll-top (fn [_] (.scrollTo js/window #js {:top 0 :behavior "auto"})))
(rf/reg-fx :studio/send-command
  (fn [{:keys [op fields epoch session runtime-id on-success on-failure on-uncertain timeout-ms beacon?]}]
    (let [payload (merge {:op op :epoch epoch :command-id (str (random-uuid))
                          :source {:adapter "web" :session-id session}} fields
                         (when runtime-id {:runtime-id runtime-id}))]
      (if (and beacon? (.-sendBeacon js/navigator))
        (.sendBeacon js/navigator "/api/command"
                     (js/Blob. #js [(js/JSON.stringify (clj->js payload))] #js {:type "application/json"}))
        (http! {:path "/api/command" :body payload :on-success on-success :on-failure on-failure
                :on-uncertain on-uncertain :timeout-ms timeout-ms})))))
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

(defn owns-cue-launch? [db {:keys [source]}]
  (and (#{"web" :web} (:adapter source)) (= (:session db) (:session-id source))))

(defn- current-show? [show owner]
  (and (= (:epoch owner) (:epoch show)) (= (:runtime-id owner) (:runtime-id show))))

(defn command [db op fields & [on-success epoch beacon? runtime-id]]
  [:studio/send-command {:op op :fields fields :epoch (or epoch (get-in db [:show :epoch]))
                        :session (:session db)
                        ;; A window bound to its show names that runtime even before the first frame (#669).
                        :runtime-id (or runtime-id (get-in db [:show :runtime-id]) (show-window/runtime-id db))
                        :on-success on-success :beacon? beacon?}])

(defn notice [message] [:dispatch [:studio/toast message]])

(defn slug [value]
  (let [s (-> value str/lower-case str/trim (str/replace #"[^a-z0-9._/-]+" "-") (str/replace #"^-+|-+$" ""))]
    (if (seq s) s "untitled")))

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

(def ^:private history-path [:ui :operator-history])

(def ^:private history-http-timeout-ms 7000)

(def ^:private history-confirmation-ms 30000)

(def ^:private history-waiting-message "Waiting for the live show to confirm the history command.")

(defn history-inspector [db]
  (let [projection (get-in db [:server-projection :view :operator-history])
        state (get-in db history-path)]
    {:open? (:open? state)
     :projection projection
     :view-state state
     :apply-status (cond-> {:pending? (boolean (:pending state))
                           :message (:message state) :error? (:error? state) :conflict (:conflict state)}
                     (or (:awaiting-full? db) (not (get-in db [:connection :connected?])))
                     (assoc :connection-message "Waiting for the live show to reconnect."))}))

(defn- fail-history [{:keys [pending preview] :as state} message]
  (cond-> (-> state (dissoc :pending) (assoc :message message :error? true))
    (and (= :history/select (:op pending)) (= (:preview pending) preview))
    (assoc :conflict message)))

(defn- settle-history [db]
  (let [{:keys [operator-history revision epoch history-command-outcomes]} (get-in db [:server-projection :view])
        {:keys [identity open? pending preview] :as state} (get-in db history-path)]
    (cond
      (nil? state) db
      (not= identity (:identity operator-history))
      (assoc-in db history-path {:identity (:identity operator-history) :open? open?})
      (nil? pending) db
      :else
      (let [receipt (or (some #(when (and (= (:id pending) (:command-id %)) (= epoch (:epoch %))) %)
                              (rseq (or history-command-outcomes [])))
                        (:receipt pending))]
        (case (:status receipt)
          (:rejected "rejected")
          (update-in db history-path fail-history (or (:message receipt) "The live show rejected the history command."))

          (:accepted "accepted")
          (if (and (integer? (:revision receipt)) (>= revision (:revision receipt)))
            (let [current (:current-id operator-history)]
              (update-in db history-path
                         #(cond-> (-> %
                                      (dissoc :pending :conflict :error?)
                                      (assoc :message (if (and (= current (:history-cursor receipt))
                                                              (= (:position operator-history) (:history-position receipt)))
                                                        "History step applied."
                                                        "History applied. The current step has since changed.")))
                            (= (:preview pending) preview) (-> (dissoc :preview) (assoc :focus-id current)))))
            db)

          db)))))

(rf/reg-event-db :operator-history/open
  (fn [db _] (-> db (assoc-in (conj history-path :open?) true) settle-history)))
(rf/reg-event-db :operator-history/close
  (fn [db _]
    (update-in db history-path #(-> % (assoc :open? false) (dissoc :preview :focus-id :message :conflict :error?)))))
(rf/reg-event-db :operator-history/inspect
  (fn [db [_ interaction]]
    (let [{:keys [open? projection view-state]} (history-inspector db)]
      (if (and open? projection)
        (assoc-in db history-path (history/update-interaction view-state projection interaction))
        db))))

(rf/reg-event-fx :operator-history/navigate
  (fn [{:keys [db]} [_ op request]]
    (let [{:keys [open? projection view-state apply-status]} (history-inspector db)]
      (when (and open? request (not (:pending? apply-status)))
        (if (= request (get (history/commands projection view-state apply-status) op))
          (let [{[runtime-id _ epoch] :identity :keys [expected-cursor expected-history-position target-id]} request
                command-id (str (random-uuid))
                fields (cond-> {:expected-epoch epoch :expected-cursor expected-cursor :command-id command-id
                                :expected-history-position expected-history-position}
                         (#{:history/redo :history/select} op) (assoc :node-id target-id))
                [effect payload] (command db op fields [:operator-history/received command-id] epoch false runtime-id)]
            {:db (update-in db history-path #(-> % (dissoc :message :conflict :error?)
                                                 (assoc :pending {:id command-id :op op :preview (:preview view-state)})))
             :dispatch-later [{:ms history-confirmation-ms :dispatch [:operator-history/unconfirmed command-id]}]
             :fx [[effect (assoc payload :on-failure [:operator-history/rejected command-id]
                                        :on-uncertain [:operator-history/uncertain command-id]
                                        :timeout-ms history-http-timeout-ms)]]})
          {:db (assoc-in db (conj history-path :message) "The live history changed. Review the current step.")})))))

(rf/reg-event-db :operator-history/received
  (fn [db [_ command-id receipt]]
    (if (and (= command-id (:command-id receipt) (get-in db (into history-path [:pending :id])))
             (= (:epoch receipt) (get-in db [:server-projection :view :epoch])))
      (case (:status receipt)
        (:pending "pending") (assoc-in db (conj history-path :message) history-waiting-message)
        (:accepted "accepted") (-> db (assoc-in (into history-path [:pending :receipt]) receipt) settle-history)
        db)
      db)))
(rf/reg-event-db :operator-history/uncertain
  (fn [db [_ command-id]]
    (if (= command-id (get-in db (into history-path [:pending :id])))
      (assoc-in db (conj history-path :message) history-waiting-message)
      db)))
(rf/reg-event-db :operator-history/rejected
  (fn [db [_ command-id message]]
    (if (= command-id (get-in db (into history-path [:pending :id])))
      (update-in db history-path fail-history message)
      db)))
(rf/reg-event-db :operator-history/unconfirmed
  (fn [db [_ command-id]]
    (if (= command-id (get-in db (into history-path [:pending :id])))
      (update-in db history-path fail-history "The result could not be confirmed. Review the current step before trying again.")
      db)))

(rf/reg-event-fx :studio/initialize
  [(rf/inject-cofx :show-window/host) (rf/inject-cofx :view-tabs/saved) (rf/inject-cofx :studio/room-view-saved)]
  (fn [{host :show-window/host saved :view-tabs/saved room-view :studio/room-view-saved} [_ session]]
    (let [db (-> (initial-db session) (show-window/install host) (view-tabs/install saved) (restore-room-view room-view))]
      {:db db :studio/open-stream session :fx [[:view-tabs/listen true] (view-tabs/store-fx db)]})))

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

(defn- missed-cue-launches [db previous outcomes]
  (when-not (identical? previous outcomes)
    (let [own-miss? #(and (#{"missed" :missed} (:status %)) (owns-cue-launch? db %))
          seen (into #{} (comp (filter own-miss?) (keep :command-id)) previous)]
      ;; The owner publishes an ordered, bounded suffix of terminal receipts.
      ;; Compare to the installed view, which survives stream reconnects.
      (second (reduce (fn [[seen missed :as acc] {:keys [command-id] :as outcome}]
                        (if (and command-id (not (contains? seen command-id)) (own-miss? outcome))
                          [(conj seen command-id) (conj missed outcome)]
                          acc)) [seen []] outcomes)))))

(defn- cue-miss-cause [{:keys [reason code]}]
  (cond
    (#{"clock-changed" :clock-changed} reason) "the show clock changed"
    (#{"unknown-cue" :unknown-cue} code) "the cue was removed"))

(defn- cue-miss-message [db previous outcomes]
  (let [by-cause (group-by cue-miss-cause outcomes)]
    (str/join " " (for [cause (distinct (map cue-miss-cause outcomes))
                       :let [launches (get by-cause cause)
                             cue-id (:cue-id (first launches))
                             cue-name (or (:name (cue db cue-id)) (:name (cue previous cue-id))
                                          (some-> cue-id wire-id (str/replace #"[-_]" " ")) "Cue")]]
                    (str (if (= 1 (count launches))
                           (str cue-name " missed its scheduled launch")
                           (str (count launches) " scheduled launches missed"))
                         (when cause (str " because " cause)) ".")))))

(defn prune-plot-views
  "Plot view choices (`:plot-views`, keyed [launch-id control]) for launches
  still in the show; a launch that left takes its choices with it."
  [views instances]
  (let [ids (into #{} (map (comp wire-id :id)) instances)]
    (into {} (filter (fn [[[id] _]] (contains? ids id))) views)))

(defn- install-show [db server]
  (let [presentation (projection/presentation (:view server)
                                              (get-in db [:server-projection :view]) (:server-presentation db))
        runtime-changed? (and (:show db) (not= (:runtime-id presentation) (get-in db [:show :runtime-id])))
        drafts (if runtime-changed? {} (outstanding-drafts (:drafts db) presentation))
        show (apply-drafts presentation drafts)
        previous (when (= (projection/identity-of show) (projection/identity-of (:server-presentation db)))
                   (:server-presentation db))
        missed (missed-cue-launches db (:cue-launch-outcomes previous) (:cue-launch-outcomes show))
        page-id (get-in db [:ui :page-id])
        selected (get-in db [:ui :selected-instance])
        epoch-changed? (or runtime-changed? (and (:show db) (not= (get-in db [:show :epoch]) (:epoch show))))
        checkpoint-view-changed? (and (:show-checkpoints db)
                                      (or (not (get-in db [:connection :connected?]))
                                          (not= (projection/identity-of (:view server))
                                                (projection/identity-of (get-in db [:server-projection :view])))))
        updated (-> db (assoc :show show :drafts drafts :connection {:connected? true}
                             :server-projection server :server-presentation presentation)
                    settle-history
                    spatial-drafts/settle
                    (assoc-in [:ui :page-id] (if (some #(= page-id (:id %)) (:pages show)) page-id (:id (first (:pages show))))))
        updated (settle-connection-drafts updated (:server-presentation db) (get-in db [:server-projection :view]))
        updated (cond-> updated
                  (seq (:plot-views updated)) (update :plot-views prune-plot-views (:instances show))
                  (checkpoints/replaces-rig-draft? db (:view server)) (assoc-in [:ui :rig-dirty?] false)
                  epoch-changed? (dissoc :ending-holds :section-pattern-request)
                  epoch-changed? (update-in [:ui :definition] dissoc :publishing? :publication)
                  epoch-changed? (update :ui dissoc :macro-append-intents :typed-page-id)
                  (and epoch-changed? (get-in db [:ui :macro-picker])) (update :ui dissoc :macro-picker :action-cell)
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
    {:db (checkpoints/carry-restore-draft db updated)
     :fx (cond-> []
           (and (or (:recover-held? updated) (and epoch-changed? (seq (:pointers db))))
                (or (:awaiting-full? db) epoch-changed? (not= (:revision show) (get-in db [:show :revision]))
                    (not= (:pending-cue-launches show) (get-in db [:show :pending-cue-launches]))))
           (conj [:dispatch [:studio/reconcile-held]])
           epoch-changed? (conj [:dispatch [:studio/release-pointers]] [:dispatch [:chat/runtime-change]]
                                        [:dispatch [:fixtures/runtime-change]] [:dispatch [:connections/runtime-change]] [:dispatch [:sensors/runtime-change]] [:dispatch [:modulation/runtime-change]] [:dispatch [:patchbay/runtime-change]] [:dispatch [:waveform/runtime-change]] [:dispatch [:repl/runtime-change]] [:dispatch [:performance/runtime-change]] [:dispatch [:workspace/runtime-change]] [:dispatch [:subscribers/runtime-change]]
                                        (notice "Show restarted; controls follow the new epoch."))
           checkpoint-view-changed? (conj [:dispatch [:checkpoints/show-changed]])
           (and (not checkpoint-view-changed?) (checkpoints/queued-result updated))
           (conj [:dispatch [:checkpoints/receipt-pushed]])
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
           (conj [:dispatch [:studio/settings-received (or (:settings show) {})]])
           (seq missed) (conj [:dispatch [:studio/failure (cue-miss-message updated {:show previous} missed)]]))}))

(rf/reg-event-db :studio/stream-opening
  (fn [db [_ session id]]
    (if (= session (:session db))
      (assoc db :stream-id id :awaiting-full? true)
      db)))

(defn- current-stream? [db session id]
  (and (= session (:session db)) (some? id) (= id (:stream-id db))))

(defn- lane-installs
  "Split an accepted projection into the app-db projection and one whole frame
  per high-rate lane at the accepted token. view-token names the message that
  last changed the app-db view."
  [{:keys [token view] :as state} view-token]
  (let [[app-view lanes] (frame-stream/split-view view)]
    [(assoc state :view app-view)
     (into {} (map (fn [[lane parts]] [lane (frame-stream/frame token view-token view parts)])) lanes)]))

(rf/reg-event-fx :studio/stream-frame
  [(rf/inject-cofx :studio/frame-baselines)]
  (fn [{:keys [db frame-baselines]} [_ session id envelope]]
    (when (current-stream? db session id)
      (let [current (frame-stream/join-view (:server-projection db) frame-baselines)
            received (if (and (:awaiting-full? db) (not= :full (:kind envelope)))
                       {:status :resync}
                       (projection/receive current envelope))]
        (case (if (and (= :accepted (:status received))
                       (show-window/refuses? db (get-in received [:state :view :runtime-id])))
                :foreign (:status received))
          ;; Another show answered on this window's address: never install or acknowledge it.
          :foreign {:db (assoc db :awaiting-full? true
                               :connection {:connected? false :error (show-window/refusal-message db)})}
          ;; High-rate lanes install outside app-db at the same token. The ACK
          ;; follows both installations; lane consumers hear of the frame only
          ;; after it, so their work never holds back the next server frame.
          ;; A patch inside lane-owned subtrees only (clocks, scheduler
          ;; status, fixture values) leaves app-db untouched: no db write, no
          ;; subscription recompute, no Reagent render beyond lane readers.
          :accepted (let [state (:state received)
                          lanes-only? (and (= :patch (:kind envelope)) (:token current)
                                           (frame-stream/lane-only? (:changes envelope)))
                          view-token (if lanes-only? (get-in db [:server-projection :token]) (:token state))
                          [app-state installs] (lane-installs state view-token)
                          lane-fx [[:studio/frame-install installs]
                                   [:studio/stream-reply [id {:op :ack :token (:token envelope)}]]
                                   [:studio/frame-notify (vec (keys installs))]]]
                      (if lanes-only?
                        {:fx lane-fx}
                        (-> (install-show db app-state)
                            (update :db dissoc :awaiting-full?)
                            (update :fx (fnil into []) lane-fx))))
          :duplicate (cond-> {:db (-> db (dissoc :awaiting-full?) (assoc :connection {:connected? true}))
                              :studio/stream-reply [id {:op :ack :token (:token envelope)}]}
                       (and (:awaiting-full? db) (:recover-held? db))
                       (update :fx (fnil conj []) [:dispatch [:studio/reconcile-held]])
                       (and (:awaiting-full? db) (:show-checkpoints db))
                       (update :fx (fnil conj []) [:dispatch [:checkpoints/show-changed]])
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

(rf/reg-event-db :studio/ui-update (fn [db [_ path f]] (update-in db (into [:ui] path) f)))

(rf/reg-event-db :studio/numeric-edit
  (fn [db [_ field-id text]] (assoc-in db [:ui :numeric-edits field-id] text)))
(rf/reg-event-db :studio/numeric-blur
  (fn [db [_ field-id]] (update-in db [:ui :numeric-edits] dissoc field-id)))
(rf/reg-event-fx :studio/settings-load
  (fn [_ _] {:studio/http {:path "/api/settings" :on-success [:studio/settings-received] :on-failure [:studio/settings-unavailable]}}))
(rf/reg-event-db :studio/settings-unavailable (fn [db _] (assoc db :settings-supported? false)))
(defn apply-ui-preferences [db settings]
  (let [prefs (:ui settings) raw-view (:view prefs)
        ;; This device's restored tabs choose the opening view before the show's preference.
        view (or (view-tabs/restored-view db) (when (or (string? raw-view) (keyword? raw-view)) (keyword raw-view)))]
    (cond-> (assoc db :settings-loaded? true :settings-supported? true)
      (and (contains? #{nil :home} (:route-name db)) (pages/views view)) (assoc-in [:ui :view] view)
      (string? (:page-id prefs)) (assoc-in [:ui :page-id] (:page-id prefs))
      (contains? #{"toggle" "held" "one-shot"} (:launch-mode prefs)) (assoc-in [:ui :launch-mode] (:launch-mode prefs))
      (contains? prefs :knob-skin) (assoc-in [:ui :knob-skin] (control-geometry/knob-skin (:knob-skin prefs))))))
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
                                                :launch-mode (get-in db [:ui :launch-mode])
                                                :knob-skin (name (control-geometry/knob-skin (get-in db [:ui :knob-skin])))}}}})))

(rf/reg-sub :ui/knob-skin (fn [db _] (control-geometry/knob-skin (get-in db [:ui :knob-skin]))))

(rf/reg-event-fx :studio/set-knob-skin
  (fn [{:keys [db]} [_ skin]]
    {:db (assoc-in db [:ui :knob-skin] (control-geometry/knob-skin skin)) :fx [[:dispatch [:studio/persist-ui]]]}))
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
      (let [updated (-> db (assoc :route-name page) (assoc-in [:ui :view] page) (assoc-in [:ui :rig-drag] nil)
                        (view-tabs/follow-route page (not (pages/views (:route-name db)))))]
        {:db updated
         :fx (cond-> [[:dispatch [:studio/release-pointers]] [:studio/scroll-top true]]
               (:view-tabs updated) (conj (view-tabs/store-fx updated))
               ;; Initial deep links and the home preference restore are read-only.
               (pages/views (:route-name db)) (conj [:dispatch [:studio/persist-ui]])
               (and (= page :assistant) (not (get-in db [:chat :provider]))) (conj [:dispatch [:chat/load]]))
         :dispatch-later (when (= page :perform) [{:ms 30 :dispatch [:studio/preview-resize]}])}))))

(rf/reg-event-fx :studio/set-page
  (fn [{:keys [db]} [_ id typed-id]]
    (let [recent? (recent-page? db id)]
      {:db (cond-> (assoc-in db [:ui :recent-page?] recent?)
             recent? (update :ui dissoc :action-cell :macro-picker)
             (not recent?) (update :ui assoc :page-id id :typed-page-id typed-id))
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

(rf/reg-cofx :studio/unique (fn [cofx _] (assoc cofx :unique (str (random-uuid)))))

(defn section-pattern-control [db]
  (let [view (get-in db [:server-projection :view])
        {:keys [id accepted-revision]} (:section-pattern-request db)]
    (when (:section-patterns-enabled? view)
      {:override? (true? (get-in view [:settings :section-patterns :override?]))
       :pending? (and (some? id) (or (nil? accepted-revision) (< (:revision view) accepted-revision)))
       :connected? (true? (get-in db [:connection :connected?]))})))

(rf/reg-event-fx :studio/section-pattern-override [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ override?]]
    (let [control (section-pattern-control db)]
      (when (and (:connected? control) (not (:pending? control)) (boolean? override?)
                 (not= override? (:override? control)))
        {:db (assoc db :section-pattern-request {:id unique})
         :studio/http {:path "/api/settings" :typed? true
                       :body {:scope :section-patterns :values {:override? override?}}
                       :on-success [:studio/section-pattern-saved unique]
                       :on-failure [:studio/section-pattern-failed unique]}}))))

(rf/reg-event-fx :studio/section-pattern-saved
  (fn [{:keys [db]} [_ id {:keys [status revision message]}]]
    (when (= id (get-in db [:section-pattern-request :id]))
      (if (and (= "accepted" status) (integer? revision))
        {:db (assoc-in db [:section-pattern-request :accepted-revision] revision)}
        {:dispatch [:studio/section-pattern-failed id (or message "The show did not acknowledge the change.")]}))))

(rf/reg-event-fx :studio/section-pattern-failed
  (fn [{:keys [db]} [_ id message]]
    (when (= id (get-in db [:section-pattern-request :id]))
      {:db (dissoc db :section-pattern-request)
       :dispatch [:studio/failure (str "Section pattern override: " message)]})))

(defn launch-fx [db cue-id mode on-success & [{:keys [grid command-id gesture-id]}]]
  (let [cue (cue db cue-id)]
    (command db "cue/launch"
             (cond-> {:cue-id cue-id
                      :launch (cond-> {:mode (if (= mode "toggle") "persistent" mode)}
                                (and (= mode "one-shot") (:duration cue)) (assoc :duration (:duration cue))
                                (and (= mode "one-shot") (not (:duration cue))) (assoc :duration-ms (or (:duration-ms cue) 1000)))}
               grid (assoc :grid grid :command-id command-id)
               gesture-id (assoc :gesture-id gesture-id))
             on-success)))
(rf/reg-event-fx :studio/launch [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ cue-id mode options]]
    (let [mode (or mode (get-in db [:ui :launch-mode])) instances (active db cue-id)
          grid (get options :grid (selected-grid db))
          owner (select-keys (:show db) [:epoch :runtime-id])]
      (cond
        (and (:owner options) (not (current-show? (:show db) (:owner options)))) {}
        (not (get-in db [:connection :connected?])) {:dispatch [:studio/failure "Wait for the show to reconnect before launching."]}
        (and (= mode "toggle") (seq instances)) {:fx (mapv #(command db "instance/end" {:instance-id (:id %)}) instances)}
        :else {:fx [(launch-fx db cue-id mode [:studio/launched owner] {:grid grid :command-id unique})]}))))
(rf/reg-event-fx :studio/launched
  (fn [{:keys [db]} [_ owner {:keys [instance-id]}]]
    (when (and instance-id (current-show? (:show db) owner))
      {:db (assoc-in db [:ui :selected-instance] instance-id)})))

(defn scene-draft-control
  "The staged Room draft as Studio shows it, or nil. :stale is the refusal an
  apply would get. :missed is the last scheduled apply of this draft revision
  that did not land, as `show/scene-draft` reports it under :last-outcome."
  [db]
  (when-let [{:keys [draft status pending-apply stale last-outcome]} (get-in db [:server-projection :view :scene-draft])]
    (when draft
      (cond-> {:id (:id draft) :revision (:revision draft) :operations (count (:operations draft))
               :status (keyword status) :due-beat (:due-beat pending-apply)
               :stale (some-> stale keyword)
               :missed (when (and (= :missed (keyword (:status last-outcome)))
                                  (= [(:id draft) (:revision draft)]
                                     [(:draft-id last-outcome) (:draft-revision last-outcome)]))
                         {:due-beat (:due-beat last-outcome) :code (some-> (:code last-outcome) keyword)})
               :pending? (= (:id draft) (get-in db [:scene-draft-request :draft-id]))
               :connected? (true? (get-in db [:connection :connected?]))}
        ;; An audition-only draft, such as a generated knob candidate (#667).
        (:preview draft) (assoc :preview (:preview draft))))))

(def ^:private scene-draft-reasons
  {:stale-patch "the rig changed" :stale-registry "a cue or effect changed"
   :stale-prefabs "the material library changed"
   :clock-changed "the clock source changed" :clock-moved-back "the beat moved back"})

(defn scene-draft-note
  "The banner's status text for a draft control: when it applies, why it is
  stale, and a scheduled apply that missed its boundary."
  [{:keys [status due-beat stale missed]}]
  (let [reason #(get scene-draft-reasons % (some-> % name))]
    (str (case status
           :scheduled (str " · applies at beat " due-beat)
           :stale (str " · stale: " (reason (or stale :stale-patch)) ". Stage it again")
           :error " · audition failed"
           "")
         (when (and missed (not= :scheduled status))
           (str " · the apply at beat " (:due-beat missed) " was missed"
                (when-let [why (reason (:code missed))] (str ": " why)))))))

(defn room-show
  "The show the Perform Room renders. Live fixture values and a staged draft's
  audition values both arrive on the :room frame lane; only this Room shows
  the audition, so every other preview keeps the live frame."
  [db]
  (or (:server-presentation db) (:show db)))

(defn- scene-draft-fx [db unique op fields]
  (let [{:keys [id revision]} (scene-draft-control db)]
    (when id
      {:db (assoc db :scene-draft-request {:id unique :draft-id id})
       :fx [(-> (command db op (merge {:draft-id id :expected-draft-revision revision} fields)
                         [:studio/scene-draft-result unique])
                (assoc-in [1 :fields :command-id] unique)
                (assoc-in [1 :on-failure] [:studio/scene-draft-failed unique]))]})))

(rf/reg-event-fx :studio/scene-draft-apply [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ grid]]
    (scene-draft-fx db unique "scene-draft/apply" (when grid {:grid grid}))))

(rf/reg-event-fx :studio/scene-draft-discard [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    (scene-draft-fx db unique "scene-draft/discard" nil)))

(rf/reg-event-fx :studio/scene-draft-result
  (fn [{:keys [db]} [_ id {:keys [status due-beat discarded? message]}]]
    (when (= id (get-in db [:scene-draft-request :id]))
      {:db (dissoc db :scene-draft-request)
       :dispatch (cond
                   ;; A reply such as "pending" confirms nothing; the banner shows what the show did.
                   (not (#{:accepted "accepted" :scheduled "scheduled"} status))
                   [:studio/failure (str "Room draft: " (or message "The show has not confirmed this")
                                         ". Check the banner before trying again.")]
                   discarded? [:studio/toast "Room draft discarded."]
                   (#{:scheduled "scheduled"} status) [:studio/toast (str "Room draft applies at beat " due-beat ".")]
                   :else [:studio/toast "Room draft applied to the show."])})))

(rf/reg-event-fx :studio/scene-draft-failed
  (fn [{:keys [db]} [_ id message]]
    (when (= id (get-in db [:scene-draft-request :id]))
      {:db (dissoc db :scene-draft-request)
       :dispatch [:studio/failure (str "Room draft: " message)]})))

(rf/reg-event-fx :studio/end-all
  (fn [{:keys [db]} _] {:fx (mapv #(command db "instance/end" {:instance-id (:id %)}) (get-in db [:show :instances]))}))

(defn- launch-held [db {:keys [cue-id press-id grid] :as pointer}]
  ;; The command's origin survives pointer release, reuse and show replacement.
  (let [owner (select-keys pointer [:pointer-id :press-id :epoch :runtime-id :grid])]
    (assoc-in (launch-fx db cue-id "held" [:studio/held-launched owner]
                        {:grid grid :command-id press-id :gesture-id press-id})
              [1 :on-failure] (if grid [:studio/failure] [:studio/held-failed]))))

(rf/reg-event-fx :studio/gesture-down [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ {:keys [pointer-id cue-id page-id] :as input}]]
    (let [recent? (recent-page? db page-id)]
      (when-not (or (get-in db [:pointers pointer-id]) (and recent? (nil? cue-id)))
        (let [mode (get-in db [:ui :launch-mode])
              held? (and cue-id (= mode "held") (or recent? (not (get-in db [:ui :edit?]))))
              pointer (merge input (select-keys (:show db) [:epoch :runtime-id])
                             {:press-id unique :mode mode :held? held? :recent-page? recent?
                              :grid (selected-grid db)})]
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

(def ^:private action-cell-keys [:page-id :cue-id :typed-page-id :typed-cue-id :cue-name :owner :x :y])

(rf/reg-event-fx :studio/gesture-long
  (fn [{:keys [db]} [_ id press-id]]
    (let [pointer (get-in db [:pointers id])]
      (when (and press-id (= press-id (:press-id pointer)) (not-any? pointer [:moved? :recent-page?]))
        {:db (assoc-in db [:pointers id :long?] true)
         :dispatch [:studio/open-actions (select-keys pointer action-cell-keys)]}))))

(defn- end-held [db {:keys [press-id epoch runtime-id] :as owner} id beacon?]
  (let [fields (cond-> {:instance-id id} press-id (assoc :command-id (str press-id "-end")))]
    (assoc-in (command db "instance/end" fields nil epoch beacon? runtime-id)
              [1 :on-failure] [:studio/held-end-failed owner])))

(defn- cancel-cue-launch [db {:keys [press-id epoch runtime-id] :as owner} reason beacon?]
  (let [held? (= "held-release" reason)
        fields {:launch-command-id press-id :reason reason :command-id (str press-id "-" reason)}]
    (assoc-in (command db "cue/cancel-launch" fields (when-not held? [:studio/cue-cancelled owner]) epoch beacon? runtime-id)
              [1 :on-failure] (if held? [:studio/held-end-failed owner] [:studio/failure]))))

(rf/reg-event-fx :studio/cancel-queued-cue
  (fn [{:keys [db]} [_ launch-id]]
    (when (and (get-in db [:connection :connected?])
               (some #(and (= launch-id (:command-id %)) (owns-cue-launch? db %)) (get-in db [:show :pending-cue-launches])))
      (let [owner (assoc (select-keys (:show db) [:epoch :runtime-id]) :press-id launch-id)]
        {:fx [(cancel-cue-launch db owner "operator-cancel" false)]}))))

(rf/reg-event-fx :studio/cue-cancelled
  (fn [{:keys [db]} [_ owner {:keys [status]}]]
    (when (current-show? (:show db) owner)
      (case status
        ("cancelled" :cancelled) {:dispatch [:studio/toast "Queued cue cancelled."]}
        ("fired" :fired) {:dispatch [:studio/toast "The cue already launched."]}
        nil))))

(defn- live-web-hold? [session {:keys [gesture-id source launch-mode phase]}]
  (and gesture-id (= "held" launch-mode) (= "running" phase)
       (= "web" (:adapter source)) (= session (:session-id source))))

(rf/reg-event-fx :studio/reconcile-held
  (fn [{:keys [db]} _]
    (let [show (:show db)
          holds (filter #(live-web-hold? (:session db) %) (:instances show))
          pressed (into {} (keep (fn [[id pointer]]
                                   (when (and (:held? pointer) (current-show? show pointer))
                                     [(:press-id pointer) id]))) (:pointers db))
          orphaned (remove #(contains? pressed (:gesture-id %)) holds)
          orphaned-queued (filter #(and (= "held" (:mode %)) (owns-cue-launch? db %)
                                        (not (contains? pressed (:command-id %)))) (:pending-cue-launches show))
          pending (into (set (map :gesture-id orphaned)) (map :command-id) orphaned-queued)
          ending (or (:ending-holds db) #{})
          attach (fn [state {:keys [id gesture-id]}]
                   (if-let [pointer (get pressed gesture-id)]
                     (assoc-in state [:pointers pointer :instance-id] id)
                     state))
          release (fn [{:keys [id gesture-id]}]
                    (end-held db (assoc (select-keys show [:epoch :runtime-id]) :press-id gesture-id) id false))]
      (when (get-in db [:connection :connected?])
        {:db (assoc (reduce attach db holds) :ending-holds pending)
         :fx (into (mapv release (remove #(ending (:gesture-id %)) orphaned))
                   (comp (remove #(ending (:command-id %)))
                         (map #(cancel-cue-launch db (assoc (select-keys show [:epoch :runtime-id]) :press-id (:command-id %))
                                                  "held-release" false))) orphaned-queued)}))))

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
  (fn [{:keys [db]} [_ {:keys [pointer-id press-id grid] :as owner} {:keys [instance-id]}]]
    (let [pressed? (and (current-show? (:show db) owner) (= press-id (get-in db [:pointers pointer-id :press-id])))]
      (cond
        (and pressed? instance-id) {:db (-> db (assoc-in [:pointers pointer-id :instance-id] instance-id)
                                            (assoc-in [:ui :selected-instance] instance-id))}
        ;; Grid holds release by command ID, even if the launch reply arrives late.
        grid nil
        (nil? instance-id) {:db (assoc db :recover-held? true) :dispatch [:studio/reconcile-held]}
        :else {:fx [(end-held db owner instance-id false)]}))))

(defn release-pointer [db id beacon?]
  (if-let [pointer (get-in db [:pointers id])]
    (let [released (update db :pointers dissoc id)]
      (cond
        (and (:held? pointer) (:grid pointer) (current-show? (:show db) pointer))
        {:db released :fx [(cancel-cue-launch db pointer "held-release" beacon?)]}

        (:instance-id pointer) {:db released :fx [(end-held db pointer (:instance-id pointer) beacon?)]}
        :else {:db released}))
    {}))
(rf/reg-event-fx :studio/gesture-up
  (fn [{:keys [db]} [_ id]]
    (let [pointer (get-in db [:pointers id]) result (release-pointer db id false)]
      (cond-> result
        (and pointer (current-show? (:show db) pointer) (not-any? pointer [:held? :moved? :long?]))
        (assoc :dispatch (if (and (not (:recent-page? pointer))
                                  (or (get-in db [:ui :edit?]) (nil? (:cue-id pointer))))
                           [:studio/open-actions (select-keys pointer action-cell-keys)]
                           [:studio/launch (:cue-id pointer) (:mode pointer)
                            {:grid (:grid pointer) :owner (select-keys pointer [:epoch :runtime-id])}]))))))
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
(defn- vector-value [db instance-id param fallback]
  (let [instance (some #(when (= (:id %) instance-id) %) (get-in db [:show :instances]))]
    (vec (or (get-in instance [:params param]) fallback))))

;; A compound widget (#655) sets components against the current value in
;; app-db, so a pad gesture and a Z fader moved together, or repeated key
;; presses inside one frame, never overwrite each other with a stale vector.
(rf/reg-event-fx :studio/param-components
  (fn [{:keys [db] :as cofx} [_ instance-id param changes fallback]]
    (let [current (vector-value db instance-id param fallback)
          next (when (every? #(< -1 % (count current)) (keys changes)) (reduce-kv assoc current changes))]
      ;; param/set carries the whole vector (docs/repl-studio-parity.md), so
      ;; a gesture that changes nothing sends nothing.
      (when (and next (not= next current))
        (set-param-fx cofx [:studio/param-set instance-id param next])))))
(rf/reg-event-fx :studio/param-set-if-changed
  ;; Grouped X/Y/Z axes are separate parameters. A pad gesture sends an axis
  ;; only when it differs from the latest value in app-db, so two pointer
  ;; events inside one frame cannot suppress a real change.
  (fn [{:keys [db] :as cofx} [_ instance-id param value fallback]]
    (let [params (:params (some #(when (= (:id %) instance-id) %) (get-in db [:show :instances])))
          current (some #(when (contains? params %) (get params %)) [param (keyword (wire-id param)) (wire-id param)])]
      (when (not= value (if (some? current) current fallback))
        (set-param-fx cofx [:studio/param-set instance-id param value])))))
(rf/reg-event-fx :studio/param-component-key
  (fn [{:keys [db] :as cofx} [_ instance-id param schema index key fallback]]
    (let [current (vector-value db instance-id param fallback)]
      (when-let [next (and (< -1 index (count current)) (number? (get current index))
                           (widgets/keyboard-value schema (get current index) key))]
        (set-param-fx cofx [:studio/param-set instance-id param (assoc current index next)])))))
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
        (-> db (update :ui dissoc :macro-picker)
            (assoc-in [:ui :action-cell] (assoc cell :page-id page-id
                                                :owner (or (:owner cell) (select-keys (:show db) [:runtime-id :epoch]))))
            (assoc-in [:ui :assign-cue] (:id (first (get-in db [:show :cues])))))))))

(defn- close-actions [db] (update db :ui dissoc :action-cell :macro-picker))
(rf/reg-event-db :studio/close-actions (fn [db _] (close-actions db)))

(defn saved-macros [db]
  (filterv #(= :macro (:mode %)) (get-in db [:server-projection :view :compositions])))

(def ^:private changed-cue-cell "The cue cell changed. Open its actions again.")

(defn- typed-action-cue [db {:keys [typed-page-id typed-cue-id x y]}]
  (let [show (get-in db [:server-projection :view])
        page (some #(when (= typed-page-id (:id %)) %) (:pages show))]
    (when (some #(and (= [x y] [(:x %) (:y %)]) (= typed-cue-id (:cue-id %))) (:cells page))
      (some #(when (= typed-cue-id (:id %)) %) (:cues show)))))

(defn macro-picker-retry? [db {:keys [cue-id macro-id]}]
  (contains? (get-in db [:ui :macro-append-intents]) [cue-id macro-id]))

(rf/reg-event-fx :studio/macro-picker-open
  (fn [{:keys [db]} _]
    (when-let [{:keys [owner] :as cell} (get-in db [:ui :action-cell])]
      (let [cue (when (current-show? (:show db) owner) (typed-action-cue db cell))]
        (if cue
          {:db (assoc-in db [:ui :macro-picker] {:cue-id (:id cue) :cue-name (or (:name cue) (:cue-name cell)) :owner owner})}
          {:db (assoc-in db [:ui :action-cell :error] changed-cue-cell)})))))

(rf/reg-event-db :studio/macro-picker-back
  (fn [db _] (update db :ui dissoc :macro-picker)))

(rf/reg-event-fx :studio/macro-picker-submit [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    (let [{:keys [cue-id macro-id owner request]} (get-in db [:ui :macro-picker])
          {:keys [typed-page-id x y] :as cell} (get-in db [:ui :action-cell])
          path [:ui :macro-append-intents [cue-id macro-id]]
          prior (get-in db path)]
      (when (and cue-id macro-id (not request) (current-show? (:show db) owner)
                 (get-in db [:connection :connected?]) (some #(= macro-id (:id %)) (saved-macros db)))
        (if (or prior (= cue-id (:id (typed-action-cue db cell))))
          (let [body (or prior {:macro-id macro-id :cue-id cue-id :command-id unique
                                :base-revision (get-in db [:show :revision])
                                :cell {:page-id typed-page-id :x x :y y}})]
            {:db (-> db (assoc-in path body) (update-in [:ui :macro-picker] assoc :request unique :error nil))
             :studio/http {:path "/api/composition/append" :typed? true :raw-response? true :timeout-ms 7000
                           :body body
                           :on-success [:studio/macro-picker-reply owner unique body]
                           :on-failure [:studio/macro-picker-failed owner unique body]
                           :on-uncertain [:studio/macro-picker-uncertain owner unique]}})
          {:db (assoc-in db [:ui :macro-picker :error] changed-cue-cell)})))))

(rf/reg-event-fx :studio/macro-picker-reply
  (fn [{:keys [db]} [_ owner request {:keys [cue-id macro-id command-id]} receipt]]
    (when (and (current-show? (:show db) owner) (= request (get-in db [:ui :macro-picker :request])))
      (if (and (= :accepted (:status receipt)) (= macro-id (:macro-id receipt)) (= command-id (:command-id receipt)))
        (let [macro (some #(when (= macro-id (:id %)) %) (saved-macros db))]
          {:db (-> db close-actions (update-in [:ui :macro-append-intents] dissoc [cue-id macro-id]))
           :fx [(notice (str "Added to " (or (:name macro) (wire-id macro-id)) "."))]})
        {:db (update-in db [:ui :macro-picker] assoc :request nil
                        :error (if (= :pending (:status receipt)) "Macro update is still processing."
                                   "The show did not confirm the macro update."))}))))

(defn- current-macro-request? [db owner request]
  (and (current-show? (:show db) owner) (= request (get-in db [:ui :macro-picker :request]))))

;; A definite rejection did not change the show. Forget its command so the next
;; attempt validates the current cell instead of replaying a refused intent.
(rf/reg-event-db :studio/macro-picker-failed
  (fn [db [_ owner request {:keys [cue-id macro-id] :as body} error]]
    (if (current-macro-request? db owner request)
      (cond-> (update-in db [:ui :macro-picker] assoc :request nil :error error)
        (= body (get-in db [:ui :macro-append-intents [cue-id macro-id]]))
        (update-in [:ui :macro-append-intents] dissoc [cue-id macro-id]))
      db)))

;; No reply leaves the outcome unknown. Keep the command for an exact retry.
(rf/reg-event-db :studio/macro-picker-uncertain
  (fn [db [_ owner request]]
    (if (current-macro-request? db owner request)
      (update-in db [:ui :macro-picker] assoc :request nil
                 :error "The show did not reply. Retry append to confirm the same change.")
      db)))

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
        :launch-now {:db closed :dispatch [:studio/launch (:id cue) "one-shot" {:grid "off"}]}
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
(rf/reg-event-db :studio/slot-param-component-key
  (fn [db [_ index parameter schema component key]]
    (let [current (vec (get (slot-parameters db index) parameter))]
      (if-let [next (and (< -1 component (count current)) (number? (get current component))
                         (widgets/keyboard-value schema (get current component) key))]
        (assoc-in db [:ui :composition :slots index :params parameter] (assoc current component next))
        db))))
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
      (assoc-in db [:ui :composition] (assoc (dissoc composition :running? :playback :layer-id :member-slots) :slots slots)))))
(rf/reg-event-db :studio/composition-new [(rf/inject-cofx :studio/unique)]
  (fn [db _] (assoc-in db [:ui :composition] {:id "new-composition" :name "New composition" :mode "sequence" :loop "once" :slots []})))
(rf/reg-event-fx :studio/composition-save
  (fn [{:keys [db]} [_ run?]]
    ;; The show keeps a macro's card arrangement when a save omits it (#795).
    (let [composition (dissoc (get-in db [:ui :composition]) :cards)]
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

(defn- definition-owner [db]
  (select-keys (:show db) [:runtime-id :epoch]))

(rf/reg-event-fx :studio/definition-load
  (fn [{:keys [db]} [_ kind id]]
    (let [token (assoc (definition-owner db) :id (str (random-uuid)))
          typed? (= "effects" kind)]
      {:db (assoc-in db [:ui :definition :load-token] token)
       :studio/http {:path (str "/api/definition?kind=" kind "&id=" (js/encodeURIComponent id) (when typed? "&data=true"))
                     :typed? typed? :raw-response? typed?
                     :on-success [:studio/definition-loaded kind id token]}})))
(rf/reg-event-fx :studio/definition-loaded
  (fn [{:keys [db]} [_ kind id token {:keys [source definition]}]]
    (when (and (= token (get-in db [:ui :definition :load-token]))
               (= (dissoc token :id) (definition-owner db)))
      (let [version (or (:version definition) (js/parseInt (or (second (re-find #":version\s+(\d+)" source)) "1") 10))
            typed (when (and (= "effects" kind) (:reactive definition)) definition)]
        {:db (cond-> (update-in db [:ui :definition] merge {:kind kind :source source :loaded-id id :version version
                                                          :loaded-owner (dissoc token :id)
                                                          :mode :source :auto? false :status (str id " · v" version)
                                                          :result "Loaded from the running show." :error? false
                                                          :baseline {:source source :status (str id " · v" version)}
                                                          :publication nil :publishing? false :load-token nil})
               (or typed (get-in db [:ui :builder :reactive :base])) (assoc-in [:ui :builder] default-builder)
               typed (update-in [:ui :builder] merge {:id id :name (or (:name typed) id) :cue? false
                                                    :reactive (reactive-editor/loaded-draft typed)}))
         :studio/nvim-source source}))))
(rf/reg-event-fx :studio/definition-new
  (fn [{:keys [db]} _]
    {:db (cond-> (update-in db [:ui :definition] merge {:kind "effects" :source initial-source :loaded-id nil :version 1
                                                       :loaded-owner nil
                                                       :auto? false :status "New definition" :result "" :error? false
                                                       :baseline nil :publication nil :publishing? false :load-token nil})
           (get-in db [:ui :builder :reactive]) (assoc-in [:ui :builder] default-builder))
     :studio/nvim-source initial-source}))
(def ^:private auto-publish-delay-ms
  "Quiet time after the latest edit before Publish valid edits sends it."
  850)

(defn- published-source? [definition source]
  (= source (get-in definition [:baseline :source])))

(defn- edit-definition-source
  "Install an edited source. The status follows the source rather than the edit:
  text equal to the published baseline (the text last loaded from or accepted by
  the running show) reads as that baseline's status, anything else as unpublished."
  [db source]
  (let [definition (get-in db [:ui :definition])
        revision (inc (:edit-revision definition 0))
        published? (published-source? definition source)]
    {:db (update-in db [:ui :definition] merge {:source source :edit-revision revision :error? false :result ""
                                               :status (if published? (get-in definition [:baseline :status])
                                                         "Unpublished changes")})
     :dispatch-later (when (and (:auto? definition) (not published?))
                       [{:ms auto-publish-delay-ms :dispatch [:studio/auto-publish revision]}])}))

(rf/reg-event-fx :studio/definition-source
  ;; Neovim echoes text it was given (the initial load) and delivers its last edit up to
  ;; 200 ms late, possibly after that text was published. Neither is a new edit, so keep
  ;; the status, result and pending auto-publish of the text Studio already holds.
  (fn [{:keys [db]} [_ source]]
    (when (not= source (get-in db [:ui :definition :source]))
      (edit-definition-source db source))))
(rf/reg-event-fx :studio/auto-publish
  (fn [{:keys [db]} [_ revision]]
    (let [definition (get-in db [:ui :definition])]
      (when (and (:auto? definition) (= revision (:edit-revision definition))
                 (not (published-source? definition (:source definition))))
        {:dispatch [:studio/definition-publish true]}))))
(rf/reg-event-fx :studio/editor-engine
  (fn [{:keys [db]} [_ engine]]
    (cond
      (and (= engine :textarea) (= :neovim (get-in db [:ui :definition :engine])))
      {:studio/nvim-read {:on-success [:studio/editor-to-text]}}
      (and (= engine :neovim) (not (and (.-crossOriginIsolated js/window) (exists? js/SharedArrayBuffer))))
      {:dispatch [:studio/editor-error "Neovim requires localhost or HTTPS with COOP/COEP. The accessible editor remains available."]}
      :else {:db (assoc-in db [:ui :definition :engine] engine)})))
;; Neovim may hold an edit it has not delivered yet; reading it is that delivery.
(rf/reg-event-fx :studio/editor-to-text
  (fn [{:keys [db]} [_ source]]
    (let [db (assoc-in db [:ui :definition :engine] :textarea)]
      (if (= source (get-in db [:ui :definition :source]))
        {:db db}
        (edit-definition-source db source)))))
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

(defn builder-definition
  [{:keys [id name color level duration-value duration-unit release-value release-unit keyframes blocks reactive]} version]
  (when-not (or (:base reactive) (re-matches #"[a-zA-Z][a-zA-Z0-9._/-]*" id))
    (throw (js/Error. "Use an identifier beginning with a letter.")))
  (when (and (:base reactive) (not (:enabled? reactive)))
    (throw (js/Error. "Replace reactive output expressions in EDN to change the evaluation mode of this definition.")))
  (let [color-output {:op :const :value (mapv #(/ (js/parseInt (subs color % (+ % 2)) 16) 255) [1 3 5])}
        draft {:id (keyword id) :version version :name name :swatch color
               :duration (musical-duration duration-value duration-unit)}]
    (if (:enabled? reactive)
      (let [{:keys [targets dimmer] config :reactive} (reactive-editor/definition-fields reactive)]
        (if-let [base (:base reactive)]
          (cond-> (assoc base :version version :targets targets :reactive config)
            (not= name (or (:name base) id)) (assoc :name name))
          (do
            (when-not (pos? duration-value) (throw (js/Error. "Duration must be positive.")))
            (assoc draft :targets targets :reactive config :outputs {:color color-output :dimmer dimmer}))))
      (do
        (when-not (and (number? level) (<= 0 level 1) (pos? duration-value) (<= 0 release-value))
          (throw (js/Error. "Intensity must be 0–1, duration positive, and release zero or positive.")))
        (cond-> (assoc draft
                       :envelope {:attack (if (= release-unit "ms") {:ms 100} {:beats 0.125})
                                  :release (musical-duration release-value release-unit)}
                       :params {:level {:type :number :default level :bounds [0.0 1.0]}}
                       :outputs {:color color-output :dimmer {:op :param :key :level}})
          keyframes (assoc :keyframes keyframes)
          (seq blocks) (assoc :blocks blocks))))))

(defn- builder-source [db]
  (let [builder (get-in db [:ui :builder])
        version (get-in db [:ui :definition :version] 1)
        baseline (get-in builder [:reactive :base :version])]
    (when (and baseline (not= baseline version))
      (throw (js/Error. "The definition changed. Reload it from the registry.")))
    (let [definition (builder-definition builder version)
          fixtures (into #{} (map :id) (get-in db [:server-projection :view :rig :fixtures]))]
      (when (and (:reactive definition) (not (every? fixtures (:targets definition))))
        (throw (js/Error. "Choose targets from the current rig.")))
      (pretty/edn definition))))

(rf/reg-event-db :studio/builder-reactive
  (fn [db [_ edit]]
    (if (get-in db [:ui :definition :publishing?]) db
      (-> db (update-in [:ui :builder :reactive] edit)
          (update-in [:ui :definition] merge {:status "Unpublished changes" :result "" :error? false})))))

(rf/reg-event-fx :studio/apply-block-source
  (fn [{:keys [db]} [_ baseline updated]]
    (if (= baseline (get-in db [:ui :definition :source]))
      (assoc (edit-definition-source db updated) :studio/nvim-source updated)
      {:dispatch [:studio/editor-error "The source changed. Reopen the block editor."]})))

(rf/reg-event-fx :studio/builder-generate
  (fn [{:keys [db]} _]
    (try
      (let [source (builder-source db)]
        ;; Generated text takes its status from the published baseline, like an edit,
        ;; but Generate only produces text: it never queues Publish valid edits.
        (-> (edit-definition-source db source)
            (dissoc :dispatch-later)
            (update :db update-in [:ui :definition] merge {:kind "effects" :mode :source
                                                           :result "Generated EDN from the builder."})
            (assoc :studio/nvim-source source)))
      (catch :default error {:dispatch [:studio/editor-error (error-message error)]}))))
(defn- publication-current? [db token]
  (and (some? token) (= token (get-in db [:ui :definition :publication]))
       (= (:epoch token) (get-in db [:show :epoch])) (= (:runtime-id token) (get-in db [:show :runtime-id]))))

(defn- publication-failed
  "The show refused the text that was sent, or it could not be sent. An edit made
  while the request was in flight was never sent: like a reply to a publication
  (:studio/published), the failure leaves it unpublished and, with auto-publish
  on, queues it after the usual quiet time, because its own debounce fired during
  the request and was dropped. `sent` is nil when nothing was sent."
  [db token sent error]
  (when (publication-current? db token)
    (let [definition (get-in db [:ui :definition])
          newer? (and (some? sent) (not= sent (:source definition)))
          ;; Edited back to the text the show runs: nothing is unpublished.
          clean? (and newer? (published-source? definition (:source definition)))]
      (cond-> {:db (update-in db [:ui :definition] merge
                              (cond
                                clean? {:publishing? false :error? false
                                        :status (get-in definition [:baseline :status])
                                        :result (str "The text sent earlier was not published: " (error-message error)
                                                     " The editor holds the text the show runs.")}
                                newer? {:publishing? false :error? true :status "Unpublished changes"
                                        :result (str "The text sent earlier was not published: " (error-message error)
                                                     " Your newer edits are not published yet.")}
                                :else {:publishing? false :error? true :status "Publication failed"
                                       :result (error-message error)}))}
        (and newer? (not clean?) (:auto? definition))
        (assoc :dispatch-later [{:ms auto-publish-delay-ms
                                 :dispatch [:studio/auto-publish (:edit-revision definition)]}])))))

(rf/reg-event-fx :studio/publication-error
  (fn [{:keys [db]} [_ token error]] (publication-failed db token nil error)))

(rf/reg-event-fx :studio/publication-refused
  (fn [{:keys [db]} [_ token sent error]] (publication-failed db token sent error)))

(rf/reg-event-fx :studio/definition-publish
  (fn [{:keys [db]} [_ automatic?]]
    (when-not (get-in db [:ui :definition :publishing?])
      (try
        (let [definition (get-in db [:ui :definition]) built? (= :form (:mode definition))
              owner (definition-owner db)
              _ (when (and (:loaded-owner definition) (not= owner (:loaded-owner definition)))
                  (throw (js/Error. "The show changed. Reload the definition from the registry.")))
              token (assoc owner :id (str (random-uuid)))
              pending (update-in db [:ui :definition] merge {:publication token :publishing? true})
              source (if built? (builder-source db) (:source definition))]
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
            baseline (get-in built [:reactive :base :version])
            expected (cond
                       baseline baseline
                       (and (get-in built [:reactive :enabled?]) (= loaded-id (:id built))) version
                       built (:version existing)
                       loaded-id version)]
        {:db (update-in db [:ui :definition] merge {:publishing? true :source source :kind kind})
         :studio/http {:path "/api/definition"
                       :body (cond-> {:kind kind :source source :expected-epoch (:epoch token) :runtime-id (:runtime-id token)}
                               expected (assoc :expected-version expected))
                       :on-success [:studio/published token automatic? built kind expected source]
                       :on-failure [:studio/publication-refused token source]}}))))

(rf/reg-event-fx :studio/published
  ;; The show now runs the source that was sent. An edit that arrived while the
  ;; request was in flight (a late Neovim delivery, say) is newer than that text and
  ;; stays unpublished; with auto-publish on it goes out next.
  (fn [{:keys [db]} [_ token automatic? built kind expected sent ack]]
    (when (publication-current? db token)
      (let [definition (get-in db [:ui :definition])
            current? (= sent (:source definition))
            id (or (:id ack) (:definition-id ack) (:id built) (:loaded-id definition))
          version (or (:version ack) (when expected (inc expected)) 1)
          existing-cue (when built (cue db (:id built)))
          cue-source (when built (pr-str {:id (keyword (:id built)) :version 1 :name (:name built)
                                         :effect-id (keyword (:id built)) :swatch (:color built)
                                         :params {} :duration (musical-duration (:duration-value built) (:duration-unit built)) :reload-policy :preserve-state}))]
      {:db (cond-> (update-in db [:ui :definition] merge {:publishing? false :loaded-id id :version version
                                                        :loaded-owner (dissoc token :id) :error? false
                                                        :baseline {:source sent :status "Published"}}
                                 (if current?
                                   {:status "Published"
                                    :result "Validated and published. The running show now has this definition."}
                                   {:status "Unpublished changes"
                                    :result "Published the text sent earlier. Your newer edits are not published yet."}))
             (get-in built [:reactive :base]) (assoc-in [:ui :builder :reactive :base :version] version))
       :fx (cond-> [] (not automatic?) (conj (notice "Definition validated and published."))
             ;; Keep the edit's quiet time: publishing at once would chain a request per
             ;; reply while the operator is still typing.
             (and (not current?) (:auto? definition))
             (conj [:dispatch-later {:ms auto-publish-delay-ms
                                     :dispatch [:studio/auto-publish (:edit-revision definition)]}])
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
        ;; A signal binding set from the REPL is kept by the plan, not this slider.
        saved (let [target (get-in db [:show :auto :config :activity-target])] (when (number? target) target))]
    {:enabled? (get form :target-activity? (some? saved))
     :target (or (:activity-target form) saved 0.5)}))

(defn auto-plan
  "The section plan and filters saved on the show's auto config, if any."
  [db]
  (let [config (get-in db [:show :auto :config])]
    (when (or (seq (:sections config)) (:categories config) (:max-lit config)) config)))

(defn auto-varies?
  "Vary marked parameters: the form's choice, else whether the saved config varies."
  [db]
  (let [saved (get-in db [:show :auto :config :randomize])]
    (get-in db [:ui :auto-form :randomize?]
            (boolean (and (map? saved) (not (#{:none "none"} (:params saved))))))))

(defn auto-keeps-plan? [db]
  (and (some? (auto-plan db)) (get-in db [:ui :auto-form :keep-plan?] true)))

(def auto-lines
  "When Start and Stop take effect: now, or on the next bar, phrase or auto
  section line, as the REPL's and agent's :at (#767)."
  [["now" "Now"] ["next-bar" "Next bar"] ["next-phrase" "Next phrase"] ["next-section" "Next section line"]])

(defn auto-at
  "The :at Start and Stop send, or nil to apply now. A section line chosen
  while no section plays is still sent, so the show refuses it with its
  reason, as it does for the REPL and agents, rather than applying now."
  [db]
  (let [line (get-in db [:ui :auto-form :applies] "now")]
    (when-not (= "now" line) line)))

(defn- auto-request [db body]
  (let [at (auto-at db)]
    (cond-> body
      at (assoc :at at :source {:adapter "web" :session-id (:session db)}))))

(defn auto-pending-text
  "One line per auto change waiting on its line, with its zero-based position."
  [db]
  (let [per-bar (get-in db [:show :transport :meter :beats-per-bar] 4)]
    (mapv (fn [{:keys [label due-beat]}]
            (let [bar (js/Math.floor (/ (+ due-beat 1e-6) per-bar))
                  beat (- due-beat (* bar per-bar))]
              (str label " at bar " bar (when (> (js/Math.abs beat) 1e-6) (str " beat " (.toFixed beat 2))))))
          (get-in db [:show :auto :pending]))))

(def ^:private auto-reason-text
  {:eligible "chose cues" :added-emission "added light for a moving cue"
   :intentional-darkness "moving in darkness" :unsatisfied-emission "nothing in scope lights the moving heads"
   :empty-scope "no eligible cues" :lit-limit "the room already has its light"
   :cut "cut to the new section" :release "released the last section"
   :show-restarted "the show restarted" :planner-failed "auto mode stopped"})

(defn auto-summary
  "One line describing the planner's current section and last decision."
  [db]
  (let [{:keys [section reasons]} (get-in db [:show :auto])
        decision (last reasons)
        handed (some #(when (= :performer-took-over (keyword (:reason %))) (count (:instance-ids %))) reasons)
        refused (some #(when (= :launch-refused (keyword (:reason %))) (count (:refused %))) reasons)
        text (get auto-reason-text (keyword (:reason decision)))]
    (str/join " · " (cond-> []
                      section (conj (str "Section " (wire-id section)))
                      text (conj (str/capitalize text))
                      (:message decision) (conj (:message decision))
                      handed (conj (str "handed " handed " to a performer"))
                      refused (conj (str "skipped " refused " refused cue" (when (> refused 1) "s")))))))

(rf/reg-event-fx :studio/auto-start
  (fn [{:keys [db]} _]
    (let [{:keys [interval-value interval-unit count scope mode duration-value duration-unit seed]} (get-in db [:ui :auto-form])
          randomize? (auto-varies? db)
          {:keys [enabled? target]} (auto-activity db)]
      {:studio/http {:path "/api/auto"
                     :body (auto-request db
                             (cond-> {:enabled? true :interval (musical-duration interval-value interval-unit) :count count
                                      :scope (if (= scope "all") "all" {:page-id (get-in db [:ui :page-id])})
                                      :launch {:mode mode :duration (musical-duration duration-value duration-unit)}
                                      :seed (or seed (get-in db [:show :auto :config :seed]) 42)}
                               enabled? (assoc :activity-target target)
                               randomize? (assoc :randomize {:params "marked"})
                               (auto-keeps-plan? db) (assoc :keep-plan? true)))
                     :on-success [:studio/success (if (auto-at db)
                                                    "Auto mode starts on the line. You can keep playing alongside it."
                                                    "Auto mode started. You can keep playing alongside it.")]}})))
(rf/reg-event-fx :studio/auto-stop
  (fn [{:keys [db]} _]
    {:studio/http {:path "/api/auto" :body (auto-request db (cond-> {:enabled? false} (auto-keeps-plan? db) (assoc :keep-plan? true)))
                   :on-success [:studio/success (if (auto-at db) "Auto mode stops on the line." "Auto mode stopped.")]}}))

(defn response-definition [db]
  (let [{:keys [id params scope attack-value decay-value sustain release-value unit curve]
         :or {decay-value 0 sustain 1}} (get-in db [:ui :response-form])
        selected (get-in db [:ui :selected-instance])
        instance (some #(when (= (:id %) selected) %) (get-in db [:server-projection :view :instances]))
        params (->> (str/split (or params "") #"[,\s]+") (remove str/blank?) distinct (mapv keyword))]
    (when (or (str/blank? id) (empty? params)) (throw (js/Error. "Name the response and choose at least one numeric parameter.")))
    (when (and (not= scope "all") (nil? instance)) (throw (js/Error. "Select a running effect before targeting this response.")))
    (when-not (and (number? sustain) (js/Number.isFinite sustain) (<= 0 sustain 1))
      (throw (js/Error. "Sustain level must be between 0 and 1.")))
    (let [decay (musical-duration decay-value unit)
          envelope (cond-> {:attack (musical-duration attack-value unit)
                            :release (musical-duration release-value unit)
                            :curve (keyword curve)}
                     (pos? decay-value) (assoc :decay decay)
                     (not= sustain 1) (assoc :sustain sustain))]
      (cond-> {:id (keyword id) :params params :operation :multiply :range [0 1] :envelope envelope}
        (= scope "instance") (assoc :select {:instance-ids [(:id instance)]})
        (= scope "effect") (assoc :select {:effect-ids [(:effect-id instance)]})))))
(rf/reg-event-fx :studio/response-start
  (fn [{:keys [db]} _]
    (try {:studio/http {:path "/api/response" :typed? true :body (response-definition db)
                       :on-success [:studio/success "Response started. Release fades down; remove restores the original parameters."]}}
         (catch :default error {:dispatch [:studio/failure (error-message error)]}))))
(rf/reg-event-fx :studio/response-action
  (fn [_ [_ action id]]
    {:studio/http {:path (str "/api/response/" (if (= action :release) "release" "delete")) :typed? true :body {:id id}
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
                                      (merge default-preview-settings (get-in db [:ui :preview-settings])))))}))
(rf/reg-event-fx :studio/preview-setting
  (fn [{:keys [db]} [_ key value]]
    (when (preview-setting-valid? key value)
      (let [db (assoc-in db [:ui :preview-settings key] value)]
        (cond-> {:db db :studio/preview [key value]}
          (some #{key} room-view-keys)
          (assoc :studio/room-view-store (select-keys (merge default-preview-settings (get-in db [:ui :preview-settings]))
                                                      room-view-keys)))))))
(rf/reg-event-fx :studio/preview-expand
  (fn [{:keys [db]} _]
    (let [expanded? (not (get-in db [:ui :preview-expanded?]))]
      {:db (assoc-in db [:ui :preview-expanded?] expanded?)
       :studio/preview-layout expanded?})))
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
