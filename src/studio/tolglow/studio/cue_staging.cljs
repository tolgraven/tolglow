(ns tolglow.studio.cue-staging
  "Staged cue launches (#477). Reads the shared :staged-cues projection and
  sends the same :cue-stage/* owner commands as the REPL. Each committed
  slider change is one step in the shared operator history; this panel's
  Undo and Redo travel that history only across the stage's own steps.
  Colour, choice and other parameters use the shared parameter controls and
  are set without a history step, as on a running cue.
  See docs/repl-studio-parity.md#stage-a-cue-before-launching-it."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.parameter-widgets :as widgets]))

(def ^:private ui-path [:ui :cue-staging])

(def ^:private flush-ms 80)

(defn- wire-keyword [value] (some-> value events/wire-id keyword))

(defn stages
  "Staged cues in opening order, with :status as a keyword."
  [db]
  (mapv #(update % :status wire-keyword) (get-in db [:show :staged-cues])))

(defn selected-grid
  "The launch grid the cue grid uses, or nil when launches are immediate."
  [db]
  (some (fn [[choice _ grid]] (when (= choice (get-in db [:ui :launch-grid])) grid)) events/launch-grids))

(defn- grid-label [db]
  (some (fn [[choice title]] (when (= choice (get-in db [:ui :launch-grid])) title)) events/launch-grids))

(defn history-commands
  "Undo and Redo requests limited to one stage's steps in the shared history.
  Undo applies when the current step is the stage's; Redo picks the newest
  child of the current step that belongs to the stage."
  [{history-identity :identity :keys [current-id position nodes]} stage-id]
  (let [by-id (into {} (map (juxt :id identity)) nodes)
        current (get by-id current-id)
        own? #(and (= stage-id (:stage-id %)) (nil? (:unavailable-reason %)))
        redo (last (sort-by :id (filter #(and (= current-id (:parent-id %)) (own? %)) nodes)))
        request (fn [target-id]
                  (when (and history-identity (some? position) (some? target-id))
                    {:identity history-identity :expected-cursor current-id :expected-history-position position
                     :target-id target-id}))]
    {:history/undo (when (and current (own? current)) (request (:parent-id current)))
     :history/redo (when redo (request (:id redo)))}))

(defn numeric-schema
  "[low high step] for a bounded numeric parameter, else nil."
  [schema]
  (let [[low high] (:bounds schema)
        type (wire-keyword (:type schema))]
    (when (and (#{:number :integer} type) (number? low) (number? high) (< low high))
      [low high (if (= :integer type) 1 (/ (- high low) 200))])))

(defn shown-value
  "A local draft until the show's revision reaches the one that acknowledged
  it; then the committed value."
  [db stage-id param committed]
  (let [{:keys [value revision] :as draft} (get-in db (into ui-path [:drafts [stage-id (events/wire-id param)]]))]
    (if (and draft (or (nil? revision) (< (get-in db [:show :revision] 0) revision)))
      value
      committed)))

(rf/reg-event-db ::failed
  (fn [db [_ message]] (assoc-in db (conj ui-path :error) (str message))))

(rf/reg-event-db ::acknowledged
  (fn [db [_ key value {:keys [revision]}]]
    (if (and (= value (get-in db (into ui-path [:drafts key :value]))) (some? revision))
      (assoc-in db (into ui-path [:drafts key :revision]) revision)
      db)))

(rf/reg-event-fx ::edit-failed
  (fn [{:keys [db]} [_ key value message]]
    {:db (cond-> db (= value (get-in db (into ui-path [:drafts key :value])))
           (update-in (conj ui-path :drafts) dissoc key))
     :dispatch [::failed message]}))

(defn- send [db op fields on-success]
  (let [[effect request] (events/command db op fields on-success)]
    [effect (assoc request :on-failure [::failed])]))

(rf/reg-event-fx ::open [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ cue-id]]
    (when (get-in db [:connection :connected?])
      {:db (-> db (update-in ui-path dissoc :error) (update :ui dissoc :action-cell :macro-picker))
       :fx [(send db "cue-stage/open" {:cue-id cue-id :stage-id unique :command-id unique} nil)]})))

(defn- edit-fx [db unique stage-id param value]
  (let [key [stage-id (events/wire-id param)]
        [effect request] (send db "cue-stage/edit" {:stage-id stage-id :param (events/wire-id param)
                                                   :value value :command-id unique}
                               [::acknowledged key value])]
    {:db (-> db (update-in ui-path dissoc :error) (assoc-in (into ui-path [:drafts key]) {:value value}))
     :fx [[effect (assoc request :on-failure [::edit-failed key value])]]}))

(rf/reg-event-fx ::edit [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ stage-id param value]]
    (edit-fx db unique stage-id param value)))

;; A shared control can change on every pointer move. Its latest value is
;; sent once per flush interval, like the running cue's parameter queue.
(rf/reg-event-fx ::set
  (fn [{:keys [db]} [_ stage-id param value]]
    (let [key [stage-id (events/wire-id param)]
          queued? (contains? (get-in db (conj ui-path :queued)) key)]
      (cond-> {:db (-> db (assoc-in (into ui-path [:drafts key]) {:value value})
                       (update-in (conj ui-path :queued) (fnil conj #{}) key))}
        (not queued?) (assoc :dispatch-later [{:ms flush-ms :dispatch [::flush stage-id param]}])))))

(rf/reg-event-fx ::flush [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ stage-id param]]
    (let [key [stage-id (events/wire-id param)]
          db (update-in db (conj ui-path :queued) disj key)]
      (if-let [{:keys [value revision]} (get-in db (into ui-path [:drafts key]))]
        (if (nil? revision) (edit-fx db unique stage-id param value) {:db db})
        {:db db}))))

(rf/reg-event-fx ::launch [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ stage-id grid]]
    {:db (update-in db ui-path dissoc :error)
     :fx [(send db "cue-stage/launch" (cond-> {:stage-id stage-id :command-id unique} grid (assoc :grid grid)) nil)]}))

(rf/reg-event-fx ::cancel [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ stage-id]]
    {:db (update-in db ui-path dissoc :error)
     :fx [(send db "cue-stage/cancel" {:stage-id stage-id :command-id unique} nil)]}))

(rf/reg-event-fx ::history [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ op stage-id]]
    (let [projection (get-in db [:server-projection :view :operator-history])
          {[runtime-id _ epoch] :identity :keys [expected-cursor expected-history-position target-id] :as request}
          (get (history-commands projection stage-id) op)]
      (when request
        (let [fields (cond-> {:expected-epoch epoch :expected-cursor expected-cursor :command-id unique
                              :expected-history-position expected-history-position}
                       (= :history/redo op) (assoc :node-id target-id))
              [effect payload] (events/command db op fields nil epoch false runtime-id)]
          {:db (update-in db ui-path dissoc :error)
           :fx [[effect (assoc payload :on-failure [::failed])]]})))))

(defn- slider [stage-id param schema value]
  (r/with-let [local (r/atom nil)]
    (let [[low high step] (numeric-schema schema)
          shown (if (some? @local) @local value)
          label (or (:name schema) (events/wire-id param))
          commit! (fn []
                    (when-let [next @local]
                      (reset! local nil)
                      (when (not= next value) (rf/dispatch [::edit stage-id param next]))))]
      [:label.cue-stage-param
       [:span.cue-stage-param-name label]
       [:input {:type "range" :min low :max high :step step :value shown
                :aria-label (str label " for the staged cue")
                :on-change #(reset! local (js/parseFloat (.. % -target -value)))
                :on-pointer-up commit! :on-key-up commit! :on-blur commit!}]
       [:output (if (number? shown) (.toFixed shown (if (= 1 step) 0 2)) (str shown))]])))

(defn- shared-control
  "A colour, choice or unbounded parameter through the shared parameter
  control, with handlers that set the staged value."
  [parameter-control stage-id param schema params]
  (let [value (get params param)]
    [parameter-control {:id (str "staged-" stage-id) :params params} param schema
     {:change #(rf/dispatch [::set stage-id param %])
      :component #(rf/dispatch [::set stage-id param (assoc (vec value) %1 %2)])
      :key-change #(when-let [next (and (number? value) (widgets/keyboard-value schema value %))]
                     (rf/dispatch [::set stage-id param next]))}]))

(defn- status-line [{:keys [status effect-label due-beat stale]}]
  (cond
    (= :stale status) (str "Stale · " (:message stale))
    (= :scheduled status) (str "Launches at beat " due-beat)
    :else (str "Staged · " (events/wire-id effect-label))))

(defn- notes [{:keys [revised last-launch]}]
  (remove nil?
          [(when (seq (:adjusted revised))
             (str "The effect was revised; adjusted " (str/join ", " (map events/wire-id (:adjusted revised))) "."))
           (when last-launch
             (str "The last launch " (events/wire-id (:status last-launch))
                  (when-let [reason (:reason last-launch)] (str ": " (str/replace (events/wire-id reason) "-" " ")))
                  (when-let [code (:code last-launch)] (str " (" (events/wire-id code) ")"))
                  "."))]))

(defn- stage-card [db {:keys [stage-id cue-label status params schema swatch] :as stage} projection parameter-control]
  (let [connected? (get-in db [:connection :connected?])
        editing? (= :staged status)
        {:history/keys [undo redo]} (history-commands projection stage-id)
        grid (selected-grid db)
        title (events/wire-id cue-label)
        shown (into {} (map (fn [[param value]] [param (shown-value db stage-id param value)])) params)]
    [:li.cue-stage {:data-status (name status) :data-stage-id stage-id
                    :style (when swatch {"--cue-color" swatch})
                    :aria-label (str "Staged " title)}
     [:div.cue-stage-head
      [:strong title]
      [:span.cue-stage-state (status-line stage)]]
     (for [note (notes stage)] ^{:key note} [:p.cue-stage-note {:role "status"} note])
     [:div.cue-stage-params
      (for [[param schema] (sort-by (comp events/wire-id key) schema)
            :let [key (events/wire-id param)
                  value (get shown param (get shown (keyword key)))]]
        ^{:key key}
        (cond
          (and editing? (numeric-schema schema)) [slider stage-id param schema value]
          (and editing? parameter-control) [shared-control parameter-control stage-id param schema shown]
          :else [:div.cue-stage-param [:span.cue-stage-param-name (or (:name schema) key)]
                 [:output (widgets/value-label schema value)]]))]
     [:div.cue-stage-actions
      [:button.text-button {:type "button" :disabled (or (not connected?) (nil? undo))
                            :on-click #(rf/dispatch [::history :history/undo stage-id])} "Undo"]
      [:button.text-button {:type "button" :disabled (or (not connected?) (nil? redo))
                            :on-click #(rf/dispatch [::history :history/redo stage-id])} "Redo"]
      [:button.button {:type "button" :disabled (or (not connected?) (not editing?))
                       :on-click #(rf/dispatch [::launch stage-id nil])} "Launch now"]
      (when grid
        [:button.button.primary {:type "button" :disabled (or (not connected?) (not editing?))
                                 :on-click #(rf/dispatch [::launch stage-id grid])}
         (str "Launch on " (str/lower-case (grid-label db)))])
      [:button.text-button {:type "button" :disabled (not connected?)
                            :on-click #(rf/dispatch [::cancel stage-id])} "Cancel"]]]))

(defn panel
  "Staged cues waiting to launch, beside the cue grid. `parameter-control`
  is the shared parameter control for colour, choice and other values."
  [db & [parameter-control]]
  (let [items (stages db)
        projection (get-in db [:server-projection :view :operator-history])
        error (get-in db (conj ui-path :error))]
    (when (or (seq items) error)
      [:section.cue-staging-panel.panel {:aria-label "Staged cues"}
       [:div.panel-top [:strong "Staged cues"]
        [:span.small.muted "Adjust, then launch with these values · edits join operator history"]]
       [:ol.cue-stages
        (for [stage items] ^{:key (:stage-id stage)} [stage-card db stage projection parameter-control])]
       (when error [:p.cue-stage-error {:role "alert"} error])])))

(defn stage-button
  "The cue actions entry that stages a cue instead of launching it."
  [db cue-id]
  [:button.button {:type "button" :data-cue-action "stage"
                   :disabled (or (nil? cue-id) (not (get-in db [:connection :connected?])))
                   :on-click #(rf/dispatch [::open cue-id])}
   "Stage and adjust"])
