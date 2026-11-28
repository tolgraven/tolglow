(ns tolglow.studio.parameter-source
  "Attach a musical source to one running input. The show supplies its sampled result."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.fixture-picker :as fixtures]
            [tolglow.studio.modulation :as motion]
            [tolglow.studio.parameter-widgets :as widgets]))

(defn reading [instance param]
  (get (:parameter-sources instance) (keyword (events/wire-id param))))

(defn bound? [instance param] (boolean (seq (:sources (reading instance param)))))

(defn- driving-source? [{:keys [held? pending? error]}] (not (or held? pending? error)))

(defn driven? [instance param]
  (let [{:keys [sources error]} (reading instance param)]
    (and (nil? error) (boolean (some driving-source? sources)))))

(defn- driving-block [source]
  (some #(when (and (driving-source? %) (= :blocks (keyword (:type %)))) %) (:sources source)))

(defn- source-of [source kind]
  (some #(when (= kind (keyword (:type %))) %) (:sources source)))

(defn- selected-parameter [db id] (get-in db [:ui :parameter-source id]))

(defn- editor-parameters [param components]
  (if (seq components) (mapv first components) [param]))

(defn- editor-open? [selected params] (boolean (some #{selected} params)))

(defn scope
  "The key of one mounted control's source editor: the instance ID, or the ID
  within `mount` when the same parameter is also shown elsewhere (a layout tile),
  so each mount has its own DOM IDs and opens its own editor."
  [id mount]
  (if mount (str mount "-" id) id))

(defn- trigger-id [id param] (str "source-" id "-" (events/wire-id param)))

(defn- editor-id [id param] (str "source-editor-" id "-" (events/wire-id param)))

(defn- heading-id [id param] (str "source-editor-heading-" id "-" (events/wire-id param)))

(defn- selection-effects
  "`doc` is the document of the control that was used: a tile detached into its
  own window (tolglow.studio.panel-window) has its trigger and editor there."
  [db id params param doc]
  {:db (if (some? param)
         (assoc-in db [:ui :parameter-source id] param)
         (update-in db [:ui :parameter-source] dissoc id))
   ::focus [id params (some? param) doc]})

(rf/reg-sub ::selection (fn [db [_ id]] (selected-parameter db id)))
(rf/reg-fx ::focus
  (fn [[id params opening? ^js doc]]
    (r/after-render
     (fn []
       (let [selected (selected-parameter @rf-db/app-db id)]
         (when (if opening? (editor-open? selected params) (nil? selected))
           (let [target ((if opening? heading-id trigger-id) id (first params))]
             (some-> (.getElementById (or doc js/document) target) .focus))))))))
(rf/reg-event-fx ::toggle
  (fn [{:keys [db]} [_ id params param doc]]
    ;; Multiple activations can arrive before the trigger renders again.
    (selection-effects db id params
                       (when-not (editor-open? (selected-parameter db id) params) param) doc)))
(rf/reg-event-fx ::close
  (fn [{:keys [db]} [_ id params doc]]
    (if (editor-open? (selected-parameter db id) params)
      (selection-effects db id params nil doc)
      {})))
(rf/reg-event-db ::select-axis
  (fn [db [_ id params param]]
    (if (and (editor-open? (selected-parameter db id) params) (some #{param} params))
      (assoc-in db [:ui :parameter-source id] param)
      db)))

(rf/reg-sub ::waveforms (fn [db _] (get-in db [:server-projection :view :parameter-waveforms])))
(defn priority-destination
  "The show's layer priority destination with its parameter as a keyword. The
  projection carries keyword values as text, as the request must send a keyword."
  [show]
  (when-let [destination (:layer-priority-destination show)]
    (update destination :param #(if (keyword? %) % (keyword (events/wire-id %))))))

(rf/reg-sub ::priority-destination
  (fn [db _] (or (priority-destination (:show db))
                 (priority-destination (get-in db [:server-projection :view])))))

(defn priority-param?
  "Is param the show's reserved layer priority destination?"
  [destination param]
  (and (some? param) (= (:param destination) param)))

(defn destination-schema
  "Typed schema for a waveform destination: an effect parameter or layer priority."
  [instance destination param]
  (if (priority-param? destination param)
    (:schema destination)
    (get-in instance [:definition :params param])))
(rf/reg-sub ::raw-instance
  (fn [db [_ id]]
    (some #(when (= id (:id %)) %) (get-in db [:server-projection :view :instances]))))

(defn- identity-of [show] ((juxt :runtime-id :epoch) show))

(defn- context [db] (identity-of (:show db)))

(def ^:private requests-path [:ui :source-requests])

(defn- request-status [db [owner :as key] {:keys [id accepted-revision] :as request}]
  (when (and request (= owner (context db)))
    (let [view (get-in db [:server-projection :view])
          current? (= owner (identity-of view))
          receipt (when current?
                    (some #(when (and (= id (:request-id %)) (= (second owner) (:epoch %))) %)
                          (rseq (:parameter-source-outcomes view []))))]
      (let [status (cond
                     (= :rejected (:status receipt))
                     (cond-> {:error (or (:message receipt) "The show rejected the source edit.")}
                       (:reason receipt) (assoc :reason (keyword (events/wire-id (:reason receipt)))))
                     (= :accepted (:status receipt)) nil
                     (and current? (integer? accepted-revision) (>= (:revision view -1) accepted-revision)) nil
                     :else (select-keys request [:pending? :waiting? :error :reason]))]
        ;; A move's state names the destination it was sent to.
        (cond-> status
          (and (seq status) (:move-label request)) (assoc :move-label (:move-label request)))))))

(defn request-state [db id param]
  (let [owner (context db)
        key [owner id param]
        requests (get-in db requests-path)
        direct (request-status db key (get requests key))]
    (or (when (:pending? direct) direct)
        ;; A move from another input (or activation) that targets this one.
        (some (fn [[[session :as other-key] request]]
                (when (and (= owner session) (not= key other-key)
                           (= [id param] [(:target-instance request) (:target request)]))
                  (let [status (request-status db other-key request)]
                    (when (:pending? status) status)))) requests)
        direct)))

(rf/reg-sub ::context (fn [db _] (context db)))
(rf/reg-sub ::request (fn [db [_ id param]] (request-state db id param)))

(def stale-session-message
  "The show restarted or reconnected after this destination was chosen. The waveform has not moved; choose the destination again.")

(defn request
  "Send one bind or unbind for input `param` of activation `id`. A move names
  the input it leaves (`from-param`, and `from-instance-id` when that input
  belongs to another activation) and the show session it was chosen in
  (`owner`). A move chosen in another session is refused here, with that
  reason, instead of being dropped."
  [db path id param binding request-id & [{:keys [from-param from-instance-id expected-source owner label]}]]
  (let [[runtime epoch :as current] (context db)
        source (or from-param param)
        source-instance (or from-instance-id id)
        key [current source-instance source]
        reply [::reply key request-id]
        entry (cond-> {:id request-id :target param :target-instance id} label (assoc :move-label label))]
    (cond
      (and from-param (not= owner current))
      {:db (assoc-in db (conj requests-path key)
                     (assoc entry :pending? false :reason :stale-show-session :error stale-session-message))}

      (or (:pending? (request-state db source-instance source)) (:pending? (request-state db id param))) {}

      :else
      {:db (assoc-in db (conj requests-path key) (assoc entry :pending? true))
       :dispatch-later [{:ms 20000 :dispatch (conj reply {:status :expired})}]
       :studio/http {:path (str "/api/modulation/" path) :typed? true :raw-response? true
                     :body (cond-> {:expected-epoch epoch :runtime-id runtime :instance-id id :param param :request-id request-id}
                             binding (assoc :binding binding)
                             from-param (assoc :from-param from-param :expected-source expected-source)
                             (and from-param (not= source-instance id)) (assoc :from-instance-id source-instance))
                     ;; A refusal arrives whole, with its details, on the reply.
                     :on-success reply :on-rejection reply :on-failure [::failed key request-id]
                     :on-uncertain (conj reply {:status :pending})}})))

(defn move-result
  "What became of the move `request-id` sent from input `param` of activation
  `id`: nil while it is unsent or pending, else {:status :accepted} or
  {:status :rejected :error … :reason …}. A settled move no longer drives the
  Destination select, which then shows where the waveform actually is."
  [db id param request-id]
  (let [key [(context db) id param]
        request (get-in db (conj requests-path key))]
    (when (and (some? request-id) (= request-id (:id request)))
      (let [status (request-status db key request)]
        (cond
          (:pending? status) nil
          (:error status) (assoc (select-keys status [:error :reason :move-label]) :status :rejected)
          :else {:status :accepted})))))

(rf/reg-sub ::move-result (fn [db [_ id param request-id]] (move-result db id param request-id)))

(defn settled-move
  "The settled state of a move draft: `result` (see `move-result`), or a
  :stale-show-session refusal as soon as the show session the draft was
  chosen in (`[:source :owner]`) is no longer `owner`, sent or not."
  [draft owner result]
  (or result
      (when (and draft (not= owner (get-in draft [:source :owner])))
        {:status :rejected :reason :stale-show-session :error stale-session-message :move-label (:label draft)})))

(defn rejection-reason
  "The refusal reason of a rejected acknowledgement: the binding refusal's
  :reason, or :stale-show-session when the show refused the request's epoch."
  [{:keys [details]}]
  (cond
    (some? (:reason details)) (keyword (events/wire-id (:reason details)))
    (contains? details :received-epoch) :stale-show-session))

(defn record-reply [db [owner :as key] request-id {:keys [status revision message] :as receipt}]
  (let [path (conj requests-path key)
        request (get-in db path)]
    (if (and (= owner (context db)) (= request-id (:id request)))
      (case status
        :accepted (if (integer? revision) (assoc-in db (conj path :accepted-revision) revision) db)
        :pending (assoc-in db (conj path :waiting?) true)
        :rejected (update-in db path merge (cond-> {:pending? false :error (events/error-message message)}
                                             (rejection-reason receipt) (assoc :reason (rejection-reason receipt))))
        :expired (if-let [visible (request-status db key request)]
                   (if (:pending? visible)
                     (update-in db path merge {:pending? false :error "The show has not confirmed this edit. Check the current source before retrying."})
                     (assoc-in db path (merge request visible {:pending? false})))
                   (update-in db requests-path dissoc key))
        db)
      db)))

(rf/reg-event-fx ::bind [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ id param binding]] (request db "bind" id param binding unique)))
(rf/reg-event-fx ::unbind [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ id param]] (request db "unbind" id param nil unique)))
(rf/reg-event-fx ::move [(rf/inject-cofx :studio/unique)]
  ;; The editor passes its own request ID so it can tell when its move settles.
  (fn [{:keys [db unique]} [_ id param endpoints move request-id]]
    (request db "bind" id param endpoints (or request-id unique) move)))
(rf/reg-event-db ::reply (fn [db [_ key request-id receipt]] (record-reply db key request-id receipt)))
(rf/reg-event-db ::failed
  (fn [db [_ key request-id message]] (record-reply db key request-id {:status :rejected :message message})))

(rf/reg-sub ::fixture-limits
  (fn [[_ id] _] (rf/subscribe [::raw-instance id]))
  (fn [instance _]
    (cond-> (select-keys (:definition instance) [:targets :outputs])
      (contains? instance :targets) (assoc :launch-targets (:targets instance)))))

(defn fixture-options [rig {:keys [targets launch-targets outputs]}]
  (let [groups (when (vector? outputs) (map :targets outputs))
        output-targets (when (and (seq groups) (every? some? groups)) (into #{} cat groups))
        defined (when (some? targets) (set targets))
        ordered (if (some? launch-targets) (keep (into {} (map (juxt :id identity)) rig) launch-targets) rig)]
    (into [] (comp (filter (fn [{:keys [id]}]
                             (and (or (nil? defined) (contains? defined id))
                                  (or (nil? output-targets) (contains? output-targets id)))))
                   (map #(select-keys % [:id :name]))) ordered)))

(rf/reg-sub ::fixtures
  (fn [[_ id] _] [(rf/subscribe [::fixtures/fixtures]) (rf/subscribe [::fixture-limits id])])
  (fn [[rig limits] _] (fixture-options rig limits)))

(rf/reg-sub ::fixture-choice (fn [db [_ id]] (get-in db [:ui :block-fixtures id])))
(rf/reg-event-db ::fixture-select (fn [db [_ id target]] (assoc-in db [:ui :block-fixtures id] target)))
(rf/reg-sub ::fixture
  (fn [[_ id] _] [(rf/subscribe [::fixtures id]) (rf/subscribe [::fixture-choice id])])
  (fn [[rows chosen] _] (or (some #(when (= (:id %) chosen) %) rows) (first rows))))

(defn- fixture-selector [id]
  (r/with-let [search (r/atom "")]
    (let [rows @(rf/subscribe [::fixtures id]) target (:id @(rf/subscribe [::fixture id]))
          options (fixtures/fixture-options rows @search target)]
      (if (empty? options) [:span.block-trace-empty "No target fixtures"]
        [:div.source-fields
         (when (> (count rows) fixtures/page-size)
           [:label.source-field "Find fixture" [:input {:type "search" :value @search :on-change #(reset! search (.. % -target -value))}]])
         [:label.source-field "Sample fixture"
          [:select {:aria-label "Sample fixture" :value "0"
                    :on-change #(rf/dispatch [::fixture-select id (get options (parse-long (.. % -target -value)))])}
           (for [[i fixture] (map-indexed vector options)] ^{:key i} [:option {:value i} (events/wire-id fixture)])]]]))))

(defn- native-value [instance param]
  (let [source (reading instance param)]
    (if-let [block (driving-block source)]
      (when-let [target (:id @(rf/subscribe [::fixture (:id instance)]))]
        (:result @(rf/subscribe [:studio/remote [:runtime/block-trace (:id instance) (mapv keyword (:path block)) target]])))
      (:effective-value source))))

(defn- result-view [view instance schema sample block? & [options]]
  (if-not block? [view schema sample options]
    (if-let [{:keys [id name]} @(rf/subscribe [::fixture (:id instance)])]
      [:<> [:span.source-sample-target (str "Fixture · " (or name (events/wire-id id)))]
       ^{:key (pr-str id)} [view schema sample options]]
      [:span.block-trace-empty "No target fixtures"])))

(defn source-period
  "Beats in one cycle of the waveform that currently drives an input. The show
  measures it in the meter the binding follows."
  [source]
  (some #(when (and (driving-source? %) (= :lfo (keyword (:type %)))) (:period-beats %)) (:sources source)))

(defn result-visual [view instance param schema & [options]]
  (when (driven? instance param)
    (let [source (reading instance param)]
      [result-view view instance schema #(native-value instance param) (driving-block source)
       (merge options {:period-beats (source-period source)})])))

(defn axes-visual [view instance components schema & [options]]
  (let [axis (fn [[param spec]]
               (if (driven? instance param)
                 (native-value instance param)
                 (get-in instance [:params param] (:default spec))))
        sample (fn [] (let [values (mapv axis components)] (when (every? some? values) values)))
        block? (some (fn [[k _]] (driving-block (reading instance k))) components)]
    [result-view view instance schema sample block?
     (merge options {:period-beats (some (fn [[k _]] (source-period (reading instance k))) components)})]))

(defn- owner-document [^js event] (some-> event .-currentTarget .-ownerDocument))

(defn- close! [id params event]
  (rf/dispatch [::close id params (owner-document event)]))

(defn trigger [instance-id param title source & [components mount]]
  (let [id (scope instance-id mount)
        selected @(rf/subscribe [::selection id]) inputs (:sources source)
        params (editor-parameters param components)
        owner (first params)
        expanded? (editor-open? selected params)
        state (cond (:error source) :error (some driving-source? inputs) :driving
                    (some :error inputs) :error (some :pending? inputs) :pending (seq inputs) :base)
        action (str (if state "Edit" "Attach") " source for " title)
        status (case state :driving "Source is driving this input" :pending "Awaiting a source sample"
                           :base "Using the manual base value" :error "Source error" nil)]
    [:button.param-source-trigger {:type "button" :class (when (= state :driving) "bound")
                                  :id (trigger-id id owner)
                                  :aria-expanded expanded?
                                  :aria-controls (when expanded? (editor-id id owner))
                                  :title (if status (str action " · " status) action) :aria-label action :aria-description status
                                  :on-click #(rf/dispatch [::toggle id params param (owner-document %)])}
     [:span {:aria-hidden true :class (when (= state :base) "transport-symbol pause")}
      (case state :driving "∿" :base nil :pending "…" :error "!" "+")]
     (if components "Sources" "Source")]))

(defn- options [schema]
  (or (when (seq (:choices schema)) (mapv :value (:choices schema))) (:values schema) (:options schema)))

(defn- numeric-bounds [schema [lo hi]]
  (case (keyword (:type schema))
    :integer [(when (some? lo) (js/Math.ceil lo)) (when (some? hi) (js/Math.floor hi))]
    :color [(max 0 (or lo 0)) (min 1 (or hi 1))]
    [lo hi]))

(defn- bounds [schema base]
  (let [[lo hi :as limits] (numeric-bounds schema (or (:bounds schema) (:recommended schema) [0 1]))
        ;; A recommendation can miss the color range or contain no integer.
        [lo hi] (if (<= lo hi) limits [0 1])]
    (cond
      (seq (options schema)) [(first (options schema)) (last (options schema))]
      (= :boolean (keyword (:type schema))) [false true]
      (sequential? base) (let [n (count base)] [(vec (repeat n lo)) (vec (repeat n hi))])
      :else [lo hi])))

(defn draft [schema base binding]
  (let [[lo hi] (bounds schema base)
        [unit count] (or (first (:period binding)) [:beats 1])]
    {:waveform (get binding :waveform :sine) :unit (keyword unit) :period (str count)
     :phase-reference (:phase-reference binding :launch) :phase (str (:phase binding 0))
     :from (get binding :from lo) :to (get binding :to hi)
     :inputs (or (:inputs binding) {})}))

(defn- priority-schema? [schema] (= :priority (keyword (:category schema))))

(defn binding-config [form]
  (let [period (motion/number-value (:period form) "Period")]
    (when-not (pos? period) (throw (js/Error. "Period must be positive.")))
    (cond-> {:type :lfo :waveform (:waveform form) :period {(:unit form) period}
             :phase-reference (:phase-reference form) :phase (motion/number-value (:phase form) "Phase")
             :from (:from form) :to (:to form)
             :inputs (:inputs form)}
      (contains? form :band)
      (assoc :hysteresis
             (let [band (motion/number-value (:band form) "Hysteresis band")
                   dwell (if (str/blank? (str (:dwell form))) 0 (motion/number-value (:dwell form) "Minimum dwell"))]
               (when (neg? band) (throw (js/Error. "Hysteresis band cannot be negative.")))
               (when (neg? dwell) (throw (js/Error. "Minimum dwell cannot be negative.")))
               (cond-> {:band band} (pos? dwell) (assoc :dwell {(keyword (:dwell-unit form :beats)) dwell})))))))

(defn- number-field [label value change! schema]
  (let [[low high] (numeric-bounds schema (:bounds schema))]
    [:label.source-field [:span label]
     [:input {:type "number" :aria-label label :value value :min low :max high
              :step (if (= :integer (keyword (:type schema))) 1 "any")
              :on-change #(let [raw (.. % -target -value)]
                            (change! (if (str/blank? raw) "" (js/Number raw))))}]]))

(defn- endpoint [label schema value change!]
  (cond
    (or (seq (options schema)) (= :boolean (keyword (:type schema))))
    (let [choices (vec (or (options schema) [false true]))]
      [:label.source-field [:span label]
       [:select {:aria-label label :value (first (keep-indexed #(when (= %2 value) %1) choices))
                 :on-change #(change! (get choices (js/Number (.. % -target -value))))}
        (for [[i v] (map-indexed vector choices)] ^{:key i} [:option {:value i} (widgets/label v)])]])
    (sequential? value)
    (let [kind (widgets/parameter-kind schema)
          names (or (:component-labels schema) (case kind :color ["R" "G" "B"] :range ["Low" "High"] ["X" "Y" "Z" "W"]))]
      [:fieldset.source-components
       [:legend (when (= :color kind) [:i.source-color-swatch {:style {:background (widgets/color-css value)}}]) label]
       (for [[index v] (map-indexed vector value)]
         ^{:key index} [number-field (str label " " (get names index (inc index))) v #(change! (assoc (vec value) index %)) schema])])
    :else [number-field label value change! schema]))

(defn editor-draft [schema base binding catalog]
  (let [form (cond-> (draft schema base binding)
               ;; Layer priority quantizes with hysteresis; a quarter tier is the show default.
               (priority-schema? schema)
               (assoc :band (str (get-in binding [:hysteresis :band] 0.25))
                      ;; Keep the authored musical unit; a bar dwell stays in bars.
                      :dwell (str (or (second (first (get-in binding [:hysteresis :dwell]))) 0))
                      :dwell-unit (keyword (or (ffirst (get-in binding [:hysteresis :dwell])) :beats)))
               ;; A whole-range sweep of every tier is rarely meant; start one tier either side.
               (and (priority-schema? schema) (nil? binding)) (assoc :from -1 :to 1))]
    (if (or binding (some #(= :sine (:id %)) catalog)) form
      (assoc form :waveform (:id (first catalog))))))

(defn destination-options
  "Destinations on the same activation that can take the waveform: unbound effect
  parameters, then the show's layer priority destination when it is free."
  [instance param & [priority]]
  (let [free? (fn [target] (not (or (= param target) (source-of (reading instance target) :lfo))))]
    (cond-> (->> (get-in instance [:definition :params])
                 (filter (comp free? first))
                 (sort-by (comp str first))
                 vec)
      (and priority (free? (:param priority))) (conj [(:param priority) (:schema priority)]))))

(defn activation-label
  "How the Destination select names another running activation."
  [instance]
  (str (or (get-in instance [:definition :name]) (widgets/label (:cue-id instance)))
       " · " (events/wire-id (:id instance))))

(defn other-destinations
  "Free inputs on the other running activations, in the show's activation
  order: [activation [[param schema] …]] for each activation that has one."
  [instances id & [priority]]
  (into [] (keep (fn [instance]
                   (when (and (not= id (:id instance)) (= "running" (events/wire-id (:phase instance))))
                     (when-let [options (seq (destination-options instance nil priority))]
                       [instance (vec options)]))))
        instances))

(defn destination-value
  "The select value of a destination: the parameter on this activation, or
  [activation-id parameter] on another one."
  [instance-id param]
  (pr-str (if instance-id [instance-id param] param)))

(defn refusal-text
  "The alert for an input's source request. A refused move names the
  destination, the show's reason and, while the projection still shows the
  waveform here, the input it stays on."
  [{:keys [error move-label]} here still-here?]
  (when error
    (if move-label
      (let [sentence (if (re-find #"[.!?]$" (str/trim error)) (str/trim error) (str (str/trim error) "."))]
        (str "Moving the LFO to " move-label " was refused. " sentence (when still-here? (str " It stays on " here "."))))
      error)))

(rf/reg-sub ::activations (fn [db _] (get-in db [:server-projection :view :instances])))

(defn- lfo-editor [id param schema base source catalog instance & [{:keys [shown forget!] :as panel}]]
  ;; From lfo-panel: `shown` is the LFO to edit, the one last seen bound here,
  ;; so the actions stay mounted (disabled) while the projection briefly
  ;; lacks it (#795); `current` is what the projection shows now.
  (let [current (if panel (:current panel) (source-of source :lfo))
        lfo (if panel shown current)]
    (r/with-let [form (r/atom (editor-draft schema base (:binding lfo) catalog))
                 move (r/atom nil) error (r/atom nil)]
      (let [{:keys [waveform period unit phase-reference phase inputs band dwell dwell-unit]} @form
            in-flight @(rf/subscribe [::request id param])
            owner @(rf/subscribe [::context])
            priority @(rf/subscribe [::priority-destination])
            activations @(rf/subscribe [::activations])
            destinations (when lfo (destination-options instance param priority))
            elsewhere (when lfo (other-destinations activations id priority))
            ;; A settled move (refused or committed) no longer drives the select,
            ;; which then shows the input the waveform is actually on.
            result (settled-move @move owner @(rf/subscribe [::move-result id param (:request-id @move)]))
            draft (when-not result @move)
            destination (:param draft)
            target-instance (when-let [other (:instance-id draft)] (some #(when (= other (:id %)) %) activations))
            target-schema (if (:instance-id draft)
                            (when target-instance (destination-schema target-instance priority destination))
                            (destination-schema instance priority destination))
            offered? (or (some #(= destination (first %)) (when-not (:instance-id draft) destinations))
                         (some (fn [[activation options]]
                                 (and (= (:id activation) (:instance-id draft)) (some #(= destination (first %)) options)))
                               elsewhere))
            here (or (:name schema) (widgets/label param))
            {:keys [from to]} (or draft @form)
            wave (if (some? waveform)
                   (some #(when (= waveform (:id %)) %) catalog)
                   (first catalog))
            update! (fn [key value] (swap! form assoc key value))
            update-endpoint! (fn [key value] (swap! (if destination move form) assoc key value))
            choose! (fn [value]
                      (let [[activation target spec]
                            (or (some (fn [[key spec]] (when (= value (destination-value nil key)) [nil key spec])) destinations)
                                (some (fn [[activation options]]
                                        (some (fn [[key spec]] (when (= value (destination-value (:id activation) key))
                                                                 [activation key spec])) options))
                                      elsewhere))
                            [lo hi] (if (priority-param? priority target)
                                      [-1 1]
                                      (bounds spec (get-in (or activation instance) [:params target] (:default spec))))
                            label (cond-> (or (:name spec) (widgets/label target))
                                    activation (str " on " (activation-label activation)))]
                        (reset! move (when target
                                       (cond-> {:param target :from lo :to hi :label label
                                                :source {:owner owner :from-param param :expected-source (:identity lfo)}}
                                         activation (assoc :instance-id (:id activation)))))))
            apply! (fn [e]
                     (.preventDefault e)
                     (reset! error nil)
                     (try (if destination
                            (let [request-id (str (random-uuid))]
                              (swap! move assoc :request-id request-id)
                              (rf/dispatch [::move (or (:instance-id draft) id) destination {:from from :to to}
                                            (cond-> (assoc (:source draft) :label (:label draft))
                                              (:instance-id draft) (assoc :from-instance-id id))
                                            request-id]))
                            (rf/dispatch [::bind id param (binding-config (assoc @form :waveform (:id wave)))]))
                          (catch :default problem (reset! error (events/error-message problem)))))]
        [:form.live-source-form {:on-submit apply!}
         [:div.source-fields
          (when-not destination
            [:<>
             [:label.source-field [:span "Waveform"]
              [:select {:aria-label "Waveform source" :value (pr-str (:id wave))
                        :on-change #(let [selected (.. % -target -value)
                                          id (:id (some (fn [item] (when (= selected (pr-str (:id item))) item)) catalog))]
                                      (swap! form assoc :waveform id :inputs {}))}
               (for [{:keys [id name]} catalog] ^{:key (pr-str id)} [:option {:value (pr-str id)} (or name (widgets/label id))])]]
             [:label.source-field [:span "Period"]
              [:input {:type "text" :aria-label "Source period" :value period :required true
                       :on-change #(update! :period (.. % -target -value))}]]
             [:label.source-field [:span "Unit"]
              [:select {:aria-label "Source period unit" :value (name unit) :on-change #(update! :unit (keyword (.. % -target -value)))}
               (for [u [:beats :bars :phrases]] ^{:key u} [:option {:value (name u)} (widgets/label u)])]]
             [:label.source-field [:span "Phase reference"]
              [:select {:value (name phase-reference)
                        :on-change #(update! :phase-reference (keyword (.. % -target -value)))}
               [:option {:value "launch"} "Cue launch"]
               [:option {:value "transport"} "Show transport"]]]
             [number-field "Phase (cycles)" phase #(update! :phase %) {}]
             (when (priority-schema? schema)
               [:<>
                [number-field "Hysteresis band (tiers)" band #(update! :band %) {:bounds [0 8]}]
                [number-field "Minimum dwell" dwell #(update! :dwell %) {:bounds [0 nil]}]
                [:label.source-field [:span "Dwell unit"]
                 [:select {:aria-label "Minimum dwell unit" :value (name (or dwell-unit :beats))
                           :on-change #(update! :dwell-unit (keyword (.. % -target -value)))}
                  (for [u [:beats :bars :phrases]] ^{:key u} [:option {:value (name u)} (widgets/label u)])]]])])
          (when (or destination (seq destinations) (seq elsewhere))
            (let [local (into [[:option {:key "here" :value (destination-value nil param)} here]]
                              (for [[key spec] destinations]
                                [:option {:key (pr-str key) :value (destination-value nil key)} (or (:name spec) (widgets/label key))]))]
              [:label.source-field [:span "Destination"]
               [:select {:aria-label "Destination" :disabled (:pending? in-flight)
                         :value (if destination (destination-value (:instance-id draft) destination) (destination-value nil param))
                         :on-change #(choose! (.. % -target -value))}
                (when (and destination (not offered?))
                  [:option {:value (destination-value (:instance-id draft) destination) :disabled true} (:label draft)])
                (if (seq elsewhere)
                  (into [:optgroup {:label "This activation"}] local)
                  (into [:<>] local))
                (for [[activation options] elsewhere] ^{:key (pr-str (:id activation))}
                  [:optgroup {:label (activation-label activation)}
                   (for [[key spec] options] ^{:key (pr-str key)}
                     [:option {:value (destination-value (:id activation) key)} (or (:name spec) (widgets/label key))])])]]))
          [endpoint "From" (or target-schema schema) from #(update-endpoint! :from %)]
          [endpoint "To" (or target-schema schema) to #(update-endpoint! :to %)]]
         (when (and (not destination) (seq (:params wave)))
           [:div.source-fields
            (for [[key input] (:params wave)] ^{:key key}
              [number-field (or (:name input) (widgets/label key)) (get inputs key (:default input))
               #(swap! form assoc-in [:inputs key] %) input])])
         (when-let [message (or @error
                                (when (= :stale-show-session (:reason result)) (refusal-text result here (some? current)))
                                (refusal-text in-flight here (some? current)))]
           [:p.performance-error {:role "alert"} message])
         (when (and (:pending? in-flight) (:waiting? in-flight))
           [:p {:role "status"} "Waiting for the show to confirm the source change."])
         [:div.source-actions
          [:button.button.primary {:type "submit" :disabled (or (:pending? in-flight) (not wave))}
           (cond destination "Move LFO" lfo "Update LFO" :else "Attach LFO")]
          ;; Kept mounted (disabled) while a change is pending, so a projection
          ;; that briefly lacks the LFO during the round trip does not remove
          ;; the buttons under a press.
          (when (and (or lfo (:pending? in-flight)) (not destination))
            [:<>
             [:button.button {:type "button" :disabled (or (:pending? in-flight) (not current))
                              :on-click #(rf/dispatch [(if (:held? lfo) :modulation/resume :modulation/hold) id param])}
              (if (:held? lfo) "Resume LFO" "Use base value")]
             [:button.button {:type "button" :disabled (or (:pending? in-flight) (not current))
                              :on-click #(do (when forget! (forget!)) (rf/dispatch [::unbind id param]))} "Detach LFO"]])]]))))

(defn lfo-identity
  "What tells one bound LFO from another: its projected :identity, else its
  binding (older projections)."
  [lfo]
  (when lfo (or (:identity lfo) (:binding lfo))))

(defn remembered-lfo
  "The next state of an LFO panel's memory, {:known :seen :detaching}, given
  what the projection shows now (`current`, at `revision`) and whether a
  request for this parameter is pending. The panel shows `:known`.
  - A brief gap during a round trip keeps the LFO: while a request is
    pending, or while the projection is still at the revision that last
    showed it.
  - An absence confirmed past that (an unbind from the REPL or another
    surface) forgets it, so the panel offers Attach again.
  - The binding this panel asked to detach is not learned again while the
    projection still shows it."
  [{:keys [known seen detaching] :as state} {:keys [current revision pending?]}]
  ;; LFOs compare by their projected :identity (bound-at, definition and
  ;; waveform versions), not their settings, so a detach followed by a
  ;; re-attach with identical settings is a new LFO.
  (let [current-id (lfo-identity current)]
    (cond
      (and current (not= current-id detaching)) {:known current :seen revision :detaching detaching}
      current state
      :else (cond-> (assoc state :detaching nil)
              (and known (not pending?) (integer? revision) (integer? seen) (> revision seen))
              (assoc :known nil :seen nil)))))

(rf/reg-sub ::view-revision (fn [db _] (get-in db [:server-projection :view :revision])))

(defn- lfo-panel
  "The LFO editor, keyed by the LFO binding last seen here (`remembered-lfo`).
  A projection that briefly lacks the LFO during a round trip neither resets
  the draft nor unmounts the actions under a press; a confirmed absence, or
  this panel's own Detach, forgets it."
  [id param schema base source catalog instance]
  (r/with-let [memory (atom {})]
    (let [current (source-of source :lfo)
          revision @(rf/subscribe [::view-revision])
          pending? (boolean (:pending? @(rf/subscribe [::request id param])))
          {:keys [known detaching]} (swap! memory remembered-lfo {:current current :revision revision :pending? pending?})
          live (when (not= (lfo-identity current) detaching) current)
          lfo (or live known)]
      (with-meta [lfo-editor id param schema base source catalog instance
                  {:shown lfo :current live
                   :forget! #(swap! memory (fn [m] (assoc m :detaching (lfo-identity (:known m)) :known nil :seen nil)))}]
        {:key (pr-str (lfo-identity lfo))}))))

(defn- source-label [{:keys [type binding controller-id source-id controller lane-note source id]}]
  (case (keyword type)
    :lfo (str (widgets/label (:waveform binding)) " LFO")
    :random (str "Random · " (widgets/label controller-id))
    :patch (str "Patch · " (widgets/label source-id))
    :live (str "Live · CC " controller (when lane-note (str " · note " lane-note)))
    :spatial (str "Spatial" (when (or (string? source) (keyword? source)) (str " · " (widgets/label source))))
    :blocks "Fixture block chain"
    :response (str "Response envelope" (when id (str " · " (widgets/label id))))
    (widgets/label type)))

(defn editor [instance param schema components & [mount]]
  (let [id (:id instance) title (or (:name schema) (widgets/label param))
        params (editor-parameters param components)
        owner (first params)
        scoped (scope id mount)
        raw @(rf/subscribe [::raw-instance id])
        priority @(rf/subscribe [::priority-destination])
        priority? (priority-param? priority param)
        typed (if priority? (:schema priority) (get-in raw [:definition :params param] schema))
        source (or (reading raw param) (reading instance param)) catalog @(rf/subscribe [::waveforms])
        base (if priority?
               (:base-value (or (reading raw param) (reading instance param)) 0)
               (get-in raw [:params param] (get-in instance [:params param] (:default schema))))]
    [:section.live-source-editor {:id (editor-id scoped owner) :aria-labelledby (heading-id scoped owner)
                                 :on-key-down #(when (= "Escape" (.-key %)) (.stopPropagation %) (close! scoped params %))}
     [:header [:h4 {:id (heading-id scoped owner) :tab-index -1} (str title " source")]
      [:button.text-button {:type "button" :aria-label "Close source editor" :on-click #(close! scoped params %)} "Close"]]
     (when (source-of source :blocks) [fixture-selector id])
     (when (seq components)
       [:label.source-field "Axis" [:select {:aria-label "Source axis" :value (events/wire-id param)
                                          :on-change #(let [value (.. % -target -value)
                                                            axis (some (fn [k] (when (= value (events/wire-id k)) k)) params)]
                                                        (rf/dispatch [::select-axis scoped params axis]))}
                                  (for [[k axis] components] ^{:key k} [:option {:value (events/wire-id k)} (or (:name axis) (widgets/label k))])]])
     (when-let [error (:error source)] [:p.performance-error {:role "alert"} error])
     (when (seq (:sources source))
       [:div.source-connections
        (for [[i input] (map-indexed vector (:sources source))]
          ^{:key i} [:span (source-label input)
                     (cond (:held? input) " · using base" (:pending? input) " · awaiting sample") (when-let [error (:error input)] [:span.performance-error {:role "alert"} error])])])
     ^{:key [id param]} [lfo-panel id param typed base source catalog raw]
     (when-not priority? [motion/parameter-widget id param schema base])]))

(defn mounted-editor
  "The source editor of one parameter shown in `mount`, while its trigger there
  has it open."
  [instance param schema mount]
  (when (editor-open? @(rf/subscribe [::selection (scope (:id instance) mount)]) [param])
    [editor instance param schema nil mount]))
