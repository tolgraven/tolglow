(ns tolglow.studio.performance
  "System controls over the shared command boundary. Snapshot groups provide
  category membership; this UI does not infer categories or run effect clocks."
  (:require [clojure.string :as str]
            [tolglow.studio.surfaces]
            [clojure.set :as set]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.authoring.card-layout :as card-layout]
            [tolglow.authoring.parameter-controls :as controls]
            [tolglow.authoring.parameter-layout :as layout]
            [tolglow.authoring.slot-grid :as slot-grid]
            [tolglow.studio.events :as events]
            [tolglow.studio.cue-variations :as variations]
            [tolglow.studio.effect-selection :as selection]
            [tolglow.studio.modulation :as modulation]
            [tolglow.studio.effect-inspector :as inspect]
            [tolglow.studio.spatial-picker :as spatial]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.parameter-source :as sources]
            [tolglow.studio.param-visualization :as visualization]
            [tolglow.visualization.control-geometry :as control-geometry]))

(def defaults {:generation 0 :serial 0 :requests {} :errors {} :opacity {} :envelopes {}
               :group {:scope "categories" :categories [] :match "any" :envelope-open? false}})

(defn- wire [value] (events/wire-id value))

(defn visible-cue-rows [page full?]
  (if full? 8 (max 2 (min 8 (inc (reduce max 0 (keep :y (:cells page))))))))

(defn card-definition [show instance]
  (or (:definition instance)
      (some #(when (= (wire (:id %)) (wire (:effect-id instance))) %) (:effects show))))

(defn- state [db] (merge defaults (get-in db [:ui :performance])))

(defn- store [db value] (assoc-in db [:ui :performance] value))

(defn request-token [db] [(get-in db [:show :runtime-id]) (get-in db [:show :epoch]) (:generation (state db))])

(defn ordered-instances [{:keys [instances active-instance-order]}]
  (if active-instance-order
    (let [by-id (into {} (map (juxt :id identity)) instances)]
      (into [] (keep by-id) active-instance-order))
    instances))

(defn effective-instances
  "Running cards in the order the latest frame composed them. A waveform on
  layer priority can differ from the committed order; reorder captures and
  move targets keep using the committed order from ordered-instances."
  [{:keys [instances effective-instance-order] :as show}]
  (if effective-instance-order
    (let [by-id (into {} (map (juxt :id identity)) instances)]
      (into [] (keep by-id) effective-instance-order))
    (ordered-instances show)))

(defn- mix-capture [show instances]
  {:identity [(:runtime-id show) (:epoch show)
              (into [] (comp (filter #(= "running" (:phase %))) (map :id)) instances)]
   :revision (:revision show)})

(defn- reorder-pending? [show request]
  (and request (or (nil? (:revision request)) (< (:revision show) (:revision request)))))

(defn- priority-source-id [source [runtime-id epoch running-ids]]
  (let [[source-runtime source-epoch] (:identity source)
        id (:id source)]
    (when (and id (= runtime-id source-runtime) (= epoch source-epoch) (some #{id} running-ids))
      id)))

(defn- render-priority [instance]
  (get instance :render-priority (get instance :priority 0)))

(defn modulated-priority
  "The frame tier from a driving layer priority waveform when it differs from
  the committed priority, else nil."
  [instance destination]
  (when-let [param (:param destination)]
    (when (sources/driven? instance param)
      (let [tier (:effective-value (sources/reading instance param))]
        (when (and (number? tier) (not= tier (render-priority instance))) tier)))))

(defn target-key [target] (if-let [id (:instance-id target)] [:instance (wire id)] [:group (:selector target)]))

(defn group-selector [form]
  (case (:scope form)
    "all" {}
    "categories" {:categories (vec (distinct (map wire (:categories form)))) :match (:match form "any")}
    {:categories [] :match "any"}))

(defn selected-instances
  "Project current server group membership for a visible target list. The host
  remains responsible for validating and applying the eventual selector."
  [show form]
  (let [instances (:instances show)]
    (if (= "all" (:scope form)) (vec instances)
      (let [wanted (map wire (:categories form))
            groups (into {} (map (fn [group] [(wire (:id group)) (set (map wire (:instance-ids group)))]) (:groups show)))
            sets (map #(get groups % #{}) wanted)
            ids (if (seq sets) (apply (if (= "all" (:match form)) set/intersection set/union) sets) #{})]
        (filterv #(ids (wire (:id %))) instances)))))

(defn envelope-duration [value unit]
  (let [n (modulation/number-value value "Envelope duration")]
    (when-not (and (not (neg? n)) (#{"beats" "bars" "phrases" "ms"} unit))
      (throw (js/Error. "Use a nonnegative duration in beats, bars, phrases or explicit milliseconds.")))
    {(keyword unit) n}))

(defn command-payload [target operation fields]
  (when-not (#{"instance/pause" "instance/resume" "instance/end" "instance/remove"
               "instance/opacity-set" "instance/opacity-toggle" "instance/envelope-set"} operation)
    (throw (js/Error. "Unsupported performance action.")))
  (cond
    (:instance-id target) [operation (assoc fields :instance-id (wire (:instance-id target)))]
    (map? (:selector target)) ["group/apply" {:selector (:selector target) :action (assoc fields :op operation)}]
    :else (throw (js/Error. "Choose a running instance or category group."))))

(defn- target-instances [db target]
  (if-let [id (:instance-id target)]
    (filterv #(= (wire id) (wire (:id %))) (get-in db [:show :instances]))
    (let [selector (:selector target)]
      (selected-instances (:show db) {:scope (if (empty? selector) "all" "categories")
                                     :categories (:categories selector) :match (wire (:match selector "any"))}))))

(defn- require-targets [db target]
  (when-not (seq (target-instances db target))
    (throw (js/Error. "No running instances match this selection."))))

(defn opacity-value [show draft instances]
  (if (and draft (or (nil? (:revision draft)) (< (:revision show 0) (:revision draft))))
    (:value draft)
    (let [values (set (map #(get-in % [:system :opacity] 1) instances))]
      (when (= 1 (count values)) (first values)))))

(defn- duration-form [duration]
  (if-let [[unit value] (when (map? duration) (first duration))]
    {:value (str value) :unit (wire unit)} {:value "0" :unit "beats"}))

(defn envelope-form [s key stage instances]
  (let [values (set (map #(get-in % [:system stage]) instances))
        duration (when (= 1 (count values)) (first values))]
    (merge (duration-form duration) (get-in s [:envelopes key stage])
           {:current duration :mixed? (> (count values) 1)})))

(defn- fail [db key error]
  {:db (store db (assoc-in (state db) [:errors key] (events/error-message error)))})

(defn- send-action [db target operation fields]
  (require-targets db target)
  (let [s (state db) key (target-key target) serial (inc (:serial s)) token (request-token db)
        [op payload] (command-payload target operation fields)
        request-key [key operation]
        [effect command] (events/command db op payload [:performance/ack token request-key serial])
        next (-> s (assoc :serial serial) (assoc-in [:requests request-key] serial) (update :errors dissoc key))
        next (if (= operation "instance/opacity-set")
               (assoc-in next [:opacity key] {:value (:value fields) :serial serial :target target}) next)]
    {:db (store db next)
     :fx [[effect (assoc command :on-failure [:performance/failed token request-key serial])]]}))

(rf/reg-sub :performance/instance
  (fn [db [_ id]]
    (let [target {:instance-id id} key (target-key target) instances (target-instances db target) s (state db)]
      {:state s :target target :key key :instances instances
       :opacity (opacity-value (:show db) (get-in s [:opacity key]) instances)
       :error (get-in s [:errors key])})))
(rf/reg-sub :performance/group
  (fn [db _]
    (let [s (state db) form (:group s) instances (selected-instances (:show db) form)
          target {:selector (group-selector form)} key (target-key target)]
      {:state s :form form :groups (get-in db [:show :groups]) :instances instances :target target :key key
       :opacity (opacity-value (:show db) (get-in s [:opacity key]) instances)
       :error (get-in s [:errors key])})))
(rf/reg-event-db :performance/group-set
  (fn [db [_ field value]] (store db (assoc-in (state db) [:group field] value))))
(rf/reg-event-db :performance/group-clear
  (fn [db _] (store db (assoc-in (state db) [:group] (:group defaults)))))
(rf/reg-event-db :performance/category-toggle
  (fn [db [_ id]]
    (let [id (wire id)]
      (store db (-> (state db) (assoc-in [:group :scope] "categories")
                    (update-in [:group :categories]
                               #(if (some #{id} %) (vec (remove #{id} %)) (conj (vec %) id))))))))
(rf/reg-event-db :performance/envelope-edit
  (fn [db [_ key stage field value]] (store db (assoc-in (state db) [:envelopes key stage field] value))))
(rf/reg-event-db :performance/envelope-open
  (fn [db [_ key]] (store db (update-in (state db) [:envelopes key :open?] not))))
(rf/reg-event-db :performance/card-toggle
  (fn [db [_ id identity]]
    (let [show (:show db)
          instance (some #(when (= id (:id %)) %) (:instances show))
          key (wire id)
          expanded? (get-in (state db) [:expanded key])]
      (if (and instance (= identity (select-keys show [:runtime-id :epoch])))
        (cond-> (store db (assoc-in (state db) [:expanded key] (not expanded?)))
          (and (not expanded?) (= "running" (:phase instance))) (assoc-in [:ui :selected-instance] id))
        db))))

(rf/reg-sub :performance/card-expanded
  (fn [db [_ id]] (boolean (get-in (state db) [:expanded (wire id)]))))

(rf/reg-event-db :performance/reorder-toggle
  (fn [db _]
    (let [{:keys [open? request]} (:reorder (state db))]
      (if (reorder-pending? (:show db) request) db
          (store db (assoc (state db) :reorder {:open? (not open?)}))))))

(defn- submit-reorder [db s capture op fields]
  (let [serial (inc (:serial s))
        result [:performance/reorder-result (request-token db) serial]
        [effect command] (events/command db op (assoc fields :expected-revision (:revision capture))
                                         (conj result true))
        reorder (-> (:reorder s) (assoc :request {:serial serial}) (dissoc :error :message))]
    {:db (store db (assoc s :serial serial :reorder reorder))
     :fx [[effect (assoc command :on-failure (conj result false))]]}))

(rf/reg-event-fx :performance/reorder-pick
  (fn [{:keys [db]} [_ capture id]]
    (let [show (:show db)
          s (state db)
          {:keys [open? source request]} (:reorder s)
          identity (:identity (mix-capture show (ordered-instances show)))
          source-id (when (= identity (:identity source)) (:id source))
          update-reorder #(store db (assoc s :reorder (assoc % :open? true)))]
      (cond
        (or (not open?) (reorder-pending? show request)) {}
        (not (and (= identity (:identity capture)) (some #{id} (peek identity))))
        {:db (update-reorder {:error "Cue order changed. Choose a cue again."})}
        (= source-id id) {:db (update-reorder {})}
        (nil? source-id) {:db (update-reorder {:source {:id id :identity identity}})}
        :else
        (submit-reorder db s capture "instance/move-after" {:instance-id source-id :after-instance-id id})))))

(rf/reg-event-db :performance/reorder-priority-edit
  (fn [db [_ source value]]
    (let [s (state db)]
      (if (= source (get-in s [:reorder :source]))
        (store db (-> s (assoc-in [:reorder :priority-draft] value) (update :reorder dissoc :error)))
        db))))

(rf/reg-event-fx :performance/reorder-priority-set
  (fn [{:keys [db]} [_ capture id value]]
    (let [show (:show db)
          s (state db)
          {:keys [open? source request]} (:reorder s)
          identity (:identity (mix-capture show (ordered-instances show)))
          fail #(hash-map :db (store db (assoc-in s [:reorder :error] %)))]
      (cond
        (or (not open?) (reorder-pending? show request)) {}
        (not (and id (= identity (:identity capture)) (= id (priority-source-id source identity))))
        (fail "Cue order changed. Choose a cue again.")
        :else
        (try (submit-reorder db (assoc-in s [:reorder :priority-draft] value) capture "instance/set-priority"
                             {:instance-id id :priority (modulation/number-value value "Layer priority")})
             (catch :default error (fail (events/error-message error))))))))

(rf/reg-event-db :performance/reorder-result
  (fn [db [_ token serial success? result]]
    (let [s (state db)]
      (if (and (= token (request-token db)) (= serial (get-in s [:reorder :request :serial])))
        (let [revision (when (and success? (contains? #{"accepted" :accepted} (:status result))) (:revision result))
              reorder (if revision
                        {:open? true :request {:revision revision}
                         :message (if (contains? result :render-priority) "Layer priority updated." "Cue order updated.")}
                        (-> (:reorder s)
                            (dissoc :request)
                            (assoc :error (if success? "Reorder is still queued. Check the displayed order before retrying."
                                                (events/error-message result)))))]
          (store db (assoc s :reorder reorder)))
        db))))

(defn- value-by-id [m k]
  (get m k (get m (keyword (wire k)) (get m (wire k)))))

(defn- parameter-overlay [show instance-id parameter]
  (value-by-id (value-by-id (get-in show [:modulation :overrides]) instance-id) parameter))

(defn xyz-readings [show instance components]
  (let [base (mapv (fn [[k schema]] (or (value-by-id (:params instance) k) (:default schema))) components)
        effective (mapv (fn [[k _] v] (or (:effective-value (sources/reading instance k)) (parameter-overlay show (:id instance) k) v)) components base)]
    {:base base :effective effective}))

(rf/reg-sub :performance/parameter-overlay
  (fn [db [_ id parameter]] (parameter-overlay (:show db) id parameter)))
(rf/reg-event-db :performance/runtime-change
  (fn [db _] (store (update db :ui dissoc :parameter-source :source-requests :block-fixtures)
                    (assoc defaults :generation (inc (:generation (state db))) :group (:group (state db))))))
(rf/reg-event-fx :performance/action
  (fn [{:keys [db]} [_ target operation fields]]
    (try (send-action db target operation (or fields {}))
         (catch :default error (fail db (target-key target) error)))))
(rf/reg-event-fx :performance/envelope-apply
  (fn [{:keys [db]} [_ target stage]]
    (let [key (target-key target)]
      (try
        (when-not (#{:attack :release} stage) (throw (js/Error. "Choose attack or release.")))
        (let [form (envelope-form (state db) key stage (target-instances db target))
              duration (envelope-duration (:value form) (:unit form))]
          (send-action db target "instance/envelope-set" {:envelope {stage duration}}))
        (catch :default error (fail db key error))))))
(rf/reg-event-fx :performance/opacity
  (fn [{:keys [db]} [_ target raw]]
    (let [key (target-key target)]
      (try
        (require-targets db target)
        (let [value (modulation/number-value raw "Opacity") s (state db) existing (get-in s [:opacity key])]
          (when-not (<= 0 value 1) (throw (js/Error. "Opacity must be between zero and one.")))
          {:db (store db (-> s (assoc-in [:opacity key] {:value value :target target :scheduled? true}) (update :errors dissoc key)))
           :dispatch-later (when-not (or (:scheduled? existing) (get-in s [:requests [key "instance/opacity-set"]]))
                             [{:ms 65 :dispatch [:performance/flush-opacity (request-token db) key]}])})
        (catch :default error (fail db key error))))))
(rf/reg-event-fx :performance/flush-opacity
  (fn [{:keys [db]} [_ token key]]
    (if (not= token (request-token db)) {}
      (let [draft (get-in (state db) [:opacity key])]
        (if-not (and (:scheduled? draft) (nil? (get-in (state db) [:requests [key "instance/opacity-set"]]))) {}
          (try (send-action db (:target draft) "instance/opacity-set" {:value (:value draft)})
               (catch :default error
                 (fail (store db (update (state db) :opacity dissoc key)) key error))))))))
(rf/reg-event-fx :performance/ack
  (fn [{:keys [db]} [_ token [key operation :as request-key] serial ack]]
    (if (and (= token (request-token db)) (= serial (get-in (state db) [:requests request-key])))
      {:db (store db (cond-> (update (state db) :requests dissoc request-key)
                       (and (= operation "instance/opacity-set") (= serial (get-in (state db) [:opacity key :serial])))
                       (assoc-in [:opacity key :revision] (:revision ack))))
       :dispatch-later (when (and (= operation "instance/opacity-set") (get-in (state db) [:opacity key :scheduled?]))
                         [{:ms 65 :dispatch [:performance/flush-opacity token key]}])} {})))
(rf/reg-event-fx :performance/failed
  (fn [{:keys [db]} [_ token [key operation :as request-key] serial error]]
    (if (and (= token (request-token db)) (= serial (get-in (state db) [:requests request-key])))
      (cond-> (fail (store db (cond-> (update (state db) :requests dissoc request-key)
                               (and (= operation "instance/opacity-set") (= serial (get-in (state db) [:opacity key :serial])))
                               (update :opacity dissoc key))) key error)
        (and (= operation "instance/opacity-set") (get-in (state db) [:opacity key :scheduled?]))
        (assoc :dispatch-later [{:ms 65 :dispatch [:performance/flush-opacity token key]}])) {})))

(defn- action-button [target operation label options]
  [:button.button (merge {:type "button" :on-click #(rf/dispatch [:performance/action target operation {}])} options) label])

(defn- opacity-control [target value disabled?]
  [:div.performance-opacity
   [:label [:span "Opacity"] [:output (if (some? value) (str (js/Math.round (* 100 value)) "%") "Mixed")]]
   [:input {:type "range" :min 0 :max 1 :step 0.01 :value (or value 1) :disabled disabled?
            :aria-label "Effect opacity" :aria-valuetext (if (some? value) (str (js/Math.round (* 100 value)) " percent") "Mixed opacity values")
            :on-change #(rf/dispatch-sync [:performance/opacity target (.. % -target -value)])}]])

(defn- envelope-stage [s key target stage instances disabled?]
  (let [form (envelope-form s key stage instances)
        title (str/capitalize (name stage))
        edit! #(rf/dispatch-sync [:performance/envelope-edit key stage %1 %2])
        numeric (try (modulation/number-value (:value form) title) (catch :default _ 0))
        maximum (if (= "ms" (:unit form)) 4000 16)]
    [:div.performance-envelope-stage
     [:div.performance-envelope-heading [:strong title]
      [:span (cond (:mixed? form) "Mixed" (:current form) (widgets/duration-label (:current form)) :else "Authored timing")]]
     [:div.performance-duration
      [:input {:type "text" :input-mode "decimal" :value (:value form) :disabled disabled?
               :aria-label (str title " duration") :on-change #(edit! :value (.. % -target -value))}]
      [:select {:value (:unit form) :disabled disabled? :aria-label (str title " unit") :on-change #(edit! :unit (.. % -target -value))}
       [:option {:value "beats"} "Beats"] [:option {:value "bars"} "Bars"] [:option {:value "phrases"} "Phrases"]
       [:option {:value "ms"} "Milliseconds"]]]
     [:input.performance-duration-range {:type "range" :min 0 :max (max maximum numeric) :step (if (= "ms" (:unit form)) 10 0.125)
                                         :value numeric :disabled disabled? :aria-label (str title " duration adjustment")
                                         :on-change #(edit! :value (.. % -target -value))}]
     (when (not= "ms" (:unit form))
       [:div.performance-duration-presets
        (for [[value label] [["0" "0"] ["1/8" "⅛"] ["1/4" "¼"] ["1/2" "½"] ["1" "1"] ["2" "2"]]]
          ^{:key value} [:button {:type "button" :disabled disabled? :aria-label (str title " " label " " (:unit form))
                                  :on-click #(edit! :value value)} label])])
     [:button.button {:type "button" :disabled disabled? :on-click #(rf/dispatch [:performance/envelope-apply target stage])}
      (str "Apply " (str/lower-case title))]]))

(defn- envelope-controls [s key target instances disabled?]
  (let [open? (boolean (get-in s [:envelopes key :open?]))]
    [:div.performance-envelope
     [:button.text-button {:type "button" :aria-expanded open? :on-click #(rf/dispatch [:performance/envelope-open key])}
      (if open? "− Attack / release" "+ Attack / release")]
     (when open?
       [:div.performance-envelope-body
        [:p.small.muted "Set the system envelope without changing authored parameters. Each apply changes only its named stage."]
        [:div.performance-envelope-grid
         [envelope-stage s key target :attack instances disabled?]
         [envelope-stage s key target :release instances disabled?]]])]))

(defn instance-controls [instance & [options]]
  (when (:id instance)
    (let [{:keys [state key target instances opacity error]} @(rf/subscribe [:performance/instance (:id instance)])
          current (first instances) paused? (true? (get-in current [:system :paused?]))
          disabled? (empty? instances) zero? (= 0 opacity) compact? (:compact? options)]
      [:section.performance-instance {:aria-label "Effect system controls" :data-system-instance (wire (:id instance))
                                      :class (when compact? "compact")}
       (when-not compact?
         [:div.performance-heading [:span.section-label "SYSTEM"]
          [:span.performance-state (if paused? "Evolution paused" "Evolving")]])
       [:div.performance-actions
        [action-button target (if paused? "instance/resume" "instance/pause") (if paused? "Resume" "Pause")
         {:disabled disabled? :aria-pressed paused? :title "Pause evolution; manual parameters remain adjustable."}]
        [action-button target "instance/opacity-toggle" (if zero? "Restore" "Zero")
         {:disabled disabled? :aria-pressed zero? :title "Zero contribution; restore the previous opacity."}]
        [action-button target "instance/end" "Release" {:disabled disabled? :title "End through the release envelope."}]
        (when-not compact?
          [action-button target "instance/remove" "Remove" {:disabled disabled? :class "performance-remove" :title "Remove immediately."}])]
       [opacity-control target opacity disabled?]
       (when (and paused? (not compact?)) [:p.performance-hint "Evolution paused · manual values stay adjustable"])
       (when error [:p.performance-error {:role "alert"} error])
       (when-not compact? [envelope-controls state key target instances disabled?])])))

(defn group-panel [selection-view rows]
  (let [{:keys [state form groups instances target key opacity error]} @(rf/subscribe [:performance/group])
        disabled? (empty? instances)
        all-zero? (and (seq instances) (every? #(zero? (get-in % [:system :opacity] 1)) instances))]
    [:section.performance-groups {:role "region" :aria-label "Category performance controls"}
     [:div.performance-category-list {:role "group" :aria-label "Effect categories"}
      [:button.performance-category {:type "button" :aria-pressed (= "all" (:scope form))
                                      :on-click #(rf/dispatch [:performance/group-set :scope (if (= "all" (:scope form)) "categories" "all")])}
       [:strong "All running"]]
      (for [group groups :let [id (wire (:id group)) selected? (and (= "categories" (:scope form)) (some #{id} (:categories form)))]]
        ^{:key id} [:button.performance-category {:type "button" :aria-pressed (boolean selected?)
                                                 :style {"--category-color" (widgets/swatch-css (:color group "#91a4bb"))}
                                                 :on-click #(rf/dispatch [:performance/category-toggle id])}
                    [:strong (widgets/label id)] [:span (count (:instance-ids group))]])]
     (when-not disabled?
       [:div.performance-group-body
        [:div.performance-group-selection
         [:strong (str (count instances) " selected")]
         [:span.performance-target-names {:aria-label "Targeted running instances"}
          (str/join ", " (map #(or (:name %) (wire (:cue-id %))) instances))]
         [:button.text-button {:type "button" :on-click #(rf/dispatch [:performance/group-clear])} "Clear"]]
        (when (and (= "categories" (:scope form)) (> (count (:categories form)) 1))
          [:select.performance-match {:value (:match form) :aria-label "Category matching"
                                       :on-change #(rf/dispatch [:performance/group-set :match (.. % -target -value)])}
           [:option {:value "any"} "Match any category"] [:option {:value "all"} "Match every category"]])
        [:div.performance-actions.performance-group-actions
         (when (:selection selection-view)
           (selection/action-buttons selection-view (:selector target)
                                     (selection/group-flags (map #(get rows (:id %)) instances))))
         [action-button target "instance/pause" "Pause" {}]
         [action-button target "instance/resume" "Resume" {}]
         [:button.button {:type "button"
                          :on-click #(rf/dispatch [:performance/action target (if all-zero? "instance/opacity-toggle" "instance/opacity-set")
                                                   (if all-zero? {} {:value 0})])} (if all-zero? "Restore opacity" "Zero opacity")]
         [action-button target "instance/end" "Release" {}]]
        [:details.performance-group-more
         [:summary "Group opacity, envelope & remove"]
         [opacity-control target opacity false]
         [envelope-controls state key target instances false]
         [action-button target "instance/remove" "Remove now" {:class "performance-remove" :title "Remove selected instances immediately."}]]])
     (when error [:p.performance-error {:role "alert"} error])]))

(defn- layer-controls [selection-view instances]
  (when-let [layers (seq (get-in selection-view [:selection :layers]))]
    (let [names (into {} (map (juxt :id #(or (:name %) (widgets/label (:cue-id %))))) instances)]
      [:details.effect-selection-layers
       [:summary (str "Layers (" (count layers) ")")]
       (for [{:keys [id instance-ids] :as layer} layers
             :let [label (str/join " + " (map names instance-ids))]]
         ^{:key id}
         [:div.effect-selection-layer {:role "group" :aria-label (str "Layer " label) :data-selection-layer (wire id)}
          [:span {:title (str label " · " (wire id))} label]
          (selection/action-buttons selection-view {:layer-id id} layer)])])))

(defn control-id [{:keys [attribute components parameter]}]
  (if components
    (str "group/" (pr-str [(wire attribute) (mapv (comp wire first) components)]))
    (str "param/" (wire parameter))))

(defn- control-title [{:keys [attribute parameter schema]}]
  (or (:name schema) (widgets/label (or parameter attribute))))

(defn xyz-schema [{:keys [attribute components]}]
  (let [schemas (mapv second components)
        bounds (mapv :bounds schemas)
        widths (mapv :dmx-bits schemas)]
    (cond-> {:type :vector :size 3 :semantic/type attribute :component-schemas schemas}
      (and (first bounds) (apply = bounds)) (assoc :bounds (first bounds))
      (and (#{8 16} (first widths)) (apply = widths)) (assoc :dmx-bits (first widths)))))

(defn- xyz-axis-label [axis parameter schema]
  (let [authored (some-> (:name schema) str str/trim not-empty)
        description (some #(when (and % (not= axis (str/upper-case %))) %)
                          [authored (wire parameter)])]
    (if description (str axis " · " description) axis)))

(defn- xyz-editor [show instance {:keys [components] :as control} compact? & [slot-attrs]]
  (let [{:keys [base effective]} (xyz-readings show instance components)
        title (control-title control)
        axes (mapv (fn [[id schema] axis] (assoc schema :axis-label (xyz-axis-label axis id schema)))
                   components ["X" "Y" "Z"])
        update! (fn [index value]
                  (rf/dispatch-sync [:studio/param-set (:id instance) (first (nth components index)) value]))
        ;; Each axis is its own parameter: a pad move sends param/set only for
        ;; the axes whose latest app-db value it changes, so a horizontal
        ;; drag leaves Y alone.
        set! (fn [changes]
               (doseq [[index value] changes]
                 (rf/dispatch-sync [:studio/param-set-if-changed (:id instance) (first (nth components index)) value (get base index)])))
        key! (fn [index schema key]
               (rf/dispatch-sync [:studio/param-key (:id instance) (first (nth components index)) schema key]))
        widget (controls/compound-widget (xyz-schema control))
        visual? (and (not slot-attrs) (not compact?) (some #(sources/driven? instance (first %)) components))]
    [:div.param-row (merge {:data-group (control-id control) :class (when compact? "strip-control")
                            :data-control (name widget)
                            :data-slot (name (controls/slot-size {:type :vector :widget widget}))}
                           slot-attrs)
     [:label title]
     (if (#{:xyz :rings} widget)
       [widgets/compound-control {:title title :widget widget :components (mapv second components)
                                  :labels (mapv :axis-label axes) :values effective :base base
                                  :set! set! :set-component! update! :key! key! :skin @(rf/subscribe [:ui/knob-skin])}]
       [widgets/axes-control title axes base update!])
     (let [param (or (some #(when (sources/driven? instance (first %)) (first %)) components)
                     (some #(when (sources/bound? instance (first %)) (first %)) components)
                     (ffirst components))]
       [sources/trigger (:id instance) param title (sources/reading instance param) components])
     (when visual?
       [sources/axes-visual visualization/studio instance components (xyz-schema control)])]))

(defonce ^:private packings (atom {}))

(defn- remember! [key packed]
  (swap! packings #(assoc (if (< 256 (count %)) {} %) key packed))
  packed)

(defn- exact-pack
  "The remembered exact slot-grid/pack for these spans and column limit, or
  nil when it has not been computed yet."
  [spans columns exact?]
  (get @packings [spans columns exact?]))

(defn- compute-pack! [spans columns exact?]
  (remember! [spans columns exact?] (slot-grid/pack spans columns {:exact-columns? exact?})))

(defn- css-px [style property]
  (let [n (js/parseFloat (.getPropertyValue style property))] (when (js/Number.isFinite n) n)))

(defn- element-window
  "The window that owns `element`: Studio's own, or a popped-out panel's."
  [element]
  (or (some-> element .-ownerDocument .-defaultView) js/window))

(defn slot-columns
  "How many slot columns of the element's own --slot-col and --slot-gap fit
  its width."
  [node]
  (let [style (.getComputedStyle (element-window node) node)
        unit (or (css-px style "--slot-col") 92)
        gap (or (css-px style "--slot-gap") 6)]
    (max 1 (js/Math.floor (/ (+ (.-clientWidth node) gap) (+ unit gap))))))

(def fluid-units
  "Smallest track and row:track proportion of a macro card's bank, which
  stretches its tracks to the card's own width (#795). Tracks grow up to
  `:stretch` times the smallest one, so a bank with few controls grows its
  knobs and pads rather than its margins."
  {"strip" {:min 84 :ratio (/ 144 92) :gap 6 :stretch 1.6}
   "detail" {:min 160 :ratio (/ 180 172) :gap 8 :stretch 1.4}})

(defn fluid-measure
  "Column limit and width of a stretched bank of `surface` units."
  [surface]
  (fn [node]
    (let [{:keys [min gap]} (fluid-units surface)
          width (.-clientWidth node)]
      {:columns (max 1 (js/Math.floor (/ (+ width gap) (+ min gap)))) :width width})))

(defn fluid-stretch
  "How far a stretched bank's tracks may grow over the smallest: further for
  one or two controls, so a small card's knob fills the card instead of
  sitting between wide margins."
  [surface control-count]
  (let [{:keys [stretch]} (fluid-units surface)]
    (case (min 3 (max 1 (or control-count 3)))
      1 (* stretch 1.6)
      2 (* stretch 1.25)
      stretch)))

(defn fluid-style
  "Track and smallest row size for `columns` stretched tracks across
  `width`, each at most the stretch limit (so a lone strip knob stays near
  twice its usual size; the grid centres in the card). The grid's rows then
  share any further height the card gives the bank (a card grown beside a
  taller one), and every widget sizes to the smaller side of its slot, so
  pads stay square."
  [surface columns width & [control-count]]
  (let [{:keys [min ratio gap]} (fluid-units surface)
        stretch (fluid-stretch surface control-count)
        track (js/Math.max min (js/Math.min (* stretch min) (/ (- width (* gap (dec columns))) (max 1 columns))))
        ;; Rows keep the surface's usual stretch, so one or two controls widen
        ;; their slot without making it needlessly tall; extra card height
        ;; still reaches them through the grid's 1fr rows.
        row-track (js/Math.min track (* (:stretch (fluid-units surface)) min))]
    {"--slot-col" (str (js/Math.floor track) "px") "--slot-row" (str (js/Math.floor (* ratio row-track)) "px")}))

(defn overlay-anchor
  "Where an overlay two slots wide and two tall, opened from the card at
  `col`/`row` (`w` columns wide, `h` rows tall), draws: the 2x2 area of the
  grid touching the card that covers the fewest other cards, never the card
  itself, so the card and its toggle stay uncovered. Areas inside the grid
  win over areas that hang below or above it (over other content); among
  those, the fewest covered cards, so a free area is used when there is one;
  ties prefer the right, then the left, then below, then above.
  Returns {:style {css offsets} :covers #{indices of covered cards}}."
  [columns rows cells {:keys [index col row w h]}]
  (let [owner (into {} (for [{i :index c :col r :row cw :w ch :h} cells dr (range ch) dc (range cw)]
                         [[(+ r dr) (+ c dc)] i]))
        candidates (concat (for [dy [0 -1]] [:right (+ col w) (+ row dy)])
                           (for [dy [0 -1]] [:left (- col 2) (+ row dy)])
                           (for [dx [0 -1]] [:below (+ col dx) (+ row h)])
                           (for [dx [0 -1]] [:above (+ col dx) (- row 2)]))
        scored (for [[side c r] candidates
                     :when (and (<= 0 c) (<= (+ c 2) (max 2 columns)) (<= 0 r))
                     :let [area (for [dr (range 2) dc (range 2)] [(+ r dr) (+ c dc)])
                           covers (disj (set (keep owner area)) nil)]
                     :when (not (contains? covers index))]
                 {:side side :col c :row r :covers covers
                  :rank [(if (<= (+ r 2) rows) 0 1) (count covers)
                         ({:right 0 :left 1 :below 2 :above 3} side)]})
        {c :col r :row covers :covers} (first (sort-by :rank scored))
        step (fn [n unit] (str "calc(" n " * (var(" unit ") + var(--slot-gap)) - 1px)"))]
    (if c
      {:style {"--overlay-left" (step (- c col) "--slot-col") "--overlay-top" (step (- r row) "--slot-row")}
       :covers covers}
      {:style {} :covers #{}})))

(defn packed-grid
  "A grid whose cards `slot-grid/pack` places. `measure` turns the element
  into its column limit (a ResizeObserver from the element's own window,
  owned by this component, re-measures it, also in a popped-out panel),
  `spans` turns that limit into each card's [columns rows], and `render`
  draws card `i` with its grid-area style and the keys of the cards its
  overlay would cover. With `exact?` the grid keeps the measured column
  count (fractional board tracks); otherwise it narrows to its content
  (fixed-width slot tracks). Cards are drawn in reading order, so DOM and
  tab order follow the screen; they are keyed, so moving one never remounts
  it."
  [{:keys [attrs measure spans render keys exact? fluid in-order? grow-down? control-count]}]
  (r/with-let [limit (r/atom nil)
               ;; Bumped when an exact layout computed while idle arrives.
               upgraded (r/atom 0)
               ;; The idle job in flight, [key handle window], cancelled when a
               ;; newer width supersedes it or the grid unmounts.
               job (atom nil)
               cancel-job! (fn []
                             (when-let [[_ handle win] @job]
                               (if (.-cancelIdleCallback win) (.cancelIdleCallback win handle) (.clearTimeout win handle)))
                             (reset! job nil))
               node (atom nil)
               observer (atom nil)
               resized (atom nil)
               remeasure! #(when-let [n @node] (reset! limit (measure n)))
               detach! (fn []
                         (some-> @observer .disconnect)
                         (reset! observer nil)
                         (when-let [[win f] @resized] (.removeEventListener win "resize" f))
                         (reset! resized nil))
               attach! (fn [element]
                         (detach!)
                         (reset! node element)
                         (when element
                           (reset! limit (measure element))
                           (let [win (element-window element)]
                             ;; The window's own resize event backs up the
                             ;; observer, e.g. for a popped-out panel.
                             (.addEventListener win "resize" remeasure!)
                             (reset! resized [win remeasure!])
                             (when-let [Observer (.-ResizeObserver win)]
                               (let [o (Observer. remeasure!)]
                                 (.observe o element)
                                 (reset! observer o))))))]
    (let [reading @limit
          measured (if (map? reading) (:columns reading) reading)
          span-list (spans (or measured 4))
          _ @upgraded
          ;; Until the width is measured, and while an exact layout is being
          ;; computed, draw first fit in authored order at once; the exact
          ;; result replaces it when the browser is idle.
          ;; An arranged board places in stored order without searching.
          exact (cond in-order? (slot-grid/pack span-list (or measured 4) {:exact-columns? exact? :in-order? true})
                      measured (exact-pack span-list measured exact?))
          packed (or exact (slot-grid/pack span-list (or measured 4) {:exact-columns? exact? :quick? true}))
          {:keys [columns rows cells]} (if grow-down? (slot-grid/grow-down packed) packed)
          wanted [span-list measured]]
      (when (and measured (not exact) (not= wanted (first @job)))
        (cancel-job!)
        (let [win (element-window @node)
              run (fn [& _]
                    (reset! job nil)
                    (compute-pack! span-list measured exact?)
                    (swap! upgraded inc))
              handle (if-let [idle (.-requestIdleCallback win)]
                       (.call idle win run #js {:timeout 200})
                       (.setTimeout win run 0))]
          (reset! job [wanted handle win])))
      (into [:div (merge attrs {:ref attach! :data-columns columns :data-rows rows
                                :data-packing (if exact "final" "quick")
                                :data-slot-scale (when fluid "fluid")
                                :style (merge (:style attrs) {"--slot-columns" columns "--slot-rows" (max 1 rows)}
                                              (when (and fluid (map? reading)) (fluid-style fluid columns (:width reading) control-count)))})]
            (for [{:keys [index col row w h] :as cell} cells
                  :let [{overlay :style covers :covers} (overlay-anchor columns rows cells cell)]]
              ;; `render` returns hiccup: called as a function, not mounted as
              ;; a component, so a parent re-render never remounts a card.
              (with-meta
                (render index (merge {:grid-column (str (inc col) " / span " w) :grid-row (str (inc row) " / span " h)}
                                     overlay)
                        (mapv #(nth keys %) (sort covers)))
                {:key (nth keys index)}))))
    (finally (cancel-job!) (detach!))))

(defn control-slot
  "The standard slot of one bank control: its shared description's :slot,
  or the compound widget of a grouped X/Y/Z."
  [{:keys [parameter schema components] :as control} catalog]
  (if components
    (controls/slot-size {:type :vector :widget (controls/compound-widget (xyz-schema control))})
    (:slot (controls/control parameter schema catalog))))

(defn- parameter-bank
  "One effect's parameter cards on the shared slot grid (#795). Every card
  fills a standard slot of one column and row unit; `slot-grid/pack` places
  them with the fewest empty cells, in authored order where it can, on as
  many columns as fit and the content fills."
  [show instance controls compact? parameter-control & [fluid?]]
  (let [catalog @(rf/subscribe [:waveform/catalog])
        spans (mapv #(controls/slot-span (control-slot % catalog)) controls)
        surface (if compact? "strip" "detail")]
    [packed-grid
     {:attrs {:class (str "live-effect-parameters " (if compact? "control-strip" "control-detail"))
              :data-slot-grid surface
              :role "group" :aria-label (if compact? "Parameter strip" "Detailed parameter controls")}
      ;; A macro card's bank stretches its tracks to the card (#795).
      :measure (if fluid? (fluid-measure surface) slot-columns)
      :fluid (when fluid? surface)
      :control-count (count controls)
      :spans (constantly spans)
      :keys (mapv control-id controls)
      :render (fn [index style covers]
                (let [{:keys [parameter schema components] :as control} (nth controls index)
                      attrs {:style style :data-overlay-covers (str/join " " covers)}]
                  (if components
                    [xyz-editor show instance control compact? attrs]
                    [parameter-control instance parameter schema nil
                     {:compact? compact? :strip? compact? :slot-grid? true :slot-attrs attrs}])))}]))

(defn driven-controls [instance controls]
  (filter (fn [{:keys [parameter components]}]
            (if components (some #(sources/driven? instance (first %)) components) (sources/driven? instance parameter)))
          controls))

(defn- detail-plots
  "Plots of the driven parameters, below the Detail grid. A plot appearing
  when a source starts never moves a card."
  [instance controls]
  (when-let [driven (seq (driven-controls instance controls))]
    [:div.detail-plots
     (for [{:keys [parameter schema components] :as control} driven]
       ^{:key (control-id control)}
       [:figure.detail-plot {:data-plot (if components (control-id control) (wire parameter))}
        [:figcaption (control-title control)]
        ;; The view and beat window live in app-db, so a plot that unmounts
        ;; while its source briefly stops driving (after Resume LFO, say)
        ;; returns as the operator left it.
        (let [options {:view-key [(wire (:id instance)) (control-id control)]}]
          (if components
            [sources/axes-visual visualization/studio instance components (xyz-schema control) options]
            [sources/result-visual visualization/studio instance parameter schema options]))])]))

(defn- viewable? [{:keys [schema components]}]
  (or (seq components)
      (#{:integer :number :range :vector :color} (widgets/parameter-kind schema))))

(defn resolve-monitor [controls requested preferred]
  (let [choices (filterv viewable? controls)
        specific (fn [id] (some #(when (= (control-id %) id) %) choices))
        preferred-id (wire preferred)
        parameter (some #(when (and (:parameter %) (= (wire (:parameter %)) preferred-id)) %) choices)
        group (some #(when (and (:components %) (= (wire (:attribute %)) preferred-id)) %) choices)]
    {:choices choices :selected (or (specific requested) parameter group (first choices))}))

(rf/reg-event-db :performance/monitor-select
  (fn [db [_ id parameter]] (store db (assoc-in (state db) [:monitor id] parameter))))

(defn- strip-monitor [instance {:keys [choices selected]}]
  (when selected
    (let [id (wire (:id instance))
          schema (if (:components selected) (xyz-schema selected) (:schema selected))
          title (control-title selected)]
      [:div.strip-monitor
       [:div.strip-monitor-bar
        (if (next choices)
          [:select {:value (control-id selected) :aria-label "Monitored parameter"
                    :on-change #(rf/dispatch [:performance/monitor-select id (.. % -target -value)])}
           (for [control choices]
             ^{:key (control-id control)} [:option {:value (control-id control)} (control-title control)])]
          [:strong title])
        [:span (case (visualization/monitor-kind schema) :spatial "3D" :color "Colour" "4 beats")]]
       ^{:key (control-id selected)}
       (if-let [axes (:components selected)] [sources/axes-visual visualization/monitor instance axes schema]
           [sources/result-visual visualization/monitor instance (:parameter selected) schema])])))

(defn- resolve-controls [definition cue-layout]
  (let [{:keys [parameters monitor]} (layout/resolve-layout definition {:ui/layout cue-layout})]
    {:controls (inspect/xyz-controls definition parameters) :monitor monitor}))

(defn- reorder-button [id title {:keys [capture source-id ready?]}]
  (let [source? (= id source-id)
        [action label] (cond source? ["Cancel move" "Cancel"]
                             source-id ["Place after" "Place after"]
                             :else ["Move" "Move"])]
    [:button.button.live-reorder-action
     {:type "button" :disabled (not ready?)
      :aria-label (str action " " title " launch " id)
      :on-click #(rf/dispatch [:performance/reorder-pick capture id])}
     label]))

(defn- instance-cue [show instance]
  (some #(when (= (wire (:id %)) (wire (:cue-id instance))) %) (:cues show)))

(defn- instance-title [cue instance]
  (or (:name cue) (:name instance) (widgets/label (:cue-id instance))))

(defn- layer-priority-source
  "Attach or edit the waveform that drives this launch's layer priority."
  [instance {:keys [param schema]}]
  (let [reading (sources/reading instance param)
        title (:name schema "Layer priority")]
    [:div.param-row.layer-priority-source {:data-param (events/wire-id param)}
     [:label title]
     [:span.live-effect-phase
      (if-let [tier (modulated-priority instance {:param param})]
        (str (pr-str tier) " · set " (pr-str (render-priority instance)))
        (pr-str (render-priority instance)))]
     (when (some #(= :manual-priority (keyword (:hold-reason %))) (:sources reading))
       [:span.live-selection-state "Held after a manual priority change"])
     [sources/trigger (:id instance) param title reading]]))

(defn- layer-priority-form [capture source instance title draft]
  (let [value (or draft (pr-str (render-priority instance)))]
    [:form.live-priority-form
     {:on-submit (fn [event]
                   (.preventDefault event)
                   (let [fields (js/FormData. (.-currentTarget event))]
                     (rf/dispatch [:performance/reorder-priority-set capture (:id source) (.get fields "priority")])))}
     [:label (str "Layer priority · " title)
      [:input {:type "text" :name "priority" :input-mode "decimal" :value value
               :on-change #(rf/dispatch [:performance/reorder-priority-edit source (.. % -target -value)])}]]
     [:button.button {:type "submit"} "Set priority"]]))

(defn macro-cards
  "Running macros with their member cards on the board (#795), in each
  macro's card order: [{:macro definition :cards [{:slot-id :member :size
  :instance :position :total}]}]. A member instance carries the macro run's
  layer ID and its slot index; members that are not running have no card.
  :position and :total count the macro's whole arrangement, the slots the
  move command counts, so ‹ › stop where a move would. `compositions` are
  the typed composition projections."
  [compositions instances]
  (vec (for [{:keys [layer-id member-slots mode running?] :as macro} compositions
             :when (and running? layer-id (= "macro" (some-> mode name)))
             :let [members (into {} (keep (fn [instance]
                                            (let [{:keys [id member]} (get-in instance [:source :native-layer])]
                                              (when (= layer-id id) [member instance]))))
                                 instances)
                   arranged (mapv (comp card-layout/slot-key :slot-id) (card-layout/arrange (:slots macro) (:cards macro)))
                   position #(first (keep-indexed (fn [i k] (when (= k (card-layout/slot-key %)) i)) arranged))
                   cards (keep (fn [card]
                                 (when-let [instance (members (:member card))]
                                   (assoc card :instance instance :position (position (:slot-id card)) :total (count arranged))))
                               (card-layout/arrange (mapv #(hash-map :id %) member-slots) (:cards macro)))]
             :when (seq cards)]
         {:macro macro :cards (vec cards)})))

(def small-cells
  "How many slot cells a small macro card shows: its leading controls in
  layout order, so the macro's author chooses them with :ui/order."
  4)

(defn small-controls
  "The controls a small card shows: in layout order, every control whose
  slot still fits in the `small-cells` left, skipping larger ones, and at
  least the first."
  [controls catalog]
  (let [picked (:picked (reduce (fn [{:keys [left] :as acc} control]
                                  (let [area (apply * (controls/slot-span (control-slot control catalog)))]
                                    (if (<= area left)
                                      (-> acc (update :picked conj control) (update :left - area))
                                      acc)))
                                {:picked [] :left small-cells} controls))]
    (if (seq picked) picked (vec (take 1 controls)))))

(defn macro-columns
  "Board tracks for macro cards: six on a board at least 960 px wide, so a
  small card is a third of the row, a medium half and a large all of it; two
  from 640 px, where small and medium are half; one below that, where every
  size takes the full width and differs by its content."
  [node]
  (let [width (.-clientWidth node)]
    (cond (>= width 960) 6 (>= width 640) 2 :else 1)))

(defn member-control-count
  "How many controls a macro member's card shows, counted as the size picker
  counts them (grouped X/Y/Z axes are one control)."
  [show instance]
  (count (:controls (resolve-controls (card-definition show instance) (:ui/layout (instance-cue show instance))))))

(defn macro-span
  "[columns rows] of a macro card of `size` on `columns` board tracks. Its
  height in board rows follows its size (small 1, medium 2, large 3; a
  single-control member's large 2), so the packer can stand short cards
  beside or under tall ones. A small card keeps two of six tracks: its
  heading needs that width for the title, state and Solo/Mute targets."
  ([columns size] (macro-span columns size 2))
  ([columns size control-count]
   [(case columns
      6 (case size :large 6 :medium 3 2)
      2 (if (= :large size) 2 1)
      1)
    (case size :small 1 :medium 2 (if (<= control-count 1) 2 3))]))

(defn member-sizes
  "The sizes a macro member offers: small and large for a single control,
  where medium would show the same control, else small, medium and large."
  [control-count]
  (if (<= control-count 1) [:small :large] card-layout/sizes))

(rf/reg-sub :performance/macro-arrange
  (fn [db [_ id]] (boolean (get-in db [:ui :macro-arrange id]))))

(defn- typed-macro [db macro-id]
  (some #(when (= macro-id (:id %)) %) (get-in db [:server-projection :view :compositions])))

(defn- send-macro-cards [db macro-id change]
  (when (typed-macro db macro-id)
    {:studio/http {:path "/api/composition/cards" :typed? true :raw-response? true
                   :body (assoc change :id macro-id)
                   :on-failure [:studio/failure]}}))

;; The same validated :composition-cards command the REPL submits. A resize or
;; move applies to the arrangement the show holds when it arrives.
(rf/reg-event-fx :performance/macro-card-size
  (fn [{:keys [db]} [_ macro-id slot-id size]]
    (send-macro-cards db macro-id {:resize {:slot-id slot-id :size size}})))

(rf/reg-event-fx :performance/macro-card-move
  (fn [{:keys [db]} [_ macro-id slot-id offset]]
    (send-macro-cards db macro-id {:move {:slot-id slot-id :offset offset}})))

(defn- card-size-picker [{:keys [macro-id slot-id size position total]} title control-count]
  [:div.card-size-picker {:role "group" :aria-label (str title " card")}
   [:span "Card size"]
   (for [option (member-sizes control-count)
         :let [label (str (str/capitalize (name option)) " card")]]
     ^{:key option}
     [:button {:type "button" :aria-pressed (= option size) :title label :aria-label (str label " for " title)
               :on-click #(rf/dispatch [:performance/macro-card-size macro-id slot-id option])}
      (card-layout/size-labels option)])
   [:button {:type "button" :disabled (not (pos? position)) :aria-label (str "Move " title " card earlier")
             :on-click #(rf/dispatch [:performance/macro-card-move macro-id slot-id -1])} "‹"]
   [:button {:type "button" :disabled (not (< position (dec total))) :aria-label (str "Move " title " card later")
             :on-click #(rf/dispatch [:performance/macro-card-move macro-id slot-id 1])} "›"]])

(defn- instance-card [show instance parameter-control
                      {:keys [duplicate? reorder? source-id selection-view selection-row macro-card] :as options}]
  (let [id (wire (:id instance))
        ;; A macro member's size chooses its content (#795): small shows its
        ;; leading controls, medium the whole strip and large the Detail bank.
        size (:size macro-card)
        expanded? (if macro-card (= :large size) @(rf/subscribe [:performance/card-expanded id]))
        definition (card-definition show instance)
        faults @(rf/subscribe [:inspect/faults])
        faulted? (seq (inspect/faults-for faults instance))
        cue (instance-cue show instance)
        {:keys [controls monitor]} @(r/track resolve-controls definition (:ui/layout cue))
        shown (if (= :small size) (small-controls controls @(rf/subscribe [:waveform/catalog])) controls)
        title (instance-title cue instance)
        paused? (get-in instance [:system :paused?])
        {:keys [state key target instances]} @(rf/subscribe [:performance/instance id])
        editing @(rf/subscribe [::sources/selection id])
        priority (sources/priority-destination show)
        scope (resolve-monitor (driven-controls instance controls) (get-in state [:monitor id]) monitor)
        on-surface @(rf/subscribe [:surfaces/on-surface])
        strip (get (:instances on-surface) id)
        ;; A cue grid's committed selection, from a Push pad, the REPL or Follow selection (#545).
        grid-selection (get (:selected on-surface) id)]
    [:article.live-effect-card {:data-live-instance id :aria-label (str title " · " id)
                                :data-reorder-source (= id source-id)
                                :data-density (if expanded? "detail" "strip")
                                ;; Outlined while a surface's session box shows its cue strip (#705).
                                :data-on-surface (boolean strip)
                                :data-wide (boolean (or expanded? (> (count controls) 3)))
                                ;; Only a macro member chooses its card size (#795).
                                :data-card-size (some-> (:size macro-card) name)
                                :class (str (when faulted? "has-fault ") (when paused? "paused ") (when (= "releasing" (:phase instance)) "releasing"))
                                :style {"--cue-color" (widgets/color-css (:swatch cue))}}
     [:header.live-effect-header
      [:div.live-effect-title
       [:h3 title]
       [:div.live-effect-meta
        [:span.live-effect-phase (cond faulted? "Fault" paused? "Paused" :else (widgets/label (:phase instance)))]
        (when strip [:span.surface-strip-badge {:title "Shown on this control surface strip"} strip])
        (when grid-selection
          [:span.surface-strip-badge {:data-surface-selected grid-selection
                                      :title "The selected launch on this control surface"}
           (str "Selected on " grid-selection)])
        (when-let [label (case (:suppressed-by selection-row) :mute "Muted" :solo "Excluded by Solo" nil)]
          [:span.live-selection-state {:data-suppressed-by (name (:suppressed-by selection-row))} label])
        (when duplicate? [:span.live-effect-phase (str "Launch " id)])
        (if-let [tier (modulated-priority instance priority)]
          [:span.live-effect-phase {:data-modulated-priority true}
           (str "Priority " (pr-str tier) " · set " (pr-str (render-priority instance)))]
          (when reorder? [:span.live-effect-phase (str "Priority " (pr-str (render-priority instance)))]))
        [widgets/category-tags (or (:categories instance) (:categories definition)) (:category-colors show)]]]
      [:div.live-effect-header-actions
       (when selection-row
         (selection/action-buttons selection-view {:instance-ids [(:id selection-row)]}
                                   (selection/instance-flags selection-row)))
       (cond
         reorder? (when (= "running" (:phase instance)) [reorder-button id title options])
         ;; The large size is a macro member's Detail.
         macro-card nil
         :else
         [:button.live-effect-expand {:type "button" :aria-expanded expanded?
                                      :aria-label (str (if expanded? "Collapse " "Expand ") title " controls"
                                                       (when duplicate? (str " for launch " id)))
                                      :on-click #(do (rf/dispatch [:performance/card-toggle id (select-keys show [:runtime-id :epoch])])
                                                     (rf/dispatch [:surfaces/follow [:instance id]]))}
          (if expanded? "Compact" "Detail")])]]
     (when (:arranging? macro-card) [card-size-picker macro-card title (count controls)])
     (when (seq shown)
       [:div.live-effect-body
        ;; One or two controls in a macro card use their Detail form (knob,
        ;; slider and exact entry): the card's room holds useful controls
        ;; rather than one enlarged knob.
        (let [detail? (or expanded? (and macro-card (<= (count shown) 2)))]
          ^{:key (if detail? "detail" "strip")} [parameter-bank show instance shown (not detail?) parameter-control (some? macro-card)])
        (when (< (count shown) (count controls))
          [:p.live-effect-more (str (- (count controls) (count shown)) " more in the medium and large sizes")])
        (if expanded?
          [detail-plots instance controls]
          (when (and (not= :small size) (:selected scope)) [strip-monitor instance scope]))])
     (when-let [schema (sources/destination-schema {:definition definition} priority editing)]
       (let [group (some #(when (some (fn [[p _]] (= p editing)) (:components %)) (:components %)) controls)]
         ^{:key [id (or (ffirst group) editing)]} [sources/editor instance editing schema group]))
     (when (and priority (= "running" (name (:phase instance)))
                (or (and expanded? (not macro-card)) (sources/bound? instance (:param priority))))
       [layer-priority-source instance priority])
     [instance-controls instance {:compact? true}]
     (when expanded?
       (let [advanced [:div.live-effect-advanced
                       (when (and macro-card priority (= "running" (name (:phase instance))))
                         [layer-priority-source instance priority])
                       (when (and cue (contains? #{:running "running"} (:phase instance)) (seq (:params definition)))
                         [variations/panel instance])
                       [inspect/panel show instance faults]
                       (when (:cue-id instance) [spatial/cue-target-picker (:cue-id instance)])
                       [envelope-controls state key target instances (empty? instances)]
                       [action-button target "instance/remove" "Remove immediately" {:class "performance-remove"}]
                       [:span.live-effect-identity id]]]
         ;; A large macro card is a large widget: its controls and plots. The
         ;; inspector waits behind a disclosure.
         (if macro-card
           [:details.live-effect-inspector [:summary "Inspector"] advanced]
           advanced)))]))

(defn board [db parameter-control]
  (let [show (:show db)
        committed (ordered-instances show)
        instances (effective-instances show)
        selection-view @(rf/subscribe [:effect-selection/view])
        rows (selection/by-instance (:selection selection-view))
        duplicate-counts (frequencies (map :cue-id instances))
        capture (mix-capture show committed)
        running-ids (peek (:identity capture))
        ready? (< 1 (count running-ids))
        {:keys [open? source request error message priority-draft]} (:reorder (state db))
        pending? (reorder-pending? show request)
        source-id (when (= (:identity capture) (:identity source)) (:id source))
        priority-id (priority-source-id source (:identity capture))
        selected (some #(when (= priority-id (:id %)) %) instances)]
    [:section.live-board {:aria-label "Running effects" :data-skin (name @(rf/subscribe [:ui/knob-skin]))}
     [:div.live-board-heading
      [:h2 "In the mix"] [:span#active-count.count (str (count instances) " active")]
      [:div.live-board-actions
       (when (:selection selection-view) [selection/toolbar selection-view])
       (let [skin @(rf/subscribe [:ui/knob-skin])]
         [:label.knob-skin-choice [:span "Widgets"]
          [:select {:aria-label "Widget style" :value (name skin)
                    :on-change #(rf/dispatch [:studio/set-knob-skin (.. % -target -value)])}
           (for [{:keys [id label]} control-geometry/knob-skins]
             ^{:key id} [:option {:value (name id)} label])]])
       (when (or (seq running-ids) open?)
         [:button.button.live-reorder-toggle {:type "button" :disabled (boolean pending?)
                                             :on-click #(rf/dispatch [:performance/reorder-toggle])}
          (if open? "Done" "Reorder")])]]
     (when-let [error (get-in selection-view [:interaction :error])]
       [:p.performance-error {:role "alert"} error])
     (when open?
       (if error
         [:p.live-reorder-status {:role "alert"} error]
         [:p.live-reorder-status {:role "status"}
          (cond pending? "Applying cue order…"
                (and source-id ready?) "Choose where to place the selected launch or set its priority."
                selected "Set the priority of the selected launch."
                (empty? running-ids) "Launch a cue to change its layer priority."
                source "Cue order changed. Choose a cue again."
                message message
                :else "Choose a cue to move.")]))
     (when (and open? selected (not pending?))
       [layer-priority-form capture source selected (instance-title (instance-cue show selected) selected) priority-draft])
     [inspect/unassigned-faults show @(rf/subscribe [:inspect/faults])]
     (if (seq instances)
       (let [macros (macro-cards (get-in db [:server-projection :view :compositions]) instances)
             in-macro (into #{} (comp (mapcat :cards) (map (comp :id :instance))) macros)
             card-options (fn [instance]
                            {:duplicate? (< 1 (get duplicate-counts (:cue-id instance) 0)) :reorder? open?
                             :capture capture :source-id source-id :ready? (not pending?)
                             :selection-view selection-view :selection-row (get rows (:id instance))})]
         [:<> [group-panel selection-view rows]
          [layer-controls selection-view instances]
          [:div.live-effect-grid
           (for [instance instances :when (not (in-macro (:id instance)))]
             ^{:key (wire (:id instance))}
             [instance-card show instance parameter-control (card-options instance)])]
          ;; A running macro is a custom setup: its member cards keep the sizes
          ;; and order saved with it. Every other card takes a standard size.
          (for [{:keys [macro cards]} macros
                :let [title (or (:name macro) (widgets/label (wire (:id macro))))
                      macro-key (wire (:id macro))
                      arranging? @(rf/subscribe [:performance/macro-arrange macro-key])]]
            ^{:key macro-key}
            [:section.live-macro {:aria-label (str "Macro " title) :data-macro macro-key}
             [:div.live-macro-heading [:h3 title] [:span "Macro"]
              [:button.button.live-macro-arrange
               {:type "button" :aria-pressed arranging? :aria-label (str (if arranging? "Finish arranging " "Arrange ") title)
                :on-click #(rf/dispatch [:studio/ui-set [:macro-arrange macro-key] (not arranging?)])}
               (if arranging? "Done" "Arrange")]]
             [packed-grid
              {:attrs {:class "live-effect-grid live-macro-grid"}
               ;; Stored order is reading order, so Arrange always moves a
               ;; card the operator sees; a short card grows down into the
               ;; band it would leave empty beside a taller one.
               :measure macro-columns :exact? true :in-order? true :grow-down? true
               :spans (fn [columns]
                        (mapv #(macro-span columns (:size %) (member-control-count show (:instance %))) cards))
               :keys (mapv (comp wire :id :instance) cards)
               :render (fn [index style _]
                         (let [{:keys [instance] :as card} (nth cards index)]
                           [:div.live-macro-slot {:style (select-keys style [:grid-column :grid-row])}
                            [instance-card show instance parameter-control
                             (assoc (card-options instance)
                                    :macro-card (assoc (select-keys card [:slot-id :size :position :total])
                                                       :macro-id (:id macro) :arranging? arranging?))]]))}]])])
       [:div.live-board-empty [:span "◌"] [:strong "Your running effects appear here"]
        [:p "Launch cues to mix their parameters together."]])]))
