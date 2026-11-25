(ns tolglow.studio.performance
  "System controls over the shared command boundary. Snapshot groups provide
  category membership; this UI does not infer categories or run effect clocks."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.authoring.parameter-layout :as layout]
            [tolglow.studio.events :as events]
            [tolglow.studio.cue-variations :as variations]
            [tolglow.studio.effect-selection :as selection]
            [tolglow.studio.modulation :as modulation]
            [tolglow.studio.effect-inspector :as inspect]
            [tolglow.studio.spatial-picker :as spatial]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.parameter-source :as sources]
            [tolglow.studio.param-visualization :as visualization]))

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

(defn- xyz-editor [show instance {:keys [components] :as control} compact?]
  (let [{:keys [base]} (xyz-readings show instance components)
        title (control-title control)
        axes (mapv (fn [[id schema] axis] (assoc schema :axis-label (xyz-axis-label axis id schema)))
                   components ["X" "Y" "Z"])
        update! (fn [index value]
                  (rf/dispatch-sync [:studio/param-set (:id instance) (first (nth components index)) value]))]
    [:div.param-row {:data-group (control-id control) :class (when compact? "strip-control")}
     [:label title]
     [widgets/axes-control title axes base update!]
     (let [param (or (some #(when (sources/driven? instance (first %)) (first %)) components)
                     (some #(when (sources/bound? instance (first %)) (first %)) components)
                     (ffirst components))]
       [sources/trigger (:id instance) param title (sources/reading instance param) components])
     (when (and (not compact?) (some #(sources/driven? instance (first %)) components))
       [sources/axes-visual visualization/studio instance components (xyz-schema control)])]))

(defn- parameter-bank [show instance controls compact? parameter-control]
  [:div.live-effect-parameters {:class (if compact? "control-strip" "control-detail")
                                :data-size (if (<= (count controls) 2) "small" "bank")
                                :style {"--control-count" (min 8 (count controls))}
                                :role "group" :aria-label (if compact? "Parameter strip" "Detailed parameter controls")}
   (for [{:keys [parameter schema components] :as control} controls]
     ^{:key (control-id control)}
     (if components
       [xyz-editor show instance control compact?]
       [parameter-control instance parameter schema nil {:compact? compact? :strip? compact?}]))])

(defn- viewable? [{:keys [schema components]}]
  (or (seq components)
      (#{:integer :number :range :vector :color} (widgets/parameter-kind schema))))

(defn driven-controls [instance controls]
  (filter (fn [{:keys [parameter components]}]
            (if components (some #(sources/driven? instance (first %)) components) (sources/driven? instance parameter)))
          controls))

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
        [:span (if (visualization/spatial-kind schema) "3D" "4 beats")]]
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

(defn- instance-card [show instance parameter-control
                      {:keys [duplicate? reorder? source-id selection-view selection-row] :as options}]
  (let [id (wire (:id instance)) expanded? @(rf/subscribe [:performance/card-expanded id])
        definition (card-definition show instance)
        faults @(rf/subscribe [:inspect/faults])
        faulted? (seq (inspect/faults-for faults instance))
        cue (instance-cue show instance)
        {:keys [controls monitor]} @(r/track resolve-controls definition (:ui/layout cue))
        title (instance-title cue instance)
        paused? (get-in instance [:system :paused?])
        {:keys [state key target instances]} @(rf/subscribe [:performance/instance id])
        editing @(rf/subscribe [::sources/selection id])
        priority (sources/priority-destination show)
        scope (resolve-monitor (driven-controls instance controls) (get-in state [:monitor id]) monitor)]
    [:article.live-effect-card {:data-live-instance id :aria-label (str title " · " id)
                                :data-reorder-source (= id source-id)
                                :data-density (if expanded? "detail" "strip")
                                :data-wide (boolean (or expanded? (> (count controls) 3)))
                                :class (str (when faulted? "has-fault ") (when paused? "paused ") (when (= "releasing" (:phase instance)) "releasing"))
                                :style {"--cue-color" (widgets/color-css (:swatch cue))}}
     [:header.live-effect-header
      [:div.live-effect-title
       [:h3 title]
       [:div.live-effect-meta
        [:span.live-effect-phase (cond faulted? "Fault" paused? "Paused" :else (widgets/label (:phase instance)))]
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
       (if reorder?
         (when (= "running" (:phase instance)) [reorder-button id title options])
         [:button.live-effect-expand {:type "button" :aria-expanded expanded?
                                      :aria-label (str (if expanded? "Collapse " "Expand ") title " controls"
                                                       (when duplicate? (str " for launch " id)))
                                      :on-click #(rf/dispatch [:performance/card-toggle id (select-keys show [:runtime-id :epoch])])}
          (if expanded? "Compact" "Detail")])]]
     (when (seq controls)
       [:div.live-effect-body
        [parameter-bank show instance controls (not expanded?) parameter-control]
        (when (and (not expanded?) (:selected scope)) [strip-monitor instance scope])])
     (when-let [schema (sources/destination-schema {:definition definition} priority editing)]
       (let [group (some #(when (some (fn [[p _]] (= p editing)) (:components %)) (:components %)) controls)]
         ^{:key [id (or (ffirst group) editing)]} [sources/editor instance editing schema group]))
     (when (and priority (= "running" (name (:phase instance)))
                (or expanded? (sources/bound? instance (:param priority))))
       [layer-priority-source instance priority])
     [instance-controls instance {:compact? true}]
     (when expanded?
       [:div.live-effect-advanced
        (when (and cue (contains? #{:running "running"} (:phase instance)) (seq (:params definition)))
          [variations/panel instance])
        [inspect/panel show instance faults]
        (when (:cue-id instance) [spatial/cue-target-picker (:cue-id instance)])
        [envelope-controls state key target instances (empty? instances)]
        [action-button target "instance/remove" "Remove immediately" {:class "performance-remove"}]
        [:span.live-effect-identity id]])]))

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
    [:section.live-board {:aria-label "Running effects"}
     [:div.live-board-heading
      [:h2 "In the mix"] [:span#active-count.count (str (count instances) " active")]
      [:div.live-board-actions
       (when (:selection selection-view) [selection/toolbar selection-view])
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
       [:<> [group-panel selection-view rows]
        [layer-controls selection-view instances]
        [:div.live-effect-grid
         (for [instance instances]
           ^{:key (wire (:id instance))}
           [instance-card show instance parameter-control
            {:duplicate? (< 1 (get duplicate-counts (:cue-id instance) 0)) :reorder? open?
             :capture capture :source-id source-id :ready? (not pending?)
             :selection-view selection-view :selection-row (get rows (:id instance))}])]]
       [:div.live-board-empty [:span "◌"] [:strong "Your running effects appear here"]
        [:p "Launch cues to mix their parameters together."]])]))
