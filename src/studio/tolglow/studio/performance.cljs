(ns tolglow.studio.performance
  "System controls over the shared command boundary. Snapshot groups provide
  category membership; this UI does not infer categories or run effect clocks."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.authoring.parameter-layout :as layout]
            [tolglow.studio.events :as events]
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
  (fn [db [_ id]] (store db (update-in (state db) [:expanded (wire id)] not))))
(rf/reg-sub :performance/card-expanded
  (fn [db [_ id]] (boolean (get-in (state db) [:expanded (wire id)]))))
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

(defn group-panel [_db]
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

(defn- instance-card [show instance parameter-control]
  (let [id (wire (:id instance)) expanded? @(rf/subscribe [:performance/card-expanded id])
        definition (card-definition show instance)
        faults @(rf/subscribe [:inspect/faults])
        faulted? (seq (inspect/faults-for faults instance))
        cue (some #(when (= (wire (:id %)) (wire (:cue-id instance))) %) (:cues show))
        {:keys [controls monitor]} @(r/track resolve-controls definition (:ui/layout cue))
        title (or (:name cue) (:name instance) (widgets/label (:cue-id instance)))
        paused? (get-in instance [:system :paused?])
        {:keys [state key target instances]} @(rf/subscribe [:performance/instance id])
        editing @(rf/subscribe [::sources/selection id])
        scope (resolve-monitor (driven-controls instance controls) (get-in state [:monitor id]) monitor)]
    [:article.live-effect-card {:data-live-instance id :aria-label (str title " · " id)
                                :data-density (if expanded? "detail" "strip")
                                :data-wide (boolean (or expanded? (> (count controls) 3)))
                                :class (str (when faulted? "has-fault ") (when paused? "paused ") (when (= "releasing" (:phase instance)) "releasing"))
                                :style {"--cue-color" (widgets/color-css (:swatch cue))}}
     [:header.live-effect-header
      [:div.live-effect-title
       [:h3 title]
       [:div.live-effect-meta
        [:span.live-effect-phase (cond faulted? "Fault" paused? "Paused" :else (widgets/label (:phase instance)))]
        [widgets/category-tags (or (:categories instance) (:categories definition)) (:category-colors show)]]]
      [:button.live-effect-expand {:type "button" :aria-expanded expanded? :aria-label (str (if expanded? "Collapse " "Expand ") title " controls")
                                   :on-click #(rf/dispatch [:performance/card-toggle id])}
       (if expanded? "Compact" "Detail")]]
     (when (seq controls)
       [:div.live-effect-body
        [parameter-bank show instance controls (not expanded?) parameter-control]
        (when (and (not expanded?) (:selected scope)) [strip-monitor instance scope])])
     (when-let [schema (get-in definition [:params editing])]
       (let [group (some #(when (some (fn [[p _]] (= p editing)) (:components %)) (:components %)) controls)]
         ^{:key [id (or (ffirst group) editing)]} [sources/editor instance editing schema group]))
     [instance-controls instance {:compact? true}]
     (when expanded?
       [:div.live-effect-advanced
        [inspect/panel show instance faults]
        (when (:cue-id instance) [spatial/cue-target-picker (:cue-id instance)])
        [envelope-controls state key target instances (empty? instances)]
        [action-button target "instance/remove" "Remove immediately" {:class "performance-remove"}]
        [:span.live-effect-identity id]])]))

(defn board [db parameter-control]
  (let [show (:show db) instances (:instances show)]
    [:section.live-board {:aria-label "Running effects"}
     [:div.live-board-heading [:h2 "In the mix"] [:span#active-count.count (str (count instances) " active")]]
     [inspect/unassigned-faults show @(rf/subscribe [:inspect/faults])]
     (if (seq instances)
       [:<> [group-panel db]
        [:div.live-effect-grid
         (for [instance instances]
           ^{:key (wire (:id instance))} [instance-card show instance parameter-control])]]
       [:div.live-board-empty [:span "◌"] [:strong "Your running effects appear here"]
        [:p "Launch cues to mix their parameters together."]])]))
