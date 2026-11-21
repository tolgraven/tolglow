(ns tolglow.studio.modulation
  "Re-frame controls for the server's musical parameter modulation owner."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]))

(def defaults
  {:id "parameter-motion" :every "1" :unit "beats" :scope "all" :targets []
   :param-mode "all" :selected-params [] :ranges {} :mode "glide" :curve "smoothstep"
   :distribution "uniform" :seed "1" :max-change ""
   :cue-id "parameter-motion" :cue-name "Parameter motion"
   :pending {} :errors {} :notice nil :widgets {} :generation 0})
(defn- wire [value] (events/wire-id value))
(defn- state [db] (merge defaults (get-in db [:ui :modulation])))
(defn- store [db s] (assoc-in db [:ui :modulation] s))
(defn- value-of [event] (.. event -target -value))
(defn- set-form! [key value] (rf/dispatch-sync [:modulation/set key value]))
(defn- lookup [data key] (get data key (get data (keyword (wire key)) (get data (wire key)))))
(defn- display [value] (cond (vector? value) (str/join ", " (map display value))
                            (number? value) (str (/ (js/Math.round (* value 1000)) 1000))
                            :else (wire value)))

(defn number-value
  "Finite decimal or simple fraction. Parsing happens on submission, preserving
  incomplete editing strings in app-db."
  [value label]
  (let [raw (str/trim (str value)) parts (str/split raw #"/" -1)
        n (if (= 2 (count parts))
            (if (every? #(re-matches #"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)" %) parts)
              (/ (js/Number (first parts)) (js/Number (second parts))) js/NaN)
            (if (= 1 (count parts)) (js/Number raw) js/NaN))]
    (when (or (str/blank? raw) (not (js/Number.isFinite n)))
      (throw (js/Error. (str label " needs a finite number, such as 0.5 or 1/2."))))
    n))

(defn build-config [form]
  (let [every (number-value (:every form) "Interval") seed (number-value (:seed form) "Seed")
        scope (:scope form) targets (vec (distinct (map wire (:targets form))))
        selected (vec (distinct (map wire (:selected-params form))))
        ranges (into {} (for [[key {:keys [enabled? low high]}] (:ranges form) :when enabled?]
                         (let [a (number-value low (str (wire key) " minimum"))
                               b (number-value high (str (wire key) " maximum"))]
                           (when (> a b) (throw (js/Error. "Random range minimum cannot exceed maximum.")))
                           [(wire key) [a b]])))
        select (case scope "all" {} "instances" {:instance-ids targets} "effects" {:effect-ids targets}
                     "cues" {:cue-ids targets} "categories" {:categories targets}
                     (throw (js/Error. "Choose a supported modulation scope.")))]
    (when-not (and (pos? every) (#{"beats" "bars" "phrases"} (:unit form)))
      (throw (js/Error. "Choose a positive interval in beats, bars or phrases.")))
    (when-not (and (#{"sample-hold" "glide"} (:mode form))
                   (#{"linear" "smoothstep" "sine" "ease-in" "ease-out" "step"} (:curve form))
                   (#{"uniform" "triangular"} (:distribution form))
                   (#{"all" "selected"} (:param-mode form)))
      (throw (js/Error. "Choose supported modulation mode, curve, distribution and parameter selection.")))
    (when-not (js/Number.isSafeInteger seed) (throw (js/Error. "Seed needs a safe integer.")))
    (when (and (not= scope "all") (empty? targets)) (throw (js/Error. "Choose at least one target.")))
    (when (and (= "selected" (:param-mode form)) (empty? selected))
      (throw (js/Error. "Choose at least one parameter.")))
    (cond-> {:every {(keyword (:unit form)) every} :select select :seed seed
             :params (if (= "all" (:param-mode form)) "all" selected)
             :mode (:mode form) :curve (:curve form) :distribution (:distribution form) :ranges ranges}
      (seq (str/trim (:id form ""))) (assoc :id (str/trim (:id form)))
      (seq (str/trim (:max-change form "")))
      (assoc :max-change (let [n (number-value (:max-change form) "Maximum change")]
                           (when (neg? n) (throw (js/Error. "Maximum change cannot be negative."))) n)))))

(defn schema-range [schema]
  (or (get-in schema [:random :range]) (:recommended schema) (:bounds schema)
      (when (= "color" (wire (:type schema))) [0 1])))
(defn- schema-numeric? [schema] (contains? #{"number" "integer" "color" "vector" "range"} (wire (:type schema))))
(defn- widget-key [instance-id parameter] [(wire instance-id) (wire parameter)])
(defn widget-form [s instance-id parameter schema]
  (let [key (widget-key instance-id parameter) [low high] (schema-range schema)]
    (merge {:every "1" :unit "beats" :mode "glide" :curve "smoothstep" :seed "1" :hold-beats ""
            :low (if (some? low) (str low) "") :high (if (some? high) (str high) "") :expanded? false}
           (get-in s [:widgets key]))))
(defn widget-config [form instance-id parameter schema id]
  (build-config (merge defaults form {:id id :scope "instances" :targets [(wire instance-id)]
                                      :param-mode "selected" :selected-params [(wire parameter)]
                                      :ranges (if (schema-numeric? schema)
                                                {(wire parameter) {:enabled? true :low (:low form) :high (:high form)}} {})})))

(defn held? [show instance-id parameter]
  (let [hold (lookup (lookup (get-in show [:modulation :holds]) instance-id) parameter)
        beat (get-in show [:transport :beat] 0)]
    (and (map? hold) (or (nil? (:until-beat hold)) (< beat (:until-beat hold))))))
(defn- controller-for [show instance-id parameter]
  (first (filter #(and (vector? (get-in % [:config :params])) (= [(wire instance-id)] (mapv wire (get-in % [:config :select :instance-ids])))
                       (= [(wire parameter)] (mapv wire (get-in % [:config :params]))))
                 (get-in show [:modulation :controllers]))))
(defn request-token [db] [(get-in db [:show :runtime-id]) (get-in db [:show :epoch]) (:generation (state db))])
(defn- send-request [db key path body notice]
  {:db (store db (-> (state db) (assoc-in [:pending key] true) (update :errors dissoc key) (assoc :notice nil)))
   :studio/http {:path path :body body :on-success [:modulation/accepted (request-token db) key notice]
                 :on-failure [:modulation/failed (request-token db) key]}})
(defn- failure [db key error]
  {:db (store db (-> (state db) (assoc-in [:errors key] (events/error-message error)) (update :pending dissoc key)))})

(rf/reg-sub :modulation/state
  (fn [db _]
    {:form (state db)
     :show (select-keys (:show db) [:effects :cues :instances :pages :modulation])
     :manual-holds (vec (for [[id holds] (get-in db [:show :modulation :holds]) [parameter hold] holds
                              :when (held? (:show db) id parameter)]
                          {:instance-id (wire id) :param (wire parameter) :until-beat (:until-beat hold)}))
     :page-id (get-in db [:ui :page-id]) :playing? (get-in db [:show :transport :playing?])}))
(rf/reg-sub :modulation/parameter
  (fn [db [_ instance-id parameter schema]]
    (let [s (state db) key (widget-key instance-id parameter) show (:show db)]
      {:form (widget-form s instance-id parameter schema) :key key
       :controller (controller-for show instance-id parameter)
       :held? (held? show instance-id parameter)
       :value (lookup (lookup (get-in show [:modulation :overrides]) instance-id) parameter)
       :busy? (boolean (get-in s [:pending key])) :error (get-in s [:errors key])})))
(rf/reg-event-db :modulation/set
  (fn [db [_ key value]]
    (store db (cond-> (assoc (state db) key value)
                (= key :scope) (assoc :targets [] :selected-params [] :ranges {})))))
(rf/reg-event-db :modulation/toggle
  (fn [db [_ key value]]
    (store db (update (state db) key #(if (some #{value} %) (vec (remove #{value} %)) (conj (vec %) value))))))
(rf/reg-event-db :modulation/range
  (fn [db [_ parameter field value]] (store db (assoc-in (state db) [:ranges (wire parameter) field] value))))
(rf/reg-event-db :modulation/widget-set
  (fn [db [_ key field value]] (store db (assoc-in (state db) [:widgets key field] value))))
(rf/reg-event-db :modulation/runtime-change
  (fn [db _] (store db (-> (state db) (assoc :pending {} :errors {} :widgets {} :notice nil) (update :generation inc)))))
(rf/reg-event-fx :modulation/accepted
  (fn [{:keys [db]} [_ token key notice _]]
    (if (= token (request-token db))
      {:db (store db (-> (state db) (update :pending dissoc key) (assoc :notice notice)))
       :dispatch [:studio/poll]} {})))
(rf/reg-event-fx :modulation/failed
  (fn [{:keys [db]} [_ token key error]]
    (if (= token (request-token db)) (failure db key error) {})))
(rf/reg-event-fx :modulation/start [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    (try
      (let [config (build-config (state db)) config (assoc config :id (or (:id config) (str "motion-" unique)))]
        (send-request db :global "/api/modulation/start" config "Parameter motion started."))
      (catch :default error (failure db :global error)))))
(rf/reg-event-fx :modulation/save-cue
  (fn [{:keys [db]} _]
    (try
      (let [s (state db) id (str/trim (:cue-id s)) name (str/trim (:cue-name s))
            config (dissoc (build-config s) :id)]
        (when (or (str/blank? id) (str/blank? name)) (throw (js/Error. "Give the modulation cue an ID and a name.")))
        (send-request db :global "/api/modulation/cue"
                      (cond-> {:id id :name name :modulation config}
                        (get-in db [:ui :page-id]) (assoc :page-id (get-in db [:ui :page-id])))
                      "Modulation cue saved. Launch it from the cue grid."))
      (catch :default error (failure db :global error)))))
(rf/reg-event-fx :modulation/stop
  (fn [{:keys [db]} [_ controller key]]
    (if-let [owner (:owner-instance-id controller)]
      {:dispatch [:studio/command "instance/end" {:instance-id owner}]}
      (send-request db (or key [:controller (wire (:id controller))]) "/api/modulation/stop" {:id (:id controller)} "Parameter motion stopped."))))
(rf/reg-event-fx :modulation/widget-start [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ instance-id parameter schema]]
    (let [key (widget-key instance-id parameter)]
      (try
        (let [existing (controller-for (:show db) instance-id parameter)
              id (if (:owner-instance-id existing) (str "parameter-" unique) (or (:id existing) (str "parameter-" unique)))
              config (widget-config (widget-form (state db) instance-id parameter schema) instance-id parameter schema id)]
          (send-request db key "/api/modulation/start" config "This parameter now follows its musical modulation."))
        (catch :default error (failure db key error))))))
(rf/reg-event-fx :modulation/hold
  (fn [{:keys [db]} [_ instance-id parameter beats]]
    (let [key (widget-key instance-id parameter)]
      (try
        (let [duration (when (seq (str/trim (str beats))) (number-value beats "Hold length"))
              _ (when (and duration (not (pos? duration))) (throw (js/Error. "Hold length must be positive beats.")))
              payload (cond-> {:instance-id (wire instance-id) :params [(wire parameter)]}
                        duration (assoc :until-beat (+ (get-in db [:show :transport :beat] 0) duration)))]
          (send-request db key "/api/modulation/hold" payload "Manual value is in control."))
        (catch :default error (failure db key error))))))
(rf/reg-event-fx :modulation/resume
  (fn [{:keys [db]} [_ instance-id parameter]]
    (send-request db (widget-key instance-id parameter) "/api/modulation/resume"
                  {:instance-id (wire instance-id) :params [(wire parameter)]} "Musical modulation resumed.")))

(defn target-options [show scope]
  (case scope
    "instances" (mapv (fn [instance] {:id (wire (:id instance)) :label (str (or (:name instance) (wire (:cue-id instance))) " · " (:id instance))}) (:instances show))
    "effects" (mapv (fn [effect] {:id (wire (:id effect)) :label (or (:name effect) (wire (:id effect)))}) (:effects show))
    "cues" (mapv (fn [cue] {:id (wire (:id cue)) :label (or (:name cue) (wire (:id cue)))}) (:cues show))
    "categories" (mapv #(hash-map :id % :label (str/capitalize (str/replace % "-" " ")))
                       (sort (set (map wire (mapcat #(concat (:categories %) (when (:category %) [(:category %)])) (:effects show))))))
    []))
(defn parameter-options [show form]
  (let [selected (set (:targets form))
        effects (:effects show)
        definitions (case (:scope form)
                      "instances" (keep #(when (selected (wire (:id %)))
                                            (or (:definition %) (some (fn [effect] (when (= (wire (:id effect)) (wire (:effect-id %))) effect)) effects)))
                                        (:instances show))
                      "effects" (filter #(selected (wire (:id %))) effects)
                      "cues" (let [ids (set (map (comp wire :effect-id) (filter #(selected (wire (:id %))) (:cues show))))]
                               (filter #(ids (wire (:id %))) effects))
                      "categories" (filter #(some selected (map wire (concat (:categories %) (when (:category %) [(:category %)])))) effects)
                      effects)
        entries (group-by (comp wire first) (mapcat :params definitions))]
    (mapv (fn [[key entries]]
            (let [schemas (mapv second entries) domains (keep schema-range schemas)
                  low (when (seq domains) (apply max (map first domains)))
                  high (when (seq domains) (apply min (map second domains)))]
              {:id key :label (or (:name (first schemas)) key) :schemas schemas
               :numeric? (every? schema-numeric? schemas)
               :domain (when (and (some? low) (<= low high)) [low high])}))
          (sort-by key entries))))

(defn- interval-fields [every unit change! disabled?]
  [:div.modulation-interval
   [:label "Every"
    [:input {:type "text" :input-mode "decimal" :value every :disabled disabled?
              :aria-label "Modulation interval" :on-change #(change! :every (value-of %))}]]
   [:label "Musical unit"
    [:select {:value unit :disabled disabled? :aria-label "Modulation interval unit" :on-change #(change! :unit (value-of %))}
     [:option {:value "beats"} "Beats"] [:option {:value "bars"} "Bars"] [:option {:value "phrases"} "Phrases"]]]])
(defn- mode-fields [form change! disabled?]
  [:div.modulation-pair
   [:label "Motion"
    [:select {:value (:mode form) :disabled disabled? :on-change #(change! :mode (value-of %))}
     [:option {:value "sample-hold"} "Step to each value"] [:option {:value "glide"} "Glide between values"]]]
   [:label "Curve"
    [:select {:value (:curve form) :disabled (or disabled? (not= "glide" (:mode form)))
               :on-change #(change! :curve (value-of %))}
     (for [[id label] [["linear" "Linear"] ["smoothstep" "Smooth"] ["sine" "Sine"]
                      ["ease-in" "Ease in"] ["ease-out" "Ease out"] ["step" "Hold until boundary"]]]
       ^{:key id} [:option {:value id} label])]]])
(defn- status-message [s key]
  (when-let [error (get-in s [:errors key])] [:p.modulation-error {:role "alert"} error]))
(defn- scope-form [s show busy?]
  [:div.modulation-scope
   [:label {:for "modulation-scope"} "Control"
    [:select#modulation-scope {:value (:scope s) :disabled busy? :on-change #(set-form! :scope (value-of %))}
     [:option {:value "all"} "All running effects"] [:option {:value "instances"} "Chosen running instances"]
     [:option {:value "effects"} "Chosen effect definitions"] [:option {:value "cues"} "Chosen cues"]
     [:option {:value "categories"} "Chosen categories"]]]
   (when (not= "all" (:scope s))
     (let [choices (target-options show (:scope s))]
       [:div.modulation-choices {:role "group" :aria-label "Modulation targets"}
        (if (seq choices)
          (for [choice choices]
            ^{:key (:id choice)} [:label.modulation-choice
                                 [:input {:type "checkbox" :checked (boolean (some #{(:id choice)} (:targets s))) :disabled busy?
                                           :on-change #(rf/dispatch [:modulation/toggle :targets (:id choice)])}]
                                 [:span (:label choice)]])
          [:p.small.muted "No targets of this kind are available in this show."])]))])
(defn- parameters-form [s show busy?]
  (let [parameters (parameter-options show s)]
    [:div.modulation-parameters
     [:label {:for "modulation-parameter-mode"} "Parameters"
      [:select#modulation-parameter-mode {:value (:param-mode s) :disabled busy? :on-change #(set-form! :param-mode (value-of %))}
       [:option {:value "all"} "All eligible parameters"] [:option {:value "selected"} "Choose parameters"]]]
     (when (= "selected" (:param-mode s))
       [:div.modulation-choices {:role "group" :aria-label "Modulated parameters"}
        (for [parameter parameters]
          ^{:key (:id parameter)} [:label.modulation-choice
                                  [:input {:type "checkbox" :disabled busy?
                                            :checked (boolean (some #{(:id parameter)} (:selected-params s)))
                                            :on-change #(rf/dispatch [:modulation/toggle :selected-params (:id parameter)])}]
                                  [:span (:label parameter)]])])
     [:details.modulation-ranges
      [:summary "Choose ranges · optional"]
      [:p.small.muted "Unset ranges follow the parameter's recommended and hard limits. Missing numeric limits appear as diagnostics."]
      (for [{:keys [id label domain numeric?]} parameters
            :when (and numeric? (or (= "all" (:param-mode s)) (some #{id} (:selected-params s))))
            :let [entry (merge {:enabled? false :low (if domain (str (first domain)) "")
                                :high (if domain (str (second domain)) "")} (get-in s [:ranges id]))]]
        ^{:key id}
        [:div.modulation-range-row
         [:label.modulation-choice [:input {:type "checkbox" :disabled busy? :checked (:enabled? entry)
                                            :on-change (fn [event]
                                                         (rf/dispatch [:modulation/range id :low (:low entry)])
                                                         (rf/dispatch [:modulation/range id :high (:high entry)])
                                                         (rf/dispatch [:modulation/range id :enabled? (.. event -target -checked)]))}]
          [:span label]]
         [:label "Minimum" [:input {:type "text" :input-mode "decimal" :value (:low entry) :disabled (or busy? (not (:enabled? entry)))
                                      :aria-label (str label " random minimum")
                                      :on-change #(rf/dispatch-sync [:modulation/range id :low (value-of %)])}]]
         [:label "Maximum" [:input {:type "text" :input-mode "decimal" :value (:high entry) :disabled (or busy? (not (:enabled? entry)))
                                      :aria-label (str label " random maximum")
                                      :on-change #(rf/dispatch-sync [:modulation/range id :high (value-of %)])}]]])]]))

(defn- controller-list [show]
  [:div.modulation-running
   [:h3 "Running controllers"]
   (if (seq (get-in show [:modulation :controllers]))
     (for [controller (get-in show [:modulation :controllers])
           :let [config (:config controller) period (first (:every config))]]
       ^{:key (pr-str (:id controller))}
       [:div.modulation-controller
        [:div [:strong (if (:owner-instance-id controller) "Cue-owned modulation" (wire (:id controller)))]
         [:span.small.muted (str (wire (:mode config)) " · every " (display (second period)) " " (wire (first period))
                                " · " (:ticks controller) " changes")]
         [:span.small.muted (str "Next beat " (display (:next-beat controller)))]]
        [:button.button {:type "button" :on-click #(rf/dispatch [:modulation/stop controller nil])}
         (if (:owner-instance-id controller) "Release cue" "Stop")]])
     [:p.small.muted "No parameter modulation is running."])
   (when (seq (get-in show [:modulation :diagnostics]))
     [:ul.modulation-diagnostics {:aria-label "Modulation diagnostics"}
      (for [[index diagnostic] (map-indexed vector (take 20 (get-in show [:modulation :diagnostics])))]
        ^{:key index} [:li (or (:message diagnostic) (pr-str diagnostic))])])])

(defn panel [_db]
  (let [{s :form :keys [show playing? manual-holds]} @(rf/subscribe [:modulation/state])
        busy? (boolean (get-in s [:pending :global]))]
    [:section.panel.modulation-panel {:aria-label "Musical parameter modulation"}
     [:div.panel-top [:span.section-label "PARAMETER MOTION"]
      [:span.small.muted (if (false? playing?) "Transport paused" "Musical clock")]]
     [:div.modulation-body
      [:h2 "Let selected parameters evolve"]
      [:p.small.muted "Make a new seeded choice on each musical interval, or glide between choices. Your manual values remain available."]
      [status-message s :global]
      (when (:notice s) [:p.modulation-notice {:role "status"} (:notice s)])
      [:div.modulation-form-grid
       [:div
        [:label {:for "modulation-id"} "Controller ID"
         [:input#modulation-id {:type "text" :value (:id s) :disabled busy? :on-change #(set-form! :id (value-of %))}]]
        [interval-fields (:every s) (:unit s) set-form! busy?]
        [:div.modulation-presets {:aria-label "Common musical intervals"}
         (for [[value label] [["1/4" "¼"] ["1/2" "½"] ["1" "1"] ["2" "2"] ["4" "4"]]]
           ^{:key value} [:button.button {:type "button" :disabled busy? :on-click #(set-form! :every value)} label])]
        [mode-fields s set-form! busy?]
        [:div.modulation-pair
         [:label "Distribution" [:select {:value (:distribution s) :disabled busy? :on-change #(set-form! :distribution (value-of %))}
                                  [:option {:value "uniform"} "Even spread"] [:option {:value "triangular"} "Favor the middle"]]]
         [:label "Seed" [:input {:type "text" :input-mode "numeric" :value (:seed s) :disabled busy?
                                  :on-change #(set-form! :seed (value-of %))}]]]
        [:label "Maximum change per interval · optional"
         [:input {:type "text" :input-mode "decimal" :value (:max-change s) :disabled busy? :placeholder "Follow each parameter's policy"
                   :on-change #(set-form! :max-change (value-of %))}]]]
       [:div [scope-form s show busy?] [parameters-form s show busy?]]]
      [:div.button-row
       [:button.button.primary {:type "button" :disabled busy? :on-click #(rf/dispatch [:modulation/start])} "Start / replace controller"]]
      [:details.modulation-save-cue
       [:summary "Save this behavior as a cue"]
       [:p.small.muted "Launch or hold the cue from any controller. Releasing it stops its owned modulation."]
       [:div.modulation-pair
        [:label "Cue ID" [:input {:type "text" :value (:cue-id s) :disabled busy? :on-change #(set-form! :cue-id (value-of %))}]]
        [:label "Cue name" [:input {:type "text" :value (:cue-name s) :disabled busy? :on-change #(set-form! :cue-name (value-of %))}]]]
       [:button.button {:type "button" :disabled busy? :on-click #(rf/dispatch [:modulation/save-cue])} "Save cue on current page"]]
      [controller-list show]
      (when (seq manual-holds)
        [:div.modulation-holds [:h3 "Manual overrides"]
         (for [{:keys [instance-id param until-beat]} manual-holds]
           ^{:key (str instance-id "/" param)}
           [:div.modulation-hold-row [:span (str instance-id " · " param)]
            [:span.small.muted (if until-beat (str "Until beat " (display until-beat)) "Until resumed")]
            [:button.button {:type "button" :on-click #(rf/dispatch [:modulation/resume instance-id param])} "Resume"]])])]]))

(defn parameter-widget [instance-id parameter schema _value]
  (when instance-id
    (let [{:keys [form key controller held? value busy? error]} @(rf/subscribe [:modulation/parameter instance-id parameter schema])
          change! #(rf/dispatch-sync [:modulation/widget-set key %1 %2])]
      [:div.parameter-modulation {:data-modulation-param (wire parameter)}
       [:div.parameter-modulation-summary
        [:button.text-button {:type "button" :aria-expanded (:expanded? form) :on-click #(change! :expanded? (not (:expanded? form)))}
         (if (:expanded? form) "− Random motion" "+ Random motion")]
        (when (some? value) [:span.small.muted (str "Effective " (display value))])
        (when held? [:span.modulation-held "Manual"])]
       (when error [:p.modulation-error {:role "alert"} error])
       (when (:expanded? form)
         [:div.parameter-modulation-fields
          [interval-fields (:every form) (:unit form) change! busy?]
          [mode-fields form change! busy?]
          (when (schema-numeric? schema)
            [:div.modulation-pair
             [:label "Random minimum" [:input {:type "text" :input-mode "decimal" :value (:low form) :disabled busy?
                                                 :on-change #(change! :low (value-of %))}]]
             [:label "Random maximum" [:input {:type "text" :input-mode "decimal" :value (:high form) :disabled busy?
                                                 :on-change #(change! :high (value-of %))}]]])
          [:div.button-row
           [:button.button {:type "button" :disabled busy? :on-click #(rf/dispatch [:modulation/widget-start instance-id parameter schema])}
            (if (and controller (not (:owner-instance-id controller))) "Update motion" "Randomize this parameter")]
           (when controller [:button.button {:type "button" :disabled busy? :on-click #(rf/dispatch [:modulation/stop controller key])}
                             (if (:owner-instance-id controller) "Release cue" "Stop motion")])]
          [:div.modulation-manual
           [:label "Manual hold · beats, blank until resumed"
            [:input {:type "text" :input-mode "decimal" :value (:hold-beats form) :disabled busy?
                      :placeholder "Until resumed" :on-change #(change! :hold-beats (value-of %))}]]
           [:button.button {:type "button" :disabled busy?
                             :on-click #(rf/dispatch (if held? [:modulation/resume instance-id parameter]
                                                       [:modulation/hold instance-id parameter (:hold-beats form)]))}
            (if held? "Resume modulation" "Use manual value")]]])])))
