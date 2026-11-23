(ns tolglow.studio.parameter-source
  "Attach a musical source to one running input. The show supplies its sampled result."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.fixture-picker :as fixtures]
            [tolglow.studio.modulation :as motion]
            [tolglow.studio.parameter-widgets :as widgets]))

(defn reading [instance param]
  (get (:parameter-sources instance) (keyword (events/wire-id param))))

(defn bound? [instance param] (boolean (seq (:sources (reading instance param)))))
(defn- source-of [source kind]
  (some #(when (= kind (keyword (:type %))) %) (:sources source)))
(defn- selected-parameter [db id] (get-in db [:ui :parameter-source id]))

(rf/reg-sub ::selection (fn [db [_ id]] (selected-parameter db id)))
(rf/reg-event-db ::select
  (fn [db [_ id param]]
    (if (= param (selected-parameter db id))
      (update-in db [:ui :parameter-source] dissoc id)
      (assoc-in db [:ui :parameter-source id] param))))

(rf/reg-sub ::waveforms (fn [db _] (get-in db [:server-projection :view :parameter-waveforms])))
(rf/reg-sub ::raw-instance
  (fn [db [_ id]]
    (some #(when (= id (:id %)) %) (get-in db [:server-projection :view :instances]))))

(defn- context [db] ((juxt :runtime-id :epoch) (:show db)))
(defn- request-key [db id param] [(context db) id param])
(rf/reg-sub ::request (fn [db [_ id param]] (get-in db [:ui :source-requests (request-key db id param)])))

(defn request [db path id param binding]
  (let [[runtime epoch] (context db)
        key (request-key db id param)]
    (if (get-in db [:ui :source-requests key :pending?]) {}
      {:db (assoc-in db [:ui :source-requests key] {:pending? true})
       :studio/http {:path (str "/api/modulation/" path) :typed? true
                     :body (cond-> {:expected-epoch epoch :runtime-id runtime :instance-id id :param param}
                             binding (assoc :binding binding))
                     :on-success [::complete key] :on-failure [::failed key]}})))
(rf/reg-event-fx ::bind (fn [{:keys [db]} [_ id param binding]] (request db "bind" id param binding)))
(rf/reg-event-fx ::unbind (fn [{:keys [db]} [_ id param]] (request db "unbind" id param nil)))
(rf/reg-event-db ::complete (fn [db [_ key]] (update-in db [:ui :source-requests] dissoc key)))
(rf/reg-event-db ::failed
  (fn [db [_ [owner :as key] error]]
    (if (= owner (context db))
      (assoc-in db [:ui :source-requests key] {:error (events/error-message error)}) db)))

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
    (if-let [block (source-of source :blocks)]
      (when-let [target (:id @(rf/subscribe [::fixture (:id instance)]))]
        (:result @(rf/subscribe [:studio/remote [:runtime/block-trace (:id instance) (mapv keyword (:path block)) target]])))
      (:effective-value source))))

(defn- result-view [view instance schema sample block?]
  (if-not block? [view schema sample]
    (if-let [{:keys [id name]} @(rf/subscribe [::fixture (:id instance)])]
      [:<> [:span.source-sample-target (str "Fixture · " (or name (events/wire-id id)))]
       ^{:key (pr-str id)} [view schema sample]]
      [:span.block-trace-empty "No target fixtures"])))

(defn result-visual [view instance param schema]
  (when (bound? instance param)
    [result-view view instance schema #(native-value instance param) (source-of (reading instance param) :blocks)]))

(defn axes-visual [view instance components schema]
  (let [axis (fn [[param spec]]
               (if (bound? instance param)
                 (native-value instance param)
                 (get-in instance [:params param] (:default spec))))
        sample (fn [] (let [values (mapv axis components)] (when (every? some? values) values)))
        block? (some (fn [[k _]] (source-of (reading instance k) :blocks)) components)]
    [result-view view instance schema sample block?]))

(defn- trigger-id [id param] (str "source-" id "-" (events/wire-id param)))

(defn- close! [id param components]
  (rf/dispatch [::select id param])
  (when-let [node (.getElementById js/document (trigger-id id (or (ffirst components) param)))] (.focus node)))

(defn trigger [id param title source & [components]]
  (let [selected @(rf/subscribe [::selection id])
        bound? (seq (:sources source))]
    [:button.param-source-trigger {:type "button" :class (when bound? "bound") :id (trigger-id id (or (ffirst components) param))
                                  :aria-expanded (boolean (or (= param selected) (some #(= selected (first %)) components)))
                                  :title (str (if bound? "Edit" "Attach") " source for " title)
                                  :aria-label (str (if bound? "Edit" "Attach") " source for " title)
                                  :on-click #(rf/dispatch [::select id (or (some (fn [[k _]] (when (= k selected) k)) components) param)])}
     [:span {:aria-hidden true} (if bound? "∿" "+")] (if components "Sources" "Source")]))

(defn- options [schema]
  (or (when (seq (:choices schema)) (mapv :value (:choices schema))) (:values schema) (:options schema)))
(defn- bounds [schema base]
  (let [[lo hi] (or (:bounds schema) (:recommended schema) [0 1])]
    (cond
      (seq (options schema)) [(first (options schema)) (last (options schema))]
      (= :boolean (keyword (:type schema))) [false true]
      (sequential? base) (let [n (count base)] [(vec (repeat n lo)) (vec (repeat n hi))])
      :else [lo hi])))

(defn draft [schema base binding]
  (let [[lo hi] (bounds schema base)
        [unit count] (or (first (:period binding)) [:beats 1])]
    {:waveform (get binding :waveform :sine) :unit (keyword unit) :period (str count)
     :phase (str (:phase binding 0)) :from (get binding :from lo) :to (get binding :to hi)
     :inputs (or (:inputs binding) {})}))

(defn binding-config [form]
  (let [period (motion/number-value (:period form) "Period")]
    (when-not (pos? period) (throw (js/Error. "Period must be positive.")))
    {:type :lfo :waveform (:waveform form) :period {(:unit form) period}
     :phase (motion/number-value (:phase form) "Phase") :from (:from form) :to (:to form)
     :inputs (:inputs form)}))

(defn- number-field [label value change! schema]
  (let [[low high] (:bounds schema)]
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

(defn- lfo-editor [id param schema base source catalog]
  (let [lfo (source-of source :lfo)]
    (r/with-let [form (r/atom (draft schema base (:binding lfo))) error (r/atom nil)]
      (let [{:keys [waveform period unit phase from to inputs]} @form
            in-flight @(rf/subscribe [::request id param])
            wave (some #(when (= waveform (:id %)) %) catalog)
            update! (fn [key value] (swap! form assoc key value))
            apply! (fn [e]
                     (.preventDefault e)
                     (try (let [config (binding-config @form)] (reset! error nil) (rf/dispatch [::bind id param config]))
                          (catch :default problem (reset! error (events/error-message problem)))))]
        [:form.live-source-form {:on-submit apply!}
         [:div.source-fields
          [:label.source-field [:span "Waveform"]
           [:select {:aria-label "Waveform source" :value (pr-str waveform)
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
          [number-field "Phase (cycles)" phase #(update! :phase %) {}]
          [endpoint "From" schema from #(update! :from %)]
          [endpoint "To" schema to #(update! :to %)]]
         (when (seq (:params wave))
           [:div.source-fields
            (for [[key input] (:params wave)] ^{:key key}
              [number-field (or (:name input) (widgets/label key)) (get inputs key (:default input))
               #(swap! form assoc-in [:inputs key] %) input])])
         (when-let [message (or @error (:error in-flight))] [:p.performance-error {:role "alert"} message])
         [:div.source-actions
          [:button.button.primary {:type "submit" :disabled (or (:pending? in-flight) (not wave))} (if lfo "Update LFO" "Attach LFO")]
          (when lfo
            [:<>
             [:button.button {:type "button" :on-click #(rf/dispatch [(if (:held? lfo) :modulation/resume :modulation/hold) id param])}
              (if (:held? lfo) "Resume LFO" "Use base value")]
             [:button.button {:type "button" :disabled (:pending? in-flight) :on-click #(rf/dispatch [::unbind id param])} "Detach LFO"]])]]))))

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

(defn editor [instance param schema components]
  (let [id (:id instance) title (or (:name schema) (widgets/label param))
        raw @(rf/subscribe [::raw-instance id])
        typed (get-in raw [:definition :params param] schema)
        source (or (reading raw param) (reading instance param)) catalog @(rf/subscribe [::waveforms])
        base (get-in raw [:params param] (get-in instance [:params param] (:default schema)))]
    [:section.live-source-editor {:aria-label (str title " source")
                                 :on-key-down #(when (= "Escape" (.-key %)) (.stopPropagation %) (close! id param components))}
     [:header [:h4 (str title " source")]
      [:button.text-button {:type "button" :aria-label "Close source editor" :on-click #(close! id param components)} "Close"]]
     (when (source-of source :blocks) [fixture-selector id])
     (when (seq components)
       [:label.source-field "Axis" [:select {:aria-label "Source axis" :value (events/wire-id param)
                                          :on-change #(rf/dispatch [::select id (keyword (.. % -target -value))])}
                                  (for [[k axis] components] ^{:key k} [:option {:value (events/wire-id k)} (or (:name axis) (widgets/label k))])]])
     (when (seq (:sources source))
       [:div.source-connections
        (for [[i input] (map-indexed vector (:sources source))]
          ^{:key i} [:span (source-label input)
                     (when (:held? input) " · held") (when-let [error (:error input)] [:span.performance-error {:role "alert"} error])])])
     ^{:key [id param (:binding (source-of source :lfo))]} [lfo-editor id param typed base source catalog]
     [motion/parameter-widget id param schema base]]))
