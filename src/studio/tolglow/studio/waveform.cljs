(ns tolglow.studio.waveform
  "A waveform picker and bounded server-sampled preview. No browser oscillator."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.parameter-widgets :as widgets]))

(def channels
  [[:value "Value"] [:slope "Slope"] [:angular-velocity "Angular velocity"] [:jagginess "Jagginess"]])

(def category-colors
  {"smooth" "#b6ce9b" "angular" "#dbbd80" "discontinuous" "#d99ca6" "composite" "#9ebfd5"})

(def entry-defaults {:open? false :active? false :channel :value :settings {} :request-id 0})

(defn- wire [value] (events/wire-id value))

(defn- entry [db key] (merge entry-defaults (get-in db [:ui :waveform :entries key])))

(defn- put-entry [db key value] (assoc-in db [:ui :waveform :entries key] value))

(defn request-token [db]
  [(get-in db [:show :runtime-id]) (get-in db [:show :epoch]) (get-in db [:ui :waveform :generation] 0)])

(defn picker-key [instance-id parameter] [(or (wire instance-id) "draft") (wire parameter)])

(defn waveform-schema? [schema] (= "waveform-selector" (wire (:semantic/type schema))))

(defn choice-index
  "Nearest numeric choice, with ties going to the later choice, matching the
  registered selector's half-up rounding. Continuous values keep their meaning."
  [choices value]
  (if (and (seq choices) (widgets/finite? value))
    (first
     (reduce (fn [[best distance] [index choice]]
               (let [d (if (widgets/finite? (:value choice)) (js/Math.abs (- value (:value choice))) js/Infinity)]
                 (if (<= d distance) [index d] [best distance])))
             [0 js/Infinity] (map-indexed vector choices)))
    0))

(defn- numeric [value label]
  (let [raw (str/trim (str value)) n (js/Number raw)]
    (when (or (str/blank? raw) (not (js/Number.isFinite n)))
      (throw (js/Error. (str label " needs a finite number.")))) n))

(defn- numeric-bindings [bindings]
  (when-not (map? bindings) (throw (js/Error. "Waveform preview inputs must be a map.")))
  (into {} (for [[key value] bindings]
             (let [value (if (and (map? value) (= "const" (wire (:op value)))) (:value value) value)]
               (when-not (widgets/finite? value)
                 (throw (js/Error. (str "Preview input " (wire key) " needs a numeric value. An effect expression needs explicit preview bindings."))))
               [(wire key) value]))))

(defn- portable-input [value]
  (cond (map? value) (into {} (map (fn [[k v]] [(wire k) (portable-input v)]) value))
        (sequential? value) (mapv portable-input value)
        (keyword? value) (wire value) :else value))

(defn preview-payload
  "Build the real preview API body. Keep namespaced IDs and numeric bindings;
  reject unresolved effect expressions instead of inventing a browser context."
  [selection options settings]
  (let [options (merge {:samples 257 :cycles 1 :window (/ 1 1024) :angle-span-deg 360} options settings)
        samples (numeric (:samples options) "Sample count")
        cycles (numeric (:cycles options) "Cycles")
        window (numeric (:window options) "Derivative window")
        angle (numeric (:angle-span-deg options) "Angular span")
        start (numeric (:start-phase options 0) "Starting phase")
        context (:context-params options)
        params (when (contains? options :params)
                 (if context (portable-input (:params options)) (numeric-bindings (:params options))))
        options (cond-> (assoc (dissoc options :context-params) :samples samples :cycles cycles :window window
                              :angle-span-deg angle :start-phase start)
                  params (assoc :params params))]
    (when-not (and (js/Number.isInteger samples) (<= 2 samples 2048))
      (throw (js/Error. "Choose 2 to 2048 preview samples.")))
    (when-not (and (pos? cycles) (js/Number.isFinite (+ start cycles)))
      (throw (js/Error. "Preview cycles must form a finite positive interval.")))
    (when-not (<= 0.000001 window 0.25)
      (throw (js/Error. "Derivative window must be between 0.000001 and 0.25 cycles.")))
    (when-not (pos? angle) (throw (js/Error. "Angular span must be positive.")))
    (cond
      (:instance-id selection) {:instance-id (wire (:instance-id selection))
                                :parameter (wire (:parameter selection))
                                :input-key (:input-key selection) :options options}
      (map? selection)
      (let [choices (:choices selection) selector (:selector selection)]
        (when-not (and (vector? choices) (<= 1 (count choices) 16) (widgets/finite? selector))
          (throw (js/Error. "Preview needs one to sixteen waveform choices and a numeric selection.")))
        (cond-> {:selection {:selector selector
                     :choices (mapv (fn [choice]
                                      (if (map? choice)
                                        (cond-> {:id (wire (:id choice))}
                                          (:version choice) (assoc :version (:version choice))
                                          (contains? choice :params) (assoc :params (if context (portable-input (:params choice)) (numeric-bindings (:params choice)))))
                                        (wire choice))) choices)}
         :options options}
          context (assoc :context-params (portable-input context))))
      :else (cond-> {:id (wire selection) :options options}
              context (assoc :context-params (portable-input context))))))

(defn chart-data
  "Project finite server samples into SVG coordinates. Invalid/oversize data
  yields no trace; the zero baseline and true value range remain visible."
  [samples channel]
  (when (and (vector? samples) (<= 2 (count samples) 2048)
             (every? #(and (widgets/finite? (:phase %)) (widgets/finite? (get % channel))) samples))
    (let [phases (mapv :phase samples) values (mapv channel samples)
          x-min (first phases) x-max (last phases)
          minimum (apply min values) maximum (apply max values)
          low (min 0 minimum) high (max 0 maximum)
          [low high] (if (= low high) [(- low 0.5) (+ high 0.5)] [low high])
          scale (max (js/Math.abs low) (js/Math.abs high))
          y (fn [value] (- 164 (* 144 (/ (- (/ value scale) (/ low scale))
                                        (- (/ high scale) (/ low scale))))))]
      (when (and (< x-min x-max) (every? pos? (map - (rest phases) phases)))
        {:points (str/join " " (map (fn [phase value] (str (+ 10 (* 492 (/ (- phase x-min) (- x-max x-min)))) "," (y value))) phases values))
         :minimum minimum :maximum maximum :x-min x-min :x-max x-max :low low :high high :zero-y (y 0)}))))

(defn clock-bound? [value]
  (boolean (some #(and (map? %) (contains? #{"time" "time-seconds" "beat" "bar" "phrase"} (wire (:op %))))
                 (tree-seq coll? seq value))))

(defn- transport-key [transport]
  (update (select-keys transport [:source :source-epoch :source-selection-epoch :playing? :meter :generation]) :source wire))

(defn live-timing-key [entry transport]
  [(transport-key transport) (when (get-in entry [:data :timeline]) (:beat transport))])

(defn timeline-chart [timeline transport]
  (when (and timeline (widgets/finite? (:beat timeline)) (widgets/finite? (:beat transport))
             (= (transport-key (assoc (:transport timeline) :generation (:transport-generation timeline 0)))
                (transport-key transport))
             (<= 0 (- (:beat transport) (:beat timeline)) (get-in transport [:meter :beats-per-bar] 4)))
    (when-let [{:keys [x-min x-max] :as chart} (chart-data (:samples timeline) :value)]
      (when (and (<= -1.000001 x-min -0.999999) (<= 0.999999 x-max 1.000001))
        (assoc chart :ticks (into [] (comp (filter #(and (widgets/finite? (:position %))
                                                        (<= -1 (:position %) 1)
                                                        (#{"sixteenth" "beat" "bar" "phrase"} (wire (:kind %)))))
                                           (map (fn [{:keys [kind position]}]
                                                  {:kind (wire kind) :x (+ 256 (* position 246))})))
                                  (take 195 (:ticks timeline))))))))

(rf/reg-sub :waveform/live-inputs
  (fn [db [_ id]]
    (let [instance (some #(when (= (wire id) (wire (:id %))) %) (get-in db [:show :instances]))
          overrides (or (get-in db [:show :modulation :overrides id])
                        (get-in db [:show :modulation :overrides (keyword (wire id))]))]
      {:instance instance :params (merge (:params instance) overrides)
       :transport (assoc (get-in db [:show :transport]) :generation (get-in db [:show :transport-generation] 0))
       :clock (when (clock-bound? (get-in instance [:definition :params]))
                [(get-in db [:show :time-ms]) (get-in db [:show :transport]) (:system instance)])
       :waveform-revision (get-in db [:show :waveform-revision])})))

(rf/reg-sub :waveform/entry (fn [db [_ key]] (entry db key)))
(rf/reg-sub :waveform/session (fn [db _] (request-token db)))
(rf/reg-event-db :waveform/toggle
  (fn [db [_ key]] (put-entry db key (update (entry db key) :open? not))))
(rf/reg-event-db :waveform/channel
  (fn [db [_ key channel]]
    (if (some #(= channel (first %)) channels) (put-entry db key (assoc (entry db key) :channel channel)) db)))
(rf/reg-event-db :waveform/setting
  (fn [db [_ key setting value]] (put-entry db key (assoc-in (entry db key) [:settings setting] value))))
(rf/reg-event-db :waveform/deactivate
  (fn [db [_ key]] (put-entry db key (assoc (entry db key) :active? false))))
(rf/reg-event-db :waveform/runtime-change
  (fn [db _] (assoc-in db [:ui :waveform] {:generation (inc (get-in db [:ui :waveform :generation] 0)) :entries {}})))

(defn- schedule [db key]
  (let [e (entry db key)]
    (if (and (:active? e) (:desired e) (not (:in-flight? e)) (not (:scheduled? e))
             (not= (:desired e) (:result-payload e)) (not= (:desired e) (:failed-payload e)))
      {:db (put-entry db key (assoc e :scheduled? true))
       :dispatch-later [{:ms 120 :dispatch [:waveform/send (request-token db) key]}]}
      {:db db})))
(rf/reg-event-fx :waveform/ensure
  (fn [{:keys [db]} [_ key selection options settings]]
    (try
      (let [payload (preview-payload selection options settings)
            e (entry db key)]
        (schedule (put-entry db key (cond-> (assoc e :desired payload :active? true :input-error nil)
                                     (not= payload (:desired e)) (dissoc :error :failed-payload))) key))
      (catch :default error
        {:db (put-entry db key (assoc (entry db key) :desired nil :input-error (events/error-message error)))}))))
(rf/reg-event-fx :waveform/send
  (fn [{:keys [db]} [_ token key]]
    (if (not= token (request-token db)) {}
      (let [e (entry db key) payload (:desired e)]
        (if-not (and (:scheduled? e) (:active? e) payload (not (:in-flight? e)))
          {:db (put-entry db key (assoc e :scheduled? false))}
          (let [request-id (inc (:request-id e))]
            {:db (put-entry db key (assoc e :scheduled? false :in-flight? true :request-id request-id :sent payload))
             :studio/http {:path "/api/waveforms/preview" :body payload
                           :on-success [:waveform/received token key request-id payload]
                           :on-failure [:waveform/failed token key request-id payload]}}))))))
(rf/reg-event-fx :waveform/received
  (fn [{:keys [db]} [_ token key request-id payload data]]
    (if (and (= token (request-token db)) (= request-id (:request-id (entry db key))))
      (let [valid? (every? #(chart-data (:samples data) (first %)) channels)
            previous (entry db key)
            e (cond-> (assoc previous :in-flight? false)
                valid? (assoc :data data :result-payload payload :error nil :failed-payload nil
                              :previous-timeline (when (:timeline data) (get-in previous [:data :timeline])))
                (not valid?) (assoc :error "The server returned invalid waveform samples." :failed-payload payload))]
        (schedule (put-entry db key e) key)) {})))
(rf/reg-event-fx :waveform/failed
  (fn [{:keys [db]} [_ token key request-id payload error]]
    (if (and (= token (request-token db)) (= request-id (:request-id (entry db key))))
      (schedule (put-entry db key (assoc (entry db key) :in-flight? false :error (events/error-message error) :failed-payload payload)) key) {})))
(rf/reg-event-fx :waveform/retry
  (fn [{:keys [db]} [_ key]] (schedule (put-entry db key (dissoc (entry db key) :failed-payload :error)) key)))

(defn- unit-label [unit] (case (wire unit)
                         "amplitude" "Amplitude" "amplitude-per-cycle" "Amplitude / cycle"
                         "degrees-per-cycle" "Degrees / cycle" "amplitude-per-cycle-squared" "Amplitude / cycle²"
                         (widgets/label unit)))

(defn- cycle-x
  "Place a grid line of the sampled cycle window in the chart."
  [line]
  (+ 10 (* 492 (:position line))))

(defn- svg-chart [data channel]
  (when-let [{:keys [points minimum maximum low high zero-y x-min x-max]} (chart-data (:samples data) channel)]
    (let [channel-label (second (first (filter #(= channel (first %)) channels)))
          units (unit-label (get-in data [:units channel]))]
      [:div.waveform-chart
       [:div.waveform-chart-heading [:strong channel-label] [:span units]]
       [:svg {:view-box "0 0 512 184" :preserve-aspect-ratio "none" :role "img"
              :aria-label (str channel-label ", " units ", minimum " (widgets/number-label minimum)
                               ", maximum " (widgets/number-label maximum) ", across " (widgets/number-label (- x-max x-min)) " cycles")}
        [:line {:x1 10 :x2 502 :y1 20 :y2 20 :class "waveform-grid"}]
        [:line {:x1 10 :x2 502 :y1 164 :y2 164 :class "waveform-grid"}]
        [widgets/grid-lines (:grid data) cycle-x 20 164]
        [:line {:x1 10 :x2 502 :y1 zero-y :y2 zero-y :class "waveform-zero"}]
        [:polyline {:points points :fill "none" :stroke "currentColor" :stroke-width 1.8 :vector-effect "non-scaling-stroke"}]]
       (into [:div.waveform-axis [:span (str (widgets/number-label x-min) " cycles")]]
             (remove nil? [(widgets/grid-period (:grid data))
                           [:span (str "Range " (widgets/number-label low) " … " (widgets/number-label high))]
                           [:span (str (widgets/number-label x-max) " cycles")]]))])))

(defn- preview-view [key _selection options settings _token]
  (let [e @(rf/subscribe [:waveform/entry key])
        opts (merge {:cycles 1 :samples 257 :window (/ 1 1024) :angle-span-deg 360} options settings)
        change! #(rf/dispatch-sync [:waveform/setting key %1 (.. %2 -target -value)])
        updating? (or (:in-flight? e) (:scheduled? e))
        stale? (and (:data e) (not= (:desired e) (:result-payload e)))]
    [:div.waveform-preview {:aria-label "Waveform preview" :aria-busy (boolean updating?)}
     [:div.waveform-channels {:role "group" :aria-label "Signal channel"}
      (for [[id label] channels]
        ^{:key id} [:button {:type "button" :aria-pressed (= (:channel e) id)
                             :on-click #(rf/dispatch [:waveform/channel key id])} label])]
     (when-let [error (or (:input-error e) (:error e))]
       [:div.waveform-error {:role "alert"} [:span error]
        (when-not (:input-error e) [:button.text-button {:type "button" :disabled (boolean updating?) :on-click #(rf/dispatch [:waveform/retry key])} "Retry preview"])])
     (if (:data e) [svg-chart (:data e) (:channel e)]
         [:div.waveform-placeholder (if updating? "Sampling the registered waveform…" "No preview samples yet.")])
     [:div.waveform-preview-status
      [:span (cond updating? "Updating…" stale? "Previous preview" (:data e) "Server-sampled signal" :else "")]
      (when (:data e) [:span (str (count (get-in e [:data :samples])) " samples")])]
     [:div.waveform-options
      [:label "Cycles" [:select {:value (str (:cycles opts)) :aria-label "Preview cycles" :on-change #(change! :cycles %)}
                         (for [value [0.25 0.5 1 2 4 8]] ^{:key value} [:option {:value (str value)} value])]]
      [:label "Detail" [:select {:value (str (:samples opts)) :aria-label "Preview sample count" :on-change #(change! :samples %)}
                         (for [value [129 257 513 1025 2048]] ^{:key value} [:option {:value (str value)} (str value " samples")])]]]
     [:details.waveform-advanced
      [:summary "Derivative settings"]
      [:div.waveform-options
       [:label "Window · cycles" [:input {:type "text" :input-mode "decimal" :value (str (:window opts))
                                          :aria-label "Derivative window in cycles" :on-change #(change! :window %)}]]
       [:label "Angular span · degrees" [:input {:type "text" :input-mode "decimal" :value (str (:angle-span-deg opts))
                                                 :aria-label "Angular span in degrees" :on-change #(change! :angle-span-deg %)}]]]
      [:p.small.muted "Slope is measured per cycle. Angular velocity maps amplitude across the chosen angular span. Jagginess is absolute curvature per cycle²; jumps produce window-dependent peaks."]]
     [:p.waveform-caption "Registered waveform and explicit numeric inputs. The chart shows cycle shape; it does not advance the transport or simulate the whole effect."]]))

(defn- ensure-preview! [component]
  (let [[_ key selection options settings] (r/argv component)]
    (rf/dispatch [:waveform/ensure key selection options settings])))

(def preview-component
  (r/create-class
   {:display-name "Registered waveform preview"
    :component-did-mount ensure-preview!
    :component-did-update (fn [component old-argv]
                            (when (not= (rest old-argv) (rest (r/argv component))) (ensure-preview! component)))
    :component-will-unmount (fn [component] (rf/dispatch [:waveform/deactivate (second (r/argv component))]))
    :reagent-render preview-view}))

(defn preview
  "Reusable always-mounted preview. selection is a waveform ID or
  {:selector number :choices [...]}; options are numeric server preview options."
  [key selection & [options]]
  (let [e @(rf/subscribe [:waveform/entry key]) token @(rf/subscribe [:waveform/session])]
    [preview-component key selection (or options {}) (:settings e) token]))

(defn- compact-preview-view [key _selection _options _settings _token]
  (let [e @(rf/subscribe [:waveform/entry key]) data (:data e)
        live @(rf/subscribe [:waveform/live-inputs (first key)])
        timed (when (and (:instance-id (:desired e)) (not (or (:input-error e) (:error e))))
                (or (timeline-chart (:timeline data) (:transport live))
                    (timeline-chart (:previous-timeline e) (:transport live))))
        shape (or timed (chart-data (:samples data) :value))
        current? (= (:desired e) (:result-payload e))]
    [:div.waveform-compact {:aria-label "Live waveform shape" :aria-busy (boolean (or (:scheduled? e) (:in-flight? e)))}
     (if shape
       [:svg {:view-box "0 0 512 184" :preserve-aspect-ratio "none" :role "img"
               :aria-label (str "Resulting waveform, minimum " (widgets/number-label (:minimum shape))
                                ", maximum " (widgets/number-label (:maximum shape))
                                (when timed ", centered on the current beat with beat, bar and phrase marks"))}
        (for [{:keys [x kind]} (:ticks timed)]
          ^{:key x} [:line {:class (str "waveform-tick " kind) :x1 x :x2 x :y1 20 :y2 164}])
        (when-not timed [widgets/grid-lines (:grid data) cycle-x 20 164])
        [:line.waveform-zero {:x1 10 :x2 502 :y1 (:zero-y shape) :y2 (:zero-y shape)}]
        [:polyline {:points (:points shape) :fill "none" :stroke "currentColor" :stroke-width 1.8 :vector-effect "non-scaling-stroke"}]
        (when timed [:line.waveform-now {:x1 256 :x2 256 :y1 14 :y2 172}])]
       [:span (or (:input-error e) (:error e) "Sampling waveform…")])
     (when timed [:span.waveform-now-label "Now"])
     [:span.waveform-compact-status (cond (:input-error e) (:input-error e) (:error e) (:error e) timed "Live waveform"
                                         (not current?) "Updating shape…" :else "Live shape")]]))

(def compact-preview-component
  (r/create-class
   {:display-name "Live waveform shape"
    :component-did-mount ensure-preview!
    :component-did-update (fn [component old-argv]
                           (when (not= (rest old-argv) (rest (r/argv component))) (ensure-preview! component)))
    :component-will-unmount (fn [component] (rf/dispatch [:waveform/deactivate (second (r/argv component))]))
    :reagent-render compact-preview-view}))

(defn picker
  "Always show the actual registered shape. Disclosure adds derivative channels
  and sampling controls without owning a second request stream."
  [instance-id parameter schema value change! & [context]]
  (let [key (picker-key instance-id parameter) e @(rf/subscribe [:waveform/entry key])
        token @(rf/subscribe [:waveform/session]) live @(rf/subscribe [:waveform/live-inputs instance-id])
        choices (vec (:choices schema)) selected (choice-index choices value) choice (get choices selected)
        title (or (:label schema) (:name schema) (widgets/label parameter)) category (wire (:category choice))
        actual? (and (:instance live) (not= false (:live? context)))
        transport (:transport live)
        timing-key (live-timing-key e transport)
        selection (if actual? {:instance-id instance-id :parameter parameter
                               :input-key (str (hash [(:params live) (:definition (:instance live)) choices
                                                      (:waveform-revision live) (:clock live) timing-key
                                                      (get-in live [:instance :system :paused?])]))}
                              {:selector selected :choices choices})
        options (cond-> (:preview/options schema {})
                  actual? (assoc :live-timing? true)
                  (and (not actual?) (:params context)) (assoc :context-params (:params context)))]
    [:div.waveform-picker {:data-waveform-param (wire parameter)}
     [:div.waveform-choice-row
      [:select {:aria-label title :value selected
                 :on-change #(change! (:value (get choices (js/parseInt (.. % -target -value) 10))))}
       (for [[index item] (map-indexed vector choices)]
         ^{:key index} [:option {:value index} (or (:label item) (widgets/label (:id item)))])]
      (when category [:span.waveform-category {:style {"--waveform-category" (get category-colors category "#a8b99a")}}
                      (widgets/label category)])]
     [compact-preview-component key selection options (:settings e) token]
     [:button.waveform-toggle.text-button {:type "button" :aria-expanded (:open? e)
                                           :on-click #(rf/dispatch [:waveform/toggle key])}
      (if (:open? e) "Hide waveform preview" "Preview waveform + derived channels")]
     (when (:open? e) [preview-view key selection options (:settings e) token])]))
