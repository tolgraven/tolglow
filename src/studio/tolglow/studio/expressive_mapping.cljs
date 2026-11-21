(ns tolglow.studio.expressive-mapping
  "Portable mapping drafts and backend-owned previews. No MIDI or semantic mapping in the browser."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.parameter-widgets :as widgets]))

(def device-kinds [["push-3" "Push 3 · MPE"] ["seaboard-m" "Seaboard M"] ["mpe" "Generic MPE"]])
(def axis-names {"strike" "Strike · note velocity" "pressure" "Pressure · aftertouch"
                 "glide" "Glide · pitch bend" "slide" "Slide · CC74" "lift" "Lift · release velocity"})
(def defaults {:device-id "push-main" :cue-id nil :device-kind "push-3" :scope :cue
               :axes ["strike" "pressure" "glide" "slide"] :generation 0 :drafts {} :dirty #{} :numeric {}})
(defn wire-id [id] (if (keyword? id) (subs (str id) 1) (str id)))
(defn wire-data
  "Preserve namespaces in IDs through clj->js, which otherwise uses keyword name."
  [value]
  (cond (map? value) (into {} (map (fn [[k v]] [(if (keyword? k) (wire-id k) k) (wire-data v)]) value))
        (or (sequential? value) (set? value)) (mapv wire-data value)
        (keyword? value) (wire-id value)
        :else value))
(defn state [db] (merge defaults (:expressive-mapping db)))
(defn context [db] (select-keys (:show db) [:runtime-id :epoch]))
(defn saved-devices [db] (get-in db [:show :settings :expressive :devices] {}))
(defn device-policy [db]
  (let [{:keys [device-id drafts]} (state db) id (keyword device-id)]
    (get drafts id (get (saved-devices db) id {}))))
(defn scope-path [s] (if (= :cue (:scope s)) [:cues (keyword (:cue-id s))] []))
(defn scoped-policy [db] (get-in (device-policy db) (scope-path (state db))))
(defn request-token [db]
  (let [s (state db)]
    [(context db) (:generation s) (:device-id s) (:cue-id s) (:instance-id s) (:device-kind s)]))
(defn current? [db token] (and (= (:context (state db)) (context db)) (= token (request-token db))))
(defn open-state [db]
  (let [s (state db) reset? (not= (:context s) (context db))
        device (or (some-> (saved-devices db) keys sort first wire-id) "push-main")
        cue (or (some #(when (= (get-in db [:ui :selected-instance]) (:id %)) (:cue-id %)) (get-in db [:show :instances]))
                (:id (first (get-in db [:show :cues]))))]
    (assoc db :expressive-mapping
           (if reset? (assoc defaults :context (context db) :device-id device :cue-id cue
                            :generation (inc (:generation s)))
               (-> s (assoc :error nil) (update :generation inc))))))
(defn edit-policy [db path value remove?]
  (let [s (state db) id (keyword (:device-id s)) policy (device-policy db)
        policy (if remove?
                 (if (= 1 (count path)) (dissoc policy (first path))
                     (if (contains? (get-in policy (vec (butlast path))) (last path))
                       (update-in policy (vec (butlast path)) dissoc (last path)) policy))
                 (assoc-in policy path value))]
    (assoc db :expressive-mapping
           (-> s (assoc-in [:drafts id] policy) (update :dirty conj id)
               (update :generation inc) (dissoc :error :saved?) (assoc :numeric {})))))
(defn preview-request [db]
  (let [s (state db) token (request-token db)]
    {:path "/api/expressive/preview"
     :body (cond-> {:device-id (:device-id s) :cue-id (:cue-id s) :device-kind (:device-kind s) :policy (wire-data (device-policy db))}
             (= "mpe" (:device-kind s)) (assoc :axes (:axes s))
             (:instance-id s) (assoc :instance-id (:instance-id s)))
     :on-success [:expressive/previewed token] :on-failure [:expressive/failed token :preview]}))
(defn refresh [db]
  (cond-> {:db (assoc-in db [:expressive-mapping :preview-pending?] (some? (:cue-id (state db))))}
    (:cue-id (state db)) (assoc :dispatch-later [{:ms 180 :dispatch [:expressive/preview (request-token db)]}])))
(defn parameter-mode [policy param]
  (if (contains? (:params policy) param) (if (nil? (get-in policy [:params param])) "off" "manual") "inherit"))
(defn bindings [value]
  (cond (vector? value) value (map? value) [value]
        (or (keyword? value) (string? value)) [{:axis (wire-id value)}] :else []))
(defn compound-size [schema]
  (case (keyword (:type schema)) :color 3 :range 2 :vector (:size schema) nil))
(defn mapping-bindings [preview param]
  (mapv #(select-keys % [:axis :component :range :input-range :curve :inverted? :enabled?])
        (filter #(= (wire-id param) (wire-id (:param %))) (:mappings preview))))
(defn manual-value [db param]
  (let [s (state db) schema (get-in s [:preview :schemas param]) existing (mapping-bindings (:preview s) param)
        values (if (seq existing) existing [(cond-> {:axis (or (some-> (:preview s) :axes keys sort first wire-id) "pressure")}
                                                             (compound-size schema) (assoc :component 0))])]
    (if (compound-size schema) values (first values))))
(defn save-request [db]
  {:path "/api/settings"
   :body {:scope "expressive" :values {:devices (wire-data (assoc (saved-devices db) (keyword (:device-id (state db))) (device-policy db)))}}
   :on-success [:expressive/saved (request-token db)] :on-failure [:expressive/failed (request-token db) :save]})
(defn- change-binding [db param index f]
  (let [s (state db) path (into (scope-path s) [:params param]) raw (get-in (device-policy db) path)
        values (bindings raw) values (assoc values index (f (get values index)))]
    (edit-policy db path (if (vector? raw) values (first values)) false)))

(rf/reg-sub :expressive/state (fn [db _] (state db)))
(rf/reg-event-fx :expressive/open (fn [{:keys [db]} _] (refresh (open-state db))))
(rf/reg-event-fx :expressive/sync
  (fn [{:keys [db]} _] (when (not= (:context (state db)) (context db)) (refresh (open-state db)))))
(rf/reg-event-fx :expressive/select
  (fn [{:keys [db]} [_ key value]]
    (when-not (:saving? (state db))
      (refresh (update db :expressive-mapping
                       #(-> (merge defaults %) (assoc key value :numeric {}) (dissoc :preview :error :saved?)
                            (cond-> (#{:cue-id :device-id} key) (dissoc :instance-id)) (update :generation inc)))))))
(rf/reg-event-fx :expressive/add-device
  (fn [{:keys [db]} [_ id]]
    (if (and (string? id) (re-matches #"[A-Za-z0-9][A-Za-z0-9_./:-]{0,127}" id))
      {:dispatch [:expressive/select :device-id id]}
      {:db (assoc-in db [:expressive-mapping :error] "Use a device ID with letters, numbers, hyphens or slashes.")})))
(rf/reg-event-db :expressive/new-id
  (fn [db [_ value]] (assoc-in db [:expressive-mapping :new-id] value)))
(rf/reg-event-fx :expressive/edit
  (fn [{:keys [db]} [_ path value remove?]]
    (when-not (:saving? (state db)) (refresh (edit-policy db path value remove?)))))
(rf/reg-event-fx :expressive/mode
  (fn [{:keys [db]} [_ param mode]]
    (when-not (:saving? (state db))
      (refresh (edit-policy db (into (scope-path (state db)) [:params param])
                            (when (= "manual" mode) (manual-value db param)) (= "inherit" mode))))))
(rf/reg-event-fx :expressive/binding
  (fn [{:keys [db]} [_ param index field value remove?]]
    (when-not (:saving? (state db))
      (refresh (change-binding db param index #(if remove? (dissoc % field) (assoc % field value)))))))
(rf/reg-event-fx :expressive/component
  (fn [{:keys [db]} [_ param index]]
    (when-not (:saving? (state db))
      (let [s (state db) path (into (scope-path s) [:params param]) values (bindings (get-in (device-policy db) path))
            n (compound-size (get-in s [:preview :schemas param])) used (set (map :component values))
            component (first (remove used (range n)))
            next (if (some? index) (vec (concat (subvec values 0 index) (subvec values (inc index))))
                     (cond-> values (some? component) (conj {:axis (or (:axis (first values)) "pressure") :component component})))]
        (refresh (edit-policy db path (when (seq next) next) false))))))
(rf/reg-event-db :expressive/number-edit
  (fn [db [_ key value]] (assoc-in db [:expressive-mapping :numeric key] value)))
(rf/reg-event-fx :expressive/number-commit
  (fn [{:keys [db]} [_ param index field endpoint original key]]
    (let [s (state db) raw (get-in s [:numeric key]) value (js/Number raw)]
      (when (some? raw)
        (if (and (not (str/blank? raw)) (js/Number.isFinite value))
          (refresh (change-binding db param index #(assoc % field (assoc (vec original) endpoint value))))
          {:db (assoc-in db [:expressive-mapping :error] "Enter a finite number for each range endpoint.")})))))
(rf/reg-event-fx :expressive/preview
  (fn [{:keys [db]} [_ token]]
    (when (and (current? db token) (:cue-id (state db))) {:studio/http (preview-request db)})))
(rf/reg-event-db :expressive/previewed
  (fn [db [_ token result]]
    (if (current? db token)
      (update db :expressive-mapping #(-> % (assoc :preview result :preview-pending? false) (dissoc :error))) db)))
(rf/reg-event-db :expressive/failed
  (fn [db [_ token kind error]]
    (if (current? db token)
      (update db :expressive-mapping #(-> % (assoc :error (events/error-message error) :preview-pending? false)
                                          (cond-> (= kind :save) (assoc :saving? false)))) db)))
(rf/reg-event-fx :expressive/save
  (fn [{:keys [db]} _]
    (when (and (= (:context (state db)) (context db)) (not (:saving? (state db))))
      (if (seq (:numeric (state db)))
        {:db (assoc-in db [:expressive-mapping :error] "Finish editing the range before saving.")}
        {:db (assoc-in db [:expressive-mapping :saving?] true) :studio/http (save-request db)}))))
(rf/reg-event-fx :expressive/saved
  (fn [{:keys [db]} [_ token ack]]
    (when (current? db token)
      (if (or (false? (:accepted? ack)) (= "rejected" (:status ack)))
        {:db (update db :expressive-mapping assoc :saving? false :error (events/error-message ack))}
        {:db (update db :expressive-mapping #(-> % (assoc :saving? false :saved? true) (update :dirty disj (keyword (:device-id %)))))
         :dispatch [:studio/poll]}))))
(rf/reg-event-fx :expressive/discard
  (fn [{:keys [db]} _]
    (when-not (:saving? (state db))
      (let [id (keyword (:device-id (state db)))]
        (refresh (update db :expressive-mapping #(-> % (update :drafts dissoc id) (update :dirty disj id)
                                                    (update :generation inc) (assoc :numeric {}) (dissoc :error :saved? :preview))))))))

(defn- text [event] (.. event -target -value))
(defn- checked [event] (.. event -target -checked))
(defn- dispatch! [event] (rf/dispatch-sync event))
(defn- identifier-label [id] (widgets/label (wire-id id)))
(defn- axis-label [axis] (get axis-names (wire-id axis) (identifier-label axis)))
(defn- select-control [label value options on-change attrs]
  [:label.expressive-field [:span label]
   [:select (merge {:value value :on-change #(on-change (text %))} attrs)
    (for [[id title] options] ^{:key id} [:option {:value id} title])]])
(defn- toggle [label enabled? change attrs]
  [:label.expressive-toggle [:input (merge {:type "checkbox" :checked enabled? :on-change #(change (checked %))} attrs)] [:span label]])
(defn- range-field [s param index field values]
  [:div.expressive-range
   (for [endpoint [0 1] :let [key [(:scope s) (:cue-id s) param index field endpoint]]]
     ^{:key endpoint}
     [:label.expressive-field [:span (str (if (= :range field) "Output " "Input ") (if (zero? endpoint) "start" "end"))]
      [:input {:type "number" :step "any" :input-mode "decimal"
               :aria-label (str (identifier-label param) " " (name field) " " endpoint)
               :value (get (:numeric s) key (get values endpoint))
               :on-change #(dispatch! [:expressive/number-edit key (text %)])
               :on-blur #(dispatch! [:expressive/number-commit param index field endpoint values key])}]])])
(defn- binding-editor [s param schema index binding]
  (let [preview (:preview s) axis (wire-id (:axis binding)) axes (:axes preview)
        source-range (get-in axes [(keyword axis) :range]) n (compound-size schema)
        output-range (or (:range binding) (when (= "color" (:type schema)) [0 1]) (:bounds schema))]
    [:div.expressive-binding
     [select-control "Input" axis (mapv (fn [axis] [(wire-id axis) (axis-label axis)]) (sort (keys axes)))
      #(dispatch! [:expressive/binding param index :axis %]) {:aria-label (str (identifier-label param) " input " index)}]
     (when n [select-control "Component" (str (:component binding))
              (mapv (fn [i] [(str i) (if (= "color" (:type schema)) (nth ["Red" "Green" "Blue"] i) (str "Component " (inc i)))]) (range n))
              #(dispatch! [:expressive/binding param index :component (js/parseInt % 10)]) {}])
     [select-control "Response curve" (wire-id (:curve binding "linear")) (mapv #(vector % (widgets/label %)) (:curves preview))
      #(dispatch! [:expressive/binding param index :curve %]) {:aria-label (str (identifier-label param) " curve " index)}]
     [toggle "Invert" (true? (:inverted? binding)) #(dispatch! [:expressive/binding param index :inverted? %]) {}]
     (when source-range [range-field s param index :input-range (or (:input-range binding) source-range)])
     (when (and output-range (not (#{"boolean" "enum"} (:type schema)))) [range-field s param index :range output-range])
     [:div.expressive-binding-actions
      (when (or (:range binding) (:input-range binding))
        [:button.text-button {:type "button" :on-click #(do (dispatch! [:expressive/binding param index :range nil true])
                                                           (dispatch! [:expressive/binding param index :input-range nil true]))} "Reset ranges"])
      (when n [:button.text-button {:type "button" :on-click #(dispatch! [:expressive/component param index])} "Remove component"])]]))
(defn- parameter-row [db s param schema]
  (let [preview (:preview s) policy (scoped-policy db) mode (parameter-mode policy param)
        mappings (filter #(= (wire-id param) (wire-id (:param %))) (:mappings preview))
        reasons (filter #(= (wire-id param) (wire-id (:param %))) (concat (:disabled-reasons preview) (:diagnostics preview)))
        status (cond (and (seq mappings) (seq reasons)) "Partial" (seq reasons) "Off" (some #(= "explicit" (:origin %)) mappings) "Manual" (seq mappings) "Automatic" :else "Unmapped")
        values (bindings (get-in policy [:params param]))
        samples (keep #(get-in % [:values param]) (:samples preview))]
    [:article.expressive-param {:data-param (wire-id param)}
     [:div.expressive-param-heading
      [:div [:h3 (or (:name schema) (identifier-label param))]
       [:span.small.muted (str (widgets/label (:type schema)) (when (:bounds schema) (str " · " (str/join " – " (:bounds schema)))))]]
      [:span.expressive-badge {:data-status (str/lower-case status)} status]
      [select-control "Mapping" mode [["inherit" (if (= :cue (:scope s)) "Inherit / automatic" "Automatic")]
                                      ["manual" "Manual mapping"] ["off" "Off"]]
       #(dispatch! [:expressive/mode param %]) {:aria-label (str (identifier-label param) " mapping mode")}]]
     (when (seq mappings)
       [:div.expressive-assignment
        [:span (str/join " + " (map #(axis-label (:axis %)) mappings))]
        (when (every? number? samples) [widgets/sparkline samples])
        [:span.small.muted (str/join " → " (map #(widgets/value-label schema %) [(first samples) (last samples)]))]])
     (for [[i reason] (map-indexed vector reasons)] ^{:key i}
       [:p.expressive-reason (case (:code reason)
                              "policy-disabled" "Expression is disabled by this device or cue. Notes still launch and release cues."
                              "parameter-disabled" "This parameter is disabled by its mapping policy."
                              "axis-disabled" (str (axis-label (:axis reason)) " is disabled by this device or cue.")
                              "unavailable-axis" "This input is not available for the chosen controller mode."
                              "unsupported-schema" "This mapping needs a supported parameter range or component."
                              "invalid-range" "Use input endpoints within the axis range and output endpoints within the parameter bounds."
                              (widgets/label (:code reason)))])
     (when (= "manual" mode)
       [:div.expressive-bindings
        (for [[i binding] (map-indexed vector values)] ^{:key i} [binding-editor s param schema i binding])
        (when (and (compound-size schema) (< (count values) (compound-size schema)))
          [:button.text-button {:type "button" :on-click #(dispatch! [:expressive/component param nil])} "+ Map another component"])])]))

(defn- panel-content [show]
  (let [s @(rf/subscribe [:expressive/state]) db {:show show :expressive-mapping s}
        devices (sort (set (conj (map wire-id (concat (keys (saved-devices db)) (keys (:drafts s)))) (:device-id s))))
        policy (device-policy db) scoped (scoped-policy db) preview (:preview s)
        scope (scope-path s) dirty? (contains? (:dirty s) (keyword (:device-id s)))
        instances (filter #(= (:cue-id %) (:cue-id s)) (:instances show))]
    [:section#expressive-mapping.panel.expressive-panel {:aria-labelledby "expressive-title"}
     [:div.panel-top [:div [:span.section-label "EXPRESSIVE CONTROLLERS"] [:h2#expressive-title "Make every touch count"]]
      [:span.expressive-badge (cond (:saving? s) "Saving…" dirty? "Unsaved draft" (:saved? s) "Saved" :else "Shared mappings")]]
     [:p.expressive-intro "Map pressure, pitch and slide to lighting. Preview the same rules used by Push 3 and Seaboard, then save them with your show."]
     [:fieldset {:disabled (:saving? s)}
      [:div.expressive-selection
       [select-control "Device profile" (:device-id s) (mapv #(vector % %) devices) #(dispatch! [:expressive/select :device-id %]) {:id "expressive-device"}]
       [select-control "Controller mode" (if (= "connected" (:profile-source preview)) (:device-kind preview) (:device-kind s)) device-kinds
        #(dispatch! [:expressive/select :device-kind %]) {:id "expressive-kind" :disabled (= "connected" (:profile-source preview))}]
       [select-control "Preview cue" (or (:cue-id s) "") (mapv #(vector (:id %) (or (:name %) (:id %))) (:cues show))
        #(dispatch! [:expressive/select :cue-id %]) {:id "expressive-cue"}]
       [select-control "Edit mappings for" (name (:scope s)) [["cue" "This cue"] ["device" "All cues on device"]]
        #(dispatch! [:expressive/select :scope (keyword %)]) {:id "expressive-scope"}]]
      [:details.expressive-device-add [:summary "Add a device profile"]
       [:form {:on-submit #(do (.preventDefault %) (rf/dispatch [:expressive/add-device (:new-id s)]))}
        [:label.expressive-field [:span "Device ID used by the controller connection"]
         [:input {:value (:new-id s "") :max-length 128 :placeholder "push-main" :on-change #(dispatch! [:expressive/new-id (text %)])}]]
        [:button.button {:type "submit"} "Use device ID"]]]
      [toggle "Device expression enabled" (not (false? (:enabled? policy))) #(dispatch! [:expressive/edit [:enabled?] %]) {:id "expressive-device-enabled"}]
      [:p.small.muted "Disabling expression keeps note launches and releases working. Saving a profile does not open a MIDI device."]
      [:div.expressive-policy
       (if (= :cue (:scope s))
         [select-control "Cue expression" (if (contains? scoped :enabled?) (str (:enabled? scoped)) "inherit")
          [["inherit" "Inherit device"] ["true" "Enabled"] ["false" "Disabled"]]
          #(dispatch! [:expressive/edit (conj scope :enabled?) (= "true" %) (= "inherit" %)]) {:id "expressive-cue-enabled"}]
         [:span.small.muted "Device defaults apply to every cue unless a cue overrides them."])
       [select-control "Automatic mapping" (if (contains? scoped :auto?) (str (:auto? scoped)) "inherit")
        [["inherit" (if (= :cue (:scope s)) "Inherit device" "Enabled by default")] ["true" "Enabled"] ["false" "Disabled · manual only"]]
        #(dispatch! [:expressive/edit (conj scope :auto?) (= "true" %) (= "inherit" %)]) {:id "expressive-auto"}]
       [select-control "Parameter version" (or (:instance-id s) "")
        (into [["" "Cue definition"]] (map #(vector (:id %) (str "Running " (:id %) " · v" (:effect-version %))) instances))
        #(dispatch! [:expressive/select :instance-id (when (seq %) %)]) {:id "expressive-instance"}]]
      (when (and (= "mpe" (:device-kind s)) (not= "connected" (:profile-source preview)))
        [:div.expressive-axes [:span.expressive-axis-title "Documented inputs on this generic MPE connection"]
         (for [[axis title] (sort axis-names)] ^{:key axis}
           [toggle title (boolean (some #{axis} (:axes s)))
            #(dispatch! [:expressive/select :axes (vec (if % (conj (set (:axes s)) axis) (disj (set (:axes s)) axis)))])])])
      (when (seq (:axes preview))
        [:details.expressive-axis-details [:summary "Input availability and axis switches"]
         [:p.small.muted "Glide is pitch bend and slide is CC74. These are not calibrated native Push pad X/Y coordinates. Hardware calibration is separate."]
         [:div.expressive-axes
          (for [[axis {:keys [range]}] (sort-by key (:axes preview))
                :let [own (set (map wire-id (:disabled-axes scoped))) disabled? (contains? own (wire-id axis))
                      inherited? (and (= :cue (:scope s)) (contains? (set (map wire-id (:disabled-axes policy))) (wire-id axis)))]]
            ^{:key axis}
            [toggle (str (axis-label axis) " · " (str/join "…" range) (when inherited? " · disabled on device"))
             (not (or disabled? inherited?))
             #(dispatch! [:expressive/edit (conj scope :disabled-axes) (vec (if % (disj own (wire-id axis)) (conj own (wire-id axis))))]) {:disabled inherited?}])]])
      (when (= "connected" (:profile-source preview)) [:p.small.muted "Using the connected controller’s actual inputs and profile defaults."])
      [:div.expressive-preview-header
       [:span.small.muted (if preview (str "Effect " (:effect-id preview) " · version " (:effect-version preview) " · " (count (:mappings preview)) " active mappings") "Choose a cue to preview mappings.")]
       [:button.text-button {:type "button" :disabled (nil? (:cue-id s)) :on-click #(dispatch! [:expressive/open])} "Refresh preview"]]
      (when (:preview-pending? s) [:p.small.muted {:role "status"} "Checking mapping plan…"])
      (when (:error s) [:p.expressive-error {:role "alert"} (:error s)])
      (when (and preview (empty? (:schemas preview))) [:p.muted "This cue has no exposed parameters."])
      [:div.expressive-parameters
       (for [[param schema] (sort-by key (:schemas preview))] ^{:key param} [parameter-row db s param schema])]
      [:div.expressive-save
       [:button#expressive-save.button.primary {:type "button" :disabled (or (not dirty?) (seq (:numeric s))) :on-click #(rf/dispatch [:expressive/save])} (if (:saving? s) "Saving…" "Save device mappings")]
       [:button.button {:type "button" :disabled (not dirty?) :on-click #(rf/dispatch [:expressive/discard])} "Discard draft"]
       (when (= :cue (:scope s)) [:button.text-button {:type "button" :on-click #(dispatch! [:expressive/edit [:cues (keyword (:cue-id s))] nil true])} "Reset cue to device defaults"])
       [:span.small.muted "Saved in show files · applies to this device’s next expressive event"]]]]))

(def panel
  (r/create-class
    {:display-name "ExpressiveMappingSettings"
     :component-did-mount (fn [_] (rf/dispatch [:expressive/open]))
     :component-did-update (fn [this old-argv]
                             (when (not= (select-keys (second old-argv) [:runtime-id :epoch])
                                         (select-keys (second (r/argv this)) [:runtime-id :epoch]))
                               (rf/dispatch [:expressive/sync])))
     :reagent-render panel-content}))
