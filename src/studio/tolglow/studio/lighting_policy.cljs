(ns tolglow.studio.lighting-policy
  "Live controls for independent output budgets, optical response and perceived activity."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.lighting-response :as response-editor]
            [tolglow.studio.lighting-scenes :as scenes]
            [tolglow.studio.projection :as projection]))

(defn- view [db] (get-in db [:server-projection :view]))

(defn form [db]
  (let [identity (projection/identity-of (view db))
        saved (:lighting-policy-ui db)]
    (if (= identity (:identity saved)) saved {:identity identity})))

(rf/reg-sub ::form (fn [db _] (form db)))
(rf/reg-sub ::online (fn [db _] (true? (get-in db [:connection :connected?]))))
(defn visible-settings [db section]
  (let [server (view db) ui (form db) edit (:submitted ui)
        settings (get-in server [:settings section])]
    (if (and (= section (:section edit))
             (or (:pending ui) (= (:revision server) (:revision edit))))
      (reduce-kv (fn [updated key value]
                   (case key
                     :global (update updated key merge value)
                     (:targets :target-response :hdr :signals :cue-gains :instance-gains :source-gains) (update updated key merge value)
                     :groups (update updated key
                                     #(reduce-kv (fn [m id rule]
                                                   (if (nil? rule) (dissoc m id) (update m id merge rule))) % value))
                     (assoc updated key value)))
                 settings (:values edit))
      settings)))
(rf/reg-sub ::settings (fn [db [_ section]] (visible-settings db section)))
(rf/reg-sub ::output (fn [db [_ section]] (get-in (view db) [:output section])))
(rf/reg-sub ::audio (fn [db _] (:audio/energy (view db))))
(rf/reg-sub ::scope-fixtures :<- [::scenes/fixtures] :<- [::scenes/members]
  (fn [[fixtures groups] [_ location]]
    (if (= location [:global]) fixtures
      (let [id (second location)
            saved (some #(when (= id (:group-id %)) (set (:fixture-ids %))) groups)]
        (filterv (fn [fixture]
                   (if saved (contains? saved (:id fixture)) (some #{id} (:groups fixture)))) fixtures)))))
(rf/reg-sub ::group-ids :<- [::scenes/fixtures] :<- [::scenes/members]
  (fn [[fixtures groups] _]
    (sort-by (juxt events/wire-id keyword?)
             (into (set (mapcat :groups fixtures)) (map :group-id) groups))))

(rf/reg-event-db ::choose
  (fn [db [_ location]] (assoc db :lighting-policy-ui (assoc (form db) :location location :error nil))))

(rf/reg-event-db ::filter-priorities
  (fn [db [_ text]] (assoc db :lighting-policy-ui (assoc (form db) :fixture-filter text))))

(rf/reg-event-db ::edit
  (fn [db [_ section location field value]]
    (assoc db :lighting-policy-ui
           (-> (form db) (assoc-in [:drafts [section location] field] value) (dissoc :error)))))

(rf/reg-event-db ::edit-stage
  (fn [db [_ location stage stored changes]]
    (let [path [:drafts [:output-response location] stage]]
      (assoc db :lighting-policy-ui
             (-> (form db)
                 (update-in path #(merge (response-editor/stage-form stored) % changes))
                 (dissoc :error))))))

(defn- checked-count [value]
  (when-not (= "" value)
    (let [n (js/Number value)]
      (when-not (and (js/Number.isSafeInteger n) (<= 0 n))
        (throw (js/Error. "Enter a whole number of active sources or leave the limit empty.")))
      n)))

(defn patch-values
  "Encode only the edited controls, leaving sibling group budgets and bound durations alone."
  [section location changes]
  (let [fields (case section
                 :lighting-budget (cond-> changes
                                    (contains? changes :max-active) (update :max-active checked-count))
                 :output-response (update-vals changes response-editor/stage-duration)
                 changes)]
    (assoc-in {} location fields)))

(defn save-request [db section values draft-key]
  (let [ui (form db)
        {:keys [runtime-id epoch]} (view db)
        token [(:identity ui) (str (random-uuid))]]
    (when (and runtime-id (get-in db [:connection :connected?]) (nil? (:pending ui)))
      {:db (assoc db :lighting-policy-ui (assoc ui :pending token :error nil
                                                 :submitted {:section section :values values :revision (:revision (view db))}))
       :studio/http {:path "/api/settings" :typed? true
                     :body {:scope section :values values :expected-epoch epoch :runtime-id runtime-id}
                     :on-success [::saved token draft-key (get-in ui [:drafts draft-key])]
                     :on-failure [::failed token]}})))

(rf/reg-event-fx ::send
  (fn [{:keys [db]} [_ section values draft-key]]
    (or (save-request db section values draft-key) {})))

(rf/reg-event-fx ::apply
  (fn [{:keys [db]} [_ section location]]
    (let [key [section location] ui (form db) draft (get-in ui [:drafts key])]
      (try (if (seq draft) (or (save-request db section (patch-values section location draft) key) {}) {})
           (catch js/Error e {:db (assoc db :lighting-policy-ui (assoc ui :error (.-message e)))})))))

(rf/reg-event-db ::saved
  (fn [db [_ token key submitted _]]
    (let [ui (form db)]
      (if (= token (:pending ui))
        (assoc db :lighting-policy-ui (cond-> (dissoc ui :pending)
                                       (and key (= submitted (get-in ui [:drafts key]))) (update :drafts dissoc key)))
        db))))

(rf/reg-event-db ::failed
  (fn [db [_ token error]]
    (let [ui (form db)]
      (if (= token (:pending ui)) (assoc db :lighting-policy-ui (-> ui (dissoc :pending :submitted) (assoc :error (str error)))) db))))

(defn- percent [n] (if (number? n) (str (js/Math.round (* n 100)) "%") "—"))
(defn- watt-label [n]
  (if (number? n) (str (/ (js/Math.round (* n 10)) 10) " W") "—"))

(defn- edit! [section location field value]
  (rf/dispatch-sync [::edit section location field value]))

(defn- toggle [section label enabled? disabled?]
  [:label.policy-toggle
   [:input {:type "checkbox" :disabled disabled? :checked (boolean enabled?)
            :on-change #(rf/dispatch [::send section {:enabled? (.. % -target -checked)}])}]
   label])

(defn- percent-input [label value on-change]
  [:label.scene-target [:span label [:output (percent value)]]
   [:input {:type "range" :value (js/Math.round (* 100 value)) :min 0 :max 100 :step 1
            :aria-label label :aria-valuetext (percent value)
            :on-change #(on-change (/ (js/Number (.. % -target -value)) 100))}]])

(defn- apply-buttons [section location changed? override? title]
  [:div.performance-actions
   [:button.button {:type "button" :disabled (not changed?) :on-click #(rf/dispatch [::apply section location])} title]
   (when (and (= :groups (first location)) override?)
     [:button.button {:type "button" :on-click #(rf/dispatch [::send section (assoc-in {} location nil) [section location]])}
      "Remove group override"])])

(defn- budget-reading [location]
  (let [reading (get-in @(rf/subscribe [::output :lighting-budget]) location)]
    (when reading
      [:dl.policy-readings
       [:div [:dt "Requested"] [:dd (watt-label (:requested-watts reading))]]
       [:div [:dt "Allocated / limit"] [:dd (str (watt-label (:used-watts reading)) " / " (watt-label (:limit-watts reading)))]]
       [:div [:dt "Active / shed"] [:dd (str (:active-count reading) " / " (:shed-count reading))]]
       (when (pos? (:unrated-count reading 0))
         [:div [:dt "Unrated sources"] [:dd (:unrated-count reading)]])])))

(defn- fixture-priority [location order edit]
  (let [query (:fixture-filter @(rf/subscribe [::form]) "")
        fixtures @(rf/subscribe [::scope-fixtures location])
        selected (set order)
        available (into [] (comp (remove #(contains? selected (:id %)))
                                 (filter #(str/includes? (str/lower-case (str (:name %) " " (events/wire-id (:id %))))
                                                         (str/lower-case query)))
                                 (take 24)) fixtures)]
    [:details.source-readings.priority-order
     [:summary (str "Fixture priority" (when (seq order) (str " · " (count order))))]
     (if (seq order)
       [:ol (for [[idx id] (map-indexed vector order)]
              ^{:key (pr-str id)}
              [:li [:span (events/wire-id id)]
               [:button.button {:type "button" :disabled (zero? idx)
                                :aria-label (str "Raise priority of " (events/wire-id id))
                                :on-click #(edit :fixture-order
                                                 (assoc order idx (nth order (dec idx)) (dec idx) id))}
                "↑"]
               [:button.button {:type "button" :aria-label (str "Remove priority for " (events/wire-id id))
                                :on-click #(edit :fixture-order (filterv (fn [entry] (not= entry id)) order))}
                "×"]])]
       [:p.small.muted "Fixtures follow patch order until you prioritize a source."])
     [:div.policy-grid
      [:label "Find source" [:input {:type "search" :value query
                                     :on-change #(rf/dispatch-sync [::filter-priorities (.. % -target -value)])}]]
      [:label "Prioritize source"
       [:select {:value "" :on-change #(let [key (.. % -target -value)]
                                         (when-let [fixture (some (fn [f] (when (= (pr-str (:id f)) key) f)) available)]
                                           (edit :fixture-order (conj (vec order) (:id fixture)))))}
        [:option {:value ""} "Choose a source…"]
        (for [{:keys [id name]} available]
          ^{:key (pr-str id)} [:option {:value (pr-str id)} (or name (events/wire-id id))])]]]]))

(defn- budget [location disabled?]
  (let [scope :lighting-budget
        settings @(rf/subscribe [::settings scope])
        draft (get-in @(rf/subscribe [::form]) [:drafts [scope location]])
        saved (get-in settings location)
        controls (merge {:limit 1.0 :strategy :proportional :standby-level 0.0 :standby-mode :shed} saved draft)
        edit #(edit! scope location %1 %2)]
    [:section.output-policy {:aria-label "Light output budget" :data-location (pr-str location)}
     [:header [:h3 "Output budget"] [toggle scope "Enable budgets" (:enabled? settings) disabled?]]
     [budget-reading location]
     [:fieldset {:disabled disabled?}
      [:div.policy-grid
       [percent-input "Emitter power budget" (:limit controls) #(edit :limit %)]
       [:label "Maximum active sources"
        [:input {:type "number" :min 0 :step 1 :input-mode "numeric"
                 :placeholder "Unlimited" :value (or (:max-active controls) "")
                 :on-change #(edit :max-active (.. % -target -value))}]]
       [percent-input "Standby brightness" (:standby-level controls) #(edit :standby-level %)]
       [:label "When over budget"
        [:select {:value (name (:strategy controls))
                  :on-change #(edit :strategy (keyword (.. % -target -value)))}
         [:option {:value "proportional"} "Dim sources proportionally"]
         [:option {:value "priority"} "Keep higher-priority sources"]]]
       [:label "Standby behavior"
        [:select {:value (name (:standby-mode controls))
                  :on-change #(edit :standby-mode (keyword (.. % -target -value)))}
         [:option {:value "shed"} "Dim shed sources"]
         [:option {:value "all"} "Standby glow on idle sources"]]]]
      (when (= :priority (:strategy controls)) [fixture-priority location (:fixture-order controls []) edit])
      [apply-buttons scope location (seq draft) (some? saved) "Apply budget"]]]))

(defn- response [location disabled?]
  (let [scope :output-response settings @(rf/subscribe [::settings scope])
        saved (get-in settings location)
        effective (merge (:global settings) saved)
        draft (get-in @(rf/subscribe [::form]) [:drafts [scope location]])]
    [:section.output-policy {:aria-label "Output response envelope"}
     [:header [:h3 "Response envelope"] [toggle scope "Enable envelopes" (:enabled? settings) disabled?]]
     [:fieldset {:disabled disabled?}
      [:div.policy-grid
       (for [stage [:attack :release]]
         ^{:key stage} [response-editor/stage-field stage (get effective stage) (get draft stage)
                        #(rf/dispatch-sync [::edit-stage location stage (get effective stage) %])])]
      [apply-buttons scope location (seq draft) (some? saved) "Apply response"]]]))

(defn- activity [disabled?]
  (let [scope :lighting-activity
        settings @(rf/subscribe [::settings scope])
        {:keys [scene effects]} @(rf/subscribe [::output scope])
        audio @(rf/subscribe [::audio])]
    [:section.output-policy {:aria-label "Visual activity"}
     [:header [:h3 "Visual activity"] [toggle scope "Meter activity" (:enabled? settings) disabled?]]
     [:dl.policy-readings
      [:div [:dt "Music energy"] [:dd (percent audio)]]
      [:div [:dt "Visual activity"] [:dd (percent (:score scene))]]
      (when scene [:div [:dt "Lit coverage"] [:dd (percent (:coverage scene))]])]
     (when scene
       [:details.source-readings
        [:summary "Activity contributors"]
        [:dl.policy-readings
         (for [[field label] [[:brightness "Brightness"] [:flashing "Flashing"] [:movement "Movement"] [:strobe "Strobe"]]]
           ^{:key field} [:div [:dt label] [:dd (percent (get scene field))]])]
        (when (seq effects)
          [:ul.policy-effects
           (for [[id measurement] (take 12 (sort-by (comp - :score val) effects))]
             ^{:key (pr-str id)} [:li [:span (events/wire-id id)] [:strong (percent (:score measurement))]])])])]))

(defn panel []
  (let [ui @(rf/subscribe [::form]) ids @(rf/subscribe [::group-ids])
        locations (into [[:global]] (map #(vector :groups %) ids))
        location (if (some #{(:location ui)} locations) (:location ui) [:global])
        disabled? (boolean (or (:pending ui) (not @(rf/subscribe [::online]))))]
    [:section.scene-controls.panel {:aria-label "Lighting policies"}
     [:header [:h2 "Lighting controls"]]
     [activity disabled?]
     [:label.policy-scope "Budget and envelope scope"
      [:select {:value (pr-str location) :disabled disabled?
                :on-change #(let [selected (.. % -target -value)]
                              (when-let [match (some (fn [candidate] (when (= (pr-str candidate) selected) candidate)) locations)]
                                (rf/dispatch-sync [::choose match])))}
       (for [entry locations]
         ^{:key (pr-str entry)} [:option {:value (pr-str entry)}
                                 (if (= entry [:global]) "Entire rig" (events/wire-id (second entry)))])]]
     [budget location disabled?] [response location disabled?]
     (when-let [error (:error ui)] [:p.performance-error {:role "alert"} error])]))
