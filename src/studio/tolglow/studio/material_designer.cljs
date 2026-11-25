(ns tolglow.studio.material-designer
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.signal-picker :as signal]))

(def ^:private path [:ui ::designer])

(def ^:private initial-preset :optics/through-water)

(def ^:private custom-material
  {:name "Travelling ripple" :mode :multiply :inputs {:depth 0.35 :phase {:op :bar}}
   :glsl "1.0 - depth * (0.5 + 0.5 * cos(6.2831853 * (position.x + position.y - phase)))"})

(defn material-id [s]
  (when (and (string? s) (<= (count s) 96)
             (re-matches #"[A-Za-z][A-Za-z0-9._-]*(/[A-Za-z][A-Za-z0-9._-]*)?" s))
    (keyword s)))

(defn- slug [id] (subs (str id) 1))

(defn- owner [db] (select-keys (:show db) [:runtime-id :show-id :epoch]))

(defn- current? [db token field]
  (let [form (get-in db path)]
    (and (= (:owner form) (owner db)) (= token (get form field)))))

(defn- editing [form id material]
  (assoc form :id (if id (slug id) "") :expected (when id material) :draft material
         :beat 0.0 :dry 1.0 :error nil :notice nil :confirm-delete? false :preview nil :sample-token nil :preview-note nil :preview-error nil))

(rf/reg-sub ::state (fn [db _] (get-in db path)))
(rf/reg-event-db ::close (fn [db _] (update db :ui dissoc ::designer)))
(rf/reg-event-fx ::open
  (fn [{:keys [db]} _]
    (let [token (random-uuid)]
      {:db (assoc-in db path {:owner (owner db) :load-token token})
       :studio/http {:path "/api/materials" :typed? true :raw-response? true
                     :on-success [::loaded token] :on-failure [::failed token :load-token]}})))

(rf/reg-event-fx ::loaded
  (fn [{:keys [db]} [_ token {:keys [materials presets]}]]
    (when (current? db token :load-token)
      (let [form (assoc (get-in db path) :materials materials :presets (assoc presets :shader/custom custom-material))
            [id saved] (first (sort-by (comp str key) materials))]
        {:db (assoc-in db path (editing form id (or saved (get presets initial-preset))))
         :dispatch [::schedule]}))))

(rf/reg-event-fx ::choose
  (fn [{:keys [db]} [_ kind id]]
    (let [form (get-in db path)]
      {:db (assoc-in db path (editing form (when (= kind :materials) id) (get-in form [kind id])))
       :dispatch [::schedule]})))

(rf/reg-event-db ::duplicate
  (fn [db _]
    (update-in db path (fn [form]
                         (-> form
                             (assoc :id (str (:id form) "-copy") :expected nil :confirm-delete? false :notice nil :error nil)
                             (update-in [:draft :name] str " copy"))))))

(rf/reg-event-fx ::set
  (fn [{:keys [db]} [_ field value]]
    (let [edit-path (into path field)]
      (cond-> {:db (-> db (assoc-in edit-path value) (assoc-in (conj path :notice) nil))}
        (#{:draft :beat :dry} (first field)) (assoc :dispatch [::schedule])))))

(rf/reg-event-fx ::schedule
  (fn [{:keys [db]} _]
    (let [token (random-uuid)]
      {:db (update-in db path assoc :sample-token token :sampling? true)
       :dispatch-later [{:ms 220 :dispatch [::sample token]}]})))

(rf/reg-event-fx ::sample
  (fn [{:keys [db]} [_ token]]
    (when (current? db token :sample-token)
      (let [{:keys [draft beat dry]} (get-in db path)]
        {:studio/http {:path "/api/materials/preview" :typed? true :raw-response? true
                       :body {:beat beat :dry dry :material draft}
                       :on-success [::sampled token] :on-failure [::failed token :sample-token]}}))))

(defn rgba [pixels]
  (into [] (mapcat #(conj % 255)) pixels))

(defn- paint-plane! [canvas preview]
  (when (and canvas preview)
    (let [{:keys [columns rows rgb]} preview
          context (.getContext canvas "2d")
          pixels (.createImageData context columns rows)]
      (.set (.-data pixels) (to-array (rgba rgb)))
      (.putImageData context pixels 0 0))))

(defn- sample-plane [preview]
  [:canvas.material-field {:role "img" :width (:columns preview 40) :height (:rows preview 18)
                           :aria-label "Spatial sample of material output over a four by two metre plane"
                           :ref #(paint-plane! % preview)}])

(rf/reg-event-db ::sampled
  (fn [db [_ token result]]
    (if (current? db token :sample-token)
      (update-in db path #(cond-> (assoc % :sampling? false :preview-error nil :preview-note (:message result))
                            (not (:unavailable? result)) (assoc :preview result)))
      db)))

(rf/reg-event-db ::failed
  (fn [db [_ token field message]]
    (if (current? db token field)
      (update-in db path #(if (= field :sample-token)
                            (assoc % :sampling? false :preview-error message)
                            (assoc % :error message field nil)))
      db)))

(rf/reg-event-fx ::write
  (fn [{:keys [db]} [_ delete?]]
    (let [{:keys [expected draft id write-token] :as form} (get-in db path)
          key (material-id id) replacement (when-not delete? draft) token (random-uuid)]
      (when (and (= (:owner form) (owner db)) key (nil? write-token))
        {:db (update-in db path assoc :write-token token :error nil)
         :studio/http {:path "/api/materials" :typed? true :raw-response? true
                       :body {:id key :expected expected :material replacement}
                       :on-success [::written token key replacement] :on-failure [::failed token :write-token]}}))))

(rf/reg-event-fx ::written
  (fn [{:keys [db]} [_ token id material receipt]]
    (when (current? db token :write-token)
      (let [form (get-in db path)
            entries (:materials form)
            updated (assoc form :write-token nil :materials (if material (assoc entries id material) (dissoc entries id)))]
        (if (= :accepted (:status receipt))
          {:db (assoc-in db path (assoc (editing updated (when material id) (or material (get-in form [:presets initial-preset])))
                                        :notice (if material "Material saved." "Material removed.")))
           :dispatch [::schedule]}
          {:db (update-in db path assoc :write-token nil :error "The show has not confirmed this change. Reload the library to check.")})))))

(defn- field-value [event] (.. event -target -value))

(defn- set-field! [path v] (rf/dispatch-sync [::set path v]))

(defn- title [k] (str/capitalize (str/replace (name k) #"-" " ")))

(defn- number-field [label value change!]
  [:label label [:input {:type "number" :step "any" :default-value value :aria-label label
                         :on-blur #(let [n (parse-double (field-value %))]
                                     (when (and n (js/Number.isFinite n)) (change! n)))}]])

(defn- default-control [field label value]
  (cond
    (number? value) [number-field label value #(set-field! field %)]
    (and (vector? value) (every? number? value))
    [:fieldset.material-vector [:legend label]
     (for [[i v] (map-indexed vector value)]
       ^{:key i} [number-field (str label " " (inc i)) v #(set-field! (conj field i) %)])]
    (contains? #{:beat :bar :phrase} (:op value))
    [:label label [:select {:aria-label label :value (name (:op value))
                            :on-change #(set-field! field (assoc value :op (keyword (field-value %))))}
                   [:option {:value "beat"} "Beat"] [:option {:value "bar"} "Bar"]
                   [:option {:value "phrase"} "Phrase"]]]
    (= :variable (:op value))
    [:div.signal-fixed [:span label]
     [:button.text-button {:type "button" :on-click #(set-field! field (signal/fixed-value (:type value)))}
      "Use fixed value"]]
    :else [:div [:span label] [:code (pr-str value)]]))

(defn- input-controls [id value]
  (let [field [:draft :inputs id] label (title id)]
    [:div.material-input-source
     [default-control field label value]
     [signal/picker label value (signal/value-type value) #(set-field! field %)]]))

(defn- add-input [inputs]
  (r/with-let [id (r/atom "") kind (r/atom "number")]
    (let [valid? (and (re-matches #"[a-zA-Z][a-zA-Z0-9_]*" @id)
                      (not (contains? inputs (keyword @id))))
          initial {"number" 0.5 "rgb" [1.0 1.0 1.0] "beat" {:op :beat} "bar" {:op :bar}}]
      [:details.material-new-input
       [:summary "Add an input"]
       [:div.material-metadata
        [:label "Input name" [:input {:value @id :on-change #(reset! id (field-value %))}]]
        [:label "Input type" [:select {:value @kind :on-change #(reset! kind (field-value %))}
                              [:option {:value "number"} "Number"] [:option {:value "rgb"} "RGB"]
                              [:option {:value "beat"} "Beat"] [:option {:value "bar"} "Bar"]]]]
       [:button.button {:disabled (not valid?)
                        :on-click #(do (set-field! [:draft :inputs (keyword @id)] (get initial @kind))
                                       (reset! id ""))}
        "Add input"]])))

(defn- select-material [{:keys [materials presets expected id]}]
  [:div.material-library-select
   [:label "Saved material"
    [:select {:aria-label "Saved material" :value (if expected id "")
              :on-change #(when-let [k (material-id (field-value %))] (rf/dispatch-sync [::choose :materials k]))}
     [:option {:value ""} "New material"]
     (for [[k m] (sort-by (comp :name val) materials)]
       ^{:key k} [:option {:value (slug k)} (:name m)])]]
   [:label "Start from a template"
    [:select {:aria-label "Start from a template" :value ""
              :on-change #(rf/dispatch-sync [::choose :presets (keyword (field-value %))])}
     [:option {:value ""} "Choose template…"]
     (for [[k m] (sort-by (comp :name val) presets)]
       ^{:key k} [:option {:value (slug k)} (:name m)])]]])

(defn- preview-card [{:keys [preview beat dry sampling? preview-error preview-note]}]
  [:section.material-preview {:aria-label "Material preview"}
   [sample-plane preview]
   [:div.material-preview-controls
    [:div.material-readout [:strong "XY plane"] [:span "4 × 2 m"]]
    [:label (str "Sample beat " (.toFixed beat 2))
     [:input {:type "range" :min 0 :max 4 :step 0.05 :value beat :aria-label "Sample beat"
              :on-change #(set-field! [:beat] (parse-double (field-value %)))}]]
    [:label (str "Dry level " (.toFixed dry 2))
     [:input {:type "range" :min 0 :max 1 :step 0.01 :value dry :aria-label "Dry level"
              :on-change #(set-field! [:dry] (parse-double (field-value %)))}]]
    [:output.material-readout {:aria-live "polite"}
     (cond preview-error [:span.danger preview-error]
           sampling? "Sampling…"
           preview-note preview-note
           preview (str "Output " (.toFixed (:min preview) 2) " – " (.toFixed (:max preview) 2)))]]])

(defn- editor [form]
  (let [{:keys [id draft expected write-token notice error confirm-delete? sampling? preview-error]} form]
    [:fieldset.material-edit {:disabled (some? write-token)}
     [select-material form]
     [:div.material-designer-grid
      [preview-card form]
      [:div.material-definition
       [:div.material-metadata
        [:label "Material name" [:input {:value (:name draft) :on-change #(set-field! [:draft :name] (field-value %))}]]
        [:label "Material ID" [:input {:value id :placeholder "venue/ripple" :disabled (some? expected)
                                      :on-change #(set-field! [:id] (field-value %))}]]
        [:label "Blend mode" [:select {:value (name (:mode draft)) :on-change #(set-field! [:draft :mode] (keyword (field-value %)))}
                              [:option {:value "multiply"} "Multiply"] [:option {:value "add"} "Add"]
                              [:option {:value "replace"} "Replace"]]]]
       [:div.material-defaults
        (for [[k v] (sort-by (comp str key) (:inputs draft))]
          ^{:key [k (pr-str v)]} [input-controls k v])]
       (when (:glsl draft) [add-input (:inputs draft)])
       (when (contains? draft :glsl)
         [:label "GLSL expression" [:textarea {:rows 2 :spell-check false :value (:glsl draft)
                                              :on-change #(set-field! [:draft :glsl] (field-value %))}]])]]
     [:footer.material-actions
      [:button.button.primary {:disabled (or (nil? (material-id id)) (str/blank? (:name draft)) sampling? preview-error)
                               :on-click #(rf/dispatch [::write false])} "Save material"]
      (when expected
        [:button.text-button {:on-click #(rf/dispatch [::duplicate])} "Duplicate material"])
      (when expected
        (if confirm-delete?
          [:<> [:button.button.danger {:on-click #(rf/dispatch [::write true])} "Confirm removal"]
           [:button.text-button {:on-click #(set-field! [:confirm-delete?] false)} "Keep material"]]
          [:button.text-button {:on-click #(set-field! [:confirm-delete?] true)} "Remove material"]))
      [:button.text-button {:on-click #(rf/dispatch [::open])} "Reload library"]
      (when (or error notice) [:output {:role "status" :class (when error "danger")} (or error notice)])]]))

(defn panel []
  (r/with-let [opened? (r/atom false)]
    (let [form @(rf/subscribe [::state])]
      [:section.panel.material-designer
       [:button.material-heading
        {:aria-expanded @opened? :on-click #(do (swap! opened? not) (rf/dispatch [(if @opened? ::open ::close)]))}
        [:span [:strong "Material library"] [:small "Optical materials and signal generators"]]
        [:span (if @opened? "Close" "Design materials")]]
       (when @opened?
         (cond (:draft form) [editor form]
               (:error form) [:div.material-actions [:output.danger (:error form)]
                              [:button.text-button {:on-click #(rf/dispatch [::open])} "Retry"]]
               :else [:p.muted "Opening material library…"]))])
    (finally (rf/dispatch [::close]))))
