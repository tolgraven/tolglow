(ns tolglow.studio.spatial
  "Studio controls for venue volumes and late-bound cue launches."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.autopilot-signal :as signal]
            [tolglow.studio.spatial-drafts :as drafts]
            [tolglow.studio.spatial-fields :refer [number-input xyz]]
            [tolglow.studio.volume-sculpt :as sculpt]
            [tolglow.studio.spatial-picker :as picker]
            [tolglow.studio.spatial-preview :as preview]))

(def shapes [[:ball "Ball"] [:sphere "Sphere"] [:box "Box"] [:cylinder "Cylinder"] [:hdr "HDR spill"]])
(defn default-spec [shape]
  (case shape
    :ball {:shape :ball :position [0 2 0] :radius 0.6 :velocity [2 1 0] :restitution 0.85 :hardness 0.7}
    :sphere {:shape :sphere :position [0 2 0] :radius 1}
    :cylinder {:shape :cylinder :position [0 2 0] :radius 1 :height 2}
    :box {:shape :box :min [-1 0 -1] :max [1 2 1]}
    :hdr {:shape :hdr :threshold 0.01}))
(defn sorted-ids [m] (sort-by (juxt picker/label pr-str) (keys m)))
(defn fresh-id [prefix ids] (first (remove (set ids) (map #(str prefix "-" %) (iterate inc 1)))))
(defn view [db] (picker/show db))
(defn config [db] (get-in (view db) [:settings :spatial] {}))
(defn active-id [db]
  (let [id (:chosen (drafts/state db)) volumes (:volumes (config db))]
    (if (contains? volumes id) id (first (sorted-ids volumes)))))
(rf/reg-sub ::state (fn [db _] (drafts/state db)))
(rf/reg-sub ::settings (fn [db _] (config db)))
(rf/reg-sub ::selected (fn [db _] (active-id db)))
(rf/reg-sub ::connected (fn [db _] (get-in db [:connection :connected?])))
(rf/reg-event-db ::choose (fn [db [_ id]] (drafts/update-ui db assoc :chosen id :message nil)))
(rf/reg-event-db ::select-preview
  (fn [db [_ key]]
    (if-let [id (some #(when (= (pr-str %) key) %) (keys (:volumes (config db))))]
      (drafts/update-ui db assoc :chosen id) db)))
(rf/reg-event-db ::edit (fn [db [_ path value]] (drafts/update-ui db assoc-in path value)))

(rf/reg-event-db ::volume-field
  (fn [db [_ id field value]]
    (let [path [:drafts id] current (or (get-in (drafts/state db) path) (get-in (config db) [:volumes id]))
          value (if (fn? value) (value current) value)]
      (if current
        (drafts/update-ui db assoc-in path (if (nil? value) (dissoc current field) (assoc current field value)))
        db))))

(defn save [db values message draft-key selected]
  (let [owner (picker/identity-of db) token (str (random-uuid))]
    (when (and (:runtime-id owner) (get-in db [:connection :connected?]) (nil? (:pending (drafts/state db))))
      {:db (drafts/update-ui db assoc :pending token :message nil)
       :studio/http {:path "/api/settings" :typed? true
                     :body {:scope :spatial :values values :runtime-id (:runtime-id owner) :expected-epoch (:epoch owner)}
                     :on-success [::saved owner token message draft-key (get-in (drafts/state db) draft-key) selected]
                     :on-failure [::failed owner token]}})))
(defn- parse-volumes [volumes]
  (update-vals volumes (fn [spec] (if (:radius-source spec) (update spec :radius-source signal/parse-spec) spec))))

(rf/reg-event-fx ::save
  (fn [{:keys [db]} [_ values message draft-key selected]]
    (try (save db (cond-> values (:volumes values) (update :volumes parse-volumes)) message draft-key selected)
         (catch js/Error e {:db (drafts/update-ui db assoc :message (.-message e))}))))
(rf/reg-event-db ::saved drafts/acknowledge)
(rf/reg-event-db ::failed
  (fn [db [_ owner token error]]
    (if (and (= owner (picker/identity-of db)) (= token (:pending (drafts/state db))))
      (drafts/update-ui db assoc :pending nil :message (str error)) db)))

(defn thrown [spec point velocity throw-id]
  (assoc spec :shape :ball :position point :velocity velocity :throw-id throw-id))
(rf/reg-event-fx ::throw
  (fn [{:keys [db]} [_ editor-owner point velocity]]
    (let [id (active-id db) spec (get-in (config db) [:volumes id])]
      (when (and (= editor-owner (get-in db [:ui :rig-editor :identity])) (= :ball (:shape spec)))
        (save db {:enabled? true :volumes {id (thrown spec point velocity (str (random-uuid)))}} "Ball thrown." nil id)))))

(def cue-target-picker picker/cue-target-picker)

(defn- edn-input [title spec apply-value]
  (r/with-let [draft (r/atom nil) error (r/atom nil)]
    [:details [:summary title]
     [:label [:span title]
      [:textarea {:aria-label title :value (or @draft (pr-str spec)) :spell-check false
                  :on-change #(reset! draft (.. % -target -value))}]]
     [:button.button {:type "button" :disabled (nil? @draft)
                      :on-click #(try (let [v (reader/read-string @draft)]
                                        (when-not (map? v) (throw (js/Error. "Enter an EDN map.")))
                                        (apply-value v) (reset! draft nil) (reset! error nil))
                                      (catch js/Error e (reset! error (.-message e))))} "Use definition"]
     (when @error [:span {:role "alert"} @error])]))

(defn- new-volume [volumes disabled?]
  (r/with-let [shape (r/atom :ball) name (r/atom "")]
    (let [id (if (str/blank? @name) (fresh-id (picker/label @shape) (keys volumes)) (str/trim @name))]
      [:div.spatial-new
       [:label [:span "New volume"] [:select {:aria-label "New volume shape" :value (picker/label @shape)
                                              :on-change #(reset! shape (keyword (.. % -target -value)))}
                                      (for [[k text] shapes] ^{:key k} [:option {:value (picker/label k)} text])]]
       [:label [:span "Identifier"] [:input {:aria-label "New volume ID" :value @name :placeholder id
                                             :on-change #(reset! name (.. % -target -value))}]]
       [:button.button {:disabled (or disabled? (contains? volumes id) (> (count id) 120))
                        :on-click #(rf/dispatch [::save {:enabled? true :volumes {id (default-spec @shape)}} "Volume added." nil id])} "Add volume"]])))

(rf/reg-sub ::signals (fn [db _] (get-in (view db) [:settings :autopilot :signals])))

(defn- volume-fields [id spec edit!]
  (let [{:keys [shape position radius velocity restitution hardness gravity attraction angular-velocity]} spec]
    [:div.spatial-volume-fields
     (when (#{:ball :sphere :cylinder} shape)
       [:<> [xyz "Position · m" position #(edit! :position %)]
        [number-input "Radius · m" radius #(edit! :radius %) {:min 0.001}]
        (when (#{:ball :sphere} shape)
          [:<> [:label.spatial-check [:input {:type "checkbox" :checked (contains? spec :radius-source)
                                              :on-change #(edit! :radius-source
                                                                 (when (.. % -target -checked)
                                                                   (fn [{r :radius}]
                                                                     {:waveform :sine :period {:bars 1} :range [(* r 0.5) r]})))}]
                "Modulate radius"]
           (when-some [source (:radius-source spec)]
             [signal/field [:spatial id] "Radius modulation" source {:min 0.001 :max (max 4 radius) :step 0.01}
              @(rf/subscribe [::signals]) (fn [value] (edit! :radius-source (if (fn? value) (value source) value)))])])])
     (case shape
       :ball [:<> [xyz "Launch velocity · m/s" velocity #(edit! :velocity %)]
              [number-input "Restitution" restitution #(edit! :restitution %) {:min 0 :max 1}]
              [number-input "Hardness" hardness #(edit! :hardness %) {:min 0 :max 1}]
              [number-input "Attraction · m³/s²" (or attraction 0) #(edit! :attraction %) {:min 0}]
              [xyz "Spin · rad/s" (or angular-velocity [0 0 0]) #(edit! :angular-velocity %)]
              [:label.spatial-check [:input {:type "checkbox" :checked (boolean (:fixed? spec))
                                             :on-change #(edit! :fixed? (.. % -target -checked))}] "Fixed attractor"]]
       :box [:<> [xyz "Minimum · m" (:min spec) #(edit! :min %)] [xyz "Maximum · m" (:max spec) #(edit! :max %)]]
       :cylinder [number-input "Height · m" (:height spec) #(edit! :height %) {:min 0.001}]
       :hdr [:<> [picker/picker {:title "Spill emitter" :kinds [:fixture] :selected (some-> (:source-id spec) picker/label)
                                 :on-select #(edit! :source-id (:id %))}]
             [:button.text-button {:type "button" :on-click #(edit! :source-id nil)} "Any emitter"]
             [number-input "Minimum spill" (:threshold spec 0) #(edit! :threshold %) {:min 0}]]
       nil)]))

(def actions [[:flicker "Flicker"] [:strobe "Strobe"] [:aim "Aim at centre"] [:cue "Launch cue"]
              [:effect "Run effect"] [:random-cue "Ranked random cue"] [:random-effect "Ranked random effect"]])
(defn binding-label [{:keys [action]}]
  (cond (:preset action) (second (some #(when (= (:preset action) (first %)) %) actions))
        (:cue-id action) (str "Cue · " (picker/label (:cue-id action)))
        (:effect-id action) (str "Effect · " (picker/label (:effect-id action)))
        :else (str "Ranked " (picker/label (get-in action [:random :kind])))))
(defn- binding-editor [id bindings disabled?]
  (r/with-let [kind (r/atom :aim) length-unit (r/atom :contact) length (r/atom 1)
               record (r/atom nil) filter-to (r/atom nil) limit (r/atom 3) category (r/atom "")]
    (let [action (case @kind
                   :cue (when (= :cue (:kind @record)) {:cue-id (:id @record)})
                   :effect (when (= :effect (:kind @record)) {:effect-id (:id @record)})
                   (:random-cue :random-effect)
                   {:random (cond-> {:kind (if (= :random-cue @kind) :cues :effects) :limit @limit}
                              (not (str/blank? @category)) (assoc :categories (mapv keyword (remove str/blank? (str/split @category #"[ ,]+")))))}
                   {:preset @kind})
          rule (cond-> {:volume-id id :action action :duration (if (= :contact @length-unit) :contact {@length-unit @length})}
                 @filter-to (assoc :selector (:selector @filter-to)))
          matches (filter #(= (:volume-id (val %)) id) (sort-by (comp picker/label key) bindings))]
      [:fieldset.spatial-binding [:legend "Contact actions"]
       (for [[key binding] matches]
         ^{:key (pr-str key)}
         [:div.spatial-binding-row [:span (binding-label binding)] [:small (if (= :contact (:duration binding :contact)) "Until exit" (pr-str (:duration binding)))]
          [:button.text-button {:disabled disabled? :aria-label (str "Remove binding " (picker/label key))
                                :on-click #(rf/dispatch [::save {:bindings {key nil}} "Binding removed."])} "Remove"]])
       [:div.spatial-bind-fields
        [:label [:span "On entry"] [:select {:aria-label "Contact action" :value (name @kind)
                                             :on-change #(do (reset! kind (keyword (.. % -target -value))) (reset! record nil))}
                                    (for [[k l] actions] ^{:key k} [:option {:value (name k)} l])]]
        (when (#{:cue :effect} @kind)
          [picker/picker {:kinds [@kind] :title "Registered look" :selected (:label @record) :on-select #(reset! record %)}])
        (when (#{:random-cue :random-effect} @kind)
          [:<> [number-input "Top candidates" @limit #(reset! limit %) {:min 1 :max 128 :step 1}]
           [:label "Categories" [:input {:placeholder "movement, color" :value @category :on-change #(reset! category (.. % -target -value))}]]])
        [:label [:span "End action"] [:select {:aria-label "Contact action duration" :value (name @length-unit)
                                              :on-change #(reset! length-unit (keyword (.. % -target -value)))}
                                     (for [[unit l] [[:contact "On exit"] [:beats "Beats"] [:bars "Bars"] [:phrases "Phrases"] [:ms "Milliseconds"]]]
                                       ^{:key unit} [:option {:value (name unit)} l])]]
        (when-not (= :contact @length-unit) [number-input "Action length" @length #(reset! length %) {:min 0.001}])]
       [picker/picker {:kinds [:group :fixture :volume] :title "Limit contact to" :selected (or (:label @filter-to) "All members")
                       :on-select #(reset! filter-to %)}]
       (when @filter-to [:button.text-button {:on-click #(reset! filter-to nil)} "Use all members"])
       [:button.button {:disabled (or disabled? (nil? action))
                        :on-click #(rf/dispatch [::save {:bindings {(fresh-id (str (picker/label id) "-contact") (keys bindings)) rule}}
                                                 "Contact action added."])} "Bind action"]])))

(defn- visible-value [n] (if (number? n) (.toFixed n 2) "—"))
(defn- telemetry [id]
  (let [body @(rf/subscribe [:studio/remote [:spatial/body id]])
        members @(rf/subscribe [:studio/remote [:spatial/members id]])]
    [:div.spatial-telemetry {:aria-label "Live volume"}
     (when-let [p (:position body)] [:span "XYZ " (str/join " / " (map visible-value p))])
     (when-let [s (:speed body)] [:span (visible-value s) " m/s"])
     (when (number? (:deformation body)) [:span "Compression " (js/Math.round (* 100 (:deformation body))) "%"])
     (when (some? members) [:details [:summary (str (count members) " inside")]
                            [:div (str/join " · " (sort (map picker/label members)))]] )]))

(defn- world-fields [settings disabled?]
  (let [s @(rf/subscribe [::state]) draft (merge (select-keys settings [:bounds :gravity]) (get-in s [:world :draft]))
        edit #(rf/dispatch [::edit [:world :draft] (assoc draft %1 %2)])]
    [:details.spatial-world [:summary "Room bounds and gravity"]
     [xyz "Room minimum · m" (get-in draft [:bounds :min] [-5 0 -5]) #(edit :bounds (assoc (:bounds draft {:max [5 5 5]}) :min %))]
     [xyz "Room maximum · m" (get-in draft [:bounds :max] [5 5 5]) #(edit :bounds (assoc (:bounds draft {:min [-5 0 -5]}) :max %))]
     [xyz "Gravity · m/s²" (:gravity draft [0 -9.81 0]) #(edit :gravity %)]
     [:button.button {:disabled (or disabled? (nil? (get-in s [:world :draft])))
                      :on-click #(rf/dispatch [::save draft "Room updated." [:world :draft]])} "Apply room"]]))

(defn- body-editor [id stored settings ui disabled?]
  (let [draft-key [:drafts id] draft (or (get-in ui draft-key) stored)
        update! #(rf/dispatch [::edit draft-key %])
        edit! #(rf/dispatch-sync [::volume-field id %1 %2])]
    [:div.spatial-body
     [:div.spatial-body-header [:strong (picker/label id)] [:small (picker/label (:shape draft))]
      [:button.text-button {:disabled disabled? :on-click #(let [bound (into {} (keep (fn [[k b]] (when (= id (:volume-id b)) [k nil]))) (:bindings settings))]
                                                            (rf/dispatch [::save {:volumes {id nil} :bindings bound} "Volume removed." draft-key]))} "Remove volume"]]
     [preview/watch-visibility (fn [visible?] (when (and visible? (:enabled? settings)) [telemetry id]))]
     (when (not= :hdr (:shape draft))
       [:button.button {:on-click #(rf/dispatch [::sculpt/open id draft])} "Sculpt in 3D"])
     [volume-fields id draft edit!]
     [edn-input "Volume definition (EDN)" draft update!]
     [:div.spatial-actions
      [:button.button.primary {:disabled (or disabled? (= stored draft))
                               :on-click #(rf/dispatch [::save {:volumes {id draft}} "Volume saved." draft-key id])} "Apply volume"]
      (when (= :ball (:shape draft))
        [:<> [:button.button {:disabled disabled? :on-click #(rf/dispatch [::save {:enabled? true :volumes {id (assoc draft :throw-id (str (random-uuid)))}}
                                                                            "Ball thrown." draft-key id])} "Throw ball"]
         [:button.button {:disabled disabled?
                          :on-click #(do (rf/dispatch-sync [:rig-editor/open])
                                         (rf/dispatch-sync [:rig-editor/set [:tool] "throw"])
                                         (rf/dispatch [:rig-editor/set [:plane-value] (or (get (:position draft) 1) 1)] )
                                         (when-let [canvas (.querySelector js/document ".rig-editor canvas")] (.scrollIntoView canvas #js {:block "center"})))}
          "Throw in 3D"]])]
     [binding-editor id (:bindings settings) disabled?]]))

(defn panel []
  (let [settings @(rf/subscribe [::settings]) current @(rf/subscribe [::state]) id @(rf/subscribe [::selected])
        connected? @(rf/subscribe [::connected]) disabled? (or (not connected?) (some? (:pending current)))
        volumes (:volumes settings)]
    [:section.panel.spatial-panel {:aria-label "Spatial controls"}
     [:div.panel-top [:h2 "Spatial controls"]
      [:label.spatial-check [:input {:type "checkbox" :aria-label "Enable spatial controls" :checked (boolean (:enabled? settings))
                                     :disabled disabled? :on-change #(rf/dispatch [::save {:enabled? (.. % -target -checked)} "Spatial controls updated."])}]
       "Enabled"]]
     [:div.spatial-toolbar [:div.spatial-volume-list {:aria-label "Venue volumes"}
                            (for [v (sorted-ids volumes)] ^{:key (pr-str v)}
                              [:button.button {:aria-pressed (= id v) :on-click #(rf/dispatch [::choose v])} (picker/label v)])]
      [:label.spatial-check [:input {:type "checkbox" :checked (= :enabled (:run-when settings)) :disabled disabled?
                                     :on-change #(rf/dispatch [::save {:run-when (if (.. % -target -checked) :enabled :demand)}
                                                               "Simulation lifetime updated."])}] "Keep running offscreen"]]
     [new-volume volumes disabled?] [world-fields settings disabled?]
     (when id ^{:key (pr-str [(:owner current) id])} [body-editor id (volumes id) settings current disabled?])
     (when-let [message (:message current)] [:p.small {:role "status"} message])]))
