(ns tolglow.studio.effect-inspector
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.authoring.output-patch :as patch]
            [tolglow.studio.block-inspector :as block-inspector]
            [tolglow.studio.checkbox :as checkbox]
            [tolglow.studio.fixture-picker :as picker]
            [tolglow.studio.events :as events]
            [tolglow.studio.keyframe-editor :as keyframes]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.pretty :as pretty]))

(defn- wire [id] (events/wire-id id))

(defn- same? [a b] (and (some? a) (some? b) (= (wire a) (wire b))))

(defn attributed? [fault instance]
  (cond
    (:instance-id fault) (same? (:instance-id fault) (:id instance))
    (:cue-id fault) (same? (:cue-id fault) (:cue-id instance))
    (:effect-id fault) (same? (:effect-id fault) (:effect-id instance))
    :else false))

(defn faults-for [faults instance]
  (into (vec (keep instance [:fault :reload-fault])) (filter #(attributed? % instance)) faults))

(defn related [show instance]
  (let [layer (get-in instance [:source :native-layer :id])]
    (filterv (fn [other]
               (or (= (:id other) (:id instance))
                   (and layer (= layer (get-in other [:source :native-layer :id])))
                   (same? (:id instance) (get-in other [:source :parent-instance-id]))))
             (:instances show))))

(defn- numeric-schema? [schema] (#{"number" "integer"} (wire (:type schema))))

(defn xyz-controls
  "Group three scalar controllers only when a real aim/direction expression
  consumes them in XYZ order and they can be edited independently."
  [definition parameters]
  (let [groups (for [{:keys [attribute value]} (patch/rows definition)
                     :when (and (#{"aim" "direction"} (wire attribute))
                                (= "vec3" (wire (:op value)))
                                (= 3 (count (:args value)))
                                (every? #(= "param" (wire (:op %))) (:args value)))
                     :let [lookup (into {} (map (fn [[id s]] [(wire id) [id s]]) parameters))
                           components (mapv #(lookup (wire (:key %))) (:args value))]
                     :when (and (every? some? components) (= 3 (count (distinct (map first components))))
                                (every? (comp numeric-schema? second) components))]
                 {:attribute attribute :components components})]
    (:controls
     (reduce (fn [{:keys [used] :as result} [id schema]]
               (if (used id) result
                 (if-let [group (some #(when (and (some #{id} (map first (:components %)))
                                                  (not-any? used (map first (:components %)))) %) groups)]
                   (-> result (update :controls conj group) (update :used into (map first (:components group))))
                   (-> result (update :controls conj {:parameter id :schema schema}) (update :used conj id)))))
             {:controls [] :used #{}} parameters))))

(defn- expression-label [value]
  (cond
    (map? value) (case (wire (:op value))
                   "param" (wire (:key value))
                   "const" (str (:value value))
                   "position" (str "position." (wire (:axis value)))
                   (str (wire (:op value))
                        (when (seq (:args value)) (str "(" (str/join ", " (map expression-label (:args value))) ")"))))
    (vector? value) (str "[" (str/join ", " value) "]")
    :else (str value)))

(defn- typed-instance [db id] (some #(when (= (:id %) id) %) (get-in db [:server-projection :view :instances])))

(defn- token [db] (select-keys (:show db) [:runtime-id :epoch]))

(rf/reg-sub :inspect/definition
  (fn [db [_ id]]
    (let [instance (typed-instance db id)
          view (get-in db [:server-projection :view])
        current (some #(when (= (:id %) (:effect-id instance)) %) (:effects view))]
      {:definition (:definition instance) :latest-version (:version current) :token (token db)
       :values (merge (:params instance) (get-in view [:modulation :overrides id]))})))
(rf/reg-sub :inspect/faults
  (fn [db _] (into (vec (get-in db [:show :reactive-faults])) (get-in db [:ui :reactive-faults]))))
(rf/reg-sub :inspect/status (fn [db [_ key]] (get-in db [:ui :inspector key])))

(defn patch-request [db [instance-id version ref :as key] context spec]
  (let [instance (typed-instance db instance-id)
        definition (:definition instance)]
    (when-not (and (= context (token db)) instance (= version (:version definition)))
      (throw (js/Error. "The show or effect changed. Reopen the patch editor.")))
    (patch/apply-patch definition spec)
    {:db (assoc-in db [:ui :inspector key] {:busy? true})
     :studio/http {:path "/api/definition" :typed? true
                   :body {:kind :effects :definition {:id (:effect-id instance)}
                          :expected-version version :output-patch (assoc spec :ref ref)}
                   :on-success [:inspect/result key context true]
                   :on-failure [:inspect/result key context false]}}))
(rf/reg-event-fx :inspect/patch
  (fn [{:keys [db]} [_ key context spec]]
    (try (patch-request db key context spec)
         (catch :default error {:db (assoc-in db [:ui :inspector key] {:error (.-message error)})}))))
(rf/reg-event-db :inspect/result
  (fn [db [_ key context ok? result]]
    (if (and (= context (token db)) (get-in db [:ui :inspector key :busy?]))
      (assoc-in db [:ui :inspector key]
                (if ok? {:sent (:version result)} {:error (events/error-message result)})) db)))

(rf/reg-event-db :inspect/forget (fn [db [_ key]] (update-in db [:ui :inspector] dissoc key)))

(defn- choice [label options selection on-change]
  ;; Index transport keeps wire-identical keyword/string identifiers distinct.
  [:label [:span label]
   [:select {:aria-label label :value (or (first (keep-indexed #(when (= selection %2) %1) options)) "")
             :on-change #(let [i (.. % -target -value)] (on-change (when (seq i) (get options (js/Number i)))))}
    [:option {:value ""} "Choose parameter"]
    (for [[i id] (map-indexed vector options)] ^{:key i} [:option {:value i} (str id)])]])

(defn- number-field [title draft field]
  [:label [:span title]
   [:input {:type "number" :step "any" :aria-label title :value (get @draft field)
            :on-change #(swap! draft assoc field (.. % -target -value))}]])

(defn- value! [s]
  (let [n (js/Number s)]
    (when-not (and (seq (str/trim s)) (js/Number.isFinite n))
      (throw (js/Error. "Enter a finite number in each transform field."))) n))

(defn- patch-editor [instance definition row latest context]
  (r/with-let [key [(:id instance) (:version definition) (:ref row)]
               draft (r/atom {:mode :one :one nil :xyz [nil nil nil]
                              :scale "1" :offset "0" :clamp? false :low "0" :high "1"})
               local-error (r/atom nil)]
    (let [{:keys [busy? error sent]} @(rf/subscribe [:inspect/status key])
          {:keys [mode one xyz clamp?] :as form} @draft
          w (patch/output-width (:attribute row))
          options (fn [n] (->> (:params definition) (keep (fn [[id schema]] (when (= n (patch/width schema)) id)))
                               (sort-by str) vec))
          selected (if (= :one mode) [one] xyz)
          stale? (not= latest (:version definition))]
      [:details.cue-patch-editor
       [:summary (str "Patch " (wire (:attribute row)))]
       [:div.cue-patch-form
        (when (= 3 w)
          [:label [:span "Source layout"]
           [:select {:aria-label "Source layout" :value (name mode)
                     :on-change #(swap! draft assoc :mode (keyword (.. % -target -value)))}
            [:option {:value "one"} "One vector"] [:option {:value "components"} "Three parameters"]]])
        (if (= :one mode)
          [choice "Source parameter" (options w) one #(swap! draft assoc :one %)]
          (for [[i label] (map-indexed vector (if (= :color (:attribute row)) ["R" "G" "B"] ["X" "Y" "Z"]))]
            ^{:key i} [choice (str label " source") (options 1) (get xyz i) #(swap! draft assoc-in [:xyz i] %)]))
        [number-field "Scale" draft :scale] [number-field "Offset" draft :offset]
        [:label.cue-patch-clamp [checkbox/input {:checked clamp?
                                                  :on-change #(swap! draft assoc :clamp? (.. % -target -checked))}] "Clamp"]
        (when clamp? [:<> [number-field "Minimum" draft :low] [number-field "Maximum" draft :high]])
        [:button.button {:type "button" :disabled (or busy? stale? (some nil? selected))
                         :on-click (fn []
                                     (try
                                       (let [spec (cond-> {:ref (:ref row) :sources selected
                                                           :scale (value! (:scale form)) :offset (value! (:offset form))}
                                                    clamp? (assoc :clamp [(value! (:low form)) (value! (:high form))]))]
                                         (reset! local-error nil) (rf/dispatch [:inspect/patch key context spec]))
                                       (catch :default err (reset! local-error (.-message err)))))}
         (cond busy? "Publishing…" stale? "Newer revision available" :else "Publish connection")]
        (when sent [:output (str "Published v" sent)])
        (when-let [msg (or @local-error error)] [:p.performance-error {:role "alert"} msg])]])
    (finally (rf/dispatch [:inspect/forget key]))))

(defn- fault-details [fault]
  [:details.cue-fault [:summary (str (widgets/label (or (:stage fault) (:kind fault) (:type fault)))
                                     " · " (:message fault))]
   (when-let [id (:id fault)] [:code (str id)])
   (when (some? (:time-ms fault)) [:span (str "Show time " (:time-ms fault) " ms")])
   (for [[i cause] (map-indexed vector (:causes fault))]
     ^{:key i} [:div [:strong (:class cause)] [:p (:message cause)]
                (when (seq (:location cause)) [:pre (pretty/edn (:location cause))])
                (when (seq (:trace cause))
                  [:pre (str/join "\n" (map #(str (:class %) "/" (:method %) "  " (:file %) ":" (:line %)) (:trace cause)))])])
   (when-let [trace (or (:stacktrace fault) (:stack fault))]
     [:pre (if (string? trace) trace (str/join "\n" (map str trace)))])])

(defn unassigned-faults [show faults]
  (when-let [entries (seq (remove #(some (partial attributed? %) (:instances show)) faults))]
    [:details.cue-fault-log [:summary (str "Show diagnostics · " (count entries))]
     (for [[i fault] (map-indexed vector entries)] ^{:key i} [fault-details fault])]))

(defn keyframe-request [db [id version _ :as key] context spec]
  (let [instance (typed-instance db id)
        original (:definition instance)]
    (when-not (and instance (= version (:version original)) (= context (token db)))
      (throw (js/Error. "The show or effect changed. Reopen the keyframe editor.")))
    {:db (assoc-in db [:ui :inspector key] {:busy? true})
     :studio/http {:path "/api/definition" :typed? true
                   :body {:kind :effects :expected-version version
                          :definition (cond-> (dissoc original :keyframes) spec (assoc :keyframes spec))}
                   :on-success [:inspect/result key context true]
                   :on-failure [:inspect/result key context false]}}))

(rf/reg-event-fx :inspect/keyframes
  (fn [{:keys [db]} [_ key context spec]]
    (try (keyframe-request db key context spec)
         (catch :default e {:db (assoc-in db [:ui :inspector key] {:error (.-message e)})}))))

(defn- sample-view [id target attribute]
  (let [query (cond-> [:runtime/keyframe id] (and target attribute) (conj target attribute))
        sample @(rf/subscribe [:studio/remote query])]
    (when (:unit sample)
      [:div.keyframe-sample
       [:p (str "Progress " (when (number? (:progress sample)) (.toFixed (:progress sample) 3))
                " · " (keyframes/duration-text (:start sample)) " – " (keyframes/duration-text (:end sample)) " " (name (:unit sample)))]
       (when (contains? sample :origin)
         [:dl (for [k [:origin :target :result]] ^{:key k} [:div [:dt (name k)] [:dd (pr-str (get sample k))]])])])))

(defn- timeline [instance definition latest context]
  (r/with-let [draft (r/atom (:keyframes definition)) selection (r/atom {})
               key [(:id instance) (:version definition) :keyframes]]
    (let [{:keys [busy? error sent]} @(rf/subscribe [:inspect/status key])
          numeric (into [] (keep (fn [[id schema]] (when (numeric-schema? schema) id))) (:params definition))
          first-fixture @(rf/subscribe [::picker/first-id])
          attributes (into [] (distinct (map :attribute (patch/rows definition))))
          target (get @selection :target first-fixture)
          attribute (get @selection :attribute (first attributes))]
      [:section
       [keyframes/editor @draft numeric #(reset! draft %)]
       [picker/choose "Inspect fixture" "Choose fixture" target #(swap! selection assoc :target %)]
       [choice "Inspect output" attributes attribute #(swap! selection assoc :attribute %)]
       [sample-view (:id instance) target attribute]
       [:button.button {:disabled (or busy? (not= latest (:version definition)) (= @draft (:keyframes definition)))
                        :on-click #(rf/dispatch [:inspect/keyframes key context @draft])}
        "Publish keyframe settings"]
       (when error [:p {:role "alert"} error])
       (when sent [:small (str "Published v" sent)])])
    (finally (rf/dispatch [:inspect/forget key]))))

(defn- timing-panel [instance definition latest context]
  (r/with-let [open? (r/atom false)]
    [:details.slot-parameter-editor {:on-toggle #(reset! open? (.. % -currentTarget -open))}
     [:summary "Keyframe timing"]
     (when @open? [timeline instance definition latest context])]))

(defn- effect-inputs [instance faults]
  (let [{:keys [definition latest-version token values]} @(rf/subscribe [:inspect/definition (:id instance)])]
    [:section.cue-inspected-effect
     [:header [:strong (or (:name definition) (wire (:effect-id instance)))]
      [:small (str "v" (:effect-version instance) " · " (widgets/label (:reload-policy instance)))]]
     ^{:key (str (:id instance) "/" (:version definition))} [timing-panel instance definition latest-version token]
     (for [[i fault] (map-indexed vector (faults-for faults instance))] ^{:key i} [fault-details fault])
     (when (seq (:blocks definition))
       [block-inspector/inspector (:id instance) (:blocks definition)])
     (for [{:keys [ref attribute value] :as row} (patch/rows definition)
           :let [parameters (patch/sources value)]]
       ^{:key (pr-str [(:version definition) ref])}
       [:div.cue-patch-row
        [:div.cue-patch-path
         [:div.cue-patch-inputs
          (if (seq parameters)
            (for [id parameters
                  :let [schema (select-keys (get-in definition [:params id]) [:dmx-bits :bounds])]]
              ^{:key (pr-str id)} [:span (wire id) [:b (widgets/value-label schema (get values id))]])
            [:span (if (and (map? value) (not= :const (:op value))) "Generator" "Constant")])]
         [:span.cue-patch-wire {:aria-hidden true}]
         [:strong (wire attribute)]]
        [:code.cue-patch-expression (expression-label value)]
        (when (seq (:params definition)) [patch-editor instance definition row latest-version token])])]))

(defn panel [show instance faults]
  (let [members (related show instance)]
    [:section.cue-inspector {:aria-label "Cue effects and connections"}
     [:h4 (str "Effects & connections · " (count members))]
     (for [member members] ^{:key (:id member)} [effect-inputs member faults])]))
