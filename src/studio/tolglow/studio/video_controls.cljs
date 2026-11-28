(ns tolglow.studio.video-controls
  "Control cards for the video editors (#672, #673, #674): the parameter
  card markup of a Detail bank (title and value, an Ember or Standard knob
  with its slider and exact entry, a menu or a toggle), placed on the
  standard slot grid by `tolglow.authoring.slot-grid/pack` through the same
  `performance/packed-grid` the running effects use. Each card describes
  itself as {:id :title :schema :value :change!} plus :kind (:number,
  :choice, :toggle or :field) and an optional :slot."
  (:require [re-frame.core :as rf]
            [tolglow.authoring.parameter-controls :as controls]
            [tolglow.studio.checkbox :as checkbox]
            [tolglow.studio.numeric-display :as numeric]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.performance :as performance]))

(defn- number-value [event]
  (let [n (js/Number (.. event -target -value))]
    (when (and (not= "" (.. event -target -value)) (js/Number.isFinite n)) n)))

(defn- step [schema]
  (if (= :integer (:type schema)) 1 (or (:step schema) "any")))

(defn- range-step [{:keys [bounds] :as schema}]
  (if (= :integer (:type schema))
    1
    (or (:step schema) (when-let [[low high] bounds] (/ (- high low) 200)) "any")))

(defn- number-card [{:keys [id title schema value change! unit]} attrs skin]
  (let [[low high] (:bounds schema)
        shown (or value 0)
        schema (cond-> schema unit (assoc :unit unit))]
    [:div.param-row (merge {:data-param id :data-control "rotary" :data-slot "compact"} attrs)
     [:label [:span title] [:output (widgets/value-label schema shown)]]
     [:div.parameter-number-layout {:data-widget "rotary"}
      [widgets/rotary-control schema shown change! {:name (str title " rotary") :skin skin :base-value shown}]
      [:div.parameter-number-fields
       [:input {:type "range" :aria-label title :value shown :min low :max high :step (range-step schema)
                :on-change #(when-let [n (number-value %)] (change! n))}]
       [numeric/number-input {:class "numeric-exact" :display-schema schema :type "number" :aria-label (str title " exact value")
                              :value shown :min low :max high :step (step schema)
                              :on-change #(when-let [n (number-value %)] (change! n))}]]]]))

(defn- choice-card [{:keys [id title value change! choices]} attrs]
  [:div.param-row (merge {:data-param id :data-control "choice" :data-slot "compact"} attrs)
   [:label [:span title]]
   [:select {:aria-label title :value (pr-str value)
             :on-change #(when-let [[v] (some (fn [[v :as c]] (when (= (pr-str v) (.. % -target -value)) c)) choices)] (change! v))}
    (for [[v label] choices] ^{:key (pr-str v)} [:option {:value (pr-str v)} label])]])

(defn- toggle-card [{:keys [id title value change!]} attrs]
  [:div.param-row (merge {:data-param id :data-control "toggle" :data-slot "compact"} attrs)
   [:label [:span title]]
   [checkbox/input {:aria-label title :checked (boolean value) :on-change #(change! (.. % -target -checked))}]])

(defn- field-card [{:keys [id title value change! placeholder]} attrs]
  [:div.param-row (merge {:data-param id :data-control "field" :data-slot "compact"} attrs)
   [:label [:span title]]
   [:input {:type "text" :aria-label title :value (or value "") :placeholder placeholder
            :on-change #(change! (.. % -target -value))}]])

(defn card
  "One control card; attrs carries its grid area."
  [control attrs skin]
  (case (:kind control :number)
    :number [number-card control attrs skin]
    :choice [choice-card control attrs]
    :toggle [toggle-card control attrs]
    :field [field-card control attrs]))

(defn grid
  "Control cards on the standard slot grid, in reading order."
  [label controls]
  (let [skin @(rf/subscribe [:ui/knob-skin])
        spans (mapv #(controls/slot-span (:slot % :compact)) controls)]
    [performance/packed-grid
     {:attrs {:class "live-effect-parameters video-slot-grid control-detail" :data-slot-grid "detail"
              :data-skin (name skin) :role "group" :aria-label label}
      :measure performance/slot-columns
      :spans (constantly spans)
      :keys (mapv (comp str :id) controls)
      :render (fn [index style _] (card (nth controls index) {:style style} skin))}]))

(defn coalesce-key
  "A token shared by the edits of one control while they keep arriving: a
  knob turned without a pause of a second counts as one undo step. tokens is
  the app-db map of control id → {:token :at}; returns [token tokens]."
  [tokens control-id now]
  (let [{:keys [token at]} (get tokens control-id)
        token (if (and token (< (- now at) 1000)) token (str (random-uuid)))]
    [token (assoc tokens control-id {:token token :at now})]))

;; The clock reaches events as a coeffect, never read inside a handler body.
(rf/reg-cofx ::now (fn [cofx _] (assoc cofx :now (.now js/Date))))
