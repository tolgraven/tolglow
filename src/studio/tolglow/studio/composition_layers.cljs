(ns tolglow.studio.composition-layers
  "Re-frame controls for typed transforms in an ordered compound cue."
  (:require [re-frame.core :as rf]))

(defn- previous [attribute fallback]
  {:op :previous :attribute attribute
   :default {:op :incoming :attribute attribute :default fallback}})

(def presets
  [[:hue "Rotate previous color" {:attribute :color :value {:op :rotate-hue :args [(previous :color [0 0 0]) 0.25]}}
    [["Hue (turns)" [:value :args 1] 0.01]]]
   [:shift "Offset aim point" {:attribute :aim :value {:op :add :args [(previous :aim [0 0 0]) [1 0 0]]}}
    [["X (m)" [:value :args 1 0] 0.1] ["Y (m)" [:value :args 1 1] 0.1] ["Z (m)" [:value :args 1 2] 0.1]]]
   [:rotate "Turn direction around Y" {:attribute :direction :value {:op :rotate-y :args [(previous :direction [0 0 1]) 0.5]}}
    [["Rotation (rad)" [:value :args 1] 0.1]]]
   [:gain "Scale previous dimmer" {:attribute :dimmer :value {:op :multiply :args [(previous :dimmer 1) 0.5]}}
    [["Gain" [:value :args 1] 0.05]]]
   [:ratio "Set beat ratio" {:attribute :beat-ratio :value {:op :divide :args [1 4]}}
    [["Numerator" [:value :args 0] 1] ["Denominator" [:value :args 1] 1]]]
   [:beat-hue "Rotate hue by beat ratio"
    {:attribute :color :value {:op :rotate-hue :args [(previous :color [0 0 0]) {:op :field :attribute :beat-ratio :default 0}]}} []]
   [:add-aim "Add incoming aim offset"
    {:attribute :aim :value {:op :add :args [{:op :previous :attribute :aim :default [0 0 0]} {:op :incoming :attribute :aim :default [0 0 0]}]}} []]])

(defn preset-for [transform]
  ;; Match the complete expression except for parameter values. Custom EDN
  ;; expressions can be reopened and reordered without altering their operands.
  (some (fn [[_ _ template fields :as preset]]
          (when (and (every? #(number? (get-in transform (second %))) fields)
                     (= template (reduce (fn [v [_ path]] (assoc-in v path (get-in template path))) transform fields)))
            preset))
        presets))

(defn- path [i] [:ui :composition :slots i :transforms])

(rf/reg-event-db :composition-layer/add
  (fn [db [_ i id]]
    (if-let [template (some (fn [[key _ expression]] (when (= key id) expression)) presets)]
      (update-in db (path i) (fnil conj []) template)
      db)))

(rf/reg-event-db :composition-layer/remove
  (fn [db [_ i j]]
    (update-in db (path i) #(into (subvec % 0 j) (subvec % (inc j))))))

(rf/reg-event-db :composition-layer/move
  (fn [db [_ i from to]]
    (update-in db (path i)
               (fn [v] (if (and (get v from) (get v to)) (assoc v from (v to) to (v from)) v)))))

(rf/reg-event-db :composition-layer/value
  (fn [db [_ i j key-path number]]
    (let [transform (get-in db (conj (path i) j))
          ratio? (= :ratio (first (preset-for transform)))
          denominator? (= key-path [:value :args 1])]
      (if (and (js/Number.isFinite number)
               (or (not ratio?) (and (js/Number.isSafeInteger number)
                                     (or (not denominator?) (pos? number)))))
        (assoc-in db (into (conj (path i) j) key-path) number)
        db))))

(defn opacity-control [opacity]
  [:label.layer-opacity "Layer opacity"
   [:input#composition-opacity {:type "range" :min 0 :max 1 :step 0.01 :value (or opacity 1)
                               :on-change #(rf/dispatch-sync [:studio/ui-set [:composition :opacity] (.. % -target -valueAsNumber)])}]
   [:output {:for "composition-opacity"} (str (js/Math.round (* 100 (or opacity 1))) "%")]])

(rf/reg-event-db :composition-layer/include-previous
  (fn [db [_ i include?]]
    (update-in db [:ui :composition :slots i]
               #(if include? (assoc % :targets :previous) (dissoc % :targets)))))

(defn slot-controls [i {:keys [transforms targets]}]
  [:details.slot-parameter-editor.layer-transforms
   [:summary (str "Layer transforms" (when (seq transforms) (str " · " (count transforms))))]
   [:label.layer-targets [:input {:type "checkbox" :checked (= :previous (some-> targets keyword))
                    :on-change #(rf/dispatch-sync [:composition-layer/include-previous i (.. % -target -checked)])}]
    "Include earlier members’ heads"]
   (for [[j transform] (map-indexed vector transforms)
         :let [[_ title _ fields] (preset-for transform)]]
     ^{:key j}
     [:div.layer-transform
      [:div.layer-transform-heading
       [:strong (or title (str "Transform " (name (:attribute transform))))]
       [:div.slot-actions
        (for [[caption action enabled event]
              [["↑" "Move transform earlier" (pos? j) [:composition-layer/move i j (dec j)]]
               ["↓" "Move transform later" (< j (dec (count transforms))) [:composition-layer/move i j (inc j)]]
               ["×" "Remove transform" true [:composition-layer/remove i j]]]]
          ^{:key action}
          [:button.icon-button {:type "button" :aria-label (str action ", slot " (inc i) ", transform " (inc j))
                                :disabled (not enabled) :on-click #(rf/dispatch event)} caption])]]
      (when-not title [:code (pr-str (:value transform))])
      (when (seq fields)
        (into [:div.layer-transform-values]
              (map (fn [[label key-path step]]
                     [:label label
                      [:input {:type "number" :step step :value (get-in transform key-path)
                               :on-change #(rf/dispatch-sync [:composition-layer/value i j key-path (.. % -target -valueAsNumber)])}]]))
              fields))])
   [:label "Add transform"
    [:select {:aria-label (str "Add layer transform to slot " (inc i))
              :value "" :on-change #(rf/dispatch [:composition-layer/add i (keyword (.. % -target -value))])}
     [:option {:value "" :disabled true} "Choose a transform…"]
     (for [[id title] presets] ^{:key id} [:option {:value (name id)} title])]]])
