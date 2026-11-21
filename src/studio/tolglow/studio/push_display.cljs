(ns tolglow.studio.push-display
  "Passive device display: the transport supplies snapshots; this route performs
  no HTTP, timers, show commands, WebGL or device acquisition."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.parameter-widgets :as widgets]))

(defn normalize-view [view]
  (let [view (if (map? view) view {})]
    (-> (select-keys view [:title :page :origin :status :master :blackout? :bpm :selected
                          :cue :effect :swatch :category-colors])
        (assoc :parameters (vec (take 8 (:parameters view)))
               :cells (vec (take 8 (:cells view))) :labels (vec (take 8 (:labels view)))))))
(rf/reg-event-db :push-display/update (fn [db [_ view]] (assoc db :push-display (normalize-view view))))
(rf/reg-sub :push-display/view (fn [db _] (:push-display db)))

(defn install-api! []
  (let [api (js-obj)]
    (aset api "ready" false)
    (aset api "update" (fn [view]
                         (rf/dispatch-sync [:push-display/update (js->clj view :keywordize-keys true)])
                         ;; Flush Reagent's pending render; the capture worker waits
                         ;; two animation frames before reading the composited image.
                         (r/flush)))
    (aset js/globalThis "tolglowPushDisplay" api)))

(defn- meter [schema value]
  (when-let [amount (widgets/proportion schema value)]
    [:div.push-meter {:role "meter" :aria-label "Parameter within range" :aria-valuemin 0 :aria-valuemax 1 :aria-valuenow amount}
     [:i {:style {:width (str (* 100 amount) "%")}}]]))
(defn- typed-value [{:keys [schema value preview-samples]}]
  (let [type (widgets/label (:type schema))]
    [:<>
     [:div.push-value {:title (widgets/value-label schema value)}
      (when (= type "color") [:i.push-color {:style {:background (widgets/color-css value)}}])
      (widgets/value-label schema value)]
     (cond
       (seq preview-samples) [widgets/sparkline preview-samples]
       (seq (:choices schema))
       [:div.push-options (for [choice (take 8 (:choices schema))]
                            ^{:key (str (:value choice))}
                            [:i {:class (when (= value (:value choice)) "chosen")
                                 :title (or (:label choice) (widgets/label (:id choice)))}])]
       (= type "boolean") [:div.push-boolean {:class (when value "on")} (if value "ENABLED" "DISABLED")]
       (#{"vector" "range" "color"} type)
       [:div.push-components (for [[index component] (map-indexed vector (take 4 value))]
                               ^{:key index} [meter (if (= type "color") {:bounds [0 1]} schema) component])]
       (= type "enum") [:div.push-enum (str (count (or (:values schema) (:options schema))) " OPTIONS")]
       :else [meter schema value])]))
(defn- envelope [definition]
  (when-let [env (:envelope definition)]
    [:span.push-envelope {:title "Effect envelope definition"}
     (for [[key short] [[:attack "A"] [:decay "D"] [:sustain "S"] [:release "R"]]
           :let [value (get env key)] :when (some? value)]
       ^{:key key} [:span [:b short] " " (if (= key :sustain) (widgets/number-label value) (widgets/duration-label value))])]))

(defn display [view]
  (let [cue (:cue view) effect (:effect view) accent (widgets/color-css (or (:swatch view) (:color cue)))
        active? (:active? cue)]
    [:main#push-display {:style {"--push-accent" accent} :aria-label "Push lighting display"}
     [:header.push-heading
      [:span.push-brand (or (:title view) "Tolglow")]
      [:span.push-page (widgets/label (:page view))]
      [:strong.push-selection (or (:name cue) (:name effect) (some-> (:selected view) widgets/label) "Choose a cue")]
      [widgets/category-tags (:categories effect) (:category-colors view)]
      [envelope effect]
      [:span.push-tempo (widgets/number-label (:bpm view)) [:small " BPM"]]
      [:span.push-master (str (widgets/number-label (when (widgets/finite? (:master view)) (* 100 (:master view)))) "%")]
      [:span.push-status {:class (cond (:blackout? view) "blackout" (:releasing? cue) "releasing" active? "active")}
       (cond (:blackout? view) "BLACKOUT" (:releasing? cue) "RELEASE" active? "PLAYING" :else (widgets/label (:status view)))]]
     [:section.push-parameters {:aria-label "Eight encoder parameters"}
      (for [index (range 8) :let [parameter (get (:parameters view) index)]]
        ^{:key index} [:article.push-parameter {:data-encoder index :class (when-not parameter "empty")}
                      [:div.push-parameter-name [:span.push-encoder (inc index)]
                       (or (:name parameter) (some-> (:id parameter) widgets/label) "—")]
                      (if parameter [typed-value parameter] [:div.push-empty "·"])])]
     [:footer.push-cells
      (for [index (range 8) :let [cell (get (:cells view) index)
                                 color (widgets/color-css (:color cell))
                                 text (or (:name cell) (get (:labels view) index) "—")]]
        ^{:key index} [:div.push-cell {:style {"--cell-color" color} :class (when (:active? cell) "active")}
                      [:i] [:span text]])]]))

(def component
  (r/create-class
   {:display-name "PushDeviceDisplay"
    :component-did-mount #(aset (aget js/globalThis "tolglowPushDisplay") "ready" true)
    :component-will-unmount #(aset (aget js/globalThis "tolglowPushDisplay") "ready" false)
    :reagent-render (fn [] [display @(rf/subscribe [:push-display/view])])}))
