(ns tolglow.studio.push-display
  "Passive device display: the transport supplies snapshots; this route performs
  no HTTP, timers, show commands, WebGL or device acquisition."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.parameter-widgets :as widgets]))

(defn normalize-view [view]
  (let [view (if (map? view) view {})]
    (-> (select-keys view [:title :page :origin :status :master :blackout? :bpm :selected
                          :cue :effect :swatch :category-colors :display-mode :audio-waveform
                          :parameter-bank :active-effect-navigation :touched-parameter :fine?])
        (assoc :parameters (vec (take 8 (:parameters view)))
               :active-effects (vec (take 8 (:active-effects view)))
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

(defn- envelope [definition]
  (when-let [env (:envelope definition)]
    [:span.push-envelope {:title "Effect envelope definition"}
     (for [[key short] [[:attack "A"] [:decay "D"] [:sustain "S"] [:release "R"]]
           :let [value (get env key)] :when (some? value)]
       ^{:key key} [:span [:b short] " " (if (= key :sustain) (widgets/number-label value) (widgets/duration-label value))])]))

(defn bank-label [{:keys [offset total count]}]
  (when (and (number? count) (> count 1) (number? offset) (number? total) (pos? total))
    (str "Params " (inc offset) "–" (min (+ offset 8) total) " / " total)))

(defn- navigation [view]
  (let [{:keys [index count offset]} (:active-effect-navigation view)]
    [:div.push-navigation
     (when (and (number? count) (pos? count) (number? index))
       [:span.push-effect-navigation {:aria-label (str "Active effect " (inc index) " of " count)}
        [:span (str "Effect " (inc index) " / " count)]
        [:span.push-effect-dots
         (for [[local effect] (map-indexed vector (:active-effects view))]
           ^{:key (:id effect)} [:i {:class (str (when (= (+ (or offset 0) local) index) "selected ") (widgets/label (:phase effect)))
                                     :title (str (widgets/label (:cue-id effect)) " · " (widgets/label (:phase effect)))}])]])
     (when-let [label (bank-label (:parameter-bank view))] [:span.push-bank label])
     (when (:fine? view) [:span.push-fine "Fine"])]))

(defn- parameters [view compact?]
  [:section.push-parameters {:aria-label "Eight encoder parameters" :class (when compact? "compact")}
   (for [index (range 8) :let [parameter (get (:parameters view) index)
                             touched? (= index (get-in view [:touched-parameter :encoder]))
                             component (:component-label parameter)]]
     ^{:key index} [:article.push-parameter {:data-encoder index :class (cond (nil? parameter) "empty" touched? "touched")}
                   [:div.push-parameter-name [:span.push-encoder (inc index)]
                    [:span.push-parameter-label (or (:name parameter) (some-> (:id parameter) widgets/label) "—")]
                    (when (and component (not (contains? #{:value "value"} component)))
                      [:span.push-component {:title "Encoder component"} (widgets/label component)])]
                   (cond
                     (nil? parameter) (when-not compact? [:div.push-empty "·"])
                     compact? [:div.push-compact-value (widgets/value-label (:schema parameter) (if (some? (:effective-value parameter)) (:effective-value parameter) (:value parameter)))]
                     :else [widgets/typed-display parameter])])])

(defn display [view]
  (let [cue (:cue view) effect (:effect view) accent (widgets/color-css (or (:swatch view) (:color cue)))
        active? (:active? cue) audio? (contains? #{:audio "audio"} (:display-mode view))]
    [:main#push-display {:style {"--push-accent" accent} :aria-label "Push lighting display" :class (when audio? "audio-mode")}
     [:header.push-heading
      [:span.push-brand (or (:title view) "Tolglow")]
      [:span.push-page (widgets/label (:page view))]
      [:strong.push-selection (or (:name cue) (:name effect) (some-> (:selected view) widgets/label) "Choose a cue")]
      [navigation view]
      [widgets/category-tags (:categories effect) (:category-colors view)]
      [envelope effect]
      [:span.push-tempo (widgets/number-label (:bpm view)) [:small " BPM"]]
      [:span.push-master (str (widgets/number-label (when (widgets/finite? (:master view)) (* 100 (:master view)))) "%")]
      [:span.push-status {:class (cond (:blackout? view) "blackout" (:releasing? cue) "releasing" active? "active")}
       (cond (:blackout? view) "BLACKOUT" (:releasing? cue) "RELEASE" active? "PLAYING" :else (widgets/label (:status view)))]]
     (if audio?
       [:<> [:section.push-audio {:aria-label "Captured audio display"} [widgets/audio-waveform (:audio-waveform view)]]
        [parameters view true]]
       [:<> [parameters view false]
        [:footer.push-cells
         (for [index (range 8) :let [cell (get (:cells view) index)
                                    color (widgets/color-css (:color cell))
                                    text (or (:name cell) (get (:labels view) index) "—")]]
           ^{:key index} [:div.push-cell {:style {"--cell-color" color} :class (when (:active? cell) "active")}
                         [:i] [:span text]])]])]))

(def component
  (r/create-class
   {:display-name "PushDeviceDisplay"
    :component-did-mount #(aset (aget js/globalThis "tolglowPushDisplay") "ready" true)
    :component-will-unmount #(aset (aget js/globalThis "tolglowPushDisplay") "ready" false)
    :reagent-render (fn [] [display @(rf/subscribe [:push-display/view])])}))
