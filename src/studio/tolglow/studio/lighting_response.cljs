(ns tolglow.studio.lighting-response
  "Author a fixed or live-controlled attack/release time in the selected musical unit."
  (:require [cljs.reader :as reader]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.modulation :as modulation]
            [tolglow.studio.performance :as performance]))

(def units ["beats" "bars" "phrases" "ms"])

(defn stage-form [stage]
  (let [[unit amount] (first (dissoc (or stage {:beats 0}) :mode))
        blend? (= unit :blend)]
    (merge {:unit (if blend? "beats" (name unit))
            :mode (if blend? :blend (:mode stage :exponential))
            :value (str (if (number? amount) amount (:default amount 0)))}
           (cond (:variable amount) {:source :variable :path (pr-str (:variable amount))}
                 (:cue-id amount) {:source :cue :cue-id (:cue-id amount) :param (:param amount)}
                 :else {:source :fixed}))))

(defn stage-duration [{:keys [source mode unit value cue-id param path] :or {source :fixed mode :exponential}}]
  (let [blend? (= mode :blend)
        unit-key (if blend? :blend (keyword unit))
        fixed (if blend?
                (let [factor (modulation/number-value value "Frame blend")]
                  (when-not (<= 0 factor 1) (throw (js/Error. "Use a frame blend from zero to one.")))
                  {:blend factor})
                (performance/envelope-duration value unit))
        fallback (get fixed unit-key)
        amount (case source
                 :fixed fallback
                 :cue (if (and (some? cue-id) (some? param))
                        {:cue-id cue-id :param param :default fallback}
                        (throw (js/Error. "Choose a cue and a numeric parameter for this stage.")))
                 :variable (let [route (try (reader/read-string (or path "")) (catch js/Error _ nil))]
                             (when-not (and (vector? route) (<= 1 (count route) 64)
                                            (every? #(or (keyword? %) (string? %)) route))
                               (throw (js/Error. "Enter a variable path such as [:audio/energy].")))
                             {:variable route :default fallback}))]
    (cond-> {unit-key amount} (= :linear mode) (assoc :mode :linear))))

(rf/reg-sub ::effects (fn [db _] (get-in db [:server-projection :view :effects])))
(rf/reg-sub ::cues (fn [db _] (get-in db [:server-projection :view :cues])))
(rf/reg-sub ::choices :<- [::effects] :<- [::cues]
  (fn [[effects cues] _]
    (let [by-id (into {} (map (juxt :id identity)) effects)]
      (into []
            (keep (fn [{:keys [id name effect-id]}]
                    (let [params (->> (:params (by-id effect-id))
                                      (keep (fn [[key schema]]
                                              (when (#{:number :integer} (:type schema)) key)))
                                      (sort-by events/wire-id) vec)]
                      (when (seq params) {:id id :name (or name (events/wire-id id)) :params params}))))
            cues))))

(defn- keyed-option [choices value]
  (some #(when (= (pr-str %) value) %) choices))

(defn stage-field [stage stored draft write!]
  (let [{:keys [source mode unit value cue-id param path]} (merge (stage-form stored) draft)
        title (if (= stage :attack) "Attack" "Release")
        edit #(write! {%1 %2})]
    [:div.policy-stage
     [:label.policy-binding "Response"
      [:select {:aria-label (str title " response") :value (name mode)
                :on-change #(edit :mode (keyword (.. % -target -value)))}
       [:option {:value "exponential"} "Exponential smoothing"]
       [:option {:value "linear"} "Reach in duration"]
       [:option {:value "blend"} "Blend per frame"]]]
     [:label (if (= :blend mode) (str title " fraction") title) [:input {:type "text" :input-mode "decimal" :value value
                            :aria-label (str title (when-not (= source :fixed) " fallback") (if (= :blend mode) " fraction" " duration"))
                            :on-change #(edit :value (.. % -target -value))}]]
     (when-not (= :blend mode) [:label "Unit" [:select {:aria-label (str title " unit") :value unit :on-change #(edit :unit (.. % -target -value))}
                     (for [u units] ^{:key u} [:option {:value u} u])]])
     [:label.policy-binding (if (= :blend mode) "Fraction source" "Time source")
      [:select {:aria-label (str title " time source") :value (name source)
                :on-change #(edit :source (keyword (.. % -target -value)))}
       [:option {:value "fixed"} (if (= :blend mode) "Fixed fraction" "Fixed duration")]
       [:option {:value "cue"} "Cue parameter"]
       [:option {:value "variable"} "Show variable"]]]
     (case source
       :variable [:label.policy-binding "Variable path"
                  [:input {:type "text" :aria-label (str title " variable path") :value (or path "")
                           :placeholder "[:audio/energy]" :on-change #(edit :path (.. % -target -value))}]]
       :cue (let [options @(rf/subscribe [::choices])
                  selected (some #(when (= (:id %) cue-id) %) options)
                  params (:params selected)]
              [:<>
               [:label "Cue"
                [:select {:value (if (some? cue-id) (pr-str cue-id) "") :aria-label (str title " control cue")
                          :on-change #(let [id (keyed-option (map :id options) (.. % -target -value))]
                                        (write! {:cue-id id :param nil}))}
                 [:option {:value ""} "Choose cue…"]
                 (when (and cue-id (nil? selected)) [:option {:value (pr-str cue-id)} (events/wire-id cue-id)])
                 (for [{:keys [id name]} options] ^{:key (pr-str id)} [:option {:value (pr-str id)} name])]]
               [:label "Parameter"
                [:select {:value (if (some? param) (pr-str param) "") :aria-label (str title " control parameter")
                          :on-change #(edit :param (keyed-option params (.. % -target -value)))}
                 [:option {:value ""} "Choose parameter…"]
                 (when (and param (not (some #{param} params))) [:option {:value (pr-str param)} (events/wire-id param)])
                 (for [k params] ^{:key (pr-str k)} [:option {:value (pr-str k)} (events/wire-id k)])]]])
       nil)
     (when (not= source :fixed) [:small "The entered value is used while the source is unavailable."])]))
