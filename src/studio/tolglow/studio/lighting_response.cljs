(ns tolglow.studio.lighting-response
  "Author a fixed or live-controlled attack/release time in the selected musical unit."
  (:require [cljs.reader :as reader]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.performance :as performance]))

(def units ["beats" "bars" "phrases" "ms"])

(defn stage-form [duration]
  (let [[unit amount] (first (or duration {:beats 0}))]
    (merge {:unit (name unit) :value (str (if (number? amount) amount (:default amount 0)))}
           (cond (:variable amount) {:source :variable :path (pr-str (:variable amount))}
                 (:cue-id amount) {:source :cue :cue-id (:cue-id amount) :param (:param amount)}
                 :else {:source :fixed}))))

(defn stage-duration [{:keys [source unit value cue-id param path] :or {source :fixed}}]
  (let [fixed (performance/envelope-duration value unit)
        unit-key (keyword unit)
        fallback (get fixed unit-key)]
    (case source
      :fixed fixed
      :cue (if (and (some? cue-id) (some? param))
             {unit-key {:cue-id cue-id :param param :default fallback}}
             (throw (js/Error. "Choose a cue and a numeric parameter for the response time.")))
      :variable (let [route (try (reader/read-string (or path "")) (catch js/Error _ nil))]
                  (when-not (and (vector? route) (<= 1 (count route) 64) (every? #(or (keyword? %) (string? %)) route))
                    (throw (js/Error. "Enter the show variable as a nonempty vector of keys, such as [:audio/energy].")))
                  {unit-key {:variable route :default fallback}}))))

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
  (let [{:keys [source unit value cue-id param path]} (merge (stage-form stored) draft)
        title (if (= stage :attack) "Attack" "Release")
        edit #(write! {%1 %2})]
    [:div.policy-stage
     [:label title [:input {:type "text" :input-mode "decimal" :value value
                            :aria-label (str title (if (= source :fixed) " duration" " fallback duration"))
                            :on-change #(edit :value (.. % -target -value))}]]
     [:label "Unit" [:select {:aria-label (str title " unit") :value unit :on-change #(edit :unit (.. % -target -value))}
                     (for [u units] ^{:key u} [:option {:value u} u])]]
     [:label.policy-binding "Time source"
      [:select {:aria-label (str title " time source") :value (name source)
                :on-change #(edit :source (keyword (.. % -target -value)))}
       [:option {:value "fixed"} "Fixed duration"]
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
     (when (not= source :fixed) [:small "Duration is the fallback when the source is unavailable."])]))
