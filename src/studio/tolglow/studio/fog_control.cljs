(ns tolglow.studio.fog-control
  (:require [re-frame.core :as rf]))

(rf/reg-sub :fog-control/settings (fn [db _] (get-in db [:show :settings :fog-control])))
(rf/reg-sub :fog-control/live (fn [db _] (get-in db [:show :output :fog-control])))
(rf/reg-sub :fog-control/fixtures (fn [db _] (filterv :fogger (get-in db [:show :rig :fixtures]))))

(defn wire-id [id] (if (keyword? id) (subs (str id) 1) (str id)))
(defn selected? [ids id] (boolean (some #{(wire-id id)} (map wire-id ids))))
(defn select-pump [ids id]
  (if (selected? ids id) (filterv #(not= (wire-id id) (wire-id %)) ids) (conj (vec ids) id)))

(defn- percent [x] (str (js/Math.round (* 100 x)) "%"))
(defn- change! [key value] (rf/dispatch [:connections/set-fog-control {key value}]))
(defn- bound [label key value minimum maximum disabled?]
  [:label label
   [:select {:aria-label label :disabled disabled? :value (str value)
             :on-change #(change! key (js/Number (.. % -target -value)))}
    (for [v (sort (conj (set (filter #(<= minimum % maximum) (map #(/ % 100) (range 101)))) value))]
      ^{:key v} [:option {:value (str v)} (percent v)])]])

(defn panel [pending?]
  (let [{:keys [enabled? target hysteresis fog-fixture-ids]
         :or {target 0.2 hysteresis 0.05 fog-fixture-ids []}} @(rf/subscribe [:fog-control/settings])
        {:keys [level lower upper zone fresh? pumps]} @(rf/subscribe [:fog-control/live])
        fixtures @(rf/subscribe [:fog-control/fixtures])]
    [:section.panel {:aria-label "Venue fog control"}
     [:div.panel-top [:span.section-label "VENUE FOG"]]
     [:div.form-body
      (if (number? level)
        [:div [:strong (str (percent level) " estimated light loss")]
         [:meter {:min 0 :max 1 :value level :aria-label "Estimated fog level" :style {:width "100%"}}]
         [:p.small (if (and fresh? zone) (str (name zone) " · target band " (percent lower) "–" (percent upper))
                       "Waiting for fresh fogger output")]]
        [:p.small "Awaiting an output frame from a profiled fogger."])
      [bound "Target light loss" :target target 0 1 pending?]
      [bound "Band either side" :hysteresis hysteresis 0.01 1 pending?]
      (when (seq fixtures)
        [:fieldset [:legend "Feedback pumps"]
         (for [{:keys [id name]} fixtures
               :let [selected? (selected? fog-fixture-ids id) mode (get-in pumps [id :mode])]]
           ^{:key (str id)}
           [:label [:input {:type "checkbox" :disabled pending? :checked (boolean selected?)
                            :on-change #(change! :fog-fixture-ids
                                          (select-pump fog-fixture-ids id))}]
            (str (or name id) (when (and selected? mode) (str " · " (cljs.core/name mode))))])])
      [:label [:input {:type "checkbox" :checked (boolean enabled?)
                       :disabled (or pending? (and (not enabled?) (empty? fog-fixture-ids)))
                       :on-change #(change! :enabled? (.. % -target -checked))}] "Arm automatic fog control"]]]))
