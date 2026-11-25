(ns tolglow.studio.lighting-capacity
  (:require [re-frame.core :as rf]))

(defn- percent [x] (str (js/Math.round (* 100 x)) "%"))

(defn- change! [key value] (rf/dispatch [:connections/set-capacity {key value}]))

(defn- toggle [title key value pending?]
  [:label [:input {:type "checkbox" :checked (boolean value) :disabled pending?
                   :on-change #(change! key (.. % -target -checked))}] title])

(defn- bound [label key value minimum maximum pending?]
  [:label label
   [:select {:value (str value) :disabled pending?
             :on-change #(change! key (js/Number (.. % -target -value)))}
    (for [x (sort (conj (set (filter #(<= minimum % maximum) (map #(/ % 20) (range 21)))) value))]
      ^{:key x} [:option {:value (str x)} (percent x)])]])

(rf/reg-sub :lighting-capacity/settings
  (fn [db _]
    (merge (get-in db [:show :settings :lighting-capacity])
           (get-in db [:connections :capacity-draft]))))
(rf/reg-sub :lighting-capacity/output (fn [db _] (get-in db [:show :output :lighting-capacity])))
(rf/reg-sub :lighting-capacity/fixtures (fn [db _] (get-in db [:show :rig :fixtures])))
(rf/reg-sub :lighting-capacity/foggers :<- [:lighting-capacity/fixtures]
  (fn [fixtures _] (filterv #(get-in % [:channels :fog]) fixtures)))

(defn panel [pending?]
  (let [{:keys [enabled? follow-energy? minimum maximum fog? fog-fixture-ids]
         :or {minimum 0.1 maximum 0.9}} @(rf/subscribe [:lighting-capacity/settings])
        {:keys [utilization headroom rated-watts used-rated-watts rated-heads heads-count mode target writers color fog]}
        @(rf/subscribe [:lighting-capacity/output])
        foggers @(rf/subscribe [:lighting-capacity/foggers])]
    [:section.panel {:aria-label "Lighting capacity"}
     [:div.panel-top [:span.section-label "LIGHTING CAPACITY"]]
     [:div.form-body
      [toggle "Meter the rig" :enabled? enabled? pending?]
      (when enabled?
        [:<>
         (when (number? utilization)
           [:div
            [:strong (str (percent utilization) " of visible output")]
            [:meter {:min 0 :max 1 :value utilization :aria-label "Visible output as a fraction of rig capacity"
                     :style {:width "100%" :accent-color
                             (str "rgb(" (apply str (interpose "," (map #(js/Math.round (* 255 %)) color))) ")")}}]
            [:p.small (str (percent headroom) " headroom"
                           (when (number? target) (str " · " (percent target) " music target")))]
            (when (pos? rated-heads)
              [:p.small (str (js/Math.round used-rated-watts) " / " (js/Math.round rated-watts)
                             " W estimated emitter drive · " rated-heads " of " heads-count " heads rated")])
            (when (seq writers)
              [:p.small (str "Writing light · " (apply str (interpose ", " (distinct (map :effect-id writers)))))])])
         [toggle "Limit the look to audio energy" :follow-energy? follow-energy? pending?]
         (when follow-energy?
           [:<>
            [bound "Quiet target" :minimum minimum 0 maximum pending?]
            [bound "Loud target" :maximum maximum minimum 1 pending?]
            [:p.small.muted (case (some-> mode name)
                              "no-signal" "Waiting for fresh audio. The operator master is in control."
                              "bypassed" "Operator pause, blackout or calibration is in control."
                              "Target follows the smoothed RMS of the audio input.")]])
         (when (seq foggers)
           [:<>
            [toggle "Pulse selected foggers on high energy" :fog? fog? pending?]
            (when fog?
              [:fieldset
               [:legend "Fog outputs"]
               (for [{:keys [id name]} foggers]
                 ^{:key id} [:label [:input {:type "checkbox" :disabled pending?
                                             :checked (boolean (some #{id} fog-fixture-ids))
                                             :on-change #(change! :fog-fixture-ids
                                                            (if (.. % -target -checked)
                                                              (conj (vec fog-fixture-ids) id)
                                                              (filterv (partial not= id) fog-fixture-ids)))}]
                             (or name (str id))])
               [:p.small.muted (if (:active? fog) "Pulse active"
                                  "A high-energy crossing requests half channel drive for 0.4 s, with at least 6 s rest.")]])])])]]))
