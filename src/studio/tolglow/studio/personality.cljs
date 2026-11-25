(ns tolglow.studio.personality
  "Pick the show's active track-feel personality. The show draws every icon;
  this namespace displays that SVG and sends the same operation the REPL does."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.projection :as projection]))

(defn- view [db] (get-in db [:server-projection :view]))

(defn- identity-key [db] (projection/identity-of (view db)))

(defn- form [db]
  (let [stored (:personality-ui db) identity (identity-key db)]
    (if (= identity (:identity stored)) stored {:identity identity})))

(defn option-key
  "A select or DOM key that keeps a keyword ID apart from the same text."
  [id]
  (str (if (keyword? id) "k:" "s:") (events/wire-id id)))

(defn icon-src
  "An image source for the SVG text the show projected. An <img> never runs
  markup inside it."
  [svg]
  (str "data:image/svg+xml;charset=utf-8," (js/encodeURIComponent svg)))

(defn icon
  "The personality's own picture at a square size in CSS pixels."
  [{:keys [svg size label]}]
  (when (string? svg)
    [:img.personality-icon {:src (icon-src svg) :width size :height size :alt (or label "") :draggable false}]))

(defn- css-color [[r g b]]
  (let [channel #(js/Math.round (* 255 (max 0 (min 1 %))))]
    (str "rgb(" (channel r) "," (channel g) "," (channel b) ")")))

(defn paint-css
  "A CSS background for one palette entry: a colour, a gradient or hard-edged bands."
  [entry]
  (cond
    (vector? entry) (css-color entry)
    (:gradient entry)
    (str "linear-gradient(" (+ 90 (:angle-deg entry 90)) "deg,"
         (str/join "," (map (fn [{:keys [position color]}] (str (css-color color) " " (* 100 position) "%"))
                            (:gradient entry))) ")")
    (:split entry)
    (let [bands (count (:split entry))]
      (str "linear-gradient(" (+ 90 (:angle-deg entry 0)) "deg,"
           (str/join "," (map-indexed (fn [index color]
                                        (str (css-color color) " " (* 100 (/ index bands)) "% " (* 100 (/ (inc index) bands)) "%"))
                                      (:split entry))) ")"))
    :else "transparent"))

(defn- percent [share] (js/Math.round (* 100 share)))

(defn form-summary [forms]
  (str/join " · " (map (fn [{:keys [form weight]}] (str (name form) " " (percent weight) "%")) (take 3 forms))))

(defn base-id
  "The heaviest part of the active mix; the first one when parts are equal."
  [active]
  (when-let [parts (seq (:mix active))]
    (:id (apply max-key :weight (reverse parts)))))

(defn blend-mix
  "Mix the heaviest current part with another personality. share is the other's part in percent."
  [active other share]
  (let [base (base-id active)]
    (when (and (some? base) (some? other) (not= base other) (< 0 share 100))
      [{:id base :weight (/ (- 100 share) 100)} {:id other :weight (/ share 100)}])))

(defn request
  "The effect map for one operation, or nil while offline or awaiting a reply."
  [db operation]
  (let [v (view db) state (form db)
        token [(identity-key db) (str (random-uuid))]]
    (when (and (get-in db [:connection :connected?]) (nil? (:pending state)))
      {:db (assoc db :personality-ui (-> state (assoc :pending token) (dissoc :error)))
       :studio/http {:path "/api/personality" :typed? true
                     :body (assoc operation :expected-epoch (:epoch v) :runtime-id (:runtime-id v))
                     :on-success [::finished token] :on-failure [::failed token]}})))

(rf/reg-sub ::projection (fn [db _] (:personalities (view db))))
(rf/reg-sub ::library :<- [::projection] (fn [projection _] (:library projection)))
(rf/reg-sub ::active :<- [::projection] (fn [projection _] (:active projection)))
(rf/reg-sub ::form (fn [db _] (form db)))
(rf/reg-sub ::online? (fn [db _] (true? (get-in db [:connection :connected?]))))
(rf/reg-event-db ::edit
  (fn [db [_ key value]] (assoc db :personality-ui (-> (form db) (assoc key value) (dissoc :error)))))
(rf/reg-event-fx ::send (fn [{:keys [db]} [_ operation]] (or (request db operation) {})))
(rf/reg-event-db ::finished
  (fn [db [_ token _]]
    (let [state (form db)]
      (if (= token (:pending state)) (assoc db :personality-ui (dissoc state :pending)) db))))
(rf/reg-event-db ::failed
  (fn [db [_ token error]]
    (let [state (form db)]
      (if (= token (:pending state))
        (assoc db :personality-ui (-> state (dissoc :pending) (assoc :error (str error)))) db))))

(defn- swatches [palette]
  [:span.personality-swatches {:role "img" :aria-label (str (count palette) " palette entries")}
   (for [[index entry] (map-indexed vector palette)]
     ^{:key index} [:i {:style {:background (paint-css entry)}}])])

(defn- current [active]
  (if active
    [:div.personality-current
     [icon {:svg (:icon active) :size 48 :label ""}]
     [:div
      [:span.section-label "TRACK FEEL"]
      [:strong (:name active)]
      [swatches (:palette active)]]]
    [:div.personality-current.empty
     [:span.personality-none {:aria-hidden true}]
     [:div
      [:span.section-label "TRACK FEEL"]
      [:strong "No track feel"]
      [:span.small.muted "Effects keep their authored shapes."]]]))

(defn- blend-controls [active library state disabled?]
  (let [base (base-id active)
        others (remove #(= base (:id %)) library)
        chosen (or (some #(when (= (:other state) (option-key (:id %))) %) others) (first others))
        share (:share state 30)]
    (when chosen
      [:div.personality-blend
       [:label "Blend with"
        [:select {:value (option-key (:id chosen)) :aria-label "Personality to blend in"
                  :on-change #(rf/dispatch-sync [::edit :other (.. % -target -value)])}
         (for [{:keys [id name]} others] ^{:key (option-key id)} [:option {:value (option-key id)} name])]]
       [:label [:span "Share" [:output (str share "%")]]
        [:input {:type "range" :min 5 :max 95 :step 5 :value share :aria-label "Share of the blended personality"
                 :on-change #(rf/dispatch-sync [::edit :share (js/Number (.. % -target -value))])}]]
       [:button.button {:type "button" :disabled disabled?
                        :on-click #(rf/dispatch [::send {:op :blend :mix (blend-mix active (:id chosen) share)}])}
        "Blend"]])))

(defn panel
  "Show the active personality. Opened, it lets the operator choose, blend,
  reshuffle or clear it."
  []
  (let [library @(rf/subscribe [::library]) active @(rf/subscribe [::active])
        state @(rf/subscribe [::form])
        disabled? (boolean (or (:pending state) (not @(rf/subscribe [::online?]))))
        single (when (= 1 (count (:mix active))) (:id (first (:mix active))))]
    (when (seq library)
      [:section.personality-panel.panel {:aria-label "Track feel"}
       [:details
        [:summary [current active]]
        (when active
          [:p.small.muted.personality-detail
           (str (form-summary (:forms active)) " · seed " (:seed active)
                (when (false? (:current? active)) " · edited since it was selected"))])
        [:div.personality-choices {:role "radiogroup" :aria-label "Personality"}
         (for [{:keys [id name] :as entry} library
               :let [selected? (= id single)]]
           ^{:key (option-key id)}
           [:button {:type "button" :role "radio" :aria-checked selected? :disabled disabled? :title (:description entry)
                     :on-click #(rf/dispatch [::send {:op :select :id id}])}
            [icon {:svg (:icon entry) :size 40 :label ""}]
            [:span name]])]
        (when active [blend-controls active library state disabled?])
        [:div.performance-actions
         [:button.button {:type "button" :disabled (or disabled? (nil? active))
                          :on-click #(rf/dispatch [::send {:op :blend :mix (mapv (fn [part] (select-keys part [:id :weight])) (:mix active))
                                                           :seed (rand-int 1000000)}])}
          "Reshuffle shapes"]
         [:button.button {:type "button" :disabled (or disabled? (nil? active))
                          :on-click #(rf/dispatch [::send {:op :clear}])}
          "Clear feel"]]]
       (when (:error state) [:p.performance-error {:role "alert"} (:error state)])])))
