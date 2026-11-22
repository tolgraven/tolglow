(ns tolglow.studio.lighting-scenes
  "Live performance controls for groups of light-emitting fixtures."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.projection :as projection]))

(defn- view [db] (get-in db [:server-projection :view]))
(defn- identity-key [db] (projection/identity-of (view db)))
(defn- form [db]
  (let [stored (:lighting-scene-ui db) identity (identity-key db)]
    (if (= identity (:identity stored)) stored {:identity identity})))

(defn- label [id] (str/replace (events/wire-id id) #"-" " "))
(defn- option-key [id] (str (if (keyword? id) "group:" "group-string:") (events/wire-id id)))
(defn eligible-fixtures [fixtures eligible-ids]
  (let [ids (set eligible-ids)]
    (filterv #(contains? ids (:id %)) fixtures)))

(defn choices [fixtures saved]
  (let [all-ids (mapv :id fixtures)
        patch-groups (reduce (fn [by-group {:keys [id groups]}]
                               (reduce #(update %1 %2 (fnil conj []) id) by-group groups))
                             {} fixtures)
        options (merge {:studio/all-lights all-ids} patch-groups
                       (into {} (map (juxt :group-id :fixture-ids)) saved))]
    (into [{:value "custom" :label "Custom fixture group…"}]
          (map (fn [[id ids]] {:value (option-key id) :id id :ids ids
                               :label (if (= :studio/all-lights id) "All lights" (label id))}))
          (sort-by (fn [[id]] [(if (= :studio/all-lights id) 0 1) (events/wire-id id)]) options))))

(defn- custom-id [name]
  (let [slug (-> (or name "") str/trim str/lower-case (str/replace #"\s+" "-"))]
    (when (re-matches #"[a-z0-9][a-z0-9._-]*" slug) (keyword "studio.scene" slug))))

(defn selection [state options members]
  (let [option (or (some #(when (= (:value %) (:selected state)) %) options) (second options))
        custom? (= "custom" (:value option))
        id (if custom? (custom-id (:name state)) (:id option))
        ids (if custom? (:picked state []) (:ids option))
        elsewhere (into #{} (mapcat :fixture-ids) (remove #(= id (:group-id %)) members))
        occupied? (boolean (some elsewhere ids))]
    {:option option :id id :ids ids :occupied? occupied?
     :custom? custom?}))

(defn- active-scene [id groups] (some #(when (= id (:group-id %)) %) groups))

(defn- requested-percent [state choice scene]
  (get-in state [:percent (get-in choice [:option :value])]
          (if-let [target (:target scene)] (js/Math.round (* target 100)) 50)))

(rf/reg-sub ::rig (fn [db _] (get-in (view db) [:rig :fixtures])))
(rf/reg-sub ::eligible (fn [db _] (get-in (view db) [:lighting-scenes :eligible-fixture-ids])))
(rf/reg-sub ::fixtures :<- [::rig] :<- [::eligible] (fn [[fixtures ids] _] (eligible-fixtures fixtures ids)))
(rf/reg-sub ::saved (fn [db _] (get-in (view db) [:lighting-scenes :groups])))
(rf/reg-sub ::members :<- [::saved] (fn [groups _] (mapv #(select-keys % [:group-id :fixture-ids]) groups)))
(rf/reg-sub ::group-count :<- [::members] (fn [members _] (count members)))
(rf/reg-sub ::choices :<- [::fixtures] :<- [::members] (fn [[fixtures groups] _] (choices fixtures groups)))
(rf/reg-sub ::form (fn [db _] (form db)))
(rf/reg-sub ::selection :<- [::form] :<- [::choices] :<- [::members]
  (fn [[state options members] _] (selection state options members)))
(rf/reg-sub ::active :<- [::selection] :<- [::saved]
  (fn [[choice groups] _] (active-scene (:id choice) groups)))
(rf/reg-sub ::online? (fn [db _] (true? (get-in db [:connection :connected?]))))
(rf/reg-event-db ::edit
  (fn [db [_ path value]]
    (assoc db :lighting-scene-ui (-> (form db) (assoc-in path value) (dissoc :error)))))
(rf/reg-event-db ::pick
  (fn [db [_ id checked?]]
    (assoc db :lighting-scene-ui
           (update (form db) :picked (fn [ids] (if checked? (vec (distinct (conj (vec ids) id))) (filterv #(not= % id) ids)))))))

(defn request [db op]
  (let [v (view db) {:keys [eligible-fixture-ids groups]} (:lighting-scenes v)
        fixtures (eligible-fixtures (get-in v [:rig :fixtures]) eligible-fixture-ids)
        state (form db)
        {:keys [id ids occupied?] :as choice} (selection state (choices fixtures groups) groups)
        scene (active-scene id groups)
        percent (requested-percent state choice scene)
        key (get-in choice [:option :value])
        edited (get-in state [:percent key])
        reference (get-in state [:brightness-reference key])
        manual? (or (= :apply op) (and (= :vary op) (some? edited) (nil? (:target-var scene))))
        valid? (and id (seq ids) (not occupied?) (every? (set eligible-fixture-ids) ids)
                    (integer? percent) (<= 0 percent 100))
        token [(identity-key db) (str (random-uuid))]]
    (when (and (get-in db [:connection :connected?]) (nil? (:pending state))
               (case op :release scene :apply valid? :vary (and valid? scene) false))
      {:db (assoc db :lighting-scene-ui (-> state (assoc :pending token) (dissoc :error)))
       :studio/http {:path "/api/lighting-scene" :typed? true
                     :body (cond-> {:op op :group-id id :command-id (second token)
                                    :expected-epoch (:epoch v) :runtime-id (:runtime-id v)}
                             (not= :release op) (assoc :fixture-ids ids)
                             (and (not= :release op) reference) (assoc :brightness-reference reference)
                             manual? (assoc :target (/ percent 100)))
                     :on-success [::finished token id op key (when manual? edited) reference] :on-failure [::failed token]}})))

(rf/reg-event-fx ::send (fn [{:keys [db]} [_ op]] (or (request db op) {})))
(rf/reg-event-db ::finished
  (fn [db [_ token id op key edited reference _]]
    (let [state (form db)]
      (if (= token (:pending state))
        (assoc db :lighting-scene-ui (cond-> (dissoc state :pending)
                                       (= edited (get-in state [:percent key])) (update :percent dissoc key)
                                       (= reference (get-in state [:brightness-reference key])) (update :brightness-reference dissoc key)
                                       (and (not= :release op) (= "custom" (:selected state))) (assoc :selected (option-key id)))) db))))
(rf/reg-event-db ::failed
  (fn [db [_ token error]]
    (let [state (form db)]
      (if (= token (:pending state)) (assoc db :lighting-scene-ui (-> state (dissoc :pending) (assoc :error (str error)))) db))))

(defn- edit! [path value] (rf/dispatch-sync [::edit path value]))
(defn- custom-fields [state fixtures]
  (let [search (str/lower-case (str/trim (:search state "")))
        matches (filter #(str/includes? (str/lower-case (str (:name %) " " (events/wire-id (:id %)))) search) fixtures)
        picked (set (:picked state))]
    [:div.scene-custom
     [:label "Group name" [:input {:type "text" :value (:name state "") :placeholder "Front wash"
                                  :on-change #(edit! [:name] (.. % -target -value))}]]
     [:label "Find fixtures" [:input {:type "search" :value (:search state "")
                                      :on-change #(edit! [:search] (.. % -target -value))}]]
     [:fieldset [:legend (str (count picked) " fixtures selected")]
      (for [{:keys [id name]} (take 40 matches)]
        ^{:key (events/wire-id id)}
        [:label [:input {:type "checkbox" :checked (contains? picked id)
                         :on-change #(rf/dispatch-sync [::pick id (.. % -target -checked)])}]
         (or name (label id))])
      (when (seq (drop 40 matches)) [:p.small.muted "Search to narrow the fixture list."])]]))

(defn- percent-text [n] (when (number? n) (str (js/Math.round (* 100 n)) "%")))
(def status-label {:at-target "At target" :seeking "Adjusting" :limited "Output limited"
                   :over-target "Other light exceeds target" :unlit "No light available"})

(defn- source-readings [{:keys [sources source-count]}]
  [:details.source-readings
   [:summary (str "Compare fixture brightness · " source-count)]
   [:table [:thead [:tr [:th {:scope "col"} "Fixture"] [:th {:scope "col"} "Local"]
                   [:th {:scope "col"} "Rig scale"] [:th {:scope "col" :title "Full white at maximum output, relative to the brightest fixture"} "Max. white"]]]
    [:tbody (for [{:keys [fixture-id local-brightness global-brightness global-maximum global-color-maximum limited?]} sources]
              ^{:key (option-key fixture-id)}
              [:tr [:th {:scope "row"} (events/wire-id fixture-id)
                    (when limited? [:small (if (number? global-color-maximum)
                                              (str "Color limit " (percent-text global-color-maximum)) "At limit")])]
               [:td (or (percent-text local-brightness) "—")]
               [:td (or (percent-text global-brightness) "—")]
               [:td (or (percent-text global-maximum) "—")]])]]
   (when (< (count sources) source-count)
     [:p.small.muted (str "Showing the first " (count sources) " of " source-count " fixtures.")])])

(defn panel []
  (let [state @(rf/subscribe [::form]) options @(rf/subscribe [::choices])
        group-count @(rf/subscribe [::group-count]) eligible @(rf/subscribe [::eligible])
        {:keys [option id ids custom? occupied?] :as choice} @(rf/subscribe [::selection])
        scene @(rf/subscribe [::active])
        percent (requested-percent state choice scene)
        group-key (:value option)
        reference (get-in state [:brightness-reference group-key] (:brightness-reference scene :local))
        disabled? (or (:pending state) (not @(rf/subscribe [::online?])))
        invalid? (or disabled? (nil? id) (empty? ids) occupied?)
        status (:status scene)]
    [:section.scene-controls.panel {:aria-label "Fixture group lighting"}
     [:header [:h2 "Group lighting"]
      (when (pos? group-count) [:span.small.muted (str group-count " active")])]
     [:div.scene-fields
      [:label "Fixture group"
       [:select {:value group-key :disabled (boolean (:pending state)) :aria-label "Lighting fixture group"
                 :on-change #(edit! [:selected] (.. % -target -value))}
        (for [{:keys [value label ids]} options]
          ^{:key value} [:option {:value value} (str label (when ids (str " · " (count ids))))])]]
      [:label "Brightness reference"
       [:select {:value (name reference) :disabled (boolean disabled?)
                 :on-change #(edit! [:brightness-reference group-key] (keyword (.. % -target -value)))}
        [:option {:value "local"} "Local · fixture capacity"]
        [:option {:value "global"} "Match · brightest fixture in the rig"]]]
      [:label.scene-target [:span "Brightness target" [:output (str percent "%")]]
       [:input {:type "range" :min 0 :max 100 :step 1 :value percent :aria-label "Group light target"
                :on-change #(edit! [:percent group-key] (js/Number (.. % -target -value)))}]]]
     (when custom? [custom-fields state @(rf/subscribe [::fixtures])])
     (when scene
       [:div.scene-reading {:class (when (#{:limited :over-target :unlit} status) "unmet")}
        [:span (str "Output " (or (percent-text (:actual scene)) "unavailable") " · requested " (percent-text (:target scene)))]
        [:strong (get status-label status "Awaiting measurement")]
        (when-let [path (:target-var scene)] [:span (str "Following " (str/join " / " (map events/wire-id path)))])
        (when-let [source (:source-effect-id scene)] [:span (label source)])])
     (when (seq (:sources scene)) [source-readings scene])
     [:div.performance-actions
      [:button.button {:type "button" :disabled (boolean invalid?) :on-click #(rf/dispatch [::send :apply])} (if (:target-var scene) "Set manual target" "Apply target")]
      [:button.button {:type "button" :disabled (boolean (or invalid? (nil? scene))) :on-click #(rf/dispatch [::send :vary])} "Something different"]
      [:button.button {:type "button" :disabled (boolean (or disabled? (nil? scene))) :on-click #(rf/dispatch [::send :release])} "Release group"]]
     (cond (:error state) [:p.performance-error {:role "alert"} (:error state)]
           occupied? [:p.performance-hint "Another active group owns fixtures in this selection."]
           (and custom? (seq (:name state)) (nil? id)) [:p.performance-hint "Use letters, numbers, spaces, dots or hyphens in the group name."]
           (and (some? eligible) (empty? eligible)) [:p.performance-hint "Patch a light-emitting fixture to set a target."])]))
