(ns tolglow.studio.subscriber-editor
  (:require [clojure.string :as str]
            [cognitect.transit :as transit]
            [re-frame.core :as rf]
            [tolglow.transit :as wire]))

(def endpoint "/api/event-subscribers")
(defn- title [v] (if (keyword? v) (subs (str v) 1) (str v)))
(defn- raw [event] (.. event -target -value))
(defn- dispatch [event] (rf/dispatch event))
(defn- input! [path v] (rf/dispatch-sync [:subscribers/field path v]))
(defn- show-identity [db] (select-keys (:show db) [:runtime-id :epoch]))
(defn- guard [db] [(get-in db [:debug :handle]) (show-identity db)])
(defn- same-view? [db [handle identity]]
  (and handle (= handle (get-in db [:debug :handle]))
       (or (empty? identity) (= identity (show-identity db)))))
(defn- subscriber-map [value]
  (cond (vector? value) (into {} (map (juxt :id identity)) value)
        (map? value) value
        :else {}))
(defn- registered [db]
  (let [state (get-in db [:server-projection :view :event-subscribers])]
    (subscriber-map (or (:subscribers state) state (get-in db [:debug :catalog :subscribers])))))

;; This editor sends actual keywords and reads exact ID types, including namespaced IDs.
;; Form-oriented Studio HTTP transforms enums for display. This boundary keeps them typed.
(rf/reg-fx :subscribers/http
  (fn [{:keys [body ok fail]}]
    (-> (js/fetch endpoint (clj->js (cond-> {:method (if body "POST" "GET") :cache "no-store"
                                            :headers {"Accept" "application/transit+json"}}
                                     body (assoc :body (wire/encode body)
                                                 :headers {"Accept" "application/transit+json"
                                                           "Content-Type" "application/transit+json"}))))
        (.then (fn [response]
                 (-> (.text response)
                     (.then (fn [text]
                              (let [payload (wire/decode text)]
                                (if (and (.-ok response) (not= :rejected (:status payload)))
                                  (rf/dispatch (conj ok payload))
                                  (throw (js/Error. (or (:message payload) (some-> (:errors payload) pr-str)
                                                       (str "HTTP " (.-status response))))))))))))
        (.catch #(rf/dispatch (conj fail (.-message %)))))))

(rf/reg-event-fx :subscribers/runtime-change
  (fn [{:keys [db]} _]
    (cond-> {:db (update db :debug dissoc :saving? :form :editing-show :original :saved :catalog :catalog-error)}
      (get-in db [:debug :handle]) (assoc :dispatch [:subscribers/load]))))

(rf/reg-event-fx :subscribers/load
  (fn [{:keys [db]} _]
    (when (get-in db [:debug :handle])
      (let [g (guard db)]
        {:subscribers/http {:ok [:subscribers/catalog g] :fail [:subscribers/error g]}}))))
(rf/reg-event-db :subscribers/catalog
  (fn [db [_ g catalog]]
    (if (same-view? db g)
      (-> db (assoc-in [:debug :catalog] catalog) (update :debug dissoc :catalog-error)) db)))
(rf/reg-event-db :subscribers/error
  (fn [db [_ g message]]
    (if (same-view? db g)
      (-> db (assoc-in [:debug :catalog-error] message) (update :debug dissoc :saving?)) db)))

(def action-titles {:cue/launch "Launch cue" :effect/launch "Launch effect" :instance/end "Release effect"
                    :instance/kill "Kill effect" :instance/opacity "Set opacity" :callback "Custom callback"})
(defn new-action [op]
  (case op
    :callback {:op op :implementation {:type :var :symbol ""}}
    (:cue/launch :effect/launch) {:op op :select {:pick :random} :launch {:mode :persistent}}
    :instance/opacity {:op op :select {:pick :all} :value 1}
    {:op op :select {:pick :all}}))
(defn- seed-form [catalog]
  {:id "" :enabled? true :match (if-let [t (get-in catalog [:events 0 :type])] {:type t} {})
   :actions [(new-action :cue/launch)]})

(def ^:private decimal-id #"[+-]?\d+")
(def ^:private uuid-id
  #"#uuid\s+\"([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})\"")

(defn- pattern-text [id]
  (cond
    (transit/uuid? id) (pr-str id)
    (transit/bigint? id) (aget id "rep")
    (and (string? id)
         (or (not= id (str/trim id)) (re-matches decimal-id id)
             (some #(str/starts-with? id %) [":" "\"" "#uuid"])))
    (js/JSON.stringify id)
    :else (str id)))

(rf/reg-event-db :subscribers/edit
  (fn [db [_ subscriber]]
    (if-not (get-in db [:debug :handle]) db
      (let [form (or subscriber (seed-form (get-in db [:debug :catalog])))
            form (cond-> form (contains? (:match form) :pattern) (update-in [:match :pattern] pattern-text))]
        (-> db (assoc-in [:debug :form] form)
            (assoc-in [:debug :original] subscriber) (assoc-in [:debug :editing-show] (show-identity db))
            (update :debug dissoc :catalog-error :saved))))))
(rf/reg-event-db :subscribers/cancel
  (fn [db _] (update db :debug dissoc :form :original :editing-show :catalog-error)))
(rf/reg-event-db :subscribers/field
  (fn [db [_ path v]] (assoc-in db (into [:debug :form] path) v)))
(rf/reg-event-db :subscribers/append
  (fn [db _] (update-in db [:debug :form :actions] conj (new-action :cue/launch))))
(defn move-step [steps i dest]
  (cond
    (not (< -1 i (count steps))) steps
    (= :remove dest) (into (subvec steps 0 i) (subvec steps (inc i)))
    (and (int? dest) (< -1 dest (count steps))) (assoc steps i (nth steps dest) dest (nth steps i))
    :else steps))
(rf/reg-event-db :subscribers/step
  (fn [db [_ i dest]] (update-in db [:debug :form :actions] move-step i dest)))

(defn- interval-text [n]
  (if (wire/fraction? n) (str (:numerator n) "/" (:denominator n)) (str n)))
(defn- interval-value [source]
  (if-let [[_ n d] (re-matches #"([1-9][0-9]*)/([1-9][0-9]*)" source)]
    (wire/->BeatFraction n d)
    (let [v (js/Number source)]
      (when-not (and (re-matches #"[0-9]*\.?[0-9]+" source) (pos? v) (js/Number.isFinite v))
        (throw (js/Error. "Enter a positive musical interval such as 2 or 1/4.")))
      v)))
(defn- interval-map [m]
  (when-let [[k v] (first m)]
    {k (if (string? v) (interval-value v) v)}))
(defn- normalize-step [{:keys [delay select] :as action}]
  (cond-> (dissoc action :delay)
    (and select (empty? (:ids select))) (update :select dissoc :ids)
    (and select (empty? (:categories select))) (update :select dissoc :categories)
    (and select (nil? (:page-id select))) (update :select dissoc :page-id)
    (seq delay) (assoc :delay (interval-map delay))
    (get-in action [:launch :duration]) (update-in [:launch :duration] interval-map)))

(defn- named-id [id]
  (if (and (string? id) (str/starts-with? id ":")) (keyword (subs id 1)) id))

(defn- integer-pattern-id [text]
  (let [n (js/Number text)]
    (if (js/Number.isSafeInteger n)
      n
      (let [exact (js/BigInt text)]
        (if (< (js/BigInt "-9223372036854775809") exact (js/BigInt "9223372036854775808"))
          (transit/integer (str exact))
          (transit/bigint (str exact)))))))

(defn- pattern-id [text]
  (cond
    (not (string? text)) text
    (re-matches decimal-id text) (integer-pattern-id text)
    (str/starts-with? text "#uuid")
    (if-let [[_ id] (re-matches uuid-id text)]
      (uuid (str/lower-case id))
      (throw (js/Error. "Enter a UUID as #uuid followed by its quoted value.")))
    (str/starts-with? text "\"") (js/JSON.parse text)
    :else (named-id text)))

(defn- clean-match [match]
  (let [match (into {} (remove (comp nil? val)) match)]
    (cond-> match
      (contains? match :emitter-id) (update :emitter-id named-id)
      (not= :music/section-pattern (:type match)) (dissoc :pattern)
      (and (= :music/section-pattern (:type match)) (contains? match :pattern))
      (update :pattern pattern-id))))

(defn clean-form [form]
  (let [id (str/trim (title (:id form)))]
    (when (or (str/blank? id) (str/includes? id " ")) (throw (js/Error. "Enter a subscriber ID without spaces.")))
    (when (empty? (:actions form)) (throw (js/Error. "Add at least one action.")))
    (-> form (assoc :id (keyword id))
        (update :match clean-match)
        (update :actions #(mapv normalize-step %)))))
(rf/reg-event-fx :subscribers/submit
  (fn [{:keys [db]} [_ op]]
    (let [{:keys [original form editing-show saving?]} (:debug db)
          current (get (registered db) (:id original)) g (guard db)]
      (cond
        (or (not form) saving?) {}
        (or (not= editing-show (show-identity db)) (not= original current))
        {:db (assoc-in db [:debug :catalog-error] "This show or subscriber changed. Reopen it to edit the current version.")}
        :else
        (try
          (let [subscriber (if (= op :remove) original (clean-form form)) id (:id subscriber)]
            (when (and (nil? original) (contains? (registered db) id))
              (throw (js/Error. "A subscriber already has this ID.")))
            {:db (-> db (assoc-in [:debug :saving?] true) (update :debug dissoc :catalog-error :saved))
             :subscribers/http {:body (merge {:expected-epoch (:epoch editing-show) :runtime-id (:runtime-id editing-show)
                                              :expected-subscriber original}
                                              (if (= op :remove) {:op :remove :id id} {:op :save :subscriber subscriber}))
                                :ok [:subscribers/saved g op id] :fail [:subscribers/error g]}})
          (catch :default e {:db (assoc-in db [:debug :catalog-error] (.-message e))}))))))
(rf/reg-event-fx :subscribers/saved
  (fn [{:keys [db]} [_ g op id _receipt]]
    (when (same-view? db g)
      {:db (-> db (update :debug dissoc :form :original :editing-show :saving? :catalog-error)
               (assoc-in [:debug :saved] (str (if (= op :save) "Saved " "Removed ") (title id))))
       :dispatch [:subscribers/load]})))

(rf/reg-sub :subscribers/editor (fn [db _] (select-keys (:debug db) [:form :original :saving? :saved :catalog-error :catalog])))
(rf/reg-sub :subscribers/list (fn [db _] (sort-by (comp title :id) (vals (registered db)))))
(rf/reg-sub :subscribers/registry
  (fn [db _]
    (let [show (get-in db [:server-projection :view])]
      (select-keys show [:effects :cues :pages :instances :category-colors :runtime-id]))))
(defn- select-box [caption choice pairs change]
  [:label caption [:select {:aria-label caption :value (title choice)
                            :on-change #(change (some (fn [[k _]] (when (= (title k) (raw %)) k)) pairs))}
                   (for [[k text] pairs] ^{:key (title k)} [:option {:value (title k)} text])]])
(defn- choices [values] (mapv (fn [v] [v (title v)]) values))
(defn- tags [caption selected available change]
  [:div.debug-values
   [select-box caption nil (into [[nil "Add a filter…"]] (remove (comp (set selected) first) available))
    (fn [item] (when item (change (conj (vec selected) item))))]
   (for [item selected]
     ^{:key (title item)} [:button.debug-chip {:type "button" :aria-label (str "Remove " (title item))
                                              :on-click #(change (filterv (partial not= item) selected))}
                           (str (title item) " ×")])])
(defn- candidate-options [registry catalog op]
  (let [kinds (case op :cue/launch [:cues] :effect/launch [:effects] [:instances :cues :effects])]
    (->> (for [kind kinds
              :let [eligible (set (get-in catalog [:candidates kind]))]
              item (get registry kind)
              :when (contains? eligible (:id item))]
          [(:id item) (str (case kind :instances "Instance" :effects "Effect" "Cue") " · "
                           (or (:name item) (title (:id item))))])
         (into {}) (sort-by (comp title first)) vec)))

(defn- action-view [i action registry num-steps catalog]
  (let [path [:actions i] set-at (fn [field v] (input! (into path field) v))
        op (:op action) select (:select action)
        options (distinct (concat (:actions catalog) [op]))]
    [:fieldset.debug-action
     [:legend (str "Step " (inc i))]
     [:div.debug-action-head
      [select-box "Action" op (mapv (fn [v] [v (get action-titles v (title v))]) options)
       #(input! path (merge (select-keys action [:delay]) (new-action %)))]
      [:div.debug-order
       (for [[dest caption disabled?] [[(dec i) "↑" (zero? i)] [(inc i) "↓" (= i (dec num-steps))] [:remove "×" false]]]
         ^{:key caption} [:button.button {:disabled disabled? :type "button" :aria-label (str (case caption "↑" "Move up" "↓" "Move down" "Remove") " step " (inc i))
                                         :on-click #(dispatch [:subscribers/step i dest])} caption])]]
     (if (= op :callback)
       (let [{:keys [type symbol namespace form id]} (:implementation action)
             types [[:registered "Registered function"] [:var "Named Clojure var"] [:source "Clojure source"]]
             types (cond-> types (= :inline type) (conj [:inline "Process-local REPL function"]))]
         [:div.debug-fields
          [select-box "Callback" type types
           #(when (and % (not= :inline %))
              (set-at [:implementation] (case % :registered {:type :registered :id nil} :var {:type :var :symbol ""}
                                           {:type :source :namespace "user" :form "" :bindings {}})))]
          (case type
            :inline nil
            :registered [:label "Registered function ID" [:input {:value (title id) :placeholder "show/hit"
                                                                  :on-change #(set-at [:implementation :id] (some-> (raw %) not-empty keyword))}]]
            :var [:label "Function" [:input {:placeholder "show.effects/on-event" :value symbol :on-change #(set-at [:implementation :symbol] (raw %))}]]
            [:<> [:label "Namespace" [:input {:value namespace :on-change #(set-at [:implementation :namespace] (raw %))}]]
             [:label.debug-full "Callback source" [:textarea {:spell-check false :value form :placeholder "(fn [context event] context)"
                                                              :on-change #(set-at [:implementation :form] (raw %))}]]])])
       [:div.debug-fields
        [select-box "Choose" (:pick select) [[:random "Random match"] [:first "First match"] [:all "All matches"]]
         #(set-at [:select :pick] %)]
        [tags "Candidate IDs" (:ids select) (candidate-options registry catalog op)
         #(set-at [:select :ids] %)]
        [tags "Categories" (:categories select) (choices (sort-by title (keys (:category-colors registry)))) #(set-at [:select :categories] %)]
        [select-box "Page" (:page-id select) (into [[nil "All pages"]] (map (fn [{:keys [id name]}] [id (or name (title id))]) (:pages registry)))
         #(set-at [:select :page-id] %)]
        (when (#{:cue/launch :effect/launch} op)
          [select-box "Launch" (get-in action [:launch :mode] :persistent) [[:persistent "Persistent"] [:one-shot "One-shot"]]
           #(set-at [:launch] (if (= % :one-shot) {:mode % :duration {:beats 1}} {:mode %}))])
        (when (= :one-shot (get-in action [:launch :mode]))
          (let [[unit n] (or (first (get-in action [:launch :duration])) [:beats 1])]
            [:<> [select-box "Duration unit" unit [[:beats "Beats"] [:bars "Bars"] [:phrases "Phrases"]]
                  #(set-at [:launch :duration] {% n})]
             [:label "Duration" [:input {:value (interval-text n) :on-change #(set-at [:launch :duration] {unit (raw %)})}]]]))
        (when (= op :instance/opacity)
          [:label "Opacity" [:input {:type "number" :min 0 :max 1 :step 0.01 :value (:value action)
                                     :on-change #(set-at [:value] (js/Number (raw %)))}]])])
     (let [[unit amount] (first (:delay action))]
       [:div.debug-fields
        [select-box "Run after" unit [[nil "Immediately"] [:beats "Beats"] [:bars "Bars"] [:phrases "Phrases"]]
         #(set-at [:delay] (when % {% (or amount 1)}))]
        (when unit [:label "Interval" [:input {:value (interval-text amount)
                                               :on-change #(set-at [:delay] {unit (raw %)})}]])])]))
(defn panel []
  (let [{:keys [form original saving? saved catalog-error catalog]} @(rf/subscribe [:subscribers/editor])
        registry @(rf/subscribe [:subscribers/registry]) registered @(rf/subscribe [:subscribers/list])
        events (:events catalog) match (:match form)]
    [:section.debug-subscribers
     [:div.debug-feed-tools [:h2 "Event subscribers"] [:button.button.primary {:disabled (or saving? (nil? catalog) (nil? (:runtime-id registry)))
                                                                             :on-click #(dispatch [:subscribers/edit nil])} "New subscriber"]]
     (when catalog-error [:div.debug-gap {:role "alert"} catalog-error " " [:button.button {:on-click #(dispatch [:subscribers/load])} "Reload catalog"]])
     (when saved [:p.debug-status {:role "status"} saved])
     (if (seq registered)
       [:div.debug-registrations
        (for [{:keys [id enabled? match actions] :as sub} registered]
          ^{:key id} [:button.debug-registration {:disabled saving? :on-click #(dispatch [:subscribers/edit sub])}
                      [:strong (title id)] [:span (or (some-> (:type match) title) "Any event")]
                      [:small (str (count actions) " actions · " (if (false? enabled?) "Disabled" "Enabled"))]])]
       [:p.debug-empty (if catalog "No subscribers registered." "Loading subscriber catalog…")])
     (when form
       [:form.debug-editor {:on-submit (fn [e] (.preventDefault e) (dispatch [:subscribers/submit :save]))}
        [:fieldset {:disabled saving?}
         [:legend (if original "Edit subscriber" "New subscriber")]
         [:div.debug-fields
          [:label "Subscriber ID" [:input {:required true :disabled (some? original) :value (title (:id form))
                                          :placeholder "show/kick-accent" :on-change #(input! [:id] (raw %))}]]
          [select-box "Event" (:type match) (into [[nil "Any event"]] (choices (sort-by title (distinct (keep :type events)))))
           #(input! [:match :type] %)]
          [select-box "Event category" (:category match) (into [[nil "Any category"]] (choices (sort-by title (distinct (keep :category events)))))
           #(input! [:match :category] %)]
          [:label "Emitter ID" [:input {:value (str (:emitter-id match)) :placeholder ":audio/input or device ID"
                                        :on-change #(let [v (raw %)] (input! [:match :emitter-id] (not-empty v)))}]]
          (when (= :music/section-pattern (:type match))
            [:label "Pattern ID" [:input {:value (str (:pattern match)) :placeholder ":slow, slow, 42, or #uuid \"…\""
                                          :on-change #(input! [:match :pattern] (not-empty (str/trim (raw %))))}]])
          [:label.debug-check [:input {:type "checkbox" :checked (get form :enabled? true)
                                       :on-change #(input! [:enabled?] (.. % -target -checked))}] "Enabled"]]
         (for [[i action] (map-indexed vector (:actions form))]
           ^{:key i} [action-view i action registry (count (:actions form)) catalog])
         [:div.debug-form-actions
          [:button.button {:type "button" :on-click #(dispatch [:subscribers/append])} "Add step"]
          [:button.button.primary {:type "submit"} (if saving? "Saving…" "Save subscriber")]
          [:button.button {:type "button" :on-click #(dispatch [:subscribers/cancel])} "Cancel"]
          (when original [:button.button {:type "button" :on-click #(dispatch [:subscribers/submit :remove])} "Remove subscriber"])]]])
     [:details.debug-catalog [:summary "Registered event types"]
      (if (seq events) [:ul (for [{:keys [type category]} events] ^{:key type} [:li [:code (title type)] [:small (title category)]])]
        [:p.debug-empty "Event catalog unavailable."])]]))
