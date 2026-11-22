(ns tolglow.studio.spatial-picker
  "Autocomplete over the show's typed fixture, volume and definition index."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.projection :as projection]))

(defn show [db] (get-in db [:server-projection :view]))
(defn identity-of [db] (projection/identity-of (show db)))
(defn label [id] (if (keyword? id) (subs (str id) 1) (str id)))

(rf/reg-sub ::identity (fn [db _] (identity-of db)))
(rf/reg-sub ::result (fn [db [_ token]] (get-in db [::queries token])))
(rf/reg-event-fx ::query
  (fn [{:keys [db]} [_ owner token query]]
    (when (= owner (identity-of db))
      (let [request (str (random-uuid))]
        {:db (assoc-in db [::queries token] {:request request :waiting? true})
         :dispatch-later [{:ms 100 :dispatch [::fetch owner token request query]}]}))))
(rf/reg-event-fx ::fetch
  (fn [{:keys [db]} [_ owner token request query]]
    (when (and (= owner (identity-of db)) (= request (get-in db [::queries token :request])))
      {:studio/http {:path "/api/spatial/search" :typed? true :raw-response? true
                     :body (merge {:limit 16} query owner)
                     :on-success [::results owner token request true]
                     :on-failure [::results owner token request false]}})))
(rf/reg-event-db ::results
  (fn [db [_ owner token request ok? data]]
    (if (and (= owner (identity-of db)) (= request (get-in db [::queries token :request])))
      (assoc-in db [::queries token] (if ok? {:rows (:results data)} {:error (str data)}))
      db)))
(rf/reg-event-db ::close (fn [db [_ token]] (update db ::queries dissoc token)))

(defn properties [text]
  (let [m (if (str/blank? text) {} (reader/read-string text))]
    (when-not (and (map? m) (every? #{:attributes :params} (keys m)))
      (throw (js/Error. "Enter a map with :attributes or :params.")))
    m))

(defn picker
  "on-select receives the whole typed row, including its portable selector."
  [{:keys [title kinds selected on-select]}]
  (r/with-let [token (str (random-uuid)) opened (r/atom false) text (r/atom "") advanced (r/atom "")
               problem (r/atom nil) focus (r/atom 0)]
    (let [owner @(rf/subscribe [::identity])
          {:keys [rows waiting? error]} @(rf/subscribe [::result token])
          query! (fn []
                   (try (let [q (merge (properties @advanced) {:kinds kinds :text @text})]
                          (reset! problem nil) (reset! focus 0) (reset! opened true)
                          (rf/dispatch [::query owner token q]))
                        (catch js/Error e (reset! problem (.-message e)) (rf/dispatch [::close token]))))
          close! #(do (reset! opened false) (rf/dispatch [::close token]))
          choose! (fn [row] (when row (on-select row) (reset! text "") (close!)))]
      [:div.spatial-picker
       [:label [:span title]
        [:input {:type "search" :role "combobox" :aria-label title :auto-complete "off"
                 :aria-autocomplete "list" :aria-expanded @opened :aria-controls token
                 :aria-activedescendant (when (and @opened (get rows @focus)) (str token "-" @focus))
                 :placeholder (or selected "Search…") :value @text
                 :on-focus query! :on-click #(when-not @opened (query!))
                 :on-change #(do (reset! text (.. % -target -value)) (query!))
                 :on-key-down (fn [e] (case (.-key e)
                                       "ArrowDown" (do (.preventDefault e) (if @opened (swap! focus #(min (max 0 (dec (count rows))) (inc %))) (query!)))
                                       "ArrowUp" (do (.preventDefault e) (swap! focus #(max 0 (dec %))))
                                       "Enter" (when @opened (.preventDefault e) (choose! (get rows @focus)))
                                       "Escape" (close!) nil))}]]
       (when @opened
         [:div.spatial-results
          [:div {:role "listbox" :id token :aria-label title}
           (for [[i {:keys [kind id label] :as row}] (map-indexed vector rows)]
             ^{:key (pr-str [kind id])}
             [:button {:id (str token "-" i) :class (when (= i @focus) "focused")
                       :type "button" :role "option" :aria-selected (= i @focus)
                       :on-click #(choose! row)}
              [:span label] [:small (name kind)]])]
          (when (or error @problem waiting? (empty? rows))
            [:p.small {:role "status"} (cond @problem @problem error error waiting? "Searching…" :else "No matches")])
          [:details [:summary "Search by property"]
           [:label "Filters (EDN)"
            [:input {:aria-label "Registry property filters" :placeholder "{:attributes [:pan] :params {}}" :value @advanced
                     :on-change #(do (reset! advanced (.. % -target -value)) (query!))}]]]
          [:button.text-button {:type "button" :on-click close!} "Close search"]])])
    (finally (rf/dispatch [::close token]))))

(defn anchor-id [db incoming]
  (let [cues (:cues (show db)) ids (map :id cues)]
    (or (some #(when (= incoming %) %) ids)
        (let [same (filter #(= (label incoming) (label %)) ids)]
          (when (= 1 (count same)) (first same))))))
(rf/reg-event-fx ::launch
  (fn [{:keys [db]} [_ incoming selector]]
    (let [owner (identity-of db) cue (anchor-id db incoming)]
      (when (and cue selector (get-in db [:connection :connected?]))
        {:studio/http {:path "/api/command" :typed? true
                       :body {:op :cue/launch :cue-id cue :launch {:selector selector}
                              :epoch (:epoch owner) :runtime-id (:runtime-id owner) :command-id (str (random-uuid))
                              :source {:adapter :web :session-id (:session db)}}
                       :on-success [:studio/success "Cue launched on selected targets."]}}))))
(defn cue-target-picker [cue-id]
  (r/with-let [choice (r/atom nil)]
    (let [identity [@(rf/subscribe [::identity]) cue-id]
          selection (when (= identity (:identity @choice)) (:row @choice))]
      [:details.spatial-launch [:summary "Launch on selected targets"]
       [picker {:title "Launch targets" :kinds [:group :volume :fixture] :selected (:label selection)
                       :on-select #(reset! choice {:identity identity :row %})}]
       [:button.button {:disabled (nil? selection) :on-click #(rf/dispatch [::launch cue-id (:selector selection)])}
        (if selection (str "Launch on " (:label selection)) "Launch on targets")]])))
