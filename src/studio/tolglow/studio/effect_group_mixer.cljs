(ns tolglow.studio.effect-group-mixer
  "A mixer for the committed effect channels in Studio’s show projection."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.projection :as projection]))

(def ^:private overlay-options [[:fixtures "Fixtures"] [:categories "Categories"] [:cue-pages "Cue pages"]])

(def ^:private ui-path [:ui :effect-group-mixer])

(def ^:private unconfirmed "The change could not be confirmed. Check the committed value before trying again.")

(defn- show-view [db] (get-in db [:server-projection :view]))

(defn- connected? [db] (and (get-in db [:connection :connected?]) (not (:awaiting-full? db))))

(defn- state [db]
  (let [{:keys [revision] :as view} (show-view db)
        identity (projection/identity-of view)
        local (get-in db ui-path)]
    (if (= identity (:identity local))
      (update local :requests (fn [requests]
                                (into {} (remove (fn [[_ request]]
                                                   (when-let [committed (:revision request)]
                                                     (<= committed revision)))) requests)))
      {:identity identity :closed #{} :requests {}})))

(defn- group-level [view target]
  (let [{:keys [manual overlay]} (:effect-group-channels view)
        roots (if (= :manual (first target)) manual overlay)]
    (some (fn [group] (when (= target (:id group)) (:level group)))
          (mapcat #(tree-seq :groups :groups %) roots))))

(defn- valid-target? [view target value]
  (if (= target :overlay)
    (and (contains? view :effect-group-channels)
         (or (nil? value) (contains? (get-in view [:settings :effect-group-channels :overlays]) value)))
    (and (vector? target) (number? value) (js/Number.isFinite value) (<= 0 value 1)
         (some? (group-level view target)))))

(defn- committed-value [view target]
  (if (= target :overlay)
    (get-in view [:effect-group-channels :active-overlay])
    (group-level view target)))

(defn- request-effect [db identity target {:keys [id sent]}]
  (let [body (merge identity {:command-id id :source {:adapter :web :session-id (:session db)}}
                    (if (= target :overlay)
                      {:op :effect-groups/overlay-set :overlay sent}
                      {:op :effect-groups/level-set :channel-id target :value sent}))]
    [:studio/http {:path "/api/command" :typed? true :raw-response? true :body body
                   :timeout-ms 6500
                   :on-success [:effect-group-mixer/received identity target id]
                   :on-failure [:effect-group-mixer/failed identity target id]
                   :on-uncertain [:effect-group-mixer/retry identity target id]}]))

(defn- resolve-request [db local target request]
  {:db (assoc-in db ui-path (assoc-in local [:requests target] request))})

(rf/reg-event-db :effect-group-mixer/toggle
  (fn [db [_ id]]
    (update-in (assoc-in db ui-path (state db)) (conj ui-path :closed)
               #(if (contains? % id) (disj % id) (conj % id)))))

(rf/reg-event-fx :effect-group-mixer/edit
  (fn [{:keys [db]} [_ target value]]
    (let [view (show-view db)
          {:keys [identity requests] :as local} (state db)
          request (get requests target)]
      (when (and (connected? db) (valid-target? view target value)
                 (or (and request (not (:error request))) (not= value (committed-value view target))))
        (cond-> (resolve-request db local target (assoc (dissoc request :error :revision) :value value))
          (or (nil? request) (:error request) (:revision request))
          (assoc :dispatch-later [{:ms 80 :dispatch [:effect-group-mixer/flush identity target]}]))))))

(rf/reg-event-fx :effect-group-mixer/flush
  (fn [{:keys [db]} [_ identity target]]
    (let [local (state db)
          {:keys [value id error] :as request} (get-in local [:requests target])]
      (when (and request (= identity (:identity local)) (nil? id) (nil? error))
        (if (and (connected? db) (valid-target? (show-view db) target value))
          (let [request (assoc request :id (str (random-uuid)) :sent value)]
            (assoc (resolve-request db local target request) :fx [(request-effect db identity target request)]))
          (resolve-request db local target {:error "This group control is no longer available."}))))))

(defn- matching-request [local identity target id]
  (let [request (get-in local [:requests target])]
    (when (and (= identity (:identity local)) (= id (:id request)) (nil? (:revision request))) request)))

(rf/reg-event-fx :effect-group-mixer/received
  (fn [{:keys [db]} [_ identity target id receipt]]
    (let [local (state db)]
      (when-let [{:keys [value sent]} (matching-request local identity target id)]
        (case (:status receipt)
          :pending {:dispatch [:effect-group-mixer/retry identity target id]}
          :accepted (if (and (= id (:command-id receipt)) (= (:epoch identity) (:epoch receipt))
                             (= (:show-id identity) (:show-id receipt))
                             (integer? (:revision receipt)))
                      (if (= value sent)
                        (resolve-request db local target {:value sent :revision (:revision receipt)})
                        (assoc (resolve-request db local target {:value value})
                               :dispatch [:effect-group-mixer/flush identity target]))
                      (resolve-request db local target {:error unconfirmed}))
          (resolve-request db local target {:error unconfirmed}))))))

(rf/reg-event-fx :effect-group-mixer/retry
  (fn [{:keys [db]} [_ identity target id]]
    (let [local (state db)]
      (when-let [request (matching-request local identity target id)]
        (if (and (connected? db) (not (:retried? request)))
          (assoc (resolve-request db local target (assoc request :retried? true))
                 :fx [(request-effect db identity target request)])
          (resolve-request db local target {:error unconfirmed}))))))

(rf/reg-event-db :effect-group-mixer/failed
  (fn [db [_ identity target id message]]
    (let [local (state db)]
      (if (matching-request local identity target id)
        (:db (resolve-request db local target {:error message}))
        db))))

(defn panel-state [db]
  (let [view (show-view db)]
    (assoc (state db) :mixer (:effect-group-channels view) :connected? (connected? db)
           :overlays (get-in view [:settings :effect-group-channels :overlays]))))

(rf/reg-sub :effect-group-mixer/panel (fn [db _] (panel-state db)))

(defn membership
  "Use the owner’s ordered IDs for each group and show overlapping overlay matches."
  [group overlay? instances-by-id]
  (let [ids (if overlay? (:matching-instance-ids group) (:direct-instance-ids group))
        winners (when overlay? (set (:instance-ids group)))]
    (into [] (keep (fn [id]
                     (when-let [instance (get instances-by-id id)]
                       (cond-> instance overlay? (assoc :controls-level? (contains? winners id)))))) ids)))

(defn- percent [value]
  (str (str/replace (.toFixed (* 100 value) 1) #"\.0$" "") "%"))

(defn- pending? [request] (and request (nil? (:error request))))

(defn- instance-row [{:keys [name phase member-level controls-level?]} depth]
  [:li.effect-group-mixer-instance {:style {"--group-depth" (min depth 4)}}
   [:span.effect-group-mixer-instance-name name]
   [:span.effect-group-mixer-instance-meta
    (when (= phase :releasing) [:span "Releasing"])
    (when (some? controls-level?) [:span (if controls-level? "Controls level" "Match only")])
    [:span {:title "Combined group level"} [:span.sr-only "Combined group level "] (percent member-level)]]])

(defn- group-node [{:keys [id name level groups instance-ids matching-instance-ids] :as group}
                   {:keys [closed requests connected? instances-by-id] :as state} overlay? depth]
  (let [members (membership group overlay? instances-by-id)
        expandable? (or (seq groups) (seq members))
        open? (and expandable? (not (contains? closed id)))
        request (get requests id)
        pending (pending? request)
        member-count (count instance-ids)
        match-count (count matching-instance-ids)
        committed (percent level)]
    [:li.effect-group-mixer-group
     [:div.effect-group-mixer-channel {:style {"--group-depth" (min depth 4)}}
      [:div.effect-group-mixer-channel-name
       (if expandable?
         [:button.effect-group-mixer-disclosure
          {:type "button" :aria-expanded (boolean open?) :aria-label (str "Members of " name)
           :on-click #(rf/dispatch [:effect-group-mixer/toggle id])}
          [:span.effect-group-mixer-chevron {:aria-hidden true}]
          [:span name]]
         [:span.effect-group-mixer-empty-name name])
       [:small (if (and overlay? (> match-count member-count))
                 (str member-count " controlled · " match-count " matches")
                 (str member-count " " (if (= member-count 1) "effect" "effects")))]]
      [:input.effect-group-mixer-volume
       {:type "range" :min 0 :max 1 :step 0.01 :value (if pending (:value request) level)
        :aria-label (str (if overlay? "Overlay" "Manual") " group volume for " name)
        :aria-valuetext (percent (if pending (:value request) level)) :disabled (not connected?)
        :on-change #(rf/dispatch-sync [:effect-group-mixer/edit id (.. % -target -valueAsNumber)])}]
      [:output.effect-group-mixer-level {:aria-label (str "Committed group volume for " name)} committed]]
     (when open?
       [:ul.effect-group-mixer-rows
        (for [child groups]
          ^{:key (pr-str (:id child))} [group-node child state overlay? (inc depth)])
        (for [instance members]
          ^{:key (:instance-id instance)} [instance-row instance (inc depth)])])]))

(defn- loose-members [label instances]
  (when (seq instances)
    [:div.effect-group-mixer-loose
     [:h4 label]
     [:ul.effect-group-mixer-rows
      (for [instance instances]
        ^{:key (:instance-id instance)} [instance-row instance 0])]]))

(defn panel []
  (let [{:keys [mixer overlays connected? requests] :as state} @(rf/subscribe [:effect-group-mixer/panel])
        {:keys [manual overlay instances active-overlay]} mixer
        selected (get requests :overlay)
        state (assoc state :instances-by-id (into {} (map (juxt :instance-id identity)) instances))]
    [:section.panel.effect-group-mixer {:aria-label "Effect group mixer"}
     [:header.effect-group-mixer-heading
      [:h2 "Group mixer"]
      (when (some pending? (vals requests)) [:span.effect-group-mixer-pending {:role "status"} "Applying…"])
      [:label "Overlay"
       [:select {:value (or (some-> (if (pending? selected) (:value selected) active-overlay) name) "")
                 :disabled (or (not connected?) (empty? overlays))
                 :on-change (fn [event]
                              (let [option (.. event -target -value)]
                                (rf/dispatch-sync [:effect-group-mixer/edit :overlay (when-not (str/blank? option) (keyword option))])))}
        [:option {:value ""} "Off"]
        (for [[id label] overlay-options :when (contains? overlays id)]
          ^{:key id} [:option {:value (name id)} label])]]]
     (for [[target request] requests :when (:error request)]
       ^{:key (pr-str target)} [:p.effect-group-mixer-error {:role "alert"} (:error request)])
     (cond
       (nil? mixer) [:p.effect-group-mixer-empty "Waiting for effect groups."]
       (and (empty? manual) (empty? overlay) (empty? instances)) [:p.effect-group-mixer-empty "No groups or running effects."]
       :else
       [:div.effect-group-mixer-sections
        [:div.effect-group-mixer-section
         [:h3 "Manual groups"]
         (when (seq manual)
           [:ul.effect-group-mixer-rows
            (for [group manual]
              ^{:key (pr-str (:id group))} [group-node group state false 0])])
         [loose-members "Ungrouped" (filter (comp empty? :manual-path) instances)]]
        (when active-overlay
          [:div.effect-group-mixer-section
           [:h3 "Overlay groups"]
           (when (seq overlay)
             [:ul.effect-group-mixer-rows
              (for [group overlay]
                ^{:key (pr-str (:id group))} [group-node group state true 0])])
           [loose-members "No overlay match" (filter (comp nil? :overlay-group) instances)]])])]))
