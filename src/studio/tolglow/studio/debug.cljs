(ns tolglow.studio.debug
  (:require [clojure.string :as str]
            [goog.dom :as dom]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.debug-socket]
            [tolglow.studio.subscriber-editor :as editor]))

(def retention 320)

(defn label [value] (if (keyword? value) (subs (str value) 1) (str value)))

(defn- emit! [event] (rf/dispatch event))

(defn- value [event] (.. event -target -value))

(defn- displayed-rows [debug] (or (:frozen debug) (:rows debug) []))

(defn- active? [db handle] (= handle (get-in db [:debug :handle])))

(defn- reply [db message]
  (let [{:keys [handle generation runtime]} (:debug db)]
    [handle generation message runtime]))

(rf/reg-event-fx :debug/open
  (fn [{:keys [db]} [_ handle]]
    {:db (assoc db :debug {:handle handle :tab :events :rows [] :filters {} :status :connecting})
     :debug/connect handle :dispatch [:subscribers/load]}))
(rf/reg-event-fx :debug/close
  (fn [{:keys [db]} [_ handle]]
    (when (active? db handle)
      {:db (dissoc db :debug) :debug/disconnect true})))
(rf/reg-event-fx :debug/socket-open
  (fn [{:keys [db]} [_ handle generation]]
    (when (active? db handle)
      (let [db (update db :debug #(-> % (assoc :generation generation :status :connected) (dissoc :error)))]
        (cond-> {:db db}
          (some? (get-in db [:debug :universe]))
          (assoc :debug/send (reply db {:op :dmx/subscribe :universe (get-in db [:debug :universe])})))))))
(rf/reg-event-db :debug/disconnected
  (fn [db [_ handle reason]]
    (if (active? db handle)
      (update db :debug #(-> % (dissoc :generation :packet) (assoc :status :reconnecting :error reason))) db)))

(defn accept-events [debug {:keys [runtime-id cursor events skipped]}]
  (let [same? (= runtime-id (:runtime debug))
        last-id (if same? (get debug :cursor -1) -1)
        rows (if same? (:rows debug) [])
        new-rows (filter #(and (int? (:id %)) (< last-id (:id %) (inc cursor))) events)]
    (if (< cursor last-id) debug
      (cond-> (assoc debug :runtime runtime-id :cursor cursor
                           :rows (vec (take-last retention (into rows new-rows)))
                           :skipped (+ (if same? (:skipped debug 0) 0) (or skipped 0)))
        (and (:runtime debug) (not same?))
        (dissoc :frozen :frozen-cursor :selected :packet)))))

(rf/reg-event-fx :debug/frame
  (fn [{:keys [db]} [_ handle generation {:keys [protocol kind token cursor universe bytes] :as frame}]]
    (when (and (active? db handle) (= generation (get-in db [:debug :generation]))
               (= protocol "tolglow-debug/1"))
      (case kind
        :events (when (and (string? (:runtime-id frame)) (vector? (:events frame)) (nat-int? cursor))
                  (let [db (update db :debug accept-events frame)]
                    {:db db :debug/send (reply db {:op :ack :cursor cursor})}))
        :dmx (when (string? token)
               (cond-> {:debug/send (reply db {:op :dmx/ack :token token})}
                 (and (= universe (get-in db [:debug :universe]))
                      (= (:runtime-id frame) (get-in db [:debug :runtime]))
                      (or (nil? bytes) (= 512 (.-length bytes))))
                 (assoc :db (assoc-in db [:debug :packet] (select-keys frame [:frame-number :epoch :bytes])))))
        nil))))

(rf/reg-event-db :debug/filter
  (fn [db [_ k v]] (-> db (assoc-in [:debug :filters k] v) (assoc-in [:debug :limit] 50))))
(rf/reg-event-db :debug/toggle-filters
  (fn [db _] (update-in db [:debug :filters-open?] not)))
(rf/reg-fx :debug/focus
  (fn [id]
    (r/after-render #(some-> (or (dom/getElement id) (dom/getElement "debug-filters-toggle")) .focus))))
(rf/reg-event-fx :debug/clear-filters
  (fn [{:keys [db]} _]
    {:db (update db :debug assoc :filters {} :limit 50) :debug/focus "debug-filter-type"}))
(rf/reg-event-db :debug/more (fn [db _] (update-in db [:debug :limit] #(+ (or % 50) 50))))
(defn- row-id [id] (str "debug-event-" id))
(rf/reg-event-fx :debug/select-row
  (fn [{:keys [db]} [_ id]]
    {:db (assoc-in db [:debug :selected] id)
     :debug/focus (if id "debug-inspector-title" (row-id (get-in db [:debug :selected])))}))
(rf/reg-event-db :debug/bank (fn [db [_ n]] (assoc-in db [:debug :bank] n)))
(rf/reg-event-db :debug/freeze
  (fn [db _]
    (update db :debug #(if (:frozen %) (dissoc % :frozen :frozen-cursor)
                         (assoc % :frozen (:rows %) :frozen-cursor (:cursor %))))))
(rf/reg-event-fx :debug/tab
  (fn [{:keys [db]} [_ tab]]
    (let [db (assoc-in db [:debug :tab] tab)]
      (if (and (not= tab :dmx) (get-in db [:debug :universe]))
        {:db (update db :debug dissoc :universe :packet) :debug/send (reply db {:op :dmx/unsubscribe})}
        {:db db}))))
(rf/reg-event-fx :debug/universe
  (fn [{:keys [db]} [_ universe]]
    (let [db (update db :debug #(-> % (assoc :universe universe :bank 0) (dissoc :packet)))]
      {:db db :debug/send (reply db (if (some? universe) {:op :dmx/subscribe :universe universe}
                                     {:op :dmx/unsubscribe}))})))

(rf/reg-sub :debug/state (fn [db _] (:debug db)))
(rf/reg-sub :debug/universes (fn [db _] (get-in db [:show :output :universes])))
(def filter-fields [[:type "Event type"] [:category "Category"] [:emitter "Emitter"] [:subscribers "Subscriber"]])

(defn- emitter-label [id]
  ;; EDN keeps keyword and string IDs distinct in the select's string values.
  (when (some? id) (pr-str id)))

(defn- field-values [row k]
  (case k
    :subscribers (map label (:subscribers row))
    :emitter (keep emitter-label [(get-in row [:emitter :id])])
    [(label (get row k))]))

(defn matches? [filters row]
  (every? (fn [[k selected]] (or (str/blank? selected) (some #{selected} (field-values row k)))) filters))

(defn- format-clock [stamp]
  (if (instance? js/Date stamp)
    (let [part #(-> % str (.padStart 2 "0"))]
      (str (part (.getHours stamp)) ":" (part (.getMinutes stamp)) ":" (part (.getSeconds stamp))
           "." (.padStart (str (.getMilliseconds stamp)) 3 "0"))) "—"))

(defn- filter-choice-label [field choice]
  (if (= field :emitter)
    (str choice (if (str/starts-with? choice ":") " (keyword)" " (text)"))
    choice))

(defn- filters [rows selections]
  [:div.debug-filters
   (for [[k title] filter-fields
         :let [selected (get selections k "")
               options (cond-> (set (mapcat #(field-values % k) rows)) (seq selected) (conj selected))]]
     ^{:key k} [:label title
                [:select {:id (str "debug-filter-" (name k)) :aria-label title :value selected :on-change #(emit! [:debug/filter k (value %)])}
                 [:option {:value ""} "All"]
                 (for [choice (sort options)]
                   ^{:key choice} [:option {:value choice} (filter-choice-label k choice)])]])])

(defn- subscribers-label [v]
  (cond (nil? v) "Unobserved" (empty? v) "None" :else (str/join ", " (map label v))))

(defn- event-toolbar [{:keys [filters-open? filters skipped frozen cursor frozen-cursor]} shown total]
  (let [active (keep (fn [[field title]]
                       (when-let [choice (not-empty (get filters field))]
                         (str title " · " choice)))
                     filter-fields)]
    [:div.debug-event-tools
     [:span.debug-count (str shown " of " total " events")]
     [:button#debug-filters-toggle.button {:type "button" :aria-expanded (boolean filters-open?)
                      :aria-controls "debug-filter-controls"
                      :title (when (seq active) (str/join ", " active))
                      :on-click #(emit! [:debug/toggle-filters])}
      "Filters" (when (seq active) [:span.debug-filter-count (count active)])]
     [:button.button {:type "button" :aria-pressed (some? frozen) :on-click #(emit! [:debug/freeze])}
      (if frozen (str "Resume · " (- (or cursor 0) (or frozen-cursor 0)) " new") "Pause feed")]
     (when (pos? (or skipped 0)) [:span.debug-gap (str skipped " missed upstream")])]))

(defn- event-list [debug]
  (let [rows (displayed-rows debug)
        visible (into [] (filter #(matches? (:filters debug) %)) (rseq rows))
        shown (take (:limit debug 50) visible)
        selected (some #(when (= (:id %) (:selected debug)) %) rows)]
    [:<>
     [event-toolbar debug (count shown) (count visible)]
     [:div#debug-filter-controls {:hidden (not (:filters-open? debug))}
      (when (:filters-open? debug)
        [:div.debug-filter-panel
         [filters rows (:filters debug)]
         (when (some (comp seq val) (:filters debug))
           [:button.button.debug-clear {:type "button" :on-click #(emit! [:debug/clear-filters])} "Clear filters"])])]
     (when selected
       [:section.debug-inspector
        [:div.debug-feed-tools [:h3#debug-inspector-title {:tab-index -1} (label (:type selected))] [:button.button {:on-click #(emit! [:debug/select-row nil])} "Close detail"]]
        [:dl (for [[k v] [["Event" (:id selected)] ["Emitter" (pr-str (:emitter selected))]
                          ["Subscribers" (subscribers-label (:subscribers selected))]
                          ["Delivery" (case (:delivery selected) :observed "Observed" :matched "Matched" "Unobserved")]
                          ["Timestamp" (some-> (:timestamp selected) .toISOString)]
                          ["Beat" (get-in selected [:time :beat])] ["Frame" (get-in selected [:time :frame-number])]
                          ["Epoch" (get-in selected [:time :epoch])]]]
               ^{:key k} [:div [:dt k] [:dd (str (or v "—"))]])]
        [:pre (pr-str (:data selected))]])
     (if (seq visible)
       [:div.debug-event-list
        [:div.debug-table-head
         [:span [:span.debug-time-heading "Local time"] [:span.debug-time-mobile-heading "Time / recipients"]]
         [:span "Event / emitter"] [:span "Subscribers"]]
        (for [{:keys [id timestamp type category emitter subscribers]} shown]
          ^{:key id} [:button.debug-row {:id (row-id id) :type "button" :data-category (label category) :aria-pressed (= (:id selected) id)
                                         :on-click #(emit! [:debug/select-row (when (not= id (:id selected)) id)])}
                      [:time {:date-time (when (instance? js/Date timestamp) (.toISOString timestamp))} (format-clock timestamp)]
                      [:span.debug-event-name [:strong (label type)]
                       [:small (str (label category) " · " (or (emitter-label (:id emitter)) "—"))]]
                      [:span.debug-recipients {:title (str "Subscribers · " (subscribers-label subscribers))}
                       (subscribers-label subscribers)]])]
       [:p.debug-empty (if (seq rows) "No events match these filters." "Waiting for emitted events.")])
     (when (< (count shown) (count visible))
       [:button.button {:on-click #(emit! [:debug/more])} "Show older events"])
     ]))

(defn- dmx [debug]
  (let [universes @(rf/subscribe [:debug/universes])
        channel-bank (:bank debug 0) start (* 64 channel-bank) packet (:packet debug)
        data (:bytes packet)]
    [:section.debug-dmx
     [:div.debug-feed-tools
      [:label "Output universe" [:select {:aria-label "Output universe" :value (str (:universe debug))
                                         :on-change #(let [v (value %)] (emit! [:debug/universe (when (seq v) (js/Number v))]))}
                                [:option {:value ""} "Select universe"]
                                (for [u universes] ^{:key u} [:option {:value (str u)} (str "Universe " u)])]]
      (when data [:span.debug-count (str "Frame " (:frame-number packet))])]
     (if data
       [:<> [:label.debug-bank "Channel bank" [:select {:aria-label "Channel bank" :value channel-bank :on-change #(emit! [:debug/bank (js/Number (value %))])}
                                               (for [i (range 8)] ^{:key i} [:option {:value i} (str (inc (* 64 i)) "–" (* 64 (inc i)))])]]
        [:div.debug-channels
         (for [i (range start (+ start 64)) :let [v (aget data i)]]
           ^{:key i} [:div.debug-channel {:aria-label (str "Channel " (inc i) ", value " v)
                                          :style {"--level" (str (* 100 (/ v 255)) "%")}}
                      [:small (inc i)] [:strong v]])]]
       [:p.debug-empty (cond (empty? universes) "No patched output universes."
                             (nil? (:universe debug)) "Select a universe to inspect its 512 output channels."
                             packet "This universe has no committed packet." :else "Waiting for a committed DMX packet.")])]))

(defn panel []
  (r/with-let [handle (str (random-uuid))
               _ (emit! [:debug/open handle])]
    (let [debug @(rf/subscribe [:debug/state])]
      [:section#view-debug.view.debug-view
       [:header.debug-heading [:h1 "Debug"] [:span.debug-status {:data-state (name (or (:status debug) :connecting))}
                                           (case (:status debug) :connected "Connected" :reconnecting "Reconnecting" "Connecting")]]
       [:div.debug-tabs {:aria-label "Debug views"}
        (for [[k title] [[:events "Event feed"] [:subscribers "Subscribers"] [:dmx "DMX output"]]]
          ^{:key k} [:button {:type "button" :aria-pressed (= k (:tab debug))
                              :on-click #(emit! [:debug/tab k])} title])]
       (when (:error debug) [:p.debug-gap {:role "status"} (:error debug)])
       (case (:tab debug) :subscribers [editor/panel] :dmx [dmx debug] [event-list debug])])
    (finally (emit! [:debug/close handle]))))
