(ns tolglow.studio.signal-picker
  "Choose a read-only show signal for one material input."
  (:require [cljs.tools.reader.edn :as edn]
            [cljs.tools.reader.reader-types :as readers]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(defn value-type [value]
  (cond
    (vector? value) (case (count value) 2 :vec2 3 :vec3 4 :vec4 :number)
    (map? value) (get value :type :number)
    :else :number))

(defn fixed-value [type]
  (case type :vec2 [0 0] :vec3 [0 0 0] :vec4 [0 0 0 0] 0))

(defn read-path [text]
  (try
    (let [input (readers/string-push-back-reader text)
          eof (gensym)
          read! #(edn/read {:eof eof} input)
          path (read!)]
      (when (and (vector? path) (seq path) (identical? eof (read!))
                 (every? #(or (keyword? %) (string? %) (nat-int? %)) path))
        path))
    (catch :default _ nil)))

(defn- path-title [path]
  (str/join " / " (map #(if (keyword? %) (subs (str %) 1) (pr-str %)) path)))

(defn- owner [db]
  (select-keys (get-in db [:server-projection :view]) [:runtime-id :show-id :epoch]))

(defn- current? [db id token]
  (let [picker (::picker db)]
    (and (= id (:id picker)) (= token (:token picker)) (= (:owner picker) (owner db)))))

(rf/reg-sub ::state
  (fn [db [_ id]]
    (let [picker (::picker db)]
      (when (and (= id (:id picker)) (= (:owner picker) (owner db))) picker))))

(defn- search [db id type query delay-ms]
  (let [token (random-uuid)]
    {:db (assoc db ::picker {:id id :type type :query query :token token :owner (owner db)})
     :dispatch-later [{:ms delay-ms :dispatch [::fetch id token]}]}))

(rf/reg-event-fx ::open
  (fn [{:keys [db]} [_ id type]] (search db id type "" 0)))

(rf/reg-event-fx ::filter
  (fn [{:keys [db]} [_ id query]]
    (let [picker (::picker db)]
      (when (current? db id (:token picker))
        (search db id (:type picker) query 200)))))

(rf/reg-event-fx ::fetch
  (fn [{:keys [db]} [_ id token]]
    (when (current? db id token)
      {:studio/http {:path "/api/variable-block/sources"
                     :body (assoc (select-keys (::picker db) [:type :query]) :readable? true)
                     :typed? true :raw-response? true
                     :on-success [::found id token] :on-failure [::failed id token]}})))

(rf/reg-event-db ::found
  (fn [db [_ id token result]]
    (if (current? db id token) (assoc-in db [::picker :sources] (:sources result)) db)))

(rf/reg-event-db ::failed
  (fn [db [_ id token message]]
    (if (current? db id token) (assoc-in db [::picker :error] message) db)))

(rf/reg-event-db ::close
  (fn [db [_ id]] (if (= id (get-in db [::picker :id])) (dissoc db ::picker) db)))

(defn- manual-entry [type apply!]
  (r/with-let [text (r/atom "")]
    (let [path (read-path @text)]
      [:details.signal-manual
       [:summary "Enter a path"]
       [:label "Path within show variables"
        [:input {:aria-label "Signal path" :value @text :placeholder "[:audio :features :low-rms]"
                 :on-change #(reset! text (.. % -target -value))}]]
       [:button.button.secondary {:type "button" :disabled (nil? path)
                                  :on-click #(apply! path type)} "Use path"]])))

(defn- choices [{:keys [id type query sources error]} apply!]
  [:div.signal-choices {:role "group" :aria-label "Choose a live signal"}
   [:input {:type "search" :aria-label "Find signal" :placeholder "Find a signal…" :max-length 256
            :value query :on-change #(rf/dispatch-sync [::filter id (.. % -target -value)])}]
   (cond
     error [:p.muted {:role "alert"} error]
     (nil? sources) [:p.muted {:role "status"} "Searching…"]
     (empty? sources) [:p.muted {:role "status"} "No matching signals"]
     :else [:div.signal-options
            (for [{:keys [path type]} sources
                  :let [relative (subvec path 1)]]
              ^{:key (pr-str path)}
              [:button {:type "button" :on-click #(apply! relative type)} (path-title relative)])])
   [manual-entry type apply!]])

(defn picker [label value type change!]
  (r/with-let [id (random-uuid)]
    (let [ui @(rf/subscribe [::state id])
          path (when (= :variable (:op value)) (or (:path value) [(:key value)]))
          close! #(rf/dispatch [::close id])
          select! (fn [path type] (change! {:op :variable :path path :type type}) (close!))]
      [:div.signal-picker
       [:button.signal-trigger {:type "button" :aria-label (str label " live signal") :aria-expanded (boolean ui)
                                :on-click #(if ui (close!) (rf/dispatch [::open id type]))}
        (if path (path-title path) "Map signal")]
       (when ui [choices ui select!])])
    (finally (rf/dispatch [::close id]))))
