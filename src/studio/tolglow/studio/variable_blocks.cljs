(ns tolglow.studio.variable-blocks
  "Compose and inspect global show-variable bindings."
  (:require [cljs.tools.reader.edn :as edn]
            [cljs.tools.reader.reader-types :as readers]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.block-builder :as builder]
            [tolglow.studio.block-inspector :as inspector]
            [tolglow.studio.block-trace :as trace]
            [tolglow.studio.visibility :refer [watch-visibility]]))

(defn- view [db] (get-in db [:server-projection :view]))

(defn- owner [db] (select-keys (view db) [:runtime-id :show-id :epoch]))

(defn- settings [db] (get-in (view db) [:settings :variable-blocks]))

(defn- form [db]
  (let [ui (::ui db) identity (owner db)]
    (if (= identity (:owner ui)) ui {:owner identity})))

(defn- update-form [db f & args] (assoc db ::ui (apply f (form db) args)))

(defn- text-of [e] (.. e -target -value))

(defn- label [path] (trace/path-label (rest path)))

(defn parse-path [s]
  (let [parts (str/split (str/trim s) #"\s+")
        ids (mapv #(str/replace % #"^:" "") parts)]
    (when (and (seq ids) (every? #(re-matches #"[^\s:\[\]{},/]+(?:/[^\s:\[\]{},/]+)?" %) ids))
      (into [:variables] (map keyword) ids))))

(rf/reg-sub ::ui (fn [db _] (form db)))
(rf/reg-sub ::settings (fn [db _] (settings db)))
(rf/reg-sub ::online (fn [db _] (get-in db [:connection :connected?])))
(rf/reg-sub ::revision (fn [db _] (:revision (view db))))

(rf/reg-event-db ::edit
  (fn [db [_ k v]] (update-form db #(-> % (assoc k v) (dissoc :error :notice)))))
(rf/reg-event-fx ::toggle
  (fn [{:keys [db]} _]
    (let [open? (not (:open? (form db)))]
      (cond-> {:db (update-form db assoc :open? open?)}
        open? (assoc :dispatch [::search])))))

(rf/reg-event-fx ::search
  (fn [{:keys [db]} _]
    (let [query (:search (form db) "") token (random-uuid)]
      {:db (update-form db assoc :search-token token)
       :studio/http {:path "/api/variable-block/sources" :body {:query query} :typed? true :raw-response? true
                     :on-success [::sources (owner db) token]
                     :on-failure [::failed-search (owner db) token]}})))

(defn- matches? [db identity token token-key]
  (and (= identity (owner db)) (= token (get (form db) token-key))))
(rf/reg-event-db ::sources
  (fn [db [_ identity token {:keys [sources]}]]
    (if (matches? db identity token :search-token) (update-form db assoc :sources sources :search-error nil) db)))
(rf/reg-event-db ::failed-search
  (fn [db [_ identity token message]]
    (if (matches? db identity token :search-token) (update-form db assoc :search-error message) db)))

(defn- choose [db path type]
  (let [saved (get-in (settings db) [:bindings path])
        spec (or saved {:stages []})
        base (:base spec)]
    (update-form db #(-> % (dissoc :error :notice :confirmed :pending)
                         (assoc :path path :expected saved :stages (:stages spec) :type (or (:type spec) type :number)
                                :fixed? (contains? spec :base) :base-str (if (some? base) (pr-str base) "0.0"))))))

(rf/reg-event-db ::choose
  (fn [db [_ path type]]
    (if (and path (nil? (:pending (form db)))) (choose db path type) db)))

(defn- read-base [source]
  (let [input (readers/string-push-back-reader source)
        eof (gensym "end")
        read! #(edn/read {:eof eof} input)
        value (read!)]
    (when (or (identical? eof value) (not (identical? eof (read!))))
      (throw (ex-info "Enter one dry value or expression." {})))
    value))

(defn- desired [{:keys [fixed? base-str expected stages type]}]
  (cond-> (assoc (or expected {}) :stages (vec stages))
    (or (not= type :number) (contains? expected :type)) (assoc :type type)
    fixed? (assoc :base (read-base base-str))
    (not fixed?) (dissoc :base)))

(rf/reg-event-fx ::submit
  (fn [{:keys [db]} [_ operation]]
    (let [{:keys [owner path expected pending] :as ui} (form db)
          token (random-uuid)]
      (when (and path (:runtime-id owner) (get-in db [:connection :connected?]) (not pending))
        (try
          (let [value (when (= :put operation) (desired ui))]
            {:db (update-form db #(-> % (assoc :pending token) (dissoc :error :notice)))
             :studio/http {:path "/api/variable-block" :typed? true :raw-response? true
                           :body (cond-> {:op operation :path path :expected expected :runtime-id (:runtime-id owner)
                                          :expected-epoch (:epoch owner)}
                                   (= :put operation) (assoc :binding value))
                           :on-success [::submitted owner token value]
                           :on-failure [::failed owner token]}})
          (catch js/Error e {:db (update-form db assoc :error (.-message e))}))))))

(rf/reg-event-db ::submitted
  (fn [db [_ identity token value receipt]]
    (if-not (matches? db identity token :pending) db
      (if (= :accepted (:status receipt))
        (update-form db #(cond-> (assoc % :pending nil :expected value :confirmed (:revision receipt)
                                         :notice (if value "Variable chain saved." "Variable chain removed."))
                           (nil? value) (assoc :path nil :stages [])))
        (update-form db assoc :pending nil :error (or (:message receipt) "The change has not been confirmed."))))))
(rf/reg-event-db ::failed
  (fn [db [_ identity token message]]
    (if (matches? db identity token :pending) (update-form db assoc :pending nil :error message) db)))

(defn- picker [title choices on-select]
  [:label title
   [:select {:value "" :on-change #(when-some [i (parse-long (text-of %))]
                                     (let [{:keys [path type]} (get choices i)] (on-select path type)))}
    [:option {:value ""} "Choose a variable…"]
    (for [[i {:keys [path]}] (map-indexed vector choices)]
      ^{:key (pr-str path)} [:option {:value i} (label path)])]])

(defn- find-variable [ui settings]
  (let [choose! #(rf/dispatch-sync [::choose %1 %2])
        saved (mapv (fn [path] {:path path}) (sort-by pr-str (keys (:bindings settings))))]
    [:div.form-body
     (when (seq saved) [picker "Saved chains" saved choose!])
     [:form.form-row {:on-submit (fn [e] (.preventDefault e) (rf/dispatch [::search]))}
      [:label "Find live variables" [:input {:type "search" :max-length 256 :value (:search ui "")
                                             :on-change #(rf/dispatch-sync [::edit :search (text-of %)])}]]
      [:button.button {:type "submit"} "Search sources"]]
     (when-let [error (:search-error ui)] [:p {:role "alert"} error])
     (when (contains? ui :sources)
       (if (seq (:sources ui))
         [picker "Live sources" (:sources ui) choose!]
         [:p.muted {:role "status"} "No matching numeric sources. You can still enter a path."]))
     [:details [:summary "Enter a variable path"]
      [:form.form-row {:on-submit (fn [e] (.preventDefault e)
                                    (when-let [p (parse-path (:manual ui ""))] (choose! p nil)))}
       [:label "Nested keys, separated by spaces" [:input {:value (:manual ui "") :placeholder "weather density"
                                                          :on-change #(rf/dispatch-sync [::edit :manual (text-of %)])}]]
       [:button.button {:type "submit" :disabled (nil? (parse-path (:manual ui "")))} "Use path"]]]]))

(defn- live-trace [path]
  (let [query [:runtime/block-trace nil path]
        value @(rf/subscribe [:studio/remote query])]
    (if-let [issue (:error value)]
      [:p {:role "status"} issue]
      [inspector/history-plot query])))

(defn- chain-editor [ui settings visible?]
  (let [library (when visible? @(rf/subscribe [:studio/remote [:runtime/block-library]]))
        revision @(rf/subscribe [::revision])
        online? @(rf/subscribe [::online])
        {:keys [path expected confirmed]} ui
        current (get-in settings [:bindings path])
        changed? (and (or (nil? confirmed) (>= revision confirmed)) (not= expected current))
        edit! #(rf/dispatch-sync [::edit %1 %2])]
    [:div.form-body.variable-block-editor
     [:h3 (label path)]
     (when changed? [:div {:role "alert"} "Another session changed this binding. "
                     [:button.button {:type "button" :on-click #(rf/dispatch [::choose path (:type ui)])} "Reload binding"]])
     [:div.form-row
      [:label "Dry input"
       [:select {:value (if (:fixed? ui) "fixed" "live") :on-change #(edit! :fixed? (= "fixed" (text-of %)))}
        [:option {:value "live"} "Live source"] [:option {:value "fixed"} "Fixed value"]]]
      [:label "Signal type"
       [:select {:value (name (:type ui)) :on-change #(edit! :type (keyword (text-of %)))}
        (for [[type title] [[:number "Number"] [:vec2 "2 components"] [:vec3 "3 components"] [:vec4 "4 components"]]]
          ^{:key type} [:option {:value (name type)} title])]]]
     (when (:fixed? ui)
       [:label "Dry value or expression" [:input {:value (:base-str ui) :on-change #(edit! :base-str (text-of %))}]])
     [builder/stage-editor {:stages (:stages ui) :library (merge library (:prefabs settings))
                            :parameters [] :transport-only? true :on-change #(edit! :stages %)}]
     [:div.button-row
      [:button.button.primary {:type "button" :disabled (or (not online?) changed? (:pending ui))
                               :on-click #(rf/dispatch [::submit :put])} "Save variable chain"]
      (when expected [:button.button {:type "button" :disabled (or (not online?) changed? (:pending ui))
                                      :on-click #(rf/dispatch [::submit :remove])} "Remove chain"])]
     (when expected
       [:div.block-trace-viewport [watch-visibility
                                   (fn [visible?] (when visible? [live-trace path]))]])]))

(defn panel []
  (let [ui @(rf/subscribe [::ui]) saved @(rf/subscribe [::settings])]
    [:section.panel.variable-blocks {:aria-label "Show variable chains"}
     [:div.panel-top [:h2 "Show variable chains"]
      [:button.button {:type "button" :aria-expanded (boolean (:open? ui))
                       :on-click #(rf/dispatch [::toggle])} (if (:open? ui) "Close editor" "Edit variable chains")]]
     (when (:open? ui)
       [:div.variable-block-workspace
        [:fieldset {:disabled (boolean (:pending ui))} [find-variable ui saved]]
        (when (:path ui)
          [watch-visibility (fn [visible?] [chain-editor ui saved visible?])])
        (when-let [error (:error ui)] [:p.form-body {:role "alert"} error])
        (when-let [message (:notice ui)] [:p.form-body {:role "status"} message])])]))
