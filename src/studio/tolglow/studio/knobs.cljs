(ns tolglow.studio.knobs
  "Linked knobs panel (#666). Reads the shared :knobs projection and sends the
  same :knob/* owner commands as the REPL and Push. See docs/linked-knobs.md."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.db-slice :as db-slice]
            [tolglow.studio.events :as events]))

(def ^:private ui-path [:ui :knobs])

(def curves [[:linear "Linear"] [:ease-in "Ease in"] [:ease-out "Ease out"] [:ease-in-out "Ease in-out"]
             [:smoothstep "Smoothstep"] [:sine "Sine"]])

(def quantize-choices [["" "Continuous"] ["integer" "Whole numbers"] ["preferred" "Preferred beat ratios"]
                       ["free" "Free beat ratios"]])

(def ^:private numeric-types #{:number :integer})

(defn- type-of [schema] (some-> (:type schema) events/wire-id keyword))

(defn- choice-key [destination]
  (pr-str (into (sorted-map) (update-vals destination #(if (keyword? %) (events/wire-id %) %)))))

(defn- owner-of [instance]
  (if-let [cue (:cue-id instance)] {:cue-id cue} {:instance-id (:id instance)}))

(defn- owner-label [instance] (events/wire-id (or (:cue-id instance) (:id instance))))

(def ^:private period-schema {:type :number :bounds [0.125 8] :unit :beats :semantic/type :beat-ratio})

(defn destination-choices
  "Numeric destinations on running launches: effect parameters, the fields of
  bound waveforms and their registered inputs. A cue is addressed by cue ID, so
  the link follows relaunches; a launch without a cue by its instance ID."
  [show]
  (let [waveforms (into {} (map (juxt :id identity)) (:parameter-waveforms show))
        instances (->> (:instances show) (filter #(= "running" (events/wire-id (:phase % "running"))))
                       (sort-by :order))
        seen (volatile! #{})]
    (vec
     (for [instance instances
           :let [params (get-in instance [:definition :params])]
           choice (concat
                   (for [[param schema] (sort-by (comp str key) params)
                         :when (numeric-types (type-of schema))]
                     {:destination (merge {:kind :param :param param} (owner-of instance))
                      :schema schema :label (str (owner-label instance) " · " (name param))})
                   (for [[param {:keys [sources]}] (sort-by (comp str key) (:parameter-sources instance))
                         source sources
                         :when (= "lfo" (events/wire-id (:type source)))
                         :let [schema (get params param)
                               waveform (get waveforms (get-in source [:binding :waveform]))
                               base (merge {:kind :lfo :param param} (owner-of instance))
                               label (str (owner-label instance) " · " (name param) " LFO ")]
                         entry (concat
                                (when (numeric-types (type-of schema))
                                  [{:destination (assoc base :field :from) :schema schema :label (str label "low")}
                                   {:destination (assoc base :field :to) :schema schema :label (str label "gain (high)")}])
                                [{:destination (assoc base :field :period) :schema period-schema :label (str label "period")}]
                                (for [[input input-schema] (sort-by (comp str key) (:params waveform))
                                      :when (numeric-types (type-of input-schema))]
                                  {:destination (assoc base :input input) :schema input-schema
                                   :label (str label (name input))}))]
                     entry))
           :let [key (choice-key (:destination choice))]
           :when (not (@seen key))]
       (do (vswap! seen conj key) (assoc choice :key key))))))

(defn- parse-number [text]
  (let [text (str/trim (str text))]
    (cond
      (re-matches #"-?\d+/\d+" text) (let [[n d] (map js/parseInt (str/split text #"/"))]
                                       (when (and (js/isFinite n) (pos? d)) (/ n d)))
      (re-matches #"-?(\d+\.?\d*|\.\d+)" text) (js/parseFloat text)
      :else nil)))

(defn default-form
  "Starting endpoints for a destination: its bounds, or two to half a beat for a period."
  [{:keys [schema destination]}]
  (let [[low high] (or (:bounds schema) [0 1])
        period? (= :period (:field destination))]
    {:from (str (if period? 2 low)) :to (str (if period? 0.5 high)) :direction "up" :curve "linear"
     :quantize (if period? "preferred" "") :input-low "0" :input-high "1"}))

(defn link-payload
  "Build a :knob/link link from the add form, or {:error message}."
  [{:keys [choice form variable link-id]}]
  (let [from (parse-number (:from form)) to (parse-number (:to form))
        low (parse-number (:input-low form "0")) high (parse-number (:input-high form "1"))
        destination (if (= "variable" choice)
                      {:kind :variable :variable (keyword (str/trim (str variable)))}
                      (:destination choice))
        quantize (case (:quantize form) "integer" :integer "preferred" {:ratios :preferred} "free" {:ratios :free} nil)]
    (cond
      (and (= "variable" choice) (str/blank? (str variable))) {:error "Name the show variable."}
      (nil? destination) {:error "Choose a destination."}
      (not (and (number? from) (number? to))) {:error "Enter numbers for From and To, such as 0.25 or 1/2."}
      (not (and (number? low) (number? high) (<= 0 low) (< low high) (<= high 1)))
      {:error "The knob window needs Start below End, both between 0 and 1."}
      :else
      {:link (cond-> {:id link-id :destination destination :range [from to]
                      :direction (keyword (:direction form "up")) :curve (keyword (:curve form "linear"))
                      :input [low high]}
               quantize (assoc :quantize quantize))})))

(defn edited-link
  "A projected link with one field changed, ready to send back as :knob/link."
  [link field value]
  (-> (select-keys link [:id :destination :range :direction :curve :input :quantize])
      (assoc field value)
      (cond-> (nil? (:quantize link)) (dissoc :quantize))))

(defn- next-link-id [knob]
  (let [ids (set (map (comp events/wire-id :id) (:links knob)))]
    (first (remove ids (map #(str "link-" %) (iterate inc (inc (count ids))))))))

;; Events

(defn- send [db op fields]
  (let [knob (events/wire-id (:knob-id fields))
        [effect request] (events/command db op fields nil nil)]
    {:db (update-in db ui-path dissoc :error)
     :fx [[effect (assoc request :on-failure [::failed knob])]]}))

(rf/reg-event-db ::failed
  (fn [db [_ _ message]] (assoc-in db (conj ui-path :error) (str message))))

(rf/reg-event-db ::edit (fn [db [_ path value]] (assoc-in db (into ui-path path) value)))

(rf/reg-event-fx ::create
  (fn [{:keys [db]} _]
    (let [name (str/trim (str (get-in db (conj ui-path :new-name))))
          existing (set (map (comp events/wire-id :id) (get-in db [:show :knobs :knobs])))
          id (first (remove existing (map #(str "knob-" %) (iterate inc 1))))]
      (if (str/blank? name)
        {:db (assoc-in db (conj ui-path :error) "Name the knob first.")}
        (-> (send db :knob/create {:knob-id id :name name})
            (assoc-in [:db :ui :knobs :new-name] ""))))))

;; Slider turns: at most one :knob/set per knob is in flight, so the owner
;; admits them in the order they were made and the last position wins. Turns
;; made while one is in flight collapse into the latest value, sent when the
;; receipt arrives. The draft stays on screen until a snapshot at or past the
;; receipt's revision arrives; afterwards the projection wins, including
;; values set from the REPL or Push.

(defn- send-value [db knob-id value]
  (let [key (events/wire-id knob-id)
        [effect request] (events/command db :knob/set {:knob-id knob-id :value value} [::value-acked knob-id value] nil)]
    {:db (-> db (update-in ui-path dissoc :error) (assoc-in (into ui-path [:in-flight key]) value))
     :fx [[effect (assoc request :on-failure [::value-failed knob-id value])]]}))

(defn- after-value-receipt
  "Send the newest unsent turn of a knob, if any, after a receipt for sent."
  [db knob-id sent]
  (let [key (events/wire-id knob-id)
        db (update-in db (conj ui-path :in-flight) dissoc key)
        draft (get-in db (into ui-path [:drafts key]))]
    (if (and draft (not= sent (:value draft)))
      (send-value db knob-id (:value draft))
      {:db db})))

(defn draft-value
  "The slider position to show for a knob: an unconfirmed local turn, or nil
  once the projection has caught up with it."
  [db knob]
  (let [{:keys [value revision epoch runtime-id]} (get-in db (into ui-path [:drafts (events/wire-id (:id knob))]))
        show (:show db)]
    (when (and (number? value) (= epoch (:epoch show)) (= runtime-id (:runtime-id show))
               (or (nil? revision) (< (:revision show 0) revision)))
      value)))

(rf/reg-event-fx ::set-value
  (fn [{:keys [db]} [_ knob-id value]]
    (when (and (number? value) (<= 0 value 1))
      (let [key (events/wire-id knob-id)
            db (assoc-in db (into ui-path [:drafts key])
                         {:value value :epoch (get-in db [:show :epoch]) :runtime-id (get-in db [:show :runtime-id])})]
        (if (contains? (get-in db (conj ui-path :in-flight)) key)
          {:db db}
          (send-value db knob-id value))))))

(rf/reg-event-fx ::value-acked
  (fn [{:keys [db]} [_ knob-id value ack]]
    (let [key (events/wire-id knob-id)
          db (cond-> db
               (= value (get-in db (into ui-path [:drafts key :value])))
               (assoc-in (into ui-path [:drafts key :revision]) (:revision ack)))]
      (after-value-receipt db knob-id value))))

(rf/reg-event-fx ::value-failed
  (fn [{:keys [db]} [_ knob-id value message]]
    (let [key (events/wire-id knob-id)
          db (cond-> (assoc-in db (conj ui-path :error) (str message))
               (= value (get-in db (into ui-path [:drafts key :value])))
               (update-in (conj ui-path :drafts) dissoc key))
          next-step (after-value-receipt db knob-id value)]
      ;; A newer turn that is sent keeps the refusal visible until its own receipt.
      (update next-step :db assoc-in (conj ui-path :error) (str message)))))

(rf/reg-event-fx ::add-link
  (fn [{:keys [db]} [_ knob]]
    (let [key (events/wire-id (:id knob))
          {:keys [choice-key form variable]} (get-in db (into ui-path [:forms key]))
          choice (if (= "variable" choice-key) "variable"
                     (some #(when (= choice-key (:key %)) %) (destination-choices (:show db))))
          result (link-payload {:choice choice :form form :variable variable :link-id (next-link-id knob)})]
      (if-let [error (:error result)]
        {:db (assoc-in db (conj ui-path :error) error)}
        (-> (send db :knob/link {:knob-id (:id knob) :link (:link result)})
            (update-in [:db :ui :knobs :forms] dissoc key))))))

(rf/reg-event-fx ::edit-link
  (fn [{:keys [db]} [_ knob-id link field value]]
    (send db :knob/link {:knob-id knob-id :link (edited-link link field value)})))

(rf/reg-event-fx ::unlink
  (fn [{:keys [db]} [_ knob-id link-id]] (send db :knob/unlink {:knob-id knob-id :link-id link-id})))

(rf/reg-event-fx ::remove
  (fn [{:keys [db]} [_ knob-id]] (send db :knob/remove {:knob-id knob-id})))

;; Views

(defn- input-value [event] (.. event -target -value))

(defn- status-text [{:keys [status reason message diverged? shared-with]}]
  (str/join " · " (remove nil? [(when-not (= "ok" (events/wire-id status))
                                   (str (events/wire-id status) (when reason (str " (" (events/wire-id reason) ")"))))
                                 (when (and message (not= "ok" (events/wire-id status))) message)
                                 (when diverged? "differs from the knob until it moves")
                                 (when (seq shared-with) (str "shared with " (str/join ", " (map events/wire-id shared-with))))])))

(defn- range-input [knob-id link index]
  (let [value (get-in link [:range index])]
    [:input.knob-range-input
     {:type "text" :inputMode "decimal" :default-value (str value) :key (str value)
      :aria-label (str (:label link) (if (zero? index) " from" " to"))
      :on-blur #(when-let [n (parse-number (input-value %))]
                  (when (not= n value)
                    (rf/dispatch [::edit-link knob-id link :range (assoc (vec (:range link)) index n)])))
      :on-key-down #(when (= "Enter" (.-key %)) (.blur (.-target %)))}]))

(defn- link-row [knob link connected?]
  (let [knob-id (:id knob) id (events/wire-id (:id link))
        numeric? (every? number? (:range link))]
    [:li.knob-link {:data-link-id id :data-status (events/wire-id (:status link))}
     [:div.knob-link-head
      [:span.knob-link-label (:label link)]
      [:output.knob-link-value {:aria-label (str (:label link) " resolved value")}
       (or (get-in link [:resolved :text]) "—")]]
     (when-let [current (get-in link [:current :text])]
       (when (:diverged? link) [:span.small.muted (str "now " current)]))
     (let [status (status-text link)]
       (when (seq status) [:p.knob-link-status.small status]))
     [:div.knob-link-controls
      (if numeric?
        [:<> [range-input knob-id link 0] [:span.small.muted "→"] [range-input knob-id link 1]]
        [:span.small.muted (pr-str (:range link))])
      [:select {:value (events/wire-id (:direction link "up")) :disabled (not connected?)
                :aria-label (str (:label link) " direction")
                :on-change #(rf/dispatch [::edit-link knob-id link :direction (keyword (input-value %))])}
       [:option {:value "up"} "Up"] [:option {:value "down"} "Down"]]
      [:select {:value (events/wire-id (:curve link :linear)) :disabled (not connected?)
                :aria-label (str (:label link) " curve")
                :on-change #(rf/dispatch [::edit-link knob-id link :curve (keyword (input-value %))])}
       (for [[curve title] curves] ^{:key curve} [:option {:value (name curve)} title])]
      [:button.text-button {:type "button" :disabled (not connected?) :aria-label (str "Remove link " (:label link))
                            :on-click #(rf/dispatch [::unlink knob-id (:id link)])} "Remove"]]]))

(defn- add-link-form [db knob connected?]
  (let [key (events/wire-id (:id knob))
        {:keys [choice-key form variable]} (get-in db (into ui-path [:forms key]))
        choices (destination-choices (:show db))
        choice (some #(when (= choice-key (:key %)) %) choices)
        form (or form (when choice (default-form choice)) (default-form {}))
        edit! (fn [field] #(rf/dispatch [::edit [:forms key :form field] (input-value %)]))]
    [:details.knob-add-link
     [:summary "Add destination"]
     [:div.knob-add-grid
      [:label "Destination"
       [:select {:value (or choice-key "") :aria-label "Knob destination"
                 :on-change #(let [k (input-value %)
                                   c (some (fn [c] (when (= k (:key c)) c)) choices)]
                               (rf/dispatch [::edit [:forms key] {:choice-key k :form (default-form (or c {}))}]))}
        [:option {:value ""} "Choose…"]
        (for [{:keys [key label]} choices] ^{:key key} [:option {:value key} label])
        [:option {:value "variable"} "Show variable…"]]]
      (when (= "variable" choice-key)
        [:label "Variable" [:input {:type "text" :aria-label "Variable" :value (or variable "") :placeholder "look/energy"
                                    :on-change #(rf/dispatch [::edit [:forms key :variable] (input-value %)])}]])
      [:label "From" [:input {:type "text" :inputMode "decimal" :aria-label "From" :value (:from form) :on-change (edit! :from)}]]
      [:label "To" [:input {:type "text" :inputMode "decimal" :aria-label "To" :value (:to form) :on-change (edit! :to)}]]
      [:label "Direction" [:select {:aria-label "Direction" :value (:direction form) :on-change (edit! :direction)}
                           [:option {:value "up"} "Up"] [:option {:value "down"} "Down"]]]
      [:label "Curve" [:select {:aria-label "Curve" :value (:curve form) :on-change (edit! :curve)}
                       (for [[curve title] curves] ^{:key curve} [:option {:value (name curve)} title])]]
      [:label "Steps" [:select {:aria-label "Steps" :value (:quantize form) :on-change (edit! :quantize)}
                       (for [[value title] quantize-choices] ^{:key value} [:option {:value value} title])]]
      [:label "Knob start" [:input {:type "text" :inputMode "decimal" :aria-label "Knob start" :value (:input-low form) :on-change (edit! :input-low)}]]
      [:label "Knob end" [:input {:type "text" :inputMode "decimal" :aria-label "Knob end" :value (:input-high form) :on-change (edit! :input-high)}]]]
     [:button.button {:type "button" :disabled (or (not connected?) (str/blank? (str choice-key)))
                      :on-click #(do (when-not (get-in db (into ui-path [:forms key :form]))
                                       (rf/dispatch-sync [::edit [:forms key :form] form]))
                                     (rf/dispatch [::add-link knob]))}
      "Link"]]))

(defn- knob-card [db knob connected?]
  (let [key (events/wire-id (:id knob))
        value (or (draft-value db knob) (:value knob))]
    [:article.knob-card {:data-knob-id key}
     [:div.knob-head
      [:strong (:name knob)]
      [:output.knob-value {:aria-label (str (:name knob) " value")} (.toFixed (js/Number value) 3)]
      [:button.text-button {:type "button" :disabled (not connected?) :aria-label (str "Remove knob " (:name knob))
                            :on-click #(rf/dispatch [::remove (:id knob)])} "Remove"]]
     [:input.knob-slider {:type "range" :min 0 :max 1 :step 0.001 :value value :disabled (not connected?)
                          :aria-label (str (:name knob) " knob")
                          :on-change #(rf/dispatch [::set-value (:id knob) (js/parseFloat (input-value %))])}]
     (if (seq (:links knob))
       [:ul.knob-links (for [link (:links knob)] ^{:key (events/wire-id (:id link))} [link-row knob link connected?])]
       [:p.small.muted "No destinations yet. Add one below."])
     [add-link-form db knob connected?]]))

(def ^:private card-paths
  ;; Destination choices follow the running launches, and a draft is checked
  ;; against the show's identity and revision.
  [[:show] [:ui :knobs :drafts] [:ui :knobs :forms]])

(def ^:private panel-paths
  [[:show :knobs :knobs] [:connection :connected?] [:ui :knobs :error] [:ui :knobs :new-name]])

(defn panel
  "Perform panel listing every knob with its resolved destinations. It and
  each knob card subscribe to their own slice of app-db."
  []
  (let [db @(rf/subscribe [:studio/db-slice panel-paths])
        knobs (get-in db [:show :knobs :knobs])
        connected? (get-in db [:connection :connected?])
        error (get-in db (conj ui-path :error))]
    [:section.knobs-panel.panel {:aria-label "Linked knobs"}
     [:div.panel-top [:strong "Knobs"] [:span.small.muted "One knob, several destinations"]]
     [:div.knobs-body
      (for [knob knobs] ^{:key (events/wire-id (:id knob))} [db-slice/view card-paths knob-card knob connected?])
      [:div.knob-create
       [:input {:type "text" :placeholder "New knob name" :aria-label "New knob name"
                :value (or (get-in db (conj ui-path :new-name)) "")
                :on-change #(rf/dispatch [::edit [:new-name] (input-value %)])
                :on-key-down #(when (= "Enter" (.-key %)) (rf/dispatch [::create]))}]
       [:button.button {:type "button" :disabled (not connected?) :on-click #(rf/dispatch [::create])} "Create knob"]]
      (when error [:p.knob-error {:role "alert"} error])]]))
