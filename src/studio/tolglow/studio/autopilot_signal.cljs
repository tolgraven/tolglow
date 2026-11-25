(ns tolglow.studio.autopilot-signal
  "Edit portable target and HDR bindings against the show’s waveform registry."
  (:require [cljs.reader :as reader]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.lighting-response :as response]
            [tolglow.studio.modulation :as modulation]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.waveform :as waveform]))

(def units [[:beats "Beats"] [:bars "Bars"] [:phrases "Phrases"] [:ms "Milliseconds"]])

(defn percent [n] (if (number? n) (str (js/Math.round (* n 100)) "%") "—"))

(defn choice [label value choices change! & [opts]]
  (let [choices (vec choices)]
    [:label label
     [:select (merge {:aria-label label :value (pr-str value)
                      :on-change #(let [raw (.. % -target -value)]
                                    (when-let [entry (some (fn [[v :as entry]] (when (= raw (pr-str v)) entry)) choices)]
                                      (change! (first entry))))} opts)
      (for [[v title] choices] ^{:key (pr-str v)} [:option {:value (pr-str v)} title])]]))

(defn duration-field [title duration change!]
  (let [[unit raw] (first duration)]
    [:div.conduct-duration
     [:label title [:input {:aria-label title :type "text" :input-mode "text" :value (str raw)
                            :on-change #(change! {unit (.. % -target -value)})}]]
     [choice (str title " unit") unit units #(change! {% raw})]]))

(defn parse-duration [duration label]
  (let [[u value] (first duration)
        value (modulation/number-value value label)]
    (when-not (and (= 1 (count duration)) (contains? (set (map first units)) u) (pos? value))
      (throw (js/Error. (str label " needs a positive musical duration or milliseconds."))))
    {u value}))

(defn- registry-token [db]
  (let [v (get-in db [:server-projection :view])]
    [(projection/identity-of v) (:waveform-revision v)]))

(rf/reg-event-fx ::load-registry
  (fn [{:keys [db]} _]
    (let [token (registry-token db)]
      (if (and (= token (get-in db [:autopilot-waves :token])) (not (get-in db [:autopilot-waves :error]))) {}
        {:db (assoc db :autopilot-waves {:token token})
         :studio/http {:path "/api/waveforms" :typed? true
                       :on-success [::registry token] :on-failure [::registry-error token]}}))))
(rf/reg-event-db ::registry
  (fn [db [_ token value]]
    (if (= token (registry-token db))
      (assoc db :autopilot-waves {:token token :entries (sort-by second (for [[id row] (:waveforms value)]
                                                                         [id (or (:name row) (events/wire-id id))]))})
      db)))
(rf/reg-event-db ::registry-error
  (fn [db [_ token message]]
    (if (= token (registry-token db)) (assoc-in db [:autopilot-waves :error] (str message)) db)))
(rf/reg-sub ::registry (fn [db _] (when (= (registry-token db) (get-in db [:autopilot-waves :token])) (:autopilot-waves db))))

(defn signal-kind [v]
  (cond (number? v) :fixed
        (:waveform v) :waveform
        (:random v) :random
        (:cue-id v) :cue
        (:signal v) :shared
        (= [:audio/energy] (:variable v)) :audio
        :else :data))

(defn starting-spec [mode {:keys [min max]} current signals]
  (case mode
    :fixed (if (number? current) current min)
    :audio {:variable [:audio/energy] :default (if (number? current) current min)}
    :random {:random {:seed 1 :distribution :uniform} :period {:beats 1} :phase :global :range [min max]}
    :waveform {:waveform :sine :period {:beats 4} :phase-reference :transport :range [min max]}
    :shared (when-let [id (first (sort-by events/wire-id (keys signals)))] {:signal id})
    :data current))

(defn read-form [source]
  (let [forms (reader/read-string (str "[" source "\n]"))]
    (when-not (= 1 (count forms)) (throw (js/Error. "Enter one complete EDN value.")))
    (first forms)))

(defn parse-spec [spec]
  (cond
    (and (map? spec) (contains? spec ::edn)) (read-form (::edn spec))
    (or (:waveform spec) (:random spec))
    (cond-> spec
      (:period spec) (update :period parse-duration "Signal period")
      (:range spec) (update :range #(mapv parse-spec %))
      (:params spec) (update :params update-vals parse-spec)
      (:random spec) (update-in [:random :seed] #(let [n (modulation/number-value % "Random seed")]
                                                  (when-not (js/Number.isSafeInteger n) (throw (js/Error. "Random seed must be a whole number.")))
                                                  n)))
    :else spec))

(defn- numeric-slider [label v lo hi step pct? change!]
  [:label.scene-target [:span label [:output (if pct? (percent v) (str v))]]
   [:input {:type "range" :min lo :max hi :step step :value v :aria-label label
            :aria-valuetext (if pct? (percent v) (str v))
            :on-change #(change! (js/Number (.. % -target -value)))}]])

(defn- preview-toggle [key label id params]
  (r/with-let [expanded? (r/atom false)]
    [:div.conduct-preview
     [:button.text-button {:type "button" :aria-expanded @expanded? :on-click #(swap! expanded? not)}
      (str (if @expanded? "Hide " "Preview ") label " waveform")]
     (when @expanded? [waveform/preview [::preview key] id (if (every? number? (vals params)) {:params (or params {})} {})])]))

(declare field)

(defn waveform-reference [spec]
  (:phase-reference spec (if (= :local (:phase spec)) :launch :transport)))

(defn set-waveform-reference [spec reference]
  (if (contains? (:period spec) :ms)
    (-> spec (dissoc :phase-reference) (assoc :phase (if (= :launch reference) :local :global)))
    (-> spec (dissoc :phase) (assoc :phase-reference reference))))

(defn- wave-fields [key label spec options signals change!]
  (let [{:keys [entries error]} @(rf/subscribe [::registry])
        {:keys [min max step percent?]} options
        id (:waveform spec) [lo hi] (:range spec [min max])
        entries (cond-> (vec entries) (not (some #(= id (first %)) entries)) (conj [id (events/wire-id id)]))]
    [:div.conduct-wave.policy-grid
     (if (:waveform spec)
       [choice (str label " waveform") id entries (fn [v] (change! #(assoc % :waveform v :params {})))
      {:on-focus #(rf/dispatch [::load-registry])}]
       [:<>
        [choice (str label " distribution") (get-in spec [:random :distribution] :uniform)
         [[:uniform "Uniform"] [:triangular "Triangular"]]
         (fn [v] (change! #(assoc-in % [:random :distribution] v)))]
        [:label "Random seed" [:input {:aria-label (str label " random seed") :type "text" :input-mode "numeric"
                                        :value (get-in spec [:random :seed])
                                        :on-change #(let [v (.. % -target -value)] (change! (fn [s] (assoc-in s [:random :seed] v))))}]]])
     (if id
       [choice (str label " phase reference") (waveform-reference spec)
        [[:transport "Show transport"] [:launch "Since activation"]]
        (fn [reference] (change! #(set-waveform-reference % reference)))]
       [choice (str label " phase") (:phase spec :global) [[:global "Show clock"] [:local "Since activation"]]
        (fn [v] (change! #(assoc % :phase v)))])
     [duration-field (str label " period") (or (:period spec) {:beats 4})
      (fn [v] (change! #(cond-> (assoc % :period v)
                        (:phase-reference %) (set-waveform-reference (waveform-reference %)))))]
     (for [[i bound v] [[0 "minimum" lo] [1 "maximum" hi]] :when (number? v)]
       ^{:key bound} [numeric-slider (str label " " bound) v min max step percent?
                     (fn [v] (change! #(assoc-in % [:range i] v)))])
     (when error [:p.performance-error {:role "alert"} error])
     [:details.source-readings [:summary "Modulate range"]
      (for [[i title v] [[0 "lower bound" lo] [1 "upper bound" hi]]]
        ^{:key i} [field [key i] (str label " " title) v options signals
                   (fn [next-val] (change! #(update-in % [:range i] (if (fn? next-val) next-val (constantly next-val)))))]) ]
     (when id [preview-toggle key label id (:params spec)])]))

(defn field [key label spec {:keys [min max step percent?] :as options} signals change!]
  (let [cues @(rf/subscribe [::response/choices])
        mode (if (contains? (if (map? spec) spec {}) ::edn) :data (signal-kind spec))
        modes (cond-> [[:fixed "Fixed"] [:audio "Audio energy"] [:waveform "Waveform"] [:random "Rhythmic random"]]
                (or (= mode :cue) (seq cues)) (conj [:cue "Cue parameter"])
                (or (= mode :shared) (seq signals)) (conj [:shared "Shared signal"])
                true (conj [:data "Binding (EDN)"]))
        select-mode! (fn [next]
                       (when (= next :waveform) (rf/dispatch [::load-registry]))
                       (case next
                         :data (change! {::edn (pr-str spec)})
                         :cue (let [{:keys [id params]} (first cues)]
                                (change! {:cue-id id :param (first params) :default (if (number? spec) spec min)}))
                         (change! (starting-spec next options spec signals))))]
    [:div.conduct-signal
     (if (= mode :fixed)
       [numeric-slider label spec min max step percent? change!]
       [:span.conduct-name label])
     [choice (str label " source") mode modes select-mode!]
     (case mode
       (:waveform :random) [wave-fields key label spec options signals change!]
       :cue (let [row (some #(when (= (:id %) (:cue-id spec)) %) cues)
                  choices (cond-> (mapv (juxt :id :name) cues)
                            (nil? row) (conj [(:cue-id spec) (events/wire-id (:cue-id spec))]))
                  params (distinct (cons (:param spec) (:params row)))]
              [:div.policy-grid
               [choice (str label " cue") (:cue-id spec) choices
                (fn [id] (when-let [r (some #(when (= (:id %) id) %) cues)]
                           (change! #(assoc % :cue-id id :param (first (:params r))))))]
               [choice (str label " parameter") (:param spec) (map (juxt identity events/wire-id) params)
                (fn [v] (change! #(assoc % :param v)))]])
       :shared [choice (str label " shared signal") (:signal spec)
                (for [id (distinct (cons (:signal spec) (sort-by events/wire-id (keys signals))))] [id (events/wire-id id)])
                #(change! {:signal %})]
       :data [:label (str label " binding")
              [:textarea {:aria-label (str label " binding") :spell-check false :value (get spec ::edn (pr-str spec))
                          :on-change #(change! {::edn (.. % -target -value)})}]]
       :audio [:small "Follows the captured audio energy."]
       nil)]))
