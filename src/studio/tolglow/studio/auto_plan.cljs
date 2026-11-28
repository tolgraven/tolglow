(ns tolglow.studio.auto-plan
  "Author the auto plan (#217): section plans, the colour palette, category
  filters and the lit limit. The editor drafts from the show's saved auto
  config and Save sends one :auto-plan edit to /api/auto/plan. The REPL sends
  the same edit with show/submit! runtime :auto-plan. The show validates the
  whole config, so a refusal arrives with the show's own message and the
  saved plan is unchanged. While auto runs the plan installs now or on the
  chosen line; while it is off it is stored for Start auto. See
  docs/auto-show.md."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]))

(def ^:private ui-path [:ui :auto-plan])

(def units ["beats" "bars" "phrases"])

(def transitions [["overlap" "Overlap"] ["release" "Release"] ["cut" "Cut"]])

(defn- wire [x] (events/wire-id x))

(defn words
  "Comma- or space-separated text as keywords, in order, without repeats."
  [text]
  (->> (str/split (str text) #"[,\s]+") (remove str/blank?) distinct (mapv keyword)))

(defn- word-text [xs] (str/join ", " (map wire xs)))

(defn color->hex
  "A palette colour [r g b] in 0–1 as #rrggbb, as Studio's colour inputs show it."
  [components]
  (str "#" (apply str (map (fn [n] (.padStart (.toString (js/Math.round (* 255 (max 0 (min 1 (or n 0))))) 16) 2 "0"))
                           (take 3 components)))))

(defn hex->color [hex]
  (mapv #(/ (js/Math.round (* 1000 (/ (js/parseInt (subs hex % (+ % 2)) 16) 255))) 1000) [1 3 5]))

(defn- palette-rows
  "Editable colours. The saved colour is kept beside its hex, so a colour
  nobody touched is sent back exactly rather than rounded through #rrggbb."
  [palette]
  (mapv (fn [{:keys [color weight]}] {:hex (color->hex color) :color color :weight (or weight 1)}) palette))

(defn- duration-row [length]
  (let [[unit value] (first length)]
    {:length-value value :length-unit (wire unit)}))

(defn- section-row
  "An editable section. Settings the editor does not show (interval, launch,
  count, other randomize settings) ride along unchanged as :extra."
  [{:keys [id weight length transition max-run config]}]
  (let [{:keys [scope categories max-lit intent randomize]} config]
    (merge {:id (wire id) :weight (or weight 1) :transition (wire (or transition :overlap))
            :max-run (if max-run (str max-run) "")
            :cues (word-text (:cue-ids scope)) :include (word-text (:include categories))
            :exclude (word-text (:exclude categories)) :max-lit (if (some? max-lit) (str max-lit) "")
            :darkness? (= "darkness" (some-> intent wire))
            :own-palette? (some? (:palette randomize))
            :palette (palette-rows (:palette randomize))
            :extra (cond-> (dissoc config :scope :categories :max-lit :intent)
                     (map? randomize) (update :randomize dissoc :palette)
                     (= {} (dissoc randomize :palette)) (dissoc :randomize)
                     (and (map? scope) (not (contains? scope :cue-ids))) (assoc :scope scope))}
           (duration-row length))))

(defn plan->draft
  "Editor rows from a saved auto config's plan."
  [config]
  (let [{:keys [sections categories max-lit randomize]} config]
    {:include (word-text (:include categories)) :exclude (word-text (:exclude categories))
     :max-lit (if (some? max-lit) (str max-lit) "")
     :palette (palette-rows (:palette randomize))
     :sections (mapv section-row sections)}))

(defn- whole [text label]
  (when-not (str/blank? (str text))
    (let [n (js/Number text)]
      (when-not (and (js/Number.isInteger n) (<= 0 n 512))
        (throw (js/Error. (str label " is a whole number of lit heads, or blank for no limit."))))
      n)))

(defn- palette [rows label]
  (when (seq rows)
    (mapv (fn [{:keys [hex color weight]}]
            (let [w (js/Number weight)]
              (when-not (and (js/Number.isFinite w) (<= 0 w))
                (throw (js/Error. (str label " weights are nonnegative numbers."))))
              {:color (if (and color (= hex (color->hex color))) (vec color) (hex->color hex)) :weight w}))
          rows)))

(defn- filters [include exclude]
  (let [include (words include) exclude (words exclude)]
    (when (or (seq include) (seq exclude))
      (cond-> {} (seq include) (assoc :include include) (seq exclude) (assoc :exclude exclude)))))

(defn draft->plan
  "The :auto-plan edit for a draft. Every part is named, so a part left empty
  clears it. Throws a readable error for what the editor can check; the show
  checks the rest."
  [{:keys [include exclude max-lit sections] :as draft}]
  (let [ids (map #(str/trim (str (:id %))) sections)]
    (when (some str/blank? ids) (throw (js/Error. "Name every section.")))
    (when-not (= (count ids) (count (distinct ids))) (throw (js/Error. "Each section needs its own name.")))
    {:categories (filters include exclude)
     :max-lit (whole max-lit "The lit limit")
     :palette (palette (:palette draft) "Palette")
     :sections (mapv (fn [{:keys [id weight length-value length-unit transition max-run cues max-lit darkness? own-palette? extra]
                           :as row}]
                       (let [label (str "Section " id)
                             length (js/Number length-value)
                             weight (js/Number weight)
                             cue-ids (words cues)
                             section-filters (filters (:include row) (:exclude row))
                             lit (whole max-lit (str label "'s lit limit"))
                             own (when own-palette? (palette (:palette row) (str label "'s palette")))]
                         (when-not (and (js/Number.isFinite length) (pos? length) (some #{length-unit} units))
                           (throw (js/Error. (str label " needs a positive length in beats, bars or phrases."))))
                         (when-not (and (js/Number.isFinite weight) (pos? weight))
                           (throw (js/Error. (str label " needs a positive weight."))))
                         (when (and own-palette? (empty? own)) (throw (js/Error. (str label " has its own palette but no colours."))))
                         (cond-> {:id (keyword (str/trim id)) :weight weight :length {(keyword length-unit) length}
                                  :transition (keyword transition)
                                  :config (cond-> (or extra {})
                                            (seq cue-ids) (assoc :scope {:cue-ids cue-ids})
                                            section-filters (assoc :categories section-filters)
                                            (some? lit) (assoc :max-lit lit)
                                            darkness? (assoc :intent :darkness)
                                            own (assoc-in [:randomize :palette] own))}
                           (not (str/blank? (str max-run))) (assoc :max-run (js/Number max-run)))))
                     sections)}))

(defn saved-config [db] (get-in db [:show :auto :config]))

(defn draft
  "The draft being edited, or one made from the saved plan."
  [db]
  (or (get-in db (conj ui-path :draft)) (plan->draft (saved-config db))))

(defn- edit [db f] (assoc-in db (conj ui-path :draft) (f (draft db))))

(def blank-section
  {:id "" :weight 1 :length-value 2 :length-unit "bars" :transition "release" :max-run "" :cues ""
   :include "" :exclude "" :max-lit "" :darkness? false :own-palette? false :palette [] :extra {}})

(rf/reg-event-db ::edit
  (fn [db [_ path value]] (-> db (edit #(assoc-in % path value)) (update-in ui-path dissoc :error :saved))))

(rf/reg-event-db ::add-section
  (fn [db _] (edit db #(update % :sections (fnil conj []) (assoc blank-section :id (str "section-" (inc (count (:sections %)))))))))

(rf/reg-event-db ::remove-section
  (fn [db [_ i]] (edit db #(update % :sections (fn [rows] (into (subvec rows 0 i) (subvec rows (inc i))))))))

(rf/reg-event-db ::add-colour
  (fn [db [_ path]] (edit db #(update-in % path (fnil conj []) {:hex "#ff0000" :weight 1}))))

(rf/reg-event-db ::remove-colour
  (fn [db [_ path i]] (edit db #(update-in % path (fn [rows] (into (subvec rows 0 i) (subvec rows (inc i))))))))

(rf/reg-event-db ::reload
  (fn [db _] (update-in db ui-path dissoc :draft :error :saved)))

(rf/reg-event-db ::set-applies
  (fn [db [_ value]] (assoc-in db (conj ui-path :applies) value)))

(defn save-request
  "The /api/auto/plan request for db, with the page's show identity."
  [db]
  (let [applies (get-in db (conj ui-path :applies) "now")
        running? (get-in db [:show :auto :enabled?])]
    (cond-> {:plan (draft->plan (draft db))
             :runtime-id (get-in db [:show :runtime-id]) :expected-epoch (get-in db [:show :epoch])
             :source {:adapter "web" :session-id (:session db)}}
      (and running? (not= "now" applies)) (assoc :at applies))))

(rf/reg-event-fx ::save
  (fn [{:keys [db]} _]
    (try
      {:db (update-in db ui-path dissoc :error :saved)
       :studio/http {:path "/api/auto/plan" :body (save-request db) :on-success [::saved] :on-failure [::failed]}}
      (catch :default error
        {:db (assoc-in db (conj ui-path :error) (.-message error))}))))

(rf/reg-event-db ::saved
  (fn [db [_ ack]]
    (-> db (update-in ui-path dissoc :draft :error)
        (assoc-in (conj ui-path :saved)
                  (cond (:scheduled? ack) "Plan saved; it starts on the line."
                        (false? (:enabled? ack)) "Plan saved. Start auto plays it."
                        :else "Plan saved and playing.")))))

(rf/reg-event-db ::failed
  (fn [db [_ message]] (assoc-in db (conj ui-path :error) (events/error-message message))))

(defn- text-input [id label value path & [placeholder]]
  [:label label [:input {:id id :value (str value) :placeholder placeholder
                         :on-change #(rf/dispatch [::edit path (.. % -target -value)])}]])

(defn- palette-editor [prefix path rows]
  [:div.auto-plan-palette {:role "group" :aria-label (str prefix " colours")}
   (for [[i {:keys [hex weight]}] (map-indexed vector rows)]
     ^{:key i}
     [:div.auto-plan-colour
      [:input {:type "color" :value hex :aria-label (str prefix " colour " (inc i))
               :on-change #(rf/dispatch [::edit (conj path i :hex) (.. % -target -value)])}]
      [:input {:type "number" :min 0 :step "any" :value weight :aria-label (str prefix " colour " (inc i) " weight")
               :on-change #(rf/dispatch [::edit (conj path i :weight) (.. % -target -value)])}]
      [:button.text-button {:type "button" :aria-label (str "Remove " prefix " colour " (inc i))
                            :on-click #(rf/dispatch [::remove-colour path i])} "Remove"]])
   [:button.text-button {:type "button" :on-click #(rf/dispatch [::add-colour path])} "+ Colour"]])

(defn- section-card [i row cue-options]
  (let [path [:sections i]
        label (str "Section " (inc i))]
    [:li.auto-plan-section {:data-section-id (:id row)}
     [:div.form-row
      (text-input (str "auto-plan-section-" i "-id") "Name" (:id row) (conj path :id))
      [:label "Weight" [:input {:type "number" :min 0 :step "any" :value (:weight row) :aria-label (str label " weight")
                                :on-change #(rf/dispatch [::edit (conj path :weight) (.. % -target -value)])}]]
      [:label "Length" [:span.auto-plan-duration
                        [:input {:type "number" :min 0 :step "any" :value (:length-value row) :aria-label (str label " length")
                                 :on-change #(rf/dispatch [::edit (conj path :length-value) (.. % -target -value)])}]
                        [:select {:value (:length-unit row) :aria-label (str label " length unit")
                                  :on-change #(rf/dispatch [::edit (conj path :length-unit) (.. % -target -value)])}
                         (for [u units] ^{:key u} [:option {:value u} u])]]]]
     [:div.form-row
      [:label "Transition" [:select {:value (:transition row) :aria-label (str label " transition")
                                     :on-change #(rf/dispatch [::edit (conj path :transition) (.. % -target -value)])}
                            (for [[v t] transitions] ^{:key v} [:option {:value v} t])]]
      [:label "Repeat at most" [:input {:type "number" :min 1 :value (:max-run row) :placeholder "any" :aria-label (str label " maximum run")
                                        :on-change #(rf/dispatch [::edit (conj path :max-run) (.. % -target -value)])}]]
      [:label "Lit limit" [:input {:type "number" :min 0 :value (:max-lit row) :placeholder "plan's" :aria-label (str label " lit limit")
                                   :on-change #(rf/dispatch [::edit (conj path :max-lit) (.. % -target -value)])}]]]
     [:label "Cues" [:input {:value (:cues row) :placeholder "every cue in the plan's scope" :list "auto-plan-cues"
                             :aria-label (str label " cues")
                             :on-change #(rf/dispatch [::edit (conj path :cues) (.. % -target -value)])}]]
     [:div.form-row
      (text-input (str "auto-plan-section-" i "-include") "Only categories" (:include row) (conj path :include) "strobe")
      (text-input (str "auto-plan-section-" i "-exclude") "Leave out" (:exclude row) (conj path :exclude) "strobe")]
     [:div.auto-plan-toggles
      [:label.policy-toggle [:input {:type "checkbox" :checked (boolean (:darkness? row)) :aria-label (str label " moves in darkness")
                                     :on-change #(rf/dispatch [::edit (conj path :darkness?) (.. % -target -checked)])}]
       "Move in darkness"]
      [:label.policy-toggle [:input {:type "checkbox" :checked (boolean (:own-palette? row)) :aria-label (str label " has its own palette")
                                     :on-change #(rf/dispatch [::edit (conj path :own-palette?) (.. % -target -checked)])}]
       "Own palette"]
      [:button.text-button {:type "button" :aria-label (str "Remove " label) :on-click #(rf/dispatch [::remove-section i])} "Remove section"]]
     (when (:own-palette? row) [palette-editor label (conj path :palette) (:palette row)])
     (when (seq (:extra row))
       [:p.small.muted (str "Kept as saved: " (str/join ", " (map wire (keys (:extra row)))))])]))

(defn panel
  "Perform → Tools: section plan, palette, filters and lit limit for auto mode."
  [db]
  (let [{:keys [include exclude max-lit sections] :as d} (draft db)
        {:keys [error saved applies]} (get-in db ui-path)
        running? (get-in db [:show :auto :enabled?])
        dirty? (some? (get-in db (conj ui-path :draft)))
        cue-options (sort (map (comp wire :id) (get-in db [:show :cues])))]
    [:details.panel.auto-panel.auto-plan-panel
     [:summary [:span.section-label "PLAN THE WANDERING"]
      [:span#auto-plan-status.muted.small
       (str (count (:sections (saved-config db))) " sections · " (count (get-in (saved-config db) [:randomize :palette])) " colours"
            (when dirty? " · edited"))]]
     [:div.form-body
      [:p.small.muted "Sections take turns by weight on the bar or phrase grid. Their settings override the plan's while they play. Colours are chosen whole from the palette."]
      [:datalist#auto-plan-cues (for [c cue-options] ^{:key c} [:option {:value c}])]
      [:div.form-row
       (text-input "auto-plan-include" "Only categories" include [:include] "every category")
       (text-input "auto-plan-exclude" "Leave out" exclude [:exclude] "strobe")
       [:label "Lit limit" [:input#auto-plan-max-lit {:type "number" :min 0 :value max-lit :placeholder "none"
                                                      :on-change #(rf/dispatch [::edit [:max-lit] (.. % -target -value)])}]]]
      [:div [:span.small.muted "Palette"] [palette-editor "Palette" [:palette] (:palette d)]]
      [:ol.auto-plan-sections {:aria-label "Sections"}
       (for [[i row] (map-indexed vector sections)] ^{:key i} [section-card i row cue-options])]
      [:button#auto-plan-add-section.text-button {:type "button" :on-click #(rf/dispatch [::add-section])} "+ Section"]
      (when running?
        [:label "Apply" [:select#auto-plan-applies {:value (or applies "now") :on-change #(rf/dispatch [::set-applies (.. % -target -value)])}
                         (for [[value label] events/auto-lines]
                           ^{:key value} [:option {:value value :disabled (and (= "next-section" value)
                                                                               (nil? (get-in db [:show :auto :section-end-beat])))} label])]])
      [:div.button-row
       [:button#auto-plan-save.button.primary {:type "button" :disabled (not (get-in db [:connection :connected?]))
                                               :on-click #(rf/dispatch [::save])} "Save plan"]
       [:button#auto-plan-reload.button {:type "button" :disabled (not dirty?) :on-click #(rf/dispatch [::reload])} "Reload from show"]]
      (cond error [:p#auto-plan-error.small {:role "alert" :style {:color "var(--danger)"}} error]
            saved [:p#auto-plan-saved.small {:role "status"} saved])
      [:p.small.muted (if running?
                        "Auto is running: saving restarts it with this plan and the same seed, now or on the chosen line."
                        "Auto is off: saving stores the plan, and Start auto plays it.")]]]))
