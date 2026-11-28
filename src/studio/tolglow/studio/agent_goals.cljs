(ns tolglow.studio.agent-goals
  "Goals panel (#857). Lists the operator's goals for the agent from the
  shared :agent-goals projection, in order, and sends the same
  :agent-goal command as the REPL (`show/submit! runtime :agent-goal {…}`)
  through /api/agent-goal to add, edit, reorder and remove them. Each goal is a
  wide card on the standard slot grid. Palette keyframes and energy curves are
  shown and kept on edit; they are authored from the REPL.
  See docs/repl-studio-parity.md."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]))

(def ^:private ui-path [:ui :agent-goals])

(def scopes [["night" "Night"] ["set" "Set"] ["section" "Section"] ["scene" "Scene"]])

(def ^:private units [["bar" "Bar"] ["beat" "Beat"] ["phrase" "Phrase"]])

(def default-limit 8)

(def default-text-limit 600)

(defn- wire [value] (when (some? value) (events/wire-id value)))

(defn text-length
  "Unicode characters, as the owner counts them."
  [s]
  (count (js/Array.from (str s))))

(defn goals [db] (vec (get-in db [:show :agent-goals :active])))

(defn- limit [db] (get-in db [:show :agent-goals :limit] default-limit))

(defn- text-limit [db] (get-in db [:show :agent-goals :text-limit] default-text-limit))

(defn- number-text [n]
  (if (== n (js/Math.round n)) (str (js/Math.round n)) (str n)))

(defn position-text
  "A zero-based musical position as the scheduler writes it: phrase, bar
  then beat, added together."
  [at]
  (when (map? at)
    (->> [:phrase :bar :beat]
         (keep (fn [unit] (when-let [n (get at unit)] (str (name unit) " " (number-text n)))))
         (str/join " + ")
         not-empty)))

(defn range-text [{:keys [from until]}]
  (let [from (position-text from) until (position-text until)]
    (cond (and from until) (str from " → " until)
          from (str "from " from)
          until (str "until " until))))

(defn scope-title [scope]
  (or (some (fn [[id title]] (when (= id (wire scope)) title)) scopes) (wire scope)))

(defn palette-gradient
  "CSS gradient through each palette keyframe's first colour, in order."
  [palette]
  (let [stops (for [{:keys [colors]} palette :let [[r g b] (first colors)] :when (number? r)]
                (str "rgb(" (js/Math.round (* 255 r)) " " (js/Math.round (* 255 g)) " " (js/Math.round (* 255 b)) ")"))]
    (case (count stops)
      0 nil
      1 (first stops)
      (str "linear-gradient(90deg," (str/join "," stops) ")"))))

(defn- tenths [x] (/ (js/Math.round (* 10 x)) 10))

(defn energy-points
  "SVG polyline points for an energy curve in a 100×24 box, spaced by order."
  [energy]
  (let [n (count energy)]
    (when (pos? n)
      (str/join " " (map-indexed (fn [i {:keys [level]}]
                                   (str (tenths (if (= 1 n) 50 (* 100 (/ i (dec n))))) "," (tenths (- 23 (* 22 (or level 0))))))
                                 energy)))))

;; ---------------------------------------------------------------------------
;; Draft and commands (pure)

(defn- single-unit [at]
  (when (and (map? at) (= 1 (count at)))
    (let [[unit n] (first at)] [(name unit) (number-text n)])))

(defn new-draft [] {:scope "section" :text "" :from-unit "bar" :from "" :until-unit "bar" :until ""})

(defn draft-from-goal
  "Editor fields for a goal. A position that combines units is shown as its
  text and kept unless the operator types a new one."
  [goal]
  (let [[from-unit from] (single-unit (:from goal))
        [until-unit until] (single-unit (:until goal))]
    {:id (wire (:id goal)) :expected-revision (:revision goal)
     :scope (wire (:scope goal)) :text (:text goal "")
     :from-unit (or from-unit "bar") :from (or from (position-text (:from goal)) "")
     :until-unit (or until-unit "bar") :until (or until (position-text (:until goal)) "")}))

(defn- position-field [unit value label]
  (let [value (str/trim (str value))]
    (when-not (str/blank? value)
      (let [n (js/Number value)]
        (if (and (js/Number.isFinite n) (>= n 0))
          {(keyword unit) n}
          (throw (ex-info (str label " is a nonnegative number of " unit "s") {})))))))

(defn draft->command
  "The :agent-goal command a draft saves, or {:error message}. An edit names
  only the fields it changes and the revision it was shown."
  [draft text-limit]
  (try
    (let [text (:text draft "")
          n (text-length text)]
      (cond
        (str/blank? text) {:error "Write what the agent should aim for."}
        (> n text-limit) {:error (str "A goal is at most " text-limit " characters; this one has " n ".")}
        :else
        (let [position (fn [k label] (position-field (get draft (keyword (str (name k) "-unit"))) (get draft k) label))
              fields (cond-> {:scope (:scope draft) :text text}
                       (or (nil? (:id draft)) (:from-touched? draft)) (assoc :from (position :from "From"))
                       (or (nil? (:id draft)) (:until-touched? draft)) (assoc :until (position :until "Until")))]
          (if (:id draft)
            {:op :edit :id (:id draft) :expected-revision (:expected-revision draft) :changes fields}
            {:op :put :goal (into {} (remove (comp nil? val)) fields)}))))
    (catch :default e {:error (ex-message e)})))

(defn goal-actions
  "Move and remove commands for the goal at position r of n."
  [goal n]
  (let [id (wire (:id goal)) r (:order goal)]
    [[:up "Move up" (when (> r 1) {:op :reorder :id id :order (dec r)})]
     [:down "Move down" (when (< r n) {:op :reorder :id id :order (inc r)})]
     [:remove "Remove" {:op :remove :id id :expected-revision (:revision goal)}]]))

(defn request
  "The Studio HTTP effect for a command, authored by this browser session."
  [db command]
  [:studio/http {:path "/api/agent-goal"
                 :body (assoc command :source {:adapter "web" :session-id (:session db)})
                 :on-success [::sent] :on-failure [::failed]}])

;; ---------------------------------------------------------------------------
;; Events

(rf/reg-event-db ::start-add (fn [db _] (assoc-in db ui-path {:draft (new-draft)})))

(rf/reg-event-db ::start-edit (fn [db [_ goal]] (assoc-in db ui-path {:draft (draft-from-goal goal)})))

(rf/reg-event-db ::cancel (fn [db _] (assoc-in db ui-path {})))

(rf/reg-event-db ::field
  (fn [db [_ k value]]
    (update-in db (conj ui-path :draft)
               #(cond-> (assoc % k value)
                  (#{:from :from-unit} k) (assoc :from-touched? true)
                  (#{:until :until-unit} k) (assoc :until-touched? true)))))

(rf/reg-event-fx ::save
  (fn [{:keys [db]} _]
    (let [command (draft->command (get-in db (conj ui-path :draft)) (text-limit db))]
      (if-let [error (:error command)]
        {:db (assoc-in db (conj ui-path :error) error)}
        {:db (update-in db ui-path #(-> % (assoc :pending? true) (dissoc :error)))
         :fx [(request db command)]}))))

(rf/reg-event-fx ::send
  (fn [{:keys [db]} [_ command]]
    {:db (update-in db ui-path #(-> % (assoc :pending? true) (dissoc :error)))
     :fx [(request db command)]}))

(rf/reg-event-db ::sent (fn [db _] (assoc-in db ui-path {})))

(rf/reg-event-db ::failed
  (fn [db [_ message]] (update-in db ui-path #(-> % (assoc :error (str message)) (dissoc :pending?)))))

;; ---------------------------------------------------------------------------
;; View

(defn- goal-card [goal n busy?]
  (let [order (:order goal)
        scope (wire (:scope goal))
        range (range-text goal)
        gradient (palette-gradient (:palette goal))
        points (energy-points (:energy goal))
        label (str "Goal " order ", " (scope-title scope))]
    [:li.agent-goal {:data-slot "wide" :data-scope scope :aria-label label}
     [:div.agent-goal-head
      [:span.agent-goal-order {:aria-hidden true} order]
      [:span.agent-goal-scope (scope-title scope)]
      (when range [:span.agent-goal-range range])
      [:span.agent-goal-by (str "by " (:by goal))]]
     [:p.agent-goal-text {:title (:text goal)} (:text goal)]
     (when (or gradient points)
       [:div.agent-goal-hints
        (when gradient
          [:span.agent-goal-palette {:style {:background gradient} :role "img"
                                     :aria-label (str "Palette, " (count (:palette goal)) " keyframes")}])
        (when points
          [:svg.agent-goal-energy {:view-box "0 0 100 24" :preserve-aspect-ratio "none" :role "img"
                                   :aria-label (str "Energy curve, " (count (:energy goal)) " points")}
           [:polyline {:points points}]])])
     [:div.agent-goal-actions
      [:button.agent-goal-button {:type "button" :disabled busy? :aria-label (str "Edit " label)
                                  :on-click #(rf/dispatch [::start-edit goal])} "Edit"]
      (for [[id title command] (goal-actions goal n)]
        ^{:key id}
        [:button.agent-goal-button {:type "button" :disabled (or busy? (nil? command)) :data-action (name id)
                                    :aria-label (str title " " label)
                                    :on-click #(rf/dispatch [::send command])} title])]]))

(defn- field! [k] #(rf/dispatch-sync [::field k (.. % -target -value)]))

(defn- position-input [draft k label]
  (let [unit-key (keyword (str (name k) "-unit"))]
    [:div.agent-goal-position
     [:span.small label]
     [:input {:type "text" :inputmode "decimal" :value (get draft k "") :placeholder "any"
              :aria-label (str label " position") :on-change (field! k)}]
     [:select {:value (get draft unit-key "bar") :aria-label (str label " unit") :on-change (field! unit-key)}
      (for [[id title] units] ^{:key id} [:option {:value id} title])]]))

(defn- editor [draft limit busy?]
  (let [n (text-length (:text draft))]
    [:form.agent-goal-editor {:aria-label (if (:id draft) "Edit goal" "New goal")
                              :on-submit (fn [e] (.preventDefault e) (rf/dispatch [::save]))}
     [:div.agent-goal-editor-row
      [:select {:value (:scope draft) :aria-label "Scope" :on-change (field! :scope)}
       (for [[id title] scopes] ^{:key id} [:option {:value id} title])]
      [position-input draft :from "From"]
      [position-input draft :until "Until"]]
     [:textarea {:value (:text draft) :rows 3 :aria-label "Goal text" :aria-describedby "agent-goal-count"
                 :placeholder "What should this feel like? The agent reads it as data."
                 :on-change (field! :text)}]
     [:div.agent-goal-editor-row
      [:span#agent-goal-count.small {:data-over (when (> n limit) "true")} (str n " / " limit)]
      [:button.agent-goal-button.primary {:type "submit" :disabled busy?} (if (:id draft) "Save goal" "Add goal")]
      [:button.agent-goal-button {:type "button" :on-click #(rf/dispatch [::cancel])} "Cancel"]]]))

(defn panel
  "The operator's goals for the agent, in order."
  [db]
  (let [items (goals db)
        n (count items)
        {:keys [draft error pending?]} (get-in db ui-path)
        busy? (or pending? (not (get-in db [:connection :connected?])))
        full? (>= n (limit db))]
    [:section.agent-goals-panel.panel {:aria-label "Agent goals"}
     [:div.panel-top [:strong "Goals"]
      [:span.small.muted (str n " of " (limit db) " · read by the agent as data")]
      (when-not draft
        [:button.agent-goal-button {:type "button" :disabled (or busy? full?)
                                    :title (when full? "Remove a goal before adding another")
                                    :on-click #(rf/dispatch [::start-add])} "Add goal"])]
     (when draft [editor draft (text-limit db) busy?])
     (if (seq items)
       [:ol.agent-goals {:data-slot-grid "goals"}
        (for [goal items] ^{:key (wire (:id goal))} [goal-card goal n busy?])]
       (when-not draft
         [:p.small.muted.agent-goals-empty "No goals yet. Tell the agent what the night, a set, a section or a scene should feel like."]))
     (when error [:p.agent-goal-error {:role "alert"} error])]))
