(ns tolglow.studio.rehearsal
  "Rehearse with the agent (#670): arm, start and stop a Room rehearsal and
  read its review: phrasing, repetition and what the agent changed. The panel
  calls /api/rehearsal, which runs the same tolglow.rehearsal.session
  functions as the REPL (session/arm!, start!, stop!, review!, status).
  Arming loads the rehearsal show in place of the open one, so it asks for
  confirmation unless this session loaded it. The review is the headless
  seeded replay of the same passage, seed and agent options. The panel polls
  the status once a second while it is open. See docs/rehearsal.md."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]))

(def ^:private ui-path [:ui :rehearsal])

(def poll-ms 1000)

(def passages [["club" "Club: groove, breakdown, return, quiet"] ["concert-a" "Concert, take A"] ["concert-b" "Concert, take B"]])

(def outages [["none" "Connected"] ["refused" "Disconnected: refuses"] ["silent" "Disconnected: silent"]])

(defn- wire [x] (when (some? x) (events/wire-id x)))

(def default-form {:passage "club" :seed 670 :latency-beats 0 :outage "none" :outage-from 1 :replace? false})

(defn form [db] (merge default-form (get-in db (conj ui-path :form))))

(defn choice
  "The session choice a form makes, as tolglow.rehearsal.session takes it."
  [{:keys [passage seed latency-beats outage outage-from]}]
  (let [seed (js/Number seed) latency (js/Number latency-beats) from (js/Number outage-from)]
    (when-not (and (js/Number.isInteger seed) (<= 0 seed)) (throw (js/Error. "The seed is a whole number from 0.")))
    (when-not (and (js/Number.isFinite latency) (<= 0 latency 64))
      (throw (js/Error. "The agent's reply time is 0 to 64 beats.")))
    (when (and (not= "none" outage) (not (and (js/Number.isInteger from) (<= 0 from 31))))
      (throw (js/Error. "Choose the section the agent drops out from, counting from 0.")))
    (cond-> {:mode (if (= "club" passage) :club :concert) :seed seed
             :agent (cond-> {:latency-beats latency}
                      (not= "none" outage) (assoc :outage {:from-section from :mode (keyword outage)}))}
      (not= "club" passage) (assoc :take (if (= "concert-b" passage) :b :a)))))

(defn- identity-of [db] {:runtime-id (get-in db [:show :runtime-id]) :expected-epoch (get-in db [:show :epoch])})

(defn request
  "The POST body for op from db. Arm carries the choice and the replace
  confirmation; every request carries the page's show identity."
  [db op]
  (let [f (form db)]
    (merge (identity-of db)
           (case op
             :arm (assoc (choice f) :replace-show? (boolean (:replace? f)))
             :review (choice f)
             {}))))

(rf/reg-event-db ::edit
  (fn [db [_ k value]] (-> db (assoc-in (conj ui-path :form k) value) (update-in ui-path dissoc :error))))

(rf/reg-event-db ::toggle
  (fn [db [_ open?]] (assoc-in db (conj ui-path :open?) open?)))

(rf/reg-event-fx ::poll
  (fn [{:keys [db]} _]
    (when (get-in db (conj ui-path :open?))
      {:studio/http {:path "/api/rehearsal" :on-success [::status] :on-failure [::poll-failed]}})))

(rf/reg-event-db ::status
  (fn [db [_ status]] (-> db (assoc-in [:rehearsal :status] status) (update-in ui-path dissoc :poll-error))))

(rf/reg-event-db ::poll-failed
  (fn [db [_ message]] (assoc-in db (conj ui-path :poll-error) (events/error-message message))))

(rf/reg-event-fx ::act
  (fn [{:keys [db]} [_ op]]
    (try
      {:db (-> db (assoc-in (conj ui-path :pending) op) (update-in ui-path dissoc :error))
       :studio/http {:path (str "/api/rehearsal/" (name op)) :body (request db op) :timeout-ms 20000
                     :on-success [::acted op] :on-failure [::failed op]}}
      (catch :default error {:db (assoc-in db (conj ui-path :error) (.-message error))}))))

(rf/reg-event-db ::acted
  (fn [db [_ _ status]]
    (-> db (assoc-in [:rehearsal :status] status) (update-in ui-path dissoc :pending :error)
        ;; Re-arming this session's own show needs no new confirmation.
        (assoc-in (conj ui-path :form :replace?) false))))

(rf/reg-event-db ::failed
  (fn [db [_ _ message]] (-> db (update-in ui-path dissoc :pending) (assoc-in (conj ui-path :error) (events/error-message message)))))

(defn- bar-text [beat]
  (when (number? beat)
    (let [bar (js/Math.floor (/ (+ beat 1e-6) 4)) within (- beat (* bar 4))]
      (str "bar " bar (when (> (js/Math.abs within) 0.01) (str " beat " (.toFixed within 2)))))))

(defn- number-text [x] (cond (nil? x) "–" (number? x) (str (/ (js/Math.round (* 100 x)) 100)) :else (str (wire x))))

(defn status-line
  "One line for the session: its state, passage, seed and where the transport is."
  [{:keys [status choice beat bars playing? loaded?]}]
  (let [state (keyword (wire (or status :idle)))]
    (str/join " · " (cond-> [(case state :idle "Nothing armed" :armed "Armed" :playing "Playing" :stopping "Stopping"
                                   :finished "Finished" :stopped "Stopped" :failed "Failed" (str/capitalize (name state)))]
                      choice (conj (str (wire (:mode choice)) (when (:take choice) (str " take " (wire (:take choice))))
                                        " · seed " (:seed choice)))
                      (and loaded? (number? beat)) (conj (str (bar-text beat) (when bars (str " of " bars))
                                                              (when-not playing? " (transport stopped)")))))))

(def outcome-text
  {:applied "applied" :refused-stale "refused as stale" :agent-failed "agent did not answer"
   :timed-out "agent timed out" :never-applied "never applied"})

(defn- outcome-label [o] (get outcome-text (keyword (wire o)) (wire o)))

(defn- table [label headers rows]
  [:div.rehearsal-table-wrap
   [:table.rehearsal-table {:aria-label label}
    [:thead [:tr (for [h headers] ^{:key h} [:th {:scope "col"} h])]]
    [:tbody (for [[i row] (map-indexed vector rows)] ^{:key i} [:tr (for [[j cell] (map-indexed vector row)] ^{:key j} [:td cell])])]]])

(defn- responses-table [label responses]
  [table label ["Section" "Line" "Outcome" "Applied" "Late" "Carry-over" "Early"]
   (for [r responses]
     [(wire (:section r)) (or (:position r) (bar-text (:section-beat r))) (outcome-label (:outcome r))
      (number-text (:applied-beat r)) (number-text (:late-beats r)) (number-text (:carry-over-launches r)) (number-text (:early-launches r))])])

(defn- moods-list [moods]
  [:ul.rehearsal-moods {:aria-label "What the agent sets at each section line"}
   (for [{:keys [section kind bar scope interval launch max-lit intent plan anchor]} moods]
     ^{:key (wire section)}
     [:li [:strong (wire section)] " " [:span.muted (str (wire kind) " from bar " bar)] " — "
      (str/join ", " (map wire scope))
      (str " · every " (str/join " " (map (fn [[u v]] (str (number-text v) " " (wire u))) interval))
           " · " (wire launch)
           (when max-lit (str " · at most " max-lit " lit"))
           (when intent (str " · " (wire intent)))
           (when (seq plan) (str " · plan " (str/join "/" (map wire plan))))
           (when anchor (str " · anchor " (wire anchor))))])])

(defn- continuity-line [{:keys [decisions longest-gap stalls pending-auto-changes agent-runs-open-at-end]}]
  (when decisions
    (str decisions " auto decisions; longest gap " (number-text (:beats longest-gap)) " beats; "
         (count stalls) " stalls; " (or pending-auto-changes 0) " changes left waiting; "
         (or agent-runs-open-at-end 0) " agent runs left open.")))

(defn- club-review [{:keys [responses phrases repetition launch-timing boundaries continuity]}]
  [:<>
   [:h4 "What the agent changed"]
   [responses-table "Agent changes" responses]
   [:h4 "Phrasing"]
   [:p.small (str (:launches launch-timing) " auto launches; " (:due-on-own-grid launch-timing) " of " (:measured launch-timing)
                  " fell due on their own interval's grid, the latest " (number-text (:max-after-due launch-timing)) " beats after. "
                  (:count boundaries) " auto sections began, " (:with-agent-change boundaries) " with an agent change; "
                  (:on-bar boundaries) " plan boundaries on the bar, " (:on-phrase boundaries) " on the phrase.")]
   [:h4 "Repetition"]
   [:p.small (str "Mean novelty " (number-text (:mean-novelty repetition)) "; repeats (novelty below "
                  (:threshold repetition) ") in phrases " (str/join ", " (:repeat-phrases repetition))
                  "; longest repeat run " (get-in repetition [:longest-repeat-run :length]) ".")]
   [table "Phrase by phrase" ["Phrase" "Bar" "Music" "Launches" "New cues" "Novelty" "Lit"]
    (for [p phrases]
      [(:phrase p) (:first-bar p) (str/join ", " (map wire (:music p))) (:launches p)
       (str/join ", " (map wire (:new-cues p))) (number-text (:novelty p)) (number-text (:mean-lit p))])]
   (when-let [line (continuity-line continuity)] [:p.small line])])

(defn- concert-review [{:keys [takes comparison]}]
  [:<>
   (for [t takes]
     ^{:key (wire (:take t))}
     [:div [:h4 (str "Take " (wire (:take t)) ": what the agent changed")]
      [responses-table (str "Take " (wire (:take t)) " agent changes") (:responses t)]
      (when-let [line (continuity-line (:continuity t))] [:p.small line])])
   [:h4 "Anchors and variation"]
   [table "Anchors" ["Section" "Anchor" "Take A" "Take B" "Kept"]
    (for [a (:anchors comparison)] [(wire (:section a)) (wire (:cue-id a)) (number-text (:a a)) (number-text (:b a)) (if (:kept? a) "yes" "no")])]
   [table "Variation" ["Section" "Fills A" "Fills B" "Shared" "Fill distance" "Stem distance"]
    (for [s (:sections comparison)]
      [(wire (:section s)) (:fills-a s) (:fills-b s) (:fills-shared s) (number-text (:fill-distance s)) (number-text (:stem-distance s))])]])

(defn- review-block [{:keys [review review-status review-error review-choice]}]
  (let [state (keyword (wire review-status))]
    [:section.rehearsal-review {:aria-label "Rehearsal review"}
     [:h3 "Review" (when review-choice
                     [:span.small.muted (str " · " (wire (:mode review-choice))
                                             (when (:take review-choice) " (both takes)") " · seed " (:seed review-choice)
                                             " · seeded replay")])]
     (case state
       :reviewing [:p.small {:role "status"} "Reviewing the seeded replay…"]
       :failed [:p.small {:role "alert"} (str "The review failed: " review-error)]
       nil)
     (when review
       [:<>
        (if (seq (:flags review))
          [:ul#rehearsal-flags.small {:aria-label "Measured flags"} (for [[i f] (map-indexed vector (:flags review))] ^{:key i} [:li f])]
          [:p#rehearsal-flags.small "No measured flags."])
        (if (= :concert (keyword (wire (:id review)))) [concert-review review] [club-review review])
        [:details.rehearsal-report [:summary.small "Full report"] [:pre (:report review)]]
        [:p.small.muted "The numbers say where to look and listen. Whether a change felt early, repetitive or unrelated to the music is your call."]])]))

(defn- run-block [{:keys [run progress]}]
  (let [rows (or (:agent run) (:agent progress))]
    (when (seq rows)
      [:div.rehearsal-run
       [:h4 "This run"]
       [table "Agent replies in this run" ["Section" "Heard" "Accepted" "Receipt" "Reply"]
        (for [{:keys [section suggested-beat accepted-beat receipts status timed-out? wall-ms]} rows]
          [(wire section) (bar-text suggested-beat) (bar-text accepted-beat)
           (str/join ", " (map (fn [[tool {s :status code :code}]] (str (wire tool) " " (wire s) (when code (str " (" (wire code) ")")))) receipts))
           (str (if timed-out? "timed out" (wire status)) (when wall-ms (str " · " (number-text wall-ms) " ms")))])]
       (when-let [gestures (seq (:gestures run))]
         [:p.small (str (count gestures) " Push gestures: " (str/join ", " (map #(str (:label %) " " (wire (:status %))) gestures)))])])))

(defn- form-view [db status]
  (let [f (form db)
        pending (get-in db (conj ui-path :pending))
        state (keyword (wire (:status status :idle)))
        connected? (get-in db [:connection :connected?])
        busy? (or (some? pending) (not connected?))]
    [:<>
     [:div.form-row
      [:label "Passage" [:select#rehearsal-passage {:value (:passage f) :on-change #(rf/dispatch-sync [::edit :passage (.. % -target -value)])}
                         (for [[v t] passages] ^{:key v} [:option {:value v} t])]]
      [:label "Seed" [:input#rehearsal-seed {:type "number" :min 0 :step 1 :value (:seed f)
                                             :on-change #(rf/dispatch-sync [::edit :seed (.. % -target -value)])}]]]
     [:div.form-row
      [:label "Agent reply time (beats)" [:input#rehearsal-latency {:type "number" :min 0 :max 64 :step "any" :value (:latency-beats f)
                                                                    :on-change #(rf/dispatch-sync [::edit :latency-beats (.. % -target -value)])}]]
      [:label "Agent" [:select#rehearsal-outage {:value (:outage f) :on-change #(rf/dispatch-sync [::edit :outage (.. % -target -value)])}
                       (for [[v t] outages] ^{:key v} [:option {:value v} t])]]
      (when (not= "none" (:outage f))
        [:label "From section" [:input#rehearsal-outage-from {:type "number" :min 0 :step 1 :value (:outage-from f)
                                                              :on-change #(rf/dispatch-sync [::edit :outage-from (.. % -target -value)])}]])]
     (when-not (:loaded? status)
       [:label.policy-toggle [:input#rehearsal-replace {:type "checkbox" :checked (boolean (:replace? f))
                                                        :on-change #(rf/dispatch-sync [::edit :replace? (.. % -target -checked)])}]
        "Replace the open show with the rehearsal show"])
     [:div.button-row
      [:button#rehearsal-arm.button {:type "button" :disabled (or busy? (#{:playing :stopping} state))
                                     :on-click #(rf/dispatch [::act :arm])} (if (= :arm pending) "Arming…" "Arm")]
      [:button#rehearsal-start.button.primary {:type "button" :disabled (or busy? (not= :armed state))
                                               :on-click #(rf/dispatch [::act :start])} "Start"]
      [:button#rehearsal-stop.button {:type "button" :disabled (or busy? (not= :playing state))
                                      :on-click #(rf/dispatch [::act :stop])} "Stop"]
      [:button#rehearsal-review.text-button {:type "button" :disabled busy?
                                             :on-click #(rf/dispatch [::act :review])} "Review the replay"]]]))

(defn- body [db]
  (let [status (get-in db [:rehearsal :status])
        {:keys [error poll-error]} (get-in db ui-path)]
    [:div.form-body
     [:p.small.muted "Play a recorded passage through this show with the recorded agent and watch it in the Room. Arming loads the rehearsal show, transport stopped at bar 0, and takes the agent's first round. Nothing is sent to fixtures."]
     [form-view db status]
     [:p#rehearsal-status.small {:role "status"} (status-line status)]
     (when-let [e (or error poll-error (:error status))] [:p#rehearsal-error.small {:role "alert" :style {:color "var(--danger)"}} e])
     (when (seq (:moods status)) [:<> [:h4 "What the agent will set"] [moods-list (:moods status)]])
     [run-block status]
     (when (or (:review status) (:review-status status)) [review-block status])]))

(defn panel
  "Perform → Tools: rehearse with the agent. Polls /api/rehearsal while open."
  [_db]
  (let [timer (atom nil)]
    (r/create-class
     {:display-name "rehearsal-panel"
      :component-did-mount (fn [] (reset! timer (js/setInterval #(rf/dispatch [::poll]) poll-ms)))
      :component-will-unmount (fn [] (some-> @timer js/clearInterval) (reset! timer nil))
      :reagent-render
      (fn [db]
        [:details.panel.auto-panel.rehearsal-panel
         {:open (boolean (get-in db (conj ui-path :open?)))
          :on-toggle (fn [e] (let [open? (.. e -target -open)]
                               (when (not= open? (boolean (get-in db (conj ui-path :open?))))
                                 (rf/dispatch [::toggle open?])
                                 (when open? (rf/dispatch [::poll])))))}
         [:summary [:span.section-label "REHEARSE WITH THE AGENT"]
          [:span#rehearsal-summary.muted.small (status-line (get-in db [:rehearsal :status]))]]
         (when (get-in db (conj ui-path :open?)) [body db])])})))
