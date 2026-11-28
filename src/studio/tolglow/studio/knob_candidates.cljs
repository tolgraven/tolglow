(ns tolglow.studio.knob-candidates
  "Generated knob candidates (#667). Reads the shared :knob-candidates
  projection and sends the same :knob-candidate/* owner commands as the REPL
  and agents: generate, reroll, sweep in the Room, edit, keep or discard.
  See docs/generated-knobs.md."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.knobs :as knobs]))

(def ^:private ui-path [:ui :knob-candidates])

(defn running-cues
  "Cue IDs with a running launch, in launch order, as wire text."
  [show]
  (->> (:instances show)
       (filter #(and (:cue-id %) (= "running" (events/wire-id (:phase % "running")))))
       (sort-by :order)
       (map (comp events/wire-id :cue-id))
       distinct vec))

(def type-choices
  "Destination types a candidate may be restricted to, as the show names them."
  [["number" "Numbers"] ["integer" "Integers"] ["colour" "Colours"] ["vector" "Vectors"]
   ["choice" "Choices"] ["switch" "Switches"]])

(defn generate-fields
  "Fields for :knob-candidate/generate from the form, or {:error message}.
  A blank seed lets the show choose one and report it. No selected type means
  every type."
  [{:keys [character seed cue-ids types]}]
  (let [text (str/trim (str seed))]
    (cond
      (and (seq text) (not (re-matches #"-?\d{1,15}" text))) {:error "The seed is a whole number."}
      :else {:fields (cond-> {}
                       (seq text) (assoc :seed (js/parseInt text 10))
                       (seq character) (assoc :character character)
                       (seq cue-ids) (assoc :cue-ids (vec (sort cue-ids)))
                       (seq types) (assoc :types (vec (sort types))))})))

(defn- number-text [x]
  (cond (and (number? x) (integer? x)) (str x)
        (number? x) (let [s (.toFixed (js/Number x) 3)] (str/replace s #"\.?0+$" ""))
        :else (str x)))

(defn- ratio-text [x]
  (if (and (number? x) (not (integer? x)))
    ;; Transit carries exact ratios as numbers; show the common musical fractions.
    (or (some (fn [[n d]] (when (< (js/Math.abs (- x (/ n d))) 1e-9) (str n "/" d)))
              (for [d [2 3 4 6 8] n [1 2 3 4 6 8]] [n d]))
        (number-text x))
    (number-text x)))

(defn range-text
  "Readable [from → to] for a link range in the order the knob sweeps it."
  [{:keys [range direction quantize]}]
  (let [[a b] (if (= "down" (events/wire-id (or direction "up"))) (reverse range) range)
        show (if (and quantize (not (:step quantize))) ratio-text number-text)]
    (if (every? number? [a b])
      (str (show a) " → " (show b))
      (str (knobs/value-text a) " → " (knobs/value-text b)))))

(defn- colour? [x] (and (vector? x) (= 3 (count x)) (every? number? x)))

(defn- css-colour [[r g b]]
  (let [channel #(js/Math.round (* 255 (js/Math.pow (max 0 (min 1 %)) (/ 1 2.2))))]
    (str "rgb(" (channel r) "," (channel g) "," (channel b) ")")))

;; Events

(defn- send [db op fields]
  (let [[effect request] (events/command db op fields nil nil)]
    {:db (update-in db ui-path dissoc :error)
     :fx [[effect (assoc request :on-failure [::failed])]]}))

(rf/reg-event-db ::failed (fn [db [_ message]] (assoc-in db (conj ui-path :error) (str message))))

(rf/reg-event-db ::edit (fn [db [_ path value]] (assoc-in db (into ui-path path) value)))

(rf/reg-event-db ::toggle-cue
  (fn [db [_ cue]]
    (update-in db (conj ui-path :cue-ids) (fn [ids] (let [ids (set ids)] (if (ids cue) (disj ids cue) (conj ids cue)))))))

(rf/reg-event-db ::toggle-type
  (fn [db [_ type]]
    (update-in db (conj ui-path :types) (fn [ids] (let [ids (set ids)] (if (ids type) (disj ids type) (conj ids type)))))))

(defn candidate-link-target
  "The typed picker's target for the candidate: :knob-candidate/link adds the
  link, and the sweep then auditions it in the Room with the rest."
  [candidate]
  (let [ids (set (map (comp events/wire-id :id) (:links candidate)))]
    {:form-key "candidate" :op :knob-candidate/link :fields {}
     :link-id (first (remove ids (map #(str "picked-" %) (iterate inc 1))))
     :error-path (conj ui-path :error) :on-failure [::failed]}))

(rf/reg-event-fx ::generate
  (fn [{:keys [db]} _]
    (let [{:keys [error fields]} (generate-fields (get-in db ui-path))]
      (if error
        {:db (assoc-in db (conj ui-path :error) error)}
        (send (update-in db ui-path dissoc :name :audition :acked-revision) :knob-candidate/generate fields)))))

(rf/reg-event-fx ::repeat
  (fn [{:keys [db]} [_ knob-id]]
    (send (update-in db ui-path dissoc :name :audition :acked-revision) :knob-candidate/generate {:from-knob knob-id})))

(rf/reg-event-fx ::reroll
  (fn [{:keys [db]} _] (send (update-in db ui-path dissoc :name) :knob-candidate/reroll {})))

(rf/reg-event-fx ::discard
  (fn [{:keys [db]} _] (send (update-in db ui-path dissoc :name :audition :acked-revision) :knob-candidate/discard {})))

(rf/reg-event-fx ::unlink
  (fn [{:keys [db]} [_ link-id]] (send db :knob-candidate/unlink {:link-id link-id})))

(rf/reg-event-fx ::keep
  (fn [{:keys [db]} [_ candidate value]]
    (let [name (str/trim (str (get-in db (conj ui-path :name) (:name candidate))))]
      (send (update-in db ui-path dissoc :name :audition :acked-revision) :knob-candidate/keep
            (cond-> {:value value} (seq name) (assoc :name name))))))

;; Sweeping: at most one audition is in flight. Moves made meanwhile collapse
;; into the newest value, sent when the receipt arrives, so the Room ends on
;; the last position.

(defn- send-audition [db value]
  (let [[effect request] (events/command db :knob-candidate/audition {:value value} [::audition-acked value] nil)]
    {:db (-> db (update-in ui-path dissoc :error) (assoc-in (conj ui-path :in-flight) value))
     :fx [[effect (assoc request :on-failure [::audition-failed value])]]}))

(defn- after-audition [db sent]
  (let [db (update-in db ui-path dissoc :in-flight)
        latest (get-in db (conj ui-path :audition))]
    (if (and (number? latest) (not= latest sent)) (send-audition db latest) {:db db})))

(rf/reg-event-fx ::audition
  (fn [{:keys [db]} [_ value]]
    (when (and (number? value) (<= 0 value 1))
      (let [db (assoc-in db (conj ui-path :audition) value)]
        (if (contains? (get-in db ui-path) :in-flight) {:db db} (send-audition db value))))))

(rf/reg-event-fx ::audition-acked
  (fn [{:keys [db]} [_ value ack]]
    (after-audition (cond-> db
                      (= value (get-in db (conj ui-path :audition)))
                      (assoc-in (conj ui-path :acked-revision) (:draft-revision ack)))
                    value)))

(defn sweep-value
  "The sweep position to show: the local move until a snapshot carries the Room
  draft revision its receipt reported, then the shared audition value, which
  may come from the REPL or an agent."
  [db projection]
  (let [{:keys [audition acked-revision] :as ui} (get-in db ui-path)
        server (:audition projection)]
    (cond
      (and (number? audition)
           (or (contains? ui :in-flight) (nil? acked-revision) (nil? server)
               (< (:draft-revision server -1) acked-revision)))
      audition
      server (:value server)
      :else (get-in projection [:candidate :value] 0.5))))

(rf/reg-event-fx ::audition-failed
  (fn [{:keys [db]} [_ value message]]
    (update (after-audition db value) :db assoc-in (conj ui-path :error) (str message))))

;; Views

(defn- input-value [event] (.. event -target -value))

(defn- range-swatches [{:keys [range direction]}]
  (when (every? colour? range)
    (let [[a b] (if (= "down" (events/wire-id (or direction "up"))) (reverse range) range)]
      [:span.candidate-swatches {:aria-hidden true}
       [:span.candidate-swatch {:style {:background (css-colour a)}}]
       [:span.candidate-swatch {:style {:background (css-colour b)}}]])))

(defn- link-row [candidate link explain connected?]
  (let [id (events/wire-id (:id link))
        why (some #(when (= id (events/wire-id (:link-id %))) %) explain)]
    [:li.candidate-link {:data-link-id id :data-status (events/wire-id (:status link))}
     [:div.candidate-link-head
      [:span.candidate-link-label (:label link)]
      (when-let [role (:role link)] [:span.candidate-role (events/wire-id role)])
      [:output.candidate-link-value {:aria-label (str (:label link) " at the sweep position")}
       (or (get-in link [:resolved :text]) "—")]]
     [:div.candidate-link-shape
      [range-swatches link]
      [:span (range-text link)]
      [:span.small.muted (events/wire-id (:curve link))]
      (when-let [text (knobs/envelope-text link)] [:span.small.muted text])
      (when (not= [0 1] (mapv js/Number (:input link)))
        [:span.small.muted (str "knob " (str/join "–" (map number-text (:input link))))])
      [:button.text-button {:type "button" :disabled (or (not connected?) (<= (count (:links candidate)) 2))
                            :aria-label (str "Drop " (:label link) " from the candidate")
                            :on-click #(rf/dispatch [::unlink (:id link)])} "Drop"]]
     (when why
       [:p.candidate-why.small (:why why)
        (when-let [partner (:correlation why)] [:span.candidate-pair (str " · " partner)])])]))

(defn- insufficient-view [{:keys [message excluded]}]
  [:div.candidate-insufficient {:role "status"}
   [:p message]
   (when (seq excluded)
     [:ul.candidate-excluded
      (for [{:keys [label message reason]} (take 12 excluded)]
        ^{:key (str label (events/wire-id reason))}
        [:li [:span label] [:span.small.muted (str " — " message)]])])])

(defn- candidate-view [db {:keys [candidate audition]} connected?]
  (let [ui (get-in db ui-path)
        value (sweep-value db {:audition audition :candidate candidate})
        name (get ui :name (:name candidate))]
    [:article.candidate-card {:data-candidate-id (:id candidate)}
     [:div.candidate-head
      [:input.candidate-name {:type "text" :aria-label "Candidate knob name" :value (or name "")
                              :on-change #(rf/dispatch [::edit [:name] (input-value %)])}]
      [:span.small.muted (str "Seed " (:seed candidate) " · " (:character-name candidate)
                              " · score " (number-text (get-in candidate [:score :total]))
                              (when (:edited? candidate) " · edited")
                              (when (:repeated? candidate) " · no new mapping found"))]]
     [:label.candidate-sweep
      [:span "Sweep in the Room"]
      [:input.knob-slider {:type "range" :min 0 :max 1 :step 0.01 :value value :disabled (not connected?)
                           :aria-label "Sweep the candidate in the Room"
                           :on-change #(rf/dispatch [::audition (js/parseFloat (input-value %))])}]
      [:output.knob-value (.toFixed (js/Number value) 2)]]
     [:p.small.muted (if audition
                       "The Room shows this candidate. Live output and saved knobs are unchanged until you keep it."
                       "Move the sweep to audition the candidate in the Room.")]
     [:ul.candidate-links
      (for [link (:links candidate)]
        ^{:key (events/wire-id (:id link))} [link-row candidate link (:explain candidate) connected?])]
     [knobs/add-link-form db (candidate-link-target candidate) connected? "Add a destination to the candidate"]
     [:div.button-row
      [:button.button.primary {:type "button" :disabled (not connected?)
                               :on-click #(rf/dispatch [::keep candidate value])} "Keep as knob"]
      [:button.button {:type "button" :disabled (not connected?) :on-click #(rf/dispatch [::reroll])} "Reroll"]
      [:button.text-button {:type "button" :disabled (not connected?) :on-click #(rf/dispatch [::discard])} "Discard"]]]))

(defn panel
  "Perform panel that generates, auditions, rerolls and keeps a knob mapping."
  [db]
  (let [projection (get-in db [:show :knob-candidates])
        connected? (get-in db [:connection :connected?])
        ui (get-in db ui-path)
        cues (running-cues (:show db))
        selected (set (:cue-ids ui))
        recipes (filter :recipe (get-in db [:show :knobs :knobs]))
        status (some-> (:status projection) events/wire-id)]
    [:section.knob-candidates-panel.panel {:aria-label "Generate a knob"}
     [:div.panel-top [:strong "Generate a knob"] [:span.small.muted "Try a mapping in the Room, reroll, keep"]]
     [:div.candidates-body
      [:div.candidate-form
       [:label "Character"
        [:select {:aria-label "Candidate character" :value (or (:character ui) "")
                  :on-change #(rf/dispatch [::edit [:character] (input-value %)])}
         [:option {:value ""} "Any"]
         (for [{:keys [id name]} (:characters projection)]
           ^{:key (events/wire-id id)} [:option {:value (events/wire-id id)} name])]]
       [:label "Seed"
        [:input {:type "text" :inputMode "numeric" :aria-label "Candidate seed" :placeholder "any"
                 :value (or (:seed ui) "") :on-change #(rf/dispatch [::edit [:seed] (input-value %)])}]]
       [:button.button {:type "button" :disabled (not connected?) :on-click #(rf/dispatch [::generate])} "Generate"]]
      [:div.candidate-cues {:role "group" :aria-label "Destination types the candidate may use"}
       [:span.small.muted (if (seq (:types ui)) "Only these types:" "Every type, or only:")]
       (for [[type title] type-choices]
         ^{:key type}
         [:button.candidate-cue {:type "button" :aria-pressed (contains? (set (:types ui)) type)
                                 :on-click #(rf/dispatch [::toggle-type type])} title])]
      [:div.candidate-cues {:role "group" :aria-label "Cues the candidate may use"}
       [:span.small.muted (if (seq selected) "Only these cues:" "All running cues, or only:")]
       (for [cue cues]
         ^{:key cue}
         [:button.candidate-cue {:type "button" :aria-pressed (contains? selected cue)
                                 :on-click #(rf/dispatch [::toggle-cue cue])} cue])]
      (when (seq recipes)
        [:div.candidate-recipes
         [:span.small.muted "Repeat a kept knob:"]
         (for [knob recipes]
           ^{:key (events/wire-id (:id knob))}
           [:button.text-button {:type "button" :disabled (not connected?)
                                 :aria-label (str "Regenerate " (:name knob) " from its recipe")
                                 :on-click #(rf/dispatch [::repeat (:id knob)])}
            (str (:name knob) " · seed " (get-in knob [:recipe :seed]))])])
      (case status
        "ready" [candidate-view db projection connected?]
        "insufficient" [insufficient-view (:candidate projection)]
        nil)
      (when-let [error (:error ui)] [:p.knob-error {:role "alert"} error])]]))
