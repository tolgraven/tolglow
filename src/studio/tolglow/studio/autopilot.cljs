(ns tolglow.studio.autopilot
  "Operator and telemetry views of the headless closed-loop conductor."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.autopilot-signal :as signal]
            [tolglow.studio.checkbox :as checkbox]
            [tolglow.studio.events :as events]
            [tolglow.studio.lighting-policy :as policy]
            [tolglow.studio.lighting-scenes :as scenes]
            [tolglow.studio.modulation :as modulation]))

(def hdr-defaults {:gain 1 :white-mix 1 :spill 0 :spill-white-mix 1 :radius 1 :max-excess 1 :falloff :linear})

(def defaults
  {:scope :global :targets {:brightness 0.5 :activity 0.4} :window {:bars 2} :interval {:beats 1}
   :duration {:bars 1} :strategy :gradual :preference :parameters :step 0.1 :tolerance 0.025
   :standby-level 0 :max-gain 2 :overshoot 0 :rotate? true :seed 1 :hdr hdr-defaults})

(defn- initial [section settings]
  (if (= :hdr section)
    (merge hdr-defaults {:scope :global} settings)
    (-> (merge defaults settings)
        (update :targets #(merge (:targets defaults) %))
        (update :hdr #(merge hdr-defaults %)))))

(defn edited [section settings draft]
  (reduce-kv assoc-in (initial section settings) (or draft {})))

(defn- draft-key [section] [section ::controls])

(defn edit-db [db section path change]
  (let [ui (policy/form db) k (draft-key section)
        draft (get-in ui [:drafts k])
        state (edited section (policy/visible-settings db section) draft)
        v (if (fn? change) (change (get-in state path)) change)]
    (assoc db :lighting-policy-ui (-> ui (assoc-in [:drafts k path] v) (dissoc :error)))))

(rf/reg-event-db ::edit (fn [db [_ section path change]] (edit-db db section path change)))
(defn- edit! [section path value] (rf/dispatch-sync [::edit section path value]))

(defn target-stage [v]
  (if (contains? v :blend)
    (let [fraction (modulation/number-value (:blend v) "Target blend per frame")]
      (when-not (<= 0 fraction 1) (throw (js/Error. "Enter a per-frame blend between zero and one.")))
      {:blend fraction})
    (let [[u value] (first v) n (modulation/number-value value "Target response")]
      (when-not (and (= 1 (count v)) (some #{u} (map first signal/units)) (<= 0 n))
        (throw (js/Error. "Target response needs a nonnegative duration.")))
      {u n})))

(defn patch [draft]
  (reduce-kv
    (fn [m path v]
      (assoc-in m path
        (case (first path)
          :target-response (target-stage v)
          (:window :interval :duration) (signal/parse-duration v (name (first path)))
          :seed (let [n (modulation/number-value v "Rotation seed")]
                  (when-not (js/Number.isSafeInteger n) (throw (js/Error. "Rotation seed must be a whole number.")))
                  n)
          (signal/parse-spec v)))) {} draft))

(defn write-settings [db section enable]
  (let [ui (policy/form db) k (draft-key section) draft (get-in ui [:drafts k])]
    (try
      (cond
        ;; Turning off always takes effect, even with an unfinished edit.
        (false? enable) (or (policy/save-request db section {:enabled? false} nil) {})
        (or (some? enable) (seq draft))
        (or (policy/save-request db section (cond-> (patch draft) enable (assoc :enabled? true)) k) {})
        :else {})
      (catch js/Error e {:db (assoc db :lighting-policy-ui (assoc ui :error (.-message e)))}))))

(rf/reg-event-fx ::save (fn [{:keys [db]} [_ section]] (write-settings db section nil)))
(rf/reg-event-fx ::enable (fn [{:keys [db]} [_ section on?]] (write-settings db section on?)))

(rf/reg-sub ::report (fn [db _] (get-in db [:server-projection :view :autopilot])))
(rf/reg-sub ::effective-enabled :<- [::report] (fn [report _] (true? (:enabled? report))))
(rf/reg-sub ::page-ids
  (fn [db _] (mapv (juxt :id #(or (:name %) (events/wire-id (:id %))))
                    (get-in db [:server-projection :view :pages]))))
(rf/reg-sub ::cue-ids
  (fn [db _] (mapv (juxt :id #(or (:name %) (events/wire-id (:id %))))
                    (get-in db [:server-projection :view :cues]))))
(rf/reg-sub ::instances
  (fn [db _] (mapv #(select-keys % [:id :cue-id]) (get-in db [:server-projection :view :instances]))))
(rf/reg-event-db ::pick
  (fn [db [_ field v]] (assoc db :lighting-policy-ui (assoc (policy/form db) field v))))

(rf/reg-sub ::draft :<- [::policy/form] (fn [form [_ section]] (get-in form [:drafts (draft-key section)])))
(rf/reg-sub ::busy :<- [::policy/form] :<- [::policy/online] (fn [[ui online?] _] (or (not online?) (some? (:pending ui)))))

(defn- enable [section label on? disabled?]
  [:label.policy-toggle
   [checkbox/input {:checked (boolean on?) :disabled disabled? :aria-label label
                    :on-change #(rf/dispatch [::enable section (.. % -target -checked)])}] label])

(defn- scope-field [scope change!]
  (let [groups @(rf/subscribe [::policy/group-ids])
        pages @(rf/subscribe [::page-ids])
        fixtures @(rf/subscribe [::scenes/fixtures])
        selected (:fixture-ids scope)
        available (concat [[:global "Entire rig"]]
                          (for [g groups] [{:group-id g} (str "Group · " (events/wire-id g))])
                          (for [[p label] pages] [{:page-id p} (str "Cue page · " label)])
                          [[:fixtures "Choose fixtures…"]])
        token (if (some? selected) :fixtures scope)
        entries (if (some #(= token (first %)) available) available
                  (conj (vec available) [token (str "Saved · " (pr-str token))]))]
    [:div.conduct-scope
     [signal/choice "Affected fixtures" token entries #(change! (if (= % :fixtures) {:fixture-ids []} %))]
     (when (some? selected)
       [:div.conduct-fixtures {:role "group" :aria-label "Selected fixtures"}
        (for [{:keys [id name]} fixtures]
          ^{:key (pr-str id)}
          [:label.policy-toggle
           [checkbox/input {:checked (boolean (some #{id} selected))
                            :on-change #(let [on? (.. % -target -checked)]
                                          (change! (fn [prior]
                                                     (update prior :fixture-ids
                                                             (fn [ids] (if on? (vec (distinct (conj ids id))) (into [] (remove #{id}) ids)))))))}]
           (or name (events/wire-id id))])])]))

(def unit-opts {:min 0 :max 1 :step 0.005 :percent? true})

(def gain-opts {:min 0 :max 4 :step 0.025})

(defn- bound [section controls signals path label opts]
  [signal/field [section path] label (get-in controls path) opts signals #(edit! section path %)])

(defn- shared-signals [signals]
  (let [ui @(rf/subscribe [::policy/form]) name (:new-signal ui "")
        id (if (str/starts-with? name ":") (keyword (subs (str/trim name) 1)) (str/trim name))
        ids (sort-by events/wire-id (keys signals))
        current (if (contains? signals (:chosen-signal ui)) (:chosen-signal ui) (first ids))]
    [:details.source-readings [:summary "Shared signals"]
     (when (seq ids)
       [signal/choice "Edit shared signal" current (map (juxt identity events/wire-id) ids)
        #(rf/dispatch-sync [::pick :chosen-signal %])])
     (when current
       [signal/field [:shared current] "Shared signal value" (get signals current) unit-opts
        (dissoc signals current) #(edit! :autopilot [:signals current] %)])
     [:div.conduct-new-signal
      [:label "New signal name" [:input {:value name :on-change #(rf/dispatch-sync [::pick :new-signal (.. % -target -value)])}]]
      [:button.button {:type "button" :disabled (or (str/blank? name) (= ":" name) (contains? signals id))
                       :on-click #(do (edit! :autopilot [:signals id] (signal/starting-spec :waveform unit-opts 0 signals))
                                      (rf/dispatch-sync [::pick :chosen-signal id])
                                      (rf/dispatch-sync [::pick :new-signal ""])
                                      (rf/dispatch [::signal/load-registry]))} "Add shared waveform"]]]))

(defn- gain-override [controls signals]
  (let [ui @(rf/subscribe [::policy/form]) selected (:hdr-gain-selection ui)
        options (concat [[nil "Choose a cue, playing instance or fixture…"]]
                        (for [[id label] @(rf/subscribe [::cue-ids])] [[:cue-gains id] (str "Cue · " label)])
                        (for [{:keys [id cue-id]} @(rf/subscribe [::instances])]
                          [[:instance-gains id] (str "Playing · " (events/wire-id cue-id) " · " (events/wire-id id))])
                        (for [{:keys [id name]} @(rf/subscribe [::scenes/fixtures])]
                          [[:source-gains id] (str "Fixture · " (or name (events/wire-id id)))]))
        active (when (some #(= selected (first %)) options) selected)]
    [:details.source-readings [:summary "Individual gains"]
     [signal/choice "Gain override for" active options #(rf/dispatch-sync [::pick :hdr-gain-selection %])]
     (when active
       [signal/field [:hdr active] "Override gain" (get-in controls active 1) gain-opts signals
        #(edit! :hdr active %)])]))

(defn falloff-choices
  "Preset spill falloffs and the current authored curve. When a preset replaces
  the committed curve in the draft, a restore option keeps that curve reachable."
  [current saved]
  (cond-> [[:linear "Linear"] [:quadratic "Quadratic"] [:smoothstep "Smoothstep"]]
    (vector? current) (conj [current (str "Authored curve · " (count current) " points")])
    (and (vector? saved) (not= saved current))
    (conj [saved (str "Restore authored curve · " (count saved) " points")])))

(defn- hdr-fields [section base controls signals saved-falloff]
  [:<>
   [:div.conduct-fields
    (for [[k label opts] [[:gain "Emission gain" gain-opts] [:max-excess "Allowed excess" gain-opts]
                          [:white-mix "Source whitening" unit-opts] [:spill "Neighbor spill" unit-opts]
                          [:radius "Spill radius (m)" {:min 0 :max 20 :step 0.05}]
                          [:spill-white-mix "Neighbor whitening" unit-opts]]]
      ^{:key k} [bound section controls signals (conj base k) label opts])]
   ;; An authored [[distance-fraction weight] ...] curve from the REPL or a
   ;; show file stays selected. Its points are edited from the REPL.
   (let [falloff (get-in controls (conj base :falloff))]
     [signal/choice "Spill falloff" falloff (falloff-choices falloff saved-falloff)
      #(edit! section (conj base :falloff) %)])])

(defn- apply-button [section label draft busy?]
  [:div.performance-actions [:button.button {:type "button" :disabled (or busy? (empty? draft))
                                            :on-click #(rf/dispatch [::save section])} label]])

(defn- hist [series]
  (when (and (vector? series) (= 10 (count series)) (every? number? series))
    [:div.conduct-distribution {:aria-label "Time spent in each tenth of the output range"}
     (for [[i value] (map-indexed vector series)]
       ^{:key i} [:span {:title (str (* i 10) "–" (* (inc i) 10) "% · " (signal/percent value) " of time")
                         :style {:height (str (max 2 (* 100 value)) "%")}}])]))

(defn action-label [result]
  (cond (or (keyword? result) (string? result)) (str/replace (events/wire-id result) "-" " ")
        (map? result) (let [operation (or (:op result) (:error result))]
                        (when operation
                          (str (events/wire-id operation)
                               (when-let [id (or (:cue-id result) (:param result) (:instance-id result))]
                                 (str " · " (events/wire-id id))))))
        :else nil))

(defn- telemetry []
  (let [{:keys [statistics targets requested-targets decision last-action]} @(rf/subscribe [::report])]
    (when (seq statistics)
      [:section.conduct-meters {:aria-label "Measured energy"}
       (for [[k text] [[:brightness "Fixture brightness"] [:activity "Visual activity"]]
             :let [s (k statistics)] :when (map? s)]
         ^{:key k} [:div.conduct-meter
                    [:div.conduct-meter-title [:span text] [:strong (signal/percent (:mean s))]]
                    [:meter {:min 0 :max 1 :value (or (:mean s) 0) :aria-label (str text " rolling average")}]
                    [:span (str "Target " (signal/percent (k targets))
                                (when (and (number? (k requested-targets)) (not= (signal/percent (k requested-targets)) (signal/percent (k targets))))
                                  (str " · requested " (signal/percent (k requested-targets))))
                                " · now " (signal/percent (:instant s)))]
                    [:small (str (signal/percent (:min s)) " – " (signal/percent (:max s))
                                 " · in band " (signal/percent (:target-hit-fraction s)))]
                    (when (number? (:stddev s)) [:small (str "Std. deviation " (signal/percent (:stddev s)))])
                    [hist (:histogram s)]])
       (when (map? decision)
         [:div.conduct-action
          [:span (str (:active-count decision 0) " active sources")]
          (when-let [action (action-label last-action)] [:span (str "Last action · " action)])])])))

(defn- target-stage-field [stage v]
  (let [title (str "Target " (name stage))
        write! #(edit! :autopilot [:target-response stage] %)
        mode (if (contains? v :blend) :blend :duration)]
    [:div.policy-stage
     [:div.policy-binding
      [signal/choice (str title " response") mode [[:duration "Reach in duration"] [:blend "Blend per frame"]]
       #(write! (if (= % :blend) {:blend 0.1} {:beats 0}))]]
     (if (= mode :duration)
       [signal/duration-field (str title " duration") (or v {:beats 0}) write!]
       [:label (str title " fraction")
        [:input {:type "text" :input-mode "text" :value (str (:blend v))
                 :on-change #(write! {:blend (.. % -target -value)})}]])]))

(defn- settings-panel [controls draft signals busy? saved-falloff]
  (let [section :autopilot edit #(edit! section [%1] %2)]
    [:fieldset {:disabled busy?}
     [scope-field (:scope controls) #(edit :scope %)]
     [:div.conduct-fields
      [bound section controls signals [:targets :brightness] "Brightness target" unit-opts]
      [bound section controls signals [:targets :activity] "Visual activity target" unit-opts]]
     [:div.policy-grid
      [signal/duration-field "Averaging window" (:window controls) #(edit :window %)]
      [signal/duration-field "Decision interval" (:interval controls) #(edit :interval %)]
      [signal/choice "Approach" (:strategy controls) [[:gradual "Gradual"] [:instant "Next decision"]] #(edit :strategy %)]
      [signal/choice "Preferred changes" (:preference controls)
       [[:parameters "Adjust existing parameters"] [:cues "Launch or retire cues"]
        [:generate "Compose one-shot effects"] [:author "Request an agent-authored effect"]] #(edit :preference %)]]
     [:label.policy-toggle [checkbox/input {:checked (:rotate? controls)
                                            :on-change #(edit :rotate? (.. % -target -checked))}] "Rotate active fixtures"]
     [:details.source-readings [:summary "Target attack and release"]
      [:div.policy-grid
       (for [stage [:attack :release]]
         ^{:key stage} [target-stage-field stage (get-in controls [:target-response stage])])]]
     [:details.source-readings [:summary "Decision limits"]
      [:div.conduct-fields
       (for [[k label opts] [[:step "Change per decision" unit-opts] [:standby-level "Standby level" unit-opts]
                             [:max-gain "Maximum emission gain" (assoc gain-opts :min 1)] [:overshoot "Allowed overshoot" unit-opts] [:tolerance "Target tolerance" unit-opts]]]
         ^{:key k} [bound section controls signals [k] label opts])]
      [:div.policy-grid
       [signal/choice "Run while cue plays" (:enable-cue controls)
        (into [[nil "Manual switch only"]] @(rf/subscribe [::cue-ids])) #(edit :enable-cue %)]
       [:label "Rotation seed" [:input {:type "text" :input-mode "numeric" :value (:seed controls)
                                         :on-change #(edit :seed (.. % -target -value))}]]
       [signal/duration-field "One-shot duration" (:duration controls) #(edit :duration %)]]]
     [:details.source-readings [:summary "Highlight spill"] [hdr-fields section [:hdr] controls signals saved-falloff]]
     [shared-signals signals]
     [apply-button section "Apply autopilot settings" draft busy?]]))

(defn panel []
  (let [settings @(rf/subscribe [::policy/settings :autopilot])
        draft @(rf/subscribe [::draft :autopilot])
        controls (edited :autopilot settings draft)
        busy? @(rf/subscribe [::busy])
        ui @(rf/subscribe [::policy/form])
        running? @(rf/subscribe [::effective-enabled])]
    [:section.panel.scene-controls.conduct {:aria-label "Energy autopilot"}
     [:header [:h2 "Energy autopilot"] [enable :autopilot "Enable autopilot" (:enabled? settings) busy?]]
     (when (and running? (not (:enabled? settings)) (:enable-cue settings))
       [:p.conduct-action (str "Running with cue " (events/wire-id (:enable-cue settings)))])
     [telemetry]
     [settings-panel controls draft (:signals controls {}) busy? (get-in settings [:hdr :falloff])]
     (when-let [error (:error ui)] [:p.performance-error {:role "alert"} error])]))

(defn hdr-panel []
  (let [settings @(rf/subscribe [::policy/settings :hdr]) draft @(rf/subscribe [::draft :hdr])
        controls (edited :hdr settings draft) signals (:signals @(rf/subscribe [::policy/settings :autopilot]))
        busy? @(rf/subscribe [::busy])]
    [:section.panel.scene-controls.conduct {:aria-label "HDR emission"}
     [:header [:h2 "HDR emission"] [enable :hdr "Enable HDR emission" (:enabled? settings) busy?]]
     [:fieldset {:disabled busy?}
      [scope-field (:scope controls) #(edit! :hdr [:scope] %)]
      [:details.source-readings [:summary "Gain and spatial spill"] [hdr-fields :hdr [] controls signals (:falloff settings)]]
      [gain-override controls signals]
      [apply-button :hdr "Apply HDR settings" draft busy?]]]))
