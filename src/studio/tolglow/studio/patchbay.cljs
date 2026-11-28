(ns tolglow.studio.patchbay
  "Code-free editing of the server's pure signal routes. Loading settings never
  starts capture or enables routing; those remain explicit user actions."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.checkbox :as checkbox]
            [tolglow.studio.clock]
            [tolglow.studio.events :as events]
            [tolglow.studio.audio-returns :as audio-returns]
            [tolglow.studio.modulation :as modulation]))

(def defaults {:id "performance" :revision 0 :routes [] :dirty? false :edit-revision 0
               :generation 0 :pending nil :error nil :notice nil :server nil :visible? false})

(def route-defaults
  {:id "" :source-id "" :source-kind "midi" :cue-id "" :param ""
   :input-low "0" :input-high "127" :source-low 0 :source-high 127 :low "0" :high "1" :combine "max" :base "0"
   :curve "linear" :invert? false :steps "" :priority "0"
   :envelope? false :attack "0" :decay "0" :sustain "1" :release "1/2" :unit "beats"
   :attack-unit "beats" :decay-unit "beats" :release-unit "beats"
   :smoothing "" :smoothing-unit "beats" :timeout "4" :timeout-unit "beats" :min-confidence "0"})

(defn state [db] (merge defaults (:patchbay db)))

(defn token [db] [(get-in db [:show :runtime-id]) (get-in db [:show :epoch]) (:generation (state db))])

(defn current? [db request-token] (= request-token (token db)))

(defn- wire [id] (events/wire-id id))

(defn- lookup [data id] (get data id (get data (keyword (wire id)) (get data (wire id)))))

(defn source-ports [remote]
  (mapv #(assoc % :source-id (or (:source-id %) (:id %))) (or (:ports remote) (:sources remote) [])))

(defn- value-of [event] (.. event -target -value))

(defn- changed [s] (-> s (assoc :dirty? true :notice nil) (update :edit-revision inc)))

(defn- number! [value label] (modulation/number-value value label))

(defn- range! [low high label]
  (let [low (number! low (str label " minimum")) high (number! high (str label " maximum"))]
    (when-not (< low high) (throw (js/Error. (str label " needs an increasing range.")))) [low high]))

(defn- duration! [value unit label positive?]
  (let [n (number! value label)]
    (when-not (and (#{"beats" "bars" "phrases" "ms"} unit) (if positive? (pos? n) (not (neg? n))))
      (throw (js/Error. (str label " needs " (if positive? "a positive" "a nonnegative") " duration."))))
    {(keyword unit) n}))

(defn- duration-parts [duration fallback]
  (if-let [[unit value] (first duration)] [(str value) (wire unit)] fallback))

(defn form-from-spec [spec]
  (if-not spec (select-keys defaults [:id :revision :routes])
    {:id (wire (:id spec)) :revision (:revision spec)
     :routes
     (mapv (fn [route]
             (let [source (lookup (:sources spec) (:source route)) destination (lookup (:destinations spec) (:destination route))
                   env (:envelope route) [attack unit] (duration-parts (:attack env) ["0" "beats"])
                   [decay decay-unit] (duration-parts (:decay env) ["0" unit])
                   [release release-unit] (duration-parts (:release env) ["0" unit])
                   ;; Stored envelopes have one positive domain; preserve a
                   ;; release-only unit instead of treating zero attack as ms.
                   unit (cond (pos? (js/Number attack)) unit (pos? (js/Number decay)) decay-unit :else release-unit)
                   [smoothing smoothing-unit] (duration-parts (:smoothing route) ["" "beats"])
                   [timeout timeout-unit] (duration-parts (:timeout source) ["4" "beats"])]
               (merge route-defaults
                      {:id (wire (:id route)) :source-id (wire (:source route)) :source-kind (wire (:kind source))
                       :cue-id (if (:knob-id destination) "" (wire (:cue-id destination)))
                       :param (if (:knob-id destination) "" (wire (:param destination)))
                       :knob-id (some-> (:knob-id destination) wire)
                       :input-low (str (first (or (:source-range route) (:range source))))
                       :input-high (str (second (or (:source-range route) (:range source))))
                       :source-low (first (:range source)) :source-high (second (:range source))
                       :low (str (first (:range route))) :high (str (second (:range route)))
                       :destination-low (first (:range destination)) :destination-high (second (:range destination))
                       :combine (wire (:combine destination)) :base (str (:base destination))
                       :curve (if (map? (:curve route)) "power" (wire (:curve route :linear)))
                       :exponent (str (get-in route [:curve :exponent] 2))
                       :invert? (:invert? route false) :steps (if (:steps route) (str (:steps route)) "")
                       :priority (str (:priority route 0)) :envelope? (boolean env)
                       :attack attack :attack-unit (second (duration-parts (:attack env) ["0" unit]))
                       :decay decay :decay-unit decay-unit :sustain (str (:sustain env 1)) :release release :release-unit release-unit :unit unit
                       :envelope-curve (when (:curve env) (if (map? (:curve env)) (update (:curve env) :type #(keyword (wire %))) (keyword (wire (:curve env)))))
                       :smoothing smoothing :smoothing-unit smoothing-unit
                       :timeout timeout :timeout-unit timeout-unit :min-confidence (str (:min-confidence source 0))}))) (:routes spec))}))

(defn parameter-options [show cue-id]
  (let [cue (some #(when (= (wire cue-id) (wire (:id %))) %) (:cues show))
        effect (some #(when (= (wire (:effect-id cue)) (wire (:id %))) %) (:effects show))]
    (vec (for [[id schema] (:params effect)
               :let [bounds (:bounds schema)]
               :when (and (#{"number" "integer"} (wire (:type schema))) (vector? bounds) (= 2 (count bounds))
                          (every? number? bounds) (< (first bounds) (second bounds)))]
           {:id (wire id) :label (or (:name schema) (wire id)) :range bounds}))))

(defn build-spec [form]
  (when (or (str/blank? (:id form)) (empty? (:routes form))) (throw (js/Error. "Add a route and name the patch before saving.")))
  (let [built
        (mapv (fn [route]
                (when (or (some str/blank? ((juxt :id :source-id) route))
                          (and (str/blank? (:knob-id route)) (some str/blank? ((juxt :cue-id :param) route))))
                  (throw (js/Error. "Every route needs a source and a cue parameter or linked knob.")))
                (let [input (range! (:input-low route) (:input-high route) "Source")
                      output (range! (:low route) (:high route) "Output")
                      destination-range (if (and (number? (:destination-low route)) (number? (:destination-high route)))
                                          [(:destination-low route) (:destination-high route)] output)
                      base (number! (:base route) "Idle value") confidence (number! (:min-confidence route) "Confidence")
                      priority (number! (:priority route) "Priority")
                      knob (not-empty (:knob-id route))
                      destination-id (if knob (str "knob/" knob) (str (:cue-id route) "/" (:param route)))
                      curve (if (= "power" (:curve route)) {:type :power :exponent (number! (:exponent route "2") "Curve exponent")}
                                (keyword (:curve route)))
                      mapping (cond-> {:id (:id route) :source (:source-id route) :destination destination-id
                                       :source-range input :range output :curve curve :invert? (:invert? route) :priority priority}
                                (seq (:steps route)) (assoc :steps (number! (:steps route) "Quantization steps"))
                                (seq (:smoothing route)) (assoc :smoothing (duration! (:smoothing route) (:smoothing-unit route) "Smoothing" false))
                                (:envelope? route)
                                (assoc :envelope (cond-> {:attack (duration! (:attack route) (:attack-unit route (:unit route)) "Attack" false)
                                                         :decay (duration! (:decay route) (:decay-unit route (:unit route)) "Decay" false)
                                                         :sustain (number! (:sustain route) "Sustain")
                                                         :release (duration! (:release route) (:release-unit route (:unit route)) "Release" false)}
                                                  (:envelope-curve route) (assoc :curve (:envelope-curve route)))))]
                  (when-not (and (#{"midi" "audio" "automation" "control"} (:source-kind route))
                                 (#{"max" "min" "sum" "mean" "product" "priority"} (:combine route))
                                 (#{:linear :smoothstep :sine :ease-in :ease-out :step :power} (if (map? curve) (:type curve) curve))
                                 (or (not (map? curve)) (pos? (:exponent curve)))
                                 (js/Number.isSafeInteger priority) (<= 0 confidence 1)
                                 (<= (first destination-range) (first output) (second output) (second destination-range))
                                 (<= (first destination-range) base (second destination-range))
                                 (or (nil? (:steps mapping)) (and (js/Number.isInteger (:steps mapping)) (<= 2 (:steps mapping) 128)))
                                 (or (nil? (:envelope mapping)) (<= 0 (get-in mapping [:envelope :sustain]) 1)))
                    (throw (js/Error. "Check curve, priority, steps, confidence, sustain and destination bounds.")))
                  {:source-id (:source-id route)
                   :source {:kind (keyword (:source-kind route))
                            :range (range! (or (:source-low route) (:input-low route)) (or (:source-high route) (:input-high route)) "Source calibration")
                            :timeout (duration! (:timeout route) (:timeout-unit route) "Source timeout" true) :min-confidence confidence}
                   :destination-id destination-id
                   ;; A linked knob (#675) takes 0–1 and fans out to its links; the owner resolves its ID.
                   :destination (merge (if knob {:knob-id knob} {:cue-id (keyword (:cue-id route)) :param (keyword (:param route))})
                                       {:range destination-range :base base :combine (keyword (:combine route))})
                   :route mapping})) (:routes form))
        _ (when-not (= (count built) (count (distinct (map (comp :id :route) built)))) (throw (js/Error. "Route IDs must be unique.")))
        shared (fn [id-key value-key]
                 (reduce (fn [out item]
                           (let [id (id-key item) value (value-key item)]
                             (when (and (contains? out id) (not= (get out id) value))
                               (throw (js/Error. "Routes sharing a source or destination need matching calibration, timeout and combine settings.")))
                             (assoc out id value))) {} built))]
    {:version 1 :id (:id form) :revision (inc (:revision form 0))
     :sources (shared :source-id :source) :destinations (shared :destination-id :destination) :routes (mapv :route built)}))

(defn- request [db operation path body]
  (let [db (update db :patchbay #(-> (merge defaults %) (update :generation inc) (assoc :pending operation :error nil :notice nil)))
        s (state db)]
    {:db db :studio/http (cond-> {:path path :on-success [:patchbay/received (token db) operation (:edit-revision s)]
                                  :on-failure [:patchbay/failed (token db)]}
                          body (assoc :body (merge {:runtime-id (get-in db [:show :runtime-id])
                                                    :expected-epoch (get-in db [:show :epoch])} body)))}))

(defn- with-sources [patch server signals]
  (if patch
    (cond-> patch (contains? signals :patch-sources) (assoc :sources (:patch-sources signals)))
    server))

(defn remote
  "The server's patch as Studio shows it: the projection's, with its observed
  sources from the presented :signals frame lane (they change on every frame
  while audio runs), else the last loaded patch."
  [db signals]
  (with-sources (get-in db [:show :patch]) (:server (state db)) signals))

(rf/reg-sub ::app-view (fn [db _] {:form (state db) :show (:show db) :patch (get-in db [:show :patch])}))

(rf/reg-sub :patchbay/view
  :<- [::app-view]
  :<- [:studio/signals]
  (fn [[{:keys [form show patch]} signals] _]
    {:form form :show show :remote (with-sources patch (:server form) signals)}))
(rf/reg-event-fx :patchbay/open
  (fn [{:keys [db]} _] {:db (assoc-in db [:patchbay :visible?] true) :dispatch [:patchbay/load]}))
(rf/reg-event-fx :patchbay/load
  (fn [{:keys [db]} _]
    (when (:visible? (state db))
      (if-not (:show db) {:dispatch-later [{:ms 100 :dispatch [:patchbay/load]}]}
          (request db :load "/api/patch" nil)))))
(rf/reg-event-fx :patchbay/runtime-change
  (fn [{:keys [db]} _]
    (let [s (state db)]
      {:db (assoc db :patchbay (assoc defaults :visible? (:visible? s) :generation (inc (:generation s))))
       :dispatch (when (:visible? s) [:patchbay/load])})))
(rf/reg-event-db :patchbay/invalidate
  (fn [db _] (update db :patchbay #(-> (merge defaults %) (update :generation inc) (assoc :pending nil :visible? false)))))
(rf/reg-event-db :patchbay/edit
  (fn [db [_ path value]] (assoc db :patchbay (changed (assoc-in (state db) path value)))))
(rf/reg-event-db :patchbay/remove
  (fn [db [_ index]] (assoc db :patchbay (changed (update (state db) :routes #(vec (concat (take index %) (drop (inc index) %))))))))
(rf/reg-event-db :patchbay/add
  (fn [db _]
    (let [s (state db) ids (set (map :id (:routes s))) id (first (remove ids (map #(str "route-" %) (range 1 514))))]
      (assoc db :patchbay (changed (update s :routes conj (assoc route-defaults :id id)))))))
(rf/reg-event-fx :patchbay/source [(rf/inject-cofx :studio/signals)]
  (fn [{:keys [db signals]} [_ index id]]
    (let [source (some #(when (= id (wire (:source-id %))) %) (source-ports (remote db signals)))
          [low high] (or (:range source) (if (= "midi" (wire (:kind source))) [0 127] [0 1]))]
      {:db (assoc db :patchbay (changed (update-in (state db) [:routes index] merge
                                                 {:source-id id :source-kind (wire (:kind source)) :input-low (str low) :input-high (str high)
                                                  :source-low low :source-high high})))})))
(rf/reg-event-db :patchbay/parameter
  (fn [db [_ index parameter]]
    (let [route (get-in (state db) [:routes index]) option (some #(when (= parameter (:id %)) %) (parameter-options (:show db) (:cue-id route)))
          [low high] (:range option)]
      (assoc db :patchbay (changed (update-in (state db) [:routes index] merge {:param parameter :low (str low) :high (str high)
                                                                            :base (str low) :destination-low low :destination-high high}))))))
(defn target-fields
  "Route fields for a destination choice: a cue (then a parameter), or
  \"knob:<id>\" for a linked knob, which takes its whole 0–1 travel."
  [choice]
  (if-let [[_ knob] (re-matches #"knob:(.+)" (str choice))]
    {:knob-id knob :cue-id "" :param "" :low "0" :high "1" :base "0" :destination-low 0 :destination-high 1}
    {:knob-id nil :cue-id choice :param ""}))
(rf/reg-event-db :patchbay/target
  (fn [db [_ index choice]]
    (assoc db :patchbay (changed (update-in (state db) [:routes index] merge (target-fields choice))))))
(rf/reg-event-fx :patchbay/save
  (fn [{:keys [db]} _]
    (try (request db :save "/api/patch/save" {:source (pr-str (build-spec (state db)))})
         (catch :default error {:db (assoc-in db [:patchbay :error] (events/error-message error))}))))
(rf/reg-event-fx :patchbay/enabled
  (fn [{:keys [db]} [_ enabled?]] (request db :enabled "/api/patch/enabled" {:enabled? (boolean enabled?)})))
(rf/reg-event-fx :patchbay/received
  (fn [{:keys [db]} [_ request-token operation edit-revision data]]
    (if-not (current? db request-token) {}
      (let [s (state db) unchanged? (= edit-revision (:edit-revision s))
            s (cond-> (assoc s :pending nil :error nil)
                (= operation :load) (assoc :server data)
                (and (= operation :load) (not (:dirty? s)) unchanged?) (merge (form-from-spec (:spec data)))
                (= operation :save) (assoc :revision (inc (:revision s)))
                (and (= operation :save) unchanged?) (assoc :dirty? false)
                (= operation :save) (assoc :notice (if unchanged? "Routes saved. Enable routing when ready." "Saved the submitted version; your newer edits are still local.")))]
        {:db (assoc db :patchbay s)}))))
(rf/reg-event-db :patchbay/failed
  (fn [db [_ request-token error]]
    (if (current? db request-token) (update db :patchbay merge {:pending nil :error (events/error-message error)}) db)))

(defn- edit! [index field value] (rf/dispatch-sync [:patchbay/edit [:routes index field] value]))

(defn- input-field [index route field label]
  [:label [:span label] [:input {:type "text" :input-mode "decimal" :value (get route field "")
                               :on-change #(edit! index field (value-of %))}]])

(defn- select-field [index route field label options]
  [:label [:span label] [:select {:aria-label label :value (get route field) :on-change #(edit! index field (value-of %))}
                        (for [[value label] options] ^{:key value} [:option {:value value} label])]])

(def units [["beats" "Beats"] ["bars" "Bars"] ["phrases" "Phrases"] ["ms" "Milliseconds"]])

(def curves [["linear" "Linear"] ["smoothstep" "Smooth"] ["sine" "Sine"] ["ease-in" "Ease in"] ["ease-out" "Ease out"] ["step" "Step"] ["power" "Power"]])

(defn- route-card [show sources index route]
  (let [source (some #(when (= (:source-id route) (wire (:source-id %))) %) sources)
        options (parameter-options show (:cue-id route))]
    [:article.patch-route {:aria-label (str "Signal route " (inc index))}
     [:div.patch-route-heading [:span.patch-port-dot] [:strong (str "Route " (inc index))]
      [:span.patch-source-state (if source (str "Value " (:value source)) "Waiting for source")]
      [:button.icon-button {:type "button" :aria-label (str "Remove route " (inc index)) :on-click #(rf/dispatch [:patchbay/remove index])} "×"]]
     [:div.patch-route-flow
      [:label [:span "Source"] [:select {:aria-label "Source" :value (:source-id route) :on-change #(rf/dispatch-sync [:patchbay/source index (value-of %)])}
                                [:option {:value ""} "Choose an observed source"]
                                (when (and (seq (:source-id route)) (nil? source)) [:option {:value (:source-id route)} (str (:source-id route) " · unavailable")])
                                (for [source sources] ^{:key (:source-id source)} [:option {:value (wire (:source-id source))} (or (:label source) (wire (:source-id source)))])]]
      [:span.patch-wire {:aria-hidden true} "→"]
      [:label [:span "Lighting cue"] [:select {:aria-label "Lighting cue" :value (if (seq (:knob-id route)) (str "knob:" (:knob-id route)) (:cue-id route))
                                               :on-change #(rf/dispatch-sync [:patchbay/target index (value-of %)])}
                                       [:option {:value ""} "Choose a cue"]
                                       (for [cue (:cues show)] ^{:key (:id cue)} [:option {:value (wire (:id cue))} (or (:name cue) (wire (:id cue)))])
                                       [:optgroup {:label "Linked knobs"}
                                        (for [knob (get-in show [:knobs :knobs])]
                                          ^{:key (wire (:id knob))} [:option {:value (str "knob:" (wire (:id knob)))} (str "Knob · " (:name knob))])]]]
      (when-not (seq (:knob-id route))
        [:label [:span "Parameter"] [:select {:aria-label "Parameter" :value (:param route) :on-change #(rf/dispatch-sync [:patchbay/parameter index (value-of %)])}
                                     [:option {:value ""} "Choose a numeric parameter"]
                                     (for [option options] ^{:key (:id option)} [:option {:value (:id option)} (:label option)])]])]
     [:div.patch-controls
      [input-field index route :input-low "Source min"] [input-field index route :input-high "Source max"]
      [input-field index route :low "Output min"] [input-field index route :high "Output max"]
      [select-field index route :curve "Curve" curves]
      [:label.checkbox-label [checkbox/input {:checked (:invert? route) :on-change #(edit! index :invert? (.. % -target -checked))}] "Invert"]]
     (when (= "power" (:curve route)) [input-field index route :exponent "Power exponent"])
     [:details.patch-envelope [:summary "Envelope and response"]
      [:label.checkbox-label [checkbox/input {:checked (:envelope? route) :on-change #(edit! index :envelope? (.. % -target -checked))}] "Shape each gate with ADSR"]
      (when (:envelope? route)
        [:div.patch-controls [input-field index route :attack "Attack"] [select-field index route :attack-unit "Attack units" units]
         [input-field index route :decay "Decay"] [select-field index route :decay-unit "Decay units" units]
         [input-field index route :sustain "Sustain (0–1)"] [input-field index route :release "Release"]
         [select-field index route :release-unit "Release units" units]])
      [:div.patch-controls [input-field index route :smoothing "Smoothing (optional)"]
       [select-field index route :smoothing-unit "Smoothing units" units]
       [input-field index route :steps "Steps (optional)"] [input-field index route :timeout "Source timeout"]
       [select-field index route :timeout-unit "Timeout units" units] [input-field index route :min-confidence "Minimum confidence"]]]
     [:details.patch-envelope [:summary "Destination mixing"]
      [:p.small.muted "Routes sharing this cue parameter use the same combination and idle value."]
      [:div.patch-controls [select-field index route :combine "Combine" [["max" "Highest"] ["min" "Lowest"] ["sum" "Add"] ["mean" "Average"] ["product" "Multiply"] ["priority" "Priority"]]]
       [input-field index route :base "Idle value"] [input-field index route :priority "Route priority"]]]]))

(def panel
  (r/create-class
   {:display-name "MusicalPatchbay"
    :component-did-mount (fn [_] (rf/dispatch [:patchbay/open]))
    :component-will-unmount (fn [_] (rf/dispatch [:patchbay/invalidate]))
    :reagent-render
    (fn [_db]
      (let [{:keys [form show remote]} @(rf/subscribe [:patchbay/view])
            sources (source-ports remote) busy? (some? (:pending form))]
        [:section.panel.patchbay-panel {:aria-labelledby "patchbay-title"}
         [:div.panel-top [:div [:span.section-label "MUSIC INTO LIGHT"] [:h2#patchbay-title "Signal patchbay"]]
          [:span.connection-chip {:class (when (:enabled? remote) "active")} [:i] (if (:enabled? remote) "Routing on" "Routing off")]]
         [:div.form-body
          [:p.small.muted "Connect observed notes, controllers and audio features to lighting parameters. Sources must be connected in the controls above."]
          [audio-returns/panel]
          [:div.patch-toolbar
           [:label [:span "Patch name"] [:input {:value (:id form) :on-change #(rf/dispatch-sync [:patchbay/edit [:id] (value-of %)])}]]
           [:button.button {:disabled busy? :on-click #(rf/dispatch [:patchbay/load])} "Refresh sources"]
           [:button.button {:disabled (or busy? (and (not (:enabled? remote)) (or (nil? (:spec remote)) (:dirty? form)))) :aria-pressed (boolean (:enabled? remote))
                            :on-click #(rf/dispatch [:patchbay/enabled (not (:enabled? remote))])}
            (if (:enabled? remote) "Disable routing" "Enable routing")]]
          (when (empty? sources) [:p.patch-empty "No input activity yet. Connect MIDI, the Live bridge or audio capture, then play a note or signal."])
          [:div.patch-routes (for [[index route] (map-indexed vector (:routes form))]
                              ^{:key (:id route)} [route-card show sources index route])]
          [:div.patch-actions [:button.text-button {:disabled (or busy? (>= (count (:routes form)) 512)) :on-click #(rf/dispatch [:patchbay/add])} "+ Add connection"]
           [:button.button.primary {:disabled (or busy? (empty? (:routes form))) :on-click #(rf/dispatch [:patchbay/save])} (if (= :save (:pending form)) "Saving…" "Save routing")]
           (when (:dirty? form) [:span.small.muted "Unsaved changes"])]
          (when (:error form) [:p.error {:role "alert"} (:error form)])
          (when (:notice form) [:p.small {:role "status"} (:notice form)])
          (when (seq (:diagnostics remote))
            [:details.patch-envelope [:summary "Routing diagnostics"]
             [:ul (for [[index diagnostic] (map-indexed vector (take 12 (:diagnostics remote)))]
                    ^{:key index} [:li.small (str (wire (:code diagnostic)) (when-let [id (:source-id diagnostic)] (str " · " (wire id))))])]])]]))}))
