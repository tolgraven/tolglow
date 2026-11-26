(ns tolglow.studio.push-display
  "Passive device display: the transport supplies snapshots; this route performs
  no show requests or device acquisition. The selected scene renders capture frames."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.block-trace :as trace]
            [tolglow.studio.push-scene :as scene]
            [tolglow.studio.page-overview :as pages]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.param-visualization :as visualization]
            [tolglow.visualization.parameter-views :as views]))

(defn- saved-show-catalog [catalog]
  (when (map? catalog)
    (cond-> (assoc (select-keys catalog [:kind :page :page-count :total :next-kind?])
                   :rows (mapv #(select-keys % [:name :selected? :sources :version]) (take 8 (:rows catalog))))
      (map? (:checkpoint catalog))
      (assoc :checkpoint (select-keys (:checkpoint catalog) [:name :label :modified-at-ms :status]))
      (map? (:preview catalog))
      (assoc :preview (let [preview (:preview catalog)]
                        (cond-> (assoc (select-keys preview [:name :unavailable? :version :effect-version])
                                       :sources (mapv #(select-keys % [:label :modified-at-ms])
                                                      (take 2 (:sources preview))))
                          (sequential? (:categories preview))
                          (assoc :categories (into [] (comp (filter #(or (keyword? %) (string? %))) (take 6))
                                                   (:categories preview)))
                          (map? (:usage preview))
                          (assoc :usage (select-keys (:usage preview) [:cue-count :page-count :running-count]))
                          (seq (:load-options preview)) (assoc :load-options (vec (take 2 (:load-options preview))))))))))

(defn personality
  "The active track feel's name and picture, when the show supplied an SVG data URI."
  [{:keys [name icon]}]
  (when (and (string? icon) (str/starts-with? icon "data:image/svg+xml;base64,") (<= (count icon) 16384))
    {:name (when (string? name) (subs name 0 (min 80 (count name)))) :icon icon}))

(defn- knob-entry
  "A linked knob (#666) as the show projected it for a Push display, bounded."
  [knob]
  (when (map? knob)
    (cond-> (select-keys knob [:knob-id :name :value :text :missing? :links])
      (sequential? (:columns knob))
      (assoc :columns (mapv #(select-keys % [:label :text :ratio :status :diverged?])
                            (take 8 (filter map? (:columns knob))))))))

(defn normalize-view [view]
  (let [view (if (map? view) view {})]
    (-> (select-keys view [:title :page :origin :status :master :blackout? :bpm :selected
                          :cue :effect :swatch :category-colors :page-overview :display-mode :scene :audio-waveform :instance-id
                          :parameter-bank :active-effect-navigation :touched-parameter :fine? :musical-timing])
        (assoc :touched-parameters (vec (take 8 (:touched-parameters view)))
               :personality (personality (:personality view))
               :saved-show-catalog (saved-show-catalog (:saved-show-catalog view))
               :pad-overlay (when-let [overlay (:pad-overlay view)]
                              (cond
                                (#{:beat-ratios "beat-ratios"} (:layout overlay))
                                (assoc (select-keys overlay [:layout :ratio-mode :param :title :current-ratio])
                                       :cells (mapv #(select-keys % [:position :label :selected?])
                                                    (take 16 (:cells overlay))))
                                ;; Knob looks (#547): the top pad row's eight saved knob positions.
                                (#{:knob-presets "knob-presets"} (:layout overlay))
                                (assoc (select-keys overlay [:layout :title])
                                       :cells (mapv (fn [cell]
                                                      (-> (select-keys cell [:position :slot :state :matches? :label])
                                                          (assoc :knobs (mapv #(select-keys % [:name :knob-id :text :missing?])
                                                                              (take 4 (:knobs cell))))))
                                                    (take 8 (:cells overlay))))
                                :else
                                (assoc (select-keys overlay [:layout :ratio-mode])
                                       :rows (vec (take 7 (:rows overlay)))
                                       :cells (vec (take 64 (:cells overlay))))))
               :target-picker (when-let [picker (:target-picker view)]
                                 (assoc (select-keys picker [:open? :page :pages :armed])
                                        :rows (mapv #(select-keys % [:kind :label]) (take 8 (:rows picker)))))
               :preset (when (map? (:preset view))
                         (select-keys (:preset view) [:saved? :changed? :record? :action]))
               :touched-control (when (map? (:touched-control view))
                                  (select-keys (:touched-control view) [:target :editable? :knob-id]))
               :knob (knob-entry (:knob view))
               :knob-page (when-let [page (:knob-page view)]
                            (when (sequential? (:knobs page))
                              {:knobs (into [] (comp (take 8) (keep knob-entry)) (:knobs page))})))
        (cond-> (seq (:waveform-glyphs view))
          ;; Registered waveform glyphs for shape choices, bounded like the rest.
          (assoc :waveform-glyphs (into [] (comp (take 32) (filter map?)
                                                 (map #(-> (select-keys % [:id :name :glyph])
                                                           (update :glyph (fn [g] (vec (take 129 g)))))))
                                        (:waveform-glyphs view))))
        (assoc :parameters (vec (take 8 (:parameters view)))
               :active-effects (vec (take 8 (:active-effects view)))
               :cells (vec (take 8 (:cells view))) :labels (vec (take 8 (:labels view)))))))

(defn observe-view [previous view]
  (let [next (visualization/update-push previous (normalize-view view) view)
        {:keys [binding-path target-id sample]} (:block-trace view)
        identity [(:runtime-id view) (:epoch view) (:instance-id view) (:definition-version view) binding-path target-id]]
    (if (and (contains? #{:block-trace "block-trace"} (:display-mode next)) (seq binding-path))
      (let [history (when (= identity (:trace-identity previous)) (:trace-samples previous))]
        (assoc next :trace-identity identity
               :trace-options (select-keys (:block-trace view) [:binding-path :stage-index :component-index :write-index])
               :trace-samples (trace/append-sample (or history []) sample)))
      next)))

(rf/reg-event-db :push-display/update
  (fn [db [_ view]] (update db :push-display observe-view view)))
(rf/reg-sub :push-display/view (fn [db _] (:push-display db)))

(defn install-api! []
  (let [api (js-obj)]
    (aset api "ready" false)
    (aset api "frameReady" scene/rendered!)
    (aset api "update" (fn [view]
                         (rf/dispatch-sync [:push-display/update (js->clj view :keywordize-keys true)])
                         ;; Flush Reagent's pending render; the capture worker waits
                         ;; two animation frames before reading the composited image.
                         (r/flush)))
    (aset js/globalThis "tolglowPushDisplay" api)))

(defn- envelope [definition]
  (when-let [env (:envelope definition)]
    [:span.push-envelope {:title "Effect envelope definition"}
     (for [[key short] [[:attack "A"] [:decay "D"] [:sustain "S"] [:release "R"]]
           :let [value (get env key)] :when (some? value)]
       ^{:key key} [:span [:b short] " " (if (= key :sustain) (widgets/number-label value) (widgets/duration-label value))])]))

(defn bank-label [{:keys [offset total count]}]
  (when (and (number? count) (> count 1) (number? offset) (number? total) (pos? total))
    (str "Params " (inc offset) "–" (min (+ offset 8) total) " / " total)))

(defn preset-model [view]
  (when (and (:cue view) (map? (:preset view)))
    (let [{:keys [saved? changed? record? action]} (:preset view)
          clear? (contains? #{:clear "clear"} action)]
      {:state (cond changed? :changed saved? :saved :else :default)
       :label (cond changed? "Edited" saved? "Saved" :else "Default")
       :hint (when (and record? (:instance-id view))
               (if clear? "Clear preset · top button" "Save preset · top button"))
       :action (when (and record? (:instance-id view)) (if clear? :clear :save))})))

(defn global-control-model [view]
  (let [{:keys [target editable?]} (:touched-control view)
        master? (contains? #{:master "master"} target)
        tempo? (contains? #{:tempo "tempo"} target)]
    (when (or master? tempo?)
      {:target (if master? :master :tempo)
       :label (if master? "Master" "Tempo")
       :value (if master? (when (widgets/finite? (:master view)) (* 100 (:master view))) (:bpm view))
       :schema {:type :number :bounds (if master? [0 100] [20 200]) :unit (if master? "%" "BPM")}
       :locked? (false? editable?)})))

(defn knob-model
  "The touched knob, or the knob page, for the encoder area. Push sends the
  knob's resolved link values from the same projection Studio's Knobs panel reads."
  [view]
  (let [{:keys [target]} (:touched-control view)]
    (cond
      (and (contains? #{:knob "knob"} target) (:knob view))
      {:kind :knob :knob (:knob view)}
      (seq (get-in view [:knob-page :knobs]))
      {:kind :page :knobs (get-in view [:knob-page :knobs])})))

(defn- knob-value-label [{:keys [value text]}]
  (or text (when (widgets/finite? value) (widgets/number-label value)) "—"))

(defn- knob-display [{:keys [kind knob knobs]}]
  (if (= :knob kind)
    (let [{:keys [name missing? columns]} knob]
      [:section.push-knob {:aria-label (str "Knob " (or name (widgets/label (:knob-id knob)))
                                            ", " (knob-value-label knob))}
       [:div.push-knob-heading
        [:span.push-knob-kind "Knob"]
        [:strong (or name (widgets/label (:knob-id knob)))]
        [:span.push-knob-value (if missing? "Removed" (knob-value-label knob))]]
       [:div.push-target-columns
        (for [i (range 8) :let [{:keys [label text ratio status diverged?] :as column} (get columns i)]]
          ^{:key i}
          [:article {:class (cond (nil? column) "empty"
                                  (not (contains? #{:ok "ok"} status)) "dangling"
                                  :else "available")
                     :title (when diverged? "Edited since the knob last moved")}
           (when column
             [:<> [:small label]
              [:strong (or text (widgets/label status))]
              (when diverged? [:span.push-knob-diverged "Edited"])])])]])
    [:section.push-knob {:aria-label "Knob page"}
     [:div.push-knob-heading [:span.push-knob-kind "Knobs"] [:strong "Track encoders"]]
     [:div.push-target-columns
      (for [i (range 8) :let [{:keys [knob-id name missing? links] :as entry} (get knobs i)]]
        ^{:key i}
        [:article {:class (cond (nil? entry) "empty" missing? "dangling" :else "available")}
         (when entry
           [:<> [:small (if missing? "Missing knob" (str links " links"))]
            [:strong (or name (widgets/label knob-id))]
            (when-not missing? [:span.push-knob-value (knob-value-label entry)])])])]]))

(defn- preset-feedback [view]
  (when-let [{:keys [state label hint action]} (preset-model view)]
    [:div.push-preset {:data-state (name state) :data-action (some-> action name)
                      :aria-label (str "Cue parameters: " label (when hint (str ". " hint)))
                      :title (or hint (case state :changed "Current parameters differ from cue defaults"
                                                :saved "Saved cue parameters" "Authored cue defaults"))}
     [:span.push-preset-dot] [:span label]
     (when hint [:span.push-preset-action hint])]))

(defn- global-control [model]
  (let [{:keys [target label schema value locked?]} model
        [low high] (:bounds schema)
        amount (widgets/proportion schema value)]
    [:section.push-global-control {:data-target (name target) :class (when locked? "locked")
                                   :aria-label (str label ", " (widgets/number-label value) " " (:unit schema)
                                                    ", range " low " to " high
                                                    (when locked? ", controlled by external sync"))}
     [:div.push-global-dial [widgets/rotary schema value {}]]
     [:div.push-global-readout
      [:span.push-global-name label]
      [:strong (widgets/number-label value) [:small (:unit schema)]]]
     [:div.push-global-scale
      [:div.push-global-endpoints [:span (widgets/number-label low)] [:span (widgets/number-label high)]]
      [:div.push-global-track [:i {:style {:width (str (* 100 (or amount 0)) "%")}}]]
      [:span.push-global-hint (if locked? "External sync · locked" "Encoder + touch strip")]]]))

(defn- navigation [view]
  (let [{:keys [index count offset]} (:active-effect-navigation view)]
    [:div.push-navigation
     (when (and (number? count) (pos? count) (number? index))
       [:span.push-effect-navigation {:aria-label (str "Active effect " (inc index) " of " count)}
        [:span (str "Effect " (inc index) " / " count)]
        [:span.push-effect-dots
         (for [[local effect] (map-indexed vector (:active-effects view))]
           ^{:key (:id effect)} [:i {:class (str (when (= (+ (or offset 0) local) index) "selected ") (widgets/label (:phase effect)))
                                     :title (str (widgets/label (:cue-id effect)) " · " (widgets/label (:phase effect)))}])]])
     (when-let [label (bank-label (:parameter-bank view))] [:span.push-bank label])
     (when (:fine? view) [:span.push-fine "Fine"])]))

(defn- parameter-id [parameter] (or (:param parameter) (:id parameter)))

(defn encoder-slots [view]
  (let [touches (cons (:touched-parameter view) (:touched-parameters view))]
    (mapv (fn [index]
            (let [parameter (get (:parameters view) index)
                  id (parameter-id parameter)
                  touched? (and parameter
                                (some (fn [{:keys [encoder param instance-id]}]
                                        (and (or (nil? instance-id) (= instance-id (:instance-id view)))
                                             (or (nil? param) (= param id))
                                             (if (some? encoder) (= index encoder) (and param (= param id)))))
                                      touches))]
              {:index index :parameter parameter :touched? touched?}))
          (range 8))))

(defn- compound-key [parameter]
  (when (#{:color :range :vector} (widgets/parameter-kind (:schema parameter)))
    (parameter-id parameter)))

(defn compound-areas [slots]
  (keep (fn [group]
          (when-let [selected (first (filter :touched? group))]
            (let [start (:index (first group))
                  end (inc (:index (last group)))
                  free (take-while #(nil? (:parameter %)) (subvec slots end))
                  width (+ (count group) (count free))]
              (when (and (> width 1) (compound-key (:parameter selected)))
                {:start start :width width :parameter (:parameter selected)}))))
        (partition-by (fn [{:keys [index parameter]}]
                        (if-let [id (compound-key parameter)] [:compound id] [:encoder index]))
                      slots)))

(defn- axis-reading [{:keys [schema component component-label effective-value value]}]
  (let [values (visualization/components schema (if (some? effective-value) effective-value value))
        color? (= :color (views/color-kind schema))]
    (when-let [shown (if color?
                       (widgets/component-amount values component-label component (views/color-space schema))
                       (get values component))]
      [:output.push-axis-reading
       (widgets/number-label (get (:component-schemas schema) component (if color? {:bounds [0 1]} schema)) shown)])))

(defn- parameters [view compact?]
  (let [slots (encoder-slots view)
        areas (when-not compact? (compound-areas slots))
        expanded (into #{} (mapcat (fn [{:keys [start width]}] (range start (+ start width)))) areas)]
    [:section.push-parameters {:aria-label "Eight encoder parameters" :class (when compact? "compact")}
     (for [{:keys [index parameter touched?]} slots
           :let [expanded? (contains? expanded index)
                 component (visualization/component-label parameter)]]
       ^{:key index}
       [:article.push-parameter {:data-encoder index
                                 :style {:grid-column (inc index)}
                                 :class (cond (nil? parameter) "empty" touched? "touched")}
        (when (or parameter (not expanded?))
          [:div.push-parameter-name [:span.push-encoder (inc index)]
           [:span.push-parameter-label (or (:name parameter) (some-> (:id parameter) widgets/label) "—")]
           (when-let [state (:source-state parameter)]
             (let [text (case (keyword state)
                          :active (if-let [fixture (:sample-fixture parameter)]
                                    (widgets/label (or (:name fixture) (:id fixture)))
                                    (widgets/label (first (:source-types parameter))))
                          :held "Held" :pending "Waiting" :error "Error" nil)]
               [:span.push-source-state {:data-state (name state) :title (or (:source-error parameter) text)} text]))
           (when (and component (not (contains? #{:value "value"} component)))
             [:span.push-component {:title "Encoder component"} (widgets/label component)])])
        (cond
          (nil? parameter) nil
          compact? [:div.push-compact-value
                    (widgets/value-label (:schema parameter)
                                         (if (some? (:effective-value parameter))
                                           (:effective-value parameter)
                                           (:value parameter)))]
          expanded? (when touched? [axis-reading parameter])
          :else [:div.push-slot-content
                 [visualization/push view parameter touched?]
                 (when (and touched? (compound-key parameter)) [axis-reading parameter])])])
     (for [{:keys [start width parameter]} areas]
       ^{:key start} [:div.push-compound-area {:style {:grid-column (str (inc start) " / span " width)}}
                      [visualization/push view parameter true]])]))

(defn- target-choices [{:keys [rows page pages]}]
  [:section.push-targets {:aria-label "Cue launch targets"}
   [:div.push-targets-heading [:strong "Launch targets"] [:span (str "Page " page " / " pages)]
    [:small "Choose with a pad in the bottom row"]]
   [:div.push-target-columns
    (for [i (range 8) :let [row (get rows i)]]
      ^{:key i} [:article {:class (if row "available" "empty")}
                 (when row [:<> [:small (widgets/label (:kind row))] [:strong (:label row)]])])]])

(defn- variation-overlay [{:keys [layout ratio-mode rows cells]}]
  (let [layout (name layout)
        row-labels (if (= "rows" layout) (map :param rows) ["Full states"])
        slots (into #{} (keep (fn [{:keys [position kind stale?]}]
                               (when (and (contains? #{:saved "saved"} kind) (not stale?)) (first position)))) cells)]
    [:section.push-targets {:aria-label "Cue variations"}
     [:div.push-targets-heading
      [:strong (if (= "rows" layout) "Parameter variations" "Full cue variations")]
      [:span (if (contains? #{:free "free"} ratio-mode) "Free ratios" "Preferred ratios")]
      [:small "Amber · Generated / Blue · Saved"]]
     [:div.push-target-columns
      (for [i (range 8)]
        ^{:key i} [:article {:class (if (contains? slots i) "available" "empty")}
                   [:small (str "Saved " (inc i))]
                   (when-let [row (nth row-labels i nil)] [:strong (widgets/label row)])])]]))

(defn- ratio-overlay [{:keys [param title current-ratio ratio-mode cells]}]
  (let [by-position (into {} (map (juxt :position identity)) cells)]
    [:section.push-targets {:aria-label "Beat ratio pad choices"}
     [:div.push-targets-heading
      [:strong "Beat ratios"]
      [:span (or title (widgets/label param)) " · " current-ratio]
      [:span (if (#{:free "free"} ratio-mode) "Free" "Preferred")]
      [:small "Top two pad rows · Lift encoder to return"]]
     [:div.push-ratio-pads
      (for [y [7 6] x (range 8)
            :let [{:keys [label selected?]} (get by-position [x y])]]
        ^{:key (str x "-" y)}
        [:div {:class (cond selected? "selected" (nil? label) "unavailable")
               :aria-label (when selected? (str label ", current"))}
         (or label "—")])]]))

(defn- knob-preset-overlay
  "Knob looks (#547): one column per top-row pad. Push 3 lists only the pads
  its captured layout has; the others read as not captured."
  [{:keys [cells]}]
  (let [by-slot (into {} (map (juxt :slot identity)) cells)]
    [:section.push-targets {:aria-label "Knob looks"}
     [:div.push-targets-heading
      [:strong "Knob looks"]
      [:span "Empty pad saves · Pad recalls · Record + pad overwrites"]
      [:small "Dim · Empty / Green · Saved / White · Selected / Blue · Moved"]]
     [:div.push-target-columns.push-knob-looks
      (for [i (range 8) :let [{:keys [state matches? knobs] :as cell} (get by-slot i)
                              state (some-> state name)
                              moved? (and (= "selected" state) (false? matches?))]]
        ^{:key i}
        [:article {:class (cond (nil? cell) "empty" moved? "moved" (= "selected" state) "selected"
                                (= "stored" state) "available" :else "empty")}
         [:small (str "Look " (inc i))]
         (cond
           (nil? cell) [:strong "—"]
           (= "empty" state) [:strong "Empty"]
           :else [:<> (for [{:keys [name knob-id text missing?]} knobs]
                        ^{:key (str knob-id)}
                        [:span.push-knob-look-value (str (or name (widgets/label knob-id)) " " text
                                                         (when missing? " ✕"))])])])]]))

(defn- live-display [view]
  (let [cue (:cue view) effect (:effect view) accent (widgets/color-css (or (:swatch view) (:color cue)))
        active? (:active? cue) audio? (contains? #{:audio "audio"} (:display-mode view))
        global (global-control-model view)]
    [:main#push-display {:style {"--push-accent" accent} :aria-label "Push lighting display"
                        :class (str (when audio? "audio-mode ") (when global "global-control-active"))}
     [:header.push-heading
      [:span.push-brand (or (:title view) "Tolglow")]
      (when-let [{:keys [name icon]} (:personality view)]
        [:span.push-personality {:title name} [:img {:src icon :alt (or name "Track feel")}]])
      [:span.push-page (widgets/label (:page view))]
      [:strong.push-selection (or (:name cue) (:name effect) (some-> (:selected view) widgets/label) "Choose a cue")]
      (if-let [armed (get-in view [:target-picker :armed])]
        [:span.push-armed-target (str "Next cue → " (:label armed))]
        [navigation view])
      [preset-feedback view]
      (if global
        [global-control global]
        [:<> [widgets/category-tags (:categories effect) (:category-colors view)]
         [envelope effect]
         [:span.push-tempo (widgets/number-label (:bpm view)) [:small " BPM"]]
         [:span.push-master (str (widgets/number-label (when (widgets/finite? (:master view)) (* 100 (:master view)))) "%")]])
      [:span.push-status {:class (cond (:blackout? view) "blackout" (:releasing? cue) "releasing" active? "active")}
       (cond (:blackout? view) "BLACKOUT" (:releasing? cue) "RELEASE" active? "PLAYING" :else (widgets/label (:status view)))]]
     (cond
       (#{:beat-ratios "beat-ratios"} (get-in view [:pad-overlay :layout])) [ratio-overlay (:pad-overlay view)]
       ;; A touched knob encoder shows its value even while knob looks are open.
       (= :knob (:kind (knob-model view))) [knob-display (knob-model view)]
       (#{:knob-presets "knob-presets"} (get-in view [:pad-overlay :layout])) [knob-preset-overlay (:pad-overlay view)]
       (:pad-overlay view) [variation-overlay (:pad-overlay view)]
       (get-in view [:target-picker :open?]) [target-choices (:target-picker view)]
       (knob-model view) [knob-display (knob-model view)]
       (contains? #{:simulator :rig "simulator" "rig"} (:display-mode view)) [scene/view view]
       (contains? #{:pages "pages"} (:display-mode view)) [pages/view view]
       (contains? #{:block-trace "block-trace"} (:display-mode view)) [trace/plot (:trace-samples view) true (:trace-options view)]
       audio? [:<> [:section.push-audio {:aria-label "Captured audio display"} [widgets/audio-waveform (:audio-waveform view)]]
        [parameters view true]]
       :else [:<> [parameters view false]
        [:footer.push-cells
         (for [index (range 8) :let [cell (get (:cells view) index)
                                    color (widgets/color-css (:color cell))
                                    text (or (:name cell) (get (:labels view) index) "—")]]
           ^{:key index} [:div.push-cell {:style {"--cell-color" color} :class (when (:active? cell) "active")}
                         [:i] [:span text]])]])]))

(defn- checkpoint-time [milliseconds]
  (when (and (widgets/finite? milliseconds) (<= 0 milliseconds 8640000000000000))
    (let [utc (.toISOString (js/Date. milliseconds))]
      (str (subs utc 0 10) " " (subs utc 11 16) " UTC"))))

(defn- checkpoint-heading [status]
  (case status
    (:ready "ready") "Confirm checkpoint restore"
    (:pending "pending") "Restoring checkpoint"
    (:restored "restored") "Checkpoint restored"
    (:changed "changed") "Checkpoint changed"
    (:unavailable "unavailable") "Checkpoint unavailable"
    (:session-changed "session-changed") "Show session changed"
    "Checkpoint restore refused"))

(defn- registered-preview [type {:keys [version effect-version categories usage] label :name}]
  [:section.push-catalog-detail {:aria-label (str type " metadata")}
   [:h2 label]
   [:div.push-catalog-sources
    [:article
     [:strong (str type " version " version)]
     (when effect-version [:span (str "Effect version " effect-version)])
     (when (seq categories) [:span (str/join " · " (map name categories))])]
    [:article
     [:strong "Usage"]
     (if (= type "Effect")
       [:span "Registered cues · " (:cue-count usage)]
       [:span "Cue pages · " (:page-count usage)])
     [:span (str (:running-count usage) " running")]]]])

(defn- catalog-display [{:keys [kind page page-count rows preview total checkpoint next-kind?]}]
  (let [[library type unavailable empty-label]
        (case kind
          (:effect "effect") ["Registered effects" "Effect" "This effect version is no longer available." "No registered effects."]
          (:cue "cue") ["Registered cues" "Cue" "This cue version is no longer available." "No registered cues."]
          ["Saved shows" nil "This show is no longer available." "No saved shows available."])]
    [:main#push-display.push-catalog {:aria-label library}
     [:header.push-catalog-header
      [:span.push-brand "Tolglow"]
      [:h1 (cond checkpoint (checkpoint-heading (:status checkpoint))
                 preview (if type (str type " preview") "Saved show preview")
                 :else library)]
      (when (pos? (or page-count 0))
        [:span.push-catalog-page (str (inc page) " / " page-count " pages")])]
     (cond
       checkpoint
       [:section.push-catalog-detail {:aria-label "Checkpoint restore"}
        [:h2 (:name checkpoint)]
        [:div.push-catalog-sources
         [:article [:strong (:label checkpoint)]
          (when-let [time (checkpoint-time (:modified-at-ms checkpoint))] [:span time])]]]

       (:unavailable? preview)
       [:p.push-catalog-empty unavailable]

       (and preview type)
       [registered-preview type preview]

       preview
       [:section.push-catalog-detail {:aria-label "Saved show checkpoints"}
        [:h2 (:name preview)]
        [:div.push-catalog-sources
         (for [{:keys [label modified-at-ms]} (:sources preview)]
           ^{:key label} [:article
                          [:strong (if (some #{label} (:load-options preview)) (str "Choose " label) label)]
                          (when-let [time (checkpoint-time modified-at-ms)] [:span time])])]]

       (empty? rows)
       [:p.push-catalog-empty empty-label]

       :else
       [:section.push-catalog-rows {:aria-label (if type library "Shows on this page")}
        (for [[index {:keys [name selected? sources version]}] (map-indexed vector rows)]
          ^{:key index} [:article {:class (when selected? "selected")}
                         [:strong {:title name} name]
                         (when version [:span (str "Version " version)])
                         (for [source sources] ^{:key source} [:span source])])])
     [:footer.push-catalog-footer
      (if checkpoint
        (case (:status checkpoint)
          (:pending "pending") [:span "Waiting for the show owner"]
          (:restored "restored") [:span "Back · Live display"]
          (:ready "ready") [:<> [:span "Back · Choose checkpoint"] [:span "Confirm · Replace running show"]]
          [:span "Back · Choose checkpoint"])
        [:<> [:span (if preview (str "Back · " library) "Back · Live display")]
         (when (and next-kind? (not preview)) [:span "Next library"])
         (when (and (not preview) (pos? (or total 0)))
           [:span (if (> page-count 1) "Page buttons · Browse   ·   Row buttons · Preview" "Row buttons · Preview")])])]]))

(defn display [view]
  (if-let [catalog (:saved-show-catalog view)]
    [catalog-display catalog]
    [live-display view]))

(def component
  (r/create-class
   {:display-name "PushDeviceDisplay"
    :component-did-mount #(aset (aget js/globalThis "tolglowPushDisplay") "ready" true)
    :component-will-unmount #(aset (aget js/globalThis "tolglowPushDisplay") "ready" false)
    :reagent-render (fn [] [display @(rf/subscribe [:push-display/view])])}))
