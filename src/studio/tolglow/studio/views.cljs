(ns tolglow.studio.views
  "Reagent views. Domain changes go through re-frame events; the app-db is the
  source of truth. WebGL and Neovim own only their isolated canvas surfaces."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.chat :as chat]
            [tolglow.studio.checkpoints :as checkpoints]
            [tolglow.studio.cue-pad-preview :as pad-preview]
            [tolglow.studio.composition-layers :as layers]
            [tolglow.studio.variable-blocks :as variable-blocks]
            [tolglow.studio.debug :as debug]
            [tolglow.studio.camera-controls :as camera-controls]
            [tolglow.studio.preview-playback :as playback]
            [tolglow.studio.airplay :as airplay]
            [tolglow.studio.block-builder :as block-builder]
            [tolglow.studio.material-designer :as materials]
            [tolglow.studio.connections :as connections]
            [tolglow.studio.events :as events]
            [tolglow.studio.keyframe-editor :as keyframes]
            [tolglow.studio.expressive-mapping :as expressive]
            [tolglow.studio.fixture-library :as fixture-library]
            [tolglow.studio.fog-fields :as fog-fields]
            [tolglow.studio.rdm :as rdm]
            [tolglow.studio.interop :as interop]
            [tolglow.studio.rig-editor :as rig-editor]
            [tolglow.studio.spatial :as spatial]
            [tolglow.studio.spatial-preview :as spatial-preview]
            [tolglow.studio.modulation :as modulation]
            [tolglow.studio.navigation :as nav]
            [tolglow.studio.page-routes :as pages]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.numeric-display :as numeric]
            [tolglow.studio.parameter-source :as sources]
            [tolglow.studio.parameter-visualization :as visualization]
            [tolglow.studio.waveform :as waveform]
            [tolglow.studio.repl :as repl]
            [tolglow.studio.performance :as performance]
            [tolglow.studio.lighting-scenes :as lighting-scenes]
            [tolglow.studio.lighting-policy :as lighting-policy]
            [tolglow.studio.autopilot :as autopilot]
            [tolglow.studio.workspace :as workspace]))

(defn- send! [event] (rf/dispatch event))
;; Controlled values must reach app-db before React restores the input after
;; its DOM event. Commands remain queued; their acknowledgements arrive later.
(defn- input! [event] (rf/dispatch-sync event))
(defn- set-ui! [path value] (input! [:studio/ui-set path value]))
(defn- input-value [event] (.. event -target -value))
(defn- checked? [event] (.. event -target -checked))
(defn- number-value [event]
  (let [value (js/parseFloat (input-value event))]
    (when (js/Number.isFinite value) value)))
(defn- label [value] (str/replace (if (keyword? value) (name value) (str value)) #"[-_]" " "))
(defn- find-id [items id] (some #(when (= (:id %) id) %) items))
(defn- display-name [item] (or (:name item) (label (:id item))))
(defn- swatch [item] (widgets/swatch-css (or (:swatch item) "#a8b99a")))
(defn- color-style [item] {"--cue-color" (swatch item)})
(defn- hex-color [components]
  (str "#" (apply str (map (fn [n] (.padStart (.toString (js/Math.round (* 255 (max 0 (min 1 (or n 0)))) ) 16) 2 "0")) (take 3 components)))))
(defn- color-components [hex]
  (mapv #( / (js/parseInt (subs hex % (+ % 2)) 16) 255) [1 3 5]))

(defn- button [id text event & [class]]
  [:button {:id id :type "button" :class (or class "button") :on-click #(send! event)} text])

(defn- import-selection! [event]
  (let [input (.-target event)]
    (when-let [file (aget (.-files input) 0)] (send! [:studio/import-file file]))
    (set! (.-value input) "")))

(defn- file-actions [mobile?]
  [:<>
   [button (if mobile? "mobile-export" "export-show") "Export show" [:studio/export] "text-button"]
   [button (if mobile? "mobile-restore-show" "restore-show") "Restore show" [:checkpoints/open] "text-button"]
   [:label.text-button.file-label {:id (when mobile? "mobile-import")} "Import show"
    [:input.studio-file-input {:id (if mobile? "mobile-import-show" "import-show") :type "file"
                               :accept ".transit,.edn,application/transit+json" :on-change import-selection!}]]])

(defn- navigation-link [current {:keys [route label short-label icon shortcut]}]
  [:a.nav-item {:href (nav/href route) :class (when (= route current) "selected")
                :data-view (name route) :aria-label label :title label
                :aria-current (when (= route current) "page")}
   [:span.nav-icon {:aria-hidden true
                    :style {"--nav-icon" (str "url('/studio/icons/lucide/" (name icon) ".svg')")}}]
   [:span.nav-title label]
   [:span.nav-short {:aria-hidden true} (or short-label label)]
   [:span.nav-key {:aria-hidden true} shortcut]])

(defn- sidebar [db]
  (let [{:keys [ui connection]} db]
    [:aside.sidebar
     [:a.brand {:href (nav/href :home) :aria-label "Tolglow studio home"}
      [:span.brand-mark "t" [:span "g"]] [:span "tolglow" [:small "LIGHT IN MOTION"]]]
     [:nav {:aria-label "Studio views"}
      (for [link pages/navigation]
        ^{:key (:route link)} [navigation-link (:view ui) link])]
     [:div.sidebar-bottom
      [:div.connection [:i#connection-dot {:class (if (:connected? connection) "connected" "failed")}]
       [:span#connection-label (if (:connected? connection) "Studio connected" "Connecting to studio")]]
      [:div#output-status.muted.small
       (if (= "art-net" (get-in db [:show :status :output :type])) "Art-Net output enabled" "Local studio · preview")]
      [file-actions false]]]))

(defn- topbar [db]
  (let [show (:show db) ui (:ui db) playing? (get-in show [:transport :playing?] true)
        master (:master show 1)]
    [:header.topbar
     [:div.session-title [:span.eyebrow "STUDIO / " [:span#view-label (str/upper-case (name (:view ui :perform)))]]
      [:span#session-title (or (get-in show [:rig :name]) "Night studies")]]
     [:div.transport-group
      [:button#transport.icon-button {:aria-label (if playing? "Pause transport" "Play transport")
                                      :title "Play / pause transport"
                                      :on-click #(send! [:studio/command "transport/set" {:transport {:playing? (not playing?)}}])}
       [:span.transport-symbol {:class (if playing? "pause" "play") :aria-hidden true}]]
      [:label.bpm-label
       [numeric/number-input {:id "bpm" :type "number" :min 1 :max 999 :value (get-in show [:transport :bpm] 120)
                    :aria-label "Tempo in beats per minute"
                    :on-change #(when-let [value (number-value %)]
                                  (send! [:studio/command "transport/set" {:transport {:bpm value}}]))}]
       [:span "BPM"]] [:span#beat-pulse.beat-pulse {:aria-hidden true}]]
     [:label.master-control [:span "MASTER"]
      [:input#master {:type "range" :min 0 :max 1 :step 0.01 :value master
                      :aria-label "Master intensity" :on-change #(when-let [value (number-value %)] (input! [:studio/master value]))}]
      [:output#master-value (str (js/Math.round (* 100 master)) "%")]]
     [:button#open-repl.icon-button.repl-open {:aria-label "Open Clojure REPL" :title "Clojure REPL · Ctrl+`"
                                              :on-click #(send! [:repl/toggle])} ">_"]
     [:button#blackout.blackout {:aria-pressed (boolean (:blackout? show))
                                :on-click #(send! [:studio/command "blackout/set" {:value (not (:blackout? show))}])}
      [:span {:aria-hidden true} "●"] " Blackout"]]))

(defn- heading [title & [action]]
  [:div.view-heading [:h1 title] action])

(defn- duration-control [id title value unit on-change & [allow-zero?]]
  [:label [:span.duration-name title]
   [:span.duration-control {:style {:display "flex" :gap "0.5rem"}}
    [numeric/number-input {:id id :type "number" :min (if allow-zero? 0 0.001) :step "any"
             :value value :aria-label (str title " value")
             :on-change #(when-let [n (number-value %)] (on-change n nil))}]
    [:select {:id (str id "-unit") :aria-label (str title " unit") :value unit
              :on-change #(on-change nil (input-value %))}
     [:option {:value "beats"} "Beats"] [:option {:value "bars"} "Bars"]
     [:option {:value "phrases"} "Phrases"] [:option {:value "ms"} "Milliseconds"]]]])

(defn- slot-duration [slot]
  (or (some (fn [[k v]] (when (#{:beats :bars :phrases :ms} k) [v (name k)])) (:duration slot))
      (when-let [ms (:duration-ms slot)] [ms "ms"])
      [4 "beats"]))

(defn- cue-cell [db page x y colors]
  (let [show (:show db) ui (:ui db)
        recent? (events/recent-page? db (:id page))
        cue-id (:cue-id (some #(when (and (= x (:x %)) (= y (:y %))) %) (:cells page)))
        cue (find-id (:cues show) cue-id)
        preview (or (get colors cue-id) (when (string? cue-id) (get colors (keyword cue-id))))
        active (filter #(= cue-id (:cue-id %)) (:instances show))
        queued? (and cue (some #(= cue-id (:cue-id %)) (:pending-cue-launches show)))
        cell {:cue-id cue-id :x x :y y :page-id (:id page)}
        down (fn [event]
               (when (and (or cue (not recent?))
                          (or (= 0 (.-button event)) (= "touch" (.-pointerType event))))
                 (.setPointerCapture (.-currentTarget event) (.-pointerId event))
                 (send! [:studio/gesture-down (merge cell {:pointer-id (.-pointerId event)
                                                          :client-x (.-clientX event) :client-y (.-clientY event)})])))]
    [:button.cue-cell
     {:class (str (when-not cue "empty ") (when (seq active) "active ")
                  (when (some #(= "releasing" (:phase %)) active) "releasing ")
                  (when queued? "queued ")
                  (when (and (:edit? ui) (not recent?)) "editing"))
      :style (when cue (if preview {"--cue-color" (widgets/color-css preview)} (color-style cue)))
      :data-preview (when preview "live") :data-x x :data-y y :data-cue-id (or cue-id "")
      :disabled (not (get-in db [:connection :connected?]))
      :aria-label (str (if cue (display-name cue) "Empty cell") ", column " (inc x) ", row " (inc y)
                       (when queued? ", queued"))
      :aria-pressed (boolean (seq active)) :on-pointer-down down
      :on-pointer-move #(send! [:studio/gesture-move {:pointer-id (.-pointerId %) :client-x (.-clientX %) :client-y (.-clientY %)}])
      :on-pointer-up #(send! [:studio/gesture-up (.-pointerId %)])
      :on-pointer-cancel #(send! [:studio/gesture-cancel (.-pointerId %)])
      :on-lost-pointer-capture #(send! [:studio/gesture-cancel (.-pointerId %)])
      :on-context-menu (fn [event] (.preventDefault event)
                         (send! [:studio/gesture-cancel (.-pointerId event)])
                         (when-not recent? (send! [:studio/open-actions cell])))
      :on-key-down (fn [event]
                     (cond
                       (contains? #{"ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown"} (.-key event))
                       (let [[dx dy] (get {"ArrowLeft" [-1 0] "ArrowRight" [1 0]
                                          "ArrowUp" [0 -1] "ArrowDown" [0 1]} (.-key event))
                             selector (str "[data-x=\"" (max 0 (min 7 (+ x dx)))
                                           "\"][data-y=\"" (max 0 (min 7 (+ y dy))) "\"]")]
                         (.preventDefault event)
                         (when-let [target (.querySelector (.closest (.-currentTarget event) "#cue-grid") selector)]
                           (.focus target)))
                       (or (= "ContextMenu" (.-key event)) (and (= "F10" (.-key event)) (.-shiftKey event)))
                       (do (.preventDefault event) (when-not recent? (send! [:studio/open-actions cell])))
                       (and (#{" " "Enter"} (.-key event)) (not (.-repeat event)))
                       (do (.preventDefault event)
                           (send! [:studio/gesture-down (merge cell {:pointer-id "keyboard" :client-x 0 :client-y 0})]))))
      :on-key-up (fn [event] (when (#{" " "Enter"} (.-key event))
                                (.preventDefault event) (send! [:studio/gesture-up "keyboard"])))
      :on-blur #(send! [:studio/gesture-cancel "keyboard"])}
     (if cue
       [:<> [:span.cue-index (str (inc y) "." (inc x))] [:span.cue-dot]
        [:span.cue-name (display-name cue)] [:span.cue-category (if queued? "Queued" (or (:category cue) "CUE"))]]
       (when-not recent? "+"))]))

(defn queued-cues [db]
  (let [{:keys [cues pending-cue-launches]} (:show db)]
    (when (seq pending-cue-launches)
      [:section.queued-launches {:aria-label "Queued cue launches"}
       [:strong "Queued launches"]
       [:ul
        (for [{:keys [command-id cue-id due-beat] :as launch} pending-cue-launches
              :let [cue-name (or (some-> (find-id cues cue-id) display-name) (label cue-id))]]
          ^{:key command-id}
           [:li {:data-launch-id command-id}
           [:span (str cue-name " · beat " (numeric/number-label due-beat))]
           (when (events/owns-cue-launch? db launch)
             [:button.text-button {:type "button" :aria-label (str "Cancel queued " cue-name)
                                   :disabled (not (get-in db [:connection :connected?]))
                                   :on-click #(send! [:studio/cancel-queued-cue command-id])} "Cancel"])])]])))

(defn- cue-grid [db]
  (let [show (:show db) ui (:ui db)
        authored-pages (:pages show)
        recent-page (:recent-page show)
        pages (cond-> (vec authored-pages) recent-page (conj recent-page))
        selected-recent (events/selected-recent-page db)
        page (or selected-recent (find-id authored-pages (:page-id ui)) (first authored-pages))
        rows (performance/visible-cue-rows page (or selected-recent (:edit? ui) (:full-grid? ui)))]
    [:section.cue-panel.panel {:aria-label "Cue performance grid"}
     [:div.panel-top
      [:div#page-tabs.page-tabs {:aria-label "Cue pages"}
       (for [item pages]
         ^{:key (pr-str (:id item))} [:button.page-tab {:class (when (= (:id item) (:id page)) "selected")
                                             :on-click #(send! [:studio/set-page (:id item)])} (display-name item)])]
      [:button#add-page.icon-button {:title "Create cue page" :aria-label "Create cue page" :on-click #(send! [:studio/add-page])} "+"]]
     [:div.grid-toolbar [:span#cue-count.muted.small (str (count (:cells page)) " cues / 64 slots")]
      [:div#launch-modes.segmented {:role "group" :aria-label "Cue launch behavior"}
       (for [[mode title] [["toggle" "Toggle"] ["one-shot" "One-shot"] ["held" "Hold"]]]
         ^{:key mode} [:button {:data-mode mode :class (when (= mode (:launch-mode ui)) "selected")
                                :aria-pressed (= mode (:launch-mode ui))
                                :on-click #(send! [:studio/set-launch mode])} title])]
      [:label.launch-grid {:for "launch-grid"}
       [:span "Launch grid"]
       [:select#launch-grid {:value (:launch-grid ui "off") :on-change #(set-ui! [:launch-grid] (input-value %))}
        (for [[choice title] events/launch-grids]
          ^{:key choice} [:option {:value choice} title])]]
      (when-not selected-recent
        [:button#edit-grid.edit-button {:aria-pressed (boolean (:edit? ui)) :on-click #(send! [:studio/toggle-edit])} "Edit grid"])]
     [:div.cue-grid-scroll
      [pad-preview/grid (:id page) rows (and (not selected-recent) (= :perform (:view ui))
                                             (get-in db [:connection :connected?]))
       cue-cell db page]]
     [queued-cues db]
     [:div.grid-footer [:span#gesture-hint (cond (and (:edit? ui) (not selected-recent)) "Choose a cell to edit its actions."
                                               (and (= "held" (:launch-mode ui)) (not= "off" (:launch-grid ui)))
                                               "Hold through the grid boundary to play. Release to cancel or fade out."
                                               (= "held" (:launch-mode ui)) "Hold a cue to play it. Release to fade it out."
                                               selected-recent "Tap to play."
                                               :else "Tap to play. Long-press for cue actions.")]
      (when-not selected-recent
        [:button.text-button {:type "button" :aria-pressed (boolean (or (:edit? ui) (:full-grid? ui))) :disabled (boolean (:edit? ui))
                             :on-click #(set-ui! [:full-grid?] (not (:full-grid? ui)))}
         (cond (:edit? ui) "64 slots while editing" (= rows 8) "Compact grid" :else "All 64 slots")])
      [button "end-all" "Release all" [:studio/end-all] "text-button"]]]))

(defn- parameter-control [instance parameter schema & [handlers options]]
  (let [value (get-in instance [:params parameter] (:default schema))
        choices (:choices schema)
        type (cond (waveform/waveform-schema? schema) "waveform" (seq choices) "choice" :else (:type schema "number"))
        change! (or (:change handlers) #(input! [:studio/param-set (:id instance) parameter %]))
        component! (or (:component handlers) #(input! [:studio/param-component (:id instance) parameter %1 %2]))
        source (when-not handlers (sources/reading instance parameter))
        overlay (if (and source (some? (:effective-value source))) (:effective-value source)
                  (when-not handlers @(rf/subscribe [:performance/parameter-overlay (:id instance) parameter])))
        bounds (:bounds schema) title (or (:name schema) (label parameter))
        fader? (and (:strip? options) (#{:fader "fader"} (:ui/widget schema)))]
    [:div.param-row {:data-param (name parameter) :class (when (:strip? options) "strip-control")}
     [:label [:span title] [:output (if (vector? value) (str/join ", " (map #(numeric/number-label schema %) value)) (widgets/value-label schema value))]]
     (case type
       "waveform" [waveform/picker (:id instance) parameter schema value change!
                   {:params (:params instance) :live? (nil? handlers)}]
       "choice" (let [selected (or (first (keep-indexed #(when (= value (:value %2)) %1) choices)) 0)]
                  [:select {:aria-label title :value selected :on-change #(change! (:value (get choices (js/parseInt (input-value %) 10))))}
                   (for [[index choice] (map-indexed vector choices)]
                     ^{:key index} [:option {:value index} (or (:label choice) (label (:id choice)) (str (:value choice)))])])
       "boolean" [:input {:type "checkbox" :aria-label title :checked (boolean value) :on-change #(change! (checked? %))}]
       "enum" (let [choices (vec (or (:values schema) (:options schema)))
                    selected (or (first (keep-indexed #(when (= value %2) %1) choices)) 0)]
                [:select {:aria-label title :value selected :on-change #(change! (get choices (js/parseInt (input-value %) 10)))}
                 (for [[index item] (map-indexed vector choices)] ^{:key index} [:option {:value index} (label item)])])
       "color" [:input {:type "color" :aria-label title :value (hex-color (widgets/color-components value))
                         :on-change #(change! (cond-> (color-components (input-value %))
                                               (and (vector? value) (= 4 (count value))) (conj (nth value 3))))}]
       "vector" [widgets/vector-control title schema value component!]
       "range"
       [:div.param-vector
        (for [index (range (or (:size schema) (count value)))]
          ^{:key index} [numeric/number-input {:display-schema schema :type "number" :step "any" :value (get value index 0)
                                 :min (first bounds) :max (second bounds)
                                 :aria-label (str title " component " (inc index))
                                 :on-change #(when-let [n (number-value %)]
                                               (component! index n))}])]
       (if (widgets/numeric-range schema)
         [:div.parameter-number-layout {:data-widget (when (:strip? options) (if fader? "fader" "rotary"))}
          [widgets/rotary-control schema (or value 0) change!
           {:name (str title " rotary") :base-value value :effective-value overlay
            :strip? (:strip? options)
            :key-change (or (:key-change handlers) #(input! [:studio/param-key (:id instance) parameter schema %]))}]
          [:div.parameter-number-fields
           [numeric/number-input {:type "range" :aria-label title :value (or value 0)
                          :min (first bounds) :max (second bounds)
                          :step (if (= "integer" type) 1 (or (numeric/channel-step schema) (/ (- (second bounds) (first bounds)) 1000)))
                          :on-change #(when-let [n (number-value %)] (change! n))}]
           (when fader?
             [:span.strip-fader-bounds
              [:span (widgets/number-label (first bounds))]
              [:span (widgets/number-label (second bounds))]])
           (when-not (:compact? options)
             [numeric/number-input {:class "numeric-exact" :display-schema schema :type "number" :aria-label (str title " exact value") :value (or value 0)
                            :min (first bounds) :max (second bounds) :step (if (= "integer" type) 1 "any")
                            :on-change #(when-let [n (number-value %)] (change! n))}])]]
         [numeric/number-input {:display-schema schema :type "number" :aria-label title :value (or value 0) :step (if (= "integer" type) 1 "any")
                        :on-change #(when-let [n (number-value %)] (change! n))}]))
     (when (and (nil? handlers) (:id instance)) [sources/trigger (:id instance) parameter title source])
     (when (and (sources/driven? instance parameter) (not (:strip? options)) (#{"number" "integer" "vector" "color" "range"} type)
                (or (not (:compact? options)) (visualization/spatial-kind schema)))
       [sources/result-visual visualization/studio instance parameter schema])]))

(defn- preview-film-controls [settings]
  [:details.preview-film
   [:summary "Image"]
   (for [[key label high step default] [[:exposure "Exposure" 3 0.01 0.85]
                                         [:bloom "Bloom" 2 0.01 0.28]
                                         [:grain "Grain" 0.08 0.001 0.0015]]]
     ^{:key key}
     [:label label
      [:input {:type "range" :min 0 :max high :step step :value (get settings key default)
               :aria-label (str "Preview " label)
               :on-change #(when-let [v (number-value %)] (input! [:studio/preview-setting key v]))}]])])

(defn- preview-panel [db]
  [:section#preview-panel.preview-panel.panel {:aria-label "Live fixture preview"
                                              :class (when (get-in db [:ui :preview-expanded?]) "expanded")}
   [:div.panel-top [:div [:span.section-label "THE ROOM"] [:span#fixture-count.muted.small (str (count (get-in db [:show :rig :fixtures])) " fixtures")]]
    [:div.preview-size-controls
     (when (= :perform (get-in db [:ui :view])) [:<> [playback/control "preview"] [airplay/control "preview"]])
     [:button.text-button.preview-fullscreen-tools
      {:type "button" :aria-pressed (boolean (get-in db [:ui :fullscreen-preview-controls?]))
       :on-click #(set-ui! [:fullscreen-preview-controls?] (not (get-in db [:ui :fullscreen-preview-controls?])))} "Controls"]
     [:button.text-button.preview-expand-control {:type "button" :aria-pressed (boolean (get-in db [:ui :preview-expanded?]))
                            :on-click #(send! [:studio/preview-expand])} (if (get-in db [:ui :preview-expanded?]) "Compact" "Expand")]
     [:button.icon-button {:type "button" :aria-label "Fullscreen preview" :title "Fullscreen preview"
                            :on-click #(input! [:studio/preview-fullscreen])} "⛶"]]]
   [spatial-preview/scene (or (:server-presentation db) (:show db)) (get-in db [:server-projection :view :settings :spatial]) :perform]
   [camera-controls/controls]
   (when (get-in db [:ui :preview-controls?])
     [:div.preview-settings
      [:label "Work light"
       [:input {:type "range" :min 0 :max 1 :step 0.01 :aria-label "Preview work light"
                 :value (get-in db [:ui :preview-settings :worklight] 0.12)
                 :on-change #(when-let [value (number-value %)] (input! [:studio/preview-setting :worklight value]))}]]
      [:select {:aria-label "Preview quality" :value (get-in db [:ui :preview-settings :quality] "auto")
                 :on-change #(input! [:studio/preview-setting :quality (input-value %)])}
       [:option {:value "auto"} "Auto quality"] [:option {:value "mobile"} "Mobile"]
       [:option {:value "desktop"} "Desktop"] [:option {:value "high"} "High"]]
      [:label "Fixture updates"
       [:select {:aria-label "Preview fixture updates" :value (get-in db [:ui :preview-settings :lighting-mode]
                                                        (:lighting-mode events/default-preview-settings))
                 :on-change #(input! [:studio/preview-setting :lighting-mode (input-value %)])}
        [:option {:value "hold"} "Follow show frames"]
        [:option {:value "interpolate"} "Smooth · one frame delay"]]]
      [:select {:aria-label "Preview camera" :value (get-in db [:ui :preview-settings :camera] "audience")
                 :on-change #(input! [:studio/preview-setting :camera (input-value %)])}
       [:option {:value "audience"} "Audience"] [:option {:value "overview"} "Overview"]]
      [preview-film-controls (get-in db [:ui :preview-settings])]])
   [:div.fog-controls [:label {:html-for "fog-density"} "Atmosphere "
                        [:span.muted
                         (if-let [air (get-in db [:show :output :atmosphere])]
                           (let [flow (* 60 (reduce + 0 (keep :flow-m3-s (:sources air))))]
                             (str "DMX · " (.toFixed flow 1) " m³/min"))
                           "preview")]]
    [:input#fog-density {:type "range" :aria-label "Preview fog density" :min 0 :max 1 :step 0.01
                         :value (or (get-in db [:ui :fog]) (if (get-in db [:show :output :atmosphere]) 1 0.3))
                         :on-change #(when-let [value (number-value %)] (send! [:studio/preview-fog value]))}]
    (when-not (get-in db [:show :output :atmosphere])
      [button "fog-burst" "+ Plume" [:studio/preview-burst] "text-button"])
    [:button#reset-camera.icon-button {:aria-label "Reset preview camera" :title "Reset preview camera"
                                       :on-click #(send! [:studio/preview-reset])} "⟳"]]])

(defn- auto-panel [db]
  (let [form (get-in db [:ui :auto-form])
        {:keys [enabled? target]} (events/auto-activity db)
        percent (js/Math.round (* 100 target))]
    [:details.panel.auto-panel
     [:summary [:span.section-label "LET IT WANDER"] [:span#auto-status.muted.small (if (get-in db [:show :auto :enabled?]) "Auto mode · on" "Auto mode · off")]]
     [:div.form-body [:p.small.muted "Let the show choose cues at an interval. Movement cues are paired with light when the rig needs it."]
      [:div.form-row
       [duration-control "auto-interval" "Every" (:interval-value form 8) (:interval-unit form "beats")
        (fn [value unit] (when (some? value) (set-ui! [:auto-form :interval-value] value))
          (when unit (set-ui! [:auto-form :interval-unit] unit)))]
       [:label "Cues per step" [numeric/number-input {:id "auto-count" :type "number" :min 1 :max 8 :value (:count form 1)
                                                 :on-change #(when-let [n (number-value %)] (set-ui! [:auto-form :count] n))}]]]
      [:div.form-row
       [:label "Choose from" [:select#auto-scope {:value (:scope form "page") :on-change #(set-ui! [:auto-form :scope] (input-value %))}
                              [:option {:value "page"} "Current page"] [:option {:value "all"} "Every page"]]]
       [:label "Launch as" [:select#auto-mode {:value (:mode form "timed") :on-change #(set-ui! [:auto-form :mode] (input-value %))}
                            [:option {:value "timed"} "Timed"] [:option {:value "one-shot"} "One-shot"] [:option {:value "persistent"} "Persistent"]]]]
      [duration-control "auto-duration" "Cue duration" (:duration-value form 4) (:duration-unit form "beats")
       (fn [value unit] (when (some? value) (set-ui! [:auto-form :duration-value] value))
         (when unit (set-ui! [:auto-form :duration-unit] unit)))]
      [:label.policy-toggle [:input {:type "checkbox" :checked enabled?
                                    :on-change #(set-ui! [:auto-form :target-activity?] (checked? %))}]
       "Choose cues by visual activity"]
      (when enabled?
        [:label.scene-target
         [:span "Target visual activity" [:output (str percent "%")]]
         [:input {:type "range" :min 0 :max 100 :step 1 :value percent
                  :aria-label "Target visual activity" :aria-valuetext (str percent "%")
                  :on-change #(set-ui! [:auto-form :activity-target] (/ (js/Number (input-value %)) 100))}]])
      [:div.button-row [button "auto-start" "Start auto" [:studio/auto-start] "button primary"] [button "auto-stop" "Stop auto" [:studio/auto-stop]]]
      [:p.small.muted "Persistent cues remain active until released. You can keep playing the grid while auto mode runs."]]]))

(defn- response-panel [db]
  (let [form (get-in db [:ui :response-form]) responses (get-in db [:show :responses])]
    [:details.panel.auto-panel.response-panel
     [:summary [:span.section-label "SHAPE THE RESPONSE"] [:span.muted.small (str (count responses) " envelopes")]]
     [:div.form-body
      [:p.small.muted "Fade numeric parameters across the show or the selected effect. Original knob values stay intact."]
      [:div.form-row
       [:label "Name" [:input#response-id {:value (:id form) :on-change #(set-ui! [:response-form :id] (input-value %))}]]
       [:label "Parameters" [:input#response-params {:value (:params form) :placeholder "level, speed"
                                                    :on-change #(set-ui! [:response-form :params] (input-value %))}]]]
      [:div.form-row
       [:label "Apply to" [:select#response-scope {:value (:scope form) :on-change #(set-ui! [:response-form :scope] (input-value %))}
                           [:option {:value "all"} "All effects"] [:option {:value "effect"} "Selected effect type"]
                           [:option {:value "instance"} "Selected instance"]]]
       [:label "Curve" [:select#response-curve {:value (:curve form) :on-change #(set-ui! [:response-form :curve] (input-value %))}
                        [:option {:value "linear"} "Linear"] [:option {:value "smoothstep"} "Smooth"]
                        [:option {:value "ease-in"} "Ease in"] [:option {:value "ease-out"} "Ease out"]]]]
      [duration-control "response-attack" "Attack" (:attack-value form) (:unit form)
       (fn [value unit] (when (some? value) (set-ui! [:response-form :attack-value] value))
         (when unit (set-ui! [:response-form :unit] unit))) true]
      [duration-control "response-release" "Release" (:release-value form) (:unit form)
       (fn [value unit] (when (some? value) (set-ui! [:response-form :release-value] value))
         (when unit (set-ui! [:response-form :unit] unit))) true]
      [:div.button-row [button "response-start" "Start / retrigger" [:studio/response-start] "button primary"]]
      (for [response responses]
        ^{:key (:id response)}
        [:div.list-card {:data-response-id (:id response)}
         [:div [:strong (label (:id response))]
          [:p.small.muted (if (or (get-in response [:state :released-at-beat]) (get-in response [:state :released-at-ms]))
                           "Releasing / held at zero" "Attack / sustain")]]
         [:div.button-row [button nil "Release" [:studio/response-action :release (:id response)]]
          [button nil "Remove" [:studio/response-action :delete (:id response)] "text-button"]]])
      [:p.small.muted "Attack and release share one unit. Release fades to zero and holds there; remove the envelope to restore the unaffected parameters."]]]))

(defn- section-pattern-panel [db]
  (when-let [{:keys [override? pending? connected?]} (events/section-pattern-control db)]
    [:section.panel {:aria-label "Section patterns"}
     [:div.panel-top
      [:div [:span.section-label "SECTION PATTERNS"]
       [:span.small.muted {:role "status"} (if override? "Override active" "Override off")]]
      [:button.button {:type "button" :disabled (or pending? (not connected?))
                       :aria-label (if override? "Resume automatic patterns" "Suspend automatic patterns")
                       :on-click #(send! [:studio/section-pattern-override (not override?)])}
       (cond pending? "Applying…" override? "Resume" :else "Suspend")]]]))

(defn- perform-view [db]
  (let [section (get-in db [:ui :performance-section] "cues")
        count-active (count (get-in db [:show :instances]))]
    [:section#view-perform.view {:hidden (not= :perform (get-in db [:ui :view]))
                                :data-section section}
     [:div.perform-heading [:h1 "Perform"]
      [:span.small.muted (str count-active " running")]
      [:div.perform-section-tabs {:role "group" :aria-label "Performance sections"}
       (for [[id title] [["cues" "Cues"] ["live" (str "Live · " count-active)] ["room" "Room"]]]
         ^{:key id} [:button {:type "button" :aria-pressed (= id section)
                              :on-click #(set-ui! [:performance-section] id)} title])]]
     [:div.performance-layout {:class (when (get-in db [:ui :preview-expanded?]) "preview-expanded")}
      [:div.performance-launch [cue-grid db]]
      [:div.performance-live [section-pattern-panel db] [performance/board db parameter-control]
       [autopilot/panel] [lighting-scenes/panel] [lighting-policy/panel] [autopilot/hdr-panel]]
      [:div.performance-room [preview-panel db]]
      [:div.performance-tools [auto-panel db] [response-panel db] [modulation/panel db]]]]))

(defn- slot-parameter-editor [db index slot]
  (let [cue (find-id (get-in db [:show :cues]) (:cue-id slot))
        definition (find-id (get-in db [:show :effects]) (:effect-id cue))
        instance {:id (str "slot-" index) :params (events/slot-parameters db index)}]
    [:details.slot-parameter-editor
     [:summary (str "Parameters" (when (seq (:params slot)) (str " · " (count (:params slot)))))]
     (if (seq (:params definition))
       [:<>
        (for [[parameter schema] (:params definition)]
          ^{:key parameter}
          [parameter-control instance parameter (assoc schema :name (str "Step " (inc index) " · " (or (:name schema) (label parameter))))
           {:change #(input! [:studio/slot-param index parameter %])
            :key-change #(input! [:studio/slot-param-key index parameter schema %])
            :component #(input! [:studio/slot-param-component index parameter %1 %2])}])
        [button nil "Reset to cue defaults" [:studio/slot-params-reset index] "text-button"]]
       [:p.small.muted "This cue has no adjustable parameters."])]))

(defn- composition-catalog [items]
  (when (seq items)
    [:div#composition-library.compose-catalog {:role "group" :aria-label "Saved compositions"}
     (for [item items]
       ^{:key (:id item)}
       [:div.list-card {:data-composition-id (:id item)}
        [:div [:strong (display-name item)]
         [:p (str (label (:mode item)) " · " (count (:slots item)) " slots" (when (:running? item) " · playing"))]]
        [button nil "Open" [:studio/composition-open item]]])]))

(defn- composition-settings [composition macro?]
  [:div.compose-settings
   [:label "Name" [:input#composition-name {:value (:name composition "") :max-length 80
                                            :on-change #(set-ui! [:composition :name] (input-value %))}]]
   [:label "Identifier" [:input#composition-id {:value (:id composition "") :on-change #(set-ui! [:composition :id] (input-value %))}]]
   [:label "Behavior" [:select#composition-mode {:value (:mode composition "sequence") :on-change #(set-ui! [:composition :mode] (input-value %))}
                       [:option {:value "sequence"} "Step sequence"] [:option {:value "macro"} "Macro · together"]]]
   (when-not macro?
     [:label "Playback" [:select#composition-loop {:value (:loop composition "once") :on-change #(set-ui! [:composition :loop] (input-value %))}
                         [:option {:value "once"} "Once"] [:option {:value "loop"} "Loop"] [:option {:value "ping-pong"} "Back and forth"] [:option {:value "random"} "Random"]]])
   [layers/opacity-control (:opacity composition)]])

(defn- slot-actions [index last?]
  [:div.slot-actions
   (for [[caption title offset] [["↑" "Move earlier" -1] ["↓" "Move later" 1] ["×" "Remove slot" 0]]]
     ^{:key offset} [:button.icon-button {:aria-label (str title ": step " (inc index)) :title title
                                          :disabled (or (and (= offset -1) (zero? index)) (and (= offset 1) last?))
                                          :on-click #(send! (if (zero? offset) [:studio/composition-remove index]
                                                             [:studio/composition-move index offset]))} caption])])

(defn- composition-slot [db index slot macro? last?]
  (let [[duration-value duration-unit] (slot-duration slot)]
    [:li.composition-slot {:class (when macro? "macro-slot")}
     [:span.slot-number (inc index)]
     [:strong.slot-title (display-name (find-id (get-in db [:show :cues]) (:cue-id slot)))]
     (when-not macro?
       [duration-control (str "slot-duration-" index) (str "Step " (inc index) " duration") duration-value duration-unit
        (fn [value unit] (input! [:studio/slot-duration index value unit]))])
     [slot-actions index last?]
     [:div.slot-editors [slot-parameter-editor db index slot] [layers/slot-controls index slot]]]))

(defn- composition-view [db]
  (let [show (:show db) composition (get-in db [:ui :composition]) slots (:slots composition [])
        selected-cue (or (get-in db [:ui :slot-cue]) (:id (first (:cues show))))
        macro? (= "macro" (:mode composition))]
    [:section#view-compose.view {:hidden (not= :compose (get-in db [:ui :view]))}
     [heading "Macros & sequences"]
     [:div.compose-layout
      [:section.panel.composition-editor
       [:div.panel-top [:span.section-label "Composition"] [button "new-composition" "New +" [:studio/composition-new] "text-button"]]
       [composition-catalog (:compositions show)]
       [:div.form-body
        [composition-settings composition macro?]
        [:div.slots-heading
         [:h2 (if macro? "Cue slots" "Steps")] [:span.count (count slots)]
         [:div.form-row.add-slot
          [:select#slot-cue {:aria-label "Cue to add" :value (or selected-cue "") :on-change #(set-ui! [:slot-cue] (input-value %))}
           (for [cue (:cues show)] ^{:key (:id cue)} [:option {:value (:id cue)} (display-name cue)])]
          [button "add-slot" "Add cue +" [:studio/composition-add]]]]
        [:ol#composition-slots.composition-slots
         (if (seq slots)
           (for [[index slot] (map-indexed vector slots)]
             ^{:key (str index "/" (:cue-id slot))} [composition-slot db index slot macro? (= index (dec (count slots)))])
           [:li.empty-state "Choose a cue to add the first slot."])]
        [:div.button-row [button "save-composition" "Save composition" [:studio/composition-save false] "button primary"]
         [button "run-composition" "Save & play ▷" [:studio/composition-save true]] [button "stop-composition" "Stop" [:studio/composition-stop]]]]]
      (when (= :compose (get-in db [:ui :view])) [variable-blocks/panel])]]))

(defn- plan-point [svg event width depth]
  (let [rect (.getBoundingClientRect svg)]
    [(- (* width (/ (- (.-clientX event) (.-left rect)) (.-width rect))) (/ width 2))
     (- (* depth (/ (- (.-clientY event) (.-top rect)) (.-height rect))) (/ depth 2))]))

(defn- rig-plan [rig]
  (let [width (max 1 (get-in rig [:room :width] 12)) depth (max 1 (get-in rig [:room :depth] 10))
        interaction
        (fn [kind index item]
          {:tab-index 0 :role "button" :aria-label (str "Move " (display-name item) " in the rig plan")
           :data-rig-kind (name kind) :data-rig-id (:id item)
           :on-pointer-down (fn [event]
                              (.preventDefault event)
                              (let [svg (.-ownerSVGElement (.-currentTarget event))
                                    [x z] (plan-point svg event width depth)]
                                (.setPointerCapture svg (.-pointerId event))
                                (input! [:studio/rig-drag-start kind index (.-pointerId event) x z])))
           :on-key-down (fn [event]
                          (when-let [[dx dz] (get {"ArrowLeft" [-1 0] "ArrowRight" [1 0] "ArrowUp" [0 -1] "ArrowDown" [0 1]} (.-key event))]
                            (.preventDefault event)
                            (let [step (if (.-shiftKey event) 1 0.1)]
                              (input! [:studio/rig-nudge kind index (* step dx) (* step dz)]))))})]
    [:section.panel.rig-plan-panel
     [:div.panel-top [:span.section-label "ROOM PLAN"] [:span.small.muted "TOP VIEW · METRES"]]
     [:div.rig-plan-wrap
      [:svg#rig-plan {:view-box (str (- (/ width 2)) " " (- (/ depth 2)) " " width " " depth)
                      :preserve-aspect-ratio "none" :style {:aspect-ratio (str width " / " depth)}
                      :role "group" :aria-label "Graphical rig placement" :aria-describedby "rig-plan-help"
                      :on-pointer-move (fn [event] (let [[x z] (plan-point (.-currentTarget event) event width depth)]
                                                    (input! [:studio/rig-drag-move (.-pointerId event) x z])))
                      :on-pointer-up #(input! [:studio/rig-drag-end (.-pointerId %)])
                      :on-pointer-cancel #(input! [:studio/rig-drag-end (.-pointerId %)])
                      :on-lost-pointer-capture #(input! [:studio/rig-drag-end (.-pointerId %)])}
       [:defs [:pattern#rig-plan-grid {:width 1 :height 1 :pattern-units "userSpaceOnUse"}
               [:path {:d "M 1 0 L 0 0 0 1" :fill "none" :stroke "#35412e" :stroke-width 0.015}]]]
       [:rect {:x (- (/ width 2)) :y (- (/ depth 2)) :width width :height depth :fill "url(#rig-plan-grid)"}]
       [:path {:d (str "M " (- (/ width 2)) " 0 H " (/ width 2) " M 0 " (- (/ depth 2)) " V " (/ depth 2))
               :stroke "#56664b" :stroke-width 0.025 :stroke-dasharray "0.1 0.15"}]
       (for [[index truss] (map-indexed vector (:trusses rig))
             :let [[x _ z] (:position truss [0 3 0])
                   angle (* (/ js/Math.PI 180) (get-in truss [:rotation-deg 1] 0))
                   dx (* (/ (:length truss 4) 2) (js/Math.cos angle)) dz (- (* (/ (:length truss 4) 2) (js/Math.sin angle)))]]
         ^{:key (:id truss)}
         [:g.rig-plan-truss (interaction :trusses index truss)
          [:line.rig-plan-hit {:x1 (- x dx) :y1 (- z dz) :x2 (+ x dx) :y2 (+ z dz)}]
          [:line.rig-plan-beam {:x1 (- x dx) :y1 (- z dz) :x2 (+ x dx) :y2 (+ z dz)}]
          [:text {:x x :y (- z 0.25) :text-anchor "middle"} (display-name truss)]])
       (for [[index fixture] (take 128 (remove (comp :pixel-layout-id second) (map-indexed vector (:fixtures rig))))
             :let [[x _ z] (:position fixture [0 3 0])]]
         ^{:key (:id fixture)}
         [:g.rig-plan-fixture (interaction :fixtures index fixture)
          [:circle.rig-plan-hit {:cx x :cy z :r 0.34}]
          [:circle.rig-plan-dot {:cx x :cy z :r 0.12}]
          [:text {:x x :y (+ z 0.4) :text-anchor "middle"} (str (inc index))]])]]
     [:p#rig-plan-help.small.muted "Drag fixtures and trusses to position them. Arrow keys move 0.1 m; Shift moves 1 m. Height stays unchanged. Save rig to apply."]
     (when (> (count (:fixtures rig)) 128) [:p.small.muted "The plan shows up to 128 fixtures. Use the 3D editor to inspect pixel layouts."])]))

(defn- truss-editor [rig]
  [:section.panel.truss-editor
   [:div.panel-top [:span.section-label "TRUSSES"] [button "add-truss" "Add truss +" [:studio/truss-add] "text-button"]]
   [:div.form-body
    (if (seq (:trusses rig))
      (for [[index truss] (map-indexed vector (:trusses rig))]
        ^{:key (:id truss)} [:div.fixture-card.truss-card
         [:div.fixture-card-head
          [:input {:aria-label (str "Truss " (inc index) " name") :value (or (:name truss) (label (:id truss)))
                   :on-change #(input! [:studio/rig-set [:trusses index :name] (input-value %)])}]
          [:button.icon-button {:aria-label (str "Remove truss " (display-name truss))
                                :on-click #(send! [:studio/truss-remove index])} "×"]]
         [:div.fixture-fields
          (for [[title path value] [["X · m" [:position 0] (get-in truss [:position 0] 0)]
                                   ["Height · m" [:position 1] (get-in truss [:position 1] 3)]
                                   ["Z · m" [:position 2] (get-in truss [:position 2] 0)]
                                   ["Length · m" [:length] (:length truss 4)]
                                   ["Angle · °" [:rotation-deg 1] (get-in truss [:rotation-deg 1] 0)]]]
            ^{:key path} [:label title
                          [numeric/number-input {:type "number" :step 0.1 :min (when (= path [:length]) 0.1) :value value
                                         :on-change #(when-let [n (number-value %)]
                                                       (input! [:studio/truss-set index path n]))}]])]])
      [:p.small.muted "Add a truss, set its height and length, then place it in the plan."])]] )

(defn- rig-view [db]
  (let [rig (get-in db [:ui :rig-draft])]
    [:section#view-rig.view {:hidden (not= :rig (get-in db [:ui :view]))}
     [heading "Rig & space"
      [button "save-rig" "Save rig" [:studio/rig-save] "button primary"]]
     [rig-editor/panel db]
     (when (= :rig (get-in db [:ui :view])) [spatial/panel])
     [:details.rig-plan-alternative [:summary "Top-down placement alternative"] [rig-plan rig]]
     [:div.rig-layout
      [:section.panel [:div.panel-top [:span.section-label "ROOM & FIXTURES"] [button "add-fixture" "Add RGBW head +" [:studio/rig-add] "text-button"]]
       [:div.form-body
        [:div.form-row.room-dimensions
         (for [[dimension title] [[:width "Width · m"] [:height "Height · m"] [:depth "Depth · m"]]]
           ^{:key dimension} [:label title [numeric/number-input {:id (str "room-" (name dimension)) :type "number" :min 1 :max 1000 :step 0.1
                                                   :value (get-in rig [:room dimension] 1)
                                                   :on-change #(when-let [n (number-value %)] (input! [:studio/rig-set [:room dimension] n]))}]])]
        (when-let [measurement (get-in rig [:room :measured-fog])]
          [fog-fields/measured-clearance measurement])
        [:fieldset.form-row.room-dimensions {:disabled (some? (get-in rig [:room :measured-fog]))}
         [:legend (if (get-in rig [:room :measured-fog]) "Configured fallback" "Room atmosphere")]
         (for [[field title default minimum maximum]
               [[:air-changes-per-hour "Ventilation (air changes/h)" 0 0 3600]
                [:fog-half-life-s "Fog deposition half-life (s)" 180 0 nil]]]
           ^{:key field}
           [:label {:for (name field)} title
            [numeric/number-input {:id (name field) :type "number" :step "any"
                           :min minimum :max maximum :placeholder (str default)
                           :value (get-in rig [:room field] "")
                           :on-change (fn [e]
                                        (let [raw (input-value e)]
                                          (if (empty? raw) (input! [:studio/rig-atmosphere field nil])
                                            (when-let [n (number-value e)] (input! [:studio/rig-atmosphere field n])))))}]])]
        (when (seq (:pixel-layouts rig))
          [:p.small.muted "Use Pixel layouts above to edit strips, grids and individual pixels."])
        [:div#fixture-list.fixture-list
         (for [[index fixture] (map-indexed vector (:fixtures rig)) :when (nil? (:pixel-layout-id fixture))]
           ^{:key (:id fixture)} [:div.fixture-card
            [:div.fixture-card-head
             [:input {:aria-label (str "Fixture " (inc index) " name") :value (or (:name fixture) (:id fixture))
                      :on-change #(input! [:studio/rig-set [:fixtures index :name] (input-value %)])}]
             [:span.fixture-id (:id fixture)]
             [:button.icon-button {:aria-label (str "Remove fixture " (display-name fixture))
                                    :on-click #(send! [:studio/rig-remove index])} "×"]]
            [:div.fixture-fields
             (for [[title key] [["X · m" 0] ["Y · m" 1] ["Z · m" 2] ["Universe" :universe] ["Address" :address]]]
               ^{:key key} [:label title
                            [numeric/number-input {:type "number" :step (if (number? key) 0.1 1) :min (when (keyword? key) 1) :max (when (= key :address) 512)
                                     :value (if (number? key) (get-in fixture [:position key] 0) (get fixture key))
                                     :on-change #(when-let [n (number-value %)]
                                                   (input! [:studio/rig-set (if (number? key) [:fixtures index :position key] [:fixtures index key]) n]))}]])]])]]]
      [:section.panel.rig-help [:div.panel-top [:span.section-label "A SHARED SPACE"]]
       [:div.form-body [:div.rig-diagram {:aria-hidden true} [:span.rig-axis-y "Y ↑"] [:div.rig-cube "✧" [:span "✧"] "✧" [:span "✧"]] [:span.rig-axis-x "X →"] [:span.rig-axis-z "↙ Z"]]
        [:h2 "One rig, every view."] [:p "Saving updates the runtime fixture layout and the same live preview used while performing."]
        [:p.muted "X runs left to right, Y is height, Z is depth. Positions use metres. DMX addresses start at 1. Overlapping addresses are rejected by the server."]
        [:p.muted.small "Existing fixture channel maps are preserved when you edit positions."]
        [button "rig-to-perform" "Open live preview ↗" [:studio/set-view :perform]]]]]
     [truss-editor rig]
     [fixture-library/panel db]
     [rdm/panel]]))

(defn- visual-builder [db]
  (let [builder (get-in db [:ui :builder])]
    [:div#visual-editor {:hidden (not= :form (get-in db [:ui :definition :mode]))}
     [:div.form-row
      [:label "Identifier" [:input#builder-id {:value (:id builder "") :pattern "[a-zA-Z][a-zA-Z0-9._/-]*"
                                              :on-change #(set-ui! [:builder :id] (input-value %))}]]
      [:label "Name" [:input#builder-name {:value (:name builder "") :on-change #(set-ui! [:builder :name] (input-value %))}]]]
     [:div.form-row
      [:label "Color" [:input#builder-color {:type "color" :value (:color builder "#b981a3") :on-change #(set-ui! [:builder :color] (input-value %))}]]
      [:label "Intensity" [numeric/number-input {:id "builder-level" :type "number" :min 0 :max 1 :step 0.01 :value (:level builder 0.7)
                                               :on-change #(when-let [n (number-value %)] (set-ui! [:builder :level] n))}]]
      [duration-control "builder-duration" "Duration" (:duration-value builder 4) (:duration-unit builder "beats")
       (fn [value unit] (when (some? value) (set-ui! [:builder :duration-value] value))
         (when unit (set-ui! [:builder :duration-unit] unit)))]]
     [duration-control "builder-release" "Release" (:release-value builder 1) (:release-unit builder "beats")
      (fn [value unit] (when (some? value) (set-ui! [:builder :release-value] value))
        (when unit (set-ui! [:builder :release-unit] unit))) true]
     [keyframes/editor (:keyframes builder) [:level] #(set-ui! [:builder :keyframes] %)]
     (when (and (= :library (get-in db [:ui :view])) (= :form (get-in db [:ui :definition :mode])))
       [block-builder/field {:id (some-> builder :id not-empty keyword) :params {:level {}}
                             :outputs {:color nil :dimmer nil}}
        (or (:blocks builder) {}) #(set-ui! [:builder :blocks] %)])
     [:label.checkbox-label [:input#builder-cue {:type "checkbox" :checked (boolean (:cue? builder)) :on-change #(set-ui! [:builder :cue?] (checked? %))}]
      "Create matching cue"]
     [button "builder-preview" "Generate EDN ↗" [:studio/builder-generate]]]))

(defn- registry-results [show search]
  (let [query (str/lower-case search)
        matches? (fn [[_ item]] (str/includes? (str/lower-case (str (:name item) " " (:id item))) query))]
    (->> (concat (map #(vector "effects" %) (:effects show)) (map #(vector "cues" %) (:cues show)))
         (filter matches?))))

(defn- registry-option [definition kind item]
  (let [selected? (and (= kind (:kind definition)) (= (:id item) (:loaded-id definition)))]
    [:button.definition-item {:class (when selected? "selected") :aria-current (when selected? "true")
                              :style (color-style item) :on-click #(send! [:studio/definition-load kind (:id item)])}
     [:span.definition-swatch] [:span [:strong (display-name item)]
                                [:small (str (if (= kind "effects") "Effect" "Cue") " · v" (:version item 1))]]]))

(defn- definition-registry [catalog selected search]
  (let [results (registry-results catalog search)]
    [:aside.panel.definition-library {:aria-label "Definition registry"}
     [:div.registry-toolbar
      [:label.search-label [:input#definition-search {:type "search" :placeholder "Search registry" :aria-label "Search definitions"
                                                     :value search :on-change #(set-ui! [:definition-search] (input-value %))}]]
      [:button#new-definition.text-button {:aria-label "New definition" :on-click #(send! [:studio/definition-new])} "New"]]
     [:div#definition-list.definition-list
      (if (seq results)
        (for [[kind item] results]
          ^{:key (str kind "/" (:id item))} [registry-option selected kind item])
        [:p.registry-empty {:role "status"} "No matching definitions."])]]))

(defn- definition-toolbar [definition]
  [:<>
   [:div.definition-toolbar
    [:div#editor-modes.segmented {:role "group" :aria-label "Editing mode"}
     (for [[mode title] [[:source "EDN source"] [:form "Visual builder"]]]
       (let [selected? (= mode (:mode definition))]
         ^{:key mode} [:button {:class (when selected? "selected") :data-editor-mode (name mode) :aria-pressed selected?
                                :on-click #(set-ui! [:definition :mode] mode)} title]))]
    [:button#publish-definition.button.primary {:disabled (boolean (:publishing? definition))
                                                 :on-click #(send! [:studio/definition-publish])} "Validate & publish"]]
   [:div.definition-settings
    [:label "Definition type" [:select#definition-kind {:value (:kind definition "effects")
                                                        :on-change #(set-ui! [:definition :kind] (input-value %))}
                               [:option {:value "effects"} "Effect"] [:option {:value "cues"} "Cue"]]]
    [:label "Editor" [:select#editor-engine {:value (name (:engine definition :textarea)) :disabled (= :form (:mode definition))
                                             :on-change #(send! [:studio/editor-engine (keyword (input-value %))])}
                      [:option {:value "textarea"} "Text editor"] [:option {:value "neovim"} "Neovim · WASM"]]]
    [:label.auto-publish [:input#auto-publish {:type "checkbox" :checked (boolean (:auto? definition))
                                                             :on-change #(set-ui! [:definition :auto?] (checked? %))}]
     "Publish valid edits"]]
   [:div.definition-feedback
    [:span#definition-status.small.muted (:status definition "Ready to create")]
    [:div#validation-result.validation-result {:class (when (:error? definition) "error") :role "status" :aria-live "polite"}
     (:result definition "")]]])

(defn- definition-view [db]
  (let [show (:show db) ui (:ui db) definition (:definition ui)]
    [:section#view-library.view {:hidden (not= :library (:view ui))}
     [heading "Effect & cue definitions"
      [button "generate-registry-page" "Build registry page" [:studio/generate-page]]]
     (when (= :library (:view ui))
       ^{:key [(:runtime-id show) (:show-id show) (:epoch show)]} [materials/panel])
     [:div.definition-layout
      [definition-registry (select-keys show [:effects :cues])
       (select-keys definition [:kind :loaded-id]) (:definition-search ui "")]
      [:section.panel.definition-editor
       [definition-toolbar definition]
       [:div.form-body
        [:div#source-editor {:hidden (= :form (:mode definition))}
         (if (= :neovim (:engine definition))
           [interop/neovim definition]
           [:<> [:label.sr-only {:html-for "definition-source"} "Declarative EDN definition"]
            [:textarea#definition-source {:spell-check false :value (:source definition "") :aria-describedby "source-help"
                                           :on-change #(input! [:studio/definition-source (input-value %)])
                                           :on-key-down (fn [event]
                                                          (when (and (or (.-ctrlKey event) (.-metaKey event)) (= "Enter" (.-key event)))
                                                            (.preventDefault event) (send! [:studio/definition-publish])))}]])
         [:p#source-help.muted.small "EDN is data. Publishing validates the definition and compiles its effect expressions; arbitrary Clojure is not evaluated."]]
        (when (and (= :library (:view ui)) (= :source (:mode definition)) (= "effects" (:kind definition)))
          [block-builder/source-field (:source definition) #(send! [:studio/apply-block-source (:source definition) %])])
        [visual-builder db]]]]]))

(defn- dialog [attrs & children]
  (into [:dialog (assoc attrs :ref (fn [node] (when (and node (not (.-open node))) (.showModal node))))] children))

(defn- cue-actions [db]
  (when-let [cell (get-in db [:ui :action-cell])]
    (let [cue (find-id (get-in db [:show :cues]) (:cue-id cell))
          clipboard (get-in db [:ui :clipboard])
          assigned (or (get-in db [:ui :assign-cue]) (:id (first (get-in db [:show :cues]))))]
      [dialog {:id "cue-actions" :class "action-dialog" :aria-labelledby "action-cue-name"
               :on-cancel (fn [event] (.preventDefault event) (send! [:studio/close-actions]))}
       [:div.dialog-header [:div [:span.eyebrow "CUE ACTIONS"] [:h2#action-cue-name (if cue (display-name cue) "An open space")]]
        [:button.icon-button {:aria-label "Close actions" :on-click #(send! [:studio/close-actions])} "×"]]
       (when-not cue
         [:div#assign-cue-row.form-row.add-slot
          [:select#assign-cue {:aria-label "Choose a cue for this empty cell" :value (or assigned "")
                              :on-change #(set-ui! [:assign-cue] (input-value %))}
           (for [item (get-in db [:show :cues])] ^{:key (:id item)} [:option {:value (:id item)} (display-name item)])]
          [:button#assign-cue-button.button.primary {:disabled (nil? assigned)
                                                     :on-click #(send! [:studio/cue-action :assign])} "Place cue"]])
       (when cue [spatial/cue-target-picker (:id cue)])
       [:div.radial-actions
        (for [[action icon title] (cond-> [[:launch "▷" "One-shot"]]
                                   (not= "off" (get-in db [:ui :launch-grid])) (conj [:launch-now "▷" "One-shot now"])
                                   true (into [[:inspect "⌁" "Inspect"] [:copy "⧉" "Copy cue"]
                                               [:paste "↙" "Paste here"] [:compose "+" "Add to composition"]
                                               [:edit "⌘" "Edit definition"] [:remove "−" "Clear cell"]]))]
          ^{:key action} [:button {:data-cue-action (name action)
                                    :disabled (if (= action :paste) (or (nil? clipboard) (some? cue)) (nil? cue))
                                    :on-click #(send! [:studio/cue-action action])} [:span icon] title])]
       [:p.small.muted "These actions are also available with Shift + F10 on a focused cell."]])))

(defn- prompt-dialog [db]
  (when-let [prompt (get-in db [:ui :prompt])]
    [dialog {:id "text-prompt" :class "prompt-dialog" :aria-labelledby "text-prompt-title"
             :on-cancel (fn [event] (.preventDefault event) (send! [:studio/prompt-cancel]))}
     [:form#text-prompt-form {:on-submit (fn [event] (.preventDefault event) (send! [:studio/prompt-submit]))}
      [:h2#text-prompt-title (:title prompt)]
      [:label [:span#text-prompt-label (:label prompt)]
       [:input#text-prompt-input {:required true :max-length 80 :auto-focus true :value (:value prompt "")
                                 :on-change #(set-ui! [:prompt :value] (input-value %))}]]
      [:div.button-row [button "text-prompt-cancel" "Cancel" [:studio/prompt-cancel]]
       [:button.button.primary {:type "submit"} "Create"]]]]))

(defn- footer [db]
  [:footer.workspace-footer
   [:span "TOLGLOW STUDIO " [:span.muted "/"] " "
    [:span#session-meta (str (count (get-in db [:show :rig :fixtures])) " fixtures · " (count (get-in db [:show :cues])) " cues · revision " (get-in db [:show :revision] 0))]]
   [:span.mobile-file-actions [file-actions true]]
   [:span#shortcut-hint "B blackout · Space transport · 1–8 views"]])

(defn studio []
  (let [db @(rf/subscribe [:studio/db])
        error (or (get-in db [:show :status :error :message]) (get-in db [:connection :error]))
        connected? (get-in db [:connection :connected?])]
    [:<>
     [:a.skip {:href "#main"} "Skip to show controls"]
     [sidebar db]
     [:div.workspace [topbar db]
      [:main#main {:tab-index -1}
       (when (or error (not connected?))
         [:div#connection-banner.connection-banner {:role "status"}
          (or error "Connecting to the studio. Cue controls become available when the show is ready.")])
       [perform-view db]
       (when (= :compose (get-in db [:ui :view])) [composition-view {:ui (:ui db) :show (select-keys (:show db) [:cues :effects :compositions])}])
       (when (= :rig (get-in db [:ui :view])) [rig-view (select-keys db [:ui :show :server-presentation :session])])
       [definition-view {:ui (:ui db) :show (select-keys (:show db) [:effects :cues :pages :runtime-id :show-id :epoch])}]
       [chat/panel db]
       (when (= :connections (get-in db [:ui :view])) [:<> [connections/panel {:show (select-keys (:show db) [:cues :transport])}]
              [expressive/panel (:show db)]])
       (when (= :develop (get-in db [:ui :view])) [workspace/panel db])
       (when (= :debug (get-in db [:ui :view])) [debug/panel])
       [footer db]]]
     [cue-actions db] [prompt-dialog db] [checkpoints/panel (checkpoints/view-props db)] [repl/console-view]
     (when-let [toast (get-in db [:ui :toast])]
       [:div#toast.toast {:class (when (:error? toast) "error") :role "status" :aria-live "polite"} (:message toast)])]))
