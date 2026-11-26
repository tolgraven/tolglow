(ns tolglow.studio.views
  "Reagent views. Domain changes go through re-frame events; the app-db is the
  source of truth. WebGL and Neovim own only their isolated canvas surfaces."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.chat :as chat]
            [tolglow.studio.checkpoints :as checkpoints]
            [tolglow.studio.cue-pad-preview :as pad-preview]
            [tolglow.studio.pad-breath :as pad-breath]
            [tolglow.studio.composition-layers :as layers]
            [tolglow.studio.variable-blocks :as variable-blocks]
            [tolglow.studio.debug :as debug]
            [tolglow.studio.camera-controls :as camera-controls]
            [tolglow.studio.preview-playback :as playback]
            [tolglow.studio.airplay :as airplay]
            [tolglow.studio.room-window :as room-window]
            [tolglow.studio.panel-layout :as panel-layout]
            [tolglow.studio.panel-tiles :as panel-tiles]
            [tolglow.studio.block-builder :as block-builder]
            [tolglow.studio.material-designer :as materials]
            [tolglow.studio.connections :as connections]
            [tolglow.studio.events :as events]
            [tolglow.studio.keyframe-editor :as keyframes]
            [tolglow.studio.knob-candidates :as knob-candidates]
            [tolglow.studio.knobs :as knobs]
            [tolglow.studio.knob-presets :as knob-presets]
            [tolglow.studio.reactive-editor :as reactive-editor]
            [tolglow.studio.expressive-mapping :as expressive]
            [tolglow.studio.effect-group-mixer :as effect-group-mixer]
            [tolglow.studio.fixture-library :as fixture-library]
            [tolglow.studio.fog-fields :as fog-fields]
            [tolglow.studio.rdm :as rdm]
            [tolglow.studio.interop :as interop]
            [tolglow.studio.rig-editor :as rig-editor]
            [tolglow.studio.spatial :as spatial]
            [tolglow.studio.spatial-preview :as spatial-preview]
            [tolglow.studio.video-strips :as video-strips]
            [tolglow.studio.video-surfaces :as video-surfaces]
            [tolglow.studio.modulation :as modulation]
            [tolglow.studio.navigation :as nav]
            [tolglow.studio.operator-history :as history]
            [tolglow.studio.page-routes :as pages]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.numeric-display :as numeric]
            [tolglow.studio.parameter-source :as sources]
            [tolglow.studio.param-visualization :as visualization]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.waveform :as waveform]
            [tolglow.studio.repl :as repl]
            [tolglow.studio.performance :as performance]
            [tolglow.studio.lighting-scenes :as lighting-scenes]
            [tolglow.studio.lighting-policy :as lighting-policy]
            [tolglow.studio.personality :as personality]
            [tolglow.studio.autopilot :as autopilot]
            [tolglow.studio.timeline :as timeline]
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
     [:button#open-operator-history.button.operator-history-open
      {:type "button" :aria-label "Open operator history" :aria-haspopup "dialog"
       :on-click #(send! [:operator-history/open])} "History"]
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
  (let [show (get-in db [:server-projection :view]) ui (:ui db)
        page-id (when page (projection/presentation (:id page)))
        recent? (events/recent-page? db page-id)
        typed-cue-id (:cue-id (some #(when (and (= x (:x %)) (= y (:y %))) %) (:cells page)))
        cue-id (when typed-cue-id (events/wire-id typed-cue-id))
        cue (find-id (:cues show) typed-cue-id)
        cue-name (when cue (events/wire-id (display-name cue)))
        preview (or (get colors typed-cue-id) (get colors cue-id))
        active (filter #(= typed-cue-id (:cue-id %)) (:instances show))
        queued? (and cue (some #(= typed-cue-id (:cue-id %)) (:pending-cue-launches show)))
        state (pad-breath/pad-state active queued?)
        cell {:cue-id cue-id :typed-cue-id typed-cue-id :cue-name cue-name
              :x x :y y :page-id page-id :typed-page-id (:id page)
              :owner (select-keys show [:runtime-id :epoch])}
        down (fn [event]
               (when (and (or cue (not recent?))
                          (or (= 0 (.-button event)) (= "touch" (.-pointerType event))))
                 (.setPointerCapture (.-currentTarget event) (.-pointerId event))
                 (send! [:studio/gesture-down (merge cell {:pointer-id (.-pointerId event)
                                                          :client-x (.-clientX event) :client-y (.-clientY event)})])))]
    [:button.cue-cell
     {:class (str (when-not cue "empty ") (pad-breath/pad-classes state)
                  (when (and (:edit? ui) (not recent?)) "editing"))
      :style (when cue (if preview {"--cue-color" (widgets/color-css preview)} (color-style cue)))
      :data-preview (when preview "live") :data-x x :data-y y :data-cue-id (or cue-id "")
      :disabled (not (get-in db [:connection :connected?]))
      :aria-label (str (or cue-name "Empty cell") ", column " (inc x) ", row " (inc y)
                       (cond (:held state) ", held" (:running state) ", running"
                             (:releasing state) ", releasing")
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
        [:span.cue-name cue-name]
        [:span.cue-category (if queued? "Queued" (or (some-> (:category cue) events/wire-id) "CUE"))]]
       (when-not recent? "+"))]))

(defn- pad-breath-wanted?
  "Whether any pad on this page breathes, blinks or flashes, so the shared
  clock only runs while something on screen needs it."
  [db page]
  (let [show (get-in db [:server-projection :view])
        cue-ids (set (keep :cue-id (:cells page)))]
    (boolean (or (some #(and (contains? cue-ids (:cue-id %))
                             (contains? #{"running" "releasing"} (some-> (:phase %) events/wire-id)))
                       (:instances show))
                 (some #(contains? cue-ids (:cue-id %)) (:pending-cue-launches show))))))

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

(defn- ramp-target-label [to]
  (if (number? to) (numeric/number-label to) (str to)))

(defn scheduled-events
  "Beat-clock schedule from the shared :schedule projection (docs/scheduler.md):
  pending scheduled events and running ramps, each cancellable by event ID."
  [db]
  (let [{:keys [upcoming ramps]} (get-in db [:show :schedule])
        show (:show db)
        ms? (fn [ramp] (= "ms" (events/wire-id (:unit ramp))))
        ;; The projection is stable; live progress comes from the transport clock.
        progress (fn [{:keys [start end] :as ramp}]
                   (let [now (if (ms? ramp) (:time-ms show) (get-in show [:transport :beat]))]
                     (if (and (number? now) (number? start) (number? end) (< start end))
                       (max 0 (min 1 (/ (- now start) (- end start)))) 0)))
        ;; An :ms ramp ends on the owner's millisecond clock, not on a beat.
        ending (fn [{:keys [end] :as ramp}]
                 (cond
                   (not (ms? ramp)) (str "by beat " (numeric/number-label end))
                   (and (number? end) (number? (:time-ms show)))
                   (str "in " (numeric/number-label (/ (max 0 (- end (:time-ms show))) 1000)) " s")
                   :else "on the millisecond clock"))
        scheduled (filterv #(= "scheduled" (events/wire-id (:origin %))) upcoming)
        connected? (get-in db [:connection :connected?])
        cancel! (fn [event-id] #(send! [:studio/command "schedule/cancel" {:event-ids [event-id]}]))]
    (when (or (seq scheduled) (seq ramps))
      [:section.queued-launches.scheduled-events {:aria-label "Scheduled events"}
       [:strong "Scheduled"]
       [:ul
        (for [{:keys [event-id label op due-beat category]} scheduled
              :let [title (or label (events/wire-id op))]]
          ^{:key (str "event-" event-id)}
          [:li {:data-event-id event-id :data-category (events/wire-id category)}
           [:span (str title " · beat " (numeric/number-label due-beat))]
           [:button.text-button {:type "button" :aria-label (str "Cancel scheduled " title)
                                 :disabled (not connected?) :on-click (cancel! event-id)} "Cancel"]])
        (for [{:keys [event-id label to destination] :as ramp} ramps
              :let [title (or label event-id)]]
          ^{:key (str "ramp-" event-id "-" (pr-str destination))}
          [:li {:data-ramp-id event-id}
           [:span (str title " → " (ramp-target-label to) " " (ending ramp))]
           [:progress {:max 1 :value (progress ramp) :aria-label (str title " progress")}]
           [:button.text-button {:type "button" :aria-label (str "Stop ramp " title)
                                 :disabled (not connected?) :on-click (cancel! event-id)} "Stop"]])]])))

(defn- cue-grid [db]
  (let [view (get-in db [:server-projection :view]) ui (:ui db)
        authored-pages (:pages view)
        recent-page (:recent-page view)
        pages (cond-> (vec authored-pages) recent-page (conj recent-page))
        selected-recent (when (events/selected-recent-page db) recent-page)
        matching (filter #(= (events/wire-id (:id %)) (:page-id ui)) authored-pages)
        page (or selected-recent (find-id matching (:typed-page-id ui)) (first matching) (first authored-pages))
        rows (performance/visible-cue-rows page (or selected-recent (:edit? ui) (:full-grid? ui)))]
    [:section.cue-panel.panel {:aria-label "Cue performance grid"}
     [:div.panel-top
      [:div#page-tabs.page-tabs {:aria-label "Cue pages"}
       (for [item pages]
         ^{:key (pr-str (:id item))} [:button.page-tab {:class (when (= (:id item) (:id page)) "selected")
                                                       :on-click #(send! [:studio/set-page (projection/presentation (:id item)) (:id item)])}
                                     (events/wire-id (display-name item))])]
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
       cue-cell db page]
      [pad-breath/clock-driver (get-in db [:show :transport]) (get-in db [:show :transport-generation])
       (and (= :perform (:view ui)) (pad-breath-wanted? db page))]]
     [queued-cues db]
     [scheduled-events db]
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
        shapes (when-not (waveform/waveform-schema? schema) (waveform/shape-choices schema))
        type (cond (waveform/waveform-schema? schema) "waveform" shapes "shape" (seq choices) "choice" :else (:type schema "number"))
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
       "shape" [widgets/shape-picker title shapes value change!]
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

(defn- scene-draft-banner [db]
  (when-let [{:keys [id revision operations status pending? connected? preview] :as control} (events/scene-draft-control db)]
    (let [busy? (or pending? (not connected?))
          scheduled? (= :scheduled status)]
      (if preview
        ;; A generated knob candidate (#667) auditions here; it is kept or discarded from its panel.
        [:div.scene-draft-banner {:role "status" :aria-label "Room draft audition" :data-draft-id id}
         [:div [:strong "Knob candidate"]
          [:span.small.muted (str " · " (:name preview) " at " (.toFixed (js/Number (:value preview)) 2)
                                  (events/scene-draft-note control))]
          [:p.small.muted "The Room shows the candidate. Keep or discard it in Generate a knob; live output is unchanged."]]
         [:div.button-row
          [:button.text-button {:type "button" :disabled busy?
                                :on-click #(send! [:studio/scene-draft-discard])} "Stop audition"]]]
      [:div.scene-draft-banner {:role "status" :aria-label "Room draft audition" :data-draft-id id}
       [:div [:strong "Draft audition"]
        [:span.small.muted (str " · " id " · revision " revision " · " operations " staged"
                                (events/scene-draft-note control))]
        [:p.small.muted (if (#{:stale :error} status)
                          "The Room shows the live show. Live output is unchanged."
                          "The Room shows this draft. Live output is unchanged until you apply it.")]]
       [:div.button-row
        [:button.button.primary {:type "button" :disabled (or busy? scheduled? (#{:stale :error} status))
                                 :on-click #(send! [:studio/scene-draft-apply nil])} "Apply"]
        [:button.button {:type "button" :disabled (or busy? scheduled? (#{:stale :error} status))
                         :on-click #(send! [:studio/scene-draft-apply {:bars 1}])} "Apply on next bar"]
        [:button.text-button {:type "button" :disabled busy?
                              :on-click #(send! [:studio/scene-draft-discard])} "Discard"]]]))))

(defn- preview-panel [db]
  [:section#preview-panel.preview-panel.panel {:aria-label "Live fixture preview"
                                              :class (when (get-in db [:ui :preview-expanded?]) "expanded")}
   [:div.panel-top [:div [:span.section-label "THE ROOM"] [:span#fixture-count.muted.small (str (count (get-in db [:show :rig :fixtures])) " fixtures")]]
    [:div.preview-size-controls
     (when (or (= :perform (get-in db [:ui :view])) (room-window/popped? db))
       [:<> [room-window/controls db] [playback/control "preview"] [airplay/control "preview"]])
     [:button.text-button.preview-fullscreen-tools
      {:type "button" :aria-pressed (boolean (get-in db [:ui :fullscreen-preview-controls?]))
       :on-click #(set-ui! [:fullscreen-preview-controls?] (not (get-in db [:ui :fullscreen-preview-controls?])))} "Controls"]
     (when-not (or (room-window/popped? db) (panel-layout/active-layout db))
       [:button.text-button.preview-expand-control {:type "button" :aria-pressed (boolean (get-in db [:ui :preview-expanded?]))
                              :on-click #(send! [:studio/preview-expand])} (if (get-in db [:ui :preview-expanded?]) "Compact" "Expand")])
     [:button.icon-button {:type "button" :aria-label "Fullscreen preview" :title "Fullscreen preview"
                            :on-click #(input! [:studio/preview-fullscreen])} "⛶"]]]
   [scene-draft-banner db]
   [spatial-preview/scene (events/room-show db) (get-in db [:server-projection :view :settings :spatial]) :perform]
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

(defn- response-duration-label [envelope stage]
  (if-let [[unit value] (or (first (get envelope stage))
                           (when-let [value (get envelope (keyword (str (name stage) "-ms")))]
                             [:ms value]))]
    (str (numeric/number-label value) " " (name unit))
    "0"))

(defn- response-panel [db]
  (let [form (get-in db [:ui :response-form])
        responses (get-in db [:server-projection :view :responses])]
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
      [duration-control "response-decay" "Decay" (:decay-value form 0) (:unit form)
       (fn [value unit] (when (some? value) (set-ui! [:response-form :decay-value] value))
         (when unit (set-ui! [:response-form :unit] unit))) true]
      [:label "Sustain level (0–1)"
       [numeric/number-input {:id "response-sustain" :type "number" :min 0 :max 1 :step "any"
                             :value (:sustain form 1)
                             :on-change #(when-let [n (number-value %)] (set-ui! [:response-form :sustain] n))}]]
      [duration-control "response-release" "Release" (:release-value form) (:unit form)
       (fn [value unit] (when (some? value) (set-ui! [:response-form :release-value] value))
         (when unit (set-ui! [:response-form :unit] unit))) true]
      [:div.button-row [button "response-start" "Start / retrigger" [:studio/response-start] "button primary"]]
      (for [{:keys [id envelope state enabled?]} responses
            :let [envelope (projection/presentation envelope)]]
        ^{:key (pr-str id)}
        [:div.list-card {:data-response-id (events/wire-id id)}
         [:div [:strong (pr-str id)]
          [:p.small.muted (cond
                           (false? enabled?) "Inactive"
                           (or (:released-at-beat state) (:released-at-ms state)) "Releasing / held at zero"
                           :else "Gate held")]
          [:p.small.muted (str "Attack " (response-duration-label envelope :attack)
                              " · Decay " (response-duration-label envelope :decay)
                              " · Sustain " (numeric/number-label (:sustain envelope 1))
                              " · Release " (response-duration-label envelope :release))]]
         [:div.button-row [button nil "Release" [:studio/response-action :release id]]
          [button nil "Remove" [:studio/response-action :delete id] "text-button"]]])
      [:p.small.muted "Attack, decay and release share one unit. Release fades to zero and holds there. Remove the envelope to restore the original parameters."]]]))

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
        count-active (count (get-in db [:show :instances]))
        custom? (some? (panel-layout/active-layout db))]
    [:section#view-perform.view {:hidden (not= :perform (get-in db [:ui :view]))
                                :data-section section :data-layout (if custom? "custom" "built-in")}
     [:div.perform-heading [:h1 "Perform"]
      [:span.small.muted (str count-active " running")]
      [:div.perform-section-tabs {:role "group" :aria-label "Performance sections"}
       (for [[id title] [["cues" "Cues"] ["live" (str "Live · " count-active)] ["room" "Room"]]]
         ^{:key id} [:button {:type "button" :aria-pressed (= id section)
                              :on-click #(set-ui! [:performance-section] id)} title])]
      [panel-layout/toolbar db]]
     [timeline/panel]
     [panel-layout/arrangement db
      {:class (when (and (not custom?) (get-in db [:ui :preview-expanded?])) "preview-expanded")}
      {:cues [:div.performance-launch [cue-grid db]]
       :live [:div.performance-live [section-pattern-panel db] [personality/panel] [performance/board db parameter-control]
              [effect-group-mixer/panel] [knobs/panel db] [knob-presets/panel db] ; knob looks (#547)
              [knob-candidates/panel db]
              [autopilot/panel] [lighting-scenes/panel] [lighting-policy/panel] [autopilot/hdr-panel]]
       :room [:div.performance-room [room-window/room db preview-panel]]
       :tools [:div.performance-tools [auto-panel db] [response-panel db] [modulation/panel db]]}
      {:render (fn [entry tier] [panel-tiles/render db parameter-control entry tier])
       :chrome (fn [entry] [panel-tiles/chrome db entry])}]]))

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

(defn empty-slot?
  "Sequence steps authored with :kind :empty occupy time without a cue."
  [slot]
  (= "empty" (some-> (:kind slot) name)))

(defn sequence-only-steps?
  "Empty and muted steps only have meaning in a step sequence."
  [slots]
  (boolean (some #(or (empty-slot? %) (:muted? %)) slots)))

(defn- composition-settings [composition macro?]
  [:div.compose-settings
   [:label "Name" [:input#composition-name {:value (:name composition "") :max-length 80
                                            :on-change #(set-ui! [:composition :name] (input-value %))}]]
   [:label "Identifier" [:input#composition-id {:value (:id composition "") :on-change #(set-ui! [:composition :id] (input-value %))}]]
   [:label "Behavior" [:select#composition-mode {:value (:mode composition "sequence") :on-change #(set-ui! [:composition :mode] (input-value %))}
                       [:option {:value "sequence"} "Step sequence"] (let [blocked? (sequence-only-steps? (:slots composition))]
                         [:option {:value "macro" :disabled blocked?
                                   :title (when blocked? "Remove empty and muted steps to play these cues together")}
                          (if blocked? "Macro · needs no empty or muted steps" "Macro · together")])]]
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

(defn slot-title
  "Name a step from the saved program, including empty and muted steps."
  [cues slot]
  (str (if (empty-slot? slot) "Empty step" (display-name (find-id cues (:cue-id slot))))
       (when (:muted? slot) " · muted")))

(defn- composition-slot [db index slot macro? last?]
  (let [[duration-value duration-unit] (slot-duration slot)]
    [:li.composition-slot {:class (when macro? "macro-slot")}
     [:span.slot-number (inc index)]
     [:strong.slot-title (slot-title (get-in db [:show :cues]) slot)]
     (when-not macro?
       [duration-control (str "slot-duration-" index) (str "Step " (inc index) " duration") duration-value duration-unit
        (fn [value unit] (input! [:studio/slot-duration index value unit]))])
     [slot-actions index last?]
     (when-not (empty-slot? slot)
       [:div.slot-editors [slot-parameter-editor db index slot] [layers/slot-controls index slot]])]))

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
     (when (= :rig (get-in db [:ui :view])) [video-strips/panel])
     (when (= :rig (get-in db [:ui :view])) [video-surfaces/panel])
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
  (let [builder (get-in db [:ui :builder])
        reactive (:reactive builder)
        loaded? (some? (:base reactive))
        reactive? (true? (:enabled? reactive))]
    [:div#visual-editor {:hidden (not= :form (get-in db [:ui :definition :mode]))}
     [:div.form-row
      [:label "Identifier" [:input#builder-id {:value (:id builder "") :pattern "[a-zA-Z][a-zA-Z0-9._/-]*"
                                              :disabled loaded?
                                              :on-change #(set-ui! [:builder :id] (input-value %))}]]
      [:label "Name" [:input#builder-name {:value (:name builder "") :on-change #(set-ui! [:builder :name] (input-value %))}]]]
     (when (and (= :library (get-in db [:ui :view])) (= :form (get-in db [:ui :definition :mode])))
       [reactive-editor/editor reactive (boolean (get-in db [:ui :definition :publishing?]))
        #(input! [:studio/builder-reactive %])])
     (when-not loaded?
       [:<>
        [:div.form-row
         [:label "Color" [:input#builder-color {:type "color" :value (:color builder "#b981a3") :on-change #(set-ui! [:builder :color] (input-value %))}]]
         (when-not reactive?
           [:label "Intensity" [numeric/number-input {:id "builder-level" :type "number" :min 0 :max 1 :step 0.01 :value (:level builder 0.7)
                                                    :on-change #(when-let [n (number-value %)] (set-ui! [:builder :level] n))}]])
         [duration-control "builder-duration" "Duration" (:duration-value builder 4) (:duration-unit builder "beats")
          (fn [value unit] (when (some? value) (set-ui! [:builder :duration-value] value))
            (when unit (set-ui! [:builder :duration-unit] unit)))]]
        (when-not reactive?
          [:<>
           [duration-control "builder-release" "Release" (:release-value builder 1) (:release-unit builder "beats")
            (fn [value unit] (when (some? value) (set-ui! [:builder :release-value] value))
              (when unit (set-ui! [:builder :release-unit] unit))) true]
           [keyframes/editor (:keyframes builder) [:level] #(set-ui! [:builder :keyframes] %)]
           (when (and (= :library (get-in db [:ui :view])) (= :form (get-in db [:ui :definition :mode])))
             [block-builder/field {:id (some-> builder :id not-empty keyword) :params {:level {}}
                                   :outputs {:color nil :dimmer nil}}
              (or (:blocks builder) {}) (fn [f] (input! [:studio/ui-update [:builder :blocks] #(f (or % {}))]))])])
        [:label.checkbox-label [:input#builder-cue {:type "checkbox" :checked (boolean (:cue? builder)) :on-change #(set-ui! [:builder :cue?] (checked? %))}]
         "Create matching cue"]])
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
      ;; Replace the source before the next input event, which would otherwise be overwritten.
      [:button#new-definition.text-button {:aria-label "New definition" :on-click #(input! [:studio/definition-new])} "New"]]
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
          [block-builder/source-field (:source definition) #(input! [:studio/apply-block-source (:source definition) %])])
        [visual-builder db]]]]]))

(defn- dialog [attrs & children]
  (into [:dialog (assoc attrs :ref (fn [node] (when (and node (not (.-open node))) (.showModal node))))] children))

(defn- cancel-macro-picker! []
  (input! [:studio/macro-picker-back])
  (r/after-render #(some-> (.getElementById js/document "add-to-saved-macro") .focus)))

(defn- cue-macro-picker [db {:keys [macro-id request error] :as picker}]
  (let [macros (events/saved-macros db)
        ids (into {} (map (juxt (comp pr-str :id) :id)) macros)
        selected? (contains? ids (pr-str macro-id))]
    [:form {:aria-busy (boolean request)
            :on-submit (fn [event] (.preventDefault event) (send! [:studio/macro-picker-submit]))}
     (if (seq macros)
       [:div.form-row
        [:label {:html-for "cue-saved-macro"} "Saved macro"
         [:select#cue-saved-macro {:auto-focus true :disabled (boolean request)
                                 :value (if selected? (pr-str macro-id) "")
                                 :on-change #(set-ui! [:macro-picker] (assoc picker :macro-id (get ids (input-value %)) :error nil))}
          [:option {:value ""} "Choose a macro"]
          (for [macro macros :let [id (pr-str (:id macro))]]
            ^{:key id} [:option {:value id} (display-name macro)])]]]
       [:p "No saved macros. Save a macro in Compose to add cues here."])
     (when error [:p {:role "alert"} error])
     [:div.button-row
      [:button.button {:type "button" :auto-focus (empty? macros) :disabled (boolean request)
                       :on-click cancel-macro-picker!} "Cancel"]
      [:button.button.primary {:type "submit"
                               :disabled (or (boolean request) (not selected?) (not (get-in db [:connection :connected?])))}
       (cond request "Adding…" (events/macro-picker-retry? db picker) "Retry append" :else "Add cue")]]]))

(defn- cue-actions [db]
  (when-let [cell (get-in db [:ui :action-cell])]
    (let [cue (find-id (get-in db [:server-projection :view :cues]) (:typed-cue-id cell))
          occupied? (some? (:typed-cue-id cell))
          clipboard (get-in db [:ui :clipboard])
          assigned (or (get-in db [:ui :assign-cue]) (:id (first (get-in db [:show :cues]))))
          picker (get-in db [:ui :macro-picker])]
      [dialog {:id "cue-actions" :class (if picker "action-dialog cue-macro-dialog" "action-dialog")
               :aria-labelledby "action-cue-name"
               :on-cancel (fn [event]
                            (.preventDefault event)
                            (when-not (:request picker)
                              (if picker (cancel-macro-picker!) (send! [:studio/close-actions]))))}
       [:div.dialog-header
        [:div (when-not picker [:span.eyebrow "CUE ACTIONS"])
         [:h2#action-cue-name (if picker (str "Add " (:cue-name picker) " to macro")
                                  (or (:cue-name cell) "An open space"))]]
        [:button.icon-button {:aria-label "Close actions" :disabled (boolean (:request picker))
                              :on-click #(send! [:studio/close-actions])} "×"]]
       (if picker
         [cue-macro-picker db picker]
         [:<>
          (when-not occupied?
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
                                      :disabled (if (= action :paste) (or (nil? clipboard) occupied?) (nil? cue))
                                      :on-click #(send! [:studio/cue-action action])} [:span icon] title])]
          (when occupied? [button "add-to-saved-macro" "Add to macro" [:studio/macro-picker-open]])
          (when-let [error (:error cell)] [:p {:role "alert"} error])
          [:p.small.muted "These actions are also available with Shift + F10 on a focused cell."]])])))

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

(defn- operator-history []
  (let [{:keys [open?] :as inspector} @(rf/subscribe [:operator-history/inspector])]
    (when open?
      [history/panel
       (assoc inspector
              :dispatch-local! #(input! [:operator-history/inspect %])
              :command! #(send! [:operator-history/navigate %1 %2])
              :on-close (fn [_]
                          (send! [:operator-history/close])
                          (some-> (.getElementById js/document "open-operator-history") .focus)))])))

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
     [cue-actions db] [prompt-dialog db] [checkpoints/panel (checkpoints/view-props db)] [operator-history] [repl/console-view]
     (when-let [toast (get-in db [:ui :toast])]
       [:div#toast.toast {:class (when (:error? toast) "error") :role "status" :aria-live "polite"} (:message toast)])]))
