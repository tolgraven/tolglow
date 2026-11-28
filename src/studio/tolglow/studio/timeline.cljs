(ns tolglow.studio.timeline
  "Musical timeline for the Perform view (#646). Now is fixed at the centre:
  the left half has played and the right half is upcoming. The panel leases
  the shared [:timeline/view {:bars n}] projection, the same map the REPL reads
  with (show/timeline runtime), and only maps its beats to pixels. One
  animation-frame loop owned by the mounted panel slides the lanes under the
  centre line between committed transport samples."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.timeline-model :as model]
            [tolglow.studio.view-tabs :as view-tabs]))

(def query-id :timeline/view)

(def lanes [[:events "Events"] [:audio "Audio"] [:sections "Sections"] [:grid "Grid"]])

(defn settings [db]
  (merge {:open? true :bars model/default-bars :collapsed #{}} (get-in db [:ui :timeline])))

(rf/reg-sub ::settings (fn [db _] (settings db)))

(rf/reg-sub ::connected? (fn [db _] (true? (get-in db [:connection :connected?]))))

;; On the main page's Perform view or in a detached Perform window (#669).
(rf/reg-sub ::visible? (fn [db _] (view-tabs/shown? db :perform)))

(defn toggle-selection [db id]
  (assoc-in db [:ui :timeline :selected] (when (not= id (get-in db [:ui :timeline :selected])) id)))

(rf/reg-event-db ::select (fn [db [_ id]] (toggle-selection db id)))

(defn- set-ui! [key value] (rf/dispatch-sync [:studio/ui-set [:timeline key] value]))

(defn- wire [value] (if (keyword? value) (subs (str value) 1) (str value)))

(defn- px [value] (str (.toFixed value 2) "px"))

;; Icons ---------------------------------------------------------------------------

(def ^:private icon-paths
  "One glyph per action category, drawn on a 16-unit grid."
  {"cue" [[:path {:d "M5 3.2v9.6L13 8z" :class "solid"}]]
   "instance" [[:rect {:x 4 :y 4 :width 8 :height 8 :rx 1.2 :class "solid"}]]
   "param" [[:path {:d "M2.5 8h11"}] [:circle {:cx 10 :cy 8 :r 2.4 :class "solid"}]]
   "ramp" [[:path {:d "M2.5 12.5C7 12.5 8 3.5 13.5 3.5"}]]
   "master" [[:path {:d "M8 2.5v11M4.5 13.5h7"}] [:rect {:x 5.2 :y 5 :width 5.6 :height 3 :rx 0.8 :class "solid"}]]
   "variable" [[:path {:d "M5.5 2.5c-2 0-1 5.5-3 5.5 2 0 1 5.5 3 5.5M10.5 2.5c2 0 1 5.5 3 5.5-2 0-1 5.5-3 5.5"}]]
   "macro" [[:path {:d "M3 4.5h10M3 8h7M3 11.5h10"}]]
   "scene" [[:circle {:cx 8 :cy 8 :r 2.6 :class "solid"}]
            [:path {:d "M8 1.8v2M8 12.2v2M1.8 8h2M12.2 8h2M3.6 3.6l1.4 1.4M11 11l1.4 1.4M12.4 3.6L11 5M5 11l-1.4 1.4"}]]
   "callback" [[:path {:d "M6 4.5L2.5 8 6 11.5M10 4.5L13.5 8 10 11.5"}]]
   "auto" [[:path {:d "M2.5 11.5l3-4 2.5 2.5 3-5 2.5 3"}] [:circle {:cx 13.5 :cy 8 :r 1.4 :class "solid"}]]})

(defn- icon [category]
  (into [:svg.timeline-icon {:view-box "0 0 16 16" :aria-hidden true :focusable "false"}]
        (get icon-paths (wire category) [[:circle {:cx 8 :cy 8 :r 3 :class "solid"}]])))

;; Store and frame loop -----------------------------------------------------------------

(def ^:private parts
  [:window :meter :grid :events :ramps :sections :audio :audio-status :meters :feel :clock])

(defn new-store []
  {:parts (into {} (map (fn [key] [key (r/atom nil)])) parts)
   :width (r/atom nil) :origin (r/atom nil) :status (r/atom :loading)
   :playhead (atom nil) :layers (atom #{}) :canvas (atom nil) :audio-cache (atom nil)
   :drawn (atom nil) :lines (atom {}) :frame (atom nil)})

(defn- part [store key] @(get-in store [:parts key]))

(defn install!
  "Split a committed projection into per-lane cells, so a lane re-renders only
  when its own data changed, and give the playhead its next sample."
  [store view now]
  (when (map? view)
    (let [position (:position view)
          audio (:audio view)
          derived {:clock {:bar (:bar position) :phrase (:phrase position)
                           :beat (when (number? (:beat position)) (js/Math.floor (:beat position)))
                           :bpm (:bpm view) :playing? (:playing? view)}
                   :audio-status {:available? (:available? audio) :timing-valid? (:timing-valid? audio)
                                  :empty? (empty? (:columns audio))}}]
      (doseq [[key cell] (:parts store)]
        (let [value (get derived key (get view key))]
          (when (not= value @cell) (reset! cell value))))
      (when-let [scale (model/scale @(:width store) (:window view))]
        (let [origin (model/origin @(:origin store) (:window view) scale)]
          (when (not= origin @(:origin store)) (reset! (:origin store) origin))))
      (swap! (:playhead store) model/accept (select-keys view [:beat :time-ms :playing? :epoch]) now))))

(defn- number-attribute [el name]
  (let [value (js/parseFloat (.getAttribute el name))]
    (when (js/Number.isFinite value) value)))

(defn- place-layer!
  "Slide one layer so `beat` sits under the centre line, from the origin and
  scale its children were positioned with."
  [el beat]
  (when-let [transform (model/translation {:ppb (number-attribute el "data-ppb")
                                           :centre (number-attribute el "data-centre")}
                                          (number-attribute el "data-origin")
                                          (number-attribute el "data-shift")
                                          beat)]
    (set! (.. el -style -transform) transform)))

(defn- audio-columns
  "Draw list for the canvas, rebuilt only when new columns arrive."
  [store]
  (let [columns (:columns (part store :audio))
        cache @(:audio-cache store)]
    (if (identical? columns (:columns cache))
      (:list cache)
      (let [list (into-array (map (fn [column]
                                    #js [column (widgets/audio-bin-color {:low (nth column 5) :mid (nth column 6)
                                                                          :high (nth column 7)})])
                                  (vals columns)))]
        (reset! (:audio-cache store) {:columns columns :list list})
        list))))

(defn- draw-audio!
  "Redraw the fixed audio canvas for this frame's playhead. Columns keep their
  measured extrema; nothing is resampled or invented."
  [store canvas beat]
  (let [width (.-clientWidth canvas) height (.-clientHeight canvas)
        ppb (number-attribute canvas "data-ppb") centre (number-attribute canvas "data-centre")
        list (audio-columns store)
        key [list beat width height ppb]]
    (when (and (pos? width) (pos? height) ppb centre (number? beat) (not= key @(:drawn store)))
      (let [ratio (or (.-devicePixelRatio js/window) 1)
            pixel-width (js/Math.round (* width ratio)) pixel-height (js/Math.round (* height ratio))
            context (.getContext canvas "2d")
            scale {:ppb ppb :centre centre :width width}]
        (when (or (not= pixel-width (.-width canvas)) (not= pixel-height (.-height canvas)))
          (set! (.-width canvas) pixel-width)
          (set! (.-height canvas) pixel-height))
        (.setTransform context ratio 0 0 ratio 0 0)
        (.clearRect context 0 0 width height)
        ;; The playhead trails the newest committed sample by up to one sample
        ;; period, so audio stamped after the shown beat waits behind now.
        (.save context)
        (.beginPath context)
        (.rect context 0 0 centre height)
        (.clip context)
        (dotimes [index (alength list)]
          (let [entry (aget list index)]
            (when-let [[left column-width top column-height] (model/column-rect (aget entry 0) beat scale height)]
              (set! (.-fillStyle context) (aget entry 1))
              (.fillRect context left top (max 1 (- column-width 0.4)) column-height))))
        (.restore context)
        (reset! (:drawn store) key)))))

(defn- frame! [store]
  (when-let [beat (model/position @(:playhead store) (.now js/performance))]
    (doseq [el @(:layers store)] (place-layer! el beat))
    (when-let [canvas @(:canvas store)] (draw-audio! store canvas beat))))

(defn- start-loop! [store]
  (letfn [(tick [_]
            (frame! store)
            (reset! (:frame store) (js/requestAnimationFrame tick)))]
    (reset! (:frame store) (js/requestAnimationFrame tick))))

(defn- stop-loop! [store]
  (when-let [id @(:frame store)] (js/cancelAnimationFrame id))
  (reset! (:frame store) nil))

(defn- layer
  "Beat-positioned content that the frame loop slides. :shift is the left edge
  of a clipping parent, for the past-only and future-only halves.

  The render writes the transform for the current playhead together with the
  children's positions. A zoom, resize or origin move then commits new
  positions and their matching transform in one DOM update. Leaving the
  transform to the next frame-loop tick showed one painted frame of new
  positions under the old transform whenever Reagent rendered after that
  frame's tick. Zooming out then shifted every item left by the playhead's
  distance from the origin at the new scale, often hundreds of pixels, so
  upcoming events appeared behind now (#801)."
  [store _ & _]
  (let [node (volatile! nil)
        ref (fn [el]
              (when-let [old @node] (swap! (:layers store) disj old))
              (vreset! node el)
              (when el
                (swap! (:layers store) conj el)
                (place-layer! el (model/position @(:playhead store) (.now js/performance)))))]
    (fn [_ {:keys [scale origin shift class]} & children]
      (let [transform (model/translation scale origin shift (model/position @(:playhead store) (.now js/performance)))]
        (into [:div.timeline-layer {:class class :ref ref :data-origin origin :data-ppb (:ppb scale)
                                    :data-centre (:centre scale) :data-shift (or shift 0)
                                    :style (when transform {:transform transform})}]
              children)))))

(defn- geometry [store]
  (let [window (part store :window)
        scale (model/scale @(:width store) window)
        origin @(:origin store)]
    (when (and scale (number? origin)) {:scale scale :origin origin :window window})))

;; Lanes ---------------------------------------------------------------------------------

(defn- curve [shape falling? fill?]
  (when-let [points (model/curve-points shape falling?)]
    (let [last-x (dec (count shape))]
      [:svg.timeline-curve {:view-box (str "0 -0.15 " last-x " 1.3") :preserve-aspect-ratio "none"
                            :aria-hidden true :focusable "false"}
       (when fill? [:polygon {:points (str "0,1.15 " points " " last-x ",1.15")}])
       [:polyline {:points points :vector-effect "non-scaling-stroke"}]])))

(defn- item-style [{:keys [left right line]}]
  {:left (px left) :top (px (* line model/line-height)) :width (px (- right left))})

(defn- event-button [item selected? select!]
  (let [{:keys [event-id category state title caption]} item]
    [:button.timeline-event
     {:type "button" :style (item-style item) :class (when selected? "selected")
      :data-event-id event-id :data-category (wire category) :data-state (wire state)
      :aria-label (model/item-label item) :aria-pressed selected?
      :title (str title " · " (model/state-label state))
      :on-click #(select! item)}
     [:span.timeline-chip [icon category]]
     (when (pos? caption) [:span.timeline-caption title])]))

(defn- ramp-button [item selected? select!]
  (let [{:keys [event-id state title shape span]} item]
    [:button.timeline-ramp
     {:type "button" :style (item-style item) :class (when selected? "selected")
      :data-event-id event-id :data-category "ramp" :data-state (wire state)
      :aria-label (model/item-label item) :aria-pressed selected?
      :title (str title " · " (model/state-label state))
      :on-click #(select! item)}
     [:span.timeline-ramp-span {:style {:width (px (max 8 span))}}
      [curve shape (model/falling? item) false]]
     [:span.timeline-ramp-label [icon :ramp] title]]))

(defn- ramp-fill
  "The same bar in the past-only half: the part left of now is the progress."
  [item]
  [:div.timeline-ramp-fill {:style (assoc (item-style item) :width (px (max 8 (:span item))))
                            :data-state (wire (:state item))}
   [curve (:shape item) (model/falling? item) true]])

(defn events-lane [store selected select!]
  (when-let [{:keys [scale origin]} (geometry store)]
    (let [boxes (concat (map #(model/event-box scale origin %) (part store :events))
                        (map #(model/ramp-box scale origin %) (part store :ramps)))
          {:keys [items lines assignment]} (model/stack boxes @(:lines store) 4)
          ramps (filter #(= :ramp (:kind %)) items)]
      (reset! (:lines store) assignment)
      [:div.timeline-lane-body {:style {:height (px (* (max 1 lines) model/line-height))}}
       [:div.timeline-past {:style {:width (px (:centre scale))} :aria-hidden true}
        (into [layer store {:scale scale :origin origin}]
              (for [item ramps] ^{:key (:key item)} [ramp-fill item]))]
       (into [layer store {:scale scale :origin origin :class "interactive"}]
             (for [item items
                   :let [selected? (= selected (model/stack-id item))]]
               (if (= :ramp (:kind item))
                 ^{:key (:key item)} [ramp-button item selected? select!]
                 ^{:key (:key item)} [event-button item selected? select!])))
       (when (empty? items) [:p.timeline-empty "Nothing scheduled in this window"])])))

(defn audio-lane [store]
  (r/with-let [ref (fn [el] (reset! (:canvas store) el) (reset! (:drawn store) nil))]
    (let [scale (:scale (geometry store))
          {:keys [available? timing-valid? empty?]} (part store :audio-status)]
      [:div.timeline-lane-body.timeline-audio
       [:canvas {:ref ref :data-ppb (:ppb scale) :data-centre (:centre scale)
                 :role "img" :aria-label "Captured audio, scrolling into the past from now"}]
       (cond
         (not available?) [:p.timeline-empty "No captured audio"]
         (false? timing-valid?) [:p.timeline-empty "Capture timing is not valid"]
         (and empty? (false? (:playing? (part store :clock)))) [:p.timeline-empty "Transport stopped"])])))

(defn- confidence-text [confidence]
  (when (number? confidence) (str (js/Math.round (* 100 confidence)) "%")))

(defn- section-block [scale origin window {:keys [label confidence state source] :as section}]
  (let [{:keys [left width]} (model/section-box scale origin section window)]
    [:div.timeline-section {:style {:left (px left) :width (px width)}
                            :data-state (wire state) :data-source (wire source)
                            :title (str label (when-let [text (confidence-text confidence)] (str " · " text " confidence")))}
     [:strong label] (when-let [text (confidence-text confidence)] [:span text])]))

(defn sections-lane [store]
  (when-let [{:keys [scale origin window]} (geometry store)]
    (let [{:keys [items match]} (part store :sections)
          {causal true reference false} (group-by #(= "causal" (wire (:source %))) items)
          status (some-> (:status match) wire)]
      [:div.timeline-lane-body.timeline-sections
       [:div.timeline-past {:style {:width (px (:centre scale))}}
        (into [layer store {:scale scale :origin origin}]
              (for [section causal] ^{:key (:key section)} [section-block scale origin window section]))]
       [:div.timeline-future {:style {:left (px (:centre scale))}}
        (into [layer store {:scale scale :origin origin :shift (:centre scale)}]
              (for [section reference] ^{:key (:key section)} [section-block scale origin window section]))]
       (when match
         [:span.timeline-match {:data-status status}
          (if (= "locked" status) "Matched recording" (str "No match" (when-let [reason (:reason match)] (str " · " (wire reason)))))])
       (when (and (empty? items) (nil? match)) [:p.timeline-empty "No section analysis"])])))

(defn grid-lane [store]
  (when-let [{:keys [scale origin]} (geometry store)]
    (let [marks (model/grid-marks (:lines (part store :grid)) scale (part store :meter))]
      [:div.timeline-lane-body.timeline-grid {:aria-hidden true}
       (into [layer store {:scale scale :origin origin}]
             (for [{:keys [beat kind bar phrase label? phrase-label?]} marks]
               ^{:key (str (wire kind) beat)}
               [:div.timeline-tick {:data-kind (wire kind) :style {:left (px (model/x scale origin beat))}}
                (when label? [:span bar])
                (when phrase-label? [:small (str "phrase " phrase)])]))])))

(defn- guides
  "Bar and phrase lines behind every lane, from the same grid as the ruler."
  [store]
  (when-let [{:keys [scale origin]} (geometry store)]
    (let [marks (model/grid-marks (:lines (part store :grid)) scale (part store :meter))]
      [:div.timeline-guides {:aria-hidden true}
       (into [layer store {:scale scale :origin origin}]
             (for [{:keys [beat kind]} marks
                   :when (not= :beat kind)]
               ^{:key (str (wire kind) beat)}
               [:i {:data-kind (wire kind) :style {:left (px (model/x scale origin beat))}}]))])))

(defn- lane [store collapsed id title body]
  (let [closed? (contains? collapsed id)]
    [:div.timeline-lane {:data-lane (name id) :class (when closed? "collapsed")}
     [:button.timeline-lane-toggle
      {:type "button" :aria-expanded (not closed?) :aria-label (str (if closed? "Show " "Hide ") (str/lower-case title) " lane")
       :on-click #(set-ui! :collapsed ((if closed? disj conj) collapsed id))}
      [:span title] [:i {:aria-hidden true} (if closed? "▸" "▾")]]
     (when-not closed? body)]))

;; Meters ----------------------------------------------------------------------------------

(defn- percent [value] (str (js/Math.round (* 100 value)) "%"))

(defn- meter-text [{:keys [available? value dbfs relative breakdown? used-watts limit-watts]}]
  (cond
    (not available?) "No reading"
    (number? dbfs) (str (percent value) ", " dbfs " dBFS")
    (number? relative) (str relative " × recent level" (when breakdown? ", breakdown"))
    (number? limit-watts) (str (percent value) ", " used-watts " of " limit-watts " W")
    :else (percent value)))

(defn- meter [{:keys [id label available? value mean target peak class breakdown?] :as reading}]
  [:div.timeline-meter {:role "meter" :aria-label label :aria-valuemin 0 :aria-valuemax 1
                        :aria-valuenow (if available? value 0) :aria-valuetext (meter-text reading)
                        :data-meter (wire id) :data-class (some-> class wire) :title (str label ": " (meter-text reading))
                        :class [(when-not available? "unavailable") (when breakdown? "breakdown")]}
   [:div.timeline-meter-track
    [:i.timeline-meter-fill {:style {:transform (str "scaleY(" (if available? value 0) ")")}}]
    (when (and available? (number? peak)) [:i.timeline-meter-mark.peak {:style {:bottom (percent peak)}}])
    (when (and available? (number? mean)) [:i.timeline-meter-mark.mean {:style {:bottom (percent mean)}}])
    (when (number? target) [:i.timeline-meter-mark.target {:style {:bottom (percent target)}}])]
   [:output (if available? (percent value) "—")]
   [:span label]])

(defn feel-slot
  "The active track-feel personality (#647), drawn from whatever the projection
  supplies: an icon, a name and a palette. Empty until one is published."
  [{:keys [label name icon-url icon-svg palette] :as feel}]
  (let [text (or label name (some-> (:id feel) wire))
        source (or icon-url (when (string? icon-svg)
                              (str "data:image/svg+xml;charset=utf-8," (js/encodeURIComponent icon-svg))))]
    [:div.timeline-feel {:data-filled (boolean feel) :aria-label (if feel (str "Track feel: " text) "Track feel: none")}
     (if feel
       [:<>
        (if source [:img {:src source :alt "" :width 40 :height 40}] [:span.timeline-feel-mark {:aria-hidden true}])
        [:span text]
        (when (seq palette)
          [:span.timeline-feel-palette {:aria-hidden true}
           (for [[index colour] (map-indexed vector (take 6 palette))
                 :when (string? colour)]
             ^{:key index} [:i {:style {:background colour}}])])]
       [:span.muted "Feel"])]))

(defn meters-panel [store]
  [:aside.timeline-meters {:aria-label "Energy and light meters"}
   [feel-slot (part store :feel)]
   (into [:div.timeline-meter-row]
         (for [reading (part store :meters)] ^{:key (wire (:id reading))} [meter reading]))])

;; Details and toolbar -----------------------------------------------------------------------

(defn- selected-item [store selected]
  (when selected
    (when-let [{:keys [scale origin]} (geometry store)]
      (some #(when (= selected (model/stack-id %)) %)
            (concat (map #(model/event-box scale origin %) (part store :events))
                    (map #(model/ramp-box scale origin %) (part store :ramps)))))))

(defn detail-panel [item connected?]
  (let [action (model/action-label item)]
    [:section.timeline-detail {:aria-label "Selected event" :data-event-id (:event-id item)
                               :data-category (wire (:category item)) :data-state (wire (:state item))}
     [:header
      [:span.timeline-chip [icon (:category item)]]
      [:strong (:title item)]
      [:span.timeline-state (model/state-label (:state item))]]
     [:dl (for [[term text] (model/details item)] ^{:key term} [:div [:dt term] [:dd text]])]
     [:div.timeline-detail-actions
      (when action
        [:button.button.timeline-cancel
         {:type "button" :disabled (not connected?)
          :aria-label (str action " " (:title item))
          ;; The same validated owner command as (show/cancel-events! runtime {:event-ids [...]}).
          :on-click #(rf/dispatch [:studio/command "schedule/cancel" {:event-ids [(:event-id item)]}])}
         action])
      [:button.text-button {:type "button" :on-click #(set-ui! :selected nil)} "Close"]]]))

(defn- readout [store]
  (let [{:keys [bar beat bpm playing?]} (part store :clock)]
    [:span.timeline-readout.muted.small {:role "status"}
     (if (number? bar)
       (str "bar " bar " · beat " beat (when (number? bpm) (str " · " (.toFixed bpm 1) " BPM"))
            (when (false? playing?) " · stopped"))
       "Waiting for the show")]))

(defn- toolbar [store {:keys [bars open?]}]
  [:div.panel-top.timeline-top
   [:div [:span.section-label "TIMELINE"] (when open? [readout store])]
   [:div.timeline-tools
    (when open?
      [:<>
       [:button.icon-button {:type "button" :aria-label "Zoom in: show fewer bars" :title "Show fewer bars"
                             :disabled (= bars (first model/zoom-steps))
                             :on-click #(set-ui! :bars (model/zoom bars :in))} "+"]
       [:output.timeline-zoom {:aria-label "Visible window"} (str "±" bars (if (= 1 bars) " bar" " bars"))]
       [:button.icon-button {:type "button" :aria-label "Zoom out: show more bars" :title "Show more bars"
                             :disabled (= bars (last model/zoom-steps))
                             :on-click #(set-ui! :bars (model/zoom bars :out))} "−"]])
    [:button.text-button {:type "button" :aria-expanded open? :on-click #(set-ui! :open? (not open?))}
     (if open? "Hide" "Show")]]])

(defn- observe-width
  "Track the lane viewport's width in the store. Returns the ref callback."
  [store]
  (let [observer (volatile! nil)]
    (fn [el]
      (when-let [old @observer] (.disconnect old) (vreset! observer nil))
      (when el
        (reset! (:width store) (.-clientWidth el))
        (when (exists? js/ResizeObserver)
          (let [watch (js/ResizeObserver. (fn [_] (reset! (:width store) (.-clientWidth el))))]
            (.observe watch el)
            (vreset! observer watch)))))))

(defn- status-line [store]
  (case @(:status store)
    :error [:p.timeline-status {:role "status"} "The timeline is unavailable for this show."]
    :loading (when (nil? (part store :window)) [:p.timeline-status {:role "status"} "Waiting for the timeline…"])
    nil))

(defn- selection [store selected]
  (when-let [item (selected-item store selected)]
    [detail-panel item @(rf/subscribe [::connected?])]))

(defn- body [store]
  (r/with-let [viewport (observe-width store)
               select! (fn [item] (rf/dispatch-sync [::select (model/stack-id item)]))
               ;; A zoom change leases a new window. The lanes keep the previous
               ;; one until the first sample of the new window arrives. The
               ;; playhead comes from the same samples and never passes the
               ;; newest one, so it holds still while the old events show.
               feed (r/track! (fn []
                                (let [bars (:bars @(rf/subscribe [::settings]))
                                      result @(rf/subscribe [:studio/remote-result [query-id {:bars bars}]])
                                      status (cond (:error result) :error (nil? result) :loading :else :ready)]
                                  (when (not= status @(:status store)) (reset! (:status store) status))
                                  (install! store (:value result) (.now js/performance)))))
               _ (start-loop! store)]
    (let [{:keys [bars collapsed selected]} @(rf/subscribe [::settings])]
      [:<>
       [:div.timeline-body
        [:div.timeline-lanes {:role "group" :aria-label (str "Timeline, " bars " bars either side of now")}
         [:div.timeline-viewport {:ref viewport} [guides store]]
         [lane store collapsed :events "Events" [events-lane store selected select!]]
         [lane store collapsed :audio "Audio" [audio-lane store]]
         [lane store collapsed :sections "Sections" [sections-lane store]]
         [lane store collapsed :grid "Grid" [grid-lane store]]
         [:div.timeline-now-layer {:aria-hidden true} [:div.timeline-now [:span "NOW"]]]]
        [meters-panel store]]
       [status-line store]
       [selection store selected]])
    (finally
      (stop-loop! store)
      (r/dispose! feed))))

(defn panel
  "The timeline panel. Hidden, or on another view, it holds no lease and runs
  no frame loop."
  []
  (r/with-let [store (new-store)]
    (let [{:keys [open?] :as settings} @(rf/subscribe [::settings])
          visible? @(rf/subscribe [::visible?])]
      [:section#timeline-panel.timeline-panel.panel {:aria-label "Musical timeline" :data-open (boolean open?)}
       [toolbar store settings]
       (when (and open? visible?) [body store])])))
