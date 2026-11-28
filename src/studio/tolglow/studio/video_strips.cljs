(ns tolglow.studio.video-strips
  "2D video strip mapping (#672): the latched video frame with each mapped
  strip drawn over it as the footprints the sampler reads, filled with the
  colours it sampled. Edits use the same `tolglow.video.mapping` operations
  as the REPL (`show/video-action! runtime :edit op`) through
  /api/video-mapping; the committed settings come back on the state stream."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.video-controls :as video-controls]
            [tolglow.studio.video-strip-tools :as tools]
            [tolglow.video.mapping :as mapping]))

(defn- view [db] (get-in db [:server-projection :view]))

(defn- identity-key [db] (projection/identity-of (view db)))

(defn ui-state [db]
  (let [stored (:video-strips-ui db) identity (identity-key db)]
    (if (= identity (:identity stored)) stored {:identity identity})))

(def wire-id events/wire-id)

(defn source-param
  "The frame route reads keyword ids with a leading colon."
  [id] (str id))

(defn layout-choices
  "Pixel layouts with their pixel counts, for new strips."
  [rig]
  (mapv (fn [{:keys [id] :as spec}]
          {:id id :pixels (count (filter #(= id (:pixel-layout-id %)) (:fixtures rig)))
           :label (or (:name spec) (wire-id id))})
        (:pixel-layouts rig)))

(defn new-strip
  "A horizontal strip across the middle of the source for a pixel layout,
  stacked below strips already mapped."
  [mapping layout-id]
  (let [n (count (:strips mapping))
        v (+ 0.1 (* 0.8 (/ (mod (* n 0.37) 1.0) 1.0)))]
    {:id layout-id :layout layout-id :points [[0.1 v] [0.9 v]] :height 0.04}))

(defn edited-strip
  "Apply a local geometry edit to a strip (pure, shared geometry)."
  [strip [op & args] aspect]
  (case op
    :move (update strip :points mapping/translate (first args))
    :point (assoc-in strip [:points (first args)] (second args))
    :rotate (update strip :points mapping/rotate (first args) aspect)
    :length (update strip :points mapping/scale (first args))
    :height (assoc strip :height (first args))
    :filter (assoc strip :filter (first args))
    :reverse (assoc strip :reverse? (first args))
    ;; Numeric placement and the rectangle gesture (shared geometry).
    ;; :center-u or :center-v set one coordinate of the centre, so two
    ;; quick edits of different knobs both land.
    :place (let [p (first args)
                 [cu cv] (:center (mapping/strip-placement strip aspect))
                 center (if (or (contains? p :center-u) (contains? p :center-v))
                          [(get p :center-u cu) (get p :center-v cv)]
                          (:center p))]
             (mapping/place-strip strip (cond-> (dissoc p :center-u :center-v) center (assoc :center center)) aspect))
    :rect (merge strip (select-keys (first args) [:points :height]))
    :targets (assoc strip :targets (vec (first args)))
    strip))

(defn- request [db op]
  (let [v (view db)]
    {:path "/api/video-mapping" :typed? true
     :body (assoc op :expected-epoch (:epoch v) :runtime-id (:runtime-id v))
     :on-failure [::failed]}))

(defn- send [db state op]
  (let [token (str (random-uuid))]
    {:db (assoc db :video-strips-ui (-> state (assoc :pending token :settled-token token) (dissoc :queued :error)))
     :studio/http (assoc (request db op) :on-success [::committed token])}))

(defn edit-fx
  "Effects for one edit. A strip edit becomes the local draft until the state
  stream returns it, so quick successive edits build on each other. One
  request is in flight at a time; while it is, only the newest edit waits, so
  an older full-strip write can never land after a newer one."
  [db op]
  (let [state (cond-> (dissoc (ui-state db) :error)
                (= :put-strip (:op op)) (assoc :draft (:strip op) :draft-mapping (:mapping-id op))
                (#{:delete-strip :undo :redo} (:op op)) (dissoc :draft))]
    (if (:pending state)
      {:db (assoc db :video-strips-ui (assoc state :queued op))}
      (send db state op))))

(rf/reg-event-fx ::edit (fn [{:keys [db]} [_ op]] (edit-fx db op)))

(defn current-strip
  "The strip an edit builds on: this panel's unsettled draft, else the
  committed strip from the state stream."
  [db mapping-id strip-id]
  (let [state (ui-state db) draft (:draft state)]
    (if (and draft (= mapping-id (:draft-mapping state)) (= strip-id (:id draft)))
      draft
      (some #(when (= strip-id (:id %)) %) (get-in (view db) [:settings :video :mappings mapping-id :strips])))))

(rf/reg-event-fx ::edit-strip
  ;; Resolve the base strip from app-db when the event runs, not from the
  ;; render that created the control. {:coalesce-control id}, when given,
  ;; makes the edits of one knob turn one undo step.
  [(rf/inject-cofx ::video-controls/now)]
  (fn [{:keys [db now]} [_ mapping-id strip-id op aspect {:keys [coalesce-control]}]]
    (if-let [strip (current-strip db mapping-id strip-id)]
      (let [[token tokens] (when coalesce-control
                             (video-controls/coalesce-key (:coalesce (ui-state db)) coalesce-control now))
            db (cond-> db coalesce-control (assoc :video-strips-ui (assoc (ui-state db) :coalesce tokens)))]
        (edit-fx db (cond-> {:op :put-strip :mapping-id mapping-id :strip (edited-strip strip op aspect)}
                      token (assoc :coalesce token))))
      {})))

(rf/reg-event-fx ::committed
  ;; Send a waiting edit, else let the committed settings arrive on the state
  ;; stream and drop the draft once the newest edit has had time to land.
  (fn [{:keys [db]} [_ token]]
    (let [state (ui-state db)]
      (cond
        (not= token (:pending state)) {}
        (:queued state) (send db (dissoc state :pending) (:queued state))
        :else {:db (assoc db :video-strips-ui (dissoc state :pending))
               :dispatch-later [{:ms 1000 :dispatch [::settle token]}]}))))

(rf/reg-event-db ::settle
  (fn [db [_ token]]
    (let [state (ui-state db)]
      (if (and (nil? (:pending state)) (= token (:settled-token state)))
        (assoc db :video-strips-ui (dissoc state :draft :draft-mapping))
        db))))

(rf/reg-event-db ::failed
  (fn [db [_ error]] (assoc db :video-strips-ui (-> (ui-state db) (dissoc :pending :queued :draft) (assoc :error (str error))))))

(rf/reg-event-db ::ui
  (fn [db [_ path value]] (assoc db :video-strips-ui (assoc-in (ui-state db) path value))))

(rf/reg-event-fx ::find-syphon
  (fn [{:keys [db]} _]
    {:db (assoc db :video-strips-ui (assoc (ui-state db) :syphon {:loading? true}))
     :studio/http {:path "/api/video/syphon" :raw-response? true
                   :on-success [::ui [:syphon]] :on-failure [::ui [:syphon :error]]}}))

(rf/reg-sub ::view (fn [db _] (view db)))
(rf/reg-sub ::ui (fn [db _] (ui-state db)))
(rf/reg-sub ::settings :<- [::view] (fn [v _] (get-in v [:settings :video])))
(rf/reg-sub ::status :<- [::view] (fn [v _] (:video v)))
(rf/reg-sub ::rig :<- [::view] (fn [v _] (:rig v)))

(defn- edit! [op] (rf/dispatch [::edit op]))

(defn- ui! [path value] (rf/dispatch-sync [::ui path value]))

(defn- syphon-id [{:keys [app name]}]
  (keyword "syphon" (str/replace (str/lower-case (str app "-" name)) #"[^a-z0-9]+" "-")))

(defn- source-row [{:keys [id kind label width height stale? frozen? error age-ms]} selected?]
  [:li.video-source {:class (when selected? "selected")}
   [:button.text-button {:type "button" :aria-pressed (boolean selected?) :on-click #(ui! [:source] id)}
    [:strong (or label (wire-id id))]]
   [:span.small.muted (str (name (or kind :unknown)) (when width (str " · " width "×" height)))]
   [:span.video-state {:class (cond error "error" stale? "stale" frozen? "frozen" :else "live")}
    (cond error "unavailable" stale? "stale" frozen? "frozen · transport stopped" :else
          (str "live" (when age-ms (str " · " (js/Math.round age-ms) " ms"))))]
   (when error [:p.small.muted error])])

(defn- sources-panel [settings status ui]
  (let [statuses (into {} (map (juxt :id identity)) (:sources status))
        syphon (:syphon ui)]
    [:div.video-sources
     [:ul.video-source-list
      (for [[id _] (sort-by (comp str key) (:sources settings))]
        ^{:key (str id)} [source-row (merge {:id id} (get statuses id)) (= id (:source ui))])
      ;; A running shader cue's feed (#673) can carry strips too.
      (for [{:keys [id] :as row} (:sources status) :when (= :shader (:kind row))]
        ^{:key (str id)} [source-row row (= id (:source ui))])]
     [:div.video-source-actions
      [:button.text-button {:type "button"
                            :on-click #(edit! {:op :put-source :id :test-grid :source {:kind :test-pattern :width 640 :height 360}})}
       "Add test grid"]
      [:button.text-button {:type "button" :on-click #(rf/dispatch [::find-syphon])} "Find Syphon sources"]]
     (when syphon
       [:div.syphon-list
        (cond (:loading? syphon) [:p.small.muted "Looking for Syphon servers…"]
              (:error syphon) [:p.small.muted (str (:error syphon))]
              (empty? (:servers syphon)) [:p.small.muted "No Syphon servers are publishing."])
        (for [{:keys [app name] :as server} (:servers syphon)]
          ^{:key (str app "/" name)}
          [:div.syphon-server [:span (str app " · " name)]
           [:button.text-button {:type "button"
                                 :on-click #(do (edit! {:op :put-source :id (syphon-id server)
                                                        :source {:kind :syphon :app app :name name}})
                                                (ui! [:source] (syphon-id server)))}
            "Use"]])])]))

(defn- svg-point [^js svg event width height]
  (let [rect (.getBoundingClientRect svg)]
    [(/ (- (.-clientX event) (.-left rect)) (.-width rect))
     (/ (- (.-clientY event) (.-top rect)) (.-height rect))]))

(defn- footprint-polygon [{:keys [center direction along across]}]
  (let [[cx cy] center [dx dy] direction
        a (/ along 2) c (/ (max across 1.5) 2)
        corner (fn [sa sc] (str (+ cx (* sa dx) (* sc (- dy))) "," (+ cy (* sa dy) (* sc dx))))]
    (str/join " " [(corner (- a) (- c)) (corner a (- c)) (corner a c) (corner (- a) c)])))

(defn- strip-overlay [strip colors width height selected? drag!]
  ;; The projection lists colours by footprint along the points, whichever
  ;; end the first pixel is wired to, so they fill the polygons in order.
  (let [n (count colors)
        prints (mapping/footprints (:points strip) n (:height strip 0) width height)
        [[x0 y0]] (mapv (fn [[u v]] [(* u width) (* v height)]) (:points strip))]
    [:g.video-strip {:class (when selected? "selected")}
     (map-indexed (fn [i print]
                    ^{:key i} [:polygon {:points (footprint-polygon print) :fill (get colors i "#333")}])
                  prints)
     [:polyline.video-strip-line {:points (str/join " " (map (fn [[u v]] (str (* u width) "," (* v height))) (:points strip)))
                                  :on-pointer-down #(drag! % [:move])}]
     [:circle.video-strip-start {:cx x0 :cy y0 :r (/ width 160)}]
     (when selected?
       (map-indexed (fn [i [u v]]
                      ^{:key (str "h" i)} [:circle.video-strip-handle {:cx (* u width) :cy (* v height) :r (/ width 90)
                                                                      :on-pointer-down #(drag! % [:point i])}])
                    (:points strip)))
     ;; Labels scale with the frame and stay inside its right edge.
     (let [size (/ width 48) right? (> x0 (* 0.8 width))]
       [:text.video-strip-label {:x (if right? (- x0 (/ size 2)) (+ x0 (/ size 2))) :y (- y0 (/ size 2))
                                 :font-size size :stroke-width (/ size 6) :text-anchor (if right? "end" "start")}
        (wire-id (:id strip))])]))

(defn- frame-image [source sequence width]
  ;; Refresh the still at most four times a second while frames advance.
  (let [shown (r/atom {:token nil :at 0})]
    (fn [source sequence width]
      (let [now (.now js/Date)]
        (when (and (not= sequence (:token @shown)) (> (- now (:at @shown)) 250))
          (reset! shown {:token sequence :at now}))
        [:img.video-frame {:alt "Latched video frame" :draggable false
                           :src (str "/api/video/frame?source=" (js/encodeURIComponent (source-param source))
                                     "&width=" width "&v=" (:token @shown))}]))))

(defn- editor-canvas [mapping samples ui]
  (let [drag (r/atom nil)]
    (fn [mapping samples ui]
      (let [[width height] (or (:size samples) [1600 900])
            aspect (/ width height)
            selected (:strip ui)
            draft (when (= (:id mapping) (:draft-mapping ui)) (:draft ui))
            strips (mapv #(if (and draft (= (:id %) (:id draft))) draft %) (:strips mapping))
            colors (into {} (map (juxt :id :colors)) (:strips samples))
            drag! (fn [strip-id]
                    (fn [event op]
                      (.stopPropagation event)
                      (.setPointerCapture (.-currentTarget event) (.-pointerId event))
                      (ui! [:strip] strip-id)
                      (reset! drag {:strip-id strip-id :op op :from (svg-point (.closest (.-target event) "svg") event width height)
                                    ;; Build on the unsettled draft, as button edits do.
                                    :original (some #(when (= strip-id (:id %)) %) strips)})))
            move-strip (fn [event]
                   (when-let [{:keys [op from original]} @drag]
                     (let [[u v] (svg-point (.closest (.-target event) "svg") event width height)
                           strip (case (first op)
                                   :move (edited-strip original [:move [(- u (first from)) (- v (second from))]] aspect)
                                   :point (edited-strip original [:point (second op) [u v]] aspect))]
                       (swap! drag assoc :draft strip)
                       (ui! [:draft-mapping] (:id mapping))
                       (ui! [:draft] strip))))
            ;; A drag on the frame outside the strips draws a sampling
            ;; rectangle: it places the selected strip, or waits for a new
            ;; strip or fixture group when none is selected.
            draw! (fn [event]
                    (when (= 0 (.-button event))
                      (.setPointerCapture (.-currentTarget event) (.-pointerId event))
                      (let [p (svg-point (.closest (.-target event) "svg") event width height)]
                        (reset! drag {:rect [p p]}))))
            move (fn [event]
                   (if-let [[from] (:rect @drag)]
                     (swap! drag assoc :rect [from (svg-point (.closest (.-target event) "svg") event width height)])
                     (move-strip event)))
            finish (fn [_]
                     (when-let [{:keys [draft rect]} @drag]
                       (reset! drag nil)
                       (cond
                         draft (edit! {:op :put-strip :mapping-id (:id mapping) :strip draft})
                         rect (let [[[u0 v0] [u1 v1]] rect]
                                (if (and (< (abs (- u1 u0)) 0.01) (< (abs (- v1 v0)) 0.01))
                                  (do (ui! [:strip] nil) (ui! [:pending-rect] nil))
                                  (let [placed (mapping/rect-strip [u0 v0] [u1 v1] aspect)]
                                    (if selected
                                      (rf/dispatch-sync [::edit-strip (:id mapping) selected [:rect placed] aspect])
                                      (ui! [:pending-rect] placed))))))))]
        [:div.video-canvas {:style {:aspect-ratio (str width " / " height)}}
         (when (:frame? samples) [frame-image (:source mapping) (:sequence samples) 640])
         (when-not (:frame? samples) [:p.video-empty "No frame from this source yet."])
         [:svg.video-overlay {:viewBox (str "0 0 " width " " height) :preserveAspectRatio "none"
                              :on-pointer-move move :on-pointer-up finish :on-pointer-cancel finish
                              :aria-label "Strip mapping over the video frame"}
          [:rect.video-draw-area {:x 0 :y 0 :width width :height height :on-pointer-down draw!}]
          (when-let [{rect-points :points rect-height :height} (and (nil? selected) (:pending-rect ui))]
            [:polygon.video-pending-rect
             {:points (footprint-polygon (first (mapping/footprints rect-points 1 rect-height width height)))}])
          (for [strip strips]
            ^{:key (str (:id strip))}
            [strip-overlay strip (get colors (:id strip) []) width height (= selected (:id strip)) (drag! (:id strip))])
          (when-let [[[u0 v0] [u1 v1]] (:rect @drag)]
            [:rect.video-draw-rect {:x (* width (min u0 u1)) :y (* height (min v0 v1))
                                    :width (* width (abs (- u1 u0))) :height (* height (abs (- v1 v0)))}])]]))))

(defn- strip-editor [mapping strip aspect rig]
  ;; Synchronous: the strip draft changes before a controlled input is restored.
  (let [put! #(rf/dispatch-sync [::edit-strip (:id mapping) (:id strip) % aspect])]
    [:div.video-strip-editor
     [:h3 (str "Strip " (wire-id (:id strip)))
      [:span.small.muted (str " · " (if (:layout strip) (str "layout " (wire-id (:layout strip)))
                                         (str "fixture group of " (count (:targets strip)))))]]
     [:div.form-row
      [:button.text-button {:type "button" :on-click #(put! [:rotate -15])} "↺ 15°"]
      [:button.text-button {:type "button" :on-click #(put! [:rotate 15])} "↻ 15°"]
      [:button.text-button {:type "button" :on-click #(put! [:length 0.9])} "Shorter"]
      [:button.text-button {:type "button" :on-click #(put! [:length (/ 1 0.9)])} "Longer"]
      [:span.small.muted "Drag on the frame to draw this strip's sampling rectangle."]]
     [tools/placement-controls mapping strip aspect]
     (when (contains? strip :targets) [tools/targets-editor mapping strip aspect rig])
     [:button.text-button.danger {:type "button"
                                  :on-click #(edit! {:op :delete-strip :mapping-id (:id mapping) :strip-id (:id strip)})}
      "Remove strip"]]))

(defn panel []
  (let [settings @(rf/subscribe [::settings]) status @(rf/subscribe [::status])
        ui @(rf/subscribe [::ui]) rig @(rf/subscribe [::rig])
        source (or (:source ui) (first (sort-by str (keys (:sources settings)))))
        mapping (some #(when (= source (:source %)) %) (sort-by (comp str :id) (vals (:mappings settings))))
        samples (get-in status [:mappings (:id mapping)])
        [width height] (or (:size samples) [1600 900])
        draft (when (= (:id mapping) (:draft-mapping ui)) (:draft ui))
        strip (if (and draft (= (:strip ui) (:id draft)))
                draft
                (some #(when (= (:strip ui) (:id %)) %) (:strips mapping)))
        used (set (keep :layout (:strips mapping)))
        layouts (remove #(used (:id %)) (layout-choices rig))]
    [:section#video-strips.panel {:aria-label "Video strip mapping"}
     [:div.panel-top [:span.section-label "VIDEO STRIPS"]
      [:span.small.muted (if (:frozen? status) "Transport stopped · frames held" "Sampled from the latched frame")]
      [tools/history-bar (:history status)]]
     [:div.form-body
      [sources-panel settings status (assoc ui :source source)]
      (cond
        (nil? source) [:p.small.muted "Add a test grid, or a Syphon source from Resolume or another app on this Mac."]
        (nil? mapping)
        [:button.button {:type "button"
                         :on-click #(edit! {:op :put-mapping :mapping {:id (keyword "video" (wire-id source)) :source source :strips []}})}
         "Map strips on this source"]
        :else
        [:<>
         [editor-canvas mapping samples ui]
         [:div.form-row
          (if (seq layouts)
            [:label "Add a strip"
             [:select {:value "" :aria-label "Add a pixel layout as a strip"
                       :on-change #(let [id (some (fn [l] (when (= (.. % -target -value) (wire-id (:id l))) (:id l))) layouts)]
                                     (when id
                                       (edit! {:op :put-strip :mapping-id (:id mapping)
                                               ;; On the pending rectangle when one is drawn.
                                               :strip (merge (new-strip mapping id) (select-keys (:pending-rect ui) [:points :height]))})
                                       (ui! [:pending-rect] nil)
                                       (ui! [:strip] id)))}
              [:option {:value ""} "Choose a pixel layout…"]
              (for [{:keys [id label pixels]} layouts]
                ^{:key (str id)} [:option {:value (wire-id id)} (str label " · " pixels " px")])]]
            [:p.small.muted "Every pixel layout already has a strip on this source."])]
         (when (and (:pending-rect ui) (nil? strip))
           [:p.small.muted.video-pending-note {:role "status"}
            "Rectangle drawn. Add a pixel layout or place a fixture group to sample it, or click the frame to clear it."])
         [tools/group-builder mapping ui rig]
         (when strip [strip-editor mapping strip (/ width height) rig])
         (let [unresolved (filter :unresolved? (:strips samples))]
           (when (seq unresolved)
             [:p.small.muted (str "No patched pixels for " (str/join ", " (map (comp wire-id :id) unresolved)) ".")]))])
      (when-let [error (:error ui)] [:p.performance-error {:role "alert"} error])]]))
