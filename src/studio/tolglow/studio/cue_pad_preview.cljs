(ns tolglow.studio.cue-pad-preview
  (:require [re-frame.core :as rf]
            [reagent.core :as r]))

(defn- foreground?
  "Whether `doc`, the document the grid is in, is visible and has focus. A Cues
  panel detached into its own window (tolglow.studio.panel-window) follows that
  window, not Studio's."
  [^js doc]
  (and (not (.-hidden doc)) (.hasFocus doc)))

(defn- row-observer [visible]
  (when (exists? js/IntersectionObserver)
    (js/IntersectionObserver.
     (fn [entries]
       (swap! visible
              (fn [rows]
                (reduce (fn [rows entry]
                          (let [row (js/Number (.. entry -target -dataset -row))]
                            ((if (.-isIntersecting entry) conj disj) rows row)))
                        rows (array-seq entries))))))))

(defn- row-ref [observer visible row]
  (let [current (volatile! nil)]
    (fn [el]
      (when-let [old @current]
        (when observer (.unobserve observer old))
        (swap! visible disj row))
      (vreset! current el)
      (when el
        (if observer (.observe observer el) (swap! visible conj row))))))

(defn cell-colors
  "The part of a pad preview result for the cue at (x, y) of page, under the
  typed or text ID it may be keyed by. A cell's props then change only when
  its own preview changes, so one animating pad does not re-render the grid."
  [colors page x y]
  (when colors
    (when-let [cue-id (:cue-id (some #(when (and (= x (:x %)) (= y (:y %))) %) (:cells page)))]
      (select-keys colors [cue-id (if (keyword? cue-id) (subs (str cue-id) 1) (str cue-id))]))))

(defn grid
  "Subscribe to the contiguous rectangle of visible rows. A tab change, window
  blur or scrolling the grid offscreen disposes its remote reaction."
  [page-id rows mounted? render-cell db page]
  (r/with-let [visible (r/atom #{})
               doc (atom js/document)
               focused (r/atom (foreground? js/document))
               refresh #(reset! focused (foreground? @doc))
               listen! (fn [^js d]
                         (.addEventListener d "visibilitychange" refresh)
                         (.addEventListener (.-defaultView d) "focus" refresh)
                         (.addEventListener (.-defaultView d) "blur" refresh))
               unlisten! (fn [^js d]
                           (.removeEventListener d "visibilitychange" refresh)
                           (some-> (.-defaultView d) (.removeEventListener "focus" refresh))
                           (some-> (.-defaultView d) (.removeEventListener "blur" refresh)))
               _ (listen! js/document)
               ;; The grid moves between documents when its panel is detached or redocked.
               follow (fn [^js el]
                        (when-let [owner (some-> el .-ownerDocument)]
                          (when-not (identical? owner @doc)
                            (unlisten! @doc)
                            (reset! doc owner)
                            (listen! owner)
                            (refresh))))
               observer (row-observer visible)
               refs (mapv #(row-ref observer visible %) (range 8))]
    (let [shown (when (and mounted? page-id @focused) (seq (sort @visible)))
          colors (when shown
                   @(rf/subscribe [:studio/remote [:visualization/cue-pads page-id [0 (first shown)]
                                                   (inc (- (last shown) (first shown)))]]))]
      [:div#cue-grid.cue-grid {:role "group" :aria-label "Eight-column cue grid" :ref follow}
       (for [y (range rows)]
         ^{:key y}
         [:div.cue-preview-row {:data-row y :ref (refs y)}
          (for [x (range 8)] ^{:key x} [render-cell db page x y (cell-colors colors page x y)])])])
    (finally
      (when observer (.disconnect observer))
      (unlisten! @doc))))
