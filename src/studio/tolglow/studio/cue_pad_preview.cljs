(ns tolglow.studio.cue-pad-preview
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.state-socket :as socket]))

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

(defn- text-id [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn- cue-label [cues cue-id]
  (let [id (text-id cue-id)]
    (or (some #(when (= id (text-id (:id %))) (:name %)) cues) id)))

(def ^:private stage-text
  {"plan" "cannot be compiled for its preview head"
   "parameters" "parameters did not resolve"
   "sample" "preview failed"})

(def ^:private listed-faults 4)

(defn fault-lines
  "What the preview fault notice says, from the cue pad query result and the
  show status: the whole query or its lease failing, sampling that is behind
  (a :stale result), each faulted cue on the page (a few by name, then a count) and response-sample lease faults (the
  messages of the records show/response-sample-faults returns at the REPL,
  and whether each repeated). Empty when nothing
  failed. A faulted cue has no preview colour; this says why."
  [result cues lease-faults]
  (let [refused (get-in result [:value :fault])
        cue-faults (sort-by (comp text-id key) (get-in result [:value :faults]))
        extra (- (count cue-faults) listed-faults)]
    (cond-> []
      (socket/behind? result)
      (conj (socket/stale-note "The pad preview for this page" result))
      (and (:error result) (not (socket/behind? result)))
      (conj "Pad previews unavailable: the show did not sample this page.")
      refused
      (conj (str "Pad previews unavailable: " (:message refused) "."))
      (seq cue-faults)
      (into (for [[cue-id {:keys [stage message]}] (take listed-faults cue-faults)]
              (str (cue-label cues cue-id) " " (get stage-text (text-id stage) "preview failed") ": " message)))
      (pos? extra)
      (conj (str extra " more " (if (= 1 extra) "pad preview" "pad previews") " failed."))
      (seq lease-faults)
      (into (for [{:keys [message repeated?]} lease-faults]
              (str "Parameter sample lease " (if repeated? "keeps failing" "failed") ": " message))))))

(defn fault-notice
  "Say which pad previews failed and why, instead of leaving a colour
  unexplained."
  [lines]
  (when (seq lines)
    [:div.cue-preview-faults {:role "status" :aria-live "polite"}
     (for [[i line] (map-indexed vector lines)] ^{:key i} [:p line])]))

(defn grid
  "Subscribe to the contiguous rectangle of visible rows. A tab change, window
  blur or scrolling the grid offscreen disposes its remote reaction. Preview
  and response-sample lease faults are shown below the grid."
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
          result (when shown
                   @(rf/subscribe [:studio/remote-result [:visualization/cue-pads page-id [0 (first shown)]
                                                          (inc (- (last shown) (first shown)))]]))
          colors (get-in result [:value :colors])
          faults (fault-lines result (get-in db [:show :cues]) (get-in db [:show :status :response-sample-faults]))]
      [:<>
       [:div#cue-grid.cue-grid {:role "group" :aria-label "Eight-column cue grid" :ref follow
                                :data-preview-faults (when (seq faults) (count faults))}
        (for [y (range rows)]
          ^{:key y}
          [:div.cue-preview-row {:data-row y :ref (refs y)}
           (for [x (range 8)] ^{:key x} [render-cell db page x y (cell-colors colors page x y)])])]
       [fault-notice faults]])
    (finally
      (when observer (.disconnect observer))
      (unlisten! @doc))))
