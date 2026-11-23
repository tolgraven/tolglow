(ns tolglow.studio.cue-pad-preview
  (:require [re-frame.core :as rf]
            [reagent.core :as r]))

(defn- foreground? []
  (and (not (.-hidden js/document)) (.hasFocus js/document)))

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

(defn grid
  "Subscribe to the contiguous rectangle of visible rows. A tab change, window
  blur or scrolling the grid offscreen disposes its remote reaction."
  [page-id rows mounted? render-cell db page]
  (r/with-let [visible (r/atom #{})
               focused (r/atom (foreground?))
               refresh #(reset! focused (foreground?))
               observer (row-observer visible)
               refs (mapv #(row-ref observer visible %) (range 8))
               _ (do (.addEventListener js/document "visibilitychange" refresh)
                     (.addEventListener js/window "focus" refresh)
                     (.addEventListener js/window "blur" refresh))]
    (let [shown (when (and mounted? page-id @focused) (seq (sort @visible)))
          colors (when shown
                   @(rf/subscribe [:studio/remote [:visualization/cue-pads page-id [0 (first shown)]
                                                   (inc (- (last shown) (first shown)))]]))]
      [:div#cue-grid.cue-grid {:role "group" :aria-label "Eight-column cue grid"}
       (for [y (range rows)]
         ^{:key y}
         [:div.cue-preview-row {:data-row y :ref (refs y)}
          (for [x (range 8)] ^{:key x} [render-cell db page x y colors])])])
    (finally
      (when observer (.disconnect observer))
      (.removeEventListener js/document "visibilitychange" refresh)
      (.removeEventListener js/window "focus" refresh)
      (.removeEventListener js/window "blur" refresh))))
