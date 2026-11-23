(ns tolglow.studio.visibility
  (:require [reagent.core :as r]))

(defn watch-visibility
  "Render f with true only while the element intersects the viewport. The
  caller decides which reactions exist; scrolling off releases their leases."
  [f]
  (r/with-let [seen (r/atom false) observer (atom nil) node (atom nil)
               foreground (r/atom (not (.-hidden js/document)))
               visibility! #(reset! foreground (not (.-hidden js/document)))
               _ (.addEventListener js/document "visibilitychange" visibility!)
               attach (fn [el]
                        (when-not (identical? el @node)
                          (reset! node el)
                          (when @observer (.disconnect @observer) (reset! observer nil))
                          (when el
                            (if (exists? js/IntersectionObserver)
                              (let [watch (js/IntersectionObserver.
                                            (fn [entries] (reset! seen (.-isIntersecting (aget entries 0)))))]
                                (reset! observer watch) (.observe watch el))
                              (reset! seen true)))))]
    [:div {:ref attach} (f (and @foreground @seen))]
    (finally
      (.removeEventListener js/document "visibilitychange" visibility!)
      (when @observer (.disconnect @observer)))))
