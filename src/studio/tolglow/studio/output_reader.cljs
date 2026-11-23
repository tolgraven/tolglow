(ns tolglow.studio.output-reader
  "Own one cursor's remote subscription for the lifetime of its mounted pane."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.projection :as projection]))

(defn reader [query on-page]
  (r/with-let [listen (r/track! (fn []
                                 (when-let [page @(rf/subscribe [:studio/remote-result query])]
                                   (rf/dispatch (conj on-page (projection/presentation page))))))]
    nil
    (finally (r/dispose! listen))))

(defn at [query on-page]
  (with-meta [reader query on-page] {:key (pr-str [query on-page])}))
