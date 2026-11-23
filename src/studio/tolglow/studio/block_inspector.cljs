(ns tolglow.studio.block-inspector
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.ratom :as ratom]
            [tolglow.studio.block-trace :as trace]
            [tolglow.studio.spatial-preview :refer [watch-visibility]]))

(rf/reg-sub ::fixtures (fn [db _] (get-in db [:server-projection :view :rig :fixtures])))

(rf/reg-sub-raw ::history
  (fn [db [_ query]]
    (let [remote (rf/subscribe [:studio/remote query])
          epoch (r/cursor db [:show :epoch])
          runtime (r/cursor db [:show :runtime-id])
          last-id (volatile! nil)
          samples (volatile! [])]
      (ratom/make-reaction
       (fn []
         (let [id [@runtime @epoch]]
           (when (not= id @last-id)
             (vreset! last-id id)
             (vreset! samples []))
           (vswap! samples trace/append-sample @remote)))))))

(defn- selected [options chosen] (or (some #{chosen} options) (first options)))

(defn- choice [title options value label on-change]
  [:label title
   [:select {:aria-label title :value (str (first (keep-indexed #(when (= %2 value) %1) options)))
             :on-change #(on-change (get options (parse-long (.. % -target -value))))}
    (for [[i option] (map-indexed vector options)]
      ^{:key i} [:option {:value i} (label option)])]])

(defn history-plot
  "Subscribe only while mounted. The caller owns visibility."
  [query]
  [trace/plot @(rf/subscribe [::history query]) false {:binding-path (nth query 2)}])

(defn- observe [instance-id path]
  (r/with-let [target (r/atom nil)]
    (let [fixtures @(rf/subscribe [::fixtures])
          ids (into [nil] (map :id) fixtures)
          target-id (selected ids @target)
          names (into {} (map (juxt :id #(or (:name %) (trace/path-label [(:id %)])))) fixtures)
          query (cond-> [:runtime/block-trace instance-id path] target-id (conj target-id))]
      [:<>
       [choice "Fixture" ids target-id #(get names % "Origin") #(reset! target %)]
       [:div.block-trace-viewport
        [watch-visibility (fn [visible?] (when visible? [history-plot query]))]]])))

(defn inspector [instance-id blocks]
  (r/with-let [open? (r/atom false) chosen (r/atom nil)]
    (let [paths (vec (sort-by trace/path-label (keys blocks)))
          path (selected paths @chosen)]
      [:section.block-inspector
       [:button.text-button {:type "button" :aria-expanded @open?
                             :on-click #(swap! open? not)}
        "Block trace"]
       (when (and @open? path)
         [:div.block-inspector-controls
          [choice "Binding" paths path trace/path-label #(reset! chosen %)]
          [observe instance-id path]])])))
