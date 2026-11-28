(ns tolglow.studio.video-strip-tools
  "The 2D strip editor's placement and fixture-group tools (#672): numeric
  placement on the standard slot grid, the explicit-target editor for a
  fixture group, and Undo/Redo for video edits. Every change is the same
  `:video-mapping` edit the REPL sends (`show/video-action! runtime :edit
  op`), dispatched through `tolglow.studio.video-strips` so it builds on the
  unsettled strip draft; Undo and Redo are `{:op :undo}` and `{:op :redo}`."
  (:require [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.video-controls :as video-controls]
            [tolglow.video.mapping :as mapping]))

(def wire-id events/wire-id)

(defn fixture-choices
  "Fixtures a group strip can target: every rig fixture that is not a pixel
  of a layout (those are mapped by layout), with a label and position."
  [rig]
  (->> (:fixtures rig)
       (remove :pixel-layout-id)
       (mapv (fn [{:keys [id name position]}] {:id id :label (or name (wire-id id)) :position position}))
       (sort-by (comp str :id))
       vec))

(defn move-target
  "targets with the one at index i moved by delta places, clamped to the ends."
  [targets i delta]
  (let [j (max 0 (min (dec (count targets)) (+ i delta)))]
    (if (or (= i j) (not (< -1 i (count targets))))
      targets
      (let [item (nth targets i) without (vec (concat (subvec targets 0 i) (subvec targets (inc i))))]
        (vec (concat (subvec without 0 j) [item] (subvec without j)))))))

(defn left-to-right
  "targets ordered by their fixtures' x position, then depth; targets the
  rig does not place keep their relative order at the end."
  [targets rig]
  (let [positions (into {} (map (juxt :id :position)) (:fixtures rig))]
    (vec (sort-by (fn [id] (if-let [[x _ z] (get positions id)] [0 x z] [1 0 0])) targets))))

(defn group-id
  "The next free :group-n strip id in a mapping."
  [mapping]
  (let [used (set (map :id (:strips mapping)))]
    (first (remove used (map #(keyword (str "group-" %)) (iterate inc 1))))))

(defn group-strip
  "A new explicit-target strip for a fixture group: on the pending rectangle
  when one is drawn, else across the frame below the strips already mapped."
  [mapping targets rect]
  (let [v (+ 0.15 (* 0.7 (mod (* (count (:strips mapping)) 0.37) 1.0)))]
    (merge {:id (group-id mapping) :targets (vec targets) :points [[0.1 v] [0.9 v]] :height 0.1}
           (select-keys rect [:points :height]))))

;; Numeric placement -----------------------------------------------------------

(def coalescing (atom {}))

(defn- place! [mapping strip aspect control placement]
  (rf/dispatch-sync [:tolglow.studio.video-strips/edit-strip (:id mapping) (:id strip) [:place placement] aspect
                     (video-controls/coalesce-key coalescing [(:id mapping) (:id strip) control] (.now js/Date))]))

(defn placement-controls
  "Centre, length, angle and sampling height of the selected strip as knobs
  on the slot grid, with filter and direction. A knob turned without a
  pause is one undo step."
  [mapping strip aspect]
  (let [{[cu cv] :center :keys [length angle height]} (mapping/strip-placement strip aspect)
        put #(place! mapping strip aspect %1 %2)]
    [video-controls/grid "Strip placement"
     [{:id "centre-u" :title "Centre across" :schema {:type :number :bounds [0 1] :step 0.001} :value cu
       :change! #(put :centre-u {:center [% cv]})}
      {:id "centre-v" :title "Centre down" :schema {:type :number :bounds [0 1] :step 0.001} :value cv
       :change! #(put :centre-v {:center [cu %]})}
      {:id "length" :title "Length" :unit "frame widths" :schema {:type :number :bounds [0 1.2] :step 0.001} :value length
       :change! #(put :length {:length %})}
      {:id "angle" :title "Angle" :unit "°" :schema {:type :number :bounds [-180 180] :step 0.5} :value angle
       :change! #(put :angle {:angle %})}
      {:id "height" :title "Sampling height" :unit "of frame" :schema {:type :number :bounds [0 1] :step 0.005} :value height
       :change! #(put :height {:height %})}
      {:id "filter" :kind :choice :title "Filter" :value (or (:filter strip) (:filter mapping) :box)
       :choices [[:box "Average footprint"] [:nearest "Nearest pixel"]]
       :change! #(rf/dispatch-sync [:tolglow.studio.video-strips/edit-strip (:id mapping) (:id strip) [:filter %] aspect])}
      {:id "reverse" :kind :toggle :title "First pixel at the far end" :value (:reverse? strip)
       :change! #(rf/dispatch-sync [:tolglow.studio.video-strips/edit-strip (:id mapping) (:id strip) [:reverse %] aspect])}]]))

;; Fixture groups --------------------------------------------------------------

(defn- target-row [mapping strip aspect choices i id n]
  (let [put! #(rf/dispatch-sync [:tolglow.studio.video-strips/edit-strip (:id mapping) (:id strip) [:targets %] aspect])
        targets (:targets strip)]
    [:li.video-target
     [:span.video-target-index (inc i)]
     [:span.video-target-name (or (some #(when (= id (:id %)) (:label %)) choices) (wire-id id))]
     [:button.icon-button {:type "button" :aria-label (str "Move " (wire-id id) " earlier") :disabled (zero? i)
                           :on-click #(put! (move-target targets i -1))} "↑"]
     [:button.icon-button {:type "button" :aria-label (str "Move " (wire-id id) " later") :disabled (= i (dec n))
                           :on-click #(put! (move-target targets i 1))} "↓"]
     [:button.icon-button {:type "button" :aria-label (str "Remove " (wire-id id) " from the group") :disabled (= n 1)
                           :on-click #(put! (vec (concat (subvec targets 0 i) (subvec targets (inc i)))))} "×"]]))

(defn targets-editor
  "The explicit targets of a fixture-group strip in sampling order: the
  first target samples the first point's end. Reorder, remove, add a
  fixture or order them left to right in the room."
  [mapping strip aspect rig]
  (let [choices (fixture-choices rig)
        targets (vec (:targets strip))
        put! #(rf/dispatch-sync [:tolglow.studio.video-strips/edit-strip (:id mapping) (:id strip) [:targets %] aspect])
        unused (remove (comp (set targets) :id) choices)]
    [:div.video-targets {:role "group" :aria-label "Fixture group targets"}
     [:h4 (str (count targets) " fixtures in sampling order")]
     [:ol.video-target-list
      (for [[i id] (map-indexed vector targets)]
        ^{:key (str id)} [target-row mapping strip aspect choices i id (count targets)])]
     [:div.form-row
      [:label "Add a fixture"
       [:select {:value "" :aria-label "Add a fixture to the group"
                 :on-change #(let [v (.. % -target -value)]
                               (when-let [id (some (fn [c] (when (= v (wire-id (:id c))) (:id c))) unused)]
                                 (put! (conj targets id))))}
        [:option {:value ""} (if (seq unused) "Choose a fixture…" "Every fixture is in the group")]
        (for [{:keys [id label]} unused] ^{:key (str id)} [:option {:value (wire-id id)} label])]]
      [:button.text-button {:type "button" :disabled (< (count targets) 2)
                            :on-click #(put! (left-to-right targets rig))} "Order left to right in the room"]
      [:button.text-button {:type "button" :disabled (< (count targets) 2)
                            :on-click #(put! (vec (rseq targets)))} "Reverse order"]]]))

(defn group-builder
  "Choose fixtures for a new group strip, then place it on the pending
  rectangle or across the frame. The choice is local until placed, since a
  group strip needs at least one target."
  [mapping ui rig]
  (let [choices (fixture-choices rig)
        chosen (vec (:group-draft ui))
        choose! #(rf/dispatch-sync [:tolglow.studio.video-strips/ui [:group-draft] %])]
    (when (seq choices)
      [:details.video-group-builder {:open (boolean (seq chosen))}
       [:summary "Map a fixture group"]
       [:p.small.muted "Tick fixtures in the order the strip should sample them. The first samples the start of the line."]
       [:div.video-group-choices
        (for [{:keys [id label]} choices
              :let [i (.indexOf chosen id)]]
          ^{:key (str id)}
          [:label.video-check
           [:input {:type "checkbox" :checked (>= i 0) :aria-label (str "Include " (wire-id id))
                    :on-change #(choose! (if (>= i 0) (filterv (partial not= id) chosen) (conj chosen id)))}]
           (str " " label (when (>= i 0) (str " · " (inc i))))])]
       [:div.form-row
        [:button.button {:type "button" :disabled (empty? chosen)
                         :on-click #(let [strip (group-strip mapping chosen (:pending-rect ui))]
                                      (rf/dispatch [:tolglow.studio.video-strips/edit {:op :put-strip :mapping-id (:id mapping) :strip strip}])
                                      (rf/dispatch-sync [:tolglow.studio.video-strips/ui [:strip] (:id strip)])
                                      (rf/dispatch-sync [:tolglow.studio.video-strips/ui [:pending-rect] nil])
                                      (choose! []))}
         (if (:pending-rect ui) "Place group on the rectangle" "Place group")]
        (when (seq chosen) [:button.text-button {:type "button" :on-click #(choose! [])} "Clear"])]])))

;; History ---------------------------------------------------------------------

(defn history-bar
  "Undo and Redo for video settings edits, shared with the REPL."
  [history]
  [:div.video-history {:role "group" :aria-label "Video edit history"}
   [:button.text-button {:type "button" :disabled (zero? (:undo history 0))
                         :title (when-let [l (:undo-label history)] (str "Undo " l))
                         :on-click #(rf/dispatch [:tolglow.studio.video-strips/edit {:op :undo}])}
    (str "Undo" (when-let [l (:undo-label history)] (str " " l)))]
   [:button.text-button {:type "button" :disabled (zero? (:redo history 0))
                         :title (when-let [l (:redo-label history)] (str "Redo " l))
                         :on-click #(rf/dispatch [:tolglow.studio.video-strips/edit {:op :redo}])}
    (str "Redo" (when-let [l (:redo-label history)] (str " " l)))]])
