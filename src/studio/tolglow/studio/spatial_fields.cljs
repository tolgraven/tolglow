(ns tolglow.studio.spatial-fields
  (:require [reagent.core :as r]))

(defn number-input [caption value commit options]
  (r/with-let [draft (r/atom nil)]
    [:label [:span caption]
     [:input (merge {:type "number" :step "any" :aria-label caption :value (if (nil? @draft) (or value "") @draft)
                     :on-change #(reset! draft (.. % -target -value))
                     :on-blur #(let [v (.. % -target -value) n (js/Number v)]
                                 (when (and @draft (seq v) (js/Number.isFinite n) (.checkValidity (.-target %))) (commit n))
                                 (reset! draft nil))
                     :on-key-down #(when (= "Enter" (.-key %)) (.blur (.-target %)))} options)]]))

(defn xyz [caption values commit & [options]]
  [:fieldset.spatial-xyz [:legend caption]
   (for [[axis i] [["X" 0] ["Y" 1] ["Z" 2]]]
     ^{:key i} [number-input (str caption " " axis) (get values i)
                #(commit (assoc (or values [0 0 0]) i %)) options])])
