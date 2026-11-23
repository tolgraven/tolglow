(ns tolglow.studio.rig-fields
  (:require [reagent.core :as r]
            [re-frame.core :as rf]))

(defn number-field [label number event & [options]]
  (r/with-let [draft (r/atom nil)]
    (let [commit! (fn [e]
                    (let [text (.. e -target -value)
                          value (js/Number text)]
                      (when (and (seq text) (js/Number.isFinite value))
                        (rf/dispatch-sync (conj event value)))
                      (reset! draft nil)))]
      [:label label
       [:input (merge {:type "number" :step 0.1 :aria-label label :value (or @draft number 0)
                       :on-change #(reset! draft (.. % -target -value))
                       :on-blur commit!
                       :on-key-down #(when (= "Enter" (.-key %)) (.blur (.-target %)))}
                      options)]])))
