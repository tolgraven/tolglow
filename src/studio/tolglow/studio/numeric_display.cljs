(ns tolglow.studio.numeric-display
  (:require [afterglow.control.resolution :as resolution]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(defn finite? [v] (and (number? v) (js/Number.isFinite v)))

(defn channel-step [{:keys [dmx-bits bounds]}]
  (let [[lo hi] bounds]
    (resolution/fine-step dmx-bits (if (and (finite? lo) (finite? hi) (< lo hi)) bounds [0 1]))))

(defn number-label
  "Format an operator readout without changing the control value. A normalized
  16-bit control needs five decimal places to distinguish adjacent DMX codes."
  ([value] (number-label {} value))
  ([schema value]
   (if-not (finite? value)
     "—"
     (let [bits (:dmx-bits schema)
           places (if-let [step (channel-step schema)]
                    (max 0 (min 15 (js/Math.ceil (- (js/Math.log10 step)))))
                    3)
           rounded (js/Number (.toFixed value places))]
       (if (and (zero? rounded) (not (zero? value)) (nil? bits))
         (let [[mantissa exponent] (.split (.toExponential value 2) "e")]
           (str (js/Number mantissa) "e" exponent))
         (str rounded))))))

(defn number-input [attrs]
  (r/with-let [field-id (or (:id attrs) (str (random-uuid)))]
    (if (= "number" (:type attrs))
      (let [editing @(rf/subscribe [:studio/numeric-edit field-id])
            value (:value attrs)
            shown (if (finite? value) (number-label (:display-schema attrs) value) value)]
        [:input (assoc (dissoc attrs :display-schema)
                  :value (if (some? editing) editing shown)
                  :on-focus #(rf/dispatch-sync [:studio/numeric-edit field-id (str (or value ""))])
                  :on-change (fn [event]
                               (rf/dispatch-sync [:studio/numeric-edit field-id (.. event -target -value)])
                               (when-let [change (:on-change attrs)] (change event)))
                  :on-blur #(rf/dispatch-sync [:studio/numeric-blur field-id]))])
      [:input (dissoc attrs :display-schema)])
    (finally (rf/dispatch [:studio/numeric-blur field-id]))))
