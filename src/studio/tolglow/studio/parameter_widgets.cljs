(ns tolglow.studio.parameter-widgets
  "Pure presentation shared by the web inspector and device displays."
  (:require [clojure.string :as str]))

(defn label [value]
  (str/replace (cond (keyword? value) (name value) (nil? value) "—" :else (str value)) #"[-_]" " "))
(defn finite? [value] (and (number? value) (js/Number.isFinite value)))
(defn number-label [value]
  (if (finite? value)
    (str (js/Number (.toPrecision value 5))) "—"))
(defn value-label [schema value]
  (or (some #(when (= value (:value %)) (or (:label %) (label (:id %)))) (:choices schema))
      (cond (boolean? value) (if value "On" "Off")
            (number? value) (number-label value)
            (sequential? value) (str/join " · " (map #(if (number? %) (number-label %) (label %)) value))
            :else (label value))))
(defn color-css [value]
  (cond
    (and (string? value) (re-matches #"#[a-fA-F0-9]{3}(?:[a-fA-F0-9]{3})?(?:[a-fA-F0-9]{2})?" value)) value
    (and (sequential? value) (<= 3 (count value)) (every? finite? (take 3 value)))
    (str "rgb(" (str/join "," (map #(js/Math.round (* 255 (max 0 (min 1 %)))) (take 3 value))) ")")
    :else "#b6c9a9"))
(defn proportion [schema value]
  (let [[low high] (:bounds schema)]
    (when (and (finite? value) (finite? low) (finite? high) (< low high))
      (max 0 (min 1 (/ (- value low) (- high low)))))))
(defn sample-points
  "Bound and normalize real supplied samples. Missing/nonfinite samples produce
  no trace, and a constant signal remains centered. No synthetic samples."
  [samples]
  (let [values (->> samples (take 256) (map #(if (map? %) (:value %) %)) vec)]
    (when (and (< 1 (count values)) (every? finite? values))
      (let [low (apply min values) high (apply max values) span (- high low)]
        (str/join " " (map-indexed (fn [i value]
                                    (str (* 100 (/ i (dec (count values)))) ","
                                         (if (pos? span) (- 28 (* 24 (/ (- value low) span))) 16))) values))))))
(defn sparkline [samples]
  (when-let [points (sample-points samples)]
    [:svg.parameter-sparkline {:view-box "0 0 100 32" :preserve-aspect-ratio "none"
                               :role "img" :aria-label "Signal preview from supplied samples"}
     [:polyline {:points points :fill "none" :stroke "currentColor" :stroke-width 1.6 :vector-effect "non-scaling-stroke"}]]))
(defn duration-label [duration]
  (if-let [[unit amount] (when (map? duration) (first duration))]
    (str (number-label amount) " " (label unit)) "—"))
(defn category-tags [categories colors]
  [:span.category-tags
   (for [category (take 8 categories)]
     ^{:key (str category)}
     [:span.category-tag {:style {"--category-color" (or (get colors category)
                                                         (get colors (keyword (str category))) "#a8b99a")}}
      (label category)])])
