(ns tolglow.studio.block-trace
  "Shared plot for an observed native-value transform chain."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [tolglow.studio.parameter-widgets :as widgets]))

(def sample-limit 96)

(defn append-sample [samples sample]
  (let [now (:time-ms sample)
        previous (:time-ms (peek samples))]
    (cond
      (not (widgets/finite? now)) []
      (or (nil? previous) (< now previous)) [sample]
      (= now previous) (if (= sample (peek samples)) samples (conj (pop samples) sample))
      :else (conj (vec (take-last (dec sample-limit) samples)) sample))))

(defn- binding-label [id]
  (if (keyword? id) (subs (str id) 1) (str id)))

(defn path-label [path] (str/join " / " (map binding-label path)))

(defn- component [value axis]
  (let [n (if (sequential? value) (nth value axis nil) (when (zero? axis) value))]
    (when (widgets/finite? n) n)))

(defn- stage-value [s index field] (get-in s [:stages index field]))

(defn lines [samples stage axis]
  (let [readings (fn [f] (mapv #(component (f %) axis) samples))]
    [[:base "Dry" (readings :base)]
     [:offset "Offset" (readings #(stage-value % stage :offset))]
     [:stage "Stage result" (readings #(stage-value % stage :value))]
     [:result "Final" (readings :result)]]))

(defn- fmt [n] (if (nil? n) "—" (str (js/Number (.toPrecision n 3)))))

(defn- bounds [series]
  (let [ns (into [0] (comp (mapcat last) (keep identity)) series)
        lo (apply min ns) hi (apply max ns)]
    (if (= hi lo) [(- lo 0.5) (+ hi 0.5)] [lo hi])))

(defn- segments [xs ys]
  (->> (map (fn [x y] (when (some? y) (str x "," y))) xs ys)
       (partition-by some?)
       (keep #(when (first %) (str/join " " %)))))

(defn- graph [samples series]
  (let [[low high] (bounds series)
        start (:time-ms (first samples))
        elapsed (max 1 (- (:time-ms (peek samples)) start))
        xs (mapv #(+ 4 (* 392 (/ (- (:time-ms %) start) elapsed))) samples)
        y #(- 76 (* 70 (/ (- % low) (- high low))))]
    [:div.block-trace-grid
     [:div.block-trace-scale [:span (fmt high)] [:span (fmt low)]]
     [:svg {:view-box "0 0 400 82" :preserve-aspect-ratio "none" :role "img"
            :aria-label "Dry value, signed offset, stage result and final value by show time"}
      [:line.block-trace-zero {:x1 0 :x2 400 :y1 (y 0) :y2 (y 0)}]
      (for [[key _ values] series
            :let [ys (mapv #(when (some? %) (y %)) values)]]
        ^{:key key} [:g {:class (str "block-trace-" (name key))}
                     (for [[i points] (map-indexed vector (segments xs ys))]
                       ^{:key i} [:polyline {:points points :vector-effect "non-scaling-stroke"}])
                     (when-let [last-y (peek ys)]
                       [:circle {:cx (peek xs) :cy last-y :r 2}])])]]))

(defn- axis-name [path i]
  (let [color? (#{"color" "rgb" "rgbw"} (some-> path last binding-label))]
    (or (get (if color? ["Red" "Green" "Blue" "White"] ["X" "Y" "Z" "W"]) i) (str (inc i)))))

(defn plot [samples compact? & [{:keys [binding-path stage-index component-index]}]]
  (r/with-let [stage (r/atom 0) axis (r/atom 0)]
    (if-let [latest (peek samples)]
      (let [stages (:stages latest)
            selected (max 0 (min (or stage-index @stage) (max 0 (dec (count stages)))))
            axes (max 1 (count (when (sequential? (:result latest)) (:result latest))))
            active-axis (max 0 (min (or component-index @axis) (dec axes)))
            series (lines samples selected active-axis)]
        [:section.block-trace {:aria-label "Block signal trace" :class (when compact? "compact")}
         [:header.block-trace-tools
          (if (and (not compact?) (seq stages))
            [:label "Stage" [:select {:aria-label "Trace stage" :value selected
                                      :on-change #(reset! stage (parse-long (.. % -target -value)))}
                             (for [[i s] (map-indexed vector stages)]
                               ^{:key i} [:option {:value i} (str (inc i) " · " (binding-label (:block s)))])]]
            [:strong (some-> (get stages selected) :block binding-label)])
          (when (> axes 1)
            (if compact?
              [:span (axis-name binding-path active-axis)]
              [:label "Component" [:select {:value active-axis :aria-label "Trace component"
                                            :on-change #(reset! axis (parse-long (.. % -target -value)))}
                                   (for [i (range axes)] ^{:key i} [:option {:value i} (axis-name binding-path i)])]]))]
         [graph samples series]
         [:div.block-trace-legend
          (for [[id title values] series]
            ^{:key id} [:span {:class (str "block-trace-" (name id))}
                        [:i] title " " [:b (fmt (peek values))]])]])
      [:p.block-trace-empty {:role "status"} "Waiting for a block observation…"])))
