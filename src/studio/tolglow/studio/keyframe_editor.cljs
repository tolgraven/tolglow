(ns tolglow.studio.keyframe-editor
  "Touch and keyboard editing of the same easing data sampled by render workers."
  (:require [afterglow.render.easing :as easing]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [reagent.core :as r]))

(def defaults {:interval {:beats 1} :curve :linear})
(def curve-types [:linear :smoothstep :sine :ease-in :ease-out :ease-in-out :step :cubic-bezier :spring])
(defn- title [k] (-> (name k) (str/replace "-" " ") str/capitalize))
(defn- text [e] (.. e -target -value))
(defn- limit [lo x hi] (max lo (min x hi)))

(defn- choose-curve [kind]
  (case kind
    :cubic-bezier {:type kind :points [0.25 0.1 0.25 1.0]}
    :spring {:type kind :frequency 1.5 :damping 0.45}
    kind))

(defn ratio-value [s]
  (when-let [[_ top bottom] (re-matches #"\s*(\d*\.?\d+)(?:\s*/\s*(\d*\.?\d+))?\s*" s)]
    (let [n (/ (js/Number top) (if bottom (js/Number bottom) 1))]
      (when (and (js/Number.isFinite n) (pos? n)) n))))

(defn- select [label value options change]
  [:label [:span label]
   [:select {:aria-label label :value (name value) :on-change #(change (keyword (text %)))}
    (for [k options] ^{:key k} [:option {:value (name k)} (title k)])]])

(defn- screen-point [svg event]
  (let [p (.createSVGPoint svg)]
    (set! (.-x p) (.-clientX event))
    (set! (.-y p) (.-clientY event))
    (let [local (.matrixTransform p (.inverse (.getScreenCTM svg)))] [(.-x local) (.-y local)])))

(defn move-handle [points i x y [lo hi]]
  (-> points (assoc i (limit 0.0 (/ (- x 18) 264.0) 1.0))
      (assoc (inc i) (limit lo (- hi (* (/ (- y 12) 148.0) (- hi lo))) hi))))

(defn- chart [curve change]
  (r/with-let [drag (r/atom nil)]
    (let [points (when (= :cubic-bezier (:type curve)) (:points curve))
          f (easing/evaluator curve)
          samples (mapv (fn [i] (let [x (/ i 72)] [x (f x)])) (range 73))
          ys (into [-0.4 1.4] (map second) samples)
          ys (if points (conj ys (points 1) (points 3)) ys)
          [lo hi :as domain] (or (:domain @drag) [(reduce min ys) (reduce max ys)])
          sx #(+ 18 (* 264 %)) sy #(+ 12 (* 148 (/ (- hi %) (- hi lo))))
          path (apply str (map-indexed (fn [i [x y]]
                                         (str (if (zero? i) "M" "L") (sx x) "," (sy y))) samples))
          end (fn [e cancel?]
                (when (= (:pointer @drag) (.-pointerId e))
                  (when cancel? (change (assoc curve :points (:before @drag))))
                  (reset! drag nil)))]
      [:svg.keyframe-graph {:viewBox "0 0 300 175" :role "group" :aria-label "Tween response curve"
                            :on-pointer-cancel #(end % true) :on-pointer-up #(end % false)
                            :on-pointer-move
                            (fn [e] (when (= (:pointer @drag) (.-pointerId e))
                                      (let [[x y] (screen-point (.-currentTarget e) e)]
                                        (change (assoc curve :points (move-handle points (:i @drag) x y domain))))))}
       (for [y [0 1]] ^{:key y} [:line.grid {:x1 18 :x2 282 :y1 (sy y) :y2 (sy y)}])
       [:path.signal {:d path}]
       (when points
         (for [[i ax ay] [[0 0 0] [2 1 1]] :let [x (sx (points i)) y (sy (points (inc i)))]]
           ^{:key i} [:g [:line.handle-link {:x1 (sx ax) :y1 (sy ay) :x2 x :y2 y}]
                      [:circle.control {:cx x :cy y :r 5.5}]
                      [:circle.hit {:cx x :cy y :r 16
                                    :on-pointer-down (fn [e]
                                                       (let [svg (.. e -currentTarget -ownerSVGElement)]
                                                         (.setPointerCapture svg (.-pointerId e))
                                                         (reset! drag {:i i :domain domain :before points
                                                                       :pointer (.-pointerId e)})))}]]))])))

(defn- numeric [label value low high change]
  [:label [:span label]
   [:input {:type "number" :aria-label label :step "any" :min low :max high
            :value value :on-change (fn [e]
                                     (let [raw (text e) n (js/Number raw)]
                                       (when (and (seq raw) (js/Number.isFinite n)
                                                  (or (nil? low) (<= low n)) (or (nil? high) (<= n high)))
                                         (change n))))}]])

(defn- progress [spec params change]
  (let [source (:progress spec)
        mode (:mode source :timeline)
        parameter? (contains? source :param)]
    [:div.keyframe-fields
     [select "Progress" mode [:timeline :replace :offset]
      #(change (if (= % :timeline) (dissoc spec :progress)
                   (assoc spec :progress (assoc (or source {:variable [:audio :low]}) :mode %))))]
     (when source
       [:<>
        [select "Progress source" (if parameter? :parameter :show-variable)
         (cond-> [:show-variable] (seq params) (conj :parameter))
         #(change (assoc spec :progress (merge {:mode mode}
                                               (if (= % :parameter) {:param (first params)}
                                                   {:variable [:audio :low]}))))]
        (if parameter?
          [:label [:span "Parameter"]
           [:select {:aria-label "Tween progress parameter" :value (str (:param source))
                     :on-change (fn [e] (when-let [id (some #(when (= (str %) (text e)) %) params)]
                                          (change (assoc-in spec [:progress :param] id))))}
            (for [id params] ^{:key (str id)} [:option {:value (str id)} (str id)])]]
          [:label [:span "Variable path (EDN)"]
           ^{:key (pr-str (:variable source))}
           [:input {:aria-label "Tween progress variable path" :default-value (pr-str (:variable source))
                    :on-blur (fn [e]
                               (let [el (.-target e) v (try (reader/read-string (text e)) (catch :default _ nil))]
                                 (if (and (vector? v) (seq v) (every? #(or (keyword? %) (string? %) (integer? %)) v))
                                   (do (.setCustomValidity el "") (change (assoc-in spec [:progress :variable] v)))
                                   (do (.setCustomValidity el "Enter a nonempty EDN path vector.") (.reportValidity el)))))}]])])]))

(defn editor [spec params change]
  (let [curve (:curve spec :linear) kind (or (:type curve) curve)
        [unit amount] (first (:interval spec))]
    [:section.keyframe-editor {:aria-label "Effect keyframes"}
     [:label.checkbox-label [:input {:type "checkbox" :checked (boolean spec)
                                     :on-change #(change (when (.. % -target -checked) defaults))}]
      "Use effect keyframes"]
     (when spec
       [:<>
        [:div.keyframe-fields
         [:label [:span "Keyframe interval"]
          ^{:key (str unit amount)}
          [:input {:aria-label "Keyframe interval" :default-value (str amount)
                   :on-blur #(if-let [value (ratio-value (text %))]
                               (do (.setCustomValidity (.-target %) "") (change (assoc spec :interval {unit value})))
                               (do (.setCustomValidity (.-target %) "Use a positive duration or fraction.")
                                   (.reportValidity (.-target %))))}]]
         [select "Interval unit" unit [:beats :bars :phrases :ms] #(change (assoc spec :interval {% amount}))]
         [select "Tween curve" kind curve-types #(change (assoc spec :curve (choose-curve %)))]]
        [chart curve #(change (assoc spec :curve %))]
        (case kind
          :cubic-bezier [:div.keyframe-coordinates
                         (for [[i label] (map-indexed vector ["First X" "First Y" "Second X" "Second Y"])]
                           ^{:key label} [numeric label (nth (:points curve) i) (when (even? i) 0) (when (even? i) 1)
                                          #(change (assoc-in spec [:curve :points i] %))])]
          :spring [:div.keyframe-fields
                   [numeric "Spring frequency" (:frequency curve 1.5) 0.01 16 #(change (assoc-in spec [:curve :frequency] %))]
                   [numeric "Spring damping" (:damping curve 0.45) 0 8 #(change (assoc-in spec [:curve :damping] %))]]
          nil)
        [progress spec params change]])]))
