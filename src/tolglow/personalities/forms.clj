(ns tolglow.personalities.forms
  "The shape language a personality weights. A form names the existing input it
  stands for in each family: a sculpt morph, a spatial volume, an aim-path
  figure and a registered waveform. A form with no input for a family is
  skipped there, so weights are preferences and never requirements."
  (:require [afterglow.render.ir :as ir]
            [tolglow.signals :as signals]))

(def order
  "Catalogue order. Seeded selection walks cumulative weights in this order."
  [:square :rectangle :bar :squircle :pill :circle :ring :spiral :zigzag :sawtooth :triangle])

(def families [:sculpt :volume :aim-path :waveform])

(def ^:private waveforms
  (delay (into {} (map (fn [{:keys [id] :as definition}]
                         [id (assoc definition :sample (ir/compile-expression (:expression definition)))]))
               (signals/builtin-waveforms))))

(defn- turn [fraction] (* 2.0 Math/PI fraction))

(defn- superellipse [exponent samples]
  (let [power (/ 2.0 exponent)
        axis (fn [v] (* (Math/signum (double v)) (Math/pow (abs v) power)))]
    (mapv (fn [i] (let [angle (turn (/ i (double samples)))]
                    [(axis (Math/cos angle)) (axis (Math/sin angle))]))
          (range samples))))

(defn- arc [[cx cy] radius from to samples]
  (mapv (fn [i] (let [angle (turn (+ from (* (- to from) (/ i (double (dec samples))))))]
                  [(+ cx (* radius (Math/cos angle))) (+ cy (* radius (Math/sin angle)))]))
        (range samples)))

(defn- spiral [turns samples]
  (mapv (fn [i] (let [along (/ i (double (dec samples)))
                      radius (+ 0.08 (* 0.92 along))
                      angle (turn (* turns along))]
                  [(* radius (Math/cos angle)) (* radius (Math/sin angle))]))
        (range samples)))

(defn- collinear? [[ax ay] [bx by] [cx cy]]
  (< (abs (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))) 1.0e-9))

(defn- simplify [points]
  (reduce (fn [kept point]
            (let [n (count kept)]
              (if (and (>= n 2) (collinear? (kept (- n 2)) (kept (dec n)) point))
                (assoc kept (dec n) point)
                (conj kept point))))
          [] points))

(defn waveform-figure
  "Trace a registered waveform across the unit square from its engine
  definition. A declared value jump is drawn as a vertical edge."
  [id params cycles amplitude]
  (let [{:keys [sample discontinuities] :as definition} (get @waveforms id)
        defaults (update-vals (:params definition) :default)
        value (fn [phase] (sample {:params (merge defaults params {signals/phase-input phase})}))
        jumps (keep (fn [{:keys [channel phase phase-param]}]
                      (when (= :value channel) (or phase (get (merge defaults params) phase-param))))
                    discontinuities)
        phases (sort (into (set (map #(/ % 16.0) (range (inc (* 16 cycles)))))
                           (for [cycle (range (inc cycles)) jump jumps
                                 :let [before (- (+ cycle jump) 1.0e-9)]
                                 :when (< 0.0 before cycles)]
                             before)))]
    (simplify (mapv (fn [phase] [(- (* 2.0 (/ phase cycles)) 1.0)
                                 (* amplitude (- (* 2.0 (value phase)) 1.0))])
                    phases))))

(def catalog
  "Every form's display name, icon glyph and the input it stands for per family.
  :volume is a unit description for volume-spec; a :sculpt input is a volume
  whose shape a running volume cue can morph to."
  {:square {:name "Square" :glyph {:kind :rect :width 1.0 :height 1.0 :corner 0.0}
            :volume {:shape :rounded-box :rounding 0.0 :aspect [1.0 1.0 1.0]}
            :figure {:points [[-1.0 -1.0] [1.0 -1.0] [1.0 1.0] [-1.0 1.0]] :closed? true}
            :waveform {:id :pulse :params {:duty 0.5}}}
   :rectangle {:name "Rectangle" :glyph {:kind :rect :width 1.0 :height 0.62 :corner 0.0}
               :volume {:shape :rounded-box :rounding 0.0 :aspect [1.0 0.62 0.62]}
               :figure {:points [[-1.0 -0.62] [1.0 -0.62] [1.0 0.62] [-1.0 0.62]] :closed? true}
               :waveform {:id :pulse :params {:duty 0.25}}}
   :bar {:name "Bar" :glyph {:kind :rect :width 1.0 :height 0.24 :corner 0.0}
         :volume {:shape :rounded-box :rounding 0.0 :aspect [1.0 0.24 0.24]}
         :figure {:points [[-1.0 0.0] [1.0 0.0]] :closed? false}
         :waveform {:id :pulse :params {:duty 0.125}}}
   :squircle {:name "Squircle" :glyph {:kind :outline}
              :volume {:shape :rounded-box :rounding 0.6 :aspect [1.0 1.0 1.0]}
              :figure {:points (superellipse 5.0 32) :closed? true}
              :waveform {:id :sine :params {}}}
   :pill {:name "Pill" :glyph {:kind :rect :width 1.0 :height 0.5 :corner 0.25}
          :volume {:shape :rounded-box :rounding 1.0 :aspect [1.0 0.5 0.5]}
          :figure {:points (into (arc [0.5 0.0] 0.5 -0.25 0.25 9) (arc [-0.5 0.0] 0.5 0.25 0.75 9)) :closed? true}
          :waveform {:id :sine :params {}}}
   :circle {:name "Circle" :glyph {:kind :circle}
            :volume {:shape :sphere}
            :figure {:points (arc [0.0 0.0] 1.0 0.0 (/ 23.0 24.0) 24) :closed? true}
            :waveform {:id :sine :params {}}}
   :ring {:name "Ring" :glyph {:kind :ring}
          :volume {:shape :ring :inner 0.6 :height 0.35}
          :waveform {:id :sine :params {}}}
   :spiral {:name "Spiral" :glyph {:kind :stroke}
            :figure {:points (spiral 2.0 33) :closed? false}
            :waveform {:id :saw :params {}}}
   :zigzag {:name "Zigzag" :glyph {:kind :stroke}
            :figure {:points (waveform-figure :triangle {} 3 0.55) :closed? false}
            :waveform {:id :triangle :params {}}}
   :sawtooth {:name "Sawtooth" :glyph {:kind :stroke}
              :figure {:points (waveform-figure :saw {} 3 0.5) :closed? false}
              :waveform {:id :saw :params {}}}
   :triangle {:name "Triangle" :glyph {:kind :outline}
              :figure {:points [[0.0 1.0] [-0.866 -0.5] [0.866 -0.5]] :closed? true}
              :waveform {:id :triangle :params {}}}})

(defn form? [form] (contains? catalog form))

(defn input
  "The existing input one form stands for in a family, or nil when it has none."
  [form family]
  (let [{:keys [volume figure waveform]} (get catalog form)]
    (case family
      :sculpt (when (#{:sphere :rounded-box} (:shape volume)) volume)
      :volume volume
      :aim-path figure
      :waveform waveform
      nil)))

(defn sculpt-values
  "Morph parameters of a static volume cue for a sculpt input. The shape keeps
  the cue's longest side."
  [{:keys [shape rounding aspect]} extent]
  (case shape
    :sphere {:shape :sphere :radius (* 0.5 extent)}
    :rounded-box {:shape :rounded-box :rounding rounding :size (mapv #(* extent %) aspect)}))

(defn volume-spec
  "A venue shape for a volume input, centred on position with this longest side."
  [{:keys [shape rounding aspect inner height]} position extent]
  (let [radius (* 0.5 extent)]
    (case shape
      :sphere {:shape :sphere :position position :radius radius}
      :rounded-box {:shape :rounded-box :position position :rounding rounding
                    :size (mapv #(* extent %) aspect)}
      :ring {:shape :difference
             :parts [{:shape :cylinder :position position :radius radius :height (* extent height)}
                     {:shape :cylinder :position position :radius (* radius inner) :height (* 2.0 extent height)}]})))

(defn aim-points
  "World-space points for an aim-path input. The figure lies in the plane of
  the two named axes around center; the third coordinate stays at center."
  [{:keys [points]} {:keys [center radius axes] :or {axes [0 2]}}]
  (let [[u v] axes]
    (mapv (fn [[x y]] (-> (vec center) (update u + (* radius x)) (update v + (* radius y)))) points)))
