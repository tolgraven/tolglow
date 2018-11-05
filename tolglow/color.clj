(ns tolglow.color "Color utility functions"
  (:require [afterglow.effects.params :as params]
            [afterglow.show :as show]
            [com.evocomputing.colors :as colors :refer [color-name]]
            [tolglow.debug :as debug]))

(defn color? "Is color?" [color] (= (type color) ::colors/color))

; XXX for color effects simply scale end alpha with alpha from color...

;; (declare cap)
(defn create "Create reasonable color"
 ([color]
  (let [color (cond
               (color? color) color
               ;; (some true? (map #(% color) [string? keyword? map?]))
               (or (string? color) (keyword? color) (map? color))
               (colors/create-color color)

               (vector? color) (apply colors/create-color (interleave [:h :s :l] color))
               (params/param? color) color ;or do we resolve?
               :else (try (colors/create-color color)
                          #_(catch IllegalArgumentException e (print color "aint a color.  "))
                          (finally (create :black))))]
   #_(cap color :s 90) ;circular
   color))
 ([h s l & {:keys [mode] :or {mode :full}}]
  (case mode
   :full ;360, 100, 100
   (colors/create-color :h (colors/clamp-hue h)
                        :s (colors/clamp-percent-float s)
                        :l (colors/clamp-percent-float l))
   :fraction
   #_(colors/create-color (mapv colors/clamp-unit-float [h s l]))))) ;should get from cfg but argh cross reqs


; ev make proper so can + - directly? not possible tho i guess
(defn- adjust-maker "Get/adjust saturation of color"
 [get-fn adjust-fn & {:keys [down-fn set-fn]}]
 (fn
  ([color] (get-fn (create color)))
  ([change color & {:keys [abs]}] ;XXX handle absolute
   (let [color (create color)]
    (if (or (pos? change) (not down-fn))
     (adjust-fn color change) ;pos, or adjuster works both ways (with negative numbers) if no specific down-fn
     (down-fn color (* -1 change))))))) ;call down-fn with pos number


;diz woz rong idea
;; (defn h "get/adjust hue" [& args] (apply (adjust-maker colors/hue colors/adjust-hue #(colors/adjust-hue %1 (* -1 %2))) args))
(defn h "get/adjust hue" [& args] (apply (adjust-maker colors/hue colors/adjust-hue) args))
(defn s "get/adjust sat" [& args] (apply (adjust-maker colors/saturation colors/saturate :down-fn colors/desaturate) args))
(defn l "get/adjust lightness" [& args] (apply (adjust-maker colors/lightness colors/lighten :down-fn colors/darken) args))
(defn a "get/adjust alpha" [& args] (apply (adjust-maker colors/alpha colors/adjust-alpha) args))
;; (defn add [one two] (apply colors/color-add (map create [one two])))
(defn add [& args] (apply colors/color-add (map create args)))
(defn sub [one two] (apply colors/color-sub (map create [one two])))
(defn mul [one two] (apply colors/color-mult (map create [one two])))
(defn div [one two] (apply colors/color-div (map create [one two])))


(defn black? [color] (= color (create :black)))
(defn white? [color] (= color (create :white)))
(defn unsaturated? "Pretty much black-and/or-white" [color] (< (s color) 5)) ;or whatever tolerance?
;; (defn primary? "No mixing = RACIST. How dare they" [color] (and (< 357.5 color) (> 2.5 color)))
(defn primary? "No mixing = RACIST. How dare they"
 [color]
 (or    (< color 2.5)
  (< 117.5 color 122.5)
  (< 237.5 color 242.5)
  (< 357.5 color)))



(defn cap "Ensure color aspect within bounds"
 [color & {:keys [s l fraction] :or {s 90 l 100 #_fraction #_like-ratio-highest-rgb-to-lowest...}}] ;90 as in cfg sat cap
 (let [color (create color)
       [chue csat clight] (:hsl color)
       capped (map (fn [curr bound] (if (< curr bound) curr bound))
                   [csat clight] [s l])]
  (apply create chue capped)))



(defn compliment "Return a different color matching provided one, according to science or whatever"
 [color & {:keys [looseness taste] :or {looseness "monet", taste :yourmom}}])

(defn bound-palette "Give n colors puck with above. Create ranges around these values, that color-params can be clipped to
                     XXX including post global hue rotation, lightening effects etc.
                     Colors too far from appropriate points get darkened and desaturated..."
 [])

(def default-color-like {:h-spread 50 :s-spread 30 :l-spread 15})
(defn like "Return a random color similar to one passed in, with adjustable rng range per h s l"
 ([] ;random color
  (like (create (rand 360), (+ 30 (rand 50)), (+ 40 (rand 20)))))

 ([color & {:keys [h-spread s-spread l-spread] :as spread
            :or {h-spread 50 s-spread 30 l-spread 15}}] ;should be a percentage variation instead right
  (if-let [color (create color)] ;ensure input ok
  (if (or (black? color) (> 1 (l color)))
   color ;return straight if black
   (let [hsl (map (fn [k]
                   (or (k spread)
                       (try (show/get-variable (keyword (str "color-like-" (name k))))
                        (catch Exception e))
                       (k default-color-like)))
                  [:h-spread :s-spread :l-spread])
         rand-res (map rand hsl)
         weight (map #(- %1 (* %1 %2)) hsl [0.5 0.25 0.25]) ;weight, where is (inverse) center? 0.25 = 75% below
         hsl  (mapv - rand-res weight)
         #_s #_(if (< 80 (s color)) (- s (- (s color) 80)) s) ;this would need to check resulting sat rather tho, surely...
         hsl [(colors/clamp-hue (+ (h color) (hsl 0)))
              (colors/clamp-percent-float (+ (s color) (hsl 1)))
              (colors/clamp-percent-float (+ (l color) (hsl 2)))]]
    (apply create hsl))))))



(defn random
 ([]
  (like)) ;swap tho, like should call this...
 ([color] ;dunno, maybe...
  (like color :h 360 :s 20 :l 10))
 ([low high]
  (let [low (or low (random))
        high (or high (random))
        rng (fn [low high] (+ low (rand (- high low))))
        f #(map % [low high])]
   (create (apply rng (f h))
           (apply rng (f s))
           (apply rng (f l))))))

