(ns tolglow.color "Color utility functions"
  (:require [afterglow.effects.params :as params]
            [afterglow.show :as show]
            [afterglow.transform :as tf]
            [com.evocomputing.colors :as colors :refer [color-name]]
            [thi.ng.color.core :as clr]
            [thi.ng.math.core :as cmath]
            [tolglow.debug :as debug]))

(def used-type thi.ng.color.core.HSLA)
(defn color? "Is color?" [color] (= (type color) used-type))

; XXX for color effects simply scale end alpha with alpha from color...
(defn convert "for now, from evocomputing to thi.ng since latter lacks named colors etc"
 [color]
 (clr/as-hsla (apply clr/rgba (map #(/ % 255) (:rgba (colors/create-color color))))))

(def default (convert "black"))

(defn create "Create reasonable color, or pass through existing. XXX make sure to handle keywords (maybe show vars) properly"
 ([color]
  (let [color (cond ;XXX chill with keyword since can be show vars... tho won't find a matching one then, try again binding instead...
               (color? color) color
               (string? color) (convert color) ;new is record = map. avoid check  ;(colors/create-color color) ;; (or (string? color) (keyword? color) (map? color))
               (keyword? color) (try (convert color)
                                     (catch IllegalArgumentException _
                                       (params/bind-keyword-param color used-type default (str (name color))))) ;lets see if this is a good idea
               ;; (vector? color) (apply colors/create-color (interleave [:h :s :l] color))
               (vector? color) (apply clr/hsla color)
               (params/param? color) color
                ;(create :black)
               #_(try (colors/create-color color)
                          #_(catch IllegalArgumentException e (print color "aint a color.  "))
                          (finally (create :black))))]
   #_(cap color :s 0.90) ;circular
   color))
 ([h s l & {:keys [mode] :or {mode :full}}]
  (case mode
   :full ;360, 100, 100
   (clr/hsla h s l)
   :fraction
   #_(colors/create-color (mapv colors/clamp-unit-float [h s l]))))) ;should get from cfg but argh cross reqs
;; (convert "fafaa")

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

;diz woz rong idea, fix macros
(defn h "get/adjust hue" [& args] (apply (adjust-maker clr/hue clr/rotate-hue) args)) ;XXX ::colors/color fix rotate degrees yada
(defn s "get/adjust sat" [& args] (apply (adjust-maker clr/saturation clr/adjust-saturation) args))
(defn l "get/adjust lightness" [& args] (apply (adjust-maker clr/luminance clr/adjust-luminance) args))
(defn a "get/adjust alpha" [& args] (apply (adjust-maker clr/alpha clr/adjust-alpha) args))

(defn black? [color] (= color (convert :black))) ;; (defn black? [color] (= color (create :black)))
(defn white? [color] (= color (convert :white)))
(defn unsaturated? "Pretty much black-and/or-white" [color] (< (s color) 0.05)) ;or whatever tolerance?
;; (defn primary? "No mixing = RACIST. How dare they" [color] (and (< 357.5 color) (> 2.5 color)))
(defn primary? "No mixing = RACIST. How dare they"
 [color]
 (or    (< color 2.5)
  (< 117.5 color 122.5)
  (< 237.5 color 242.5)
  (< 357.5 color)))



(defn cap "Ensure color aspect within bounds"
 [color & {:keys [s l fraction] :or {s 0.90 l 1.00 #_fraction #_like-ratio-highest-rgb-to-lowest...}}] ;90 as in cfg sat cap
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

;; (def default-color-like {:h-spread (/ (tf/degrees 50) Math/PI) :s-spread 0.3 :l-spread 0.15})
(def default-color-like {:h-spread (/ 50 360) :s-spread 0.3 :l-spread 0.15})
(defn like "Return a random color similar to one passed in, with adjustable rng range per h s l"
 ([] ;random color
  (like (create (rand 1.0), (+ 0.25 (rand 0.5)), (+ 0.4 (rand 0.2)))))

 ([color & {:keys [h-spread s-spread l-spread] :as spread
            :or {h-spread (/ 60 360) s-spread 0.2 l-spread 0.15}}] ;should be a percentage variation instead right
  (if-let [color (create color)] ;ensure input ok
   (if (or (black? color) (> 0.01 (l color)))
    color ;return straight if black
    (let [hsl (map (fn [k]
                   (or (k spread)
                       (try (show/get-variable (keyword (str "color-like-" (name k))))
                        (catch Exception e))
                       (k default-color-like)))
                  [:h-spread :s-spread :l-spread])
         rand-res (map rand hsl)
         weight (map #(- %1 (* %1 %2)) hsl [0.5 0.15 0.25]) ;weight, where is (inverse) center? 0.25 = 75% below
         hsl  (mapv - rand-res weight)]
    (apply create (map #(cmath/clamp (+ (%1 color) (hsl %2)) 0.0 1.0) [h s l] [0 1 2])))))))


(defn random
 ([]
  (like)) ;swap tho, like should call this...
 ([color] ;dunno, maybe...
  ;; (like color :h 360 :s 20 :l 10)) ;doesnt seem to use those keys tho
  (like color))
 ([low high]
  (let [low (or low (random))
        high (or high (random))
        rng (fn [low high] (+ low (rand (- high low))))
        f #(map % [low high])]
   (create (apply rng (f h))
           (apply rng (f s))
           (apply rng (f l))))))

