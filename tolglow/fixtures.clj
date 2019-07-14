(ns tolglow.fixtures "tolgrAVen fixtures" {:author "Joen Tolgraven"}
  (:require [afterglow.channels :as chan :refer [dimmer functions pan tilt fine-channel color-wheel-hue focus]]
            [afterglow.effects.channel :as chan-fx :refer [function-value-scaler]]
            [afterglow.transform :as tf :refer [degrees]]
            [clojure.string :as string]))

(defn shutter-strobe-map "helper for strobe defs and scaling"
 [& {:keys [label hz] :or {}}]
 (let [common {:type :strobe, :range :variable}
       scaler (when hz {:scale-fn (apply partial function-value-scaler hz)})
       [lo hi] (map #(if %1 (str %1 "Hz") %2) (or hz [nil nil]) ["Slow" "Fast"])
       label {:label (or label (str "Strobe (" lo "->" hi ")"))}]
  (merge common scaler label)))

(defn strobe "Shutter channel with full-range strobe, scaled by :hz"
 [offset & {:keys [open start hz] :or {start 1}}]
 (let [kind (if open :shutter :strobe)
       args (flatten [(if open (if (pos? open)
                                [0 "Shutter Closed" open "Shutter Open"]
                                [0 "Shutter Open"])
                       [0 nil])
                      start (shutter-strobe-map :hz hz)])]
  (apply functions kind offset args))) ;or use :strobe?

(defn scaler "wrap function-value-scaler"
 [low high]
 (partial function-value-scaler low high))


(def color-layouts {:rgb [:red :green :blue] ;XXX add BRG etc
                    :rgbw [:red :green :blue :white]
                    :rgbwa [:red :green :blue :white :amber]})
(def named-colors [:red :green :blue :white :amber :uv :cyan :magenta :yellow])
(def color-hues {:amber 45 :uv 270 :cyan 180 :magenta 300 :yellow 55})

(defn color
 [color-key offset & {:keys [hue fine]}]
 (let [hue (if hue [:hue hue]
            (if-let [hue (color-key color-hues)]
             [:hue hue])) ;tho, yeah just add to afterglow lol...
       fine (when fine [:fine-channel fine])]
  (apply chan/color offset color-key (into hue fine))))

(defn colors
 [color-layout & offsets]
 (let [color-keys (color-layout color-layouts)
       offsets (flatten offsets)
       offsets (if (= 1 (count offsets))
                (range (first offsets)
                       (+ (first offsets) (count color-keys)))
                offsets)]
  (map color color-keys offsets)))


(defn head-pos "Get position for head given amount of heads, index and total length of fixture"
 [heads index length]
 (let [end (/ length 2)
       start (- end)]
  (if (= heads 1)
   0.0
   (float (+ start (* index (/ length (dec heads))))))))

(defn resolve-channel "Automap key to appropriate channel def" ;XXX how map to functions?
 [ch-key index & args]
 (let [f (or (ns-resolve 'afterglow.channels (symbol (name ch-key)))
             (ns-resolve 'tolglow.fixtures (symbol (name ch-key)))
             (if (some (set named-colors) [ch-key])
              (partial color ch-key))
             (if (some (set (keys color-layouts)) [ch-key])
              (partial colors ch-key))
             (partial fine-channel ch-key))]
  (apply f index args)))

(defn make-channels "Create channels from ks, sequentially from offset. Empty coll if none valid" ;XXX lookup ks to chan/ fns. if not, use fine-channel
 [channels & global-offset] ;bit weird maybe why'd we need a global global-offset like this?
 (let [global-offset (or (first global-offset) 0)]
  (when channels
  ((comp vec flatten vector)
   (for [[ch args] channels]
   (#_tolglow.debug/det
    let [args (if (coll? args) args [args]) ;ensure vec
           ;; [i & args] args]
         i (+ global-offset (first args))
         args (rest args)]
    (apply resolve-channel ch i args))))))) ;needed apply here since resolve got rest arg


;; DEPRECATED XXX
(defn color-head "Create a head of subpixels"
 [offset color-layout x y z]
 {:channels (colors color-layout offset) :x x :y y :z z})

(defn pos-for-head "Get position for head given bounds, size and index"
 [pixels index start end]
 (+ start (* index (/ (- end start) (dec pixels)))))

(defn pixel-strip "one strip as one head per pixel, supports RGB or RGBW."
  [pixels & {:keys [x y z color-layout channels]
             :or {color-layout :rgbw, x [-1.0 1.0] y [2.0 2.0] z [0.0 0.0]}}]
  (let [subpixels (if (= color-layout :rgbw) 4 3)
        channels (make-channels channels)]
   {:name (str (string/upper-case (name color-layout)) " LED strip")
    :channels channels
    :heads (for [i (range pixels)]
            (let [[x y z] (map #(apply pos-for-head pixels i %) [x y z])
                  c (+ (* i subpixels) (+ (count channels) 1))]  ;; offset for fn channels
             (color-head c color-layout x y z)))}))
;;_______________________________________

;; DEPRECATED XXX

(defn create-pixel-heads "Create heads of individual pixels"
 [strip & offset]
 (when strip
  (let [subpixels (count ((:color-layout strip) color-layouts))
        length (/ (:pixels strip) (:density strip))
        offset (or (when (seq? offset) (first offset)) offset 1)] ;dunno why it didnt come as a seq some time???
   (for [i (range (:pixels strip))]
    (let [x (head-pos (:pixels strip) i length)
          offset (+ offset (* i subpixels))] ; offset for fn channels
     {:channels (colors (:color-layout strip) offset)
      :x x :y 0.0 :z 0.0})))))

(defn create-heads "Some other heads presumably more advanced. Individual movement, dimmers or similar..."
 [heads]
 nil) ;XXX implement

(defn moving-head-calibration ;; parser like [x y] -> *-center *-half-circle...
 [calibration]
 (when (seq calibration)
  (let [[[pan-c pan-half] [tilt-c tilt-half]] (map calibration [:pan :tilt])]
   (merge ; how long a half circle pan/tilt actually takes (motor speed) should
          ; also be part of it. So can adapt synchronized movements where slower cant keep up
    (when pan-c {:pan-center pan-c})
    (when pan-half {:pan-half-circle pan-half})
    (when tilt-c {:tilt-center tilt-c})
    (when tilt-half {:tilt-half-circle tilt-half})))))

(defn create "Generic fixture map creator"
 [data]
 (let [channels (make-channels (:channels data))
       heads (or (create-pixel-heads (:strip data) (inc (count channels)))
                 (create-heads data))]
  (merge
   {:name (:name data "Unnamed fixture")}
    (when channels {:channels channels}) ; since some pixel fixtures have no global channels, all are per-head
    (when heads {:heads heads})
    (moving-head-calibration (:calibration data)))))



;; some old hardcoded defs...
(defn rgbw-60-moving-beam "Mini RGBW moving head beam, 60 W" []
 {:name "60w Beam MOVING HEAD LIGHT"
  :channels [(pan 1 2) (tilt 3 4) (functions :movement-speed 5)
             (dimmer 6)
             (functions :shutter 7
                        0 "Shutter Closed"
                        8 "Shutter Open"
                        16 {:type :strobe ; 0-10 Hz per manual
                            :scale-fn (scaler 0.2 10)
                            :label "Strobe (Slow->Fast)"
                            :range :variable}
                        132 "Shutter Open 2"
                        140 :pulse-strobe     182 "Shutter Open 3"
                        190 :pulse-strobe-rev 232 "Shutter Open 4"
                        240 :random-strobe    248 "Shutter Open 5")
             (chan/color 8 :red) (chan/color 9 :green) (chan/color 10 :blue) (chan/color 11 :white)
             (functions :auto 12)
             (functions :control 13
                        0 nil 200 :reset
                        210 nil 240 :sound-active)]
  :pan-center 127 :pan-half-circle  85
  :tilt-center 0  :tilt-half-circle 256}) ;219 ; only 210 deg? (219 val) per manual. Looks more like 180?

