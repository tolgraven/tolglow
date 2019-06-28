(ns tolglow.fixtures "tolgrAVen fixtures" {:author "Joen Tolgraven"}
  (:require [afterglow.channels :as chan :refer [dimmer functions pan tilt fine-channel color-wheel-hue focus]]
            [afterglow.effects.channel :as chan-fx :refer [function-value-scaler]]
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

(defn color
 [color offset & {:keys [hue fine]}]
 (let [hue (when hue [:hue hue])
       fine (when fine [:fine-channel fine]) ]
  (apply chan/color offset color (into hue fine))))

(defn colors
 [mode & offsets]
 (let [color-keys (case mode
                   :rgb [:red :green :blue] ;XXX add BRG etc
                   :rgbw [:red :green :blue :white])
       offsets (flatten offsets)
       offsets (if (= 1 (count offsets))
                (range (first offsets)
                       (+ (first offsets) (count color-keys)))
                offsets)]
  (map color color-keys offsets)))

(declare pixel-strip)
(defn strip "Easy helper wrapper for pixel-strip"
 [pixels [[x-start x-end] [y-start y-end] [z-start z-end] :as bounds] & mode]
 (let [mode (or (first mode) :rgbw)
       pos [:x-start (or x-start 0.0) :x-end (or x-end 0.0)
            :y-start (or y-start 0.0) :y-end (or y-end 0.0)
            :z-start (or z-start 0.0) :z-end (or z-end 0.0)]]
  (apply pixel-strip :pixels pixels :mode mode pos)))

(defn color-head "Create a head of subpixels"
 [offset mode x y z & {:keys [colors subpixels] :or {colors [:red :green :blue :white]}}]
 (let [pos {:x x :y y :z z}
       subpixels (or subpixels (case mode :rgb 3 :rgbw 4))
       channels (mapv #(chan/color (+ %1 offset) %2) (range subpixels) colors)]
  (merge {:channels channels}
         pos)))

(defn pos-for-head "Get position for head given bounds, size and index"
 [pixels index start end]
 (+ start (* index (/ (- end start) pixels))))

(defn resolve-channel "Automap key to appropriate channel def" ;XXX how map to functions?
 [ch-key index & args]
 (let [f (or (ns-resolve 'afterglow.channels (symbol (name ch-key)))
             (ns-resolve 'tolglow.fixtures (symbol (name ch-key)))
             (if (some #{:red :green :blue :white :amber :cyan :magenta :yellow} [ch-key])
              (partial color ch-key)) ;XXX add :rgb [3 4 5] :rgbw [4 5 6 7] support...
             (if (some #{:rgb :rgbw} [ch-key])
              (partial colors ch-key))
             (partial fine-channel ch-key))]
  (apply f index args)))

(defn merge-vec "Magically tidies up vector to one 'base' layer"
 [& v-or-vs]
 (apply (comp vec flatten vector) v-or-vs))

(defn make-channels "Create channels from ks, sequentially from offset. Empty coll if none valid" ;XXX lookup ks to chan/ fns. if not, use fine-channel
 [channels & global-offset] ;bit weird maybe why'd we need a global global-offset like this?
 (let [global-offset (or (first global-offset) 0)]
  (merge-vec
   (for [[ch args] channels]
   (#_tolglow.debug/det
    let [args (if (coll? args) args [args]) ;ensure vec
           ;; [i & args] args]
           i (+ global-offset (first args))
           args (rest args)]
    (apply resolve-channel ch i args)))))) ;needed apply here since resolve got rest arg

; XXX make helpers and turn everything proper abstracted. xyz-bounds (vec? map?) and geometry (function) optional
(defn pixel-strip "one strip as one head per pixel, supports RGB or RGBW."
  [pixels & {:keys [x y z mode channels #_xyz geometry]
             :or {mode :rgbw, x [-1.0 1.0] y [2.0 2.0] z [0.0 0.0]}}]
  (let [subpixels (if (= mode :rgbw) 4 3)
        channels (make-channels channels)
        fx-channels (count channels)
        ;; [x-base y-base z-base] (map #(/ (apply + %) 2) [x y z])]
        [x-base y-base z-base] (map #(/ (apply + %) 2) [x y z])]
   {:name (str (string/upper-case (name mode)) " LED strip")
    :x x-base, :y y-base, :z z-base
    :channels channels
   :heads (for [i (range pixels)]
            (let [[x y z] (map #(apply pos-for-head pixels i %) [x y z])
                  c (+ (* i subpixels) (+ fx-channels 1))]  ;; offset for fn channels
             (color-head c mode x y z)))}))


;; ; pixel heads should be relative and mapped onto actual xyz shit only at patch-time
;;anything like that in brunch code with heads?
(defn strip-from-data "temp strip fixture map creator"
 [data]
 (let [subpixels (case (:mode data) :rgb 3 :rgbw 4)
       channels (make-channels (:channels data))]
  (merge {:name (:name data "Unknown Strip")
          :head-offsets "maybe use :pixels and density?"}
          (when (:channels data) channels)))) ;etc

(defn patch-strip! ;will have to wrap reg patcher to add :heads?
 [data & {:keys [x y z] :or {x [0 0] y [0 0] z [0 0]}}]
 (let [subpixels (case (:mode data) :rgb 3 :rgbw 4)
       channels (make-channels (:channels data))
       [x-base y-base z-base] (map #(/ (apply + %) 2) [x y z])]
  (merge {:name (:name data "Unknown Strip")
          :x x-base, :y y-base, :z z-base
          :heads (for [i (range (:pixels data))]
                  (let [[x y z] (map #(apply pos-for-head (:pixels data) i %) [x y z])
                        c (+ (inc (count channels)) (* i subpixels))] ;; offset for fn channels
                   (color-head c (:mode data) x y z)))}
         (when (:channels data) {:channels channels}))))

;; parser like [x y] -> *-center *-half-circle...
(defn create "Generic fixture map creator"
 [data]
 (let [[pan-center pan-half-circle] (-> data :calibration :pan)
       [tilt-center tilt-half-circle] (-> data :calibration :tilt)]
  (merge {:name (:name data "Unnamed fixture")
          :channels (make-channels (:channels data))}
  (when pan-center {:pan-center pan-center})
  (when pan-half-circle {:pan-half-circle pan-half-circle})
  (when tilt-center {:tilt-center tilt-center})
  (when tilt-half-circle {:tilt-half-circle tilt-half-circle})))) ;etc

;    MOVING HEADS
(defn moving-head "Moving head fixture map creator"
 [data]
 {:name (:name data "Unnamed moving head")
  :channels (make-channels (:channels data))
  :pan-center (first (-> data :calibration :pan))
  :pan-half-circle (second (-> data :calibration :pan))
  :tilt-center (first (-> data :calibration :tilt))
  :tilt-half-circle (second (-> data :calibration :tilt))})

;; (defn rgbw-36-moving "108 / 36 LED RGBW Focus moving head" []
;;   {:name "RGBW moving-head"
;;    :channels [(pan        1  13)
;;               (tilt       2  14)
;;               (fine-channel :movement-speed 3 :function-name "Movement Speed (fast->slow)")
;;               (dimmer     4)
;;               (chan/color 5 :red)
;;               (chan/color 6 :green)
;;               (chan/color 7 :blue)
;;               (chan/color 8 :white)
;;               (functions :strobe 9
;;                           0 nil
;;                           10 {:type :strobe ;scale-fn: cap all by value all (important?) fixtures can employ. Ie is cap 10Hz, for 2nd value find where it reaches 10 Hz - maybe 90 or whatever
;;                               :scale-fn (partial function-value-scaler 1 16)
;;                               :label "Strobe (1.0Hz->30Hz)" ; 1-30 according to manual
;;                               :range :variable})
;;               (focus 10)
;;               (functions :auto 11)
;;               (functions :speed 12)
;;               (functions :reset 15 255)]
;;    :pan-center 127
;;    :pan-half-circle 85
;;    :tilt-center 35 ;; :tilt-center 220
;;    :tilt-half-circle 188 ; 270 deg full rotation
;;    })

(defn rgbw-7-12-moving "Mini RGBW moving head, 84 W" []
  {:name "LED MOVING HEAD 7x12W"
   :channels [(pan                 1  2) ;; (functions :pan-offset 2)
              (tilt                3  4) ;; (functions :tilt-offset 4)
              (fine-channel :movement-speed 5 :function-name "Movement Speed (fast->slow)") ;; (functions :pan-tilt-speed 5)
              (dimmer              6)
              (functions :strobe   7
                              0 nil
                              10 {:type :strobe  ; 0-10 Hz per manual, tss...
                                  :scale-fn (partial function-value-scaler 1.0 10)
                                  :label "Strobe (?Hz->10Hz)"
                                  :range :variable})
              (functions :auto     8)
              (functions :speed    9)
              (chan/color              10 :red)
              (chan/color              11 :green)
              (chan/color              12 :blue)
              (chan/color              13 :white)
              (functions :reset   14 255)]
   :pan-center        127 ; 'flipped' 35, two rotations past front 127
   :pan-half-circle    85
   :tilt-center         0
   :tilt-half-circle  256 ; only 180 deg
   })

(defn rgbw-12-12-moving "RGBW moving head, 144 W" []
  {:name "HY12x12 MOVING HEAD BEAM"
   :channels [(pan                 1  14)
              (tilt                2  15)
              (dimmer              3)
              (chan/color               4 :red)
              (chan/color               5 :green)
              (chan/color               6 :blue)
              (chan/color               7 :white)
              (functions :strobe   8
                              0 nil
                              10 {:type :strobe
                                  ;; :scale-fn (partial function-value-scaler 2 100)
                                  :label "Strobe (Slow->Fast)" ;no info in manual
                                  :range :variable})
              (fine-channel :movement-speed 9 :function-name "Movement Speed (fast->slow)")
              (functions :auto     10)
              (functions :speed    11)
              (functions :control  12
                              255 :soundcontrol) ; flesh out
              (functions :sound-sensitivity  13)
              (functions :reset   16 150)]
   ;; :pan-center        127 ; 'flipped' 35, two rotations past front 127
   :pan-center        102
   :pan-half-circle    73 ; seems this one suddenly does 630, not 540...
   :tilt-center         0
   :tilt-half-circle  256})

(defn rgbw-60-moving-beam "Mini RGBW moving head beam, 60 W" []
  {:name "60w Beam MOVING HEAD LIGHT"
   :channels [(pan       1 2)
              (tilt      3 4)
              (functions :pan-tilt-speed 5)
              (dimmer    6)
              (functions :shutter 7
                              0 "Shutter Closed"
                              8 "Shutter Open"
                              16 {:type :strobe ; 0-10 Hz per manual
                                  :scale-fn (partial function-value-scaler 0.2 10)
                                  :label "Strobe (Slow->Fast)"
                                  :range :variable}
                              132 "Shutter Open 2"
                              ;; 140 {:type :strobe
                              ;;      :label "Strobe (slowly open quickly close)"
                              ;;      :range :variable}
                              140 :pulse-strobe
                              182 "Shutter Open 3"
                              190 :pulse-strobe-rev ; XXX
                              232  "Shutter Open 4"
                              240 :random-strobe
                              248  "Shutter Open 5")
              (chan/color     8 :red)
              (chan/color     9 :green)
              (chan/color     10 :blue)
              (chan/color     11 :white)
              (functions :auto 12)
              (functions :control 13
                            0 nil
                            200 :reset
                            210 nil
                            240 :sound-active)]
   :pan-center 127
   :pan-half-circle 85
   :tilt-center 0
   :tilt-half-circle 256 ;219 ; only 210 deg? (219 val) per manual. Looks more like 180?
   })


(defn rgbw-mirror "RGBW churchy window thing" []
  {:name "whatever its called"
   :channels [(dimmer 1)
              (chan/color 2 :red)
              (chan/color 3 :green)
              (chan/color 4 :blue)
              (chan/color 5 :white)
              (functions :strobe 6
                              0 nil
                              10 {:type :strobe
                                  :scale-fn (partial function-value-scaler 0.2 10)
                                  :label "Strobe (0.2Hz->10Hz)"
                                  :range :variable})
              ;; (fine-channel :movement-speed 7 :function-name "Movement Speed (slow->fast)")
              (functions :control 7 :function-name "Movement Speed (slow->fast)")
              ] })

;    WASHES
(defn rgb-54-3-par "54*1.5W RGB 3 IN 1    9+1" []
  {:name "54 LED RGB PAR"
   :channels [(dimmer 1)
              (chan/color 2 :red)
              (chan/color 3 :green)
              (chan/color 4 :blue)
              (functions :strobe 5
                              0 nil
                              10 {:type :strobe
                                  :scale-fn (partial function-value-scaler 0.2 10)
                                  :label "Strobe (0.2Hz->10Hz)"
                                  :range :variable})
              (functions :control 6
                            0 nil
                            51 :color-wheel-hue ;fill in rest...
                            251 :sound-active)
              (functions :speed 7)] })

(defn rgbw-7-12-par "7x12 RGBW, 75 W" []
  {:name "LED RGBW Par Light"
   :channels [(dimmer 1)
              (chan/color 2 :red)
              (chan/color 3 :green)
              (chan/color 4 :blue)
              (chan/color 5 :white)
              (functions :strobe 6
                              0 nil
                              10 {:type :strobe
                                  :scale-fn (partial function-value-scaler 0.2 10)
                                  :label "Strobe (0.5Hz->10Hz)"
                                  :range :variable})
              (functions :control 7
                            0 nil
                            51 :color-wheel-hue ;fill in rest...
                            251 :sound-active)
              (functions :speed 8)] })

(defn rgbw-18-12-par "18x12 RGBW, 200+ W" []
  {:name "RGBW LED PAR"
   :channels [(dimmer 1)
              (chan/color 2 :red)
              (chan/color 3 :green)
              (chan/color 4 :blue)
              (chan/color 5 :white)
              (functions :strobe 6
                              0 nil
                              10 {:type :strobe
                                  ;; :scale-fn (partial function-value-scaler 2 100)
                                  :label "Strobe (Slow->Fast)" ;no info manua
                                  :range :variable})
              (functions :control 7
                            0 nil
                            8 :color-wheel-hue ;fill in rest...
                            )]})

(defn rgbwauv-5-18-par "5x18 RGBWA+UV, ? W" []
  {:name "RGBWAUV LED Par"
   :channels [(dimmer 1)
              (chan/color 2 :red)
              (chan/color 3 :green)
              (chan/color 4 :blue)
              (chan/color 5 :white)
              (chan/color 6 :amber :hue 45)
              (chan/color 7 :uv :hue 270)
              (functions :strobe 8
                              0 nil
                              10 {:type :strobe
                                  ;; :scale-fn (partial function-value-scaler 2 100)
                                  :label "Strobe (Slow->Fast)"
                                  :range :variable})
              (functions :control 9
                            0 nil
                            51 :macrosnshit ;fill in rest...
                            )
              (functions :speed 10)]})

(defn wa-100-cob-par "LED COB PAR, 100 W" []
  {:name "100Watt W/A LED COB PAR Light"
   :channels [(dimmer 1)
              (chan/color 2 :white) ;cant get color mixing working without rgb... why?
              ;; (chan/color 3 :amber :hue 45)
              ;; (dimmer 2) ; white is broken on dmx(??), flickers...
              ;; (functions :strobe 2 ; but not so noticable when strobing...
              ;;                 0 nil
              ;;                 255 {:type :strobe ;actually no yeah it is...
              ;;                      :range :variable})
              (dimmer 3)
              (functions :strobe 4
                              0 nil
                              10 {:type :strobe ;1-10 hz per manual, but way off. 0.4-0.5 something
                                  :scale-fn (partial function-value-scaler 0.45 10)
                                  :label "Strobe (0.45Hz->10Hz)"
                                  :range :variable})] })




;; (defn robe-1200 [channels]
;;  {:name "Robe 1200 w/e"
;;   :channels (make-channels channels)
;;    :pan-center 127
;;    :pan-half-circle 85
;;    :tilt-center 35 ;; :tilt-center 220
;;    :tilt-half-circle 188}) ; 270 deg full rotation

(defn color-milight "Milight RGB +W" []
  {:name "Milight RGB +W"
   :channels [(color-wheel-hue 1)
              (dimmer          2)
              (chan/color           3 :white)]})
            ;; impl dmxy strobe etc ch for these as well? when running off own LN
;    P2
(def ^:private p2-head-offsets
  "The X-axis positions of the three p2 segments"
  [-1.0 0.1 1.0])

(defn- p2-head "Creates a head definition for one segment of the P2"
  [index]
  {:channels [(chan/color (+ 4 (* 4 index)) :red)
              (chan/color (+ 5 (* 4 index)) :green)
              (chan/color (+ 6 (* 4 index)) :blue)
              (chan/color (+ 7 (* 4 index)) :white)]
   :x (get p2-head-offsets index)})

(defn p2 "SGM P-2" []
  (letfn [(build-shutter [channel]
            (functions :shutter channel
                            0 "Shutter Closed"
                            8 "Shutter Open"
                            16 {:type :strobe :label "Strobe (Fast->Slow)" :range :variable}
                            152 {:type :pulse-strobe :label "Pulse - Open (Slow->Fast)"}
                            176 {:type :pulse-strobe :label "Pulse - Close (Slow->Fast)"}
                            200 {:type :random-strobe :label "Strobe - Random" :range :variable}
                            245 "Shutter Open 2"))]
    {:channels [(build-shutter 1)
                (dimmer 2)
              ;; (chan/color :white 3) ; color temp 2000 - 10 000 K, how define? as generic chan so doesnt fuck with it while doing other color stuff?
                (fine-channel :white 3)] ; color temp 2000 - 10 000 K, how define? as generic chan so doesnt fuck with it while doing other color stuff?
     :heads (map p2-head (range 3)) ; three segments / heads
     :name "SGM P-2"}))

;  AMERICAN DJ SHITTY PAR
(defn adj-pro-led-tol "American DJ 64B LED PRO"
  ([] (adj-pro-led-tol :6-channel))
  ([mode]
   (merge {:name "ADJ 64B LED PRO"
           :mode mode}
          (case mode
   :6-channel
  {:channels [(chan/color     1 :red)
              (chan/color     2 :green)
              (chan/color     3 :blue)
              (functions :color-macros  4 0 nil
                              8 {:type :color-macros :label "Macro 1->Macro31" :range :variable})
              (functions :shutter       5
                              0  "Shutter Open"
                              16 {:type :strobe
                                  ;; :scale-fn (partial function-value-scaler 1.12 32)
                                  :label "Strobe (1.12Hz->32Hz)"
                                  :range :variable})
              (functions :speed         6
                              0   {:type :slow-fast-nothing :label "Nothing" :range :variable}
                              32  {:type :slow-fast-dim-bright :label "Dim > Bright" :range :variable}
                              64  {:type :slow-fast-bright-dim :label "Bright > Dim" :range :variable}
                              96  {:type :slow-fast-dim-bright-dim :label "Dim > Bright > Dim" :range :variable}
                              128 {:type :slow-fast-colour-mixing :label "Colour Mixing" :range :variable}
                              160 {:type :slow-fast-3-colour-change :label "3 Colour Change" :range :variable}
                              192 {:type :slow-fast-7-colour-change :label "7 Colour Change" :range :variable}
                              224 {:type :slow-fast-sound-active :label "Sound Active" :range :variable})]}
   :3-channel
  {:channels [(chan/color 1 :red)
              (chan/color 2 :green)
              (chan/color 3 :blue)]}))))


;  TRACKSPOT-attempt
(defn trackspot "Trackspot" []
  (letfn [(build-color-wheel [channel]
            (functions :color channel
                            0 "Color Wheel Open" ;color 1
                            8 {:type :color-clockwise :label "Color Wheel Clockwise (slow->fast)" :var-label "CW (slow->fast)" :range :variable}
                            64 {:type :color-counterclockwise :label "Color Wheel Counterclockwise (slow->fast)" :var-label "CCW (slow->fast)" :range :variable}
                            128 (color-wheel-hue "yellow") ;color 2, not sure what number is what hue...
                            140 (color-wheel-hue "green") ;color 3
                            156 "Color Wheel Peachblow" ;color 4
                            168 "Color Wheel Light Blue" ;color 5
                            184 "Color Wheel Kelly" ;color 6
                            196 (color-wheel-hue "red") ;color 7
                            212 (color-wheel-hue "blue") ;color 8
                            224 "Color Wheel White + Yellow" ;color 9
                            240 "Color Wheel Yellow + Pink" ;color 10
                            252 "Color Wheel Pink + Green")) ;color 1

          (build-gobo-wheel [channel]
                            (functions :gobo channel
                                            0 "Gobo Open 1"
                                            10 {:type :gobo-clockwise :label "Gobo Clockwise Speed" :var-label "CW Speed" :range :variable}
                                            82 {:type :gobo-counterclockwise :label "Gobo Counterclockwise Speed" :var-label "CCW Speed" :range :variable}
                                            155 "Gobo 2" 179 "Gobo 3" 191 "Gobo 4" 201 "Gobo 5" 209 "Gobo 6"
                                            217 "Gobo 7" 227 "Gobo 8" 235 "Gobo 9" 245 "Gobo 10" 252 "Gobo Open 1"))
          (build-gobo-speed [channel]
                            (functions :gobo-rotation channel
                                            0 {:type :gobo-rotation-clockwise
                                               :label "Gobo Rotation Speed (fast->slow)" :var-label "CW (fast->slow)"
                                               :range :variable}))
          (build-shutter [channel]
                         (functions :shutter channel
                                         0 "Shutter Closed" 26 "Shutter Open"
                                         51 {:type :strobe :label "Strobe" :range :variable}
                                         230 "Shutter Closed 2" 242 "Shutter Open 2"))]
    {:channels [(pan 1)
                (tilt 2)
                (build-color-wheel 3)
                (build-gobo-wheel 4)
                (build-shutter 5)
                (dimmer 6)
                (build-gobo-speed 7)]
     :name "Trackspot"}))
