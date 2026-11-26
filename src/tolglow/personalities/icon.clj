(ns tolglow.personalities.icon
  "Draw a personality as a small SVG. The picture is a pure function of the
  personality's name, palette, geometry weights and motion character: equal
  data gives the same bytes on every host and after every save and load."
  (:require [clojure.string :as str]
            [tolglow.automation.random :as random]
            [tolglow.personalities.forms :as forms])
  (:import [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util Base64]))

(def size "Side of the square view box, in SVG user units." 96)

(def ^:private tile-color "#101418")

(def ^:private layouts
  "Centre x, centre y and side of the three shapes at no symmetry and at full symmetry."
  [[[36.0 37.0 50.0] [48.0 32.0 46.0]]
   [[72.0 63.0 30.0] [67.0 72.0 30.0]]
   [[33.0 77.0 22.0] [29.0 72.0 30.0]]])

(defn- number [value]
  ;; Tenths of a unit, printed without a locale or a platform float format.
  (let [tenths (Math/round (* 10.0 (double value)))
        whole (quot (abs tenths) 10)
        part (rem (abs tenths) 10)]
    (str (when (neg? tenths) "-") whole (when (pos? part) (str "." part)))))

(defn- hex [[r g b]]
  (let [channel #(Math/round (* 255.0 (max 0.0 (min 1.0 (double %)))))]
    (format "#%02x%02x%02x" (channel r) (channel g) (channel b))))

(defn- escape [text]
  (str/escape (str text) {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;" \' "&apos;"}))

(defn- canonical-entry [entry]
  (let [color #(mapv double %)]
    (cond
      (vector? entry) (color entry)
      (:gradient entry) [:gradient (mapv (fn [{:keys [position color]}] (into [(double position)] (map double) color))
                                         (:gradient entry))
                         (double (:angle-deg entry 90))]
      :else [:split (mapv color (:split entry)) (double (:angle-deg entry 0))])))

(defn- visual-data
  "The fields the picture depends on, in one printable order. Integers and
  floats, and array maps and hash maps, describe the same picture."
  [{:keys [palette geometry motion]}]
  [(mapv canonical-entry palette)
   (vec (for [form forms/order :let [weight (get-in geometry [:forms form])] :when weight]
          [form (double weight)]))
   (mapv #(double (get motion %)) [:speed :smoothness :jitter :symmetry])])

(defn- digest [personality]
  (.digest (MessageDigest/getInstance "SHA-256")
           (.getBytes (pr-str (visual-data personality)) StandardCharsets/UTF_8)))

(defn- randoms
  "Unit values drawn from one seed, in a fixed order."
  [seed]
  (map second (rest (iterate (fn [[seed _]] (random/next-random seed)) [seed nil]))))

(defn icon-forms
  "The three forms to draw: the heaviest weights first, ties broken by the
  personality's own seed. Fewer than three weighted forms repeat in order."
  [personality]
  (let [weights (get-in personality [:geometry :forms])
        seed (.getInt (ByteBuffer/wrap (digest personality)))
        ranked (->> (map vector (filter #(pos? (get weights % 0)) forms/order) (randoms seed))
                    (sort-by (fn [[form tie]] [(- (double (get weights form))) tie]))
                    (mapv first))]
    (vec (take 3 (cycle ranked)))))

(defn- paint
  "Return [definition reference] for one palette entry inside a shape's box."
  [entry id [cx cy side]]
  (if (vector? entry)
    [nil (hex entry)]
    (let [split (:split entry)
          angle (Math/toRadians (double (:angle-deg entry (if split 0 90))))
          dx (* 0.5 side (Math/cos angle)) dy (* 0.5 side (Math/sin angle))
          stop (fn [position color]
                 (str "<stop offset=\"" (number (* 100.0 position)) "%\" stop-color=\"" (hex color) "\"/>"))
          stops (if split
                  (let [bands (count split)]
                    (mapcat (fn [index color]
                              [(stop (/ index (double bands)) color) (stop (/ (inc index) (double bands)) color)])
                            (range) split))
                  (map #(stop (:position %) (:color %)) (:gradient entry)))]
      [(str "<linearGradient id=\"" id "\" gradientUnits=\"userSpaceOnUse\" x1=\"" (number (- cx dx))
            "\" y1=\"" (number (- cy dy)) "\" x2=\"" (number (+ cx dx)) "\" y2=\"" (number (+ cy dy)) "\">"
            (apply str stops) "</linearGradient>")
       (str "url(#" id ")")])))

(defn- path-data [points closed?]
  (str "M" (str/join "L" (map (fn [[x y]] (str (number x) " " (number y))) points)) (when closed? "Z")))

(defn- shape
  "One glyph in its box. Jitter turns the whole glyph and shakes drawn vertices."
  [form [cx cy side] fill {:keys [jitter smoothness]} [turn-a & shakes]]
  (let [{:keys [kind width height corner]} (get-in forms/catalog [form :glyph])
        half (* 0.5 side)
        degrees (* 56.0 jitter (- turn-a 0.5))
        transform (when-not (zero? (Math/round (* 10.0 degrees)))
                    (str " transform=\"rotate(" (number degrees) " " (number cx) " " (number cy) ")\""))
        soft? (>= smoothness 0.5)
        line (max 3.5 (* 0.11 side))
        stroke (str " fill=\"none\" stroke=\"" fill "\" stroke-width=\"" (number line) "\" stroke-linecap=\""
                    (if soft? "round" "butt") "\" stroke-linejoin=\"" (if soft? "round" "miter") "\"")
        reach (- half (* 0.5 line))
        drawn (fn [radius]
                (map (fn [[x y] [sx sy]]
                       [(+ cx (* radius x) (* 0.14 side jitter (- sx 0.5)))
                        (- cy (* radius y) (* 0.14 side jitter (- sy 0.5)))])
                     (get-in forms/catalog [form :figure :points]) (partition 2 shakes)))]
    (case kind
      :rect (str "<rect x=\"" (number (- cx (* half width))) "\" y=\"" (number (- cy (* half height)))
                 "\" width=\"" (number (* side width)) "\" height=\"" (number (* side height)) "\""
                 (when (pos? corner) (str " rx=\"" (number (* side corner)) "\""))
                 " fill=\"" fill "\"" transform "/>")
      :circle (str "<circle cx=\"" (number cx) "\" cy=\"" (number cy) "\" r=\"" (number half) "\" fill=\"" fill "\"/>")
      :ring (let [line (* 0.2 side)]
              (str "<circle cx=\"" (number cx) "\" cy=\"" (number cy) "\" r=\"" (number (- half (* 0.5 line)))
                   "\" fill=\"none\" stroke=\"" fill "\" stroke-width=\"" (number line) "\"/>"))
      :outline (str "<path d=\"" (path-data (drawn half) true) "\" fill=\"" fill "\""
                    (when-not soft? " stroke-linejoin=\"miter\"") transform "/>")
      :stroke (str "<path d=\"" (path-data (drawn reach) false) "\"" stroke transform "/>"))))

(defn svg
  "Draw a valid personality. Returns the same string for the same data."
  [{:keys [name palette motion] :as personality}]
  (let [bytes (digest personality)
        seed (.getInt (ByteBuffer/wrap bytes))
        tag (apply str (map #(format "%02x" (bit-and 255 %)) (take 3 bytes)))
        {:keys [jitter symmetry]} motion
        ;; The first draws ranked the forms; each shape then owns a fixed run.
        draws (partition 80 (drop (count forms/order) (randoms seed)))
        boxes (mapv (fn [[loose even] [shift-x shift-y]]
                      (let [[x y side] (mapv #(+ %1 (* symmetry (- %2 %1))) loose even)]
                        [(+ x (* 10.0 jitter (- shift-x 0.5))) (+ y (* 10.0 jitter (- shift-y 0.5))) side]))
                    layouts draws)
        paints (mapv (fn [index box] (paint (nth palette (mod index (count palette))) (str "p" tag "-" index) box))
                     (range) boxes)
        shapes (map (fn [form box [_ reference] values] (shape form box reference motion (drop 2 values)))
                    (icon-forms personality) boxes paints draws)
        label (escape name)]
    (str "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 " size " " size "\" width=\"" size
         "\" height=\"" size "\" role=\"img\" aria-label=\"" label "\">\n"
         "<title>" label "</title>\n"
         (when-let [definitions (seq (keep first paints))]
           (str "<defs>" (apply str definitions) "</defs>\n"))
         "<rect x=\"2\" y=\"2\" width=\"92\" height=\"92\" rx=\"20\" fill=\"" tile-color "\"/>\n"
         (str/join "\n" shapes)
         "\n</svg>\n")))

(defn data-uri
  "A self-contained image source for an <img>, a canvas or a device display page."
  [svg]
  (str "data:image/svg+xml;base64,"
       (.encodeToString (Base64/getEncoder) (.getBytes ^String svg StandardCharsets/UTF_8))))
