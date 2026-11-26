(ns tolglow.video.preview
  "Studio and REPL projections of video mapping (#672): source status, the
  colours each mapped strip samples from the latched frame at its authored
  placement, and a downscaled JPEG of a latched frame for the 2D editor. All
  of it reads the latch the lighting frame already took; nothing here pulls a
  new frame from a source."
  (:require [tolglow.video.hub :as hub]
            [tolglow.video.sampler :as sampler])
  (:import [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream]
           [javax.imageio ImageIO]))

(defn- srgb-byte [linear]
  (let [c (max 0.0 (min 1.0 (double linear)))
        e (if (<= c 0.0031308) (* 12.92 c) (- (* 1.055 (Math/pow c (/ 1 2.4))) 0.055))]
    (long (Math/round (* 255.0 e)))))

(defn hex
  "Display colour for a linear RGB sample."
  [[r g b]]
  (format "#%02x%02x%02x" (srgb-byte r) (srgb-byte g) (srgb-byte b)))

(defn mapping-samples
  "For each mapping with a latched frame: strip id → display colours in strip
  order, plus the frame size. Uses the authored placement; a running cue may
  move its own copy with placement parameters."
  [latch fixtures cache]
  (into {}
        (for [[id mapping] (:mappings latch)
              :let [f (get-in latch [:frames (:source mapping)])]]
          [id (if-not f
                {:source (:source mapping) :frame? false}
                (let [plan (cache [mapping fixtures (:width f) (:height f)]
                                  #(sampler/plan mapping fixtures (:width f) (:height f) nil))
                      sampled (sampler/sample plan f)
                      offsets (reductions + 0 (map (comp count :targets) (:strips plan)))]
                  {:source (:source mapping) :frame? true :size [(:width f) (:height f)] :sequence (:sequence f)
                   :strips (mapv (fn [strip start]
                                   {:id (:id strip) :pixels (count (:targets strip)) :unresolved? (:unresolved? strip)
                                    :colors (mapv (fn [i] (let [o (* 3 (+ start i))]
                                                            (hex [(aget sampled o) (aget sampled (+ o 1)) (aget sampled (+ o 2))])))
                                                  (range (count (:targets strip))))})
                                 (:strips plan) offsets)}))])))

(defn project
  "The :video key of the show snapshot."
  [video-hub latch fixtures cache]
  {:sources (hub/status video-hub latch)
   :frozen? (boolean (:frozen? latch))
   :mappings (mapping-samples latch fixtures cache)})

(defn frame-jpeg
  "JPEG bytes of a latched frame scaled to at most max-width, or nil."
  ^bytes [latch source-id max-width]
  (when-let [f (get-in latch [:frames source-id])]
    (let [w (:width f) h (:height f)
          step (max 1 (long (Math/ceil (/ (double w) max-width))))
          ;; A thin frame (a 1-row strip render target) still yields an image.
          ow (max 1 (quot w step)) oh (max 1 (quot h step))
          image (BufferedImage. ow oh BufferedImage/TYPE_INT_RGB)
          ^bytes px (:pixels f)]
      (dotimes [y oh]
        (dotimes [x ow]
          (let [i (* 4 (+ (* x step) (* y step w)))]
            (.setRGB image x y (bit-or (bit-shift-left (bit-and 0xff (aget px (+ i 2))) 16)
                                       (bit-shift-left (bit-and 0xff (aget px (+ i 1))) 8)
                                       (bit-and 0xff (aget px i)))))))
      (let [out (ByteArrayOutputStream.)]
        (ImageIO/write image "jpg" out)
        (.toByteArray out)))))
