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

(defn shader-sources
  "Status rows for the shader feeds in the latch (#673), in the same shape as
  hub sources, so strips can be mapped onto a running shader cue's feed."
  [latch shader-status now-ms]
  (vec (for [[feed rows] (sort-by (comp str key) (group-by :feed (:instances shader-status)))
             :let [f (get-in latch [:frames feed])
                   errors (keep :error rows)
                   age (when-let [t (:captured-at-ms f)] (max 0 (- now-ms t)))]]
         (merge {:id feed :kind :shader :label (str "Shader · " (name feed)) :connected? (some? f)
                 :instances (mapv :instance-id rows) :fps (:fps (first rows))
                 :frozen? (boolean (:frozen? latch))
                 ;; A static shader publishes once; it is not stale while its cue runs.
                 :stale? (nil? f) :age-ms age}
                (when f {:width (:width f) :height (:height f) :sequence (:sequence f)})
                (when (seq errors) {:error (:message (first errors)) :code (:code (first errors))})))))

(defn project
  "The :video key of the show snapshot."
  ([video-hub latch fixtures cache] (project video-hub latch fixtures cache nil))
  ([video-hub latch fixtures cache shader-status]
   (cond-> {:sources (into (hub/status video-hub latch)
                           (when shader-status (shader-sources latch shader-status ((:now-ms video-hub)))))
            :frozen? (boolean (:frozen? latch))
            :mappings (mapping-samples latch fixtures cache)}
     shader-status (assoc :shaders shader-status))))

(def max-jpeg-dimension
  "Largest served preview width or height, whatever the caller asks for."
  1920)

(def max-jpeg-bytes
  "Largest served preview JPEG; a larger encoding is refused."
  (* 4 1024 1024))

(defn frame-jpeg
  "JPEG bytes of a latched frame scaled to at most max-width (and never more
  than `max-jpeg-dimension` on either side), or nil without a frame. Throws
  ex-info with :code :video/preview-too-large past `max-jpeg-bytes`."
  ^bytes [latch source-id max-width]
  (when-let [f (get-in latch [:frames source-id])]
    (let [w (:width f) h (:height f)
          bound (max 1 (min (long max-width) max-jpeg-dimension))
          step (max 1 (long (Math/ceil (/ (double w) bound))) (long (Math/ceil (/ (double h) max-jpeg-dimension))))
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
        (when (> (.size out) max-jpeg-bytes)
          (throw (ex-info "The preview frame is too large to serve" {:code :video/preview-too-large :bytes (.size out)})))
        (.toByteArray out)))))
