(ns tolglow.video.preview
  "Studio and REPL projections of video mapping (#672): source status, the
  colours each mapped strip samples from the latched frame at its authored
  placement, and a downscaled JPEG of a latched frame for the 2D editor. All
  of it reads the latch the lighting frame already took; nothing here pulls a
  new frame from a source."
  (:require [tolglow.video.hub :as hub]
            [tolglow.video.sampler :as sampler]
            [tolglow.video.surface :as surface])
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

(def max-projected-contacts
  "Contacts listed per surface view; :count still gives the whole number."
  4096)

(defn- surface-view [cache fixtures resolved]
  (let [contacts (cache [:contacts resolved fixtures] #(surface/contacts resolved fixtures))]
    {:resolved resolved :count (count contacts)
     :contacts (if (> (count contacts) max-projected-contacts) (subvec contacts 0 max-projected-contacts) contacts)}))

(defn surface-views
  "Surfaces for the Room (#674). :authored has every configured surface at
  its saved pose. :running has each running surface output at the pose its
  effect evaluated on its last frame, read from the instance state, so the
  projection never evaluates a placement, modulation or the musical clock a
  second time. Each view lists the heads in contact: their surface uv,
  distance and contact weight. Fixture colours come from the committed
  output, not from here."
  [model fixtures cache]
  {:authored (into {} (for [[id spec] (get-in model [:settings :video :surfaces])]
                        [id (surface-view cache fixtures (surface/resolve-surface spec nil 0))]))
   :running (vec (for [instance (sort-by (juxt :order :id) (vals (:instances model)))
                       [i {:keys [surface resolved]}] (sort-by key (get-in instance [:state :tolglow.video/surfaces]))]
                   (merge {:instance-id (:id instance) :cue-id (:cue-id instance) :output i :surface surface}
                          (surface-view cache fixtures resolved))))})

(defn project
  "The :video key of the show snapshot. model, when given, adds :surfaces."
  ([video-hub latch fixtures cache] (project video-hub latch fixtures cache nil))
  ([video-hub latch fixtures cache model]
   (cond-> {:sources (hub/status video-hub latch)
            :frozen? (boolean (:frozen? latch))
            :mappings (mapping-samples latch fixtures cache)}
     model (assoc :surfaces (surface-views model fixtures cache)))))

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
