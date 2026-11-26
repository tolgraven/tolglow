(ns tolglow.video.source
  "Video frame sources for 2D strip mapping (#672).

  A frame is an immutable BGRA8 snapshot: the pixel array is copied at the
  source boundary and never written afterwards, so the lighting frame can
  share it between effect workers. Sources only return their latest frame;
  scheduling, latching and transport holds belong to `tolglow.video.hub`.

  Kinds:
  - test-pattern: a deterministic labelled colour grid, shared with the native
    Syphon self-test (`native/VideoBridge`), for tests and calibration.
  - image: a still read once from a PNG or JPEG file.
  - feed: the memory-mapped TGVF file written by the native video bridge.
  - pushed: frames supplied by the caller, for tests and future shader feeds."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.lang.invoke VarHandle]
           [java.nio ByteBuffer ByteOrder MappedByteBuffer]
           [java.nio.channels FileChannel FileChannel$MapMode]
           [java.nio.charset StandardCharsets]
           [java.nio.file LinkOption OpenOption StandardOpenOption]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(defrecord Frame [source-id width height ^bytes pixels sequence captured-at-ms label])

(defn frame
  "Build a frame from owned BGRA bytes (width × height × 4)."
  [{:keys [source-id width height pixels sequence captured-at-ms label]}]
  (when-not (and (pos-int? width) (pos-int? height) (bytes? pixels)
                 (= (alength ^bytes pixels) (* 4 width height)))
    (throw (ex-info "A frame needs width × height × 4 BGRA bytes" {:width width :height height})))
  (->Frame source-id width height pixels (or sequence 0) captured-at-ms label))

(defn rgb-at
  "sRGB bytes [r g b] at integer pixel x, y. For tests and inspection."
  [^Frame frame x y]
  (let [^bytes px (:pixels frame) i (* 4 (+ x (* y (long (:width frame)))))]
    [(bit-and 0xff (aget px (+ i 2))) (bit-and 0xff (aget px (+ i 1))) (bit-and 0xff (aget px i))]))

(defprotocol VideoSource
  (latest [source] "The newest complete frame, or nil before the first one.")
  (describe [source] "Portable status: :kind, :label, :connected? and :error.")
  (close! [source] "Release files or processes this source owns."))

;; Test pattern --------------------------------------------------------------

(def grid-columns 8)

(def grid-rows 4)

(defn grid-color
  "sRGB [r g b] bytes of a labelled grid cell. The native self-test publishes
  the same colours through Syphon."
  [column row]
  (let [r (Math/round (double (/ (* 255.0 column) (dec grid-columns))))]
    [r (Math/round (double (/ (* 255.0 row) (dec grid-rows)))) (- 255 r)]))

(defn- fill-bgra! [^bytes px width height color-at]
  (dotimes [y height]
    (dotimes [x width]
      (let [[r g b] (color-at x y) i (* 4 (+ x (* y width)))]
        (aset px i (unchecked-byte b))
        (aset px (+ i 1) (unchecked-byte g))
        (aset px (+ i 2) (unchecked-byte r))
        (aset px (+ i 3) (unchecked-byte 255)))))
  px)

(defn test-pattern-frame
  "A deterministic frame. :grid is the 8 × 4 colour grid; :gradient ramps red
  across u and green down v. `shift` cycles grid columns, so a test can tell
  consecutive frames apart."
  [{:keys [source-id width height pattern shift sequence captured-at-ms]
    :or {width 640 height 360 pattern :grid shift 0 sequence 0}}]
  (let [px (byte-array (* 4 width height))
        color-at (case pattern
                   :grid (fn [x y] (grid-color (mod (+ shift (quot (* x grid-columns) width)) grid-columns)
                                               (quot (* y grid-rows) height)))
                   :gradient (fn [x y] [(quot (* 255 x) (max 1 (dec width))) (quot (* 255 y) (max 1 (dec height))) 0]))]
    (frame {:source-id source-id :width width :height height :pixels (fill-bgra! px width height color-at)
            :sequence sequence :captured-at-ms captured-at-ms :label (str "Test pattern · " (name pattern))})))

(defn test-pattern
  "A source that always returns the same deterministic frame."
  [id {:keys [width height pattern] :as options}]
  (let [f (test-pattern-frame (assoc options :source-id id :sequence 1))]
    (reify VideoSource
      (latest [_] f)
      (describe [_] {:kind :test-pattern :label (:label f) :connected? true})
      (close! [_] nil))))

;; Pushed frames -------------------------------------------------------------

(defn pushed
  "A source whose frames the owner supplies with `push!`."
  [id]
  (let [state (atom nil)]
    (reify
      VideoSource
      (latest [_] @state)
      (describe [_] {:kind :pushed :label (str id) :connected? (some? @state)})
      (close! [_] (reset! state nil))
      clojure.lang.IFn
      (invoke [_ f] (reset! state (assoc f :source-id id))))))

(defn push!
  "Offer a frame to a pushed source."
  [source frame] (source frame))

;; Still images --------------------------------------------------------------

(def max-image-dimension
  "Largest still width or height decoded (pixels)."
  8192)

(def max-image-bytes
  "Largest still file read, checked before decoding."
  (* 64 1024 1024))

(def resource-prefix
  "Bundled stills live on the classpath under this prefix."
  "video/")

(defn- refuse! [code message data]
  (throw (ex-info message (assoc data :code code :errors [{:path [:file] :message message}]))))

(defn- inside? [^java.io.File root ^java.io.File file]
  ;; Strictly below the root: the show directory itself is not a video file.
  (let [r (.getCanonicalPath root) f (.getCanonicalPath file)]
    (.startsWith f (str r java.io.File/separator))))

(defn resolve-file
  "Resolve a source file against the allowed roots and return the canonical
  java.io.File, or throw ex-info with a :code. Relative paths resolve against
  the first root (the show directory). A path with a `..` segment, or one
  whose canonical form (symlinks followed) leaves every root, is refused, so
  a source can never read and re-serve an arbitrary file the server can read."
  ^java.io.File [file roots]
  (let [text (str file)
        segments (str/split text #"[/\\]")]
    (when (or (str/blank? text) (some #{".."} segments))
      (refuse! :video/path-traversal "Video files may not use .. path segments" {:file text}))
    (when (empty? roots)
      (refuse! :video/no-file-roots "No show directory is configured for video files" {:file text}))
    (let [candidate (io/file text)
          resolved (if (.isAbsolute candidate) candidate (io/file (first roots) text))
          canonical (.getCanonicalFile resolved)]
      (when-not (some #(inside? (io/file %) canonical) roots)
        (refuse! :video/path-outside-show "Video files must be inside the show directory" {:file text}))
      canonical)))

(defn- decoded-size
  "Width and height from the image header, without decoding the pixels."
  [input]
  (with-open [stream (ImageIO/createImageInputStream input)]
    (let [readers (when stream (ImageIO/getImageReaders stream))]
      (when-not (and readers (.hasNext readers))
        (refuse! :video/unreadable-image "Not a readable PNG or JPEG image" {}))
      (let [^javax.imageio.ImageReader reader (.next readers)]
        (try (.setInput reader stream true true)
             [(.getWidth reader 0) (.getHeight reader 0)]
             (finally (.dispose reader)))))))

(defn- check-image! [bytes-count [w h] {:keys [max-bytes max-dimension]
                                         :or {max-bytes max-image-bytes max-dimension max-image-dimension}}]
  (when (> bytes-count max-bytes)
    (refuse! :video/image-too-large (str "Video stills are limited to " max-bytes " bytes") {:bytes bytes-count}))
  (when (or (> w max-dimension) (> h max-dimension))
    (refuse! :video/image-too-large (str "Video stills are limited to " max-dimension " × " max-dimension " pixels")
             {:width w :height h})))

(defn- image->frame [id ^BufferedImage image captured-at-ms label]
  (let [w (.getWidth image) h (.getHeight image)
        argb (.getRGB image 0 0 w h nil 0 w)
        px (byte-array (* 4 w h))]
    (dotimes [i (* w h)]
      (let [c (aget argb i) o (* 4 i)]
        (aset px o (unchecked-byte c))
        (aset px (+ o 1) (unchecked-byte (bit-shift-right c 8)))
        (aset px (+ o 2) (unchecked-byte (bit-shift-right c 16)))
        (aset px (+ o 3) (unchecked-byte (bit-shift-right c 24)))))
    (frame {:source-id id :width w :height h :pixels px :sequence 1
            :captured-at-ms captured-at-ms :label label})))

(defn- read-bounded
  "All bytes of a stream, refusing past limit without reading further."
  ^bytes [^java.io.InputStream in limit]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (let [buffer (byte-array 65536)]
      (loop [total 0]
        (let [n (.read in buffer)]
          (when (pos? n)
            (when (> (+ total n) limit)
              (refuse! :video/image-too-large (str "Video stills are limited to " limit " bytes") {}))
            (.write out buffer 0 n)
            (recur (+ total n)))))
      (.toByteArray out))))

(defn- bytes->frame [id ^bytes data captured-at-ms label limits]
  (check-image! (alength data) (decoded-size (java.io.ByteArrayInputStream. data)) limits)
  (image->frame id (or (ImageIO/read (java.io.ByteArrayInputStream. data))
                       (refuse! :video/unreadable-image "Not a readable PNG or JPEG image" {}))
                captured-at-ms label))

(def ^:private no-follow
  "Open the checked path itself: a symlink swapped in after the check is refused."
  (into-array OpenOption [StandardOpenOption/READ LinkOption/NOFOLLOW_LINKS]))

(defn image-frame
  "Read a PNG/JPEG file into a frame after checking its byte size and header
  dimensions. The file is opened once, without following a final symlink, and
  read into memory up to the byte limit, so the bytes checked are the bytes
  decoded. ImageIO runs headless. `limits` may override :max-bytes and
  :max-dimension."
  ([id file captured-at-ms] (image-frame id file captured-at-ms {}))
  ([id file captured-at-ms limits]
   (let [f (io/file file)]
     (when-not (.isFile f)
       (refuse! :video/unreadable-image "A video still must be a regular file" {:file (str file)}))
     (let [data (with-open [channel (FileChannel/open (.toPath f) ^"[Ljava.nio.file.OpenOption;" no-follow)]
                  (read-bounded (java.nio.channels.Channels/newInputStream channel) (:max-bytes limits max-image-bytes)))]
       (bytes->frame id data captured-at-ms (.getName f) limits)))))

(defn resource-frame
  "Read a still bundled with the application, from the classpath under
  `resource-prefix`."
  ([id path captured-at-ms] (resource-frame id path captured-at-ms {}))
  ([id path captured-at-ms limits]
   (let [text (str path)]
     (when (or (str/blank? text) (str/starts-with? text "/") (some #{".."} (str/split text #"[/\\]")))
       (refuse! :video/path-traversal "Bundled video stills are named relative to the video resources" {:resource text}))
     (let [url (or (io/resource (str resource-prefix text))
                   (refuse! :video/missing-resource "No bundled video still has that name" {:resource text}))
           data (with-open [in (io/input-stream url)] (read-bounded in (:max-bytes limits max-image-bytes)))]
       (bytes->frame id data captured-at-ms (last (str/split text #"/")) limits)))))

(defn image
  "A still from :file (inside `roots`) or a bundled :resource."
  ([id config] (image id config nil))
  ([id {:keys [file resource]} roots]
   (let [frame (if resource
                 (resource-frame id resource (System/currentTimeMillis))
                 (image-frame id (resolve-file file roots) (System/currentTimeMillis)))]
     (reify VideoSource
       (latest [_] frame)
       (describe [_] {:kind :image :label (:label frame) :connected? true})
       (close! [_] nil)))))

;; TGVF memory-mapped feed ---------------------------------------------------

(def header-bytes 320)

(defn- acquire-long
  "Read the seqlock word, ordered before the reads that follow it."
  ^long [^ByteBuffer buffer offset]
  (let [value (.getLong buffer (int offset))]
    (VarHandle/acquireFence)
    value))

(defn- recheck-long
  "Read the seqlock word again after the pixel copy."
  ^long [^ByteBuffer buffer offset]
  (VarHandle/loadLoadFence)
  (.getLong buffer (int offset)))

(defn- u32 ^long [^ByteBuffer buffer offset] (Integer/toUnsignedLong (.getInt buffer (int offset))))

(defn read-feed
  "Copy the latest stable frame from a mapped TGVF buffer, or nil while it is
  being written, empty or malformed. A reader retries a torn copy up to three
  times; the writer's even sequence brackets every completed frame."
  [^ByteBuffer buffer id]
  (when (and (>= (.capacity buffer) header-bytes)
             (= 0x46564754 (.getInt buffer 0)))                    ; "TGVF" little endian
    (loop [attempt 0]
      (when (< attempt 3)
        (let [before (acquire-long buffer 8)
              w (u32 buffer 16) h (u32 buffer 20) stride (u32 buffer 24)
              frames (.getLong buffer 32) written (.getLong buffer 40)
              connected? (= 1 (bit-and 1 (u32 buffer 48)))
              capacity (u32 buffer 52)]
          (cond
            (odd? before) (do (Thread/onSpinWait) (recur (inc attempt)))
            (or (zero? w) (zero? h) (< stride (* 4 w)) (> (* stride h) capacity)
                (> (+ header-bytes (* stride h)) (.capacity buffer)))
            nil
            :else
            (let [px (byte-array (* 4 w h))
                  view (.duplicate buffer)]
              (dotimes [y h]
                (.position view (int (+ header-bytes (* y stride))))
                (.get view px (int (* y 4 w)) (int (* 4 w))))
              (if (= before (recheck-long buffer 8))
                (let [label-bytes (byte-array 256)
                      _ (.get (.position (.duplicate buffer) 64) label-bytes)
                      n (or (first (keep-indexed #(when (zero? %2) %1) label-bytes)) 256)]
                  (assoc (frame {:source-id id :width (int w) :height (int h) :pixels px :sequence frames
                                 :captured-at-ms (/ written 1000.0)
                                 :label (String. label-bytes 0 (int n) StandardCharsets/UTF_8)})
                         :connected? connected?))
                (recur (inc attempt))))))))))

(defn feed
  "Read the TGVF file at :file. The file is mapped once; a writer that
  recreates it at a new size is picked up on the next frame. With :roots, the
  path is checked against them again before every remap and opened without
  following a final symlink, so a file swapped for a link after the first
  check is never mapped."
  [id {:keys [file roots]}]
  (let [state (atom {})
        mapped (fn []
                 (let [f (io/file file)
                       {:keys [buffer size]} @state]
                   (if (and buffer (= size (.length f)))
                     buffer
                     (when (.isFile f)
                       (when roots (resolve-file file roots))
                       (with-open [channel (FileChannel/open (.toPath f) (if roots
                                                                           ^"[Ljava.nio.file.OpenOption;" no-follow
                                                                           (into-array OpenOption [StandardOpenOption/READ])))]
                         (let [buffer (.order (.map channel FileChannel$MapMode/READ_ONLY 0 (.size channel)) ByteOrder/LITTLE_ENDIAN)]
                           (swap! state assoc :buffer buffer :size (.length f) :word nil)
                           buffer))))))]
    (reify VideoSource
      (latest [_]
        ;; Copy pixels only when the writer's sequence moved; a torn read
        ;; keeps the previous complete frame.
        (try (when-let [^ByteBuffer buffer (mapped)]
               (let [word (.getLong buffer 8)
                     {:keys [frame] :as current} @state]
                 (if (and frame (= word (:word current)))
                   frame
                   (if-let [f (read-feed buffer id)]
                     (do (swap! state assoc :frame f :word word :connected? (:connected? f) :label (:label f) :error nil)
                         f)
                     frame))))
             (catch Exception e (swap! state assoc :error (or (ex-message e) (str (class e)))) nil)
             ;; Another process shrank the file under the mapping; the JVM
             ;; reports that fault as InternalError. Keep the last frame.
             (catch InternalError e (swap! state assoc :buffer nil :size nil :error (or (ex-message e) "Feed file shrank"))
               (:frame @state))))
      (describe [_] (let [{:keys [connected? label error]} @state]
                      {:kind :feed :label (or label (str (.getName (io/file file))))
                       :connected? (boolean connected?) :error error}))
      (close! [_] (reset! state {})))))

(defn write-feed!
  "Write a frame into a TGVF file in place, with the native bridge's layout
  and seqlock, so a reader that has the file mapped never sees it truncated.
  Used by tests and the QA harness to stand in for the helper."
  [file ^Frame frame {:keys [connected? frames written-at-us capacity]
                      :or {connected? true frames 1}}]
  (let [w (:width frame) h (:height frame)
        f (io/file file)
        capacity (long (or capacity (max (* 4 w h) (- (.length f) header-bytes))))
        size (+ header-bytes capacity)
        label (.getBytes (str (:label frame)) StandardCharsets/UTF_8)]
    (with-open [channel (FileChannel/open (.toPath f) (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/READ
                                                                              StandardOpenOption/WRITE]))]
      (when (< (.size channel) size)
        (.write channel (ByteBuffer/allocate 1) (long (dec size))))
      (let [^MappedByteBuffer buffer (.order (.map channel FileChannel$MapMode/READ_WRITE 0 size) ByteOrder/LITTLE_ENDIAN)
            sequence (.getLong buffer 8)
            sequence (if (odd? sequence) (inc sequence) sequence)]
        (.putLong buffer 8 (inc sequence))
        (VarHandle/fullFence)
        (.putInt buffer 0 0x46564754)
        (.putInt buffer 4 1)
        (.putInt buffer 16 w) (.putInt buffer 20 h) (.putInt buffer 24 (* 4 w)) (.putInt buffer 28 1)
        (.putLong buffer 32 frames)
        (.putLong buffer 40 (long (or written-at-us (* 1000 (System/currentTimeMillis)))))
        (.putInt buffer 48 (if connected? 1 0))
        (.putInt buffer 52 capacity)
        (let [labels (byte-array 256)]
          (System/arraycopy label 0 labels 0 (min 255 (alength label)))
          (.put ^ByteBuffer (.position (.duplicate buffer) 64) labels))
        (.put ^ByteBuffer (.position (.duplicate buffer) (int header-bytes)) ^bytes (:pixels frame))
        (VarHandle/fullFence)
        (.putLong buffer 8 (+ 2 sequence))
        (.force buffer)))
    file))

(defn open
  "Construct a source from portable settings. :image and :feed files must
  resolve inside `roots` (see `resolve-file`). Syphon sources are opened by
  the hub, which owns the helper process."
  ([id config] (open id config nil))
  ([id {:keys [kind] :as config} roots]
   (case kind
     :test-pattern (test-pattern id config)
     :image (image id config roots)
     :feed (feed id (assoc config :file (str (resolve-file (:file config) roots)) :roots roots))
     (throw (ex-info "Unsupported video source kind" {:kind kind})))))
