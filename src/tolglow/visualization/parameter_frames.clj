(ns tolglow.visualization.parameter-frames
  "Shared parameter-visualization frames (#625). Studio, the Push display and
  a Live parameter device can each mount a view of one running instance's
  parameter: its recent history over a beat window, drawn in a style at a
  size. When two mounted views ask for the same instance, parameter, window
  and style at compatible sizes, one frame is rendered and presented to both.

  - A *request* is {:instance-id :param :window-beats :style :width :height
    :dpr}. Sizes are CSS pixels; the frame is width x dpr by height x dpr
    device pixels.
  - Requests share a *render key* when instance, parameter, window, style and
    device pixel ratio match and the size is within `scale-tolerance` of the
    key's size (the consumer scales by at most 10%, which keeps text and lines
    sharp). Anything else gets its own render.
  - Each *consumer* has a mailbox of at most `:queue` frames (default 1). A
    frame arriving at a full mailbox replaces the oldest and counts a drop, so
    a hidden view or a slow host never holds back the others. A lease that is
    not read within `:lease-ms` expires, and the last lease of a key leaving
    stops its rendering. The worker thread ends when no key remains.
  - Input samples are shared per instance and parameter: every key showing
    that parameter reads one sample stream, taken once per cycle from the
    committed show snapshot (`:snapshot!`), never from the owner thread and
    never by evaluating a signal again.
  - A frame holds its PNG as a read-only ByteBuffer: in-process consumers
    share it without a copy, and HTTP consumers write the same bytes.

  The service measures what it does: renders and presentations per key,
  drops, and the time spent sampling, drawing and encoding."
  (:require [tolglow.authoring.parameter-controls :as controls])
  (:import [java.awt BasicStroke Color Font RenderingHints]
           [java.awt.geom Path2D$Double]
           [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream]
           [java.nio ByteBuffer]
           [javax.imageio ImageIO]))

(def scale-tolerance
  "Largest relative size difference at which a view reuses another's frame."
  0.1)

(def styles #{:history :history-filled})

(def max-keys 32)

(def max-consumers 64)

(def max-pixels
  "Largest frame, in device pixels: a 2x 1280x400 strip."
  (* 2560 800))

(defn- fail! [message data] (throw (ex-info message data)))

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

(defn- now [] (System/currentTimeMillis))

(defn validate-request
  "A canonical request, or an exception naming what is wrong."
  [{:keys [instance-id param window-beats style width height dpr] :or {window-beats 4 style :history dpr 1}}]
  (let [param (if (string? param) (keyword param) param)
        style (if (string? style) (keyword style) style)
        dpr (double dpr)]
    (when-not (and (some? instance-id) (keyword? param)) (fail! "A frame needs an instance and a parameter" {}))
    (when-not (contains? #{1 2 4 8 16 32} (long window-beats)) (fail! "The window is 1, 2, 4, 8, 16 or 32 beats" {:window-beats window-beats}))
    (when-not (styles style) (fail! "Unknown frame style" {:style style}))
    (when-not (and (pos-int? width) (pos-int? height) (<= 8 width 1280) (<= 8 height 400)) (fail! "Frame size is 8-1280 by 8-400" {}))
    (when-not (<= 1.0 dpr 3.0) (fail! "Device pixel ratio is 1-3" {:dpr dpr}))
    (when (> (* width height dpr dpr) max-pixels) (fail! "Frame too large" {}))
    {:instance-id (str instance-id) :param param :window-beats (long window-beats) :style style
     :width width :height height :dpr dpr}))

(defn compatible?
  "Can a view of request b show the frame rendered for request a?"
  [a b]
  (and (= (select-keys a [:instance-id :param :window-beats :style :dpr])
          (select-keys b [:instance-id :param :window-beats :style :dpr]))
       (<= (Math/abs (- 1.0 (/ (double (:width b)) (:width a)))) scale-tolerance)
       (<= (Math/abs (- 1.0 (/ (double (:height b)) (:height a)))) scale-tolerance)))

;; ---------------------------------------------------------------------------
;; Drawing

(defn- sample-value
  "A number to plot for a value, or nil: numbers as they are, booleans as 0/1."
  [value]
  (cond (finite? value) (double value)
        (boolean? value) (if value 1.0 0.0)))

(defn render-png
  "Draw a parameter's history into a PNG. samples are [[beat value] ..] in
  beat order; beat is the transport beat at the frame's right edge. The label
  and decimals come from the shared control projection."
  [{:keys [width height dpr style window-beats]} {:keys [samples beat schema param]}]
  (let [w (long (Math/round (* width dpr))) h (long (Math/round (* height dpr)))
        image (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics image)
        [lo hi] (or (controls/bounds schema)
                    (let [vs (keep (comp sample-value second) samples)]
                      (if (seq vs) [(reduce min vs) (reduce max vs)] [0.0 1.0])))
        span (if (> hi lo) (- hi lo) 1.0)
        start (- beat window-beats)
        x (fn [b] (* w (/ (- b start) window-beats)))
        y (fn [v] (- h (* (- h (* 2 dpr)) (/ (- v lo) span)) dpr))
        label (controls/title param schema)
        last-value (some-> (peek samples) second sample-value)]
    (try
      (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (.setRenderingHint g RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
      (.setColor g (Color. 18 20 23))
      (.fillRect g 0 0 w h)
      ;; One line per beat on the musical grid.
      (.setColor g (Color. 52 58 64))
      (doseq [b (range (Math/ceil start) (inc (Math/floor beat)))]
        (let [gx (int (Math/round (double (x b))))] (.drawLine g gx 0 gx h)))
      (let [path (Path2D$Double.)
            points (keep (fn [[b v]] (when-let [v (sample-value v)] [(x b) (y (max lo (min hi v)))])) samples)]
        (when (seq points)
          (let [[[x0 y0]] points]
            (.moveTo path (double x0) (double y0))
            (doseq [[px py] (rest points)] (.lineTo path (double px) (double py))))
          (when (= :history-filled style)
            (let [fill (Path2D$Double. path)
                  [lx] (last points)]
              (.lineTo fill (double lx) (double h))
              (.lineTo fill (double (ffirst points)) (double h))
              (.closePath fill)
              (.setColor g (Color. 120 200 170 70))
              (.fill g fill)))
          (.setColor g (Color. 152 212 188))
          (.setStroke g (BasicStroke. (float (* 1.5 dpr))))
          (.draw g path)))
      (.setFont g (Font. Font/SANS_SERIF Font/PLAIN (int (Math/round (* 11 dpr)))))
      (.setColor g (Color. 220 224 228))
      (.drawString g (str label (when last-value (str "  " (format (str "%." (controls/decimal-places schema) "f") last-value))))
                   (int (* 4 dpr)) (int (* 13 dpr)))
      (finally (.dispose g)))
    (let [out (ByteArrayOutputStream.)]
      (ImageIO/write image "png" out)
      (.toByteArray out))))

;; ---------------------------------------------------------------------------
;; Service

(defn create
  "A frame service. Options:
    :snapshot!  (fn [] committed show state) read on the worker each cycle
    :period-ms  cycle period, default 100 (10 frames a second, independent of
                show output)
    :lease-ms   a lease not read for this long expires, default 5000
    :render     (fn [render-request input] png-bytes), default render-png
    :worker?    false runs no thread; the caller runs step! (tests)
  Nothing runs until the first subscription."
  [{:keys [snapshot! period-ms lease-ms render worker?] :or {period-ms 100 lease-ms 5000 worker? true}}]
  (when-not (fn? snapshot!) (fail! "A frame service needs a snapshot source" {}))
  {:snapshot! snapshot! :period-ms period-ms :lease-ms lease-ms :render (or render render-png) :worker? worker?
   :lock (Object.)
   :state (atom {:keys {} :leases {} :samples {} :serial 0 :worker nil
                 :counts {:renders 0 :presentations 0 :drops 0 :expired 0 :cycles 0}
                 :cost {:sample-ns 0 :render-ns 0 :max-render-ns 0}})})

(declare ensure-worker!)

(defn subscribe!
  "Mount a view. consumer: {:id name :queue n (1-8)}. Returns
    {:lease id :key render-key :render {:width :height :dpr} :shared? bool}
  :shared? is true when the view joined a frame another view already renders."
  [service request {:keys [id queue] :or {queue 1}}]
  (let [request (validate-request request)]
    (when-not (<= 1 queue 8) (fail! "A consumer queue holds 1-8 frames" {:queue queue}))
    (let [result
          (locking (:lock service)
            (let [{:keys [keys leases]} @(:state service)
                  _ (when (>= (count leases) max-consumers) (fail! "Too many frame consumers" {}))
                  joined (some (fn [[k entry]] (when (compatible? (:request entry) request) k)) keys)
                  _ (when (and (nil? joined) (>= (count keys) max-keys)) (fail! "Too many frame renders" {}))
                  serial (:serial (swap! (:state service) update :serial inc))
                  render-key (or joined (str "k" serial))
                  lease (str "l" serial)]
              (swap! (:state service)
                     (fn [s]
                       (cond-> (assoc-in s [:leases lease] {:key render-key :consumer id :queue queue :mailbox []
                                                            :read-at (now) :presented 0 :dropped 0 :request request})
                         (nil? joined) (assoc-in [:keys render-key] {:request request :renders 0 :sequence 0 :frame nil}))))
              {:lease lease :key render-key :shared? (some? joined)
               :render (select-keys (get-in @(:state service) [:keys render-key :request]) [:width :height :dpr])}))]
      (when (:worker? service) (ensure-worker! service))
      result)))

(defn- drop-lease [s lease]
  (let [k (get-in s [:leases lease :key])
        s (update s :leases dissoc lease)]
    (if (some #(= k (:key %)) (vals (:leases s)))
      s
      (update s :keys dissoc k))))

(defn unsubscribe!
  "Unmount a view. The last view of a key stops its rendering."
  [service lease]
  (locking (:lock service)
    (swap! (:state service) drop-lease lease))
  nil)

(defn take-frame!
  "The oldest waiting frame of a lease, removed from its mailbox, or nil.
  Reading renews the lease. A frame is
    {:key :sequence :width :height :dpr :rendered-at :png (read-only ByteBuffer)}"
  [service lease]
  (locking (:lock service)
    (when (get-in @(:state service) [:leases lease])
      (let [frame (first (get-in @(:state service) [:leases lease :mailbox]))]
        (swap! (:state service) (fn [s] (-> s
                                            (assoc-in [:leases lease :read-at] (now))
                                            (update-in [:leases lease :mailbox] #(vec (rest %))))))
        frame))))

(defn latest-frame
  "The key's newest frame without touching any mailbox (for a late joiner)."
  [service lease]
  (let [s @(:state service)]
    (get-in s [:keys (get-in s [:leases lease :key]) :frame])))

(defn png-bytes
  "A copy of a frame's PNG bytes."
  [frame]
  (let [b (.duplicate ^ByteBuffer (:png frame)) out (byte-array (.remaining b))]
    (.get b out)
    out))

(defn- history-input
  "Append this cycle's committed value of every shown parameter to its shared
  sample stream, trimmed to the widest window any key shows."
  [samples snapshot wanted]
  (let [model (:model snapshot)
        beat (double (get-in model [:transport :beat] 0.0))
        overrides (or (:resolved-param-overrides snapshot) (get-in snapshot [:modulation-view :overrides]))]
    (into {}
          (for [[[instance-id param :as k] window] wanted
                :let [instance (get-in model [:instances instance-id])
                      value (when instance (get (merge (:params instance) (get overrides instance-id)) param))
                      previous (get samples k [])
                      ;; A backwards seek starts the stream again.
                      previous (if (and (seq previous) (< beat (first (peek previous)))) [] previous)
                      kept (into [] (drop-while #(< (first %) (- beat window 1))) previous)]]
            [k (cond-> kept (and instance (some? value)) (conj [beat value]))]))))

(defn- cycle!
  "One worker cycle: sample once per shown parameter, render each key once,
  present the frame to each of its leases and expire unread leases."
  [service]
  (let [{:keys [snapshot! render lease-ms]} service
        snapshot (snapshot!)
        model (:model snapshot)
        beat (double (get-in model [:transport :beat] 0.0))
        t0 (System/nanoTime)
        keys-now (:keys @(:state service))
        wanted (reduce (fn [m [_ {{:keys [instance-id param window-beats]} :request}]]
                         (update m [instance-id param] (fnil max 0) window-beats))
                       {} keys-now)
        samples (history-input (:samples @(:state service)) snapshot wanted)
        t1 (System/nanoTime)
        frames (into {}
                     (for [[k {:keys [request]}] keys-now
                           :let [{:keys [instance-id param]} request
                                 instance (get-in model [:instances instance-id])
                                 schema (get-in instance [:definition :params param])
                                 start (System/nanoTime)
                                 png (render request {:samples (get samples [instance-id param] []) :beat beat
                                                      :schema schema :param param})
                                 ns (- (System/nanoTime) start)]]
                       [k {:png (.asReadOnlyBuffer (ByteBuffer/wrap png)) :ns ns :bytes (count png)
                           :width (long (Math/round (* (:width request) (:dpr request))))
                           :height (long (Math/round (* (:height request) (:dpr request)))) :dpr (:dpr request)}]))
        stamp (now)]
    (locking (:lock service)
      (swap! (:state service)
             (fn [s]
               (let [s (reduce (fn [s [k {:keys [png width height dpr ns bytes]}]]
                                 (if-not (get-in s [:keys k])
                                   s
                                   (let [sequence (inc (get-in s [:keys k :sequence]))
                                         frame {:key k :sequence sequence :width width :height height :dpr dpr
                                                :rendered-at stamp :beat beat :png png :bytes bytes}]
                                     (-> s
                                         (update-in [:keys k] assoc :sequence sequence :frame frame)
                                         (update-in [:keys k :renders] inc)
                                         (update-in [:counts :renders] inc)
                                         (update-in [:cost :render-ns] + ns)
                                         (update-in [:cost :max-render-ns] max ns)
                                         (as-> s (reduce (fn [s [lease {:keys [key queue mailbox]}]]
                                                           (if (not= key k)
                                                             s
                                                             (let [full? (>= (count mailbox) queue)]
                                                               (-> s
                                                                   (assoc-in [:leases lease :mailbox]
                                                                             (conj (if full? (vec (rest mailbox)) mailbox) frame))
                                                                   (update-in [:leases lease :presented] inc)
                                                                   (cond-> full? (update-in [:leases lease :dropped] inc))
                                                                   (update-in [:counts :presentations] inc)
                                                                   (cond-> full? (update-in [:counts :drops] inc))))))
                                                         s (:leases s)))))))
                               s frames)
                     expired (for [[lease {:keys [read-at]}] (:leases s) :when (> (- stamp read-at) lease-ms)] lease)
                     s (reduce drop-lease s expired)]
                 (-> s
                     (assoc :samples (select-keys samples (keys (reduce (fn [m [_ {{:keys [instance-id param]} :request}]]
                                                                         (assoc m [instance-id param] true))
                                                                       {} (:keys s)))))
                     (update-in [:counts :expired] + (count expired))
                     (update-in [:counts :cycles] inc)
                     (update-in [:cost :sample-ns] + (- t1 t0)))))))))

(defn- ensure-worker!
  "Start the worker when a key exists and none runs. It ends itself when the
  last key leaves."
  [service]
  (locking (:lock service)
    (let [{:keys [worker keys]} @(:state service)]
      (when (and (seq keys) (not (and worker (.isAlive ^Thread worker))))
        (let [t (Thread. (fn []
                           (loop []
                             (let [start (System/currentTimeMillis)]
                               (try (cycle! service)
                                    (catch Throwable error
                                      (swap! (:state service) assoc :last-error (.getMessage error))))
                               (when (locking (:lock service)
                                       (if (seq (:keys @(:state service)))
                                         true
                                         (do (swap! (:state service) assoc :worker nil) false)))
                                 (let [left (- (:period-ms service) (- (System/currentTimeMillis) start))]
                                   (when (pos? left) (Thread/sleep (long left))))
                                 (when-not (:stopped? @(:state service)) (recur))))))
                         "tolglow-parameter-frames")]
          (.setDaemon t true)
          (swap! (:state service) assoc :worker t)
          (.start t))))))

(defn step!
  "Run one cycle on the calling thread (tests and the REPL)."
  [service]
  (cycle! service))

(defn stop!
  "End every lease and the worker."
  [service]
  (locking (:lock service)
    (swap! (:state service) assoc :keys {} :leases {} :samples {} :stopped? true))
  nil)

(defn status
  "Renders, presentations, drops and time per cycle, and each key with its
  views. Times are averages over the cycles run so far."
  [service]
  (let [{:keys [keys leases counts cost worker last-error]} @(:state service)
        renders (max 1 (:renders counts))
        cycles (max 1 (:cycles counts))]
    {:running? (boolean (and worker (.isAlive ^Thread worker)))
     :counts counts
     :cost {:sample-us (/ (:sample-ns cost) cycles 1000.0)
            :render-us (/ (:render-ns cost) renders 1000.0)
            :max-render-us (/ (:max-render-ns cost) 1000.0)}
     :keys (vec (for [[k {:keys [request renders sequence frame]}] (sort-by key keys)]
                  {:key k :request request :renders renders :sequence sequence :bytes (:bytes frame)
                   :views (vec (for [[lease {:keys [key consumer presented dropped request]}] (sort-by key leases)
                                     :when (= key k)]
                                 {:lease lease :consumer consumer :presented presented :dropped dropped
                                  :size [(:width request) (:height request)]}))}))
     :last-error last-error}))
