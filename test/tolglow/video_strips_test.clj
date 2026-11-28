(ns tolglow.video-strips-test
  "Video sources, strip mapping geometry and sampling (#672), without a show."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.video :as video-output]
            [tolglow.video.hub :as hub]
            [tolglow.video.mapping :as mapping]
            [tolglow.video.preview :as preview]
            [tolglow.video.sampler :as sampler]
            [tolglow.video.source :as source])
  (:import [java.awt.image BufferedImage]
           [java.io File]
           [javax.imageio ImageIO]))

(defn- linear [byte]
  (let [c (/ byte 255.0)]
    (if (<= c 0.04045) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4))))

(defn- grid-linear [column row] (mapv linear (source/grid-color column row)))

(defn- close? [a b & [tolerance]]
  (every? #(< (Math/abs (double %)) (or tolerance 1e-3)) (map - a b)))

(defn- heads [layout n] (mapv (fn [i] {:id (keyword (str (name layout) "-" i)) :pixel-layout-id layout :pixel-index i
                                       :position [i 0 0] :channels {:red 1 :green 2 :blue 3}})
                              (range n)))

(def grid (source/test-pattern-frame {:source-id :grid :width 640 :height 360}))

(defn- sample-strip [strip fixtures frame & [placement]]
  (let [plan (sampler/plan {:id :m :source :grid :strips [strip]} fixtures (:width frame) (:height frame) placement)]
    (sampler/colors plan (sampler/sample plan frame))))

(deftest settings-validate-and-edit-through-one-operation-shape
  (is (= [] (mapping/settings-errors {})))
  (is (= [[:sources :shader/cam]] (map :path (mapping/settings-errors {:sources {:shader/cam {:kind :test-pattern}}})))
      "shader cue feeds own the shader namespace (#673)")
  (is (= [] (mapping/settings-errors {:sources {:s {:kind :syphon :app "A" :name "B" :max-width 1920 :max-height 1080}}})))
  (is (= [[:sources :s :max-width] [:sources :s :max-height]]
         (map :path (mapping/settings-errors {:sources {:s {:kind :syphon :app "A" :name "B" :max-width 100000 :max-height "1"}}})))
      "the helper sizes its feed file from these limits")
  (is (seq (mapping/settings-errors {:mappings {:a {:id :b :source :grid :strips []}}})))
  (is (seq (mapping/settings-errors {:mappings {:a {:id :a :source :grid
                                                   :strips [{:id :s :layout :t :points [[0 0]]}]}}})))
  (is (seq (mapping/settings-errors {:mappings {:a {:id :a :source :grid
                                                   :strips [{:id :s :layout :t :targets [:x] :points [[0 0] [1 1]]}]}}})))
  (is (seq (mapping/settings-errors {:sources {:x {:kind :syphon :app "Arena"}}})))
  (let [settings (-> {}
                     (mapping/apply-operation {:op :put-source :id :grid :source {:kind :test-pattern}})
                     (mapping/apply-operation {:op :put-mapping :mapping {:id :wall :source :grid}})
                     (mapping/apply-operation {:op :put-strip :mapping-id :wall
                                               :strip {:id :a :layout :tube :points [[0 0.5] [1 0.5]] :height 0.1}})
                     (mapping/apply-operation {:op :put-strip :mapping-id :wall
                                               :strip {:id :a :layout :tube :points [[0 0.25] [1 0.25]]}}))]
    (is (= [[0 0.25] [1 0.25]] (get-in settings [:mappings :wall :strips 0 :points])) "same id replaces")
    (is (thrown? clojure.lang.ExceptionInfo (mapping/apply-operation settings {:op :delete-source :id :grid}))
        "a source in use cannot be removed")
    (is (= [] (get-in (mapping/apply-operation settings {:op :delete-strip :mapping-id :wall :strip-id :a})
                      [:mappings :wall :strips])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (mapping/apply-operation settings {:op :put-strip :mapping-id :wall :strip {:id :b :points [[0 0]]}})))))

(deftest geometry-keeps-length-direction-and-height-independent
  (let [points [[0.25 0.5] [0.75 0.5]]
        aspect (/ 16.0 9.0)
        prints (mapping/footprints points 4 0.1 640 360)]
    (is (= 4 (count prints)))
    (is (close? [200.0 180.0] (:center (first prints))) "first pixel samples near the first point")
    (is (close? [440.0 180.0] (:center (last prints))))
    (is (close? [80.0 36.0] [(:along (first prints)) (:across (first prints))]))
    (testing "rotation is a rigid turn on the frame's pixels"
      (let [turned (mapping/rotate points 90 aspect)
            [a b] (mapping/footprints turned 1 0 640 360)
            length #(let [[[x0 y0] [x1 y1]] %] (Math/hypot (* 640 (- x1 x0)) (* 360 (- y1 y0))))]
        (is (close? [0.0 1.0] (:direction (first (mapping/footprints turned 2 0 640 360)))))
        (is (< (Math/abs (- (length points) (length turned))) 1e-6))
        (is (nil? b))
        (is (some? a))))
    (testing "scaling sets length about the centre and keeps height"
      (is (close? [0.375 0.5 0.625 0.5] (flatten (mapping/scale points 0.5)))))
    (is (= [[0.75 0.5] [0.25 0.5]] (:points (mapping/reverse-direction {:points points}))))))

(deftest sampling-reads-grid-cells-in-endpoint-order
  (let [fixtures (heads :row 8)
        strip {:id :a :layout :row :points [[0.0 0.125] [1.0 0.125]] :height 0.05}
        colors (sample-strip strip fixtures grid)]
    (doseq [i (range 8)]
      (is (close? (grid-linear i 0) (get colors (keyword (str "row-" i)))) (str "pixel " i)))
    (testing "reversal swaps which end the first head reads"
      (let [colors (sample-strip (assoc strip :reverse? true) fixtures grid)]
        (is (close? (grid-linear 7 0) (get colors :row-0)))
        (is (close? (grid-linear 0 0) (get colors :row-7)))))
    (testing "placement moves the whole mapping by source units"
      (let [colors (sample-strip (assoc strip :points [[0.0 0.125] [0.5 0.125]]) (heads :row 4) grid {:x 0.25})]
        (is (close? (grid-linear 2 0) (get colors :row-0)))
        (is (close? (grid-linear 5 0) (get colors :row-3)))))
    (testing "explicit targets keep their given order"
      (let [colors (sample-strip {:id :b :targets [:row-3 :row-1] :points [[0.0625 0.375] [1.0625 0.375]]
                                               :height 0.0 :filter :nearest}
                                 fixtures grid)]
        (is (close? (grid-linear 2 1) (get colors :row-3)))
        (is (close? (grid-linear 6 1) (get colors :row-1)))))))

(deftest box-filter-averages-rows-in-linear-light-and-nearest-does-not
  (let [fixtures (heads :t 1)
        ;; One pixel straddling the boundary between rows 0 and 1 of column 0,
        ;; half in each.
        strip {:id :a :layout :t :points [[0.02 0.25] [0.1 0.25]] :height 0.2}
        box (get (sample-strip strip fixtures grid) :t-0)
        nearest (get (sample-strip (assoc strip :filter :nearest :points [[0.02 0.2] [0.1 0.2]]) fixtures grid) :t-0)
        [_ g0 _] (grid-linear 0 0) [_ g1 _] (grid-linear 0 1)]
    (is (close? [(first (grid-linear 0 0)) (/ (+ g0 g1) 2) (nth (grid-linear 0 0) 2)] box 0.01)
        "green is the linear-light mean of the two rows")
    (is (close? (grid-linear 0 0) nearest))
    (testing "footprints outside the frame read black"
      (is (close? [0 0 0] (get (sample-strip (assoc strip :points [[1.2 0.5] [1.4 0.5]]) fixtures grid) :t-0))))))

(deftest unresolved-strips-sample-nothing
  (let [plan (sampler/plan {:id :m :source :grid :strips [{:id :a :layout :missing :points [[0 0] [1 1]]}]}
                           (heads :t 2) 640 360 nil)]
    (is (= [] (:targets plan)))
    (is (true? (:unresolved? (first (:strips plan)))))
    (is (= {} (sampler/colors plan (sampler/sample plan grid))))))

(deftest feed-file-reads-stable-frames-resizes-and-disconnects
  (let [file (File/createTempFile "tolglow-video" ".tgvf")
        feed (source/feed :wall {:file (str file)})]
    (try
      (source/write-feed! file (source/test-pattern-frame {:width 64 :height 32}) {:frames 1 :capacity (* 4 128 64)})
      (let [f (source/latest feed)]
        (is (= [64 32 1] [(:width f) (:height f) (:sequence f)]))
        (is (= (source/grid-color 0 0) (source/rgb-at f 0 0)))
        (is (= (source/grid-color 7 3) (source/rgb-at f 63 31)))
        (is (identical? f (source/latest feed)) "an unchanged sequence reuses the copied frame"))
      (source/write-feed! file (source/test-pattern-frame {:width 128 :height 64 :shift 1}) {:frames 2})
      (let [f (source/latest feed)]
        (is (= [128 64 2] [(:width f) (:height f) (:sequence f)]) "a sender resize reaches the reader")
        (is (= (source/grid-color 1 0) (source/rgb-at f 0 0))))
      (testing "a frame being written (odd sequence) keeps the last complete one"
        (with-open [raf (java.io.RandomAccessFile. file "rw")]
          (.seek raf 8) (.write raf (byte-array [3 0 0 0 0 0 0 0])))
        (is (= 2 (:sequence (source/latest feed))))
        (is (nil? (source/read-feed (.order (java.nio.ByteBuffer/wrap (java.nio.file.Files/readAllBytes (.toPath file)))
                                            java.nio.ByteOrder/LITTLE_ENDIAN) :wall)))
        (with-open [raf (java.io.RandomAccessFile. file "rw")]
          (.seek raf 8) (.write raf (byte-array [4 0 0 0 0 0 0 0]))))
      (source/write-feed! file (source/test-pattern-frame {:width 128 :height 64}) {:frames 3 :connected? false})
      (is (false? (:connected? (source/latest feed))))
      (is (false? (:connected? (source/describe feed))))
      (finally (source/close! feed) (.delete file)))))

(deftest image-files-and-sources-stay-isolated-in-the-hub
  (let [file (File/createTempFile "tolglow-video" ".png")
        image (BufferedImage. 4 2 BufferedImage/TYPE_INT_RGB)
        video (hub/create {:now-ms (constantly 0) :show-directory (.getParent file)})]
    (try
      (.setRGB image 3 1 0x102030)
      (ImageIO/write image "png" file)
      (is (= #{:still :grid :missing}
             (hub/reconcile! video {:still {:kind :image :file (str file)}
                                    :grid {:kind :test-pattern :width 64 :height 32}
                                    :missing {:kind :syphon :app "Arena" :name "Composition"}})))
      (let [latch (hub/latch video {:transport {:playing? true}
                                    :settings {:video {:mappings {:m {:id :m :source :still :strips []}}}}} nil)
            status (into {} (map (juxt :id identity)) (hub/status video latch))]
        (is (= [0x10 0x20 0x30] (source/rgb-at (get-in latch [:frames :still]) 3 1)))
        (is (= [64 32] [(get-in status [:grid :width]) (get-in status [:grid :height])]))
        (is (nil? (get-in latch [:frames :missing])))
        (is (true? (get-in status [:missing :stale?])))
        (is (re-find #"bridge" (get-in status [:missing :error])))
        (is (= {:m {:id :m :source :still :strips []}} (:mappings latch))))
      (testing "replacing one source leaves the others open"
        (let [grid-source (hub/source-of video :grid)]
          (hub/reconcile! video {:still {:kind :image :file (str file)} :grid {:kind :test-pattern :width 64 :height 32}})
          (is (identical? grid-source (hub/source-of video :grid)))))
      (finally (hub/stop! video) (.delete file)))))

(deftest a-stopped-hub-opens-nothing-and-stills-never-go-stale
  (let [video (hub/create {:now-ms #(+ 60000 (System/currentTimeMillis))})]
    (hub/reconcile! video {:grid {:kind :test-pattern :width 8 :height 4}})
    (hub/stop! video)
    (testing "a frame that commits while the show stops cannot reopen sources"
      (is (nil? (hub/request-reconcile! video {:grid {:kind :test-pattern :width 8 :height 4}})))
      (Thread/sleep 50)
      (is (= {} @(:sources video)))))
  (let [file (File/createTempFile "tolglow-video" ".png")
        video (hub/create {:now-ms #(+ 60000 (System/currentTimeMillis)) :show-directory (.getParent file)})]
    (try
      (ImageIO/write (BufferedImage. 4 2 BufferedImage/TYPE_INT_RGB) "png" file)
      (hub/reconcile! video {:still {:kind :image :file (str file)}})
      (let [[still] (hub/status video (hub/latch video {:transport {:playing? true}} nil))]
        (is (false? (:stale? still)) "a still image a minute old is not a stale feed")
        (is (nil? (:age-ms still))))
      (finally (hub/stop! video) (.delete file))))
  (testing "a one-row frame still makes a preview JPEG"
    (is (pos? (alength (preview/frame-jpeg {:frames {:s (source/test-pattern-frame {:width 1920 :height 1})}} :s 640))))))

(deftest latch-holds-while-stopped-and-for-paused-instances
  (let [video (hub/create)
        pushed (source/pushed :p)
        frame-a (source/test-pattern-frame {:width 8 :height 4 :sequence 1})
        frame-b (source/test-pattern-frame {:width 8 :height 4 :sequence 2 :shift 1})]
    (reset! (:sources video) {:p {:config {:kind :pushed} :source pushed}})
    (source/push! pushed frame-a)
    (let [playing {:transport {:playing? true} :instances {"i1" {:system {:paused? false}}}}
          first-latch (hub/latch video playing nil)
          _ (source/push! pushed frame-b)
          stopped (hub/latch video (assoc-in playing [:transport :playing?] false) first-latch)
          paused (hub/latch video (assoc-in playing [:instances "i1" :system :paused?] true) first-latch)
          resumed (hub/latch video (assoc-in playing [:instances "i1" :system :paused?] true) paused)]
      (is (= 1 (get-in first-latch [:frames :p :sequence])))
      (is (= 1 (get-in stopped [:frames :p :sequence])) "a stopped transport keeps the frame it stopped on")
      (is (:frozen? stopped))
      (is (= 2 (get-in paused [:frames :p :sequence])) "other instances keep receiving")
      (is (= 1 (get-in paused [:held "i1" :p :sequence])) "the paused instance holds what it last saw")
      (is (= 1 (get-in resumed [:held "i1" :p :sequence]))))))

(deftest video-outputs-validate-and-stay-out-of-forecasts
  (let [definition {:id :v :version 1 :params {:drift {:type :number :default 0.0 :bounds [-1 1]}}
                    :outputs {:color {:op :video :mapping :wall :placement {:x {:op :param :key :drift}}}}}]
    (is (= [] (compiler/definition-errors :effects definition)))
    (is (false? (compiler/predictable-outputs? definition)) "keyframes never interpolate video")
    (is (seq (compiler/definition-errors :effects (assoc definition :outputs {:dimmer {:op :video :mapping :wall}})))
        "colour cannot feed a scalar attribute")
    (is (= [] (compiler/definition-errors :effects (assoc definition :outputs {:dimmer {:op :video :mapping :wall :channel :luma}}))))
    (is (seq (compiler/definition-errors :effects (assoc-in definition [:outputs :color :placement :x] {:op :param :key :gone})))
        "a placement reading a dropped parameter is refused (#703)")
    (is (seq (compiler/definition-errors :effects (assoc-in definition [:outputs :color :placement :x] {:op :position :axis :x}))))
    (let [fixtures (heads :row 8)
          step (compiler/compile-effect definition)
          frame {:time-ms 0 :fixtures fixtures
                 video-output/input-key {:frames {:grid grid}
                                         :mappings {:wall {:id :wall :source :grid
                                                           :strips [{:id :a :layout :row :points [[0 0.125] [1 0.125]]}]}}}}
          run #(:assignments (step {:frame frame :instance {:started-at-ms 0 :params %} :assignments [] :intents []}))
          values (into {} (map (juxt :target-id :value)) (run {}))]
      (is (= 8 (count values)))
      (is (close? (grid-linear 3 0) (:row-3 values)))
      (is (close? (grid-linear 4 0) (:row-3 (into {} (map (juxt :target-id :value)) (run {:drift 0.125}))))
          "a cue parameter moves the mapping")
      (is (= [] (:assignments (step {:frame (dissoc frame video-output/input-key)
                                     :instance {:started-at-ms 0} :assignments [] :intents []})))
          "no latched frame writes nothing"))))

(deftest sampling-cost-for-two-thousand-pixels
  ;; 25 strips of 80 pixels over a 1024 × 576 frame, box filtered. Sampling
  ;; is one weighted read per planned tap, so its cost is counted here as
  ;; taps: a tight wall-clock median flaked on loaded CI runners. The inner
  ;; loop's primitive hints are checked in the next test, a loose ceiling
  ;; here catches a catastrophic slowdown, and `tolglow.bench-video-strips`
  ;; times it in the performance workflow (docs/video-strips.md).
  (let [frame (source/test-pattern-frame {:width 1024 :height 576 :pattern :gradient})
        fixtures (into [] (mapcat #(heads (keyword (str "s" %)) 80)) (range 25))
        strips (mapv (fn [i] {:id i :layout (keyword (str "s" i)) :height 0.02
                              :points [[0.05 (+ 0.02 (* i 0.038))] [0.95 (+ 0.04 (* i 0.038))]]})
                     (range 25))
        taps (fn [plan] (let [^ints starts (:starts plan)] (aget starts (dec (alength starts)))))
        per-pixel (fn [plan] (let [^ints starts (:starts plan)]
                               (map #(- (aget starts (inc %)) (aget starts %)) (range (dec (alength starts))))))
        plan (sampler/plan {:id :m :source :g :strips strips} fixtures 1024 576 nil)
        cap (* 4 sampler/max-subsamples-per-axis sampler/max-subsamples-per-axis)]
    (is (= 2000 (count (:targets plan))))
    (is (= 64 cap) "16 bilinear sub-samples of 4 texel reads each")
    (testing "a footprint wider than 4 texels each way is capped, so work is linear in pixels, not in footprint area"
      (is (every? #(<= 1 % cap) (per-pixel plan)))
      (is (<= 2000 (taps plan) (* 2000 cap)))
      (is (= (alength ^ints (:offsets plan)) (taps plan)) "sampling reads exactly the planned taps")
      (let [huge (sampler/plan {:id :m :source :g :strips (mapv #(assoc % :height 0.5) strips)} fixtures 1024 576 nil)]
        (is (<= (taps huge) (* 2000 cap)) "a sampling height 25 × taller costs no more (reads off the frame are dropped)")))
    (testing "nearest reads one texel per pixel"
      (is (= 2000 (taps (sampler/plan {:id :m :source :g :filter :nearest :strips strips} fixtures 1024 576 nil)))))
    (is (= 6000 (alength ^doubles (sampler/sample plan frame))))
    (testing "a catastrophic slowdown fails even on a loaded runner (the sampler measures about 0.2 ms)"
      (let [times (sort (for [_ (range 30)]
                          (let [t0 (System/nanoTime)] (sampler/sample plan frame) (/ (- (System/nanoTime) t0) 1e6))))]
        (is (< (nth times 15) 50.0) (str "median sample " (nth times 15) " ms"))))))

(deftest the-sampler-inner-loop-stays-primitive
  ;; A lost type hint makes `sample` reflect or box every read, 10-100x
  ;; slower, without changing a single tap. Compile the file again with the
  ;; warnings on: no reflection anywhere, no boxed math inside `sample`.
  (let [err (java.io.StringWriter.)
        source (slurp (io/resource "tolglow/video/sampler.clj"))
        lines (vec (str/split-lines source))
        start (inc (first (keep-indexed #(when (str/starts-with? %2 "(defn sample") %1) lines)))
        end (inc (first (keep-indexed #(when (and (> %1 start) (str/starts-with? %2 "(defn")) %1) lines)))]
    (try
      (binding [*err* err *warn-on-reflection* true *unchecked-math* :warn-on-boxed]
        (load "/tolglow/video/sampler"))
      ;; :warn-on-boxed also compiles unchecked maths; restore the real build
      ;; for the rest of this JVM.
      (finally (load "/tolglow/video/sampler")))
    (let [report (str err)
          boxed-lines (map #(Long/parseLong (second %)) (re-seq #"Boxed math warning, tolglow/video/sampler\.clj:(\d+)" report))]
      (is (not (re-find #"Reflection warning" report)) report)
      (is (empty? (filter #(< start % end) boxed-lines)) report))))

(defn- temp-dir ^File [prefix]
  (.toFile (java.nio.file.Files/createTempDirectory prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- png! [^File file w h]
  (ImageIO/write (BufferedImage. w h BufferedImage/TYPE_INT_RGB) "png" file)
  file)

(defn- code-of [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:code (ex-data e)))))

(deftest file-sources-stay-inside-the-show-directory
  (let [show (temp-dir "tolglow-show") outside (temp-dir "tolglow-outside")
        secret (png! (File. outside "secret.png") 2 2)
        inside (png! (File. show "still.png") 2 2)
        roots [show]]
    (try
      (java.nio.file.Files/createSymbolicLink (.toPath (File. show "escape.png")) (.toPath secret)
                                              (make-array java.nio.file.attribute.FileAttribute 0))
      (java.nio.file.Files/createSymbolicLink (.toPath (File. show "linked-dir")) (.toPath outside)
                                              (make-array java.nio.file.attribute.FileAttribute 0))
      (is (= (.getCanonicalFile inside) (source/resolve-file "still.png" roots)) "relative to the show directory")
      (is (= (.getCanonicalFile inside) (source/resolve-file (str inside) roots)) "absolute inside is fine")
      (is (= :video/path-traversal (code-of #(source/resolve-file "../tolglow-outside/secret.png" roots))))
      (is (= :video/path-traversal (code-of #(source/resolve-file (str show "/sub/../still.png") roots))))
      (is (= :video/path-outside-show (code-of #(source/resolve-file (str secret) roots))))
      (is (= :video/path-outside-show (code-of #(source/resolve-file "/etc/hosts" roots))))
      (is (= :video/path-outside-show (code-of #(source/resolve-file "escape.png" roots))) "a symlink cannot escape")
      (is (= :video/path-outside-show (code-of #(source/resolve-file "linked-dir/secret.png" roots))))
      (is (= :video/no-file-roots (code-of #(source/resolve-file "still.png" []))))
      (testing "the hub lists a refused file as unavailable with its code, and never latches it"
        (let [video (hub/create {:show-directory show})]
          (try
            (hub/reconcile! video {:in {:kind :image :file "still.png"}
                                   :out {:kind :image :file (str secret)}
                                   :link {:kind :image :file "escape.png"}
                                   :feed {:kind :feed :file "../tolglow-outside/secret.png"}})
            (let [latch (hub/latch video {:transport {:playing? true}} nil)
                  status (into {} (map (juxt :id identity)) (hub/status video latch))]
              (is (= #{:in} (set (keys (:frames latch)))))
              (is (= :video/path-outside-show (get-in status [:out :code])))
              (is (= :video/path-outside-show (get-in status [:link :code])))
              (is (= :video/path-traversal (get-in status [:feed :code]))))
            (finally (hub/stop! video)))))
      (testing "bundled stills are named under the video resources only"
        (is (= :video/path-traversal (code-of #(source/resource-frame :r "../studio/index.html" 0))))
        (is (= :video/path-traversal (code-of #(source/resource-frame :r "/etc/hosts" 0))))
        (is (= :video/missing-resource (code-of #(source/resource-frame :r "none.png" 0)))))
      (testing "settings refuse .. before the runtime sees them"
        (is (seq (mapping/settings-errors {:sources {:s {:kind :image :file "../x.png"}}}))))
      (finally
        (doseq [d [show outside]] (run! #(.delete ^File %) (reverse (file-seq d))))))))

(deftest a-file-swapped-for-a-link-after-the-check-is-not-read
  (let [show (temp-dir "tolglow-show") outside (temp-dir "tolglow-outside")
        link! (fn [^File from ^File to]
                (.delete from)
                (java.nio.file.Files/createSymbolicLink (.toPath from) (.toPath to) (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (is (= :video/path-outside-show (code-of #(source/resolve-file "." [show]))) "the show directory itself is not a file")
      (testing "a feed re-checks its path before remapping"
        (let [inside (File. show "feed.tgvf") evil (File. outside "evil.tgvf")]
          (source/write-feed! inside (source/test-pattern-frame {:width 8 :height 4}) {})
          (source/write-feed! evil (source/test-pattern-frame {:width 16 :height 4}) {})
          (let [feed (source/open :f {:kind :feed :file "feed.tgvf"} [show])]
            (is (= 8 (:width (source/latest feed))))
            (link! inside evil)
            (is (not= 16 (:width (source/latest feed))) "the outside feed is never mapped")
            (is (some? (:error (source/describe feed)))))))
      (testing "a still opens the checked path without following a swapped-in link"
        (let [inside (png! (File. show "still.png") 2 2) secret (png! (File. outside "secret.png") 3 3)
              checked (source/resolve-file "still.png" [show])]
          (link! inside secret)
          (is (thrown? java.io.IOException (source/image-frame :s checked 0)))))
      (finally
        (doseq [d [show outside]] (run! #(.delete ^File %) (reverse (file-seq d))))))))

(deftest stills-are-size-checked-before-decoding
  (let [show (temp-dir "tolglow-show")
        wide (png! (File. show "wide.png") (inc source/max-image-dimension) 1)
        small (png! (File. show "small.png") 4 4)]
    (try
      (is (= :video/image-too-large (code-of #(source/image-frame :w wide 0))) "dimensions come from the header")
      (is (= :video/image-too-large (code-of #(source/image-frame :s small 0 {:max-bytes 10}))) "bytes are checked first")
      (is (= [4 4] ((juxt :width :height) (source/image-frame :s small 0))))
      (is (= :video/unreadable-image (code-of #(source/image-frame :t (doto (File. show "text.png") (spit "not an image")) 0))))
      (finally (run! #(.delete ^File %) (reverse (file-seq show)))))))

(deftest served-previews-are-bounded
  (let [tall {:frames {:t (source/test-pattern-frame {:width 4 :height 4000})}}
        jpeg (preview/frame-jpeg tall :t 640)
        image (ImageIO/read (java.io.ByteArrayInputStream. jpeg))]
    (is (<= (.getHeight image) preview/max-jpeg-dimension) "height is bounded as well as width")
    (is (<= (.getWidth image) 640))
    (is (<= (alength jpeg) preview/max-jpeg-bytes))))

(deftest source-and-helper-counts-are-limited
  (let [sources (fn [n kind] (into {} (for [i (range n)]
                                        [(keyword (str "s" i)) (if (= kind :syphon) {:kind :syphon :app "A" :name (str i)}
                                                                   {:kind :test-pattern})])))]
    (is (= [] (mapping/settings-errors {:sources (sources mapping/max-sources :grid)})))
    (is (seq (mapping/settings-errors {:sources (sources (inc mapping/max-sources) :grid)})))
    (is (= [] (mapping/settings-errors {:sources (sources mapping/max-syphon-sources :syphon)})))
    (is (seq (mapping/settings-errors {:sources (sources (inc mapping/max-syphon-sources) :syphon)})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (reduce (fn [s [id source]] (mapping/apply-operation s {:op :put-source :id id :source source}))
                         {} (sources (inc mapping/max-sources) :grid)))
        "the edit that adds a ninth source is refused")
    (testing "the hub runs at most max-helpers helpers even from unvalidated settings"
      (let [started (atom [])
            video (hub/create {:bridge "/bin/cat"
                               :spawn (fn [_] (let [p (.start (ProcessBuilder. ^java.util.List ["/bin/cat"]))]
                                                (swap! started conj p) p))})]
        (try
          (hub/reconcile! video (sources (+ 2 hub/max-helpers) :syphon))
          (let [status (hub/status video {})]
            (is (= hub/max-helpers (count @started)))
            (is (= 2 (count (filter #(= :video/too-many-helpers (:code %)) status)))))
          (finally (hub/stop! video)))
        (is (every? #(not (.isAlive ^Process %)) @started) "stop closes every helper")))))

(deftest a-failed-helper-start-stops-the-helpers-started-in-that-pass
  (let [started (atom [])
        calls (atom 0)
        video (hub/create {:bridge "/bin/cat"
                           :spawn (fn [_]
                                    (if (= 2 (swap! calls inc))
                                      (throw (java.io.IOException. "spawn refused"))
                                      (let [p (.start (ProcessBuilder. ^java.util.List ["/bin/cat"]))]
                                        (swap! started conj p) p)))})]
    (try
      (hub/reconcile! video {:a {:kind :syphon :app "A" :name "1"} :b {:kind :syphon :app "A" :name "2"}
                             :c {:kind :syphon :app "A" :name "3"}})
      (is (= 1 (count @started)))
      (.waitFor ^Process (first @started) 3 java.util.concurrent.TimeUnit/SECONDS)
      (is (not (.isAlive ^Process (first @started))) "the helper started before the failure is stopped")
      (is (every? #(= :video/helper-start-failed (:code %)) (hub/status video {})))
      (testing "the hub lock stays free while helpers start"
        (let [entered (promise)
              blocking (hub/create {:bridge "/bin/cat"
                                    :spawn (fn [_] (deliver entered true) (Thread/sleep 300)
                                             (.start (ProcessBuilder. ^java.util.List ["/bin/cat"])))})
              pass (future (hub/reconcile! blocking {:a {:kind :syphon :app "A" :name "1"}}))]
          (try
            (deref entered 2000 nil)
            (let [t0 (System/nanoTime)]
              (locking (:lock blocking) nil)
              (is (< (/ (- (System/nanoTime) t0) 1e6) 100.0) "status and latch readers are not blocked by a start"))
            @pass
            (finally (hub/stop! blocking)))))
      (finally (hub/stop! video)))))

(deftest stop-removes-the-feed-directory-it-created
  (let [video (hub/create {:bridge "/bin/cat"
                           :spawn (fn [_] (.start (ProcessBuilder. ^java.util.List ["/bin/cat"])))})]
    (hub/reconcile! video {:a {:kind :syphon :app "A" :name "1"}})
    (let [dir @(:created-dir video)]
      (is (.isDirectory ^File dir))
      (hub/stop! video)
      (is (not (.exists ^File dir)) "the per-process temp directory is gone after stop"))))
