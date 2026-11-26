(ns tolglow.video-surfaces-test
  "Video on 3D surfaces (#674) without a show: surface coordinates, seams,
  contact distance and weight, sampling footprints and the compiled
  `{:op :video :surface}` output against the labelled grid."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.video :as video-output]
            [tolglow.video.mapping :as mapping]
            [tolglow.video.sampler :as sampler]
            [tolglow.video.source :as source]
            [tolglow.video.surface :as surface]))

(defn- linear [byte]
  (let [c (/ byte 255.0)]
    (if (<= c 0.04045) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4))))

(defn- grid-linear [column row] (mapv linear (source/grid-color column row)))

(defn- close? [a b & [tolerance]]
  (every? #(< (Math/abs (double %)) (or tolerance 1e-3)) (map - a b)))

(def grid (source/test-pattern-frame {:source-id :grid :width 640 :height 320}))

(def sheet
  "An 8 × 4 m vertical sheet at the origin facing +z: one metre per grid cell."
  {:id :sheet :source :grid :shape :plane :position [0 0 0] :size-m [8 4]
   :contact {:distance-m 0.25}})

(defn- cell-centre
  "Venue point in front of grid cell (column, row) on the unrotated sheet."
  [column row z]
  [(+ -3.5 column) (- 1.5 row) z])

(defn- head [id position & [extra]] (merge {:id id :position position :channels {:red 1 :green 2 :blue 3}} extra))

(defn- sample [spec placement fixtures & [frame]]
  (let [frame (or frame grid)
        resolved (surface/resolve-surface spec placement 0)
        {:keys [contacts plan]} (video-output/surface-plan resolved fixtures nil (:width frame) (:height frame))]
    {:contacts (into {} (map (juxt :id identity)) contacts)
     :colors (sampler/colors plan (sampler/sample plan frame))}))

(deftest surfaces-validate-and-edit-through-the-video-settings
  (is (= [] (surface/surfaces-errors {:sheet sheet})))
  (is (= [] (mapping/settings-errors {:sources {:grid {:kind :test-pattern}} :surfaces {:sheet sheet}})))
  (doseq [[bad path] [[(assoc sheet :shape :cube) [:surfaces :sheet :shape]]
                      [(dissoc sheet :size-m) [:surfaces :sheet :size-m]]
                      [(assoc sheet :shape :sphere) [:surfaces :sheet :radius-m]]
                      [(assoc sheet :contact {:distance-m -1}) [:surfaces :sheet :contact]]
                      [(assoc sheet :crop {:x 0 :y 0 :width 0 :height 1}) [:surfaces :sheet :crop]]
                      [(assoc sheet :strips {:tube {:height 2}}) [:surfaces :sheet :strips :tube]]
                      [(assoc sheet :wave {:wavelength-m 0}) [:surfaces :sheet :wave]]]]
    (is (some #{path} (map :path (surface/surfaces-errors {:sheet bad}))) (pr-str path)))
  (is (seq (surface/surfaces-errors {:other sheet})) "the key matches the id")
  (let [settings (-> {}
                     (mapping/apply-operation {:op :put-source :id :grid :source {:kind :test-pattern}})
                     (mapping/apply-operation {:op :put-surface :surface sheet}))]
    (is (= sheet (get-in settings [:surfaces :sheet])))
    (is (thrown? clojure.lang.ExceptionInfo (mapping/apply-operation settings {:op :delete-source :id :grid}))
        "a source a surface reads cannot be removed")
    (is (thrown? clojure.lang.ExceptionInfo
                 (mapping/apply-operation settings {:op :put-surface :surface (dissoc sheet :position)})))
    (is (= {} (:surfaces (mapping/apply-operation settings {:op :delete-surface :id :sheet}))))))

(deftest surface-coordinates-round-trip-for-every-shape
  (doseq [spec [(assoc sheet :rotation-deg [20 35 -10] :position [1 2 -3])
                (assoc sheet :shape :wave :wave {:amplitude-m 0.4 :wavelength-m 3 :phase 0.2})
                (assoc sheet :shape :wave :wave {:amplitude-m 0.3 :wavelength-m 2 :axis :v})
                {:id :ball :source :grid :shape :sphere :position [0 3 0] :radius-m 2 :rotation-deg [0 90 0]}]
          :let [resolved (surface/resolve-surface spec {:scale 1.5} 0)]
          uv [[0.1 0.2] [0.5 0.5] [0.9 0.75] [0.3 0.95]]]
    (let [{:keys [distance inside?] :as projected} (surface/project resolved (surface/world-point resolved uv))]
      (is (close? uv (:uv projected) 1e-9) (pr-str (:shape spec) uv))
      (is (< distance 1e-9) (pr-str (:shape spec) uv))
      (is inside?)))
  (testing "a point off the sheet's edge is never in contact"
    (let [resolved (surface/resolve-surface sheet nil 0)]
      (is (false? (:inside? (surface/project resolved [4.2 0 0]))))
      (is (empty? (surface/contacts resolved [(head :off [4.2 0 0.1])])))))
  (testing "the first-order wave distance is exact at a crest"
    (let [resolved (surface/resolve-surface (assoc sheet :shape :wave :wave {:amplitude-m 0.5 :wavelength-m 4}) nil 0)]
      ;; sin peaks at x = wavelength / 4 = 1 m.
      (is (close? [0.4] [(:distance (surface/project resolved [1 0 0.9]))] 1e-9)))))

(deftest a-head-qualifies-up-to-the-contact-distance-and-fades-over-the-feather
  (let [inside (head :inside (cell-centre 2 1 0.24))
        outside (head :outside (cell-centre 2 1 0.26))
        behind (head :behind (cell-centre 5 2 -0.2))
        {:keys [contacts colors]} (sample sheet nil [inside outside behind])]
    (is (= #{:inside :behind} (set (keys contacts))) "distance is unsigned: either face qualifies")
    (is (close? (grid-linear 2 1) (:inside colors)))
    (is (close? (grid-linear 5 2) (:behind colors)))
    (is (nil? (:outside colors)) "past the threshold the head takes nothing")
    (testing "a feather fades the write weight with the volumes' curve"
      (let [spec (assoc sheet :contact {:distance-m 0.2 :feather-m 0.2 :curve :linear})
            resolved (surface/resolve-surface spec nil 0)
            weight #(:weight (first (surface/contacts resolved [(head :h (cell-centre 0 0 %))])))]
        (is (= 1.0 (weight 0.2)))
        (is (close? [0.5] [(weight 0.3)]))
        (is (nil? (weight 0.41)))
        (is (close? [0.5] [(surface/contact-weight {:distance-m 0 :feather-m 1 :curve :smoothstep} 0.5)]))))
    (testing "a cue's contact multiplier moves the threshold"
      (is (contains? (:contacts (sample sheet {:contact 1.2} [outside])) :outside)))))

(deftest moving-the-crop-and-moving-the-surface-differ
  (let [fixed (head :fixed (cell-centre 2 1 0.1))
        base (:fixed (:colors (sample sheet nil [fixed])))]
    (is (close? (grid-linear 2 1) base))
    (testing "the crop slides the picture under a head that stays in contact"
      (let [{:keys [contacts colors]} (sample sheet {:crop-x 0.125} [fixed])]
        (is (contains? contacts :fixed))
        (is (close? [0.3125 0.375] (:uv (:fixed contacts)) 1e-9) "same surface point")
        (is (close? (grid-linear 3 1) (:fixed colors)) "next column's colour")))
    (testing "moving the surface by one cell shows the neighbouring cell at the same head"
      (let [{:keys [contacts colors]} (sample sheet {:x 1.0} [fixed])]
        (is (close? [0.1875 0.375] (:uv (:fixed contacts)) 1e-9) "a different surface point")
        (is (close? (grid-linear 1 1) (:fixed colors)))))
    (testing "moving the surface away leaves the head out of contact"
      (is (empty? (:contacts (sample sheet {:z 0.5} [fixed])))))
    (testing "crop zoom keeps the centre and magnifies"
      (let [centre (head :centre [0.25 0.25 0.0])]
        (is (close? (grid-linear 4 1) (:centre (:colors (sample sheet {:crop-scale 0.5} [centre])))))))
    (testing "yaw turns the sheet so a head beside it lands on another column"
      (let [resolved (surface/resolve-surface sheet {:yaw 90} 0)
            {:keys [uv]} (surface/project resolved [0 0 -2.5])]
        (is (close? [0.8125 0.5] uv 1e-9) "local x now runs along venue −z")))))

(deftest a-moving-wavy-sheet-carries-heads-in-and-out-of-contact
  (let [spec (assoc sheet :shape :wave :wave {:amplitude-m 0.5 :wavelength-m 4 :speed 0.25}
                    :contact {:distance-m 0.1})
        ;; A head 0.5 m in front of cell (4, 1): local x = 0.5 m.
        crest (head :crest (cell-centre 4 1 0.5))
        at (fn [beats] (let [resolved (surface/resolve-surface spec nil beats)]
                         (surface/contacts resolved [crest])))]
    (is (empty? (at 0)) "phase 0: the sheet is about 0.13 m from the head")
    ;; At 0.5 beats the phase is 0.125 cycles: x/4 + 0.125 = 0.25, the crest.
    (is (= [:crest] (mapv :id (at 0.5))) "the travelling crest reaches the head")
    (is (empty? (at 2.5)) "and leaves it at the trough")
    (testing "the head still samples its own grid cell while in contact"
      (let [resolved (surface/resolve-surface spec nil 0.5)
            {:keys [plan]} (video-output/surface-plan resolved [crest] nil 640 320)]
        (is (close? (grid-linear 4 1) (:crest (sampler/colors plan (sampler/sample plan grid)))))))
    (testing "a cue phase offset and the beat clock add"
      (is (= (at 0.5) (surface/contacts (surface/resolve-surface spec {:phase 0.125} 0) [crest]))))
    (testing "the Room mesh lies on the sampled surface"
      (let [resolved (surface/resolve-surface spec nil 0.5)
            {:keys [positions uvs columns rows]} (surface/mesh resolved)]
        (is (= (* 3 (inc columns) (inc rows)) (count positions)))
        (doseq [i (range 0 (/ (count positions) 3) 7)]
          (let [p (subvec positions (* 3 i) (* 3 (inc i)))
                {:keys [uv distance]} (surface/project resolved p)]
            (is (< distance 1e-9))
            (is (close? (surface/source-uv resolved uv) (subvec uvs (* 2 i) (* 2 (inc i))) 1e-9))))))))

(deftest a-sphere-wraps-its-seam
  (let [ball {:id :ball :source :grid :shape :sphere :position [0 0 0] :radius-m 2
              :contact {:distance-m 0.2} :footprint {:width 0.0 :height 0.02}}
        resolved (surface/resolve-surface ball nil 0)
        ;; The seam is the local −z meridian. A hair either side of it on the equator:
        west (head :west [-0.01 0.3 -2.05])
        east (head :east [0.01 0.3 -2.05])
        front (head :front [0 0.3 2.05])
        {:keys [contacts colors]} (sample ball nil [west east front])]
    (is (< (first (:uv (:west contacts))) 0.01) "just past the seam: u near 0")
    (is (> (first (:uv (:east contacts))) 0.99) "just before it: u near 1")
    (is (close? (grid-linear 0 1) (:west colors) 0.03) "the first column")
    (is (close? (grid-linear 7 1) (:east colors) 0.03) "the last column")
    (is (close? [0.5] [(first (:uv (:front contacts)))] 1e-3) "the front meridian shows the middle of the crop")
    (testing "a footprint straddling the seam averages both edges instead of reading black"
      (let [on (head :on [0 0.3 -2.05])
            wide (assoc ball :footprint {:width 0.2 :height 0.0})
            color (:on (:colors (sample wide nil [on])))
            mean (mapv #(/ (+ %1 %2) 2) (grid-linear 0 1) (grid-linear 7 1))]
        (is (close? mean color 0.02) (pr-str color))))
    (testing "a strip crossing the seam keeps its direction the short way round"
      (let [pixels (mapv (fn [i] (let [a (+ Math/PI (* (- i 1.5) 0.05))]
                                   (head (keyword (str "p" i)) [(* 2.0 (Math/sin a)) 0.3 (* 2.0 (Math/cos a))]
                                         {:pixel-layout-id :ring :pixel-index i})))
                         (range 4))
            prints (surface/footprints resolved (surface/contacts resolved pixels) 640 320)]
        (is (= 4 (count prints)))
        (is (every? #(close? [1.0 0.0] (:direction %) 1e-6) prints))
        (is (every? #(< (Math/abs (- (:along %) (:along (first prints)))) 1e-6) prints)
            "no pixel sees a near-full-width jump across the seam")))))

(deftest strip-sampling-height-is-chosen-per-strip
  ;; A horizontal strip of 8 pixels just above the boundary between rows 1 and 2.
  (let [strip (mapv (fn [i] (head (keyword (str "s" i)) [(+ -3.5 i) 0.02 0.05] {:pixel-layout-id :tube :pixel-index i}))
                    (range 8))
        thin (sample sheet nil strip)
        thick (sample (assoc sheet :strips {:tube {:height 0.5}}) nil strip)
        [_ g1 _] (grid-linear 3 1) [_ g2 _] (grid-linear 3 2)
        g (fn [result] (second (get-in result [:colors :s3])))]
    (is (= 8 (count (:contacts thin))))
    (is (close? [g1] [(g thin)] 1e-3) "a zero height reads only the row the strip lies on")
    (is (close? [(/ (+ g1 g2) 2)] [(g thick)] 0.01) "two rows of height average both rows in linear light")
    (testing "along the strip each pixel covers its spacing"
      (let [prints (surface/footprints (surface/resolve-surface sheet nil 0) (surface/contacts (surface/resolve-surface sheet nil 0) strip) 640 320)]
        (is (close? [80.0] [(:along (nth prints 3))] 1e-6) "one metre of an 8 m sheet over 640 px")
        (is (close? [1.0 0.0] (:direction (nth prints 3)) 1e-9))))))

(deftest surface-outputs-compile-sample-once-and-record-the-evaluated-surface
  (let [definition (surface/cue-effect :wall :sheet {})
        heads [(head :a (cell-centre 1 1 0.1)) (head :b (cell-centre 6 3 0.1)) (head :far (cell-centre 6 3 2.0))]
        frame {:time-ms 0 :fixtures heads
               video-output/input-key {:frames {:grid grid} :surfaces {:sheet (assoc sheet :contact {:distance-m 0.05 :feather-m 0.1})}}}
        step (compiler/compile-effect definition)
        run (fn [params] (step {:frame frame :instance {:id "i" :started-at-ms 0 :params params} :assignments [] :intents []}))
        result (run {})
        by-id (into {} (map (juxt :target-id identity)) (:assignments result))]
    (is (= [] (compiler/definition-errors :effects definition)))
    (is (false? (compiler/predictable-outputs? definition)))
    (is (= #{:a :b} (set (keys by-id))) "a head out of contact is not written")
    (is (close? (grid-linear 1 1) (:value (:a by-id))))
    (is (close? [0.5] [(:opacity (:a by-id))]) "half way through the feather writes at half opacity")
    (is (= :sheet (get-in result [:state video-output/observation-key 0 :surface])))
    (is (= [0.0 0.0 0.0] (get-in result [:state video-output/observation-key 0 :resolved :position])))
    (testing "the recorded surface follows the cue parameters"
      (is (= [0.0 0.0 0.5] (get-in (run {:z 0.5}) [:state video-output/observation-key 0 :resolved :position]))))
    (testing "validation"
      (is (seq (compiler/definition-errors :effects (assoc-in definition [:outputs :color :mapping] :wall)))
          "a mapping and a surface at once")
      (is (seq (compiler/definition-errors :effects (assoc-in definition [:outputs :color :placement :rotation] 1)))
          "a 2D-only placement key")
      (is (seq (compiler/definition-errors :effects (assoc-in definition [:outputs :color :placement :x] {:op :position :axis :x})))))
    (testing "without a latched frame the surface is still recorded but nothing is written"
      (let [result (step {:frame (update frame video-output/input-key dissoc :frames)
                          :instance {:id "i" :started-at-ms 0} :assignments [] :intents []})]
        (is (= [] (:assignments result)))
        (is (some? (get-in result [:state video-output/observation-key 0])))))))
