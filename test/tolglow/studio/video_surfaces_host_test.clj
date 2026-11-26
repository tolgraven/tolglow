(ns tolglow.studio.video-surfaces-host-test
  "Video on 3D surfaces through a real show runtime (#674): captured DMX for
  the labelled grid on either side of a contact threshold, a moving wavy
  sheet, a sphere seam, two feeds overlapping, the Room projection against
  the committed fixture values, transport stop and save/reload."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.persistence :as persistence]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.video.hub :as video-hub]
            [tolglow.video.source :as source]
            [tolglow.video.surface :as surface]))

(def tubes
  ;; tube-a: 8 pixels 0.1 m in front of grid row 1 of the sheet, one per column.
  ;; tube-b: 8 pixels in front of row 2, drifting from 0.2 m to 0.3 m away, so
  ;;         the 0.25 m contact threshold falls between pixels 4 and 5.
  ;; seam:   4 pixels on a chord just behind the globe's seam meridian.
  [{:id :tube-a :shape :path :color-layout :rgb :universe 1 :address 1 :pixels 8 :points [[-3.5 0.5 0.1] [3.5 0.5 0.1]]}
   {:id :tube-b :shape :path :color-layout :rgb :universe 1 :address 101 :pixels 8 :points [[-3.5 -0.5 0.2] [3.5 -0.5 0.3]]}
   {:id :seam :shape :path :color-layout :rgb :universe 1 :address 201 :pixels 4 :points [[-0.15 3.3 -1.05] [0.15 3.3 -1.05]]}])

(def sources
  {:grid {:kind :test-pattern :width 640 :height 320}
   :ramp {:kind :test-pattern :width 640 :height 320 :pattern :gradient}})

(def surfaces
  {:sheet {:id :sheet :source :grid :shape :plane :position [0 0 0] :size-m [8 4] :contact {:distance-m 0.25}}
   ;; A second feed whose sphere crosses tube-a's pixels 6 and 7 (x = 1.5, 2.5).
   :ball {:id :ball :source :ramp :shape :sphere :position [2 0.5 -1.0] :radius-m 1.1 :contact {:distance-m 0.25}}
   :globe {:id :globe :source :grid :shape :sphere :position [0 3 0] :radius-m 1 :contact {:distance-m 0.2}}})

(defn- model []
  (-> (demo/model)
      (assoc :rig (layout/materialize {:fixtures [] :pixel-layouts tubes}))
      (assoc-in [:settings :video] {:sources sources :surfaces surfaces})))

(defn- start [model]
  (let [runtime (show/create-runtime {:model model :workspace-options nil})]
    (video-hub/reconcile! (:video runtime) sources)
    runtime))

(defn- dmx [runtime address n]
  (let [^bytes u (get-in @(:state runtime) [:output :universes 1])
        base (+ (dec address) (* 3 n))]
    (mapv #(bit-and 0xff (aget u (+ base %))) (range 3))))

(defn- linear-byte [b]
  (let [c (/ b 255.0)] (* 255.0 (if (<= c 0.04045) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4)))))

(defn- grid-dmx [column row] (mapv linear-byte (source/grid-color column row)))

(defn- near? [expected actual & [tolerance]]
  (every? #(<= (Math/abs (double %)) (or tolerance 1.01)) (map - expected actual)))

(defn- accepted! [reply] (is (= :accepted (:status @reply)) (pr-str @reply)) @reply)

(defn- send! [runtime type payload t]
  (let [reply (show/submit! runtime type payload)]
    (show/tick! runtime t)
    (accepted! reply)))

(defn- spawn! [runtime surface-id t & [extra]]
  (send! runtime :video-mapping (merge {:op :surface-cue :surface surface-id} extra) t))

(defn- instance-of [runtime cue-id]
  (some (fn [[id i]] (when (= cue-id (:cue-id i)) id)) (get-in @(:state runtime) [:model :instances])))

(defn- publish-red! [runtime t]
  (send! runtime :definition {:kind :effects :definition {:id :red :version 1 :outputs {:color [1.0 0.0 0.0]}}} t)
  (send! runtime :definition {:kind :cues :definition {:id :red :effect-id :red :priority 0}} t)
  (send! runtime :command {:op :cue/launch :cue-id :red} t))

(deftest a-surface-cue-lights-heads-in-contact-from-their-grid-cells
  (let [runtime (start (model))]
    (try
      (publish-red! runtime 0)
      (let [receipt (spawn! runtime :sheet 25 {:priority 10})]
        (is (= :video-surface-sheet (:cue-id receipt)))
        (is (= :video (:page-id receipt)))
        (is (= :video-surface-sheet (get-in @(:state runtime) [:model :pages :video :cells (:cell receipt)])))
        (show/tick! runtime 50)
        (doseq [n (range 8)]
          (is (near? (grid-dmx n 1) (dmx runtime 1 n)) (str "tube A pixel " n)))
        (testing "either side of the contact threshold"
          (doseq [n (range 4)]
            (is (near? (grid-dmx n 2) (dmx runtime 101 n)) (str "tube B pixel " n " is within 0.25 m")))
          (doseq [n (range 4 8)]
            (is (= [255 0 0] (dmx runtime 101 n)) (str "tube B pixel " n " is past it and keeps the look below"))))
        (testing "a feather blends the video over the look below by contact weight"
          (send! runtime :video-mapping {:op :put-surface :surface (assoc (:sheet surfaces) :contact {:distance-m 0.2 :feather-m 0.1})} 75)
          (show/tick! runtime 100)
          ;; Pixel 4 sits 0.2571 m away: weight (0.3 − 0.2571) / 0.1 ≈ 0.43.
          (let [w (/ (- 0.3 (+ 0.2 (* 4 (/ 0.1 7)))) 0.1)
                [r g b] (grid-dmx 4 2)]
            (is (near? [(+ (* w r) (* (- 1 w) 255)) (* w g) (* w b)] (dmx runtime 101 4) 1.6)))
          (send! runtime :video-mapping {:op :put-surface :surface (:sheet surfaces)} 125)))
      (testing "the Room projection names the heads in contact and agrees with the captured values"
        (show/tick! runtime 150)
        (let [view (first (get-in (show/snapshot runtime) [:video :surfaces :running]))
              fixtures (into {} (map (juxt :id identity)) (get-in @(:state runtime) [:model :rig :fixtures]))]
          (is (= :sheet (:surface view)))
          (is (= 12 (:count view)) "8 on tube A and 4 on tube B")
          (doseq [{:keys [id uv weight]} (:contacts view)
                  :let [[su sv] (surface/source-uv (:resolved view) uv)
                        {:keys [address]} (fixtures id)
                        column (long (Math/floor (* 8 su))) row (long (Math/floor (* 4 sv)))]]
            (is (= 1.0 weight))
            (is (near? (grid-dmx column row) (dmx runtime (if (< address 101) 1 101) (quot (mod (dec address) 100) 3)))
                (str id)))))
      (finally (show/stop! runtime)))))

(deftest cue-parameters-move-the-crop-and-the-surface-differently
  (let [runtime (start (model))]
    (try
      (spawn! runtime :sheet 0)
      (let [id (instance-of runtime :video-surface-sheet)]
        (show/tick! runtime 25)
        (is (near? (grid-dmx 2 1) (dmx runtime 1 2)))
        (send! runtime :command {:op :param/set :instance-id id :param :crop-x :value 0.125} 50)
        (show/tick! runtime 75)
        (is (near? (grid-dmx 3 1) (dmx runtime 1 2)) "the crop slides the picture under a fixed surface")
        (is (= [0.0 0.0 0.0] (get-in (show/snapshot runtime) [:video :surfaces :running 0 :resolved :position])))
        (send! runtime :command {:op :param/set :instance-id id :param :crop-x :value 0.0} 100)
        (send! runtime :command {:op :param/set :instance-id id :param :y :value 1.0} 125)
        (show/tick! runtime 150)
        (is (near? (grid-dmx 2 2) (dmx runtime 1 2)) "raising the sheet a metre puts row 2 behind tube A")
        (is (= [0.0 1.0 0.0] (get-in (show/snapshot runtime) [:video :surfaces :running 0 :resolved :position]))
            "the Room reads the pose the effect evaluated")
        (send! runtime :command {:op :param/set :instance-id id :param :z :value 0.5} 175)
        (show/tick! runtime 200)
        (is (= [0 0 0] (dmx runtime 1 2)) "moved away: out of contact and unlit"))
      (finally (show/stop! runtime)))))

(deftest a-moving-wavy-sheet-follows-the-beat-and-freezes-on-stop
  ;; 120 BPM: a beat is 500 ms. The crest travels a quarter cycle per beat.
  (let [wave (assoc (:sheet surfaces) :shape :wave :wave {:amplitude-m 0.6 :wavelength-m 8 :speed 0.25}
                    :contact {:distance-m 0.15})
        runtime (start (assoc-in (model) [:settings :video :surfaces :sheet] wave))
        lit? #(not= [0 0 0] (dmx runtime 1 %))]
    (try
      (spawn! runtime :sheet 0)
      ;; Tube A sits 0.1 m out. The sheet passes through it where sin(2π(x/8 + phase)) = 1/6.
      (show/tick! runtime 10)
      (let [first-lit (set (filter lit? (range 8)))]
        (is (seq first-lit))
        (show/tick! runtime 1010)
        (let [later-lit (set (filter lit? (range 8)))]
          (is (seq later-lit))
          (is (not= first-lit later-lit) "two beats later the sheet crosses the tube elsewhere")
          (testing "every lit pixel shows its own grid cell"
            (doseq [n later-lit] (is (near? (grid-dmx n 1) (dmx runtime 1 n)) (str n))))
          (testing "stopping the transport holds the sheet and its contacts"
            (send! runtime :command {:op :transport/set :transport {:playing? false}} 1020)
            (let [pose (get-in (show/snapshot runtime) [:video :surfaces :running 0 :resolved :wave :phase])
                  held (set (filter lit? (range 8)))]
              (show/tick! runtime 1500)
              (show/tick! runtime 2500)
              (is (= held (set (filter lit? (range 8)))))
              (is (= pose (get-in (show/snapshot runtime) [:video :surfaces :running 0 :resolved :wave :phase])))
              (testing "play resumes from the held phase without catching up"
                (send! runtime :command {:op :transport/set :transport {:playing? true}} 2600)
                (show/tick! runtime 3100)
                (let [resumed (get-in (show/snapshot runtime) [:video :surfaces :running 0 :resolved :wave :phase])]
                  ;; One beat after play: a quarter cycle on, not the 3.2 beats of wall time since the stop.
                  (is (< 0.2 (- resumed pose) 0.3) (str "held " pose " resumed " resumed))))))))
      (finally (show/stop! runtime)))))

(deftest a-sphere-seam-and-two-overlapping-feeds
  (let [runtime (start (model))]
    (try
      (spawn! runtime :globe 0)
      (show/tick! runtime 25)
      (testing "the seam: pixels west of it read the first column, east of it the last"
        (is (near? (grid-dmx 0 1) (dmx runtime 201 0)))
        (is (near? (grid-dmx 0 1) (dmx runtime 201 1)))
        (is (near? (grid-dmx 7 1) (dmx runtime 201 2)))
        (is (near? (grid-dmx 7 1) (dmx runtime 201 3))))
      (spawn! runtime :sheet 50 {:priority 10})
      (spawn! runtime :ball 75 {:priority 20})
      (show/tick! runtime 100)
      (let [ramp? (fn [n] (zero? (nth (dmx runtime 1 n) 2)))]
        (testing "where both surfaces touch tube A the higher-priority feed wins"
          (is (ramp? 5)) (is (ramp? 6))
          (is (not (ramp? 4))) (is (near? (grid-dmx 4 1) (dmx runtime 1 4))))
        (testing "lowering that cue lets the sheet's feed take the overlap back, deterministically"
          (let [ball (instance-of runtime :video-surface-ball)]
            (send! runtime :command {:op :instance/set-priority :instance-id ball :priority 5} 125)
            (show/tick! runtime 150)
            (is (near? (grid-dmx 5 1) (dmx runtime 1 5)))
            (is (near? (grid-dmx 6 1) (dmx runtime 1 6))))))
      (finally (show/stop! runtime)))))

(deftest room-and-fixture-values-agree-after-save-and-reload
  (let [runtime (start (model))]
    (try
      (spawn! runtime :sheet 0 {:priority 10})
      (let [id (instance-of runtime :video-surface-sheet)]
        (send! runtime :command {:op :param/set :instance-id id :param :crop-x :value 0.125} 25)
        (send! runtime :command {:op :param/set :instance-id id :param :yaw :value 4} 50))
      (show/tick! runtime 75)
      (let [before-dmx (vec (for [address [1 101] n (range 8)] (dmx runtime address n)))
            before-view (first (get-in (show/snapshot runtime) [:video :surfaces :running]))
            saved (persistence/read-string (show/export-show-source runtime))
            thawed (persistence/thaw (demo/model) saved)
            reloaded (start thawed)]
        (try
          (is (= (get-in @(:state runtime) [:model :settings :video]) (get-in thawed [:settings :video])))
          (testing "the restored instance carries the pose it last evaluated, before any frame runs"
            (is (= (:resolved before-view)
                   (:resolved (first (get-in (show/snapshot reloaded) [:video :surfaces :running]))))))
          (show/tick! reloaded 100)
          (let [after-view (first (get-in (show/snapshot reloaded) [:video :surfaces :running]))]
            (is (= (:resolved before-view) (:resolved after-view)))
            (is (= (:contacts before-view) (:contacts after-view)))
            (is (= before-dmx (vec (for [address [1 101] n (range 8)] (dmx reloaded address n))))
                "the same heads take the same captured values"))
          (finally (show/stop! reloaded))))
      (finally (show/stop! runtime)))))

(deftest two-feeds-add-where-a-cue-blends-with-add
  (let [runtime (start (model))
        launch! (fn [cue t] (:instance-id (send! runtime :command {:op :cue/launch :cue-id cue} t)))
        end! (fn [id t] (send! runtime :command {:op :instance/end :instance-id id :force? true} t))
        add-ball {:id :ball-add :version 1
                  :outputs [{:attribute :color :blend :add :value {:op :video :surface :ball}}]}]
    (try
      (send! runtime :definition {:kind :effects :definition add-ball} 0)
      (send! runtime :definition {:kind :cues :definition {:id :ball-add :effect-id :ball-add :priority 20}} 0)
      (spawn! runtime :sheet 0 {:priority 10 :launch false})
      (let [ball (launch! :ball-add 25)]
        (show/tick! runtime 50)
        (let [ball-only (mapv #(dmx runtime 1 %) [5 6])]
          (end! ball 75)
          (let [sheet (launch! :video-surface-sheet 100)]
            (show/tick! runtime 125)
            (let [sheet-only (mapv #(dmx runtime 1 %) [5 6])]
              (launch! :ball-add 150)
              (show/tick! runtime 175)
              (is (every? (fn [c] (some pos? c)) ball-only) "the ball alone lights the overlap")
              (doseq [[i n] [[0 5] [1 6]]]
                (is (near? (mapv #(min 255 (+ %1 %2)) (nth ball-only i) (nth sheet-only i)) (dmx runtime 1 n) 2.01)
                    (str "pixel " n " sums both feeds")))
              (is (near? (grid-dmx 4 1) (dmx runtime 1 4)) "outside the ball the sheet alone")
              sheet))))
      (finally (show/stop! runtime)))))
