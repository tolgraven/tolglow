(ns tolglow.autopilot-host-test
  (:require [afterglow.control.model :as control]
            [afterglow.time :as time]
            [clojure.test :refer [deftest is testing]]
            [tolglow.autopilot.signal :as signal]
            [tolglow.persistence :as save]
            [tolglow.rig.pixel-layout :as pixels]
            [tolglow.show :as show]))

(defn- fixture [id address group]
  {:id id :universe 1 :address address :position [(* 0.5 (dec address)) 0 0]
   :channels {:dimmer 1} :groups [group] :max-watts 100 :measured-relative-brightness 1.0})

(defn- look [id targets outputs]
  {:id id :name (name id) :version 1 :targets targets :params {} :outputs outputs})

(defn- runtime [fixtures effects batch?]
  (let [registry {:effects (into {} (map (juxt :id identity)) effects)
                  :cues (into {} (for [{:keys [id]} effects] [id {:id id :version 1 :effect-id id}]))}]
    (show/create-runtime {:model (assoc (control/new-show {:registry registry}) :rig {:fixtures fixtures})
                          :workspace-options nil :batch? batch?})))

(defn- tick [rt now]
  (show/tick! rt now)
  (is (nil? (get-in @(:state rt) [:status :error])) (str (get-in @(:state rt) [:status :error]))))

(defn- submit [rt now type payload]
  (let [ack (show/submit! rt type payload)]
    (tick rt now)
    (is (= :accepted (:status @ack)) (pr-str @ack))
    @ack))

(defn- levels [rt ids]
  (mapv #(:local-brightness %) (map (show/source-brightness rt ids) ids)))

(deftest conductor-rebalances-on-musical-boundaries-and-releases-control
  (let [ids [:a :b :c :d]
        rig (conj (mapv #(fixture %1 %2 :stage) ids (range 1 5)) (fixture :outside 5 :other))
        rt (runtime rig [(look :wash (conj ids :outside) {:dimmer 1.0})
                         (assoc (look :conduct [] {}) :show-control? true)] true)]
    (try
      (submit rt 0 :command {:op :cue/launch :cue-id :wash})
      (submit rt 25 :settings {:scope :autopilot :values {:enable-cue :conduct :scope {:group-id :stage}
                               :targets {:brightness 0.5 :activity 0.23} :strategy :instant :seed 71
                               :interval {:beats 1} :window {:bars 1} :rotate? true}})
      (is (every? #(= 1.0 %) (levels rt ids)))
      (let [cue (submit rt 50 :command {:op :cue/launch :cue-id :conduct})
            first-look (levels rt ids)
            count-lit #(count (filter (fn [n] (> n 0.99)) %))]
        (is (= 2 (count-lit first-look)) (str first-look))
        (is (= [1.0] (levels rt [:outside])))
        (tick rt 75)
        (is (= first-look (levels rt ids)))
        (doseq [t (range 100 2800 25)] (show/tick! rt t))
        (let [view (:autopilot (show/snapshot rt))]
          (is (> (get-in view [:decision :decision] 0) 1))
          (is (< (abs (- (get-in view [:statistics :brightness :mean] 0) 0.5)) 0.08))
          (is (number? (get-in view [:statistics :activity :target-hit-fraction])))
          (is (= [1.0] (levels rt [:outside])))
          (is (= (:targets view) (get-in (:model (save/read-string (show/export-show-source rt)))
                                        [:variables :lighting/autopilot :targets]))))
        (let [instance (some #(when (= :conduct (:cue-id %)) (:id %)) (:instances (show/snapshot rt)))]
          (is (some? instance) (str cue))
          (submit rt 2825 :command {:op :instance/remove :instance-id instance}))
        (is (false? (get-in (show/snapshot rt) [:autopilot :enabled?])))
        (is (every? #(= 1.0 %) (levels rt ids))))
      (finally (show/stop! rt)))))

(deftest hdr-highlights-whiten-the-source-and-its-neighbour-before-final-caps
  (doseq [batch? [false true]]
    (let [ids [:hot :near :far]
          rig (mapv (fn [id addr x] {:id id :universe 1 :address addr :position [x 0 0]
                                     :channels {:red 1 :green 2 :blue 3}})
                    ids [1 4 7] [0 0.5 5])
          rt (runtime rig [(look :hot-red [:hot] {:color [1.21 0.0 0.0]})
                           (look :blue [:near :far] {:color [0.0 0.0 0.1]})] batch?)]
      (try
        (submit rt 0 :settings {:scope :hdr :values {:enabled? true :gain 1.0 :max-excess 1.0
                                                     :spill 1.0 :radius 1.0 :falloff :linear}})
        (submit rt 25 :command {:op :cue/launch :cue-id :blue})
        (submit rt 50 :command {:op :cue/launch :cue-id :hot-red})
        (let [[r g b nr ng nb fr fg fb] (map #(bit-and 255 %) (get-in @(:state rt) [:output :universes 1]))]
          (is (= 255 r))
          (is (and (pos? g) (pos? b)) (str "source " [r g b]))
          (is (and (pos? nr) (pos? ng) (> nb 25)) (str "receiving lamp " [nr ng nb]))
          (is (= [0 0] [fr fg]))
          (is (<= 25 fb 26)))
        (submit rt 75 :command {:op :blackout/set :value true})
        (is (every? zero? (get-in @(:state rt) [:output :universes 1])))
        (finally (show/stop! rt))))))

(deftest conductor-eases-the-setpoint-between-musical-decisions
  (let [rig [(fixture :lamp 1 :stage)]
        rt (runtime rig [(look :wash [:lamp] {:dimmer 1.0})] true)
        target #(get-in (show/snapshot rt) [:autopilot :targets :brightness])]
    (try
      (submit rt 0 :command {:op :cue/launch :cue-id :wash})
      (submit rt 25 :settings {:scope :autopilot
                               :values {:enabled? true :targets {:brightness 0.0}
                                        :interval {:bars 2} :target-response {:release {:ms 100}}}})
      (is (== 1 (target)))
      (is (== 0.4 (get-in (show/snapshot rt) [:autopilot :requested-targets :activity])))
      (is (zero? (get-in (show/snapshot rt) [:autopilot :requested-targets :brightness])))
      (tick rt 75)
      (is (== 0.5 (target)))
      (tick rt 125)
      (is (zero? (target)))
      (finally (show/stop! rt)))))

(deftest repatching-to-a-new-universe-resets-the-conductor-meter
  (let [lamp (fixture :lamp 1 :stage)
        rt (runtime [lamp] [(look :wash [:lamp] {:dimmer 1.0})] true)]
    (try
      (submit rt 0 :command {:op :cue/launch :cue-id :wash})
      (submit rt 25 :settings {:scope :autopilot :values {:enabled? true :interval {:ms 25}}})
      (submit rt 50 :rig {:fixtures [(assoc lamp :universe 3)]})
      (is (contains? (get-in @(:state rt) [:output :universes]) 3))
      (tick rt 75)
      (is (number? (get-in (show/snapshot rt) [:autopilot :statistics :brightness :mean])))
      (finally (show/stop! rt)))))

(deftest a-manual-blackout-counts-as-dark-song-time-and-holds-cue-decisions
  (let [rt (runtime [(fixture :lamp 1 :stage)] [(look :wash [:lamp] {:dimmer 1.0})] true)
        view #(:autopilot (show/snapshot rt))]
    (try
      (submit rt 0 :command {:op :cue/launch :cue-id :wash})
      (submit rt 25 :settings {:scope :autopilot :values {:enabled? true :targets {:brightness 1.0}
                               :window {:ms 100} :interval {:ms 25}}})
      (is (== 1 (get-in (view) [:statistics :brightness :instant])))
      (let [decision (:decision (view))]
        (submit rt 50 :command {:op :blackout/set :value true})
        (doseq [t (range 75 176 25)] (tick rt t))
        (is (= decision (:decision (view))))
        (is (zero? (get-in (view) [:statistics :brightness :mean])))
        (is (zero? (get-in (view) [:statistics :brightness :lit-fraction]))))
      (finally (show/stop! rt)))))

(deftest standalone-hdr-reuses-the-scoped-members-and-invalidates-on-repatch
  (let [a (fixture :a 1 :front) b (fixture :b 2 :back)
        rt (runtime [a b] [(look :wash [:a :b] {:dimmer 0.4})] true)]
    (try
      (submit rt 0 :command {:op :cue/launch :cue-id :wash})
      (submit rt 25 :settings {:scope :hdr :values {:enabled? true :scope {:group-id :front} :gain 2.0}})
      (let [plan (:hdr-scope @(:state rt))]
        (is (= [:a] (:ids plan)))
        (is (= [0.8 0.4] (levels rt [:a :b])))
        (tick rt 50)
        (is (identical? plan (:hdr-scope @(:state rt))))
        (submit rt 75 :rig {:fixtures [(assoc a :groups [:back]) (assoc b :groups [:front])]})
        (is (= [:b] (:ids (:hdr-scope @(:state rt)))))
        (is (= [0.4 0.8] (levels rt [:a :b]))))
      (finally (show/stop! rt)))))

(defn- reds [rt n]
  (vec (take-nth 3 (map #(bit-and 255 %) (take (* 3 n) (get-in @(:state rt) [:output :universes 1]))))))

(defn- rgb-line [ids xs]
  (mapv (fn [id i x] {:id id :universe 1 :address (inc (* 3 i)) :position [x 0 0]
                      :channels {:red 1 :green 2 :blue 3}})
        ids (range) xs))

(def ^:private receiver-distances [0.1 0.2 0.3 0.4])

(defn- expected-spill
  "Red bytes on receivers for 0.21 excess, linear falloff and no whitening."
  [spill radius]
  (mapv #(Math/round (* 255.0 (* 0.21 spill (max 0.0 (- 1.0 (/ % radius)))))) receiver-distances))

(defn- modulated-run [seed]
  (let [ids [:hot :r1 :r2 :r3 :r4]
        rt (runtime (rgb-line ids (cons 0 receiver-distances))
                    [(look :hot-red [:hot] {:color [1.21 0.0 0.0]})] true)
        radius {:random {:seed seed} :period {:beats 1} :range [0.15 0.45]}
        spill {:waveform :sine :period {:beats 4} :range [0.25 1.0]}]
    (try
      (submit rt 0 :settings {:scope :hdr :values {:enabled? true :white-mix 0.0 :spill-white-mix 0.0
                                                   :falloff :linear :radius radius :spill spill}})
      (submit rt 25 :command {:op :cue/launch :cue-id :hot-red})
      (vec (for [t (range 50 4050 125)]
             (do (tick rt t)
                 (let [model (assoc (:model @(:state rt)) :time-ms t)
                       context {:start-clock (get-in model [:extensions :tolglow.autopilot/hdr-clock])}
                       clock (time/frame-clock model)
                       r (signal/value model radius context)
                       s (signal/value model spill context)]
                   {:beat (long (Math/floor (:beat clock))) :radius r :spill s
                    :expected (expected-spill s r) :red (subvec (reds rt 5) 1)}))))
      (finally (show/stop! rt)))))

(deftest seeded-lfo-and-random-signals-drive-spill-radius-and-strength-per-frame
  (let [frames (modulated-run 97)]
    (testing "every frame spills exactly what the resolved radius and strength predict"
      (doseq [{:keys [red expected] :as frame} frames]
        (is (= expected red) (pr-str frame))))
    (testing "the sample-and-hold radius stays in range, holds for a beat and changes between beats"
      (is (every? #(<= 0.15 (:radius %) 0.45) frames))
      (is (every? #(= 1 (count (set (map :radius %)))) (vals (group-by :beat frames))))
      (is (<= 4 (count (set (map :radius frames)))) (pr-str (map :radius frames)))
      (is (< 1 (count (set (map #(count (filter pos? (:red %))) frames)))) "the lit neighbour count follows the radius"))
    (testing "the LFO sweeps spill strength across its range"
      (is (< (reduce min (map :spill frames)) 0.3))
      (is (> (reduce max (map :spill frames)) 0.95)))
    (testing "the same seed replays identically and another seed differs"
      (is (= (map :red frames) (map :red (modulated-run 97))))
      (is (not= (map :radius frames) (map :radius (modulated-run 98)))))))

(deftest excess-spills-to-neighbouring-strip-pixels-by-distance
  (doseq [batch? [false true]]
    (let [strip (pixels/fixtures {:id :strip :group :strip :shape :path :color-layout :rgb
                                  :universe 1 :address 1 :pixels 9 :points [[0 0 0] [0.8 0 0]]})
          ids (mapv :id strip)
          rt (runtime strip [(look :hot [(nth ids 4)] {:color [1.21 0.0 0.0]})] batch?)
          hdr {:enabled? true :scope {:group-id :strip} :white-mix 0.0 :spill-white-mix 0.0
               :spill 1.0 :radius 0.35}
          set-hdr! (fn [t values] (submit rt t :settings {:scope :hdr :values values}) (reds rt 9))]
      (try
        (is (= [0.1 0.0 0.0] (:position (nth strip 1))) "pixels are 0.1 m apart")
        ;; Overrange light needs HDR. Without spill only the source clamps.
        (submit rt 0 :settings {:scope :hdr :values (assoc hdr :spill 0.0)})
        (submit rt 10 :command {:op :cue/launch :cue-id :hot})
        (is (= [0 0 0 0 255 0 0 0 0] (reds rt 9)) "without spill the clamped pixel keeps its excess")
        ;; Linear weights at 0.1, 0.2 and 0.3 m are 5/7, 3/7 and 1/7 of 0.21 excess.
        (is (= [0 8 23 38 255 38 23 8 0] (set-hdr! 25 (assoc hdr :falloff :linear))))
        (is (= [0 1 10 27 255 27 10 1 0] (set-hdr! 50 (assoc hdr :falloff :quadratic))))
        (let [curve [[0 1] [0.3 1] [0.6 0.8] [1 0]]]
          (is (= [0 15 44 54 255 54 44 15 0] (set-hdr! 75 (assoc hdr :falloff curve))))
          (is (= curve (get-in (show/snapshot rt) [:settings :hdr :falloff])))
          (is (= curve (get-in (save/read-string (show/export-show-source rt)) [:model :settings :hdr :falloff]))
              "an authored curve is portable show data"))
        (is (= [0 0 0 38 255 38 0 0 0]
               (set-hdr! 100 (assoc hdr :falloff :linear :scope {:fixture-ids (subvec ids 3 6)}))))
        (let [receipt (show/submit! rt :settings {:scope :hdr :values (assoc hdr :falloff [[0 1] [0.5 2] [1 0]])})]
          (show/tick! rt 125)
          (is (= :rejected (:status (deref receipt 1000 {:status :timeout})))
              "an out-of-range weight is rejected before rendering")
          (is (= [0 0 0 38 255 38 0 0 0] (reds rt 9)) "the previous settings stay in force"))
        (finally (show/stop! rt))))))
