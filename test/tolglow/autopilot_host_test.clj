(ns tolglow.autopilot-host-test
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.persistence :as save]
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
