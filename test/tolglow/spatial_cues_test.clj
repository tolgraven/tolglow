(ns tolglow.spatial-cues-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.composition :as composition]
            [tolglow.persistence :as persistence]
            [tolglow.show :as show]
            [tolglow.spatial.cues :as cues]
            [tolglow.spatial-host-test :as host]))

(def request {:op :spawn :id :orb :page-id :main :cell [2 1]
              :volume {:shape :ball :position [0 1 0] :velocity [0 0 0] :gravity [0 0 0]
                       :radius 0.5 :restitution 0.75 :selection #{:position}
                       :falloff {:inner-m 0.5 :outer-m 0.5 :curve :linear}}
              :paint {:red 1.0 :blue 0.0}})

(defn- state [rt] (:model @(:state rt)))
(defn- launch! [rt now cue] (host/submit rt now :command {:op :cue/launch :cue-id cue}))
(defn- change! [rt now id param value]
  (host/submit rt now :command {:op :param/set :instance-id id :param param :value value}))

(deftest spatial-cue-paints-native-fields-in-a-grid-and-follows-its-controls
  (doseq [batch? [true false]]
    (let [rt (host/runtime batch?)]
      (try
        (launch! rt 0 :blue)
        (let [ack (host/submit rt 0 :spatial-cue request)
              id (:instance-id ack)]
          (is (= [:orb :main [2 1]] ((juxt :cue-id :page-id :cell) ack)))
          (is (= :orb (get-in (state rt) [:pages :main :cells [2 1]])))
          (is (= [255 0 0 128 0 128 0 0 255] (host/output rt)))
          (change! rt 10 id :strength 0.5)
          (is (= [128 0 128 64 0 191 0 0 255] (host/output rt)))
          (change! rt 20 id :position [3 1 0])
          (is (= [0 0 255 0 0 255 128 0 128] (host/output rt)))
          (host/submit rt 30 :command {:op :instance/remove :instance-id id})
          (is (= [0 0 255 0 0 255 0 0 255] (host/output rt)))
          (let [next-id (:instance-id (launch! rt 40 :orb))]
            (is (not= id next-id))
            (is (= [255 0 0 128 0 128 0 0 255] (host/output rt)))))
        (finally (show/stop! rt))))))

(deftest volume-spawn-rejects-occupied-identities-and-cells-atomically
  (let [rt (host/runtime true)]
    (try
      (host/submit rt 0 :spatial-cue request)
      (doseq [bad [(assoc request :cell [3 2]) (assoc request :id :other)
                   (assoc request :id :other :cell [9 9])
                   (assoc request :id :other :cell [3 2] :members [{:cue-id :missing}])]]
        (let [before (update (select-keys (state rt) [:registry :pages :instances]) :instances keys)
              receipt (show/submit! rt :spatial-cue bad)]
          (host/tick rt 0)
          (is (= :rejected (:status @receipt)))
          (is (= before (update (select-keys (state rt) [:registry :pages :instances]) :instances keys)))))
      (finally (show/stop! rt)))))

(deftest saved-volume-scene-keeps-head-scopes-and-owned-activations
  (let [rt (host/runtime true)]
    (try
      (let [snapshot (state rt)
            macro {:id :wash-scene :mode :macro :slots [{:id :blue :cue-id :blue}]}
            ready (composition/save snapshot macro)
            _ (host/submit rt 0 :import {:source (persistence/write-string ready)})
            normal-id (:instance-id (launch! rt 5 :blue))
            scene (assoc request :id :scene :paint {} :scene-id :wash-scene)
            parent (:instance-id (host/submit rt 5 :spatial-cue scene))
            model (state rt)
            key (cues/key-for (get-in model [:instances parent]))
            child (get-in model (into cues/run-path [key :ids 0]))
            restored (persistence/thaw model (persistence/read-string (persistence/write-string model)))
            restored-parent (first (filter #(= :scene (:cue-id %)) (vals (:instances restored))))
            next-child (get-in restored (into cues/run-path [key :ids 0]))]
        (is (and child (not= child normal-id)))
        (is (= key (cues/key-for restored-parent)))
        (is (= :spatial-cue (get-in restored [:instances next-child :source :adapter])))
        (host/submit rt 10 :command {:op :instance/remove :instance-id normal-id})
        (is (= [0 0 255 0 0 128 0 0 0] (host/output rt)))
        (host/submit rt 15 :command {:op :instance/remove :instance-id parent})
        (is (empty? (:instances (state rt))))
        (is (= (vec (repeat 9 0)) (host/output rt))))
      (finally (show/stop! rt)))))

(deftest signal-driven-placement-and-hsl-paint-use-the-current-show
  (let [rt (host/runtime false)
        source (assoc request :id :hue :paint {:color [0 0 0]}
                      :sources {:position [{:variable [:x] :default 3} 1 0]
                                :color {:hsl [0.5 1 0.5]}})]
    (try
      (host/submit rt 0 :spatial-cue source)
      (is (= [0 0 0 0 0 0 0 255 255] (host/output rt)))
      (let [model (assoc-in (state rt) [:variables :x] 0)]
        (host/submit rt 5 :import {:source (persistence/write-string model)}))
      (host/tick rt 10)
      (is (= [0 255 255 0 127 128 0 0 0] (host/output rt)))
      (finally (show/stop! rt)))))
