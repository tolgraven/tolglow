(ns tolglow.spatial-cues-test
  (:require [afterglow.render.frame :as frame]
            [afterglow.spatial.field :as field]
            [clojure.test :refer [deftest is]]
            [tolglow.composition :as composition]
            [tolglow.persistence :as persistence]
            [tolglow.show :as show]
            [tolglow.visualization.cue-pads :as pads]
            [tolglow.spatial.cues :as cues]
            [tolglow.studio.web :as web]
            [tolglow.transit :as transit]
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
      (let [colors (pads/present! (:cue-pad-previews rt) :grid @(:state rt) [pads/query-id :main [0 0] 8])]
        (is (= [0 255 255] (mapv #(Math/round (* 255.0 %)) (:hue colors))) "Preview reads the same resolved HSL paint as the output"))
      (let [model (assoc-in (state rt) [:variables :x] 0)]
        (host/submit rt 5 :import {:source (persistence/write-string model)}))
      (host/tick rt 10)
      (is (= [0 255 255 0 127 128 0 0 0] (host/output rt)))
      (finally (show/stop! rt)))))

(deftest ring-spawn-morphs-a-ball-and-honors-the-ordinary-launch-targets
  (let [rt (host/runtime true)
        submit! (fn [op data]
                  (let [receipt (show/submit! rt op data)]
                    (host/tick rt 0)
                    receipt))
        app (web/app {:heartbeat! (constantly nil) :submit! submit!})
        req (-> request (dissoc :cell) (assoc :launch {:targets [:near]}))
        response (app {:uri "/api/spatial/cues" :request-method :post
                       :headers {"host" "localhost" "content-type" "application/transit+json" "accept" "application/transit+json"}
                       :body (java.io.ByteArrayInputStream. (.getBytes (transit/encode req) "UTF-8"))})
        ack (transit/decode (:body response))]
    (try
      (is (= 200 (:status response)) (pr-str ack))
      (is (= :accepted (:status ack)))
      (is (= [0 0] (:cell ack)))
      (is (= [0 0 0 128 0 0 0 0 0] (host/output rt)))
      (let [id (:instance-id ack)
            key (cues/key-for (get-in (state rt) [:instances id]))]
        (change! rt 5 id :shape :rounded-box)
        (change! rt 10 id :size [4 1 1])
        (change! rt 15 id :rounding 0.75)
        (change! rt 20 id :scale [1.5 1 1])
        (let [signal (get-in (state rt) [:variables :spatial :signals key])]
          (is (= id (:instance-id signal)))
          (is (= :rounded-box (get-in signal [:volume :geometry :shape])))
          (is (= 0.75 (get-in signal [:volume :geometry :rounding])))
          (is (= [1.5 1 1] (get-in signal [:volume :scale])))
          (is (= 1 (:affected-head-count signal)))
          (is (= #{:near} (get-in (state rt) [:variables :spatial :members key])))))
      (finally (show/stop! rt)))))

(deftest timed-scene-releases-its-output-on-the-exact-musical-deadline
  (let [rt (host/runtime false)]
    (try
      (host/submit rt 0 :spatial-cue (assoc request :paint {} :members [{:cue-id :blue}]
                                           :launch {:mode :one-shot :duration {:beats 1/2}}))
      (host/tick rt 249)
      (is (pos? (nth (host/output rt) 2)))
      (host/tick rt 250)
      (is (= (vec (repeat 9 0)) (host/output rt)))
      (is (empty? (:instances (state rt))))
      (finally (show/stop! rt)))))

(deftest two-optical-cues-share-the-previous-compact-output
  (let [rt (host/runtime true)
        region (assoc-in request [:volume :selection] #{:position :beam})
        observe field/compact-observations
        reads (atom [])]
    (try
      (host/submit rt 0 :spatial-cue region)
      (host/submit rt 25 :spatial-cue (assoc region :id :second :cell [3 1]))
      (let [previous (get-in @(:state rt) [:output :compact])]
        (with-redefs [field/compact-observations
                      (fn [opts index compact]
                        (swap! reads conj compact)
                        (observe opts index compact))]
          (host/tick rt 50))
        (is (some? previous))
        (is (= 1 (count @reads)))
        (is (identical? previous (first @reads))))
      (finally (show/stop! rt)))))

(deftest cache-only-optical-plans-across-committed-frames
  (doseq [batch? [false true]]
    (let [rt (host/runtime batch?)
          select-beams (assoc-in request [:volume :selection] #{:beam :position})]
      (try
        (host/submit rt 0 :spatial-cue select-beams)
        (show/tick! rt 25)
        (let [first-plan (get-in @(:state rt) [:spatial-cache :optics])]
          (is (some? (:options first-plan)))
          (is (nil? (:values (:options first-plan))))
          (show/tick! rt 50)
          (let [second-plan (get-in @(:state rt) [:spatial-cache :optics])]
            (is (identical? (:options first-plan) (:options second-plan)))
            (when batch? (is (identical? (:index first-plan) (:index second-plan))))))
        (finally (show/stop! rt))))))

(deftest a-room-edit-isolates-the-invalid-ball-and-retries-after-correction
  (let [rt (host/runtime true)]
    (try
      (launch! rt 0 :blue)
      (let [id (:instance-id (host/submit rt 0 :spatial-cue request))
            key (cues/key-for (get-in (state rt) [:instances id]))
            view #(get-in (state rt) [:variables :spatial])]
        (host/settings rt 10 {:bounds {:min [0 0 0] :max [0.6 0.6 0.6]}})
        (is (string? (get-in (view) [:signals key :error])))
        (is (= [key] (mapv :volume-id (filter #(= :spatial/invalid-volume (:type %)) (:events (view))))))
        (is (empty? (get-in (view) [:members key])))
        (is (= [0 0 255 0 0 255 0 0 255] (host/output rt)))
        (host/tick rt 25)
        (is (empty? (filter #(= :spatial/invalid-volume (:type %)) (:events (view)))))
        (host/settings rt 50 {:bounds {:min [-5 0 -5] :max [5 5 5]}})
        (is (nil? (get-in (view) [:signals key :error])))
        (is (= [255 0 0 128 0 128 0 0 255] (host/output rt))))
      (finally (show/stop! rt)))))

(deftest an-aim-only-sculpted-cue-steers-the-reachable-moving-head
  (doseq [batch? [true false]]
    (let [rt (host/runtime batch?)
          head {:id :moving :position [0 1 0] :direction [0 0 1] :beam-angle 12
                :universe 1 :address 4 :channels {:red 1 :green 2 :blue 3 :pan 4 :tilt 5}}
          rig [(first host/fixtures) head]
          model (assoc-in (state rt) [:rig :fixtures] rig)
          centre [2.0 1.0 3.0]
          command (assoc request :paint {:aim :volume}
                         :volume {:shape :sphere :position centre :radius 0.0 :selection #{:aimable}})]
      (try
        (host/submit rt 0 :import {:source (persistence/write-string model)})
        (let [id (:instance-id (host/submit rt 0 :spatial-cue command))
              key (cues/key-for (get-in (state rt) [:instances id]))
              aim (fn []
                    (let [output (:output @(:state rt))]
                      (or (get-in output [:values :moving :aim])
                          (some #(when (= :moving (:id %)) (:aim %)) (frame/project-compact (:compact output))))))]
          (is (= #{:moving} (get-in (state rt) [:variables :spatial :members key])))
          (is (= centre (aim)))
          (change! rt 25 id :position [1.0 2.0 3.0])
          (is (= [1.0 2.0 3.0] (aim))))
        (finally (show/stop! rt))))))

(deftest a-volume-and-its-children-retain-their-mask-through-the-release-envelope
  (doseq [batch? [true false]]
    (let [rt (host/runtime batch?)]
      (try
        (launch! rt 0 :blue)
        (let [parent (:instance-id (host/submit rt 0 :spatial-cue
                                               (assoc request :paint {:blue 0.0} :members [{:effect-id :red}]
                                                      :launch {:system {:release {:beats 1}}})))]
          (host/submit rt 25 :command {:op :instance/end :instance-id parent})
          (host/tick rt 275)
          (is (= [128 0 128 64 0 191 0 0 255] (host/output rt)))
          (is (= 3 (count (:instances (state rt)))))
          (host/tick rt 525)
          (is (= [0 0 255 0 0 255 0 0 255] (host/output rt)))
          (is (= 1 (count (:instances (state rt))))))
        (finally (show/stop! rt))))))

(deftest a-failed-scene-member-recovers-on-its-next-registry-version
  (let [active {"test-owner" {:members [{:effect-id :wash :launch {:params {:brightness 0.5}}}]}}
        effect {:id :wash :version 1 :params {}}
        initial {:registry {:effects {:wash effect}}}
        start (fn [model _ _] {:model model :ack {:status :accepted :instance-id "test-child"}})
        failure (cues/reconcile initial active 0 start)
        lease-path (conj cues/run-path "test-owner")
        lease (get-in failure lease-path)]
    (is (string? (:error lease)))
    (is (identical? lease (get-in (cues/reconcile failure active 25 start) lease-path)))
    (let [updated (assoc effect :version 2 :params {:brightness {:type :number :default 0.6 :bounds [0 1]}})
          repaired (assoc-in failure [:registry :effects :wash] updated)
          recovered (cues/reconcile repaired active 50 start)]
      (is (= {0 "test-child"} (get-in recovered (conj lease-path :ids))))
      (is (nil? (get-in recovered (conj lease-path :error)))))))

(deftest beam-volumes-follow-the-last-committed-profiled-shutter
  (doseq [batch? [true false]]
    (let [rt (host/runtime batch?)
          lamp {:id :green-lamp :position [0 0 0] :address 1 :universe 1
                :channels {:green 1 :focus 3
                           :shutter {:offset 2 :functions [{:type :shutter-closed :start 0 :end 10}]}}}
          emit {:id :emit :version 1 :params {:opening {:type :number :default 0.0 :bounds [0 1]}}
                :outputs {:green 1.0 :shutter {:op :param :key :opening}}}
          model (-> (state rt) (assoc-in [:rig :fixtures] [lamp]) (assoc-in [:registry :effects :emit] emit))
          region (assoc request :paint {:focus 0.25}
                        :volume {:shape :sphere :position [0 0 2] :radius 0.25 :selection #{:beam}})]
      (try
        (host/submit rt 0 :import {:source (persistence/write-string model)})
        (let [emitter (:instance-id (host/submit rt 0 :command {:op :effect/launch :effect-id :emit}))
              id (:instance-id (host/submit rt 25 :spatial-cue region))
              key (cues/key-for (get-in (state rt) [:instances id]))
              members #(get-in (state rt) [:variables :spatial :members key])]
          (is (empty? (members)))
          (change! rt 50 emitter :opening 1.0)
          (is (empty? (members)))
          (host/tick rt 75)
          (is (= #{:green-lamp} (members)))
          (change! rt 100 emitter :opening 0.0)
          (host/tick rt 125)
          (is (empty? (members))))
        (finally (show/stop! rt))))))

(deftest a-cylinders-native-mask-follows-its-live-radius
  (let [rt (host/runtime true)
        shape {:shape :cylinder :position [0 1 0] :radius 0.25 :height 2.0}
        region (assoc request :volume shape :paint {:red 1.0})
        covered? (fn [n] (pos? (nth (host/output rt) n)))]
    (try
      (let [id (:instance-id (host/submit rt 0 :spatial-cue region))]
        (is (covered? 0))
        (is (not (covered? 3)))
        (change! rt 25 id :radius 0.75)
        (is (covered? 3))
        (is (not (covered? 6)))
        (host/submit rt 50 :command {:op :instance/remove :instance-id id}))
      (host/submit rt 75 :spatial-cue (assoc region :id :sourced-cylinder :cell [3 1] :sources {:radius 0.75}))
      (is (covered? 3))
      (is (not (covered? 6)))
      (finally (show/stop! rt)))))

(deftest instance-waveform-aims-the-spatial-field-on-the-same-commit
  (let [rt (host/runtime true)]
    (try
      (launch! rt 0 :blue)
      (let [id (:instance-id (host/submit rt 0 :spatial-cue request))
            binding {:type :lfo :waveform :sine :from [3 1 0] :to [3 1 0]}
            addr {:instance-id id :param :position}]
        (is (= [255 0 0 128 0 128 0 0 255] (host/output rt)))
        (is (= :accepted (:status (host/submit rt 25 :modulation-bind (assoc addr :binding binding)))))
        (is (= [0 0 255 0 0 255 255 0 0] (host/output rt)))
        (is (= :accepted (:status (host/submit rt 50 :modulation-unbind addr))))
        (is (= [255 0 0 128 0 128 0 0 255] (host/output rt))))
      (finally (show/stop! rt)))))

(deftest spatial-cue-links-read-the-driving-activation-at-this-frame
  (let [rt (host/runtime true)
        definition {:id :driver :version 1 :targets [:left] :outputs {:red 0.0}
                    :params {:position {:type :vector :size 3 :default [0 0 0]}
                             :color {:type :color :default [1 0 0]}}}
        region (assoc request :volume (host/sphere 0) :paint {:color [0 0 0]}
                              :sources {:position {:cue-id :driver :param :position}
                                        :color {:cue-id :driver :param :color}})
        dark (vec (repeat 9 0))]
    (try
      (host/submit rt 0 :command {:op :registry/publish :kind :effects :definition definition})
      (host/submit rt 0 :command {:op :cue/put :cue {:id :driver :effect-id :driver :version 1}})
      (let [driver (:instance-id (launch! rt 0 :driver))
            bind! (fn [param value]
                    (host/submit rt 25 :modulation-bind
                                 {:instance-id driver :param param
                                  :binding {:type :lfo :waveform :sine :from value :to value}}))
            hold {:instance-id driver :params [:position :color]}]
        (host/submit rt 0 :spatial-cue region)
        (is (= dark (host/output rt)))
        (bind! :position [3 1 0])
        (is (= [0 0 0 0 0 0 255 0 0] (host/output rt)))
        (bind! :color [0 1 0])
        (is (= [0 0 0 0 0 0 0 255 0] (host/output rt)))
        (host/submit rt 50 :modulation-hold hold)
        (is (= dark (host/output rt)))
        (host/submit rt 75 :modulation-resume hold)
        (is (= [0 0 0 0 0 0 0 255 0] (host/output rt)))
        (let [latest (:instance-id (launch! rt 100 :driver))]
          (is (= dark (host/output rt)))
          (host/submit rt 125 :command {:op :instance/remove :instance-id latest})
          (is (= [0 0 0 0 0 0 0 255 0] (host/output rt)))))
      (finally (show/stop! rt)))))

(deftest linked-volumes-use-one-committed-frame-for-mutual-feedback
  (let [rt (host/runtime true)
        region (fn [id cell x link paint]
                 (assoc request :id id :cell cell :volume (host/sphere x) :paint paint
                                :sources {:position {:cue-id link :param :position}}))]
    (try
      (let [one (show/submit! rt :spatial-cue (region :one [0 0] 0 :two {:red 1.0}))
            two (show/submit! rt :spatial-cue (region :two [1 0] 3 :one {:green 1.0}))]
        (host/tick rt 0)
        (is (= [:accepted :accepted] (mapv (comp :status deref) [one two])))
        (is (= [0 255 0 0 0 0 255 0 0] (host/output rt)))
        (host/tick rt 25)
        (is (= [255 0 0 0 0 0 0 255 0] (host/output rt)))
        (host/tick rt 50)
        (is (= [0 255 0 0 0 0 255 0 0] (host/output rt))))
      (finally (show/stop! rt)))))
