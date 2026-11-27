(ns tolglow.spatial-reload-test
  "Spatial cues around a hot reload that the engine accepts or rejects. A
  rejected revision must leave captured DMX, the spatial signals and the
  scene member leases exactly as in a twin show that never published it. An
  accepted revision switches the volume on its takeover frame."
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.show :as show]
            [tolglow.spatial :as spatial]
            [tolglow.spatial.cues :as cues]
            [tolglow.spatial-host-test :as host]))

(def orb
  {:op :spawn :id :orb :page-id :main :cell [2 1]
   :volume {:shape :ball :position [0 1 0] :velocity [0 0 0] :gravity [0 0 0]
            :radius 0.5 :restitution 0.75 :selection #{:position}
            :falloff {:inner-m 0.5 :outer-m 0.5 :curve :linear}}
   :paint {:red 1.0 :blue 0.0}})

(def wide-falloff {:inner-m 0.0 :outer-m 3.0 :curve :linear})

(def aimer
  "A sphere that steers the reachable head toward its centre and paints red."
  {:op :spawn :id :orb :page-id :main :cell [4 1]
   :volume {:shape :sphere :position [0 3 0] :radius 3.0 :selection #{:position}}
   :paint {:aim :volume :red 1.0}})

(def variants
  "Each spawns an orb and revises one thing its spatial pass reads before the
  engine runs: the volume, the scene members, sources that shape the mask,
  a sourced paint value that reaches an output, or the aim points."
  {:volume {:spawn orb
            :revise #(-> % (assoc-in [:spatial-volume :volume :falloff] wide-falloff)
                         (update :params dissoc :inner-m :outer-m))}
   :members {:spawn (assoc orb :paint {:blue 0.0} :members [{:cue-id :blue}])
             :revise #(assoc-in % [:spatial-volume :members] [{:effect-id :red}])}
   :sources {:spawn orb
             :revise #(assoc-in % [:spatial-volume :sources] {:inner-m 0.25 :outer-m 3.0})}
   :paint {:spawn orb
           :revise #(assoc-in % [:spatial-volume :sources] {:red 0.3})}
   :aim {:spawn aimer
         :revise #(assoc-in % [:spatial-volume :sources] {:position [2 1 2]})}})

(defn- param [key] {:op :param :key key})

(def head
  "A moving head at the orb's centre, where every variant weighs it fully, so
  a revision can aim it at its own position, which the renderer refuses."
  {:id :moving :position [0 1 0] :direction [0 0 1] :beam-angle 12
   :universe 1 :address 10 :channels {:red 1 :green 2 :blue 3 :pan 4 :tilt 5}})

(def driver
  "An ordinary cue whose level a spatial source reads."
  {:id :driver :version 1 :name "Driver" :targets [:far]
   :params {:level {:type :number :default 0.5 :bounds [0.0 1.0]}}
   :outputs {:green {:op :param :key :level}}})

(defn- runtime [{:keys [failing-compile background-compile]}]
  (let [captured (atom nil)
        admitted (atom [])
        effects {:red {:id :red :version 1 :params {} :outputs {:red 1.0}}
                 :blue {:id :blue :version 1 :params {} :outputs {:blue 1.0}}
                 :driver driver
                 :hot {:id :hot :version 1 :params {} :targets [:left] :outputs {:color [1.4 0 0]}}}
        cues {:blue {:id :blue :version 1 :effect-id :blue}
              :hot {:id :hot :version 1 :effect-id :hot}
              :driver {:id :driver :version 1 :effect-id :driver :reload-policy :preserve-state}}]
    (cond-> (assoc (show/create-runtime {:model (assoc (control/new-show {:registry {:effects effects :cues cues}})
                                                       :rig {:fixtures (conj host/fixtures head)})
                                         :worker-timeout-ms 10000 :effect-log! (fn [_])
                                         :background-compile background-compile
                                         ;; Record every committed domain event on its way to the subscribers.
                                         :event-admit (fn [model events]
                                                        (swap! admitted into events)
                                                        (subscribers/admit model events))
                                         :sink {:send! (fn [universes _]
                                                         (reset! captured (mapv #(bit-and 255 %) (take 14 (get universes 1)))))}})
                   ::host/captured-dmx captured ::admitted admitted)
      failing-compile
      (update :compile (fn [compile-effect]
                         (fn [{:keys [id version] :as definition}]
                           (if (= failing-compile [id version])
                             (throw (ex-info "Compiler rejected this revision" {:effect-id id}))
                             (compile-effect definition))))))))

(defn- state [rt] (:model @(:state rt)))

(defn- observed
  "What one committed frame leaves behind beside its captured universe."
  [rt]
  (let [{:keys [model status]} @(:state rt)]
    {:dmx (host/output rt)
     :spatial (get-in model [:variables :spatial])
     :physics (get-in model spatial/state-path)
     :leases (get-in model cues/run-path)
     :instances (into (sorted-map) (map (fn [[id i]] [id ((juxt :cue-id :effect-id :effect-version) i)]))
                      (:instances model))
     :error (:error status)}))

(def times (vec (range 50 501 50)))

(defn- capture!
  "Spawn the cue, publish the revision at `publish-at` and return every later frame."
  [rt spawn policy revise publish-at]
  (host/submit rt 0 :spatial-cue (assoc spawn :launch {:reload-policy policy}))
  (mapv (fn [t]
          (let [reply (when (and revise (= t publish-at))
                        (show/submit! rt :definition {:kind :effects
                                                      :definition (revise (get-in (state rt) [:registry :effects :orb]))}))]
            (show/tick! rt t)
            (assoc (observed rt) :time-ms t :ack (some-> reply (deref 2000 ::timeout) :status))))
        times))

(defn- differing [field control actual]
  (vec (keep (fn [[expected frame]] (when (not= (field expected) (field frame)) (:time-ms frame)))
             (map vector control actual))))

(defn- frames-match [control broken]
  (is (= (count control) (count broken)))
  (doseq [field [:dmx :spatial :physics :leases :instances]]
    (is (= [] (differing field control broken)) (str (name field) " differs at these frame times")))
  (is (every? nil? (map :error broken))))

(defn- twin-run [spawn policy revise options]
  (let [broken (runtime options) control (runtime {})]
    (try
      (let [control-frames (capture! control spawn policy nil nil)
            broken-frames (capture! broken spawn policy revise 200)]
        {:control control-frames :broken broken-frames
         :orb (some #(when (= :orb (:cue-id %)) %) (vals (:instances (state broken))))
         :events (get-in (state broken) [:variables :runtime :events])})
      (finally (show/stop! broken) (show/stop! control)))))

(defn- painted [definition]
  (first (sort (keys (:outputs definition)))))

(defn- rejected-at
  "Revise `definition` with `revise`, then make the engine reject it at `stage`."
  [revise policy stage definition]
  (let [next (-> (revise definition) (assoc :version 2)
                 (cond-> (and (= :migrate policy) (not= :migrate stage)) (assoc :state-migration {:from {1 []}})))]
    (case stage
      :reload (assoc-in next [:params :strength] {:type :number :default 2.5 :bounds [2.0 3.0]})
      :compile next
      :migrate (assoc next :state-migration {:from {1 [{:op :scale :path [:missing] :by 2}]}})
      :evaluate (-> next
                    (assoc-in [:params :divisor] {:type :number :default 0.0 :bounds [0.0 1.0]})
                    (assoc-in [:outputs (painted definition)] {:op :divide :args [(param (painted definition)) (param :divisor)]}))
      ;; A volume that aims its heads writes their aim after the paint, so
      ;; that revision is refused for adding red beyond the unit range instead.
      :output (if (get-in definition [:spatial-volume :aim-at-volume?])
                (assoc next :outputs [{:attribute :red :value (param :red)}
                                      {:attribute :red :blend :add :value {:op :const :value 0.8}}])
                (assoc-in next [:outputs :aim] {:op :const :value (:position head)})))))

(def cases
  (for [variant (sort (keys variants))
        policy [:preserve-state :restart :migrate]
        stage [:reload :compile :migrate :evaluate :output]
        :when (or (not= :migrate stage) (= :migrate policy))]
    [variant policy stage]))

(deftest a-rejected-spatial-revision-leaves-the-show-as-if-never-published
  (doseq [[variant policy stage] cases]
    (testing (str variant " " policy " rejected at " stage)
      (let [{:keys [spawn revise]} (variants variant)
            {:keys [control broken orb events]}
            (twin-run spawn policy (partial rejected-at revise policy stage)
                      (when (= :compile stage) {:failing-compile [:orb 2]}))]
        (is (= :accepted (some :ack broken)) "Publication accepts the revision; only the running engine rejects it")
        (is (= [1 stage 2] [(:effect-version orb) (get-in orb [:reload-fault :stage])
                            (get-in orb [:reload-fault :effect-version])]))
        (is (= [:effect/reload-failed] (mapv :type events)) "One bounded event")
        (frames-match control broken)))))

(defn- accepted [revise policy definition]
  (cond-> (assoc (revise definition) :version 2)
    (= :migrate policy) (assoc :state-migration {:from {1 []}})))

(def adopted
  "A spawn that renders from the start as each variant's revision does."
  {:volume (assoc-in orb [:volume :falloff] wide-falloff)
   :sources (assoc orb :sources {:inner-m 0.25 :outer-m 3.0})
   :members (assoc orb :paint {:blue 0.0} :members [{:effect-id :red}])})

(defn- dmx-from [t frames] (mapv :dmx (drop-while #(< (:time-ms %) t) frames)))

(deftest an-accepted-spatial-revision-switches-the-volume-on-its-takeover-frame
  (doseq [variant [:volume :sources] policy [:preserve-state :restart :migrate]]
    (testing (str variant " " policy)
      (let [{:keys [spawn revise]} (variants variant)
            published (runtime {}) before (runtime {}) after (runtime {})]
        (try
          (let [frames (capture! published spawn policy (partial accepted revise policy) 200)
                old (capture! before spawn policy nil nil)
                new (capture! after (adopted variant) policy nil nil)
                orb (some #(when (= :orb (:cue-id %)) %) (vals (:instances (state published))))]
            (is (= :accepted (some :ack frames)))
            (is (= [2 nil nil] [(:effect-version orb) (:reload-fault orb) (:fault orb)]))
            (is (empty? (get-in (state published) [:variables :runtime :events])))
            (is (not= (dmx-from 200 old) (dmx-from 200 new)) "The revision changes the output")
            (is (= (mapv :dmx (take-while #(< (:time-ms %) 200) old))
                   (mapv :dmx (take-while #(< (:time-ms %) 200) frames))))
            (is (= (dmx-from 200 new) (dmx-from 200 frames)) "The new volume renders from the takeover frame")
            (is (every? nil? (map :error frames))))
          (finally (show/stop! published) (show/stop! before) (show/stop! after)))))))

(deftest an-accepted-revision-replaces-its-scene-members-on-the-next-frame
  (doseq [policy [:preserve-state :restart :migrate]]
    (testing (str policy)
      (let [{:keys [spawn revise]} (variants :members)
            published (runtime {}) before (runtime {}) after (runtime {})]
        (try
          (let [frames (capture! published spawn policy (partial accepted revise policy) 200)
                old (capture! before spawn policy nil nil)
                new (capture! after (adopted :members) policy nil nil)
                members (fn [frame] (mapv :members (vals (:leases frame))))
                at (fn [t frames] (some #(when (= t (:time-ms %)) %) frames))]
            (is (= :accepted (some :ack frames)))
            (is (= (:dmx (at 200 old)) (:dmx (at 200 frames))) "The leased members render through the takeover frame")
            (is (= (members (at 200 old)) (members (at 200 frames))))
            (is (= (members (at 250 new)) (members (at 250 frames))) "The lease follows the adopted revision a frame later")
            (is (= (dmx-from 250 new) (dmx-from 250 frames)))
            (is (every? nil? (map :error frames))))
          (finally (show/stop! published) (show/stop! before) (show/stop! after)))))))

(def saw {:type :lfo :waveform :saw :period {:beats 1} :phase-reference :transport :from 0.1 :to 0.9})

(def narrowed
  "A driver revision whose schema narrows the level its bound waveform writes."
  (-> driver (assoc :version 2) (assoc-in [:params :level] {:type :number :default 0.2 :bounds [0.15 0.25]})))

(defn- linked-run
  "Launch the driver with a waveform on its level, spawn a volume whose
  strength reads that level and publish `revision` at 0 or 200 ms."
  [rt revision publish-at]
  (let [driver-id (:instance-id (host/submit rt 0 :command {:op :cue/launch :cue-id :driver
                                                           :launch {:params {:level 0.2}}}))]
    (host/submit rt 0 :modulation-bind {:instance-id driver-id :param :level :binding saw})
    (when (and revision (zero? publish-at))
      (host/submit rt 0 :definition {:kind :effects :definition revision}))
    (capture! rt (assoc orb :id :linked :sources {:strength {:cue-id :driver :param :level}})
              :preserve-state (when (and revision (pos? publish-at)) (constantly revision)) publish-at)))

(deftest a-volume-reading-a-cue-whose-revision-is-rejected-keeps-the-running-samples
  (let [broken (runtime {}) control (runtime {})
        failing (-> narrowed
                    (assoc-in [:params :divisor] {:type :number :default 0.0 :bounds [0.0 1.0]})
                    (assoc :outputs {:green {:op :divide :args [(param :level) (param :divisor)]}}))]
    (try
      (let [expected (linked-run control nil 200)
            actual (linked-run broken failing 200)
            target (some #(when (= :driver (:cue-id %)) %) (vals (:instances (state broken))))]
        (is (< 1 (count (distinct (map :dmx expected)))) "The waveform moves the volume in the twin")
        (is (= [1 :evaluate] [(:effect-version target) (get-in target [:reload-fault :stage])]))
        (frames-match expected actual))
      (finally (show/stop! broken) (show/stop! control)))))

(deftest a-volume-reading-a-cue-whose-revision-is-accepted-follows-it-on-the-takeover-frame
  (let [published (runtime {}) adopted (runtime {}) unpublished (runtime {})]
    (try
      (let [frames (linked-run published narrowed 200)
            new (linked-run adopted narrowed 0)
            old (linked-run unpublished nil 200)
            target (some #(when (= :driver (:cue-id %)) %) (vals (:instances (state published))))]
        (is (= [2 nil] [(:effect-version target) (:reload-fault target)]))
        (is (not= (dmx-from 200 old) (dmx-from 200 new)) "The narrowed level changes the output")
        (is (= (dmx-from 200 new) (dmx-from 200 frames)))
        (is (every? nil? (map :error frames))))
      (finally (show/stop! published) (show/stop! adopted) (show/stop! unpublished)))))

(defn- hdr-run
  "Light a source above unity, scale its HDR excess by the driver's
  waveform-bound level and publish `revision` at `publish-at` ms."
  [rt revision & [publish-at]]
  (let [driver-id (:instance-id (host/submit rt 0 :command {:op :cue/launch :cue-id :driver
                                                           :launch {:params {:level 0.2}}}))]
    (host/submit rt 0 :modulation-bind {:instance-id driver-id :param :level :binding saw})
    (host/submit rt 0 :command {:op :cue/launch :cue-id :hot})
    (host/submit rt 0 :settings {:scope :hdr :values {:enabled? true :gain {:cue-id :driver :param :level}
                                                      :spill 1.0 :spill-white-mix 1.0}})
    (mapv (fn [t]
            (let [reply (when (and revision (= t (or publish-at 200)))
                          (show/submit! rt :definition {:kind :effects :definition revision}))]
              (show/tick! rt t)
              (some-> reply (deref 2000 ::timeout))
              (assoc (observed rt) :time-ms t)))
          times)))

(deftest hdr-reading-a-cue-whose-revision-is-rejected-keeps-the-running-samples
  (let [broken (runtime {}) control (runtime {})
        failing (-> narrowed
                    (assoc-in [:params :divisor] {:type :number :default 0.0 :bounds [0.0 1.0]})
                    (assoc :outputs {:green {:op :divide :args [(param :level) (param :divisor)]}}))]
    (try
      (let [expected (hdr-run control nil)
            actual (hdr-run broken failing)
            target (some #(when (= :driver (:cue-id %)) %) (vals (:instances (state broken))))]
        (is (< 1 (count (distinct (map :dmx expected)))) "The waveform moves the HDR gain in the twin")
        (is (= [1 :evaluate] [(:effect-version target) (get-in target [:reload-fault :stage])]))
        (is (= [] (differing :dmx expected actual)) "Captured DMX differs at these frame times")
        (is (every? nil? (map :error actual))))
      (finally (show/stop! broken) (show/stop! control)))))

(defn- run
  "Spawn `spawns` under `policy`, then before each tick publish the revisions
  `plan` lists for that time ({t [[effect-id revise] ...]}), all in one cycle."
  [rt spawns policy plan]
  (doseq [spawn spawns] (host/submit rt 0 :spatial-cue (assoc spawn :launch {:reload-policy policy})))
  (mapv (fn [t]
          (let [replies (mapv (fn [[id revise]]
                                (show/submit! rt :definition
                                              {:kind :effects :definition (revise (get-in (state rt) [:registry :effects id]))}))
                              (get plan t))]
            (show/tick! rt t)
            (assoc (observed rt) :time-ms t :acks (mapv #(:status (deref % 2000 ::timeout)) replies))))
        times))

(defn- versions [rt ids]
  (into (sorted-map) (keep (fn [i] (when (ids (:cue-id i))
                                     [(:cue-id i) [(:effect-version i) (get-in i [:reload-fault :stage])]])))
        (vals (:instances (state rt)))))

(def orb-a (assoc orb :id :orb-a :cell [2 1]))

(def orb-b (assoc orb :id :orb-b :cell [3 1] :volume (assoc (:volume orb) :position [3 1 0])))

(deftest a-rejected-and-an-accepted-spatial-revision-in-one-frame
  ;; The engine defers every candidate of a frame that rejects one, so the
  ;; accepted revision takes over a frame later, as in a twin publishing it then.
  (doseq [variant [:volume :paint] stage [:compile :evaluate :output] policy [:preserve-state :restart]]
    (testing (str variant " rejected at " stage " " policy)
      (let [revise (get-in variants [variant :revise])
            ;; The renderer refusal aims the head at the first orb's centre.
            [good bad] (if (= :output stage) [:orb-b :orb-a] [:orb-a :orb-b])
            mixed (runtime (when (= :compile stage) {:failing-compile [bad 2]})) late (runtime {}) never (runtime {})]
        (try
          (let [frames (run mixed [orb-a orb-b] policy {200 [[good (partial accepted revise policy)]
                                                             [bad (partial rejected-at revise policy stage)]]})
                expected (run late [orb-a orb-b] policy {250 [[good (partial accepted revise policy)]]})
                unpublished (run never [orb-a orb-b] policy {})]
            (is (= {good [2 nil] bad [1 stage]} (versions mixed #{:orb-a :orb-b})))
            (is (not= (map :dmx expected) (map :dmx unpublished)) "The accepted revision is visible")
            (frames-match expected frames))
          (finally (show/stop! mixed) (show/stop! late) (show/stop! never)))))))

(deftest a-rejected-revision-that-leaves-the-spatial-group-keeps-its-group-level-once
  ;; The revision drops the volume, so the frame that accepts it would apply
  ;; the scene member's group level in the engine instead of in the spatial
  ;; member mix. (The halved aim of the output-stage revision is not
  ;; zero-distance, so that stage is omitted.)
  (doseq [stage [:compile :evaluate]]
    (testing (str stage)
      (let [revise #(dissoc % :spatial-volume)
            broken (runtime (when (= :compile stage) {:failing-compile [:orb 2]})) control (runtime {})
            grouped (fn [rt publish]
                      (host/submit rt 0 :command {:op :effect-groups/configure :config {:manual [{:id :mix :cue-ids [:blue]}]}})
                      (host/submit rt 0 :command {:op :effect-groups/level-set :channel-id [:manual :mix] :value 0.5})
                      (run rt [(get-in variants [:members :spawn])] :preserve-state (if publish {200 [[:orb (partial rejected-at revise :preserve-state stage)]]} {})))]
        (try
          (let [expected (grouped control false)
                actual (grouped broken true)]
            (is (= [:accepted] (some #(not-empty (:acks %)) actual)))
            (is (= {:orb [1 stage]} (versions broken #{:orb})))
            (is (= [0 0 128] (subvec (:dmx (first expected)) 0 3)) "The group level halves the scene member")
            (frames-match expected actual))
          (finally (show/stop! broken) (show/stop! control)))))))

(deftest a-volume-linked-to-a-rejected-volume-keeps-its-committed-field-next-frame
  ;; A volume reading another volume's sourced parameter reads last frame's
  ;; committed field, which must be the running revision's after a rejection.
  (let [leader (assoc orb :sources {:strength 0.4})
        follower (assoc orb-b :id :follower :sources {:strength {:cue-id :orb :param :strength}})
        revise #(assoc-in % [:spatial-volume :sources] {:strength 0.9})
        broken (runtime {}) control (runtime {})]
    (try
      (let [expected (run control [leader follower] :preserve-state {})
            actual (run broken [leader follower] :preserve-state {200 [[:orb (partial rejected-at revise :preserve-state :evaluate)]]})]
        (is (= {:orb [1 :evaluate]} (versions broken #{:orb})))
        (frames-match expected actual))
      (finally (show/stop! broken) (show/stop! control)))))

(deftest an-accepted-revision-publishes-its-scene-view-on-the-takeover-frame
  (doseq [variant [:volume :paint :aim]]
    (testing (str variant)
      (let [{:keys [spawn revise]} (variants variant)
            published (runtime {}) adopted (runtime {})]
        (try
          (let [frames (capture! published spawn :preserve-state (partial accepted revise :preserve-state) 200)
                new (capture! adopted spawn :preserve-state nil nil)
                ;; The adopted twin publishes at 0, before any frame renders.
                new (do (show/stop! adopted)
                        (let [rt (runtime {})]
                          (try (host/submit rt 0 :spatial-cue (assoc spawn :launch {:reload-policy :preserve-state}))
                               (host/submit rt 0 :definition {:kind :effects
                                                              :definition (accepted revise :preserve-state
                                                                                    (get-in (state rt) [:registry :effects :orb]))})
                               (mapv (fn [t] (show/tick! rt t) (assoc (observed rt) :time-ms t)) times)
                               (finally (show/stop! rt)))))
                scene (fn [frame] (-> (:spatial frame)
                                      (update :signals update-vals #(select-keys % [:volume :paint :affected-head-count :error]))
                                      (select-keys [:signals :members])))]
            (is (= (dmx-from 200 new) (dmx-from 200 frames)))
            (is (= (map scene (drop-while #(< (:time-ms %) 200) new))
                   (map scene (drop-while #(< (:time-ms %) 200) frames)))
                "The scene view shows the field the fixtures render from the takeover frame"))
          (finally (show/stop! published) (show/stop! adopted)))))))

(deftest hdr-reading-a-cue-whose-revision-is-accepted-follows-it-on-the-takeover-frame
  (let [published (runtime {}) adopted (runtime {}) unpublished (runtime {})]
    (try
      (let [frames (hdr-run published narrowed)
            ;; Publishing at 0 adopts the revision before the first measured frame.
            new (with-redefs [times (vec (cons 0 times))] (hdr-run adopted narrowed 0))
            new (vec (rest new))
            old (hdr-run unpublished nil)]
        (is (not= (dmx-from 200 old) (dmx-from 200 new)) "The narrowed level changes the HDR output")
        (is (= (dmx-from 200 new) (dmx-from 200 frames)))
        (is (every? nil? (map :error frames))))
      (finally (show/stop! published) (show/stop! adopted) (show/stop! unpublished)))))

(defn- admitted-invalid-volumes
  "The :spatial/invalid-volume events the owner committed to its subscribers."
  [rt]
  (filterv #(= :spatial/invalid-volume (:type (:event % %))) @(::admitted rt)))

(defn- invalid-volume-events [frames]
  (vec (for [frame frames, event (get-in frame [:spatial :events])
             :when (= :spatial/invalid-volume (:type event))]
         [(:time-ms frame) (:volume-id event)])))

(deftest an-invalid-volume-is-reported-once-whichever-revision-the-engine-keeps
  ;; The revision drops the ball's radius parameter, which leaves its volume
  ;; without a radius inside the small room.
  (let [revise #(update % :params dissoc :radius)
        room (fn [rt] (host/settings rt 0 {:bounds {:min [-1 0 -1] :max [1 2 1]}}))]
    (testing "accepted"
      (let [rt (runtime {})]
        (try
          (room rt)
          (let [frames (capture! rt orb :preserve-state (partial accepted revise :preserve-state) 200)
                key (cues/key-for (some #(when (= :orb (:cue-id %)) %) (vals (:instances (state rt)))))]
            (is (= [[200 key]] (invalid-volume-events frames)) "One event, on the takeover frame")
            (is (= 1 (count (admitted-invalid-volumes rt))) "Subscribers receive it once")
            (is (string? (get-in (peek frames) [:spatial :signals key :error])))
            (is (every? nil? (map :error frames))))
          (finally (show/stop! rt)))))
    (testing "rejected"
      (let [broken (runtime {}) control (runtime {})]
        (try
          (room broken) (room control)
          (let [expected (capture! control orb :preserve-state nil nil)
                actual (capture! broken orb :preserve-state (partial rejected-at revise :preserve-state :evaluate) 200)]
            (is (= [] (invalid-volume-events actual)))
            (is (= [] (admitted-invalid-volumes broken)))
            (frames-match expected actual))
          (finally (show/stop! broken) (show/stop! control)))))
    (testing "an ordinary edit"
      (let [rt (runtime {})]
        (try
          (room rt)
          (let [frames (capture! rt orb :preserve-state nil nil)
                id (some #(when (= :orb (:cue-id %)) (:id %)) (vals (:instances (state rt))))
                _ (host/settings rt 600 {:bounds {:min [0 0 0] :max [0.6 0.6 0.6]}})
                settled (assoc (observed rt) :time-ms 600)
                later (mapv (fn [t] (show/tick! rt t) (assoc (observed rt) :time-ms t)) [650 700])]
            (is (= [] (invalid-volume-events frames)))
            (is (some? id))
            (is (= 1 (count (invalid-volume-events (into [settled] later)))))
            (is (= 1 (count (admitted-invalid-volumes rt)))))
          (finally (show/stop! rt)))))))

(deftest a-spatial-revision-that-compiles-paints-its-volume-only-when-adopted
  ;; The orb's wider volume compiles off the frame (#774) while the spatial
  ;; pass runs. It must keep painting the volume the engine runs: every frame
  ;; equals a twin that publishes the revision one frame later.
  (let [run (fn [publish-at]
              (let [rt (runtime {})]
                (try
                  (host/submit rt 0 :spatial-cue (assoc orb :launch {:reload-policy :preserve-state}))
                  (let [wide (accepted (:revise (variants :volume)) :preserve-state
                                       (get-in (state rt) [:registry :effects :orb]))]
                    (mapv (fn [t]
                            (let [reply (when (= t publish-at)
                                          (show/submit! rt :definition {:kind :effects :definition wide}))]
                              (show/tick! rt t)
                              (assoc (observed rt) :time-ms t :ack (some-> reply (deref 2000 ::timeout) :status))))
                          times))
                  (finally (show/stop! rt)))))
        ;; A worker that finishes each compile before the frame after it is queued.
        queued (atom [])
        work! (fn [] (let [tasks @queued] (reset! queued []) (doseq [task tasks] (task))))
        waiting (let [rt (runtime {:background-compile (fn [task] (swap! queued conj task))})]
                  (try
                    (host/submit rt 0 :spatial-cue (assoc orb :launch {:reload-policy :preserve-state}))
                    (work!)
                    (let [wide (accepted (:revise (variants :volume)) :preserve-state
                                         (get-in (state rt) [:registry :effects :orb]))]
                      (mapv (fn [t]
                              (work!)
                              (let [reply (when (= t 200) (show/submit! rt :definition {:kind :effects :definition wide}))]
                                (show/tick! rt t)
                                (assoc (observed rt) :time-ms t :ack (some-> reply (deref 2000 ::timeout) :status))))
                            times))
                    (finally (show/stop! rt))))
        later (run 250)
        orb-version (fn [frames t] (some (fn [frame] (when (= t (:time-ms frame))
                                                       (some (fn [[_ [cue _ version]]] (when (= :orb cue) version))
                                                             (:instances frame))))
                                         frames))]
    (is (= :accepted (:ack (nth waiting 3))))
    (is (= [1 2] [(orb-version waiting 200) (orb-version waiting 250)]) "The orb's revision waits one frame for its compile")
    (is (not= (:dmx (nth later 3)) (:dmx (nth later 4))) "The wider volume changes the captured output")
    (frames-match later waiting)))
