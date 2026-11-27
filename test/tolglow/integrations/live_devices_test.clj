(ns tolglow.integrations.live-devices-test
  "One Max for Live parameter device per effect on its run channel (#625).
  The device's slots read the same control projection as Studio's parameter
  rows; run clips carry a take's values as envelopes of those slots; Live
  automation playback and a performer's turns come back as commands with
  their origin, and Tolglow's own writes go to Live once without feeding
  back. A real show runtime plays against the simulated Set
  (tolglow.integrations.fake-live); no Live, Max or device is used."
  (:require [afterglow.control.model :as control]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [tolglow.authoring.parameter-controls :as controls]
            [tolglow.integrations.fake-live :as fake]
            [tolglow.integrations.live-connection :as live]
            [tolglow.integrations.live-devices :as devices]
            [tolglow.integrations.live-runs :as runs]
            [tolglow.show :as show]
            [tolglow.studio :as studio])
  (:import [java.io ByteArrayInputStream]))

(def effect
  {:id :beam :version 1 :category :light
   :params {:level {:type :number :default 0.5 :bounds [0.0 1.0] :name "Level"}
            :period {:type :number :default 2.0 :bounds [0.5 32.0] :unit :beats}
            :steps {:type :integer :default 4 :bounds [1 16]}
            :mode {:type :enum :default :soft :values [:soft :hard :strobe]}
            :on {:type :boolean :default true}
            :tint {:type :color :default [1.0 0.5 0.0]}
            :aim {:type :vector :size 3 :default [0.0 0.0 1.0] :bounds [-1.0 1.0]}
            :band {:type :range :default [0.25 0.75] :bounds [0.0 1.0]}
            :fade {:type :gradient :default [{:position 0.0 :color [0.0 0.0 0.0]}]}
            :drift {:type :number :default 0.0}}
   :outputs {:dimmer {:op :param :key :level}}})

(def registry
  {:effects {:beam effect}
   :cues {:a {:id :a :effect-id :beam :version 1 :name "Beam"}}})

(defn- model-with [registry]
  (control/new-show {:show-id :devices :transport {:playing? true :beat 0.0 :bpm 120.0}
                     :registry registry
                     ;; A 16-bit dimmer: Studio shows five decimals for :level.
                     :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer [1 2]}}]}}))

(defn- slot-of [spec param component]
  (some #(when (and (= param (:param %)) (= component (:component %))) %) (:slots spec)))

(deftest the-device-reads-studios-control-projection
  (let [model (model-with registry)
        layout (devices/allocate nil (:keys (devices/slot-keys effect {} nil)))
        spec (devices/device-spec model :a layout nil)]
    (testing "a scalar slot has Studio's label, unit and decimals for this patch"
      (doseq [param [:level :period :steps]
              :let [schema (get-in effect [:params param])
                    annotated (cond-> schema (= :level param) (assoc :dmx-bits 16))
                    control (controls/control param annotated nil)
                    slot (slot-of spec param nil)]]
        (is (= (:label control) (:name slot)) param)
        (is (= (:unit control) (:unit slot)) param)
        (is (= (:places control) (:places slot)) param))
      (is (= 5 (:places (slot-of spec :level nil))) "16-bit dimmer: adjacent DMX codes stay distinguishable")
      (is (= {:type :float :min 0.5 :max 32.0 :unit "beats" :widget :rotary}
             (select-keys (slot-of spec :period nil) [:type :min :max :unit :widget])))
      (is (= {:type :int :min 1 :max 16 :steps 16} (select-keys (slot-of spec :steps nil) [:type :min :max :steps]))))
    (testing "choices and toggles are enumerations with Studio's labels"
      (is (= {:type :enum :choices ["soft" "hard" "strobe"] :widget :menu}
             (select-keys (slot-of spec :mode nil) [:type :choices :widget])))
      (is (= ["Off" "On"] (:choices (slot-of spec :on nil)))))
    (testing "compound values expose each component reversibly"
      (is (= ["tint hue" "tint saturation" "tint brightness"] (mapv #(:name (slot-of spec :tint %)) [0 1 2])))
      (is (= ["aim X" "aim Y" "aim Z"] (mapv #(:name (slot-of spec :aim %)) [0 1 2])))
      (is (= ["band low" "band high"] (mapv #(:name (slot-of spec :band %)) [0 1]))))
    (testing "unsupported parameters are listed, not dropped silently"
      (is (= #{{:param :fade :reason :gradient} {:param :drift :reason :unbounded}} (set (:unsupported spec)))))
    (testing "banks of eight follow Studio's parameter order"
      (is (= 1 (:devices spec)))
      (is (= [8 5] (mapv count (first (:banks spec))))))))

(deftest slots-stay-with-their-parameters-across-revisions
  (let [v1 (devices/allocate nil (:keys (devices/slot-keys effect {} nil)))
        level (get-in v1 [:slots [:level nil]])
        v2-effect (-> effect (update :params dissoc :period) (assoc-in [:params :gain] {:type :number :bounds [0.0 2.0]}))
        v2 (devices/allocate v1 (:keys (devices/slot-keys v2-effect {} nil)))
        v3 (devices/allocate v2 (:keys (devices/slot-keys effect {} nil)))
        model (model-with (assoc-in registry [:effects :beam] (assoc v2-effect :version 2)))
        spec (devices/device-spec model :a v2 nil)]
    (is (= level (get-in v2 [:slots [:level nil]]) (get-in v3 [:slots [:level nil]])) "recorded automation stays on its parameter")
    (is (= (:next v1) (get-in v2 [:slots [:gain nil]])) "a new parameter takes the next free slot")
    (is (= (get-in v1 [:slots [:period nil]]) (get-in v3 [:slots [:period nil]])) "a parameter re-added keeps its old slot")
    (is (:retired? (slot-of spec :period nil)) "a removed parameter's slot is shown as unused, not reused")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"256"
                          (devices/allocate nil (for [i (range 257)] [(keyword (str "p" i)) nil]))))))

(deftest native-values-round-trip-through-each-control-shape
  (let [schema #(get-in effect [:params %])
        round (fn [param current component value]
                (let [native (devices/native-value (schema param) value component nil)]
                  (devices/typed-value (schema param) current component native nil)))]
    (is (= 0.25 (round :level 0.5 nil 0.25)))
    (is (= 1.0 (devices/typed-value (schema :level) 0.5 nil 7.0 nil)) "clamped to the bounds")
    (is (= 12 (round :steps 4 nil 12)))
    (is (= :strobe (round :mode :soft nil :strobe)))
    (is (= 2 (devices/native-value (schema :mode) :strobe nil nil)))
    (is (= false (round :on true nil false)))
    (is (= [0.0 0.5 -0.25] (devices/typed-value (schema :aim) [0.0 0.0 -0.25] 1 0.5 nil)))
    (is (= [0.5 0.5] (devices/typed-value (schema :band) [0.25 0.5] 0 0.9 nil)) "range endpoints never cross")
    (testing "a colour round-trips through hue, saturation and brightness"
      (let [rgb [0.2 0.6 0.4]
            natives (mapv #(devices/native-value (schema :tint) rgb % nil) [0 1 2])
            back (devices/color-value (schema :tint) natives)]
        (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-9) rgb back)) (pr-str back))
        (is (= [0.0 0.0 0.0] (devices/color-value (schema :tint) [0.3 1.0 0.0])))))))

(deftest a-take-becomes-envelope-points-on-its-slots
  (let [model (model-with registry)
        layout (devices/allocate nil (:keys (devices/slot-keys effect {} nil)))
        spec (devices/device-spec model :a layout nil)
        take {:cue-id :a :params {:level 0.5}
              :moves [{:beat 3 :at 1 :param :level :value 0.75}
                      {:beat 4 :at 2 :param :aim :value [0.0 0.5 1.0]}
                      {:beat 9 :at 7 :param :level :value 0.1}]}
        envelopes (devices/take-envelopes model spec layout take 4 nil)
        by-slot (into {} (map (juxt :slot :points)) envelopes)]
    (is (= [[0 0.5] [1 0.75]] (by-slot (:slot (slot-of spec :level nil)))) "launch value at 0, moves at held offsets, none past the hold")
    (is (= [[0 0.5]] (by-slot (:slot (slot-of spec :aim 1)))) "a slot without a launch value starts at its first move")
    (is (= 3 (count (filter #(= :aim (:param (slot-of spec :aim (:component %)))) (map #(some (fn [s] (when (= (:slot s) (:slot %)) s)) (:slots spec)) envelopes)))))))

(deftest ownership-echo-and-holds
  (let [model (model-with registry)
        layout (devices/allocate nil (:keys (devices/slot-keys effect {} nil)))
        slot (get-in layout [:slots [:level nil]])
        record {:lanes {[:a 0] "r.1"} :open {"i1" "r.1"} :takes {"r.1" {:instance-id "i1" :cue-id :a :lane 0}}}
        model (assoc-in model [:instances "i1"] {:id "i1" :cue-id :a :params {:level 0.5}})
        ctx {:model model :record record :mirror {:tracks {[:lane :a 0] "track:1"}} :player {:playing {}}
             :envelope-fn (fn [] {:command-id "c"}) :catalog nil}
        state (assoc-in (devices/new-state) [:layouts :a] layout)
        value (fn [v automation serial] {:type :parameter-device-value :track-id "track:1" :slot slot :value v
                                         :automation-state automation :watch-session "w" :serial serial})
        step (fn [state events] (devices/ingest state ctx events))]
    (testing "automation playback and a performer's turn carry their origins"
      (let [{:keys [commands]} (step state [(value 0.75 1 1)])]
        (is (= [{:op :param/set :instance-id "i1" :param :level :value 0.75}]
               (mapv #(select-keys % [:op :instance-id :param :value]) commands)))
        (is (= :live-playback (get-in commands [0 :source :origin]))))
      (is (= :live-input (get-in (step state [(value 0.3 0 1)]) [:commands 0 :source :origin]))))
    (testing "one command per parameter: the last value wins, a repeated serial is dropped"
      (let [r (step state [(value 0.1 0 1) (value 0.2 0 2) (value 0.9 0 2)])]
        (is (= [0.2] (mapv :value (:commands r))))
        (is (= 1 (get-in r [:state :counts :repeat])))))
    (testing "a Tolglow write is mirrored once, its echo dropped, and Live automation held"
      (let [history {:history {"s1" {:command {:op :param/set :instance-id "i1" :param :level :value 0.25
                                               :source {:adapter :studio :origin nil}}
                                     :ack {:status :accepted}}
                               "s2" {:command {:op :param/set :instance-id "i1" :param :level :value 0.4
                                               :source {:adapter :live-device :origin :live-input}}
                                     :ack {:status :accepted}}}
                     :history-order ["s1" "s2"]}
            {:keys [state writes]} (devices/mirror-writes state (assoc-in ctx [:model :control] history))]
        (is (= [{:track-id "track:1" :cue-id :a :lane 0 :values [{:slot slot :device 0 :index slot :value 0.25 :seq 1}]}] writes)
            "only the Studio write; the value from Live is not written back")
        (is (empty? (:writes (devices/mirror-writes state (assoc-in ctx [:model :control] history)))) "each write is sent once")
        (let [r (step state [(value 0.25 2 1)])]
          (is (empty? (:commands r)) "Live's report of Tolglow's own value is an echo")
          (is (= 1 (get-in r [:state :counts :echo])))
          (let [held (step state [(value 0.8 1 2)])]
            (is (empty? (:commands held)) "automation is held while Tolglow owns the slot")
            (is (= 1 (get-in held [:state :counts :held]))))
          (let [resumed (step (:state (step (:state r) [(value 0.25 2 3)])) [(value 0.9 1 4)])]
            (is (= [0.9] (mapv :value (:commands resumed))) "re-enabled automation owns the slot again"))
          (let [restarted (step (:state r) [{:type :clip-started :track-id "track:1"} (value 0.6 1 5)])]
            (is (= [0.6] (mapv :value (:commands restarted))) "a run clip starting hands the slot back")))))
    (testing "values for an unknown track or slot, or with no running instance, are counted and dropped"
      (let [r (step state [(assoc (value 0.1 0 1) :track-id "track:9") (assoc (value 0.1 0 2) :slot 200)])]
        (is (empty? (:commands r)))
        (is (= {:unknown-track 1 :unknown-slot 1} (select-keys (get-in r [:state :counts]) [:unknown-track :unknown-slot]))))
      (is (= 1 (get-in (devices/ingest state (assoc ctx :record {}) [(value 0.1 0 1)]) [:state :counts :no-instance]))))))

;; Review of #769: fixtures shared by the regressions below.
(defn- owned [& {:keys [effect-def instance-def player record] :or {effect-def effect}}]
  (let [model (model-with (assoc-in registry [:effects :beam] effect-def))
        layout (devices/allocate nil (:keys (devices/slot-keys effect {} nil)))
        model (assoc-in model [:instances "i1"] (cond-> {:id "i1" :cue-id :a :params {:level 0.5 :aim [0.0 0.0 1.0] :tint [1.0 0.0 0.0]}}
                                                   instance-def (assoc :definition instance-def)))]
    {:layout layout
     :ctx {:model model :mirror {:tracks {[:lane :a 0] "track:1"}} :player (or player {:playing {}})
           :record (or record {:lanes {[:a 0] "r.1"} :open {"i1" "r.1"} :takes {"r.1" {:instance-id "i1" :cue-id :a :lane 0}}})
           :envelope-fn (fn [] {:command-id "c"}) :catalog nil :now-ms 1000}
     :state (assoc-in (devices/new-state) [:layouts :a] layout)}))

(defn- writes-of [state ctx values]
  (let [history (into {} (map-indexed (fn [i v] [(str "w" i) {:command {:op :param/set :instance-id "i1" :param :level :value v
                                                                       :source {:adapter :studio}}
                                                             :ack {:status :accepted}}])
                                      values))]
    (devices/mirror-writes state (assoc-in ctx [:model :control] {:history history :history-order (vec (sort (keys history)))}))))

(deftest every-outstanding-write-is-recognised-as-its-echo
  (let [{:keys [layout ctx state]} (owned)
        slot (get-in layout [:slots [:level nil]])
        value (fn [v serial] {:type :parameter-device-value :track-id "track:1" :slot slot :value v
                              :automation-state 0 :watch-session "w" :serial serial})
        ;; A drag: two owner cycles write 0.3 and 0.4 before Live echoes either.
        state (:state (writes-of state ctx [0.3]))
        state (:state (writes-of state ctx [0.3 0.4]))]
    (is (= [0.3 0.4] (mapv :value (get-in state [:echo ["track:1" slot]]))))
    (testing "two writes, then both echoes in order"
      (let [r (devices/ingest state ctx [(value 0.3 1) (value 0.4 2)])]
        (is (empty? (:commands r)) "neither echo is a performer move")
        (is (= 2 (get-in r [:state :counts :echo])))
        (is (empty? (get-in r [:state :echo])))))
    (testing "three writes, echoes out of order: a newer echo also retires the older writes"
      (let [state (:state (writes-of state ctx [0.3 0.4 0.5]))
            r (devices/ingest state ctx [(value 0.5 1) (value 0.3 2)])]
        (is (= [0.3 0.4 0.5] (mapv :value (get-in state [:echo ["track:1" slot]]))))
        (is (= 1 (get-in r [:state :counts :echo])))
        (is (= [0.3] (mapv :value (:commands r))) "a value after every write was echoed is Live's own")))
    (testing "an echo that never comes back expires; a failed send forgets only its own write"
      (let [later (devices/ingest state (assoc ctx :now-ms (+ 1000 devices/echo-timeout-ms 1)) [(value 0.3 1)])]
        (is (= [0.3] (mapv :value (:commands later)))))
      (let [[a b] (get-in state [:echo ["track:1" slot]])
            forgotten (devices/forget-writes state "track:1" {slot [(:seq a)]})]
        (is (= [b] (get-in forgotten [:echo ["track:1" slot]])))))
    (testing "an automation-state change alone moves nothing"
      (let [r (devices/ingest state ctx [(assoc (value 0.9 1) :value-changed? false :automation-state 2)])]
        (is (empty? (:commands r)))
        (is (= 1 (get-in r [:state :counts :state-only])))))))

(deftest retired-slots-never-reach-a-reshaped-parameter
  (testing "Level became a colour: old Level automation is dropped, and the batch survives"
    (let [colour-level (assoc-in effect [:params :level] {:type :color :default [1.0 1.0 1.0]})
          {:keys [layout ctx state]} (owned :effect-def (assoc colour-level :version 2))
          level (get-in layout [:slots [:level nil]])
          tint (get-in layout [:slots [:tint 0]])
          ctx (assoc-in ctx [:model :instances "i1" :params :level] [1.0 1.0 1.0])
          r (devices/ingest state ctx [{:type :parameter-device-value :track-id "track:1" :slot level :value 0.7 :automation-state 1}
                                       {:type :parameter-device-value :track-id "track:1" :slot tint :value 0.5 :automation-state 1}])]
      (is (= 1 (get-in r [:state :counts :retired])))
      (is (= [:tint] (mapv :param (:commands r))) "the other slot's value still applies")))
  (testing "aim shrank to two components: its third slot is retired"
    (let [two (assoc-in effect [:params :aim] {:type :vector :size 2 :default [0.0 0.0] :bounds [-1.0 1.0]})
          {:keys [layout ctx state]} (owned :effect-def (assoc two :version 2))
          z (get-in layout [:slots [:aim 2]])
          ctx (assoc-in ctx [:model :instances "i1" :params :aim] [0.0 0.0])
          r (devices/ingest state ctx [{:type :parameter-device-value :track-id "track:1" :slot z :value 0.9}])]
      (is (empty? (:commands r)))
      (is (= 1 (get-in r [:state :counts :retired])))))
  (testing "a running instance pinned to its own definition decodes through it"
    (let [{:keys [layout ctx state]} (owned :instance-def (assoc-in effect [:params :level :bounds] [0.0 0.5]))
          r (devices/ingest state ctx [{:type :parameter-device-value :track-id "track:1"
                                        :slot (get-in layout [:slots [:level nil]]) :value 0.9}])]
      (is (= [0.5] (mapv :value (:commands r))) "clamped to the instance's own bounds"))))

(deftest a-pinned-cue-gets-the-spec-of-the-revision-it-launches
  (let [v2 (-> effect (assoc :version 2) (assoc-in [:params :level :bounds] [0.0 100.0]))
        reg (-> registry (assoc-in [:effects :beam] v2) (assoc-in [:versions [:effects :beam 1]] effect)
                (assoc-in [:cues :a :effect-version] 1))
        model (model-with reg)
        layout (devices/allocate nil (:keys (devices/slot-keys effect {} nil)))
        spec (devices/device-spec model :a layout nil)]
    (is (= 1 (:effect-version spec)))
    (is (= [0.0 1.0] ((juxt :min :max) (slot-of spec :level nil))))))

(deftest values-wait-for-the-replay-they-belong-to
  (let [{:keys [layout ctx state]} (owned :player {:playing {"track:1" {:take-id "r.0" :command-id "launch-1"}}})
        slot (get-in layout [:slots [:level nil]])
        early (devices/ingest state ctx [{:type :parameter-device-value :track-id "track:1" :slot slot :value 0.8
                                          :automation-state 1 :watch-session "w" :serial 1}])]
    (is (empty? (:commands early)) "not the lane's open run: the replay has not launched yet")
    (is (devices/waiting? (:state early)))
    (let [model (assoc-in (:model ctx) [:instances "i2"] {:id "i2" :cue-id :a :params {:level 0.1}})
          bound (devices/ingest (:state early) (assoc ctx :model model :player {:playing {"track:1" {:instance-id "i2"}}}) [])]
      (is (= [["i2" 0.8 :live-playback]] (mapv (juxt :instance-id :value (comp :origin :source)) (:commands bound)))
          "the held envelope value applies to the replay once it exists")
      (is (not (devices/waiting? (:state bound)))))
    (let [stopped (devices/ingest (:state early) (assoc ctx :player {:playing {}}) [])]
      (is (empty? (:commands stopped)))
      (is (= 1 (get-in stopped [:state :counts :waiting-dropped])) "a clip stopped before its launch drops its values"))))

(deftest a-colour-changed-in-tolglow-is-not-overwritten-by-a-stale-live-hue
  (let [{:keys [layout ctx state]} (owned)
        hue (get-in layout [:slots [:tint 0]])
        turn (fn [state ctx v] (devices/ingest state ctx [{:type :parameter-device-value :track-id "track:1" :slot hue :value v}]))
        first-turn (turn state ctx 0.3)
        studio {:history {"c1" {:command {:op :param/set :instance-id "i1" :param :tint :value [0.0 0.0 0.25] :source {:adapter :studio}}
                                :ack {:status :accepted}}}
                :history-order ["c1"]}
        after-studio (:state (devices/mirror-writes (:state first-turn) (assoc-in ctx [:model :control] studio)))
        ctx2 (assoc-in ctx [:model :instances "i1" :params :tint] [0.0 0.0 0.25])
        r (turn after-studio ctx2 0.6)]
    (is (nil? (get-in after-studio [:hsv ["i1" :tint]])) "a Tolglow colour write drops the cached components")
    (is (< (Math/abs (- 0.25 (reduce max (:value (first (:commands r)))))) 1e-9) "Studio's brightness is kept")
    (is (empty? (:hsv (:state (turn (:state first-turn) (update ctx :model update :instances dissoc "i1") 0.4)))))
    "ended instances leave no cached colour"))

(deftest an-unknown-choice-is-not-shown-as-the-first
  (is (nil? (devices/native-value (get-in effect [:params :mode]) :nonexistent nil nil))))

;; ---------------------------------------------------------------------------
;; A real runtime against the simulated Set

(defn- request
  ([app uri] (request app :get uri nil))
  ([app method uri body]
   (app {:request-method method :uri uri :headers {"host" "localhost" "content-type" "application/json"}
         :body (ByteArrayInputStream. (.getBytes (if body (json/write-str body) "") "UTF-8"))})))

(defn- wait-for [pred]
  ;; A bounded wait: generous so a loaded runner is not a failure.
  (loop [n 0] (cond (pred) true (> n 2000) false :else (do (Thread/sleep 5) (recur (inc n))))))

(deftest run-clips-carry-parameter-moves-and-live-automation-steers-the-show
  (let [sent (atom [])
        rt (show/create-runtime
            {:model (model-with registry) :sink {:send! (fn [_ _])} :workspace-options nil :phrase-capture false
             :live-runs {:record-id "p"
                         :device-send! (fn [payload] (swap! sent conj payload) {:status :executed :result {:written (count (:values payload))}})}})
        app (studio/handler rt)
        tick! (fn [beat] (show/tick! rt (long (* 500 beat))))
        submit! (fn [beat payload]
                  (let [reply (show/submit! rt :command payload)]
                    (tick! beat)
                    (deref reply 1000 ::missing)))
        push {:adapter :push :session-id "pads"}
        level-slot #(get-in (show/live-parameter-devices rt) [:channels 0 :slots])]
    (try
      (tick! 0)
      (let [a (:instance-id (submit! 1 {:op :cue/launch :cue-id :a :source push :launch {:mode :persistent :params {:level 0.5}}}))]
        (submit! 2 {:op :param/set :instance-id a :param :level :value 0.75 :source push})
        (submit! 4 {:op :instance/end :instance-id a :source push :force? true})
        (tick! 5))
      (let [ls (fake/create {:companion? false :tracks [{:name "Tolglow run a"}]})
            device-id (fake/add-parameter-device! ls "Tolglow run a")
            result (show/sync-live-runs! rt (live/open (fake/transport ls) {}))
            projection (show/live-parameter-devices rt)
            slots (get-in projection [:channels 0 :slots])
            level (some #(when (= :level (:param %)) %) slots)
            track-id (get-in result [:tracks [:lane :a 0]])
            clip-id (get-in result [:clips "p.1" :clip-id])
            clip (some #(when (= clip-id (:clip-id %)) %) (vals (:slots (first (:tracks @ls)))))]
        (testing "sync configures the device and writes the take's values into its run clip"
          (is (:complete? result) (pr-str (:problems result)))
          (is (= {[:a 0] {:present [0] :missing [] :stale []}} (update-vals (:parameter-devices result) #(dissoc % :devices))))
          (is (= (mapv :name slots) (mapv :name (take (count slots) (:parameters (fake/parameter-device ls "Tolglow run a" 0)))))
              "the read-back names are the spec's")
          (is (= {[device-id (:index level)] [[0 0.5] [1 0.75]]} (select-keys (:envelopes clip) [[device-id (:index level)]]))
              "launch value at the clip start, the Push move one beat in, in Level's own units")
          (is (= {:status :executed :result {:watching 1 :watching-slots 32}} (:watch result)))
          (is (= [track-id] (:watched @ls))))
        (testing "Studio reads the REPL's projection"
          (let [ring (json/read-str (:body (request app "/api/live-parameter-devices")) :key-fn keyword)]
            (is (= (mapv :name slots) (mapv :name (get-in ring [:channels 0 :slots]))))
            (is (= [{:lane 0 :track-id track-id :track-name "Tolglow run a" :present [0] :missing [] :stale []}]
                   (get-in ring [:channels 0 :tracks])))))
        (testing "playing the clip launches the take; its moves come from Live automation, not a replay"
          (tick! 10)
          (show/live-run-clip! rt {:type :clip-started :track-id track-id :clip-id clip-id :watch-session "w" :serial 1})
          (tick! 10)
          (tick! 10)
          (let [[instance] (vals (:instances (:model @(:state rt))))]
            (is (= 0.5 (get-in instance [:params :level])))
            (tick! 11.25) (tick! 11.5)
            (is (= 0.5 (get-in (first (vals (:instances (:model @(:state rt))))) [:params :level])) "the player did not replay the move")
            (show/live-parameter-device-value! rt {:type :parameter-device-value :track-id track-id :slot (:slot level)
                                                   :value 0.75 :automation-state 1 :watch-session "w" :serial 2})
            (tick! 11.75) (tick! 12)
            (is (= 0.75 (get-in @(:state rt) [:model :instances (:id instance) :params :level])) "Live's envelope moved Level")
            (is (= 1 (count (runs/takes (show/live-runs-record rt)))) "playback made no take")
            (testing "a Studio move is mirrored into Live once and Live's echo is dropped"
              (submit! 12.25 {:op :param/set :instance-id (:id instance) :param :level :value 0.2 :source {:adapter :studio :session-id "b"}})
              (tick! 12.5)
              (is (wait-for #(seq @sent)))
              (is (= [{:track-id track-id :expected-name "Tolglow run a" :values [{:device 0 :index (:index level) :value 0.2}]}] @sent)
                  "the payload names the device slot; sequence numbers stay on the host")
              (show/live-parameter-device-value! rt {:type :parameter-device-value :track-id track-id :slot (:slot level)
                                                     :value 0.2 :automation-state 2 :watch-session "w" :serial 3})
              (tick! 12.6) (tick! 12.7)
              (is (= 1 (get-in (show/live-parameter-devices rt) [:counts :echo])))
              (is (= 1 (count @sent)) "nothing that came from Live is written back"))))
        (testing "a performer's turn in Live is a performance the run record keeps"
          (show/live-run-clip! rt {:type :clip-stopped :track-id track-id :watch-session "w" :serial 4})
          (tick! 16) (tick! 16.5)
          (let [b (:instance-id (submit! 17 {:op :cue/launch :cue-id :a :source push :launch {:mode :persistent}}))]
            (show/live-parameter-device-value! rt {:type :parameter-device-value :track-id track-id :slot (:slot level)
                                                   :value 0.3 :automation-state 0 :watch-session "w" :serial 5})
            (tick! 17.5) (tick! 18)
            (is (= 0.3 (get-in @(:state rt) [:model :instances b :params :level])))
            (let [take (last (runs/takes (show/live-runs-record rt)))]
              (is (= b (:instance-id take)))
              (is (= [[:level 0.3]] (mapv (juxt :param :value) (:moves take))) "the turn is a take move"))
            (is (= 1 (count @sent)) "a Live input is not echoed back to Live")))
        (testing "the owner step does bounded work (time is reported, not gated)"
          (let [{:keys [cycles mean-us max-us values max-values values-per-cycle-cap]} (:cost (show/live-parameter-devices rt))]
            (println (format "live parameter devices owner step (report only): %d cycles, mean %.1f us, max %.1f us" cycles mean-us max-us))
            (is (pos? cycles))
            (is (= 4 values) "each queued entry (three slot values, one clip start) was decoded once")
            (is (<= max-values values-per-cycle-cap)))))
      (finally (show/stop! rt)))))

(deftest a-stale-device-is-reconfigured-and-a-device-that-ignores-it-stays-pending
  (let [rt (show/create-runtime {:model (model-with registry) :sink {:send! (fn [_ _])} :workspace-options nil
                                 :phrase-capture false :live-runs {:record-id "q"}})
        tick! (fn [beat] (show/tick! rt (long (* 500 beat))))]
    (try
      (tick! 0)
      (let [reply (show/submit! rt :command {:op :cue/launch :cue-id :a :launch {:mode :persistent}})]
        (tick! 1)
        (show/submit! rt :command {:op :instance/end :instance-id (:instance-id @reply) :force? true})
        (tick! 2) (tick! 3))
      (let [ls (fake/create {:companion? false :device-applies? false :tracks [{:name "Tolglow run a"}]})
            _ (fake/add-parameter-device! ls "Tolglow run a")
            result (show/sync-live-runs! rt (live/open (fake/transport ls) {}) {:watch? false})]
        (is (not (:complete? result)) "the read-back still differs from the spec")
        (is (= [{:op :configure-parameter-device :cue-id :a :lane 0 :device 0 :parameter-device? true}]
               (filterv :parameter-device? (:pending result))))
        (is (= [0] (get-in result [:parameter-devices [:a 0] :stale])))
        (is (empty? (filter #(= :watch-run-tracks (first %)) (:requests @ls))) "{:watch? false} skips the watch"))
      (finally (show/stop! rt)))))

(deftest slot-values-from-another-thread-never-hold-the-owner
  (let [sent (atom 0)
        levels (atom [])
        writers (atom 0)
        writer-count #(count (filter (fn [^Thread t] (= "tolglow-live-parameter-devices" (.getName t))) (keys (Thread/getAllStackTraces))))
        rt (show/create-runtime {:model (model-with registry) :sink {:send! (fn [_ _])} :workspace-options nil :phrase-capture false
                                 :live-runs {:record-id "c"
                                             :device-send! (fn [payload]
                                                             (swap! sent inc)
                                                             (swap! writers max (writer-count))
                                                             (swap! levels into (map :value (:values payload)))
                                                             ;; A slow Live: sends take longer than owner cycles.
                                                             (Thread/sleep (+ 20 (rand-int 60)))
                                                             {:status :executed})}})
        tick! (fn [beat] (show/tick! rt (long (* 500 beat))))]
    (try
      (tick! 0)
      (let [a (let [reply (show/submit! rt :command {:op :cue/launch :cue-id :a :launch {:mode :persistent}})]
                (tick! 1)
                (:instance-id (deref reply 1000 nil)))]
        (let [reply (show/submit! rt :command {:op :cue/launch :cue-id :a :launch {:mode :persistent}})]
          (tick! 1.5)
          (show/submit! rt :command {:op :instance/end :instance-id (:instance-id @reply) :force? true}))
        (tick! 2) (tick! 3)
        (let [ls (fake/create {:companion? false :tracks [{:name "Tolglow run a"} {:name "Tolglow run a · 2"}]})
              _ (fake/add-parameter-device! ls "Tolglow run a")
              _ (fake/add-parameter-device! ls "Tolglow run a · 2")
              result (show/sync-live-runs! rt (live/open (fake/transport ls) {}))
              track-id (get-in result [:tracks [:lane :a 0]])
              running (atom true)
              owner (future (loop [beat 4.0 i 0]
                              (when @running
                                (when (even? i)
                                  (show/submit! rt :command {:op :param/set :instance-id a :param :level :value (min 1.0 (* i 0.001))}))
                                (show/tick! rt (long (* 500 beat)))
                                (recur (+ beat 0.05) (inc i)))))
              callback (future (dotimes [i 500]
                                 (show/live-parameter-device-value! rt {:type :parameter-device-value :track-id track-id :slot (rand-int 13)
                                                                        :value (rand) :automation-state (rand-int 3)
                                                                        :watch-session "t" :serial i})
                                 (when (zero? (mod i 50)) (Thread/sleep 1))))]
          (try
            (is (not= ::timeout (deref callback 10000 ::timeout)) "the callback thread finished")
            (is (wait-for #(zero? (get-in (show/live-parameter-devices rt) [:inbox :queued]))) "the owner drained the inbox")
            (reset! running false)
            (is (not= ::timeout (deref owner 10000 ::timeout)) "the owner kept cycling")
            (is (nil? (.findDeadlockedThreads (java.lang.management.ManagementFactory/getThreadMXBean))))
            (is (pos? @sent) "Tolglow writes left on their own thread")
            (is (= 1 @writers) "one writer thread, however slow the sends")
            (is (apply < @levels) "values reach Live in the order Tolglow accepted them (coalesced, never reordered)")
            (let [{:keys [counts cost inbox]} (show/live-parameter-devices rt)]
              (is (= 500 (:values cost)) "every value dequeued once")
              (is (nil? (:repeat counts)) "no value was handed to the decoder twice")
              (is (= 500 (reduce + (vals (dissoc counts :mirrored))))
                  "every value has exactly one outcome (applied, held, echo, invalid ...)")
              (is (<= (:max-values cost) (:values-per-cycle-cap cost) 256) "no cycle decodes more than the cap")
              (println (format "live parameter devices under load: %d cycles, mean %.1f us, max %.1f us; %s; inbox %s"
                               (:cycles cost) (:mean-us cost) (:max-us cost) (pr-str counts) (pr-str inbox))))
            (finally (reset! running false)))))
      (finally (show/stop! rt)))))

(deftest a-restarted-show-keeps-automation-on-its-parameter
  ;; Session one syncs Beam with all its parameters. The show restarts with a
  ;; revision that dropped `band`, which came before `level` in Studio's order.
  ;; The new session reads the slot names from the Set's device and keeps
  ;; Level on its old slot instead of shifting it.
  (let [ls (fake/create {:companion? false :tracks [{:name "Tolglow run a"}]})
        _ (fake/add-parameter-device! ls "Tolglow run a")
        session (fn [registry]
                  (let [rt (show/create-runtime {:model (model-with registry) :sink {:send! (fn [_ _])} :workspace-options nil
                                                 :phrase-capture false :live-runs {:record-id (str (gensym "r"))}})
                        tick! (fn [beat] (show/tick! rt (long (* 500 beat))))]
                    (try
                      (tick! 0)
                      (let [reply (show/submit! rt :command {:op :cue/launch :cue-id :a :launch {:mode :persistent}})]
                        (tick! 1)
                        (show/submit! rt :command {:op :instance/end :instance-id (:instance-id @reply) :force? true})
                        (tick! 2) (tick! 3))
                      (show/sync-live-runs! rt (live/open (fake/transport ls) {}) {:watch? false})
                      (into {} (map (juxt (juxt :param :component) (juxt :slot :name :retired?)))
                            (get-in (show/live-parameter-devices rt) [:channels 0 :slots]))
                      (finally (show/stop! rt)))))
        first-run (session registry)
        second-run (session (assoc-in registry [:effects :beam] (-> effect (assoc :version 2) (update :params dissoc :band))))]
    (is (= (first (first-run [:level nil])) (first (second-run [:level nil]))) "Level keeps its slot")
    (is (= (first (first-run [:tint 2])) (first (second-run [:tint 2]))))
    (is (not-any? #(and (= (first (first-run [:band 0])) (first %)) (not (last %))) (vals second-run))
        "band's old slots stay unused, not given to another parameter")
    (is (= "Level" (:name (nth (:parameters (fake/parameter-device ls "Tolglow run a" 0)) (first (second-run [:level nil]))))))))

(deftest a-performer-picking-an-earlier-value-is-a-move-not-an-echo
  ;; Delta review of #769, finding A: without the companion's receipt the
  ;; queue still holds earlier writes; on a choice slot or at a range end only
  ;; the newest may be taken for an echo.
  (let [{:keys [layout ctx state]} (owned)
        writes (fn [state param values]
                 (let [history (into {} (map-indexed (fn [i v] [(str "w" (name param) i)
                                                                {:command {:op :param/set :instance-id "i1" :param param :value v :source {:adapter :studio}}
                                                                 :ack {:status :accepted}}])
                                                     values))]
                   (devices/mirror-writes state (assoc-in ctx [:model :control] {:history history :history-order (vec (sort (keys history)))}))))
        mode (get-in layout [:slots [:mode nil]])
        level (get-in layout [:slots [:level nil]])
        turn (fn [state slot v] (devices/ingest state ctx [{:type :parameter-device-value :track-id "track:1" :slot slot :value v}]))]
    (testing "Studio went soft -> hard -> strobe; the performer picks hard in Live"
      (let [state (:state (writes state :mode [:soft :hard :strobe]))
            r (turn state mode 1)]
        (is (= [[:mode :hard :live-input]] (mapv (juxt :param :value (comp :origin :source)) (:commands r))))))
    (testing "Studio wrote 1.0 then 0.6; the performer turns the dial to its maximum"
      (let [state (:state (writes state :level [1.0 0.6]))
            r (turn state level 1.0)]
        (is (= [1.0] (mapv :value (:commands r))))))
    (testing "the newest write's echo is still dropped"
      (let [state (:state (writes state :mode [:soft :strobe]))]
        (is (empty? (:commands (turn state mode 2))))))
    (testing "slots the companion reports as its own stop waiting for an echo"
      (let [{:keys [state writes]} (writes state :level [0.3])
            seqs (into {} (map (fn [{:keys [slot seq]}] [slot [seq]])) (:values (first writes)))
            confirmed (devices/writes-confirmed state "track:1" [{:device 0 :index level}] seqs)]
        (is (seq (get-in state [:echo ["track:1" level]])))
        (is (empty? (get-in confirmed [:echo ["track:1" level]])))))))

(deftest seeding-merges-every-lane-device-and-reports-conflicts
  ;; Delta review of #769, finding B.
  (let [configured {:ordinal 0 :parameters [{:index 0 :name "aim X"} {:index 1 :name "steps"} {:index 2 :name "Level"}]}
        fresh {:ordinal 0 :parameters [{:index 0 :name "slot 1"} {:index 1 :name "slot 2"} {:index 2 :name "slot 3"}]}
        other {:ordinal 0 :parameters [{:index 0 :name "aim X"} {:index 1 :name "mode"}]}]
    (testing "an unconfigured device inspected after a configured one does not erase its names"
      (is (= {:names {0 "aim X" 1 "steps" 2 "Level"} :conflicts []} (devices/merge-read-back [configured fresh])))
      (is (= (devices/merge-read-back [configured fresh]) (devices/merge-read-back [fresh configured]))))
    (testing "two configured devices naming one slot differently are reported and the slot is reserved"
      (let [{:keys [names conflicts]} (devices/merge-read-back [configured other])
            layout (devices/seed-layout {"aim X" [:aim 0] "Level" [:level nil] "mode" [:mode nil] "steps" [:steps nil]}
                                        names (set (map :slot conflicts)) [[:aim 0] [:level nil] [:mode nil] [:steps nil]])]
        (is (= [{:slot 1 :names ["mode" "steps"]}] conflicts))
        (is (= 0 (get-in layout [:slots [:aim 0]])))
        (is (= 2 (get-in layout [:slots [:level nil]])))
        (is (not-any? #{1} (map (:slots layout) [[:mode nil] [:steps nil]])) "neither guess takes the disputed slot")))
    (testing "shared labels are told apart by parameter ID, independent of order"
      (let [twins (-> effect (assoc-in [:params :gain] {:type :number :bounds [0.0 1.0] :name "Level"}))
            model (model-with (assoc-in registry [:effects :beam] twins))
            names (fn [keys] (let [spec (devices/device-spec model :a (devices/allocate nil keys) nil)]
                               (set (keep #(when (#{:level :gain} (:param %)) [(:param %) (:name %)]) (:slots spec)))))]
        (is (= #{[:level "Level [level]"] [:gain "Level [gain]"]}
               (names [[:level nil] [:gain nil]]) (names [[:gain nil] [:level nil]])))))))

(deftest a-burst-is-decoded-at-most-256-values-per-owner-cycle
  ;; 300 values queued before one owner cycle: 256 are decoded, the other 44
  ;; stay queued in order and drain on the next cycle.
  (let [rt (show/create-runtime {:model (model-with registry) :sink {:send! (fn [_ _])} :workspace-options nil
                                 :phrase-capture false :live-runs {:record-id "b"}})
        status #(show/live-parameter-devices rt)]
    (try
      (show/tick! rt 0)
      (dotimes [i 300]
        (show/live-parameter-device-value! rt {:type :parameter-device-value :track-id "track:9" :slot 0 :value 0.5
                                               :watch-session "burst" :serial i}))
      (is (= 300 (get-in (status) [:inbox :queued])))
      (show/tick! rt 10)
      (is (= 256 (get-in (status) [:cost :max-values])) "one cycle decodes the cap")
      (is (= 44 (get-in (status) [:inbox :queued])) "the rest waits")
      (show/tick! rt 20)
      (is (= 0 (get-in (status) [:inbox :queued])))
      (is (= {:values 300 :max-values 256} (select-keys (:cost (status)) [:values :max-values])))
      (is (= {:unknown-track 300} (:counts (status))) "each value decoded once, in its own cycle or the next")
      (finally (show/stop! rt)))))
