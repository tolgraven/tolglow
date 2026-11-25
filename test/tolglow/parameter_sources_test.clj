(ns tolglow.parameter-sources-test
  (:require [afterglow.control.model :as control]
            [afterglow.control.params :as params]
            [afterglow.control.system :as system]
            [afterglow.output.artnet :as artnet]
            [clojure.test :refer [deftest is testing]]
            [tolglow.clock :as clock]
            [tolglow.effects.parameter-sources :as sources]
            [tolglow.modulation :as modulation]
            [tolglow.modulation-test :as fixture]
            [tolglow.show :as show]
            [tolglow.signals :as signals]
            [tolglow.studio.web :as web]
            [tolglow.transit :as transit])
  (:import [java.io ByteArrayInputStream]))

(def saw {:type :lfo :waveform :saw :period {:beats 1} :from 0.0 :to 1.0})

(defn- bind-to [model param binding]
  (sources/bind model {:instance-id "0:1" :param param :binding binding}))

(def pulse (assoc saw :waveform :pulse :phase-reference :transport))

(defn- without-cycle-replay [run]
  (with-redefs-fn {#'sources/pulse-cycle (constantly nil)} run))

(deftest pulse-cycle-replays-the-exact-phase-boundaries
  (let [sample (fn [model period]
                 (let [points [(- Double/MAX_VALUE) -3 -9/8 -1 (- Double/MIN_VALUE) -0.0
                               0 1/8 3/8 (Math/nextDown 0.5) 0.5 (Math/nextUp 0.5)
                               (Math/nextDown 1.0) 1 9/8 3 Double/MAX_VALUE]
                       step (fn [out phase]
                              (let [result (sources/sample model {:beat (* period phase) :time-ms 0} (:plans out))
                                    value (get-in result [:overrides "0:1" :level])]
                                {:plans (:plans result)
                                 :observations (conj (:observations out)
                                                     [(some-> value double Double/doubleToLongBits)
                                                      (:sources result)])}))]
                   (reduce step {:observations []} points)))]
    (doseq [duty [nil 0.0 Double/MIN_VALUE 3/8 0.5 (Math/nextDown 1.0) 1.0]
            period [1 1/2]]
      (let [binding (cond-> (assoc pulse :period {:beats period} :phase 0 :from 0.19 :to 0.83)
                      duty (assoc :inputs {:duty duty}))
            bound (bind-to (fixture/model) :level binding)
            replay (sample bound period)
            reference (without-cycle-replay #(sample bound period))]
        (is (= [:pulse] (mapv :cycle (vals (:plans replay)))))
        (is (= (:observations reference) (:observations replay)) (pr-str [duty period]))))))

(deftest pulse-cycle-reuses-only-captured-source-inputs
  (let [model (bind-to (fixture/model) :level pulse)
        frame (fixture/frame 1/4)
        first-sample (sources/sample model frame {})
        sample #(sources/sample %1 %2 (:plans first-sample))
        plan #(first (vals (:plans %)))
        value #(get-in % [:overrides "0:1" :level])
        seek (assoc frame :beat 3/4)
        changed (bind-to model :level (assoc pulse :inputs {:duty 0.1} :phase 1/8))
        builtin (some #(when (= :pulse (:id %)) %) (signals/builtin-waveforms))
        altered-template (assoc builtin :version 2 :expression {:op :const :value 0.37})
        replaced (assoc model :waveforms (signals/publish (signals/new-registry) altered-template))]
    (is (= 1.0 (value first-sample)))
    (is (= 0.0 (value (sample model seek))))
    (is (= (value first-sample) (value (sample model (assoc frame :time-ms 9000 :bpm 90)))))
    (is (identical? (plan first-sample) (plan (sample model seek))))
    (is (= 0.0 (value (sample changed frame))))
    (is (= 0.37 (value (sample replaced seek))))
    (is (nil? (:cycle (plan (sample replaced seek)))))
    (doseq [updated [(update model :epoch inc)
                     (assoc-in model [:instances "0:1" :definition :version] 2)
                     changed replaced]]
      (is (not (identical? (plan first-sample) (plan (sample updated frame))))))
    ;; The plan supplies a typed parameter before targeting or fixture addressing.
    (let [repatch (assoc-in model [:rig :fixtures] [{:id :new :universe 2 :address 44}])]
      (is (identical? (plan first-sample) (plan (sample repatch frame))))
      (is (= (value first-sample) (value (sample repatch frame)))))))

(deftest wide-pulse-output-uses-the-direct-evaluator
  (let [from (vec (repeat 65 0.2))
        to (vec (repeat 65 0.7))
        effect (assoc-in fixture/effect [:params :level] {:type :vector :size 65 :default from})
        bound (bind-to (fixture/model effect) :level (assoc pulse :from from :to to))
        results (mapv #(sources/sample bound (fixture/frame %) {}) [1/4 3/4])]
    (is (every? #(nil? (:cycle %)) (mapcat (comp vals :plans) results)))
    (is (= [to from] (mapv #(get-in % [:overrides "0:1" :level]) results)))))

(def triangle (assoc saw :waveform :triangle :phase-reference :transport :phase 0))

(deftest triangle-cycle-replays-only-proven-discrete-values
  (let [effect (assoc-in fixture/all-types [:params :binary] {:type :integer :default 0 :bounds [0 1]})
        endpoints {:binary [0 1] :gate [false true] :shape [:sine :square]
                   :level [0.0 1.0] :steps [1 4] :color [[0 0 0] [1 1 1]]}
        bound (reduce-kv (fn [m k [low high]] (bind-to m k (assoc triangle :from low :to high)))
                         (fixture/model effect) endpoints)
        points (concat [(- Double/MAX_VALUE) (- Double/MIN_VALUE) -0.0 0 1/4 1/2 3/4 1
                        0.25M (float 0.75) Double/MAX_VALUE]
                       (mapcat (fn [phase] [(Math/nextDown (double phase)) phase (Math/nextUp (double phase))])
                               [-1.0 -0.75 -0.25 0.0 0.25 0.75 1.0 1.25 1.75]))
        sample (fn [model]
                 (rest (reductions (fn [prior beat]
                                     (sources/sample model {:time-ms 0 :beat beat} (:plans prior)))
                                   {} points)))
        actual (vec (sample bound))
        direct (with-redefs-fn {#'sources/triangle-cycle (constantly nil)} #(vec (sample bound)))
        observations (fn [results] (mapv #(dissoc % :plans) results))
        captured (fn [result] (frequencies (map :cycle (vals (:plans result)))))
        binary (fn [i] (get-in actual [i :overrides "0:1" :binary]))]
    (is (= {:triangle 3 nil 3} (captured (first actual))))
    (is (= (observations direct) (observations actual)))
    (is (= [0 1 1 1 0] (mapv binary [3 4 5 6 7])))
    (is (every? #(instance? Long (get-in % [:overrides "0:1" :binary])) actual))
    (let [new-waveform (assoc (get-in (signals/new-registry) [:waveforms :triangle]) :version 2
                              :expression {:op :const :value 0.1})
          replaced (assoc bound :waveforms (signals/publish (signals/new-registry) new-waveform))
          next (sources/sample replaced (fixture/frame 1/2) (:plans (first actual)))]
      (is (= {nil 6} (captured next)))
      (is (zero? (get-in next [:overrides "0:1" :binary]))))))

(deftest phase-reference-aligns-separate-launches-on-the-shared-clock
  (let [old (assoc (fixture/instance "0:1" fixture/effect) :started-at-beat 5/2)
        recent (assoc (fixture/instance "0:2" fixture/effect) :started-at-beat 17/4
                      :timing-meter {:beats-per-bar 7/2 :bars-per-phrase 2})
        model (assoc (fixture/model) :instances {"0:1" old "0:2" recent})
        frame {:time-ms 3000 :transport {:beat 6 :meter {:beats-per-bar 3 :bars-per-phrase 2}}}
        sample (fn [period reference frame]
                 (let [binding (assoc saw :phase 1/8 :period period :phase-reference reference)
                       bound (reduce (fn [m id] (sources/bind m {:instance-id id :param :level :binding binding}))
                                     model ["0:1" "0:2"])
                       {:keys [overrides]} (sources/sample bound frame {})]
                   (mapv #(get-in overrides [% :level]) ["0:1" "0:2"])))]
    (is (= [0.125 0.125] (sample {:beats 2} :transport frame)))
    (is (= [0.875 0.0] (sample {:beats 2} :launch frame)))
    (is (= [0.125 0.125] (sample {:bars 1} :transport frame)))
    (is (= [0.0 0.625] (sample {:bars 1} :launch frame)))
    (is (= [0.125 0.125] (sample {:phrases 1} :transport frame)))
    (let [new-meter (assoc-in frame [:transport :meter :beats-per-bar] 4)]
      (is (= [0.625 0.625] (sample {:bars 1} :transport new-meter)))
      (is (= [0.875 0.875] (sample {:phrases 1} :transport new-meter)))
      (is (= (sample {:bars 1} :launch frame) (sample {:bars 1} :launch new-meter))))))

(deftest phase-reference-follows-pause-resume-and-seek-in-its-own-clock
  (doseq [[reference expected] [[:launch [0.25 0.25 0.5 0.5]]
                                [:transport [0.5 0.75 0.0 0.25]]]]
    (testing (name reference)
      (let [model (-> (fixture/model)
                      (assoc-in [:instances "0:1" :started-at-beat] 5/2)
                      (assoc-in [:transport :beat] 3)
                      (bind-to :level (assoc saw :period {:beats 2} :phase-reference reference)))
            pause-at (fn [m op frame]
                       (update-in m [:instances "0:1"] system/apply-control {:op op} frame))
            level (fn [m frame] (get-in (sources/sample m frame {}) [:overrides "0:1" :level]))
            initial (fixture/frame 3)
            waiting (fixture/frame 7/2)
            paused (pause-at model :instance/pause initial)
            resumed (pause-at paused :instance/resume waiting)
            later (fixture/frame 4)
            seek (assoc later :beat 5/2 :from-beat 4 :playing? false)
            sought (clock/rebase resumed seek)
            shared (fn [m t] {:time-ms t :transport (:transport m)})]
        (is (= expected [(level model initial) (level paused waiting) (level resumed later)
                         (level sought (shared sought 2000))]))
        (is (= (level sought (shared sought 2000)) (level sought (shared sought 9000))))))))

(deftest legacy-saved-bindings-default-to-cue-launch
  (let [model (-> (fixture/model)
                  (assoc-in [:instances "0:1" :started-at-beat] 5/2)
                  (bind-to :level (assoc saw :period {:beats 2})))
        instance (get-in model [:instances "0:1"])
        result (sources/sample model (fixture/frame 3) {})]
    (is (nil? (sources/validate-saved! model instance)))
    (is (nil? (get-in instance [:parameter-bindings :level :binding :phase-reference])))
    (is (= 0.25 (get-in result [:overrides "0:1" :level])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (sources/validate-saved! model (assoc-in instance [:parameter-bindings :level :binding :phase-reference] :bar))))))

(deftest every-declared-parameter-type-and-musical-period
  (let [model (fixture/model fixture/all-types)
        endpoints {:level [0.0 1.0] :steps [1 4] :gate [false true] :shape [:sine :square]
                   :color [[0 1 0] [1 0 1]] :position [[-2 0 2] [2 0 -2]] :spread [[0 1] [4 5]]}
        bound (reduce-kv (fn [m k [lo hi]] (bind-to m k (assoc saw :from lo :to hi :period {:bars 1})))
                         model endpoints)
        frame #(sources/sample bound {:beat % :time-ms 0} {})
        half (get-in (frame 2) [:overrides "0:1"])]
    (is (= (set (keys endpoints)) (set (keys half))))
    (is (empty? (params/validate-values (:params fixture/all-types) half)))
    (is (= [0.5 3 true :square [0.5 0.5 0.5] [0.0 0.0 0.0] [2.0 3.0]]
           (mapv half [:level :steps :gate :shape :color :position :spread])))
    (is (= false (get-in (frame 1) [:overrides "0:1" :gate])))
    (doseq [bad [(assoc saw :period {:beats 0}) (assoc saw :period {:ms 50}) (assoc saw :phase ##NaN)
                 (assoc saw :phase-reference nil) (assoc saw :phase-reference "transport")
                 (assoc saw :from 2) (assoc saw :inputs {:bogus 1}) (assoc saw :waveform :missing)]]
      (is (thrown? clojure.lang.ExceptionInfo (bind-to model :level bad))))))

(deftest registered-waveform-hot-reload-and-unavailable-targets-isolate-one-binding
  (let [library (signals/new-registry)
        constant (fn [v value] {:id :custom :version v :range [0 1] :params {} :expression {:op :const :value value}})
        model (assoc (fixture/model) :waveforms (signals/publish library (constant 1 0.2)))
        bound (bind-to model :level (assoc saw :waveform :custom))
        frame {:time-ms 0 :beat 0} first-sample (sources/sample bound frame {})
        updated (update bound :waveforms signals/publish (constant 2 0.6))
        next-sample (sources/sample updated frame (:plans first-sample))
        incompatible (assoc-in updated [:instances "0:1" :definition :params :level] {:type :boolean :default true})
        failed (sources/sample incompatible frame (:plans next-sample))]
    (is (= 0.2 (get-in first-sample [:overrides "0:1" :level])))
    (is (= 0.6 (get-in next-sample [:overrides "0:1" :level])))
    (is (string? (get-in failed [:sources "0:1" :level :error])))
    (is (empty? (:overrides failed)))
    (is (empty? (:plans (sources/sample (assoc updated :instances {}) frame (:plans next-sample)))))))

(defn- live-model []
  (control/new-show
    {:show-id :test :transport {:playing? true :beat 0.0 :bpm 120.0}
     :registry {:effects {:wash (assoc-in fixture/effect [:params :level :default] 0.25)}
                :cues {:wash {:id :wash :effect-id :wash :version 1}}}
     :rig {:fixtures (mapv (fn [id address] {:id id :universe 1 :address address :channels {:dimmer 1}})
                           [:left :right] [1 2])}}))

(deftest pulse-cycle-and-direct-sampling-publish-identical-show-frames
  (let [run (fn []
              (let [packet (volatile! nil)
                    sink {:send! (fn [universes _]
                                   (vreset! packet (into {} (map (fn [[u bytes]]
                                                                   [u (vec (artnet/artdmx-packet u 1 bytes))])
                                                                 universes))))}
                    rt (show/create-runtime {:model (live-model) :sink sink})
                    submit! (fn [time type body]
                              (let [receipt (show/submit! rt type body)]
                                (show/tick! rt time)
                                (select-keys @receipt [:status :instance-id])))
                    timeline (fn [times]
                               (mapv (fn [now]
                                       (show/tick! rt now)
                                       [@packet (mapv #(get-in % [:parameter-sources :level :effective-value])
                                                      (:instances (show/snapshot rt)))])
                                     times))
                    launch! (fn [now target]
                              (submit! now :command {:op :cue/launch :cue-id :wash :launch {:targets [target]}}))
                    bind! (fn [now id binding]
                            (submit! now :modulation-bind {:instance-id id :param :level :binding binding}))]
                (try
                  (let [left (launch! 0 :left)
                        right (launch! 0 :right)
                        binding (assoc pulse :period {:beats 3/2} :from 0.2 :to 0.8)
                        bound (bind! 0 (:instance-id left) binding)
                        initial (timeline (range 25 1001 25))
                        tempo (submit! 1000 :command {:op :transport/set :transport {:bpm 79}})
                        changing (timeline (range 1025 1501 25))
                        edit (bind! 1500 (:instance-id left) (assoc binding :inputs {:duty 1/4} :phase 1/8))
                        fade (submit! 1500 :command {:op :master/set :value 0.6})
                        faded (timeline (range 1525 2001 25))
                        repatch (submit! 2000 :rig (assoc-in (:rig (show/snapshot rt)) [:fixtures 0 :address] 6))
                        moved (timeline [2025])
                        blackout (submit! 2025 :command {:op :blackout/set :value true})
                        dark (timeline [2050])
                        reset (submit! 2075 :command {:op :show/restart})
                        lit (submit! 2075 :command {:op :blackout/set :value false})
                        relaunched (launch! 2075 :left)
                        rebound (bind! 2075 (:instance-id relaunched) (assoc binding :phase-reference :launch))
                        restarted (timeline (range 2100 2601 25))]
                    {:receipts [left right bound tempo edit fade repatch blackout reset lit relaunched rebound]
                     :frames [initial changing faded moved dark restarted]})
                  (finally (show/stop! rt)))))
        replay (run)
        reference (without-cycle-replay run)]
    (is (every? #(= :accepted (:status %)) (:receipts replay)) (pr-str (:receipts replay)))
    (is (every? (comp seq first) (mapcat identity (:frames replay))))
    (is (= #{0.2 0.8} (set (keep first (map second (first (:frames replay)))))))
    (let [packet (get-in replay [:frames 3 0 0 1])]
      (is (zero? (nth packet 18)))
      (is (pos? (bit-and 255 (nth packet 23)))))
    (is (= replay reference))))

(deftest ring-through-render-two-instances-base-hold-and-transit-restore
  (let [packet (atom nil) t (atom 0)
        rt (show/create-runtime {:model (live-model) :sink {:send! (fn [bytes _] (reset! packet (mapv #(bit-and 255 %) (take 2 (get bytes 1)))))}})
        submit! (fn [op body] (let [ack (show/submit! rt op body)] (show/tick! rt (swap! t inc)) ack))
        app (web/app {:submit! submit! :heartbeat! (constantly nil)})
        post (fn [route data]
               (:status (app {:request-method :post :uri (str "/api/modulation/" route)
                              :headers {"host" "localhost" "content-type" "application/transit+json"}
                              :body (ByteArrayInputStream. (.getBytes (transit/encode data) "UTF-8"))})))
        instance (fn [id] (some #(when (= id (:id %)) %) (:instances (show/snapshot rt))))
        launch (fn [target] (:instance-id @(submit! :command {:op :cue/launch :cue-id :wash :launch {:targets [target]}})))]
    (try
      (let [left (launch :left) right (launch :right)
            addr {:instance-id left :param :level} hold {:instance-id left :params [:level]}]
        (is (not= left right))
        (is (= 200 (post "bind" (assoc addr :binding (assoc saw :phase-reference :transport :from 0.8 :to 0.8)))))
        (is (= [204 64] @packet))
        (is (= 0.25 (get-in (instance left) [:params :level])))
        (is (= 0.8 (get-in (instance left) [:parameter-sources :level :effective-value])))
        (is (empty? (:parameter-sources (instance right))))
        (is (= 200 (post "hold" hold)))
        (is (= [64 64] @packet))
        (is (true? (get-in (instance left) [:parameter-sources :level :sources 0 :held?])))
        (is (= 200 (post "resume" hold)))
        (is (= [204 64] @packet))
        (let [source (show/export-show-source rt)
              receipt @(submit! :import {:source source})
              restored-id (:id (first (:instances (show/snapshot rt))))
              restored {:instance-id restored-id :param :level}]
          (is (= :accepted (:status receipt)) (pr-str receipt))
          (is (not= left restored-id))
          (is (= 0.8 (get-in (instance restored-id) [:parameter-sources :level :effective-value])))
          (is (= :transport (get-in (instance restored-id) [:parameter-sources :level :sources 0 :binding :phase-reference])))
          (is (= 200 (post "bind" (assoc restored :binding (assoc saw :period {:beats 1/2})))))
          (let [before @packet]
            (show/tick! rt (swap! t + 125))
            (is (not= (first before) (first @packet)))
            (is (= 64 (second @packet))))
          (is (= 200 (post "unbind" restored)))
          (is (= [64 64] @packet))
          (is (empty? (:parameter-sources (instance restored-id))))))
      (finally (show/stop! rt)))))

(deftest ring-moves-one-running-waveform-between-typed-destinations
  (let [model (-> (live-model)
                  (assoc-in [:registry :effects :wash :params :step] {:type :integer :bounds [1 5] :default 1})
                  (assoc-in [:registry :effects :wash :params :hue] {:type :number :bounds [0 360] :default 60})
                  (assoc-in [:registry :effects :wash :outputs :strobe]
                            {:op :divide :args [{:op :param :key :step} 5]})
                  (assoc-in [:rig :fixtures 0 :channels :strobe] 3)
                  (assoc-in [:rig :fixtures 1 :channels :strobe] 3))
        packet (atom nil)
        rt (show/create-runtime {:model model :sink {:send! (fn [bytes _] (reset! packet (mapv #(bit-and 255 %) (take 3 (get bytes 1)))))}})
        now (atom 0)
        submit! (fn [op body] (let [ack (show/submit! rt op body)] (show/tick! rt @now) ack))
        app (web/app {:submit! submit! :heartbeat! (constantly nil)})
        post (fn [route data]
               (:status (app {:request-method :post :uri (str "/api/modulation/" route)
                              :headers {"host" "localhost" "content-type" "application/transit+json"}
                              :body (ByteArrayInputStream. (.getBytes (transit/encode data) "UTF-8"))})))
        launch (fn [target] (:instance-id @(submit! :command {:op :cue/launch :cue-id :wash :launch {:targets [target]}})))
        projection (fn [id] (some #(when (= id (:id %)) %) (:instances (show/snapshot rt))))
        committed (fn [id] (get-in @(:state rt) [:model :instances id]))]
    (try
      (let [left (launch :left) right (launch :right)
            address {:instance-id left}
            bind (fn [param binding] (post "bind" (assoc address :param param :binding binding)))
            move (fn [from param lo hi]
                   (post "bind" (assoc address :from-param from :param param :binding {:from lo :to hi}
                                       :expected-source (get-in (projection left) [:parameter-sources from :sources 0 :identity]))))
            binding (assoc saw :period {:beats 2} :phase 1/8 :phase-reference :launch)]
        (reset! now 250)
        (is (= 200 (bind :level binding)))
        (is (= [96 64 51] @packet))
        (is (= 200 (bind :hue (assoc saw :from 60 :to 60))))
        (is (= 200 (bind :hue (assoc saw :from 90 :to 90))))
        (is (= 90.0 (get-in (projection left) [:parameter-sources :hue :effective-value])))
        (let [before (:parameter-bindings (committed left))]
          (is (= 422 (move :level :hue 0 360)))
          (is (= 422 (move :level :step 0.25 5)))
          (is (= 422 (move :step :hue 0 360)))
          (is (= 422 (move :level :level 0 1)))
          (is (= before (:parameter-bindings (committed left))))
          (is (= [96 64 51] @packet)))
        (is (= 200 (post "hold" (assoc address :params [:level :step]))))
        (is (= [64 64 51] @packet))
        (let [owned-keys [:id :order :params :state :phase :timing-meter :started-at-beat :started-at-ms]
              before (select-keys (committed left) owned-keys)
              holds (get-in @(:state rt) [:model :modulation :holds])]
          (reset! now 375)
          (is (= 200 (move :level :step 1 5)))
          (is (= before (select-keys (committed left) owned-keys)))
          (is (= holds (get-in @(:state rt) [:model :modulation :holds])))
          (is (= [64 64 51] @packet))
          (is (= #{:hue :step} (set (keys (:parameter-sources (projection left))))))
          (is (true? (get-in (projection left) [:parameter-sources :step :sources 0 :held?])))
          (is (= 200 (post "resume" (assoc address :params [:step]))))
          (is (= [64 64 153] @packet))
          (is (= 3 (get-in (projection left) [:parameter-sources :step :effective-value])))
          (is (empty? (:parameter-sources (projection right))))
          (is (= 200 (post "unbind" (assoc address :param :hue))))
          (reset! now 500)
          (is (= 200 (move :step :hue 0 360)))
          (is (= [64 64 51] @packet))
          (is (= 225.0 (get-in (projection left) [:parameter-sources :hue :effective-value])))
          (is (= (assoc binding :from 0 :to 360 :inputs {})
                 (get-in (projection left) [:parameter-sources :hue :sources 0 :binding])))
          (is (= #{:hue} (set (keys (:parameter-sources (projection left))))))))
      (finally (show/stop! rt)))))

(deftest stale-queued-moves-reject-and-committed-receipts-survive-supersession
  (let [model (assoc-in (live-model) [:registry :effects :wash :params :step]
                        {:type :integer :bounds [1 5] :default 1})
        rt (show/create-runtime {:model model})
        enqueue! (partial show/submit! rt)
        tick! #(show/tick! rt 250)
        projection #(show/snapshot rt)
        app (web/app {:submit! enqueue! :heartbeat! (constantly nil)})]
    (try
      (let [launch (enqueue! :command {:op :cue/launch :cue-id :wash :launch {:reload-policy :preserve-state}})
            _ (tick!)
            address {:instance-id (:instance-id @launch)}
            bind (fn [waveform] (enqueue! :modulation-bind (assoc address :param :level :binding (assoc saw :waveform waveform))))
            source (fn [] (get-in (projection) [:instances 0 :parameter-sources :level :sources 0]))
            move (fn [id expected hi]
                   (assoc address :param :step :from-param :level :expected-source expected
                          :binding {:from 1 :to hi} :request-id id))
            before (bind :saw)]
        (tick!)
        (is (= :accepted (:status @before)))
        (let [visible (:identity (source))
              replacement (bind :triangle)
              stale (enqueue! :modulation-bind (move "stale-waveform" visible 5))]
          (is (not (realized? stale)))
          (tick!)
          (is (= :accepted (:status @replacement)))
          (is (= :rejected (:status @stale)))
          (is (re-find #"source or its effect revision changed" (:message @stale)))
          (is (= #{:level} (set (keys (get-in (projection) [:instances 0 :parameter-sources])))))
          (is (= :triangle (get-in (source) [:binding :waveform]))))
        (let [visible (:identity (source))
              new-effect (-> (get-in model [:registry :effects :wash]) (assoc :version 2)
                             (assoc-in [:params :step :bounds] [1 3]))
              publish (enqueue! :definition {:kind :effects :definition new-effect})
              stale (enqueue! :modulation-bind (move "stale-schema" visible 5))]
          (tick!)
          (is (= :accepted (:status @publish)) (pr-str @publish))
          (is (= :rejected (:status @stale)))
          (is (re-find #"source or its effect revision changed" (:message @stale)))
          (is (= #{:level} (set (keys (get-in (projection) [:instances 0 :parameter-sources])))))
          (is (= 2 (get-in (projection) [:instances 0 :effect-version]))))
        (let [request (move "queued-invalid" (:identity (source)) 5)
              response (app {:request-method :post :uri "/api/modulation/bind"
                             :headers {"host" "localhost" "accept" "application/transit+json"
                                       "content-type" "application/transit+json"}
                             :body (ByteArrayInputStream. (.getBytes (transit/encode request) "UTF-8"))})]
          (is (= 200 (:status response)))
          (is (= :pending (:status (transit/decode (:body response)))))
          (is (not-any? #(= "queued-invalid" (:request-id %)) (:parameter-source-outcomes (projection))))
          (tick!)
          (let [result (last (:parameter-source-outcomes (projection)))]
            (is (= {:request-id "queued-invalid" :status :rejected :epoch 0}
                   (select-keys result [:request-id :status :epoch])))
            (is (re-find #"destination parameter schema" (:message result))))
          (is (some? (source))))
        (let [accepted (enqueue! :modulation-bind (move "committed-move" (:identity (source)) 3))
              superseded (enqueue! :modulation-unbind (assoc address :param :step :request-id "superseding-edit"))]
          (tick!)
          (is (= :accepted (:status @accepted)))
          (is (= :accepted (:status @superseded)))
          (is (empty? (get-in (projection) [:instances 0 :parameter-sources])))
          (is (= [{:request-id "committed-move" :status :accepted :epoch 0 :revision (:revision @accepted)}
                  {:request-id "superseding-edit" :status :accepted :epoch 0 :revision (:revision @superseded)}]
                 (take-last 2 (:parameter-source-outcomes (projection))))))
        (let [restart (enqueue! :command {:op :show/restart})]
          (tick!)
          (is (= :accepted (:status @restart)))
          (is (empty? (:parameter-source-outcomes (projection))))))
      (finally (show/stop! rt)))))

(deftest observed-source-types-and-static-projection-cost
  (let [base (fixture/model)
        older (get-in base [:instances "0:1"])
        newer (assoc older :id "0:2" :order 1)
        model (-> base (assoc-in [:instances "0:2"] newer)
                  (assoc-in [:instances "0:1" :definition :spatial-volume :sources :level] {:variable [:ball :speed]})
                  (assoc :routing {:enabled? true
                                   :spec {:routes [{:id :bass :destination :left :source "audio/bass-pitch-hz"}]
                                          :destinations {:left {:param :level}}}
                                   :state {:routes {:bass {:instance-id "0:1"}}}}))
        live {:channel 2 :bridge-id "venue" :parameters {23 {:cue-id :wash :param :level :lane-note 60}
                                                          45 {:cue-id :wash :param :level :disabled? true}}}
        sources (sources/source-index model {:spatial-overrides {"0:1" {:level 0.5}} :live-routing live})]
    (is (= [:patch :spatial] (mapv :type (get-in sources ["0:1" :level]))))
    (is (= "audio/bass-pitch-hz" (get-in sources ["0:1" :level 0 :source-id])))
    (is (= {:variable [:ball :speed]} (get-in sources ["0:1" :level 1 :source])))
    (is (= [{:type :live :controller 23 :bridge-id "venue" :channel 2 :lane-note 60}]
           (get-in sources ["0:2" :level])))
    (is (empty? (sources/source-index base {})))
    (is (empty? (sources/project older {} #(throw (AssertionError. "Unbound static cards must not resolve parameters")) false)))))

(deftest composed-registered-waveforms-capture-their-lowered-input-version
  (let [library (signals/new-registry)
        composite (fn [registry version]
                    {:id :stacked :version version :range [0 1] :params {}
                     :expression (signals/sample registry :saw {:phase {:op :param :key signals/phase-input}})})
        first-library (signals/publish library (composite library 1))
        model (bind-to (assoc (fixture/model) :waveforms first-library) :level (assoc saw :waveform :stacked))
        sample (fn [m cache] (sources/sample m {:beat 0.25 :time-ms 125} cache))
        first-output (sample model {})
        replace-saw (assoc (get-in library [:waveforms :saw]) :version 2 :expression {:op :const :value 0.7})
        second-library (signals/publish first-library replace-saw)
        captured (sample (assoc model :waveforms second-library) (:plans first-output))
        updated (sample (assoc model :waveforms (signals/publish second-library (composite second-library 2)))
                        (:plans captured))]
    (is (= 0.25 (get-in first-output [:overrides "0:1" :level])))
    (is (= (:overrides first-output) (:overrides captured)))
    (is (= 0.7 (get-in updated [:overrides "0:1" :level])))))

(deftest random-sources-only-drive-after-an-actual-valid-sample
  (let [model (fixture/model fixture/all-types)
        start (fixture/start {:params [:unbounded :level]})
        initial (modulation/advance start model (fixture/frame 0))
        index #(sources/source-index (assoc model :modulation (:state %)) {:diagnostics (:diagnostics %)})
        source (fn [result param] (get-in (index result) ["0:1" param 0]))
        held (modulation/hold (:state initial) {:instance-id "0:1" :params [:level]})
        resume (modulation/release-hold held "0:1" [:level])
        between (modulation/advance resume model (fixture/frame 1/2))]
    (is (true? (:pending? (source initial :unbounded))))
    (is (string? (:error (source initial :unbounded))))
    (is (nil? (:pending? (source initial :level))))
    (is (true? (:held? (source {:state held} :level))))
    (is (true? (:pending? (source between :level))))
    (is (nil? (:pending? (source (modulation/advance (:state between) model (fixture/frame 1)) :level))))))

(deftest restored-random-source-matches-the-captured-dmx-before-another-tick
  (let [packet (atom nil)
        rt (show/create-runtime {:model (live-model) :sink {:send! (fn [bytes _] (reset! packet (bit-and 255 (first (get bytes 1)))))}})
        submit! (fn [op body time] (let [ack (show/submit! rt op body)] (show/tick! rt time) @ack))]
    (try
      (let [id (:instance-id (submit! :command {:op :cue/launch :cue-id :wash :launch {:targets [:left]}} 0))]
        (submit! :modulation-start {:id :dice :seed 13 :every {:beats 1} :select {:instance-ids [id]}
                                    :params [:level] :ranges {:level [0.8 0.8]}} 10)
        (is (= 204 @packet))
        (let [source (show/export-show-source rt)
              schedule #(select-keys (first (get-in @(:state rt) [:model :modulation :controllers])) [:seed :ticks :next-beat])
              saved (schedule)]
          (is (= :accepted (:status (submit! :import {:source source} 20))))
          (let [restored (first (:instances (show/snapshot rt)))]
            (is (not= id (:id restored)))
            (is (= 204 @packet))
            (is (= saved (schedule)))
            (is (= 0.25 (get-in restored [:parameter-sources :level :base-value])))
            (is (= 0.8 (get-in restored [:parameter-sources :level :effective-value])))
            (is (= :random (get-in restored [:parameter-sources :level :sources 0 :type]))))))
      (finally (show/stop! rt)))))

(deftest a-failed-response-remains-visible-only-on-the-affected-input
  (let [model (assoc (fixture/model fixture/all-types) :responses
                     [{:id :gate-envelope :params [:gate :level] :operation :replace :range [0.7 0.7]
                       :state {:started-at-ms 0}}])
        rt (show/create-runtime {:model model})]
    (try
      (let [params (:parameter-sources (first (:instances (show/snapshot rt))))]
        (is (string? (get-in params [:gate :error])))
        (is (= :gate-envelope (get-in params [:gate :sources 0 :id])))
        (is (string? (get-in params [:gate :sources 0 :error])))
        (is (nil? (get-in params [:gate :effective-value])))
        (is (nil? (get-in params [:level :error])))
        (is (= 0.7 (get-in params [:level :effective-value])))
        (is (= [{:type :response :id :gate-envelope}] (get-in params [:level :sources]))))
      (finally (show/stop! rt)))))
