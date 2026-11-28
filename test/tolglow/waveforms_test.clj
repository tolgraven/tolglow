(ns tolglow.waveforms-test
  (:require [clojure.test :refer [deftest is testing]]
            [afterglow.runtime :as runtime]
            [afterglow.time :as time]
            [clojure.data.json :as json]
            [tolglow.clock :as clock]
            [tolglow.composition.response :as response]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.state-stream :as state-stream]
            [tolglow.waveforms :as waveforms]
            [tolglow.audio.capture :as audio]
            [tolglow.audio.capture-test :as pcm]
            [tolglow.studio.display :as display]
            [tolglow.signals :as signals]
            [tolglow.effects :as fx]
            [tolglow.compiler :as compiler]
            [tolglow.effects-test :as recipes]
            [afterglow.control.devices.push-2 :as push]
            [afterglow.control.devices.transports :as transports])
  (:import [java.awt Color]
           [java.awt.image BufferedImage]
           [java.io ByteArrayInputStream]
           [java.util.concurrent CountDownLatch TimeUnit]))

(defn near? [a b] (< (Math/abs (double (- a b))) 1.0e-8))

(defn fixture
  ([] (fixture 1))
  ([period]
   (let [registered (signals/new-registry)
         schema (signals/selector-schema registered [{:id :pulse :params {:duty (fx/param :width)}} :sine])
         effect (fx/effect :shape {:params {:waveform schema :width (fx/parameter 0.25 {:bounds [0 1]})}}
                           (fx/intensity (signals/select-waveform registered (fx/param :waveform) (:choices schema)
                                                                  {:phase (fx/cycles {:beats period})})))
         instance {:id "0:1" :effect-id :shape :effect-version 1 :definition effect :cue-id :shape
                   :phase :running :params {:waveform 0.0 :width 0.25} :state {:ticks 3} :started-at-ms 0
                   :started-at-beat 0 :system {:last-clock {:time-ms 250 :beat 0.5}}}]
     {:model {:epoch 0 :time-ms 250 :transport {:bpm 120 :beat 0.5 :playing? true}
              :waveforms registered :instances {"0:1" instance}
              :registry {:effects {:shape effect} :cues {:shape {:id :shape :effect-id :shape}}}}
      :modulation-view {:overrides {}}})))

(defn- with-phase
  "Drive the fixture selector from another musical phase."
  [snapshot phase]
  (let [registered (get-in snapshot [:model :waveforms])
        schema (get-in snapshot [:model :instances "0:1" :definition :params :waveform])
        effect (fx/effect :shape {:params {:waveform schema :width (fx/parameter 0.25 {:bounds [0 1]})}}
                          (fx/intensity (signals/select-waveform registered (fx/param :waveform) (:choices schema)
                                                                 {:phase phase})))]
    (assoc-in snapshot [:model :instances "0:1" :definition] effect)))

(deftest live-waveform-uses-captured-parameters-and-the-real-effect-ir
  (let [snapshot (fixture) request {:instance-id "0:1" :parameter "waveform" :options {:samples 9}}
        result (waveforms/preview snapshot request)
        step (compiler/compile-effect (get-in snapshot [:model :instances "0:1" :definition]))]
    (is (= [1.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0] (:preview-samples result)))
    (doseq [{:keys [phase value]} (:samples result)]
      (let [context (assoc (recipes/context phase) :instance (get-in snapshot [:model :instances "0:1"]))]
        (is (near? value (get-in (recipes/frame step context) [:values :left :dimmer])))))
    (is (= result (waveforms/preview snapshot (assoc request :context-params {:width 0.99 :waveform 1}))))
    (is (= 3 (get-in snapshot [:model :instances "0:1" :state :ticks])))
    (is (= :amplitude-per-cycle (get-in result [:units :slope])))
    (is (= [1.0 1.0 1.0 1.0 1.0 1.0 0.0 0.0 1.0]
           (:preview-samples (waveforms/preview (assoc-in snapshot [:modulation-view :overrides "0:1" :width] 0.75) request))))
    (is (thrown? clojure.lang.ExceptionInfo (waveforms/preview snapshot (assoc request :parameter :width))))
    (is (thrown? clojure.lang.ExceptionInfo (waveforms/preview snapshot (assoc request :instance-id "stale"))))))

(deftest compact-timeline-retains-short-pulses-and-follows-the-committed-beat
  (let [snapshot (-> (fixture) (assoc :transport-generation 18)
                     (assoc-in [:model :transport :meter] {:beats-per-bar 3.5 :bars-per-phrase 4})
                     (assoc-in [:model :instances "0:1" :params :width] 0.002))
        request {:instance-id "0:1" :parameter :waveform :options {:samples 9 :live-timing? true}}
        project #(get (waveforms/preview % request) :timeline)
        first (project snapshot)
        rebase (project (-> snapshot (assoc :transport-generation 19)
                            (update :model clock/rebase {:beat 25.25 :source :link :source-epoch 2})))
        static (project (assoc-in snapshot [:model :instances "0:1" :params :width] 0.0))
        narrow-edge? (fn [[a b]]
                       (and (not= (:value a) (:value b)) (< (- (:phase b) (:phase a)) 0.000001)))]
    (is (= 0.5 (:beat first)))
    (is (zero? (:now-value first)))
    (is (= [0.0 1.0] (sort (distinct (map :value (:samples first))))))
    (is (<= 12 (count (filter narrow-edge? (partition 2 1 (:samples first))))))
    (is (= #{:beat :bar :phrase} (set (map :kind (:ticks first)))))
    (is (some #(and (= :bar (:kind %)) (near? (/ 3.0 3.5) (:position %))) (:ticks first)))
    (is (= 25.25 (:beat rebase)))
    (is (= :link (get-in rebase [:transport :source])))
    (is (= 2 (get-in rebase [:transport :source-epoch])))
    (is (= [18 19] (mapv :transport-generation [first rebase])))
    (is (= (:samples first) (:samples rebase)))
    (is (not= (:ticks first) (:ticks rebase)))
    (is (= #{0.0} (into #{} (map :value) (:samples static))))
    (is (nil? (project (assoc-in snapshot [:model :instances "0:1" :definition :outputs] {:dimmer 0.5}))))
    (is (nil? (:timeline (waveforms/preview snapshot {:id :pulse :options {:live-timing? true}}))))))

(deftest previews-and-push-views-carry-the-timing-and-grid-of-their-signal
  (let [snapshot (fixture 4)
        request {:instance-id "0:1" :parameter :waveform :options {:samples 9 :cycles 2}}
        result (waveforms/preview snapshot request)
        seven-eight {:beats-per-bar 3.5 :bars-per-phrase 4}
        bar (fn [state] (waveforms/preview (with-phase state (fx/cycles {:bars 1}))
                                           (assoc-in request [:options :cycles] 1)))
        view (push/display-view {:selected-instance "0:1" :selected-cue :shape :page-id :main :origin [0 0]} (:model snapshot))
        projected (waveforms/project-display (waveforms/create-cache)
                                             (assoc-in snapshot [:model :transport :meter] seven-eight) view nil)
        waveform (first (filter #(= :waveform (:param %)) (:parameters projected)))]
    (is (= {:period-beats 4.0 :meter time/default-meter :start-beat 0.0 :span-beats 8.0} (:signal-timing result)))
    (is (= [:beat :bar] (get-in result [:grid :units])))
    (is (= [:bar :beat :beat :beat :bar :beat :beat :beat :bar] (mapv :unit (get-in result [:grid :lines]))))
    (is (= (mapv #(/ % 8.0) (range 9)) (mapv :position (get-in result [:grid :lines]))))
    (is (= {:unit :bar :count 1.0 :beats 4.0} (get-in result [:grid :period])))
    (testing "a bar period follows the meter the launch captured"
      (let [captured (bar (assoc-in snapshot [:model :instances "0:1" :timing-meter] seven-eight))]
        (is (= 3.5 (get-in captured [:signal-timing :period-beats])))
        (is (= seven-eight (get-in captured [:signal-timing :meter])))
        (is (= [0.0 1.0 2.0 3.0 3.5] (mapv :beat (get-in captured [:grid :lines]))))
        (is (= 4.0 (get-in (bar snapshot) [:signal-timing :period-beats])))))
    (testing "a draft or an output without one musical phase has no grid"
      (is (nil? (:grid (waveforms/preview snapshot {:id :pulse}))))
      (is (nil? (:signal-timing (waveforms/preview snapshot {:id :pulse}))))
      (is (nil? (:grid (waveforms/preview (assoc-in snapshot [:model :instances "0:1" :definition :outputs] {:dimmer 0.5})
                                          request)))))
    (testing "the Push view carries the transport meter and the previewed cycle"
      (is (= {:meter seven-eight} (:musical-timing projected)))
      (is (= {:period-beats 4.0 :meter time/default-meter :start-beat 0.0 :span-beats 4.0} (:signal-timing waveform)))
      (is (= {:meter time/default-meter :bar-origin-beat 1.0}
             (:musical-timing (waveforms/project-display (waveforms/create-cache)
                                                         (assoc-in snapshot [:model :transport :bar-origin-beat] 1.0)
                                                         view nil)))))))

(deftest compact-timeline-keeps-refreshing-through-a-flat-part-of-a-long-cycle
  (let [snapshot (fixture 16)
        request {:instance-id "0:1" :parameter :waveform :options {:live-timing? true}}
        sample (fn [beat]
                 (let [time-ms (* beat 500)]
                   (:timeline (waveforms/preview
                               (-> snapshot
                                   (assoc-in [:model :transport :beat] beat)
                                   (assoc-in [:model :instances "0:1" :system :last-clock]
                                             {:time-ms time-ms :beat beat}))
                               request))))
        middle (sample 9)
        next-pulse (sample 14)]
    (is (= 9 (:beat middle)))
    (is (= #{0.0} (into #{} (map :value) (:samples middle))))
    (is (= 14 (:beat next-pulse)))
    (is (= #{0.0 1.0} (into #{} (map :value) (:samples next-pulse))))))

(deftest transport-timeline-samples-the-composed-registry-wave-at-the-emitted-phase
  (let [base (fixture)
        waves (get-in base [:model :waveforms])
        phase-input (fx/param signals/phase-input)
        composition {:id :layered :name "Layered square and sine" :category :composite :version 1
                     :range [0 1] :params {}
                     :expression (fx/mix (signals/sample waves :pulse {:phase phase-input :params {:duty 0.25}})
                                         (signals/sample waves :sine {:phase (fx/add phase-input 0.25)}) 0.35)
                     :discontinuities [{:channel :value :phase 0} {:channel :value :phase 0.25}]}
        waves (signals/publish waves composition)
        selector (signals/selector-schema waves [:layered])
        definition (fx/effect :shape {:params {:waveform selector}}
                              (fx/intensity (signals/select-waveform waves (fx/param :waveform) (:choices selector)
                                                                    {:phase (fx/cycles {:beats 2} {:phase-reference :transport
                                                                                                   :phase 0.125})})))
        snapshot (-> base (assoc-in [:model :waveforms] waves)
                     (assoc-in [:model :instances "0:1" :definition] definition)
                     (update-in [:model :instances "0:1" :params] dissoc :width))
        request {:instance-id "0:1" :parameter :waveform :options {:live-timing? true}}
        step (compiler/compile-effect definition)
        current (fn [snapshot]
                  (let [beat (get-in snapshot [:model :transport :beat])
                        instance (get-in snapshot [:model :instances "0:1"])
                        context (-> (recipes/context beat) (assoc :instance instance)
                                    (assoc-in [:frame :transport-clock] {:beat beat}))]
                    (get-in (recipes/frame step context) [:values :left :dimmer])))
        first (get (waveforms/preview snapshot request) :timeline)
        later-snapshot (update snapshot :model clock/rebase {:beat 7.125 :source :link :source-epoch 2})
        later (get (waveforms/preview later-snapshot request) :timeline)]
    (is (near? (current snapshot) (:now-value first)))
    (is (near? (current later-snapshot) (:now-value later)))
    (is (not= (:samples first) (:samples later)))))

(deftest pure-response-and-paused-clock-match-frame-parameter-transform
  (let [snapshot (-> (fixture)
                     (assoc-in [:modulation-view :overrides "0:1" :width] 0.8)
                     (assoc-in [:model :time-ms] 10000)
                     (assoc-in [:model :responses]
                               [{:id :attack :params [:width] :operation :multiply
                                 :envelope {:attack {:ms 500}} :state {:started-at-ms 0}}]))
        params (waveforms/effective-params snapshot (get-in snapshot [:model :instances "0:1"]))]
    (is (near? 0.4 (:width params)))
    (is (= 0.25 (get-in snapshot [:model :instances "0:1" :params :width])))))

(defn- response-show [update-definition]
  (-> (demo/model)
      (update-in [:registry :effects :ember] update-definition)
      (assoc :responses [{:id :gain :select {:cue-ids [:ember]} :params [:level]
                          :operation :multiply :range [0.0 1.0] :envelope {:attack {:beats 1}}
                          :state {:started-at-ms 0 :started-at-beat 0}}])))

(defn- command-at! [runtime at command]
  (let [reply (show/submit! runtime command)]
    (show/tick! runtime at)
    (deref reply 1000 ::timeout)))

(defn- counted-response [calls]
  (let [compile-response response/compile-response]
    (fn [spec]
      (let [evaluate (compile-response spec)]
        (fn [context]
          (let [result (evaluate context)]
            (when (not= (:response-values result) (:response-values context))
              (swap! calls conj (get-in context [:frame :beat])))
            result))))))

(deftest studio-and-push-read-the-committed-exact-response
  (let [calls (atom [])
        model (response-show
                #(assoc % :blocks {[:outputs :color] {:stages [{:block :optics/gel}]}}))
        runtime (show/create-runtime {:model model :workspace-options nil :worker-timeout-ms 10000})
        display (with-redefs [display/browser-display (fn [options] {:open! (constantly {}) :close! (fn [_])
                                                                       :project! (:project! options)})]
                  (show/push-browser-display runtime {}))
        project (:project! display)
        handle ((:open! display))]
    (try
      (with-redefs [response/compile-response (counted-response calls)]
        (let [{:keys [instance-id status]} (command-at! runtime 0 {:op :cue/launch :cue-id :ember})
              _ (reset! calls [])
              _ (show/tick! runtime 125)
              snap @(:state runtime)
              instance (get-in snap [:model :instances instance-id])
              view (assoc (push/display-view {:selected-instance instance-id :selected-cue :ember
                                              :page-id :main :origin [0 0]} (:model snap))
                          :display-mode :parameters)]
          (is (= :accepted status))
          (is (= [0.25] @calls))
          (is (= 0.175 (get-in (studio/stream-view runtime snap) [:instances 0 :parameter-sources :level :effective-value])))
          (is (= 0.175 (:effective-value (first (filter #(= :level (:param %)) (:parameters (project view)))))))
          (is (= 45 (Byte/toUnsignedInt (aget ^bytes (get-in snap [:output :universes 1]) 0))))
          (is (seq (:stages (runtime/observe instance [[:outputs :color] :head-1]))))
          (is (= [0.25] @calls))
          (is (= 0.1 (:level (waveforms/effective-params
                              (assoc-in snap [:resolved-param-overrides instance-id :level] 0.4) instance))))
          (is (= 0.35 (:level (waveforms/effective-params snap (assoc-in instance [:system :last-clock :beat] 0.5)))))
          (is (= 0.175 (:level (waveforms/effective-params (update snap :transport-generation inc) instance))))
          (is (= 0.175 (:level (waveforms/effective-params
                                (assoc-in snap [:model :epoch] 1) instance))))
          (is (= 0.175 (:level (waveforms/effective-params
                                snap (assoc-in instance [:definition :params :level :bounds] [0 2])))))
          (is (= [0.25 0.25 0.5 0.25 0.25 0.25] @calls))
          (is (= :accepted (:status (command-at! runtime 175 {:op :param/set :instance-id instance-id
                                                              :param :level :value 0.4}))))
          (is (near? 0.14 (:effective-value (first (filter #(= :level (:param %)) (:parameters (project view)))))))
          (is (= 0.35 (last @calls)))
          (is (= 7 (count @calls)))))
      (finally ((:close! display) handle) (show/stop! runtime)))))

(deftest response-samples-are-retained-only-while-a-display-reads-them
  (let [model (response-show identity)
        runtime (show/create-runtime {:model model :workspace-options nil :worker-timeout-ms 10000})
        display (with-redefs [display/browser-display (fn [_] {:open! (constantly {:sink :open}) :close! (fn [_])})]
                  (show/push-browser-display runtime {}))
        sample (fn []
                 (let [snap @(:state runtime)
                       instance (first (vals (get-in snap [:model :instances])))]
                   [(some? (::waveforms/response-frame snap))
                    (some? (runtime/observe instance [::waveforms/response-inputs nil]))]))]
    (try
      (is (= :accepted (:status (command-at! runtime 0 {:op :cue/launch :cue-id :ember}))))
      (show/tick! runtime 125)
      (testing "no reader leaves the owner frame without a retained sample"
        (is (= [false false] (sample))))
      (let [handle ((:open! display))]
        (is (= :open (:sink handle)))
        (show/tick! runtime 150)
        (testing "an open Push display leases exact response inputs"
          (is (= [true true] (sample))))
        ((:close! display) handle)
        ((:close! display) handle))
      (show/tick! runtime 175)
      (testing "closing the display releases the lease once"
        (is (empty? @(:response-sample-readers runtime)))
        (is (= [false false] (sample))))
      (testing "a failed Studio bind does not leak its lease"
        (is (thrown? clojure.lang.ExceptionInfo
                     (with-redefs [state-stream/publish! (fn [& _] (throw (ex-info "publish failed" {})))]
                       (studio/handler runtime))))
        (is (empty? @(:response-sample-readers runtime))))
      (studio/handler runtime)
      (let [clients (get-in @(:observers runtime) [:studio :state-stream :clients])]
        (show/tick! runtime 200)
        (is (= [false false] (sample)) "an idle Studio stream does not demand samples")
        (swap! clients assoc :socket (atom {}))
        (show/tick! runtime 225)
        (is (= [true true] (sample)) "a connected Studio client demands samples")
        (swap! clients dissoc :socket)
        (show/tick! runtime 250)
        (is (= [false false] (sample)) "the last client leaving drops demand")
        (show/detach-observer! runtime :studio)
        (is (empty? @(:response-sample-readers runtime)) "closing the Studio observer releases its lease"))
      (finally (show/stop! runtime)))))

(deftest a-throwing-response-sample-predicate-does-not-fault-the-tick
  (let [runtime (show/create-runtime {:model (response-show identity) :workspace-options nil :worker-timeout-ms 10000})
        _ (show/lease-response-samples! runtime #(throw (ex-info "earlier reader failed" {:reader :earlier})))
        broken (show/lease-response-samples! runtime #(throw (ex-info "reader failed" {:reader :broken})))]
    (try
      (is (= :accepted (:status (command-at! runtime 0 {:op :cue/launch :cue-id :ember}))))
      (let [before (get-in @(:state runtime) [:model :frame-number])
            _ (show/tick! runtime 125)
            _ (show/tick! runtime 150)
            snap @(:state runtime)]
        (is (nil? (get-in snap [:status :error])))
        (is (= (+ 2 before) (get-in snap [:model :frame-number])) "the owner kept committing frames")
        (is (nil? (::waveforms/response-frame snap)) "a failing predicate counts as not demanded"))
      (is (= #{{:message "earlier reader failed" :details {:reader :earlier} :count 3}
               {:message "reader failed" :details {:reader :broken} :count 3}}
             (set (show/response-sample-faults runtime))) "one bounded record per lease")
      (is (= #{{:message "earlier reader failed" :count 3} {:message "reader failed" :count 3}}
             (set (get-in (show/snapshot runtime) [:status :response-sample-faults])))
          "Studio's status projection carries the same records, without their ex-data")
      (let [release (show/lease-response-samples! runtime)]
        (show/tick! runtime 175)
        (is (some? (::waveforms/response-frame @(:state runtime))) "other leases still demand samples")
        (is (= #{4} (set (map :count (show/response-sample-faults runtime))))
            "every lease is polled even when one already demands samples")
        (release))
      (broken)
      (is (= [:earlier] (map (comp :reader :details) (show/response-sample-faults runtime))))
      (testing "an interrupted reader restores the thread's interrupt flag"
        (let [release (show/lease-response-samples! runtime #(throw (ex-info "wrapped" {} (InterruptedException.))))]
          (try
            (show/tick! runtime 200)
            (is (Thread/interrupted) "interrupt flag restored, then cleared here")
            (finally (release)))))
      (finally (show/stop! runtime)))))

(deftest virtual-machine-errors-from-a-response-sample-predicate-propagate
  (let [runtime (show/create-runtime {:model (response-show identity) :workspace-options nil :worker-timeout-ms 10000})]
    (try
      (is (= :accepted (:status (command-at! runtime 0 {:op :cue/launch :cue-id :ember}))))
      (show/lease-response-samples! runtime #(throw (StackOverflowError.)))
      (is (thrown? StackOverflowError (show/tick! runtime 125)))
      (is (empty? (show/response-sample-faults runtime)) "an Error is not recorded as a reader fault")
      (finally (show/stop! runtime)))))

(deftest forecast-and-fault-observations-keep-their-own-response-semantics
  (let [forecast (response-show #(assoc % :keyframes {:interval {:beats 1}}))
        fails-at-zero (response-show #(assoc-in % [:outputs :dimmer]
                                               {:op :divide :args [0.1 {:op :param :key :level}]}))]
    (doseq [[model fault?] [[forecast false] [fails-at-zero true]]]
      (let [runtime (show/create-runtime {:model model :workspace-options nil :worker-timeout-ms 10000
                                          :effect-log! (fn [_])})
            _ (show/lease-response-samples! runtime)
            calls (atom [])]
        (try
          (with-redefs [response/compile-response (counted-response calls)]
            (let [{:keys [instance-id status]} (command-at! runtime 125 {:op :cue/launch :cue-id :ember})]
              (is (= :accepted status))
              (if fault?
                (let [reply (command-at! runtime 175 {:op :param/set :instance-id instance-id :param :level :value 0.0})
                      snapshot @(:state runtime)
                      instance (get-in snapshot [:model :instances instance-id])]
                  (is (= :accepted (:status reply)))
                  (is (= :evaluate (get-in instance [:fault :stage])))
                  (is (= [0.25 0.35] @calls))
                  (is (zero? (:level (waveforms/effective-params snapshot instance))))
                  (is (= [0.25 0.35 0.35] @calls)))
                (do
                  (is (= [0.25 1.25] @calls))
                  (show/tick! runtime 250)
                  (let [snapshot @(:state runtime)
                        instance (get-in snapshot [:model :instances instance-id])]
                    (is (= [0.25 1.25] @calls))
                    (is (= 0.35 (:level (waveforms/effective-params snapshot instance))))
                    (is (= [0.25 1.25 0.5] @calls)))))))
          (finally (show/stop! runtime)))))))

(deftest clock-bound-shape-inputs-use-the-captured-private-frame
  (let [snapshot (assoc-in (fixture) [:model :instances "0:1" :definition :params :waveform :choices 0 :params :duty]
                           {:op :beat :clock :absolute})
        cache (waveforms/create-cache) request {:instance-id "0:1" :parameter :waveform :options {:samples 5}}
        first (waveforms/cached-preview cache snapshot request)
        later (waveforms/cached-preview cache (assoc-in snapshot [:model :instances "0:1" :system :last-clock :beat] 0.8) request)]
    (is (= [1.0 1.0 0.0 0.0 1.0] (:preview-samples first)))
    (is (= [1.0 1.0 1.0 1.0 1.0] (:preview-samples later)))
    (is (not= (get-in first [:identity :context-digest]) (get-in later [:identity :context-digest])))))

(deftest draft-bindings-options-and-cache-invalidate-with-effective-shape
  (let [snapshot (fixture) cache (waveforms/create-cache)
        request {:instance-id "0:1" :parameter :waveform :options {:samples 5}}
        first (waveforms/cached-preview cache snapshot request)
        changed (waveforms/cached-preview cache (assoc-in snapshot [:model :instances "0:1" :params :width] 0.8) request)
        draft (waveforms/preview snapshot {:selection {:selector 0 :choices [{:id "pulse" :version 1 :params {:duty (fx/param :width)}}]}
                                          :context-params {:width 0.8} :options {:samples 5}})]
    (is (not= (:preview-samples first) (:preview-samples changed)))
    (is (= (:preview-samples changed) (:preview-samples draft)))
    (is (identical? changed (waveforms/cached-preview cache (assoc-in snapshot [:model :instances "0:1" :params :width] 0.8)
                                                      (assoc request :input-key "client-only"))))
    (doseq [i (range 70)]
      (waveforms/cached-preview cache snapshot {:id :sine :options {:samples 5 :start-phase (/ i 100.0)}}))
    (is (= 64 (count (:entries @cache)) (count (:order @cache)))))
  (is (thrown? clojure.lang.ExceptionInfo (waveforms/preview (fixture) {:id :sine :options {:samples 4096}}))))

(defn sine [hz n & [offset]]
  (double-array (map #(* 0.75 (Math/sin (* 2 Math/PI hz (/ (+ (or offset 0) %) 48000.0)))) (range n))))

(defn block [samples start & [extra]]
  (merge {:samples samples :sample-rate 48000 :channels 1 :source-id "capture" :started-at-ms start} extra))

(deftest measured-pcm-min-max-rms-and-frequency-colors-have-physical-basis
  (doseq [[hz expected] [[50 :low] [1000 :mid] [10000 :high]]]
    (let [state (waveforms/capture-block (waveforms/new-capture) (block (sine hz 48000) 0))
          view (waveforms/capture-view state) bin (last (:bins view))]
      (is (= 50 (count (:bins view))))
      (is (= expected (key (apply max-key val (:bands bin)))))
      (is (< -0.76 (:min bin) -0.6))
      (is (< 0.6 (:max bin) 0.76))
      (is (< 0.5 (:rms bin) 0.55))
      (is (= 1000.0 (:duration-ms view)))))
  (let [zero (waveforms/capture-view (waveforms/capture-block (waveforms/new-capture) (block (double-array 480) 0)))]
    (is (zero? (get-in zero [:bins 0 :rms])))
    (is (true? (get-in zero [:bins 0 :partial?])))))

(deftest summary-carries-partial-blocks-but-never-invents-a-gap
  (let [a (waveforms/capture-block (waveforms/new-capture {:max-bins 3}) (block (sine 50 480) 0))
        b (waveforms/capture-block a (block (sine 50 480 480) 10))
        one (waveforms/capture-block (waveforms/new-capture {:max-bins 3}) (block (sine 50 960) 0))
        gap (waveforms/capture-block b (block (sine 50 480) 200))
        full (waveforms/capture-block (waveforms/new-capture {:max-bins 3}) (block (sine 50 48000) 0))]
    (is (= (:bins b) (:bins one)))
    (is (= 3 (count (:bins (waveforms/capture-view full)))))
    (is (= 2 (:generation gap)))
    (is (= 200.0 (:start-time-ms (waveforms/capture-view gap))))
    (is (= 10.0 (:duration-ms (waveforms/capture-view gap))))
    (is (= 480 (:sample-end gap)))))

(deftest beat-markers-follow-authoritative-clock-and-fractional-bars
  (let [clock {:time-ms 0 :beat 0 :bpm 120 :source :link :source-epoch 7 :epoch 2
               :meter {:beats-per-bar 7/2 :bars-per-phrase 4}}
        project (fn [options]
                  (waveforms/capture-view
                   (waveforms/capture-block (waveforms/new-capture)
                                           (block (double-array 96000) 0 options))))
        view (project {:clock clock})
        fallback (project {:clock (dissoc clock :source-epoch)})
        invalid (project {:clock clock :timing-valid? false})
        stopped (project {:clock (assoc clock :running? false)})]
    (is (= [0.0 1.0 2.0 3.0 3.5] (mapv :beat (:beat-markers view))))
    (is (= [0.0 1750.0] (mapv :offset-ms (filter :bar? (:beat-markers view)))))
    (is (= #{[:link 7]} (into #{} (map (juxt :source :epoch)) (:beat-markers view))))
    (is (= #{[:link 2]} (into #{} (map (juxt :source :epoch)) (:beat-markers fallback))))
    (is (empty? (:beat-markers invalid)))
    (is (empty? (:beat-markers stopped)))
    (is (false? (:timing-valid? invalid)))))

(deftest actual-audio-manager-publishes-bounded-pcm-and-resets-new-source
  (let [closed (atom 0) current (atom (repeat 4800 16384))
        manager (audio/create-manager {:open-source! (fn [_] (pcm/memory-source @current 47 closed))
                                       :now-ms (constantly 200)})]
    (try
      (is (false? (:available? (audio/waveform manager))))
      (let [run (audio/start! manager pcm/config {:events! identity
                                                :clock! (constantly {:time-ms 0 :beat 0 :bpm 120 :source :internal})})]
        (is (= :completed (:status (deref (:result run) 3000 :timeout))))
        (let [view (audio/waveform manager)]
          (is (= 5 (count (:bins view))))
          (is (= (:run-id run) (:source-id view)))
          (is (every? #(near? 0.5 (:rms %)) (:bins view)))
          (is (= 4800 (:sample-end view)))))
      (reset! current (take 960 (cycle [16384 -16384])))
      (let [run (audio/start! manager (assoc pcm/config :channels 2) {:events! identity})]
        (is (= :completed (:status (deref (:result run) 3000 :timeout))))
        (is (= 2 (:channels (audio/waveform manager))))
        (is (= :mono-average (:mix (audio/waveform manager))))
        (is (not (contains? (audio/waveform manager) :input-channel)))
        (is (every? #(zero? (:rms %)) (:bins (audio/waveform manager)))))
      (finally (audio/close! manager)))
    (is (= 2 @closed))))

(deftest a-different-selected-channel-resets-waveform-history
  (let [initial (waveforms/capture-block (waveforms/new-capture)
                                        (block (double-array 480 0.75) 0 {:channels 4 :input-channel 2}))
        changed (waveforms/capture-block initial
                                        (block (double-array 480 -0.25) 10 {:channels 4 :input-channel 3}))
        view (waveforms/capture-view changed)]
    (is (= {:channels 4 :input-channel 3 :mix :selected-channel :generation 2}
           (select-keys view [:channels :input-channel :mix :generation])))
    (is (= 10.0 (:start-time-ms view)))
    (is (= [0.25] (mapv :rms (:bins view))))))

(deftest actual-push-view-is-enriched-on-capture-worker-and-stale-selection-clears
  (let [snapshot (fixture) cache (waveforms/create-cache) captured (promise) done (CountDownLatch. 1)
        caller (Thread/currentThread) projected-thread (atom nil)
        view (push/display-view {:selected-instance "0:1" :selected-cue :shape :page-id :main :origin [0 0]} (:model snapshot))
        audio-view (waveforms/capture-view (waveforms/capture-block (waveforms/new-capture) (block (sine 1000 960) 0)))
        transport (display/transport
                   {:project! (fn [view] (reset! projected-thread (Thread/currentThread))
                                (waveforms/project-display cache snapshot view audio-view))
                    :capture {:open! (constantly :capture) :close! (fn [_])
                              :capture! (fn [_ view] (deliver captured view) (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB))}
                    :sink {:open! (constantly :sink) :close! (fn [_]) :render! (fn [_ _] (.countDown done))}})
        handle ((:open! transport))]
    (try
      ((:render! transport) handle view)
      (is (.await done 2 TimeUnit/SECONDS))
      (is (not (identical? caller @projected-thread)))
      (let [waveform (first (filter #(= :waveform (:param %)) (:parameters @captured)))]
        (is (= 65 (count (:preview-samples waveform))))
        (is (= :ready (:preview-status waveform)))
        (is (= 0.0 (:base-value waveform) (:effective-value waveform))))
      (is (= (waveforms/display-audio audio-view) (:audio-waveform @captured)))
      (let [changed (waveforms/project-display cache (assoc-in snapshot [:model :instances "0:1" :params :width] 0.8) view nil)
            width (first (filter #(= :width (:param %)) (:parameters changed)))]
        (is (= 0.8 (:value width) (:base-value width) (:effective-value width))))
      (let [stale (waveforms/project-display cache (assoc-in snapshot [:model :epoch] 1) view nil)]
        (is (nil? (:visualization-clock stale)))
        (is (every? #(not (contains? % :value)) (:parameters stale)))
        (is (= :stale (:preview-status (first (filter #(= :waveform (:param %)) (:parameters stale)))))))
      (finally ((:close! transport) handle)))))

(deftest native-push-displays-the-same-live-parameter-projection
  ;; The first draw initializes AWT fonts. Do that before the bounded wait for a worker frame.
  (transports/render-view! (BufferedImage. display/width display/height BufferedImage/TYPE_INT_RGB)
                           {:title "Push 2"})
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})
        definition (-> (get-in (fixture) [:model :instances "0:1" :definition])
                       (assoc :targets [:head-1])
                       (assoc-in [:params :level] (fx/parameter (/ 127.0 255) {:bounds [0 1]}))
                       (assoc-in [:params :pan] (fx/parameter (/ 32767.0 65535) {:bounds [0 1]}))
                       (update-in [:outputs 0 :value] #(fx/multiply % (fx/param :level)))
                       (update :outputs conj (fx/output :pan (fx/param :pan))))
        caller (Thread/currentThread)
        rendered (atom nil)
        image (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB)
        first-frame (promise) updated-frame (promise) stale-frame (promise)
        hardware-calls (atom [])
        trace-pixel (.getRGB (Color. 105 192 236))
        frame! (fn []
                 (let [view @rendered
                       rows (into {} (map (juxt :param identity)) (:parameters view))
                       trace (:waveform rows)
                       frame {:rows rows :thread (Thread/currentThread) :mode (:display-mode view)
                              :trace (mapv #(= trace-pixel %) (.getRGB image 0 115 960 18 nil 0 960))}]
                   (cond
                     (= :stale (:preview-status trace)) (deliver stale-frame frame)
                     (= 0.25 (get-in rows [:width :value])) (deliver first-frame frame)
                     (= 0.75 (get-in rows [:width :value])) (deliver updated-frame frame))))
        backend {:open! #(do (swap! hardware-calls conj :open) image)
                 :send! frame!
                 :close! #(swap! hardware-calls conj :close)}
        render! transports/render-view!
        apply! (fn [type payload time]
                 (let [receipt (show/submit! runtime type payload)]
                   (show/tick! runtime time) @receipt))
        options {:session-id "native-push"
                 :native-display? true
                 :midi {:open! (constantly :midi) :send! (fn [_ _]) :close! (fn [_])}
                 :schedule! (fn [_] (fn []))}]
    (try
      (is (= :accepted (:status (apply! :settings {:scope :controllers
                                                  :values {:push-display {:mode :pages :parameter-view :history}}} 0))))
      (is (= :accepted (:status (apply! :definition {:kind :effects :definition definition} 0))))
      (is (= :accepted (:status (apply! :command {:op :cue/put :cue {:id :shape :effect-id :shape}} 0))))
      (let [id (:instance-id (apply! :command {:op :cue/launch :cue-id :shape} 0))
            project-browser (:project! (with-redefs [display/browser-display identity] (show/push-browser-display runtime {})))
            browser-view (push/display-view {:selected-instance id :selected-cue :shape :page-id :main :origin [0 0]}
                                            (:model @(:state runtime)))
            browser-rows #(into {} (map (juxt :param identity))
                                (:parameters (project-browser (assoc browser-view :display-mode :parameters))))]
        (is (= :pages (:display-mode (project-browser browser-view))))
        (with-redefs-fn {#'transports/native-wayang (constantly backend)
                         #'transports/render-view! (fn [image view]
                                                     (reset! rendered view)
                                                     (render! image view))}
          #(show/connect-device! runtime :push-2 options))
        (let [before (deref first-frame 2000 {})
              studio-instance (some #(when (= id (:id %)) %) (:instances (show/snapshot runtime)))]
          (is (= :parameters (:mode before)))
          (is (= (browser-rows) (:rows before)))
          (is (= [8 16 8] (mapv #(get-in before [:rows % :schema :dmx-bits]) [:level :pan :width])))
          (is (= [8 16 8] (mapv #(get-in studio-instance [:definition :params % :dmx-bits]) [:level :pan :width])))
          (is (= 65 (count (get-in before [:rows :waveform :preview-samples]))))
          (is (some true? (:trace before)))
          (is (not (identical? caller (:thread before))))
          (let [changes {:width 0.75 :level (/ 128.0 255) :pan (/ 32768.0 65535)}
                receipts (mapv (fn [[parameter value]]
                                 (show/submit! runtime {:op :param/set :instance-id id :param parameter :value value})) changes)]
            (show/tick! runtime 50)
            (is (every? #(= :accepted (:status @%)) receipts)))
          (let [after (deref updated-frame 2000 {})]
            (is (= (browser-rows) (:rows after)))
            (is (not= (get-in before [:rows :waveform :preview-samples])
                      (get-in after [:rows :waveform :preview-samples])))
            (is (some true? (:trace after)))
            (is (not= (:trace before) (:trace after)))))
        (is (= :accepted (:status (apply! :command {:op :instance/end :instance-id id :force? true} 100))))
        (let [stale (deref stale-frame 2000 {})]
          (is (= [] (get-in stale [:rows :waveform :preview-samples])))
          (is (not-any? true? (:trace stale)))))
      (show/disconnect-device! runtime "native-push")
      (is (= [:open :close] @hardware-calls))
      (let [supplied (promise)
            display {:open! (constantly :display) :render! (fn [_ view] (deliver supplied view)) :close! (fn [_])}]
        (show/connect-device! runtime :push-2 (assoc options :display display))
        (is (= :pages (:display-mode (deref supplied 2000 nil)))))
      (finally (show/stop! runtime)))))

(deftest compact-push-audio-retains-extrema-energy-and-clock-identity
  (let [source {:bins (vec (repeat 256 {:min -0.8 :max 0.7 :rms 0.5 :samples 960
                                      :bands {:low 0.4 :mid 0.2 :high 0.1}}))
                :duration-ms 5120 :generation 3 :beat-markers [{:offset-ms 500 :beat 1 :bar? false}]}
        projected (waveforms/display-audio source)]
    (is (= 128 (count (:bins projected))))
    (is (= {:min -0.8 :max 0.7 :rms 0.5 :bands {:low 0.4 :mid 0.2 :high 0.1}}
           (first (:bins projected))))
    (is (= (dissoc source :bins) (dissoc projected :bins)))))

(deftest display-reprojects-live-audio-without-a-new-model-view
  (let [snapshot (fixture) cache (waveforms/create-cache)
        initial (waveforms/capture-block (waveforms/new-capture) (block (sine 70 960) 0))
        audio-state (atom initial) entered (CountDownLatch. 1) release (CountDownLatch. 1)
        updated (promise) seen (atom 0)
        view (push/display-view {:selected-instance "0:1" :selected-cue :shape :origin [0 0]} (:model snapshot))
        transport (display/transport
                    {:max-fps 60
                     :project! #(waveforms/project-display cache snapshot % (waveforms/capture-view @audio-state))
                     :capture {:open! (constantly :capture) :close! (fn [_])
                               :capture! (fn [_ view]
                                           (if (= 1 (swap! seen inc))
                                             (do (.countDown entered) (.await release))
                                             (deliver updated view))
                                           (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB))}
                     :sink {:open! (constantly :sink) :close! (fn [_]) :render! (fn [_ _])}})
        handle ((:open! transport))]
    (try
      ((:render! transport) handle view)
      (is (.await entered 2 TimeUnit/SECONDS))
      (reset! audio-state (waveforms/capture-block initial (block (sine 1000 960 960) 20)))
      (.countDown release)
      (is (= 1920 (get-in (deref updated 2000 {}) [:audio-waveform :sample-end])))
      (is (= 1 (:submitted ((:status! transport) handle))))
      (finally (.countDown release) ((:close! transport) handle)))))

(deftest live-preview-through-ring-reflects-owner-edits-without-an-extra-frame
  (let [runtime (show/create-runtime {:workspace-options nil})
        definition (get-in (fixture) [:model :instances "0:1" :definition])
        apply! (fn [type payload t]
                 (let [receipt (show/submit! runtime type payload)]
                   (show/tick! runtime t) @receipt))
        preview! (fn [id]
                   (let [result ((studio/handler runtime)
                                 {:request-method :post :uri "/api/waveforms/preview" :headers {"host" "localhost"}
                                  :body (ByteArrayInputStream.
                                         (.getBytes (json/write-str {:instance-id id :parameter "waveform"
                                                                    :context-params {:width 1.0} :options {:samples 5}}) "UTF-8"))})]
                     (is (= 200 (:status result)))
                     (json/read-str (:body result) :key-fn keyword)))]
    (try
      (is (= :accepted (:status (apply! :definition {:kind :effects :definition definition} 0))))
      (is (= :accepted (:status (apply! :command {:op :cue/put :cue {:id :shape :effect-id :shape :params {}}} 0))))
      (let [id (:instance-id (apply! :command {:op :cue/launch :cue-id :shape} 0))
            before @(:state runtime) first (preview! id)]
        (is (= [1.0 0.0 0.0 0.0 1.0] (:preview-samples first)))
        (is (identical? before @(:state runtime)))
        (is (= :accepted (:status (apply! :command {:op :param/set :instance-id id :param :width :value 0.8} 100))))
        (is (= [1.0 1.0 1.0 1.0 1.0] (:preview-samples (preview! id)))))
      (finally (show/stop! runtime)))))

(deftest spatial-and-history-view-use-the-committed-frame-and-device-overrides
  (let [snapshot (-> (fixture)
                     (assoc-in [:model :time-ms] 2000.0)
                     (assoc-in [:model :transport :beat] 4.0)
                     (assoc-in [:model :settings :controllers :push-display]
                               {:mode :audio :parameter-view :spatial :history-beats 8}))
        plain (waveforms/project-display (waveforms/create-cache) snapshot {:parameters []} nil)
        device (waveforms/project-display (waveforms/create-cache) snapshot
                                          {:parameters [] :display-mode :parameters :parameter-view :history :history-beats 0.5} nil)]
    (is (= {:beat 4.0 :time-ms 2000.0} (:visualization-clock plain)))
    (is (= [:audio :spatial 8] ((juxt :display-mode :parameter-view :history-beats) plain)))
    (is (= [:parameters :history 0.5] ((juxt :display-mode :parameter-view :history-beats) device)))))

(deftest push-history-reads-only-an-eligible-displayed-block-target
  (let [calls (atom [])
        snapshot (-> (fixture)
                     (assoc-in [:model :rig :fixtures] [{:id :a} {:id :b :name "Beam B"} {:id :c}])
                     (update-in [:model :instances "0:1"]
                                (fn [instance]
                                  (-> instance
                                      (assoc :targets [:c :b :a])
                                      (assoc-in [:definition :targets] [:b :c])
                                      (assoc-in [:definition :outputs] [{:attribute :dimmer :targets [:b]}])
                                      (assoc-in [:definition :blocks [:params :width]] [:optics/water])
                                      (vary-meta assoc ::runtime/observe
                                                 (fn [query] (swap! calls conj query) {:result 0.85}))))))
        config {:instance-id "0:1" :epoch 0 :definition-version 1 :parameters [{:param :width}]}
        read-row (fn [state view] (first (:parameters (waveforms/project-display (waveforms/create-cache) state view nil))))]
    (is (= {:id :b :name "Beam B"} (:sample-fixture (read-row snapshot config))))
    (is (= [[[:params :width] :b]] @calls))
    (is (= 0.85 (:effective-value (read-row snapshot (assoc config :target-id :a)))))
    (reset! calls [])
    (doseq [v [(assoc config :display-mode :audio) (assoc config :target-picker {:open? true})
               (assoc config :epoch 1) (assoc config :parameters [{:param :other}])]]
      (is (nil? (:source-state (read-row snapshot v)))))
    (let [empty (assoc-in snapshot [:model :instances "0:1" :targets] [])]
      (is (= :pending (:source-state (read-row empty config)))))
    (is (empty? @calls))))

(deftest a-static-push-bank-needs-no-source-or-response-evaluation
  (let [config {:instance-id "0:1" :epoch 0 :definition-version 1 :parameters [{:param :width}]}]
    (with-redefs [waveforms/resolved-inputs (fn [& _] (throw (AssertionError. "Static bank should read stored values")))]
      (let [row (first (:parameters (waveforms/project-display (waveforms/create-cache) (fixture) config nil)))]
        (is (= 0.25 (:base-value row) (:effective-value row)))
        (is (nil? (:source-state row)))))))

(deftest an-unchanged-target-bank-scans-the-rig-once
  (let [scans (atom 0)
        counted (fn [rear-name]
                  (let [fixtures [{:id :front} {:id :rear :name rear-name}]]
                    (reify clojure.lang.IReduceInit
                      (reduce [_ f init] (swap! scans inc) (reduce f init fixtures)))))
        state (-> (fixture)
                  (assoc-in [:model :rig :fixtures] (counted "Rear"))
                  (assoc-in [:model :instances "0:1" :targets] [:rear :front])
                  (assoc-in [:model :instances "0:1" :definition :blocks [:params :width]] [:optics/water])
                  (update-in [:model :instances "0:1"] vary-meta assoc ::runtime/observe (constantly {:result 0.5})))
        view {:instance-id "0:1" :epoch 0 :definition-version 1 :parameters [{:param :width}]}
        cache (waveforms/create-cache)
        target (fn [state view] (get-in (waveforms/project-display cache state view nil) [:parameters 0 :sample-fixture]))]
    (is (= {:id :rear :name "Rear"} (target state view) (target state view)))
    (waveforms/cached-preview cache state {:instance-id "0:1" :parameter :waveform})
    (is (= :rear (:id (target state view))))
    (is (= 1 @scans))
    (is (= :front (:id (target state (assoc view :target-id :front)))))
    (is (= :front (:id (target (assoc-in state [:model :instances "0:1" :targets] [:front]) view))))
    (let [renamed (assoc-in state [:model :rig :fixtures] (counted "South rear"))]
      (is (= {:id :rear :name "South rear"} (target renamed view) (target renamed view))))
    (is (= 4 @scans))))
