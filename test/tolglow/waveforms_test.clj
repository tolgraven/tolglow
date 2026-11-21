(ns tolglow.waveforms-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.waveforms :as waveforms]
            [tolglow.audio.capture :as audio]
            [tolglow.audio.capture-test :as pcm]
            [tolglow.studio.display :as display]
            [tolglow.signals :as signals]
            [tolglow.effects :as fx]
            [tolglow.compiler :as compiler]
            [tolglow.effects-test :as recipes]
            [afterglow.control.devices.push-2 :as push])
  (:import [java.awt.image BufferedImage]
           [java.io ByteArrayInputStream]
           [java.util.concurrent CountDownLatch TimeUnit]))

(defn near? [a b] (< (Math/abs (double (- a b))) 1.0e-8))
(defn fixture []
  (let [registered (signals/new-registry)
        schema (signals/selector-schema registered [{:id :pulse :params {:duty (fx/param :width)}} :sine])
        effect (fx/effect :shape {:params {:waveform schema :width (fx/parameter 0.25 {:bounds [0 1]})}}
                          (fx/intensity (signals/select-waveform registered (fx/param :waveform) (:choices schema)
                                                                 {:phase (fx/cycles {:beats 1})})))
        instance {:id "0:1" :effect-id :shape :effect-version 1 :definition effect :cue-id :shape
                  :phase :running :params {:waveform 0.0 :width 0.25} :state {:ticks 3} :started-at-ms 0
                  :started-at-beat 0 :system {:last-clock {:time-ms 250 :beat 0.5}}}]
    {:model {:epoch 0 :time-ms 250 :transport {:bpm 120 :beat 0.5 :playing? true}
             :waveforms registered :instances {"0:1" instance}
             :registry {:effects {:shape effect} :cues {:shape {:id :shape :effect-id :shape}}}}
     :modulation-view {:overrides {}}}))

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
  (let [clock {:time-ms 0 :beat 0 :bpm 120 :source :link :epoch 2 :meter {:beats-per-bar 7/2 :bars-per-phrase 4}}
        view (waveforms/capture-view (waveforms/capture-block (waveforms/new-capture)
                                                             (block (double-array 96000) 0 {:clock clock})))
        invalid (waveforms/capture-view (waveforms/capture-block (waveforms/new-capture)
                                                                (block (double-array 96000) 0 {:clock clock :timing-valid? false})))
        stopped (waveforms/capture-view (waveforms/capture-block (waveforms/new-capture)
                                                                (block (double-array 96000) 0 {:clock (assoc clock :running? false)})))]
    (is (= [0.0 1.0 2.0 3.0 3.5] (mapv :beat (:beat-markers view))))
    (is (= [0.0 1750.0] (mapv :offset-ms (filter :bar? (:beat-markers view)))))
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
        (is (every? #(zero? (:rms %)) (:bins (audio/waveform manager)))))
      (finally (audio/close! manager)))
    (is (= 2 @closed))))

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
        (is (= :stale (:preview-status (first (filter #(= :waveform (:param %)) (:parameters stale)))))))
      (finally ((:close! transport) handle)))))

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
