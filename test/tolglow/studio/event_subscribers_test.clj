(ns tolglow.studio.event-subscribers-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.studio :as studio]
            [tolglow.clock :as clock]
            [tolglow.studio.demo :as demo]
            [tolglow.studio.event-subscribers :as sub]
            [tolglow.studio.persistence :as files]
            [tolglow.studio.transit :as wire]))

(def kick {:category :audio :type :audio/kick :emitter {:kind :audio :id :analysis}
           :data {:confidence 0.92} :time {:beat 0}})
(def launch {:op :cue/launch :select {:ids [:ember :ice] :pick :random} :launch {:mode :one-shot}})
(defn listener [actions] {:id :kicks :enabled? true :match {:type :audio/kick} :actions actions})
(defn configured [actions] (sub/configure (demo/model) {:op :save :subscriber (listener actions)}))

(deftest bounded-registry-candidates-exclude-show-control-and-respect-page-and-ids
  (let [model (-> (demo/model)
                  (assoc-in [:registry :effects :code] {:id :code :role :show-control})
                  (assoc-in [:registry :cues :code] {:id :code :effect-id :ember :kind :code-cue})
                  (assoc-in [:registry :cues :also-code] {:id :also-code :effect-id :code})
                  (assoc-in [:pages :small :cells] {[1 1] :ember [1 2] :code}))]
    (is (= [:ember] (sub/candidates model {:op :cue/launch :select {:page-id :small}})))
    (is (= [:ember] (sub/candidates model {:op :effect/launch :select {:page-id :small}})))
    (is (not-any? #{:code :also-code} (sub/candidates model {:op :cue/launch})))
    (is (empty? (sub/candidates model {:op :cue/launch :select {:ids []}})))
    (is (empty? (sub/candidates model {:op :cue/launch :select {:categories []}})))
    (is (empty? (sub/candidates model {:op :cue/launch :select {:page-id :unknown}})))
    (is (= (set (sub/candidates model {:op :effect/launch}))
           (set (get-in (sub/catalog model) [:candidates :effects]))))))

(deftest matching-randomness-delay-replay-and-feedback
  (let [model (configured [launch {:op :instance/opacity :select {:pick :all} :value 0 :delay {:bars 1/2}}
                           {:op :instance/kill :select {:pick :all} :delay {:beats 1/2}}])
        admitted (sub/admit model [kick (assoc kick :type :audio/energy)
                                   (assoc-in kick [:emitter :id] :event-subscriber)])
        execute #(sub/advance % 0 {})
        started (execute (:model admitted))
        id (ffirst (:instances started))
        step #(sub/advance (assoc-in %1 [:transport :beat] %2) 10 {})]
    (is (= {0 [:kicks]} (:deliveries admitted)))
    (is (= started (execute (:model admitted))))
    (is (#{:ember :ice} (get-in started [:instances id :cue-id])))
    (is (= {:beats 1} (get-in started [:instances id :duration])))
    (is (= 1.0 (get-in (step started 1) [:instances id :system :opacity])))
    (is (zero? (get-in (step started 2) [:instances id :system :opacity])))
    (is (empty? (:instances (step started 5/2))))
    (let [rebased (clock/rebase started {:beat 100})]
      (is (== (+ 100 (get-in started [:event-subscribers :runs 0 :due-beat]))
             (get-in rebased [:event-subscribers :runs 0 :due-beat]))))
    (let [restored (files/thaw model (files/read-string (files/write-string started)))]
      (is (= (get-in started [:event-subscribers :runs]) (get-in restored [:event-subscribers :runs])))
      (is (= (get-in started [:event-subscribers :seed]) (get-in restored [:event-subscribers :seed]))))))

(deftest bounded-admission-and-stale-edits
  (let [model (configured [launch])
        overflow (:model (sub/admit model (vec (repeat 300 kick))))]
    (is (= 256 (count (get-in overflow [:event-subscribers :runs]))))
    (is (= 44 (get-in overflow [:event-subscribers :dropped])))
    (is (thrown? Exception (sub/configure model {:op :save :expected-subscriber nil :subscriber (listener [launch])})))
    (is (empty? (get-in (sub/configure model {:op :remove :id :kicks :expected-subscriber (listener [launch])})
                        [:event-subscribers :subscribers])))
    (is (thrown? Exception (sub/configure model {:op :save :subscriber (listener [{:op :instance/opacity :value 2}])})))
    (is (thrown? Exception (sub/configure model {:op :save :subscriber (listener [(assoc launch :delay {:ms 200})])})))))

(deftest ring-registration-and-real-host-capture-callback-launch-unplaced-effect
  (let [runtime (studio/create-runtime) app (studio/handler runtime)
        defn (listener [{:op :effect/launch :select {:ids [:ember] :pick :first}}])
        req {:uri "/api/event-subscribers" :headers {"content-type" "application/transit+json"
                                                     "accept" "application/transit+json"}}]
    (try
      (let [receipt (future (app (assoc req :request-method :post
                                        :body (java.io.StringReader. (wire/encode {:op :save :subscriber defn :expected-epoch 0})))))]
        (loop [n 0] (when (and (not (realized? receipt)) (< n 200)) (studio/tick! runtime 0) (Thread/sleep 5) (recur (inc n))))
        (is (= :accepted (:status (wire/decode (:body (deref receipt 1000 {})))))))
      (is (= [defn] (:subscribers (wire/decode (:body (app (assoc req :request-method :get)))))))
      (is (true? ((:events! (studio/audio-callbacks runtime)) [(assoc (:data kick) :type :audio/kick)])))
      (studio/tick! runtime 1)
      (is (empty? (:instances (studio/snapshot runtime))))
      (studio/tick! runtime 25)
      (let [inst (first (:instances (studio/snapshot runtime)))]
        (is (= :ember (:effect-id inst)))
        (is (nil? (:cue-id inst)))
        (is (= :event-subscriber (get-in inst [:source :adapter])))
        (is (= 1 (count (:instances (:model (studio/export-show runtime)))))))
      (finally (studio/stop! runtime)))))

(deftest middleware-can-update-variables-and-queue-actions-without-blocking-the-owner
  (let [entered (promise) release (promise)
        middleware (fn [context event]
                     (deliver entered true) (deref release 3000 nil)
                     (-> context (assoc-in [:variables :melody] (:type event))
                         (assoc :actions [launch])))
        runtime (studio/create-runtime {:event-callback-options {:callbacks {:performer middleware} :timeout-ms 4000}})]
    (try
      (let [receipt (studio/register-event-subscriber! runtime (listener [{:op :callback :implementation {:type :registered :id :performer}}]))]
        (studio/tick! runtime 0)
        (is (= :accepted (:status @receipt))))
      ((:events! (studio/audio-callbacks runtime)) [{:type :audio/kick}])
      (studio/tick! runtime 10)
      (studio/tick! runtime 20)
      (is (true? (deref entered 1000 false)))
      (let [n (:frame-number (studio/snapshot runtime))]
        (studio/tick! runtime 30)
        (is (< n (:frame-number (studio/snapshot runtime)))))
      (deliver release true)
      (loop [i 0]
        (when (and (< i 100) (empty? (:instances (studio/snapshot runtime))))
          (Thread/sleep 4) (studio/tick! runtime (+ 40 i)) (recur (inc i))))
      (is (= :audio/kick (get-in @(:state runtime) [:model :variables :melody])))
      (is (= 1 (count (:instances (studio/snapshot runtime)))))
      (finally (deliver release true) (studio/stop! runtime)))))

(deftest all-selection-chunks-preserve-chosen-targets-and-save-cursor
  (let [model (-> (configured [{:op :cue/launch :select {:pick :all}}])
                  (assoc-in [:registry :cues]
                            (into {} (for [i (range 140) :let [id (str "pad-" i)]]
                                       [id {:id id :version 1 :effect-id :ember}]))))
        started (sub/advance (:model (sub/admit model [kick])) 0 {})
        with-new-cue (assoc-in started [:registry :cues :late] {:id :late :effect-id :ember :version 1})
        finished (sub/advance with-new-cue 25 {})]
    (is (= 128 (count (:instances started))))
    (is (= 128 (get-in started [:event-subscribers :runs 0 :offset])))
    (is (= 140 (count (:instances finished))))
    (is (not-any? #(= :late (:cue-id %)) (vals (:instances finished))))
    (is (empty? (get-in finished [:event-subscribers :runs])))))

(deftest callback-failure-is-local-and-registered-source-fn-can-be-restored
  (let [definition (listener [{:op :callback :implementation
                               {:type :source :namespace "user" :bindings {"n" 7}
                                :form "(fn [context event] (assoc-in context [:variables :count] n))"}}])
        model (sub/configure (demo/model) {:op :save :subscriber definition})
        restored (files/thaw (demo/model) (files/read-string (files/write-string model)))]
    (doseq [trusted? [false true]]
      (let [runtime (studio/create-runtime {:model restored :event-callback-options {:trusted-source? trusted?}})]
        (try
          ((:events! (studio/audio-callbacks runtime)) [{:type :audio/kick}])
          (studio/tick! runtime 1)
          (studio/tick! runtime 2)
          (loop [n 0]
            (when (and (< n 150) (seq (get-in @(:state runtime) [:model :event-subscribers :runs])))
              (Thread/sleep 5) (studio/tick! runtime (+ 3 n)) (recur (inc n))))
          (if trusted?
            (is (= 7 (get-in @(:state runtime) [:model :variables :count])))
            (is (re-find #"disabled" (get-in @(:state runtime) [:model :event-subscribers :last-error :message] ""))))
          (is (nil? (get-in (studio/snapshot runtime) [:status :error])))
          (finally (studio/stop! runtime)))))))
