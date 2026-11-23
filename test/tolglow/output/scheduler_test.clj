(ns tolglow.output.scheduler-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.show :as show]
            [tolglow.demo :as demo]
            [tolglow.atmosphere.deadline :as fog-deadline]
            [afterglow.sync.session :as sync-session]
            [afterglow.output.scheduler :as scheduler]))

(defn- bytes-copy [universes]
  (into {} (map (fn [[u data]] [u (mapv #(bit-and (int %) 255) data)])) universes))

(deftest expired-packet-keeps-its-new-look-off-the-wire-and-still-stops-fog
  (let [clock (atom 0) sent (atom [])
        runtime (show/create-runtime {:model (demo/model) :workspace-options nil :independent-output? true
                                      :physical-now-ms #(deref clock)
                                      :sink {:send! (fn [universes id] (swap! sent conj [id (bytes-copy universes)]))}})
        publisher (:publisher runtime)
        owner (update publisher :prepare-output
                      (fn [prepare]
                        (fn [buffers id]
                          (let [prepared (prepare buffers id)]
                            (when (> id 1) (swap! clock + 26))
                            prepared))))
        patch (:compiled @(:patch runtime))
        universe (first (:universes patch))
        offer! (fn [slot red]
                 (let [token (scheduler/begin-frame! owner 0 patch {} false
                                                     {:slot slot :started-at-ms @clock :deadline-ms (* 25 slot)})
                       universes (into {} (map (fn [u] [u (byte-array 512)])) (:universes patch))
                       ^bytes buffer (get universes universe)]
                   (aset-byte buffer 0 (byte red))
                   (aset-byte buffer 1 (byte 100))
                   (swap! (:fog-deadlines runtime) fog-deadline/remember (:frame-id token)
                          [{:until-ms 50 :off-bytes {[universe 1] 0}}])
                   (scheduler/complete! owner token {:universes universes})))
        delivered (fn [] (mapv (fn [[id bytes]] [id (subvec (get bytes universe) 0 2)]) @sent))]
    (try
      (offer! 1 10)
      (reset! clock 25)
      (scheduler/publish! owner 25)
      (offer! 2 20)
      (reset! clock 50)
      (let [late (scheduler/publish! owner 50)]
        (is (= [[1 [10 100]] [1 [10 0]]] (delivered)))
        (is (= {:expired-before-send? true :safety-output? true :sent-at-ms 76}
               (select-keys late [:expired-before-send? :safety-output? :sent-at-ms]))))
      (offer! 4 30)
      (reset! clock 100)
      (scheduler/publish! owner 100)
      (is (= 2 (count @sent)) "A cutoff already on the wire does not repeat on another expired packet")
      (is (= 1 (:safety-publications (scheduler/status owner))))
      (finally (show/stop! runtime)))))

(deftest real-effect-worker-can-block-while-output-keeps-advancing
  (let [physical (atom 0) sent (atom []) blocked? (atom false)
        entered (promise) release (promise)
        base (show/create-runtime {:model (update-in (demo/model) [:registry :effects :ember] dissoc :envelope)
                                     :independent-output? true :physical-now-ms #(deref physical)
                                     :workspace-options nil
                                     :sink {:send! (fn [universes id] (swap! sent conj {:id id :bytes (bytes-copy universes)}))}})
        runtime (assoc base :compile (fn [definition]
                                      (let [step ((:compile base) definition)]
                                        (fn [context]
                                          (when @blocked? (deliver entered true) (deref release 2000 nil))
                                          (step context)))))]
    (try
      (show/submit! runtime :settings {:scope :output :values {:presentation {:mode :hold-fade}}})
      (show/submit! runtime {:op :cue/launch :cue-id :ember})
      (show/tick! runtime 0)
      (scheduler/publish! (:publisher runtime))
      (let [initial (last @sent) frame-number (get-in @(:state runtime) [:model :frame-number])]
        (reset! blocked? true) (reset! physical 25)
        (let [worker (future (show/tick! runtime 25))]
          (is (= true (deref entered 1000 :timeout)))
          (reset! physical 75)
          (scheduler/publish! (:publisher runtime))
          (is (not (realized? worker)))
          (is (= frame-number (get-in @(:state runtime) [:model :frame-number])))
          (is (get-in (scheduler/status (:publisher runtime)) [:last :fallback?]))
          (is (< (reduce + (mapcat val (:bytes (last @sent))))
                 (reduce + (mapcat val (:bytes initial)))))
          (reset! physical 100) (deliver release true)
          (is (not= :timeout (deref worker 2000 :timeout)))
          (scheduler/publish! (:publisher runtime))
          (is (= [25 nil] (get-in (scheduler/status (:publisher runtime)) [:last :source-times])))
          (is (= (inc frame-number) (get-in @(:state runtime) [:model :frame-number])))))
      (finally (deliver release true) (show/stop! runtime)))))

(deftest settings-validate-before-entering-the-output-clock-and-roundtrip
  (let [runtime (show/create-runtime {:workspace-options nil})]
    (try
      (let [bad (show/submit! runtime :settings {:scope :output :values {:presentation {:mode :no-such-mode}}})]
        (show/tick! runtime 0)
        (is (= :rejected (:status @bad)))
        (is (nil? (get-in @(:state runtime) [:model :settings :output :presentation]))))
      (let [good (show/submit! runtime :settings {:scope :output :values {:presentation {:mode :buffered-interpolation :latency-ms 25}}})]
        (show/tick! runtime 25)
        (is (= :accepted (:status @good)))
        (is (= {:mode :buffered-interpolation :latency-ms 25}
               (get-in (show/export-show runtime) [:model :settings :output :presentation]))))
      (finally (show/stop! runtime)))))

(deftest physical-slot-freezes-the-latest-input-and-target-beat-across-workers
  (let [physical (atom 39.0)
        captured (atom {}) entered (java.util.concurrent.CountDownLatch. 2)
        resume (promise) sent (atom []) observe? (atom true)
        scene (-> (demo/model)
                  (assoc-in [:registry :effects :ember :targets] [:head-1])
                  (assoc-in [:registry :effects :ice :targets] [:head-2])
                  (update-in [:registry :effects :ember] dissoc :envelope)
                  (update-in [:registry :effects :ice] dissoc :envelope))
        base (show/create-runtime {:model scene :workers 2 :worker-timeout-ms 5000 :workspace-options nil
                                   :physical-now-ms #(deref physical) :independent-output? true
                                   :sink {:send! (fn [u id] (swap! sent conj [id (bytes-copy u)]))}})
        runtime (assoc base :compile
                       (fn [definition]
                         (let [step ((:compile base) definition)]
                           (fn [{:keys [frame] :as context}]
                             (when @observe?
                               (swap! captured assoc (:id definition)
                                      (assoc (select-keys frame [:time-ms :transport :variables])
                                             :worker (.getId (Thread/currentThread))))
                               (.countDown entered)
                               (deref resume 5000 nil))
                             (step context)))))
        audio! (:features! (show/audio-callbacks runtime))
        slot (fn [i start due] {:slot i :started-at-ms start :deadline-ms due})]
    (try
      (with-redefs [show/now-ms (fn [_] (+ 1000.0 @physical))]
        (doseq [cue [:ember :ice]] (show/submit! runtime {:op :cue/launch :cue-id cue}))
        (audio! {:audio/energy 0.25})
        (let [frame (future (show/render-slot! runtime (slot 2 39.0 50.0)))]
          (is (.await entered 4 java.util.concurrent.TimeUnit/SECONDS))
          (reset! physical 43.0)
          (audio! {:audio/energy 0.8})
          (deliver resume true)
          (is (not= :timeout (deref frame 5000 :timeout))))
        (let [[ember ice] (map @captured [:ember :ice])]
          (is (= #{:ember :ice} (set (keys @captured))))
          (is (not= (:worker ember) (:worker ice)))
          (is (identical? (:variables ember) (:variables ice)))
          (is (= 0.25 (get-in ember [:variables :audio/energy])))
          (is (every? #(= [1050.0 2.1] [(:time-ms %) (get-in % [:transport :beat])]) [ember ice])))
        (is (= 39.0 (:inputs-at-ms @(:ready (:publisher runtime)))))
        (reset! physical 50.0)
        (scheduler/publish! (:publisher runtime) 50.0)
        (let [[_ packet] (last @sent)]
          (is (pos? (get-in packet [1 1])) "Fixture one emits Ember red")
          (is (pos? (get-in packet [1 15])) "Fixture two emits Ice blue"))
        (reset! observe? false)
        (let [command (show/submit! runtime {:op :master/set :value 0.4})]
          (reset! physical 77.0)
          (show/render-slot! runtime (slot 3 69.0 75.0))
          (is (not (realized? command)) "Expired work must leave one-shot commands queued")
          (is (= 1050.0 (get-in @(:state runtime) [:model :time-ms])))
          (show/render-slot! runtime (slot 4 77.0 100.0))
          (is (= :accepted (:status @command)))
          (is (= 0.8 (get-in @(:state runtime) [:model :variables :audio/energy])))
          (is (= 1100.0 (get-in @(:state runtime) [:model :time-ms])))))
      (finally (deliver resume true) (show/stop! runtime)))))

(deftest link-seek-pause-and-show-restart-retire-buffered-source-frames
  (let [physical (atom 0.0) beat (atom 32.0) playing? (atom true)
        runtime (show/create-runtime {:independent-output? true :workspace-options nil
                                      :physical-now-ms #(deref physical)})
        owner (:publisher runtime)
        tick (fn [t] (reset! physical t) (show/tick! runtime t) (:generation @(:gate owner)))
        external (fn [_ time-ms] {:selected-source :link :follow-transport? true
                                   :clock {:time-ms time-ms :beat @beat :bpm 120.0
                                           :source-epoch 15 :playing? @playing?}})]
    (try
      (with-redefs [sync-session/clock-at external]
        (show/submit! runtime :settings {:scope :output :values {:presentation {:mode :buffered-interpolation}}})
        (show/submit! runtime {:op :cue/launch :cue-id :ember})
        (let [g1 (tick 0.0)]
          (reset! beat 32.05)
          (is (= g1 (tick 25.0)))
          (let [live (scheduler/publish! owner)]
            (is (some? (:time-ms live))))
          ;; A backwards Link sample can arrive from the same peer/source epoch.
          (reset! beat 3.0)
          (let [after-seek (tick 50.0)]
            (is (> after-seek g1))
            (is (get-in (scheduler/publish! owner) [:presentation :waiting?]))
            (reset! playing? false)
            (let [paused (tick 75.0)]
              (is (> paused after-seek))
              (reset! playing? true)
              (is (> (tick 100.0) paused)))))
        (let [previous (:generation @(:gate owner))]
          (show/submit! runtime {:op :show/restart})
          (is (> (tick 125.0) previous))))
      (finally (show/stop! runtime)))))
