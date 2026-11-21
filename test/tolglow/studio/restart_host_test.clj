(ns tolglow.studio.restart-host-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.studio :as studio]
            [tolglow.studio.audio :as audio]
            [tolglow.studio.connections :as connections]
            [tolglow.studio.repl-manager :as repl-manager]
            [tolglow.studio.persistence :as persistence]
            [tolglow.studio.routing-host-test :as fixture]
            [afterglow.sync :as sync]
            [afterglow.sync.session :as session]
            [tolglow.integrations.live-sync :as live-sync])
  (:import [java.util.concurrent CountDownLatch LinkedBlockingQueue TimeUnit]))

(defn await! [^CountDownLatch latch] (.await latch 2 TimeUnit/SECONDS))
(defn result [receipt] (deref (:completion receipt) 3000 ::timeout))

(deftest restart-clears-performance-ownership-and-retains-definitions-and-repl-locals
  (let [runtime (studio/create-runtime {:model (fixture/model)})]
    (try
      (let [id (fixture/prepare! runtime (fixture/spec))
            conn (:connections runtime) repls (connections/repls conn) agents (connections/agents conn)
            graph (connections/reactive conn)
            composition {:id :loop :mode :sequence :timebase :beats :loop :loop
                         :slots [{:cue-id :wash :duration {:beats 1}}]}]
        (is (= :completed (:status (result (repl-manager/submit! repls "main" "(def retained-local 42)")))))
        (fixture/apply-at! runtime :composition composition 0)
        (fixture/apply-at! runtime :composition-run {:id :loop} 0)
        (fixture/apply-at! runtime :auto {:enabled? true :seed 2 :interval {:beats 1} :count 1 :scope :all} 0)
        (fixture/apply-at! runtime :modulation-start {:id :random :seed 2 :every {:beats 1/4} :params [:level]} 0)
        (fixture/apply-at! runtime :modulation-hold {:instance-id id :params [:level]} 0)
        (fixture/apply-at! runtime :response {:id :response :params [:level] :envelope {:attack {:beats 2}}} 0)
        (swap! (:leases runtime) assoc "human" 0)
        (let [before (:model @(:state runtime))
              old-audio (studio/audio-callbacks runtime) old-sync (studio/sync-callbacks runtime)
              restart (studio/submit! runtime {:op :show/restart})
              stale (studio/submit! runtime {:op :cue/launch :cue-id :wash})]
          (is (seq (:composition-runs before)))
          (is (true? (get-in before [:auto :enabled?])))
          (studio/tick! runtime 100)
          (is (= :accepted (:status @restart)))
          (is (= :rejected (:status @stale)))
          (let [after (:model @(:state runtime))]
            (is (= 1 (:epoch after)))
            (is (empty? (:instances after)))
            (is (empty? (:composition-runs after)))
            (is (empty? (:composition-tokens after)))
            (is (= (:compositions before) (:compositions after)))
            (is (= (:registry before) (:registry after)))
            (is (false? (get-in after [:auto :enabled?])))
            (is (= (get-in before [:auto :config]) (get-in after [:auto :config])))
            (is (nil? (get-in after [:auto :state])))
            (is (empty? (get-in after [:modulation :controllers])))
            (is (empty? (get-in after [:modulation :holds])))
            (is (= :random (get-in after [:settings :modulation :saved-controllers 0 :id])))
            (is (false? (get-in after [:responses 0 :enabled?])))
            (is (= (get-in before [:responses 0 :envelope]) (get-in after [:responses 0 :envelope])))
            (is (false? (get-in after [:routing :enabled?])))
            (is (empty? (get-in after [:routing :state :routes])))
            (is (= (get-in before [:routing :spec]) (get-in after [:routing :spec])))
            (is (empty? @(:leases runtime)))
            (is (false? ((:events! old-audio) [{:type :old}])))
            (is (nil? ((:events! old-sync) (fixture/observation "old" 127 0))))
            (is (identical? repls (connections/repls conn)))
            (is (identical? agents (connections/agents conn)))
            (is (identical? graph (connections/reactive conn)))
            (is (= "42" (:result (result (repl-manager/submit! repls "main" "retained-local")))))
            (let [saved (studio/export-show runtime)
                  restored (persistence/thaw after saved)]
              (is (= 2 (:schema-version saved)))
              (is (nil? (get-in restored [:auto :state])))
              (is (= 2 (:schema-version (persistence/freeze restored))))))
          (studio/tick! runtime 4000)
          (is (empty? (:instances (studio/snapshot runtime))))
          (is (every? zero? (get-in (studio/snapshot runtime) [:output :values :head :emitted-color])))))
      (finally (studio/stop! runtime)))))

(deftest restart-rebinds-active-midi-without-reopening-and-discards-old-queued-input
  (let [opened (promise) entered (CountDownLatch. 1) release (CountDownLatch. 1)
        opens (atom 0) closes (atom []) reference (atom nil)
        runtime (studio/create-runtime {:model (fixture/model)
                                        :sync-options {:midi-factory
                                                       (fn [_ _] {:open! (fn [receive!] (swap! opens inc) (deliver opened receive!) :midi)
                                                                  :close! (fn [_] (swap! closes conj (Thread/holdsLock (:lock @reference))))})}})]
    (reset! reference runtime)
    (try
      (studio/sync-action! runtime :configure {:midi {:enabled? true :input-index 0 :control? true}
                                               :live {:mappings {60 {:cue-id :wash :mode :held}}}})
      (let [callbacks (studio/sync-callbacks runtime) conn (:connections runtime) manager (connections/sync conn)]
        (connections/start-sync! conn (assoc callbacks :events!
                                              (fn [event] ((:events! callbacks) event)
                                                (when (= :sync/observation (:type event))
                                                  (.countDown entered) (.await release 3 TimeUnit/SECONDS)) true)))
        (let [receive! (deref opened 2000 nil) run-id (sync/active-run-id manager)]
          (receive! {:type :midi/message :source-id :midi-clock :time-ms 0 :bytes [0x90 60 127]})
          (is (await! entered))
          (receive! {:type :midi/message :source-id :midi-clock :time-ms 1 :bytes [0x90 60 127]})
          (is (= :accepted (:status (fixture/apply-at! runtime :command {:op :show/restart} 10))))
          (.countDown release)
          (is (session/flush! manager))
          (studio/tick! runtime 20)
          (is (empty? (:instances (studio/snapshot runtime))))
          (is (= run-id (sync/active-run-id manager)))
          (is (= 1 @opens))
          (is (empty? @closes))
          (is (false? ((:commands! callbacks) [{:op :cue/launch :cue-id :wash}])))
          (receive! {:type :midi/message :source-id :midi-clock :time-ms 30 :bytes [0x90 60 127]})
          (is (session/flush! manager))
          (studio/tick! runtime 40)
          (is (= ["1:1"] (mapv :id (:instances (studio/snapshot runtime)))))
          (is (= 40 (get-in @(:state runtime) [:model :routing :ports fixture/source-id :clock :time-ms])))
          (is (true? (get-in (connections/export-state conn) [:integrations :sync :wanted?])))))
      (finally (.countDown release) (studio/stop! runtime)))
    (is (= [false] @closes))))

(deftest delayed-stop-and-rebind-cannot-target-new-resource-ownership
  (let [closed (atom 0)
        capture (audio/create-manager {:open-source! (fn [_]
                                                       (let [done (promise)]
                                                         {:read! (fn [& _] @done -1)
                                                          :close! #(do (swap! closed inc) (deliver done true))}))})
        sync-manager (live-sync/create-manager)]
    (try
      (let [config {:default? true}
            first-run (audio/start! capture config {:events! identity})]
        (is (:stopped? (audio/stop! capture)))
        (let [new-run (audio/start! capture config {:events! identity})]
          (audio/stop! capture 0 (:run-id first-run))
          (is (= (:run-id new-run) (audio/active-run-id capture)))
          (is (not (realized? (:result new-run))))))
      (let [first-run (sync/start! sync-manager {:events! identity})]
        (is (:stopped? (sync/stop! sync-manager)))
        (let [new-run (sync/start! sync-manager {:events! identity})]
          (sync/stop! sync-manager 0 (:run-id first-run))
          (is (= (:run-id new-run) (sync/active-run-id sync-manager)))
          (is (not (realized? (:result new-run))))))
      (finally (audio/close! capture) (sync/close! sync-manager))))
  (let [runtime (studio/create-runtime {:model (fixture/model)}) conn (:connections runtime)]
    (try
      (fixture/apply-at! runtime :command {:op :show/restart} 0)
      (let [generation (:generation (connections/status conn))
            old-audio (studio/audio-callbacks runtime) old-sync (studio/sync-callbacks runtime)]
        (fixture/apply-at! runtime :command {:op :show/restart} 0)
        (is (= {:rebound? false :reason :retired-generation}
               (connections/rebind-restart! conn generation old-audio old-sync)))
        (is (nil? ((:events! old-sync) (fixture/observation "stale" 127 0))))
        (let [seen (atom []) generation (:generation (connections/status conn))]
          ;; This new run began after restart committed and already has fresh
          ;; callbacks. Delayed completion of restart must not replace them.
          (connections/start-sync! conn {:events! #(swap! seen conj %)})
          (is (= {:rebound? false :reason :replaced-run}
                 (:sync (connections/rebind-restart! conn generation (studio/audio-callbacks runtime)
                                                     (studio/sync-callbacks runtime)))))
          (session/ingest! (connections/sync conn) {:type :midi/message :source-id :midi-clock :time-ms 0 :bytes [0xfa]})
          (is (session/flush! (connections/sync conn)))
          (is (some #(= :sync/clock (:type %)) @seen)))
        (studio/stop! runtime)
        (is (= {:rebound? false :reason :retired-generation}
               (connections/rebind-restart! conn (:generation (connections/status conn)) old-audio old-sync))))
      (finally (studio/stop! runtime)))))

(deftest restart-rebinds-audio-at-block-boundary-without-waiting-for-blocked-read
  (let [input (LinkedBlockingQueue.) reads (mapv (fn [_] (CountDownLatch. 1)) (range 3))
        read-index (atom -1) opens (atom 0) closes (atom []) reference (atom nil)
        runtime (studio/create-runtime
                  {:model (fixture/model)
                   :audio-options {:open-source!
                                   (fn [_] (swap! opens inc)
                                     {:started-at-ms 0.0
                                      :read! (fn [buffer offset length]
                                               (when-let [latch (get reads (swap! read-index inc))] (.countDown ^CountDownLatch latch))
                                               (if (= :eof (.take input)) -1
                                                 (do ;; The injected PCM sample clock advances with
                                                     ;; delivered frames, independently of test/JIT speed.
                                                     (swap! (:clock @reference) update :time-ms +
                                                            (* 1000.0 (/ (quot length 2) 48000.0)))
                                                     (dotimes [i (quot length 2)]
                                                       (aset-byte buffer (+ offset (* i 2)) (unchecked-byte 0))
                                                       (aset-byte buffer (+ offset (* i 2) 1) (unchecked-byte 32))) length)))
                                      :close! #(do (swap! closes conj (Thread/holdsLock (:lock @reference))) (.offer input :eof))})}})]
    (reset! reference runtime)
    (try
      (studio/audio-action! runtime :start {:default? true :sample-rate 48000.0 :channels 1 :block-frames 480})
      (is (await! (reads 0)))
      (let [manager (connections/audio (:connections runtime)) run-id (audio/active-run-id manager)
            restarted (future (fixture/apply-at! runtime :command {:op :show/restart} 100))]
        (is (= :accepted (:status (deref restarted 1000 ::blocked))))
        (is (= run-id (audio/active-run-id manager)))
        (is (empty? @closes))
        (.offer input :block)
        (is (await! (reads 1)))
        (is (nil? (:error (audio/status manager))))
        (is (nil? (:features @(:audio-input runtime))) "In-flight block still carries the old epoch")
        (.offer input :block)
        (is (await! (reads 2)))
        (studio/tick! runtime 200)
        (is (= 0.25 (get-in @(:state runtime) [:model :variables :audio :features :rms])))
        (is (= 0.25 (get-in @(:state runtime) [:model :routing :ports "audio/rms" :value])))
        (is (= 1 (:epoch @(:audio-input runtime))))
        (is (= 1 @opens))
        (is (true? (get-in (connections/export-state (:connections runtime)) [:integrations :audio-capture :capture-wanted?]))))
      (finally (studio/stop! runtime)))
    (is (= [false] @closes))))
