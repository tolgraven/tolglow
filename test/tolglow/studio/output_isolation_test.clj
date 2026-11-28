(ns tolglow.studio.output-isolation-test
  "Preview and Studio consumers cannot hold back lighting output (#226).

  Each test drives the real show owner and Afterglow output scheduler by hand:
  an injected physical clock, one owner tick and one publication per frame,
  and a capture sink. A frame changes the grand master, so every packet has
  its own content. The same commands run once with no consumer attached. The
  stalled run must publish the same packets: frame ID, sample time, send time
  and bytes. Every owner call and wait has a bound, so a blocked owner fails
  the test instead of hanging it, and consumers are released in finally."
  (:require [afterglow.control.devices.session :as session]
            [afterglow.control.model :as control]
            [afterglow.output.scheduler :as scheduler]
            [afterglow.render.compute :as compute]
            [clojure.test :refer [deftest is testing]]
            [ring.adapter.jetty :as jetty]
            [tolglow.demo :as demo]
            [tolglow.render.workers :as workers]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.display :as display]
            [clojure.java.io :as io]
            [tolglow.controllers.content-browser-host :as browser]
            [tolglow.show.recovery :as recovery]
            [tolglow.transit :as transit])
  (:import [java.net URI]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(def ^:private bound-ms
  "Upper bound for any single wait. Never a correctness threshold."
  10000)

(def ^:private stall-ms
  "How long a stalled consumer may block. It outlasts the bound on an owner
  call, so an owner that waits for the consumer fails instead of recovering."
  (* 3 bound-ms))

(def ^:private frame-ms 25)

(def ^:private wash {:id :wash :universe 1 :address 1 :channels {:dimmer 1}})

(def ^:private wash-model
  (control/new-show
   {:registry {:effects {:wash {:id :wash :name "Wash" :version 1
                                :params {:level {:type :number :default 1.0 :bounds [0 1]}}
                                :targets [:wash] :outputs {:dimmer {:op :param :key :level}}}}
               :cues {:wash {:id :wash :effect-id :wash :version 1}}}
    :rig {:fixtures [wash]}}))

(defn- bounded
  "Run f on another thread and return its value, or fail once bound-ms passes."
  [label f]
  (let [task (future (f))
        result (deref task bound-ms ::blocked)]
    (when (= ::blocked result)
      (throw (ex-info (str label " did not return within " bound-ms " ms") {:label label})))
    result))

(defn- eventually [description predicate]
  (let [deadline (+ (System/nanoTime) (* 1000000 bound-ms))]
    (loop []
      (or (predicate)
          (if (< (System/nanoTime) deadline)
            (do (Thread/sleep 5) (recur))
            (throw (ex-info (str "Timed out waiting for " description) {})))))))

(defn- unsigned [universes]
  (into (sorted-map) (map (fn [[universe data]] [universe (mapv #(bit-and (int %) 255) data)])) universes))

(defn- with-show
  "Run f with a captured show. The sink records each packet with its ID."
  [options f]
  (let [clock (atom 0) sent (atom [])
        runtime (show/create-runtime
                 (merge {:model wash-model :workspace-options nil :workers 1 :independent-output? true
                         :physical-now-ms #(deref clock)
                         :sink {:send! (fn [universes id] (swap! sent conj [id (unsigned universes)]))}}
                        (dissoc options :launch)))]
    (try
      (f {:runtime runtime :clock clock :sent sent :launch (get options :launch :wash)})
      (finally (show/stop! runtime)))))

(defn- level [frame] (+ 0.2 (* 0.05 (mod frame 16))))

(defn- drive!
  "Advance each frame on the owner and present it at its physical deadline,
  as the scheduler's render and publication workers would. Frame 0 launches
  the fixture's cue. before! can submit more commands ahead of a frame."
  ([fixture frames] (drive! fixture frames nil))
  ([{:keys [runtime clock sent launch]} frames before!]
   (mapv (fn [frame]
           (let [t (* frame-ms (inc frame))
                 master (show/submit! runtime {:op :master/set :value (level frame)})
                 _ (when (zero? frame) (show/submit! runtime {:op :cue/launch :cue-id launch}))
                 _ (when before! (before! frame))
                 before (count @sent)]
             (reset! clock t)
             (bounded "The show owner's tick" #(show/tick! runtime t))
             (let [packet (bounded "The output publication" #(scheduler/publish! (:publisher runtime) t))
                   [id bytes] (when (> (count @sent) before) (peek @sent))]
               {:frame frame :frame-id (:frame-id packet) :time-ms (:time-ms packet)
                :sent-at-ms (:sent-at-ms packet) :sink-id id :bytes bytes
                :master (:status (deref master 0 nil))})))
         frames)))

(defn- baseline
  "The packets the same commands publish with no consumer attached."
  ([frames] (baseline {} frames nil))
  ([options frames before]
   (with-show options (fn [fixture] (drive! fixture frames (when before (partial before fixture)))))))

(defn- packets [records]
  (mapv #(select-keys % [:frame-id :time-ms :sent-at-ms :sink-id :bytes :master]) records))

(defn- check-packets [expected observed]
  (is (= (count expected) (count observed)))
  (is (every? #(= :accepted (:master %)) observed) "Every frame admitted its master command")
  (is (every? :sink-id observed) "Every frame reached the sink")
  (is (= (map :frame-id observed) (map :sink-id observed)) "The sink saw the packet each slot published")
  (is (= (packets expected) (packets observed))))

;; Real Jetty and JDK WebSocket clients ---------------------------------------

(def ^:private http (delay (HttpClient/newHttpClient)))

(defn- apply-patch [base {:keys [kind view changes]}]
  (case kind
    :full view
    :patch (reduce (fn [data [op path value]]
                     (case op
                       :set (assoc-in data path value)
                       :remove (update-in data (vec (butlast path)) dissoc (peek path))))
                   base changes)
    base))

(defn- connect
  "Open a WebSocket on path. A state message carries ::frame-number from the
  client's applied view. :auto-ack? acknowledges each one as soon as it is
  applied, as Studio does; otherwise the test decides when to acknowledge."
  [port path & {:keys [auto-ack?]}]
  (let [messages (LinkedBlockingQueue.) latest (atom nil) closed (promise) buffer (StringBuilder.) view (volatile! nil)
        listener (reify WebSocket$Listener
                   (onOpen [_ socket] (.request socket 1))
                   (onText [_ socket chunk last?]
                     (.append buffer ^CharSequence chunk)
                     (when last?
                       (let [message (transit/decode (.toString buffer))]
                         (.setLength buffer 0)
                         (vswap! view apply-patch message)
                         (let [message (assoc message ::frame-number (:frame-number @view))]
                           (reset! latest message)
                           (.offer messages message))
                         (when (and auto-ack? (:token message))
                           (.sendText socket (transit/encode {:op :ack :token (:token message)}) true))))
                     (.request socket 1)
                     nil)
                   (onClose [_ _ code reason] (deliver closed [code reason]) nil))
        socket (.get (.buildAsync (.newWebSocketBuilder ^HttpClient @http)
                                  (URI. (str "ws://127.0.0.1:" port path)) listener)
                     5 TimeUnit/SECONDS)]
    {:socket socket :messages messages :latest latest :closed closed}))

(defn- state-client [port session & options]
  (apply connect port (str "/api/state/stream?session=" session) options))

(defn- receive [client]
  (or (.poll ^LinkedBlockingQueue (:messages client) bound-ms TimeUnit/MILLISECONDS)
      (throw (ex-info "Expected a pushed message" {}))))

(defn- drain [client]
  (let [out (java.util.ArrayList.)]
    (.drainTo ^LinkedBlockingQueue (:messages client) out)
    (vec out)))

(defn- send-control [client data]
  (.get (.sendText ^WebSocket (:socket client) (transit/encode data) true) bound-ms TimeUnit/MILLISECONDS))

(defn- acknowledge [client message] (send-control client {:op :ack :token (:token message)}))

(defn- close-clients! [clients]
  (doseq [{:keys [socket]} clients] (.abort ^WebSocket socket)))

(defn- serve
  "Serve handler on an OS-assigned loopback port and call f with the port."
  [handler f]
  (let [server (jetty/run-jetty handler {:host "127.0.0.1" :port 0 :join? false})]
    (try (f (.getPort (.getURI server))) (finally (.stop server)))))

(defn- with-studio
  "Serve the actual Studio Ring application for runtime."
  [runtime f]
  (serve (studio/handler runtime) f))

(defn- frame-number [runtime] (:frame-number (show/snapshot runtime)))

;; 1. Studio clients that never or slowly acknowledge -------------------------

(deftest studio-clients-that-withhold-acks-leave-output-untouched
  (let [frames (range 48)
        expected (baseline frames)]
    (with-show {}
      (fn [{:keys [runtime] :as fixture}]
        (with-studio runtime
          (fn [port]
            (let [silent (state-client port "never-acks") slow (state-client port "acks-slowly")]
              (try
                (let [silent-first (receive silent)
                      slow-seen (atom [(receive slow)])
                      slow-acks (atom 0)
                      observed (into [] (mapcat (fn [chunk]
                                                  (let [records (drive! fixture chunk)]
                                                    ;; Acknowledge the newest view once per twelve frames.
                                                    (swap! slow-seen into (drain slow))
                                                    (acknowledge slow (peek @slow-seen))
                                                    (swap! slow-acks inc)
                                                    records)))
                                     (partition-all 12 frames))]
                  (check-packets expected observed)
                  (is (= :full (:kind silent-first)))
                  (is (empty? (drain silent)) "An unacknowledged client holds exactly one view")
                  (is (not (realized? (:closed silent))) "Withholding acknowledgements is not a fault")
                  (is (<= (count @slow-seen) (inc @slow-acks)) "At most one view waits for each acknowledgement")
                  (is (apply <= (map ::frame-number @slow-seen)) "A slow client never receives an older frame")
                  (testing "an acknowledgement after the stall delivers only the newest frame"
                    (acknowledge silent silent-first)
                    (let [latest (receive silent)]
                      (is (= (frame-number runtime) (::frame-number latest)))
                      (is (empty? (drain silent))))))
                (finally (close-clients! [silent slow]))))))))))

;; 2. Subscribers whose callbacks block --------------------------------------

(deftest event-middleware-blocked-for-seconds-leaves-output-untouched
  (let [frames (range 48)
        expected (baseline frames)
        entered (promise) release (promise) finished (promise)]
    (try
      (with-show {:event-callback-options {:timeout-ms 60000}}
        (fn [{:keys [runtime] :as fixture}]
          (let [registered (show/register-event-subscriber!
                            runtime {:id :blocks :match {:type :instance/launched}
                                     :middleware (fn [context _]
                                                   (deliver entered (Thread/currentThread))
                                                   (deref release stall-ms nil)
                                                   (deliver finished true)
                                                   context)})
                ;; The launch commits a subscriber run; a later frame starts its callback.
                opening (drive! fixture (take 3 frames))]
            (is (= :accepted (:status (deref registered bound-ms nil))))
            (is (instance? Thread (deref entered bound-ms nil)) "The launch reached the middleware")
            (let [observed (into opening (drive! fixture (drop 3 frames)))]
              (check-packets expected observed)
              (is (not (realized? finished)) "The middleware stayed blocked across every frame")
              (is (= 1 (count (get-in @(:state runtime) [:model :event-subscribers :runs])))
                  "The subscriber run waits for its callback without holding the owner")))))
      (finally (deliver release true)))))

(deftest a-blocked-remote-query-leaves-output-untouched
  ;; A Studio query surface samples each committed state on the stream worker.
  ;; The timeline surface stands in for any slow sampler. [:clock/beat] is a
  ;; graph lease whose change listener runs inside the owner's publication.
  (let [frames (range 48)
        expected (baseline frames)
        entered (promise) release (promise) finished (promise)
        slow-timeline (fn [& _]
                        (deliver entered (Thread/currentThread))
                        (deref release stall-ms nil)
                        (deliver finished true)
                        {:sampled? true})]
    (try
      (with-redefs-fn {#'show/timeline slow-timeline}
        #(with-show {}
           (fn [{:keys [runtime] :as fixture}]
             (with-studio runtime
               (fn [port]
                 (let [client (state-client port "remote-queries")]
                   (try
                     (acknowledge client (receive client))
                     (send-control client {:op :queries :queries [[:timeline/view {:bars 2}] [:clock/beat]]})
                     (let [sampler (deref entered bound-ms nil)]
                       (is (instance? Thread sampler) "The stream worker entered the slow sampler")
                       (let [observed (drive! fixture frames)]
                         (check-packets expected observed)
                         (is (not (realized? finished)) "The sampler stayed blocked across every frame")))
                     (deliver release true)
                     ;; The released pass completes the view it was sampling, and
                     ;; its acknowledgement brings the newest frame, not a backlog.
                     (let [sampled (receive client)]
                       (is (= {:value {:sampled? true}} (get-in sampled [:view :subscriptions [:timeline/view {:bars 2}]])))
                       (is (contains? (get-in sampled [:view :subscriptions]) [:clock/beat]))
                       (acknowledge client sampled)
                       (let [latest (receive client)]
                         (is (= (frame-number runtime) (::frame-number latest)))
                         (is (empty? (drain client)))))
                     (finally (close-clients! [client])))))))))
      (finally (deliver release true)))))

(deftest blocked-frame-consumers-leave-output-untouched
  ;; The debug stream's event and DMX lanes, and a parameter-frame consumer
  ;; whose renderer blocks, each with a reader that never acknowledges.
  (let [frames (range 48)
        expected (baseline frames)
        entered (promise) release (promise)
        render (fn [_ _] (deliver entered (Thread/currentThread)) (deref release stall-ms nil) (byte-array 0))]
    (try
      (with-show {:parameter-frames {:render render :period-ms 10}}
        (fn [{:keys [runtime] :as fixture}]
          (with-studio runtime
            (fn [port]
              (let [debug (connect port (str "/api/debug/events?runtime=" (:id runtime)))]
                (try
                  (send-control debug {:op :dmx/subscribe :universe 1})
                  (let [first-frame (drive! fixture [0])
                        instance (some (fn [[id instance]] (when (= :wash (:cue-id instance)) id))
                                       (get-in @(:state runtime) [:model :instances]))
                        view (show/parameter-frames-action! runtime :subscribe
                                                            {:instance-id instance :param :level
                                                             :width 120 :height 40 :consumer "never-reads"})]
                    (is (string? (:lease view)))
                    (is (instance? Thread (deref entered bound-ms nil)) "The parameter renderer is blocked")
                    (let [observed (into first-frame (drive! fixture (rest frames)))
                          received (atom [])]
                      (check-packets expected observed)
                      (eventually "a debug message" #(seq (swap! received into (drain debug))))
                      (is (<= 1 (count @received) 2) "Each unacknowledged debug lane holds one message")
                      (is (zero? (get-in (show/parameter-frames-action! runtime :status {}) [:counts :renders]))
                          "No parameter frame finished while its renderer was blocked")))
                  (finally (close-clients! [debug]))))))))
      (finally (deliver release true)))))

;; 3. A stalled distributed compute peer -------------------------------------

(def ^:private compute-token "output-isolation-compute-token-0123")

(def ^:private sweep-model
  (let [heads (mapv (fn [i] (assoc (first (demo/fixtures)) :id (keyword (str "beam-" i)) :universe 1
                                   :address (inc (* 12 i)) :position [(- (* 0.5 i) 1.0) 3.0 0.0]))
                    (range 4))
        dimmer {:op :add :args [(demo/constant 0.5)
                                {:op :multiply :args [(demo/constant 0.1) {:op :position :axis :x}]}]}]
    (assoc (control/new-show
            {:registry {:effects {:sweep {:id :sweep :name "Sweep" :version 1
                                          :params {:red {:type :number :default 0.5 :bounds [0 1]}}
                                          :outputs {:dimmer dimmer
                                                    :color {:op :vec3 :args [(demo/param :red) (demo/constant 0.125)
                                                                             (demo/constant 0.25)]}}}}
                        :cues {:sweep {:id :sweep :name "Sweep" :version 1 :effect-id :sweep}}}})
           :rig {:fixtures heads :room {:width 10 :height 5 :depth 8}})))

(def ^:private distributed-options
  {:model sweep-model :launch :sweep :compute-backend :distributed :gpu-budget-ms 100
   :adaptive-keyframes {:enabled? false}})

(defn- stalled-peer
  "A compute worker on the real Studio socket that prepares programs and then
  never answers a job, like a Room GPU tab that froze mid-frame."
  [port]
  (let [received (atom []) welcomed (promise)
        listener (reify WebSocket$Listener
                   (onOpen [_ socket] (.request socket 1))
                   (onText [_ socket text _]
                     (let [{:keys [op] :as packet} (transit/decode (str text))]
                       (swap! received conj op)
                       (case op
                         :welcome (deliver welcomed true)
                         :prepare (.sendText socket (transit/encode {:op :ready :id (:id packet)}) true)
                         nil))
                     (.request socket 1)
                     nil))
        socket (.get (.buildAsync (doto (.newWebSocketBuilder ^HttpClient @http)
                                    (.header "Origin" (str "http://127.0.0.1:" port)))
                                  (URI. (str "ws://127.0.0.1:" port "/api/compute/stream")) listener)
                     5 TimeUnit/SECONDS)]
    (.get (.sendText socket (transit/encode {:op :hello :token compute-token :caps {:max-result-floats 8192}}) true)
          bound-ms TimeUnit/MILLISECONDS)
    {:socket socket :received received :welcomed welcomed}))

(deftest a-stalled-compute-peer-costs-one-deadline-and-leaves-output-untouched
  (let [frames (range 48)
        expected (baseline distributed-options frames nil)]
    (with-show distributed-options
      (fn [{:keys [runtime] :as fixture}]
        (let [coordinator (get-in runtime [:compute :pool])
              hub (workers/create coordinator compute-token)]
          (try
            (serve (partial workers/response hub)
             (fn [port]
            (let [{:keys [socket received welcomed]} (stalled-peer port)]
              (try
                (is (true? (deref welcomed bound-ms nil)))
                (let [first-frame (drive! fixture [0])]
                  ;; Measure from the frame after the peer holds the program.
                  (eventually "the peer to prepare the program"
                              #(some (comp seq :ready) (vals @(:peers coordinator))))
                  (let [observed (into first-frame (drive! fixture (rest frames)))
                        {:keys [cpu-fallbacks] pool-status :pool} (compute/status (:compute runtime))]
                    (check-packets expected observed)
                    (eventually "the stalled peer to receive its job" #(seq (filter #{:run} @received)))
                    (is (= 1 (count (filter #{:run} @received))) "The stalled peer was offered exactly one job")
                    (is (= {:workers 1 :busy 1 :completed 0 :missed 1}
                           (select-keys pool-status [:workers :busy :completed :missed]))
                        "One frame waited for its deadline; later frames rendered locally without waiting")
                    (is (= (count frames) cpu-fallbacks) "Every frame rendered its fields locally")))
                (finally (.abort ^WebSocket socket))))))
            (finally (workers/close! hub))))))))

;; 4. Push and browser display transports that block on write -----------------

(defn- block-until!
  "Hold the calling thread until release, ignoring interrupts as a stuck USB
  or browser write would, but never longer than stall-ms. blocked counts the
  writers currently held."
  [release blocked]
  (swap! blocked inc)
  (try
    (let [deadline (+ (System/nanoTime) (* 1000000 stall-ms))]
      (loop [interrupted? false]
        (if (or (realized? release) (>= (System/nanoTime) deadline))
          (when interrupted? (.interrupt (Thread/currentThread)))
          (recur (try (deref release 20 nil) interrupted?
                      (catch InterruptedException _ true))))))
    (finally (swap! blocked dec))))

(def ^:private capture-midi {:open! (fn [_] :midi) :send! (fn [_ _]) :close! (fn [_])})

(defn- import-at [frame]
  (fn [{:keys [runtime]} current]
    (when (= frame current)
      (show/submit! runtime :import {:source (show/export-show-source runtime)}))))

(defn- blocking-display
  "A display transport whose writes block while block? is set."
  [block? entered release blocked]
  {:open! (constantly :display) :close! (fn [_])
   :render! (fn [_ _] (when @block? (deliver entered (Thread/currentThread)) (block-until! release blocked)))})

(defn- retirements [runtime] (get-in (show/snapshot runtime) [:status :device-retirement]))

(defn- refusal [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo error (ex-data error))
       (catch java.util.concurrent.ExecutionException error (ex-data (.getCause error)))))

(deftest blocked-display-writes-leave-output-untouched-through-an-epoch-change
  (let [frames (range 48)
        restart (import-at 24)
        expected (baseline {} frames restart)
        block? (atom false) release (promise) blocked (atom 0)
        direct-entered (promise) browser-entered (promise)
        direct (blocking-display block? direct-entered release blocked)
        browser (display/transport
                 {:capture {:open! (constantly :capture) :close! (fn [_]) :capture! (fn [_ view] view)}
                  :sink {:open! (constantly :sink) :close! (fn [_])
                         :render! (fn [_ _] (when @block? (deliver browser-entered (Thread/currentThread))
                                              (block-until! release blocked)))}
                  :max-fps 60 :shutdown-timeout-ms 50})]
    (with-show {}
      (fn [{:keys [runtime] :as fixture}]
        (try
          (let [direct-push (show/connect-device! runtime :push-2 {:session-id "direct-push" :midi capture-midi
                                                                     :display direct})
                browser-push (show/connect-device! runtime :push-2 {:session-id "browser-push" :midi capture-midi
                                                                      :display browser})
                epoch (get-in @(:state runtime) [:model :epoch])]
            (reset! block? true)
            (let [first-frame (drive! fixture [0] (partial restart fixture))]
              ;; Each frame changes the display view, so both writers are now stuck.
              (is (instance? Thread (deref direct-entered bound-ms nil)) "The device feedback thread is blocked")
              (is (instance? Thread (deref browser-entered bound-ms nil)) "The display transport worker is blocked")
              (let [observed (into first-frame (drive! fixture (rest frames) (partial restart fixture)))]
                (check-packets expected observed)
                (is (= 2 @blocked) "Both writers stayed blocked across every frame")
                (is (< epoch (get-in @(:state runtime) [:model :epoch])) "The import changed the show epoch")
                (is (empty? @(:devices runtime)) "The epoch change retired both sessions")
                (is (pos? (:pending (retirements runtime)))
                    "The session whose writer holds its lock is still closing, off the render thread")
                (is (contains? (:closing (retirements runtime)) "direct-push"))
                (is (= 2 (+ (:pending (retirements runtime)) (:retired (retirements runtime)))))
                (testing "a session ID is not reused while its previous session is closing"
                  (reset! block? false)
                  (is (= {:session-id "direct-push" :code :device/closing}
                         (refusal #(show/connect-device! runtime :push-2 {:session-id "direct-push" :midi capture-midi
                                                                            :display direct})))))
                (deliver release true)
                (eventually "the blocked session to retire" #(zero? (:pending (retirements runtime))))
                (is (= {:pending 0 :retired 2 :failed 0 :reopen-failed 0 :abandoned 0 :late 0 :refused 1
                        :closing #{} :reconnectable #{"direct-push" "browser-push"}}
                       (dissoc (retirements runtime) :last-refusal)))
                (is (= {:session-id "direct-push" :code :device/closing :message "Previous device session is still closing"}
                       (:last-refusal (retirements runtime)))
                    "The refusal is projected as the REPL read it")
                (is (= [:closed :closed] (mapv (comp :status session/snapshot) [direct-push browser-push])))
                (is (= :active (:status (session/snapshot
                                         (show/connect-device! runtime :push-2 {:session-id "direct-push" :midi capture-midi
                                                                                :display direct}))))
                    "The ID connects again once its previous session has closed")
                (is (= #{"browser-push"} (:reconnectable (retirements runtime)))))))
          (finally (deliver release true)))))))

(defn- with-directory [f]
  (let [directory (.toFile (Files/createTempDirectory "tolglow-output-isolation-" (make-array FileAttribute 0)))]
    (try (f directory)
         (finally (doseq [file (reverse (file-seq directory))] (io/delete-file file))))))

(defn- capture-transport [order closed]
  {:open! (constantly :virtual-midi) :send! (fn [& _]) :close! (fn [_] (swap! order conj closed))})

(deftest a-stuck-session-holds-only-its-own-retirement-lane
  ;; #905: session A's display write is blocked. A restore retires A, B and
  ;; the restored device C; B closes and C reopens without waiting for A, and
  ;; A stays :closing until its write returns.
  (with-directory
    (fn [directory]
      (let [runtime (assoc (show/create-runtime {:workspace-options nil :workers 1})
                           :recovery (atom (recovery/start! (recovery/options {:enabled? false :directory directory})
                                                            false (constantly nil) identity)))
            block? (atom false) entered (promise) release (promise) blocked (atom 0)
            order (atom []) lease (Object.) reopened (promise)
            stuck-options {:session-id "stuck-push" :midi capture-midi
                           :display (assoc (blocking-display block? entered release blocked)
                                           :close! (fn [_] (swap! order conj :a-closed)))}
            b-options {:session-id "other-midi" :transport (capture-transport order :b-closed)}]
        (try
          (show/save-show! runtime (io/file directory "named.transit"))
          (show/connect-device! runtime :midi {:session-id "checkpoint-button" ::show/restore-lease lease
                                               :transport (capture-transport order :c-closed)})
          (show/connect-device! runtime :midi b-options)
          (show/connect-device! runtime :push-2 stuck-options)
          (reset! block? true)
          (show/tick! runtime 25)
          (is (instance? Thread (deref entered bound-ms nil)) "A's display write is blocked")
          (let [projection (browser/project runtime [] {:kind :show})
                entry (some #(when (= [:show :main] (:key %)) %) (:items projection))
                confirmation (:confirmation (browser/preview runtime [] {:runtime-id (:runtime-id projection)
                                                                          :selection (:selection entry)
                                                                          :source (get-in entry [:sources :named])}))
                restore (browser/confirm-load! runtime [] confirmation
                                               {:session-id "checkpoint-button" :lease lease
                                                :after-restore! (fn [_] (swap! order conj :c-reopened) (deliver reopened true))})]
            (is (= :submitted (:status restore)))
            (bounded "The restoring tick" #(show/tick! runtime 50))
            (is (= :accepted (:status (deref (:receipt restore) bound-ms nil))))
            (is (true? (deref reopened bound-ms nil)) "C reopened while A's write is still blocked")
            (eventually "B to close" #(some #{:b-closed} @order))
            (is (= 1 @blocked) "A's write is still blocked")
            (is (= [:c-closed :c-reopened] (filterv #{:c-closed :c-reopened} @order))
                "C reopened after the session it replaces had closed")
            (is (not-any? #{:a-closed} @order))
            (is (= #{"stuck-push"} (:closing (retirements runtime))) "Only A is still closing")
            (is (= {:session-id "stuck-push" :code :device/closing}
                   (refusal #(show/connect-device! runtime :push-2 stuck-options))))
            (is (= :active (:status (session/snapshot (show/connect-device! runtime :midi b-options))))
                "B's ID connects again without waiting for A")
            (deliver release true)
            (eventually "A to close" #(empty? (:closing (retirements runtime))))
            (is (= :a-closed (peek @order)))
            (is (= {:pending 0 :retired 3 :failed 0 :reopen-failed 0}
                   (select-keys (retirements runtime) [:pending :retired :failed :reopen-failed]))))
          (finally (deliver release true) (show/stop! runtime)))))))

(defn- retirement-threads
  "Live retirement lane threads for the session IDs that start with prefix."
  [prefix]
  (filter #(and (.isAlive ^Thread %) (.startsWith (.getName ^Thread %) (str "tolglow-device-retirement " prefix)))
          (keys (Thread/getAllStackTraces))))

(deftest retirement-lanes-end-with-their-work-over-many-epochs
  ;; One session's close stays blocked across many epoch changes. Every other
  ;; session's lane runs and ends; only the blocked lane, and its one thread,
  ;; remain until its close returns.
  (let [runtime (show/create-runtime {:model wash-model :workspace-options nil :workers 1})
        release (promise) blocked (atom 0) epochs 120
        connect! (fn [id close!] (show/connect-device! runtime :midi {:session-id id
                                                                      :transport {:open! (constantly :virtual-midi)
                                                                                  :send! (fn [& _]) :close! close!}}))]
    (try
      (connect! "leak-stuck" (fn [_] (block-until! release blocked)))
      (doseq [epoch (range epochs)]
        (connect! "leak-b" (fn [_]))
        (connect! "leak-c" (fn [_]))
        (let [restart (show/submit! runtime {:op :show/restart})]
          (bounded "The restarting tick" #(show/tick! runtime (* 25 (inc epoch))))
          (is (= :accepted (:status (deref restart bound-ms nil)))))
        (eventually "the free sessions to close" #(= #{"leak-stuck"} (:closing (retirements runtime)))))
      (is (= 1 @blocked) "The stuck close was blocked throughout")
      (eventually "the free lanes to end" #(= #{"leak-stuck"} (set (keys (:lanes @(:device-retirement-lanes runtime))))))
      (eventually "only the stuck lane's thread to remain" #(= 1 (count (retirement-threads "leak-"))))
      (is (= {:pending 1 :retired (* 2 epochs) :failed 0}
             (select-keys (retirements runtime) [:pending :retired :failed])))
      (deliver release true)
      (eventually "the stuck lane to end" #(empty? (:lanes @(:device-retirement-lanes runtime))))
      (eventually "no lane thread to remain" #(empty? (retirement-threads "leak-")))
      (is (= {:pending 0 :retired (inc (* 2 epochs)) :closing #{}}
             (select-keys (retirements runtime) [:pending :retired :closing])))
      (finally (deliver release true) (show/stop! runtime)))))

(deftest stop-bounds-its-wait-for-a-blocked-retirement-and-reports-it
  (let [block? (atom false) entered (promise) release (promise) blocked (atom 0)
        runtime (show/create-runtime {:model wash-model :workspace-options nil :workers 1})]
    (try
      (show/connect-device! runtime :push-2 {:session-id "stuck-push" :midi capture-midi
                                             :display (blocking-display block? entered release blocked)})
      (reset! block? true)
      (show/tick! runtime 25)
      (is (instance? Thread (deref entered bound-ms nil)))
      (let [restart (show/submit! runtime {:op :show/restart})]
        (bounded "The restarting tick" #(show/tick! runtime 50))
        (is (= :accepted (:status (deref restart bound-ms nil)))))
      (is (= #{"stuck-push"} (:closing (retirements runtime))))
      (bounded "stop!" #(show/stop! runtime))
      ;; A stopped show has no snapshot; read the retirement state it projects.
      (is (= 1 @blocked) "stop! returned while the close was still blocked")
      (is (= {:pending 1 :abandoned 1 :closing #{"stuck-push"}}
             (select-keys @(:device-retirement-state runtime) [:pending :abandoned :closing])))
      (is (= {:pending 1 :closing #{"stuck-push"}}
             (get-in @(:state runtime) [:status :cleanup-error :details])))
      (deliver release true)
      (eventually "the abandoned close to finish" #(zero? (:pending @(:device-retirement-state runtime))))
      (finally (deliver release true) (show/stop! runtime)))))

(deftest a-retirement-after-stop-closes-on-a-late-daemon-thread
  (let [runtime (show/create-runtime {:model wash-model :workspace-options nil :workers 1})
        closed (promise) then (promise)
        device (show/connect-device! runtime :midi {:session-id "late-midi"
                                                    :transport {:open! (constantly :virtual-midi) :send! (fn [& _])
                                                                :close! (fn [_] (deliver closed (Thread/currentThread)))}})]
    (try
      (reset! (:devices runtime) {})
      (show/stop! runtime)
      ;; A tick that committed a retirement as stop! ran reaches the stopped
      ;; worker. Its close must not hold that tick's thread either.
      (#'show/retire-devices! runtime [device] {:session-id "late-midi" :run! #(deliver then (Thread/currentThread))})
      (let [closer (deref closed bound-ms nil)]
        (is (instance? Thread closer))
        (is (not (identical? (Thread/currentThread) closer)))
        (is (.isDaemon ^Thread closer))
        (is (identical? closer (deref then bound-ms nil)) "The reopen follows on the same late thread"))
      ;; A stopped show has no snapshot; read the retirement state it projects.
      (eventually "the late close to be counted" #(zero? (:pending @(:device-retirement-state runtime))))
      (is (= {:pending 0 :retired 1 :failed 0 :late 1 :closing #{}}
             (select-keys @(:device-retirement-state runtime) [:pending :retired :failed :late :closing])))
      (finally (show/stop! runtime)))))

(deftest a-reconnect-racing-the-retiring-tick-is-refused
  ;; Hold the tick between the owner lock that detached the session and the
  ;; retirement it queues. A connect of that ID in this gap must still see it
  ;; as closing.
  (let [runtime (show/create-runtime {:model wash-model :workspace-options nil :workers 1})
        held (promise) release (promise)
        retire! @#'show/retire-devices!
        transport {:open! (constantly :virtual-midi) :send! (fn [& _]) :close! (fn [_])}
        connect! #(show/connect-device! runtime :midi {:session-id "racing-midi" :transport transport})]
    (try
      (connect!)
      (with-redefs-fn {#'show/retire-devices! (fn [rt devices then!]
                                                 (when (seq devices)
                                                   (deliver held true)
                                                   (deref release stall-ms nil))
                                                 (retire! rt devices then!))}
        (fn []
          (let [restart (show/submit! runtime {:op :show/restart})
                tick (future (show/tick! runtime 25) :ticked)]
            (try
              (is (true? (deref held bound-ms nil)) "The tick detached the session and is held")
              (is (= :accepted (:status (deref restart bound-ms nil))))
              (is (empty? @(:devices runtime)))
              (is (= {:session-id "racing-midi" :code :device/closing}
                     (refusal #(bounded "The racing connect" connect!))))
              (finally (deliver release true)))
            (is (= :ticked (deref tick bound-ms nil)) "The held tick finishes"))))
      (eventually "the detached session to close" #(empty? (:closing (retirements runtime))))
      (is (= :active (:status (session/snapshot (connect!)))) "The ID connects once its session has closed")
      (finally (deliver release true) (show/stop! runtime)))))

(deftest a-failing-close-or-reopen-never-leaves-a-retirement-pending
  (let [runtime (show/create-runtime {:model wash-model :workspace-options nil :workers 1})
        ;; An Error escapes session/close!, as a reloaded device namespace can throw.
        device (assoc-in (show/connect-device! runtime :midi {:session-id "broken-midi"
                                                              :transport {:open! (constantly :virtual-midi)
                                                                          :send! (fn [& _]) :close! (fn [_])}})
                         [:options :model!] #(throw (AssertionError. "close failed")))
        done (promise)]
    (try
      (reset! (:devices runtime) {})
      (#'show/retire-devices! runtime [device] {:session-id "broken-midi" :run! #(throw (LinkageError. "reopen failed"))})
      ;; The same lane, so it runs after both errors.
      (#'show/retire-devices! runtime [] {:session-id "broken-midi" :run! #(deliver done true)})
      (is (true? (deref done bound-ms nil)) "The worker survives both errors")
      (let [state (retirements runtime)]
        (is (= {:pending 0 :failed 1 :reopen-failed 1 :closing #{}}
               (select-keys state [:pending :failed :reopen-failed :closing])))
        (is (= {:stage :reopen :message "reopen failed"} (select-keys (:last-error state) [:stage :message]))))
      (finally (show/stop! runtime)))))

;; A Studio reconnect reads like the REPL's (#907)

(defn- studio-post [app path body]
  (let [response (app {:request-method :post :uri path
                       :headers {"host" "127.0.0.1" "content-type" "application/transit+json"
                                 "x-tolglow-form" "studio-v1" "accept" "application/transit+json"}
                       :body (java.io.ByteArrayInputStream. (.getBytes ^String (transit/encode body) "UTF-8"))})]
    [(:status response) (transit/decode (:body response))]))

(defn- repl-refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error [(ex-message error) (ex-data error)])))

(deftest a-studio-reconnect-is-refused-as-at-the-repl-while-closing
  (let [runtime (show/create-runtime {:model wash-model :workspace-options nil :workers 1})
        release (promise) blocked (atom 0) closes (atom 0)
        app (studio/handler runtime)
        reconnect! #(studio-post app "/api/devices/reconnect" {:session-id % :runtime-id (:id runtime)})]
    (try
      (show/connect-device! runtime :midi {:session-id "studio-midi"
                                           :transport {:open! (constantly :virtual-midi) :send! (fn [& _])
                                                       ;; Only the first session's close blocks.
                                                       :close! (fn [_] (when (= 1 (swap! closes inc))
                                                                         (block-until! release blocked)))}})
      (let [restart (show/submit! runtime {:op :show/restart})]
        (bounded "The restarting tick" #(show/tick! runtime 25))
        (is (= :accepted (:status (deref restart bound-ms nil)))))
      (eventually "the close to block" #(= 1 @blocked))
      (is (= {:closing #{"studio-midi"} :reconnectable #{"studio-midi"}}
             (select-keys (retirements runtime) [:closing :reconnectable])))
      (let [message "Previous device session is still closing"
            data {:session-id "studio-midi" :code :device/closing}]
        (is (= [message data] (repl-refusal #(show/reconnect-device! runtime "studio-midi"))))
        (is (= [400 {:message message :details data}] (reconnect! "studio-midi"))
            "Studio's reconnect is refused with the REPL's message and ex-data")
        (is (= {:refused 2 :last-refusal (assoc data :message message)}
               (select-keys (retirements runtime) [:refused :last-refusal]))))
      (is (= [400 {:message "No retired device session has this ID"
                   :details {:session-id "never-connected" :code :device/unknown}}]
             (reconnect! "never-connected")))
      (is (= :stale-runtime (get-in (studio-post app "/api/devices/reconnect" {:session-id "studio-midi" :runtime-id "other"})
                                    [1 :details :code])))
      (deliver release true)
      (eventually "the retired session to close" #(empty? (:closing (retirements runtime))))
      (is (= [200 {:status :accepted :session-id "studio-midi"}] (reconnect! "studio-midi")))
      (is (contains? @(:devices runtime) "studio-midi") "Studio reconnected the session with its last options")
      (is (= #{} (:reconnectable (retirements runtime))))
      (show/disconnect-device! runtime "studio-midi")
      (is (= :device/unknown (get-in (reconnect! "studio-midi") [1 :details :code]))
          "A deliberately disconnected session is not offered again")
      (finally (deliver release true) (show/stop! runtime)))))

;; 5. Many Studio clients and a slow one -------------------------------------

(deftest many-studio-clients-and-a-silent-one-leave-output-untouched
  (let [frames (range 40)
        expected (baseline frames)]
    (with-show {}
      (fn [{:keys [runtime] :as fixture}]
        (with-studio runtime
          (fn [port]
            (let [fast (mapv #(state-client port (str "fast-" %) :auto-ack? true) (range 31))
                  silent (state-client port "silent")
                  clients (atom (conj fast silent))]
              (try
                ;; Admission counts opened sockets, so let all 32 open first.
                (eventually "32 clients to receive their first view" #(every? (comp deref :latest) @clients))
                (let [refused (state-client port "one-too-many")]
                  (swap! clients conj refused)
                  (is (= 1013 (first (deref (:closed refused) bound-ms nil))) "The 33rd client is refused"))
                (let [silent-first (receive silent)
                      observed (drive! fixture frames)
                      final (frame-number runtime)]
                  (check-packets expected observed)
                  (eventually "every acknowledging client to reach the final frame"
                              #(every? (fn [client] (= final (::frame-number @(:latest client)))) fast))
                  (is (= :full (:kind silent-first)))
                  (is (empty? (drain silent)) "The silent client still holds its first view")
                  (is (not-any? #(realized? (:closed %)) (conj fast silent))))
                (finally (close-clients! @clients))))))))))
