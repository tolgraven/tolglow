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
            [tolglow.transit :as transit])
  (:import [java.net URI]
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

(defn- with-studio
  "Serve the actual Studio Ring application for runtime on a lane port."
  [runtime port f]
  (let [server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port port :join? false})]
    (try (f port) (finally (.stop server)))))

(defn- frame-number [runtime] (:frame-number (show/snapshot runtime)))

;; 1. Studio clients that never or slowly acknowledge -------------------------

(deftest studio-clients-that-withhold-acks-leave-output-untouched
  (let [frames (range 48)
        expected (baseline frames)]
    (with-show {}
      (fn [{:keys [runtime] :as fixture}]
        (with-studio runtime 19800
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
             (with-studio runtime 19801
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
          (with-studio runtime 19802
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
                          received (drain debug)]
                      (check-packets expected observed)
                      (is (<= 1 (count received) 2) "Each unacknowledged debug lane holds one message")
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
              hub (workers/create coordinator compute-token)
              server (jetty/run-jetty (partial workers/response hub) {:host "127.0.0.1" :port 19803 :join? false})]
          (try
            (let [{:keys [socket received welcomed]} (stalled-peer 19803)]
              (try
                (is (true? (deref welcomed bound-ms nil)))
                (let [first-frame (drive! fixture [0])]
                  ;; Measure from the frame after the peer holds the program.
                  (eventually "the peer to prepare the program"
                              #(some (comp seq :ready) (vals @(:peers coordinator))))
                  (let [observed (into first-frame (drive! fixture (rest frames)))
                        {:keys [cpu-fallbacks] pool-status :pool} (compute/status (:compute runtime))]
                    (check-packets expected observed)
                    (is (= 1 (count (filter #{:run} @received))) "The stalled peer was offered exactly one job")
                    (is (= {:workers 1 :busy 1 :completed 0 :missed 1}
                           (select-keys pool-status [:workers :busy :completed :missed]))
                        "One frame waited for its deadline; later frames rendered locally without waiting")
                    (is (= (count frames) cpu-fallbacks) "Every frame rendered its fields locally")))
                (finally (.abort ^WebSocket socket))))
            (finally (workers/close! hub) (.stop server))))))))

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

(deftest blocked-display-writes-leave-output-untouched-through-an-epoch-change
  (let [frames (range 48)
        restart (import-at 24)
        expected (baseline {} frames restart)
        block? (atom false) release (promise) blocked (atom 0)
        direct-entered (promise) browser-entered (promise)
        direct {:open! (constantly :display) :close! (fn [_])
                :render! (fn [_ _] (when @block? (deliver direct-entered (Thread/currentThread)) (block-until! release blocked)))}
        browser (display/transport
                 {:capture {:open! (constantly :capture) :close! (fn [_]) :capture! (fn [_ view] view)}
                  :sink {:open! (constantly :sink) :close! (fn [_])
                         :render! (fn [_ _] (when @block? (deliver browser-entered (Thread/currentThread))
                                              (block-until! release blocked)))}
                  :max-fps 60 :shutdown-timeout-ms 50})]
    (try
      (with-show {}
        (fn [{:keys [runtime] :as fixture}]
          (let [direct-push (show/connect-device! runtime :push-2 {:session-id "direct-push" :midi capture-midi
                                                                     :display direct})
                browser-push (show/connect-device! runtime :push-2 {:session-id "browser-push" :midi capture-midi
                                                                      :display browser})
                retirements #(get-in (show/snapshot runtime) [:status :device-retirement])
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
                (is (pos? (:pending (retirements)))
                    "The session whose writer holds its lock is still closing, off the render thread")
                (is (= 2 (+ (:pending (retirements)) (:retired (retirements)))))
                (deliver release true)
                (eventually "the blocked session to retire" #(zero? (:pending (retirements))))
                (is (= {:pending 0 :retired 2 :failed 0} (retirements)))
                (is (= [:closed :closed] (mapv (comp :status session/snapshot) [direct-push browser-push]))))))))
      (finally (deliver release true)))))

;; 5. Many Studio clients and a slow one -------------------------------------

(deftest many-studio-clients-and-a-silent-one-leave-output-untouched
  (let [frames (range 40)
        expected (baseline frames)]
    (with-show {}
      (fn [{:keys [runtime] :as fixture}]
        (with-studio runtime 19804
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
