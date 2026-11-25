(ns tolglow.studio.sync-host-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.connections :as connections]
            [tolglow.persistence :as persistence]
            [afterglow.sync :as sync]
            [afterglow.sync.session :as session])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]))

(defn- fixture []
  (let [opened (promise) closed (atom 0)
        runtime (show/create-runtime
                 {:sync-options
                  {:midi-factory (fn [_ _]
                                   {:open! (fn [receive!] (deliver opened receive!) :captured)
                                    :close! (fn [_] (swap! closed inc))})}})]
    {:runtime runtime :opened opened :closed closed}))

(defn- start-capture! [{:keys [runtime opened]} config]
  (show/sync-action! runtime :configure config)
  (show/sync-action! runtime :start {})
  (let [receive! (deref opened 2000 nil)]
    (is (ifn? receive!)) receive!))

(defn- message! [receive! time bytes]
  (receive! {:type :midi/message :source-id :midi-clock :time-ms time :bytes bytes}))

(deftest external-clock-drives-real-effect-frames-and-pauses
  (let [{:keys [runtime closed] :as f} (fixture)]
    (try
      (let [receive! (start-capture! f {:selected-source :midi-clock
                                       :midi {:enabled? true :input-index 0}})
            manager (connections/sync (:connections runtime))]
        (message! receive! 0 [0xfa])
        (doseq [i (range 25)] (message! receive! (* i (/ 500.0 24)) [0xf8]))
        (is (session/flush! manager))
        (show/tick! runtime 500)
        (is (< (Math/abs (- 1.0 (get-in (show/snapshot runtime) [:transport :beat]))) 1.0e-9))
        (is (true? (get-in (show/snapshot runtime) [:transport :playing?])))
        (message! receive! 500 [0xfc])
        (is (session/flush! manager))
        (show/tick! runtime 1000)
        (is (= 1.0 (get-in (show/snapshot runtime) [:transport :beat])))
        (is (false? (get-in (show/snapshot runtime) [:transport :playing?]))))
      (finally (show/stop! runtime)))
    (is (= 1 @closed))))

(deftest expressive-midi-uses-shared-held-instance-ownership
  (let [{:keys [runtime] :as f} (fixture)]
    (try
      (let [receive! (start-capture! f {:midi {:enabled? true :input-index 0 :control? true}
                                       :live {:mappings {60 {:cue-id :ember :mode :held}}}})
            manager (connections/sync (:connections runtime))]
        (message! receive! 0 [0x90 60 100])
        (is (session/flush! manager))
        (show/tick! runtime 100)
        (is (session/flush! manager))
        (let [id (:id (first (:instances (show/snapshot runtime))))]
          (is (string? id))
          (is (= :held (get-in @(:state runtime) [:model :instances id :launch-mode])))
          (let [gesture-id (get-in @(:state runtime) [:model :instances id :gesture-id])]
            (is (= {:status :fired :grid :off}
                   (select-keys (show/quantized-cue-status runtime gesture-id) [:status :grid]))))
          (message! receive! 100 [0x80 60 0])
          (is (session/flush! manager))
          (show/tick! runtime 200)
          (is (= :releasing (get-in @(:state runtime) [:model :instances id :phase])))
          (show/tick! runtime 1200)
          (is (empty? (:instances (show/snapshot runtime))))))
      (finally (show/stop! runtime)))))

(deftest captured-playback-grid-cancels-a-brief-hold-and-timestamps-the-next-press
  (let [{:keys [runtime] :as f} (fixture)]
    (try
      (let [receive! (start-capture! f {:midi {:enabled? true :input-index 0 :control? true}
                                       :live {:launch-grid {:beats 1}
                                              :mappings {60 {:cue-id :ember :mode :held}
                                                         61 {:cue-id :ice :grid :off}}}})
            manager (connections/sync (:connections runtime))
            midi-at! (fn [ingress tick bytes]
                       (message! receive! ingress bytes)
                       (is (session/flush! manager))
                       (show/tick! runtime tick))
            pending-id #(-> (show/snapshot runtime) :pending-cue-launches first :command-id)]
        (message! receive! 50 [0x90 60 100])
        (midi-at! 70 90 [0x80 60 0])
        (is (empty? (:pending-cue-launches (show/snapshot runtime))))
        (midi-at! 110 120 [0x90 60 100])
        (let [id (pending-id)]
          (is (= {:status :pending :due-beat 1}
                 (select-keys (show/quantized-cue-status runtime id) [:status :due-beat])))
          (midi-at! 140 150 [0x80 60 0])
          (is (= :cancelled (:status (show/quantized-cue-status runtime id))))
          (is (empty? (:instances (show/snapshot runtime)))))
        (midi-at! 170 200 [0x90 60 100])
        (let [id (pending-id)]
          (midi-at! 210 220 [0x90 61 100])
          (show/tick! runtime 499)
          (is (= [:ice] (mapv :cue-id (:instances (show/snapshot runtime)))))
          (show/tick! runtime 525)
          (let [receipt (show/quantized-cue-status runtime id)
                instance-id (:instance-id receipt)
                command (get-in @(:state runtime) [:model :control :history id :command])]
            (is (= :fired (:status receipt)))
            (is (= 170 (get-in receipt [:raw :time-ms])))
            (is (= {:time-ms 200 :beat 0.4} (:requested-at receipt)))
            (is (= {:time-ms 525 :beat 1.05} (:admitted-at receipt)))
            (is (= 1 (get-in command [:cue-timing :due-beat])))
            (midi-at! 530 550 [0x80 60 0])
            (is (= :releasing (get-in @(:state runtime) [:model :instances instance-id :phase]))))))
      (finally (show/stop! runtime)))))

(deftest settings-and-stale-callbacks-survive-owned-restore
  (let [{:keys [runtime] :as f} (fixture)]
    (try
      (let [receive! (start-capture! f {:selected-source :manual
                                       :midi {:enabled? true :input-index 0 :control? true}
                                       :live {:mappings {60 {:cue-id :ember :mode :held}}
                                              :pairing {:track-index 2 :clip-slot-index 4}}})
            source (pr-str (show/export-show runtime))
            old-callbacks (show/sync-callbacks runtime)
            reply (show/submit! runtime :import {:source source})]
        (show/tick! runtime 0)
        (is (= :accepted (:status @reply)))
        (is (= {:track-index 2 :clip-slot-index 4}
               (get-in (show/export-show runtime) [:model :integrations :sync :config :live :pairing])))
        (is (nil? ((:envelope! old-callbacks))))
        (is (false? ((:commands! old-callbacks) [{:op :cue/launch :cue-id :ember}])))
        (message! receive! 0 [0x90 60 127])
        (show/tick! runtime 100)
        (is (empty? (:instances (show/snapshot runtime)))))
      (finally (show/stop! runtime)))))

(deftest ring-sync-routes-are-inert-until-start
  (let [{:keys [runtime closed]} (fixture)]
    (try
      (let [handler (studio/handler runtime)
            response (handler {:request-method :get :uri "/api/sync"})
            data (json/read-str (:body response) :key-fn keyword)]
        (is (= 200 (:status response)))
        (is (= "manual" (get-in data [:clock :selected-source])))
        (is (false? (:active? data)))
        (is (= 400 (:status (handler {:request-method :post :uri "/api/live"
                                      :body (java.io.StringReader. "{\"protocol-version\":9,\"kind\":\"midi\"}")})))))
      (finally (show/stop! runtime)))
    (is (zero? @closed))))

(deftest old-browser-connection-actions-cannot-target-a-loaded-show
  (let [{:keys [runtime opened]} (fixture)]
    (try
      (let [identity (select-keys (show/snapshot runtime) [:runtime-id :epoch])
            payload {:runtime-id (:runtime-id identity) :expected-epoch (:epoch identity)}
            receipt (show/submit! runtime :import {:source (pr-str (show/export-show runtime))})]
        (show/tick! runtime 0)
        (is (= :accepted (:status @receipt)))
        (doseq [operation [:configure :start :source :handoff :stop]]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"previous show session"
                               (show/sync-action! runtime operation payload))))
        (doseq [operation [:start :stop]]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"previous show session"
                               (show/audio-action! runtime operation payload))))
        (is (not (realized? opened))))
      (finally (show/stop! runtime)))))

(defn- await-live [check]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop []
      (if-let [ready (check)] ready
          (if (< (System/nanoTime) deadline) (do (Thread/sleep 15) (recur))
              (throw (ex-info "Timed out waiting for Live bridge" {})))))))

(defn- with-fresh-live-peer! [callback port check!]
  (let [command ["node" "test/js/ableton-live/behavioral_host.js" callback (str port)]
        child (.start (doto (ProcessBuilder. ^java.util.List command)
                        (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)))]
    (try
      (with-open [input (.getOutputStream child)
                  output (io/reader (.getInputStream child))]
        (await-live #(.ready output))
        (check! (:port (json/read-str (.readLine output) :key-fn keyword))))
      (finally
        (when-not (.waitFor child 2 TimeUnit/SECONDS)
          (.destroyForcibly child)
          (.waitFor child 2 TimeUnit/SECONDS))))))

(defn- recording-destination [runtime]
  (get-in (show/sync-action! runtime :status {}) [:recording :destination-id]))

(defn- load-at! [runtime file time-ms]
  (let [receipt (show/load-show! runtime (str file))]
    (show/tick! runtime time-ms)
    (is (= :accepted (:status (deref receipt 2000 nil))))))

(deftest saved-pairings-reacquire-the-deck-and-live-clip-after-a-cold-restart
  (let [file (Files/createTempFile "tolglow-pairings-" ".transit" (make-array FileAttribute 0))
        callback-handler (atom nil)
        server (jetty/run-jetty #(@callback-handler %) {:host "127.0.0.1" :port 0 :join? false})
        callback (str "http://127.0.0.1:" (.getLocalPort (first (.getConnectors server))) "/api/live")
        deck {:device-number 2 :hardware-address "02:00:00:00:00:02"}
        clip {:track-index 0 :clip-slot-index 0 :expected-track-name "Lights" :expected-clip-name "Light sequence"}
        receive (atom nil)
        opens (atom 0)
        closes (atom 0)
        options {:workspace-options nil
                 :sync-options {:beat-link-factory (fn [_ _]
                                                     {:open! (fn [receive!]
                                                               (swap! opens inc)
                                                               (reset! receive receive!))
                                                      :close! (fn [_] (swap! closes inc))})}}
        beat {:type :beat-link/beat :source-id :beat-link :time-ms 1000 :bpm 123.0
              :beat-within-bar 3 :bar-meaningful? true}
        author (atom nil)]
    (try
      (with-fresh-live-peer! callback 0
        (fn [port]
          (let [runtime (show/create-runtime options)]
            (reset! callback-handler (studio/handler runtime))
            (try
              (show/sync-action! runtime :configure
                {:config {:selected-source :beat-link
                          :beat-link {:enabled? true :align-bars? true :peer deck}
                          :live {:enabled? true :automap? false :mappings {60 {:cue-id :ember}}
                                 :endpoint (str "http://127.0.0.1:" port "/live") :pairing clip}}})
              (is (zero? @opens))
              (show/sync-action! runtime :start {})
              (is (= "clip:3" (await-live #(recording-destination runtime))))
              (let [peer (assoc deck :address "192.0.2.18")
                    manager (connections/sync (:connections runtime))]
                (is (@receive {:type :peer/found :peer peer}))
                (is (@receive (assoc beat :peer peer :bpm 118.0)))
                (is (session/flush! manager))
                (show/tick! runtime 1000)
                (is (= 118.0 (get-in (show/snapshot runtime) [:transport :bpm])))
                (show/save-show! runtime (str file))
                (let [saved (get-in (persistence/read-string (slurp (.toFile file))) [:model :integrations :sync])]
                  (is (= deck (get-in saved [:config :beat-link :peer])))
                  (is (= clip (get-in saved [:config :live :pairing])))
                  (is (true? (:wanted? saved)))
                  (reset! author {:port port :run-id (sync/active-run-id manager) :receive! @receive :saved saved})))
              (finally (show/stop! runtime))))))
      (is (= 1 @opens @closes))
      (let [{:keys [port run-id receive! saved]} @author]
        (with-fresh-live-peer! callback port
          (fn [reopened-port]
            (is (= port reopened-port))
            (let [runtime (show/create-runtime options)]
              (reset! callback-handler (studio/handler runtime))
              (try
                (is (nil? (recording-destination runtime)))
                (is (= 1 @opens))
                (load-at! runtime file 0)
                (is (= 1000 (:time-ms (show/snapshot runtime))))
                (is (= "clip:3" (await-live #(recording-destination runtime))))
                (let [manager (connections/sync (:connections runtime))
                      current-run (sync/active-run-id manager)
                      callback {:protocol-version 1 :bridge-id "tolglow-m4l" :event-id "saved-sync-run"
                                :kind :midi :event {:type :note-on :note 60 :velocity 100 :channel 1}}
                      other (assoc deck :address "192.0.2.20" :hardware-address "02:00:00:00:00:03")
                      paired (assoc deck :address "192.0.2.21")]
                  (is (= 2 @opens))
                  (is (= saved (get-in (show/export-show runtime) [:model :integrations :sync])))
                  (is (string? current-run))
                  (is (not= run-id current-run))
                  (is (empty? (:peers (sync/status manager))))
                  (is (false? (receive! (assoc beat :peer paired))))
                  (is (false? (:accepted? (show/sync-action! runtime :live (assoc callback :sync-session-id run-id)))))
                  (is (empty? (:instances (show/snapshot runtime))))
                  (is (@receive {:type :peer/found :peer other}))
                  (is (@receive (assoc beat :peer other)))
                  (is (session/flush! manager))
                  (is (= :waiting (:status (session/clock-at manager 1000))))
                  (is (@receive {:type :peer/found :peer paired}))
                  (is (@receive (assoc beat :peer paired)))
                  (is (session/flush! manager))
                  (show/tick! runtime 1000)
                  (is (= {:selected-source :beat-link :status :locked}
                         (select-keys (session/clock-at manager 1000) [:selected-source :status])))
                  (is (= 123.0 (get-in (show/snapshot runtime) [:transport :bpm])))
                  (is (:accepted? (show/sync-action! runtime :live (assoc callback :sync-session-id current-run))))
                  (is (session/flush! manager))
                  (show/tick! runtime 1050)
                  (is (= [:ember] (mapv :cue-id (:instances (show/snapshot runtime)))))
                  (is (:stopped? (show/sync-action! runtime :stop {})))
                  (show/save-show! runtime (str file)))
                (finally (show/stop! runtime))))))
        (let [runtime (show/create-runtime options)]
          (try
            (load-at! runtime file 0)
            (is (= (assoc saved :wanted? false) (get-in (show/export-show runtime) [:model :integrations :sync])))
            (is (= :stopped (:state (show/sync-action! runtime :status {}))))
            (is (= 2 @opens @closes))
            (finally (show/stop! runtime)))))
      (finally
        (.stop server)
        (Files/deleteIfExists file)))))

(defn- exercise-live-parameter! [runtime manager host! instance-id]
  (let [source "live/ableton-live/parameter/cutoff"
        state #(get-in @(:state runtime) [:model :routing :ports source])
        spec {:version 1 :id :filter :revision 1
              :sources {source {:kind :automation :range [20 20000] :timeout {:beats 16}}}
              :destinations {:level {:cue-id :ember :param :level :range [0 1] :base 0 :combine :max}}
              :routes [{:id :cutoff :source source :destination :level}]}
        apply! (fn [op payload]
                 (let [receipt (show/submit! runtime op payload)] (show/tick! runtime) @receipt))]
    (is (:accepted? (show/sync-action! runtime :learn-parameter {:id "cutoff"})))
    (let [port (try (await-live #(do (show/tick! runtime) (state)))
                    (catch Exception e
                      (throw (ex-info "The learned Live parameter did not reach the patchbay"
                                      (select-keys (sync/status manager) [:last-error :last-execution :metrics])
                                      e))))
          saved (get-in (sync/export-state manager) [:config :live :observed-parameters "cutoff"])]
      (is (= :automation (:kind port)))
      (is (= "Filter" (get-in port [:port :device-name])))
      (is (= [20 20000] (:range port)))
      (is (= "live_set tracks 0 devices 2 chains 0 devices 0 parameters 3" (:path saved))))
    (is (= :accepted (:status (apply! :patch-save {:source (pr-str spec)}))))
    (is (= :accepted (:status (apply! :patch-enabled {:enabled? true}))))
    (is (pos? (:listeners (host! {:setParameter 15005}))))
    (await-live #(do (show/tick! runtime) (= 15005 (:value (state)))))
    (is (= 0.75 (get-in @(:state runtime) [:model :instances instance-id :params :level])))
    (let [show-file (show/export-show-source runtime)]
      (is (:accepted? (show/sync-action! runtime :forget-parameter {:id "cutoff"})))
      (await-live #(zero? (:listeners (host! {:listeners true}))))
      (is (empty? (get-in (sync/export-state manager) [:config :live :observed-parameters])))
      show-file)))

(defn- restore-live-parameter! [runtime show-file host!]
  (let [receipt (show/submit! runtime :import {:source show-file})]
    (show/tick! runtime)
    (is (= :accepted (:status @receipt))))
  (let [manager (connections/sync (:connections runtime))
        source "live/ableton-live/parameter/cutoff"]
    (is (string? (get-in (sync/export-state manager) [:config :live :observed-parameters "cutoff" :path])))
    (await-live #(pos? (:listeners (host! {:listeners true}))))
    (host! {:setParameter 10010})
    (await-live #(do (show/tick! runtime) (= 10010 (get-in @(:state runtime) [:model :routing :ports source :value]))))
    (is (:stopped? (show/sync-action! runtime :stop {})))
    (await-live #(zero? (:listeners (host! {:listeners true}))))
    (is (contains? (get-in (sync/export-state manager) [:config :live :observed-parameters]) "cutoff"))))

(defn- with-live-host! [runtime exercise!]
  (let [server (ring.adapter.jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        callback (str "http://127.0.0.1:" (.getLocalPort (first (.getConnectors server))) "/api/live")
        child (.start (doto (ProcessBuilder. ^java.util.List
                                             ["node" "test/js/ableton-live/behavioral_host.js" callback])
                         (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)))]
    (try
      (with-open [out (clojure.java.io/writer (.getOutputStream child))
                  in (clojure.java.io/reader (.getInputStream child))]
        (let [receive! (fn [] (await-live #(.ready in)) (json/read-str (.readLine in) :key-fn keyword))
              host! (fn [msg] (.write out (str (json/write-str msg) "\n")) (.flush out) (receive!))
              {port :port} (receive!)]
          (exercise! host! (str "http://127.0.0.1:" port "/live"))))
      (finally
        (show/stop! runtime)
        (.stop server)
        (.destroyForcibly child)
        (.waitFor child 2 java.util.concurrent.TimeUnit/SECONDS)))))

(defn- pair-session-clip! [runtime endpoint & [poll-ms]]
  (:config (show/sync-action! runtime :configure
             {:config {:live (cond-> {:enabled? true :endpoint endpoint :pairing {:track-index 0 :clip-slot-index 0}}
                               poll-ms (assoc :poll-ms poll-ms))}
              :page-id :main})))

(deftest recorded-and-edited-midi-clip-plays-through-real-loopback-bridge
  (let [runtime (show/create-runtime)
        manager (connections/sync (:connections runtime))]
    (with-live-host! runtime
      (fn [host! endpoint]
        (let [config (pair-session-clip! runtime endpoint)
              pitch (first (for [[note m] (get-in config [:live :mappings]) :when (= :ember (:cue-id m))] note))
              lane (some #(when (= [:ember :level] ((juxt :cue-id :param) %)) (:lane-note %))
                         (vals (get-in config [:live :parameters])))
              act! (fn [command] (let [reply (show/submit! runtime command)] (show/tick! runtime) @reply))]
          (is (every? integer? [pitch lane]))
          (show/sync-action! runtime :start {})
          (await-live #(get-in (sync/status manager) [:recording :destination-id]))
          (let [launch (act! {:op :cue/launch :cue-id :ember :launch {:mode :held :params {:level 0.2}}})
                id (:instance-id launch)]
            (is (= :accepted (:status launch)))
            (is (= :accepted (:status (act! {:op :param/set :instance-id id :param :level :value 0.9}))))
            (is (= :accepted (:status (act! {:op :instance/end :instance-id id}))))
            (let [notes (await-live #(let [n (host! {:notes true})] (when (= 3 (count n)) n)))]
              (is (= [26 114] (mapv :velocity (filter #(= lane (:pitch %)) notes))))
              (is (= pitch (:pitch (first notes)))))
            (host! {:finish true})
            (is (:edited (host! {:edit true :pitch lane :velocity 64 :beat 1})))
            (is (:accepted? (show/sync-action! runtime :arrange {:request-id "place-1" :song-beat 32})))
            (let [placed (await-live #(let [n (host! {:notes true :arrangement true})] (when (= 3 (count n)) n)))]
              (is (= 1 (:start_time (first (filter #(= lane (:pitch %)) placed))))))
            (host! {:edit true :pitch lane :velocity 1 :beat 0})
            (is (:accepted? (host! {:play true :pitch pitch :arrangement true})))
            (is (:accepted? (host! {:play true :pitch lane :arrangement true})))
            (is (session/flush! manager))
            (show/tick! runtime)
            (is (session/flush! manager))
            (show/tick! runtime)
            (let [instance (last (sort-by :order (vals (get-in @(:state runtime) [:model :instances]))))]
              (is (= :ember (:cue-id instance)))
              (is (= 0.5 (get-in instance [:params :level])))
              (let [show-file (exercise-live-parameter! runtime manager host! (:id instance))]
                (is (:accepted? (host! {:play true :pitch pitch :release true :arrangement true})))
                (is (session/flush! manager))
                (show/tick! runtime)
                (is (= :releasing (get-in @(:state runtime) [:model :instances (:id instance) :phase])))
                (restore-live-parameter! runtime show-file host!)))
            (is (= 3 (count (host! {:notes true}))))))))))

(deftest operator-gesture-writes-and-restores-on-an-editable-session-lane
  (let [clock (atom 0)
        runtime (show/create-runtime {:workers 1 :workspace-options nil})
        clock-time @#'show/clock-time
        manager (connections/sync (:connections runtime))
        source {:adapter :push-2 :session-id "captured"}
        authority {:operator-id "push-2/captured" :tolglow.show/push-source source}
        act! (fn [time command]
               (reset! clock time)
               (let [reply (show/submit! runtime :command command authority)]
                 (show/tick! runtime time)
                 (deref reply 1000 ::missing)))
        running #(->> (vals (get-in @(:state runtime) [:model :instances]))
                     (filter (comp #{:running} :phase)) first)]
    (with-redefs-fn {#'show/clock-time (fn [owner-clock] (if (identical? owner-clock (:clock runtime)) @clock (clock-time owner-clock)))}
      (fn [] (with-live-host! runtime
         (fn [host! endpoint]
           (let [config (pair-session-clip! runtime endpoint 10000)
                 pitch (some (fn [[note mapping]] (when (= :ember (:cue-id mapping)) note))
                             (get-in config [:live :mappings]))
                 lane (some (fn [mapping] (when (= [:ember :level] ((juxt :cue-id :param) mapping)) (:lane-note mapping)))
                            (vals (get-in config [:live :parameters])))
                 notes #(host! {:notes true})
                 restore! (fn [time source destination]
                            (let [receipt (show/submit! runtime :import {:source source})]
                              (reset! clock time)
                              (show/tick! runtime time)
                              (is (= :accepted (:status (deref receipt 1000 ::missing)))))
                            (let [restored (connections/sync (:connections runtime))
                                  recording (await-live #(let [status (:recording (sync/status restored))]
                                                           (when (= destination (:destination-id status)) status)))]
                              (is (true? (:recording? recording)))))
                 play! (fn [time note beat & [release?]]
                         (reset! clock time)
                         (is (:accepted? (host! {:play true :pitch note :atBeat beat :release release?})))
                         (is (session/flush! (connections/sync (:connections runtime))))
                         (show/tick! runtime time))]
             (show/sync-action! runtime :start {})
             (await-live #(get-in (sync/status manager) [:recording :destination-id]))
             (let [launch (act! 0 {:command-id "launch" :op :cue/launch :cue-id :ember
                                   :launch {:mode :held :params {:level 0.25}}})
                   id (:instance-id launch)
                   turn {:source source :instance-id id :param :level :control-id "encoder-0" :gesture-id "turn"}
                   accent (assoc turn :gesture-id "accent")
                   sample (assoc turn :op :gesture/sample :command-id "second" :sequence 2 :value 0.75)]
               (is (= :accepted (:status launch)))
               (is (= :accepted (:status (act! 25 (assoc turn :op :gesture/begin)))))
               (is (= :accepted (:status (act! 125 (assoc turn :op :gesture/sample :sequence 1 :value 0.5)))))
               (let [receipt (act! 250 sample)]
                 (is (= :accepted (:status receipt)))
                 (is (= receipt (act! 255 sample))))
               (is (= :accepted (:status (act! 270 (assoc turn :op :gesture/sample :sequence 3 :value 0.75)))))
               (is (= :stale-sample (:code (act! 275 (assoc turn :op :gesture/sample :sequence 1 :value 0.1)))))
               (is (= :accepted (:status (act! 290 (assoc turn :op :gesture/end :last-sequence 3)))))
               (is (= :accepted (:status (act! 330 (assoc accent :op :gesture/begin :temporary? true)))))
               (is (= :accepted (:status (act! 375 (assoc accent :op :gesture/sample :sequence 1 :value 1.0)))))
               (is (= 1.0 (get-in (running) [:params :level])))
               (is (= :accepted (:status (act! 500 (assoc accent :op :gesture/cancel)))))
               (is (= 0.75 (get-in (running) [:params :level])))
               (show/snapshot runtime)
               (show/export-show-source runtime)
               (show/tick! runtime 550)
               (is (= :accepted (:status (act! 625 {:op :instance/end :instance-id id}))))
               (let [recorded (await-live #(let [captured (notes)]
                                             (when (some (fn [note] (== 1.25 (:duration note))) captured) captured)))]
                 (is (= [[pitch 2.0 127] [lane 2.0 33] [lane 2.25 64]
                         [lane 2.5 96] [lane 2.75 127] [lane 3.0 96]]
                        (mapv (juxt :pitch (comp double :start_time) :velocity) recorded)))
                 (restore! 650 (show/export-show-source runtime) "clip:3")
                 (is (= recorded (notes)))
                 (is (:edited (host! {:edit true :pitch lane :atBeat 2.5 :velocity 1 :beat 2.625})))
                 (let [edited (notes)]
                   (play! 1500 pitch 2.0)
                   (play! 1510 lane 2.0)
                   (is (= (/ 32.0 126.0) (get-in (running) [:params :level])))
                   (play! 1625 lane 2.25)
                   (is (= 0.5 (get-in (running) [:params :level])))
                   (play! 1812.5 lane 2.625)
                   (is (= 0.0 (get-in (running) [:params :level])))
                   (is (zero? (get-in (show/snapshot runtime) [:output :values :head-1 :intensity])))
                   (play! 1875 lane 2.75)
                   (is (= 1.0 (get-in (running) [:params :level])))
                   (play! 2000 lane 3.0)
                   (is (= (/ 95.0 126.0) (get-in (running) [:params :level])))
                   (let [replayed-id (:id (running))]
                     (is (and (string? replayed-id) (not= id replayed-id)))
                     (play! 2125 pitch 2.0 true)
                     (is (= :releasing (get-in @(:state runtime) [:model :instances replayed-id :phase]))))
                   (is (= edited (notes)))
                   (let [other (assoc-in (show/export-show runtime)
                                         [:model :integrations :sync :config :live :pairing :clip-slot-index] 1)]
                     (restore! 2250 (pr-str other) "clip:5")
                     (show/snapshot runtime)
                     (show/tick! runtime 2300)
                     (is (empty? (host! {:notes true :slot 1})))
                     (is (= edited (notes))))))))))))))
