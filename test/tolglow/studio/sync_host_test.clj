(ns tolglow.studio.sync-host-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.connections :as connections]
            [afterglow.sync :as sync]
            [afterglow.sync.session :as session]))

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
          (message! receive! 100 [0x80 60 0])
          (is (session/flush! manager))
          (show/tick! runtime 200)
          (is (= :releasing (get-in @(:state runtime) [:model :instances id :phase])))
          (show/tick! runtime 1200)
          (is (empty? (:instances (show/snapshot runtime))))))
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
    (is (:accepted? (show/sync-action! runtime :forget-parameter {:id "cutoff"})))
    (await-live #(zero? (:listeners (host! {:listeners true}))))))

(deftest recorded-and-edited-midi-clip-plays-through-real-loopback-bridge
  (let [runtime (show/create-runtime)
        server (ring.adapter.jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        callback (str "http://127.0.0.1:" (.getLocalPort (first (.getConnectors server))) "/api/live")
        child (.start (doto (ProcessBuilder. ^java.util.List
                                             ["node" "test/js/ableton-live/behavioral_host.js" callback])
                         (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)))
        manager (connections/sync (:connections runtime))]
    (try
      (with-open [out (clojure.java.io/writer (.getOutputStream child))
                  in (clojure.java.io/reader (.getInputStream child))]
        (let [receive! (fn [] (await-live #(.ready in)) (json/read-str (.readLine in) :key-fn keyword))
              host! (fn [msg] (.write out (str (json/write-str msg) "\n")) (.flush out) (receive!))
              {port :port} (receive!)
              config (:config (show/sync-action! runtime :configure
                               {:config {:live {:enabled? true :endpoint (str "http://127.0.0.1:" port "/live")
                                                :pairing {:track-index 0 :clip-slot-index 0}}} :page-id :main}))
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
            (is (= 3 (count (host! {:notes true})))))))
      (finally
        (show/stop! runtime)
        (.stop server)
        (.destroyForcibly child)
        (.waitFor child 2 java.util.concurrent.TimeUnit/SECONDS)))))
