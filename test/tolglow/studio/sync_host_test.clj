(ns tolglow.studio.sync-host-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [tolglow.studio :as studio]
            [tolglow.studio.connections :as connections]
            [tolglow.studio.sync :as sync]))

(defn- fixture []
  (let [opened (promise) closed (atom 0)
        runtime (studio/create-runtime
                 {:sync-options
                  {:midi-factory (fn [_ _]
                                   {:open! (fn [receive!] (deliver opened receive!) :captured)
                                    :close! (fn [_] (swap! closed inc))})}})]
    {:runtime runtime :opened opened :closed closed}))

(defn- start-capture! [{:keys [runtime opened]} config]
  (studio/sync-action! runtime :configure config)
  (studio/sync-action! runtime :start {})
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
        (is (sync/flush! manager))
        (studio/tick! runtime 500)
        (is (< (Math/abs (- 1.0 (get-in (studio/snapshot runtime) [:transport :beat]))) 1.0e-9))
        (is (true? (get-in (studio/snapshot runtime) [:transport :playing?])))
        (message! receive! 500 [0xfc])
        (is (sync/flush! manager))
        (studio/tick! runtime 1000)
        (is (= 1.0 (get-in (studio/snapshot runtime) [:transport :beat])))
        (is (false? (get-in (studio/snapshot runtime) [:transport :playing?]))))
      (finally (studio/stop! runtime)))
    (is (= 1 @closed))))

(deftest expressive-midi-uses-shared-held-instance-ownership
  (let [{:keys [runtime] :as f} (fixture)]
    (try
      (let [receive! (start-capture! f {:midi {:enabled? true :input-index 0 :control? true}
                                       :live {:mappings {60 {:cue-id :ember :mode :held}}}})
            manager (connections/sync (:connections runtime))]
        (message! receive! 0 [0x90 60 100])
        (is (sync/flush! manager))
        (studio/tick! runtime 100)
        (is (sync/flush! manager))
        (let [id (:id (first (:instances (studio/snapshot runtime))))]
          (is (string? id))
          (is (= :held (get-in @(:state runtime) [:model :instances id :launch-mode])))
          (message! receive! 100 [0x80 60 0])
          (is (sync/flush! manager))
          (studio/tick! runtime 200)
          (is (= :releasing (get-in @(:state runtime) [:model :instances id :phase])))
          (studio/tick! runtime 1200)
          (is (empty? (:instances (studio/snapshot runtime))))))
      (finally (studio/stop! runtime)))))

(deftest settings-and-stale-callbacks-survive-owned-restore
  (let [{:keys [runtime] :as f} (fixture)]
    (try
      (let [receive! (start-capture! f {:selected-source :manual
                                       :midi {:enabled? true :input-index 0 :control? true}
                                       :live {:mappings {60 {:cue-id :ember :mode :held}}
                                              :pairing {:track-index 2 :clip-slot-index 4}}})
            source (pr-str (studio/export-show runtime))
            old-callbacks (studio/sync-callbacks runtime)
            reply (studio/submit! runtime :import {:source source})]
        (studio/tick! runtime 0)
        (is (= :accepted (:status @reply)))
        (is (= {:track-index 2 :clip-slot-index 4}
               (get-in (studio/export-show runtime) [:model :integrations :sync :config :live :pairing])))
        (is (nil? ((:envelope! old-callbacks))))
        (is (false? ((:commands! old-callbacks) [{:op :cue/launch :cue-id :ember}])))
        (message! receive! 0 [0x90 60 127])
        (studio/tick! runtime 100)
        (is (empty? (:instances (studio/snapshot runtime)))))
      (finally (studio/stop! runtime)))))

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
      (finally (studio/stop! runtime)))
    (is (zero? @closed))))

(deftest old-browser-connection-actions-cannot-target-a-loaded-show
  (let [{:keys [runtime opened]} (fixture)]
    (try
      (let [identity (select-keys (studio/snapshot runtime) [:runtime-id :epoch])
            payload {:runtime-id (:runtime-id identity) :expected-epoch (:epoch identity)}
            receipt (studio/submit! runtime :import {:source (pr-str (studio/export-show runtime))})]
        (studio/tick! runtime 0)
        (is (= :accepted (:status @receipt)))
        (doseq [operation [:configure :start :source :handoff :stop]]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"previous show session"
                               (studio/sync-action! runtime operation payload))))
        (doseq [operation [:start :stop]]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"previous show session"
                               (studio/audio-action! runtime operation payload))))
        (is (not (realized? opened))))
      (finally (studio/stop! runtime)))))

(defn- await-live [check]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop []
      (if-let [ready (check)] ready
          (if (< (System/nanoTime) deadline) (do (Thread/sleep 15) (recur))
              (throw (ex-info "Timed out waiting for Live bridge" {})))))))

(deftest recorded-and-edited-midi-clip-plays-through-real-loopback-bridge
  (let [runtime (studio/create-runtime)
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
              config (:config (studio/sync-action! runtime :configure
                               {:config {:live {:enabled? true :endpoint (str "http://127.0.0.1:" port "/live")
                                                :pairing {:track-index 0 :clip-slot-index 0}}} :page-id :main}))
              pitch (first (for [[note m] (get-in config [:live :mappings]) :when (= :ember (:cue-id m))] note))
              lane (some #(when (= [:ember :level] ((juxt :cue-id :param) %)) (:lane-note %))
                         (vals (get-in config [:live :parameters])))
              act! (fn [command] (let [reply (studio/submit! runtime command)] (studio/tick! runtime) @reply))]
          (is (every? integer? [pitch lane]))
          (studio/sync-action! runtime :start {})
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
            (is (:accepted? (studio/sync-action! runtime :arrange {:request-id "place-1" :song-beat 32})))
            (let [placed (await-live #(let [n (host! {:notes true :arrangement true})] (when (= 3 (count n)) n)))]
              (is (= 1 (:start_time (first (filter #(= lane (:pitch %)) placed))))))
            (host! {:edit true :pitch lane :velocity 1 :beat 0})
            (is (:accepted? (host! {:play true :pitch pitch :arrangement true})))
            (is (:accepted? (host! {:play true :pitch lane :arrangement true})))
            (is (sync/flush! manager))
            (studio/tick! runtime)
            (is (sync/flush! manager))
            (studio/tick! runtime)
            (let [instance (last (sort-by :order (vals (get-in @(:state runtime) [:model :instances]))))]
              (is (= :ember (:cue-id instance)))
              (is (= 0.5 (get-in instance [:params :level])))
              (is (:accepted? (host! {:play true :pitch pitch :release true :arrangement true})))
              (is (sync/flush! manager))
              (studio/tick! runtime)
              (is (= :releasing (get-in @(:state runtime) [:model :instances (:id instance) :phase]))))
            (is (= 3 (count (host! {:notes true})))))))
      (finally
        (studio/stop! runtime)
        (.stop server)
        (.destroyForcibly child)
        (.waitFor child 2 java.util.concurrent.TimeUnit/SECONDS)))))
