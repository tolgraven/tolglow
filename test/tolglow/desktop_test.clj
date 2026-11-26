(ns tolglow.desktop-test
  "The Mac app runs one show process per window (#669). These tests start two
  desktop shows in one JVM through the same entry point, each on its own
  loopback listener, and check that they stay apart."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [tolglow.desktop :as desktop]
            [tolglow.desktop.launch :as launch]
            [tolglow.application :as app]
            [tolglow.modules :as modules]
            [tolglow.show :as show]
            [tolglow.show.recovery :as recovery]
            [tolglow.studio :as studio])
  (:import [java.io File PipedInputStream PipedOutputStream]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Duration]))

(deftest device-free-shows-keep-their-preview-and-drop-every-device
  (let [cfg {:init {:components [:show] :modules [:osc :audio :web-server]}
             :show {:hz 40 :artnet {:host "10.0.0.9"} :recovery false}
             :osc {:enabled true :port-in 16010}
             :audio true
             :web-server {:enabled true :port 8500}}
        free (launch/device-free cfg "Club night")]
    (is (= {:hz 40 :recovery false
            :preview-only {:owner "Club night"
                           :message "Club night owns this Mac's devices and output, so this show is preview-only."}}
           (:show free))
        "physical output is removed and the show is marked preview-only")
    (doseq [id launch/device-modules]
      (is (false? (:enabled (modules/settings free id))) (str id " is disabled")))
    (is (= 16010 (get-in free [:osc :port-in])) "module options stay for when the show owns devices again")
    (is (= (:web-server cfg) (:web-server free)) "Studio still starts")
    (is (= cfg (launch/desktop-config cfg {:devices? true})))
    (is (= free (launch/desktop-config cfg {:devices? false :owner "Club night"})))
    (is (re-find #"^Another window's show" (get-in (launch/device-free {} nil) [:show :preview-only :message])))
    (is (= {:window? true :devices? false :owner "Club night"}
           (launch/desktop-options {"TOLGLOW_DESKTOP_WINDOW" "1" "TOLGLOW_DESKTOP_DEVICES" "none"
                                    "TOLGLOW_DESKTOP_DEVICE_OWNER" "Club night"})))
    (is (= {:window? false :devices? true :owner nil} (launch/desktop-options {})))))

(defn- root-cause [^Throwable error]
  (loop [e error] (if-let [cause (and e (.getCause e))] (if (:code (ex-data e)) e (recur cause)) e)))

(defn- refusal [f]
  (try (f) ::not-refused
       (catch clojure.lang.ExceptionInfo error (select-keys (ex-data error) [:code :http/status]))))

(deftest a-preview-only-show-refuses-every-device-opener-at-runtime
  (let [message "Club night owns this Mac's devices and output, so this show is preview-only."
        r (show/start! {:recovery false :preview-only {:message message}})
        handler (studio/handler r)
        post (fn [uri body]
               (let [response (handler {:request-method :post :uri uri
                                        :headers {"content-type" "application/json" "accept" "application/json"}
                                        :body (java.io.ByteArrayInputStream. (.getBytes (json/write-str body) "UTF-8"))})]
                 [(:status response) (json/read-str (:body response) :key-fn keyword)]))
        coded {:code :preview-only :http/status 409}]
    (try
      (let [{:keys [epoch runtime-id]} (show/snapshot r)
            identity {:runtime-id runtime-id :expected-epoch epoch}]
        (is (= message (:preview-only (show/snapshot r))) "Studio sees why")
        (testing "the owner refuses each opener, whoever calls it"
          (is (= coded (refusal #(show/connect-device! r :osc {:session-id "osc"}))) "REPL device connection")
          (is (= coded (refusal #(show/rdm-action! r :connect (assoc identity :config {})))) "RDM")
          (is (= coded (refusal #(show/audio-action! r :start identity))) "audio input")
          (is (= coded (refusal #(show/audio-action! r :return-start identity))) "audio returns")
          (is (= coded (refusal #(show/sync-action! r :start identity))) "MIDI clock and Link")
          (is (= coded (refusal #(show/sensor-action! r :osc-start (assoc identity :host "127.0.0.1" :port 0))))
              "sensor OSC listener")
          (is (= coded (refusal #(show/sensor-action! r :stage-start identity))) "audience stage"))
        (testing "output arming is refused with the same code"
          (doseq [[type op] [[:calibration :start] [:photometry :start] [:photometry :start-surface] [:fog-calibration :start]]]
            (let [ack (deref (show/submit! r type (assoc identity :op op :session-id "s")) 5000 ::timeout)]
              (is (= [:rejected :preview-only] [(:status ack) (:code ack)]) (str type " " op)))))
        (testing "Studio's routes answer 409 with the code"
          (doseq [[uri body] [["/api/audio/start" identity] ["/api/sync/start" identity]
                              ["/api/sensors/osc/start" (assoc identity :host "127.0.0.1" :port 0)]
                              ["/api/rdm/connect" (assoc identity :config {})]
                              ["/api/preview/airplay/start" {:address "127.0.0.1"}]]]
            (let [[status response] (post uri body)]
              (is (= 409 status) uri)
              (is (= "preview-only" (get-in response [:details :code])) uri)
              (is (= message (:message response)) uri))))
        (testing "everything else still works"
          (is (= :accepted (:status (deref (show/submit! r {:op :blackout/set :value true}) 5000 ::timeout))))))
      (finally (show/stop! r))))
  (testing "a show that owns the devices is not refused"
    (let [r (show/start! {:recovery false})]
      (try (is (nil? (show/require-devices! r :audio-start)))
           (is (nil? (:preview-only (show/snapshot r))))
           (finally (show/stop! r))))))

(deftest configured-device-modules-cannot-open-on-a-preview-only-show
  (doseq [module [:rdm :push-3-display]]
    (let [cfg {:init {:components [:show] :modules [module]}
               :show {:recovery false :preview-only {:message "Preview only."}}
               module {:enabled true}}]
      (is (= :preview-only (:code (ex-data (root-cause (try (app/start! cfg) nil (catch Exception e e))))))
          (str module " is refused at startup")))))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "tolglow-recovery-test" (make-array FileAttribute 0))))

(deftest each-folder-keeps-its-own-recovery-by-a-stable-identity
  (let [base (temp-dir)
        a (temp-dir)
        b (temp-dir)
        id-a (launch/folder-id! a)]
    (is (uuid? (parse-uuid id-a)))
    (is (= id-a (launch/folder-id! a)) "the identity is written once")
    (let [moved (io/file (temp-dir) "renamed show")]
      (Files/move (.toPath a) (.toPath moved) (make-array java.nio.file.CopyOption 0))
      (is (= id-a (launch/folder-id! moved)) "renaming or moving the folder keeps its identity")
      (let [claim-a (launch/claim-folder-recovery! moved base)
            claim-b (launch/claim-folder-recovery! b base)]
        (try
          (is (= (io/file base "folders" id-a) (:directory claim-a)))
          (is (not= (:directory claim-a) (:directory claim-b)))
          (testing "a copied folder running beside its original gets its own identity"
            (let [copy (temp-dir)]
              (io/copy (io/file moved launch/folder-id-name) (io/file copy launch/folder-id-name))
              (let [claim-copy (launch/claim-folder-recovery! copy base)]
                (try
                  (is (not= id-a (:id claim-copy)))
                  (is (= (:id claim-copy) (launch/folder-id! copy)))
                  (is (= id-a (launch/folder-id! moved)) "the original keeps its identity")
                  (finally ((:release! claim-copy)))))))
          (finally ((:release! claim-a)) ((:release! claim-b))))
        (let [again (launch/claim-folder-recovery! moved base)]
          (try (is (= id-a (:id again)) "a released folder is claimed again under the same identity")
               (finally ((:release! again)))))))
    (testing "a folder that cannot hold an identity keeps the shared directory"
      (let [locked (temp-dir)]
        (.setWritable locked false)
        (try (is (nil? (launch/claim-folder-recovery! locked base)))
             (finally (.setWritable locked true)))))
    (testing "desktop-config points recovery at the claimed directory unless the show chose otherwise"
      (is (= "/r" (get-in (launch/desktop-config {} {:recovery-directory "/r"}) [:show :recovery :directory])))
      (is (= {:interval-ms 500 :directory "/r"}
             (get-in (launch/desktop-config {:show {:recovery {:interval-ms 500}}} {:recovery-directory "/r"}) [:show :recovery])))
      (is (= {:show {:recovery false}} (launch/desktop-config {:show {:recovery false}} {:recovery-directory "/r"})))
      (is (false? (launch/scopes-recovery? {:show {:recovery {:directory "/elsewhere"}}})))
      (is (= {} (launch/desktop-config {} {}))))))

(deftest the-shared-previous-session-is-offered-to-one-folder-once
  (let [base (temp-dir)
        legacy (recovery/show-directory base :main)
        a (temp-dir)
        b (temp-dir)]
    (.mkdirs legacy)
    (spit (io/file legacy "recovery.transit") "previous session")
    (let [claim-a (launch/claim-folder-recovery! a base)
          claim-b (launch/claim-folder-recovery! b base)]
      (try
        (is (= "previous session" (slurp (io/file (recovery/show-directory (:directory claim-a) :main) "recovery.transit")))
            "the first folder opened after the upgrade offers it")
        (is (not (.exists (recovery/show-directory (:directory claim-b) :main))) "a later folder does not")
        (is (= "previous session" (slurp (io/file legacy "recovery.transit"))) "the shared file stays for other hosts")
        (is (false? (launch/offer-legacy-recovery! base (:directory claim-a) :main)) "and it is offered once")
        (finally ((:release! claim-a)) ((:release! claim-b)))))
    (testing "a folder with its own checkpoint is never given the shared one"
      (let [base (temp-dir) legacy (recovery/show-directory base :main) dir (io/file base "folders" "x")]
        (.mkdirs legacy) (spit (io/file legacy "recovery.transit") "old")
        (.mkdirs (recovery/show-directory dir :main)) (spit (io/file (recovery/show-directory dir :main) "recovery.transit") "own")
        (is (false? (launch/offer-legacy-recovery! base dir :main)))
        (is (= "own" (slurp (io/file (recovery/show-directory dir :main) "recovery.transit"))))))))

(defn- temp-folder [config]
  (let [folder (.toFile (Files/createTempDirectory "tolglow-desktop-test" (make-array FileAttribute 0)))]
    (when config (spit (io/file folder "tolglow.edn") (pr-str config)))
    folder))

(def ^:private show-config
  {:init {:components [:show] :modules [:web-server]} :show {:recovery false}})

(defn- start-show! [^File folder]
  (let [ready (io/file folder "port")
        input (PipedInputStream.)
        writer (PipedOutputStream. input)
        running (future (desktop/run-show! (.getCanonicalFile folder) (.toPath ready) input {:devices? false}))]
    {:folder folder :ready ready :writer writer :running running}))

(defn- await-port [{:keys [ready running]}]
  (loop [deadline (+ (System/currentTimeMillis) 60000)]
    (cond
      (.isFile ^File ready) (parse-long (slurp ready))
      (realized? running) @running
      (> (System/currentTimeMillis) deadline) (throw (ex-info "Desktop show did not start" {}))
      :else (do (Thread/sleep 50) (recur deadline)))))

(defn- stop-show! [{:keys [^PipedOutputStream writer running]}]
  (.close writer)
  (deref running 30000 ::still-running))

(def ^:private client (HttpClient/newHttpClient))

(defn- http [port method path & [body]]
  (let [builder (-> (HttpRequest/newBuilder (URI. (str "http://127.0.0.1:" port path)))
                    (.timeout (Duration/ofSeconds 10))
                    (.header "Accept" "application/json"))
        request (if body
                  (-> builder (.header "Content-Type" "application/json")
                      (.POST (HttpRequest$BodyPublishers/ofString (json/write-str body))))
                  (.GET builder))
        response (.send ^HttpClient client (.build request) (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode response) :body (json/read-str (.body response) :key-fn keyword)}))

(deftest two-desktop-shows-refuse-each-others-commands
  (let [a (start-show! (temp-folder show-config))
        b (start-show! (temp-folder show-config))]
    (try
      (let [port-a (await-port a)
            port-b (await-port b)
            identity-a (:body (http port-a :get "/api/identity"))
            identity-b (:body (http port-b :get "/api/identity"))
            blackout (fn [port runtime-id]
                       (http port :post "/api/command" {:op "blackout/set" :value true :runtime-id runtime-id
                                                        :command-id (str (random-uuid))}))]
        (is (not= port-a port-b))
        (is (every? string? [(:runtime-id identity-a) (:runtime-id identity-b)]))
        (is (not= (:runtime-id identity-a) (:runtime-id identity-b)) "each window binds to its own runtime")
        (is (= (:show-id identity-a) (:show-id identity-b))
            "both new shows have the same show ID, so only the runtime ID tells them apart")
        (testing "a command naming show A's runtime is refused by show B"
          (let [{:keys [status body]} (blackout port-b (:runtime-id identity-a))]
            (is (= 422 status))
            (is (= "rejected" (:status body))))
          (is (false? (:blackout? (:body (http port-b :get "/api/state"))))))
        (testing "the same command naming show B's own runtime is accepted"
          (is (= "accepted" (:status (:body (blackout port-b (:runtime-id identity-b))))))
          (is (true? (:blackout? (:body (http port-b :get "/api/state")))))
          (is (false? (:blackout? (:body (http port-a :get "/api/state")))) "show A is untouched"))
        (testing "a failed second launch leaves the running shows alone"
          (let [broken (start-show! (temp-folder nil))]
            (is (thrown-with-msg? Exception #"tolglow.edn or show.clj" (await-port broken)))
            (is (= (:runtime-id identity-a) (:runtime-id (:body (http port-a :get "/api/identity")))))))
        (testing "closing one window's show releases its listener and keeps the other"
          (is (= -1 (stop-show! a)) "the show returns once its window closes the pipe")
          (is (thrown? java.io.IOException (http port-a :get "/api/identity")))
          (is (= (:runtime-id identity-b) (:runtime-id (:body (http port-b :get "/api/identity")))))))
      (finally
        (stop-show! a)
        (stop-show! b)))))
