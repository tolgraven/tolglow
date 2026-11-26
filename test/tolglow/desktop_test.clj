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
               (finally ((:release! again)))))
        (testing "a copied folder opened while its original is closed still gets its own identity"
          (let [copy (temp-dir)]
            (io/copy (io/file moved launch/folder-id-name) (io/file copy launch/folder-id-name))
            (let [claim-copy (launch/claim-folder-recovery! copy base)]
              (try (is (not= id-a (:id claim-copy)) "the copy does not take the original's recovery")
                   (finally ((:release! claim-copy)))))
            (let [claim-a (launch/claim-folder-recovery! moved base)]
              (try (is (= id-a (:id claim-a)) "the original keeps its identity and directory")
                   (finally ((:release! claim-a)))))))
        (testing "a folder moved after its last claim keeps its identity"
          (let [moved-again (io/file (temp-dir) "moved again")]
            (Files/move (.toPath moved) (.toPath moved-again) (make-array java.nio.file.CopyOption 0))
            (let [claim (launch/claim-folder-recovery! moved-again base)]
              (try (is (= id-a (:id claim)))
                   (finally ((:release! claim)))))
            (let [claim (launch/claim-folder-recovery! (io/file (str moved-again "/")) base)]
              (try (is (= id-a (:id claim)) "another spelling of the same folder is not a copy")
                   (finally ((:release! claim)))))))))
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

(deftest a-claim-reports-a-new-or-copied-folder-and-copies-nothing
  (let [base (temp-dir)
        legacy (recovery/show-directory base :main)
        a (temp-dir)]
    (.mkdirs legacy)
    (spit (io/file legacy "recovery.transit") "previous session")
    (let [first-claim (launch/claim-folder-recovery! a base)]
      (try
        (is (true? (:new? first-claim)) "the first open writes the identity file")
        (is (nil? (:copy-of first-claim)))
        (is (= (str (.getCanonicalFile a)) (:folder first-claim)))
        (is (not (.exists (recovery/show-directory (:directory first-claim) :main)))
            "the shared checkpoint is not copied into the folder's recovery")
        (is (= {:recovery-directory (:directory first-claim)
                :legacy-directory base
                :show-folder {:id (:id first-claim) :file ".tolglow-folder-id"
                              :path (str (.getCanonicalFile a)) :new? true}}
               (launch/claim-options first-claim)))
        (testing "a copy opened beside its running original names it"
          (let [copy (temp-dir)]
            (io/copy (io/file a launch/folder-id-name) (io/file copy launch/folder-id-name))
            (let [claim (launch/claim-folder-recovery! copy base)]
              (try (is (true? (:new? claim)))
                   (is (= (str (.getCanonicalFile a)) (:copy-of claim)))
                   (finally ((:release! claim)))))))
        (finally ((:release! first-claim)))))
    (let [again (launch/claim-folder-recovery! a base)]
      (try (is (false? (:new? again)) "a later open of the same folder is not new")
           (is (= {:id (:id again) :file ".tolglow-folder-id" :path (str (.getCanonicalFile a)) :new? false}
                  (:show-folder (launch/claim-options again))))
           (finally ((:release! again)))))
    (testing "a copy opened while its original is closed names it too"
      (let [copy (temp-dir)]
        (io/copy (io/file a launch/folder-id-name) (io/file copy launch/folder-id-name))
        (let [claim (launch/claim-folder-recovery! copy base)]
          (try (is (true? (:new? claim)))
               (is (= (str (.getCanonicalFile a)) (:copy-of claim)))
               (finally ((:release! claim)))))))
    (is (= "previous session" (slurp (io/file legacy "recovery.transit"))))
    (is (nil? (launch/claim-options nil)))
    (testing "desktop-config carries the shared directory and the folder identity to the show"
      (let [cfg (launch/desktop-config {} {:recovery-directory "/r" :legacy-directory "/shared"
                                           :show-folder {:id "x" :file ".tolglow-folder-id"}})]
        (is (= {:directory "/r" :legacy-directory "/shared"} (get-in cfg [:show :recovery])))
        (is (= {:id "x" :file ".tolglow-folder-id"} (get-in cfg [:show :show-folder])))))))

(defn- legacy-source
  "A real show's checkpoint source with blackout on, as an older desktop show
  autosaved it to the shared directory."
  []
  (let [runtime (show/start! {:recovery false})]
    (try
      (is (= :accepted (:status @(show/submit! runtime {:op :blackout/set :value true}))))
      (show/export-show-source runtime)
      (finally (show/stop! runtime)))))

(defn- write-legacy! [base source]
  (recovery/write-source! (io/file (recovery/show-directory base :main) "recovery.transit") source))

(defn- folder-show [base folder-id & [interval-ms]]
  (show/start! {:recovery {:directory (str (io/file base "folders" folder-id)) :legacy-directory (str base)
                           :interval-ms (or interval-ms 600000)}
                :show-folder {:id folder-id :file launch/folder-id-name :path (str "/shows/" folder-id) :new? true}}))

(deftest the-checkpoint-from-before-the-update-is-offered-until-one-folder-takes-it
  (let [base (temp-dir)
        source (legacy-source)
        _ (write-legacy! base source)
        legacy-file (io/file (recovery/show-directory base :main) "recovery.transit")
        a (folder-show base "folder-a")
        b (folder-show base "folder-b" 100)]
    (try
      (testing "every folder lists it beside its own checkpoints and applies nothing"
        (doseq [runtime [a b]]
          (let [status (show/recovery-status runtime)]
            (is (= #{:legacy} (:available status)))
            (is (= (.lastModified legacy-file) (get-in status [:checkpoints :legacy :modified-at-ms])))
            (is (= "folder-a" (get-in (show/recovery-status a) [:folder :id])))
            (is (false? (:blackout? (show/snapshot runtime))) "the folder starts with its own state")
            (is (= {:modified-at-ms (.lastModified legacy-file)}
                   (get-in (show/snapshot runtime) [:show-folder :legacy-checkpoint]))))))
      (testing "applying it in one folder restores it there and stops every folder offering it"
        (let [ack @(show/load-latest-show! a :legacy)]
          (is (= :accepted (:status ack)))
          (is (nil? (:legacy-resolution-error ack))))
        (is (true? (:blackout? (show/snapshot a))))
        (is (false? (:blackout? (show/snapshot b))) "the other folder is untouched")
        (is (= source (slurp legacy-file)) "the shared file stays for lein run and other hosts")
        (let [resolution (recovery/legacy-resolution @(:recovery a) :main)]
          (is (= {:resolution :applied :folder-id "folder-a" :folder "/shows/folder-a" :show-id :main
                  :file "recovery.transit"}
                 (select-keys resolution [:resolution :folder-id :folder :show-id :file])))
          (is (re-matches #"[0-9a-f]{64}" (:sha256 resolution))))
        (is (not (contains? (:available (show/recovery-status a)) :legacy)))
        (is (nil? (get-in (show/snapshot a) [:show-folder :legacy-checkpoint])))
        (is (loop [deadline (+ (System/currentTimeMillis) 5000)]
              (cond (nil? (get-in (show/snapshot b) [:show-folder :legacy-checkpoint])) true
                    (> (System/currentTimeMillis) deadline) false
                    :else (do (Thread/sleep 25) (recur deadline))))
            "another open folder stops offering it by its next autosave")
        (is (not (contains? (:available (show/recovery-status b)) :legacy)) "a later read in another folder sees it taken")
        (is (thrown-with-msg? Exception #"unavailable" (show/load-latest-show! b :legacy)))
        (is (false? (show/dismiss-legacy-recovery! b))))
      (finally (show/stop! a) (show/stop! b)))))

(deftest a-dismissed-or-refused-checkpoint-from-before-the-update-keeps-its-files
  (testing "dismissing it stops the offer and keeps the files"
    (let [base (temp-dir)
          source (legacy-source)
          _ (write-legacy! base source)
          runtime (folder-show base "folder-d")]
      (try
        (is (true? (show/dismiss-legacy-recovery! runtime)))
        (is (= :dismissed (:resolution (recovery/legacy-resolution @(:recovery runtime) :main))))
        (is (= source (slurp (io/file (recovery/show-directory base :main) "recovery.transit"))))
        (is (not (contains? (:available (show/recovery-status runtime)) :legacy)))
        (is (false? (show/dismiss-legacy-recovery! runtime)) "there is nothing left to dismiss")
        (is (false? (:blackout? (show/snapshot runtime))))
        (finally (show/stop! runtime)))))
  (testing "a restore the show refuses leaves it offered"
    (let [base (temp-dir)
          _ (write-legacy! base "{:not \"a show\"}")
          runtime (folder-show base "folder-e")]
      (try
        (is (= :rejected (:status @(show/load-latest-show! runtime :legacy))))
        (is (contains? (:available (show/recovery-status runtime)) :legacy))
        (is (nil? (recovery/legacy-resolution @(:recovery runtime) :main)))
        (finally (show/stop! runtime)))))
  (testing "a show that does not run from a Mac folder never offers it"
    (let [base (temp-dir)
          _ (write-legacy! base (legacy-source))
          runtime (show/start! {:recovery {:directory (str base) :legacy-directory (str base) :interval-ms 600000}})]
      (try
        (is (not (contains? (:available (show/recovery-status runtime)) :legacy))
            "the shared directory's own show reads it as its working checkpoint instead")
        (is (nil? (:show-folder (show/snapshot runtime))))
        (finally (show/stop! runtime))))))

(defn- temp-folder [config]
  (let [folder (.toFile (Files/createTempDirectory "tolglow-desktop-test" (make-array FileAttribute 0)))]
    (when config (spit (io/file folder "tolglow.edn") (pr-str config)))
    folder))

(def ^:private show-config
  {:init {:components [:show] :modules [:web-server]} :show {:recovery false}})

(defn- start-show! [^File folder & [options]]
  (let [ready (io/file folder "port")
        input (PipedInputStream.)
        writer (PipedOutputStream. input)
        running (future (desktop/run-show! (.getCanonicalFile folder) (.toPath ready) input
                                           (merge {:devices? false} options)))]
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

(defn- repl-eval
  "Evaluate `source` in the show's own REPL session, where `runtime` is its
  handle, as Studio's Develop console does; the printed result."
  [port source]
  (let [request (-> (HttpRequest/newBuilder (URI. (str "http://127.0.0.1:" port "/api/repl/eval")))
                    (.timeout (Duration/ofSeconds 20))
                    (.header "Content-Type" "application/json")
                    (.POST (HttpRequest$BodyPublishers/ofString
                            (json/write-str {:source source :request-id (str (random-uuid)) :session-id "main"})))
                    (.build))
        body (.body (.send ^HttpClient client request (HttpResponse$BodyHandlers/ofString)))
        events (keep #(when (.startsWith ^String % "data: ") (json/read-str (subs % 6) :key-fn keyword))
                     (clojure.string/split-lines body))]
    (:result (last events))))

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

(deftest a-folder-that-restores-at-startup-only-offers-the-checkpoint-from-before-the-update
  (let [base (temp-dir)
        source (legacy-source)
        _ (write-legacy! base source)
        folder (temp-folder {:init {:components [:show] :modules [:web-server :show-state]}
                             :show {:recovery {:interval-ms 600000}}
                             :show-state {:restore :recovery}})
        running (start-show! folder {:recovery-base base})]
    (try
      (let [port (await-port running)
            state #(:body (http port :get "/api/state"))
            {:keys [runtime-id show-id epoch]} (state)
            catalog (:body (http port :get "/api/show/checkpoints"))
            folder-id (slurp (io/file folder launch/folder-id-name))]
        (is (false? (:blackout? (state))) "the startup restore skipped it")
        (is (= ["legacy"] (map name (keys (:checkpoints catalog)))))
        (is (= {:id (clojure.string/trim folder-id) :file ".tolglow-folder-id"
                :path (str (.getCanonicalFile folder)) :new? true}
               (:folder catalog)))
        (is (= (:folder catalog) (dissoc (:show-folder (state)) :legacy-checkpoint))
            "Studio's state stream carries the same folder identity")
        (is (number? (get-in (state) [:show-folder :legacy-checkpoint :modified-at-ms])))
        (is (= "true" (repl-eval port "(contains? (:available (tolglow.show/recovery-status runtime)) :legacy)"))
            "Studio's REPL session reads the same checkpoints through its runtime")
        (testing "Studio's restore applies it to this folder only on request"
          (let [{:keys [status body]} (http port :post "/api/show/checkpoints/restore"
                                            {:runtime-id runtime-id :show-id show-id :epoch epoch :kind "legacy"})]
            (is (= 200 status))
            (is (= "accepted" (:status body))))
          (is (true? (:blackout? (state))))
          (is (empty? (:checkpoints (:body (http port :get "/api/show/checkpoints")))))
          (is (= "false" (repl-eval port "(contains? (:available (tolglow.show/recovery-status runtime)) :legacy)")))
          (is (nil? (get-in (state) [:show-folder :legacy-checkpoint])))
          (is (= source (slurp (io/file (recovery/show-directory base :main) "recovery.transit")))))
        (testing "a second folder is no longer offered it, and Studio's dismissal says so"
          (let [other (start-show! (temp-folder (assoc show-config :show {:recovery {:interval-ms 600000}}))
                                   {:recovery-base base})]
            (try
              (let [port (await-port other)
                    {:keys [runtime-id show-id epoch]} (:body (http port :get "/api/state"))
                    {:keys [status body]} (http port :post "/api/show/checkpoints/dismiss-legacy"
                                                {:runtime-id runtime-id :show-id show-id :epoch epoch})]
                (is (empty? (:checkpoints (:body (http port :get "/api/show/checkpoints")))))
                (is (= "rejected" (:status body)) (str status))
                (is (re-find #"no longer offered" (:message body))))
              (finally (stop-show! other))))))
      (finally (stop-show! running)))))
