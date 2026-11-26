(ns tolglow.desktop-test
  "The Mac app runs one show process per window (#669). These tests start two
  desktop shows in one JVM through the same entry point, each on its own
  loopback listener, and check that they stay apart."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [tolglow.desktop :as desktop]
            [tolglow.desktop.launch :as launch]
            [tolglow.modules :as modules]
            [tolglow.show.recovery :as recovery])
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
        free (launch/device-free cfg)]
    (is (= {:hz 40 :recovery false} (:show free)) "physical output is removed, other show settings stay")
    (doseq [id launch/device-modules]
      (is (false? (:enabled (modules/settings free id))) (str id " is disabled")))
    (is (= 16010 (get-in free [:osc :port-in])) "module options stay for when the show owns devices again")
    (is (= (:web-server cfg) (:web-server free)) "Studio still starts")
    (is (= cfg (launch/desktop-config cfg "" {:devices? true})))
    (is (= free (launch/desktop-config cfg "" {:devices? false})))
    (is (= {:devices? false} (launch/device-options "none")))
    (is (= {:devices? true} (launch/device-options nil)))))

(deftest each-folder-gets-its-own-recovery-directory
  (let [a (launch/folder-recovery-directory "/tmp/show-a")
        b (launch/folder-recovery-directory "/tmp/show-b")]
    (is (not= a b))
    (is (= a (launch/folder-recovery-directory "/tmp/show-a/../show-a")) "the canonical folder names it")
    (is (= (io/file (recovery/default-directory) "folders") (.getParentFile a)))
    (is (= (str a) (get-in (launch/scope-recovery {} "/tmp/show-a") [:show :recovery :directory])))
    (is (= {:interval-ms 500 :directory (str a)}
           (get-in (launch/scope-recovery {:show {:recovery {:interval-ms 500}}} "/tmp/show-a") [:show :recovery])))
    (is (= {:show {:recovery false}} (launch/scope-recovery {:show {:recovery false}} "/tmp/show-a")))
    (is (= {:show {:recovery {:directory "/elsewhere"}}}
           (launch/scope-recovery {:show {:recovery {:directory "/elsewhere"}}} "/tmp/show-a")))))

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
