(ns tolglow.desktop-test
  "The Mac app runs one show process per window (#669). These tests start two
  desktop shows in one JVM through the same entry point, each on its own
  loopback listener, and check that they stay apart."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell]
            [clojure.test :refer :all]
            [tolglow.desktop :as desktop]
            [tolglow.desktop.launch :as launch]
            [tolglow.application :as app]
            [tolglow.demo]
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

(deftest a-claim-reports-a-new-or-copied-folder-and-only-freezes-the-shared-checkpoint
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
        (is (= "previous session"
               (slurp (io/file (recovery/show-directory (io/file base recovery/before-folders-name) :main) "recovery.transit")))
            "the first claim freezes it where nothing autosaves")
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

(defn- show-source
  "A real show's checkpoint source after `ops`, as a desktop show autosaved it."
  [& ops]
  (let [runtime (show/start! {:recovery false})]
    (try
      (doseq [op ops] (is (= :accepted (:status @(show/submit! runtime op)))))
      (show/export-show-source runtime)
      (finally (show/stop! runtime)))))

(defn- legacy-source [] (show-source {:op :blackout/set :value true}))

(defn- write-shared! [base name source & [show-id]]
  (recovery/write-source! (io/file (recovery/show-directory base (or show-id :main)) name) source))

(defn- write-legacy! [base source] (write-shared! base "recovery.transit" source))

(defn- frozen-file [base name & [show-id]]
  (io/file (recovery/show-directory (io/file base recovery/before-folders-name) (or show-id :main)) name))

(defn- folder-show [base folder-id & [interval-ms]]
  (show/start! {:recovery {:directory (str (io/file base "folders" folder-id)) :legacy-directory (str base)
                           :interval-ms (or interval-ms 600000)}
                :show-folder {:id folder-id :file launch/folder-id-name :path (str "/shows/" folder-id) :new? true}}))

(defn- eventually [f]
  (loop [deadline (+ (System/currentTimeMillis) 5000)]
    (cond (f) true
          (> (System/currentTimeMillis) deadline) false
          :else (do (Thread/sleep 25) (recur deadline)))))

(deftest tests-never-use-the-operators-recovery-directory
  (is (not (.startsWith (.getPath (.getAbsoluteFile (recovery/default-directory))) (System/getProperty "user.home")))
      "inside clojure.test the default recovery directory is a temporary one")
  (is (= (recovery/default-directory) (:directory (recovery/options nil)))))

(deftest the-shared-checkpoints-are-frozen-once-at-the-first-folder-claim
  (let [base (temp-dir)
        before (show-source {:op :blackout/set :value true})
        prior (show-source {:op :blackout/set :value true} {:op :blackout/set :value false})]
    (write-shared! base "recovery.transit" before)
    (write-shared! base "prior.checkpoint" prior)
    (write-shared! base "recovery.transit" "a show that #739 copied" :copied)
    (spit (io/file (recovery/show-directory base :copied) "offered-to-folder") "folder-739\n")
    (write-shared! base "recovery.transit" "kept" :discarded)
    (write-shared! base "prior.checkpoint" (str "tolglow-discarded-sha256 " (apply str (repeat 64 "0"))) :discarded)
    (let [shared-time (.lastModified (io/file (recovery/show-directory base :main) "recovery.transit"))]
      (is (true? (recovery/freeze-shared! base)))
      (is (= before (slurp (frozen-file base "recovery.transit"))))
      (is (= prior (slurp (frozen-file base "prior.checkpoint"))))
      (is (= shared-time (.lastModified (frozen-file base "recovery.transit"))) "its saved time is kept")
      (is (not (.exists (frozen-file base "prior.checkpoint" :discarded))) "a discarded prior slot is not frozen")
      (is (= {:resolution :applied :folder-id "folder-739" :by :one-time-copy}
             (clojure.edn/read-string (slurp (frozen-file base recovery/legacy-resolution-name :copied))))
          "#739's one-time copy counts as applied to the folder that got it"))
    (testing "a later session in the shared directory is never offered as one from before the update"
      (write-legacy! base (show-source {:op :blackout/set :value false}))
      (is (nil? (recovery/freeze-shared! base)))
      (is (= before (slurp (frozen-file base "recovery.transit"))))
      (let [runtime (folder-show base "folder-l")]
        (try
          (is (= #{:legacy :legacy-prior} (:available (show/recovery-status runtime))))
          (is (= :accepted (:status @(show/load-latest-show! runtime :legacy))))
          (is (true? (:blackout? (show/snapshot runtime))) "the frozen session, not the later one")
          (is (= (recovery/source-hash before) (:sha256 (recovery/legacy-resolution @(:recovery runtime) :main)))
              "the record names the bytes that were restored")
          (finally (show/stop! runtime)))))
    (testing "a show whose session #739 already copied into a folder is not offered again"
      (let [runtime (show/start! {:model (assoc (tolglow.demo/model) :show-id :copied)
                                  :recovery {:directory (str (io/file base "folders" "x")) :legacy-directory (str base)
                                             :interval-ms 600000}})]
        (try (is (empty? (:checkpoints (show/recovery-status runtime))))
             (finally (show/stop! runtime)))))
    (testing "a base without shared checkpoints freezes an empty set, so nothing is offered later"
      (let [empty-base (temp-dir)]
        (is (true? (recovery/freeze-shared! empty-base)))
        (write-legacy! empty-base before)
        (is (nil? (recovery/freeze-shared! empty-base)))
        (let [runtime (folder-show empty-base "folder-n")]
          (try (is (empty? (:checkpoints (show/recovery-status runtime))))
               (finally (show/stop! runtime))))))))

(deftest the-checkpoint-from-before-the-update-is-offered-until-one-folder-takes-it
  (let [base (temp-dir)
        source (legacy-source)
        _ (write-legacy! base source)
        _ (recovery/freeze-shared! base)
        legacy-file (frozen-file base "recovery.transit")
        a (folder-show base "folder-a")
        b (folder-show base "folder-b" 100)]
    (try
      (testing "every folder lists it beside its own checkpoints and applies nothing"
        (doseq [runtime [a b]]
          (let [status (show/recovery-status runtime)]
            (is (= #{:legacy} (:available status)))
            (is (= (.lastModified legacy-file) (get-in status [:checkpoints :legacy :modified-at-ms])))
            (is (false? (:blackout? (show/snapshot runtime))) "the folder starts with its own state")
            (is (= {:legacy {:modified-at-ms (.lastModified legacy-file)}}
                   (get-in (show/snapshot runtime) [:show-folder :legacy-checkpoints]))))))
      (is (= "folder-a" (get-in (show/recovery-status a) [:folder :id])))
      (testing "applying it in one folder restores it there and stops every folder offering it"
        (let [ack @(show/load-latest-show! a :legacy)]
          (is (= :accepted (:status ack)))
          (is (nil? (:legacy-resolution-error ack))))
        (is (true? (:blackout? (show/snapshot a))))
        (is (false? (:blackout? (show/snapshot b))) "the other folder is untouched")
        (is (= source (slurp legacy-file)) "the frozen file stays")
        (is (= source (slurp (io/file (recovery/show-directory base :main) "recovery.transit"))) "and so does the shared one")
        (let [resolution (recovery/legacy-resolution @(:recovery a) :main)]
          (is (= {:resolution :applied :folder-id "folder-a" :folder "/shows/folder-a" :show-id :main
                  :file "recovery.transit" :kind :legacy :sha256 (recovery/source-hash source)}
                 (select-keys resolution [:resolution :folder-id :folder :show-id :file :kind :sha256]))))
        (is (not (contains? (:available (show/recovery-status a)) :legacy)))
        (is (nil? (get-in (show/snapshot a) [:show-folder :legacy-checkpoints])))
        (is (eventually #(nil? (get-in (show/snapshot b) [:show-folder :legacy-checkpoints])))
            "another open folder stops offering it by its next autosave")
        (is (not (contains? (:available (show/recovery-status b)) :legacy)) "a later read in another folder sees it taken")
        (is (thrown-with-msg? Exception #"already restored into show folder folder-a" (show/load-latest-show! b :legacy)))
        (is (false? (show/dismiss-legacy-recovery! b))))
      (finally (show/stop! a) (show/stop! b)))))

(deftest only-one-folder-can-take-or-dismiss-the-checkpoint-from-before-the-update
  (let [base (temp-dir)
        source (legacy-source)
        _ (write-legacy! base source)
        _ (write-shared! base "prior.checkpoint" (show-source))
        _ (recovery/freeze-shared! base)
        a (folder-show base "folder-a")
        b (folder-show base "folder-b")]
    (try
      (is (= #{:legacy :legacy-prior} (:available (show/recovery-status b))) "the prior slot is offered too")
      (let [reply (locking (:lock a)
                    ;; A's show owner cannot commit while this thread holds its lock.
                    (let [reply (show/load-latest-show! a :legacy)]
                      (testing "while A's restore is in flight, B can neither restore nor dismiss"
                        (is (= :legacy-busy (:code (ex-data (is (thrown? clojure.lang.ExceptionInfo
                                                                         (show/load-latest-show! b :legacy-prior)))))))
                        (is (= :legacy-busy (:code (ex-data (is (thrown? clojure.lang.ExceptionInfo
                                                                         (show/dismiss-legacy-recovery! b)))))))
                        (is (not (realized? reply))))
                      reply))]
        (is (= :accepted (:status @reply))))
      (is (= :applied (:resolution (recovery/legacy-resolution @(:recovery a) :main))))
      (is (= "folder-a" (:folder-id (recovery/legacy-resolution @(:recovery b) :main))) "not B, and not dismissed")
      (is (true? (:blackout? (show/snapshot a))))
      (is (false? (:blackout? (show/snapshot b))))
      (is (= :legacy-resolved (:code (ex-data (is (thrown? clojure.lang.ExceptionInfo (show/load-latest-show! b :legacy-prior))))))
          "the loser is told, never silently accepted")
      (finally (show/stop! a) (show/stop! b)))))

(defn- lock-held-by-another-process?
  "Ask a separate JVM whether it can lock `file`: true when it cannot."
  [file]
  (let [dir (temp-dir)
        probe (io/file dir "Probe.java")]
    (spit probe (str "import java.nio.channels.*; import java.nio.file.*;\n"
                     "public class Probe { public static void main(String[] a) throws Exception {\n"
                     "  try (FileChannel c = FileChannel.open(Path.of(a[0]), StandardOpenOption.CREATE, StandardOpenOption.WRITE)) {\n"
                     "    FileLock l = c.tryLock(); System.out.print(l == null ? \"held\" : \"free\"); } } }\n"))
    (let [java (str (io/file (System/getProperty "java.home") "bin" "java"))
          {:keys [out exit err]} (clojure.java.shell/sh java (str probe) (str file))]
      (is (zero? exit) err)
      (= "held" out))))

(deftest a-refused-caller-in-the-same-process-keeps-the-holders-lock
  (testing "the reservation of the checkpoint from before the update"
    (let [base (temp-dir)
          _ (write-legacy! base (legacy-source))
          _ (recovery/freeze-shared! base)
          a (folder-show base "folder-a")
          b (folder-show base "folder-b")]
      (try
        (let [held (recovery/reserve-legacy! @(:recovery a) :main)
              lock-file (io/file (:directory held) ".resolution.lock")]
          (try
            (is (= :legacy-busy (:code (ex-data @(future (try (recovery/reserve-legacy! @(:recovery b) :main)
                                                               (catch clojure.lang.ExceptionInfo error error)))))))
            (is (true? ((:valid? held))) "the refused caller did not close the holder's lock")
            (is (lock-held-by-another-process? lock-file) "another process still cannot take it")
            (finally ((:release! held))))
          (is (not (lock-held-by-another-process? lock-file)) "released, another process can take it"))
        (finally (show/stop! a) (show/stop! b)))))
  (testing "a folder's recovery directory, claimed again by a copy in the same process"
    (let [base (temp-dir)
          original (temp-dir)
          claim (launch/claim-folder-recovery! original base)
          lock-file (io/file (:directory claim) ".lock")]
      (try
        (let [copy (temp-dir)]
          (io/copy (io/file original launch/folder-id-name) (io/file copy launch/folder-id-name))
          (let [copy-claim (launch/claim-folder-recovery! copy base)]
            (try (is (not= (:id claim) (:id copy-claim)))
                 (finally ((:release! copy-claim))))))
        (is (lock-held-by-another-process? lock-file) "the original's claim still holds its directory")
        (finally ((:release! claim)))))))

(deftest a-dismissed-or-refused-checkpoint-from-before-the-update-keeps-its-files
  (testing "dismissing it stops the offer and keeps the files"
    (let [base (temp-dir)
          source (legacy-source)
          _ (write-legacy! base source)
          _ (recovery/freeze-shared! base)
          runtime (folder-show base "folder-d")]
      (try
        (is (true? (show/dismiss-legacy-recovery! runtime)))
        (let [resolution (recovery/legacy-resolution @(:recovery runtime) :main)]
          (is (= :dismissed (:resolution resolution)))
          (is (= {"recovery.transit" (recovery/source-hash source)} (:sha256 resolution))))
        (is (= source (slurp (frozen-file base "recovery.transit"))))
        (is (= source (slurp (io/file (recovery/show-directory base :main) "recovery.transit"))))
        (is (not (contains? (:available (show/recovery-status runtime)) :legacy)))
        (is (false? (show/dismiss-legacy-recovery! runtime)) "there is nothing left to dismiss")
        (is (false? (:blackout? (show/snapshot runtime))))
        (finally (show/stop! runtime)))))
  (testing "a restore the show refuses releases it, still offered"
    (let [base (temp-dir)
          _ (write-legacy! base "{:not \"a show\"}")
          _ (recovery/freeze-shared! base)
          runtime (folder-show base "folder-e")]
      (try
        (is (= :rejected (:status @(show/load-latest-show! runtime :legacy))))
        (is (contains? (:available (show/recovery-status runtime)) :legacy))
        (is (nil? (recovery/legacy-resolution @(:recovery runtime) :main)))
        (is (true? (show/dismiss-legacy-recovery! runtime)) "and nothing still holds it")
        (finally (show/stop! runtime)))))
  (testing "a stopped show cannot dismiss it"
    (let [base (temp-dir)
          _ (write-legacy! base (legacy-source))
          _ (recovery/freeze-shared! base)
          runtime (folder-show base "folder-s")]
      (show/stop! runtime)
      (is (thrown-with-msg? Exception #"Show stopped" (show/dismiss-legacy-recovery! runtime)))
      (is (nil? (recovery/legacy-resolution @(:recovery runtime) :main)))))
  (testing "a show that does not run from a Mac folder never offers it"
    (let [base (temp-dir)
          _ (write-legacy! base (legacy-source))
          _ (recovery/freeze-shared! base)
          runtime (show/start! {:recovery {:directory (str base) :legacy-directory (str base) :interval-ms 600000}})]
      (try
        (is (not (contains? (:available (show/recovery-status runtime)) :legacy))
            "the shared directory's own show reads it as its working checkpoint instead")
        (is (nil? (:show-folder (show/snapshot runtime))))
        (finally (show/stop! runtime))))))

(deftest the-offer-follows-a-change-of-show-id
  (let [base (temp-dir)
        other (show/start! {:recovery false :model (assoc (tolglow.demo/model) :show-id :other)})
        other-source (try (show/export-show-source other) (finally (show/stop! other)))
        saved (io/file (temp-dir) "other.transit")]
    (write-shared! base "recovery.transit" other-source :other)
    (recovery/freeze-shared! base)
    (spit saved other-source)
    (let [runtime (folder-show base "folder-i" 100)]
      (try
        (is (nil? (get-in (show/snapshot runtime) [:show-folder :legacy-checkpoints])) ":main has none")
        (is (= :accepted (:status @(show/load-show! runtime (str saved)))))
        (is (= :other (:show-id (show/snapshot runtime))))
        (is (eventually #(some? (get-in (show/snapshot runtime) [:show-folder :legacy-checkpoints :legacy])))
            "the open state offers the new show ID's checkpoint by the next autosave")
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
        (is (= (:folder catalog) (dissoc (:show-folder (state)) :legacy-checkpoints))
            "Studio's state stream carries the same folder identity")
        (is (number? (get-in (state) [:show-folder :legacy-checkpoints :legacy :modified-at-ms])))
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
          (is (nil? (get-in (state) [:show-folder :legacy-checkpoints])))
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
