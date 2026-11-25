(ns tolglow.show.recovery-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.persistence :as persistence]
            [tolglow.show :as show]
            [tolglow.show.recovery :as recovery]
            [tolglow.studio :as studio]
            [tolglow.transit :as wire])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- with-directory [f]
  (let [directory (.toFile (Files/createTempDirectory "tolglow-recovery-test-" (make-array FileAttribute 0)))]
    (try (f directory)
         (finally (doseq [file (reverse (file-seq directory))] (io/delete-file file))))))

(defn- await-result [predicate]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop []
      (or (predicate)
          (when (< (System/nanoTime) deadline)
            (Thread/sleep 10)
            (recur))))))

(deftest defaults-and-ineligible-write-preserve-the-last-good-file
  (is (= {:enabled? true :interval-ms 10000}
         (select-keys (recovery/options nil) [:enabled? :interval-ms])))
  (is (false? (:enabled? (recovery/options false))))
  (with-directory
    (fn [directory]
      (let [file (io/file directory "show.transit")]
        (is (= (.getAbsolutePath file) (recovery/write-source! file "last-good")))
        (is (nil? (recovery/write-source! file "retired-generation" (constantly nil))))
        (is (= "last-good" (slurp file)))
        (is (= ["show.transit"] (mapv #(.getName %) (.listFiles directory))))))))

(deftest working-recovery-and-the-bookmarked-manual-save-load-independently
  (with-directory
    (fn [directory]
      (let [entered (promise) finish (promise) first-write? (atom true)
            manual (io/file directory "named.transit")
            write! recovery/write-source!]
        (with-redefs [recovery/write-source!
                      (fn [path source & [publish!]]
                        (when (and (= "recovery.transit" (.getName (io/file path)))
                                   (compare-and-set! first-write? true false))
                          (deliver entered true)
                          (deref finish 5000 nil))
                        (if publish! (write! path source publish!) (write! path source)))]
          (let [runtime (show/start! {:workspace-options nil :recovery {:directory directory :interval-ms 35}}
                                     (constantly nil))
                owner @(:recovery runtime)
                model #(-> @(:state runtime) :model)]
            (try
              (is (true? (deref entered 5000 false)))
              (let [frame (:frame-number (model))]
                (show/save-show! runtime manual)
                (is (= :accepted (:status (deref (show/submit! runtime {:op :master/set :value 0.25}) 5000 {}))))
                (is (await-result #(> (:frame-number (model)) frame)))
                (is (nil? (:last-saved (show/recovery-status runtime)))))
              (deliver finish true)
              (let [id (:show-id (model))
                    recovery-file #(recovery/latest-file owner id :recovery)
                    stored-master #(some-> (recovery-file) slurp persistence/read-string :model :master)]
                (is (await-result #(= 0.25 (stored-master))))
                (recovery/close! owner)
                (is (= manual (recovery/latest-file owner id :named)))
                (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reserved for show recovery"
                                      (show/save-show! runtime (recovery-file))))
                (is (nil? (recovery/latest-file owner :another-show :recovery)))
                (is (= :accepted (:status (deref (show/load-latest-show! runtime :named) 5000 {}))))
                (is (= 1.0 (:master (model))))
                (is (= :accepted (:status (deref (show/load-latest-show! runtime :recovery) 5000 {}))))
                (is (= 0.25 (:master (model)))))
              (finally (deliver finish true) (show/stop! runtime)))))))))

(deftest configured-startup-finds-checkpoints-after-reopening
  (with-directory
    (fn [directory]
      (let [named (io/file directory "named.transit")
            config (assoc (app/default-config) :web-server false
                          :show {:workspace-options nil :recovery {:directory directory :interval-ms 35}}
                          :show-state {})
            runtime (app/start! config)]
        (try
          (show/save-show! runtime named)
          (is (= :accepted (:status (deref (show/submit! runtime {:op :master/set :value 0.4}) 5000 {}))))
          (let [owner @(:recovery runtime)
                captured-master #(some-> (recovery/latest-file owner :main :recovery) slurp
                                          persistence/read-string :model :master)]
            (is (await-result #(= 0.4 (captured-master)))))
          (finally (app/stop! runtime)))
        (let [config (assoc-in config [:show :recovery :enabled?] false)
              reopened (app/start! (assoc-in config [:show-state :restore] :recovery))]
          (try
            (is (nil? (:executor @(:recovery reopened))))
            (is (= #{:recovery :named :prior} (:available (show/recovery-status reopened))))
            (is (= 0.4 (:master (show/snapshot reopened))))
            (let [working (recovery/latest-file @(:recovery reopened) :main :recovery)
                  different (show/create-runtime {:workspace-options nil :model (assoc (demo/model) :show-id :other)})]
              (try (show/save-show! different named) (finally (show/stop! different)))
              (is (= :rejected (:status @(show/load-latest-show! reopened :named))))
              (spit working "[\"~#cmap\"")
              (is (= :rejected (:status @(show/load-latest-show! reopened :recovery))))
              (is (= 0.4 (:master (show/snapshot reopened)))))
            (finally (app/stop! reopened))))))))

(deftest startup-restore-precedes-background-writes
  (with-directory
    (fn [directory]
      (let [show-options {:workspace-options nil :recovery {:directory directory :interval-ms 20}}
            runtime (show/start! show-options (constantly nil))]
        (try
          (is (= :accepted (:status @(show/submit! runtime {:op :master/set :value 0.3}))))
          (let [working #(recovery/latest-file @(:recovery runtime) :main :recovery)]
            (is (await-result #(= 0.3 (some-> (working) slurp persistence/read-string :model :master)))))
          (finally (show/stop! runtime)))
        (let [config {:init {:components [:show] :modules [:slow-init :show-state]}
                      :show show-options :slow-init true :show-state {:restore :recovery}}
              registry (assoc app/registry :slow-init
                              (fn [context _]
                                (let [starting (get-in context [:modules :show])]
                                  (Thread/sleep 150)
                                  (is (nil? (:last-saved (show/recovery-status starting)))))
                                {:value :ready}))
              reopened (app/start! config registry)]
          (try
            (is (= 0.3 (:master (show/snapshot reopened))))
            (is (await-result #(:last-saved (show/recovery-status reopened))))
            (finally (app/stop! reopened))))))))

(defn- checkpoint-master [runtime kind]
  (some-> (recovery/latest-file @(:recovery runtime) (:show-id (show/snapshot runtime)) kind)
          slurp persistence/read-string :model :master))

(defn- set-master! [runtime value]
  (is (= :accepted (:status (deref (show/submit! runtime {:op :master/set :value value}) 5000 {})))))

(deftest manual-save-protects-recovery-slots-through-symbolic-links
  (with-directory
    (fn [directory]
      (let [storage (io/file directory "storage")
            recovery-dir (io/file directory "recovery")
            relocated (io/file directory "prior.checkpoint")
            alias (io/file directory "alias-to-working.transit")
            attributes (make-array FileAttribute 0)]
        (.mkdir storage)
        (Files/createSymbolicLink (.toPath recovery-dir) (.toPath storage) attributes)
        (try
          (let [runtime (show/start! {:workspace-options nil :recovery {:directory recovery-dir :enabled? false}}
                                     (constantly nil))
                prior (#'recovery/checkpoint-file recovery-dir :main "prior.checkpoint")
                working (#'recovery/checkpoint-file recovery-dir :main "recovery.transit")
                ordinary (io/file (.getParentFile prior) "show.transit")]
            (try
              (set-master! runtime 0.3)
              (show/save-show! runtime relocated)
              (recovery/write-source! working (show/export-show-source runtime))
              (Files/createSymbolicLink (.toPath prior) (.toPath relocated) attributes)
              (Files/createSymbolicLink (.toPath alias) (.toPath working) attributes)
              (set-master! runtime 0.8)
              (doseq [path [prior alias]]
                (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reserved for show recovery"
                                      (show/save-show! runtime path))))
              (is (Files/isSymbolicLink (.toPath prior)))
              (is (Files/isSymbolicLink (.toPath alias)))
              (is (= [0.3 0.3] (mapv #(checkpoint-master runtime %) [:prior :recovery])))
              (is (= (.getAbsolutePath ordinary) (show/save-show! runtime ordinary)))
              (is (= 0.8 (checkpoint-master runtime :named)))
              (finally (show/stop! runtime))))
          (finally (Files/deleteIfExists (.toPath recovery-dir))))))))

(deftest unresolved-prior-survives-new-autosaves-and-another-restart
  (with-directory
    (fn [directory]
      (let [options {:workspace-options nil :recovery {:directory directory :interval-ms 25}}
            config {:init {:components [:show] :modules [:inspect :show-state]}
                    :show options :inspect true :show-state {}}
            named (io/file directory "named.transit")
            before-autosave (atom [])
            before-modified (atom [])
            registry (assoc app/registry :inspect
                            (fn [context _]
                              (let [runtime (get-in context [:modules :show])]
                                (swap! before-autosave conj (mapv #(checkpoint-master runtime %) [:prior :recovery]))
                                (swap! before-modified conj (get-in (show/recovery-status runtime) [:checkpoints :prior :modified-at-ms])))
                              {:value :ready}))]
        (let [first-show (show/start! options (constantly nil))]
          (try
            (set-master! first-show 0.5)
            (show/save-show! first-show named)
            (set-master! first-show 0.3)
            (is (await-result #(= 0.3 (checkpoint-master first-show :recovery))))
            (is (nil? (checkpoint-master first-show :prior)))
            (finally (show/stop! first-show))))
        (let [second-show (app/start! config registry)]
          (try
            (is (= [[0.3 0.3]] @before-autosave))
            (is (= 1.0 (:master (show/snapshot second-show))))
            (is (await-result #(= 1.0 (checkpoint-master second-show :recovery))))
            (is (= 0.3 (checkpoint-master second-show :prior)))
            (is (= (first @before-modified) (get-in (show/recovery-status second-show) [:checkpoints :prior :modified-at-ms])))
            (is (= 0.5 (checkpoint-master second-show :named)))
            (let [owner @(:recovery second-show) prior (recovery/latest-file owner :main :prior)]
              (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reserved for show recovery"
                                    (show/save-show! second-show prior))))
            (set-master! second-show 0.6)
            (is (await-result #(= 0.6 (checkpoint-master second-show :recovery))))
            (finally (app/stop! second-show))))
        (let [other (show/start! (assoc options :model (assoc (demo/model) :show-id :other)) (constantly nil))]
          (try
            (set-master! other 0.8)
            (is (await-result #(= 0.8 (checkpoint-master other :recovery))))
            (is (= #{:recovery} (:available (show/recovery-status other))))
            (is (false? (show/discard-prior-recovery! other)))
            (finally (show/stop! other))))
        (let [third-show (app/start! (assoc-in config [:show-state :restore] :prior) registry)]
          (try
            (is (= [[0.3 0.3] [0.3 0.6]] @before-autosave))
            (is (= 0.3 (:master (show/snapshot third-show))))
            (is (= 0.3 (checkpoint-master third-show :prior)))
            (is (true? (show/discard-prior-recovery! third-show)))
            (is (false? (show/discard-prior-recovery! third-show)))
            (is (nil? (checkpoint-master third-show :prior)))
            (is (= 0.5 (checkpoint-master third-show :named)))
            (is (await-result #(= 0.3 (checkpoint-master third-show :recovery))))
            (finally (app/stop! third-show))))))))

(deftest discard-before-the-first-write-survives-restart-and-allows-a-new-candidate
  (with-directory
    (fn [directory]
      (let [options {:workspace-options nil :recovery {:directory directory :interval-ms 25}}
            inspect (assoc-in options [:recovery :enabled?] false)]
        (let [runtime (show/start! options (constantly nil))]
          (try
            (set-master! runtime 0.3)
            (is (await-result #(= 0.3 (checkpoint-master runtime :recovery))))
            (finally (show/stop! runtime))))
        (let [runtime (show/start! inspect (constantly nil))]
          (try
            (is (= 0.3 (checkpoint-master runtime :prior)))
            (is (true? (show/discard-prior-recovery! runtime)))
            (is (nil? (checkpoint-master runtime :prior)))
            (is (= 0.3 (checkpoint-master runtime :recovery)))
            (finally (show/stop! runtime))))
        (let [runtime (show/start! (assoc options :defer-recovery? true) (constantly nil))]
          (try
            (is (nil? (checkpoint-master runtime :prior)))
            (is (= :accepted (:status (deref (show/load-latest-show! runtime :recovery) 5000 {}))))
            (is (= 0.3 (:master (show/snapshot runtime))))
            (set-master! runtime 0.7)
            (show/activate-recovery! runtime)
            (is (await-result #(= 0.7 (checkpoint-master runtime :recovery))))
            (is (nil? (checkpoint-master runtime :prior)))
            (finally (show/stop! runtime))))
        (let [runtime (show/start! inspect (constantly nil))]
          (try
            (is (= 0.7 (checkpoint-master runtime :prior)))
            (finally (show/stop! runtime))))))))

(deftest prior-restore-read-survives-the-first-working-file-replacement
  (with-directory
    (fn [directory]
      (let [options (recovery/options {:directory directory :interval-ms 20})
            commit! (fn [_ replace!] (replace!))
            before (recovery/start! options true (constantly {:show-id :test :source "previous"}) commit!)]
        (try
          (is (await-result #(:last-saved (recovery/status before))))
          (finally (recovery/close! before)))
        (let [after (recovery/start! options false (constantly {:show-id :test :source "working"}) commit!)
              reads (atom [])]
          (try
            (is (= "previous"
                   (recovery/read-prior after :test
                     (fn [file]
                       (swap! reads conj (.getName file))
                       (when (= 1 (count @reads))
                         (recovery/activate! after)
                         (is (await-result #(:last-saved (recovery/status after)))))
                       (slurp file)))))
            (is (= ["recovery.transit" "prior.checkpoint"] @reads))
            (is (= "working" (slurp (recovery/latest-file after :test :recovery))))
            (finally (recovery/close! after))))))))

(deftest restoring-a-named-checkpoint-retires-a-staged-recovery-write
  (with-directory
    (fn [directory]
      (let [named (io/file directory "manual.transit")
            staged (promise) resume (promise) publication (promise)
            write! recovery/write-source!]
        (with-redefs [recovery/write-source!
                      (fn [path source & [publish!]]
                        (if (and publish! (= "recovery.transit" (.getName (io/file path)))
                                 (= 0.6 (-> source persistence/read-string :model :master)))
                          (write! path source
                                  (fn [replace!]
                                    (deliver staged true)
                                    (deref resume 5000 nil)
                                    (let [result (publish! replace!)]
                                      (deliver publication result)
                                      result)))
                          (if publish! (write! path source publish!) (write! path source))))]
          (let [runtime (show/start! {:workspace-options nil :recovery {:directory directory :interval-ms 35}}
                                     (constantly nil))
                owner @(:recovery runtime)
                stored-master #(some-> (recovery/latest-file owner :main :recovery) slurp
                                       persistence/read-string :model :master)]
            (try
              (show/save-show! runtime named)
              (is (await-result #(= 1.0 (stored-master))))
              (is (= :accepted (:status @(show/submit! runtime {:op :master/set :value 0.6}))))
              (is (true? (deref staged 5000 false)))
              (is (= :accepted (:status @(show/load-latest-show! runtime :named))))
              (deliver resume true)
              (is (nil? (deref publication 5000 :pending)))
              (is (= 1.0 (stored-master)))
              (finally (deliver resume true) (show/stop! runtime)))))))))

(deftest studio-checkpoint-actions-are-bound-to-the-show-that-listed-them
  (with-directory
    (fn [directory]
      (let [options {:workspace-options nil :recovery {:directory directory :interval-ms 25}}
            first-show (show/start! options (constantly nil))]
        (try
          (set-master! first-show 0.5)
          (show/save-show! first-show (io/file directory "named.transit"))
          (set-master! first-show 0.3)
          (is (await-result #(= 0.3 (checkpoint-master first-show :recovery))))
          (finally (show/stop! first-show)))
        (let [runtime (show/start! (assoc-in options [:recovery :enabled?] false) (constantly nil))]
          (try
            (let [handler (studio/handler runtime)
                  headers {"accept" "application/transit+json" "content-type" "application/transit+json"}
                  status #(wire/decode (:body (handler {:request-method :get :uri "/api/show/checkpoints"
                                                        :headers headers})))
                  post (fn [route identity origin]
                         (let [response (handler {:request-method :post
                                                  :uri (str "/api/show/checkpoints/" route)
                                                  :headers (cond-> headers origin (assoc "origin" origin "host" "localhost"))
                                                  :body (java.io.StringReader. (wire/encode identity))})]
                           [(:status response) (wire/decode (:body response))]))
                  identity #(select-keys (status) [:runtime-id :show-id :epoch])
                  initial (identity)]
              (is (= #{:recovery :named :prior} (set (keys (:checkpoints (status))))))
              (is (every? #(= #{:modified-at-ms} (set (keys %))) (vals (:checkpoints (status)))))
              (is (= 403 (first (post "restore" (assoc initial :kind :named) "http://elsewhere.test"))))
              (is (= 400 (first (post "restore" (assoc initial :kind :named :runtime-id "old-runtime") nil))))
              (is (= :accepted (:status (second (post "restore" (assoc initial :kind :named) nil)))))
              (is (= 0.5 (:master (show/snapshot runtime))))
              (let [[http rejection] (post "restore" (assoc initial :kind :recovery) nil)]
                (is (= 400 http))
                (is (= :stale-checkpoint-session (get-in rejection [:details :code]))))
              (is (= 0.5 (:master (show/snapshot runtime))))
              (is (= :accepted (:status (second (post "restore" (assoc (identity) :kind :recovery) nil)))))
              (is (= 0.3 (:master (show/snapshot runtime))))
              (is (= [200 {:status :accepted}] (post "discard-prior" (identity) nil)))
              (is (= #{:recovery :named} (set (keys (:checkpoints (status)))))))
            (finally (show/stop! runtime))))))))
