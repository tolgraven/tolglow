(ns tolglow.show.recovery-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.persistence :as persistence]
            [tolglow.show :as show]
            [tolglow.show.recovery :as recovery])
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
        (is (nil? (recovery/write-source! file "retired-generation" (constantly false))))
        (is (= "last-good" (slurp file)))
        (is (= ["show.transit"] (mapv #(.getName %) (.listFiles directory))))))))

(deftest working-recovery-and-the-bookmarked-manual-save-load-independently
  (with-directory
    (fn [directory]
      (let [entered (promise) finish (promise) first-write? (atom true)
            manual (io/file directory "named.transit")
            write! recovery/write-source!]
        (with-redefs [recovery/write-source!
                      (fn [path source & [eligible?]]
                        (when (and (= "recovery.transit" (.getName (io/file path)))
                                   (compare-and-set! first-write? true false))
                          (deliver entered true)
                          (deref finish 5000 nil))
                        (write! path source (or eligible? (constantly true))))]
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
            (is (= #{:recovery :named} (:available (show/recovery-status reopened))))
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
