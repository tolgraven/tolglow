(ns tolglow.studio.connections-host-test
  (:require [afterglow.control.devices.repl :as devices]
            [afterglow.reactive :as reactive]
            [tolglow.persistence :as files]
            [clojure.test :refer :all]
            [tolglow.show :as show]
            [tolglow.agents :as agents]
            [tolglow.audio.capture :as audio]
            [tolglow.connections :as connections])
  (:import [java.util.concurrent CountDownLatch TimeUnit]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defmacro with-runtime [[name options] & body]
  `(let [~name (show/create-runtime ~options)]
     (try ~@body (finally (show/stop! ~name)))))

(defn apply-at! [runtime payload time-ms]
  (let [reply (show/submit! runtime payload)] (show/tick! runtime time-ms) (deref reply 2000 ::timeout)))

(defn launch-on-beat [_ _ _] {:op :cue/launch :cue-id :ember})
(def launch-descriptor
  {:id ::launch :kind :fx :implementation {:type :var :symbol "tolglow.studio.connections-host-test/launch-on-beat"}})
(defn broken-sub [_ _] (throw (ex-info "subscription failed" {})))

(deftest restore-publishes-saved-state-output-and-clock-without-an-extra-effect-step
  (with-runtime [r {}]
    (let [ack (apply-at! r {:op :cue/launch :cue-id :ember} 0)]
      (is (= :accepted (:status ack)))
      (show/tick! r 150)
      (let [saved (show/export-show r) before (show/snapshot r)
            source (pr-str saved) old-graph (show/reactions r)]
        (show/tick! r 900)
        (let [reply (show/submit! r :import {:source source})]
          (show/tick! r 1200)
          (is (= :accepted (:status @reply))))
        (let [restored (show/snapshot r)]
          (is (= 150 (:time-ms restored)))
          (is (= (:frame-number before) (:frame-number restored)))
          (is (= (:output before) (:output restored)))
          (is (= (mapv :state (vals (get-in saved [:model :instances])))
                 (mapv :state (vals (get-in @(:state r) [:model :instances])))))
          (is (= 1 (:epoch restored)))
          (is (= 150 (:time-ms @(:clock r))))
          (is (not (identical? old-graph (show/reactions r))))
          (is (thrown? clojure.lang.ExceptionInfo (reactive/publish! old-graph))))
        (show/tick! r 160)
        (is (= 160 (:time-ms (show/snapshot r))))
        (is (= (inc (:frame-number before)) (:frame-number (show/snapshot r))))))))

(deftest reactive-actions-are-applied-on-the-next-frame-and-survive-save-restore-once
  (with-runtime [r {}]
    (let [graph (show/reactions r)]
      (reactive/reg-fx! graph ::launch launch-on-beat launch-descriptor)
      (reactive/watch! graph ::beat [:clock/beat] ::launch)
      (reactive/ratom graph ::level 7)
      (show/tick! r 0)
      (is (empty? (:instances (show/snapshot r))))
      (let [source (pr-str (show/export-show r))
            reply (show/submit! r :import {:source source})]
        (show/tick! r 0)
        (is (= :accepted (:status @reply)))
        (is (empty? (:instances (show/snapshot r))) "old epoch action cannot cross the restore")
        (is (= 7 @(reactive/source (show/reactions r) ::level)))
        (show/tick! r 1)
        (is (= 1 (count (:instances (show/snapshot r)))))
        (let [instance (first (:instances (show/snapshot r)))]
          (is (= :reactive (get-in instance [:source :adapter])))
          (is (= 1 (get-in @(:state r) [:model :epoch]))))
        (show/tick! r 2)
        (is (= 1 (count (:instances (show/snapshot r)))) "no duplicate action at the same beat")))))

(deftest reactive-errors-stay-local-to-diagnostics-after-the-frame-commits
  (with-runtime [r {}]
    (let [signal (reactive/reg-sub! (show/reactions r) ::broken broken-sub)]
      ;; Registering a failing query is allowed; evaluating it reports a local
      ;; graph error. A successful earlier cached query becomes dirty on change.
      (is (= ::broken signal))
      (is (thrown? clojure.lang.ExceptionInfo (reactive/subscribe (show/reactions r) [::broken])))
      (reactive/reg-sub! (show/reactions r) ::broken (fn [{:keys [db]} _] (:master @db)))
      (is (= 1.0 @(reactive/subscribe (show/reactions r) [::broken])))
      (reactive/reg-sub! (show/reactions r) ::broken broken-sub)
      (let [reply (show/submit! r {:op :master/set :value 0.6})]
        (show/tick! r 10)
        (is (= :accepted (:status @reply)))
        (is (= 0.6 (:master (show/snapshot r))))
        (is (get-in (show/snapshot r) [:status :reactive-error]))
        (is (pos? (:frame-number (show/snapshot r))))))))

(deftest stopped-and-stale-imports-dispose-their-prepared-managers
  (doseq [stop? [true false]]
    (with-runtime [r {}]
      (let [source (pr-str (show/export-show r))
            restart (when-not stop? (show/submit! r {:op :show/restart}))
            reply (show/submit! r :import {:source source})
            candidate (->> (.toArray (:queue r)) (keep #(get-in % [:payload :candidate])) first)
            prepared-audio (get-in candidate [:bundle :audio])
            prepared-graph (get-in candidate [:bundle :reactive])]
        (is candidate)
        (is (false? (:closed? (audio/status prepared-audio))))
        (if stop? (show/stop! r) (show/tick! r 0))
        (is (= :rejected (:status @reply)))
        (when restart (is (= :accepted (:status @restart))))
        (is (:closed? (audio/status prepared-audio)))
        (is (= :aborted @(:lifecycle candidate)))
        (is (thrown? clojure.lang.ExceptionInfo (reactive/publish! prepared-graph)))))))

(deftest stale-resource-generation-rejects-import-without-reverting-prior-commands
  (with-runtime [r {}]
    (let [source (pr-str (show/export-show r))
          change (show/submit! r {:op :master/set :value 0.4})
          reply (show/submit! r :import {:source source})
          candidate (->> (.toArray (:queue r)) (keep #(get-in % [:payload :candidate])) first)]
      (connections/restore! (:connections r) (:model @(:state r)))
      (show/tick! r 1)
      (is (= :accepted (:status @change)))
      (is (= :rejected (:status @reply)))
      (is (= 0.4 (:master (show/snapshot r))))
      (is (zero? (:epoch (show/snapshot r))))
      (is (= :aborted @(:lifecycle candidate))))))

(deftest retirement-callbacks-run-after-publication-outside-the-owner-lock
  (let [runtime (atom nil) closes (atom [])
        provider (with-meta (fn [& _] nil)
                   {:close! (fn [] (when-let [r @runtime]
                                     (swap! closes conj {:locked? (Thread/holdsLock (:lock r))
                                                         :epoch (:epoch (show/snapshot r))})))})]
    (with-runtime [r {:fixture-options {:provider provider}}]
      (reset! runtime r)
      (let [reply (show/submit! r :import {:source (pr-str (show/export-show r))})]
        (show/tick! r 0)
        (is (= :accepted (:status @reply)))
        (is (= [{:locked? false :epoch 1}] @closes))))))

(deftest audio-mailbox-is-bounded-coalesced-and-rejects-old-epochs
  (with-runtime [r {}]
    (let [callbacks (show/audio-callbacks r)
          first-features {:time-ms 1 :rms 0.25} latest-features {:time-ms 2 :rms 0.5}
          event {:type :kick :sequence 1 :time-ms 1}]
      ((:features! callbacks) first-features)
      ((:features! callbacks) latest-features)
      (is (true? ((:events! callbacks) [event])))
      (is (false? ((:events! callbacks) (vec (repeat 256 event)))))
      (show/tick! r 2)
      (is (= latest-features (get-in @(:state r) [:model :variables :audio :features])))
      (is (= [event] (get-in @(:state r) [:model :variables :audio :events])))
      (let [reply (show/submit! r :import {:source (pr-str (show/export-show r))})]
        (show/tick! r 3)
        (is (= :accepted (:status @reply)))
        (is (false? ((:events! callbacks) [(assoc event :sequence 2)])))
        (is (nil? ((:clock! callbacks))))
        ((:features! callbacks) {:rms 99})
        (show/tick! r 4)
        (is (= latest-features (get-in @(:state r) [:model :variables :audio :features])))))))

(deftest wanted-audio-resumes-only-after-restored-clock-is-published
  (let [owner (atom nil) opened (atom []) closed (atom 0) first-open (CountDownLatch. 1) second-open (CountDownLatch. 1)]
    (with-runtime [r {:audio-options
                      {:open-source! (fn [_]
                                       (let [gate (CountDownLatch. 1) runtime @owner]
                                         (swap! opened conj {:epoch (:epoch (show/snapshot runtime))
                                                             :origin (:time-ms @(:clock runtime))})
                                         (.countDown (if (= 1 (count @opened)) first-open second-open))
                                         {:read! (fn [& _] (.await gate 5 TimeUnit/SECONDS) -1)
                                          :close! #(do (swap! closed inc) (.countDown gate))}))}}]
      (reset! owner r)
      (show/tick! r 500)
      (connections/start-audio! (:connections r) {:default? true} (show/audio-callbacks r))
      (is (.await first-open 2 TimeUnit/SECONDS))
      (let [reply (show/submit! r :import {:source (pr-str (show/export-show r))})]
        (show/tick! r 5000)
        (is (= :accepted (:status @reply)))
        (is (.await second-open 2 TimeUnit/SECONDS))
        (is (= {:epoch 1 :origin 500} (second @opened)))
        (is (= 1 @closed))
        (is (= 500 (:time-ms (show/snapshot r))))))))

(deftest restore-disconnects-old-controller-session-and-ignores-late-input
  (let [receive (atom nil) closes (atom 0)]
    (with-runtime [r {}]
      (let [device (show/connect-device! r :sensor
                                          {:session-id "sensor-test" :bindings [{:event {:control :tap} :cue-id :ember}]
                                           :transport {:open! #(do (reset! receive %) :sensor)
                                                       :send! (fn [& _]) :close! (fn [_] (swap! closes inc))}})
            reply (show/submit! r :import {:source (pr-str (show/export-show r))})]
        (show/tick! r 0)
        (is (= :accepted (:status @reply)))
        (is (= 1 @closes))
        (is (= :closed (:status (devices/status device))))
        (is (empty? @(:devices r)))
        (@receive {:control :tap :pressed? true})
        (show/tick! r 1)
        (is (empty? (:instances (show/snapshot r))))))))

(deftest a-controller-opening-during-restore-is-closed-before-registration
  (let [entered (CountDownLatch. 1) release (CountDownLatch. 1) closed (atom 0)]
    (with-runtime [r {}]
      (let [opening (future
                      (try (show/connect-device!
                            r :sensor {:session-id "late-sensor" :bindings []
                                       :transport {:open! (fn [_] (.countDown entered)
                                                            (.await release 3 TimeUnit/SECONDS) :sensor)
                                                   :send! (fn [& _]) :close! (fn [_] (swap! closed inc))}})
                           :connected (catch Exception _ :rejected)))]
        (try
          (is (.await entered 2 TimeUnit/SECONDS))
          (let [receipt (show/submit! r :import {:source (pr-str (show/export-show r))})]
            (show/tick! r 0)
            (is (= :accepted (:status @receipt))))
          (.countDown release)
          (is (= :rejected (deref opening 3000 ::timeout)))
          (is (= 1 @closed))
          (is (empty? @(:devices r)))
          (finally (.countDown release)))))))

(deftest failed-worker-construction-cleans-up-already-created-managers
  (let [closed (atom 0)
        provider (with-meta (fn [& _] nil) {:close! #(swap! closed inc)})]
    (is (thrown? Exception (show/create-runtime {:workers 0 :fixture-options {:provider provider}})))
    (is (= 1 @closed))))

(deftest replacing-a-unicode-show-file-preserves-history-settings-and-frame-state
  (let [directory (Files/createTempDirectory "tolglow-host-show" (make-array FileAttribute 0))
        file (.resolve directory "灯光.transit")]
    (try
      (with-runtime [r {}]
        (agents/restore! (show/agent-manager r)
                         {:version 1 :config {:provider :self-hosted :model "local" :endpoint "http://127.0.0.1:1234/v1/chat/completions"}
                          :credential-ref nil :selected-conversation-id "duck"
                          :conversations [{:id "duck" :title "舞う 🦆" :status :completed :updated-at-ms 0
                                           :messages [{:role :user :content "赤い灯り、ゆっくり"}
                                                      {:role :assistant :content "紫も少し。"}]}]})
        (let [settings (show/submit! r :settings {:scope :ui :values {:title "灯光 🦆"}})]
          (show/tick! r 200)
          (is (= :accepted (:status @settings))))
        (let [before (show/snapshot r)]
          (spit (str file) "old contents" :encoding "UTF-8")
          (show/save-show! r (str file))
          (let [document (files/read-string (slurp (str file) :encoding "UTF-8"))]
            (is (= "灯光 🦆" (get-in document [:model :settings :ui :title])))
            (is (= "舞う 🦆" (get-in document [:model :integrations :agents :conversations 0 :title]))))
          (apply-at! r {:op :master/set :value 0.2} 500)
          (let [receipt (show/load-show! r (str file))]
            (show/tick! r 1000)
            (is (= :accepted (:status @receipt))))
          (is (= (:frame-number before) (:frame-number (show/snapshot r))))
          (is (= (:time-ms before) (:time-ms (show/snapshot r))))
          (is (= 1.0 (:master (show/snapshot r))))))
      (finally (Files/deleteIfExists file) (Files/delete directory)))))
