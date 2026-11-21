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
