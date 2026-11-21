(ns tolglow.studio
  "Ring bindings for an existing Tolglow show. The web server and browser
  projections are acquired only when this adapter is attached."
  (:require [clojure.data.json :as json]
            [ring.adapter.jetty :as jetty]
            [tolglow.show :as show]
            [tolglow.connections :as connections]
            [tolglow.agents :as agents]
            [tolglow.rig.calibration :as calibration]
            [tolglow.studio.debug-stream :as debug-stream]
            [tolglow.development :as development]
            [tolglow.expressive :as expressive]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.studio.projection :as projection]
            [tolglow.sensors.osc :as sensor-osc]
            [tolglow.studio.state-stream :as state-stream]
            [tolglow.studio.web :as web]))

(defn- bind! [runtime id]
  (show/attach-observer! runtime id
    (fn []
      (let [debug (debug-stream/create (:id runtime) #(deref (:state runtime)))
            stream (state-stream/create #(show/project-snapshot runtime %) #(show/heartbeat! runtime %)
                     {:frame-rate (:frame-rate runtime)
                      :observe-status! #(connections/observe-status! (:connections runtime) %)
                      :capture-status #(connections/status-at-commit (:connections runtime))})
            projections (projection/create)]
        (state-stream/publish! stream @(:state runtime))
        {:debug-stream debug :state-stream stream :browser-projections projections
         :frame! #(state-stream/publish! stream %)
         :tracing? #(debug-stream/observing? debug)
         :events! #(debug-stream/commit! debug %1 %2 %3 %4)
         :close! (fn [] (debug-stream/close! debug) (state-stream/close! stream)
                   (projection/clear! projections))}))))

(defn browser-snapshot
  "Bound preview values. Delta clients receive versioned, exact-base envelopes;
  inspection requests can retrieve a complete or authoring-conditional view."
  ([runtime query] (browser-snapshot runtime query #(bind! runtime :studio)))
  ([runtime query bindings!]
  (let [view (show/snapshot runtime)
        preview-ids (mapv :id (take 16 (get-in view [:rig :fixtures])))
        view (-> view (update-in [:output :values] select-keys preview-ids)
                 (assoc-in [:output :preview-only?] true))
        partial? (and (= (:runtime-id view) (get query "runtime-id"))
                      (= (str (:epoch view)) (get query "epoch"))
                      (= (str (:revision view)) (get query "revision")))]
    (cond
      (= "delta-v1" (get query "projection"))
      (projection/project! (:browser-projections (bindings!)) view (get query "base"))
      partial?
      (assoc (select-keys view [:runtime-id :show-id :epoch :revision :time-ms :frame-number :transport
                                :instances :groups :output :status :auto :compositions :modulation :patch]) :partial? true)
      :else view))))

(defn handler
  "Create the portable Ring application for this explicitly owned runtime."
  ([runtime] (handler runtime (bind! runtime :studio)))
  ([runtime bindings]
   (web/app {:snapshot! #(browser-snapshot runtime % (constantly bindings))
            :event-subscribers! #(subscribers/catalog (:model @(:state runtime)))
            :debug-stream! #(debug-stream/response (:debug-stream bindings) %)
            :state-stream! #(state-stream/response (:state-stream bindings) %)
            :definition! (fn [kind id]
                           (let [definitions (get-in @(:state runtime) [:model :registry kind])]
                             (or (get definitions id)
                                 (when (keyword? id) (get definitions (subs (str id) 1))))))
            :export! #(show/export-show-source runtime)
            :calibration! #(let [model (:model @(:state runtime))]
                             {:runtime-id (:id runtime) :epoch (:epoch model) :show-id (:show-id model)
                              :physical-now-ms ((:now-ms (:publisher runtime)))
                              :output-capable? (boolean (and (:independent-output? runtime) (get-in runtime [:sink :send!])))
                              :fixtures (mapv (fn [f] (select-keys f [:id :name :calibration :position :rotation-deg])) (get-in model [:rig :fixtures]))
                              :session (when-let [scan (calibration/current model)]
                                         (-> scan (dissoc :observations :receipts :base-rig :original-rig)
                                             (assoc :observation-count (count (:observations scan))
                                                    :receipt-count (count (:receipts scan)))))})
            :submit! #(show/submit! runtime %1 %2)
            :heartbeat! #(show/heartbeat! runtime %)
            :provider-get! #(agents/public-config (show/agent-manager runtime))
            :provider-put! #(agents/configure! (show/agent-manager runtime) %)
            :chat-history! #(agents/history (show/agent-manager runtime))
            :chat-select! #(agents/select-conversation! (show/agent-manager runtime) %)
            :fixture! #(show/fixture-action! runtime %1 %2)
            :rdm! #(show/rdm-action! runtime %1 %2)
            :sync! #(show/sync-action! runtime %1 %2)
            :waveform! #(show/waveform-action! runtime %1 %2)
            :expressive! (fn [payload]
                           (let [wire-id #(if (keyword? %) (subs (str %) 1) %)
                                 options (some (fn [[id device]]
                                                 (let [options (:options device)]
                                                   (when (= (wire-id (:device-id payload))
                                                            (wire-id (or (:mapping-id options) id))) options)))
                                               @(:devices runtime))
                                 profile (when options
                                           {:id (get-in options [:profile :id])
                                            :axes (keys (get-in options [:profile :capabilities :available-axes]))
                                            :defaults (select-keys options [:mapping-policy :param-mappings :cue-mappings])})]
                             (expressive/preview (:model @(:state runtime)) payload profile)))
            :patch! #(:patch (show/snapshot runtime))
            :repl! #(show/repl-action! runtime %1 %2)
            :repl-eval! #(show/repl-eval! runtime %1 %2 %3)
            :workspace! #(show/application-operation! runtime development/operation! %1 %2)
            :terminal! #(show/application-operation! runtime development/terminal-operation! %1 %2)
            :app-agent! #(show/application-operation! runtime development/app-agent-operation! %1 %2)
            :audio! #(show/audio-action! runtime %1 %2)
            :sensor! (fn [op payload]
                       (show/sensor-action! runtime op
                         (if (= op :sample) (sensor-osc/decode-json (json/write-str (web/json-data (dissoc payload :runtime-id :expected-epoch)))) payload)))
            :chat! (fn [payload emit! done!]
                     (let [identity (select-keys (:model @(:state runtime)) [:epoch :show-id])]
                       (agents/start-chat! (show/agent-manager runtime) payload
                                           {:snapshot! #(show/snapshot runtime)
                                            :dispatch! #(show/dispatch-tool! runtime %1 %2
                                                                        (merge %3 identity {:expected-epoch (:epoch identity)}))
                                            :emit! emit! :done! done!})))})))

(defn listen!
  "Bind Studio to the selected show, returning the actual port (including port
  zero) and an idempotent shutdown function."
  [runtime {:keys [host port] :or {host "127.0.0.1" port 8500}}]
  (let [id (Object.)
        bindings (bind! runtime id)]
    (try
      (let [server (jetty/run-jetty (handler runtime bindings) {:host host :port port :join? false})]
        ;; Streaming clients can remain connected for the lifetime of the show.
        (.setStopTimeout server 0)
        {:host host :port (.getLocalPort (first (.getConnectors server)))
         :stop! #(try (show/detach-observer! runtime id) (finally (.stop server)))})
      (catch Throwable error
        (show/detach-observer! runtime id)
        (throw error)))))
