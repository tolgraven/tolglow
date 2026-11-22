(ns tolglow.application
  "Configured Tolglow startup for a REPL, a headless show or Studio."
  (:gen-class)
  (:require [clojure.java.io :as io]
            [afterglow.control.devices.transports :as transports]
            [afterglow.control.devices.repl :as devices]
            [tolglow.data :as data]
            [tolglow.modules :as modules]
            [tolglow.show :as show]
            [tolglow.fixtures.patch :as patch]
            [tolglow.agents :as agents]
            [tolglow.connections :as connections]))

(defn- configured-source [path]
  (if (.isFile (io/file path))
    (data/read-edn (slurp (io/file path)))
    (let [id (symbol path)]
      (when-not (qualified-symbol? id)
        (throw (ex-info "Configuration must name an EDN file or qualified config var" {:source path})))
      (if-let [v (requiring-resolve id)] @v (throw (ex-info "Unknown config var" {:source path}))))))

(defn default-config []
  (data/read-edn (slurp (io/resource "tolglow.edn"))))

(defn- need-show [context]
  (or (get-in context [:modules :show])
      (throw (ex-info "Place :show before modules that operate on it" {}))))

(defn- start-show [{:keys [config]} opts]
  (let [runtime (show/start! (merge {:frame-rate (:hz config 40)} opts))]
    {:value runtime :stop! #(show/stop! runtime)}))

(defn- start-web [context options]
  (let [web ((requiring-resolve 'tolglow.studio/listen!) (need-show context) options)]
    {:value (dissoc web :stop!) :stop! (:stop! web)}))

(defn- accepted! [reply]
  (let [ack (deref reply 5000 {:status :pending})]
    (when-not (= :accepted (:status ack))
      (throw (ex-info "Configured show operation failed" {:ack ack})))
    ack))

(defn- show-state [context {:keys [load-file save-file actions]}]
  (let [runtime (need-show context)]
    (when load-file (accepted! (show/load-show! runtime load-file)))
    (doseq [action actions]
      (if (fn? action) (action) (accepted! (show/submit! runtime action))))
    {:value {:load-file load-file :save-file save-file}
     :before-stop! (when save-file #(show/save-show! runtime save-file))}))

(defn- start-audio [context options]
  (let [runtime (need-show context)]
    {:value (if (connections/restart-requested? (:connections runtime) :audio)
              (show/audio-action! runtime :status {}) (show/audio-action! runtime :start options))
     :stop! #(show/audio-action! runtime :stop {})}))

(defn- start-sensor-osc [context {:keys [address port-in host port]}]
  (let [runtime (need-show context)]
    {:value (show/sensor-action! runtime :osc-start {:host (or host address "127.0.0.1") :port (or port port-in 0)})
     :stop! #(show/sensor-action! runtime :osc-stop {})}))

(defn- start-osc [context {:keys [session-id port-out] :as options}]
  (let [runtime (need-show context)
        id (or session-id "osc")
        device (show/connect-device! runtime :osc (assoc options :session-id id))]
    {:value (cond-> {:device device :port (devices/osc-port device)}
              port-out (assoc :send! (partial devices/send-osc! device)))
     :stop! #(show/disconnect-device! runtime id)}))

(defn- sync-config [{:keys [device type] :as options}]
  (if (and device (= :midi type))
    (let [matches (filter #(and (:input? %) (= device (:name %))) (transports/midi-ports))]
      (when-not (= 1 (count matches))
        (throw (ex-info "Configured MIDI clock input must match one port" {:device device :matches (count matches)})))
      {:selected-source :midi-clock
       :midi {:enabled? true :input-index (:index (first matches)) :expected-input-name device}})
    options))

(defn- start-sync [context options]
  (let [runtime (need-show context)]
    {:value (if (connections/restart-requested? (:connections runtime) :sync)
              (show/sync-action! runtime :status {})
              (do (when (seq options) (show/sync-action! runtime :configure {:config (sync-config options)}))
                  (show/sync-action! runtime :start {})))
     :stop! #(show/sync-action! runtime :stop {})}))

(defn- install-fixtures [context {:keys [definitions patches factories fixture-types groups]}]
  (let [runtime (need-show context)]
    (doseq [definition definitions]
      (accepted! (show/submit! runtime :definition {:kind :fixtures :definition definition})))
    (when (some? patches)
      (let [model (:model @(:state runtime))
            fixtures (patch/expand (merge fixture-types (get-in model [:registry :fixtures])) factories patches groups)]
        (accepted! (show/submit! runtime :rig (assoc (:rig model) :fixtures fixtures)))))
    {:value (get-in @(:state runtime) [:model :rig :fixtures])}))

(defn- install-cue-pages [context {:keys [definitions]}]
  (let [runtime (need-show context)]
    (doseq [page definitions]
      (accepted! (show/submit! runtime {:op :page/put :page page})))
    {:value (get-in @(:state runtime) [:model :pages])}))

(defn- install-waveforms [context {:keys [definitions]}]
  (let [runtime (need-show context)]
    (doseq [definition definitions]
      (accepted! (show/submit! runtime :waveform {:source (pr-str definition)})))
    {:value (show/waveform-action! runtime :registry {})}))

(defn- settings-module [scope defaults]
  (fn [context options]
    {:value (accepted! (show/submit! (need-show context) :settings
                                     {:scope scope :values (merge defaults options)}))}))

(defn- install-lighting-scenes [context {:keys [groups]}]
  (let [runtime (need-show context)]
    (when-not (vector? groups)
      (throw (ex-info "Lighting scenes need a vector of group targets" {})))
    {:value (mapv #(accepted! (show/submit! runtime :lighting-scene (assoc % :op :apply))) groups)}))

(defn- configure-sensors [context {:keys [sources]}]
  (let [runtime (need-show context)]
    (doseq [[id settings] sources]
      (accepted! (show/submit! runtime :sensor-configure (assoc settings :device-id id))))
    {:value (connections/sensors (:connections runtime))}))

(defn- configure-agents [context options]
  (let [manager (show/agent-manager (need-show context))]
    (when (seq options) (agents/configure! manager options))
    {:value manager}))

(def registry
  {:show start-show :web-server start-web :show-state show-state :clock-sync start-sync
   :audio start-audio :osc start-osc :sensor-osc start-sensor-osc :sensors configure-sensors :waveforms install-waveforms
   :expressive (settings-module :expressive {}) :agents configure-agents
   :lighting-capacity (settings-module :lighting-capacity {:enabled? true})
   :hdr (settings-module :hdr {:enabled? true})
   :autopilot (settings-module :autopilot {:enabled? true})
   :lighting-budget (settings-module :lighting-budget {:enabled? true})
   :output-response (settings-module :output-response {:enabled? true})
   :lighting-activity (settings-module :lighting-activity {:enabled? true})
   :fog-control (settings-module :fog-control {:enabled? true})
   :lighting-scenes install-lighting-scenes
   :fixtures install-fixtures :cue-pages install-cue-pages})

(defn start!
  "Launch a show using :init stages and per-module :enabled options. Supply a
  second argument to extend the setup registry with application-specific modules."
  ([] (start! (if-let [source (not-empty (System/getenv "TOLGLOW_CONFIG"))]
                (configured-source source) (default-config))))
  ([config] (start! config registry))
  ([config registry]
   (when-not (some #{:show} (get-in config [:init :components]))
     (throw (ex-info "Application init needs a required :show component" {})))
   (let [app (modules/start! registry config)]
     (assoc (need-show app) :application app))))

(defn stop! [runtime]
  (if-let [close! (get-in runtime [:application :stop!])] (close!) (show/stop! runtime)))

(defn parse-options
  "Parse CLI options without opening output or starting the web server."
  [args]
  (when (odd? (count args))
    (throw (ex-info "Studio options require flag/value pairs" {})))
  (let [allowed #{"--config" "--headless" "--host" "--port" "--trust-event-callbacks" "--output" "--artnet-host" "--artnet-port" "--ola-host" "--ola-port"}
        options (reduce (fn [result [flag value]]
                          (when-not (allowed flag)
                            (throw (ex-info "Unknown Studio option" {:option flag})))
                          (when (contains? result flag)
                            (throw (ex-info "Duplicate Studio option" {:option flag})))
                          (assoc result flag value)) {} (partition 2 args))
        ola? (or (contains? options "--ola-host") (contains? options "--ola-port"))
        artnet? (or (contains? options "--artnet-host") (contains? options "--artnet-port"))
        output (get options "--output" (cond ola? "ola" artnet? "artnet" :else "preview"))
        port-number (fn [flag default minimum]
                      (let [value (try (Long/parseLong (get options flag default))
                                       (catch NumberFormatException _
                                         (throw (ex-info "Port must be an integer" {:option flag}))))]
                        (when-not (<= minimum value 65535)
                          (throw (ex-info "Port is out of range" {:option flag :value value})))
                        value))]
    (when-not (#{"true" "false"} (get options "--headless" "false"))
      (throw (ex-info "--headless requires true or false" {})))
    (when-not (#{"true" "false"} (get options "--trust-event-callbacks" "false"))
      (throw (ex-info "--trust-event-callbacks must be true or false" {})))
    (when-not (#{"preview" "ola" "artnet"} output)
      (throw (ex-info "--output must be preview, ola or artnet" {})))
    (when (or (and ola? (not= output "ola")) (and artnet? (not= output "artnet")))
      (throw (ex-info "Choose either OLA or direct Art-Net output" {})))
    (when (and (= output "artnet") (not (seq (get options "--artnet-host"))))
      (throw (ex-info "Direct Art-Net requires --artnet-host" {})))
    (cond-> {:host (get options "--host" "127.0.0.1") :port (port-number "--port" "8500" 0)}
      (contains? options "--config") (assoc :config (get options "--config"))
      (contains? options "--headless") (assoc :headless? (= "true" (get options "--headless")))
      (contains? options "--trust-event-callbacks")
      (assoc :event-callback-options {:trusted-source? (= "true" (get options "--trust-event-callbacks"))})
      (= output "ola") (assoc :ola {:host (get options "--ola-host" "127.0.0.1")
                                     :port (port-number "--ola-port" "9010" 1)})
      (= output "artnet") (assoc :artnet {:host (get options "--artnet-host")
                                           :port (port-number "--artnet-port" "6454" 1)}))))


(defn launch!
  "Apply CLI options to a configuration, preserving values unless the caller
  explicitly specified the corresponding flag."
  [args]
  (let [opts (parse-options args)
        flags (set (take-nth 2 args))
        cfg (if-let [source (or (:config opts) (not-empty (System/getenv "TOLGLOW_CONFIG")))] (configured-source source) (default-config))
        web (modules/settings cfg :web-server)
        cfg (cond-> (assoc cfg :web-server web)
              (contains? opts :headless?) (assoc-in [:web-server :enabled] (not (:headless? opts)))
              (flags "--port") (assoc-in [:web-server :port] (:port opts))
              (flags "--host") (assoc-in [:web-server :host] (:host opts)))
        output-options (select-keys opts [:ola :artnet :event-callback-options])
        explicit-output? (some flags ["--output" "--ola-host" "--artnet-host" "--ola-port" "--artnet-port"])]
    (start! (update cfg :show #(merge (cond-> (if (map? %) % {}) explicit-output? (dissoc :ola :artnet))
                                     output-options)))))

(defn -main [& args]
  (let [runtime (launch! args)
        app (:application runtime)]
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable #(stop! runtime)))
    (println "Started Tolglow" (:loaded-components app))
    (when-let [{:keys [host port scheme public-origin]} (get-in app [:modules :web-server])]
      (println "Tolglow Studio listening on" (or public-origin (str (name (or scheme :http)) "://" host ":" port))))
    @(promise)))
