(ns tolglow.fixed-host
  "Start a local Ring show in a signed, fixed Native Image isolate."
  (:gen-class :methods [^{:static true} [open [String String] int]
                        ^{:static true} [close [] void]])
  (:require [clojure.java.io :as io]
            [slf4j-timbre.adapter]
            [slf4j-timbre.factory]
            [slf4j-timbre.service-provider]
            [thi.ng.math.core :as math]
            [tolglow.application :as application]
            [tolglow.desktop.launch :as launch]
            [tolglow.modules :as modules]
            [tolglow.native-resources]
            [tolglow.native.show-source :as source]
            [tolglow.show.recovery :as recovery]
            [tolglow.studio]))

(defonce ^:private running (atom nil))

(defonce ^:private recovery-claim (atom nil))

(defn- show-path [folder path]
  (if (and (seq folder) (string? path) (not (.isAbsolute (io/file path))))
    (str (io/file folder path))
    path))

(defn launch-config [home folder config evaluator]
  (let [show (modules/settings config :show)
        path (or (show-path folder (:credential-path show)) (str (io/file home "credentials.edn")))
        state (:show-state config)]
    (cond-> (assoc config
                   :show (-> show
                             (assoc :credential-path path)
                             (assoc-in [:repl-options :session-options :evaluator-factory] evaluator))
                   :web-server {:enabled true :host "127.0.0.1" :port 0})
      (:load-file state) (update-in [:show-state :load-file] (partial show-path folder))
      (:save-file state) (update-in [:show-state :save-file] (partial show-path folder)))))

(defn- read-show [folder]
  (if (seq folder)
    (source/read-show folder application/registry)
    {:config (application/default-config) :registry application/registry
     :evaluator-factory (source/default-evaluator)}))

(defn -open [home folder]
  (locking running
    (when @running (throw (ex-info "This isolate already owns a show." {})))
    (alter-var-root #'math/*rnd* #(or % (java.util.Random.)))
    (let [{:keys [config registry evaluator-factory]} (read-show folder)
          config (launch/with-application-sources config)
          ;; Only a Mac window can run beside other shows; the iOS on-device
          ;; host keeps the shared recovery directory and its devices.
          options (launch/desktop-options)
          claim (when (and (:window? options) (seq folder) (launch/scopes-recovery? config))
                  (launch/claim-folder-recovery! folder (recovery/default-directory)))
          _ (reset! recovery-claim claim)
          config (if (:window? options)
                   (launch/desktop-config config (merge options (launch/claim-options claim)))
                   config)
          release! #(do (some-> @recovery-claim :release! (apply [])) (reset! recovery-claim nil))
          show (try (application/start! (launch-config home folder config evaluator-factory) registry)
                    (catch Throwable error (release!) (throw error)))
          port (get-in show [:application :modules :web-server :port])]
      (if (pos-int? port)
        (do (reset! running show) port)
        (do (application/stop! show)
            (release!)
            (throw (ex-info "Include :web-server in the show's modules." {})))))))

(defn -close []
  (locking running
    (when-let [show @running]
      (try (application/stop! show)
           (finally (reset! running nil)
                    (some-> @recovery-claim :release! (apply []))
                    (reset! recovery-claim nil))))))
