(ns tolglow.fixed-host
  "Start a local Ring show in a signed, fixed Native Image isolate."
  (:gen-class :methods [^{:static true} [open [String String] int]
                        ^{:static true} [close [] void]])
  (:require [clojure.java.io :as io]
            [thi.ng.math.core :as math]
            [tolglow.application :as application]
            [tolglow.modules :as modules]
            [tolglow.native-resources]
            [tolglow.native.show-source :as source]
            [tolglow.studio]))

(defonce ^:private running (atom nil))

(defn -open [home folder]
  (locking running
    (when @running (throw (ex-info "This isolate already owns a show." {})))
    (alter-var-root #'math/*rnd* #(or % (java.util.Random.)))
    (let [{cfg :config registry :registry evaluator :evaluator-factory} (if (seq folder)
                                             (source/read-show folder application/registry)
                                             {:config (application/default-config) :registry application/registry
                                              :evaluator-factory (source/default-evaluator)})
          local-show (-> (modules/settings cfg :show)
                         (assoc :credential-path (str (io/file home "credentials.edn")))
                         (assoc-in [:repl-options :session-options :evaluator-factory] evaluator))
          local-web {:enabled true :host "127.0.0.1" :port 0}
          show (application/start! (assoc cfg :show local-show :web-server local-web) registry)
          port (get-in show [:application :modules :web-server :port])]
      (if (pos-int? port)
        (do (reset! running show) port)
        (do (application/stop! show)
            (throw (ex-info "Include :web-server in the show's modules." {})))))))

(defn -close []
  (locking running
    (when-let [show @running]
      (try (application/stop! show) (finally (reset! running nil))))))
