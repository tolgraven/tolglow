(ns tolglow.qa.surface-link-browser
  "Run the optimized Studio against the preview-only demo show with two
  virtual X-Touch surfaces (injected transports, no MIDI ports) linked as one,
  and check the session box in headless Chrome both ways (#705, #706). Usage:
  lein with-profile +dev run -m tolglow.qa.surface-link-browser [directory] [port]"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

(defn- accepted! [ack what]
  (when-not (= :accepted (:status ack)) (throw (ex-info (str "Could not " what) ack)))
  ack)

(defn- node! [origin directory phase]
  (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/surface-link-qa.mjs" origin directory phase)]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info (str "Surface browser check failed (" phase ")") {:error err})))))

(defn -main [& [directory port]]
  (let [port (Long/parseLong (or port "4340"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (demo/model) :workspace-options nil}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/surface-link-browser")
        repl! #(accepted! @(show/submit! host :command (merge {:source {:adapter :repl :session-id "qa"}} %)) (pr-str %))
        virtual (fn [] {:open! (fn [_] :virtual) :send! (fn [_ _]) :close! (fn [_])})]
    (try
      (.mkdirs (io/file directory))
      (repl! {:op :effect-groups/configure
              :config {:manual (mapv #(hash-map :id (keyword (format "g%02d" %)) :name (format "Group %02d" %))
                                     (range 1 13))}})
      (repl! {:op :cue/launch :cue-id :ember :launch {:mode :persistent}})
      (show/connect-device! host :x-touch {:session-id "xt-main" :transport (virtual) :effect-group-view :manual
                                           :channels [{:cue-id :ember} {:cue-id :ice} {:cue-id :sage} {:cue-id :moon}
                                                      {:cue-id :amber} {:cue-id :indigo} {:cue-id :orchid} {:cue-id :merlot}]})
      (show/connect-device! host :x-touch {:session-id "xt-ext" :transport (virtual) :extender? true})
      (Thread/sleep 300)
      (accepted! @(show/link-surfaces! host "desk" ["xt-main" "xt-ext"]) "link the surfaces")
      (node! origin directory "browser")
      (let [layout (-> (show/snapshot host) :control-surfaces :logical first :layout)]
        (println "Studio set the encoder mode:" layout)
        (when-not (= :plug-in (:encoder-mode layout)) (throw (ex-info "Studio's encoder mode did not commit" layout))))
      ;; REPL -> Studio: page the window back, choose USER with a selected strip,
      ;; and check the open page follows.
      (accepted! @(show/bank-surface! host "desk" :first) "bank from the REPL")
      (accepted! @(show/surface-layout! host "desk" {:encoder-mode :user :selected [:cue :ember]}) "set the layout from the REPL")
      (println "REPL banked desk to" (-> (show/snapshot host) :control-surfaces :logical first :window))
      (node! origin directory "repl")
      (finally (app/stop! host) (shutdown-agents)))))
