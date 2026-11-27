(ns tolglow.qa.effect-selection-browser
  "Single-press exclusive Solo from the REPL and from Studio (#749), checked in
  both directions against one running show:

    $W/bin/repl run tolglow.qa.effect-selection-browser 18890 target/effect-selection-qa

  The show starts three demo cues with an additive Solo default. The REPL
  solos two of them, then sends one :mode :exclusive toggle; the browser
  script (dev/studio/effect-selection-browser-qa.mjs) must see only that
  card soloed. It then uses Exclusive once, Alt-click and a plain click in
  the production Studio build, and this namespace reads the committed
  selection back from show/snapshot. Preview only: no output is armed."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

(defn- submit! [host payload]
  (let [ack (deref (show/submit! host :command (merge {:source {:adapter :repl :session-id "qa"}} payload)) 5000 ::timeout)]
    (when-not (= :accepted (:status ack)) (throw (ex-info "QA command failed" {:payload payload :ack ack})))
    ack))

(defn- soloed [host]
  (into [] (keep #(when (:soloed? %) (:id %))) (:instances (:effect-selection (show/snapshot host)))))

(defn -main [& [port directory]]
  (let [port (Long/parseLong (or port "18890"))
        directory (or directory "target/effect-selection-qa")
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (demo/model) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))]
    (try
      (.mkdirs (io/file directory))
      (let [[a b c] (mapv #(:instance-id (submit! host {:op :cue/launch :cue-id % :launch {:mode :persistent}}))
                          [:ember :breathe :sweep])
            toggle (fn [id & [mode]] (cond-> {:op :effect-selection/toggle :control :solo :selector {:instance-ids [id]}}
                                       mode (assoc :mode mode)))]
        (submit! host {:op :effect-selection/configure :modes {:solo :additive}})
        (submit! host (toggle a))
        (submit! host (toggle c))
        (let [ack (submit! host (toggle b :exclusive))]
          (when-not (= [b] (:selected-instance-ids ack) (soloed host))
            (throw (ex-info "The REPL's exclusive Solo did not replace the selection" {:ack ack :soloed (soloed host)}))))
        (println "REPL: :mode :exclusive on" b "left Solo on" (soloed host))
        (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/effect-selection-browser-qa.mjs" origin directory a b c)]
          (print out) (flush)
          (when-not (zero? exit)
            (binding [*out* *err*] (print err) (flush))
            (throw (ex-info "Effect selection browser check failed" {:error err}))))
        ;; The browser's last step was a plain click on c with the additive default after Alt-click on a.
        (let [final (soloed host)]
          (when-not (= (set [a c]) (set final))
            (throw (ex-info "The REPL does not see Studio's last selection" {:soloed final})))
          (when-not (= :additive (get-in (show/snapshot host) [:effect-selection :modes :solo]))
            (throw (ex-info "A one-action mode changed the Solo default" {})))
          (println "REPL: show/snapshot after the browser shows Solo on" final "with the default still :additive")))
      (finally (app/stop! host) (shutdown-agents)))))
