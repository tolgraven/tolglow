(ns tolglow.qa.agent-goals-browser
  "Run the optimized Studio against the preview-only demo show and exercise
  the Goals panel (#857) in headless Chrome, both ways with the REPL on one
  open page: the REPL puts a goal the panel shows; the panel adds and moves a
  goal the REPL reads; a REPL edit reaches the open panel; a stale Studio edit
  and an over-long text are refused with their reasons. Usage:
  lein with-profile +dev run -m tolglow.qa.agent-goals-browser [directory] [port]"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.agent-goals :as goals]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

(defn- accepted! [ack what]
  (when-not (= :accepted (:status ack)) (throw (ex-info (str "Could not " what) {:ack ack})))
  ack)

(defn- wait-for [what predicate]
  (loop [n 0]
    (or (predicate)
        (if (< n 1200)
          (do (Thread/sleep 50) (recur (inc n)))
          (throw (ex-info (str "Timed out waiting for " what) {}))))))

(defn -main [& [directory port]]
  (let [port (Long/parseLong (or port "20120"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (demo/model) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/agent-goals-browser")
        repl! #(accepted! @(show/submit! host :agent-goal %) (pr-str %))
        active #(goals/active (:model @(:state host)))]
    (try
      (.mkdirs (io/file directory))
      (repl! {:op :put :goal {:scope :night :text "Warm and patient until the room fills. Save strobe and white for the last hour."
                              :palette [{:at {:phrase 0} :colors [[0.9 0.25 0.05]]}
                                        {:at {:phrase 8} :colors [[0.85 0.1 0.2]]}
                                        {:at {:phrase 24} :colors [[0.35 0.02 0.3]]}]
                              :energy [{:at {:phrase 0} :level 0.2} {:at {:phrase 8} :level 0.35}
                                       {:at {:phrase 16} :level 0.6} {:at {:phrase 24} :level 0.9}]}})
      (repl! {:op :put :goal {:scope :set :text "Ember reds drifting to violet by the end of this set."
                              :from {:phrase 0} :until {:phrase 12}
                              :palette [{:at {:phrase 0} :colors [[0.9 0.1 0.02]]} {:at {:phrase 12} :colors [[0.35 0.02 0.3]]}]}})
      (let [browser (future (shell/sh "node" "dev/studio/agent-goals-browser-qa.mjs" origin directory))
            studio-goal (wait-for "the goal Studio adds"
                                  #(some (fn [g] (when (= "studio" (:by g)) g)) (active)))]
        (println "REPL read Studio's goal:" (pr-str (select-keys studio-goal [:id :scope :order :by :from :until :text])))
        (wait-for "Studio's move" #(= 1 (:order (some (fn [g] (when (= (:id studio-goal) (:id g)) g)) (active)))))
        (println "REPL read Studio's move: order" (mapv :id (active)))
        (repl! {:op :edit :id (:id studio-goal) :changes {:text "Lift the second chorus; hold the first one back. (edited from the REPL)"}})
        (let [{:keys [exit out err]} (deref browser 120000 {:exit -1 :err "browser check timed out"})]
          (print out) (flush)
          (when-not (zero? exit)
            (binding [*out* *err*] (print err) (flush))
            (throw (ex-info "Goals browser check failed" {:error err})))))
      (println "Final order:" (mapv (juxt :order :id :scope :by :revision) (active)))
      (finally (app/stop! host)))
    (shutdown-agents)))
