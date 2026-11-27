(ns tolglow.qa.gradient-rows
  "Render a running gradient cue's parameter row in Studio's release build (#689).
  A preview-only show launches a cue that samples a gradient, sets a value
  over the schema's capacity and checks the row in headless Chromium."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.effects :as fx]
            [tolglow.show :as show]))

(def ramp-effect
  (assoc (fx/effect :gradient-ramp
                    {:params {:palette (fx/parameter [{:position 0.0 :color [1.0 0.1 0.0]}
                                                      {:position 0.5 :color [0.1 1.0 0.2]}
                                                      {:position 1.0 :color [0.1 0.2 1.0]}])}}
                    (fx/color (fx/gradient :palette (fx/position :x) {:extend :mirror})))
         :name "Gradient ramp"))

(def ten-stops
  (vec (for [i (range 10)] {:position (/ i 9.0) :color [(/ i 9.0) 0.2 (- 1.0 (/ i 9.0))]})))

(defn- send! [host op payload]
  (let [ack (deref (show/submit! host op payload) 10000 nil)]
    (when-not (= :accepted (:status ack)) (throw (ex-info "QA command failed" {:op op :ack ack})))
    ack))

(defn -main [& [directory]]
  (let [dir (io/file (or directory "target/gradient-rows-qa"))
        port (parse-long (or (System/getenv "TOLGLOW_QA_PORT") "0"))
        model (-> (demo/model)
                  (assoc-in [:registry :effects :gradient-ramp] ramp-effect)
                  (assoc-in [:registry :cues :gradient-ramp] {:id :gradient-ramp :name "Gradient ramp"
                                                             :effect-id :gradient-ramp :version 1 :swatch "#40a0f0"})
                  (assoc-in [:pages :main :cells [0 2]] :gradient-ramp))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model model :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})]
    (try
      (.mkdirs dir)
      (let [id (:instance-id (send! host :command {:op :cue/launch :cue-id :gradient-ramp :launch {:mode :persistent}}))
            origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))]
        (send! host :command {:op :param/set :instance-id id :param :palette :value ten-stops})
        (Thread/sleep 300)
        (println "Reductions:" (pr-str (some #(when (= id (:id %)) (:gradient-reductions %)) (:instances (show/snapshot host)))))
        (let [{:keys [out err exit]} (shell/sh "node" "dev/studio/gradient-rows-qa.mjs" origin id (str dir))]
          (print out) (flush)
          (when-not (zero? exit)
            (binding [*out* *err*] (print err) (flush))
            (throw (ex-info "Gradient row browser check failed" {:exit exit})))))
      (finally (app/stop! host) (shutdown-agents)))))
