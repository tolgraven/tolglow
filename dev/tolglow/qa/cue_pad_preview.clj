(ns tolglow.qa.cue-pad-preview
  "Preview-only release fixture for the browser check. No hardware ports open."
  (:require [tolglow.application :as app]
            [tolglow.demo :as demo]))

(defn- sweep [index]
  {:op :add :args [(demo/constant 0.55)
                   {:op :multiply
                    :args [(demo/constant 0.44)
                           {:op :sin :args [{:op :add :args [(demo/constant (* index 0.8))
                                             {:op :multiply :args [{:op :beat} (demo/constant Math/PI)]}]}]}]}]})

(defn model []
  (reduce (fn [model [i [id]]]
            (assoc-in model [:registry :effects id :outputs :dimmer] (sweep i)))
          (assoc-in (demo/model) [:pages :accents]
                    {:id :accents :name "Accents" :cells {[0 0] :breathe [1 0] :flash}})
          (map-indexed vector demo/colors)))

(defn -main [& [port-file stop-file]]
  (let [runtime (app/start! {:init {:components [:show] :modules [:web-server]}
                             :show {:model (model) :workspace-options nil}
                             :web-server {:host "127.0.0.1" :port 0}})]
    (try
      (spit port-file (get-in runtime [:application :modules :web-server :port]))
      (while (not (.exists (java.io.File. stop-file))) (Thread/sleep 200))
      (finally (app/stop! runtime) (shutdown-agents)))))
