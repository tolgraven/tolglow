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

(defn- with-overdrive
  "A Faults page whose Overdrive cue drives its dimmer past 1 without HDR, so
  its pad preview fails beside a healthy Ember."
  [model]
  (-> model
      (assoc-in [:registry :effects :overdrive]
                (-> (get-in model [:registry :effects :ember])
                    (assoc :id :overdrive :name "Overdrive")
                    (assoc-in [:outputs :dimmer] (demo/constant 1.4))))
      (assoc-in [:registry :cues :overdrive]
                (assoc (get-in model [:registry :cues :ember]) :id :overdrive :name "Overdrive" :effect-id :overdrive))
      (assoc-in [:pages :faults] {:id :faults :name "Faults" :cells {[0 0] :overdrive [1 0] :ember}})))

(defn model []
  (with-overdrive
    (reduce (fn [model [i [id]]]
              (assoc-in model [:registry :effects id :outputs :dimmer] (sweep i)))
            (assoc-in (demo/model) [:pages :accents]
                      {:id :accents :name "Accents" :cells {[0 0] :breathe [1 0] :flash}})
            (map-indexed vector demo/colors))))

(defn -main
  "Write the bound port to port-file and serve until stop-file exists. An
  optional port binds that port on 127.0.0.1 instead of an ephemeral one."
  [& [port-file stop-file port]]
  (let [runtime (app/start! {:init {:components [:show] :modules [:web-server]}
                             :show {:model (model) :workspace-options nil :recovery false}
                             :web-server {:host "127.0.0.1" :port (or (some-> port parse-long) 0)}})]
    (try
      (spit port-file (get-in runtime [:application :modules :web-server :port]))
      (while (not (.exists (java.io.File. stop-file))) (Thread/sleep 200))
      (finally (app/stop! runtime) (shutdown-agents)))))
