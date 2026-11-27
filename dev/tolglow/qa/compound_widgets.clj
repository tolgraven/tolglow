(ns tolglow.qa.compound-widgets
  "Drive the #655 widgets of the optimized Studio against a preview-only show:
  the XY pad, the XY pad with a Z fader, a vertical fader, the nested-ring knob
  and the Ember knob skin. Studio gestures must reach `/api/state`; REPL
  `param/set` commands must reach the open view; the real Push projection of
  the ring knob is rendered through the capture worker. No physical output.

  lein run -m tolglow.qa.compound-widgets [capture-directory]
  TOLGLOW_QA_PORT picks the port (default: any free port)."
  (:require [afterglow.control.devices.push-2 :as push]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.studio.display :as display]
            [tolglow.waveforms :as waveforms]))

(def widget-effect
  {:id :widget-study :name "Widget study" :version 1 :categories [:movement]
   :outputs {:dimmer {:op :param :key :level}}
   :params {:level {:type :number :name "Level" :default 0.4 :bounds [0 1] :ui/order 0}
            :offset {:type :vector :name "Offset" :size 2 :bounds [-1 1] :default [0.0 0.0] :ui/order 1}
            :aim {:type :vector :name "Aim" :size 3 :default [0.0 0.0 5.0] :ui/order 2
                  :component-labels ["Pan" "Tilt" "Depth"]
                  :component-schemas [{:bounds [-180 180] :unit :degrees} {:bounds [-90 90] :unit :degrees}
                                      {:bounds [0 20] :unit :meters}]}
            :tone {:type :vector :name "Tone" :size 4 :bounds [0 1] :default [0.2 0.4 0.6 0.8] :ui/order 3
                  :component-labels ["Low" "Mid" "High" "Air"]}
            :steps {:type :integer :name "Steps" :ui/widget :fader :bounds [0 8] :default 3 :ui/order 4}}})

(defn- send! [host op payload]
  (let [ack (deref (show/submit! host op payload) 10000 nil)]
    (when-not (= :accepted (:status ack)) (throw (ex-info "QA command failed" {:op op :ack ack})))
    ack))

(defn- browser! [& args]
  (let [{:keys [exit out err]} (apply shell/sh "node" "dev/studio/compound-widgets-qa.mjs" args)]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info "Compound widget browser check failed" {:phase (last args) :exit exit})))))

(defn- record-push!
  "One projected Push view untouched and one with the tone touched, as the
  control surface supplies the touch. The Push bank lists parameters by ID,
  so the tone is last and a touch may open its sub-knobs into the free
  encoder slots after it, as other compound parameters do."
  [host id file]
  (let [cache (waveforms/create-cache)
        selection {:selected-instance id :selected-cue :widget-study :ready? true :page-id :main :origin [0 0]}]
    (with-open [out (io/writer file)]
      (doseq [[index [label extra]] (map-indexed vector [["resting" {}]
                                                         ["touch-tone" {:touched-parameter {:instance-id id :param :tone :component 0}}]])]
        (let [snapshot @(:state host)
              view (merge (push/display-view selection (:model snapshot)) extra)]
          (.write out (display/frame-request (str index ":" label) (waveforms/project-display cache snapshot view nil)))
          (.write out "\n"))))))

(defn -main [& [directory]]
  (let [dir (io/file (or directory "target/compound-widgets-qa"))
        port (parse-long (or (System/getenv "TOLGLOW_QA_PORT") "0"))
        model (-> (demo/model)
                  (assoc-in [:registry :effects :widget-study] widget-effect)
                  (assoc-in [:registry :cues :widget-study] {:id :widget-study :name "Widget study" :effect-id :widget-study
                                                            :version 1 :swatch "#c45a3c"})
                  (assoc-in [:pages :main :cells [0 2]] :widget-study))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model model :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})]
    (try
      (.mkdirs dir)
      (let [id (:instance-id (send! host :command {:op :cue/launch :cue-id :widget-study :launch {:mode :persistent}}))
            origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))
            wire (str id)]
        (doseq [f ["studio-open" "repl-done"]] (io/delete-file (io/file dir f) true))
        (let [studio (future (browser! origin wire (str dir) "studio"))]
          ;; Once the gestures are checked the page stays open; the REPL forms
          ;; a performer would type must reach it.
          (loop [waited 0]
            (cond (.exists (io/file dir "studio-open")) nil
                  (realized? studio) @studio
                  (> waited 240000) (throw (ex-info "Studio phase never opened" {}))
                  :else (do (Thread/sleep 100) (recur (+ waited 100)))))
          (doseq [[param value] [[:offset [-0.5 0.5]] [:aim [45.0 30.0 10.0]] [:tone [0.9 0.1 0.9 0.1]]]]
            (send! host :command {:op :param/set :instance-id id :param param :value value}))
          (spit (io/file dir "repl-done") "done\n")
          @studio)
        (record-push! host id (io/file dir "push-views.ndjson"))
        (browser! origin wire (str dir) "push"))
      (finally (app/stop! host) (shutdown-agents)))))
