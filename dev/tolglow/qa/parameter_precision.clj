(ns tolglow.qa.parameter-precision
  "Record fine encoder edits in a preview-only show, its committed DMX bytes and both control surfaces."
  (:require [afterglow.control.devices.push-2 :as push]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.studio.display :as display]
            [tolglow.studio.parameter-precision :as precision]
            [tolglow.waveforms :as waveforms]))

(defn- control [name value]
  {:name name :type :number :bounds [0 1] :default value})

(defn- model []
  (let [definition {:id :precision :name "Moving head" :version 1
                    :targets [:head-1]
                    :params {:level (control "Intensity" (/ 127.0 255))
                             :pan (control "Pan" (/ 32767.0 65535))}
                    :outputs {:color (demo/constant [1 1 1]) :dimmer (demo/param :level) :pan (demo/param :pan)}}]
    (-> (demo/model)
        (assoc-in [:registry :effects :precision] definition)
        (assoc-in [:registry :cues :precision]
                  {:id :precision :name "Focus position" :effect-id :precision :version 1 :swatch "#b8eaae"})
        (assoc-in [:pages :main :cells [0 2]] :precision))))

(defn- command! [host command]
  (let [receipt (deref (show/submit! host :command command) 5000 nil)]
    (when-not (= :accepted (:status receipt))
      (throw (ex-info "Unexpected show command receipt" {:command command :receipt receipt})))
    receipt))

(defn- record [host selection]
  (let [snapshot @(:state host) model (:model snapshot)
        view (->> (waveforms/project-display (waveforms/create-cache) snapshot (push/display-view selection model) nil)
                  (precision/push-view (atom nil) model))
        buffer (get-in snapshot [:output :universes 1])
        channels (mapv #(bit-and 255 (aget ^bytes buffer %)) [0 5 6])]
    {:view view :dmx channels}))

(defn- capture! [host dir]
  (let [id (:instance-id (command! host {:op :cue/launch :cue-id :precision :launch {:mode :persistent}}))
        profile (push/profile {})
        state (assoc (:initial-state profile) :ready? true :selected-instance id :selected-cue :precision)
        input (fn [state cc]
                ((:input profile) state {:type :control-change :controller cc :raw-value 1}
                 (:model @(:state host)) {}))
        shifted (:state (input state 49))
        turn! (fn [cc] (let [command (first (:commands (input shifted cc)))]
                          (when-not command (throw (ex-info "Expected encoder edit" {:cc cc})))
                          (command! host command)))
        before (record host shifted)
        _ (turn! 71)
        _ (turn! 72)
        after (record host shifted)
        frames [before after]]
    (when-not (= [[127 127 255] [128 128 0]] (mapv :dmx frames))
      (throw (ex-info "Unexpected committed coarse/fine bytes" {:channels (mapv :dmx frames)})))
    (spit (io/file dir "capture.ndjson")
          (apply str (map-indexed (fn [i frame] (str (display/frame-request i (:view frame)) "\n")) frames)))
    (spit (io/file dir "dmx.json") (json/write-str (mapv :dmx frames)))
    id))

(defn -main [& [directory]]
  (let [dir (io/file (or directory "target/parameter-precision"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (model) :workspace-options nil} :web-server {:host "127.0.0.1" :port 0}})]
    (try
      (.mkdirs dir)
      (let [id (capture! host dir)
            origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))
            {:keys [exit out err]} (shell/sh "node" "dev/push-display/parameter-precision-qa.cjs" origin id (str dir))]
        (print out) (flush)
        (when-not (zero? exit) (throw (ex-info "Parameter precision browser QA failed" {:error err}))))
      (finally (app/stop! host) (shutdown-agents)))))
