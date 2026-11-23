(ns tolglow.qa.push-parameters
  "Exercise the physical display's IPC and the Studio editors on a preview-only show."
  (:require [afterglow.control.devices.push-2 :as push]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.studio.display :as display]
            [tolglow.waveforms :as waveforms]))

(defn- scalar [label value] {:name label :type :number :default value :bounds [0 1]})

(def orbital-effect
  {:id :orbital :name "Orbital wash" :version 1 :categories [:movement :color]
   :outputs {:color {:op :param :key :color} :dimmer {:op :param :key :dimmer}}
   :params {:aim {:type :vector :name "Orbit aim" :default [0.3 0.4 -0.8] :size 3 :bounds [-1 1]}
            :bearing {:name "Gimbal" :type :vector :semantic/type :orientation :unit :degrees
                      :size 3 :bounds [-180 180] :default [20.0 50.0 25.0]}
            :color {:name "Beam tint" :type :color :default [0.2 0.8 0.6]}
            :dimmer (scalar "Intensity" 0.7) :focus (scalar "Focus" 0.6) :glow (scalar "Glow" 0.3)
            :iris (scalar "Iris" 0.4) :spread (scalar "Beam width" 0.45)}})

(defn- send! [host op payload]
  (let [ack (deref (show/submit! host op payload) 10000 nil)]
    (when-not (= :accepted (:status ack)) (throw (ex-info "QA command failed" {:ack ack}))) ack))

(defn- command! [host command] (send! host :command command))

(defn- orbit [phase]
  (let [c (Math/cos phase) s (Math/sin phase)]
    {:aim [(* c 0.83) (* 0.68 (Math/sin (* 1.23 phase))) (* s 0.89)]
     :bearing [(* c 85) (* s 70) (* c 23)]
     :color [(+ 0.5 (* 0.45 c)) (+ 0.5 (* 0.48 s)) (+ 0.5 (* 0.49 (Math/sin (+ phase 2))))]
     :dimmer (+ 0.5 (* 0.47 (Math/sin (* 0.8 phase))))}))

(defn- record! [host id file]
  (let [cache (waveforms/create-cache)
        selection {:selected-instance id :selected-cue :orbital :ready? true :page-id :main :origin [0 0]}]
    (with-open [out (io/writer file)]
      (doseq [i (range 48)]
        (cond
          (< i 17) (doseq [[k v] (orbit (* 0.18 i))]
                     (command! host {:op :param/set :instance-id id :param k :value v}))
          (= i 17) (doseq [[param from to] [[:aim [-1 0 -1] [1 1 1]] [:dimmer 0.1 0.9] [:focus 0.6 0.6]]]
                     (send! host :modulation-bind {:instance-id id :param param
                                                   :binding {:type :lfo :waveform :sine :from from :to to
                                                             :period {:beats 2}}}))
          (= i 34) (send! host :modulation-hold {:instance-id id :params [:aim :dimmer :focus]})
          (= i 37) (send! host :modulation-resume {:instance-id id :params [:aim :dimmer :focus]})
          (= i 44) (doseq [param [:aim :dimmer :focus]]
                     (send! host :modulation-unbind {:instance-id id :param param})))
        ;; The running clock publishes the values read by the passive worker.
        (when (>= i 16) (Thread/sleep 110))
        (let [snapshot @(:state host) view (push/display-view selection (:model snapshot))]
          (.write out (display/frame-request i (waveforms/project-display cache snapshot view nil)))
          (.write out "\n"))))))

(defn -main [& [directory]]
  (let [dir (io/file (or directory "target/push-parameter-qa"))
        model (-> (demo/model)
                  (assoc-in [:registry :effects :orbital] orbital-effect)
                  (assoc-in [:registry :cues :orbital] {:id :orbital :name "Orbital wash" :effect-id :orbital
                                                        :version 1 :swatch "#b6df91"})
                  (assoc-in [:pages :main :cells [0 2]] :orbital))
        host (app/start! {:init {:components [:show] :modules [:web-server]} :show {:model model :workspace-options nil}
                          :web-server {:host "127.0.0.1" :port 0}})]
    (try
      (.mkdirs dir)
      (let [id (:instance-id (command! host {:op :cue/launch :cue-id :orbital :launch {:mode :persistent}}))
            origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))]
        (record! host id (io/file dir "requests.ndjson"))
        (let [{:keys [out err exit]} (shell/sh "node" "dev/push-display/parameter-views-qa.cjs" origin id (str dir))]
          (print out) (flush)
          (when-not (zero? exit) (throw (ex-info "Parameter display browser check failed" {:error err})))))
      (finally (app/stop! host) (shutdown-agents)))))
