(ns tolglow.qa.push-pad-feedback
  "Measure Push 2 session refresh and MIDI acknowledgements without a device."
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.devices.session :as session]
            [tolglow.bench-show :as bench]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.visualization.cue-pads :as pads])
  (:import [java.lang.management ManagementFactory ThreadMXBean]))

(defn- op [kind & inputs] {:op kind :args (vec inputs)})

(defn- wave [index]
  (let [phase (op :add (* 0.11 index) (op :multiply 0.8 {:op :beat}))]
    (op :add 0.5 (op :multiply 0.4 (op :sin phase)))))

(defn- pad [model index]
  (let [id (keyword (str "pad-" index))
        effect {:id id :version 1 :outputs {:color [1 0.4 0.2] :dimmer (wave index)}}
        cue {:id id :version 1 :effect-id id :swatch "#d88851"}]
    (-> model
        (assoc-in [:registry :effects id] effect)
        (assoc-in [:registry :cues id] cue)
        (assoc-in [:pages :main :cells [(mod index 8) (quot index 8)]] id))))

(defn- midi-recorder []
  (let [receive (volatile! nil) messages (volatile! []) ack (codec/sysex 0 [0x09])
        requested? (volatile! false)]
    {:midi {:open! (fn [callback] (vreset! receive callback) :capture)
            :send! (fn [_ batch]
                     (vswap! messages into batch)
                     (when (= ack (peek batch)) (vreset! requested? true)))
            :close! (fn [_])}
     :initialize! (fn []
                    (doseq [i (range 128)]
                      (@receive (assoc (codec/palette-write 0 i [0 0 0 0]) 6 4))))
     :drain! (fn []
               (loop [n 0]
                 (if @requested?
                   (do (when (>= n 9) (throw (ex-info "Push palette did not settle" {})))
                       (vreset! requested? false)
                       (@receive ack)
                       (recur (inc n)))
                   n)))
     :messages messages}))

(defn- frame! [runtime device recorder counter cpu frame]
  (show/tick! runtime (* 25.0 frame))
  (vreset! (:messages recorder) [])
  (vreset! counter 0)
  (let [start (cpu)
        _ (session/refresh! device)
        acks ((:drain! recorder))
        elapsed (/ (- (cpu) start) 1e6)]
    {:frame frame :cpu-ms elapsed :acknowledgements acks :projections @counter
     :palette-writes (filterv #(= 0x03 (get % 6)) @(:messages recorder))}))

(defn replay []
  (let [model (reduce pad (demo/model) (range 64))
        runtime (show/create-runtime {:model model :workspace-options nil :batch? false})
        recorder (midi-recorder) counter (volatile! 0) original pads/present!
        bean (ManagementFactory/getThreadMXBean)
        cpu #(.getCurrentThreadCpuTime ^ThreadMXBean bean)]
    (try
      (with-redefs [pads/present! (fn [& args] (vswap! counter inc) (apply original args))]
        (show/tick! runtime 0)
        (let [device (show/connect-device! runtime :push-2
                       {:session-id "capture" :midi (:midi recorder) :schedule! nil})
              capture! #(frame! runtime device recorder counter cpu %)]
          ((:initialize! recorder))
          ((:drain! recorder))
          (doseq [frame (range 1 81)] (capture! frame))
          (let [frames (mapv capture! (range 81 161))]
            {:frames frames :cpu-ms (bench/distribution (map :cpu-ms frames))
             :projections (reduce + (map :projections frames))
             :acknowledgements (reduce + (map :acknowledgements frames))
             :palette-writes (reduce + (map (comp count :palette-writes) frames))})))
      (finally (show/stop! runtime)))))

(defn -main [output-file]
  (try
    (let [report (replay)]
      (spit output-file (pr-str report))
      (prn (dissoc report :frames)))
    (finally (shutdown-agents))))
