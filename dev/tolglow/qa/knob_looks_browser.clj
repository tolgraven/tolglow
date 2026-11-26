(ns tolglow.qa.knob-looks-browser
  "Run the optimized Studio against the preview-only demo show and exercise
  knob looks (#547) in headless Chrome: save and recall from the Studio strip,
  recall from the REPL with the page open, then drive a virtual Push 2 (captured
  MIDI, no device) to the knob-look overlay and render its display views through
  the shipped Push capture worker. Usage:
  lein with-profile +dev run -m tolglow.qa.knob-looks-browser [directory] [port]"
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.devices.session :as session]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.knob-presets :as presets]
            [tolglow.show :as show]))

(defn- accepted! [ack what]
  (when-not (= :accepted (:status ack)) (throw (ex-info (str "Could not " what) {:ack ack})))
  ack)

(defn- node! [& args]
  (let [{:keys [exit out err]} (apply shell/sh "node" "dev/studio/knob-looks-browser-qa.mjs" args)]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info "Knob looks browser check failed" {:phase (last args) :error err})))))

(defn- virtual-push-2
  "A Push 2 session on captured MIDI. The display records the adapter's views."
  [host]
  (let [receive (atom nil) awaiting? (atom false) views (atom [])
        ack (codec/sysex 0 [0x09])
        device (show/connect-device! host :push-2
                 {:session-id "knob-looks-qa" :schedule! nil :timeout-ms 60000 :cue-variations? true
                  :midi {:open! #(do (reset! receive %) :capture)
                         :send! (fn [_ batch] (when (= ack (peek batch)) (reset! awaiting? true)))
                         :close! (fn [_])}
                  :display {:open! (constantly :capture) :render! (fn [_ view] (swap! views conj view)) :close! (fn [_])}})
        ;; Acknowledge LED batches until the overlay's pads are presented, as
        ;; a press counts only once the lit pads reached the device.
        settle! (fn []
                  (loop [n 0]
                    (Thread/sleep 50)
                    (when (compare-and-set! awaiting? true false) (@receive ack))
                    (when (and (< n 60) (:pad-overlay (:local (session/snapshot device)))
                               (nil? (:pad-overlay-presented (:local (session/snapshot device)))))
                      (recur (inc n)))))
        bank #(presets/bank (:model @(:state host)))]
    (doseq [index (range 128)] (@receive (assoc (codec/palette-write 0 index [0 0 0 73]) 6 4)))
    (settle!)
    {:device device :views views :settle! settle!
     :button! (fn [cc] (@receive [0xb0 cc 127]) (@receive [0xb0 cc 0]) (settle!))
     :pad! (fn [[x y]]
             (settle!)
             (let [note (codec/pad-note x y) before (bank)]
               (@receive [0x90 note 100]) (@receive [0x80 note 0])
               (loop [n 0] (when (and (< n 60) (= before (bank))) (Thread/sleep 50) (recur (inc n))))
               (settle!)))}))

(defn- json-view [view]
  (json/write-str view :key-fn #(if (keyword? %) (subs (str %) 1) (str %))
                  :value-fn (fn [_ v] (if (keyword? v) (subs (str v) 1) v))))

(defn -main [& [directory port]]
  (let [port (Long/parseLong (or port "4500"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (demo/model) :workspace-options nil}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/knob-looks-browser")
        repl! #(accepted! @(show/submit! host :command (merge {:source {:adapter :repl :session-id "qa"}} %)) (pr-str %))]
    (try
      (.mkdirs (io/file directory))
      (doseq [cue [:ember :breathe :sweep]] (repl! {:op :cue/launch :cue-id cue :launch {:mode :persistent}}))
      (repl! {:op :knob/create :knob-id "look" :name "Look"
              :links [{:id "width" :destination {:kind :param :cue-id :sweep :param :width} :range [0.05 0.45]}
                      {:id "period" :destination {:kind :param :cue-id :breathe :param :period} :range [2 1/2]
                       :quantize {:ratios :preferred}}]})
      (repl! {:op :knob/create :knob-id "glow" :name "Glow"
              :links [{:id "level" :destination {:kind :param :cue-id :ember :param :level} :range [0.2 1.0]
                       :curve :ease-in}]})
      (repl! {:op :knob/set :knob-id "look" :value 0.2})
      (repl! {:op :knob/set :knob-id "glow" :value 0.3})
      (node! origin directory "studio")
      ;; REPL -> Studio: recall the second look and check the open strip follows.
      (repl! {:op :knob-preset/recall :slot 1})
      (println "REPL recalled look 2:" (mapv :state (:slots (presets/projection (:model @(:state host))))))
      (node! origin directory "repl")
      ;; Push 2 (captured MIDI): Note cycles full states, rows, then knob looks.
      (let [{:keys [views button! pad! device]} (virtual-push-2 host)
            overlay #(:pad-overlay (peek @views))]
        (dotimes [_ 3] (button! 50))
        (when-not (= :knob-presets (:layout (overlay)))
          (throw (ex-info "Push 2 did not reach knob looks" {:overlay (overlay)})))
        (let [before (peek @views)]
          (pad! [3 7])
          (pad! [0 7])
          (let [after (peek @views)
                file (io/file directory "push-views.ndjson")]
            (println "Push 2 knob looks:" (mapv :state (:cells (:pad-overlay after)))
                     "errors" (:errors (session/snapshot device)))
            (spit file (str (json-view {:id "push:knob-looks-before" :view before}) "\n"
                            (json-view {:id "push:knob-looks-after" :view after}) "\n")))))
      (node! origin directory "push")
      (finally (app/stop! host) (shutdown-agents)))))
