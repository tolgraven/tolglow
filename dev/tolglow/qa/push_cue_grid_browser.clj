(ns tolglow.qa.push-cue-grid-browser
  "Run the optimized Studio against the preview-only demo show with 70
  running launches and a virtual Push 3 cue grid (injected transport, no MIDI
  ports), and check the round trip in headless Chrome (#545):
  Studio pages the grid's session box; recorded Push MIDI selects a launch
  and Studio badges it; Studio's Follow selection on a card selects that
  launch on the grid and the Push encoders adopt it. With push-2 as the
  third argument a virtual Push 2 runs the same round trip on
  tolglow.surface.push-2 (palette replies and LED acknowledgements recorded,
  Select on CC 48). Usage:
  lein with-profile +dev run -m tolglow.qa.push-cue-grid-browser [directory] [port] [push-3|push-2]"
  (:require [afterglow.control.codecs.push-2 :as push-2-codec]
            [afterglow.control.devices.session :as device-session]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.surface.link :as surface]
            [tolglow.surface.push-2 :as push-2]))

(def ^:private cues [:ember :ice :sage :moon :amber :indigo :orchid :merlot])

(def ^:private layout
  "A synthetic User Mode capture: pad [x y] from the bottom-left, notes 36..99."
  {:pads (into {} (for [x (range 8) y (range 8)] [[x y] {:channel 0 :note (+ 36 x (* 8 y))}]))
   :buttons {:select {:channel 0 :controller 20} :reorder {:channel 0 :controller 21}
             :up {:channel 0 :controller 46} :down {:channel 0 :controller 47}}})

(defn- accepted! [ack what]
  (when-not (= :accepted (:status ack)) (throw (ex-info (str "Could not " what) ack)))
  ack)

(defn- check! [ok? message data]
  (when-not ok? (throw (ex-info message data)))
  (println "ok:" message))

(defn- node! [origin directory phase & args]
  (let [{:keys [exit out err]} (apply shell/sh "node" "dev/studio/push-cue-grid-qa.mjs" origin directory phase args)]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info (str "Push cue grid browser check failed (" phase ")") {:error err})))))

(defn- grid [host]
  (some #(when (= "push-a" (:id %)) %) (:logical (:control-surfaces (show/snapshot host)))))

(defn- running-order [host]
  (let [{:keys [active-instance-order instances]} (show/snapshot host)
        running (into #{} (keep #(when (= :running (:phase %)) (:id %))) instances)]
    (filterv running active-instance-order)))

(defn- settle! [pred what]
  (loop [n 0]
    (cond (pred) true
          (> n 200) (throw (ex-info (str "Timed out waiting for " what) {}))
          :else (do (Thread/sleep 25) (recur (inc n))))))

(def ^:private led-query (push-2-codec/sysex 0 [0x09]))

(defn- last-palette-rgb
  "The last [r g b] a Push 2 palette write gave pad note."
  [sent note]
  (some (fn [m] (when (and (vector? m) (= 0xf0 (first m)) (= [0x03 note] (subvec m 6 8)))
                  (let [[rl rh gl gh bl bh] (subvec m 8 14)] [(+ rl (* 128 rh)) (+ gl (* 128 gh)) (+ bl (* 128 bh))])))
        (rseq (vec sent))))

(defn -main [& [directory port model]]
  (let [port (Long/parseLong (or port "19320"))
        push-2? (= "push-2" model)
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (demo/model) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/push-cue-grid-browser")
        repl! #(accepted! @(show/submit! host :command (merge {:source {:adapter :repl :session-id "qa"}} %)) (pr-str %))
        receive (atom nil)
        sent (atom [])
        transport {:open! (fn [callback] (reset! receive callback) :virtual)
                   :send! (fn [_ batch]
                            (swap! sent into batch)
                            ;; A Push 2 acknowledges each LED batch; answer off the session's thread.
                            (when (and push-2? (some #{led-query} batch))
                              (future (Thread/sleep 2) (@receive led-query))))
                   :close! (fn [_])}
        midi! (fn [& messages] (doseq [m messages] (@receive m)))]
    (try
      (.mkdirs (io/file directory))
      (dotimes [n 70]
        (repl! {:op :cue/launch :cue-id (get cues (mod n (count cues))) :launch {:mode :persistent}}))
      (check! (= 70 (count (running-order host))) "70 running launches" {:running (count (running-order host))})
      (let [device (if push-2?
                     (show/connect-device! host :push-2 {:session-id "push-a" :active-cue-grid? true :midi transport})
                     (show/connect-device! host :push-3 {:session-id "push-a" :user-mode? true
                                                         :expression-mode :poly-aftertouch :layout layout
                                                         :cue-grid {:buttons {:select :cue-grid/select :reorder :cue-grid/reorder
                                                                              :up :cue-grid/page-up :down :cue-grid/page-down}}
                                                         :transport transport}))]
        (when push-2?
          ;; The Push 2 profile reads its 128 palette entries before any LED.
          (doseq [index (range 128)] (midi! (assoc (push-2-codec/palette-write 0 index [0 0 0 0]) 6 4)))
          (settle! #(get-in (device-session/snapshot device) [:local :ready?]) "the Push 2 palettes"))
        (settle! #(some-> (grid host) :members first :attached?) "the grid surface to attach")
        (node! origin directory "browser")
        (check! (= {:x 0 :y 0} (:window (grid host))) "Studio paged the grid down and back up" (grid host))
        ;; Push -> Studio: recorded bytes open Select and tap the top-left pad. A
        ;; Push 2 pad acts once its LEDs are acknowledged.
        (if push-2?
          (do (midi! [0xb0 48 127] [0xb0 48 0])
              (settle! #(get-in (device-session/snapshot device) [:local :pad-overlay-presented]) "the Push 2 grid LEDs")
              (midi! [0x90 92 100] [0x80 92 0]))
          (midi! [0xb0 20 127] [0xb0 20 0] [0x90 92 100] [0x80 92 0]))
        (let [[first-launch second-launch] (running-order host)]
          (check! (= [:instance first-launch] (get-in (grid host) [:layout :selected]))
                  "the Push pad committed the grid selection" (:layout (grid host)))
          (check! (= 70 (count (running-order host))) "selection launched and ended nothing" {})
          (node! origin directory "push" first-launch second-launch)
          (settle! #(= [:instance second-launch] (get-in (grid host) [:layout :selected])) "Studio's follow selection")
          (println "ok: Studio Detail with Follow selection committed" (get-in (grid host) [:layout :selected]))
          (settle! #(= second-launch (get-in (device-session/snapshot device) [:local :selected-instance]))
                   "the Push encoders to adopt Studio's selection")
          (println "ok: the Push encoders follow" second-launch)
          (if push-2?
            (let [color (some #(when (= second-launch (:instance-id %)) (:color %))
                              (surface/content-cells (:model @(:state host)) {:type :active-cues}))
                  bright (push-2/pad-rgb {:color color :selected? true})]
              (settle! #(= bright (last-palette-rgb @sent 93)) "the second pad to light at full brightness")
              (println "ok: the Push 2 pad of Studio's selection is bright (captured palette write)" bright))
            (do (settle! #(some (fn [m] (= [0x99 93] (take 2 m))) @sent) "the second pad to pulse")
                (println "ok: the Push pad of Studio's selection pulses (captured LED bytes)")))
          (spit (io/file directory "push-led-bytes.edn") (pr-str @sent))))
      (finally (app/stop! host) (shutdown-agents)))))
