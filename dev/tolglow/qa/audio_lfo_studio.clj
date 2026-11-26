(ns tolglow.qa.audio-lfo-studio
  "Preview-only show for the audio-shaped LFO browser check (#676). It runs
  the real owner, capture worker and shape extractor on a paced synthetic input
  that plays the delayed-synth passage in time with the show transport, with
  two audio LFOs and the Breathe cue's level bound to one of them. Only the
  loopback web server is opened.

  lein run -m tolglow.qa.audio-lfo-studio <port> <control-dir>

  A `stop` file in the control directory shuts the show down; `silence` mutes
  the input so the silence policy shows."
  (:require [tolglow.application :as app]
            [tolglow.audio.fixtures :as fixtures]
            [tolglow.demo :as demo]
            [tolglow.show :as show])
  (:import [java.io File]))

(def ^:private sample-rate 48000.0)

(def ^:private operator {:adapter :repl :session-id "qa-operator"})

(defn- show-time [runtime]
  (let [{:keys [origin-nanos time-ms]} @(:clock runtime)]
    (+ time-ms (/ (- (System/nanoTime) origin-nanos) 1.0e6))))

(defn- beat-at [{:keys [beat bpm time-ms playing?]} at-ms]
  (if (false? playing?) beat (+ beat (* (- at-ms time-ms) (/ bpm 60000.0)))))

(defn- open-source
  "A paced mono signed-16 source that plays the passage continuously, starting
  at the sample for the transport beat of its first block, so its bars stay on
  the show's bar lines without a seam at each block. Delivered once captured,
  like a line."
  [runtime-ref ^doubles passage passage-bpm muted?]
  (fn [_config]
    (let [origin (System/nanoTime) frames (atom 0) closed? (atom false)
          per-beat (* sample-rate (/ 60.0 passage-bpm))
          anchor (atom nil)
          n (alength passage)]
      {:description {:name "Synthetic passage input" :format :signed-16-pcm}
       :read! (fn [^bytes out offset length]
                (let [count (quot length 2) start @frames
                      due (+ origin (long (* 1.0e9 (/ (+ start count) sample-rate))) 4000000)]
                  (loop []
                    (let [wait (- due (System/nanoTime))]
                      (when (and (pos? wait) (not @closed?))
                        (Thread/sleep (long (max 1 (quot wait 1000000))))
                        (recur))))
                  (if @closed?
                    -1
                    (let [runtime @runtime-ref
                          transport (get-in @(:state runtime) [:model :transport])
                          now (show-time runtime)
                          block-ms (* 1000.0 (/ count sample-rate))
                          mute? (muted?)
                          first-index (or @anchor
                                          (reset! anchor (- (long (Math/floor (* per-beat (beat-at transport (- now block-ms)))))
                                                            start)))]
                      (dotimes [i count]
                        (let [index (mod (+ first-index start i) n)
                              value (if mute? 0.0 (aget passage index))
                              pcm (long (* 32767.0 (max -1.0 (min 1.0 value))))
                              at (+ offset (* 2 i))]
                          (aset-byte out at (unchecked-byte (bit-and pcm 255)))
                          (aset-byte out (inc at) (unchecked-byte (bit-shift-right pcm 8)))))
                      (swap! frames + count)
                      (* 2 count)))))
       :close! #(reset! closed? true)})))

(defn- accepted! [reply]
  (let [receipt (deref reply 5000 {:status :pending})]
    (when-not (= :accepted (:status receipt))
      (throw (ex-info "Audio LFO QA setup was not accepted" {:receipt receipt})))
    receipt))

(defn -main [& [port control-dir]]
  (let [directory (File. (str control-dir))
        runtime-ref (atom nil)
        model (demo/model)
        bpm (double (get-in model [:transport :bpm] 120.0))
        passage (fixtures/passage :delay-synth {:bpm bpm :bars 16})
        runtime (app/start! {:init {:components [:show] :modules [:web-server]}
                             :show {:model model :workspace-options nil
                                    :audio-options {:open-source! (open-source runtime-ref passage bpm
                                                                               #(.exists (File. directory "silence")))}}
                             :web-server {:host "127.0.0.1" :port (Long/parseLong (str port))}})]
    (reset! runtime-ref runtime)
    (try
      (.mkdirs directory)
      (accepted! (show/submit! runtime {:op :audio-lfo/configure :lfo-id :bass-envelope :name "Bass envelope"
                                        :config {:extract :envelope :band :bass :morph {:bars 1}}
                                        :source operator}))
      (accepted! (show/submit! runtime {:op :audio-lfo/configure :lfo-id :synth-cycle :name "Synth cycle"
                                        :config {:extract :cycle :band :full :morph {:bars 2} :smoothing 0.25}
                                        :source operator}))
      (let [launch (accepted! (show/submit! runtime {:op :cue/launch :cue-id :breathe :launch {:mode :persistent}
                                                     :source operator}))]
        (accepted! (show/submit! runtime :modulation-bind
                                 {:instance-id (:instance-id launch) :param :level
                                  :binding {:type :lfo :waveform :audio/synth-cycle :period {:beats 1}
                                            :from 0.1 :to 1.0 :phase-reference :transport}})))
      (show/audio-action! runtime :start {:default? true :sample-rate sample-rate :channels 1 :block-frames 480})
      (println "audio-lfo-qa ready" (get-in runtime [:application :modules :web-server :port]))
      (flush)
      (loop []
        (Thread/sleep 200)
        (when-not (.exists (File. directory "stop")) (recur)))
      (finally
        (app/stop! runtime)
        (shutdown-agents)))))
