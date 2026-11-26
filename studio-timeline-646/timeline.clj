(ns tolglow.qa.timeline
  "Preview-only synthetic show for the musical timeline browser check (#646).
  It runs the real owner, scheduler, capture worker and section analyzer on
  generated PCM that follows the show transport. No device, port or provider is
  opened except the loopback web server.

  lein run -m tolglow.qa.timeline <port> <control-dir>

  Choose a port other than 4190: Node's fetch refuses it as an unsafe port.

  Files in the control directory steer the scenario while it runs:
  `match-off` stops the simulated recording match, `feel` fills the track-feel
  slot with synthetic data, and `stop` shuts the show down."
  (:require [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.integrations.reference-position :as position]
            [tolglow.show :as show])
  (:import [java.io File]
           [java.util Random]))

(def ^:private sample-rate 48000.0)

(def ^:private section-beats 16)

(def ^:private planner {:adapter :agent :session-id "qa-planner"})

(def ^:private operator {:adapter :repl :session-id "qa-operator"})

(defn- show-time
  "Current time on the show's own clock, the clock the capture worker stamps."
  [runtime]
  (let [{:keys [origin-nanos time-ms]} @(:clock runtime)]
    (+ time-ms (/ (- (System/nanoTime) origin-nanos) 1.0e6))))

(defn- beat-at [{:keys [beat bpm time-ms playing?]} at-ms]
  (if (false? playing?) beat (+ beat (* (- at-ms time-ms) (/ bpm 60000.0)))))

;; Synthetic input ---------------------------------------------------------------

(defn- voice
  "One PCM sample for a musical position. Each section alternates two bass
  notes in the opposite order from the last, so the causal section analyzer
  confirms a bass-pattern change every `section-beats`. A mid stab and noise
  hats give the frequency-coloured waveform something to show."
  [beat seconds ^Random noise]
  (let [index (long (Math/floor beat))
        fraction (- beat index)
        section (quot index section-beats)
        notes (if (even? section) [48.0 84.0] [84.0 48.0])
        hz (nth notes (mod index 2))
        bass (* 0.34 (+ 0.6 (* 0.4 (Math/exp (* -4.0 fraction)))) (Math/sin (* 2.0 Math/PI hz seconds)))
        stab (if (odd? index)
               (* 0.3 (Math/exp (* -6.0 fraction)) (Math/sin (* 2.0 Math/PI 660.0 seconds)))
               0.0)
        half (- (* 2.0 beat) (Math/floor (* 2.0 beat)))
        hat (* 0.22 (Math/exp (* -18.0 half)) (- (* 2.0 (.nextDouble noise)) 1.0))]
    (+ bass stab hat)))

(defn- open-source
  "A paced mono signed-16 source. A block is delivered only after its last
  sample's time has passed, as a real input line would."
  [runtime-ref]
  (fn [_config]
    (let [origin (System/nanoTime)
          frames (atom 0)
          noise (Random. 646)
          closed? (atom false)]
      {:description {:name "Synthetic timeline input" :format :signed-16-pcm}
       :read! (fn [^bytes out offset length]
                (let [count (quot length 2)
                      start @frames
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
                          block-ms (* 1000.0 (/ count sample-rate))]
                      (dotimes [i count]
                        (let [frame (+ start i)
                              at-ms (+ (- now block-ms) (* 1000.0 (/ i sample-rate)))
                              value (voice (beat-at transport at-ms) (/ frame sample-rate) noise)
                              pcm (long (* 32767.0 (max -1.0 (min 1.0 value))))
                              at (+ offset (* 2 i))]
                          (aset-byte out at (unchecked-byte (bit-and pcm 255)))
                          (aset-byte out (inc at) (unchecked-byte (bit-shift-right pcm 8)))))
                      (swap! frames + count)
                      (* 2 count)))))
       :close! #(reset! closed? true)})))

;; Synthetic matched recording ----------------------------------------------------------

(def ^:private track-bpm 120.0)

(def ^:private profile
  "A version 1 track profile of the same synthetic music: one section every
  `section-beats`, in two alternating families."
  (let [section-ms (* section-beats (/ 60000.0 track-bpm))
        sections 96]
    {:version 1 :reference {:id "qa:timeline-track" :version "qa:1"}
     :duration-ms (* sections section-ms)
     :sections (mapv (fn [index]
                       (cond-> {:id index :family-id (mod index 2)
                                :start-position-ms (* index section-ms) :end-position-ms (* (inc index) section-ms)
                                :start-beat (double (* index section-beats))
                                :boundary-kind (if (zero? index) :file-start :confirmed-change)
                                :boundary-confidence (if (zero? index) 1.0 (+ 0.6 (* 0.1 (mod index 4))))}
                         (pos? index) (assoc :features {:signature {:bass-notes [(if (even? index) 48 84)]}})))
                     (range sections))}))

(defn- match-loop
  "Publish a simulated match of the live music against the profile every 100
  ms: the recording position that corresponds to the transport beat, and the
  playback rate the current tempo implies. Stops publishing while `off?`."
  [runtime running? off?]
  (let [{:keys [provenance alignment-options now-ms! publish!]} (show/section-reference-callbacks runtime)]
    (loop [state (position/new-state provenance alignment-options)]
      (when @running?
        (Thread/sleep 100)
        (if (off?)
          (recur state)
          (let [now (now-ms!)
                {:keys [beat bpm]} (get-in @(:state runtime) [:model :transport])
                observation {:type :match :provenance provenance :occurrence (:occurrence state) :observed-at-ms now
                             :match {:reference (:reference profile) :position-ms (* beat (/ 60000.0 track-bpm))
                                     :rate (/ bpm track-bpm) :score 0.93}}
                next-state (try (position/advance state now observation)
                                (catch clojure.lang.ExceptionInfo _ state))]
            (publish! next-state)
            (recur next-state)))))))

;; Scheduled content -----------------------------------------------------------------

(defn- plan
  "Events for the eight bars after `bar`, posted the way an agent plans a
  phrase: launches, sets, ramps with different curves, one action that cannot
  match anything and one the planner will withdraw."
  [bar tag]
  (let [id #(str tag "-" %)
        colour (nth [:ember :indigo :sage :amber :orchid :ice] (mod (quot bar 8) 6))]
    [{:id (id "wash") :label (str "Wash " (name colour)) :op :cue/launch :cue-id colour
      :launch {:mode :one-shot :duration {:bars 4}} :at {:bar (+ bar 1)}}
     {:id (id "swell") :label "Swell" :op :param/ramp :cue-id :breathe :param :level
      :to 1.0 :over {:bars 3} :curve :ease-in-out :at {:bar (+ bar 1)}}
     {:id (id "accent") :label "Accent" :op :cue/launch :cue-id :flash :launch {:mode :one-shot}
      :at {:bar (+ bar 2) :beat 2}}
     {:id (id "period") :label "Faster pulse" :op :param/set :cue-id :breathe :param :period :value 2
      :at {:bar (+ bar 3)}}
     {:id (id "phrase") :op :variable/set :variable :qa/phrase :value (quot bar 4) :at {:bar (+ bar 4)}}
     {:id (id "pull") :label "Pull back" :op :param/ramp :cue-id :breathe :param :level
      :to 0.25 :over {:bars 2} :curve {:type :cubic-bezier :points [0.1 0.9 0.2 1.0]} :at {:bar (+ bar 4) :beat 2}}
     {:id (id "ghost") :label "Missing target" :op :instance/end :select {:ids ["no-such-instance"]}
      :at {:bar (+ bar 5)}}
     {:id (id "spot") :label "Solitary" :op :cue/launch :cue-id :spot :launch {:mode :one-shot}
      :at {:bar (+ bar 6)}}
     {:id (id "slow") :label "Slow pulse" :op :param/set :cue-id :breathe :param :period :value 8
      :at {:bar (+ bar 7)}}
     {:id (id "settle") :label "Settle" :op :param/ramp :cue-id :breathe :param :level
      :to 0.6 :over {:beats 6} :curve :ease-out :at {:bar (+ bar 7)}}
     {:id (id "dropped") :label "Second thoughts" :op :cue/launch :cue-id :moon :at {:bar (+ bar 7) :beat 2}}]))

(defn- report! [what reply]
  (let [receipt (deref reply 5000 {:status :pending})]
    (when-not (= :accepted (:status receipt))
      (println "timeline-qa" what (pr-str (select-keys receipt [:status :message :details])))
      (flush))
    receipt))

(defn- post-block! [runtime base]
  (let [tag (str "b" base)]
    (report! "plan" (show/post-events! runtime (plan base tag) {:source planner}))
    (report! "operator" (show/post-events!
                         runtime
                         [{:id (str tag "-dip") :label "Master dip" :op :param/ramp :target :master
                           :to 0.7 :over {:beats 2} :curve :sine :at {:bar (+ base 5) :beat 2}}
                          {:id (str tag "-restore") :label "Master up" :op :master/set :value 1.0
                           :at {:bar (+ base 6) :beat 2}}]
                         {:source operator}))
    (report! "withdraw" (show/submit! runtime {:op :schedule/cancel :event-ids [(str tag "-dropped")] :source planner}))))

(defn- direct!
  "Keep the queue stocked eight bars ahead, and act like the people around it:
  the operator dips and restores the master, the planner withdraws one event."
  [runtime running?]
  (loop [base nil]
    (when @running?
      (Thread/sleep 250)
      (let [{:keys [beat meter]} (get-in @(:state runtime) [:model :transport])
            bar (long (Math/floor (/ beat (:beats-per-bar meter 4))))]
        (cond
          (nil? base) (do (post-block! runtime (inc bar)) (recur (inc bar)))
          (>= bar (- base 1)) (do (post-block! runtime (+ base 8)) (recur (+ base 8)))
          :else (recur base))))))

(def ^:private feel
  "Shaped like the active entry of the personality projection (#647): SVG text
  under :icon and a palette of [r g b] colours, a gradient and a split."
  {:id :qa/squares :name "Squares (synthetic)" :version 1 :seed 0 :current? true
   :palette [[0.957 0.322 0.196]
             {:gradient [{:position 0.0 :color [0.867 0.651 0.278]} {:position 1.0 :color [1.0 1.0 1.0]}] :angle-deg 0}
             {:split [[0.412 0.729 0.780] [0.1 0.1 0.1]] :angle-deg 90}]
   :forms [{:form :square :weight 0.6} {:form :rectangle :weight 0.4}]
   :icon (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 48 48'>"
              "<rect width='48' height='48' rx='9' fill='#171b18'/>"
              "<rect x='7' y='9' width='18' height='18' fill='#f45232'/>"
              "<rect x='20' y='19' width='20' height='13' fill='#dda647'/>"
              "<rect x='12' y='30' width='11' height='11' fill='#69bac7'/></svg>")})

(defn- follow-controls! [runtime running? ^File directory]
  (loop [shown? false]
    (when @running?
      (Thread/sleep 200)
      (let [wanted? (.exists (File. directory "feel"))]
        (when (not= wanted? shown?)
          (show/submit! runtime {:op :variable/set :variable :music/personality
                                 :value (if wanted? feel false) :source operator}))
        (when (.exists (File. directory "stop")) (reset! running? false))
        (recur wanted?)))))

(defn- accepted! [reply]
  (let [receipt (deref reply 5000 {:status :pending})]
    (when-not (= :accepted (:status receipt))
      (throw (ex-info "Timeline QA setup was not accepted" {:receipt receipt})))
    receipt))

(defn -main [& [port control-dir]]
  (let [directory (File. (str control-dir))
        runtime-ref (atom nil)
        running? (atom true)
        runtime (app/start! {:init {:components [:show] :modules [:web-server]}
                             :show {:model (demo/model) :workspace-options nil
                                    :audio-options {:open-source! (open-source runtime-ref)}
                                    :section-patterns {:enabled? true :patterns [:qa/a :qa/b] :on :bar}
                                    :section-reference {:profile profile
                                                        :provenance {:source-id :qa/deck :session-id "qa-1"
                                                                     :clock-id :host}}}
                             :web-server {:host "127.0.0.1" :port (Long/parseLong (str port))}})
        spawn (fn [name task]
                (doto (Thread. ^Runnable (fn [] (try (task)
                                                     (catch InterruptedException _)
                                                     (catch Throwable error (.printStackTrace error))))
                               ^String name)
                  (.setDaemon true) (.start)))]
    (reset! runtime-ref runtime)
    (try
      (.mkdirs directory)
      (doseq [scope [:lighting-activity :lighting-budget]]
        (accepted! (show/submit! runtime :settings {:scope scope :values {:enabled? true}})))
      (accepted! (show/submit! runtime {:op :cue/launch :cue-id :breathe :launch {:mode :persistent} :source operator}))
      (accepted! (show/register-event-subscriber!
                  runtime {:id :qa/phrase-accent :match {:type :clock/phrase}
                           :actions [{:op :cue/launch :select {:ids [:flash] :pick :first}
                                      :launch {:mode :one-shot} :delay {:beats 3}}]}))
      (show/audio-action! runtime :start {:default? true :sample-rate sample-rate :channels 1 :block-frames 480
                                          :analyzer-options {:sections {:beats-per-window 2}}})
      (spawn "timeline-qa-director" #(direct! runtime running?))
      (spawn "timeline-qa-match" #(match-loop runtime running? (fn [] (.exists (File. directory "match-off")))))
      (println "timeline-qa ready" (get-in runtime [:application :modules :web-server :port]))
      (flush)
      (follow-controls! runtime running? directory)
      (finally
        (reset! running? false)
        (app/stop! runtime)
        (shutdown-agents)))))
