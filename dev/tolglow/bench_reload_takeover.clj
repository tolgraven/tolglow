(ns tolglow.bench-reload-takeover
  "Owner tick cost and adoption latency of hot-reload takeovers (#663, #774).

  Every probe cue has a waveform bound to its level, so a revision that changes
  the level's bounds makes the candidate and last-good samples differ and the
  owner supplies `::engine/reload-fallback`. Each round publishes a revision
  of 1, 8 or 32 probes in one owner cycle, whose outcome is:

  - :accepted  the engine adopts every candidate;
  - :evaluate  every candidate divides by zero (fallback pass);
  - :output    every candidate aims its head at its own position, which the
               renderer refuses after the engine accepted it (a second
               evaluation and render; one candidate per frame).

  On the mixed rig a further case revises one effect launched once per moving
  head with its own :targets (64 programs), as spatial actions launch.

  Each rig runs in two modes: :inline compiles new programs inside the frame,
  as before #774; :background compiles them on the show's worker and adopts
  the revision once they are ready. Frames hold a 25 ms cadence with a spin.
  Reported per case: the publication frame, the adoption frame (first frame
  that attempts the revision), adoption latency in frames, frames until every
  instance settled, and programs compiled on the owner inside those frames.

  Rigs: the showcase with five cues and their waveforms, and the mixed
  2,048-pixel/64-head corpus of `tolglow.bench-show`. Capture sink, two
  effect workers. No network output or devices.

    bin/repl run tolglow.bench-reload-takeover [rounds] [showcase|mixed ...] [inline|background ...]
    lein with-profile +dev run -m tolglow.bench-reload-takeover 20

  Stage timings come from wrapping `engine/advance`, `engine/reject-output`,
  command handling and modulation sampling during the run."
  (:require [afterglow.runtime.engine :as engine]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [tolglow.bench-show :as bench]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.modulation :as modulation]
            [tolglow.show :as show]
            [tolglow.showcase :as showcase]
            [tolglow.showcase.looks :as looks]
            [tolglow.showcase.rig :as rig]))

(def probes
  "Probe cues running in every measured frame; a round reloads some of them."
  32)

(def reloading [1 8 32])

(def outcomes [:accepted :evaluate :output])

(def ^:private level-binding
  ;; Outside a [0 0.5] level, so a revision with that bound samples nothing.
  {:type :lfo :waveform :sine :period {:bars 2} :phase-reference :transport :from 0.6 :to 0.9})

(defn- probe-id [i] (keyword (str "probe-" i)))

(defn- probe-effect
  "Version `version` of probe `i` on `fixture`, with level bounds [0 upper],
  rejected at `outcome` (nil or :accepted for a healthy revision)."
  [i {:keys [id position]} version upper outcome]
  (cond-> {:id (probe-id i) :version version :name (str "Probe " i) :targets [id]
           :params {:level {:type :number :default 0.5 :bounds [0.0 upper]}}
           :outputs {:dimmer {:op :param :key :level}}}
    (= :evaluate outcome)
    (-> (assoc-in [:params :divisor] {:type :number :default 0.0 :bounds [0.0 1.0]})
        (assoc-in [:outputs :dimmer] {:op :divide :args [{:op :param :key :level} {:op :param :key :divisor}]}))
    (= :output outcome)
    (assoc-in [:outputs :aim] {:op :const :value position})))

(defn- with-probes [model heads]
  (reduce (fn [model i]
            (let [id (probe-id i) head (nth heads (mod i (count heads)))]
              (-> model
                  (bench/command [id :definition] {:op :registry/publish :kind :effects
                                                   :definition (probe-effect i head 1 1.0 nil)})
                  (bench/command [id :cue] {:op :cue/put :cue {:id id :effect-id id}}))))
          model (range probes)))

(defn- showcase-rig []
  (let [positions (into {} (map (juxt :id :position)) (rig/fixtures))
        ;; The mover-orbit cue aims the movers above a probe's priority, which
        ;; would hide a probe's refused aim from the renderer: use the beams.
        heads (mapv (fn [id] {:id id :position (get positions id)}) rig/beams)
        cues [:beam-shafts :strobe :can-can :mover-orbit :wash-tide]]
    {:rig :showcase :model (with-probes (showcase/model) heads) :heads heads
     :fixtures (count (rig/fixtures))
     :base-cues (mapv (fn [cue] {:cue-id cue :sources (get looks/sources cue)}) cues)}))

(defn- mixed-rig []
  (let [{:keys [model fixtures]} (bench/corpus 2048 64)
        heads (into [] (comp (filter #(str/starts-with? (:id %) "mover-")) (map #(select-keys % [:id :position])))
                    fixtures)]
    ;; The corpus launches its four effects in the model already.
    {:rig :mixed-2048-64 :model (with-probes model heads) :heads heads :fixtures (count fixtures) :base-cues []
     ;; One more effect launched once per moving head, as spatial actions do.
     :fan-heads heads}))

(defn- request!
  "Submit one payload, tick once, and return the accepted reply."
  [rt clock type payload]
  (let [reply (show/submit! rt type payload)]
    (show/tick! rt (vswap! clock + 25))
    (let [ack (deref reply 5000 {:status :timeout})]
      (when-not (= :accepted (:status ack))
        (throw (ex-info "Benchmark request rejected" {:payload payload :ack ack})))
      ack)))

(defn- launch! [rt clock cue-id sources]
  (let [ack (request! rt clock :command {:op :cue/launch :cue-id cue-id :launch {:reload-policy :preserve-state}})
        instance-id (:instance-id ack)]
    (doseq [[param binding] sources]
      (request! rt clock :modulation-bind {:instance-id instance-id :param param :binding binding}))
    instance-id))

(defn- stage-recorder
  "Wrap the runtime's compiler and record engine, renderer-rejection,
  compile, command-handling and modulation-sampling time for the frame being
  ticked. Commands include validating each published revision; sampling is
  waveform and controller sampling, done for both outcomes while a reload is
  pending."
  [rt]
  (let [frame (volatile! nil)
        depth (volatile! 0)
        add! (fn [k ms] (when @frame (vswap! frame #(-> % (update-in [k :ms] (fnil + 0.0) ms) (update-in [k :calls] (fnil inc 0))))))
        timed (fn [k f] (fn [& args]
                          (let [start (System/nanoTime)
                                result (apply f args)]
                            (add! k (/ (- (System/nanoTime) start) 1e6))
                            (when (and (= k :engine) @frame) (vswap! frame assoc :reload-inputs (:reload-inputs result)))
                            (when (and (= k :reject-output) @frame) (vswap! frame assoc :final-inputs (:reload-inputs result)))
                            result)))]
    {:rt rt
     :frame frame
     :redefs {#'engine/advance (timed :engine engine/advance)
              #'engine/reject-output (timed :reject-output engine/reject-output)
              #'parameter-sources/sample (timed :sampling parameter-sources/sample)
              #'modulation/advance (timed :sampling modulation/advance)
              ;; Outermost command only: a command may apply nested ones.
              #'show/apply-message (let [apply-message @#'show/apply-message
                                         outer (timed :commands apply-message)]
                                     (fn [& args]
                                       (vswap! depth inc)
                                       (try (apply (if (= 1 @depth) outer apply-message) args)
                                            (finally (vswap! depth dec)))))}}))

(defn- spin-until! [deadline-ns]
  ;; A spin, not a sleep: a sleeping owner lets macOS move it to a slower core.
  (while (< (System/nanoTime) deadline-ns)))

(defn- measured-tick!
  "Tick once and hold the 25 ms frame cadence, so the compile worker gets the
  wall time a scheduled show gives it between frames."
  [{:keys [rt frame]} clock]
  (vreset! frame {})
  (let [start (System/nanoTime)]
    (show/tick! rt (vswap! clock + 25))
    (let [ms (/ (- (System/nanoTime) start) 1e6)
          stages @frame]
      (vreset! frame nil)
      (spin-until! (+ start 25000000))
      (assoc stages :ms ms))))

(defn- count-frame-compiles!
  "Count programs the rig compiler builds on the owner thread inside a tick."
  [rt frame owner]
  (swap! (:compiler-context rt) update :compiler
         (fn [compiler]
           (fn [program]
             (when (and @frame (identical? owner (Thread/currentThread)))
               (vswap! frame update :frame-compiles (fnil inc 0)))
             (compiler program)))))

(defn- running [rt effect-id]
  (filter #(= effect-id (:effect-id %)) (vals (get-in @(:state rt) [:model :instances]))))

(defn- running-probe [rt i] (first (running rt (probe-id i))))

(defn- settled?
  "True when every instance of the revision's effect adopted it, or recorded
  its reload fault."
  [rt revision outcome]
  (every? (fn [instance]
            (if (= :accepted outcome)
              (= (:version revision) (:effect-version instance))
              (and (= (:version revision) (get-in instance [:reload-fault :effect-version]))
                   (= outcome (get-in instance [:reload-fault :stage])))))
          (running rt (:id revision))))

(defn- takeover!
  "Publish `revisions` in one owner cycle, then tick until every instance has
  adopted or refused them. The adoption frame is the first that attempted a
  candidate with the reload fallback."
  [{:keys [rt] :as recorder} clock revisions outcome label]
  (let [replies (mapv #(show/submit! rt :definition {:kind :effects :definition %}) revisions)
        frames (loop [frames [(measured-tick! recorder clock)]]
                 (when (= 1 (count frames))
                   (doseq [r replies]
                     (when-not (= :accepted (:status (deref r 5000 nil)))
                       (throw (ex-info "Revision publication was not accepted" {})))))
                 (cond
                   (every? #(settled? rt % outcome) revisions) frames
                   (> (count frames) 400) (throw (ex-info "Takeover did not settle" {:case label}))
                   :else (recur (conj frames (measured-tick! recorder clock)))))
        adoption (first (keep-indexed #(when (:reload-inputs %2) %1) frames))]
    (when-not adoption
      (throw (ex-info "No frame attempted the revisions with a reload fallback" {:case label})))
    {:case label :publication (first frames) :adoption (nth frames adoption) :adoption-frames adoption
     :settle-frames (count frames) :frame-compiles (reduce + (keep :frame-compiles frames))
     :others (into [] (keep-indexed (fn [i f] (when-not (or (zero? i) (= i adoption)) f))) frames)}))

(defn- probe-revisions [rt heads k outcome]
  (let [model (:model @(:state rt))]
    (mapv (fn [i]
            (let [upper (if (= 1.0 (get-in (running-probe rt i) [:definition :params :level :bounds 1])) 0.5 1.0)]
              (probe-effect i (nth heads (mod i (count heads))) (inc (get-in model [:registry :effects (probe-id i) :version]))
                            upper outcome)))
          (range k))))

(defn- fan-effect [heads version upper]
  {:id :fan :version version :name "Fan" :targets (mapv :id heads)
   :params {:level {:type :number :default 0.5 :bounds [0.0 upper]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- fan-revision [rt heads]
  (let [model (:model @(:state rt))
        upper (if (= 1.0 (get-in (first (running rt :fan)) [:definition :params :level :bounds 1])) 0.5 1.0)]
    [(fan-effect heads (inc (get-in model [:registry :effects :fan :version])) upper)]))

(defn- burst-effect [heads version]
  {:id :burst :version version :name "Burst" :targets (mapv :id heads)
   :params {:level {:type :number :default (if (odd? version) 0.9 0.8) :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- burst!
  "Publish a revision of the burst effect, and in the same owner cycle
  schedule one launch of it per head for the next beat, each with its own
  targets, as firing a Live scene of per-fixture clips does. Tick until every
  launch has rendered. Launch latency counts frames from the frame that
  crosses the scheduled beat to the frame where all launches first render."
  [{:keys [rt] :as recorder} clock heads]
  (let [model (:model @(:state rt))
        version (inc (get-in model [:registry :effects :burst :version]))
        publication (show/submit! rt :definition {:kind :effects :definition (burst-effect heads version)})
        replies (mapv (fn [[i fixture]]
                        (show/submit! rt :command {:op :cue/launch :cue-id :burst :grid {:beats 1}
                                                   :command-id (str "burst-" version "-" i)
                                                   :source {:adapter :repl :session-id "bench"}
                                                   :launch {:mode :one-shot :duration {:beats 1/2} :targets [(:id fixture)]}}))
                      (map-indexed vector heads))
        rendered #(count (filter (fn [instance] (and (= :burst (:effect-id instance)) (= version (:effect-version instance))
                                                     (get-in instance [:system :last-frame-number])))
                                 (vals (get-in @(:state rt) [:model :instances]))))
        frames (loop [frames []]
                 (let [observed (assoc (measured-tick! recorder clock)
                                       :beat (get-in @(:state rt) [:model :transport :beat]) :rendered (rendered))]
                   (when (empty? frames)
                     (when-not (= :accepted (:status (deref publication 5000 nil)))
                       (throw (ex-info "Burst revision was not accepted" {}))))
                   (cond
                     (= (count heads) (:rendered observed)) (conj frames observed)
                     (> (count frames) 200) (throw (ex-info "Burst did not render" {}))
                     :else (recur (conj frames observed)))))
        due (some #(:due-beat (deref % 5000 nil)) replies)
        beat-frame (first (keep-indexed #(when (>= (:beat %2) due) %1) frames))
        landed (dec (count frames))]
    {:case [:grid-burst (count heads)] :publication (first frames) :beat-frame (nth frames beat-frame)
     :launch-latency-frames (- landed beat-frame)
     :frame-compiles (reduce + (keep :frame-compiles frames))}))

(defn- summary [observations]
  (let [stage (fn [k] (let [xs (keep #(get-in % [k :ms]) observations)] (when (seq xs) (bench/distribution xs))))]
    (cond-> {:n (count observations) :tick-ms (bench/distribution (map :ms observations))
             :over-25-ms (count (filter #(> (:ms %) 25.0) observations))
             :engine-ms (stage :engine)
             :frame-compiles (bench/distribution (map #(:frame-compiles % 0) observations))}
      (stage :commands) (assoc :commands-ms (stage :commands))
      (stage :sampling) (assoc :sampling-ms (stage :sampling))
      (stage :reject-output) (assoc :reject-output-ms (stage :reject-output))
      (some :reload-inputs observations) (assoc :reload-inputs (frequencies (map #(or (:final-inputs %) (:reload-inputs %)) observations))))))

(defn- load-average []
  (.getSystemLoadAverage (java.lang.management.ManagementFactory/getOperatingSystemMXBean)))

(defn profile
  "Ordinary frames, then `rounds` takeover rounds for each case. `mode` is
  :background (compiles off the frame) or :inline (compiles in the frame, the
  behaviour before #774)."
  [{:keys [model heads base-cues fan-heads] :as rig} rounds mode]
  (let [rt (show/create-runtime {:model (cond-> model
                                          fan-heads (-> (bench/command [:fan :definition]
                                                                       {:op :registry/publish :kind :effects
                                                                        :definition (fan-effect fan-heads 1 1.0)})
                                                        (bench/command [:burst :definition]
                                                                       {:op :registry/publish :kind :effects
                                                                        :definition (burst-effect fan-heads 1)})
                                                        (bench/command [:burst :cue] {:op :cue/put :cue {:id :burst :effect-id :burst}})))
                                 :workers 2 :sink (bench/capture-sink) :background-compile (= :background mode)
                                 :workspace-options nil :effect-log! (fn [_]) :worker-timeout-ms 60000})
        clock (volatile! 0)
        {:keys [redefs frame] :as recorder} (stage-recorder rt)
        cases (concat (for [k reloading outcome outcomes] {:label [k outcome] :outcome outcome :revise #(probe-revisions rt heads k outcome)})
                      (when fan-heads [{:label [:per-fixture (count fan-heads)] :outcome :accepted :revise #(fan-revision rt fan-heads)}]))]
    (try
      (count-frame-compiles! rt frame (Thread/currentThread))
      (doseq [{:keys [cue-id sources]} base-cues] (launch! rt clock cue-id sources))
      (doseq [i (range probes)] (launch! rt clock (probe-id i) {:level level-binding}))
      (doseq [fixture fan-heads]
        (let [ack (request! rt clock :command {:op :effect/launch :effect-id :fan
                                                :launch {:reload-policy :preserve-state :targets [(:id fixture)]}})]
          (request! rt clock :modulation-bind {:instance-id (:instance-id ack) :param :level :binding level-binding})))
      (with-redefs-fn redefs
        (fn []
          (dotimes [_ 200] (measured-tick! recorder clock))
          ;; One untimed round of every case compiles the code paths first.
          (doseq [{:keys [label outcome revise]} cases]
            (takeover! recorder clock (revise) outcome label)
            (dotimes [_ 8] (measured-tick! recorder clock)))
          (when fan-heads
            (burst! recorder clock fan-heads)
            (dotimes [_ 30] (measured-tick! recorder clock)))
          (let [gc-before (bench/gc-snapshot)
                ordinary (mapv (fn [_] (measured-tick! recorder clock)) (range 240))
                takeovers (vec (for [_ (range rounds) {:keys [label outcome revise]} cases]
                                 (let [observed (takeover! recorder clock (revise) outcome label)]
                                   (dotimes [_ 8] (measured-tick! recorder clock))
                                   observed)))
                bursts (when fan-heads
                         (vec (for [_ (range rounds)]
                                (let [observed (burst! recorder clock fan-heads)]
                                  (dotimes [_ 30] (measured-tick! recorder clock))
                                  observed))))
                gc-after (bench/gc-snapshot)
                status (:status @(:state rt))]
            (when (:error status) (throw (ex-info "Show error during the benchmark" {:error (:error status)})))
            {:rig (:rig rig) :mode mode :fixtures (:fixtures rig) :probes probes
             :instances (count (get-in @(:state rt) [:model :instances]))
             :rounds rounds
             :ordinary (summary ordinary)
             :takeover (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                             (for [[label xs] (group-by :case takeovers)]
                               [label (cond-> {:publication-frame (summary (map :publication xs))
                                               :adoption-frame (summary (map :adoption xs))
                                               :adoption-latency-frames (bench/distribution (map :adoption-frames xs))
                                               :frames-to-settle (bench/distribution (map :settle-frames xs))
                                               :compiles-in-frames-per-takeover (bench/distribution (map :frame-compiles xs))}
                                        (seq (mapcat :others xs)) (assoc :other-frames (summary (mapcat :others xs))))]))
             :grid-burst (when (seq bursts)
                           {:heads (count fan-heads)
                            :launch-latency-frames (bench/distribution (map :launch-latency-frames bursts))
                            :publication-frame (summary (map :publication bursts))
                            :beat-frame (summary (map :beat-frame bursts))
                            :compiles-in-frames-per-burst (bench/distribution (map :frame-compiles bursts))})
             :gc {:before gc-before :after gc-after} :load-average-after (load-average)})))
      (finally (show/stop! rt)))))

(defn -main [& args]
  (let [rounds (or (some-> (first args) parse-long) 20)
        words (map keyword (rest args))
        rigs (or (seq (filter #{:showcase :mixed} words)) [:showcase :mixed])
        modes (or (seq (filter #{:inline :background} words)) [:inline :background])
        build {:showcase showcase-rig :mixed mixed-rig}]
    (pprint/pprint {:benchmark :reload-takeover :measured-at (str (java.time.Instant/now))
                    :source-revisions {:tolglow (bench/revision ".")
                                       :afterglow (bench/revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))}
                    :environment {:java (System/getProperty "java.version") :cpu-model (bench/cpu-model)
                                  :processors (.availableProcessors (Runtime/getRuntime))
                                  :heap-bytes (.maxMemory (Runtime/getRuntime))
                                  :load-average (load-average)}})
    (doseq [rig rigs mode modes]
      (binding [pprint/*print-right-margin* 160]
        (pprint/pprint (profile ((get build rig)) rounds mode))))))
