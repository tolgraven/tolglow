(ns tolglow.bench-reload-takeover
  "Owner tick cost on hot-reload takeover frames that carry the engine's reload
  fallback (#663, #774), against ordinary frames of the same show.

  Every probe cue has a waveform bound to its level, so a revision that changes
  the level's bounds makes the candidate and last-good samples differ and the
  owner supplies `::engine/reload-fallback`. Each takeover round publishes a
  revision of 1, 8 or 32 probes in one owner cycle, whose outcome is:

  - :accepted  the engine adopts every candidate (trial pass, then the rest);
  - :evaluate  every candidate divides by zero (trial pass, then every
               instance again on the fallback inputs);
  - :output    every candidate aims its head at the head's own position, which
               the renderer refuses after the engine accepted it (a whole
               evaluation and render, then the fallback pass and a second
               render).

  Rigs: the showcase show with five cues and their authored waveforms, and the
  mixed 2,048-pixel/64-head corpus of `tolglow.bench-show`. Logical 25 ms clock,
  capture sink, two effect workers. No network output or devices.

    bin/repl run tolglow.bench-reload-takeover [rounds] [rig ...]
    lein with-profile +dev run -m tolglow.bench-reload-takeover 20 showcase mixed

  Stage timings come from wrapping `engine/advance`, `engine/reject-output` and
  the runtime's own `:compile` function during the run; they are inside the
  measured tick."
  (:require [afterglow.runtime.engine :as engine]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [tolglow.bench-show :as bench]
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
    {:rig :mixed-2048-64 :model (with-probes model heads) :heads heads :fixtures (count fixtures) :base-cues []}))

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
  "Wrap the runtime's compiler and record engine, renderer-rejection and
  compile time for the frame being ticked."
  [rt]
  (let [frame (volatile! nil)
        add! (fn [k ms] (when @frame (vswap! frame #(-> % (update-in [k :ms] (fnil + 0.0) ms) (update-in [k :calls] (fnil inc 0))))))
        timed (fn [k f] (fn [& args]
                          (let [start (System/nanoTime)
                                result (apply f args)]
                            (add! k (/ (- (System/nanoTime) start) 1e6))
                            (when (and (= k :engine) @frame) (vswap! frame assoc :reload-inputs (:reload-inputs result)))
                            (when (and (= k :reject-output) @frame) (vswap! frame assoc :final-inputs (:reload-inputs result)))
                            result)))]
    {:rt (update rt :compile #(timed :compile %))
     :frame frame
     :redefs {#'engine/advance (timed :engine engine/advance)
              #'engine/reject-output (timed :reject-output engine/reject-output)}}))

(defn- measured-tick! [{:keys [rt frame]} clock]
  (vreset! frame {})
  (let [start (System/nanoTime)]
    (show/tick! rt (vswap! clock + 25))
    (let [ms (/ (- (System/nanoTime) start) 1e6)
          stages @frame]
      (vreset! frame nil)
      (assoc stages :ms ms))))

(defn- running-probe [rt i]
  (let [model (:model @(:state rt))]
    (some #(when (= (probe-id i) (:cue-id %)) %) (vals (:instances model)))))

(defn- settled?
  "True when probe `i` has adopted `revision`, or recorded its reload fault."
  [rt i revision outcome]
  (let [probe (running-probe rt i)]
    (if (= :accepted outcome)
      (= (:version revision) (:effect-version probe))
      (and (= (:version revision) (get-in probe [:reload-fault :effect-version]))
           (= outcome (get-in probe [:reload-fault :stage]))))))

(defn- takeover!
  "Publish a revision of the first `k` probes in one owner cycle, then tick
  until every probe has adopted or refused it. Returns the takeover frame with
  the frames that followed until then."
  [{:keys [rt] :as recorder} clock heads k outcome]
  (let [model (:model @(:state rt))
        revisions (mapv (fn [i]
                          (let [running (running-probe rt i)
                                upper (if (= 1.0 (get-in running [:definition :params :level :bounds 1])) 0.5 1.0)
                                version (inc (get-in model [:registry :effects (probe-id i) :version]))]
                            (probe-effect i (nth heads (mod i (count heads))) version upper outcome)))
                        (range k))
        replies (mapv #(show/submit! rt :definition {:kind :effects :definition %}) revisions)
        observed (measured-tick! recorder clock)
        _ (doseq [r replies]
            (when-not (= :accepted (:status (deref r 5000 nil)))
              (throw (ex-info "Revision publication was not accepted" {}))))
        pending #(remove (fn [i] (settled? rt i (nth revisions i) outcome)) (range k))
        follow-up (loop [frames []]
                    (cond
                      (empty? (pending)) frames
                      (> (count frames) (+ k 8))
                      (throw (ex-info "Takeover did not settle" {:outcome outcome :k k :pending (vec (pending))}))
                      :else (recur (conj frames (measured-tick! recorder clock)))))]
    (when-not (:reload-inputs observed)
      (throw (ex-info "The takeover frame carried no reload fallback" {:outcome outcome :k k})))
    (assoc observed :outcome outcome :reloading k :follow-up follow-up)))

(defn- summary [observations]
  (let [stage (fn [k] (let [xs (keep #(get-in % [k :ms]) observations)] (when (seq xs) (bench/distribution xs))))]
    (cond-> {:n (count observations) :tick-ms (bench/distribution (map :ms observations))
             :over-25-ms (count (filter #(> (:ms %) 25.0) observations))
             :engine-ms (stage :engine)}
      (stage :compile) (assoc :compile-ms (stage :compile)
                              :compiled-per-frame (bench/distribution (map #(get-in % [:compile :calls] 0) observations)))
      (stage :reject-output) (assoc :reject-output-ms (stage :reject-output))
      (some :reload-inputs observations) (assoc :reload-inputs (frequencies (map #(or (:final-inputs %) (:reload-inputs %)) observations))))))

(defn profile
  "Ordinary frames, then `rounds` takeover rounds for each reload count and outcome."
  [{:keys [model heads base-cues] :as rig} rounds]
  (let [rt (show/create-runtime {:model model :workers 2 :sink (bench/capture-sink)
                                 :workspace-options nil :effect-log! (fn [_]) :worker-timeout-ms 60000})
        clock (volatile! 0)
        {:keys [redefs] :as recorder} (stage-recorder rt)
        recorder (assoc recorder :rt (:rt recorder))]
    (try
      (doseq [{:keys [cue-id sources]} base-cues] (launch! rt clock cue-id sources))
      (doseq [i (range probes)] (launch! rt clock (probe-id i) {:level level-binding}))
      (with-redefs-fn redefs
        (fn []
          (dotimes [_ 200] (measured-tick! recorder clock))
          ;; One untimed round of every case compiles the code paths first.
          (doseq [k reloading outcome outcomes]
            (takeover! recorder clock heads k outcome)
            (dotimes [_ 8] (measured-tick! recorder clock)))
          (let [gc-before (bench/gc-snapshot)
                ordinary (mapv (fn [_] (measured-tick! recorder clock)) (range 240))
                takeovers (vec (for [_ (range rounds) k reloading outcome outcomes]
                                 (let [observed (takeover! recorder clock heads k outcome)
                                       after (mapv (fn [_] (measured-tick! recorder clock)) (range 8))]
                                   (assoc observed :next-ms (:ms (first after))))))
                gc-after (bench/gc-snapshot)
                status (:status @(:state rt))]
            (when (:error status) (throw (ex-info "Show error during the benchmark" {:error (:error status)})))
            {:rig (:rig rig) :fixtures (:fixtures rig) :probes probes
             :instances (count (get-in @(:state rt) [:model :instances]))
             :rounds rounds
             :ordinary (summary ordinary)
             :takeover (into (sorted-map)
                             (for [[[k outcome] xs] (group-by (juxt :reloading :outcome) takeovers)]
                               [[k outcome]
                                (let [follow-up (mapcat :follow-up xs)]
                                  (cond-> (assoc (summary xs)
                                                 :next-frame-ms (bench/distribution (map :next-ms xs))
                                                 :frames-to-settle (bench/distribution (map #(inc (count (:follow-up %))) xs)))
                                    (seq follow-up) (assoc :follow-up-frames (summary follow-up))))]))
             :gc {:before gc-before :after gc-after}})))
      (finally (show/stop! rt)))))

(defn -main [& args]
  (let [rounds (or (some-> (first args) parse-long) 20)
        rigs (or (seq (map keyword (rest args))) [:showcase :mixed])
        build {:showcase showcase-rig :mixed mixed-rig}]
    (pprint/pprint {:benchmark :reload-takeover :measured-at (str (java.time.Instant/now))
                    :source-revisions {:tolglow (bench/revision ".")
                                       :afterglow (bench/revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))}
                    :environment {:java (System/getProperty "java.version") :cpu-model (bench/cpu-model)
                                  :processors (.availableProcessors (Runtime/getRuntime))
                                  :heap-bytes (.maxMemory (Runtime/getRuntime))}})
    (doseq [rig rigs]
      (binding [pprint/*print-right-margin* 160]
        (pprint/pprint (profile ((get build rig)) rounds))))))
