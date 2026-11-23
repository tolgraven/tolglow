(ns tolglow.perf.independent-effects
  "Offline comparison of worker dispatch for independent declarative lighting.
  Production scheduling is unchanged. Every frame is checked against serial evaluation."
  (:require [afterglow.control.model :as control]
            [afterglow.runtime.executor :as executor]
            [clojure.set :as set]
            [tolglow.bench-show :as bench]
            [tolglow.show :as show])
  (:import [java.security MessageDigest]
           [java.util Arrays HexFormat]))

(def ^:private lanes [:serial :per-effect :chunked])

(defn- install [model definition]
  (let [id (:id definition)]
    (-> model
        (bench/command [id :effect] {:op :registry/publish :kind :effects :definition (assoc definition :version 1)})
        (bench/command [id :cue] {:op :cue/put :cue {:id id :effect-id id}})
        (bench/command [id :start] {:op :cue/launch :cue-id id}))))

(defn- program [kind n targets]
  (let [phase {:op :multiply :args [0.125 {:op :beat}]}
        signal {:op :add :args [0.5 {:op :multiply :args [0.45 {:op :sin :args [{:op :add :args [phase n]}]}]}]}]
    {:id (keyword (str (name kind) "-" n)) :targets (mapv :id targets)
     :outputs (case kind
                :wash {:color {:op :vec3 :args [signal 0.25 0.4]}}
                :move {:aim {:op :vec3 :args [signal 0.75 1.8]}})}))

(defn- programs [fixtures pixels movers scenario]
  (let [segments (fn [kind xs n] (map-indexed #(program kind %1 %2) (partition-all n xs)))
        washes (vec (segments :wash (subvec fixtures 0 pixels) (max 1 (quot pixels 32))))
        moving (segments :move (subvec fixtures pixels) (max 1 (quot movers 16)))
        base (into washes moving)
        ids (:targets (first washes))]
    (case scenario
      :disjoint base
      :mixed (into base
                   [{:id :same-heads-dimmer :targets ids :outputs {:dimmer 0.7}}
                    {:id :overlap :targets ids :outputs {:color [0.95 0.15 0.2]}}
                    {:id :previous-layer :targets ids :outputs {}
                     :layer {:targets :previous :transforms
                             [{:attribute :color :value {:op :rotate-hue :args [{:op :field :attribute :color} 1/8]}}]}}
                    {:id :fault :targets [(first ids)] :outputs {:focus 0.5}}]))))

(defn- footprints [definitions]
  ;; This controlled corpus has static targets and map outputs. Treat arbitrary
  ;; effect code and previous-layer readers as barriers. A common beat or other
  ;; immutable frame input alone creates no dependency between workers.
  (into {} (map (fn [definition]
                  [[(:id definition) 1]
                   (when (and (:targets definition) (map? (:outputs definition))
                              (nil? (:layer definition)) (not= :fault (:id definition)))
                     {:reads #{[:transport :beat]}
                      :writes (set (for [target (:targets definition) attr (keys (:outputs definition))]
                                     [target attr]))})])) definitions))

(defn- finish-phase [groups phase] (cond-> groups (seq phase) (conj phase)))

(defn- conflict? [seen current]
  (or (some (:writes seen) (:writes current))
      (some (:writes seen) (:reads current))
      (some (:reads seen) (:writes current))))

(def ^:private empty-access {:reads #{} :writes #{}})

(defn phases
  "Preserve write/write, read/write and write/read dependencies between jobs.
  A missing footprint, a dynamic selector or a native-layer membership fences
  both sides of its job."
  [jobs known]
  (loop [remaining (seq jobs) phase [] accessed empty-access groups []]
    (if-let [job (first remaining)]
      (let [program (:program job)
            fields (when-not (or (:candidate? job) (get-in job [:instance :source :native-layer])
                                 (contains? (:instance job) :targets))
                     (known [(:id program) (:version program)]))]
        (cond
          (nil? fields) (recur (next remaining) [] empty-access (conj (finish-phase groups phase) [job]))
          (conflict? accessed fields) (recur (next remaining) [job] fields (finish-phase groups phase))
          :else (recur (next remaining) (conj phase job) (merge-with set/union accessed fields) groups)))
      (finish-phase groups phase))))

(defn- work-groups [worker-count jobs known]
  (mapv (fn [phase]
          (let [size (max 1 (long (Math/ceil (/ (count phase) (double worker-count)))))]
            (mapv vec (partition-all size phase)))) (phases jobs known)))

(defn- dispatch [lane pool known cache job-count f jobs]
  (case lane
    :serial (mapv f jobs)
    :per-effect (do (vswap! job-count + (count jobs)) (executor/map-work pool f jobs))
    :chunked
    ;; Memoize indices only. Each frame owns new state and lifecycle objects.
    (let [signature (mapv (fn [{:keys [instance program candidate?]}]
                            [(:id instance) (:id program) (:version program) candidate?
                             (boolean (get-in instance [:source :native-layer])) (contains? instance :targets)]) jobs)
          groups (if (= signature (:signature @cache)) (:groups @cache)
                     (let [indexed (mapv #(assoc %1 ::index %2) jobs (range))
                           groups (mapv #(mapv (partial mapv ::index) %) (work-groups 2 indexed known))]
                       (vreset! cache {:signature signature :groups groups})
                       groups))]
      (reduce (fn [out chunks]
                (vswap! job-count + (count chunks))
                (into out cat (executor/map-work pool #(mapv (fn [index] (f (nth jobs index))) %) chunks)))
              [] groups))))

(defn- context [scene lane]
  (let [sink (bench/capture-sink)
        base (show/create-runtime {:model scene :workspace-options nil :workers 2 :worker-timeout-ms 5000
                                   :adaptive-keyframes {:enabled? false} :effect-log! (fn [_])
                                   :sink sink})
        memory (volatile! nil) jobs (volatile! 0) events (volatile! [])
        original (:compile base)
        compiled (fn [definition]
                   (if (= :fault (:id definition))
                     (fn [_] (throw (ex-info "Injected independent-effect failure" {})))
                     (original definition)))]
    (show/attach-observer! base ::ordered-events
      (fn [] {:events! (fn [_ batch _ _] (vreset! events batch))
              :tracing? (constantly false) :frame! (fn [_]) :close! (fn [])}))
    {:runtime (assoc base :compile compiled) :sink sink :events events :cache memory :jobs jobs :lane lane}))

(defn- snapshot [{:keys [runtime sink events]}]
  (let [{:keys [model status]} @(:state runtime)]
    (when (or (:error status) (:output-error status) (empty? @(:latest sink)))
      (throw (ex-info "Incomplete worker-batch frame" {:status status})))
    {:packets @(:latest sink) :master (:master model) :transport (:transport model)
     :instances (update-vals (:instances model)
                             #(-> (select-keys % [:id :cue-id :phase :state :params :system :targets :fault
                                                 :started-at-beat :started-at-ms :release-at-ms :effect-version])
                                  (update :fault select-keys [:stage :effect-id :message :exception-class])))
     :events (mapv #(update % :data dissoc :causes :trace :stacktrace) @events)
     :intents (mapv #(dissoc % :causes :trace :stacktrace) (:last-intents status))}))

(defn- same-frame? [a b]
  (and (= (dissoc a :packets) (dissoc b :packets)) (= (count (:packets a)) (count (:packets b)))
       (every? true? (map #(Arrays/equals ^bytes %1 ^bytes %2) (:packets a) (:packets b)))))

(defn- transitions! [runtime n]
  (case n
    4 (show/submit! runtime {:op :master/set :value 0.55 :command-id :budget-change})
    9 (when-let [id (some (fn [[id inst]] (when (= :wash-0 (:cue-id inst)) id))
                           (get-in @(:state runtime) [:model :instances]))]
        (show/submit! runtime {:op :instance/end :instance-id id :force? true :command-id :end-stripe}))
    12 (show/submit! runtime {:op :cue/launch :cue-id :wash-0 :command-id :launch-stripe})
    nil))

(defn- frame! [cpu known {:keys [runtime lane cache jobs] :as context} number event-frame]
  (vreset! jobs 0)
  (transitions! runtime event-frame)
  (let [dispatch-ns (volatile! 0)
        mapper (fn [pool]
                 (fn [f work]
                   (let [start (System/nanoTime)
                         result (dispatch lane pool known cache jobs f work)]
                     (vreset! dispatch-ns (- (System/nanoTime) start))
                     result)))]
    (with-redefs [executor/mapper mapper]
      (let [before (when cpu (cpu)) start (System/nanoTime)]
        (show/tick! runtime (* 25.0 number))
        (let [wall (/ (- (System/nanoTime) start) 1e6)
              cpu-ms (when cpu (/ (- (cpu) before) 1e6))]
          {:wall-ms wall :cpu-ms cpu-ms :dispatch-ms (/ @dispatch-ns 1e6)
           :tasks @jobs :output (snapshot context)})))))

(defn- summarize [rows]
  {:complete-within-25-ms (count (filter #(<= (:wall-ms %) 25) rows)) :frames (count rows)
   :owner-and-worker-cpu-ms (when (every? :cpu-ms rows) (bench/distribution (map :cpu-ms rows)))
   :worker-dispatch-wall-ms (bench/distribution (map :dispatch-ms rows))
   :whole-frame-ms (bench/distribution (map :wall-ms rows))
   :tasks-per-frame (bench/distribution (map :tasks rows))})

(defn- replay-frame [contexts known counters n warmup]
  (let [order (take (count lanes) (drop (mod n (count lanes)) (cycle lanes)))
        cpu (when (>= n warmup) (:cpu @counters))
        capture (fn [lane] [lane (frame! cpu known (contexts lane) n (- n warmup))])
        results (into {} (map capture) order)
        reference (get-in results [:serial :output])]
    (doseq [[lane row] results]
      (when-not (same-frame? reference (:output row))
        (throw (ex-info "Effect batching differs from serial ArtDMX, state or events" {:frame n :lane lane}))))
    [reference (update-vals results #(dissoc % :output))]))

(defn- record-frame [history ^MessageDigest digest n warmup [output metrics]]
  (let [{:keys [instances events packets intents]} output
        active (count instances)
        event-types (mapv :type events)
        counted (-> history
                    (update :min-active min active)
                    (update :max-active max active)
                    (update :events #(merge-with + % (frequencies event-types)))
                    (update :intents #(merge-with + % (frequencies (map :type intents)))))]
    (doseq [^bytes packet packets] (.update digest packet))
    (if (< n warmup) counted
      (let [^bytes first-universe (first packets)
            ;; ArtDMX has an 18-byte header before the first patched RGBW pixel.
            swatch (mapv #(bit-and 255 (aget first-universe %)) (range 18 22))
            trace {:sample (- n warmup) :time-ms (* n 25) :active active
                   :pixel-rgbw swatch :events event-types :lanes metrics}]
        (-> counted
            (update :trace conj trace)
            (update :rows #(reduce-kv (fn [m lane timing] (update m lane (fnil conj []) timing)) % metrics)))))))

(defn- run-case [fixtures pixels movers scenario warmup frames]
  (let [definitions (programs fixtures pixels movers scenario)
        scene (reduce install (control/new-show {:show-id :dispatch :rig {:fixtures fixtures}}) definitions)
        known (footprints definitions)
        contexts (into {} (map (juxt identity #(context scene %))) lanes)
        counters (delay (bench/worker-counters))
        digest (MessageDigest/getInstance "SHA-256")
        initial {:min-active (count definitions) :max-active 0 :intents {} :events {} :trace [] :rows {}}]
    (try
      (let [step (fn [history n] (record-frame history digest n warmup (replay-frame contexts known counters n warmup)))
            {:keys [min-active max-active intents events trace rows]} (reduce step initial (range (+ warmup frames)))]
        (when (or (contains? events nil) (and (>= frames 13) (not= 3 (:command/accepted events))))
          (throw (ex-info "Queued controls or their ordered events were lost" {:events events})))
        (when (and (= scenario :mixed) (not= 1 (:effect/failed intents)))
          (throw (ex-info "Expected one quarantined failure in the mixed scene" {:intents intents})))
        {:scenario scenario :effects (count definitions) :active-range [min-active max-active]
         :serialized-packets-sha256 (.formatHex (HexFormat/of) (.digest digest))
         :intents intents :ordered-events events :trace trace :lanes (update-vals rows summarize)})
      (finally (run! #(show/stop! (:runtime %)) (vals contexts))))))

(defn -main [path & [warmup frames]]
  (let [pixels 1024 movers 128 fixtures (:fixtures (bench/corpus pixels movers :sculpture))
        warmup (parse-long (or warmup "40")) frames (parse-long (or frames "120"))
        result {:tolglow (bench/revision ".") :afterglow (bench/revision (System/getenv "AFTERGLOW_ROOT"))
                :cpu (bench/cpu-model) :java (System/getProperty "java.version")
                :pixels pixels :movers movers :workers 2 :warmup warmup :frames frames
                :runs (mapv #(run-case fixtures pixels movers % warmup frames) [:disjoint :mixed])}]
    (spit path (pr-str result))
    (prn (update result :runs #(mapv (fn [row] (dissoc row :trace)) %)))))
