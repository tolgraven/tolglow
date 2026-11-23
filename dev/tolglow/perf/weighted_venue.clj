(ns tolglow.perf.weighted-venue
  "Compare spatially indexed fields with a scalar venue scan and captured output."
  (:require [afterglow.render.frame :as render]
            [afterglow.spatial :as spatial]
            [afterglow.spatial.field :as field]
            [clojure.java.io :as io]
            [tolglow.bench-show :as bench]
            [tolglow.rig.surfaces :as surfaces]
            [tolglow.show :as show])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]))

(def ^:private centers [[3.5 4.0 2.5] [3.0 4.5 -2.0] [-2.5 3.0 -1.5] [-2.0 4.0 3.5]])

(defn volumes [count-fields selection beat]
  (mapv (fn [i [x y z]]
          {:shape :sphere
           :position [(+ x (Math/sin (+ beat i))) y (+ z (Math/cos (+ beat i)))]
           :radius (+ 4.5 (* 0.2 i))
           :strength 0.8
           :falloff {:inner-m 0.5 :outer-m 1.0 :curve :smoothstep}
           :selection selection})
        (range) (take count-fields centers)))

(defn- reference-weight
  "Analytic smoothstep of a sphere. Kept independent of Afterglow's SDF and index."
  [{[cx cy cz] :position :keys [radius]} [x y z]]
  (let [dx (- (double x) (double cx))
        dy (- (double y) (double cy))
        dz (- (double z) (double cz))
        distance (- (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))) (double radius))
        t (max 0.0 (min 1.0 (/ (- 1.0 distance) 1.5)))]
    (* 0.8 t t (- 3.0 (* 2.0 t)))))

(defn- verify-sphere! [volume fixtures weights]
  (doseq [{:keys [id position]} fixtures]
    (let [expected (reference-weight volume position)
          actual (double (get weights id 0.0))]
      (when-not (< (Math/abs (- (double expected) actual)) 1.0e-10)
        (throw (ex-info "Indexed volume disagrees with scalar sphere" {:id id :expected expected :actual actual}))))))

(def ^:private ^ThreadMXBean metrics (ManagementFactory/getThreadMXBean))

(defn- elapsed [f]
  (let [id (.getId (Thread/currentThread))
        bytes (.getThreadAllocatedBytes metrics id)
        cpu (.getCurrentThreadCpuTime metrics)
        start (System/nanoTime)
        value (f)
        end (System/nanoTime)]
    {:ms (/ (- end start) 1e6)
     :cpu-ms (/ (- (.getCurrentThreadCpuTime metrics) cpu) 1e6)
     :owner-bytes (- (.getThreadAllocatedBytes metrics id) bytes)
     :value value}))

(defn- aggregate [observations]
  (into {} (for [k (keys (first observations))]
             [k (bench/distribution (map k observations))])))

(defn position-case [fixtures n warmup samples]
  (let [rig (spatial/prepare-rig fixtures)
        step (fn [i]
               (let [shapes (volumes n #{:position} (/ i 20.0))
                     {:keys [value] :as measured}
                     (elapsed #(mapv (fn [shape] (:weights (field/positions shape rig))) shapes))]
                 (dorun (map #(verify-sphere! %1 fixtures %2) shapes value))
                 (assoc (dissoc measured :value) :positive (reduce + (map count value)))))]
    (dotimes [i warmup] (step i))
    (let [observations (mapv #(step (+ warmup %)) (range samples))]
      {:scope :position :heads (count fixtures) :fields n
       :summary (aggregate observations) :samples observations})))

(def ^:private paint [[1.0 0.1 0.2] [0.2 1.0 0.5] [0.12 0.4 1.0] [1.0 0.8 0.1]])

(defn field-id [i] (str "venue-field-" i))

(defn- new-show [model scalar?]
  (let [sink (bench/capture-sink)
        clock (volatile! 0.0)
        runtime (show/create-runtime (cond-> {:model model :workers (if scalar? 1 2)
                                               :batch? (not scalar?) :workspace-options nil :sink sink
                                               :independent-output? (not scalar?)
                                               :physical-now-ms #(deref clock)}
                                       scalar? (assoc :worker-timeout-ms 10000)))]
    {:runtime runtime :clock clock :sink sink}))

(defn- accepted! [receipts]
  (doseq [r receipts]
    (when-not (= :accepted (:status @r))
      (throw (ex-info "Show rejected benchmark command" {:ack @r})))))

(defn seed-model
  "Launch the weighted volumes in their four grid cells and return the prepared show."
  [model shapes]
  (let [{:keys [runtime]} (new-show model false)]
    (try
      (let [spawn (fn [i shape]
                    (show/submit! runtime :spatial-cue
                                  {:op :spawn :id (field-id i) :page-id :benchmark :cell [i 0]
                                   :volume shape :paint {:color (paint i)} :priority (+ 10 i)}))
            receipts (mapv spawn (range) shapes)]
        (show/tick! runtime 0)
        (accepted! receipts)
        (:model @(:state runtime)))
      (finally (show/stop! runtime)))))

(defn- host-step [{:keys [runtime sink clock]} ids indices i]
  (let [shapes (volumes (count ids) #{:position} (/ i 20.0))
        move (fn [id volume]
               (show/submit! runtime {:op :param/set :param :position :instance-id id
                                      :command-id (str "move-" id "-" i) :value (:position volume)}))
        replies (mapv move ids shapes)
        frame (bench/projected-host-step! runtime sink clock indices i)]
    (accepted! replies)
    (doseq [[id instance] (get-in frame [:model :instances])]
      (when (:fault instance)
        (throw (ex-info "Effect failed during volume run" {:id id :fault (:fault instance)}))))
    frame))

(defn- cue-members [spatial instances id]
  (let [key (str "spatial-cue/" (get-in instances [id :order]))
        members (get-in spatial [:members key])
        reported (get-in spatial [:signals key :affected-head-count])]
    (when-not (and (set? members) (= (count members) reported))
      (throw (ex-info "Running cue did not report its weighted heads" {:id id})))
    members))

(defn- beam-additions [fixtures beam? shape members]
  (letfn [(check [extra {:keys [id position channels]}]
            (let [origin? (pos? (reference-weight shape position))
                  present? (contains? members id)]
              (cond
                (= origin? present?) extra
                (and beam? present?) (update extra (if (:pan channels) :movers :pixels) inc)
                :else (throw (ex-info "Published members disagree with sphere coverage"
                                      {:id id :origin? origin? :present? present?})))))]
    (reduce check {:movers 0 :pixels 0} fixtures)))

(defn- coverage [model ids index beam?]
  (let [fixtures (get-in model [:rig :fixtures])
        spatial (get-in model [:variables :spatial])
        shapes (volumes (count ids) #{:position} (/ index 20.0))
        fields (mapv (partial cue-members spatial (:instances model)) ids)
        counts (frequencies (mapcat seq fields))
        inspect (partial beam-additions fixtures beam?)
        additions (reduce (partial merge-with +) (map inspect shapes fields))]
    {:positive-pairs (reduce + (map count fields))
     :distinct-heads (count counts)
     :overlapping-heads (count (filter #(< 1 %) (vals counts)))
     :beam-movers (:movers additions) :beam-pixels (:pixels additions)}))

(defn- expected-frames [model step n inspect]
  ;; Check full state and every byte. Keep only packets, intents and small visible
  ;; projections across ticks so a reference run does not retain dense field maps.
  (let [scalar (new-show model true)]
    (try
      (let [indexed (new-show model false)]
        (try
          (mapv (fn [i]
                  (let [reference (step scalar i)
                        candidate (step indexed i)]
                    (bench/check! reference candidate)
                    (assoc (dissoc reference :model) :coverage (inspect (:model reference) i))))
                (range n))
          (finally (show/stop! (:runtime indexed)))))
      (finally (show/stop! (:runtime scalar))))))

(defn- host-case [{:keys [model preview-indices pixels]} n selection warmup samples]
  (binding [*out* *err*] (println "Capturing" pixels "pixels," n "field(s) with" selection))
  (let [model (seed-model model (volumes n selection 0.0))
        by-cue (into {} (map (juxt :cue-id :id)) (vals (:instances model)))
        ids (mapv (comp by-cue field-id) (range n))
        _ (when-not (every? some? ids) (throw (ex-info "Expected running spatial cues" {:ids ids})))
        step (fn [show i] (host-step show ids preview-indices i))
        expected (expected-frames model step (+ warmup samples)
                                  #(coverage %1 ids %2 (contains? selection :beam)))
        host (new-show model false)]
    (try
      (let [observed (aggregate (mapv :coverage expected))]
        (when (or (and (contains? selection :beam) (zero? (get-in observed [:beam-movers :max])))
                  (and (> n 1) (zero? (get-in observed [:overlapping-heads :min]))))
          (throw (ex-info "Fixture coverage did not exercise this case" {:fields n :coverage observed})))
        {:scope :host :fields n :selection selection :workers 2 :output :independent
         :coverage observed
         :result (bench/measure #(dissoc (step host %) :model) expected {:warmup warmup :samples samples})})
      (finally (show/stop! (:runtime host))))))

(defn- emitter [fixture]
  (cond-> fixture (get-in fixture [:channels :pan]) (assoc :beam-angle 24.0 :lens-radius-m 0.05)))

(defn venue
  "Build the sculpture, its 64 moving-head patches and the room bounds."
  [pixels]
  (-> (bench/corpus pixels 64 :sculpture)
      (update-in [:model :rig :fixtures] #(mapv emitter %))
      (assoc-in [:model :rig :room] {:width 24.0 :height 9.0 :depth 24.0})
      (assoc-in [:model :settings :spatial]
                {:enabled? true :run-when :enabled
                 :bounds {:min [-12.0 0.0 -12.0] :max [12.0 9.0 12.0]}})))

(defn- committed-observations [model]
  (let [{:keys [runtime]} (new-show model false)
        patch (:rig model)
        {:keys [width height depth]} (:room patch)]
    (try
      (show/tick! runtime 250)
      (show/tick! runtime 850)
      (let [compact (get-in @(:state runtime) [:output :compact])]
        {:values (into {} (map (juxt :id identity)) (render/project-compact compact))
         :surfaces (surfaces/receivers patch) :extinction-per-m 0.0
         :range-m (Math/hypot (Math/hypot width depth) height)})
      (finally (show/stop! runtime)))))

(defn- optical-case [{:keys [model]} n warmup samples]
  (let [rig (spatial/prepare-rig (get-in model [:rig :fixtures]))
        opts (field/prepare-observations (committed-observations model))
        weights (fn [volume] (:weights (field/sample volume rig opts)))
        step (fn [i]
               (let [shapes (volumes n #{:position :beam} (/ i 20.0))
                     {:keys [value] :as observation} (elapsed #(mapv weights shapes))]
                 (assoc (dissoc observation :value) :positive (reduce + (map count value)))))]
    (dotimes [i warmup] (step i))
    {:scope :optics :fields n :surfaces (count (:surfaces opts))
     :summary (aggregate (mapv #(step (+ warmup %)) (range samples)))}))

(defn- arguments [args]
  (let [[size warmup-count sample-count scope output] args
        opts {:pixels (parse-long (or size "10000")) :warmup (parse-long (or warmup-count "6"))
              :samples (parse-long (or sample-count "16")) :scope (keyword (or scope "all"))
              :output output}]
    (when-not (and (<= (count args) 5) (every? #(pos-int? (get opts %)) [:pixels :warmup :samples])
                   (#{:all :position :optics :host :host-position :host-beam} (:scope opts)))
      (throw (ex-info "Expected pixels warmup samples scope [output.edn]" {:arguments args})))
    opts))

(defn- measurements [{:keys [pixels warmup samples scope]}]
  (let [{:keys [fixtures] :as data} (venue pixels)
        selections (case scope :host-position [#{:position}] :host-beam [#{:position :beam}]
                         [#{:position} #{:position :beam}])]
    {:pixels pixels :moving-heads 64 :samples samples :warmup warmup :scope scope
     :source-revisions {:tolglow (bench/revision ".")
                        :afterglow (bench/revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))}
     :environment {:java (System/getProperty "java.version") :clojure (clojure-version)
                   :processors (.availableProcessors (Runtime/getRuntime)) :cpu (bench/cpu-model)
                   :heap-bytes (.maxMemory (Runtime/getRuntime))}
     :runs (vec (concat
                 (when (#{:all :position} scope)
                   (mapv #(position-case fixtures % warmup samples) [1 4]))
                 (when (= :optics scope)
                   (mapv #(optical-case data % warmup samples) [1 4]))
                 (when (#{:all :host :host-position :host-beam} scope)
                   (for [n [1 4] selection selections]
                     (host-case data n selection warmup samples)))))}))

(defn -main [& args]
  (try
    (let [{:keys [output] :as opts} (arguments args)
          report (measurements opts)]
      (if output
        (do (io/make-parents output) (spit output (str (pr-str report) "\n")))
        (prn report)))
    (finally (shutdown-agents))))
