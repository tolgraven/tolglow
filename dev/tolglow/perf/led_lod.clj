(ns tolglow.perf.led-lod
  "Research harness for spatial level of detail on LED strips and matrices (#822).

  Five scopes, none of them production code:

  - `stages` wraps the real `show/tick!` on the mixed rig with inclusive
    timers, so one frame splits into per-pixel effect evaluation, envelope and
    validation, merge, fixture packing, ArtDMX encoding and the rest. It also
    lists every evaluated batch plan with its target count and fields.
  - `e2e` runs that same tick again with a prototype evaluator swapped in
    for large emission batches and compares every ArtDMX byte with the
    unmodified run.
  - `hidden` counts the typed writes of one frame that a later opaque
    replacement overwrites.
  - `families` times representative effect families through the batch
    compiler's own step at one pixel count.
  - `lod` evaluates the same families at full resolution and through a
    prototype of chunk heads plus interpolation, and reports error, shimmer
    and time. The prototype calls the production `ir/evaluate-batch` on the
    coarse sample positions and reconstructs per-pixel fields with a
    primitive loop.

  No sockets or devices. `stages` uses the mixed rig's ArtDMX capture sink.
  See docs/research/led-spatial-lod.md for the numbers this produced."
  (:require [afterglow.render.ir :as ir]
            [afterglow.spatial.field :as field]
            [afterglow.time :as time]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [tolglow.bench-show :as bench]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.compiler.gradient :as gradient]
            [tolglow.perf.mixed-rig :as mixed]
            [tolglow.registry :as registry]
            [tolglow.show :as show])
  (:import [java.lang.management ManagementFactory]
           [java.util Arrays]
           [java.util.concurrent ConcurrentHashMap]
           [java.util.concurrent.atomic LongAdder]))

(set! *warn-on-reflection* true)

;; ------------------------------------------------------------------ timers

(def ^:private ^ConcurrentHashMap timers (ConcurrentHashMap.))

(defn- add-time! [key ^long amount]
  (let [^LongAdder adder (or (.get timers key)
                             (do (.putIfAbsent timers key (LongAdder.)) (.get timers key)))]
    (.add adder amount)))

(defn- timed [key f]
  (fn [& args]
    (let [start (System/nanoTime)]
      (try (apply f args)
           (finally (add-time! key (- (System/nanoTime) start)))))))

(defn- timed-evaluate
  "Wrap `ir/evaluate-batch`: total time plus time and calls per plan shape."
  [f]
  (fn [plan env positions & more]
    (let [start (System/nanoTime)]
      (try (apply f plan env positions more)
           (finally
             (let [nanos (- (System/nanoTime) start)
                   n (if (vector? positions) (count positions) (quot (alength ^doubles positions) 3))
                   shape [n (mapv (juxt :attribute :components :spatial?) (:outputs plan))]]
               (add-time! :evaluate-batch nanos)
               (add-time! [:plan-nanos shape] nanos)
               (add-time! [:plan-calls shape] 1)))))))

(def ^:private instrumented
  '[[afterglow.runtime.engine/advance :engine-advance]
    [tolglow.compiler.batch/finish-fields :finish-fields]
    [afterglow.render.frame/resolve-contributions :render]
    [afterglow.render.frame/validate-batches :render/validate-batches]
    [afterglow.render.layers/compose-fields :render/compose-fields]
    [afterglow.render.frame/merge-packed-batches! :render/merge-packed]
    [afterglow.render.frame/merge-general-batch! :render/merge-general]
    [afterglow.render.frame/pack-dense-state :render/pack-dense]
    [afterglow.render.frame/finish-contributions :render/finish]])

(defn- read-axes
  "World axes that a map of output expressions reads. A `:surface-uv` node
  counts as world x (u) or y (v): that is what it reads on a head without a
  chart, and the prototypes here only take groups that have no chart at all.
  A charted group needs the UV pair in the tuple as well, which this does not
  model."
  [expressions]
  (vec (sort (into #{} (comp (mapcat #(tree-seq (fn [n] (and (map? n) (vector? (:args n)))) :args %))
                             (keep #(case (:op %)
                                      :position (:axis %)
                                      :surface-uv ({:u :x :v :y} (:axis %))
                                      nil)))
                   (vals expressions)))))

(defn- with-instrumentation
  "Run `run` with the inclusive timers installed, then restore every var.
  `replace-evaluate`, when given, wraps the timed evaluator: the end-to-end
  scope uses it to swap in a prototype for large emission batches."
  ([run] (with-instrumentation nil run))
  ([replace-evaluate run]
   (let [vars (mapv (fn [[sym key]] [(find-var sym) key]) instrumented)
         originals (into {#'ir/evaluate-batch @#'ir/evaluate-batch #'ir/compile-batch @#'ir/compile-batch}
                         (map (fn [[v _]] [v @v])) vars)]
     (try
       (doseq [[v key] vars] (alter-var-root v #(timed key %)))
       (alter-var-root #'ir/evaluate-batch timed-evaluate)
       (when replace-evaluate
         ;; The compiled plan does not keep its expressions; remember which
         ;; axes they read.
         (alter-var-root #'ir/compile-batch (fn [f] (fn [outputs] (assoc (f outputs) ::axes (read-axes outputs)))))
         (alter-var-root #'ir/evaluate-batch replace-evaluate))
       (run)
       (finally (doseq [[v original] originals] (alter-var-root v (constantly original))))))))

(defn- timer-snapshot [] (into {} (map (fn [[k ^LongAdder v]] [k (.sum v)])) timers))

(defn- median [values]
  (let [xs (vec (sort values))] (nth xs (quot (count xs) 2))))

;; ------------------------------------------------------------------ stages

(declare prototype-evaluator)

(defn- packet-difference
  "Compare two frames of ArtDMX packets byte by byte."
  [reference candidate]
  (when-not (= (count reference) (count candidate))
    (throw (ex-info "Frames have different universe counts" {})))
  (reduce (fn [result [^bytes a ^bytes b]]
            (loop [i 0 result result]
              (if (= i (alength a)) result
                  (let [d (Math/abs (- (bit-and 0xff (aget a i)) (bit-and 0xff (aget b i))))]
                    (recur (inc i) (cond-> (update result :bytes inc)
                                     (pos? d) (-> (update :differing inc) (update :max-difference max d))))))))
          {:bytes 0 :differing 0 :max-difference 0}
          (map vector reference candidate)))

(defn- named-show
  "A show named on the command line. `--show` is a qualified symbol for a
  zero-argument function that returns a control model, for example
  tolglow.showcase/model. `--timeline`, optional, is a function of the runtime
  that starts the show's own programme, for example tolglow.showcase/play!."
  [{:keys [show timeline]}]
  (let [model ((requiring-resolve show))]
    {:model model
     :play! (when timeline (requiring-resolve timeline))
     :counts {:fixtures (count (get-in model [:rig :fixtures]))}}))

(defn stage-run
  "Split a host tick into stages. One effect worker, so inclusive timers add
  up to the owner's wall time. By default the show is the mixed rig at
  `pixels`; with :show it is the named show, ticked every 25 ms on a manual
  clock. :eval-mode selects a prototype evaluator for large emission batches,
  :keep-packets? returns each measured frame's ArtDMX packets and :reference
  compares against such frames (mixed rig only)."
  [opts pixels]
  (let [named? (some? (:show opts))
        data (if named? (named-show opts) (mixed/corpus opts pixels))
        clock (volatile! 0.0)
        sink (update (bench/capture-sink) :send! #(timed :packet-encode %))
        packets (volatile! [])
        faults (atom [])
        step! (fn [runtime i]
                (let [tick (if named?
                             (let [start (System/nanoTime)
                                   _ (show/tick! runtime (* 25.0 i))
                                   ms (/ (- (System/nanoTime) start) 1e6)
                                   status (:status @(:state runtime))]
                               (when (or (:error status) (:output-error status))
                                 (throw (ex-info "The named show did not produce a successful frame"
                                                 {:status (select-keys status [:error :output-error])})))
                               ms)
                             (get-in (bench/projected-host-step! runtime sink clock (:preview-indices data) i)
                                     [:stages-ms :tick]))]
                  (when (>= i (:warmup opts)) (vswap! packets conj @(:latest sink)))
                  tick))]
    (with-instrumentation
      (when-not (contains? #{nil :full} (:eval-mode opts))
        #(prototype-evaluator % (:eval-mode opts) (:e2e-chunk opts 8) (:tolerance opts)))
      (fn []
        ;; A first frame builds the prototype's plans inside the worker, so
        ;; the worker gets a long deadline. Fault events stay in this run
        ;; instead of going to the state log under the home directory.
        (let [overrides {:workers (:stage-workers opts 1) :worker-timeout-ms 10000
                         :effect-log! #(swap! faults conj %)}
              runtime (if named?
                        (show/create-runtime (merge {:model (:model data) :workspace-options nil :sink sink} overrides))
                        (:runtime (#'mixed/runtime-for (:model data) clock sink overrides)))
              result
          (try
            (when-let [play! (:play! data)] (play! runtime))
            (dotimes [i (:warmup opts)] (step! runtime i))
            (.clear timers)
            (let [ticks (mapv #(step! runtime %) (range (:warmup opts) (+ (:warmup opts) (:samples opts))))
                  snapshot (timer-snapshot)
                  per-frame #(/ (double (get snapshot % 0)) 1e6 (:samples opts))
                  plans (for [[key nanos] snapshot :when (and (vector? key) (= :plan-nanos (first key)))
                              :let [shape (second key)
                                    [targets fields] shape
                                    calls (get snapshot [:plan-calls shape])]]
                          {:targets targets :fields fields
                           :spatial-components (reduce + (map second (filter #(nth % 2) fields)))
                           :uniform-components (reduce + (map second (remove #(nth % 2) fields)))
                           :calls-per-frame (/ (double calls) (:samples opts))
                           :ms-per-frame (/ (double nanos) 1e6 (:samples opts))})
                  spatial (reduce + (map :ms-per-frame (filter (comp pos? :spatial-components) plans)))
                  evaluate (per-frame :evaluate-batch)
                  engine (per-frame :engine-advance)
                  render (per-frame :render)
                  merge-ms (+ (per-frame :render/merge-packed) (per-frame :render/merge-general)
                              (per-frame :render/compose-fields))
                  tick (/ (reduce + ticks) (count ticks))]
              (cond->
               {:fixtures (:counts data) :universes (:universes data)
                :cues (when-not named? (count (:definitions data)))
                :show (:show opts) :timeline (:timeline opts)
                :eval-mode (:eval-mode opts :full)
                :parity (when-let [reference (:reference opts)]
                          (let [total (reduce (fn [a b] (-> (merge-with + a (dissoc b :max-difference))
                                                            (assoc :max-difference (max (:max-difference a) (:max-difference b)))))
                                              (map packet-difference reference @packets))]
                            (assoc total :identical? (zero? (:differing total))
                                   :differing-fraction (/ (double (:differing total)) (:bytes total)))))
                :lod-ms-per-frame (per-frame :lod-total)}
                (:keep-packets? opts) (assoc :packets @packets)
                true (merge
              {:tick-ms (bench/distribution ticks)
               :ms-per-frame {:tick tick
                              :evaluate-batch evaluate
                              :evaluate-spatial-plans spatial
                              :evaluate-uniform-plans (- evaluate spatial)
                              :finish-fields (per-frame :finish-fields)
                              :engine-other (- engine evaluate (per-frame :finish-fields))
                              :render render
                              :render-validate (per-frame :render/validate-batches)
                              :render-merge merge-ms
                              :render-pack-dense (per-frame :render/pack-dense)
                              :render-finish-other (- (per-frame :render/finish) (per-frame :render/pack-dense))
                              :render-other (- render (per-frame :render/validate-batches) merge-ms (per-frame :render/finish))
                              :packet-encode (per-frame :packet-encode)
                              :host-other (- tick engine render (per-frame :packet-encode))}
               :plans (vec (sort-by (comp - :ms-per-frame) plans))})))
            (finally (show/stop! runtime)))]
          (when-let [failed (seq (filter #(= :effect/failed (:type %)) @faults))]
            (throw (ex-info "An effect failed during the measured run" {:count (count failed) :first (first failed)})))
          result)))))

;; ------------------------------------------------------------ hidden writes

(defn hidden-run
  "Count typed batch writes of one mixed-rig frame that a later opaque
  replacement of the same attribute on the same fixture overwrites. This is
  what is numerically unobservable in the packed frame. It ignores that the
  renderer still validates a hidden value where it was written."
  [opts pixels]
  (let [data (mixed/corpus opts pixels)
        clock (volatile! 0.0)
        sink (bench/capture-sink)
        captured (volatile! nil)
        faults (atom [])
        render (find-var 'afterglow.render.frame/resolve-contributions)
        original @render]
    (try
      (alter-var-root render (fn [f] (fn [& args] (vreset! captured (second args)) (apply f args))))
      (let [{:keys [runtime]} (#'mixed/runtime-for (:model data) clock sink
                                                   {:workers 1 :worker-timeout-ms 10000 :effect-log! #(swap! faults conj %)})]
        (try (dotimes [i 40] (bench/projected-host-step! runtime sink clock (:preview-indices data) i))
             (finally (show/stop! runtime))))
      (finally (alter-var-root render (constantly original))))
    (when (some #(= :effect/failed (:type %)) @faults)
      (throw (ex-info "An effect failed during the captured run" {:first (first @faults)})))
    (let [covered (volatile! {})
          rows (reduce
                (fn [rows block]
                  (if-not (= :batch (:kind block))
                    rows
                    (let [{:keys [target-ids blend opacity outputs] :or {blend :replace opacity 1} :as batch} (:batch block)
                          opaque? (and (= :replace blend) (== 1 opacity) (nil? (:afterglow.render.layers/opacities batch)))]
                      (into rows
                            (for [[attribute field] (sort-by (comp str key) outputs)]
                              (let [^java.util.HashSet seen (or (get @covered attribute)
                                                                (let [fresh (java.util.HashSet.)]
                                                                  (vswap! covered assoc attribute fresh)
                                                                  fresh))
                                    hidden (count (filter #(.contains seen %) target-ids))]
                                (when opaque? (.addAll seen target-ids))
                                {:attribute attribute :targets (count target-ids) :components (:components field)
                                 :blend blend :opaque? opaque? :hidden-targets hidden}))))))
                [] (reverse @captured))
          large (filter #(>= (:targets %) 256) rows)
          total (reduce + (map #(* (:targets %) (:components %)) large))
          hidden (reduce + (map #(* (:hidden-targets %) (:components %)) large))]
      {:fixtures (:counts data)
       :blocks (count @captured)
       :pixel-batch-components total
       :hidden-components hidden
       :hidden-fraction (when (pos? total) (/ (double hidden) total))
       ;; Launch order, lowest layer first.
       :pixel-batches (vec (reverse large))})))

;; ---------------------------------------------------------------- families

(defn- op [operation & args] {:op operation :args (vec args)})

(def ^:private x {:op :position :axis :x})

(def ^:private z {:op :position :axis :z})

(def ^:private beat {:op :beat})

(def ^:private level {:op :param :key :level})

(defn- smooth [w] (op :multiply w w (op :subtract 3.0 (op :multiply 2.0 w))))

(defn- sphere
  "Weight of a sphere of `radius` whose centre sweeps the matrix, fading over
  `2 * band` length units around its surface. Rows give the matrix depth."
  [rows radius band]
  (let [cx (op :add 50.0 (op :multiply 40.0 (op :sin (op :multiply 1.5 beat))))
        cz (op :add (* 0.5 rows) (op :multiply (* 0.3 rows) (op :cos (op :multiply 1.1 beat))))
        dx (op :subtract x cx)
        dz (op :subtract z cz)
        distance (op :sqrt (op :add (op :multiply dx dx) (op :multiply dz dz)))]
    (smooth (op :clamp (op :divide (op :subtract (+ radius band) distance) (* 2.0 band)) 0.0 1.0))))

(def ^:private palette
  (mapv (fn [i color] {:position (/ i 7.0) :color color})
        (range 8)
        [[1.0 0.0 0.0] [1.0 0.5 0.0] [1.0 1.0 0.0] [0.0 1.0 0.0]
         [0.0 1.0 1.0] [0.0 0.0 1.0] [0.5 0.0 1.0] [1.0 0.0 0.5]]))

(defn families
  "Effect families on a `columns` wide matrix with `rows` rows at unit pitch.
  :smooth? is the author's expectation, not something the harness trusts."
  [rows]
  (let [soft (sphere rows 12.0 6.0)
        hard (sphere rows 12.0 0.25)
        small (sphere rows 1.5 0.25)
        cell (op :floor (op :multiply 0.2 x))]
    [{:id :uniform-wash :smooth? true
      :outputs {:color (op :vec3 level 0.2 0.6) :white 0.1}}
     {:id :sine-wash :smooth? true
      :outputs {:color (op :vec3 (op :multiply 0.5 (op :add 1.0 (op :sin (op :add (op :multiply 0.05 x) {:op :bar})))) level 0.25)
                :white 0.1}}
     {:id :saw-chase :smooth? false
      :outputs {:color (op :vec3 (op :multiply level (op :fract (op :add (op :multiply 0.5 beat) (op :multiply 0.02 x)))) 0.1 level)}}
     {:id :plasma :smooth? true
      :outputs {:color (op :vec3
                           (op :add 0.5 (op :multiply 0.25 (op :sin (op :add (op :multiply 0.11 x) beat)))
                               (op :multiply 0.25 (op :sin (op :subtract (op :multiply 0.07 z) (op :multiply 0.5 beat)))))
                           (op :add 0.5 (op :multiply 0.25 (op :sin (op :add (op :multiply 0.05 x) (op :multiply 0.09 z) (op :multiply 0.7 beat))))
                               (op :multiply 0.25 (op :cos (op :subtract (op :multiply 0.13 z) beat))))
                           (op :add 0.5 (op :multiply 0.5 (op :sin (op :add (op :multiply 0.03 x) (op :multiply -0.04 z) (op :multiply 0.3 beat))))))}}
     {:id :palette-gradient :smooth? true
      :params {:palette {:type :gradient :default palette}}
      :outputs {:color (op :multiply {:op :gradient :key :palette
                                      :args [(op :fract (op :add (op :multiply 0.01 x) (op :multiply 0.25 beat)))]}
                           level)}}
     {:id :soft-sphere :smooth? true
      :outputs {:color (op :vec3 (op :multiply level soft) (op :multiply 0.3 soft) 0.1)}}
     {:id :hard-sphere :smooth? false
      :outputs {:color (op :vec3 (op :multiply level hard) (op :multiply 0.3 hard) 0.1)}}
     {:id :small-sphere :smooth? false
      :outputs {:color (op :vec3 (op :multiply level small) (op :multiply 0.3 small) 0.1)}}
     {:id :cell-chase :smooth? false
      :outputs {:color (op :vec3 (op :clamp (op :subtract 1.0 (op :multiply 4.0 (op :fract (op :subtract (op :multiply 0.2 cell) (op :multiply 0.5 beat))))) 0.0 1.0)
                           0.1 level)}}
     {:id :fine-ripple :smooth? true
      :outputs {:color (op :vec3 (op :add 0.5 (op :multiply 0.5 (op :sin (op :add (op :multiply 1.5 x) (op :multiply 3.0 beat))))) 0.1 level)}}]))

(defn- matrix-rig [pixels columns]
  (mapv (fn [i] {:id (str "pixel-" i)
                 :universe (inc (quot (* 4 i) 512)) :address (inc (mod (* 4 i) 512))
                 :position [(double (mod i columns)) 0.0 (double (quot i columns))]
                 :channels {:red 1 :green 2 :blue 3 :white 4}})
        (range pixels)))

(defn- definition [rig {:keys [id params outputs]}]
  {:id id :version 1 :targets (mapv :id rig)
   :params (merge {:level {:type :number :default 0.8 :bounds [0 1]}} params)
   :outputs outputs})

(defn- beat-at [i] (/ (* 25.0 (inc i)) 500.0))

(defn- step-context [definition rig i]
  {:frame {:time-ms (* 25.0 (inc i)) :fixtures rig :beat (beat-at i)}
   :instance {:id "family" :definition definition :effect-id (:id definition) :params {}
              :started-at-ms 0 :started-at-beat 0.0 :timing-meter {:beats-per-bar 4 :bars-per-phrase 4}
              :phase :running}
   :state nil :assignments [] :contributions [] :intents [] :done? false})

(defn- hand-sine-wash
  "The :sine-wash colour written as one primitive loop over flat coordinates:
  what compiled code without an expression tree costs for the same field."
  ^doubles [^doubles xs ^double bar ^double level]
  (let [n (alength xs) out (double-array (* 3 n))]
    (dotimes [i n]
      (let [base (* 3 i)]
        (aset out base (* 0.5 (+ 1.0 (Math/sin (+ (* 0.05 (aget xs i)) bar)))))
        (aset out (+ base 1) level)
        (aset out (+ base 2) 0.25)))
    out))

(defn- hand-soft-sphere
  "The :soft-sphere colour written as one primitive loop: the centre is
  computed once per frame and the weight once per pixel."
  ^doubles [^doubles xs ^doubles zs beat level rows]
  (let [n (alength xs) out (double-array (* 3 n))
        beat (double beat) level (double level) rows (double rows)
        cx (+ 50.0 (* 40.0 (Math/sin (* 1.5 beat))))
        cz (+ (* 0.5 rows) (* 0.3 rows (Math/cos (* 1.1 beat))))]
    (dotimes [i n]
      (let [dx (- (aget xs i) cx) dz (- (aget zs i) cz)
            w (Math/max 0.0 (Math/min 1.0 (/ (- 18.0 (Math/sqrt (+ (* dx dx) (* dz dz)))) 12.0)))
            s (* w w (- 3.0 (* 2.0 w)))
            base (* 3 i)]
        (aset out base (* level s))
        (aset out (+ base 1) (* 0.3 s))
        (aset out (+ base 2) 0.1)))
    out))

(defn family-run
  "Per-frame cost of each family's compiled batch step at `pixels` targets."
  [{:keys [warmup samples columns]} pixels]
  (let [rig (matrix-rig pixels columns)
        rows (quot pixels columns)
        steps (mapv (fn [family]
                      (let [d (definition rig family)
                            step (#'batch/build-effect d rig {})]
                        (when-not (= :batch (:backend (meta step)))
                          (throw (ex-info "Family did not select the batch backend" {:id (:id family) :meta (meta step)})))
                        {:id (:id family) :definition d :step step
                         :spatial-components (reduce + (for [group [(compiler/compile-plan d)]
                                                             output (:outputs group)
                                                             :let [plan (ir/compile-batch {(:attribute output) (:expression output)})]
                                                             field (:outputs plan) :when (:spatial? field)]
                                                         (:components field)))}))
                    (families rows))
        run (fn [{:keys [definition step]} i] (step (step-context definition rig i)))
        volume (fn [i] {:shape :sphere :radius 12.0
                        :position [(+ 50.0 (* 40.0 (Math/sin (* 1.5 (beat-at i))))) 0.0
                                   (+ (* 0.5 rows) (* 0.3 rows (Math/cos (* 1.1 (beat-at i)))))]
                        :falloff {:inner-m 6.0 :outer-m 6.0 :curve :smoothstep}})
        prepared (field/prepare-rig rig)
        xs (double-array (map (comp first :position) rig))
        zs (double-array (map #(nth (:position %) 2) rig))]
    (with-instrumentation
      (fn []
        ;; Warm every family in turn first, so the shared evaluator call sites
        ;; see the same mix of expression shapes the live show gives them.
        (dotimes [i warmup] (doseq [entry steps] (run entry i)))
        (dotimes [i warmup]
          (field/positions (volume i) prepared)
          (hand-sine-wash xs (/ (beat-at i) 4.0) 0.8)
          (hand-soft-sphere xs zs (beat-at i) 0.8 rows))
        (let [time-frames (fn [f] (median (mapv (fn [i] (let [start (System/nanoTime)] (f i) (- (System/nanoTime) start)))
                                                 (range warmup (+ warmup samples)))))
              rows-out (mapv (fn [{:keys [id spatial-components] :as entry}]
                               (.clear timers)
                               (let [nanos (time-frames #(run entry %))
                                     snapshot (timer-snapshot)
                                     evaluate (/ (double (get snapshot :evaluate-batch 0)) samples)
                                     finish (/ (double (get snapshot :finish-fields 0)) samples)]
                                 {:id id :spatial-components spatial-components
                                  :step-us (/ nanos 1e3)
                                  :evaluate-us-mean (/ evaluate 1e3) :finish-us-mean (/ finish 1e3)
                                  :step-ns-per-pixel (/ (double nanos) pixels)
                                  :evaluate-ns-per-spatial-component
                                  (when (pos? spatial-components) (/ evaluate (* (double pixels) spatial-components)))}))
                             steps)
              volume-nanos (time-frames #(field/positions (volume %) prepared))
              hits (count (:weights (field/positions (volume warmup) prepared)))
              hand-nanos (time-frames #(hand-sine-wash xs (/ (beat-at %) 4.0) 0.8))
              sphere-nanos (time-frames #(hand-soft-sphere xs zs (beat-at %) 0.8 rows))
              ;; The hand-written loops must agree with the compiled families.
              agree? (fn [id ^doubles hand]
                       (let [entry (first (filter #(= id (:id %)) steps))
                             block (peek (:contributions (run entry warmup)))
                             ^doubles reference (get-in block [:batch :outputs :color :values])]
                         (every? #(< (Math/abs (- (aget hand %) (aget reference %))) 1e-12) (range (alength reference)))))]
          (when-not (and (agree? :sine-wash (hand-sine-wash xs (/ (beat-at warmup) 4.0) 0.8))
                         (agree? :soft-sphere (hand-soft-sphere xs zs (beat-at warmup) 0.8 rows)))
            (throw (ex-info "A hand-written loop differs from its compiled family" {})))
          {:pixels pixels :columns columns :rows rows
           :families rows-out
           :volume-cue-sphere {:us (/ volume-nanos 1e3) :ns-per-pixel (/ (double volume-nanos) pixels)
                               :weighted-heads hits
                               :note "afterglow.spatial.field/positions for one moving sphere: weights by head ID, before masking"}
           :hand-written-sine-wash {:us (/ hand-nanos 1e3) :ns-per-pixel (/ (double hand-nanos) pixels)
                                    :note "primitive loop for the :sine-wash colour field over flat x coordinates"}
           :hand-written-soft-sphere {:us (/ sphere-nanos 1e3) :ns-per-pixel (/ (double sphere-nanos) pixels)
                                      :note "primitive loop for the :soft-sphere colour field over flat x and z coordinates"}})))))

;; ----------------------------------------------------------- LOD prototype

(defn- strip-nodes
  "Node parameters 0, c, 2c ... and always the last pixel of a strip."
  [length chunk]
  (let [nodes (vec (range 0 length chunk))]
    (if (= (peek nodes) (dec length)) nodes (conj nodes (dec length)))))

(defn- plan-from
  "Build {:samples positions :taps k :indices int[] :weights double[]} from
  a per-pixel vector of [[sample-key weight] ...] and a key -> position map."
  [rig taps per-pixel sample-position]
  (let [keys (vec (distinct (mapcat #(map first %) per-pixel)))
        index (zipmap keys (range))
        n (count per-pixel)
        indices (int-array (* n taps))
        weights (double-array (* n taps))]
    (dotimes [i n]
      (let [pairs (nth per-pixel i)]
        (dotimes [k taps]
          (let [[key weight] (nth pairs k [(ffirst pairs) 0.0])]
            (aset indices (+ (* i taps) k) (int (index key)))
            (aset weights (+ (* i taps) k) (double weight))))))
    {:samples (mapv sample-position keys) :taps taps :indices indices :weights weights
     :sample-count (count keys) :pixel-count (count rig)}))

(defn- position-at
  "World position at fractional parameter u along row `row` of the matrix."
  [rig columns row u]
  (let [lo (long (Math/floor u)) hi (min (dec columns) (inc lo)) t (- u lo)
        a (:position (nth rig (+ (* row columns) lo)))
        b (:position (nth rig (+ (* row columns) hi)))]
    (mapv (fn [p q] (+ (* (- 1.0 t) p) (* t q))) a b)))

(defn- centre-plan
  "Chunk heads at chunk centres along each row (the operator's formulation).
  `linear?` interpolates between neighbouring centres and holds the end
  values; otherwise every pixel takes its own chunk's head."
  [rig columns rows chunk linear?]
  (let [chunks (mapv (fn [start] (let [end (min columns (+ start chunk))] {:u (/ (+ start (dec end)) 2.0) :start start :end end}))
                     (range 0 columns chunk))
        per-pixel (vec (for [row (range rows) i (range columns)]
                         (let [k (quot i chunk)
                               u (:u (nth chunks k))]
                           (cond
                             (not linear?) [[[row k] 1.0]]
                             (and (< i u) (pos? k)) (let [u0 (:u (nth chunks (dec k))) t (/ (- i u0) (- u u0))]
                                                      [[[row (dec k)] (- 1.0 t)] [[row k] t]])
                             (and (> i u) (< (inc k) (count chunks))) (let [u1 (:u (nth chunks (inc k))) t (/ (- i u) (- u1 u))]
                                                                         [[[row k] (- 1.0 t)] [[row (inc k)] t]])
                             :else [[[row k] 1.0] [[row k] 0.0]]))))]
    (plan-from rig (if linear? 2 1) per-pixel (fn [[row k]] (position-at rig columns row (:u (nth chunks k)))))))

(defn- node-plan
  "Samples on real pixels at chunk boundaries along each row. :linear uses
  two neighbours, :cubic a Catmull-Rom kernel over four (linear in the last,
  shorter interval)."
  [rig columns rows chunk kind]
  (let [nodes (strip-nodes columns chunk)
        last-node (dec (count nodes))
        per-pixel (vec (for [row (range rows) i (range columns)]
                         (let [k (min (dec last-node) (quot i chunk))
                               a (nth nodes k) b (nth nodes (inc k))
                               t (/ (double (- i a)) (- b a))
                               uniform? (and (= chunk (- b a)) (pos? k) (< (inc k) last-node)
                                             (= chunk (- (nth nodes (+ k 2)) b)))]
                           (if (and (= :cubic kind) uniform?)
                             (let [t2 (* t t) t3 (* t2 t)]
                               [[[row (nth nodes (dec k))] (* 0.5 (+ (- t3) (* 2.0 t2) (- t)))]
                                [[row a] (* 0.5 (+ (* 3.0 t3) (* -5.0 t2) 2.0))]
                                [[row b] (* 0.5 (+ (* -3.0 t3) (* 4.0 t2) t))]
                                [[row (nth nodes (+ k 2))] (* 0.5 (- t3 t2))]])
                             [[[row a] (- 1.0 t)] [[row b] t]]))))]
    (plan-from rig (if (= :cubic kind) 4 2) per-pixel (fn [[row i]] (:position (nth rig (+ (* row columns) i)))))))

(defn- bilinear-plan
  "Samples on real pixels of a node grid in both matrix directions."
  [rig columns rows chunk]
  (let [across (strip-nodes columns chunk)
        down (strip-nodes rows chunk)
        bracket (fn [nodes i]
                  (let [k (min (- (count nodes) 2) (quot i chunk))
                        a (nth nodes k) b (nth nodes (inc k))]
                    [a b (/ (double (- i a)) (- b a))]))
        per-pixel (vec (for [row (range rows) i (range columns)]
                         (let [[a b t] (bracket across i) [c d s] (bracket down row)]
                           [[[c a] (* (- 1.0 t) (- 1.0 s))] [[c b] (* t (- 1.0 s))]
                            [[d a] (* (- 1.0 t) s)] [[d b] (* t s)]])))]
    (plan-from rig 4 per-pixel (fn [[row i]] (:position (nth rig (+ (* row columns) i)))))))

(defn- dedup-plan
  "Exact: one sample per distinct tuple of the coordinates the fields read."
  [positions axes]
  (let [project (fn [position] (mapv #(nth position ({:x 0 :y 1 :z 2} %)) axes))
        first-position (reduce (fn [m position] (let [k (project position)] (if (contains? m k) m (assoc m k position))))
                               {} positions)
        per-pixel (mapv (fn [position] [[(project position) 1.0]]) positions)]
    (plan-from positions 1 per-pixel first-position)))

(defn- runs
  "Split ordered positions into maximal runs of evenly spaced collinear points,
  as [start end) index pairs. A matrix patched row by row gives one run per row."
  [positions]
  (let [n (count positions)
        step (fn [i] (mapv - (nth positions (inc i)) (nth positions i)))
        same? (fn [a b] (every? #(< (Math/abs (double %)) 1e-9) (map - a b)))]
    (loop [start 0 i 0 out []]
      (cond (>= i (dec n)) (conj out [start n])
            (or (= i start) (same? (step i) (step start))) (recur start (inc i) out)
            :else (recur (inc i) (inc i) (conj out [start (inc i)]))))))

(defn- run-linear-plan
  "Linear interpolation between samples on real pixels at chunk boundaries of
  each detected run. Works for any ordered subset of a strip or matrix."
  [positions chunk]
  (let [per-pixel (vec (mapcat (fn [[start end]]
                                 (let [nodes (strip-nodes (- end start) chunk)
                                       last-node (dec (count nodes))]
                                   (for [i (range (- end start))]
                                     (if (zero? last-node)
                                       [[start 1.0] [start 0.0]]
                                       (let [k (min (dec last-node) (quot i chunk))
                                             a (nth nodes k) b (nth nodes (inc k))
                                             t (/ (double (- i a)) (- b a))]
                                         [[(+ start a) (- 1.0 t)] [(+ start b) t]])))))
                               (runs positions)))]
    (plan-from positions 2 per-pixel #(nth positions %))))

(defn- reconstruct
  "Per-pixel field from coarse samples. Clamps to [0,1]: a cubic kernel can overshoot."
  ^doubles [^doubles coarse components {:keys [taps ^ints indices ^doubles weights pixel-count]}]
  (let [components (long components) taps (long taps) n (long pixel-count)
        out (double-array (* n components))]
    (dotimes [i n]
      (let [base (* i taps)]
        (dotimes [c components]
          (loop [k 0 acc 0.0]
            (if (< k taps)
              (recur (inc k) (+ acc (* (aget weights (+ base k))
                                       (aget coarse (+ (* (long (aget indices (+ base k))) components) c)))))
              (aset out (+ (* i components) c) (Math/max 0.0 (Math/min 1.0 acc))))))))
    out))

(defn- fill-uniform ^doubles [^doubles coarse components n]
  (let [components (long components) n (long n) out (double-array (* n components))]
    (if (= 1 components)
      (Arrays/fill out (aget coarse 0))
      (dotimes [i n] (dotimes [c components] (aset out (+ (* i components) c) (aget coarse c)))))
    out))

(defn- lod-evaluate
  "Evaluate the batch on the plan's samples and expand every field."
  [evaluate batch env lod]
  (let [coarse (:outputs (evaluate batch env (:samples lod)))]
    (into {} (map (fn [{:keys [attribute components spatial?]}]
                    (let [values (get-in coarse [attribute :values])]
                      [attribute (if spatial? (reconstruct values components lod)
                                     (fill-uniform values components (:pixel-count lod)))])))
          (:outputs batch))))

(defn- full-evaluate [batch env positions]
  (update-vals (:outputs (ir/evaluate-batch batch env positions)) :values))

(defn- adaptive-plan
  "Tables for the midpoint-checked adaptive variant along each detected run:
  every chunk has a left node, a right node and a middle pixel."
  [positions chunk]
  (let [chunks (vec (for [[start end] (runs positions)
                          :let [nodes (strip-nodes (- end start) chunk)]
                          k (range (max 1 (dec (count nodes))))]
                      (let [a (nth nodes k) b (nth nodes (min (inc k) (dec (count nodes))))]
                        {:base start :a a :b b :m (quot (+ a b) 2)})))
        keys (vec (distinct (mapcat (fn [{:keys [base a b m]}] [(+ base a) (+ base m) (+ base b)]) chunks)))
        index (zipmap keys (range))]
    {:samples (mapv #(nth positions %) keys)
     :positions positions
     :pixel-count (count positions)
     :chunks (mapv (fn [{:keys [base a b m]}]
                     {:base base :a a :b b :m m
                      :sa (index (+ base a)) :sm (index (+ base m)) :sb (index (+ base b))})
                   chunks)}))

(defn- adaptive-evaluate
  "Evaluate nodes and chunk midpoints. A chunk whose midpoint lies within
  `tolerance` of the line between its nodes, on every spatial component, is
  filled piecewise-linearly through node, midpoint, node. Every other chunk is
  evaluated on each of its pixels in a second call. Returns fields and the
  number of refined pixels."
  [evaluate batch env {:keys [samples positions chunks pixel-count]} tolerance]
  (let [tolerance (double tolerance)
        coarse (:outputs (evaluate batch env samples))
        fields (filterv :spatial? (:outputs batch))
        smooth? (fn [{:keys [a b m sa sm sb]}]
                  (let [t (/ (double (- (long m) (long a))) (- (long b) (long a)))]
                    (every? (fn [{:keys [attribute components]}]
                              (let [^doubles values (get-in coarse [attribute :values]) components (long components)]
                                (loop [c 0]
                                  (or (= c components)
                                      (let [va (aget values (+ (* (long sa) components) c))
                                            vb (aget values (+ (* (long sb) components) c))
                                            vm (aget values (+ (* (long sm) components) c))]
                                        (and (<= (Math/abs (- vm (+ va (* t (- vb va))))) tolerance)
                                             (recur (inc c))))))))
                            fields)))
        verdicts (mapv smooth? chunks)
        refined-pixels (vec (for [[chunk ok?] (map vector chunks verdicts) :when (not ok?)
                                  i (range (inc (long (:a chunk))) (long (:b chunk)))
                                  :when (not= i (:m chunk))]
                              (+ (long (:base chunk)) (long i))))
        fine (when (seq refined-pixels)
               (:outputs (evaluate batch env (mapv positions refined-pixels))))
        out (into {} (map (fn [{:keys [attribute components spatial?]}]
                            (let [^doubles values (get-in coarse [attribute :values]) components (long components)]
                              [attribute (if-not spatial?
                                           (fill-uniform values components pixel-count)
                                           (let [out (double-array (* (long pixel-count) components))
                                                 put! (fn [pixel sample]
                                                        (dotimes [c components]
                                                          (aset out (+ (* (long pixel) components) c)
                                                                (aget values (+ (* (long sample) components) c)))))
                                                 lerp! (fn [pixel s0 s1 t]
                                                         (let [t (double t)]
                                                           (dotimes [c components]
                                                             (let [v0 (aget values (+ (* (long s0) components) c))
                                                                   v1 (aget values (+ (* (long s1) components) c))]
                                                               (aset out (+ (* (long pixel) components) c) (+ v0 (* t (- v1 v0))))))))]
                                             (doseq [[{:keys [base a b m sa sm sb]} ok?] (map vector chunks verdicts)]
                                               (let [base (long base) a (long a) b (long b) m (long m)]
                                                 (put! (+ base a) sa) (put! (+ base m) sm) (put! (+ base b) sb)
                                                 (when ok?
                                                   (loop [i (inc a)]
                                                     (when (< i b)
                                                       (cond (< i m) (lerp! (+ base i) sa sm (/ (double (- i a)) (- m a)))
                                                             (> i m) (lerp! (+ base i) sm sb (/ (double (- i m)) (- b m))))
                                                       (recur (inc i)))))))
                                             (when fine
                                               (let [^doubles source (get-in fine [attribute :values])]
                                                 (dotimes [j (count refined-pixels)]
                                                   (let [pixel (long (nth refined-pixels j))]
                                                     (dotimes [c components]
                                                       (aset out (+ (* pixel components) c) (aget source (+ (* j components) c))))))))
                                             out))])))
                  (:outputs batch))]
    {:fields out :refined (count refined-pixels) :samples (count samples)}))

(defn- prototype-evaluator
  "Stand in for `ir/evaluate-batch` on batches of at least 256 targets that
  write only :color and :white and have no surface chart (no UV argument).
  `evaluate` is the evaluator it replaces. It refuses a spatial plan whose
  axes were not recorded by the wrapped `ir/compile-batch`.
  :dedup samples each distinct tuple of the coordinates read (exact).
  :linear interpolates between nodes `chunk` pixels apart on each run.
  :adaptive does the same and re-evaluates chunks that fail the midpoint test."
  [evaluate mode chunk tolerance]
  (let [plans (java.util.Collections/synchronizedMap (java.util.IdentityHashMap.))
        ;; Always the four-argument form: the three-argument form calls back
        ;; through the var and would re-enter this prototype on the samples.
        inner (fn [plan env positions] (evaluate plan env positions nil))]
    (fn [plan env positions & more]
      (if-not (and (vector? positions) (>= (count positions) 256) (nil? (first more))
                   (every? #{:color :white} (map :attribute (:outputs plan)))
                   (some :spatial? (:outputs plan)))
        (apply evaluate plan env positions more)
        (let [start (System/nanoTime)
              _ (when-not (seq (::axes plan))
                  ;; Never collapse a spatial field to one sample because its
                  ;; axes are unknown.
                  (throw (ex-info "A spatial batch plan has no recorded axes" {:outputs (map :attribute (:outputs plan))})))
              key [mode (::axes plan)]
              cache (or (.get plans positions) (do (.put plans positions (atom {})) (.get plans positions)))
              lod (or (get @cache key)
                      (let [built (case mode
                                    :dedup (dedup-plan positions (::axes plan))
                                    :linear (run-linear-plan positions chunk)
                                    :adaptive (adaptive-plan positions chunk))]
                        (swap! cache assoc key built)
                        built))
              fields (case mode
                       (:dedup :linear) (lod-evaluate inner plan env lod)
                       :adaptive (:fields (adaptive-evaluate inner plan env lod tolerance)))
              result {:count (count positions)
                      :outputs (into {} (map (fn [{:keys [attribute type components]}]
                                               [attribute {:type type :components components :values (get fields attribute)}]))
                                     (:outputs plan))}]
          (add-time! :lod-total (- (System/nanoTime) start))
          result)))))

;; ------------------------------------------------------------ error metrics

(defn- oklab
  "Linear RGB (sRGB primaries assumed) to OKLab."
  [^double r ^double g ^double b ^doubles out]
  (let [l (Math/cbrt (+ (* 0.4122214708 r) (* 0.5363325363 g) (* 0.0514459929 b)))
        m (Math/cbrt (+ (* 0.2119034982 r) (* 0.6806995451 g) (* 0.1073969566 b)))
        s (Math/cbrt (+ (* 0.0883024619 r) (* 0.2817188376 g) (* 0.6299787005 b)))]
    (aset out 0 (+ (* 0.2104542553 l) (* 0.7936177850 m) (* -0.0040720468 s)))
    (aset out 1 (+ (* 1.9779984951 l) (* -2.4285922050 m) (* 0.4505937099 s)))
    (aset out 2 (+ (* 0.0259040371 l) (* 0.7827717662 m) (* -0.8086757660 s)))
    out))

(defn- new-metrics []
  {:max-codes (double-array 1) :sum-codes (double-array 1) :count (long-array 1) :over-one (long-array 1)
   :delta-e (long-array 1001) :max-delta-e (double-array 1)
   :error-change (double-array 1) :reference-change (double-array 1) :max-error-change (double-array 1)
   :mislit (long-array 1) :edges (long-array 1) :previous (volatile! nil)})

(defn- accumulate!
  "Compare one frame of the colour field against the full-resolution field."
  [{:keys [^doubles max-codes ^doubles sum-codes ^longs count ^longs over-one ^longs delta-e ^doubles max-delta-e
           ^doubles error-change ^doubles reference-change ^doubles max-error-change ^longs mislit ^longs edges previous]}
   ^doubles reference ^doubles candidate columns]
  (let [n (quot (alength reference) 3) columns (long columns)
        a (double-array 3) b (double-array 3)
        [^doubles previous-reference ^doubles previous-candidate] @previous]
    (dotimes [i n]
      (let [base (* 3 i)
            worst (loop [c 0 worst 0.0]
                    (if (= c 3) worst
                        (let [r (aget reference (+ base c)) v (aget candidate (+ base c))
                              codes (Math/abs (- (Math/rint (* 255.0 r)) (Math/rint (* 255.0 v))))]
                          (aset sum-codes 0 (+ (aget sum-codes 0) codes))
                          (when previous-reference
                            (let [change (Math/abs (- (- v r) (- (aget previous-candidate (+ base c)) (aget previous-reference (+ base c)))))]
                              (aset error-change 0 (+ (aget error-change 0) change))
                              (aset reference-change 0 (+ (aget reference-change 0) (Math/abs (- r (aget previous-reference (+ base c))))))
                              (when (> change (aget max-error-change 0)) (aset max-error-change 0 change))))
                          (recur (inc c) (Math/max worst codes)))))
            worst (double worst)]
        (when (> worst (aget max-codes 0)) (aset max-codes 0 worst))
        (when (> worst 1.0) (aset over-one 0 (inc (aget over-one 0))))
        (oklab (aget reference base) (aget reference (+ base 1)) (aget reference (+ base 2)) a)
        (oklab (aget candidate base) (aget candidate (+ base 1)) (aget candidate (+ base 2)) b)
        (let [d (Math/sqrt (+ (Math/pow (- (aget a 0) (aget b 0)) 2.0) (Math/pow (- (aget a 1) (aget b 1)) 2.0)
                              (Math/pow (- (aget a 2) (aget b 2)) 2.0)))
              bin (int (Math/min 1000.0 (Math/floor (* 1000.0 d))))]
          (aset delta-e bin (inc (aget delta-e bin)))
          (when (> d (aget max-delta-e 0)) (aset max-delta-e 0 d)))
        ;; Lit/unlit disagreement on the first colour component, and how many
        ;; lit/unlit edges the reference has along its rows.
        (let [lit? (> (aget reference base) 0.5)]
          (when (not= lit? (> (aget candidate base) 0.5)) (aset mislit 0 (inc (aget mislit 0))))
          (when (and (pos? (rem i columns)) (not= lit? (> (aget reference (- base 3)) 0.5)))
            (aset edges 0 (inc (aget edges 0)))))))
    (aset count 0 (+ (aget count 0) n))
    (vreset! previous [reference candidate])))

(defn- quantile [^longs histogram q]
  (let [total (reduce + (seq histogram)) target (* (double q) total)]
    (loop [bin 0 seen 0]
      (let [seen (+ seen (aget histogram bin))]
        (if (or (>= seen target) (= bin 1000)) (/ bin 1000.0) (recur (inc bin) seen))))))

(defn- metrics-summary
  [{:keys [^doubles max-codes ^doubles sum-codes ^longs count ^longs over-one ^longs delta-e ^doubles max-delta-e
           ^doubles error-change ^doubles reference-change ^doubles max-error-change ^longs mislit ^longs edges]}]
  (let [n (double (aget count 0))]
    {:max-codes (aget max-codes 0)
     :mean-codes (/ (aget sum-codes 0) (* 3.0 n))
     :pixels-over-one-code (/ (aget over-one 0) n)
     :delta-e-ok {:p50 (quantile delta-e 0.5) :p99 (quantile delta-e 0.99) :max (aget max-delta-e 0)}
     ;; Frame-to-frame change of the error, relative to the reference's own
     ;; frame-to-frame change, and the largest single change in 8-bit codes.
     :shimmer-ratio (if (pos? (aget reference-change 0)) (/ (aget error-change 0) (aget reference-change 0)) 0.0)
     :max-error-change-codes (* 255.0 (aget max-error-change 0))
     :mislit-pixels-per-edge (when (pos? (aget edges 0)) (/ (double (aget mislit 0)) (aget edges 0)))}))

;; -------------------------------------------------- interval certification

(defn- interval
  "Enclose a scalar expression over a box of positions: [lo hi], or nil when
  this prototype has no rule for a node."
  [node env box]
  (let [point (fn [v] (when (number? v) [(double v) (double v)]))
        args (fn [] (let [xs (mapv #(interval % env box) (:args node))] (when (every? some? xs) xs)))
        monotone (fn [f] (when-let [[[lo hi]] (args)] [(f lo) (f hi)]))
        wave (fn [phase] ; sin over [lo hi] after adding `phase`
               (when-let [[[lo hi]] (args)]
                 (let [lo (+ (double lo) (double phase)) hi (+ (double hi) (double phase))]
                   (if (>= (- hi lo) (* 2.0 Math/PI)) [-1.0 1.0]
                       (let [a (Math/sin lo) b (Math/sin hi)
                             crosses? (fn [offset] (< (Math/floor (/ (- lo offset) (* 2.0 Math/PI)))
                                                     (Math/floor (/ (- hi offset) (* 2.0 Math/PI)))))]
                         [(if (crosses? (* 1.5 Math/PI)) -1.0 (min a b))
                          (if (crosses? (* 0.5 Math/PI)) 1.0 (max a b))])))))]
    (case (:op node)
      :const (point (:value node))
      :param (point (get-in env [:params (:key node)]))
      :beat (point (get env (if (= :absolute (:clock node)) :beat :elapsed-beats)))
      :bar (point (/ (double (get env (if (= :absolute (:clock node)) :beat :elapsed-beats)))
                     (double (get-in env [:meter :beats-per-bar]))))
      :time (point (get env (if (= :absolute (:clock node)) :time-ms :elapsed-ms)))
      :position (get box (:axis node))
      :add (when-let [xs (args)] [(reduce + (map first xs)) (reduce + (map second xs))])
      :subtract (when-let [xs (args)]
                  (if (= 1 (count xs)) (let [[lo hi] (first xs)] [(- hi) (- lo)])
                      (let [[[alo ahi] [blo bhi]] xs] [(- alo bhi) (- ahi blo)])))
      :multiply (when-let [xs (args)]
                  (if (and (= 2 (count xs)) (= (first (:args node)) (second (:args node))))
                    ;; A square: both factors are the same expression.
                    (let [[lo hi] (first xs)]
                      [(if (<= lo 0.0 hi) 0.0 (min (* lo lo) (* hi hi))) (max (* lo lo) (* hi hi))])
                  (reduce (fn [[alo ahi] [blo bhi]]
                            (let [products [(* alo blo) (* alo bhi) (* ahi blo) (* ahi bhi)]]
                              [(apply min products) (apply max products)])) xs)))
      :divide (when-let [[[alo ahi] [blo bhi]] (args)]
                (when (or (pos? blo) (neg? bhi))
                  (let [quotients [(/ alo blo) (/ alo bhi) (/ ahi blo) (/ ahi bhi)]]
                    [(apply min quotients) (apply max quotients)])))
      :min (when-let [xs (args)] [(apply min (map first xs)) (apply min (map second xs))])
      :max (when-let [xs (args)] [(apply max (map first xs)) (apply max (map second xs))])
      :sin (wave 0.0)
      :cos (wave (* 0.5 Math/PI))
      :abs (when-let [[[lo hi]] (args)]
             (cond (>= lo 0.0) [lo hi] (<= hi 0.0) [(- hi) (- lo)] :else [0.0 (max (- lo) hi)]))
      :floor (monotone #(Math/floor (double %)))
      :fract (when-let [[[lo hi]] (args)]
               (if (= (Math/floor (double lo)) (Math/floor (double hi)))
                 [(- lo (Math/floor (double lo))) (- hi (Math/floor (double hi)))]
                 [0.0 1.0]))
      :sqrt (when-let [[[lo hi]] (args)] (when (>= lo 0.0) [(Math/sqrt lo) (Math/sqrt hi)]))
      :exp (monotone #(Math/exp (double %)))
      :clamp (when-let [[[lo hi] [llo lhi] [hlo hhi]] (args)]
               ;; Only for bounds that are single values this frame.
               (when (and (= llo lhi) (= hlo hhi))
                 [(min hhi (max llo lo)) (min hhi (max llo hi))]))
      nil)))

(defn- component-expressions
  "Scalar component expressions of a vec3 colour, or nil when it is not built from scalars."
  [expression]
  (when (= :vec3 (:op expression)) (:args expression)))

(defn- certified-constant
  "Enclose each colour component over every row chunk for one frame and check
  the enclosures against that frame's full-resolution field `reference`.

  Returns the fraction of pixels in chunks whose components provably vary by
  at most `tolerance` (certified), the fraction in chunks whose enclosures
  have zero width (exactly constant), how many sampled values fall outside
  their enclosure (a sound rule set gives zero) and, for certified chunks
  filled with their first pixel's value, the largest difference in 8-bit
  codes from the reference."
  [expression env rig columns rows chunk tolerance ^doubles reference]
  (when-let [components (component-expressions expression)]
    (let [results (for [row (range rows) start (range 0 columns chunk)]
                    (let [end (min columns (+ start chunk))
                          a (:position (nth rig (+ (* row columns) start)))
                          b (:position (nth rig (+ (* row columns) (dec end))))
                          box {:x [(min (nth a 0) (nth b 0)) (max (nth a 0) (nth b 0))]
                               :y [(min (nth a 1) (nth b 1)) (max (nth a 1) (nth b 1))]
                               :z [(min (nth a 2) (nth b 2)) (max (nth a 2) (nth b 2))]}
                          enclosures (mapv #(interval % env box) components)
                          supported? (every? some? enclosures)
                          widths (when supported? (map (fn [[lo hi]] (- hi lo)) enclosures))
                          certified? (boolean (and supported? (every? #(<= % tolerance) widths)))
                          values (when supported?
                                   (for [i (range start end) c (range 3)]
                                     (let [v (aget reference (+ (* 3 (+ (* row columns) i)) c))
                                           [lo hi] (nth enclosures c)
                                           first-value (aget reference (+ (* 3 (+ (* row columns) start)) c))]
                                       {:outside? (not (<= (- lo 1e-12) v (+ hi 1e-12)))
                                        :fill-codes (Math/abs (- (Math/rint (* 255.0 v)) (Math/rint (* 255.0 first-value))))})))]
                      {:pixels (- end start)
                       :supported? supported?
                       :certified? certified?
                       :exact? (boolean (and supported? (every? zero? widths)))
                       :checked (count values)
                       :outside (count (filter :outside? values))
                       :fill-codes (if certified? (reduce max 0.0 (map :fill-codes values)) 0.0)}))]
      (when (every? :supported? results)
        (let [pixels (reduce + (map :pixels results))
              fraction (fn [pred] (/ (double (reduce + (map :pixels (filter pred results)))) pixels))]
          {:chunks (count results)
           :certified-chunks (count (filter :certified? results))
           :certified-pixel-fraction (fraction :certified?)
           :exact-pixel-fraction (fraction :exact?)
           :checked-values (reduce + (map :checked results))
           :values-outside-enclosure (reduce + (map :outside results))
           :certified-fill-max-codes (reduce max 0.0 (map :fill-codes results))})))))

;; ------------------------------------------------------------------ LOD run

(defn- family-batch [rig family]
  (let [d (definition rig family)
        plan (compiler/compile-plan d)
        expressions (into {} (map (juxt :attribute :expression)) (:outputs plan))
        params ((gradient/expander (:gradients plan)) (registry/resolve-accepted-params (:schemas plan) (:defaults plan) {}))]
    {:batch (ir/compile-batch expressions) :expressions expressions :params params
     :axes (vec (sort (into #{} (comp (mapcat #(tree-seq (fn [n] (and (map? n) (vector? (:args n)))) :args %))
                                      (filter #(= :position (:op %))) (map :axis))
                            (vals expressions))))}))

(defn- environment [params i]
  {:time-ms (* 25.0 (inc i)) :elapsed-ms (* 25.0 (inc i)) :params params
   :beat (beat-at i) :elapsed-beats (beat-at i)
   :meter (time/normalize-meter {:beats-per-bar 4 :bars-per-phrase 4})})

(defn lod-run
  "Error, shimmer and time of every method and chunk size for every family."
  [{:keys [warmup samples columns chunks tolerance only timing?] :or {timing? true}} pixels]
  (let [rig (matrix-rig pixels columns)
        rows (quot pixels columns)
        positions (mapv :position rig)
        frames (range warmup (+ warmup samples))
        methods (concat
                 (for [chunk chunks]
                   [{:method :nearest-centre :chunk chunk} #(centre-plan rig columns rows chunk false)])
                 (for [chunk chunks]
                   [{:method :linear-centre :chunk chunk} #(centre-plan rig columns rows chunk true)])
                 (for [chunk chunks]
                   [{:method :linear-nodes :chunk chunk} #(node-plan rig columns rows chunk :linear)])
                 (for [chunk chunks]
                   [{:method :cubic-nodes :chunk chunk} #(node-plan rig columns rows chunk :cubic)])
                 (for [chunk chunks]
                   [{:method :bilinear-nodes :chunk chunk} #(bilinear-plan rig columns rows chunk)]))
        plans (mapv (fn [[label build]] (assoc label :plan (build))) methods)
        adaptive (mapv (fn [chunk] {:chunk chunk :plan (adaptive-plan positions chunk)}) chunks)
        time-frames (fn [f] (median (mapv (fn [i] (let [start (System/nanoTime)] (f i) (- (System/nanoTime) start))) frames)))]
    {:pixels pixels :columns columns :rows rows :frames samples :tolerance tolerance
     :families
     (vec
      (for [family (families rows) :when (or (nil? only) (contains? only (:id family)))]
        (let [{:keys [batch expressions params axes]} (family-batch rig family)
              env #(environment params %)
              spatial? (boolean (some :spatial? (:outputs batch)))
              dedup (when (and spatial? (seq axes)) (dedup-plan positions axes))
              _ (dotimes [i warmup]
                  (full-evaluate batch (env i) positions)
                  (lod-evaluate ir/evaluate-batch batch (env i) (:plan (first plans))))
              full-nanos (time-frames #(full-evaluate batch (env %) positions))
              references (mapv #(:color (full-evaluate batch (env %) positions)) frames)
              measure (fn [evaluate]
                        (let [metrics (new-metrics)]
                          (dotimes [i 20] (evaluate (env (+ warmup i))))
                          ;; Time the full field and the candidate back to back
                          ;; on every frame and keep the median ratio, so load
                          ;; from other processes cancels out of the speedup.
                          (let [pairs (when timing?
                                        (mapv (fn [i]
                                                (let [e (env i)
                                                      t0 (System/nanoTime)
                                                      _ (full-evaluate batch e positions)
                                                      t1 (System/nanoTime)
                                                      _ (evaluate e)
                                                      t2 (System/nanoTime)]
                                                  [(- t1 t0) (- t2 t1)]))
                                              frames))]
                            (doseq [[i reference] (map vector frames references)]
                              (accumulate! metrics reference (:color (evaluate (env i))) columns))
                            (cond-> (metrics-summary metrics)
                              timing? (assoc :ns-per-pixel (/ (double (median (map second pairs))) pixels)
                                             :speedup (median (map (fn [[full candidate]] (/ (double full) candidate)) pairs)))))))]
          (binding [*out* *err*] (println "LOD family" (:id family)))
          {:id (:id family) :smooth? (:smooth? family) :spatial? spatial? :axes axes
           :full-ns-per-pixel (/ (double full-nanos) pixels)
           :dedup (when dedup
                    (assoc (measure #(lod-evaluate ir/evaluate-batch batch % dedup)) :samples (:sample-count dedup)))
           :fixed (when spatial?
                    (vec (for [{:keys [method chunk plan]} plans]
                           (assoc (measure #(lod-evaluate ir/evaluate-batch batch % plan))
                                  :method method :chunk chunk :samples (:sample-count plan)))))
           :adaptive (when spatial?
                       (vec (for [{:keys [chunk plan]} adaptive]
                              (let [refined (volatile! 0)
                                    result (measure #(:fields (adaptive-evaluate ir/evaluate-batch batch % plan tolerance)))
                                    _ (doseq [i frames] (vswap! refined + (:refined (adaptive-evaluate ir/evaluate-batch batch (env i) plan tolerance))))]
                                (assoc result :chunk chunk :samples (count (:samples plan))
                                       :refined-pixel-fraction (/ (double @refined) (* (double samples) pixels)))))))
           :certified-constant (when spatial?
                                 (vec (for [chunk chunks]
                                        (let [per-frame (keep (fn [[i reference]]
                                                                (certified-constant (:color expressions) (env i) rig columns rows chunk tolerance reference))
                                                              (take-nth 8 (map vector frames references)))
                                              mean (fn [k] (/ (reduce + (map k per-frame)) (count per-frame)))]
                                          (when (seq per-frame)
                                            {:chunk chunk
                                             :frames-checked (count per-frame)
                                             :certified-pixel-fraction (mean :certified-pixel-fraction)
                                             :exact-pixel-fraction (mean :exact-pixel-fraction)
                                             :checked-values (reduce + (map :checked-values per-frame))
                                             :values-outside-enclosure (reduce + (map :values-outside-enclosure per-frame))
                                             :certified-fill-max-codes (reduce max 0.0 (map :certified-fill-max-codes per-frame))})))))})))}))

;; --------------------------------------------------------------------- main

(defn- options [args]
  (when (odd? (count args)) (throw (ex-info "Every option requires a value" {})))
  (let [ints #(mapv parse-long (str/split % #","))]
    (reduce (fn [m [k v]]
              (case k
                "--scope" (assoc m :scope (keyword v))
                "--pixels" (assoc m :pixels (ints v))
                "--warmup" (assoc m :warmup (parse-long v))
                "--samples" (assoc m :samples (parse-long v))
                "--chunks" (assoc m :chunks (ints v))
                "--columns" (assoc m :columns (parse-long v))
                "--tolerance-codes" (assoc m :tolerance (/ (parse-double v) 255.0))
                "--stage-workers" (assoc m :stage-workers (parse-long v))
                "--families" (assoc m :only (set (map keyword (str/split v #","))))
                "--e2e-chunk" (assoc m :e2e-chunk (parse-long v))
                "--timing" (assoc m :timing? (parse-boolean v))
                "--show" (assoc m :show (symbol v))
                "--timeline" (assoc m :timeline (symbol v))
                "--output" (assoc m :output v)
                (throw (ex-info "Unknown option" {:option k}))))
            {:scope :stages :pixels [1000 10000 20000] :warmup 200 :samples 400 :chunks [2 4 8 16 32]
             :columns 100 :tolerance (/ 1.0 255.0) :stage-workers 1
             ;; The mixed rig's other fixtures and layer count.
             :dimmers 48 :pars 48 :movers 16 :layers 16}
            (partition 2 args))))

(defn -main [& args]
  (let [opts (options args)
        afterglow (or (System/getenv "AFTERGLOW_ROOT") "../afterglow")
        report {:benchmark :led-spatial-lod :scope (:scope opts) :measured-at (str (java.time.Instant/now))
                :source-revisions {:tolglow (bench/revision ".") :afterglow (bench/revision afterglow)}
                :source-working-trees {:tolglow (bench/source-status ".") :afterglow (bench/source-status afterglow)}
                :environment {:java (System/getProperty "java.version") :clojure (clojure-version)
                              :processors (.availableProcessors (Runtime/getRuntime))
                              :heap-bytes (.maxMemory (Runtime/getRuntime))
                              :jvm-arguments (vec (.getInputArguments (ManagementFactory/getRuntimeMXBean)))
                              :cpu-model (bench/cpu-model)
                              :load-average (.getSystemLoadAverage (ManagementFactory/getOperatingSystemMXBean))}
                :options (dissoc opts :output)
                :results (vec (for [pixels (if (:show opts) [nil] (:pixels opts))]
                                (do (binding [*out* *err*] (println "LED LOD" (:scope opts) pixels))
                                    (case (:scope opts)
                                      :stages (stage-run opts pixels)   ; mixed rig, or the show named by --show
                                      :hidden (hidden-run opts pixels)
                                      :e2e (let [reference (stage-run (assoc opts :eval-mode :full :keep-packets? true) pixels)]
                                             (into [(dissoc reference :packets :plans)]
                                                   (for [mode [:dedup :linear :adaptive]]
                                                     (dissoc (stage-run (assoc opts :eval-mode mode :reference (:packets reference)) pixels)
                                                             :plans))))
                                      :families (family-run opts pixels)
                                      :lod (lod-run opts pixels)))))}]
    (if-let [file (:output opts)]
      (do (io/make-parents file)
          (with-open [writer (io/writer file)]
            (binding [*out* writer pprint/*print-right-margin* 160] (pprint/pprint report)))
          (println "Wrote" file))
      (pprint/pprint report))))
