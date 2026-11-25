(ns tolglow.autopilot.signal
  "Portable scalar bindings for performance controls. Registered waveforms and
  arithmetic share the effect compiler's numeric expressions and explicit clock."
  (:require [afterglow.control.system :as system]
            [afterglow.render.ir :as ir]
            [afterglow.time :as time]
            [clojure.string :as str]
            [tolglow.automation.random :as random]
            [tolglow.oscillator :as oscillator]
            [tolglow.registry :as registry]
            [tolglow.signals :as signals]
            [tolglow.waveforms :as waveforms]))

(defn- id? [x] (or (keyword? x) (and (string? x) (not (str/blank? x)))))

(def ^:private builtin-registry (signals/new-registry))

(def ^:private builtin-functions
  (update-vals (:waveforms builtin-registry) #(ir/compile-expression (:expression %))))

(def ^:private arities {:add 2 :subtract 2 :multiply 2 :divide 2 :min 2 :max 2 :abs 1 :mix 3 :clamp 3})

(def ^:private arguments [:a :b :c])

(def ^:private operators
  (into {} (for [[op n] arities]
             [op (ir/compile-expression {:op op :args (mapv #(hash-map :op :param :key %) (take n arguments))})])))

(defn- positive-duration? [duration]
  (and (empty? (time/validate-duration duration)) (pos? (val (first duration)))))

(defn- identifier-path? [path]
  (and (vector? path) (<= 1 (count path) 32) (every? id? path)))

(defn- timed-source? [spec child?]
  (and (if (contains? spec :phase-reference)
         (and (not (contains? spec :phase)) (oscillator/phase-references (:phase-reference spec))
              (oscillator/musical-period? (:period spec {:bars 1})))
         (and (#{:local :global} (:phase spec :global)) (positive-duration? (:period spec {:bars 1}))))
       (child? (:offset spec 0))
       (let [range (:range spec [0 1])]
         (and (vector? range) (= 2 (count range)) (every? child? range)))))

(defn- source? [spec depth]
  (let [child? #(source? % (inc depth))]
    (cond
      (> depth 16) false
      (time/finite-number? spec) true
      (not (and (map? spec) (time/finite-number? (:default spec 0)))) false
      (:variable spec) (and (every? #{:variable :default} (keys spec)) (identifier-path? (:variable spec)))
      (:signal spec) (and (every? #{:signal :default} (keys spec)) (id? (:signal spec)))
      (:waveform spec)
      (and (every? #{:waveform :version :phase :phase-reference :period :offset :range :params :default} (keys spec))
           (id? (:waveform spec)) (or (nil? (:version spec)) (pos-int? (:version spec)))
           (timed-source? spec child?)
           (let [params (:params spec {})]
             (and (map? params) (<= (count params) 32) (every? keyword? (keys params)) (every? child? (vals params)))))
      (:random spec) (and (every? #{:random :phase :period :offset :range :default} (keys spec))
                          (map? (:random spec)) (every? #{:seed :distribution} (keys (:random spec)))
                          (integer? (get-in spec [:random :seed]))
                          (#{:uniform :triangular} (get-in spec [:random :distribution] :uniform))
                          (timed-source? spec child?))
      (:cue-id spec) (and (every? #{:cue-id :param :default} (keys spec)) (id? (:cue-id spec)) (id? (:param spec)))
      (:op spec) (and (every? #{:op :args :default} (keys spec)) (vector? (:args spec))
                      (= (get arities (:op spec)) (count (:args spec))) (every? child? (:args spec)))
      :else false)))

(defn valid? [spec] (boolean (source? spec 0)))

(defn valid-bank?
  "A named signal bank can refer to its peers. Missing and cyclic references
  are rejected at installation. Lookups are still guarded during hot reload."
  [bank]
  (when (and (map? bank) (<= (count bank) 128) (every? id? (keys bank)) (every? valid? (vals bank)))
    (let [links (update-vals bank #(into #{} (keep (fn [x] (when (map? x) (:signal x)))) (tree-seq coll? seq %)))]
      (loop [remaining links resolved #{}]
        (if (empty? remaining) true
          (let [ready (keep (fn [[name refs]] (when (every? resolved refs) name)) remaining)]
            (if (seq ready) (recur (apply dissoc remaining ready) (into resolved ready)) false)))))))

(defn prepare
  "Optional pure cache of compiled custom waveform functions. Retain at the
  runtime boundary and pass as :prepared to value. Never put this in a show file."
  [previous waveform-registry]
  (let [registry (or waveform-registry builtin-registry)]
    (if (identical? registry (:registry previous)) previous
      {:registry registry
       :functions (into {}
                        (for [[key definition] (:versions registry)]
                          [key (if (= definition (get-in builtin-registry [:versions key]))
                                 (get builtin-functions (:id definition))
                                 (ir/compile-expression (:expression definition)))]))})))

(defn cue-parameter
  "Read the local or latest running activation's typed input. Spatial links can
  supply a committed field view to resolve cross-volume feedback in one pass."
  [model options {:keys [cue-id param]}]
  (when-let [instance (or (let [selected (get-in model [:instances (:instance-id options)])]
                            (when (= cue-id (:cue-id selected)) selected))
                          (reduce (fn [found instance]
                                (if (and (= cue-id (:cue-id instance)) (= :running (:phase instance))
                                         (or (nil? found) (> (:order instance 0) (:order found 0))))
                                  instance found))
                              nil (vals (:instances model))))]
    (let [view (:modulation-view options)
          overrides (:linked-overrides view (:overrides view))
          params (if (seq (:responses model))
                   (waveforms/effective-params {:model model :resolved-param-overrides overrides} instance)
                   (merge (:params instance) (get overrides (:id instance))))]
      (get params param (get-in instance [:definition :params param :default])))))

(defn- numeric-or [n fallback] (double (if (time/finite-number? n) n fallback)))

(defn- musical-phase [model options spec]
  (let [reference (:phase-reference spec)
        clock (time/frame-clock model)
        instance (when (= reference :launch) (get-in model [:instances (:instance-id options)]))
        origin (if instance {:beat (:started-at-beat instance) :meter (:timing-meter instance)} (:start-clock options))
        private-beat (:beat (if instance (system/private-clock instance clock) clock))
        elapsed (when (and (time/finite-number? private-beat) (time/finite-number? (:beat origin)))
                  (- private-beat (:beat origin)))]
    (when (oscillator/phase-references reference)
      (oscillator/phase (merge {:period {:bars 1}} spec) clock elapsed (:meter origin)))))

(defn- phase-coordinate [model options spec]
  (let [clock (time/frame-clock model)
        local? (= :local (:phase spec))
        instance (when local? (get-in model [:instances (:instance-id options)]))
        origin (if instance {:time-ms (:started-at-ms instance) :beat (:started-at-beat instance)}
                   (:start-clock options))
        clock (if instance (system/private-clock instance clock) clock)
        {:keys [unit value]} (time/normalize-duration (:period spec {:bars 1}) (:meter clock))
        key (if (= :ms unit) :time-ms :beat)
        now (get clock key)
        start (if local? (get origin key) 0)]
    (when (and (time/finite-number? now) (time/finite-number? start))
      (/ (- (double now) (double start)) (double value)))))

(defn- scaled [spec sample fraction]
  (let [[lo hi] (mapv sample (:range spec [0 1]))]
    (+ lo (* fraction (- hi lo)))))

(defn- held-random [model options spec sample]
  (when-let [phase (phase-coordinate model options spec)]
    (let [step (long (Math/floor (+ phase (sample (:offset spec 0)))))
          seed (get-in spec [:random :seed])
          [next-seed x] (random/next-random (hash [seed step]))
          fraction (if (= :triangular (get-in spec [:random :distribution]))
                     (/ (+ x (second (random/next-random next-seed))) 2.0) x)]
      (scaled spec sample fraction))))

(defn- waveform-value [model options spec sample]
  (let [bank (or (:waveforms model) builtin-registry)
        id (:waveform spec)
        definition (if-let [version (:version spec)] (get-in bank [:versions [id version]]) (get-in bank [:waveforms id]))
        phase (if (contains? spec :phase-reference) (musical-phase model options spec) (phase-coordinate model options spec))]
    (when (and definition phase (every? (:params definition) (keys (:params spec))))
      (let [params (reduce-kv (fn [values k schema]
                                (let [value (if-let [binding (get (:params spec) k)] (sample binding) (:default schema))
                                      value (if (= :integer (:type schema)) (Math/round (double value)) value)]
                                  (assoc values k (if (registry/valid-value? schema value) value (:default schema)))))
                              {} (:params definition))
            key [id (:version definition)]
            evaluate (or (when (identical? bank (get-in options [:prepared :registry]))
                           (get-in options [:prepared :functions key]))
                         (when (= (:expression definition) (get-in builtin-registry [:versions key :expression]))
                           (get builtin-functions id))
                         (ir/compile-expression (:expression definition)))
            value (evaluate {:params (assoc params signals/phase-input (+ phase (sample (:offset spec 0))))})
            [a b] (:range definition)
            fraction (if (= a b) 0.0 (max 0.0 (min 1.0 (/ (- value a) (- b a)))))]
        (scaled spec sample fraction)))))

(defn value
  "Resolve a numeric binding using the committed show. A local waveform uses
  :start-clock, or an :instance-id for its private, pause-aware launch clock.
  Musical :phase-reference :launch captures the activation meter, and
  :transport follows the shared beat and current meter.
  Named signals use [:settings :autopilot :signals] and a shared show phase.
  :default (zero if omitted) handles missed inputs or retired registry entries."
  ([model spec] (value model spec {}))
  ([model spec options]
   (letfn [(read-source [spec options seen depth]
             (if (number? spec) (numeric-or spec 0)
               (let [fallback (numeric-or (:default spec) 0)
                     sample #(read-source % options seen (inc depth))]
                 (if (> depth 32) fallback
                   (try
                     (numeric-or
                      (cond
                        (:variable spec) (get-in (:variables model) (:variable spec))
                        (:waveform spec) (waveform-value model options spec sample)
                        (:random spec) (held-random model options spec sample)
                        (:cue-id spec) (cue-parameter model options spec)
                        (:signal spec)
                        (let [id (:signal spec) source (get-in model [:settings :autopilot :signals id])]
                          (when (and source (not (seen id)))
                            (read-source source (assoc (dissoc options :instance-id) :start-clock {:time-ms 0 :beat 0})
                                         (conj seen id) (inc depth))))
                        (:op spec) (when-let [op (get operators (:op spec))]
                                     (op {:params (zipmap arguments (map sample (:args spec)))})))
                      fallback)
                     (catch clojure.lang.ExceptionInfo _ fallback)
                     (catch ArithmeticException _ fallback))))))]
     (read-source spec options #{} 0))))
