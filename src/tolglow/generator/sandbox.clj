(ns tolglow.generator.sandbox
  "Evaluate model-written generator source with SCI so that it can only build
  and return effect DSL data (#855). The evaluator has no Java interop beyond
  pure `Math` functions, no IO, clock, threads, `eval`/`load-string`, var
  reflection or var mutation, and its randomness is a SplittableRandom seeded
  from the request. Every interpreted fn or loop entry and every element a lazy
  producer touches passes an uncatchable step, deadline and allocation check.

  This namespace evaluates in the calling thread and JVM. The application runs
  it only inside the separate worker process (`tolglow.generator.worker`),
  whose heap bound and kill switch cover compiled code that never re-enters the
  interpreter. Never call it on the lighting frame path."
  (:require [sci.core :as sci]
            [sci.interrupt :as interrupt]
            [sci.impl.opts :as sci-opts]
            [tolglow.effects]
            [tolglow.generator.limits :as limits])
  (:import [java.lang.management ManagementFactory]
           [java.util SplittableRandom]))

(def ^:private host-access
  "Core vars refused at analysis. SCI's default environment already lacks
  slurp/spit, futures, agents and host classes; these are the remaining routes
  to evaluation, var reflection, var mutation, blocking, input and identity
  dependent symbols."
  '[eval load-string load-file load load-reader read read-string read-line
    clojure.edn/read clojure.edn/read-string
    resolve ns-resolve requiring-resolve find-var var-get intern
    ns-publics ns-interns ns-map ns-refers ns-aliases ns-imports all-ns find-ns the-ns
    ns-unmap ns-unalias remove-ns create-ns
    alter-var-root var-set set! with-redefs with-redefs-fn alter-meta! reset-meta!
    push-thread-bindings pop-thread-bindings with-bindings with-bindings*
    locking promise deliver time gensym
    make-array object-array int-array long-array double-array float-array short-array
    byte-array char-array boolean-array into-array to-array to-array-2d aclone])

(defn- namespace-publics [namespace]
  (sci/eval-string* (sci/init {}) (str "(vec (keys (ns-publics '" namespace ")))")))

(def ^:private denied
  "clojure.repl's source functions read the file named in a var's metadata."
  (vec (concat host-access (map #(symbol "clojure.repl" (name %)) (namespace-publics 'clojure.repl)))))

(def ^:private closed-classes
  "SCI's default classes with every member closed: constructing a String or an
  exception is harmless; calling a method on one is interop."
  (into {} (for [[k v] sci-opts/default-classes]
             [k (assoc (if (map? v) v {:class v}) :closed true)])))

(def ^:private math
  "java.lang.Math without random(): static calls dispatch to these pure functions."
  {:class Math :closed true
   :static-methods (into {} (for [[name f] {'sin #(Math/sin %) 'cos #(Math/cos %) 'tan #(Math/tan %)
                                            'asin #(Math/asin %) 'acos #(Math/acos %) 'atan #(Math/atan %)
                                            'atan2 #(Math/atan2 %1 %2) 'sqrt #(Math/sqrt %) 'cbrt #(Math/cbrt %)
                                            'pow #(Math/pow %1 %2) 'exp #(Math/exp %) 'log #(Math/log %)
                                            'log10 #(Math/log10 %) 'hypot #(Math/hypot %1 %2)
                                            'floor #(Math/floor %) 'ceil #(Math/ceil %) 'round #(Math/round (double %))
                                            'abs #(if (integer? %) (Math/abs (long %)) (Math/abs (double %)))
                                            'signum #(Math/signum (double %))
                                            'min min 'max max 'toRadians #(Math/toRadians %) 'toDegrees #(Math/toDegrees %)}]
                              [name (fn [_class & args] (apply f args))]))
   :static-fields {'PI (fn [_] Math/PI) 'E (fn [_] Math/E)}})

(defn- effect-builders []
  (into {} (for [[sym v] (ns-publics 'tolglow.effects)] [sym @v])))

(defn- seeded [seed]
  (let [rng (SplittableRandom. (long seed))
        unit #(.nextDouble rng)]
    {'rand (fn ([] (unit)) ([n] (* n (unit))))
     'rand-int (fn [n] (int (* n (unit))))
     'rand-nth (fn [coll] (nth coll (int (* (count coll) (unit)))))
     'shuffle (fn [coll]
                (let [v (transient (vec coll))]
                  (loop [i (dec (count v)) v v]
                    (if (pos? i)
                      (let [j (.nextInt rng (inc i)) x (v i)]
                        (recur (dec i) (assoc! (assoc! v i (v j)) j x)))
                      (persistent! v)))))
     'random-sample (fn ([prob] (filter (fn [_] (< (unit) prob))))
                      ([prob coll] (filter (fn [_] (< (unit) prob)) coll)))}))

(defn- allocated-bytes-fn []
  (let [bean (ManagementFactory/getThreadMXBean)]
    (if (and (instance? com.sun.management.ThreadMXBean bean)
             (.isThreadAllocatedMemorySupported ^com.sun.management.ThreadMXBean bean))
      #(.getCurrentThreadAllocatedBytes ^com.sun.management.ThreadMXBean bean)
      (constantly 0))))

(defn- limit! [limit message]
  (interrupt/interrupt! message {::limit limit}))

(defn- guard
  "SCI :interrupt-fn. The step count is deterministic; the clock, allocation
  and thread-interrupt checks run every 16 steps, so a short loop that doubles
  a string each pass is still caught early."
  [{:keys [steps deadline-ms allocated-bytes]}]
  (let [counter (volatile! 0)
        allocated (allocated-bytes-fn)
        base (allocated)
        deadline (+ (System/nanoTime) (* 1000000 (long deadline-ms)))]
    (fn []
      (let [n (vswap! counter inc)]
        (when (> n steps)
          (limit! :steps (str "Generator exceeded " steps " evaluation steps (a loop that does not end?)")))
        (when (zero? (bit-and n 15))
          (when (> (System/nanoTime) deadline)
            (limit! :deadline (str "Generator ran past its deadline and was stopped after " deadline-ms " ms")))
          (when (> (- (allocated) base) allocated-bytes)
            (limit! :memory (str "Generator allocated more than " (quot allocated-bytes (* 1024 1024)) " MB")))
          (when (Thread/interrupted)
            (limit! :deadline "Generator was interrupted")))))))

(defn- context [seed limits]
  (sci/init {:interrupt-fn (guard limits)
             :deny denied
             :classes (assoc closed-classes 'java.lang.Math math)
             :imports {'Math 'java.lang.Math}
             :aliases {'fx 'tolglow.effects 'str 'clojure.string 'set 'clojure.set}
             :namespaces {'clojure.core (merge interrupt/clojure-core (seeded seed))
                          'clojure.string interrupt/clojure-string
                          'tolglow.effects (effect-builders)
                          'user {'seed seed}}}))

(defn- causes [^Throwable e]
  (take-while some? (iterate #(.getCause ^Throwable %) e)))

(defn- location [^Throwable e]
  (some #(let [{:keys [line column]} (ex-data %)] (when line {:line line :column column})) (causes e)))

(def ^:private hint
  "Generators build data with the fx builders (tolglow.effects), pure core
  functions, clojure.string/set, Math and the seeded rand functions. They have
  no Java interop, IO, clock, threads, eval or var mutation.")

(defn- failure [stage e & {:keys [limit message]}]
  (let [chain (causes e)
        limit-data (some #(when (::limit (ex-data %)) %) chain)
        builder (some #(when (seq (:errors (ex-data %))) (ex-data %)) chain)
        where (location e)]
    (cond
      limit-data {:stage :sandbox :limit (::limit (ex-data limit-data))
                  :errors [(merge {:path [] :message (ex-message limit-data)} where)]}
      builder {:stage :sandbox :errors (mapv #(merge % where) (:errors builder))}
      :else (let [text (or message (ex-message e) (.getName (class e)))]
              (cond-> {:stage stage
                       :errors [(cond-> (merge {:path [] :message text} where)
                                  (re-find #"Unable to resolve|not allowed" text) (assoc :hint hint))]}
                limit (assoc :limit limit))))))

(defn evaluate
  "Evaluate generator `source` with explicit integer `seed`. Returns
  {:status :ok :value definition :steps n} or {:status :refused :stage s
  :errors [{:path :message :line :column}]} with :limit when a bound stopped
  it (:steps, :deadline, :memory or :stack) and :fatal true after an
  OutOfMemoryError, after which the hosting worker process should exit. The
  last form's value must be one effect definition map of portable data."
  [source seed limit-overrides]
  (let [limits (limits/limits limit-overrides)]
    (cond
      (not (string? source)) {:status :refused :stage :request :errors [{:path [:source] :message "Generator source must be a string"}]}
      (> (count source) (:source-chars limits))
      {:status :refused :stage :request :errors [{:path [:source] :message (str "Generator source is limited to " (:source-chars limits) " characters")}]}
      (not (and (integer? seed) (<= Long/MIN_VALUE seed Long/MAX_VALUE)))
      {:status :refused :stage :request :errors [{:path [:seed] :message "A generator needs an explicit integer seed"}]}
      :else
      (try
        (let [ctx (context seed limits)
              value (sci/binding [sci/out (java.io.Writer/nullWriter)
                                  sci/err (java.io.Writer/nullWriter)
                                  sci/in (java.io.StringReader. "")]
                      (sci/eval-string* ctx source))
              problems (if (map? value)
                         (limits/portable-errors value limits)
                         [{:path [] :message (str "A generator returns one effect definition map, not "
                                                  (if (nil? value) "nil" (.getName (class value))))}])]
          (if (seq problems)
            {:status :refused :stage :result :errors (vec problems)}
            {:status :ok :value value}))
        (catch Throwable e
          (let [chain (causes e)]
            (cond
              (some #(instance? OutOfMemoryError %) chain)
              (assoc (failure :sandbox e :limit :memory :message "Generator exhausted the worker heap") :status :refused :fatal true)
              (some #(instance? StackOverflowError %) chain)
              (assoc (failure :sandbox e :limit :stack :message "Generator recursion is too deep") :status :refused)
              (instance? Exception e) (assoc (failure :sandbox e) :status :refused)
              :else (throw e))))))))
