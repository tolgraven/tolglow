(ns tolglow.show.programs
  "Compiled effect programs for one frame owner (#774).

  The engine compiles one program per definition and launch target set. With
  an owned worker, programs are compiled ahead of need, off the frame:

  - a warm tier holds every registered effect's untargeted program, each
    cue's fixture selection, each recorded live-run take's targets and each
    scheduled quantized launch, recomputed when the registry, rig, takes or
    schedule change;
  - a pending hot reload requests the programs its instances need and is
    adopted, by all of them together, once they are ready;
  - a rig edit prepares a new context whose programs for the running
    instances are compiled ahead of every other compile (`prepare!`); the
    owner adopts that context with the new rig once they are ready (#811).

  A first launch never waits: if its program is not ready the frame compiles
  it, so it renders on its start. Without a worker, as in an unscheduled
  runtime or an audition, `request!` compiles at once on the owner."
  (:require [afterglow.control.params :as params]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.faults :as runtime-faults]
            [tolglow.modulation :as modulation])
  (:import [java.nio.channels ClosedByInterruptException]
           [java.util.concurrent CancellationException ExecutorService PriorityBlockingQueue
            RejectedExecutionException ThreadFactory ThreadPoolExecutor TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))

(def cache-size
  "Programs per cache generation."
  256)

(defn context
  "A cache for `compiler`. `submit` runs a task off the frame owner, or is nil
  to compile requested programs on the caller's thread."
  [compiler submit]
  {:compiler compiler :submit submit
   :cache (atom {}) :retired (atom {})
   ;; Written by the worker, read and drained by the owner.
   :ready (atom {}) :warm (atom {}) :queued (atom #{}) :cancelled (atom #{})
   ;; Written by the worker before the owner adopts this context for a rig
   ;; edit (#811); a frame's request never drops them.
   :prepared (atom {})
   ;; True once the worker refused a task, so prepared work can never finish.
   :rejected (atom false)
   ;; Consecutive transient failures per program: {program {:failures n :error class}}.
   :transient-failures (atom {})
   :wanted (atom #{}) :warm-wanted (atom #{}) :live (atom true)
   ;; Owner only: what the warm tier was last built from.
   :warm-key (atom ::none) :scheduled-seen (atom #{}) :scheduled-added (atom 0)
   ;; Worker only: each cue's resolved selection, by cue value and rig.
   :resolutions (atom {})})

(defn retire!
  "Stop a replaced or stopped context's queued work from compiling or storing."
  [context]
  (reset! (:live context) false))

(defn- found [context program]
  (or (find @(:cache context) program) (find @(:warm context) program)
      (find @(:retired context) program) (find @(:ready context) program)
      (find @(:prepared context) program)))

(defn ready?
  "True when `program` needs no compilation in the frame."
  [context program]
  (some? (found context program)))

(defn- cache! [{:keys [cache retired]} program step]
  (when (>= (count @cache) cache-size)
    (reset! retired @cache)
    (reset! cache {}))
  (swap! cache assoc program step))

(defn- transient-failure?
  "A failure kept only until the owner meets it once, such as an
  out-of-memory error: it is thrown on that frame and then forgotten, so the
  next request compiles again, as an inline compile would."
  [step]
  (and (map? step) (contains? step ::transient)))

(defn compile!
  "The owner's compiler: return the compiled step for `program`. A full cache
  becomes the retired generation, and a program found there or among the
  worker's results or the warm tier is cached without compiling again. A
  failure the worker kept that refuses the revision is thrown on every use and
  never compiled again; a transient one is thrown once and dropped."
  [{:keys [compiler cache warm retired ready prepared] :as context} program]
  (when-let [[_ step] (or (find @ready program) (find @warm program) (find @prepared program))]
    (when (transient-failure? step)
      (swap! ready dissoc program)
      (swap! warm dissoc program)
      (swap! prepared dissoc program)
      (throw (::transient step))))
  (let [step (if-let [[_ step] (find @cache program)]
               step
               ;; A warm program a frame uses is cached too, so it stays
               ;; compiled for its running instances after leaving the tier.
               (let [step (if-let [[_ step] (or (find @warm program) (find @retired program) (find @ready program)
                                                (find @prepared program))]
                            step
                            (compiler program))]
                 (swap! ready dissoc program)
                 (swap! prepared dissoc program)
                 (cache! context program step)
                 step))]
    (if (instance? Throwable step) (throw step) step)))

(defn- interrupted? [^Throwable error]
  (or (instance? InterruptedException error) (instance? ClosedByInterruptException error)))

(defn- compile-result
  "Compile `program`, returning the step, the failure to keep, or nil to leave
  it uncompiled for a later request. A kept failure is met by the engine as an
  inline compile failure, once, and nothing compiles the program again:

  - a recoverable failure refuses the revision at :compile;
  - a StackOverflowError is kept as a plain failure naming it, because the
    stack has unwound and the overflow belongs to this definition's nesting;
  - any other VM error is kept as transient: `compile!` rethrows it on the
    owner once, so that frame fails as an inline compile would, and the next
    request compiles again;
  - a CancellationException is retried once, then kept as transient;
  - an interrupt is rethrown on the owner, and on the worker leaves the
    program uncompiled."
  [{:keys [compiler cancelled transient-failures]} program owner?]
  (try (let [step (compiler program)]
         (swap! cancelled disj program)
         (swap! transient-failures dissoc program)
         step)
       (catch Throwable error
         (cond
           (interrupted? error) (if owner?
                                  (do (.interrupt (Thread/currentThread)) (throw error))
                                  nil)
           (and (instance? CancellationException error) (not (contains? @cancelled program)))
           (do (swap! cancelled conj program) nil)
           (instance? StackOverflowError error)
           (ex-info (str "Compiling this revision failed: java.lang.StackOverflowError"
                         (when-let [message (.getMessage error)] (str ": " message)))
                    {:compile-failure "java.lang.StackOverflowError"})
           (runtime-faults/recoverable? error) error
           :else (do (swap! cancelled disj program)
                     (swap! transient-failures update program
                            (fn [entry] {:failures (inc (:failures entry 0)) :error (.getName (class error))}))
                     {::transient error})))))

(declare ahead)

(defn- enqueue!
  ([context program store!] (enqueue! context program store! false))
  ([{:keys [submit queued live rejected] :as context} program store! ahead?]
   (when-not (contains? @queued program)
     (swap! queued conj program)
     (let [task (fn []
                  (try
                    (when @live
                      (when-some [result (store! (fn [] (compile-result context program (nil? submit))))]
                        result))
                    (finally (swap! queued disj program))))]
       (if submit
         (try (submit (cond-> task ahead? ahead))
              (catch RejectedExecutionException _ (swap! queued disj program) (reset! rejected true)))
         (task))))))

(defn request!
  "This frame needs `programs`. Programs not ready are compiled off the frame,
  first in first out; a worker skips one no frame has asked for since it was
  queued, such as a superseded revision. Without a worker they are compiled
  now. Results no frame needs any more are dropped."
  [{:keys [ready wanted warm warm-wanted cancelled] :as context} programs]
  (let [needed (into [] (comp (distinct) (remove #(ready? context %))) programs)
        asked (set programs)]
    (reset! wanted (set needed))
    (swap! ready select-keys programs)
    ;; A cancellation or failure count is forgotten once no frame or warm
    ;; entry wants the program.
    (swap! cancelled (fn [c] (into #{} (filter #(or (contains? asked %) (contains? @warm-wanted %))) c)))
    (swap! (:transient-failures context)
           (fn [m] (into {} (filter #(or (contains? asked (key %)) (contains? @warm-wanted (key %)))) m)))
    (doseq [program needed]
      (enqueue! context program
                (fn [compile]
                  (when (contains? @wanted program)
                    (when-some [result (compile)]
                      (swap! ready assoc program result)
                      ;; The warm tier skips a program a frame already queued.
                      (when (contains? @warm-wanted program) (swap! warm assoc program result))
                      result)))))
    nil))

(defn prepare!
  "Compile `programs` into this context ahead of every queued compile, before
  the owner adopts it for a rig edit (#811). They are kept until a frame uses
  them, whatever that frame requests. Without a worker they compile now."
  [{:keys [prepared] :as context} programs]
  (doseq [program (distinct programs) :when (not (ready? context program))]
    (enqueue! context program
              (fn [compile]
                (when-some [result (compile)]
                  (swap! prepared assoc program result)
                  result))
              true)))

(defn worker-failed?
  "True when this context's prepared work can never finish: the worker
  refused one of its tasks, or the context was retired (#811)."
  [{:keys [rejected live]}]
  (or @rejected (not @live)))

(defn keep-prepared!
  "Keep only the prepared programs in `programs`, such as those of the
  instances still running when a rig edit is adopted (#811)."
  [{:keys [prepared]} programs]
  (swap! prepared select-keys programs))

(defn- warm-each! [{:keys [warm warm-wanted ready] :as context} programs]
  ;; A program a frame's request already compiled is kept here too, since
  ;; that request's result is dropped once no frame needs it.
  (doseq [program programs]
    (when-let [[_ step] (and (not (find @warm program)) (find @ready program))]
      (swap! warm assoc program step)))
  (doseq [program programs :when (not (ready? context program))]
    (enqueue! context program
              (fn [compile]
                (when (contains? @warm-wanted program)
                  (when-some [result (compile)]
                    (swap! warm assoc program result)
                    result))))))

(defn- on-worker! [{:keys [submit live rejected]} task]
  (try (submit (fn [] (when @live (task)))) (catch RejectedExecutionException _ (reset! rejected true) nil)))

(defn warm!
  "Rebuild the warm tier when `key` changes. `build` returns the programs to
  keep warm; it runs on the worker over immutable snapshots, before the
  compiles it queues, so the owner only compares the key and submits; true
  when it submitted a rebuild. The
  tier keeps only what `build` returns. Programs leaving it are dropped and
  never touch the cache, so a large rebuild cannot evict the programs running
  instances use. Only a context with a worker warms."
  [{:keys [submit warm warm-wanted warm-key] :as context} key build]
  (when (and submit (not= key @warm-key))
    (reset! warm-key key)
    (on-worker! context
                (fn []
                  (let [programs (vec (distinct (build)))]
                    (reset! warm-wanted (set programs))
                    (swap! warm select-keys programs)
                    (warm-each! context programs))))
    true))

(defn warm-more!
  "Add programs to the warm tier without rebuilding it, such as launches just
  scheduled for a grid boundary. `build` runs on the worker; a later rebuild
  drops what no running instance or source still needs."
  [{:keys [submit warm-wanted] :as context} build]
  (when submit
    (on-worker! context
                (fn []
                  (let [programs (vec (distinct (build)))]
                    (swap! warm-wanted into programs)
                    (warm-each! context programs))))))

(defn resolved!
  "Memoise `resolve` for one cue by the cue's value and `basis` (the rig and
  settings it resolves against). Worker only."
  [{:keys [resolutions]} cue-id cue basis resolve]
  (let [{cached-cue :cue cached-basis :basis :keys [result]} (get @resolutions cue-id)]
    (if (and (identical? cue cached-cue) (= basis cached-basis))
      result
      (let [result (resolve)]
        (swap! resolutions assoc cue-id {:cue cue :basis basis :result result})
        result))))

(def repeated-failure-limit
  "Consecutive transient failures of one program before it is reported."
  5)

(defn repeated-failures
  "Programs whose compile has failed transiently at least
  `repeated-failure-limit` times in a row, still retried every request:
  {[effect-id version] {:failures n :error class-name :targets targets}}."
  [{:keys [transient-failures]}]
  (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
        (keep (fn [[program {:keys [failures] :as entry}]]
                (when (>= failures repeated-failure-limit)
                  [[(:id program) (:version program)] (assoc entry :targets (:targets program))])))
        @transient-failures))

(defn ahead
  "Mark `task` to run ahead of queued compiles, such as checking a published
  definition the frame owner waits for (#784)."
  [task]
  (vary-meta task assoc ::ahead true))

(defn ahead?
  "True for a task marked with `ahead`."
  [task]
  (boolean (::ahead (meta task))))

(deftype QueuedTask [^long lane ^long sequence task]
  Runnable
  (run [_] (task))
  Comparable
  (compareTo [_ other]
    (let [^QueuedTask other other]
      (if (== lane (.-lane other))
        (Long/compare sequence (.-sequence other))
        (Long/compare lane (.-lane other))))))

(defn worker
  "An owned single-thread compile worker. Tasks marked `ahead` run first, in
  their order, then every other task, first in first out. :submit takes a
  task; :close! stops it, drops queued work and waits up to a second for a
  running compile; :terminated? says it has stopped."
  []
  (let [factory (reify ThreadFactory
                  (newThread [_ task] (doto (Thread. ^Runnable task "tolglow-program-compiler") (.setDaemon true))))
        pool (ThreadPoolExecutor. 1 1 0 TimeUnit/MILLISECONDS (PriorityBlockingQueue.) factory)
        sequence (AtomicLong.)]
    {:submit (fn [task] (.execute pool ^Runnable (QueuedTask. (if (ahead? task) 0 1) (.incrementAndGet sequence) task)))
     :close! (fn []
               (.shutdownNow ^ExecutorService pool)
               (try (.awaitTermination ^ExecutorService pool 1000 TimeUnit/MILLISECONDS)
                    (catch InterruptedException _ (.interrupt (Thread/currentThread)))))
     :terminated? (fn [] (.isTerminated ^ExecutorService pool))}))

(defn program
  "The program the engine compiles for `instance` running `definition`: the
  definition narrowed to the instance's launch targets by
  `afterglow.runtime.engine/launch-program`, the engine's own compile key."
  [definition instance]
  (engine/launch-program definition (:targets instance)))

(defn- fresh? [instance]
  (nil? (get-in instance [:system :last-frame-number])))

(defn- plan-held [model framed waits frame pending fresh]
  (let [registry (get-in model [:registry :effects])
        pending-effects (distinct (map :effect-id pending))
        waits (into {} (map (fn [effect-id]
                              (let [version (get-in registry [effect-id :version])
                                    [waited since] (get waits effect-id)]
                                [effect-id [version (if (= waited version) since frame)]])))
                    pending-effects)
        pending-by-effect (group-by :effect-id pending)
        groups (mapv (fn [effect-id]
                       (let [members (get pending-by-effect effect-id)]
                         {:effect-id effect-id :members members
                          :launches (filterv #(= effect-id (:effect-id %)) fresh)
                          :programs (into [] (comp (filter #(modulation/reload-candidate framed %))
                                                   (map #(program (get registry effect-id) %)) (distinct))
                                          members)}))
                     pending-effects)]
    ;; A first launch of an effect with no pending revision is not held: the
    ;; frame compiles its program if it is not ready, so it renders on its start.
    {:groups groups :waits waits :programs (into [] (comp (mapcat :programs) (distinct)) groups)}))

(defn plan
  "Which pending hot reloads this frame holds back until their programs are
  compiled. `model` is the frame's model and `framed` the same model as
  modulation reads it (with ramp values).

  A pending revision is attempted only when every program its instances need
  is ready; until then every instance of that effect keeps its captured
  definition, so one effect never runs two revisions in a frame. Instances
  that rendered before keep the program they ran, so only the revision's
  programs are needed. A launch of such an effect in the meantime starts on
  the revision its running instances use and adopts with them (see `hold`).

  `waits` ({effect-id [version since]}) carries the frame each pending
  revision was first seen. Returns {:groups :programs :waits}; call `request!`
  with :programs, then `hold`."
  [model framed waits frame]
  (let [pending (filterv #(modulation/attempted-revision model %) (vals (:instances model)))
        pending-ids (into #{} (map :id) pending)
        fresh (filterv #(and (fresh? %) (not (contains? pending-ids (:id %)))) (vals (:instances model)))]
    (if (empty? pending)
      {:groups [] :programs [] :waits {}}
      (plan-held model framed waits frame (sort-by :order pending) (sort-by :order fresh)))))

(defn- downgrade
  "`launch` started on `sibling`'s revision, or nil when that would lose any of
  its values. The launch resolved its values (launch and cue overrides, saved
  values, defaults) against the revision it captured. It may start on the
  older revision only if every value the older revision has resolves there
  unchanged, every value only the newer revision has is that revision's
  default, which adoption adds back, and its targets narrow to the same
  fixtures on both revisions. Otherwise, and for a pinned launch, it keeps the
  revision it captured."
  [launch sibling]
  (when (not= :pinned (:reload-policy launch :pinned))
    (let [old (:params (:definition sibling) {})
          new (:params (:definition launch) {})
          values (:params launch)
          kept (try (params/resolve-values old (select-keys values (keys old))) (catch Exception _ nil))
          defaults (try (params/resolve-values new {}) (catch Exception _ nil))
          lossless? (and kept defaults
                         (every? (fn [[k v]] (if (contains? old k) (= v (get kept k)) (= v (get defaults k)))) values)
                         ;; The launch must reach the same fixtures on both revisions.
                         (= (:targets (program (:definition sibling) launch))
                            (:targets (program (:definition launch) launch))))]
      (when lossless?
        (assoc launch :definition (:definition sibling) :effect-version (:effect-version sibling) :params kept :state {})))))

(defn hold
  "Decide a `plan` after its programs were requested. The instances of an
  effect whose revision's programs are not all ready keep their captured
  definition this frame (:deferred). A launch of that effect starts on the
  revision its running instances use and waits with them (:downgraded), so
  the frame compiles at most that launch's own program; it then adopts the
  revision on the same frame as the rest."
  [context {:keys [groups waits]}]
  (reduce (fn [held {:keys [members launches programs]}]
            (if (every? #(ready? context %) programs)
              held
              (let [sibling (first members)
                    downgraded (into {} (keep #(when-let [d (downgrade % sibling)] [(:id %) d])) launches)]
                (-> held
                    (update :deferred into (map :id) members)
                    (update :deferred into (keys downgraded))
                    (update :downgraded merge downgraded)))))
          {:waits waits :deferred #{} :downgraded {}} groups))

(defn frame-view
  "The model modulation and the engine read: launches started on their
  siblings' revision, and deferred instances read as pinned."
  [model {:keys [deferred downgraded]}]
  (modulation/without-reloads (cond-> model (seq downgraded) (update :instances merge downgraded)) deferred))

(defn restore
  "Return `result`, evaluated from `frame-view`, with the reload policies of
  deferred instances back. A downgraded launch keeps the revision it started
  on, as the running instances it adopts with do."
  [result model {:keys [deferred]}]
  (modulation/restore-reloads result model deferred))

(defn mark-adoptions
  "Add the frame its revision was first pending and the frame it was adopted
  to each :effect/reloaded intent, and to its emission record. `effect-of`
  maps an instance ID to its effect ID."
  [intents emissions waits effect-of frame]
  (let [mark (fn [intent]
               (let [[version since] (when (= :effect/reloaded (:type intent))
                                       (get waits (effect-of (:instance-id intent))))]
                 (if (and since (= version (:to-version intent)))
                   (assoc intent :pending-since-frame since :adopted-frame frame)
                   intent)))]
    [(some->> intents (mapv mark)) (some->> emissions (mapv #(update % :intent mark)))]))
