(ns tolglow.show.programs
  "Compiled effect programs for one frame owner (#774).

  The engine compiles one program per definition and launch target set. With
  an owned worker, programs are compiled ahead of need, off the frame:

  - a warm tier holds every registered effect's untargeted program, each
    cue's fixture selection, each recorded live-run take's targets and each
    scheduled quantized launch, recomputed when the registry, rig, takes or
    schedule change;
  - a pending hot reload requests the programs its instances need and is
    adopted, by all of them together, once they are ready.

  A first launch never waits: if its program is not ready the frame compiles
  it, so it renders on its start. Without a worker, as in an unscheduled
  runtime or an audition, `request!` compiles at once on the owner."
  (:require [afterglow.control.params :as params]
            [afterglow.runtime.faults :as runtime-faults]
            [tolglow.modulation :as modulation])
  (:import [java.nio.channels ClosedByInterruptException]
           [java.util.concurrent CancellationException ExecutorService LinkedBlockingQueue
            RejectedExecutionException ThreadFactory ThreadPoolExecutor TimeUnit]))

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
   :wanted (atom #{}) :warm-wanted (atom #{}) :live (atom true)
   ;; Owner only: what the warm tier was last built from.
   :warm-key (atom ::none) :scheduled-seen (atom #{})
   ;; Worker only: each cue's resolved selection, by cue value and rig.
   :resolutions (atom {})})

(defn retire!
  "Stop a replaced or stopped context's queued work from compiling or storing."
  [context]
  (reset! (:live context) false))

(defn- found [context program]
  (or (find @(:cache context) program) (find @(:warm context) program)
      (find @(:retired context) program) (find @(:ready context) program)))

(defn ready?
  "True when `program` needs no compilation in the frame."
  [context program]
  (some? (found context program)))

(defn- cache! [{:keys [cache retired]} program step]
  (when (>= (count @cache) cache-size)
    (reset! retired @cache)
    (reset! cache {}))
  (swap! cache assoc program step))

(defn compile!
  "The owner's compiler: return the compiled step for `program`. A full cache
  becomes the retired generation, and a program found there or among the
  worker's results or the warm tier is cached without compiling again. A
  failure the worker met is thrown here, once per
  use, as an inline compile would throw it, and is never compiled again."
  [{:keys [compiler cache warm retired ready] :as context} program]
  (let [step (if-let [[_ step] (find @cache program)]
               step
               ;; A warm program a frame uses is cached too, so it stays
               ;; compiled for its running instances after leaving the tier.
               (let [step (if-let [[_ step] (or (find @warm program) (find @retired program) (find @ready program))]
                            step
                            (compiler program))]
                 (swap! ready dissoc program)
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
  - any other VM error is kept as it is, so `compile!` rethrows it on the
    owner and the frame fails as an inline compile would;
  - a CancellationException is retried once, then kept as it is;
  - an interrupt is rethrown on the owner, and on the worker leaves the
    program uncompiled."
  [{:keys [compiler cancelled]} program owner?]
  (try (compiler program)
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
           :else error))))

(defn- enqueue! [{:keys [submit queued live] :as context} program store!]
  (when-not (contains? @queued program)
    (swap! queued conj program)
    (let [task (fn []
                 (try
                   (when @live
                     (when-some [result (store! (fn [] (compile-result context program (nil? submit))))]
                       result))
                   (finally (swap! queued disj program))))]
      (if submit
        (try (submit task) (catch RejectedExecutionException _ (swap! queued disj program)))
        (task)))))

(defn request!
  "This frame needs `programs`. Programs not ready are compiled off the frame,
  first in first out; a worker skips one no frame has asked for since it was
  queued, such as a superseded revision. Without a worker they are compiled
  now. Results no frame needs any more are dropped."
  [{:keys [ready wanted warm warm-wanted] :as context} programs]
  (let [needed (into [] (comp (distinct) (remove #(ready? context %))) programs)]
    (reset! wanted (set needed))
    (swap! ready select-keys programs)
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

(defn- on-worker! [{:keys [submit live]} task]
  (try (submit (fn [] (when @live (task)))) (catch RejectedExecutionException _ nil)))

(defn warm!
  "Rebuild the warm tier when `key` changes. `build` returns the programs to
  keep warm; it runs on the worker over immutable snapshots, before the
  compiles it queues, so the owner only compares the key and submits. The
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
                    (warm-each! context programs))))))

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

(defn worker
  "An owned single-thread compile worker, first in first out. :submit takes a
  task; :close! stops it, drops queued work and waits up to a second for a
  running compile; :terminated? says it has stopped."
  []
  (let [factory (reify ThreadFactory
                  (newThread [_ task] (doto (Thread. ^Runnable task "tolglow-program-compiler") (.setDaemon true))))
        pool (ThreadPoolExecutor. 1 1 0 TimeUnit/MILLISECONDS (LinkedBlockingQueue.) factory)]
    {:submit (fn [task] (.execute pool ^Runnable task))
     :close! (fn []
               (.shutdownNow ^ExecutorService pool)
               (try (.awaitTermination ^ExecutorService pool 1000 TimeUnit/MILLISECONDS)
                    (catch InterruptedException _ (.interrupt (Thread/currentThread)))))
     :terminated? (fn [] (.isTerminated ^ExecutorService pool))}))

(defn program
  "The program the engine compiles for `instance` running `definition`: the
  definition narrowed to the instance's launch targets, as
  `afterglow.runtime.engine/render-definition` narrows it (a test pins the
  two together)."
  [definition instance]
  (let [targets (:targets instance)]
    (if (nil? targets)
      definition
      (assoc definition :targets (if-let [authored (:targets definition)]
                                   (filterv (set authored) targets)
                                   targets)))))

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
  "`launch` started on `sibling`'s revision, or nil when its values do not fit
  that revision. A pinned launch keeps the revision it captured."
  [launch sibling]
  (when (not= :pinned (:reload-policy launch :pinned))
    (let [old (:definition sibling)
          schemas (:params old {})]
      (when-let [values (try (params/resolve-values schemas (select-keys (:params launch) (keys schemas)))
                             (catch Exception _ nil))]
        (assoc launch :definition old :effect-version (:effect-version sibling) :params values :state {})))))

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
