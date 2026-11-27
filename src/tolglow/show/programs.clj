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
  (:require [afterglow.runtime.faults :as runtime-faults]
            [tolglow.modulation :as modulation])
  (:import [java.nio.channels ClosedByInterruptException]
           [java.util.concurrent ExecutorService LinkedBlockingQueue RejectedExecutionException
            ThreadFactory ThreadPoolExecutor TimeUnit]))

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
   :ready (atom {}) :warm (atom {}) :queued (atom #{})
   :wanted (atom #{}) :warm-wanted (atom #{}) :warm-key (atom ::none) :live (atom true)})

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
  worker's results moves into the cache without compiling again. Warm programs
  stay in their own tier. A failure the worker met is thrown here, once per
  use, as an inline compile would throw it, and is never compiled again."
  [{:keys [compiler cache warm retired ready] :as context} program]
  (let [step (if-let [[_ step] (or (find @cache program) (find @warm program))]
               step
               (let [step (if-let [[_ step] (or (find @retired program) (find @ready program))]
                            step
                            (compiler program))]
                 (swap! ready dissoc program)
                 (cache! context program step)
                 step))]
    (if (instance? Throwable step) (throw step) step)))

(defn- interrupted? [^Throwable error]
  (or (instance? InterruptedException error) (instance? ClosedByInterruptException error)))

(defn- compile-result
  "Compile `program`, returning the step or the failure, which is kept so the
  engine refuses the revision at :compile once and nothing compiles it again.
  An error the engine would not attribute to an effect, such as a stack
  overflow in a deeply nested definition, is kept as a plain failure naming
  it: it was raised compiling this one revision, and treating it as an owner
  failure would stop every frame until the revision is replaced. An interrupt
  of the owner itself is rethrown."
  [compiler program owner?]
  (try (compiler program)
       (catch Throwable error
         (when (and owner? (interrupted? error))
           (.interrupt (Thread/currentThread))
           (throw error))
         (if (runtime-faults/recoverable? error)
           error
           (ex-info (str "Compiling this revision failed: " (.getName (class error))
                         (when-let [message (.getMessage error)] (str ": " message)))
                    {:compile-failure (.getName (class error))})))))

(defn- enqueue! [{:keys [compiler submit queued live]} program store!]
  (when-not (contains? @queued program)
    (swap! queued conj program)
    (let [task (fn []
                 (try
                   (when @live
                     (when-some [result (store! (fn [] (compile-result compiler program (nil? submit))))]
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
                    (let [result (compile)]
                      (swap! ready assoc program result)
                      (when (contains? @warm-wanted program) (swap! warm assoc program result))
                      result)))))
    nil))

(defn warm!
  "Keep `programs` compiled in the warm tier, replacing what an earlier call
  asked for. Only a context with a worker warms, and only the owner calls
  this; `key` names the inputs, so an unchanged one does nothing."
  [{:keys [submit warm warm-wanted warm-key] :as context} key programs]
  (when (and submit (not= key @warm-key))
    (reset! warm-key key)
    (let [programs (vec (distinct programs))
          kept (set programs)]
      (reset! warm-wanted kept)
      ;; A program leaving the tier, such as a scheduled launch that just
      ;; fired, moves to the bounded cache for the frame that uses it.
      (doseq [[program step] @warm :when (not (contains? kept program))]
        (cache! context program step))
      (swap! warm select-keys programs)
      (doseq [program programs :when (not (ready? context program))]
        (enqueue! context program
                  (fn [compile]
                    (when (contains? @warm-wanted program)
                      (let [result (compile)]
                        (swap! warm assoc program result)
                        result))))))))

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
        pending-ids (into #{} (map :id) pending)
        needs (fn [instance]
                (if (contains? pending-ids (:id instance))
                  (when (modulation/reload-candidate framed instance)
                    [(program (get registry (:effect-id instance)) instance)])
                  [(program (:definition instance) instance)]))
        members (group-by :effect-id (concat pending fresh))
        order (concat pending-effects (remove (set pending-effects) (distinct (map :effect-id fresh))))
        groups (mapv (fn [effect-id]
                       (let [members (get members effect-id)]
                         {:effect-id effect-id :members members
                          :launch? (boolean (some fresh? members))
                          :programs (into [] (comp (mapcat needs) (distinct)) members)}))
                     order)]
    {:groups groups :waits waits
     ;; A group with a first launch compiles in the frame if it must, so
     ;; only the others go to the worker.
     :programs (into [] (comp (remove :launch?) (mapcat :programs) (distinct)) groups)}))

(defn plan
  "Which pending hot reloads this frame holds back until their programs are
  compiled. `model` is the frame's model and `framed` the same model as
  modulation reads it (with ramp values).

  A pending revision is attempted only when every program its instances need
  is ready; until then every instance of that effect keeps its captured
  definition, so one effect never runs two revisions in a frame. An effect
  that also has a first launch this frame takes the revision over at once,
  compiling what is missing in the frame, so the launch renders on its start
  and on the same revision as the rest. Instances that rendered before keep
  the program they ran, so only the revision's programs are needed.

  `waits` ({effect-id [version since]}) carries the frame each pending
  revision was first seen. Returns {:groups :programs :waits}; call `request!`
  with :programs, then `hold`."
  [model framed waits frame]
  (let [pending (filterv #(modulation/attempted-revision model %) (vals (:instances model)))
        pending-ids (into #{} (map :id) pending)
        fresh (filterv #(and (fresh? %) (not (contains? pending-ids (:id %)))) (vals (:instances model)))]
    (if (and (empty? pending) (empty? fresh))
      {:groups [] :programs [] :waits {}}
      (plan-held model framed waits frame (sort-by :order pending) (sort-by :order fresh)))))

(defn hold
  "Decide a `plan` after its programs were requested: the pending instances of
  an effect whose programs are not all ready, and that has no first launch,
  keep their captured definition this frame."
  [context {:keys [groups waits]}]
  {:waits waits
   :deferred (into #{} (comp (remove :launch?)
                             (remove #(every? (fn [program] (ready? context program)) (:programs %)))
                             (mapcat :members) (map :id))
                   groups)})

(defn engine-view
  "The model the engine evaluates: deferred instances read as pinned."
  [model {:keys [deferred]}]
  (modulation/without-reloads model deferred))

(defn restore
  "Return `result`, evaluated from `engine-view`, with the reload policies of
  deferred instances back."
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
