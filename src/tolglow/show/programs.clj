(ns tolglow.show.programs
  "Compiled effect programs for one frame owner (#774).

  The engine compiles one program per definition and launch target set. A
  bounded cache keeps them. Programs that a pending hot reload or a first
  launch needs are compiled by an owned background worker, and the frame
  keeps running what it already has until they are ready, so publishing
  revisions never compiles inside a frame. Without a worker, as in an
  unscheduled runtime or an audition, `request!` compiles them at once, on the
  owner, before the engine runs."
  (:require [afterglow.runtime.faults :as runtime-faults]
            [tolglow.modulation :as modulation])
  (:import [java.util.concurrent ExecutorService LinkedBlockingQueue RejectedExecutionException
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
   :ready (atom {}) :queued (atom #{}) :wanted (atom #{}) :live (atom true)})

(defn retire!
  "Stop a replaced context's queued work from compiling."
  [context]
  (reset! (:live context) false))

(defn- found [context program]
  (or (find @(:cache context) program) (find @(:retired context) program) (find @(:ready context) program)))

(defn ready?
  "True when `program` needs no compilation in the frame."
  [context program]
  (some? (found context program)))

(defn compile!
  "The owner's compiler: return the compiled step for `program`. A full cache
  becomes the retired generation, and a program found there or among the
  worker's results moves into the cache without compiling again, so neither
  a full cache nor a finished background compile makes one frame recompile
  running effects. A program the worker failed to compile throws that
  failure, as an inline compile would."
  [{:keys [compiler cache retired ready]} program]
  (let [step (if-let [[_ step] (find @cache program)]
               step
               (let [step (if-let [[_ step] (or (find @retired program) (find @ready program))]
                            step
                            (compiler program))]
                 (swap! ready dissoc program)
                 (when (>= (count @cache) cache-size)
                   (reset! retired @cache)
                   (reset! cache {}))
                 (swap! cache assoc program step)
                 step))]
    (if (instance? Throwable step) (throw step) step)))

(defn- compile-off-frame
  "Compile on the worker. A recoverable failure is kept so the engine turns
  the revision down as it would an inline compile; any other is left for the
  owner to meet inline."
  [compiler program]
  (try (compiler program)
       (catch Throwable error
         (when (runtime-faults/recoverable? error) error))))

(defn request!
  "This frame needs `programs` (in priority order). Programs not ready are
  compiled off the frame, first in first out; a worker skips one no frame has
  asked for since it was queued, such as a superseded revision. Without a
  worker they are compiled now. Results nobody needs any more are dropped."
  [{:keys [compiler submit ready queued wanted live] :as context} programs]
  (let [needed (into [] (comp (distinct) (remove #(ready? context %))) programs)]
    (reset! wanted (set needed))
    (swap! ready select-keys programs)
    (doseq [program needed :when (not (contains? @queued program))]
      (let [task (fn []
                   (try
                     (when (and @live (contains? @wanted program))
                       (when-some [step (compile-off-frame compiler program)]
                         (swap! ready assoc program step)))
                     (finally (swap! queued disj program))))]
        (swap! queued conj program)
        (if submit
          (try (submit task) (catch RejectedExecutionException _ (swap! queued disj program)))
          (task))))
    nil))

(defn worker
  "An owned single-thread compile worker, first in first out. :submit takes a
  task; :close! stops it and drops queued work."
  []
  (let [factory (reify ThreadFactory
                  (newThread [_ task] (doto (Thread. ^Runnable task "tolglow-program-compiler") (.setDaemon true))))
        pool (ThreadPoolExecutor. 1 1 0 TimeUnit/MILLISECONDS (LinkedBlockingQueue.) factory)]
    {:submit (fn [task] (.execute pool ^Runnable task))
     :close! (fn [] (.shutdownNow ^ExecutorService pool))}))

(defn program
  "The program the engine compiles for `instance` running `definition`: the
  definition narrowed to the instance's launch targets, as
  `afterglow.runtime.engine/render-definition` narrows it."
  [definition instance]
  (let [targets (:targets instance)]
    (if (nil? targets)
      definition
      (assoc definition :targets (if-let [authored (:targets definition)]
                                   (filterv (set authored) targets)
                                   targets)))))

(defn- fresh? [instance]
  (nil? (get-in instance [:system :last-frame-number])))

(defn- plan-held [model framed waits frame pending pending-ids fresh]
  (let [registry (get-in model [:registry :effects])
        pending-effects (distinct (map :effect-id pending))
        waits (into {} (map (fn [effect-id]
                              (let [version (get-in registry [effect-id :version])
                                    [waited since] (get waits effect-id)]
                                [effect-id [version (if (= waited version) since frame)]])))
                    pending-effects)
        needs (fn [instance]
                (if (contains? pending-ids (:id instance))
                  (cond-> [(program (:definition instance) instance)]
                    (modulation/reload-candidate framed instance)
                    (conj (program (get registry (:effect-id instance)) instance)))
                  [(program (:definition instance) instance)]))
        members (group-by :effect-id (concat pending fresh))
        order (concat pending-effects (remove (set pending-effects) (distinct (map :effect-id fresh))))
        pending-set (set pending-effects)
        groups (mapv (fn [effect-id]
                       {:effect-id effect-id :members (get members effect-id) :pending? (contains? pending-set effect-id)
                        :programs (into [] (comp (mapcat needs) (distinct)) (get members effect-id))})
                     order)]
    {:groups groups :programs (into [] (comp (mapcat :programs) (distinct)) groups) :waits waits}))

(defn plan
  "Which instances this frame holds back until their programs are compiled.

  `model` is the frame's model and `framed` the same model as modulation reads
  it (with ramp values). A pending hot reload is attempted only when every
  program its instances need is ready; until then every instance of that
  effect keeps its captured definition (:deferred), so one effect never runs
  two revisions in a frame. An instance that has never rendered and whose
  program is not ready waits too (:hidden), including a launch of an effect
  whose revision is still compiling. `waits` ({effect-id [version since]})
  carries the frame each pending revision was first seen. Programs are
  requested in launch order; the worker runs them first in first out, so a
  revision that keeps being republished cannot delay older ones. Returns
  {:groups [...] :programs [program ...] :waits waits}; pass it through
  `admit-launches`, call `request!` with its :programs, then `hold`."
  [model framed waits frame]
  (let [pending (filterv #(modulation/attempted-revision model %) (vals (:instances model)))
        pending-ids (into #{} (map :id) pending)
        fresh (filterv #(and (fresh? %) (not (contains? pending-ids (:id %)))) (vals (:instances model)))]
    (if (and (empty? pending) (empty? fresh))
      {:groups [] :programs [] :waits {}}
      (plan-held model framed waits frame (sort-by :order pending) pending-ids (sort-by :order fresh)))))

(def launch-compiles
  "Programs a frame may still compile itself for first launches, so a single
  launch renders on the frame that admits it (#774)."
  4)

(defn admit-launches
  "Choose the effects whose first launches this frame compiles itself: those
  with no pending revision whose missing programs, taken in launch order,
  fit within `launch-compiles` together. The others wait for the worker.
  Returns `plan` with those groups marked :inline and left out of :programs."
  [context {:keys [groups] :as plan}]
  (let [[groups _] (reduce (fn [[groups budget] {:keys [programs] :as group}]
                             (let [missing (count (remove #(ready? context %) programs))]
                               (if (and (pos? missing) (<= missing budget) (not (:pending? group)))
                                 [(conj groups (assoc group :inline? true)) (- budget missing)]
                                 [(conj groups group) budget])))
                           [[] launch-compiles] groups)]
    (assoc plan :groups groups
           :programs (into [] (comp (remove :inline?) (mapcat :programs) (distinct)) groups))))

(defn hold
  "Decide a `plan` after its programs were requested: an effect whose programs
  are not all ready keeps its pending instances on their captured definition
  and hides its instances that never rendered. Groups `admit-launches` marked
  :inline compile in the frame."
  [context {:keys [groups waits]}]
  (reduce (fn [held {:keys [members programs inline?]}]
            (if (or inline? (every? #(ready? context %) programs))
              held
              (reduce (fn [held instance]
                        (if (fresh? instance)
                          (update held :hidden conj (:id instance))
                          (update held :deferred conj (:id instance))))
                      held members)))
          {:deferred #{} :hidden #{} :waits waits} groups))

(defn engine-view
  "The model the engine evaluates: deferred instances read as pinned and
  hidden ones are absent."
  [model {:keys [deferred hidden]}]
  (cond-> (modulation/without-reloads model deferred)
    (seq hidden) (update :instances #(apply dissoc % hidden))))

(defn restore
  "Return `result`, evaluated from `engine-view`, with the reload policies of
  deferred instances and the hidden instances of `model` back."
  [result model {:keys [deferred hidden]}]
  (cond-> (modulation/restore-reloads result model deferred)
    (seq hidden) (update :instances merge (select-keys (:instances model) hidden))))

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
