(ns tolglow.show.launch-prep
  "Prepare quantised launches ahead of their beat, off the frame (#918).

  After each committed frame the owner hands the launches newly waiting for a
  grid boundary, with that committed model, to an owned worker. The worker
  resolves each launch's targets and its capability verdict
  (`tolglow.spatial/prepare-launch`). At the beat, admission installs that
  work when everything it read is unchanged (same epoch, rig, scene groups,
  cue, launch and effect definition) and otherwise, or when the worker has
  not finished, does it inline. There is no frame budget or deadline:
  prepared work is only ever a shortcut to the same values. The instance
  itself, its ID, order, clock and history are still built by Afterglow's
  admission on the owner, in launch order."
  (:require [tolglow.compiler.capability :as capability]
            [tolglow.cue-timing :as cue-timing]
            [tolglow.spatial :as spatial])
  (:import (java.util.concurrent ExecutorService Executors RejectedExecutionException ThreadFactory)))

(defn- owned-executor []
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ task] (doto (Thread. ^Runnable task "tolglow-launch-preparation") (.setDaemon true))))))

(defn context
  "The :prepare-launches option: false or nil admits every launch inline, true
  prepares on one owned thread, and a function receives each preparation task
  (tests and replay run them themselves). Nil when off."
  [options]
  (let [choice (get options :prepare-launches false)
        executor (when (true? choice) (delay (owned-executor)))
        submit (cond
                 (or (nil? choice) (false? choice)) nil
                 (true? choice) (fn [task] (.execute ^ExecutorService @executor ^Runnable task))
                 (fn? choice) choice
                 :else (throw (ex-info "Prepare launches must be a boolean or a function" {:prepare-launches choice})))]
    (when submit
      (let [state (atom {:submitted #{} :prepared {} :counts {}})]
        {:submit submit :executor executor :state state
         ;; Bound for the owner's cycle as spatial/*prepared-launches*.
         :bindings {:lookup (fn [id]
                              (let [{:keys [submitted prepared]} @state]
                                (cond
                                  (contains? prepared id) (get prepared id)
                                  (contains? submitted id) :not-ready)))
                    :outcome (fn [_ outcome] (swap! state update-in [:counts outcome] (fnil inc 0)))}}))))

(defn- prepare-batch
  "Prepare `commands` against one committed `model`, sharing one rig index."
  [model commands]
  (let [index (delay (capability/rig-index (get-in model [:rig :fixtures])))]
    (into {} (map (fn [command]
                    [(:command-id command) (or (spatial/prepare-launch model command index) :unpreparable)]))
          commands)))

(defn submit!
  "On the owner after a frame commits: forget work for launches that no
  longer wait (fired, cancelled, missed or restarted away) and hand the
  newly scheduled ones, with this immutable `model`, to the worker. A result
  the worker finishes after its launch stopped waiting is dropped."
  [{:keys [submit state]} model]
  (let [scheduled (cue-timing/scheduled-commands model)]
    (when (or (seq scheduled) (seq (:submitted @state)))
      (let [waiting (into #{} (keep :command-id) scheduled)
            known (:submitted @state)
            fresh (into [] (filter #(and (some? (:command-id %)) (not (contains? known (:command-id %))))) scheduled)]
        (swap! state (fn [s] (-> s
                                 (assoc :submitted (into (into #{} (filter waiting) (:submitted s)) (map :command-id) fresh))
                                 (update :prepared select-keys waiting))))
        (when (seq fresh)
          (try
            (submit (fn prepare-launches []
                      (let [results (prepare-batch model fresh)]
                        (swap! state (fn [s]
                                       (-> s
                                           (update :prepared merge (select-keys results (:submitted s)))
                                           (update-in [:counts :prepared] (fnil + 0) (count results))))))))
            ;; A stopped worker leaves these launches to inline admission.
            (catch RejectedExecutionException _ nil)))))))

(defn counts
  "Launches prepared, and how admission used them: :installed, :verdict-stale
  (targets reused, capability worked out again), :stale (inline: something it
  read changed), :not-ready (inline: the worker had not finished) and
  :unpreparable (inline: admission reads more than the snapshot)."
  [context]
  (:counts @(:state context)))

(defn close!
  "Stop an owned worker without waiting for a preparation in progress."
  [{:keys [executor]}]
  (when (and executor (realized? executor))
    (.shutdownNow ^ExecutorService @executor)))
