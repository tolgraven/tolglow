(ns tolglow.compute
  "Configure Afterglow expression and calibrated movement renderers for one show."
  (:require [afterglow.render.fields :as fields]
            [afterglow.render.frame :as frame]
            [afterglow.render.gpu.movement :as movement]
            [afterglow.render.gpu.session :as gpu]
            [afterglow.render.ir :as ir]
            [afterglow.runtime.faults :as faults])
  (:import [java.util.concurrent CancellationException ExecutionException Future TimeUnit TimeoutException]))

(defn- failure [^Throwable error]
  (when-not (faults/recoverable? error) (throw error))
  {:type (or (:type (ex-data error)) (.getSimpleName (class error)))
   :message (.getMessage error)})

(defn- open-expressions! [gpu-options budget]
  (let [state (atom {:requested :native-gpu :active :cpu :gpu-budget-ms budget
                     :native-dispatches 0 :cpu-fallbacks 0 :deadline-fallbacks 0})]
    (try (let [session (gpu/open! gpu-options)]
           (swap! state assoc :active :native-gpu)
           {:session session :state state})
         (catch Throwable e
           (swap! state assoc :last-fallback (assoc (failure e) :stage :open))
           {:state state}))))

(defn open!
  "Acquire configured Afterglow stages. CPU-only shows need no owner."
  [{:keys [compute-backend movement-backend gpu-options batch? gpu-budget-ms frame-rate]
    :or {compute-backend :cpu movement-backend :cpu gpu-options {} batch? true frame-rate 40}}]
  (doseq [[key backend] [[:compute-backend compute-backend] [:movement-backend movement-backend]]]
    (when-not (#{:cpu :native-gpu} backend)
      (throw (ex-info "Unknown show backend" {key backend}))))
  (when (or (= :native-gpu compute-backend) (= :native-gpu movement-backend))
    (when (and (= :native-gpu compute-backend) (not batch?))
      (throw (ex-info "Native GPU expressions require :batch? true" {})))
    (when-not (and (map? gpu-options)
                   (every? #{:max-programs :queue-capacity :timeout-ms} (keys gpu-options))
                   (every? pos-int? (vals gpu-options))
                   (<= (:timeout-ms gpu-options 1000) 10000))
      (throw (ex-info "GPU options accept positive :max-programs, :queue-capacity and :timeout-ms" {})))
    (let [budget (or gpu-budget-ms (max 1 (long (/ 500.0 frame-rate))))
          _ (when-not (pos-int? budget)
              (throw (ex-info "Show GPU budget must be a positive integer in milliseconds" {})))
          mover (when (= :native-gpu movement-backend)
                  (movement/open! {:budget-ms budget :timeout-ms (:timeout-ms gpu-options 1000)}))]
      (try
        (merge {:budget-ns (* 1000000 budget) :movement mover}
               (when (= :native-gpu compute-backend) (open-expressions! gpu-options budget)))
        (catch Throwable error
          (when mover
            (try (movement/close! mover) (catch Throwable cleanup (.addSuppressed error cleanup))))
          (throw error))))))

(defn status [owner]
  (if-let [state (:state owner)]
    (cond-> @state (:session owner) (assoc :session (gpu/status (:session owner))))
    {:requested :cpu :active :cpu}))

(defn movement-status [owner output]
  (if (:movement owner)
    (merge {:requested :native-gpu :backend :idle} (:movement output))
    {:requested :cpu :backend :cpu}))

(defn close! [owner]
  (try (when-let [session (:session owner)] (gpu/close! session))
       (finally
         (when-let [state (:state owner)] (swap! state assoc :active :stopped))
         (when-let [mover (:movement owner)] (movement/close! mover)))))

(defn prepare-group
  "Pure compile-time preparation. Keep original float64 positions and CPU plan
  for a failed compile, unavailable graphics context, or failed readback."
  [expressions positions cpu-plan]
  (merge {:cpu-plan cpu-plan :positions positions}
         (try {:shader (gpu/group expressions positions)}
              (catch Exception e {:preparation-error (failure e)}))))

(defn compiler-options [owner batch?]
  (cond-> {:enabled? batch?} (:session owner) (assoc :prepare-group prepare-group)))

(defn- cpu-result! [owner {:keys [cpu-plan positions]} env detail]
  (swap! (:state owner)
         (fn [s] (cond-> (update s :cpu-fallbacks inc)
                   detail (assoc :last-fallback detail)
                   (= :deadline (:stage detail)) (update :deadline-fallbacks inc))))
  (ir/evaluate-batch cpu-plan env positions))

(def ^:private late {:stage :deadline :type :budget-exhausted :message "GPU result exceeded this frame's deadline"})

(defn- draw [session shader env deadline]
  (let [^Future job (gpu/submit! session shader env)]
    (try
      (let [batch (.get job (max 0 (- deadline (System/nanoTime))) TimeUnit/NANOSECONDS)]
        (if (< (System/nanoTime) deadline) {:batch batch} {:error late}))
      (catch TimeoutException _ (.cancel job false) {:error late})
      (catch CancellationException _ {:error {:stage :evaluate :type :cancelled :message "GPU job was cancelled"}})
      (catch ExecutionException e (throw (.getCause e)))
      (catch InterruptedException e
        (.cancel job false)
        (.interrupt (Thread/currentThread))
        (throw e)))))

(defn- evaluate! [owner deadline {:keys [shader preparation-error] :as group} env]
  (cond
    (= :cpu (:active @(:state owner))) (cpu-result! owner group env nil)
    preparation-error (cpu-result! owner group env (assoc preparation-error :stage :prepare))
    (>= (System/nanoTime) deadline) (cpu-result! owner group env late)
    :else
    (let [session (:session owner)
          result (try (draw session shader env deadline)
                      (catch Throwable e {:error (assoc (failure e) :stage :evaluate)}))]
      (if-let [batch (:batch result)]
        (do (swap! (:state owner) update :native-dispatches inc) batch)
        (do (when (:closed? (gpu/status session))
              (swap! (:state owner) assoc :active :cpu))
            (cpu-result! owner group env (:error result)))))))

(defn frame-evaluator
  "Share one absolute GPU deadline and detached readbacks across this frame and
  its renderer fault-isolation retries. Other frames have independent lifetimes."
  [owner frame-start-ns]
  (if (:session owner)
    (fields/frame-evaluator (partial evaluate! owner (+ frame-start-ns (:budget-ns owner))))
    identity))

(defn frame-renderer
  "Resolve each retry of this frame with one field cache and one absolute native
  deadline. Afterglow owns fixture blending, calibration, history and packing."
  [owner frame-start-ns]
  (let [evaluate-fields (frame-evaluator owner frame-start-ns)
        mover (:movement owner)
        deadline (when mover (+ frame-start-ns (:budget-ns owner)))]
    (fn [patch blocks options]
      (let [contributions (evaluate-fields blocks)]
        (if mover
          (movement/resolve-contributions! mover patch contributions (assoc options :deadline-nanos deadline))
          (frame/resolve-contributions patch contributions options))))))
