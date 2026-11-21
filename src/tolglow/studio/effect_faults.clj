(ns tolglow.studio.effect-faults
  "Owned asynchronous diagnostics and renderer-attributed effect isolation."
  (:require [afterglow.runtime.engine :as engine])
  (:import [java.util.concurrent ArrayBlockingQueue ThreadPoolExecutor ThreadPoolExecutor$AbortPolicy
            TimeUnit ThreadFactory RejectedExecutionException]
           [java.util.concurrent.atomic AtomicLong]
           [java.util.logging Logger Level]))

(defn resolve-output
  "Resolve once on the healthy path. A renderer-attributed effect failure removes
  only that evaluated attempt. The engine rejects repeated/unknown attribution."
  [result resolve!]
  (loop [result result]
    (let [attempt (try {:output (resolve! (:contributions result))}
                       (catch Exception error {:error error}))]
      (if-let [error (:error attempt)]
        (recur (engine/reject-output result error))
        {:result result :output (:output attempt)}))))

(defn create
  "Creating an owner starts no thread. Logging never runs on the frame owner.
  log! is an optional external sink for structured portable events."
  ([] (create {}))
  ([{:keys [log! queue-size] :or {queue-size 128}}]
   (when-not (and (integer? queue-size) (<= 1 queue-size 4096) (or (nil? log!) (ifn? log!)))
     (throw (ex-info "Invalid effect log configuration" {})))
   (let [logger (Logger/getLogger "tolglow.effects")
         log! (or log! (fn [event] (.log logger (if (= :effect/recovered (:type event)) Level/INFO Level/WARNING)
                                        (pr-str event))))
         pool (ThreadPoolExecutor. 1 1 0 TimeUnit/MILLISECONDS (ArrayBlockingQueue. queue-size)
                                   (reify ThreadFactory
                                     (newThread [_ task]
                                       (doto (Thread. task "tolglow-effect-diagnostics") (.setDaemon true))))
                                   (ThreadPoolExecutor$AbortPolicy.))]
     {:pool pool :log! log! :dropped (AtomicLong. 0) :failed (AtomicLong. 0)})))

(defn offer! [owner events]
  (doseq [event events]
    (try
      (.execute ^ThreadPoolExecutor (:pool owner)
                ^Runnable (fn []
                            (try ((:log! owner) event)
                                 (catch Exception _ (.incrementAndGet ^AtomicLong (:failed owner))))))
      (catch RejectedExecutionException _ (.incrementAndGet ^AtomicLong (:dropped owner)))))
  nil)

(defn status [owner]
  {:queued (.size (.getQueue ^ThreadPoolExecutor (:pool owner)))
   :dropped (.get ^AtomicLong (:dropped owner)) :failed (.get ^AtomicLong (:failed owner))})

(defn close! [owner]
  (.shutdownNow ^ThreadPoolExecutor (:pool owner))
  nil)
