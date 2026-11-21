(ns tolglow.studio.event-callbacks
  "Runtime-owned execution of event middleware. No frame waits for user code."
  (:require [afterglow.reactive.registry :as reactive]
            [clojure.set :as set]
            [tolglow.studio.event-subscribers :as subscribers])
  (:import [java.util.concurrent ArrayBlockingQueue FutureTask ThreadFactory ThreadPoolExecutor TimeUnit
            RejectedExecutionException Callable]))

(defn create [{:keys [trusted-source? callbacks timeout-ms] :or {timeout-ms 1000}}]
  (let [factory (reify ThreadFactory
                  (newThread [_ runnable]
                    (doto (Thread. runnable "tolglow-event-callback") (.setDaemon true))))]
    {:pool (ThreadPoolExecutor. 2 2 0 TimeUnit/MILLISECONDS (ArrayBlockingQueue. 32) factory)
     :jobs (atom {}) :handlers (atom (or callbacks {}))
     :trusted-source? trusted-source? :timeout-ms timeout-ms}))

(defn register! [service id handler]
  (when-not (and (keyword? id) (fn? handler)) (throw (ex-info "Expected callback id and function" {})))
  (swap! (:handlers service) assoc id handler)
  id)

(defn- invoke [service model run]
  (try
    (let [implementation (get-in run [:steps (:index run) :implementation])
          handler (if (= :registered (:type implementation))
                    (or (get @(:handlers service) (:id implementation))
                        (throw (ex-info "Register this callback in the current runtime" {})))
                    (if (:trusted-source? service)
                      (reactive/resolve-handler {:kind :fx :id ::middleware :implementation implementation} {:trusted-source? true})
                      (throw (ex-info "Trusted callback source or Var execution is disabled on this host" {}))))
          context {:show model :event (:event run) :variables (:variables model {}) :actions []}
          output (handler context (:event run))
          variables (:variables output)
          actions (:actions output)]
      (when-not (and (map? output) (map? variables) (vector? actions)
                     (<= (+ (count actions) (count (:steps run))) 32)
                     (every? #(not= :callback (:op %)) actions))
        (throw (ex-info "Middleware must return its context with variables and built-in actions" {})))
      (run! subscribers/validate-action actions)
      (let [before (:variables context)
            changed (for [k (set/union (set (keys before)) (set (keys variables)))
                          :let [old (find before k) new (find variables k)]
                          :when (not= old new)]
                      (do (when-not (and (reactive/portable? k) (or (nil? new) (reactive/portable? (val new))))
                            (throw (ex-info "Middleware returned a nonportable variable" {})))
                          [k {:before old :after new}]))]
        {:edits (into {} changed) :actions actions}))
    (catch Throwable error
      (when (or (instance? ThreadDeath error) (instance? VirtualMachineError error)) (throw error))
      {:error (or (.getMessage error) "Event callback failed")})))

(defn- start! [service model run]
  (let [task (FutureTask. ^Callable (reify Callable (call [_] (invoke service model run))))]
    (try
      (.execute ^ThreadPoolExecutor (:pool service) task)
      {:task task :started (System/nanoTime)}
      (catch RejectedExecutionException _ {:result {:error "Event callback worker queue is full"}}))))

(defn publish! [service model]
  ;; Only called after the committing owner has installed this exact model.
  (let [jobs @(:jobs service)
        waiting (into {} (keep #(when (:waiting? %) [(subscribers/callback-token model %) %]))
                      (get-in model [:event-subscribers :runs]))]
    (when (or (seq jobs) (seq waiting))
      (doseq [[token {:keys [task]}] jobs :when (and task (not (contains? waiting token)))]
        (.cancel ^FutureTask task true))
      (reset! (:jobs service)
              (reduce-kv (fn [next token run] (assoc next token (or (get jobs token) (start! service model run))))
                         {} waiting)))))

(defn results [service]
  (reduce-kv
   (fn [out token {:keys [result task started]}]
     (cond
       result (assoc out token result)
       (.isDone ^FutureTask task)
       (assoc out token (try (.get ^FutureTask task)
                             (catch Exception _ {:error "Event callback interrupted"})))
       (>= (- (System/nanoTime) started) (* 1000000 (:timeout-ms service)))
       (do (.cancel ^FutureTask task true) (assoc out token {:error "Event callback exceeded its execution deadline"}))
       :else out))
   {} @(:jobs service)))

(defn close! [service]
  (doseq [{:keys [task]} (vals @(:jobs service)) :when task] (.cancel ^FutureTask task true))
  (reset! (:jobs service) {})
  (.shutdownNow ^ThreadPoolExecutor (:pool service)))
