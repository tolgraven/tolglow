(ns tolglow.events.callbacks
  "Runtime-owned execution of event middleware. No frame waits for user code."
  (:require [afterglow.reactive.registry :as reactive]
            [clojure.set :as set]
            [tolglow.event-subscribers :as subscribers])
  (:import [java.util.concurrent ArrayBlockingQueue FutureTask ThreadFactory ThreadPoolExecutor TimeUnit
            RejectedExecutionException Callable]))

(defn create [{:keys [trusted-source? callbacks timeout-ms] :or {timeout-ms 1000}}]
  (let [factory (reify ThreadFactory
                  (newThread [_ runnable]
                    (doto (Thread. runnable "tolglow-event-callback") (.setDaemon true))))]
    {:pool (ThreadPoolExecutor. 2 2 0 TimeUnit/MILLISECONDS (ArrayBlockingQueue. 32) factory)
     :jobs (atom {}) :sources (atom {}) :handlers (atom (or callbacks {})) :inline-handlers (atom {})
     :inline-inputs (volatile! nil)
     :trusted-source? trusted-source? :timeout-ms timeout-ms}))

(defn register!
  "Register a named host callback."
  [service id handler]
  (when-not (and (keyword? id) (fn? handler)) (throw (ex-info "Expected callback id and function" {})))
  (swap! (:handlers service) assoc id handler)
  id)

(defn- inline-key [subscriber-id {:keys [type callback-id]}]
  (when (= :inline type) [subscriber-id callback-id]))

(defn- retain-inline! [service {:keys [subscribers runs]} registrations]
  (let [handlers (:inline-handlers service)
        inputs (:inline-inputs service)
        active @handlers
        [checked-subscribers checked-runs] @inputs]
    (when (or (seq registrations)
              (and (seq active)
                   (not (and (identical? subscribers checked-subscribers)
                             (identical? runs checked-runs)))))
      (let [available (into active registrations)
            steps (concat (map (juxt :id :actions) subscribers) (map (juxt :subscriber :steps) runs))
            referenced (fn [[id actions]]
                         (keep #(when-let [key (inline-key id (:implementation %))]
                                  (find available key))
                               actions))
            retained (into {} (mapcat referenced) steps)]
        (reset! handlers retained)
        (vreset! inputs (when (seq retained) [subscribers runs]))))))

(defn- resolve-handler [service implementation inline-handler]
  (let [resolve-fn #(reactive/resolve-handler {:kind :fx :id ::middleware :implementation implementation}
                                             {:trusted-source? true})]
    (cond
      (= :inline (:type implementation))
      (or inline-handler (throw (ex-info "Process-local callback unavailable. Re-register the middleware in this runtime." {})))

      (= :registered (:type implementation))
      (or (get @(:handlers service) (:id implementation))
          (throw (ex-info "Register this named callback in the current host's :event-callback-options :callbacks" {})))

      (not (:trusted-source? service))
      (throw (ex-info "Trusted callback source or Var execution is disabled on this host" {}))

      (= :source (:type implementation))
      (or (get @(:sources service) implementation)
          (let [handler (resolve-fn)]
            (swap! (:sources service)
                   (fn [cache]
                     (assoc (cond-> cache (>= (count cache) 128) (dissoc (ffirst cache)))
                            implementation handler)))
            handler))

      :else (resolve-fn))))

(defn- invoke [service model run implementation inline-handler]
  (try
    (let [handler (resolve-handler service implementation inline-handler)
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
      {:error (or (.getMessage error) "Event callback failed") :diagnostic (subscribers/error-details error)})))

(defn- start! [service model run]
  (let [implementation (get-in run [:steps (:index run) :implementation])
        ;; A queued job holds the exact closure it was started with even if a
        ;; later commit retires its registration and cancels the job.
        inline-handler (when-let [key (inline-key (:subscriber run) implementation)]
                         (get @(:inline-handlers service) key))
        task (FutureTask. ^Callable (reify Callable (call [_] (invoke service model run implementation inline-handler))))]
    (try
      (.execute ^ThreadPoolExecutor (:pool service) task)
      {:task task :started (System/nanoTime)}
      (catch RejectedExecutionException _ {:result {:error "Event callback worker queue is full"}}))))

(defn publish! [service model registrations]
  ;; Only called after the committing owner has installed this exact model.
  (retain-inline! service (:event-subscribers model) registrations)
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
  (reset! (:inline-handlers service) {})
  (vreset! (:inline-inputs service) nil)
  (.shutdownNow ^ThreadPoolExecutor (:pool service)))
