(ns tolglow.repl
  "Explicit trusted-user JVM REPL boundary. Each opened session owns a serial
  bounded queue, namespace, debug namespace and live values. Saved data contains
  only source/history/scratch text; loading it never evaluates code."
  (:require [afterglow.reactive.registry :as reactive-registry]
            [clojure.string :as str])
  (:import [java.io PushbackReader StringReader Writer]
           [java.util UUID]
           [java.util.concurrent ArrayBlockingQueue CountDownLatch TimeUnit]))

(def ^:dynamic *session* nil)
(def ^:dynamic *runtime* nil)

(defn- text-limit [value limit] (let [s (str value)] (subs s 0 (min limit (count s)))))
(defn- change! [session f & args]
  (locking (:lock session) (reset! (:state session) (apply f @(:state session) args))))
(defn- open-session! [session]
  (when-not (= :open (:status @(:state session)))
    (throw (ex-info "REPL session is closed or closing" {:type ::closed :session-id (:id session)}))))

(defn- emit! [session job event]
  (let [state (change! session update :event-sequence inc)
        event (merge event {:session-id (:id session) :request-id (:id job) :sequence (:event-sequence state)})]
    ;; User callbacks execute after the lock has been released and never in CAS.
    (doseq [callback (distinct (remove nil? [(:on-event (:options session)) (:on-event job)]))]
      (try (binding [*out* (:host-out session) *err* (:host-err session)] (callback event))
           (catch Throwable error
             (change! session update :observer-errors #(vec (take-last 8 (conj % (text-limit (.getMessage error) 512))))))))
    event))

(defn- text-writer [consume]
  (proxy [Writer] []
    (write
      ([value] (consume (cond (string? value) value (number? value) (str (char value)) :else (String. ^chars value))))
      ([value offset length]
       (consume (if (string? value) (.substring ^String value (int offset) (int (+ offset length)))
                    (String. ^chars value (int offset) (int length))))))
    (flush []) (close [])))

(defn- bounded-print [value limit]
  (let [buffer (StringBuilder.) truncated? (volatile! false)
        writer (text-writer
                 (fn [text]
                   (let [remaining (- limit (.length buffer)) n (min remaining (count text))]
                     (when (pos? n) (.append buffer ^String text 0 (int n)))
                     (when (< n (count text)) (vreset! truncated? true)))))]
    (binding [*out* writer *print-length* 64 *print-level* 10 *print-meta* false]
      (pr value))
    {:value (str buffer) :truncated? @truncated?}))

(defn- reserve-output! [session job text]
  (let [allocation
        (locking (:lock session)
          (let [state @(:state session) active (:active state)]
            (when (= (:id job) (:id active))
              (let [remaining (- (get-in session [:options :max-output-chars]) (:output-chars active))
                    n (min remaining (count text))
                    clipped (subs text 0 n)
                    truncated? (and (< n (count text)) (not (:truncated? active)))]
                (reset! (:state session) (-> state
                                            (update-in [:active :output-chars] + n)
                                            (cond-> truncated? (assoc-in [:active :truncated?] true))))
                {:text clipped :truncated? (< n (count text)) :first-truncation? truncated?}))))]
    (when (:first-truncation? allocation)
      (emit! session job {:type :output-truncated :limit (get-in session [:options :max-output-chars])}))
    allocation))

(defn- output! [session job type text]
  (when-let [allocation (reserve-output! session job text)]
    (doseq [chunk (partition-all 4096 (:text allocation))]
      (emit! session job {:type type :text (apply str chunk)}))))

(defn- output-writer [session job type]
  (text-writer #(output! session job type %)))

(defn- interrupted-error? [error]
  (boolean (some #(instance? InterruptedException %) (take-while some? (iterate #(.getCause ^Throwable %) error)))))

(defn- history! [session entry]
  (change! session update :history #(vec (take-last (get-in session [:options :history-limit]) (conj % entry)))))

(defn- finish! [session job result]
  (let [result (assoc result :request-id (:id job))]
    (history! session (merge {:request-id (:id job) :source (:source job)
                             :namespace (str (ns-name (:namespace @(:state session))))}
                            (select-keys result [:status :result :error :interrupt-requested?])))
    (emit! session job (assoc result :type :done))
    (locking (:lock session)
      (when (= (:id job) (get-in @(:state session) [:active :id]))
        (reset! (:state session) (assoc @(:state session) :active nil))))
    (deliver (:completion job) result)
    result))

(defn- evaluate! [session job initial]
  (let [values (:values initial)
        final-result (atom nil)]
    (emit! session job {:type :started})
    (binding [*session* session *runtime* (get-in session [:options :runtime])
              *ns* (:namespace initial) *out* (output-writer session job :out)
              *err* (output-writer session job :err)
              *1 (:one values) *2 (:two values) *3 (:three values) *e (:error values)
              *read-eval* false *data-readers* {} *default-data-reader-fn* nil]
      (try
        (with-open [reader (PushbackReader. (StringReader. (:source job)))]
          (let [eof (Object.)]
            (loop [index 0 last-result nil]
              (let [form (read {:eof eof} reader)]
                (if (identical? eof form)
                  (reset! final-result {:status :completed :result last-result})
                  (let [_ (when (>= index (get-in session [:options :max-forms]))
                            (throw (ex-info "REPL request exceeds its form count limit" {:type ::form-limit})))
                        value (eval form)
                        printed (bounded-print value (get-in session [:options :max-result-chars]))
                        allocation (reserve-output! session job (:value printed))
                        printed (assoc printed :value (:text allocation "")
                                               :truncated? (or (:truncated? printed) (:truncated? allocation)))]
                    (set! *3 *2) (set! *2 *1) (set! *1 value)
                    (emit! session job (merge {:type :result :form-index index} printed))
                    (recur (inc index) (:value printed))))))))
        (catch Throwable error
          (set! *e error)
          (let [status (if (interrupted-error? error) :interrupted :error)
                info {:class (.getName (class error)) :message (text-limit (.getMessage error) 2048)
                      :data (:value (bounded-print (ex-data error) 2048))}]
            (emit! session job (assoc info :type :error :status status))
            (reset! final-result {:status status :error info})))
        (finally
          (change! session assoc :namespace *ns* :values {:one *1 :two *2 :three *3 :error *e}))))
    (let [requested? (get-in @(:state session) [:active :interrupt-requested?])]
      ;; An interrupt ignored by user code is a completed evaluation, not a
      ;; successful hard cancellation. Clear its flag before the next queue job.
      (Thread/interrupted)
      (change! session assoc-in [:active :phase] :finishing)
      (finish! session job (assoc @final-result :interrupt-requested? (boolean requested?))))))

(defn- cancelled! [session job]
  (change! session #(-> % (update :pending (fn [ids] (vec (remove #{(:id job)} ids))))
                         (update :cancelled-requests disj (:id job))))
  (finish! session job {:status :cancelled :interrupt-requested? false}))

(defn- claim-job! [session job]
  (locking (:lock session)
    (let [state @(:state session)]
      (when (and (= :open (:status state)) (not (contains? (:cancelled-requests state) (:id job))))
        (reset! (:state session)
                (-> state (assoc :active {:id (:id job) :phase :running :output-chars 0 :interrupt-requested? false})
                    (update :pending (fn [ids] (vec (remove #{(:id job)} ids))))))
        state))))

(defn open!
  "Explicitly open an owned REPL. Required :session-id; optional :runtime and
  :bindings {unqualified-symbol live-value} are interned only in its namespace.
  Queue/output/history bounds and :on-event are host options. No code evaluates
  until submit!. Sessions are trusted-code tools, not execution sandboxes."
  [{:keys [session-id bindings] :as options}]
  (when-not (and (or (keyword? session-id) (and (string? session-id) (seq session-id)))
                 (or (nil? bindings) (and (map? bindings) (every? #(and (symbol? %) (nil? (namespace %))) (keys bindings)))))
    (throw (ex-info "REPL needs an ID and unqualified symbol bindings" {})))
  (let [options (merge {:queue-size 16 :max-source-chars 131072 :max-output-chars 65536
                        :max-result-chars 8192 :max-forms 256 :history-limit 100 :close-timeout-ms 1000} options)]
    (doseq [key [:queue-size :max-source-chars :max-output-chars :max-result-chars :max-forms :history-limit :close-timeout-ms]]
      (when-not (and (integer? (get options key)) (<= 1 (get options key) 1000000))
        (throw (ex-info "REPL limits must be positive bounded integers" {:field key}))))
    (let [suffix (str/replace (str (UUID/randomUUID)) "-" "_")
          namespace (create-ns (symbol (str "tolglow.repl.session_" suffix)))
          debug (create-ns (symbol (str "tolglow.repl.debug_" suffix)))
          state (atom {:status :open :namespace namespace :debug-namespace debug
                       :event-sequence 0 :request-sequence 0 :active nil :pending [] :cancelled-requests #{}
                       :history [] :scratch {} :values {} :observer-errors []})
          session {:id session-id :token suffix :options options :state state :lock (Object.) :host-out *out* :host-err *err*
                   :queue (ArrayBlockingQueue. (:queue-size options)) :worker (atom nil)
                   :owned-namespaces [namespace debug] :stopped (CountDownLatch. 1)}
          worker (Thread.
                   (fn []
                     (try
                       (loop []
                         (when (= :open (:status @state))
                           (let [job (try (.take ^ArrayBlockingQueue (:queue session))
                                          (catch InterruptedException _ nil))]
                             (when job
                               (try @(:admitted job) (catch InterruptedException _ nil))
                               (if-let [initial (claim-job! session job)]
                                 (try (evaluate! session job initial)
                                      (catch Throwable error
                                        (change! session assoc :active nil)
                                        (finish! session job {:status :error :error {:class (.getName (class error))
                                                                                   :message (text-limit (.getMessage error) 2048)}})))
                                 (cancelled! session job)))
                             (recur))))
                       (finally
                         (change! session assoc :status :closed :active nil)
                         (doseq [owned (:owned-namespaces session)]
                           (when (identical? owned (find-ns (ns-name owned))) (remove-ns (ns-name owned))))
                         (.countDown ^CountDownLatch (:stopped session)))))
                   (str "tolglow-repl-" session-id))]
      (binding [*ns* namespace] (clojure.core/refer 'clojure.core))
      (doseq [n [namespace debug]] (alter-meta! n assoc ::reactive-registry/temporary true))
      (intern namespace 'runtime (:runtime options))
      (intern namespace 'debug-ns (ns-name debug))
      (doseq [[name value] bindings] (intern namespace name value))
      (.setDaemon worker true) (reset! (:worker session) worker) (.start worker)
      session)))

(defn submit!
  "Admit explicit source. Returns {:status :queued :request-id :completion} or
  {:status :rejected :reason :queue-full}; completion is a promise. Optional
  :on-event streams only this request. Multiple forms run serially in one job."
  ([session source] (submit! session source {}))
  ([session source {:keys [on-event request-id]}]
   (when-not (and (string? source) (<= (count source) (get-in session [:options :max-source-chars])))
     (throw (ex-info "REPL source must be a bounded string" {:type ::invalid-source})))
   (when-not (or (nil? request-id) (and (string? request-id) (<= 1 (count request-id) 256)))
     (throw (ex-info "REPL request-id must be a nonempty bounded string" {:type ::invalid-request-id})))
   (let [job (locking (:lock session)
               (open-session! session)
               (let [state @(:state session) sequence (inc (:request-sequence state))
                     id (or request-id (str (:id session) ":" (:token session) ":" sequence))
                     duplicate? (or (= id (get-in state [:active :id])) (some #{id} (:pending state))
                                    (some #(= id (:request-id %)) (:history state)))
                     job {:id id :source source :on-event on-event
                          :completion (promise) :admitted (promise)}]
                 (if duplicate? ::duplicate
                   (when (.offer ^ArrayBlockingQueue (:queue session) job)
                     (reset! (:state session) (-> state (assoc :request-sequence sequence)
                                                 (update :pending conj (:id job))))
                     job))))]
     (cond (= ::duplicate job) {:status :rejected :reason :duplicate-request-id :request-id request-id}
           (nil? job) {:status :rejected :reason :queue-full}
           :else
       (do (try (emit! session job {:type :queued}) (finally (deliver (:admitted job) true)))
           {:status :queued :request-id (:id job) :completion (:completion job)})))))

(defn interrupt!
  "Request cooperative interruption of the current matching request. JVM code
  may catch/ignore interruption; the serial worker cannot run more work until
  that code returns. No Thread.stop or unsafe hard-kill is attempted."
  ([session] (interrupt! session nil))
  ([session request-id]
   (let [result
         (locking (:lock session)
           (open-session! session)
           (let [state @(:state session) active (:active state)]
             (cond
               (and (= :running (:phase active)) (or (nil? request-id) (= request-id (:id active))))
               (do (reset! (:state session) (assoc-in state [:active :interrupt-requested?] true))
                   (.interrupt ^Thread @(:worker session))
                   {:status :interrupt-requested :request-id (:id active) :cooperative? true})
               (and request-id (some #{request-id} (:pending state)))
               (let [job (some #(when (= request-id (:id %)) %) (seq (:queue session)))]
                 (if (and job (.remove ^ArrayBlockingQueue (:queue session) job))
                   {:status :cancelled :request-id request-id :job job}
                   (do (reset! (:state session) (update state :cancelled-requests conj request-id))
                       {:status :cancel-requested :request-id request-id})))
               :else {:status :idle-or-stale :request-id request-id})))]
     (when-let [job (:job result)] (cancelled! session job))
     (dissoc result :job))))

(def cancel-request! interrupt!)

(defn close!
  "Refuse new work, cancel queued requests and request interruption. Wait at most
  the configured bound; :closing means code ignored interruption. Owned namespaces
  are removed only after the worker actually exits, never while code is running."
  [session]
  (let [cancelled (locking (:lock session)
                    (when (= :open (:status @(:state session)))
                      (reset! (:state session) (cond-> (assoc @(:state session) :status :closing)
                                               (:active @(:state session))
                                               (assoc-in [:active :interrupt-requested?] true)))
                      (loop [jobs []]
                        (if-let [job (.poll ^ArrayBlockingQueue (:queue session))] (recur (conj jobs job)) jobs))))]
    (doseq [job cancelled]
      (deliver (:admitted job) true)
      (change! session update :pending #(vec (remove #{(:id job)} %)))
      (cancelled! session job))
    (when-not (identical? (Thread/currentThread) @(:worker session))
      (.interrupt ^Thread @(:worker session))
      (.await ^CountDownLatch (:stopped session) (get-in session [:options :close-timeout-ms]) TimeUnit/MILLISECONDS))
    {:status (:status @(:state session)) :cooperative? true}))

(defn snapshot [session]
  (let [state @(:state session)]
    {:session-id (:id session) :status (:status state) :namespace (str (ns-name (:namespace state)))
     :debug-namespace (str (ns-name (:debug-namespace state))) :pending (:pending state)
     :active (some-> (:active state) (select-keys [:id :output-chars :truncated? :interrupt-requested?]))
     :history (:history state) :scratch (:scratch state) :observer-errors (:observer-errors state)}))

(defn export-state [session]
  (select-keys (assoc @(:state session) :schema-version 1) [:schema-version :history :scratch]))

(defn validate-state
  "Pure validation of portable REPL history/scratch, without namespace creation,
  source reading or evaluation. Live values and opaque metadata are rejected."
  [data]
  (let [bounded-source? #(and (string? %) (<= (count %) 1000000))
        optional-string? #(or (nil? %) (bounded-source? %))]
    (if (and (map? data) (not (record? data)) (= #{:schema-version :history :scratch} (set (keys data)))
             (= 1 (:schema-version data)) (vector? (:history data)) (<= (count (:history data)) 1000)
             (map? (:scratch data)) (<= (count (:scratch data)) 32)
             (every? #(and (string? (key %)) (<= 1 (count (key %)) 128) (bounded-source? (val %))) (:scratch data))
             (every? #(and (map? %) (not (record? %)) (bounded-source? (:source %))
                          (every? #{:request-id :source :namespace :status :result :error :interrupt-requested?} (keys %))
                          (and (string? (:request-id %)) (<= (count (:request-id %)) 256))
                          (and (string? (:namespace %)) (<= (count (:namespace %)) 256))
                          (#{:completed :error :interrupted :cancelled} (:status %))
                          (optional-string? (:result %))
                          (or (nil? (:interrupt-requested? %)) (boolean? (:interrupt-requested? %)))
                          (or (nil? (:error %))
                              (and (map? (:error %)) (not (record? (:error %)))
                                   (every? #{:class :message :data} (keys (:error %)))
                                   (every? (fn [value] (and (string? value) (<= (count value) 8192))) (vals (:error %)))))) (:history data)))
      [] [{:path [] :message "Invalid portable REPL history/scratch data"}])))

(defn restore-history!
  "Restore only portable history and named scratch buffers. Never evaluate saved
  source or restore live result/local/runtime objects. Current namespaces stay owned."
  [session data]
  (when (seq (validate-state data))
    (throw (ex-info "Invalid portable REPL history/scratch data" {:type ::invalid-history})))
  (locking (:lock session)
    (open-session! session)
    (when (or (:active @(:state session)) (seq (:pending @(:state session))))
      (throw (ex-info "Restore history only while the session is idle" {:type ::busy})))
    (reset! (:state session) (assoc @(:state session)
                                  :history (vec (take-last (get-in session [:options :history-limit]) (:history data)))
                                  :scratch (:scratch data))))
  (export-state session))

(defn set-scratch! [session name source]
  (when-not (and (string? name) (<= 1 (count name) 128) (string? source)
                 (<= (count source) (get-in session [:options :max-source-chars])))
    (throw (ex-info "Scratch buffers need a bounded name and source text" {})))
  (locking (:lock session)
    (open-session! session)
    (when (and (not (contains? (:scratch @(:state session)) name)) (>= (count (:scratch @(:state session))) 32))
      (throw (ex-info "REPL scratch buffer limit reached" {})))
    (reset! (:state session) (assoc-in @(:state session) [:scratch name] source)))
  {:name name :source source})

(defn- target-ns [session prefix]
  (let [current (:namespace @(:state session))]
    (if (seq prefix) (or (get (ns-aliases current) (symbol prefix)) (find-ns (symbol prefix))) current)))

(defn- var-info [candidate value]
  (let [metadata (when (var? value) (meta value))]
    {:candidate candidate :namespace (some-> metadata :ns ns-name str)
     :name (or (some-> metadata :name str) candidate) :kind (cond (:macro metadata) :macro (var? value) :var :else :class)
     :doc (some-> (:doc metadata) (text-limit 8192)) :arglists (some-> (:arglists metadata) (bounded-print 2048) :value)
     :file (:file metadata) :line (:line metadata)}))

(defn complete
  "Complete loaded vars/classes/namespaces without evaluating source or loading
  arbitrary namespaces. Prefix and result count are bounded."
  ([session prefix] (complete session prefix 50))
  ([session prefix limit]
   (open-session! session)
   (when-not (and (string? prefix) (<= (count prefix) 256) (integer? limit) (<= 1 limit 200))
     (throw (ex-info "Invalid completion query" {})))
   (let [[qualifier part] (if (str/includes? prefix "/") (str/split prefix #"/" 2) [nil prefix])
         namespace (target-ns session qualifier)
         variables (when namespace
                     (for [[name value] (ns-map namespace) :when (str/starts-with? (str name) part)]
                       (var-info (str (when qualifier (str qualifier "/")) name) value)))
         namespaces (when-not qualifier
                      (for [namespace (all-ns) :let [name (str (ns-name namespace))] :when (str/starts-with? name prefix)]
                        {:candidate (str name "/") :namespace name :name name :kind :namespace}))]
     (vec (take limit (sort-by :candidate (concat variables namespaces)))))))

(defn lookup [session name]
  (open-session! session)
  (when-not (and (string? name) (<= 1 (count name) 256)) (throw (ex-info "Invalid documentation query" {})))
  (let [[qualifier part] (if (str/includes? name "/") (str/split name #"/" 2) [nil name])]
    (when-let [namespace (target-ns session qualifier)]
      (when-let [value (ns-resolve namespace (symbol part))] (var-info name value)))))

(defn expose-locals!
  "Runtime half of capture-locals!. Live values stay only in the owned debug
  namespace; their identities are never included in portable history/scratch."
  [session values]
  (when-not session (throw (ex-info "Capture locals inside a REPL request or pass its session explicitly" {})))
  (locking (:lock session)
    (open-session! session)
    (let [namespace (:debug-namespace @(:state session))]
      (doseq [[name value] values] (intern namespace name value))
      {:namespace (str (ns-name namespace)) :locals (mapv str (sort (keys values)))})))

(defmacro capture-locals!
  "Capture lexical let/fn locals when this form executes, without rewriting let
  or evaluating forms during macro expansion. Optional explicit owned session is
  useful in a later callback. Use only in developer code, outside pure effects."
  ([] `(capture-locals! *session*))
  ([session]
   `(expose-locals! ~session ~(into {} (map (fn [name] [(list 'quote name) name])) (keys &env)))))
