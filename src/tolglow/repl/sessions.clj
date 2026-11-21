(ns tolglow.repl.sessions
  "Inert owner of up to eight trusted-user REPL sessions. Saved history/scratch
  does not create namespaces or workers until an explicit session operation."
  (:refer-clojure :exclude [eval])
  (:require [tolglow.repl :as repl]))

(defn- fail! [code message] (throw (ex-info message {:repl/error code})))
(defn- id! [id]
  (let [id (if (keyword? id) (name id) (or id "main"))]
    (when-not (and (string? id) (re-matches #"[A-Za-z0-9_.-]{1,64}" id))
      (fail! :invalid-session "REPL session IDs use 1..64 letters, digits, dots, underscores or hyphens"))
    id))
(defn- empty-session [] {:schema-version 1 :history [] :scratch {}})

(defn validate-state
  "Validate saved session text without creating a namespace or evaluating forms."
  [data]
  (when-not (and (map? data) (not (record? data)) (= #{:version :sessions :selected-session-id} (set (keys data)))
                 (= 1 (:version data)) (map? (:sessions data)) (<= 1 (count (:sessions data)) 8)
                 (string? (:selected-session-id data))
                 (contains? (:sessions data) (:selected-session-id data)))
    (fail! :invalid-state "Invalid saved REPL manager state"))
  (doseq [[id saved] (:sessions data)]
    (when-not (and (string? id) (= id (id! id)) (empty? (repl/validate-state saved)))
      (fail! :invalid-state "Invalid saved REPL session history or scratch buffers")))
  data)

(defn create-manager
  "Create inert ownership. :runtime-supplier is called only when opening a live
  session, outside the owner lock; it should return the current runtime handle.
  :session-options supplies queue/output/history bounds and optional bindings.
  :state is portable saved data; it never evaluates on create/restore."
  ([] (create-manager {}))
  ([{:keys [max-sessions runtime-supplier session-options state open-timeout-ms]
     :or {max-sessions 8 runtime-supplier (constantly nil) session-options {} open-timeout-ms 2000}}]
   (when-not (and (integer? max-sessions) (<= 1 max-sessions 8) (ifn? runtime-supplier)
                  (map? session-options) (integer? open-timeout-ms) (<= 1 open-timeout-ms 10000))
     (fail! :invalid-options "Invalid REPL manager bounds/runtime supplier"))
   (let [saved (validate-state (or state {:version 1 :sessions {"main" (empty-session)} :selected-session-id "main"}))]
     (when (> (count (:sessions saved)) max-sessions) (fail! :session-limit "Saved REPL sessions exceed the configured limit"))
     {:lock (Object.) :runtime-supplier runtime-supplier :session-options session-options
      :max-sessions max-sessions :open-timeout-ms open-timeout-ms
      :state (atom {:closed? false :saved saved :sessions {} :opening {}})})))

(defn- current! [manager]
  (let [state @(:state manager)]
    (when (:closed? state) (fail! :closed "REPL manager is closed")) state))

(defn session!
  "Open/reuse an owned session explicitly. Namespace/worker creation is lazy.
  Concurrent opens share one result; closed manager generations cannot publish it."
  ([manager] (session! manager "main"))
  ([manager id]
   (let [id (id! id)
         request
         (locking (:lock manager)
           (let [state (current! manager) live (get-in state [:sessions id])
                 status (when live (:status (repl/snapshot live)))
                 state (if (= :closed status)
                         (-> state (assoc-in [:saved :sessions id] (repl/export-state live))
                             (update :sessions dissoc id)) state)
                 state (cond-> state (= :open status) (assoc-in [:saved :selected-session-id] id))]
             (reset! (:state manager) state)
             (cond
               (= :open status) {:session live}
               (= :closing status) (fail! :closing "The previous session is still stopping cooperatively")
               (get-in state [:opening id]) {:waiting (get-in state [:opening id])}
               :else
               (let [known? (contains? (get-in state [:saved :sessions]) id)
                     _ (when (and (not known?) (>= (count (get-in state [:saved :sessions])) (:max-sessions manager)))
                         (fail! :session-limit "Maximum owned REPL sessions reached"))
                     completion (promise)]
                 (reset! (:state manager) (cond-> (assoc-in state [:opening id] completion)
                                           (not known?) (assoc-in [:saved :sessions id] (empty-session))))
                 {:opening completion :saved (get-in @(:state manager) [:saved :sessions id])}))))]
     (cond
       (:session request) (:session request)
       (:waiting request)
       (let [value (deref (:waiting request) (:open-timeout-ms manager) ::timeout)]
         (cond (= ::timeout value) (fail! :open-timeout "REPL session creation is still in progress")
               (instance? Throwable value) (throw value) :else value))
       :else
       (let [candidate (atom nil)]
         (try
           (let [session (repl/open! (assoc (:session-options manager) :session-id id :runtime ((:runtime-supplier manager))))]
             (reset! candidate session)
             (repl/restore-history! session (:saved request))
             (locking (:lock manager)
               (let [state (current! manager)]
                 (reset! (:state manager) (-> state (assoc-in [:sessions id] session)
                                             (assoc-in [:saved :selected-session-id] id) (update :opening dissoc id)))))
             (deliver (:opening request) session)
             session)
           (catch Throwable error
             (when @candidate (repl/close! @candidate))
             (locking (:lock manager) (swap! (:state manager) update :opening dissoc id))
             (deliver (:opening request) error)
             (throw error))))))))

(defn snapshot
  "Read-only status. An unopened session stays inert; no runtime supplier runs."
  ([manager]
   (let [state (current! manager)]
     {:selected-session-id (get-in state [:saved :selected-session-id])
      :sessions (mapv #(snapshot manager %) (sort (keys (get-in state [:saved :sessions]))))}))
  ([manager id]
   (let [id (id! id) state (current! manager)]
     (if-let [session (get-in state [:sessions id])] (repl/snapshot session)
       (if-let [saved (get-in state [:saved :sessions id])]
         (merge saved {:session-id id :status (if (get-in state [:opening id]) :opening :not-started)
                       :namespace nil :debug-namespace nil :pending [] :active nil :observer-errors []})
         (fail! :unknown-session "Unknown REPL session"))))))

(defn open-session! [manager id]
  (session! manager id) (snapshot manager id))

(defn submit!
  ([manager id source] (submit! manager id source {}))
  ([manager id source options] (repl/submit! (session! manager id) source options)))

(defn interrupt!
  ([manager id] (interrupt! manager id nil))
  ([manager id request-id]
   (let [state (current! manager)]
     (if-let [session (get-in state [:sessions (id! id)])] (repl/interrupt! session request-id)
       {:status :idle-or-stale :request-id request-id}))))

(defn complete
  ([manager id prefix] (repl/complete (session! manager id) prefix))
  ([manager id prefix limit] (repl/complete (session! manager id) prefix limit)))
(defn lookup [manager id symbol] (repl/lookup (session! manager id) symbol))
(def cancel-request! interrupt!)

(defn set-scratch!
  "Saving text does not open a session or evaluate it."
  [manager id name source]
  (let [id (id! id)]
    (locking (:lock manager)
      (let [state (current! manager)]
        (if-let [session (get-in state [:sessions id])]
          (repl/set-scratch! session name source)
          (let [saved (or (get-in state [:saved :sessions id]) (fail! :unknown-session "Unknown REPL session"))
                next (assoc-in saved [:scratch name] source)]
            (when (seq (repl/validate-state next)) (fail! :invalid-scratch "Invalid scratch buffer name/text"))
            (reset! (:state manager) (assoc-in state [:saved :sessions id] next))
            {:name name :source source}))))))

(defn export-state [manager]
  (let [state @(:state manager)]
    (reduce-kv (fn [saved id session] (assoc-in saved [:sessions id] (repl/export-state session)))
               (:saved state) (:sessions state))))

(defn close-session! [manager id]
  (let [id (id! id) session (get-in (current! manager) [:sessions id])]
    (if-not session {:status :not-started :session-id id}
      (let [result (repl/close! session)]
        (locking (:lock manager)
          (when (identical? session (get-in @(:state manager) [:sessions id]))
            (reset! (:state manager) (cond-> (assoc-in @(:state manager) [:saved :sessions id] (repl/export-state session))
                                      (= :closed (:status result)) (update :sessions dissoc id)))))
        (assoc result :session-id id)))))

(defn restore!
  "Restore text into an inert manager only. Resource owners should stage a new
  manager and retire the old one when replacing a show, never replay history."
  [manager data]
  (let [data (validate-state data)]
    (locking (:lock manager)
      (let [state (current! manager)]
        (when (or (seq (:sessions state)) (seq (:opening state)))
          (fail! :busy "Restore saved REPL state before opening sessions"))
        (when (> (count (:sessions data)) (:max-sessions manager)) (fail! :session-limit "Saved sessions exceed the limit"))
        (reset! (:state manager) (assoc state :saved data))))
    (export-state manager)))

(defn close!
  "Invalidate the manager before requesting cooperative session shutdown. Report
  :stopped? false if any user code/namespace creation still has not returned."
  [manager]
  (let [sessions (locking (:lock manager)
                   (swap! (:state manager) assoc :closed? true)
                   (vals (:sessions @(:state manager))))
        results (mapv repl/close! sessions)]
    {:closed? true :stopped? (and (empty? (:opening @(:state manager))) (every? #(= :closed (:status %)) results))
     :sessions results}))
