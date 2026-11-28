(ns tolglow.cue-timing
  "Owner boundary for grid-scheduled cue launches and their local receipts."
  (:require [afterglow.control.quantization :as quantization]
            [afterglow.runtime.events :as staging]))

(def ^:private path [:control ::queue])

(def ^:private state-path (conj path :state))

(def ^:private early-path (conj path :early-releases))

(def ^:private held-lease-ms 10000)

(def ^:private feedback-path (conj path :feedback))

;; Keep a full tick's clock-fenced pending launches plus ordinary intake.
(def ^:private feedback-limit 384)

(defn active? [model] (some? (get-in model path)))

(defn without-queue [model]
  (update (dissoc model ::fired-outcomes) :control dissoc ::queue))

(defn fired-outcomes [model] (::fired-outcomes model))

(defn clear-fired-outcomes [model] (dissoc model ::fired-outcomes))

(defn receipt [model id]
  (quantization/receipt (get-in model state-path) id))

(defn pending [model]
  (mapv #(update % :due-beat double) (quantization/pending (get-in model state-path))))

(defn scheduled-commands
  "The launch commands waiting for their grid boundary, in intake order, so
  the owner can prepare what they will need before they fire."
  [model]
  (mapv #(get-in % [:request :command])
        (sort-by #(get-in % [:receipt :order]) (vals (:pending (get-in model state-path))))))

(defn outcomes [model] (get-in model feedback-path []))

(defn source [model id] (quantization/source (get-in model state-path) id))

(defn- source-owner [source]
  (if (map? source)
    (let [fields (select-keys source [:adapter :session-id :type :bridge-id :sync-run-id :source-id :note])]
      (cond
        (= "web" (:adapter source)) (assoc fields :adapter :web)
        (seq fields) fields))
    source))

(defn- web-source? [{:keys [adapter session-id]}]
  (and (#{:web "web"} adapter) (string? session-id) (seq session-id)))

(defn- held-source? [{:keys [adapter session-id type sync-run-id note] :as source}]
  (or (web-source? source)
      (and (#{:push-2 :push-3} adapter) (some? session-id))
      (and (= :ableton type) sync-run-id (some? note))))

(defn- emit [model event receipt]
  (let [status (:status receipt)
        owner (when (= :missed status)
                (source-owner (source model (:command-id receipt))))
        model (if (web-source? owner)
                (let [outcome (assoc (select-keys receipt [:command-id :cue-id :status :reason :code :instance-id])
                                     :source (select-keys owner [:adapter :session-id]))]
                  (update-in model feedback-path
                             (fn [previous] (->> (conj (or previous []) outcome)
                                                 (take-last feedback-limit)
                                                 vec))))
                model)]
    (staging/collect model :control {:kind :runtime :id :cue-timing}
                     [(assoc receipt :type event)])))

(defn- clock [model state time-ms]
  (assoc (select-keys (:transport model) [:beat :meter :playing?])
         :time-ms time-ms :clock-epoch (:clock-epoch state)))

(defn- sync-clock [model identity]
  (if-let [{:keys [owner state]} (get-in model path)]
    (if (= identity owner)
      model
      (let [old-state state
            {:keys [state missed]} (quantization/invalidate state (:clock-epoch state) :clock-changed)
            cues (when (seq missed) (into {} (map (juxt :command-id :cue-id)) (quantization/pending old-state)))
            same-show? (= (subvec identity 0 2) (subvec owner 0 2))
            model (cond-> (-> model (assoc-in (conj path :owner) identity) (assoc-in state-path state))
                    (not same-show?) (update-in path dissoc :early-releases :feedback))]
        (reduce #(emit %1 :cue/launch-missed (assoc %2 :cue-id (get cues (:command-id %2))))
                model missed)))
    model))

(defn- wrong-target [model command]
  (cond
    (and (contains? command :show-id) (not= (:show-id model) (:show-id command))) :wrong-show
    (and (contains? command :epoch) (not= (:epoch model) (:epoch command))) :stale-epoch))

(defn- fire [model state clock decisions apply-command]
  (reduce
   (fn [model {:keys [command command-id dispatched-at] :as decision}]
     (let [timing (-> decision
                      (select-keys [:raw :requested-at :grid :due-beat :dispatched-at])
                      (assoc :admitted-at dispatched-at))
           result (try
                    (apply-command model (assoc command :cue-timing timing) (:time-ms clock))
                    (catch Exception error
                      {:model model :ack {:status :rejected :code (:code (ex-data error) :invalid-launch)}}))
           ack (:ack result)
           accepted? (= :accepted (:status ack))
           state (get-in model state-path)
           settled (quantization/settle state command-id clock accepted? ack)
           committed (:model result)
           outcome (when accepted? (get-in committed [:control :history command-id]))]
       (-> (:model result)
           (assoc-in state-path (:state settled))
           (cond-> outcome (update ::fired-outcomes (fnil conj []) outcome))
           (emit (if accepted? :cue/launch-fired :cue/launch-missed)
                 (assoc (:receipt settled) :cue-id (:cue-id command))))))
   (assoc-in model state-path state) decisions))

(defn schedule [model identity time-ms request apply-command]
  (let [model (sync-clock model identity)
        state (or (get-in model state-path) (quantization/new-state))
        id (get-in request [:command :command-id])
        source (source-owner (get-in request [:command :source]))
        early-key [source id]
        early? (and (= :held (get-in request [:command :launch :mode]))
                    (some #{early-key} (get-in model early-path)))
        clock (clock model state time-ms)
        reason (or (wrong-target model (:command request))
                   (when (and (get-in model [:control :history id]) (nil? (quantization/receipt state id)))
                     :command-id-conflict))]
    (if reason
      {:model model :ack {:status :rejected :reason reason :command-id id}}
      (let [{:keys [state due duplicate?] initial :receipt}
            (quantization/admit state request clock (when early? :held-release))]
        (if (= :rejected (:status initial))
          {:model model :ack initial}
          (let [model (cond-> (-> model (assoc-in (conj path :owner) identity) (assoc-in state-path state))
                        early? (update-in early-path #(into [] (remove #{early-key}) %)))
                model (cond
                        duplicate? model
                        early? (emit model :cue/launch-cancelled (assoc initial :cue-id (get-in request [:command :cue-id])))
                        (seq due) (fire model state clock due apply-command)
                        :else (emit model :cue/launch-scheduled initial))]
            {:model model :ack (cond-> (receipt model id)
                                 duplicate? (assoc :duplicate? true))}))))))

(defn cancel [model identity {:keys [command-id reason] :as request}]
  (let [model (sync-clock model identity)
        target-error (wrong-target model request)]
    (if-let [state (when-not target-error (get-in model state-path))]
      (let [status (:status (quantization/receipt state command-id))
            result (quantization/cancel state command-id (:clock-epoch state) (or reason :operator-cancel))
            receipt (:receipt result)]
        {:model (cond-> (assoc-in model state-path (:state result))
                  (and (= :pending status) (= :cancelled (:status receipt)))
                  (emit :cue/launch-cancelled receipt))
         :ack receipt})
      {:model model :ack {:command-id command-id :status :rejected
                          :reason (or target-error :unknown-intent)}})))

(defn withdraw
  "Cancel a pending launch under the queue's current clock, for a host that
  no longer wants it, such as a cancelled or stale staged cue. Anything but
  a pending launch is left as it is."
  [model command-id reason]
  (let [owner (get-in model (conj path :owner))]
    (if (and owner (= :pending (:status (receipt model command-id))))
      (:model (cancel model owner {:command-id command-id :reason reason}))
      model)))

(defn queued-command
  "The launch command admitted under a command ID, pending or remembered."
  [model command-id]
  (let [state (get-in model state-path)]
    (get-in (or (get-in state [:pending command-id]) (get-in state [:history command-id])) [:request :command])))

(defn cancel-owned
  "Cancel a source's pending launch, or release its exact fired hold."
  [model identity time-ms {:keys [launch-command-id reason source] :as request} apply-command]
  (let [model (sync-clock model identity)
        state (get-in model state-path)
        current (receipt model launch-command-id)
        owner (source-owner source)
        target-error (wrong-target model request)
        held-release? (= reason :held-release)
        instance (when (and held-release? (some? launch-command-id) (held-source? owner))
                   (if current
                     (get-in model [:instances (:instance-id current)])
                     (some (fn [instance]
                             (when (and (= launch-command-id (:gesture-id instance))
                                        (= owner (source-owner (:source instance))))
                               instance))
                           (vals (:instances model)))))]
    (cond
      target-error
      {:model model :ack {:command-id launch-command-id :status :rejected :reason target-error}}

      (and (= :held (:launch-mode instance)) (= :running (:phase instance))
           (= owner (source-owner (:source instance))))
      (apply-command model (-> request (select-keys [:source :command-id :epoch :show-id])
                               (assoc :op :instance/end :instance-id (:id instance))) time-ms)

      (and (some? owner) current (= owner (source-owner (quantization/source state launch-command-id))))
      (cancel model identity {:command-id launch-command-id :reason reason})

      (and (nil? current) held-release? (held-source? owner)
           (string? launch-command-id) (<= 1 (count launch-command-id) 128))
      (let [key [owner launch-command-id]
            releases (get-in model early-path [])]
        {:model (cond-> (assoc-in model (conj path :owner) identity)
                  (nil? state) (assoc-in state-path (quantization/new-state))
                  (not (some #{key} releases)) (assoc-in early-path (vec (take-last 128 (conj releases key)))))
         :ack {:command-id launch-command-id :status :cancelled :reason :held-release}})

      :else {:model model :ack {:command-id launch-command-id :status :rejected :reason :unknown-intent}})))

(defn advance [model identity time-ms leases apply-command]
  (let [model (sync-clock model identity)
        state (get-in model state-path)]
    (if (seq (:pending state))
      (let [model (reduce (fn [model {:keys [command-id mode requested-at source]}]
                            (if (and (= :held mode) (web-source? source)
                                     (> (- time-ms (get leases (:session-id source) (:time-ms requested-at))) held-lease-ms))
                              (:model (cancel model identity {:command-id command-id :reason :lease-expired}))
                              model))
                          model (quantization/pending state))
            state (get-in model state-path)
            clock (clock model state time-ms)
            {:keys [state due]} (quantization/tick state clock)]
        (if (seq due) (fire model state clock due apply-command) model))
      model)))
