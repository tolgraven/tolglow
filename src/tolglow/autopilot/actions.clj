(ns tolglow.autopilot.actions
  "Portable, scope-owned cue performance. All launches and parameter/lifecycle
  changes use the show's normal command reducer. Transient registry entries are
  reaped after their last live reference disappears."
  (:require [clojure.set :as set]
            [tolglow.autopilot.candidates :as candidates]
            [tolglow.effects.authoring :as authoring]
            [tolglow.registry :as registry]))

(defn initial-state [model scope-id]
  {:owner-id (pr-str [:energy-conductor scope-id (:epoch model) (:revision model) (:time-ms model)])
   :scope-id scope-id :epoch (:epoch model) :serial 0 :owned {} :temporary {} :instance-gains {}})

(defn- manual-controls [instance]
  (cond-> (select-keys (:system instance) [:opacity :paused? :attack :release])
    (not (== 1.0 (get-in instance [:system :gain] 1.0))) (assoc :gain (get-in instance [:system :gain]))))

(defn- owner [state] {:adapter :autopilot :session-id (:owner-id state)})
(defn- outcome [model state result] {:model model :state state :result result})

(defn session-source [state instance]
  (when (and (get-in state [:owned (:id instance)]) (= (owner state) (:source instance))) (:source instance)))

(defn restore
  "Rekey live leases and gain overlays into the restored show epoch. In-flight
  provider requests belong to their originating session."
  [state ids epoch]
  (let [remap (fn [m] (into {} (keep (fn [[id v]] (when-let [n (ids id)] [n v]))) m))]
    (-> state (assoc :epoch epoch) (dissoc :pending) (update :owned remap) (update :instance-gains remap))))

(defn- issue [model state cmd time-ms command]
  (let [payload (assoc cmd :source (owner state)
                          :command-id (pr-str [(:owner-id state) (:epoch model) (:serial state)
                                               (:revision model) (:op cmd) (:instance-id cmd)]))
        {:keys [ack] :as result} (command model payload time-ms)]
    (when-not (= :accepted (:status ack))
      (throw (ex-info "Performance command rejected" {:ack ack})))
    result))

(defn- retained? [model state effect-id]
  (or (some #(= effect-id (:effect-id %)) (vals (:owned state)))
      (some #(= effect-id (:effect-id %)) (vals (get-in model [:registry :cues])))))

(defn cleanup [model state _time-ms _command]
  (let [owned (into {} (filter (fn [[id {:keys [params controls]}]]
                                 (let [i (get-in model [:instances id])]
                                   (and (session-source state i) (= params (:params i)) (= controls (manual-controls i)))))) (:owned state))
        state (assoc state :owned owned :instance-gains
                          (select-keys (:instance-gains state) (keys (:instances model))))]
    (reduce-kv
     (fn [{:keys [model state] :as result} id version]
       (let [definition (get-in model [:registry :effects id])]
         (cond
           (or (not= version (:version definition)) (not= (:owner-id state) (:performance-owner definition)))
           (update-in result [:state :temporary] dissoc id)
           (retained? model state id) result
           :else (assoc result
                   :model (-> model (update-in [:registry :effects] dissoc id)
                              (update-in [:registry :versions] dissoc [:effects id version])
                              (update-in [:registry :revision] (fnil inc 0)))
                   :state (update state :temporary dissoc id)))))
     (outcome model state :idle) (:temporary state))))

(defn- fixtures [model decision]
  (let [selected (set (:fixture-ids decision))]
    (filterv #(selected (:id %)) (get-in model [:rig :fixtures]))))

(defn- request [model state decision]
  (let [base (str "autopilot/" (:owner-id state) "/" (:serial state))
        reg (:registry model)
        id (loop [id base]
             (if (or (get-in reg [:effects id]) (some #(= id (second %)) (keys (:versions reg))))
               (recur (str id "'")) id))]
    (merge (select-keys decision [:fixture-ids :domain :direction :amount :duration :cue-ids])
           {:effect-id id :owner-id (:owner-id state) :serial (:serial state) :epoch (:epoch model)
            :channels (into [] (sort (into #{} (mapcat (comp keys :channels)) (fixtures model decision))))})))

(defn- launch [model state cmd origin time-ms command]
  (let [{:keys [model ack]} (issue model state cmd time-ms command)
        id (:instance-id ack)
        instance (get-in model [:instances id])]
    (outcome model (assoc-in state [:owned id]
                             {:origin origin :params (:params instance) :controls (manual-controls instance) :cue-id (:cue-id instance)
                              :effect-id (:effect-id instance)})
             {:op :launch :instance-id id :origin origin})))

(defn- launch-transient [model state req candidate origin opts time-ms command]
  (let [definition (authoring/prepare req candidate)
        id (:id definition)
        _ (when (or (get-in model [:registry :effects id]) (get-in model [:registry :versions [:effects id 1]]))
            (throw (ex-info "Temporary effect identity is already in use" {:id id})))
        published (:model (issue model state {:op :registry/publish :kind :effects :definition definition} time-ms command))]
    (launch published (assoc-in state [:temporary id] (:version definition))
            {:op :effect/launch :effect-id id
             :launch (merge {:mode :one-shot :duration (or (:duration req) {:bars 1})} opts)}
            origin time-ms command)))

(defn- launch-registry [model state decision time-ms command]
  (if-let [{:keys [cue effect origin ids scoped? brightness]} (candidates/choose model state decision)]
    (let [emission-only? (every? (comp authoring/light-attributes :attribute) (registry/output-entries (:outputs effect)))
          scale? (and emission-only? (= :brightness (:domain decision)) (> brightness (:amount decision)))
          level (get (:system cue) :opacity (get-in effect [:system :opacity] 1.0))
          opts (cond-> {}
                 scale? (assoc :system {:opacity (* level (/ (:amount decision) brightness))}))]
      (if scoped?
        (launch-transient model state (assoc (request model state decision) :fixture-ids ids) effect origin
                          (assoc opts :params (merge {} (:params cue) (:saved-params cue))) time-ms command)
        (launch model state (merge (if cue {:op :cue/launch :cue-id (:id cue)} {:op :effect/launch :effect-id (:id effect)})
                                   {:launch (merge opts {:mode :timed :duration (or (:duration decision) {:bars 1})})})
                origin time-ms command)))
    (outcome model state :no-eligible-look)))

(defn- scoped-instances [model state decision]
  (let [scope (set (:fixture-ids decision))
        rig (into #{} (map :id) (get-in model [:rig :fixtures]))
        cues (when (some? (:cue-ids decision)) (set (:cue-ids decision)))]
    (filter (fn [i]
              (let [lease (get-in state [:owned (:id i)])
                    targets (candidates/target-ids rig (:definition i))]
                (and (= :running (:phase i)) (seq targets) (set/subset? targets scope)
                     (or (nil? cues) (cues (:cue-id i)) (cues (:origin lease))))))
            (sort-by :order (vals (:instances model))))))

(defn- adjust [model state decision time-ms command]
  (let [active (scoped-instances model state decision)
        owned (filter #(session-source state %) active)
        choice (some (fn [i] (when-let [edit (candidates/adjustment i decision)] [i edit])) owned)]
    (if-let [[i edit] choice]
      (let [cmd (assoc edit :op :param/set :instance-id (:id i))
            changed (:model (issue model state cmd time-ms command))]
        (outcome changed (assoc-in state [:owned (:id i) :params (:param edit)] (:value edit)) cmd))
      (if-let [i (when (= :brightness (:domain decision))
                   (first (filter #(some (comp authoring/light-attributes :attribute)
                                      (registry/output-entries (get-in % [:definition :outputs]))) active)))]
        (let [id (:id i) prev (get-in state [:instance-gains id] 1.0)
              sign (if (= :down (:direction decision)) -1.0 1.0)
              gain (max 0.0 (min (:max-gain decision 4.0) (+ prev (* sign (:amount decision)))))]
          (outcome model (assoc-in state [:instance-gains id] gain) {:op :gain :instance-id id :gain gain}))
        (if (= :up (:direction decision)) (launch-registry model state decision time-ms command)
          (outcome model state :no-adjustable-look))))))

(defn- retire [model state decision time-ms command]
  (if-let [i (first (filter #(session-source state %) (scoped-instances model state decision)))]
    (let [cmd {:op :instance/end :instance-id (:id i)}
          next-model (:model (issue model state cmd time-ms command))]
      (outcome next-model state cmd))
    (outcome model state :no-owned-look)))

(defn stop [model state time-ms command]
  (let [{:keys [model state]} (cleanup model state time-ms command)
        result (reduce (fn [model id]
                         (:model (issue model state {:op :instance/remove :instance-id id} time-ms command)))
                       model (sort (keys (:owned state))))
        cleaned (cleanup result (-> state (dissoc :pending) (assoc :instance-gains {})) time-ms command)]
    (assoc cleaned :result :stopped)))

(defn accept-authored [model state req definition time-ms command]
  (if (and (= (:epoch model) (:epoch req)) (= req (:pending state)) (:pending state))
    (let [state (dissoc state :pending)]
      (try (when-not (= (set (:fixture-ids req)) (set (map :id (fixtures model req))))
             (throw (ex-info "Fixture scope changed while authoring" {})))
           (launch-transient model state req definition (:effect-id req) {} time-ms command)
           (catch Exception e (outcome model state {:error :invalid-proposal :message (.getMessage e)}))))
    (outcome model state :stale-proposal)))

(defn reject-authored [model state req reason]
  (if (and (= (:epoch model) (:epoch req)) (= req (:pending state)) (:pending state))
    (outcome model (dissoc state :pending) {:error reason})
    (outcome model state :stale-proposal)))

(defn apply-decision [model state decision time-ms command]
  (let [{:keys [model state]} (cleanup model state time-ms command)]
    (if (or (not= (:epoch model) (:epoch state)) (empty? (:fixture-ids decision)))
      (outcome model state :inactive)
      (let [pending (:pending state)
            state (cond-> (update state :serial inc)
                    (and pending (not= (select-keys pending [:domain :direction :fixture-ids])
                                       (select-keys decision [:domain :direction :fixture-ids]))) (dissoc :pending))]
        (try
          (case (:op decision)
            :adjust (adjust model state decision time-ms command)
            :retire (retire model state decision time-ms command)
            :launch (launch-registry model state decision time-ms command)
            :generate (let [req (request model state decision)]
                        (launch-transient model state req (authoring/procedural req (fixtures model decision))
                                          (:effect-id req) {} time-ms command))
            :author (if (:pending state) (outcome model state :author-pending)
                      (let [req (request model state decision)]
                        (assoc (outcome model (assoc state :pending req) :author-requested) :author-request req)))
            (outcome model state :idle))
          (catch Exception e (outcome model state {:op (:op decision) :error :action-failed :message (.getMessage e)})))))))
