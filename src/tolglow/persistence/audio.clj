(ns tolglow.persistence.audio
  "Update built-in audio signal references when restoring a saved show.")

(def ^:private old-variable [:audio :features :energy-envelope])

(def ^:private old-event-field [:data :energy-envelope])

(def ^:private old-port "audio/energy-envelope")

(def ^:private port "audio/energy")

(defn- group-target [scene]
  (cond-> scene (= old-variable (:target-var scene)) (assoc :target-var [:audio/energy])))

(defn- action [step energy-event?]
  (if (= :lighting/scene (:op step))
    (cond-> (update step :scene group-target)
      (and energy-event? (= old-event-field (:event-path step))) (assoc :event-path [:data :audio/energy])
      (and energy-event? (= old-event-field (get-in step [:gate :path]))) (assoc-in [:gate :path] [:data :audio/energy]))
    step))

(defn- event-bindings [subscribers]
  (let [update-actions (fn [steps event] (mapv #(action % (= :audio/energy (:type event))) steps))]
    (cond-> (update subscribers :subscribers
                    #(mapv (fn [d] (update d :actions update-actions (:match d))) %))
      (seq (:runs subscribers))
      (update :runs
              #(mapv (fn [{:keys [event] :as run}]
                       (cond-> (update run :steps update-actions event)
                         (and (= :audio/energy (:type event)) (contains? (:data event) :energy-envelope))
                         (update-in [:event :data] (fn [data] (-> data (dissoc :energy-envelope)
                                                                 (assoc :audio/energy (:energy-envelope data))))))) %)))))

(defn- rename-source [sources f]
  (if-let [old (get sources old-port)]
    (do (when (contains? sources port)
          (throw (ex-info "Two saved patch sources would share the audio/energy port" {:source-ids [old-port port]})))
        (-> sources (dissoc old-port) (assoc port (f old))))
    sources))

(defn- observation [value]
  (cond-> value
    (= old-port (:source-id value)) (assoc :source-id port)
    (= :energy-envelope (get-in value [:port :signal])) (assoc-in [:port :signal] :audio/energy)))

(defn- patch-routing [{:keys [spec ports state pending] :as routing}]
  (if-not (or (= :audio (get-in spec [:sources old-port :kind]))
              (= :audio (get-in ports [old-port :kind])))
    routing
    (cond-> routing
      spec (update-in [:spec :sources] rename-source identity)
      spec (update-in [:spec :routes] #(mapv (fn [route] (cond-> route (= old-port (:source route)) (assoc :source port))) %))
      ports (update :ports rename-source observation)
      state (update-in [:state :sources] rename-source observation)
      state (update-in [:state :receipts] #(mapv (fn [[source id]] [(if (= source old-port) port source) id]) %))
      (seq pending) (update :pending #(mapv observation %)))))

(defn restore [model]
  (let [features (get-in model [:variables :audio :features])
        groups [:extensions :tolglow.lighting-scenes/control :groups]]
    (cond-> model
      (contains? features :energy-envelope)
      (update :variables (fn [vars]
                           (-> vars (update-in [:audio :features] dissoc :energy-envelope)
                               (assoc :audio/energy (get vars :audio/energy (:energy-envelope features))))))
      (seq (get-in model groups)) (update-in groups update-vals group-target)
      (:event-subscribers model) (update :event-subscribers event-bindings)
      (:routing model) (update :routing patch-routing))))
