(ns tolglow.flash-guard.influence
  "Which running light an agent influenced, for the flash guard (#836).

  Light counts as agent-attributed when an agent launched it, and also when
  an agent last wrote one of its parameters or its opacity, published the
  effect revision or last saved the cue it runs, or configured the policy that
  launched it (auto, the energy autopilot, lighting scenes). The owner records
  that influence here from each committed command's source, so every route an
  agent can reach the show by (tool calls, scheduled events, ramps, future
  routes) is covered at the owner rather than in one tool path. An agent
  write to a show variable cannot yet be traced to the cues that read it; while
  one is live the guard covers the whole show.

  Pure functions over the model. This namespace must not require the
  scheduler or the show, which call it.")

(def path [:extensions :tolglow.flash-guard/influence])

(def default-sources [:agent])

(defn sources
  "Source adapters the guard treats as agents in this model."
  [model]
  (set (get-in model [:settings :flash-guard :sources] default-sources)))

(defn agent-source? [model source]
  (and (map? source) (contains? (sources model) (:adapter source))))

(defn- edit [model f]
  (let [old (get-in model path)
        new (f (or old {}))]
    (cond
      (= old new) model
      (every? empty? (vals new)) (if old (update model :extensions dissoc :tolglow.flash-guard/influence) model)
      :else (assoc-in model path new))))

(defn- write [influence agent? k v]
  (if agent?
    (update influence k (fnil conj #{}) v)
    (let [left (disj (get influence k #{}) v)]
      (if (empty? left) (dissoc influence k) (assoc influence k left)))))

(def ^:private opacity-ops #{:instance/opacity-set :instance/opacity-toggle})

(defn note
  "Record what one accepted command did. `agent?` when its source is an
  agent, `human?` when it is an operator surface; a human write ends the
  agent's claim on what it wrote, other sources leave it as it is."
  [model {:keys [op] :as command} events agent? human?]
  (if-not (or agent? human?)
    model
    (edit model
          (fn [influence]
            (let [system-op (if (= :group/apply op) (get-in command [:action :op]) op)
                  instances (:instances model)
                  influence (reduce (fn [inf {:keys [type] :as event}]
                                      (case type
                                        :param/changed (write inf agent? :params [(:instance-id event) (:param event)])
                                        :variable/changed (write inf agent? :variables (:variable event))
                                        :instance/system-changed
                                        (if (opacity-ops system-op)
                                          (write inf agent? :params [(:instance-id event) :opacity])
                                          inf)
                                        inf))
                                    influence events)
                  influence (case op
                              :lighting/scene (write influence agent? :policies :lighting-scene)
                              :auto/configure (write influence agent? :policies :auto)
                              :cue/put (if-let [id (get-in command [:cue :id])] (write influence agent? :cues id) influence)
                              :registry/publish
                              (let [{:keys [id version]} (:definition command)]
                                (if (and (= :effects (:kind command)) id agent?)
                                  (write influence true :effects [id version])
                                  influence))
                              influence)]
              ;; Forget writes to instances that have ended.
              (cond-> influence
                (seq (:params influence))
                (update :params #(into #{} (filter (fn [[id]] (contains? instances id))) %))))))))

(defn note-policy
  "An agent configured (or an operator reconfigured) the policy whose
  launches carry the source adapter `adapter`."
  [model adapter agent? human?]
  (if (or agent? human?) (edit model #(write % agent? :policies adapter)) model))

(defn summary
  "The influence as the guard reads it."
  [model]
  (get-in model path))

(defn attributed?
  "Whether this instance's light counts as agent-attributed."
  [sources influence instance]
  (let [adapter (get-in instance [:source :adapter])
        id (:id instance)]
    (boolean
     (or (contains? sources adapter)
         (contains? (:policies influence) adapter)
         (contains? (:effects influence) [(:effect-id instance) (:effect-version instance)])
         (contains? (:cues influence) (:cue-id instance))
         (some (fn [[i]] (= i id)) (:params influence))))))

(defn show-wide?
  "Agent influence the guard cannot yet trace to instances: it then covers
  the whole show."
  [influence]
  (boolean (seq (:variables influence))))
