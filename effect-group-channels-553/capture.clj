(require '[afterglow.control.model :as control]
         '[clojure.data.json :as json]
         '[tolglow.effects.group-channels :as groups]
         '[tolglow.show :as show])

(defn- stage-model []
  (reduce (fn [model [id priority outputs params]]
            (-> model
                (assoc-in [:registry :effects id] {:id id :version 1 :params params :outputs outputs})
                (assoc-in [:registry :cues id] {:id id :version 1 :effect-id id :priority priority})))
          (control/new-show {:show-id :group-trace
                             :rig {:fixtures [{:id :head :universe 1 :address 1 :position [0 1 0]
                                               :channels {:dimmer 1 :focus 2}}]}})
          [[:between 1 {:dimmer 0.55 :focus 0.4} {}]
           [:transform 3 {:dimmer 0.2 :focus 0.7} {}]
           [:edit 5 {:dimmer {:op :param :key :level}}
            {:level {:type :number :default 0.25 :bounds [0 1]}}]]))

(defn- ensure-accepted [receipt]
  (when-not (= :accepted (:status receipt))
    (throw (ex-info "Trace command rejected" receipt)))
  receipt)

(defn- submit [runtime time type command]
  (let [reply (show/submit! runtime type (merge {:source {:adapter :trace}} command))]
    (show/tick! runtime time)
    (ensure-accepted (deref reply 1000 {:status :missing}))))

(defn- packet [runtime]
  (let [{:keys [status output]} @(:state runtime)]
    (when-let [fault (:error status)] (throw (ex-info "Trace frame failed" fault)))
    (mapv #(bit-and 255 %) (take 2 (get-in output [:universes 1])))))

(def grouping {:manual [{:id :child :name "Spatial child" :cue-ids [:transform]}]})

(defn- frame [runtime label receipt]
  (let [view (show/effect-group-channels runtime)
        instance (some #(when (= :transform (:cue-id %)) %) (:instances view))]
    {:label label :dmx (packet runtime)
     :receipt (select-keys receipt [:status :revision])
     :level (get-in view [:manual 0 :level]) :alpha (:opacity instance)
     :instance (:instance-id instance)}))

(defn- mixer-trace []
  (let [runtime (show/create-runtime {:model (stage-model) :workspace-options nil :batch? true})]
    (try
      (submit runtime 0 :command {:op :cue/launch :cue-id :between})
      (let [backdrop (packet runtime)
            _ (submit runtime 10 :spatial-cue
                      {:op :spawn :id :region :priority 0 :paint {:dimmer 0.2 :focus 0.2}
                       :volume {:shape :sphere :position [0 1 0] :radius 1 :selection #{:position}}
                       :members [{:cue-id :transform :targets :previous
                                  :transforms [{:attribute :dimmer :value 0.8}]}]})
            initial (submit runtime 20 :command {:op :effect-groups/configure :config grouping})
            rows (into [(frame runtime "Unity" initial)]
                       (map-indexed (fn [index [label value]]
                                      (let [receipt (submit runtime (+ 30 (* 10 index)) :command
                                                            {:op :effect-groups/level-set
                                                             :channel-id [:manual :child] :value value})]
                                        (frame runtime label receipt))))
                       [["Half level" 0.5] ["Zero level" 0] ["Unity restored" 1]])]
        (assert (= [140 102] backdrop))
        (assert (= [[204 179] [128 115] [51 51] [204 179]] (mapv :dmx rows)))
        (assert (= [1.0 1.0 1.0 1.0] (mapv :alpha rows)))
        {:interleaved backdrop :frames rows})
      (finally (show/stop! runtime)))))

(defn- envelope [model command]
  (merge {:show-id (:show-id model) :epoch (:epoch model) :time-ms (:time-ms model)
          :source {:adapter :trace :operator-id "trace-owner"}
          :command-id (str "trace-" (:revision model))}
         command))

(defn- operator-gesture []
  (let [model (stage-model)
        authority {:operator-id "trace-owner"}
        launch (control/apply-command model (envelope model {:op :cue/launch :cue-id :edit}))
        id (:instance-id (ensure-accepted (:ack launch)))
        configured (let [m (:model launch)]
                     (control/apply-domain-command m (envelope m {:op :effect-groups/configure :config grouping})
                                                   groups/apply-operation))
        operator (fn [model cmd]
                   (let [result (control/apply-command model (envelope model cmd) authority)]
                     (ensure-accepted (:ack result))
                     result))
        touch {:instance-id id :param :level :control-id :encoder :gesture-id "trace-gesture"}
        started (:model (operator (:model configured) (assoc touch :op :gesture/begin)))
        domain (control/apply-domain-command started
                   (envelope started {:op :effect-groups/level-set :channel-id [:manual :child] :value 0.5})
                   groups/apply-operation)
        base (:model domain)
        sample (assoc touch :op :gesture/sample :sequence 1 :value 0.8)
        rejected (control/apply-domain-command base (envelope base sample) groups/apply-operation)
        sampled (:model (operator base sample))
        ended (:model (operator sampled (assoc touch :op :gesture/end :last-sequence 1)))
        undone (:model (operator ended {:op :history/undo :expected-cursor 1
                                       :expected-history-position (get-in ended [:control :edit-history :position])}))
        value #(get-in % [:instances id :params :level])
        result {:values (mapv value [started ended undone]) :domain-receipt (get-in domain [:ack :status])
                :unowned-gesture (get-in rejected [:ack :code])
                :level-after-undo (get-in (groups/projection undone) [:manual 0 :level])}]
    (assert (= [0.25 0.8 0.25] (:values result)))
    (assert (= :accepted (:domain-receipt result)))
    (assert (= :operator-required (:unowned-gesture result)))
    (assert (= 0.5 (:level-after-undo result)))
    result))

(let [[output afterglow tolglow] *command-line-args*]
  (spit output (json/write-str (assoc (mixer-trace)
                                     :gesture (operator-gesture)
                                     :afterglow afterglow :tolglow tolglow)
                              :escape-unicode false)))
