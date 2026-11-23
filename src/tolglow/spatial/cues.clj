(ns tolglow.spatial.cues
  "Author and operate an ordinary grid cue whose native output covers a moving volume."
  (:require [afterglow.control.system :as system]
            [afterglow.render.layers :as layers]
            [afterglow.spatial.field :as field]
            [afterglow.spatial :as physics]
            [afterglow.time :as time]
            [thi.ng.color.core :as color]
            [tolglow.autopilot.signal :as signal]
            [tolglow.registry :as registry]))

(def run-path [:extensions ::members])
(defn- fail! [message data] (throw (ex-info message data)))
(defn- id? [v] (or (keyword? v) (and (string? v) (seq v))))
(defn- number-param [v limits]
  (cond-> {:type :number :default v} limits (assoc :bounds limits)))
(defn- vector-param [v] {:type :vector :size 3 :default v})

(defn- dimensions [volume]
  (let [ball? (= :ball (:shape volume))
        form (or (:geometry volume) volume)]
    (cond-> {:strength (number-param (:strength volume 1.0) [0 1])
             :inner-m (number-param (get-in volume [:falloff :inner-m] 0.0) [0 10000])
             :outer-m (number-param (get-in volume [:falloff :outer-m] 0.0) [0 10000])
             :translation (vector-param (:translation volume [0 0 0]))}
      (:position volume) (assoc :position (vector-param (:position volume)))
      (:radius volume) (assoc :radius (number-param (:radius volume) [(if ball? 0.000001 0) 10000]))
      (:size form) (assoc :size (vector-param (:size form)))
      (= :rounded-box (:shape form)) (assoc :rounding (number-param (:rounding form 0) [0 1]))
      ball? (assoc :restitution (number-param (:restitution volume 0.85) [0 1])
                   :velocity (vector-param (:velocity volume [0 0 0]))))))

(defn- paint-param [[attr value]]
  (when-not (contains? registry/attributes attr)
    (fail! "Unknown native fixture attribute" {:attribute attr}))
  [attr (case attr
          :color {:type :color :default value}
          (:aim :direction) (vector-param value)
          (number-param value [0 1]))])

(defn- source? [spec]
  (or (signal/valid? spec)
      (and (vector? spec) (= 3 (count spec)) (every? signal/valid? spec))
      (and (map? spec) (= #{:hsl} (set (keys spec))) (vector? (:hsl spec))
           (= 3 (count (:hsl spec))) (every? signal/valid? (:hsl spec)))))

(defn- scene-slots [model id]
  (when id
    (let [scene (get-in model [:compositions id])]
      (when-not (= :macro (:mode scene))
        (fail! "Select a saved macro scene" {:scene-id id}))
      (mapv (fn [slot]
              (-> (select-keys slot [:cue-id :transforms :targets :launch])
                  (update :launch #(update % :params (partial merge (:params slot))))))
            (:slots scene)))))

(defn- validate-member! [model member]
  (let [{:keys [cue-id effect-id launch transforms]} member
        cue (get-in model [:registry :cues cue-id])
        effect (if cue-id (registry/cue-effect (:registry model) cue)
                   (get-in model [:registry :effects effect-id]))]
    (when-not (and (map? member) (not= (boolean cue-id) (boolean effect-id)) effect
                   (not (:show-control? cue)) (not (:show-control? effect))
                   (not (:code-cue? cue)) (not (:code-cue? effect))
                   (not (:spatial-volume effect)) (map? (or launch {}))
                   (every? #{:params :duration :duration-ms :mode} (keys launch))
                   (or (nil? (:targets member)) (= :previous (:targets member))))
      (fail! "Choose a registered lighting cue or effect for the volume" {:member member}))
    (when-let [errors (seq (registry/validate-params (:params effect {}) (:params launch {})))]
      (fail! "Scene member parameter overrides are invalid" {:member member :errors errors}))
    (when transforms (layers/validate-transforms! transforms))
    member))

(defn- cell [page preferred]
  (let [w (:columns page 8) h (:rows page 8) cells (:cells page {})
        available? (fn [p] (and (vector? p) (= 2 (count p)) (every? nat-int? p)
                                (< (first p) w) (< (second p) h) (not (contains? cells p))))]
    (or (if (some? preferred) (when (available? preferred) preferred)
            (first (for [y (range h) x (range w) :when (available? [x y])] [x y])))
        (fail! "Choose an empty cell on this page" {:page-id (:id page) :cell preferred}))))

(defn- accept [model command payload now]
  (let [result (command model payload now)]
    (when-not (= :accepted (get-in result [:ack :status]))
      (fail! (get-in result [:ack :message] "Spatial cue command rejected") {:ack (:ack result)}))
    result))

(defn spawn
  "Publish a reusable effect and cue, place it on the page and launch it. All
  edits operate on a private value until successful. An explicit ID and cell
  protect occupied user definitions; different presses may use distinct IDs."
  [model {:keys [op id name page-id cell volume paint members scene-id sources launch priority]} now command]
  (let [id (or id (str "volume-" (:epoch model) "-" (:revision model)))
        page-id (or page-id :spatial)
        page (get-in model [:pages page-id] {:id page-id :name "Spatial" :cells {}})
        pos (tolglow.spatial.cues/cell page cell)
        volume (cond-> volume (vector? (:selection volume)) (update :selection set))
        paint (or paint {}) sources (or sources {})
        member-list (into (vec (scene-slots model scene-id)) members)
        params (merge (dimensions volume) (into {} (map paint-param) paint))
        spec {:volume volume :sources sources :members (mapv (partial validate-member! model) member-list)}
        effect {:id id :version 1 :name (or name (str id)) :show-control? true
                :spatial-volume spec :layer {:transforms []} :params params
                :outputs (into {} (map (fn [attr] [attr {:op :param :key attr}])) (keys paint))}
        cue (cond-> {:id id :effect-id id :name (:name effect) :version 1 :show-control? true}
              priority (assoc :priority priority))]
    (when-not (and (= :spawn op) (id? id) (id? page-id)
                   (not (get-in model [:registry :effects id])) (not (get-in model [:registry :cues id]))
                   (map? paint) (or (nil? members) (vector? members)) (<= (count member-list) 64)
                   (or (seq paint) (seq member-list))
                   (map? sources) (every? params (keys sources)) (every? source? (vals sources)))
      (fail! "Choose a new cue ID, a native paint or scene, and valid parameter sources" {:id id}))
    (field/prepare volume)
    (physics/validate! (assoc (select-keys (get-in model [:settings :spatial]) [:bounds :gravity]) :volumes {id volume}))
    (when-let [errors (seq (registry/validate-definition :effects effect))]
      (fail! "Invalid spatial effect parameters" {:errors errors}))
    (let [model (:model (accept model command {:op :registry/publish :kind :effects :definition effect} now))
          model (:model (accept model command {:op :cue/put :cue cue} now))
          model (:model (accept model command {:op :page/put :page (assoc-in page [:cells pos] id)} now))
          launched (accept model command {:op :cue/launch :cue-id id :launch (or launch {})} now)]
      (update launched :ack assoc :cue-id id :page-id page-id :cell pos))))

(defn controls? [instance] (boolean (get-in instance [:definition :spatial-volume])))
(defn key-for [instance] (str "spatial-cue/" (:order instance)))
(defn- running? [instance] (and (controls? instance) (= :running (:phase instance))))
(defn needed? [model]
  (or (get-in model run-path) (some running? (vals (:instances model)))))

(defn- sample-source [model opts default spec]
  (let [scalar #(signal/value model % opts)]
    (cond
      (vector? spec) (mapv scalar spec)
      (:hsl spec) ((juxt :r :g :b) (color/as-rgba (apply color/hsla (map scalar (:hsl spec)))))
      (and (vector? default) (:variable spec)) (get-in (:variables model) (:variable spec) default)
      (and (vector? default) (:cue-id spec))
      (let [running (filter #(and (= :running (:phase %)) (= (:cue-id spec) (:cue-id %))) (vals (:instances model)))
            selected (last (sort-by :order running))]
        (get (:params selected) (:param spec) default))
      :else (scalar spec))))

(defn- controls [model instance modulation]
  (let [spec (get-in instance [:definition :spatial-volume])
        base (merge (:params instance) (get-in modulation [:overrides (:id instance)]))
        opts {:instance-id (:id instance) :modulation-view modulation
              :start-clock {:time-ms (:started-at-ms instance) :beat (:started-at-beat instance)}}]
    (reduce-kv
      (fn [values k source]
        (let [value (sample-source model opts (get base k) source)
              fits? (registry/valid-value? (get-in instance [:definition :params k]) value)]
          (if fits? (assoc values k value) values)))
      base (:sources spec))))

(defn- volume [template values]
  (let [shape-key (if (:geometry template) [:geometry] [])
        geometry (reduce (fn [v k]
                           (if (contains? values k) (assoc-in v (conj shape-key k) (values k)) v))
                         template [:size :rounding])]
    (-> geometry
        (merge (select-keys values [:strength :translation :position :radius :velocity :restitution]))
        (update :falloff merge (select-keys values [:inner-m :outer-m])))))

(defn volumes [model modulation]
  (into {}
        (keep (fn [instance]
                (when (running? instance)
                  (let [spec (get-in instance [:definition :spatial-volume])
                        values (controls model instance modulation)]
                    [(key-for instance) {:instance instance :volume (volume (:volume spec) values)
                                         :values values :members (:members spec)}]))))
        (vals (:instances model))))

(defn- member-source [key index]
  {:adapter :spatial-cue :volume-id key :member index})

(defn session-source [model instance]
  (let [{:keys [volume-id member] :as source} (:source instance)]
    (when (and (= source (member-source volume-id member))
               (= (:id instance) (get-in model (into run-path [volume-id :ids member]))))
      source)))

(defn restore [model ids]
  (if (get-in model run-path)
    (update-in model run-path update-vals
               (fn [lease] (update lease :ids #(into {} (keep (fn [[slot id]]
                                                                 (when-let [new-id (ids id)] [slot new-id]))) %))))
    model))

(defn- stop-members [model key lease now command]
  (reduce-kv (fn [model index id]
               (if (= (member-source key index) (get-in model [:instances id :source]))
                 (:model (accept model command {:op :instance/remove :instance-id id} now)) model))
             model (:ids lease {})))

(defn- start-members [model key members now command]
  (reduce-kv
    (fn [[model ids] index {:keys [cue-id effect-id launch]}]
      (let [source (member-source key index)
            payload (merge {:source source :launch (or launch {})}
                           (if cue-id {:op :cue/launch :cue-id cue-id} {:op :effect/launch :effect-id effect-id}))
            result (accept model command payload now)]
        [(:model result) (assoc ids index (get-in result [:ack :instance-id]))]))
    [model {}] members))

(defn reconcile
  "Scope one live activation per scene member, independent of the number of
  heads inside the field. Ending the parent only removes its recorded children."
  [model active now command]
  (let [prior (get-in model run-path {})]
    (if (and (empty? active) (empty? prior)) model
      (let [[model leases]
            (reduce
              (fn [[model leases] key]
                (let [members (get-in active [key :members])
                      old (prior key)]
                  (if (= members (:members old))
                    [model leases]
                    (let [model (stop-members model key old now command)]
                      (if (seq members)
                        (let [[model ids] (start-members model key members now command)]
                          [model (assoc leases key {:members members :ids ids})])
                        [model (dissoc leases key)])))))
              [model prior] (sort (into (set (keys prior)) (keys active))))]
        (if (seq leases) (assoc-in model run-path leases) (update model :extensions dissoc ::members))))))

(defn- group-rows [model key active weights]
  (let [{:keys [instance members]} active
        opacity (* (get-in instance [:system :opacity] 1.0) (system/gain instance (time/frame-clock model)))
        descriptor {:id key :target-weights weights :opacity opacity}
        leases (get-in model (into run-path [key :ids]))]
    (into {(:id instance) (assoc descriptor :member 0 :member-opacity 1.0)}
          (for [[index id] leases
                :let [slot (get members index)]]
            [id (merge descriptor {:member (inc index)}
                       (select-keys slot [:transforms :targets]))]))))

(defn frame-fields [model active physical sample]
  (reduce-kv
    (fn [frame key {:keys [volume values instance] :as entry}]
      (let [position (get-in physical [:signals key :position])
            volume (cond-> volume (and (= :ball (:shape volume)) position) (assoc :position position))
            {:keys [weights aims]} (sample volume)
            opacity (get-in instance [:system :opacity] 1.0)]
        (-> frame
            (update :groups into (group-rows model key entry weights))
            (assoc-in [:overrides (:id instance)] (select-keys values (keys (get-in instance [:definition :spatial-volume :sources]))))
            (assoc-in [:members key] (set (keys weights)))
            (assoc-in [:views key] {:volume volume :cue-id (:cue-id instance) :instance-id (:id instance)
                                    :paint (select-keys values (keys (get-in instance [:definition :outputs])))
                                    :affected-head-count (count weights)}))))
    {:groups {} :overrides {} :views {} :members {}} active))

(defn mask-contributions [result groups]
  (if (empty? groups) result
    (update result :contributions
            (fn [blocks]
              (mapv (fn [block]
                      (if-let [group (get groups (get-in block [:source :instance-id]))]
                        (update block :native-layer
                                (fn [native]
                                  (cond-> (merge native group)
                                    (and (seq (:transforms native)) (seq (:transforms group)))
                                    (assoc :transforms (into (:transforms group) (:transforms native))))))
                        block)) blocks)))))
