(ns tolglow.spatial.cues
  "Author and operate an ordinary grid cue whose native output covers a moving volume."
  (:require [afterglow.control.system :as system]
            [afterglow.render.layers :as layers]
            [afterglow.runtime.engine :as engine]
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

(defn- dimensions [model volume]
  (let [bounds (get-in model [:settings :spatial :bounds] physics/default-bounds)
        max-radius (* 0.5 (reduce min (map - (:max bounds) (:min bounds))))
        ball? (= :ball (:shape volume))
        form (or (:geometry volume) volume)
        shape (if (= :ball (:shape form)) :sphere (:shape form))
        morph? (#{:sphere :rounded-box} shape)
        radius (or (:radius volume) (:radius form) (when (:size form) (* 0.5 (reduce min (:size form)))))]
    (cond-> {:strength (number-param (:strength volume 1.0) [0 1])
             :inner-m (number-param (get-in volume [:falloff :inner-m] 0.0) [0 10000])
             :outer-m (number-param (get-in volume [:falloff :outer-m] 0.0) [0 10000])
             :translation (vector-param (:translation volume [0 0 0]))
             :scale (assoc (vector-param (:scale volume [1 1 1])) :bounds [0.000001 10000])}
      (:position volume) (assoc :position (vector-param (:position volume)))
      (and ball? (= :sphere (:shape (:geometry volume)))) (assoc :shape-radius (number-param (:radius form) [0 10000]))
      (:radius volume) (assoc :radius (number-param (:radius volume) [(if ball? 0.000001 0) (if ball? max-radius 10000)]))
      morph? (assoc :shape {:type :enum :values [:sphere :rounded-box] :default shape}
                    :size (assoc (vector-param (or (:size form) (vec (repeat 3 (max 0.000001 (* 2 radius))))))
                                 :bounds [0.000001 10000])
                    :rounding (number-param (:rounding form 0) [0 1])
                    :radius (number-param radius [(if ball? 0.000001 0) (if ball? max-radius 10000)]))
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

(defn- available-cell [page preferred]
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
        pos (available-cell page cell)
        volume (cond-> volume (vector? (:selection volume)) (update :selection set))
        aim-volume? (= :volume (:aim paint))
        paint (cond-> (or paint {}) aim-volume? (dissoc :aim))
        sources (or sources {})
        member-list (into (vec (scene-slots model scene-id)) members)
        params (merge (dimensions model volume) (into {} (map paint-param) paint))
        spec (cond-> {:volume volume :sources sources :members (mapv (partial validate-member! model) member-list)}
               aim-volume? (assoc :aim-at-volume? true))
        scene-opacity (get-in model [:compositions scene-id :opacity] 1.0)
        effect {:id id :version 1 :name (or name (str id)) :show-control? true
                :spatial-volume spec :layer {:transforms []} :params params
                :outputs (into {} (map (fn [attr] [attr {:op :param :key attr}])) (keys paint))}
        cue (cond-> {:id id :effect-id id :name (:name effect) :version 1 :show-control? true}
              (not= 1.0 (double scene-opacity)) (assoc :system {:opacity scene-opacity})
              priority (assoc :priority priority))]
    (when-not (and (= :spawn op) (id? id) (id? page-id)
                   (not (get-in model [:registry :effects id])) (not (get-in model [:registry :cues id]))
                   (map? paint) (or (nil? members) (vector? members)) (<= (count member-list) 64)
                   (or aim-volume? (seq paint) (seq member-list))
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
(defn- spatial-candidate? [model instance]
  (or (controls? instance)
      (get-in model [:registry :effects (:effect-id instance) :spatial-volume])))

(defn needed? [model]
  (or (get-in model run-path) (some #(spatial-candidate? model %) (vals (:instances model)))))

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
  (let [ball? (= :ball (:shape template))
        shape (:shape values)
        shape-fields (case shape
                       :rounded-box {:shape shape :size (:size values) :rounding (:rounding values)}
                       :sphere {:shape shape :radius (:shape-radius values (:radius values))}
                       nil)
        geometry (cond
                   (nil? shape-fields) template
                   ball? (if (and (nil? (:geometry template)) (= shape :sphere)) template
                             (assoc template :geometry (merge (:geometry template) shape-fields
                                                        {:position (get-in template [:geometry :position] [0 0 0])})))
                   :else (merge template shape-fields))]
    (-> geometry
        (merge (select-keys values [:strength :translation :scale :position :velocity :restitution]))
        (cond-> (#{:ball :cylinder} (:shape template)) (assoc :radius (:radius values)))
        (update :falloff merge (select-keys values [:inner-m :outer-m])))))

(defn- live-instance [model clock instance]
  (when (spatial-candidate? model instance)
    (let [{:keys [instance expired?]} (engine/instance-lifecycle model clock instance)]
      (when (and (not expired?) (controls? instance)) instance))))

(defn- live-volume [model modulation previous settings key instance]
  (let [{:keys [members] :as spec} (get-in instance [:definition :spatial-volume])]
    (try
      (let [values (controls model instance modulation)
            resolved (volume (:volume spec) values)
            room (select-keys settings [:bounds :gravity])]
        (when-not (and (= room (select-keys previous [:bounds :gravity]))
                       (= resolved (get-in previous [:volumes key])))
          (physics/validate! (assoc room :volumes {key resolved}))
          (field/prepare resolved))
        {:instance instance :volume resolved :values values :members members})
      (catch Exception ex
        {:instance instance :error (ex-message ex)}))))

(defn- admit-volume [static {:keys [entries volumes balls] :as state} [key entry]]
  (let [ball? (= :ball (get-in entry [:volume :shape]))
        error (cond
                (contains? static key) "A configured volume already uses this cue's ID"
                (>= volumes 256) "The room already has 256 active volumes"
                (and ball? (>= balls 64)) "The room already has 64 active balls"
                :else (:error entry))]
    (if error
      (assoc-in state [:entries key] {:instance (:instance entry) :error error})
      {:entries (assoc entries key entry) :volumes (inc volumes) :balls (if ball? (inc balls) balls)})))

(defn volumes [model modulation previous]
  (let [settings (get-in model [:settings :spatial])
        static (when (:enabled? settings) (:volumes settings))
        clock (time/frame-clock model)
        live (keep #(live-instance model clock %) (vals (:instances model)))
        prepare (fn [instance]
                  (let [key (key-for instance)]
                    [key (live-volume model modulation previous settings key instance)]))
        occupancy {:entries {} :volumes (count static) :balls (count (filter #(= :ball (:shape %)) (vals static)))}]
    (->> live
         (sort-by :order)
         (map prepare)
         (reduce (partial admit-volume static) occupancy)
         :entries)))

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
      (let [_ (validate-member! model (get members index))
            source (member-source key index)
            payload (merge {:source source :launch (or launch {})}
                           (if cue-id {:op :cue/launch :cue-id cue-id} {:op :effect/launch :effect-id effect-id}))
            result (accept model command payload now)]
        [(:model result) (assoc ids index (get-in result [:ack :instance-id]))]))
    [model {}] members))

(defn- member-versions [model members]
  (mapv (fn [{:keys [cue-id effect-id]}]
          (let [cue (get-in model [:registry :cues cue-id])
                id (or effect-id (:effect-id cue))]
            [(:version cue) id (get-in model [:registry :effects id :version])]))
        members))

(defn- replace-lease [model leases key members now command]
  (let [model (stop-members model key (get leases key) now command)]
    (if (empty? members)
      [model (dissoc leases key)]
      (try
        (let [[model ids] (start-members model key members now command)]
          [model (assoc leases key {:members members :ids ids})])
        (catch clojure.lang.ExceptionInfo error
          [model (assoc leases key {:members members :error (ex-message error)
                                    :failed-versions (member-versions model members)})])))))

(defn reconcile
  "Scope one live activation per scene member, independent of the number of
  heads inside the field. Ending the parent only removes its recorded children."
  [model active now command]
  (let [prior (get-in model run-path {})
        renew (fn [[model leases] key]
                (let [members (get-in active [key :members])
                      old (get leases key)]
                  (if (and (= members (:members old))
                           (or (nil? (:error old)) (= (:failed-versions old) (member-versions model members))))
                    [model leases]
                    (replace-lease model leases key members now command))))]
    (if (and (empty? active) (empty? prior))
      model
      (let [owners (sort (into (set (keys prior)) (keys active)))
            [model leases] (reduce renew [model prior] owners)]
        (if (seq leases) (assoc-in model run-path leases) (update model :extensions dissoc ::members))))))

(defn- group-rows [model key active weights]
  (let [{:keys [instance members]} active
        opacity (* (get-in instance [:system :opacity] 1.0) (system/gain instance (system/private-clock instance model)))
        descriptor {:id key :target-weights weights :opacity opacity}
        leases (get-in model (into run-path [key :ids]))]
    (into {(:id instance) (assoc descriptor :member 0 :member-opacity 1.0)}
          (for [[index id] leases
                :let [slot (get members index)]]
            [id (merge descriptor {:member (inc index)}
                       (select-keys slot [:transforms :targets]))]))))

(defn- coverage [volume instance aim? sample]
  (let [{:keys [weights aims]} (when volume (sample volume))
        weights (if (contains? instance :targets) (select-keys weights (:targets instance)) (or weights {}))]
    (if-not (and volume aim?) {:weights weights}
      (let [aims (if (contains? (:selection volume) :aimable) aims (:aims (sample (assoc volume :selection #{:aimable}))))]
        {:weights weights :aims (select-keys aims (keys weights))}))))

(defn- record-field [model physical sample frame key {:keys [volume values instance] :as entry}]
  (let [position (get-in physical [:signals key :position])
        volume (cond-> volume (and (= :ball (:shape volume)) position) (assoc :position position))
        spec (get-in instance [:definition :spatial-volume])
        {:keys [weights aims]} (coverage volume instance (:aim-at-volume? spec) sample)
        error (or (:error entry) (get-in model (into run-path [key :error])))
        paint (cond-> (select-keys values (keys (get-in instance [:definition :outputs])))
                (:aim-at-volume? spec) (assoc :aim :volume))
        view {:volume volume :cue-id (:cue-id instance) :instance-id (:id instance)
              :paint paint :affected-head-count (count weights)}]
    (cond-> (-> frame
                (update :groups into (group-rows model key entry weights))
                (assoc-in [:overrides (:id instance)] (select-keys values (keys (:sources spec))))
                (assoc-in [:members key] (set (keys weights)))
                (assoc-in [:views key] (cond-> view error (assoc :error error))))
      aims (assoc-in [:aims (:id instance)] aims))))

(defn frame-fields [model active physical sample]
  (reduce-kv (partial record-field model physical sample) {:groups {} :overrides {} :views {} :members {}} active))

(defn with-aims
  "Add per-head reachable points to the same native layer as the cue's paint."
  [definition step]
  (if-not (get-in definition [:spatial-volume :aim-at-volume?]) step
    (fn [context]
      (let [result (step context)
            aims (get-in context [:frame :spatial-aims (get-in context [:instance :id])])]
        (if (or (:done? result) (empty? aims)) result
          (update result :assignments into
                  (map (fn [[head point]] {:target-id head :attribute :aim :value point :blend :replace}))
                  aims))))))

(defn- scoped-block [groups block]
  (if-let [group (get groups (get-in block [:source :instance-id]))]
    (let [native (:native-layer block)
          layer (cond-> (merge native group)
                  (and (seq (:transforms native)) (seq (:transforms group)))
                  (assoc :transforms (into (:transforms group) (:transforms native))))]
      (assoc block :native-layer layer))
    block))

(defn mask-contributions [result groups]
  (if (empty? groups) result
    (update result :contributions #(mapv (partial scoped-block groups) %))))
