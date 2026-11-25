(ns tolglow.spatial.actions
  "Apply fixture contact edges as scoped, independently owned show commands."
  (:require [afterglow.time :as time]
            [clojure.set :as set]
            [clojure.string :as str]
            [tolglow.automation.random :as random]
            [tolglow.registry :as registry]
            [tolglow.spatial.targets :as targets]))

(def run-path [:extensions ::run])

(defn- id? [v] (or (keyword? v) (and (string? v) (not (str/blank? v)))))

(defn- duration? [v] (or (= v :contact) (and (map? v) (empty? (time/validate-duration v))
                                             (pos? (val (first v))))))

(defn- action? [a]
  (and (map? a) (= 1 (count a))
       (case (ffirst a)
         (:cue-id :effect-id) (id? (val (first a)))
         :preset (#{:flicker :strobe :aim} (:preset a))
         :random (let [{:keys [kind limit seed attributes categories] :or {kind :cues limit 3 seed 1}} (:random a)]
                   (and (map? (:random a)) (#{:cues :effects} kind) (pos-int? limit)
                        (integer? seed) (every? keyword? attributes) (every? keyword? categories)))
         false)))

(defn validate-bindings! [bindings]
  (when-not (and (map? bindings)
                 (every? (fn [[id b]]
                           (and (id? id) (map? b) (id? (:volume-id b)) (action? (:action b))
                                (every? #{:volume-id :action :duration :selector :params :priority :enabled? :force?} (keys b))
                                (duration? (:duration b :contact))
                                (or (not (contains? b :enabled?)) (boolean? (:enabled? b)))
                                (or (not (contains? b :force?)) (boolean? (:force? b)))
                                (or (not (contains? b :priority)) (registry/finite-number? (:priority b)))
                                (or (not (:selector b)) (targets/selector? (:selector b)))
                                (map? (:params b {})) (registry/data? (:params b {})))) bindings))
    (throw (ex-info "Invalid spatial binding, action, parameter map or lifetime" {:bindings bindings})))
  bindings)

(defn- source [binding-id fixture-id] {:adapter :spatial :binding-id binding-id :fixture-id fixture-id})

(defn session-source [model instance]
  (let [{:keys [binding-id fixture-id] :as s} (:source instance)]
    (when (and (= s (source binding-id fixture-id))
               (= (:id instance) (get-in model (into run-path [:leases binding-id fixture-id :id])))) s)))

(defn restore [model ids _epoch]
  (if-let [run (get-in model run-path)]
    (assoc-in model run-path
              (update run :leases update-vals
                      (fn [by-head]
                        (into {} (keep (fn [[head lease]]
                                         (when-let [id (get ids (:id lease))] [head (assoc lease :id id)]))) by-head))))
    model))

(defn- issue [model owner cmd now command]
  (let [result (command model (assoc cmd :source owner
                                     :command-id (pr-str [:spatial (:epoch model) (:revision model) owner (:op cmd) now]))
                        now)]
    (when-not (= :accepted (get-in result [:ack :status]))
      (throw (ex-info (get-in result [:ack :message] "Area action rejected") {:ack (:ack result)})))
    result))

(defn- finite [v] (when (registry/finite-number? v) (double v)))

(defn- parameter [signals value]
  (if (and (map? value) (:signal value))
    (let [sample (get signals (:signal value) (:default value))
          sample (if-some [axis (:axis value)] (get sample ({:x 0 :y 1 :z 2} axis axis)) sample)
          transform (fn [v] (let [v (+ (* (or (finite v) (throw (ex-info "Missing numeric spatial signal" {:signal value})))
                                           (:scale value 1)) (:offset value 0))]
                              (if-let [[lo hi] (:bounds value)] (max lo (min hi v)) v)))]
      (if (vector? sample) (mapv transform sample) (transform sample)))
    value))

(defn- controlled [i] (select-keys (:system i) [:opacity :gain :paused? :attack :release]))

(defn- leased? [lease i owner]
  (and i (= owner (:source i)) (= (:params lease) (:params i)) (= (:controls lease) (controlled i))))

(defn- strobe [fixture]
  (some (fn [a]
          (when-let [ch (get-in fixture [:channels a])]
            (if (seq (:functions ch))
              (when-let [{:keys [start end]} (some #(when (str/includes? (name (:type %)) "strobe") %) (:functions ch))]
                [a (/ (+ start (quot (- end start) 2)) 255.0)])
              (when (= a :strobe) [a 0.5])))) [:strobe :shutter]))

(defn- preset [kind fixture]
  (let [unit {:type :number :bounds [0 1] :default 0.5}
        fields (case kind
                 :aim (when (every? (:channels fixture) [:pan :tilt])
                        {:params {:point {:type :vector :size 3 :default [0 0 0]}}
                         :outputs {:aim {:op :param :key :point}}})
                 :strobe (when-let [[a drive] (strobe fixture)]
                           {:params {:drive unit} :outputs {a {:op :param :key :drive}} :initial {:drive drive}})
                 :flicker (when (or (get-in fixture [:channels :dimmer]) (some (:channels fixture) registry/emitter-attributes))
                            {:params {:rate {:type :number :bounds [0 64] :default 8.0}}
                             :outputs {:dimmer {:op :fract :args [{:op :multiply :args [43758.5453
                                        {:op :sin :args [{:op :add :args [1.181 {:op :floor :args [
                                        {:op :multiply :args [{:op :beat} {:op :param :key :rate}]}]}]}]}]}]}}}))]
    (when fields (let [id (keyword "tolglow.spatial" (if (= kind :strobe) (str "strobe-" (name (first (keys (:outputs fields))))) (name kind)))]
                   {:definition (merge {:id id :name (str "Spatial " (name kind)) :version 1 :spatial-preset true} (dissoc fields :initial))
                    :params (:initial fields)}))))

(defn- compatible? [fixture effect]
  (let [id (:id fixture) channels (:channels fixture)
        writes (concat (registry/output-entries (:outputs effect)) (get-in effect [:layer :transforms]))]
    (and (or (nil? (:targets effect)) (some #{id} (:targets effect)))
         (some (fn [{:keys [targets attribute]}]
                 (and (or (nil? targets) (some #{id} targets))
                      (case attribute
                        (:aim :direction) (every? channels [:pan :tilt])
                        :color (every? channels [:red :green :blue])
                        :dimmer (or (:dimmer channels) (some channels registry/emitter-attributes))
                        (contains? channels attribute)))) writes))))

(defn- lighting? [definition]
  (let [control #{:show-control :code :code-cue}]
    (and definition (not (:show-control? definition)) (not (:code-cue? definition))
         (not (some control (concat [(:kind definition) (:role definition)] (:tags definition) (:categories definition)))))))

(defn- launch-choice [model {:keys [cue-id effect-id]} fixture]
  (let [registry (:registry model)
        cue (when cue-id (get-in registry [:cues cue-id]))
        effect (if cue (registry/cue-effect registry cue) (get-in registry [:effects effect-id]))]
    (when (and (or (nil? cue-id) (lighting? cue)) (lighting? effect) (compatible? fixture effect))
      (if cue {:op :cue/launch :cue-id cue-id} {:op :effect/launch :effect-id effect-id}))))

(defn- random-catalog [model {:keys [kind limit] :as spec}]
  (let [effects? (= kind :effects)]
    (->> (targets/search model (assoc (select-keys spec [:text :categories :attributes :params])
                                     :kinds [(if effects? :effect :cue)] :lighting-only? true :limit 1000))
         (mapv #(hash-map (if effects? :effect-id :cue-id) (:id %))))))

(defn- launch [model b fixture signals owner choices draw now command]
  (let [{:keys [random preset] :as action} (:action b)
        derived (when preset (tolglow.spatial.actions/preset preset fixture))
        definition (:definition derived)
        previous (get-in model [:registry :effects (:id definition)])
        choice (cond random (let [candidates (into [] (comp (keep #(launch-choice model % fixture)) (take (:limit random 3)))
                                                    (force choices))]
                              (when (seq candidates) (second (random/choose draw candidates))))
                     definition {:op :effect/launch :effect-id (:id definition)}
                     :else (launch-choice model action fixture))]
    (when choice
      (when (and previous (not= previous definition))
        (throw (ex-info "Spatial preset identity already in use" {:effect (:id previous)})))
      (let [model (if (and definition (nil? previous))
                    (:model (issue model owner {:op :registry/publish :kind :effects :definition definition} now command)) model)
            mapping (merge (:params derived) (when (= preset :aim) {:point {:signal :position}}) (:params b))
            overrides (update-vals mapping (partial parameter signals))
            opts (cond-> {:targets [(:id fixture)] :params overrides :reload-policy :preserve-state}
                   (:priority b) (assoc :priority (:priority b))
                   (not= :contact (:duration b :contact)) (assoc :mode :one-shot :duration (:duration b)))
            result (issue model owner (assoc choice :launch opts) now command)
            id (get-in result [:ack :instance-id]) i (get-in result [:model :instances id])]
        [(:model result) {:id id :params (:params i) :controls (controlled i)
                          :mapping (into {} (filter #(and (map? (val %)) (:signal (val %)))) mapping)
                          :contact? (= :contact (:duration b :contact))}]))))

(defn- update-params [m lease signals owner now command]
  (let [id (:id lease)]
    (reduce-kv (fn [[m lease] k spec]
                 (let [v (parameter signals spec)]
                   (if (= v (get-in lease [:params k])) [m lease]
                       [(:model (issue m owner {:op :param/set :instance-id id :param k :value v} now command))
                        (assoc-in lease [:params k] v)]))) [m lease] (:mapping lease))))

(defn apply-frame
  "Reconcile contacts, updating mapped cue inputs while each head stays inside.
  Removed bindings release owned instances even if a timed launch was requested.
  A manually edited or ended instance is left alone until a new contact."
  [model {:keys [members signals events]} bindings now command]
  (let [prior (get-in model run-path)
        bindings (into {} (remove #(false? (:enabled? (val %)))) bindings)]
    (if (and (empty? bindings) (nil? prior)) model
        (let [fixtures (delay (into {} (map (juxt :id identity)) (get-in model [:rig :fixtures])))
              swept (delay (reduce (fn [m {:keys [type volume-id fixture-id]}]
                                     (if (= :spatial/enter type) (update m volume-id (fnil conj #{}) fixture-id) m))
                                   {} events))
              state (or prior {:contacts {} :leases {} :seeds {} :errors {}})
              ids (sort-by pr-str (set/union (set (keys (:bindings state))) (set (keys bindings))))
              [model result]
              (reduce
               (fn [[m state] id]
                 (let [before (get-in state [:bindings id]) b (get bindings id)
                       old (get-in state [:contacts id] #{})
                       changed? (not= b before)
                       volume-id (:volume-id b)
                       members-now (get members volume-id #{})
                       passages (when (map? (:duration b)) (get @swept volume-id))
                       selection (try
                                   (if-let [selector (:selector b)]
                                     ;; A swept contact was inside its own source volume when the entry occurred.
                                     (let [allowed (set (targets/resolve-ids m selector (update members volume-id into passages)))]
                                       {:contacts (set/intersection members-now allowed)
                                        :passages (filter allowed passages)})
                                     {:contacts members-now :passages passages})
                                   (catch clojure.lang.ExceptionInfo ex {:contacts #{} :error (ex-message ex)}))
                       contacts (:contacts selection)
                       entries (into (if changed? contacts (set/difference contacts old)) (:passages selection))
                       leases (get-in state [:leases id] {})
                       signal (get signals (:volume-id b))
                       choices (delay (random-catalog m (get-in b [:action :random])))
                       empty-state (cond-> (update state :leases dissoc id)
                                     (or changed? (seq entries)) (update :errors dissoc id)
                                     (:error selection) (assoc-in [:errors id] (:error selection)))
                       [m state]
                       (reduce-kv
                        (fn [[m st] head lease]
                          (let [owner (source id head) i (get-in m [:instances (:id lease)])]
                            (if-not (and (= :running (:phase i)) (leased? lease i owner)) [m st]
                                (try
                                  (cond
                                    (or changed? (contains? entries head) (and (:contact? lease) (not (contains? contacts head))))
                                    [(:model (issue m owner {:op :instance/end :instance-id (:id lease)
                                                            :force? (boolean (:force? before))} now command)) st]
                                    (contains? contacts head)
                                    (let [[m lease] (update-params m lease signal owner now command)]
                                      [m (assoc-in st [:leases id head] lease)])
                                    :else [m (assoc-in st [:leases id head] lease)])
                                  (catch Exception ex
                                    [m (-> st (assoc-in [:errors id] (ex-message ex)) (assoc-in [:leases id head] lease))])))))
                        [m empty-state] leases)
                       [m state]
                       (reduce (fn [[m st] head]
                                 (let [base (get-in st [:seeds id] (get-in b [:action :random :seed] 1))
                                       draw (if (get-in b [:action :random]) (first (random/next-random base)) base)
                                       st (assoc-in st [:seeds id] draw)]
                                   (try
                                     (if-let [[m lease] (when-let [f (@fixtures head)]
                                                         (launch m b f signal (source id head) choices draw now command))]
                                       [m (assoc-in st [:leases id head] lease)] [m st])
                                     (catch Exception ex [m (assoc-in st [:errors id] (ex-message ex))]))))
                               [m state] (sort-by pr-str entries))]
                   [m (if b (assoc-in state [:contacts id] contacts)
                          (-> state (update :contacts dissoc id) (update :seeds dissoc id)))]))
               [model state] ids)]
          (if (seq bindings) (assoc-in model run-path (assoc result :bindings bindings))
              (if (:extensions model) (update model :extensions dissoc ::run) model))))))
