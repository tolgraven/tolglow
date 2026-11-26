(ns tolglow.audio.stem-routes
  "What each named stem drives (#675), shared by the REPL and Studio. Onsets
  route through event subscribers that match {:type :audio/onset :emitter-id
  :stem/<id>}; levels and onset gates route through the signal patch from the
  stem/<id>/level and stem/<id>/onset ports. These functions only build and
  read that existing data: there is no separate stem routing store.")

(def presets
  "Preset names offered by Studio, in display order. tolglow.audio.stems owns
  their detector settings."
  [:kick :snare :hats :bass :guitar :vocals :keys :full])

(defn- text [x] (if (keyword? x) (if-let [ns (namespace x)] (str ns "/" (name x)) (name x)) (str x)))

(defn emitter-id [stem] (keyword "stem" (text stem)))

(defn port-id [stem feature] (str "stem/" (text stem) "/" (text feature)))

(defn- stem-of-emitter [id]
  (let [t (text id)] (when-let [[_ stem] (re-matches #"stem/([a-z][a-z0-9-]*)" t)] (keyword stem))))

(defn- stem-of-port [id]
  (when-let [[_ stem feature] (re-matches #"stem/([a-z][a-z0-9-]*)/(level|onset)" (text id))]
    [(keyword stem) (keyword feature)]))

(defn onset-subscriber
  "An event subscriber that launches cue-id on every onset of stem. Optional
  :grid quantizes the launch, e.g. {:beats 1/4}; :id overrides the default
  :stem-<stem>-<cue> identity."
  ([stem cue-id] (onset-subscriber stem cue-id {}))
  ([stem cue-id {:keys [id grid]}]
   {:id (or id (keyword (str "stem-" (text stem) "-" (text cue-id))))
    :match {:type :audio/onset :emitter-id (emitter-id stem)}
    :actions [(cond-> {:op :cue/launch :select {:ids [(keyword (text cue-id))] :pick :first}}
                grid (assoc :grid grid))]}))

(defn with-level-route
  "Return patch spec (or a new one) with a route from stem's level (or
  :feature :onset) to a numeric cue parameter. The source, destination and
  route are added or replaced by identity and the revision advances, so the
  result saves through the ordinary patch save. A destination that already
  exists keeps its settings, so routes can share it. Options: :range output
  bounds [0 1], :combine :max, :timeout {:beats 16}, :envelope ADSR map."
  [spec stem cue-id param {:keys [feature range combine timeout envelope]
                           :or {feature :level range [0.0 1.0] combine :max timeout {:beats 16}}}]
  (let [source (port-id stem feature)
        destination (keyword (str (text cue-id) "-" (text param)))
        route-id (keyword (str "stem-" (text stem) "-" (text feature) "-" (text cue-id) "-" (text param)))
        spec (or spec {:version 1 :id :stems :revision 0 :sources {} :destinations {} :routes []})]
    (-> spec
        (update :revision inc)
        (assoc-in [:sources source] {:kind :audio :range [0.0 1.0] :timeout timeout})
        (update :destinations #(merge {destination {:cue-id cue-id :param param :range range :base (first range)
                                                    :combine combine}} %))
        (update :routes (fn [routes]
                          (conj (vec (remove #(= route-id (:id %)) routes))
                                (cond-> {:id route-id :source source :destination destination}
                                  envelope (assoc :envelope envelope))))))))

(defn routes
  "Group what drives each stem: {stem {:onsets [{:subscriber-id :enabled?
  :actions}] :ports [{:route-id :feature :cue-id :param}]}} from event
  subscriber definitions and a patch spec. Ids may arrive as keywords or
  text (Studio's JSON view)."
  [subscribers spec]
  (let [destinations (:destinations spec)
        lookup (fn [m k] (or (get m k) (get m (keyword (text k))) (get m (text k))))
        onsets (for [{:keys [id match actions enabled?]} subscribers
                     :let [stem (when (= "audio/onset" (text (:type match))) (stem-of-emitter (:emitter-id match)))]
                     :when stem]
                 [stem :onsets {:subscriber-id id :enabled? (not (false? enabled?))
                                :actions (mapv #(select-keys % [:op :select :grid :delay]) actions)}])
        ports (for [{:keys [id source destination]} (:routes spec)
                    :let [[stem feature] (stem-of-port source) target (lookup destinations destination)]
                    :when stem]
                [stem :ports {:route-id id :feature feature :cue-id (:cue-id target) :param (:param target)}])]
    (reduce (fn [m [stem kind entry]] (update-in m [stem kind] (fnil conj []) entry)) {} (concat onsets ports))))
