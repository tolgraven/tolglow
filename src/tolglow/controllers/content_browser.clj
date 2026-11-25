(ns tolglow.controllers.content-browser
  "Pure Push catalog, window and action planning from committed show data."
  (:require [clojure.string :as str]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.registry :as registry]
            [tolglow.signals :as signals])
  (:import [java.util Locale]))

(def kinds [:show :effect :cue :waveform])

(def page-size 8)

(def ^:private max-save-sources 128)

(def ^:private max-query-length 64)

(def ^:private builtin-waveforms (delay (signals/new-registry)))

(defn- trim-to [limit value] (subs value 0 (min limit (count value))))

(defn- fold [value] (.toLowerCase ^String (str value) Locale/ROOT))

(defn- named? [value] (and (string? value) (not (str/blank? value))))

(defn- label [id name]
  (trim-to 80 (if (named? name) name (str id))))

(defn- token? [value]
  (let [part? #(and (string? %) (boolean (re-matches #"[A-Za-z0-9][A-Za-z0-9._:-]{0,127}" %)))]
    (if (keyword? value)
      (and (part? (name value)) (or (nil? (namespace value)) (part? (namespace value))))
      (part? value))))

(defn- save-groups [saves]
  (when-not (and (vector? saves) (<= (count saves) max-save-sources))
    (throw (ex-info "Expected a bounded vector of configured save sources" {:limit max-save-sources})))
  (reduce (fn [groups {:keys [show-id source-kind handle revision] :as save}]
            (when-not (and (or (keyword? show-id) (named? show-id))
                           (#{:named :recovery} source-kind) (token? handle)
                           (or (nil? revision) (nat-int? revision) (token? revision))
                           (not (contains? (get groups show-id) source-kind)))
              (throw (ex-info "Invalid or duplicate configured save source" {:source-kind source-kind})))
            (assoc-in groups [show-id source-kind] save))
          {} saves))

(defn- type-tags [definition]
  (let [{:keys [categories category tags]} definition]
    (->> (concat (registry/definition-categories (cond-> definition
                                                   (not (coll? categories)) (dissoc :categories)))
                 [category] (when (coll? tags) tags))
         (filter keyword?) distinct (sort-by str) (take 6) vec)))

(defn- entry [session kind id name metadata]
  (let [key [kind id]]
    (merge {:key key :kind kind :id id :name (label id name)
            :selection (assoc (merge session (select-keys metadata [:version :effect-version]))
                              :key key)}
           metadata)))

(defn- indexes [model]
  (let [instances (vals (:instances model))
        running (filter #(= :running (:phase %)) instances)]
    {:running-effects (frequencies (map :effect-id running))
     :running-cues (frequencies (keep :cue-id running))
     :effect-cues (frequencies (map :effect-id (vals (get-in model [:registry :cues]))))
     :cue-pages (frequencies (mapcat #(set (vals (:cells %))) (vals (:pages model))))
     :attached-waveforms (frequencies (for [instance instances
                                           {:keys [binding]} (vals (:parameter-bindings instance))]
                                       (:waveform binding)))}))

(defn- entry-source [model saves kind]
  (case kind
    :show (save-groups saves)
    :effect (get-in model [:registry :effects])
    :cue (get-in model [:registry :cues])
    :waveform (:waveforms (or (:waveforms model) @builtin-waveforms))))

(defn- content-entry [model usage kind id item]
  (let [session (select-keys model [:show-id :epoch])]
    (case kind
      :show
      (let [name (first (filter named? (map #(get-in item [% :name]) [:named :recovery])))
            sources (update-vals item #(select-keys % [:source-kind :handle :revision]))]
        (entry session kind id name {:sources sources}))

      :effect
      (entry session kind id (:name item)
             {:version (:version item) :categories (type-tags item)
              :usage {:cue-count (get-in usage [:effect-cues id] 0)
                      :running-count (get-in usage [:running-effects id] 0)}})

      :cue
      (let [effect (registry/cue-effect (:registry model) item)]
        (entry session kind id (or (:name item) (:name effect))
               {:version (:version item) :effect-id (:effect-id item) :effect-version (:version effect)
                :categories (vec (take 6 (distinct (concat (type-tags effect) (type-tags item)))))
                :usage {:page-count (get-in usage [:cue-pages id] 0)
                        :running-count (get-in usage [:running-cues id] 0)}}))

      :waveform
      (entry session kind id (:name item)
             {:version (:version item) :categories (type-tags item) :range (:range item)
              :usage {:input-count (count (:params item))
                      :attached-count (get-in usage [:attached-waveforms id] 0)}}))))

(defn catalog
  "Index trusted committed registry data and configured save descriptors.
  Saves provide :show-id, :source-kind (:named or :recovery), :handle, optional
  :name and :revision. Supply at most one latest descriptor per kind and show.
  The host persists or reconstructs the handle-to-full-show-file mapping after
  restart. A handle without a revision must identify an immutable save. The
  catalog contains only presentation fields and exact identities."
  [model saves]
  (let [usage (indexes model)]
    {:entries (into {}
                    (for [kind kinds]
                      [kind (into {} (map (fn [[id item]] [id (content-entry model usage kind id item)]))
                                  (entry-source model saves kind))]))}))

(defn- content-key? [key]
  (and (vector? key) (= 2 (count key)) (some #{(first key)} kinds)
       (let [id (second key)] (or (keyword? id) (and (string? id) (seq id))))))

(defn- matches? [query {:keys [id kind name categories sources]}]
  (some #(str/includes? (fold %) query) (concat [name id kind] categories (keys sources))))

(defn project
  "Return one eight-row page. Selection survives paging and filtering by exact
  [kind id] while the identity still exists in the chosen kind."
  [catalog {:keys [kind query page selected-key]}]
  (let [kind (or (some #{kind} kinds) :show)
        query (if (string? query) (str/trim (trim-to max-query-length query)) "")
        folded (fold query)
        entries (->> (get-in catalog [:entries kind]) vals (filter (partial matches? folded))
                     (sort-by (juxt (comp fold :name) (comp str :id) (comp keyword? :id))) vec)
        total (count entries)
        page-count (quot (+ total (dec page-size)) page-size)
        page (if (integer? page) (max 0 (min page (dec page-count))) 0)
        offset (* page page-size)
        items (subvec entries offset (min total (+ offset page-size)))
        selected-key (when (and (content-key? selected-key) (= kind (first selected-key))
                                 (contains? (get-in catalog [:entries kind]) (second selected-key)))
                       selected-key)]
    {:kind kind :kinds kinds :query query :page page :page-count page-count :total total
     :items items :selected-key selected-key
     :selected-visible? (boolean (some #(= selected-key (:key %)) items))}))

(defn- reject [reason] {:status :rejected :reason reason})

(defn- planned [entry type payload]
  {:status :planned :selection (:selection entry) :request {:type type :payload payload}})

(defn- plan-load [entry source]
  (let [current (get-in entry [:sources (:source-kind source)])
        {:keys [show-id epoch]} (:selection entry)]
    (if (and current (= current source))
      (planned entry :load-show (assoc current :target-show-id (:id entry)
                                               :expected-show-id show-id :expected-epoch epoch))
      (reject :stale-save-source))))

(defn- plan-launch [{:keys [kind id selection] :as entry}]
  (let [payload (select-keys selection [:show-id :epoch])]
    (planned entry :command
             (if (= :cue kind)
               (assoc payload :op :cue/launch :cue-id id)
               (assoc payload :op :effect/launch :effect-id id)))))

(defn- plan-attach [model entry {:keys [instance-id param binding-options]}]
  (let [instance (get-in model [:instances instance-id])]
    (cond
      (or (not= :running (:phase instance)) (not (contains? (get-in instance [:definition :params]) param)))
      (reject :ineligible-parameter)

      (or (not (map? binding-options)) (contains? binding-options :type) (contains? binding-options :waveform))
      (reject :invalid-binding)

      :else
      (let [payload {:instance-id instance-id :param param :expected-epoch (:epoch model)
                     :binding (assoc binding-options :type :lfo :waveform (:id entry))}]
        (try
          (let [validated (parameter-sources/bind model payload)
                binding (get-in validated [:instances instance-id :parameter-bindings param :binding])]
            (planned entry :modulation-bind (assoc payload :binding binding)))
          (catch clojure.lang.ExceptionInfo error
            (if (= ::parameter-sources/invalid-binding (:type (ex-data error)))
              (reject :invalid-binding)
              (throw error))))))))

(defn plan
  "Validate an exact row :selection captured from project, against the current
  committed model and save descriptors. :preview returns metadata. :load needs
  one exact projected :source. :launch needs a cue or effect. :attach needs a
  waveform, :instance-id, :param and the Studio LFO :binding-options (period,
  endpoints, phase and inputs). This function only produces data. A future
  dispatcher must plan with the current state at owner admission."
  [model saves {:keys [op selection source] :as request}]
  (if-not (and (map? selection)
               (= (select-keys model [:show-id :epoch]) (select-keys selection [:show-id :epoch])))
    (reject :stale-session)
    (let [[kind id :as key] (when (content-key? (:key selection)) (:key selection))
          item (when key (get (entry-source model saves kind) id))
          usage (when (and item (= :preview op)) (indexes model))
          entry (when item (content-entry model usage kind id item))]
      (cond
        (or (nil? entry) (not= (:selection entry) selection)) (reject :stale-selection)
        (= :preview op) {:status :preview :entry entry}
        (and (= :load op) (= :show kind)) (plan-load entry source)
        (and (= :launch op) (#{:cue :effect} kind)) (plan-launch entry)
        (and (= :attach op) (= :waveform kind)) (plan-attach model entry request)
        :else (reject :action-unavailable)))))
