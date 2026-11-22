(ns tolglow.registry
  "Pure, versioned authoring definitions. This namespace never starts a show."
  (:require [clojure.string :as str]
            [tolglow.fixtures.shutter :as shutter]
            [afterglow.time :as time]
            [afterglow.render.layers :as layers]
            [afterglow.render.photometry :as photometry]
            [afterglow.control.categories :as categories]
            [afterglow.control.system :as system]))

(defn finite-number? [x]
  (and (number? x) (Double/isFinite (double x))))

(defn data?
  "Whether x is portable EDN authoring data (no functions, refs or Java objects)."
  [x]
  (cond
    (or (nil? x) (string? x) (keyword? x) (boolean? x)) true
    (number? x) (finite-number? x)
    (vector? x) (every? data? x)
    (map? x) (every? (fn [[k v]] (and (or (keyword? k) (string? k) (integer? k))
                                    (data? v))) x)
    :else false))

(defn- identifier? [x]
  (or (keyword? x) (and (string? x) (not (str/blank? x)))))

(defn- error [path message] {:path path :message message})
(defn- ordered-pair? [x]
  (and (vector? x) (= 2 (count x)) (every? finite-number? x)
       (<= (first x) (second x))))

(defn- bounded? [bounds value]
  (or (nil? bounds)
      (and (ordered-pair? bounds) (<= (first bounds) value (second bounds)))))

(defn valid-value?
  "Test a parameter value against a normalized schema. Does not coerce types."
  [{:keys [type bounds values size]} value]
  (let [numeric? #(and (finite-number? %) (bounded? bounds %))]
    (case type
      :number (numeric? value)
      :integer (and (integer? value) (numeric? value))
      :boolean (boolean? value)
      :enum (and (vector? values) (boolean (some #(= value %) values)))
      :color (and (vector? value) (= 3 (count value))
                  (every? #(and (numeric? %) (<= 0 % 1)) value))
      :vector (and (vector? value) (= size (count value)) (every? numeric? value))
      :range (and (ordered-pair? value) (every? numeric? value))
      false)))

(defn validate-parameter
  "Return path/message errors for one schema. Path is optional for embedding."
  ([schema] (validate-parameter schema []))
  ([schema path]
   (if-not (map? schema)
     [(error path "Parameter schema must be a map")]
     (let [{:keys [type bounds recommended values size random]} schema
           numeric-types #{:number :integer :color :vector :range}]
       (vec
        (concat
         (when-not (#{:number :integer :boolean :enum :color :vector :range} type)
           [(error (conj path :type) "Unsupported parameter type")])
         (when-not (contains? schema :default)
           [(error (conj path :default) "Parameter needs an explicit default")])
         (when (and (contains? schema :bounds)
                    (or (not (numeric-types type)) (not (ordered-pair? bounds))))
           [(error (conj path :bounds) "Bounds must be an ordered finite numeric pair on a numeric parameter")])
         (when (and (= type :enum)
                    (not (and (vector? values) (seq values) (every? data? values)
                              (= (count values) (count (distinct values))))))
           [(error (conj path :values) "Enum needs distinct portable values")])
         (when (and (= type :vector) (not (and (integer? size) (pos? size))))
           [(error (conj path :size) "Vector needs a positive integer size")])
         (when (and (contains? schema :default)
                    (not (valid-value? schema (:default schema))))
           [(error (conj path :default) "Default does not satisfy the parameter type or bounds")])
         (when (and (contains? schema :recommended)
                    (not (and (numeric-types type) (ordered-pair? recommended)
                              (every? #(bounded? bounds %) recommended))))
           [(error (conj path :recommended) "Recommended range must fit the hard bounds")])
         (when (and (contains? schema :random) (not (map? random)))
           [(error (conj path :random) "Randomization metadata must be a map")])
         (when (and (map? random) (contains? random :max-change)
                    (not (and (finite-number? (:max-change random))
                              (not (neg? (:max-change random))))))
           [(error (conj path :random :max-change) "Maximum change must be finite and nonnegative")])
         (when-not (data? schema)
           [(error path "Parameter schema must contain portable data only")])))))))

(defn validate-params
  "Validate supplied values, rejecting unknown keys. Missing values use defaults
  in resolve-params; this function accepts a partial override map."
  [schemas values]
  (if-not (map? values)
    [(error [:params] "Parameter values must be a map")]
    (vec (mapcat (fn [[k value]]
                   (cond
                     (not (contains? schemas k))
                     [(error [:params k] "Unknown parameter")]
                     (not (valid-value? (get schemas k) value))
                     [(error [:params k] "Value does not satisfy the parameter type or bounds")]
                     :else [])) values))))

(defn resolve-params
  "Merge explicit values over defaults, validate, and return a new map."
  [schemas overrides]
  (let [schemas (or schemas {})
        schema-errors (mapcat (fn [[k schema]] (validate-parameter schema [:params k])) schemas)
        values (merge (into {} (map (fn [[k spec]] [k (:default spec)])) schemas)
                      (or overrides {}))
        errors (into (vec schema-errors) (validate-params schemas values))]
    (when (seq errors)
      (throw (ex-info "Invalid parameter values" {:errors errors})))
    values))

(defn cue-effect
  "Resolve the effect version a cue will launch, including an explicit pin."
  [registry cue]
  (let [current (get-in registry [:effects (:effect-id cue)])]
    (if-let [version (:effect-version cue)]
      (or (get-in registry [:versions [:effects (:effect-id cue) version]])
          (when (= version (:version current)) current))
      current)))

(defn validate-cue-params
  "Validate authored and saved overrides independently against a cue's effect."
  [effect cue]
  (into (validate-params (:params effect {}) (:params cue {}))
        (map #(update % :path (fn [path] (assoc path 0 :saved-params))))
        (validate-params (:params effect {}) (:saved-params cue {}))))

(def emitter-attributes (set (keys photometry/emitter-spectrum)))
(def attributes (into emitter-attributes
                      [:color :dimmer :pan :tilt :focus :zoom :strobe :fog :aim :direction
                       :shutter :iris :frost :speed]))

(def blends #{:replace :max :add})

(defn output-entries
  "Normalize attribute->expression shorthand to deterministic ordered outputs."
  [outputs]
  (cond
    (map? outputs) (mapv (fn [[attribute expression]]
                          {:attribute attribute :value expression :blend :replace})
                        (sort-by (comp str key) outputs))
    (vector? outputs) outputs
    :else []))

(def category-colors categories/colors)
(def definition-categories categories/definition-categories)

(defn- validate-outputs [outputs]
  (if-not (and (or (map? outputs) (vector? outputs)) (seq outputs))
    [(error [:outputs] "Effect needs a nonempty output map or vector")]
    (vec (mapcat
          (fn [[i {:keys [attribute value blend targets] :as output}]]
            (if-not (map? output)
              [(error [:outputs i] "Output must be a map")]
              (concat
             (when-not (attributes attribute)
               [(error [:outputs i :attribute] "Unsupported fixture attribute")])
             (when-not (contains? output :value)
               [(error [:outputs i :value] "Output needs an expression value")])
             (when-not (blends (or blend :replace))
               [(error [:outputs i :blend] "Blend must be replace, max or add")])
             (when (and (contains? output :targets)
                        (not (and (vector? targets) (every? identifier? targets))))
               [(error [:outputs i :targets] "Targets must be a vector of fixture/head IDs")])
             (when-not (data? value)
               [(error [:outputs i :value] "Expression must be portable data")]))))
          (map-indexed vector (output-entries outputs))))))

(defn- validate-layer [layer]
  (if-not (and (map? layer) (every? #{:transforms :targets} (keys layer)))
    [(error [:layer] "Effect layer needs an ordered transform program and optional target inheritance")]
    (concat
     (when (and (contains? layer :targets) (not= :previous (:targets layer)))
       [(error [:layer :targets] "Use :previous to include earlier members’ heads")])
     (try (layers/validate-transforms! (:transforms layer)) []
          (catch clojure.lang.ExceptionInfo cause
            [(error [:layer :transforms] (.getMessage cause))])))))

(defn validate-definition
  "Validate authored data. DSL operator validation occurs during compile-effect.
  Cues may omit version; publish assigns the next version."
  [kind definition]
  (cond
    (not (#{:effects :cues :fixtures} kind))
    [(error [] "Definition kind must be effects, cues or fixtures")]
    (not (map? definition)) [(error [] "Definition must be a map")]
    :else
    (let [{:keys [id version params duration-ms priority targets]} definition]
      (vec
       (concat
        (when-not (identifier? id) [(error [:id] "Definition needs a stable ID")])
        (when-not (or (and (= kind :cues) (nil? version))
                      (and (integer? version) (pos? version)))
          [(error [:version] "Version must be a positive integer")])
        (when-not (data? definition) [(error [] "Definition must contain portable data only")])
        (when (contains? definition :system)
          (try (system/validate-options (:system definition)) []
               (catch clojure.lang.ExceptionInfo e [(error [:system] (.getMessage e))])))
        (when (and (contains? definition :duration-ms)
                   (not (and (finite-number? duration-ms) (pos? duration-ms))))
          [(error [:duration-ms] "Duration must be positive finite milliseconds")])
        (when (contains? definition :duration)
          (let [errors (time/validate-duration (:duration definition))]
            (if (seq errors) (map #(update % :path (partial into [:duration])) errors)
                (when (zero? (val (first (:duration definition))))
                  [(error [:duration] "Duration must be positive")]))))
        (when (and (contains? definition :targets)
                   (not (and (vector? targets) (every? identifier? targets))))
          [(error [:targets] "Targets must be a vector of fixture/head IDs")])
        (case kind
          :effects
          (concat
           (when-not (or (nil? params) (map? params)) [(error [:params] "Parameters must be a map")])
           (when (map? params)
             (mapcat (fn [[k schema]]
                       (concat (when-not (keyword? k) [(error [:params k] "Parameter IDs must be keywords")])
                               (validate-parameter schema [:params k]))) params))
           (when (contains? definition :layer) (validate-layer (:layer definition)))
           (if (and (or (true? (:show-control? definition)) (map? (:modulation definition))
                        (and (vector? (get-in definition [:layer :transforms]))
                             (seq (get-in definition [:layer :transforms]))))
                    (or (nil? (:outputs definition))
                        (= {} (:outputs definition))
                        (= [] (:outputs definition))))
             [] (validate-outputs (:outputs definition)))
           (when (and (contains? definition :modulation) (not (map? (:modulation definition))))
             [(error [:modulation] "Modulation must be a portable configuration map")]))
          :cues
          (concat
           (when-not (identifier? (:effect-id definition)) [(error [:effect-id] "Cue needs an effect ID")])
           (when-not (or (nil? params) (map? params)) [(error [:params] "Cue parameter values must be a map")])
           (when (and (contains? definition :saved-params) (not (map? (:saved-params definition))))
             [(error [:saved-params] "Saved cue parameter values must be a map")])
           (when (and (some? priority) (not (finite-number? priority))) [(error [:priority] "Priority must be finite")]))
          :fixtures
          (let [channels (:channels definition)]
            (concat
             (photometry/errors definition)
             (when-not (map? channels) [(error [:channels] "Fixture channels must be an attribute-to-offset map")])
             (when (map? channels)
               (mapcat (fn [[attribute offset]]
                         (let [offsets (cond (vector? offset) offset
                                             (map? offset) (remove nil? [(:offset offset) (:fine-offset offset)])
                                             :else [offset])
                               inversion (:inverted-from (when (map? offset) offset))]
                           (when-not (and (keyword? attribute) (<= 1 (count offsets) 2)
                                          (every? #(and (integer? %) (pos? %)) offsets)
                                          (= (count offsets) (count (distinct offsets)))
                                          (or (not (map? offset))
                                              (and (contains? offset :offset)
                                                   (shutter/valid-channel-options? attribute offset)
                                                   (or (nil? inversion) (and (finite-number? inversion) (<= 0 inversion 255))))))
                             [(error [:channels attribute] "Channel needs positive distinct coarse/fine offsets and optional finite inversion")])) ) channels))))))))))

(defn new-registry [] {:effects {} :cues {} :fixtures {} :versions {} :revision 0})

(defn publish
  "Publish validated immutable data. An identical current version is idempotent;
  changed content needs a larger version. Cue references and overrides validate
  against the cue's selected effect version. Prior versions remain under :versions."
  [registry kind definition]
  (let [old (get-in registry [kind (:id definition)])
        historical-versions (for [[[k id version] _] (:versions registry)
                                  :when (and (= kind k) (= (:id definition) id))] version)
        latest-version (apply max (or (:version old) 0) historical-versions)
        definition (if (and (= kind :cues) (map? definition) (not (contains? definition :version)))
                     (assoc definition :version (inc latest-version)) definition)
        errors (validate-definition kind definition)
        effect (when (= kind :cues) (cue-effect registry definition))
        errors (cond-> errors
                 (and (= kind :cues) (nil? effect))
                 (conj (error [:effect-id] "Referenced effect version is not registered"))
                 (and (= kind :cues) effect)
                 (into (validate-cue-params effect definition)))]
    (when (seq errors) (throw (ex-info "Invalid definition" {:kind kind :errors errors})))
    (cond
      (= old definition) registry
      (<= (:version definition) latest-version)
      (throw (ex-info "Definition version must increase" {:errors [(error [:version] "Publish a newer version")]}))
      :else (-> registry
                (assoc-in [kind (:id definition)] definition)
                (assoc-in [:versions [kind (:id definition) (:version definition)]] definition)
                (update :revision (fnil inc 0))))))

(defn generated-page
  "Produce draft cues/placements without mutating manual layouts. Deterministic
  ID ordering; page is zero-based, rows/columns default to 8."
  ([registry] (generated-page registry {}))
  ([registry {:keys [page columns rows] :or {page 0 columns 8 rows 8}}]
   (when-not (and (integer? page) (not (neg? page))
                  (integer? columns) (pos? columns) (integer? rows) (pos? rows))
     (throw (ex-info "Invalid page dimensions" {:page page :columns columns :rows rows})))
   (let [effects (->> (:effects registry) vals (sort-by (comp str :id))
                      (drop (* page columns rows)) (take (* columns rows)))]
     {:page page :columns columns :rows rows
      :cues (mapv (fn [effect]
                    {:id (str "generated:" (:id effect)) :version 1
                     :effect-id (:id effect) :params {} :priority 0}) effects)
      :placements (mapv (fn [i effect]
                          {:cue-id (str "generated:" (:id effect))
                           :position [(mod i columns) (quot i columns)]})
                        (range) effects)})))

(defn- page-coordinate? [position]
  (and (vector? position) (= 2 (count position))
       (every? #(and (integer? %) (not (neg? %))) position)))

(defn reconcile-page
  "Build a pure draft that fills empty cells from registered effects while
  preserving every existing cell, cue identity, and customized cue definition.

  Existing page is the control model's {:id ... :cells {[x y] cue-id}}.
  Options: :columns/:rows (default8), optional :effect-ids selection, and
  :preferred-cells {effect-id [x y]}. Existing placements win. A requested
  occupied cell or conflicting generated cue ID is reported, never overwritten.
  Effects without room appear in :overflow. No existing cue is removed.

  Returns {:registry <draft registry with new cues> :page <draft page>
  :cues <new cue definitions only> :placements <new placements only>
  :collisions [...] :overflow [effect-ids]}. Apply the registry and page together.
  Reconciliation with those returned values is idempotent. Page :effect-cues
  remembers identities even when a user moves a generated cue."
  ([registry page] (reconcile-page registry page {}))
  ([registry page options]
   (let [columns (:columns options (:columns page 8))
         rows (:rows options (:rows page 8))
         cells (:cells page {})
         preferred (:preferred-cells options {})
         effect-ids (if (contains? options :effect-ids) (:effect-ids options) (keys (:effects registry)))]
     (when-not (and (map? page) (identifier? (:id page)) (map? cells)
                    (every? page-coordinate? (keys cells))
                    (integer? columns) (pos? columns) (integer? rows) (pos? rows)
                    (map? preferred) (every? page-coordinate? (vals preferred))
                    (coll? effect-ids) (every? identifier? effect-ids))
       (throw (ex-info "Invalid registry page dimensions, selection or coordinates" {})))
     (let [inside? (fn [[x y]] (and (< x columns) (< y rows)))
           positions (vec (for [y (range rows) x (range columns)] [x y]))
           initial-collisions
           (vec (mapcat (fn [[position cue-id]]
                          (concat
                           (when-not (get-in registry [:cues cue-id])
                             [{:kind :missing-cue :position position :cue-id cue-id}])
                           (when-not (inside? position)
                             [{:kind :outside-page :position position :cue-id cue-id}])))
                        (sort-by (comp pr-str key) cells)))]
       (reduce
        (fn [{:keys [registry page] :as result} effect-id]
          (let [effect (get-in registry [:effects effect-id])
                placed (first
                        (sort-by (fn [[[x y] cue-id]] [y x (pr-str cue-id)])
                                 (filter (fn [[_ cue-id]]
                                           (= effect-id (get-in registry [:cues cue-id :effect-id])))
                                         (:cells page))))
                remembered (get-in page [:effect-cues effect-id])
                cue-id (or (second placed) remembered (str "generated:" (pr-str effect-id)))
                existing (get-in registry [:cues cue-id])
                requested (get preferred effect-id)
                position (or requested (first (remove #(contains? (:cells page) %) positions)))]
            (cond
              (nil? effect)
              (update result :collisions conj {:kind :missing-effect :effect-id effect-id})

              placed
              (assoc-in result [:page :effect-cues effect-id] (second placed))

              (and existing (not= effect-id (:effect-id existing)))
              (update result :collisions conj {:kind :cue-id :effect-id effect-id :cue-id cue-id
                                              :existing-effect-id (:effect-id existing)})

              (and remembered (nil? existing))
              (update result :collisions conj {:kind :missing-remembered-cue :effect-id effect-id :cue-id cue-id})

              (and requested (not (inside? requested)))
              (update result :collisions conj {:kind :outside-page :effect-id effect-id :position requested})

              (and requested (contains? (:cells page) requested))
              (update result :collisions conj {:kind :cell :effect-id effect-id :position requested
                                              :cue-id cue-id :existing-cue-id (get-in page [:cells requested])})

              (nil? position)
              (update result :overflow conj effect-id)

              :else
              (let [cue (or existing {:id cue-id :effect-id effect-id
                                      :params {} :priority 0 :generated-effect-id effect-id})
                    registry (if existing registry (publish registry :cues cue))
                    cue (get-in registry [:cues cue-id])]
                (cond-> (-> result
                            (assoc :registry registry)
                            (assoc-in [:page :cells position] cue-id)
                            (assoc-in [:page :effect-cues effect-id] cue-id)
                            (update :placements conj {:effect-id effect-id :cue-id cue-id :position position}))
                  (nil? existing) (update :cues conj cue))))))
        {:registry registry :page (assoc page :cells cells :columns columns :rows rows)
         :cues [] :placements [] :collisions initial-collisions :overflow []}
        (sort-by pr-str (distinct effect-ids)))))))
