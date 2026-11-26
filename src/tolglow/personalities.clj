(ns tolglow.personalities
  "Track-feel personalities: versioned, validated data describing how the
  lighting should behave for the music. A personality bundles an ordered
  palette, concurrency limits, weighted geometry preferences and a motion
  character. The show model keeps authored definitions and the active
  selection; nothing here is a closure, a clock or an IO resource."
  (:require [clojure.string :as str]
            [tolglow.automation.random :as random]
            [tolglow.personalities.forms :as forms]
            [tolglow.personalities.icon :as icon]
            [tolglow.registry :as registry])
  (:import [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def schema-version 1)

(def state-version 1)

(def package-version 1)

(def state-key "Key of the saved state under the model's :extensions." ::state)

(def blend-id "ID of a personality synthesised from a weighted mix." ::blend)

(def ^:private personality-keys
  #{:id :version :schema :name :description :tags :palette :concurrency :geometry :motion :blend :extensions})

(def ^:private motion-keys [:speed :smoothness :jitter :symmetry])

(defn- error [path message] {:path path :message message})

(defn- unit? [x] (and (registry/finite-number? x) (<= 0 x 1)))

(defn- weight? [x] (and (registry/finite-number? x) (<= 0 x 1000)))

(defn- identifier? [x]
  (or (keyword? x) (and (string? x) (not (str/blank? x)) (<= (count x) 80))))

(defn- text? [x limit]
  (and (string? x) (not (str/blank? x)) (<= (count x) limit) (not (re-find #"\p{Cntrl}" x))))

(defn- color? [x] (registry/valid-value? {:type :color} x))

(defn- wire-id [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn- entry-errors [entry path]
  (let [paint (cond (vector? entry) :color
                    (and (map? entry) (contains? entry :gradient)) :gradient
                    (and (map? entry) (contains? entry :split)) :split)
        angle (:angle-deg entry 0)]
    (case paint
      :color (when-not (color? entry) [(error path "A palette colour needs three components within [0,1]")])
      (:gradient :split)
      (concat
       (when-not (every? #{paint :angle-deg} (keys entry))
         [(error path "A palette entry takes one of :gradient or :split, and an optional :angle-deg")])
       (when-not (and (registry/finite-number? angle) (<= 0 angle) (< angle 360))
         [(error (conj path :angle-deg) "A palette angle must be within [0,360) degrees")])
       (if (= :gradient paint)
         (when-not (and (registry/valid-value? {:type :gradient} (:gradient entry)) (<= 2 (count (:gradient entry)) 8))
           [(error (conj path :gradient) "A gradient needs 2 to 8 stops with increasing positions and linear RGB colours")])
         (when-not (and (vector? (:split entry)) (<= 2 (count (:split entry)) 4) (every? color? (:split entry)))
           [(error (conj path :split) "A split needs 2 to 4 colours")])))
      [(error path "A palette entry is a colour, a :gradient or a :split")])))

(defn- weight-errors [weights path known? subject]
  (cond
    (not (and (map? weights) (<= (count weights) 16)))
    [(error path (str subject " weights must be a map with at most 16 entries"))]
    :else
    (keep (fn [[key weight]]
            (cond (not (known? key)) (error (conj path key) (str "Unknown " subject))
                  (not (weight? weight)) (error (conj path key) "A weight must be within [0,1000]")))
          weights)))

(defn- concurrency-errors [{:keys [fixture-fraction max-fixtures max-groups fixture-mix] :as limits}]
  (if-not (and (map? limits) (every? #{:fixture-fraction :max-fixtures :max-groups :fixture-mix} (keys limits)))
    [(error [:concurrency] "Concurrency takes :fixture-fraction, :max-groups, and optional :max-fixtures and :fixture-mix")]
    (concat
     (when-not (and (unit? fixture-fraction) (pos? fixture-fraction))
       [(error [:concurrency :fixture-fraction] "The active fixture share must be within (0,1]")])
     (when-not (and (integer? max-groups) (<= 1 max-groups 64))
       [(error [:concurrency :max-groups] "Active groups must be an integer from 1 to 64")])
     (when (and (contains? limits :max-fixtures) (not (and (integer? max-fixtures) (<= 1 max-fixtures 4096))))
       [(error [:concurrency :max-fixtures] "Active fixtures must be an integer from 1 to 4096")])
     (when (contains? limits :fixture-mix)
       (weight-errors fixture-mix [:concurrency :fixture-mix] identifier? "fixture type")))))

(defn- blend-errors [parts]
  (when-not (and (vector? parts) (<= 2 (count parts) 4)
                 (every? (fn [{:keys [id version weight] :as part}]
                           (and (map? part) (= #{:id :version :weight} (set (keys part))) (identifier? id)
                                (pos-int? version) (unit? weight) (pos? weight)))
                         parts))
    [(error [:blend] "A blend records 2 to 4 parts with an ID, a version and a positive weight")]))

(defn errors
  "Return [] or path/message errors for one personality."
  [personality]
  (if-not (and (map? personality) (not (record? personality)) (registry/data? personality))
    [(error [] "A personality is a map of portable data")]
    (let [{:keys [id version schema name description tags palette concurrency geometry motion]} personality
          forms (:forms geometry)]
      (vec
       (concat
        (when-not (every? personality-keys (keys personality))
          [(error [] "Unknown personality fields belong in :extensions")])
        (when-not (identifier? id) [(error [:id] "A personality needs a stable ID")])
        (when-not (pos-int? version) [(error [:version] "A personality version must be a positive integer")])
        (when-not (= schema-version schema)
          [(error [:schema] (str "This build reads personality schema " schema-version))])
        (when-not (text? name 80) [(error [:name] "A personality needs a name of at most 80 characters")])
        (when (and (contains? personality :description) (not (text? description 400)))
          [(error [:description] "A description has at most 400 characters")])
        (when (and (contains? personality :tags)
                   (not (and (vector? tags) (<= (count tags) 16) (every? keyword? tags))))
          [(error [:tags] "Tags are a vector of at most 16 keywords")])
        (when (and (contains? personality :extensions) (not (map? (:extensions personality))))
          [(error [:extensions] "Extensions must be a map")])
        (if-not (and (vector? palette) (<= 1 (count palette) 8))
          [(error [:palette] "A palette holds 1 to 8 ordered entries")]
          (mapcat (fn [index entry] (entry-errors entry [:palette index])) (range) palette))
        (concurrency-errors concurrency)
        (if-not (and (map? geometry) (= #{:forms} (set (keys geometry))))
          [(error [:geometry] "Geometry takes a :forms map of weights")]
          (concat (weight-errors forms [:geometry :forms] forms/form? "form")
                  (when-not (and (map? forms) (some #(and (weight? %) (pos? %)) (vals forms)))
                    [(error [:geometry :forms] "At least one form needs a positive weight")])))
        (if-not (and (map? motion) (= (set motion-keys) (set (keys motion))))
          [(error [:motion] "Motion takes :speed, :smoothness, :jitter and :symmetry")]
          (for [key motion-keys :when (not (unit? (get motion key)))]
            (error [:motion key] "Motion character must be within [0,1]")))
        (when (contains? personality :blend) (blend-errors (:blend personality))))))))

(defn validate!
  "Return the personality unchanged, or throw with its :errors."
  [personality]
  (let [problems (errors personality)]
    (when (seq problems) (throw (ex-info "Invalid personality" {:errors problems})))
    personality))

(def builtins
  "The personalities every show starts with, in display order."
  (mapv
   validate!
   [{:id :squares :version 1 :schema 1 :name "Squares and rectangles"
     :description "Hard edges on a grid. Blocks switch rather than fade."
     :tags [:angular :graphic]
     :palette [[1.0 0.2 0.15] {:split [[1.0 0.82 0.1] [0.1 0.3 0.9]]} [0.96 0.96 0.92]]
     :concurrency {:fixture-fraction 0.5 :max-groups 4 :fixture-mix {:pixel 3.0 :wash 2.0 :moving-head 1.0}}
     :geometry {:forms {:square 5.0 :rectangle 4.0 :bar 2.0}}
     :motion {:speed 0.55 :smoothness 0.15 :jitter 0.0 :symmetry 0.85}}
    {:id :squircle :version 1 :schema 1 :name "Apple-hardware-level corner obsession"
     :description "Continuous corners, brushed gradients and nothing out of place."
     :tags [:smooth :precise]
     :palette [{:gradient [{:position 0.0 :color [0.93 0.94 0.96]} {:position 1.0 :color [0.55 0.58 0.63]}]}
               {:gradient [{:position 0.0 :color [0.98 0.78 0.67]} {:position 1.0 :color [0.86 0.55 0.5]}] :angle-deg 45.0}
               [0.16 0.5 0.98]]
     :concurrency {:fixture-fraction 0.35 :max-groups 2 :fixture-mix {:wash 3.0 :pixel 1.0}}
     :geometry {:forms {:squircle 8.0 :pill 3.0 :circle 1.0}}
     :motion {:speed 0.3 :smoothness 0.95 :jitter 0.0 :symmetry 0.95}}
    {:id :spirals :version 1 :schema 1 :name "Spiralling in general"
     :description "Everything winds inwards or back out, never in a straight line."
     :tags [:flowing :hypnotic]
     :palette [{:gradient [{:position 0.0 :color [0.2 0.9 0.8]} {:position 0.5 :color [0.3 0.4 1.0]}
                           {:position 1.0 :color [0.8 0.2 0.9]}] :angle-deg 30.0}
               [0.25 0.95 0.75] [0.6 0.35 1.0]]
     :concurrency {:fixture-fraction 0.6 :max-groups 3 :fixture-mix {:moving-head 4.0 :wash 1.0}}
     :geometry {:forms {:spiral 7.0 :ring 2.0 :circle 2.0}}
     :motion {:speed 0.6 :smoothness 0.85 :jitter 0.05 :symmetry 0.5}}
    {:id :zigzag-shaky :version 1 :schema 1 :name "Zig to the zag but shaky"
     :description "Sharp reversals that never quite land where they aimed."
     :tags [:angular :nervous]
     :palette [[1.0 0.9 0.1] [1.0 0.35 0.05] {:split [[0.1 0.95 1.0] [1.0 1.0 1.0]]}]
     :concurrency {:fixture-fraction 0.45 :max-groups 3 :fixture-mix {:moving-head 3.0 :pixel 2.0}}
     :geometry {:forms {:zigzag 7.0 :sawtooth 3.0 :triangle 3.0}}
     :motion {:speed 0.8 :smoothness 0.1 :jitter 0.6 :symmetry 0.3}}
    {:id :random-pink :version 1 :schema 1 :name "So random lol WITH PINK"
     :description "Any shape, anywhere, at any time. Also pink."
     :tags [:chaotic :playful]
     :palette [[1.0 0.2 0.65] {:split [[1.0 0.45 0.8] [0.2 1.0 0.9] [1.0 0.95 0.3]] :angle-deg 60.0}
               {:gradient [{:position 0.0 :color [1.0 0.5 0.85]} {:position 1.0 :color [0.55 0.2 1.0]}]}
               [0.3 1.0 0.4]]
     :concurrency {:fixture-fraction 0.8 :max-groups 6}
     :geometry {:forms (zipmap forms/order (repeat 1.0))}
     :motion {:speed 0.7 :smoothness 0.4 :jitter 0.9 :symmetry 0.1}}
    {:id :still-water :version 1 :schema 1 :name "Still water"
     :description "One or two sources, barely moving. Leaves room for the dark."
     :tags [:calm :minimal]
     :palette [[0.55 0.75 0.85] [0.9 0.92 0.9]]
     :concurrency {:fixture-fraction 0.15 :max-fixtures 4 :max-groups 1 :fixture-mix {:wash 1.0}}
     :geometry {:forms {:circle 5.0 :bar 2.0 :pill 1.0}}
     :motion {:speed 0.1 :smoothness 1.0 :jitter 0.0 :symmetry 1.0}}
    {:id :slow-sunset :version 1 :schema 1 :name "Slow sunset"
     :description "Long warm fades across the whole rig, settling towards the horizon."
     :tags [:warm :calm]
     :palette [{:gradient [{:position 0.0 :color [1.0 0.8 0.3]} {:position 0.55 :color [1.0 0.4 0.2]}
                           {:position 1.0 :color [0.5 0.1 0.4]}]}
               [1.0 0.55 0.2] [0.35 0.1 0.45]]
     :concurrency {:fixture-fraction 0.4 :max-groups 2 :fixture-mix {:wash 3.0 :pixel 2.0}}
     :geometry {:forms {:circle 4.0 :pill 3.0 :rectangle 2.0}}
     :motion {:speed 0.2 :smoothness 0.9 :jitter 0.02 :symmetry 0.75}}]))

(def ^:private builtin-index (into {} (map (juxt :id identity)) builtins))

(defn- rounded [x] (/ (Math/round (* (double x) 1.0e6)) 1.0e6))

(defn- shares
  "Positive weights scaled to sum to one, in the given key order."
  [weights]
  (let [positive (filter (fn [[_ weight]] (pos? weight)) weights)
        total (reduce + 0.0 (map second positive))]
    (into {} (map (fn [[key weight]] [key (/ weight total)])) positive)))

(defn- mix-shares [parts field]
  (reduce (fn [mixed {:keys [personality weight]}]
            (reduce-kv (fn [mixed key share] (update mixed key (fnil + 0.0) (* weight share)))
                       mixed (shares (get-in personality field))))
          {} parts))

(defn- interleaved-palette
  "Give each part palette slots in proportion to its weight, heaviest first."
  [parts]
  (let [slots (min 8 (reduce max (map (comp count :palette :personality) parts)))]
    (loop [taken (vec (repeat (count parts) 0)) palette []]
      (if (= slots (count palette))
        palette
        (let [behind (fn [index] (- (* (:weight (parts index)) (inc (count palette))) (taken index)))
              ;; max-key keeps the last of equal keys; walk backwards so the first part wins.
              index (apply max-key behind (reverse (range (count parts))))
              entries (get-in parts [index :personality :palette])]
          (recur (update taken index inc) (conj palette (nth entries (mod (taken index) (count entries))))))))))

(defn- weighted [parts field]
  (reduce + 0.0 (map (fn [{:keys [personality weight]}] (* weight (get-in personality field))) parts)))

(defn blend
  "Synthesise one personality from [{:personality p :weight w}]. Weights are
  scaled to sum to one. A single part returns its personality unchanged."
  [parts]
  (when-not (and (vector? parts) (<= 1 (count parts) 4)
                 (every? #(and (registry/finite-number? (:weight %)) (pos? (:weight %))) parts)
                 (apply distinct? (map (comp wire-id :id :personality) parts)))
    (throw (ex-info "Blend 1 to 4 different personalities with positive weights" {})))
  (doseq [{:keys [personality]} parts] (validate! personality))
  (if (= 1 (count parts))
    (:personality (first parts))
    (let [total (reduce + 0.0 (map :weight parts))
          parts (mapv #(update % :weight (fn [weight] (rounded (/ weight total)))) parts)
          positive (fn [weights] (into {} (keep (fn [[key weight]]
                                                  (let [weight (rounded weight)] (when (pos? weight) [key weight]))))
                                       weights))
          fixture-mix (positive (mix-shares parts [:concurrency :fixture-mix]))
          label (str/join " + " (map (fn [{:keys [personality weight]}]
                                       (str (Math/round (* 100.0 weight)) "% " (:name personality))) parts))]
      (validate!
       (cond-> {:id blend-id :version 1 :schema schema-version
                :name (if (> (count label) 80) (str (subs label 0 79) "…") label)
                :blend (mapv (fn [{:keys [personality weight]}]
                               (assoc (select-keys personality [:id :version]) :weight weight)) parts)
                :palette (interleaved-palette parts)
                :concurrency (cond-> {:fixture-fraction (rounded (weighted parts [:concurrency :fixture-fraction]))
                                      :max-groups (max 1 (Math/round (double (weighted parts [:concurrency :max-groups]))))}
                               (every? #(get-in % [:personality :concurrency :max-fixtures]) parts)
                               (assoc :max-fixtures (max 1 (Math/round (double (weighted parts [:concurrency :max-fixtures])))))
                               (seq fixture-mix) (assoc :fixture-mix fixture-mix))
                :geometry {:forms (positive (mix-shares parts [:geometry :forms]))}
                :motion (into {} (map (fn [key] [key (rounded (weighted parts [:motion key]))])) motion-keys)}
         (some (comp seq :tags :personality) parts)
         (assoc :tags (vec (take 16 (distinct (mapcat (comp :tags :personality) parts))))))))))

(defn fixture-limit
  "How many of this many eligible fixtures the personality wants lit at once."
  [personality eligible]
  (let [{:keys [fixture-fraction max-fixtures]} (:concurrency personality)
        share (long (Math/ceil (* fixture-fraction eligible)))]
    (max (min 1 eligible) (cond-> (min eligible share) max-fixtures (min max-fixtures)))))

(defn vocabulary
  "The personality's weights over the inputs one family can take, scaled to
  sum to one, in catalogue order. Empty when it weights no form of the family."
  [personality family]
  (let [weights (get-in personality [:geometry :forms])
        entries (for [form forms/order
                      :let [weight (get weights form 0) input (forms/input form family)]
                      :when (and input (pos? weight))]
                  {:form form :weight (double weight) :input input})
        total (reduce + 0.0 (map :weight entries))]
    (mapv #(update % :weight / total) entries)))

(defn- unit-value
  "A value in [0,1) fixed by a printable coordinate of IDs and integers."
  [coordinate]
  (when-not (and (vector? coordinate) (every? #(or (keyword? %) (string? %) (integer? %)) coordinate))
    (throw (ex-info "A selection coordinate holds IDs and integers" {:coordinate coordinate})))
  (let [bytes (.getBytes (pr-str coordinate) StandardCharsets/UTF_8)
        seed (.getInt (ByteBuffer/wrap (.digest (MessageDigest/getInstance "SHA-256") bytes)))]
    (second (random/next-random seed))))

(defn choose-form
  "Pick {:form :input} for one family by seeded weighted selection, or nil when
  the personality weights no form that family can take. The coordinate alone
  fixes the draw, so nearby weights give the same or a neighbouring choice."
  [personality family coordinate]
  (let [entries (vocabulary personality family)]
    (when (seq entries)
      (let [draw (unit-value coordinate)]
        (loop [[entry & others] entries reached 0.0]
          (let [reached (+ reached (:weight entry))]
            (if (or (< draw reached) (empty? others))
              (select-keys entry [:form :input])
              (recur others reached))))))))

(defn input-declaration
  "Validate an effect's request to read the active personality for one family.
  options may carry an integer :seed. Returns the portable declaration."
  [family options]
  (let [seed (:seed options 0)]
    (when-not (and (some #{family} forms/families) (map? options) (every? #{:seed} (keys options)) (integer? seed))
      (throw (ex-info "A personality input takes a known family and an optional integer :seed" {:options options})))
    {:version 1 :family family :seed seed}))

(defn input-choice
  "Resolve a declared input against the active selection. identity names the
  reader, such as its cue ID. Returns {:form :input} or nil when nothing is
  active or the personality weights no form this family can take."
  [active {:keys [family seed]} identity]
  (when-let [personality (:personality active)]
    (choose-form personality family [seed (:seed active) (wire-id identity)])))

(defn state [model] (get-in model [:extensions state-key]))

(defn library
  "Every personality this show can select, by ID. A show definition replaces a
  built-in with the same ID."
  [model]
  (merge builtin-index (:definitions (state model))))

(defn- ordered-library [model]
  (let [definitions (:definitions (state model))]
    (concat (map #(get definitions (:id %) %) builtins)
            (sort-by (comp wire-id :id) (remove (comp builtin-index :id) (vals definitions))))))

(defn active
  "The typed input effects read: {:personality :mix :seed}, or nil."
  [model]
  (:active (state model)))

(defn- part-errors [{:keys [id version weight] :as part}]
  (when-not (and (map? part) (= #{:id :version :weight} (set (keys part))) (identifier? id)
                 (pos-int? version) (unit? weight) (pos? weight))
    [(error [:active :mix] "A mix part needs an ID, a version and a weight within (0,1]")]))

(defn validate-state!
  "Check the saved state of a model. Returns the model."
  [model]
  (when-let [{:keys [version definitions active] :as saved} (state model)]
    (let [{:keys [mix seed personality]} active
          problems
          (concat
           (when-not (and (map? saved) (every? #{:version :definitions :active} (keys saved))
                          (= state-version version) (map? (or definitions {})) (<= (count definitions) 64))
             [(error [] "Saved personalities need a version and at most 64 definitions")])
           (when (map? definitions)
             (mapcat (fn [[id definition]]
                       (concat (when-not (= id (:id definition))
                                 [(error [:definitions id] "A definition is stored under its own ID")])
                               (when (contains? definition :blend)
                                 [(error [:definitions id :blend] "A blend is selected, never stored as a definition")])
                               (map #(update % :path (partial into [:definitions id])) (errors definition))))
                     definitions))
           (when (contains? saved :active)
             (if-not (and (map? active) (= #{:mix :seed :personality} (set (keys active)))
                          (vector? mix) (<= 1 (count mix) 4) (integer? seed))
               [(error [:active] "The active selection needs a mix, an integer seed and its personality")]
               (concat (mapcat part-errors mix)
                       (map #(update % :path (partial into [:active :personality])) (errors personality))
                       (when-not (if (next mix)
                                   (= mix (:blend personality))
                                   (= (select-keys (first mix) [:id :version]) (select-keys personality [:id :version])))
                         [(error [:active :personality] "The active personality does not match its mix")])))))]
      (when (seq problems)
        (throw (ex-info "Invalid saved personalities" {:path [:extensions state-key] :errors (vec problems)})))))
  model)

(defn- resolve-id
  "Match a wire ID to a library entry whether it arrives as text or a keyword."
  [entries id]
  (or (when (contains? entries id) id)
      (some #(when (= (wire-id id) (wire-id %)) %) (keys entries))
      (throw (ex-info "Unknown personality" {:id id}))))

(defn- publish
  "Add or replace one show definition. Changed content needs a higher version."
  [model personality]
  (validate! personality)
  (let [{:keys [id version]} personality
        entries (library model)
        id (if (contains? entries id) id (or (some #(when (= (wire-id id) (wire-id %)) %) (keys entries)) id))
        personality (assoc personality :id id)
        previous (get entries id)]
    (cond
      (or (= blend-id id) (contains? personality :blend))
      (throw (ex-info "A blend is selected, never stored as a definition" {:id id}))
      (= previous personality) model
      (and previous (<= version (:version previous)))
      (throw (ex-info "A changed personality needs a higher version" {:id id :current-version (:version previous)}))
      (and (nil? previous) (>= (count (:definitions (state model))) 64))
      (throw (ex-info "A show keeps at most 64 personalities" {}))
      :else (assoc-in model [:extensions state-key :definitions id] personality))))

(defn- select [model mix seed]
  (when-not (and (vector? mix) (<= 1 (count mix) 4)
                 (every? #(and (map? %) (registry/finite-number? (:weight %)) (pos? (:weight %))) mix)
                 (integer? seed))
    (throw (ex-info "Select 1 to 4 personalities with positive weights and an integer seed" {})))
  (let [entries (library model)
        parts (mapv (fn [{:keys [id weight]}] {:personality (get entries (resolve-id entries id)) :weight weight}) mix)
        personality (blend parts)]
    (assoc-in model [:extensions state-key :active]
              {:mix (or (:blend personality) [(assoc (select-keys personality [:id :version]) :weight 1.0)])
               :seed seed :personality personality})))

(defn export-package
  "Build a sharing package from an explicit selection of IDs. It carries the
  chosen definitions and their schema versions only: never the active
  selection, the rest of the library or any other show data. A definition's
  private :extensions stay behind unless options asks for :extensions? true."
  ([model ids] (export-package model ids {}))
  ([model ids {:keys [extensions?]}]
   (when-not (and (vector? ids) (seq ids) (apply distinct? ids))
     (throw (ex-info "Name the personalities to share" {})))
   (let [entries (library model)
         chosen (mapv #(get entries (resolve-id entries %)) ids)]
     {:type :tolglow/personalities :version package-version :schema schema-version
      :personalities (mapv #(cond-> % (not extensions?) (dissoc :extensions)) chosen)
      :provenance (mapv (fn [{:keys [id version]}]
                          {:id id :version version :origin (if (= (get builtin-index id) (get entries id)) :builtin :show)})
                        chosen)})))

(defn- package-personalities [{:keys [type version schema personalities provenance] :as package}]
  (when-not (and (map? package) (= #{:type :version :schema :personalities :provenance} (set (keys package)))
                 (= :tolglow/personalities type) (= package-version version) (= schema-version schema)
                 (vector? personalities) (<= 1 (count personalities) 64) (vector? provenance))
    (throw (ex-info "Expected a version 1 personality package" {})))
  personalities)

(defn- tidy
  "Keep the saved state only while it holds a definition or a selection."
  [model]
  (let [saved (state model)
        saved (cond-> saved (empty? (:definitions saved)) (dissoc :definitions))
        saved (when (or (:active saved) (:definitions saved)) (assoc saved :version state-version))]
    (cond saved (assoc-in model [:extensions state-key] saved)
          (contains? (:extensions model) state-key) (update model :extensions dissoc state-key)
          :else model)))

(defn- forget [model id]
  (let [definitions (:definitions (state model))
        known (some #(when (= (wire-id id) (wire-id %)) %) (keys definitions))]
    (when-not known
      (throw (ex-info (if (some #(= (wire-id id) (wire-id %)) (keys builtin-index))
                        "A built-in personality stays in the library"
                        "Unknown personality") {:id id})))
    (update-in model [:extensions state-key :definitions] dissoc known)))

(defn apply-operation
  "Apply one owner operation and return {:model :ack}. Operations:
  {:op :select :id id}, {:op :blend :mix [{:id id :weight w}]}, {:op :clear},
  {:op :put :personality p}, {:op :remove :id id} and {:op :import :package p}.
  :select and :blend take an optional integer :seed and otherwise keep the
  current one. A rejected operation throws and leaves the model as it was."
  [model {:keys [op id mix seed personality package] :as operation}]
  (let [op (if (string? op) (keyword op) op)
        seed (if (contains? operation :seed) seed (get-in (state model) [:active :seed] 0))
        updated (tidy (case op
                        :select (select model [{:id id :weight 1.0}] seed)
                        :blend (select model mix seed)
                        :clear (update-in model [:extensions state-key] dissoc :active)
                        :put (publish model personality)
                        :remove (forget model id)
                        :import (reduce publish model (package-personalities package))
                        (throw (ex-info "Unknown personality operation" {:op op}))))
        updated (if (= (state model) (state updated)) model (validate-state! (update updated :revision inc)))]
    {:model updated
     :ack (cond-> {:status :accepted :revision (:revision updated)
                   :active (some-> (active updated) (select-keys [:mix :seed]))}
            (= :put op) (assoc :id (:id personality) :version (:version personality)))}))

(defn- summary [personality]
  (assoc (select-keys personality [:id :version :name :description :tags :palette :concurrency :motion :blend])
         :forms (vec (sort-by (comp - :weight)
                              (map (fn [[form share]] {:form form :weight (rounded share)})
                                   (shares (map (fn [form] [form (get-in personality [:geometry :forms form] 0)])
                                                forms/order)))))
         :icon (icon/svg personality)))

(def ^:private builtin-summaries
  (delay (into {} (map (fn [personality] [personality (assoc (summary personality) :builtin? true)])) builtins)))

(defn projection
  "What Studio, the REPL and device displays read: the selectable library in
  display order and the active personality, each with its icon as SVG text."
  [model]
  (let [{:keys [mix seed personality]} (active model)
        entries (library model)]
    {:version state-version
     :library (mapv #(or (get @builtin-summaries %) (assoc (summary %) :builtin? false)) (ordered-library model))
     :active (when personality
               (assoc (or (get @builtin-summaries personality) (summary personality))
                      :mix mix :seed seed
                      :current? (every? (fn [{:keys [id version]}] (= version (:version (get entries id)))) mix)))}))

(defn without-icons
  "The same projection for a reader that cannot use pictures, such as a text agent."
  [view]
  (-> view
      (update :library (partial mapv #(dissoc % :icon)))
      (update :active #(some-> % (dissoc :icon)))))

(defn push-view
  "The active personality for a device display page: its name and icon."
  [model]
  (when-let [{:keys [personality]} (active model)]
    {:id (:id personality) :name (:name personality)
     :icon (icon/data-uri (icon/svg personality))}))
