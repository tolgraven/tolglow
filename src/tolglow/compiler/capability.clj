(ns tolglow.compiler.capability
  "Which patched heads implement each attribute an effect writes (#688).

  An output without explicit targets writes only to the heads that implement
  its attribute, so one hazer or white-only par never faults a colour cue.
  Explicitly naming a head that lacks a written attribute is refused at launch.
  Both compilers apply the same per-output filter, and an accepted launch
  carries a :target-resolution report naming the heads it skipped."
  (:require [afterglow.runtime.engine :as engine]
            [clojure.string :as str]
            [tolglow.registry :as registry]))

(def ^:private required-channels
  {:color [:red :green :blue] :aim [:pan :tilt] :direction [:pan :tilt]})

(defn requires
  "The channels a head needs before it accepts attribute."
  [attribute]
  (required-channels attribute [attribute]))

(defn capable?
  "True when the renderer accepts attribute on fixture. This mirrors the
  Afterglow patch: every patched head has a virtual dimmer, :color needs red,
  green and blue, :aim and :direction need pan and tilt, and any other
  attribute needs its own channel. A fixture map without :channels is not a
  patched head and is never filtered. Native layer :beat-ratio rows are not
  fixture writes."
  [fixture attribute]
  (let [channels (:channels fixture)]
    (or (not (map? channels)) (= :dimmer attribute) (= :beat-ratio attribute)
        (if-let [required (required-channels attribute)]
          (every? #(contains? channels %) required)
          (contains? channels attribute)))))

(defn program
  "The definition one launched instance renders, narrowed by
  `afterglow.runtime.engine/launch-program`. As in the engine, nil launch
  targets (absent or an explicit nil) leave the definition untargeted."
  [{:keys [definition targets]}]
  (engine/launch-program definition targets))

(defn rig-index
  "Fixture ID -> [rig position, fixture] for `fixtures`: one pass over the rig."
  [fixtures]
  (into {} (map-indexed (fn [i fixture] [(:id fixture) [i fixture]])) fixtures))

(def ^:dynamic *rig-index*
  "A function from a rig's fixtures to its `rig-index`, bound by a runtime
  owner for one cycle so that a burst of launches on one rig indexes it once
  (#798). The show keeps the index with its compiled patch, so each runtime
  has its own and a rig edit replaces it. Unbound, each launch indexes the rig
  it is admitted against."
  nil)

(defn- index-for [fixtures]
  (if *rig-index* (*rig-index* fixtures) (rig-index fixtures)))

(defn- scope
  "The heads `definition` addresses, in rig order. Authored targets are looked
  up in the rig index instead of filtering the whole rig, since a launch
  narrowed to one head should not cost a pass over every fixture."
  [definition fixtures index]
  (if (contains? definition :targets)
    (mapv second (sort-by first (keep #(get @index %) (distinct (:targets definition)))))
    fixtures))

(defn- addressed [heads {:keys [targets]}]
  (let [targets (when (some? targets) (set targets))]
    (filterv #(or (nil? targets) (contains? targets (:id %))) heads)))

(defn- incapable-heads
  "Per written attribute of `entries`, the `heads` an entry addresses that
  cannot implement it. Heads are checked first, so the outputs' addressed
  sets are built only when some head lacks the attribute."
  [entries heads]
  (into []
        (keep (fn [attribute]
                (let [lacking (into [] (remove #(capable? % attribute)) heads)]
                  (when (seq lacking)
                    (let [writes (into #{} (comp (filter #(= attribute (:attribute %)))
                                                 (mapcat (partial addressed lacking)) (map :id))
                                       entries)
                          skipped (into [] (comp (filter #(contains? writes (:id %))) (map :id)) lacking)]
                      (when (seq skipped)
                        {:attribute attribute :heads skipped :requires (requires attribute)}))))))
        (distinct (map :attribute entries))))

(defn- incapable [definition fixtures index output?]
  (incapable-heads (filterv output? (registry/output-entries (:outputs definition)))
                   (scope definition fixtures index)))

(defn skipped
  "Heads an output would address but cannot implement, per written attribute,
  in rig order: [{:attribute :color :heads [:par :hazer] :requires [:red :green
  :blue]}]. Empty when every addressed head implements every attribute."
  ([definition fixtures] (skipped definition fixtures (delay (index-for fixtures))))
  ([definition fixtures index] (incapable definition fixtures index any?)))

(defn- explicit-output? [definition explicit-launch?]
  (fn [output]
    (or (contains? output :targets) (contains? definition :targets) explicit-launch?)))

(defn launch-conflicts
  "Heads named explicitly that a launched instance would write without
  implementing the attribute. Output :targets, authored definition :targets and
  launch fixture IDs are explicit. explicit-launch? is false when the launch
  targets came from a group, volume or :all scope; those are filtered instead."
  ([instance explicit-launch? fixtures] (launch-conflicts instance explicit-launch? fixtures (delay (index-for fixtures))))
  ([instance explicit-launch? fixtures index]
   (incapable (program instance) fixtures index
              (explicit-output? (:definition instance) (and (some? (:targets instance)) explicit-launch?)))))

(defn- explicit-launch? [command]
  (let [{:keys [selector] :as launch} (:launch command)]
    ;; The :all selector is a keyword, not a map, and is a scope like a group.
    (and (contains? launch :targets) (or (nil? selector) (and (map? selector) (contains? selector :fixture-ids))))))

(defn- label [id] (if (keyword? id) (name id) (str id)))

(defn- conflict-message [conflicts]
  (str "Explicit targets include heads that cannot take a written attribute: "
       (str/join "; " (map (fn [{:keys [attribute heads requires]}]
                             (str (label attribute) " on " (str/join ", " (map label heads))
                                  " (needs " (str/join ", " (map label requires)) ")"))
                           conflicts))))

(defn launch-verdict
  "What admit-launch decides for an instance of `definition` launched by
  `command` with launch `targets` (nil when untargeted) on `fixtures`:
  {:conflicts [...] :skips [...]} plus the inputs it read. The verdict
  depends on nothing else, so a show can work it out ahead of a quantised
  launch, off the frame (#918). `index` is a delay of the rig index."
  ([definition targets command fixtures] (launch-verdict definition targets command fixtures (delay (index-for fixtures))))
  ([definition targets command fixtures index]
   ;; The launch's program and heads are worked out once for both checks.
   (let [explicit-launch (explicit-launch? command)
         narrowed (engine/launch-program definition targets)
         heads (scope narrowed fixtures index)
         entries (registry/output-entries (:outputs narrowed))
         explicit? (explicit-output? definition (and (some? targets) explicit-launch))
         conflicts (incapable-heads (filterv explicit? entries) heads)]
     {:definition definition :targets targets :explicit-launch? explicit-launch :fixtures fixtures
      :conflicts conflicts :skips (when (empty? conflicts) (incapable-heads entries heads))})))

(defn verdict-applies?
  "True when a verdict worked out earlier read exactly what admitting
  `instance`, launched by `command` on `fixtures`, reads: the same definition
  and rig (by identity), launch targets and explicitness."
  [verdict instance command fixtures]
  (boolean (and verdict
                (identical? (:definition verdict) (:definition instance))
                (identical? (:fixtures verdict) fixtures)
                (= (:targets verdict) (:targets instance))
                (= (:explicit-launch? verdict) (explicit-launch? command)))))

(defn admit-launch
  "Check an accepted launch result against the rig. Explicit targets naming a
  head that lacks a written attribute refuse the launch with :incapable-target
  and leave model unchanged. Otherwise, when the capability filter skips heads,
  the instance, its launched event, the acknowledgement and the retry receipt
  carry {:target-resolution {:skipped [...]}}. A launch that skips nothing is
  returned unchanged, and a retried command ID returns its receipt.
  A `prepared` launch-verdict is used only when verdict-applies?; otherwise
  the verdict is worked out here."
  ([model command result] (admit-launch model command result nil))
  ([model command {:keys [ack] :as result} prepared]
  (let [id (:instance-id ack)
        instance (get-in result [:model :instances id])
        retry? (contains? (get-in model [:control :history]) (:command-id command))
        fixtures (get-in result [:model :rig :fixtures])]
    (if (or retry? (not= :accepted (:status ack)) (nil? instance))
      result
      (let [{:keys [conflicts skips]}
            (if (verdict-applies? prepared instance command fixtures)
              prepared
              ;; Indexed once per launch, or once per rig in a runtime owner cycle.
              (launch-verdict (:definition instance) (:targets instance) command fixtures (delay (index-for fixtures))))]
        (cond
          (seq conflicts)
          {:model model :events []
           :ack (merge (select-keys model [:show-id :epoch :revision])
                       {:command-id (:command-id command) :status :rejected :code :incapable-target
                        :message (conflict-message conflicts) :details {:conflicts conflicts}})}

          (seq skips)
          (let [report {:skipped skips}
                annotated (assoc instance :target-resolution report)
                launched? #(and (= :instance/launched (:type %)) (= id (get-in % [:instance :id])))]
            (-> result
                (assoc-in [:model :instances id] annotated)
                (update :events (partial mapv #(if (launched? %) (assoc % :instance annotated) %)))
                (assoc-in [:ack :target-resolution] report)
                (cond-> (get-in result [:model :control :history (:command-id command)])
                  (assoc-in [:model :control :history (:command-id command) :ack :target-resolution] report))))

          :else result))))))
