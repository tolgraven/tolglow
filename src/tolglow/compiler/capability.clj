(ns tolglow.compiler.capability
  "Which patched heads implement each attribute an effect writes (#688).

  An output without explicit targets writes only to the heads that implement
  its attribute, so one hazer or white-only par never faults a colour cue.
  Explicitly naming a head that lacks a written attribute is refused at launch.
  Both compilers apply the same per-output filter, and an accepted launch
  carries a :target-resolution report naming the heads it skipped."
  (:require [clojure.string :as str]
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
  "The definition one launched instance renders: launch targets narrow the
  authored definition targets, as Afterglow's engine does each frame."
  [{:keys [definition targets] :as instance}]
  (if-not (contains? instance :targets)
    definition
    (assoc definition :targets (if-let [authored (:targets definition)]
                                 (filterv (set authored) targets)
                                 targets))))

(defn- scope [definition fixtures]
  (let [selected (when (contains? definition :targets) (set (:targets definition)))]
    (filterv #(or (nil? selected) (contains? selected (:id %))) fixtures)))

(defn- addressed [heads {:keys [targets]}]
  (let [targets (when (some? targets) (set targets))]
    (filterv #(or (nil? targets) (contains? targets (:id %))) heads)))

(defn- incapable [definition fixtures output?]
  (let [heads (scope definition fixtures)
        entries (filterv output? (registry/output-entries (:outputs definition)))]
    (into []
          (keep (fn [attribute]
                  (let [writes (into #{} (comp (filter #(= attribute (:attribute %)))
                                               (mapcat (partial addressed heads)) (map :id))
                                     entries)
                        skipped (into [] (comp (filter #(contains? writes (:id %)))
                                               (remove #(capable? % attribute)) (map :id))
                                      heads)]
                    (when (seq skipped)
                      {:attribute attribute :heads skipped :requires (requires attribute)}))))
          (distinct (map :attribute entries)))))

(defn skipped
  "Heads an output would address but cannot implement, per written attribute,
  in rig order: [{:attribute :color :heads [:par :hazer] :requires [:red :green
  :blue]}]. Empty when every addressed head implements every attribute."
  [definition fixtures]
  (incapable definition fixtures any?))

(defn- explicit-output? [definition explicit-launch?]
  (fn [output]
    (or (contains? output :targets) (contains? definition :targets) explicit-launch?)))

(defn launch-conflicts
  "Heads named explicitly that a launched instance would write without
  implementing the attribute. Output :targets, authored definition :targets and
  launch fixture IDs are explicit. explicit-launch? is false when the launch
  targets came from a group, volume or :all scope; those are filtered instead."
  [instance explicit-launch? fixtures]
  (incapable (program instance) fixtures
             (explicit-output? (:definition instance) (and (contains? instance :targets) explicit-launch?))))

(defn- explicit-launch? [command]
  (let [{:keys [selector] :as launch} (:launch command)]
    (and (contains? launch :targets) (or (nil? selector) (contains? selector :fixture-ids)))))

(defn- label [id] (if (keyword? id) (name id) (str id)))

(defn- conflict-message [conflicts]
  (str "Explicit targets include heads that cannot take a written attribute: "
       (str/join "; " (map (fn [{:keys [attribute heads requires]}]
                             (str (label attribute) " on " (str/join ", " (map label heads))
                                  " (needs " (str/join ", " (map label requires)) ")"))
                           conflicts))))

(defn admit-launch
  "Check an accepted launch result against the rig. Explicit targets naming a
  head that lacks a written attribute refuse the launch with :incapable-target
  and leave model unchanged. Otherwise, when the capability filter skips heads,
  the instance, its launched event, the acknowledgement and the retry receipt
  carry {:target-resolution {:skipped [...]}}. A launch that skips nothing is
  returned unchanged, and a retried command ID returns its receipt."
  [model command {:keys [ack] :as result}]
  (let [id (:instance-id ack)
        instance (get-in result [:model :instances id])
        retry? (contains? (get-in model [:control :history]) (:command-id command))
        fixtures (get-in result [:model :rig :fixtures])]
    (if (or retry? (not= :accepted (:status ack)) (nil? instance))
      result
      (let [conflicts (launch-conflicts instance (explicit-launch? command) fixtures)
            skips (when (empty? conflicts) (skipped (program instance) fixtures))]
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

          :else result)))))
