(ns tolglow.lighting-activity
  "Perceptual activity in the committed lighting look. Output is a relative
  performance control, not a measurement of electrical power or a safety limit."
  (:require [afterglow.render.frame :as frame]
            [afterglow.runtime.events :as events]
            [clojure.set :as set]
            [tolglow.fixtures.shutter :as shutter-profile]
            [tolglow.registry :as registry]))

(def ^:private motion-fields #{:pan :tilt :aim :direction})
(def ^:private shutter-fields #{:strobe :shutter})
(def ^:private light-fields (into registry/emitter-attributes [:color :dimmer]))
(def ^:private measured-fields (set/union motion-fields shutter-fields light-fields))
(def ^:private components [:brightness :coverage :flashing :movement :strobe])

(defn- unit ^double [x] (min 1.0 (max 0.0 (double x))))

(defn- channel-value [universes {:keys [coarse fine inverted-from]}]
  (when coarse
    (let [read-byte (fn [[u i]] (bit-and 255 (int (nth (get universes u) i))))
          hi (read-byte coarse)
          raw (+ hi (if fine (/ (read-byte fine) 255.0) 0.0))]
      {:byte hi
       :value (unit (/ (if (and inverted-from (>= raw inverted-from))
                         (- 255.0 (- raw inverted-from)) raw) 255.0))})))

(defn- shutter [fixture universes]
  (let [attr (if (contains? (:compiled-channels fixture) :strobe) :strobe :shutter)
        channel (get-in fixture [:compiled-channels attr])]
    (if-let [{:keys [byte value]} (channel-value universes channel)]
      (shutter-profile/response attr channel byte value)
      [true 0.0])))

(defn- emit-direction [fixture universes]
  (let [{:keys [pan tilt]} (:compiled-channels fixture)]
    (when (or pan tilt)
      (:direction (frame/fixture-pose fixture {:pan (get (channel-value universes pan) :value 0.5)
                                               :tilt (get (channel-value universes tilt) :value 0.5)})))))

(defn- write-field [by-target id field owner opacity blend]
  (if (and (measured-fields field) (pos? opacity))
    (let [old (get-in by-target [id field])
          next (cond
                 (and (= blend :replace) (>= opacity 1.0)) (if owner {owner 1.0} {})
                 (nil? owner) old
                 (= blend :replace) (update (update-vals old #(* % (- 1.0 opacity))) owner (fnil + 0.0) opacity)
                 :else (update old owner (fnil max 0.0) opacity))]
      (assoc-in by-target [id field] next))
    by-target))

(defn- ordered-members [blocks]
  (if-not (some :native-layer blocks) blocks
    (let [indexed (map-indexed vector blocks)
          groups (group-by #(get-in % [1 :native-layer :id]) indexed)]
      (mapcat (fn [[i b]]
                (if-let [layer (:native-layer b)]
                  (let [members (get groups (:id layer))]
                    (when (= i (first (peek members)))
                      (map second (sort-by #(get-in % [1 :native-layer :member]) members))))
                  [b])) indexed))))

(defn- owners [blocks]
  (:fields (reduce
   (fn [{:keys [fields scopes]} {:keys [source assignments batch native-layer]}]
     (let [owner (when-let [id (:effect-id source)] [id (:effect-version source)])
           opacity (* (:opacity native-layer 1.0) (:member-opacity native-layer 1.0))
           assigned (reduce (fn [m {:keys [target-id attribute blend] a :opacity}]
                              (write-field m target-id attribute owner (* opacity (or a 1)) (or blend :replace)))
                            fields assignments)
           batched (reduce (fn [m id]
                             (reduce (fn [m attr] (write-field m id attr owner (* opacity (:opacity batch 1.0))
                                                              (:blend batch :replace)))
                                     m (or (keys (:outputs batch))
                                           (map :attribute (get-in batch [:field-request :group :cpu-plan :outputs])))))
                           assigned (:target-ids batch))
           group-id (:id native-layer)
           scopes (if group-id (update scopes group-id (fnil into #{})
                                       (concat (when (pos? opacity) (map :target-id assignments))
                                               (when (pos? opacity) (:target-ids batch))
                                               (:target-ids native-layer))) scopes)
           targets (if (= :previous (:targets native-layer))
                     (if (:inherit-backdrop? native-layer) (keys batched) (get scopes group-id))
                     (:target-ids native-layer))]
       {:scopes scopes
        :fields (reduce (fn [m id]
                 (reduce (fn [m {:keys [attribute value]}]
                           (write-field m id attribute owner opacity (if (map? value) :add :replace)))
                         m (:transforms native-layer)))
               batched targets)})) {:fields {} :scopes {}} (ordered-members blocks))))

(defn- shares [field-owners fields]
  (reduce (fn [m attr] (merge-with max m (get field-owners attr))) {} fields))

(defn- light-shares [writers fixture]
  (let [channels (:compiled-channels fixture)
        physical (filter #(and (channels %) (not= :uv %)) registry/emitter-attributes)
        rgb (cond-> [:red :green :blue] (= :extract-min (:rgbw-policy fixture)) (conj :white))
        use-color? (some #(and (channels %) (not (contains? writers %))) rgb)]
    (shares writers (cond-> (conj physical :dimmer) use-color? (conj :color)))))

(defn- score [brightness flashing movement strobe]
  (- 1.0 (* (- 1.0 (* 0.23 brightness)) (- 1.0 (* 0.80 flashing))
            (- 1.0 (* 0.65 movement)) (- 1.0 (* 0.98 strobe)))))

(defn- motion [direction previous dt]
  (if (and direction previous dt)
    (let [dot (reduce + (map * direction previous))]
      ;; Speed of the beam in world space, one half-turn per second saturates.
      (unit (/ (Math/acos (max -1.0 (min 1.0 (double dot)))) (* Math/PI dt))))
    0.0))

(defn- emitted-color [frame fixture]
  (or (get-in frame [:values (:id fixture) :emitted-color])
      (when-let [field (get-in frame [:compact :fields :emitted-color])]
        (let [i (::frame/index fixture)]
          (when (aget ^booleans (:valid field) i)
            (let [^doubles data (:values field) j (* 3 i)]
              [(aget data j) (aget data (inc j)) (aget data (+ j 2))]))))))

(defn- sample [fixture frame local prev dt]
  (let [[open? strobe] (shutter fixture (:universes frame))
        light (if open? (unit local) 0.0)
        before (:light prev light)
        direction (emit-direction fixture (:universes frame))
        color (when open? (emitted-color frame fixture))
        color-change (if (and color (:color prev))
                       (reduce max 0.0 (map #(abs (- %1 %2)) color (:color prev))) 0.0)
        change (max (abs (- light before)) color-change)
        ;; Follow a recent steep edge for 180 ms so a short sparkle is visible
        ;; between samples. The tail remains proportional to its original light.
        edge (if dt (* (max before light) (unit (/ change (* 8.0 dt)))) 0.0)
        flashing (if dt (max edge (* (:flashing prev 0.0) (Math/exp (/ (- dt) 0.18)))) 0.0)]
    {:light light :color color :direction direction :flashing flashing
     :movement (* light (motion direction (:direction prev) dt)) :strobe (* light strobe)}))

(defn- add! [^doubles totals w count-weight light flashing movement strobe]
  (aset totals 0 (+ (aget totals 0) (* w light)))
  (when (> light (/ 1.0 255.0)) (aset totals 1 (+ (aget totals 1) count-weight)))
  (aset totals 2 (+ (aget totals 2) (* w flashing)))
  (aset totals 3 (+ (aget totals 3) (* w movement)))
  (aset totals 4 (+ (aget totals 4) (* w strobe)))
  (aset totals 5 (+ (aget totals 5) (* w (score light flashing movement strobe)))))

(defn- summary [^doubles totals]
  (assoc (zipmap components (map #(unit (aget totals %)) (range (count components))))
         :score (unit (aget totals 5))))

(defn advance
  "Sample a final packed frame after brightness matching, envelopes and budget.
  Pass capacity/measure-sources for every emitting target in the rig and ordered
  authored contribution blocks (before output processing). dt uses monotonic
  milliseconds. Drop previous state across rig revisions, seeks and show epochs."
  [previous patch source-levels frame blocks time-ms]
  (let [elapsed (when (:time-ms previous) (- time-ms (:time-ms previous)))
        dt (when (and elapsed (< 0 elapsed 1000)) (/ elapsed 1000.0))
        prior (when dt (:samples previous))
        fields (owners blocks)
        total (reduce + 0.0 (map :global-maximum (vals source-levels)))
        count-weight (/ 1.0 (max 1 (count source-levels)))
        ;; A patch group is normalized against its own complete set of emitters.
        group-weights (reduce-kv (fn [weights id meter]
                                   (reduce #(update %1 %2 (fn [[sum n]] [(+ (or sum 0.0) (:global-maximum meter)) (inc (or n 0))]))
                                           weights (distinct (get-in patch [:by-id id :groups])))) {} source-levels)
        group-totals (update-vals group-weights (fn [_] (double-array 6)))
        scene (double-array 6)
        effects (java.util.HashMap.)
        saved (reduce-kv
               (fn [m id {:keys [local-brightness global-maximum]}]
                 (let [fixture (get-in patch [:by-id id])
                       old (get prior id)
                       {:keys [light flashing movement strobe] :as current}
                       (sample fixture frame local-brightness old dt)
                       writers (get fields id)
                       light-share (or (not-empty (light-shares writers fixture))
                                       (when (or (pos? light) (> flashing 0.005)) (:light-share old)))
                       flash-share (merge-with max light-share
                                               (when (> flashing 0.005) (:light-share old)))
                       motion-share (if (or (:aim writers) (:direction writers))
                                      (shares writers [:aim :direction]) (shares writers [:pan :tilt]))
                       strobe-share (shares writers shutter-fields)
                       weight (if (pos? total) (/ global-maximum total) 0.0)]
                   (add! scene weight count-weight light flashing movement strobe)
                   (doseq [group (distinct (:groups fixture))]
                     (let [[sum n] (group-weights group)]
                       (add! (group-totals group) (if (pos? sum) (/ global-maximum sum) 0.0) (/ 1.0 n)
                             light flashing movement strobe)))
                   (doseq [owner (into #{} (mapcat keys) [flash-share light-share motion-share strobe-share])]
                     (let [a (or (.get effects owner)
                                 (let [a (double-array 6)] (.put effects owner a) a))]
                       (add! a weight count-weight
                             (* light (get light-share owner 0.0)) (* flashing (get flash-share owner 0.0))
                             (* movement (get motion-share owner 0.0)) (* strobe (get strobe-share owner 0.0)))))
                   (assoc! m id (assoc current :light-share light-share))))
               (transient {}) source-levels)
        ;; During a hot reload select the most recent revision for that ID.
        by-effect (reduce (fn [m [[id version] values]]
                            (if (and (contains? m id) (< (or version 0) (or (get-in m [id :version]) 0))) m
                              (assoc m id (assoc (summary values) :version version)))) {} effects)
        report {:scene (summary scene) :groups (update-vals group-totals summary) :effects by-effect}]
    {:state {:time-ms time-ms :samples (persistent! saved)} :report report}))

(defn publish
  "Publish the latest scores and preserve the peak learned for current effect
  revisions. A changed whole percentage point emits a lighting activity event."
  [model report]
  (if-not report
    (if (get-in model [:variables :lighting/activity])
      (update model :variables dissoc :lighting/activity) model)
    (let [old (get-in model [:variables :lighting/activity])
          score (get-in report [:scene :score])
          old-score (get-in old [:scene :score])
          history (reduce-kv (fn [m id {:keys [version score]}]
                               (if version (update m id (fn [past]
                                                          {:version version :score (if (= version (:version past))
                                                                                     (max score (:score past)) score)})) m))
                             (into {} (filter (fn [[id past]]
                                               (= (:version past) (get-in model [:registry :effects id :version]))))
                                   (:history old)) (:effects report))
          value (assoc report :history history)]
      (cond-> (if (= old value) model (assoc-in model [:variables :lighting/activity] value))
        (or (nil? old-score) (not= (Math/round (* 100.0 score)) (Math/round (* 100.0 old-score))))
        (events/collect :lighting {:kind :lighting-rig :id (:show-id model)}
                        [(assoc (:scene report) :type :lighting/activity)])))))

(defn- constant-or-midpoint [value params]
  (cond
    (number? value) [(unit value) false]
    (and (vector? value) (every? number? value)) [(unit (reduce max 0.0 value)) false]
    (= :const (:op value)) (constant-or-midpoint (:value value) params)
    (= :param (:op value)) (constant-or-midpoint (get params (:key value) 0.5) params)
    :else [0.5 true]))

(defn- provisional [outputs params fixture]
  (let [fields (into {} (keep (fn [{:keys [attribute value targets]}]
                                (when (or (nil? targets) (contains? targets (:id fixture)))
                                  [attribute (constant-or-midpoint value params)]))) outputs)
        emitters (select-keys fields (conj registry/emitter-attributes :color))
        dimmer (get-in fields [:dimmer 0] 1.0)
        ;; A movement- or shutter-only program needs an independent light source.
        ;; The provisional estimate assumes that companion is lit.
        drive (* dimmer (if (seq emitters) (reduce max 0.0 (map first (vals emitters))) 1.0))
        emits? (or (fields :dimmer) (seq emitters))
        changing? (fn [attributes] (some second (vals (select-keys fields attributes))))
        shutter-value (or (get-in fields [:strobe 0]) (get-in fields [:shutter 0]))
        attr (if (fields :strobe) :strobe :shutter)
        [open? strobe] (if shutter-value
                         (shutter-profile/response attr (get-in fixture [:channels attr])
                                                   (Math/round (* 255.0 shutter-value)) shutter-value)
                         [true 0.0])
        visible (if open? drive 0.0)]
    (score (if emits? visible 0.0)
           (* visible (if (changing? light-fields) 0.45 0.0))
           (* visible (if (changing? motion-fields) 0.55 0.0))
           (* visible strobe))))

(defn predicted-score
  "Peak observed on this effect revision, or an approximate score for an
  unperformed declarative cue. Its resolved parameters, lit target count and
  physical strobe ranges inform the provisional estimate. Code-only effects
  without measurements have no prediction."
  [model cue]
  (let [id (:effect-id cue)
        effect (get-in model [:registry :effects id])
        learned (get-in model [:variables :lighting/activity :history id])]
    (if (and effect learned (= (:version effect) (:version learned))) (:score learned)
      (when-let [entries (seq (registry/output-entries (:outputs effect)))]
        (let [params (merge (update-vals (:params effect) :default) (:saved-params cue) (:params cue))
              fixtures (or (:fixtures model) (get-in model [:rig :fixtures]))
              eligible (filter #(some (conj registry/emitter-attributes :dimmer) (keys (:channels %))) fixtures)
              targets (when (:targets effect) (set (:targets effect)))
              entries (mapv #(cond-> % (:targets %) (update :targets set)) entries)]
          (when (seq eligible)
            (/ (reduce (fn [sum f] (if (and targets (not (targets (:id f)))) sum
                                     (+ sum (provisional entries params f)))) 0.0 eligible)
               (double (count eligible)))))))))
