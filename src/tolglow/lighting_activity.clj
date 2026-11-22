(ns tolglow.lighting-activity
  "Perceptual activity in the committed lighting look. Output is a relative
  performance control, not a measurement of electrical power or a safety limit."
  (:require [afterglow.render.frame :as frame]
            [afterglow.runtime.events :as events]
            [clojure.set :as set]
            [tolglow.fixtures.shutter :as shutter-profile]
            [tolglow.lighting-capacity :as capacity]
            [tolglow.registry :as registry]))

(def ^:private motion-fields #{:pan :tilt :aim :direction})
(def ^:private shutter-fields #{:strobe :shutter})
(def ^:private light-fields (into registry/emitter-attributes [:color :dimmer]))
(def ^:private measured-fields (set/union motion-fields shutter-fields light-fields))
(def ^:private components [:brightness :coverage :flashing :movement :strobe])

(defn- unit ^double [^double x] (min 1.0 (max 0.0 x)))

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
                 (= blend :replace) (cond-> (update-vals old #(* % (- 1.0 opacity)))
                                      owner (update owner (fnil + 0.0) opacity))
                 (nil? owner) old
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
        use-color? (some #(and (channels %) (not (contains? writers %))) rgb)
        used (cond-> (conj physical :dimmer) use-color? (conj :color))]
    ;; An opaque, unattributed operator look ({}) replaces a former writer.
    ;; An ended cue with no new assignment (nil) can still own its release tail.
    (when (some #(contains? writers %) used) (shares writers used))))

(defn- score ^double [^double brightness ^double flashing ^double movement ^double strobe]
  (- 1.0 (* (- 1.0 (* 0.23 brightness)) (- 1.0 (* 0.80 flashing))
            (- 1.0 (* 0.65 movement)) (- 1.0 (* 0.98 strobe)))))

(defn- motion ^double [direction previous dt]
  (if (and direction previous dt)
    (let [dot (reduce + (map * direction previous))]
      ;; Speed of the beam in world space, one half-turn per second saturates.
      (if (> dot 0.999999999999) 0.0
        (unit (/ (Math/acos (max -1.0 (min 1.0 (double dot)))) (* Math/PI dt)))))
    0.0))

(defn- add! [^doubles totals w count-weight light flashing movement strobe]
  (let [w (double w) light (double light) flashing (double flashing)
        movement (double movement) strobe (double strobe)]
    (aset totals 0 (+ (aget totals 0) (* w light)))
    (when (> light (/ 1.0 255.0)) (aset totals 1 (+ (aget totals 1) (double count-weight))))
    (aset totals 2 (+ (aget totals 2) (* w flashing)))
    (aset totals 3 (+ (aget totals 3) (* w movement)))
    (aset totals 4 (+ (aget totals 4) (* w strobe)))
    (aset totals 5 (+ (aget totals 5) (* w (score light flashing movement strobe))))))

(defn- summary [^doubles totals]
  (assoc (zipmap components (map #(unit (aget totals %)) (range (count components))))
         :score (unit (aget totals 5))))

(defn- ownership-shape [blocks]
  (mapv (fn [{:keys [source native-layer assignments batch]}]
          [source native-layer
           (when (seq assignments)
             (mapv (fn [{:keys [target-id attribute opacity blend]}] [target-id attribute opacity blend]) assignments))
           (when batch [(:target-ids batch) (keys (:outputs batch)) (:opacity batch) (:blend batch)
                        (mapv :attribute (get-in batch [:field-request :group :cpu-plan :outputs]))])]) blocks))

(defn- layout [patch levels indices]
  (let [total (reduce + 0.0 (map :global-maximum (vals levels)))
        groups (reduce-kv
                (fn [groups id meter]
                  (reduce (fn [groups group]
                            (let [[sum n] (groups group [0.0 0])]
                              (assoc groups group [(+ sum (:global-maximum meter)) (inc n)])))
                          groups (distinct (get-in patch [:by-id id :groups])))) {} levels)]
    {:groups (keys groups) :count-weight (/ 1.0 (max 1 (count levels)))
     :rows (mapv (fn [[id {:keys [global-maximum]}]]
                   (let [fixture (get-in patch [:by-id id])]
                     {:id id :fixture fixture :index (::frame/index fixture) :meter-index (some-> (get indices id) first)
                      :moving? (some (:channels fixture) [:pan :tilt])
                      :shutter? (some (:channels fixture) [:strobe :shutter]) :weight (if (pos? total) (/ global-maximum total) 0.0)
                      :groups (mapv (fn [id]
                                      (let [[sum n] (groups id)]
                                        [id (if (pos? sum) (/ global-maximum sum) 0.0) (/ 1.0 n)]))
                                    (distinct (:groups fixture)))})) levels)}))

;; Compile the fixed patch and current writers once. The render loop reads
;; these primitive coefficients for every pixel and reads the share maps only
;; when an outgoing writer has a visible release or flash tail.
(deftype GroupShare [id ^double weight ^double fraction])
(deftype WriterShare [id ^double light ^double movement ^double strobe])
(deftype SampleTarget [id fixture ^long index ^long meterIndex ^double weight
                       ^boolean moving ^boolean shutter ^objects groups ^objects writers
                       lightShares motionShares strobeShares ownerIds])

(defn- with-writers [layout blocks]
  (let [fields (owners blocks)]
    (update layout :rows
            (fn [rows]
              (object-array
               (for [{:keys [id fixture index meter-index weight moving? shutter? groups]} rows]
                 (let [writers (fields id)
                       light (light-shares writers fixture)
                       movement (if (or (:aim writers) (:direction writers))
                                  (shares writers [:aim :direction]) (shares writers [:pan :tilt]))
                       strobe (shares writers shutter-fields)
                       ids (into #{} (mapcat keys) [light movement strobe])]
                   (SampleTarget. id fixture index (or meter-index -1) weight (boolean moving?) (boolean shutter?)
                                  (object-array (map (fn [[group part n]] (GroupShare. group part n)) groups))
                                  (object-array (map #(WriterShare. % (get light % 0.0) (get movement % 0.0)
                                                                   (get strobe % 0.0)) ids))
                                  light movement strobe ids))))))))

(defn- writer-totals ^doubles [^java.util.HashMap effects id]
  (or (.get effects id) (let [a (double-array 6)] (.put effects id a) a)))

(defn- same-packets? [a b]
  (or (identical? a b)
      (and (= (count a) (count b))
           (every? (fn [[id packet]]
                     (let [other (get b id)]
                       (and other (java.util.Arrays/equals ^bytes packet ^bytes other)))) a))))

(defn- measure [layout source-levels ^doubles measured frame prior dt]
  (let [group-totals (zipmap (:groups layout) (repeatedly #(double-array 6)))
        scene (double-array 6)
        effects (java.util.HashMap.)
        ^objects rows (:rows layout)
        n (alength rows)
        count-weight (double (:count-weight layout))
        ;; Each snapshot owns its arrays. Read the previous sample without
        ;; mutating it even if the host later rejects this frame.
        samples (double-array (* 5 n))
        ^doubles before (:levels prior)
        poses (object-array n)
        ^objects old-poses (:poses prior)
        writers (object-array n)
        ^objects old-writers (:writers prior)
        fade (if dt (Math/exp (/ (- (double dt)) 0.18)) 0.0)
        slope-scale (if dt (/ 1.0 (* 8.0 (double dt))) 0.0)
        compact-color (get-in frame [:compact :fields :emitted-color])
        ^doubles rgb (:values compact-color)
        ^booleans valid (:valid compact-color)]
    (dotimes [i n]
      (let [^SampleTarget row (aget rows i)
            id (.-id row) fixture (.-fixture row) index (.-index row)
            weight (.-weight row) moving? (.-moving row)
            light-share (.-lightShares row)
            [open? strobing] (if (.-shutter row) (shutter fixture (:universes frame)) [true 0.0])
            j (* 5 i)
            light (if open? (unit (if measured (aget measured (int (.-meterIndex row)))
                                    (get-in source-levels [id :local-brightness]))) 0.0)
            old-light (if before (aget before j) light)
            ;; Reuse the compact field's flat RGB column directly; dense strips
            ;; need no per-pixel projected maps or RGB vectors.
            ci (* 3 (int index))
            packed? (and open? rgb (aget valid (int index)))
            color (when (and open? (not packed?)) (get-in frame [:values id :emitted-color]))
            r (if packed? (aget rgb ci) (double (get color 0 0.0)))
            g (if packed? (aget rgb (inc ci)) (double (get color 1 0.0)))
            b (if packed? (aget rgb (+ ci 2)) (double (get color 2 0.0)))
            change (if before (max (abs (- light old-light)) (abs (- r (aget before (inc j))))
                                   (abs (- g (aget before (+ j 2)))) (abs (- b (aget before (+ j 3))))) 0.0)
            ;; A recent steep edge has a short perception tail. The observed
            ;; edge itself remains proportional to how much light was visible.
            edge (* (max old-light light) (min 1.0 (* change slope-scale)))
            trail (if before (* (aget before (+ j 4)) fade) 0.0)
            flash (max edge (if (< trail 0.0001) 0.0 trail))
            pose (when moving? (emit-direction fixture (:universes frame)))
            movement (if moving? (* light (motion pose (when old-poses (aget old-poses i)) dt)) 0.0)
            strobe (* light (double strobing))
            previous-share (when old-writers (aget old-writers i))
            inherited (when (and (nil? light-share) (or (pos? light) (> flash 0.005))) previous-share)
            light-share (or light-share inherited)
            tail (when (and (> flash 0.005) (not (identical? light-share previous-share))) previous-share)
            flash-share (if tail (merge-with max light-share tail) light-share)
            ^objects groups (.-groups row)
            ^objects shares (.-writers row)]
        (aset samples j light)
        (aset samples (inc j) r)
        (aset samples (+ j 2) g)
        (aset samples (+ j 3) b)
        (aset samples (+ j 4) flash)
        (aset poses i pose)
        (aset writers i light-share)
        (add! scene weight count-weight light flash movement strobe)
        (dotimes [k (alength groups)]
          (let [^GroupShare group (aget groups k)]
            (add! (group-totals (.-id group)) (.-weight group) (.-fraction group) light flash movement strobe)))
        (if (or inherited tail)
          (doseq [id (into (.-ownerIds row) (keys flash-share))]
            (add! (writer-totals effects id) weight count-weight
                  (* light (get light-share id 0.0)) (* flash (get flash-share id 0.0))
                  (* movement (get (.-motionShares row) id 0.0)) (* strobe (get (.-strobeShares row) id 0.0))))
          (dotimes [k (alength shares)]
            (let [^WriterShare writer (aget shares k)]
              (add! (writer-totals effects (.-id writer)) weight count-weight
                    (* light (.-light writer)) (* flash (.-light writer))
                    (* movement (.-movement writer)) (* strobe (.-strobe writer))))))))
    (let [by-effect (reduce (fn [m [[id version] values]]
                              (if (and (contains? m id) (< (or version 0) (or (get-in m [id :version]) 0))) m
                                (assoc m id (assoc (summary values) :version version)))) {} effects)]
      {:samples {:levels samples :poses poses :writers writers}
       :report {:scene (summary scene) :groups (update-vals group-totals summary) :effects by-effect}})))

(defn- advance* [previous patch source-levels meter frame blocks time-ms]
  (let [elapsed (when (:time-ms previous) (- time-ms (:time-ms previous)))
        dt (when (and elapsed (< 0 elapsed 1000)) (/ elapsed 1000.0))
        same-rig? (and (identical? patch (:patch previous)) (identical? meter (:meter previous)))
        shape (ownership-shape blocks)
        same-writers? (and same-rig? (= shape (:shape previous)))
        before (:report previous)
        packets (:universes frame)]
    (if (and same-writers? dt
             (zero? (get-in before [:scene :movement])) (zero? (get-in before [:scene :flashing]))
             (same-packets? packets (:packets previous)))
      {:state (assoc previous :time-ms time-ms :packets packets) :report before}
      (let [base (if same-rig? (:layout previous)
                   (layout patch (or (:source-capacities meter) source-levels) (:writers-by-head meter)))
            plan (if same-writers? (:plan previous) (with-writers base blocks))
            local (when meter (capacity/local-brightnesses meter packets))
            {:keys [samples report]} (measure plan source-levels local frame (when (and same-rig? dt) (:samples previous)) dt)]
        {:state {:time-ms time-ms :patch patch :meter meter :layout base :plan plan :shape shape
                 :packets packets :samples samples :report report}
         :report report}))))

(defn advance
  "Sample an existing complete map from capacity/measure-sources after output
  processing. Give the scorer ordered authored blocks for effect attribution."
  [previous patch source-levels frame blocks time-ms]
  (advance* previous patch source-levels nil frame blocks time-ms))

(defn advance-meter
  "Real-time entry point. Use the compiled whole-rig capacity meter and the
  final frame after envelopes, budgeting, master and blackout. Drop private
  history on seeks/restarts. No per-fixture measurement maps are constructed."
  [previous patch meter frame blocks time-ms]
  (advance* previous patch nil meter frame blocks time-ms))

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
        effect (registry/cue-effect (:registry model) cue)
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
