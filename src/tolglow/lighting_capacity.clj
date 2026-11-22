(ns tolglow.lighting-capacity
  "Headless show policy and metering. Read resolved universes after blending;
  the audio gain is a limit on the authored look, owned separately from master."
  (:require [afterglow.render.photometry :as photometry]))

(def defaults
  {:enabled? false :follow-energy? false :minimum 0.1 :maximum 0.9
   :reference-rms 0.35 :signal-timeout-ms 750
   :fog? false :fog-fixture-ids []})

(defn settings [options]
  (let [{:keys [minimum maximum reference-rms signal-timeout-ms fog? fog-fixture-ids] :as opts} (merge defaults options)
        unit? #(and (number? %) (<= 0 % 1))]
    (when-not (and (every? boolean? ((juxt :enabled? :follow-energy?) opts))
                   (boolean? fog?) (vector? fog-fixture-ids) (every? keyword? fog-fixture-ids)
                   (unit? minimum) (unit? maximum) (<= minimum maximum)
                   (unit? reference-rms) (pos? reference-rms)
                   (integer? signal-timeout-ms) (<= 1 signal-timeout-ms 60000))
      (throw (ex-info "Lighting capacity requires ordered unit bounds, positive reference RMS and a signal timeout in milliseconds" {:options options})))
    opts))

(def ^:private field-bit (zipmap (into [:dimmer :color] (keys photometry/emitter-spectrum)) (map #(bit-shift-left 1 %) (range))))

(defn- head [{:keys [id compiled-channels channels emission-share max-watts] :as fixture}]
  (let [emitters (into [] (keep (fn [[attr [response tint]]]
                                  (when-let [channel (compiled-channels attr)]
                                    {:channel channel :response response :tint tint}))) photometry/emitter-spectrum)
        dimmer (:dimmer compiled-channels)
        lamp? (and dimmer (empty? emitters)
                   (not (some #(contains? channels %) [:fog :haze :fan :relay :pump])))
        weight (if (or max-watts (:measured-relative-brightness fixture))
                 (photometry/relative-brightness fixture) (double (or emission-share 1.0)))
        response (reduce + (map :response emitters))]
    (when (or lamp? (seq emitters))
      {:id id :dimmer dimmer :emitters emitters :lamp-color (:emission-color fixture [1 1 1])
       :response response :weight (if (or lamp? (pos? response)) weight 0.0)
       :watts (when max-watts (* max-watts (or emission-share 1.0)))
       :attributes (into (cond-> #{:dimmer} (every? channels [:red :green :blue]) (conj :color))
                         (filter channels (keys photometry/emitter-spectrum)))})))

(deftype MeterAddress [^long universe ^long slot ^long fineUniverse ^long fineSlot ^double pivot])
(deftype MeterEmitter [^MeterAddress address ^double brightness ^double watts ^double red ^double green ^double blue])
(deftype MeterHead [^MeterAddress dimmer ^objects emitters])

(defn- address [indices {:keys [coarse fine inverted-from]}]
  (when coarse
    (MeterAddress. (indices (first coarse)) (second coarse)
                   (if fine (indices (first fine)) -1) (if fine (second fine) -1)
                   (or inverted-from -1.0))))

(defn- meter-head [indices {:keys [emitters dimmer lamp-color response weight watts]}]
  (let [n (count emitters)
        channels (if (zero? n)
                   [(MeterEmitter. nil weight (or watts 0.0)
                                   (* weight (nth lamp-color 0)) (* weight (nth lamp-color 1)) (* weight (nth lamp-color 2)))]
                   (for [{:keys [channel tint] sensitivity :response} emitters]
                     ;; Equal RGB drive is a neutral tint; perceived brightness
                     ;; uses the wavelength model separately.
                     (let [color-scale (/ weight n)]
                       (MeterEmitter. (address indices channel)
                         (if (pos? response) (* weight (/ sensitivity response)) 0.0) (/ (or watts 0.0) n)
                         (* color-scale (nth tint 0)) (* color-scale (nth tint 1)) (* color-scale (nth tint 2))))))]
    (MeterHead. (address indices dimmer) (object-array channels))))

(defn prepare
  "Compile a meter plan alongside an Afterglow compiled patch, once per rig."
  [patch]
  (let [heads (into [] (keep head) (:fixtures patch))
        universes (:universes patch)
        indices (zipmap universes (range))]
    {:meter-heads (object-array (map #(meter-head indices %) heads)) :universes universes
     :writers-by-head (into {} (map-indexed (fn [i h] [(:id h) [i (:attributes h)]]) heads))
     :foggers (into #{} (keep #(when (contains? (:channels %) :fog) (:id %))) (:fixtures patch))
     :relative-capacity (reduce + 0.0 (map :weight heads))
     :rated-watts (reduce + 0.0 (keep :watts heads))
     :rated-heads (count (filter :watts heads)) :heads-count (count heads)}))

(defn- level ^double [^objects buffers ^MeterAddress channel]
  (if (nil? channel) 1.0
    (let [coarse (bit-and 255 (aget ^bytes (aget buffers (.-universe channel)) (.-slot channel)))
          raw (+ coarse (if (neg? (.-fineUniverse channel)) 0.0
                          (/ (bit-and 255 (aget ^bytes (aget buffers (.-fineUniverse channel)) (.-fineSlot channel))) 255.0)))
          pivot (.-pivot channel)
          drive (if (and (>= pivot 0.0) (>= raw pivot)) (- 255.0 (- raw pivot)) raw)]
      (/ drive 255.0))))

(defn- meter-head! [^doubles totals ^objects buffers ^MeterHead head]
  (let [dimmer (level buffers (.-dimmer head))
        ^objects emitters (.-emitters head)]
    (dotimes [i (alength emitters)]
      (let [^MeterEmitter emitter (aget emitters i)
            drive (* dimmer (level buffers (.-address emitter)))]
        (aset totals 0 (+ (aget totals 0) (* drive (.-brightness emitter))))
        (aset totals 1 (+ (aget totals 1) (* drive (.-watts emitter))))
        (aset totals 2 (+ (aget totals 2) (* drive (.-red emitter))))
        (aset totals 3 (+ (aget totals 3) (* drive (.-green emitter))))
        (aset totals 4 (+ (aget totals 4) (* drive (.-blue emitter))))))))

(defn measure
  "Measure rendered DMX, including physical dimmer, fine channels and inversion.
  Electrical drive is an equal-emitter estimate for heads with known ratings."
  [{:keys [meter-heads relative-capacity] :as plan} universes]
  (let [totals (double-array 5)
        buffers (object-array (map universes (:universes plan)))
        ^objects heads meter-heads]
    (dotimes [i (alength heads)] (meter-head! totals buffers (aget heads i)))
    (let [light (aget totals 0)
          utilization (if (pos? relative-capacity) (min 1.0 (/ light relative-capacity)) 0.0)
          rgb (mapv #(aget totals %) [2 3 4])
          peak (reduce max rgb)]
      (assoc (select-keys plan [:relative-capacity :rated-watts :rated-heads :heads-count])
             :relative-output light :utilization utilization :headroom (- 1.0 utilization)
             :used-rated-watts (aget totals 1) :color (if (pos? peak) (mapv #(/ % peak) rgb) rgb)))))

(defn- source-targets [{:keys [batch assignments native-layer]}]
  (concat (:target-ids batch) (map :target-id assignments) (:target-ids native-layer)))

(defn- modifiers-targets [blocks i {:keys [native-layer] :as block}]
  (concat (source-targets block)
          (when (= :previous (:targets native-layer))
            (into [] (comp (filter #(or (:inherit-backdrop? native-layer)
                                         (= (:id native-layer) (get-in % [:native-layer :id]))))
                           (filter #(pos? (get-in % [:native-layer :member-opacity] 1.0)))
                           (mapcat source-targets))
                  (take i blocks)))))

(defn writers
  "List positive-opacity emission writers, removing writers fully superseded
  by later opaque replaces of the same native field on every target. Arithmetic
  modifier and partial blend contributors share the resulting output."
  [{:keys [writers-by-head heads-count]} blocks]
  (let [blocks (vec blocks)
        covered (int-array heads-count)
        visible! (fn [id attribute opacity blend]
                   (when-let [[index supported] (get writers-by-head id)]
                     (when (and (pos? opacity) (contains? supported attribute))
                       (let [bit (long (get field-bit attribute))
                             before (aget covered (int index))]
                         (when (zero? (bit-and before bit))
                           (when (and (= blend :replace) (== (double opacity) 1.0))
                             (aset-int covered (int index) (int (bit-or before bit))))
                           true)))))]
    (->> (range (dec (count blocks)) -1 -1)
         (keep (fn [i]
                 (let [{:keys [source assignments batch native-layer] :as block} (nth blocks i)
                       weight (* (:member-opacity native-layer 1.0) (:opacity native-layer 1.0))
                       visible? (volatile! false)
                       visit! (fn [id attr opacity blend]
                                (when (visible! id attr (* opacity weight) blend) (vreset! visible? true)))]
                   (when (pos? weight)
                     (doseq [{:keys [target-id attribute opacity blend] :or {opacity 1.0 blend :replace}}
                             (rseq (vec assignments))]
                       (visit! target-id attribute opacity blend))
                     (when batch
                       (let [attrs (or (keys (:outputs batch))
                                       (map :attribute (get-in batch [:field-request :group :cpu-plan :outputs])))
                             opacity (* (:opacity batch 1.0) weight)
                             blend (:blend batch :replace)]
                         (if (and (= blend :replace) (== (double opacity) 1.0))
                           (doseq [id (:target-ids batch) attr attrs]
                             (when (visible! id attr opacity blend) (vreset! visible? true)))
                           (when (and (pos? opacity)
                                      (some (fn [id] (some #(visible! id % opacity blend) attrs)) (:target-ids batch)))
                             (vreset! visible? true)))))
                     (when-let [transforms (seq (filter #(field-bit (:attribute %)) (:transforms native-layer)))]
                       (doseq [id (modifiers-targets blocks i block)
                               {:keys [attribute value]} transforms]
                         ;; A transform may read the previous or incoming native
                         ;; value. A performed arithmetic write keeps that source.
                         (visit! id attribute 1.0 (if (map? value) :add :replace))))
                     (when @visible? source)))))
         distinct reverse vec)))

(defn control
  "Choose the effective frame gain. Previous utilization includes previous gain
  and operator master; divide both out before estimating the current look.
  Silence is measured audio. Missing or stale capture restores manual control."
  [{:keys [follow-energy? minimum maximum reference-rms signal-timeout-ms]}
   previous master now-ms {:keys [energy-envelope received-ms]} bypass?]
  (let [live? (and (number? energy-envelope) (Double/isFinite (double energy-envelope))
                   (number? received-ms) (<= 0 (- now-ms received-ms) signal-timeout-ms))
        mode (cond bypass? :bypassed (not follow-energy?) :manual (not live?) :no-signal :else :following)
        target (when (= mode :following)
                 (+ minimum (* (- maximum minimum) (max 0.0 (min 1.0 (/ energy-envelope reference-rms))))))
        past-scale (* (:gain previous 0.0) (:operator-master previous 0.0))
        baseline (when (and (pos? past-scale) (pos? (:utilization previous 0.0)))
                   (* master (/ (:utilization previous) past-scale)))
        gain (if target
               (if (and baseline (pos? baseline))
                 ;; Within a half 8-bit unit of target, retain the current gain
                 ;; so quantization cannot make adjacent bytes toggle forever.
                 (if (and (= master (:operator-master previous)) (= target (:target previous))
                          (< (abs (- target (:utilization previous))) (/ 0.5 255)))
                   (:gain previous)
                   (min 1.0 (/ target baseline)))
                 (if (pos? master) (min 1.0 (/ target master)) 1.0))
               1.0)]
    {:mode mode :gain gain :operator-master master :effective-master (* master gain)
     :target target :energy-envelope (when live? energy-envelope)}))

(defn fog-policy
  "One simulated or presented pulse per upward crossing of 80% audio energy.
  Rearm below 55%; each 0.4 s pulse is followed by at least 6 s rest. Missing
  audio, operator blackout/master zero, pause or calibration cancels demand."
  [{:keys [fog? fog-fixture-ids]} {:keys [foggers]} previous now-ms limit bypass? reference-rms]
  (let [targets (filterv foggers (distinct fog-fixture-ids))
        energy (:energy-envelope limit)
        armed? (and fog? (seq targets) (some? energy) (not bypass?) (pos? (:operator-master limit)))
        normalized (when armed? (/ energy reference-rms))
        continuing? (and armed? (= targets (:targets previous))
                         (< now-ms (:until-ms previous 0)))
        trigger? (and armed? (not continuing?) (not (:latched? previous))
                      (>= now-ms (:rest-until-ms previous 0)) (>= normalized 0.8))
        until-ms (cond trigger? (+ now-ms 400) continuing? (:until-ms previous) :else 0)
        state {:active? (boolean (or trigger? continuing?))
               :latched? (boolean (and armed? (or (>= normalized 0.8) (and (:latched? previous) (> normalized 0.55)))))
               :targets targets :until-ms until-ms
               :rest-until-ms (if trigger? (+ until-ms 6000) (:rest-until-ms previous 0))}]
    {:state state
     :block (when (:active? state)
              {:kind :assignments
               :assignments (mapv (fn [id] {:target-id id :attribute :fog :value 0.5 :blend :max}) targets)})}))
