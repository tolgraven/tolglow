(ns tolglow.calibration.session
  "Portable calibration artifact and deterministic evidence admission. Session
  status never grants output authority after a host restart or file restore."
  (:require [tolglow.calibration.geometry :as g]
            [tolglow.rig :as rig]))

(def artifact-path [:extensions :tolglow.calibration/session])
(def palette [[0.65 0.02 0.02] [0.02 0.65 0.02] [0.02 0.02 0.65]])
(defn- fail! [message] (throw (ex-info message {:component :rig-calibration})))
(defn plan
  "Sequential, deliberately slow color codes, then spatially diverse pan/tilt
  poses. Each step is held explicitly; :minimum-hold is musical authoring data.
  No strobe channel pulses or motor motion are issued by constructing a plan."
  [fixtures]
  (vec (mapcat (fn [fixture]
                 (let [poses (if (every? #(contains? (:channels fixture) %) [:pan :tilt])
                               [[0.5 0.5] [0.35 0.5] [0.5 0.65] [0.65 0.4]] [[nil nil]])]
                   (for [[pose [pan tilt]] (map-indexed vector poses)
                         [symbol color] (map-indexed vector palette)]
                     {:fixture-id (:id fixture) :pose pose :symbol symbol :color color
                      :pan pan :tilt tilt :dimmer 0.3 :minimum-hold {:beats 2}
                      :settle-ms (if (zero? symbol) 750 150)}))) fixtures)))

(defn create [model {:keys [session-id fixture-ids world-id world-to-rig clock]} now]
  (let [fixtures (filterv #(contains? (set fixture-ids) (:id %)) (get-in model [:rig :fixtures]))]
    (when-not (and (not= session-id (get-in model (conj artifact-path :session-id))) (string? session-id) (<= 1 (count session-id) 100)
                   (string? world-id) (<= 1 (count world-id) 100)
                   (vector? fixture-ids) (= (count fixtures) (count (set fixture-ids))) (<= 1 (count fixtures) 64)
                   (every? #(every? (partial contains? (:channels %)) [:red :green :blue :dimmer]) fixtures))
      (fail! "Select 1–64 existing RGB dimmer fixtures and a unique scan/world ID"))
    (g/rigid-transform! world-to-rig)
    (when-not (and (g/finite? (:offset-ms clock)) (g/finite? (:uncertainty-ms clock))
                   (<= 0 (:uncertainty-ms clock) 100))
      (fail! "Capture clock alignment needs finite offset and uncertainty at most 100 ms"))
    {:version 1 :session-id session-id :world-id world-id :world-to-rig world-to-rig :clock clock
     :show-id (:show-id model) :epoch (:epoch model) :base-rig (:rig model) :original-rig (:rig model) :fixture-ids fixture-ids
     :status :prepared :created-at-ms now :expires-at-ms (+ now 900000) :lease-until-ms (+ now 15000) :step-index nil
     :plan (plan fixtures) :receipts [] :observations [] :candidates [] :evidence-revision 0}))

(defn receipt
  "Receipts come only from the owned successful sink boundary, never HTTP acks.
  Keep ineligible/gap records too: they fence the usable exposure windows."
  [session record]
  (if (or (not= (:epoch session) (:epoch record))
          (not= (:session-id session) (get-in record [:calibration :session-id]))) session
    (-> session (update :receipts #(vec (take-last 4096 (conj % record))))
        (update :evidence-revision inc))))
(declare match-observation)
(defn observe [session samples]
  (when-not (and (vector? samples) (<= 1 (count samples) 128)) (fail! "Upload 1–128 observations per request"))
  (let [existing (set (map :observation-id (:observations session)))
        samples (mapv #(select-keys % [:observation-id :world-id :capture-ms :pixel :depth-m :intrinsics
                                      :camera-to-world :kind :track-id :rgb :confidence])
                      (remove #(existing (:observation-id %)) samples))]
    (doseq [{:keys [observation-id world-id capture-ms pixel depth-m intrinsics camera-to-world kind track-id rgb confidence]} samples]
      (when-not (and (string? observation-id) (<= 1 (count observation-id) 100)
                     (= world-id (:world-id session)) (g/finite? capture-ms) (<= 0 capture-ms)
                     (#{:lens :beam} kind) (string? track-id) (<= 1 (count track-id) 100)
                     (g/v3? rgb) (every? #(<= 0 % 1) rgb) (#{1 2} confidence))
        (fail! "Observation needs stable identity, original AR world, tracking-normal depth confidence, RGB and capture clock"))
      (g/unproject {:pixel pixel :depth-m depth-m :intrinsics intrinsics :camera-to-world camera-to-world
                    :world-to-rig (:world-to-rig session)}))
    (when (> (+ (count samples) (count (:observations session))) 4096) (fail! "Observation capacity reached; solve or start a new scan"))
    (when-not (= (count samples) (count (set (map :observation-id samples)))) (fail! "Duplicate IDs in observation batch"))
    (let [samples (mapv (fn [sample]
                          (let [evidence (match-observation session sample)]
                            (cond-> sample (:matched? evidence) (assoc :evidence evidence)))) samples)]
      (cond-> (update session :observations into samples) (seq samples) (update :evidence-revision inc)))))

(defn- color-distance [a b]
  (let [normalize #(g/scale % (/ (max 1.0e-8 (reduce + %))))]
    (g/norm (g/sub (normalize a) (normalize b)))))
(defn match-observation
  "Require a complete exposure uncertainty interval bracketed by consecutive
  actual successful, unmodified presentations of the SAME settled pattern.
  A receipt gap, dropped offer, frame interpolation, blackout, rig epoch change,
  slow publisher or color mismatch is not positive identity evidence."
  [session observation]
  (let [t (+ (:capture-ms observation) (get-in session [:clock :offset-ms]))
        uncertainty (+ 5 (get-in session [:clock :uncertainty-ms]))
        receipts (:receipts session)
        left-index (last (keep-indexed (fn [i r] (when (<= (:published-at-ms r) (- t uncertainty)) i)) receipts))
        right-index (first (keep-indexed (fn [i r] (when (>= (:published-at-ms r) (+ t uncertainty)) i)) receipts))
        window (when (and left-index right-index (< left-index right-index))
                 (subvec receipts left-index (inc right-index)))
        pair (when window [(first window) (last window)])]
    (if-not pair {:matched? false :reason :unbracketed-exposure}
      (let [[a b] pair ma (:calibration a) mb (:calibration b)
            step (get (:plan session) (:step-index ma))
            first-time (some #(when (= (:step-index ma) (get-in % [:calibration :step-index])) (:published-at-ms %)) receipts)]
        (cond
          (or (not= ma mb)
              (some #(or (not (:eligible? %)) (not= ma (:calibration %))
                         (not= (:generation a) (:generation %))) window)
              (some #(pos? (:dropped-before % 0)) (rest window))
              (some (fn [[a b]] (> (- (:published-at-ms b) (:published-at-ms a)) 100)) (partition 2 1 window)))
          {:matched? false :reason :presentation-gap-or-modification}
          (or (nil? step) (< (- t uncertainty first-time) (:settle-ms step))) {:matched? false :reason :not-settled}
          (or (< (reduce + (:rgb observation)) 0.06) (> (color-distance (:rgb observation) (:color step)) 0.25))
          {:matched? false :reason :color-mismatch}
          :else {:matched? true :fixture-id (:fixture-id step) :step-index (:step-index ma) :symbol (:symbol step)
                 :pan (:pan step) :tilt (:tilt step) :weight (if (= 2 (:confidence observation)) 1.0 0.3)
                 :point (g/unproject (assoc observation :world-to-rig (:world-to-rig session)))
                 :receipt-proof (mapv #(select-keys % [:epoch :generation :frame-id :published-at-ms :calibration :presentation :eligible? :dropped-before]) window)})))))

(defn solve
  "Retain evidence and diagnostics. A lens track needs all three coded colors;
  each pose uses a robust centre of its beam target samples. User verifies the
  fixture definition's axis calibration before a mounting result is admissible."
  [session verified-axis-fixture-ids]
  (let [matched (mapv #(merge % (or (:evidence %) (match-observation session %))) (:observations session))
        track-valid? (into #{} (keep (fn [[k samples]] (when (= #{0 1 2} (set (map :symbol samples))) k)))
                           (group-by (juxt :fixture-id :kind :track-id) (filter :matched? matched)))
        usable (filter #(and (:matched? %) (track-valid? [(:fixture-id %) (:kind %) (:track-id %)])) matched)
        candidates
        (mapv (fn [fixture-id]
                (let [fixture (some #(when (= fixture-id (:id %)) %) (get-in session [:base-rig :fixtures]))
                      samples (filter #(= fixture-id (:fixture-id %)) usable)
                      lens (filter #(= :lens (:kind %)) samples)
                      targets (->> (filter #(= :beam (:kind %)) samples)
                                   (group-by (juxt :pan :tilt))
                                   (keep (fn [[[pan tilt] xs]] (when (and pan tilt (>= (count xs) 3))
                                                               (let [fit (g/fit-position xs)]
                                                                 (when (:accepted? fit) {:pan pan :tilt tilt :target (:position fit)}))))) vec)
                      position (when (>= (count lens) 3) (g/fit-position lens))
                      aim (when (and (:accepted? position) (>= (count targets) 3))
                            (g/fit-aim fixture (:position position) targets (contains? (set verified-axis-fixture-ids) fixture-id)))
                      proposed (cond-> fixture (:accepted? position) (assoc :position (:position position))
                                       (:accepted? aim) (assoc :rotation-deg (:rotation-deg aim)))]
                  {:fixture-id fixture-id :position position :aim aim :proposed-fixture proposed
                   :reviewable? (boolean (:accepted? position))
                   :diagnostics (cond-> [] (not (:accepted? position)) (conj :need-coded-lens-track)
                                  (not (:accepted? aim)) (conj :aim-unresolved))})) (:fixture-ids session))]
    (assoc session :status :review :candidates candidates
           :diagnostics {:total (count matched) :usable (count usable)
                         :rejected (frequencies (keep #(when-not (:matched? %) (:reason %)) matched))})))

(defn proposed-rig [session current-rig accepted-fixture-ids]
  (when-not (= current-rig (:base-rig session)) (fail! "Rig changed during scanning; rescan/rebase before applying"))
  (let [accepted (set accepted-fixture-ids)
        candidates (filterv #(and (:reviewable? %) (accepted (:fixture-id %))) (:candidates session))]
    (when-not (and (seq accepted) (= accepted (set (map :fixture-id candidates))))
      (fail! "Explicitly select reviewed, valid fixture candidates"))
    (let [replacements (into {} (map (juxt :fixture-id :proposed-fixture)) candidates)]
      (rig/validate-rig (update current-rig :fixtures #(mapv (fn [f] (get replacements (:id f) f)) %))))))
