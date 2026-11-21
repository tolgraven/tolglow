(ns tolglow.integrations.audio-return
  "Isolated dry carriers, processed return features and owner-side wet cue control."
  (:require [afterglow.control.model :as control]
            [afterglow.time :as time]
            [clojure.set :as set]
            [tolglow.composition.tween :as tween]
            [tolglow.integrations.audio :as dsp]
            [tolglow.studio.routing :as routing]))

(defn- require! [ok message]
  (when-not ok (throw (ex-info message {:integration :audio-return}))))

(defn- schema [model cue-id param]
  (let [cue (get-in model [:registry :cues cue-id])
        effect (get-in model [:registry :effects (:effect-id cue)])
        version (:effect-version cue)
        definition (if (or (nil? version) (= version (:version effect))) effect
                     (get-in model [:registry :versions [:effects (:effect-id cue) version]]))
        parameter (get-in definition [:params param])]
    (require! (and (seq (:outputs definition)) (#{:number :integer} (:type parameter))
                   (vector? (:bounds parameter)) (= 2 (count (:bounds parameter)))
                   (every? time/finite-number? (:bounds parameter)) (apply < (:bounds parameter)))
              "Carrier endpoints require registered lighting cues with bounded numeric parameters")
    parameter))

(defn- unique? [xs] (= (count xs) (count (distinct xs))))

(defn validate-config
  "Validate against the actual cue registry. Physical channels are one-based."
  [model config]
  (let [{:keys [lanes channels sample-rate block-frames output-mixer input-mixer] :as config}
        (merge {:sample-rate 48000 :block-frames 1024 :channels 2} config)]
    (require! (and (integer? channels) (<= 1 channels 64)
                   (integer? sample-rate) (<= 8000 sample-rate 96000)
                   (integer? block-frames) (<= 256 block-frames 4096)
                   (every? #(and (string? %) (<= 1 (count %) 160)) [input-mixer output-mixer])
                   (vector? lanes) (<= 1 (count lanes) 32)
                   (unique? (map :id lanes)) (unique? (map :send-channel lanes))
                   (unique? (map :return-channel lanes))
                   (or (not= input-mixer output-mixer)
                       (empty? (set/intersection (set (map :send-channel lanes)) (set (map :return-channel lanes)))))
                   (empty? (set/intersection (set (map :dry-cue lanes)) (set (map :wet-cue lanes)))))
              "Choose named input/output mixers and independent dry/wet cue and channel lanes")
    (assoc config :lanes
           (mapv
             (fn [lane]
               (let [{:keys [id dry-cue dry-param wet-cue wet-param send-channel return-channel
                             frequency-range amplitude confidence-min level-floor curve max-tail
                             silence-hold-ms max-tail-ms feature] :as lane}
                     (merge {:frequency-range [120 1800] :amplitude 0.5 :confidence-min 0.85
                             :level-floor 0.003 :curve :linear :feature :pitch :max-tail {:beats 8}
                             :silence-hold-ms 1200 :max-tail-ms 15000} lane)
                     _ (require! (and (string? id) (re-matches #"[a-zA-Z0-9_-]{1,48}" id)
                                      (every? #(and (integer? %) (<= 1 % channels)) [send-channel return-channel])
                                      (vector? frequency-range) (= 2 (count frequency-range))
                                      (every? time/finite-number? frequency-range)
                                      (< 0 (first frequency-range) (last frequency-range) (/ sample-rate 2))
                                      (<= (/ (* 2.1 sample-rate) (first frequency-range)) block-frames)
                                      (every? time/finite-number? [amplitude confidence-min level-floor silence-hold-ms max-tail-ms])
                                      (< 0 amplitude) (<= amplitude 1) (<= 0 confidence-min 1)
                                      (< 0 level-floor (/ amplitude (Math/sqrt 2)))
                                      (<= 1 silence-hold-ms max-tail-ms 120000)
                                      (#{:pitch :level :texture} feature)
                                      (empty? (time/validate-duration max-tail))
                                      (pos? (-> max-tail first val)))
                                 "Invalid carrier lane geometry, analysis window, thresholds or tail duration")]
                 (tween/curve curve 0.5)
                 (assoc lane :dry-bounds (:bounds (schema model dry-cue dry-param))
                             :wet-schema (select-keys (schema model wet-cue wet-param) [:bounds :type]))))
             lanes))))

(defn dry-instance [model cue-id]
  (reduce (fn [selected instance]
            (if (and (= cue-id (:cue-id instance)) (= :running (:phase instance))
                     (not= :audio-return (get-in instance [:source :adapter]))
                     (> (:order instance) (:order selected -1)))
              instance selected)) nil (vals (:instances model))))

(defn- options [lane sample-rate]
  {:sample-rate sample-rate :frequency-range (:frequency-range lane)
   :bounds (:dry-bounds lane) :amplitude (:amplitude lane)
   :noise-floor (:level-floor lane)})

(defn render-dry
  "The writer owns oscillator continuity. Fade the final block on source release.
  A new activation receives a new identity even when it plays the same note."
  [model lane previous drained-id sample-rate sample-count]
  (let [instance (dry-instance model (:dry-cue lane))
        prior-binding (:binding previous)
        waiting? (and prior-binding (not= prior-binding (:id instance)) (not= prior-binding drained-id))
        value (when-not waiting? (get-in instance [:params (:dry-param lane)]))
        value (when (time/finite-number? value) value)
        reference (or value (:value previous) (first (:dry-bounds lane)))
        opts (cond-> (options lane sample-rate) (nil? value) (assoc :amplitude 0.0))
        result (if (or value (pos? (get-in previous [:oscillator :amplitude] 0.0)))
                 (dsp/sine-block opts (or (:oscillator previous) {:amplitude 0.0}) reference sample-count)
                 {:samples (double-array sample-count) :state (:oscillator previous)})]
    {:samples (:samples result)
     :state {:oscillator (:state result) :value reference
             :binding (if waiting? prior-binding (:id instance)) :source-id (when (some? value) (:id instance))}}))

(defn analyze-return
  "Run on a PCM worker. A return window keeps the captured dry activation through
  silence gaps, until its musical deadline, physical maximum or silence timeout."
  [model lane previous dry samples sample-rate sample-time-ms]
  (let [dry-id (:source-id dry)
        old-id (or (:source-id previous)
                   (when (not= (:binding dry) (:retired-id previous)) (:binding dry)))
        id (or dry-id old-id)
        clock (time/frame-clock {:time-ms (:time-ms model) :transport (:transport model)})
        starting? (and old-id (nil? dry-id) (nil? (:tail previous)))
        tail (when-not dry-id
               (or (:tail previous)
                   (when starting?
                     (dsp/new-tail {:show-id (:show-id model) :epoch (:epoch model)
                                    :instance-id old-id :param (:dry-param lane)}
                                   sample-time-ms {:bounds (:dry-bounds lane) :max-tail-ms (:max-tail-ms lane)
                                                   :silence-hold-ms (:silence-hold-ms lane)
                                                   :noise-floor (:level-floor lane) :confidence-min (:confidence-min lane)}))))
        timer (when tail (or (:timer previous) (time/start (:max-tail lane) clock)))
        opts (cond-> (options lane sample-rate)
               (:value dry) (assoc :reference-frequency (dsp/value->frequency (:value dry) (options lane sample-rate))))
        features (dsp/analyze-block samples opts)
        tail-result (when tail (dsp/advance-tail tail features sample-time-ms (select-keys model [:show-id :epoch])))
        active? (and id (or dry-id (and (:active? tail-result)
                                        (not (:done? (time/sample timer clock))))))
        ;; The amount of a quiet echo follows the return gain independently of pitch.
        level (min 1.0 (/ (* (Math/sqrt 2.0) (:rms features)) (:amplitude lane)))
        confident? (and (:value features) (>= (:pitch-confidence features) (:confidence-min lane)))
        [low high] (:dry-bounds lane)
        normalized (case (:feature lane)
                     :pitch (when confident? (/ (- (:value features) low) (- high low)))
                     :texture (:spectral-residual features)
                     :level level)
        {:keys [bounds type]} (:wet-schema lane)
        value (when normalized
                (let [x (tween/interpolate (first bounds) (last bounds) (tween/curve (:curve lane) normalized))]
                  (if (= type :integer) (Math/round (double x)) x)))
        audible? (> (:rms features) (:level-floor lane))]
    {:state (if active? {:source-id id :timer timer :tail (:tail tail-result)}
               {:retired-id (or id (:retired-id previous))})
     :drained-id (when (and (not active?) (not audible?)) (or id (:retired-id previous)))
     :window {:source-id (when active? id)
              :value (when (and active? audible?) value) :opacity level
              :features (select-keys features [:rms :pitch-hz :pitch-confidence :spectral-residual])}}))

(defn- command [model command time-ms session]
  (control/apply-command model (merge {:show-id (:show-id model) :epoch (:epoch model)
                                       :source {:adapter :audio-return :session-id session}
                                       :command-id (str session "/" (:epoch model) "/" (:revision model) "/" (name (:op command)))
                                       :time-ms time-ms} command)))

(defn- remove-voice [model lane-id time-ms]
  (let [{:keys [instance-id session]} (get-in model [:variables :audio-return-voices lane-id])
        owner (get-in model [:instances instance-id :source])
        model (if (and (= :audio-return (:adapter owner)) (= session (:session-id owner)))
                (:model (command model {:op :instance/remove :instance-id instance-id} time-ms session))
                model)]
    (update-in model [:variables :audio-return-voices] dissoc lane-id)))

(defn disarm
  "Discard derived wet voices before rekeying saved activations, retaining patch settings."
  [model]
  (if-let [voices (seq (get-in model [:variables :audio-return-voices]))]
    (update (reduce #(remove-voice %1 (key %2) (:time-ms model)) model voices)
            :variables dissoc :audio-return-voices)
    model))

(defn- update-voice [model lane window session time-ms]
  (let [{:keys [id wet-cue wet-param]} lane
        {:keys [value opacity source-id sequence]} window
        old (get-in model [:variables :audio-return-voices id])
        same? (and (= session (:session old)) (= source-id (:source-id old))
                   (= {:adapter :audio-return :session-id session}
                      (get-in model [:instances (:instance-id old) :source])))
        model (if (and old (or (nil? value) (not same?))) (remove-voice model id time-ms) model)]
    (cond
      (nil? value) model
      (and same? (= sequence (:sequence old))) model
      same?
      (let [instance-id (:instance-id old)
            current (get-in model [:instances instance-id])
            model (if (= value (get-in current [:params wet-param])) model
                    (:model (command model {:op :param/set :instance-id instance-id :param wet-param :value value} time-ms session)))
            model (if (= opacity (get-in current [:system :opacity])) model
                    (:model (command model {:op :instance/opacity-set :instance-id instance-id :value opacity} time-ms session)))]
        (assoc-in model [:variables :audio-return-voices id :sequence] sequence))
      :else
      (let [result (command model {:op :cue/launch :cue-id wet-cue
                                   :launch {:mode :persistent :params {wet-param value} :system {:opacity opacity}}} time-ms session)]
        (if-let [instance-id (get-in result [:ack :instance-id])]
          (assoc-in (:model result) [:variables :audio-return-voices id]
                    {:instance-id instance-id :session session :source-id source-id :sequence sequence})
          model)))))

(defn- observations [lane window session time-ms]
  (let [{:keys [id frequency-range]} lane
        {:keys [features sequence]} window]
    (for [[key rng] [[:rms [0 1]] [:pitch-hz frequency-range] [:pitch-confidence [0 1]] [:spectral-residual [0 1]]]
          :let [value (get features key)
                voiced? (and value (or (not= :pitch-hz key) (>= (:pitch-confidence features) (:confidence-min lane))))]]
      {:type :sync/observation
       :observation {:kind :audio :source-id (str "live-audio/" id "/" (name key))
                     :event-id (str session "/" sequence "/" id "/" (name key))
                     :origin :processed-audio :observed-at-ms time-ms
                     :gate? (boolean (and voiced? (pos? value))) :value (if voiced? value 0.0)
                     :confidence (if (= :pitch-hz key) (:pitch-confidence features) 1.0)
                     :port {:signal key :range rng :transport :audio-return}}})))

(defn advance
  "Consume the latest analyzed windows at the frame boundary. An unplugged or
  stopped stream expires its own output on the next frame after the lease."
  [model {:keys [session config settings packet]} time-ms]
  (let [{:keys [windows fresh? observed-at-ms]} packet
        valid? (and session (= settings (get-in model [:settings :audio-returns]))
                    (= (:epoch model) (:epoch packet)) (= (:show-id model) (:show-id packet))
                    (number? observed-at-ms) (<= (- time-ms observed-at-ms) 500))
        model (if valid? model (disarm model))]
    (if-not valid? model
      (reduce (fn [m lane]
                (let [window (get windows (:id lane))
                      m (if (and fresh? window)
                          (routing/admit-sync m (observations lane window session observed-at-ms)) m)]
                  (let [current-id (:id (dry-instance m (:dry-cue lane)))]
                    (update-voice m lane (when (or (nil? current-id) (= current-id (:source-id window))) window)
                                  session time-ms))))
              model (:lanes config)))))
