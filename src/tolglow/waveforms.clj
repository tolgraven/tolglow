(ns tolglow.waveforms
  "Read-only signal projections and bounded captured PCM summaries. Neither
  path advances an effect, consumes RNG, reads a clock or opens a device."
  (:require [afterglow.control.system :as system]
            [afterglow.render.ir :as ir]
            [afterglow.time :as time]
            [tolglow.audio.pcm :as pcm]
            [tolglow.composition.response :as response]
            [tolglow.controllers.parameter-display :as parameter-display]
            [tolglow.effects :as fx]
            [tolglow.oscillator :as oscillator]
            [tolglow.registry :as registry]
            [tolglow.signals :as signals]))

(defn- fail! [message data] (throw (ex-info message (assoc data :type ::invalid-projection))))

(defn- resolve-key [entries key]
  (cond (contains? entries key) key
        (and (string? key) (contains? entries (keyword key))) (keyword key)
        (and (keyword? key) (contains? entries (subs (str key) 1))) (subs (str key) 1)
        :else key))

(defn- instance-clock [model instance]
  (or (get-in instance [:system :last-clock])
      (system/private-clock instance {:time-ms (:time-ms model) :transport (:transport model)})))

(defn- signal-environment [model instance]
  (when instance
    (let [clock (instance-clock model instance)]
      {:time-ms (:time-ms clock) :elapsed-ms (max 0 (- (:time-ms clock) (:started-at-ms instance 0)))
       :beat (:beat clock)
       :elapsed-beats (when (and (number? (:beat clock)) (number? (:started-at-beat instance)))
                        (max 0 (- (:beat clock) (:started-at-beat instance))))
       :meter (time/normalize-meter (:timing-meter instance))})))

(defn resolved-inputs
  "Resolve one committed snapshot's modulation and response inputs. Compile the
  response chain once for all projected instances, respecting private clocks."
  ([snapshot] (resolved-inputs snapshot response/compile-middleware))
  ([{:keys [model resolved-param-overrides modulation-view]} compile-responses]
  (let [overrides (or resolved-param-overrides (:overrides modulation-view))
        responses (filterv #(not= false (:enabled? %)) (:responses model []))
        middleware (if (seq responses) (compile-responses responses) identity)]
    (fn [instance]
      (middleware {:instance (update instance :params merge (get overrides (:id instance)))
                   :frame (instance-clock model instance)})))))

(defn effective-params [snapshot instance]
  (get-in ((resolved-inputs snapshot) instance) [:instance :params]))

(defn- normalized-selection [waveforms selection]
  (if (map? selection)
    (update selection :choices #(mapv (fn [choice]
                                       (if (map? choice)
                                         (update choice :id (partial resolve-key (:waveforms waveforms)))
                                         (resolve-key (:waveforms waveforms) choice))) %))
    (resolve-key (:waveforms waveforms) selection)))

(defn- expression-nodes [expression]
  (tree-seq #(seq (:args %)) :args expression))

(defn- phase-path [node placeholder]
  (if (= node placeholder) []
      (some (fn [[index child]]
              (when-let [path (phase-path child placeholder)]
                (into [:args index] path)))
            (map-indexed vector (:args node)))))

(defn- selector-phase [waveforms instance parameter choices options]
  (let [{:keys [outputs blocks]} (:definition instance)
        placeholder (fx/param ::phase)
        signal #(signals/select-waveform waveforms (fx/param parameter) choices (assoc options :phase %))
        pattern (signal placeholder)
        values (if (map? outputs) (vals outputs) (map :value outputs))]
    (when-let [path (and (empty? blocks) (phase-path pattern placeholder))]
      (let [matches (into #{} (comp (filter #(= (:op pattern) (:op %)))
                                     (keep (fn [node]
                                             (when-let [phase (get-in node path)]
                                               (when (and (map? phase) (= node (signal phase))) phase)))))
                          (mapcat expression-nodes values))]
        (when (= 1 (count matches)) (first matches))))))

(defn- musical-coordinate [node]
  (when-let [unit ({:beat :beats :bar :bars :phrase :phrases} (:op node))]
    (when (#{:elapsed :absolute} (:clock node :elapsed)) unit)))

(defn- phase-timing [expression environment transport paused?]
  (let [[division offset] (:args expression)
        [coordinate duration] (:args division)]
    (cond
      (= :oscillator-phase (:op expression))
      (let [reference (:phase-reference expression :launch)
            meter (if (= :transport reference) (:meter transport) (:meter environment))]
        (when (and (oscillator/musical-period? (:period expression))
                   (not (and paused? (= :transport reference))))
          {:phase (oscillator/phase expression transport (:elapsed-beats environment) (:meter environment))
           :period (time/duration->beats (:period expression) meter)}))

      (musical-coordinate expression)
      {:phase ((ir/compile-expression expression) environment)
       :period (time/duration->beats {(musical-coordinate expression) 1} (:meter environment))}

      (and (= :add (:op expression)) (= :divide (:op division))
           (= :const (:op offset)) (= :const (:op duration))
           (musical-coordinate coordinate) (time/finite-number? (:value duration)) (pos? (:value duration))
           (= expression (fx/cycles {(musical-coordinate coordinate) (:value duration)}
                                    {:clock (:clock coordinate :elapsed) :phase (:value offset)})))
      {:phase ((ir/compile-expression expression) environment)
       :period (time/duration->beats {(musical-coordinate coordinate) (:value duration)} (:meter environment))})))

(defn- musical-ticks [transport beat span]
  (let [{:keys [beats-per-bar bars-per-phrase]} (time/normalize-meter (:meter transport))
        bar-origin (:bar-origin-beat transport 0.0)
        phrase-origin (:phrase-origin-beat transport bar-origin)
        boundaries (fn [origin spacing kind]
                     (let [start (- origin beat)
                           first-offset (- (mod (+ start span) spacing) span)]
                       (for [index (range 65)
                             :let [offset (+ first-offset (* index spacing))]
                             :while (<= offset span)]
                         [(double (/ offset span)) kind])))]
    (mapv (fn [[position kind]] {:position position :kind kind})
          (sort-by first (into {} (concat (boundaries 0 1 :beat)
                                          (boundaries bar-origin beats-per-bar :bar)
                                          (boundaries phrase-origin (* beats-per-bar bars-per-phrase) :phrase)))))))

(defn- jump-phases [waveforms {:keys [selector choices]} environment]
  (let [index (long (min (dec (count choices)) (max 0 (Math/floor (+ selector 0.5)))))
        {:keys [id version params]} (nth choices index)
        {:keys [discontinuities] :as definition} (get-in waveforms [:versions [id version]])]
    (keep (fn [{:keys [channel phase phase-param]}]
            (when (= :value channel)
              (let [value (if phase-param
                            ((ir/compile-expression
                              (fx/expression (get params phase-param (get-in definition [:params phase-param :default]))))
                             environment)
                            phase)]
                (when (and (time/finite-number? value) (<= 0 value 1)) (mod (double value) 1.0)))))
          (take 16 discontinuities))))

(defn- timeline [waveforms {:keys [model transport-generation]} instance parameter selection params environment]
  (let [{:keys [transport]} model
        beat (:beat transport)
        phase-expression (selector-phase waveforms instance parameter (:choices selection) {})
        paused? (get-in instance [:system :paused?])]
    (when (and phase-expression (time/finite-number? beat) (<= -1.0e12 beat 1.0e12))
      (when-let [{:keys [phase period]} (phase-timing phase-expression environment transport paused?)]
        (let [span (:beats-per-bar (time/normalize-meter (:meter transport)))
              radius (/ span period)
              low (- phase radius) high (+ phase radius)]
          (when (and (every? time/finite-number? [low high radius]) (<= -1.0e12 low phase high 1.0e12)
                     (< low phase high) (<= radius 32))
            (let [environment (assoc environment :params params)
                  evaluate (ir/compile-expression
                            (signals/select-waveform waveforms (:selector selection) (:choices selection)
                                                     {:phase (fx/param :preview/phase)}))
                  sample #(evaluate (assoc-in environment [:params :preview/phase] %))
                  grid (map (fn [index] (+ phase (* radius (/ index 128.0)))) (range -128 129))
                  edges (for [jump (jump-phases waveforms selection environment)
                              cycle (range (long (Math/ceil (- low jump))) (inc (long (Math/floor (- high jump)))))
                              :let [phase (+ cycle jump)]] phase)
                  edges (sort (distinct (concat [low high] edges)))
                  sides (mapcat (fn [[left right]]
                                  (let [epsilon (min 1.0e-7 (* 0.25 (- right left)))]
                                    [(+ left epsilon) (- right epsilon)])) (partition 2 1 edges))
                  samples (into (sorted-map)
                                (map (fn [coordinate]
                                       [(/ (- coordinate phase) radius) (sample coordinate)]))
                                (concat grid edges sides))]
              ;; A long pulse can stay flat for the whole window.
              (when (<= 2 (count samples) 2048)
                {:beat beat :now-value (sample phase) :transport-generation (or transport-generation 0)
                 :transport (select-keys transport [:source :source-epoch :source-selection-epoch :playing? :meter])
                 :ticks (musical-ticks transport beat span)
                 :samples (mapv (fn [[position value]] {:phase position :value value}) samples)}))))))))

(defn- sample-signal [waveforms selection params options environment]
  (let [n (:samples options 129) cycles (:cycles options 1.0) start (:start-phase options 0.0)]
    (when-not (and (integer? n) (<= 2 n 2048)
                   (registry/finite-number? cycles) (pos? cycles)
                   (registry/finite-number? start) (registry/finite-number? (+ start cycles)))
      (fail! "Preview needs 2..2048 samples over a finite positive cycle interval" {}))
    (when-not (and (map? params) (<= (count params) 128) (registry/data? params))
      (fail! "Preview context must contain at most 128 portable parameter values" {}))
    (let [options (assoc options :phase {:op :param :key :preview/phase})
          expressions (if (map? selection)
                        (signals/selected-channels waveforms (:selector selection) (:choices selection) options)
                        (signals/channels waveforms selection options))
          compiled (mapv (fn [[channel expression]] [channel (ir/compile-expression expression)]) expressions)
          rows (mapv (fn [index]
                       (let [phase (+ (double start) (* (double cycles) (/ (double index) (dec n))))
                             env (assoc environment :params (assoc params :preview/phase phase))]
                         (into {:phase phase}
                               (map (fn [[channel evaluate]]
                                      (let [value (evaluate env)]
                                        (when-not (registry/finite-number? value)
                                          (fail! "Signal preview returned a non-finite value" {:channel channel :phase phase}))
                                        [channel value]))) compiled))) (range n))]
      {:samples rows :preview-samples (mapv :value rows) :units signals/units
       :window (:window options (/ 1.0 1024.0)) :angle-span-deg (:angle-span-deg options 360.0)
       :selection selection :start-phase start :cycles cycles :preview-status :ready})))

(defn preview
  "Project a live captured waveform selector or an explicit draft selection.
  Live inputs always come from the host snapshot, never client context values."
  [snapshot {:keys [instance-id parameter selection id context-params options] :or {options {}}}]
  (let [model (:model snapshot) waveforms (or (:waveforms model) (signals/new-registry))
        instance (when instance-id (get-in model [:instances instance-id]))
        _ (when (and instance-id (nil? instance)) (fail! "Waveform instance is no longer active" {:instance-id instance-id}))
        parameter (when instance (resolve-key (get-in instance [:definition :params]) parameter))
        schema (get-in instance [:definition :params parameter])
        _ (when (and instance (not= :waveform-selector (:semantic/type schema)))
            (fail! "Parameter is not a registered waveform selector" {:parameter parameter}))
        params (if instance (effective-params snapshot instance) (or context-params {}))
        selection (normalized-selection waveforms
                                         (if instance {:selector (get params parameter) :choices (:choices schema)}
                                             (or selection id)))
        environment (signal-environment model instance)
        result (sample-signal waveforms selection params options environment)
        timing (when (and instance (true? (:live-timing? options)))
                 (try (timeline waveforms snapshot instance parameter selection params environment)
                      (catch clojure.lang.ExceptionInfo _ nil)))]
    (cond-> (assoc result :identity {:epoch (:epoch model) :instance-id instance-id :parameter parameter
                             :definition-id (:effect-id instance) :definition-version (:effect-version instance)
                             :waveform-revision (:revision waveforms) :params-digest (str (hash params))
                             :selection-digest (str (hash selection)) :context-digest (str (hash environment)) :options options})
      timing (assoc :timeline timing))))

(defn create-cache [] (atom {:entries {} :order []}))

(defn cached-preview
  "Bounded off-deadline cache. Exact immutable inputs are keys; the public digest
  is only a UI identity, never a cache correctness decision."
  [cache snapshot request]
  (let [model (:model snapshot) instance (get-in model [:instances (:instance-id request)])
        clock-bound? (some #(and (map? %) (#{:time :beat :bar :phrase} (:op %)))
                          (tree-seq coll? seq [(get-in instance [:definition :params]) request]))
        live-timing? (and instance (true? (get-in request [:options :live-timing?])))
        key [(select-keys model [:epoch :waveforms])
             (select-keys instance [:id :effect-id :effect-version :definition])
             (when instance (effective-params snapshot instance))
             (when (or clock-bound? live-timing?) (signal-environment model instance))
             (when live-timing? [(:transport model) (:transport-generation snapshot)]) (dissoc request :input-key)]]
    (or (get-in @cache [:entries key])
        (let [result (preview snapshot request)]
          (swap! cache (fn [{:keys [entries order] :as state}]
                         (let [order (vec (take-last 64 (conj (filterv #(not= key %) order) key)))]
                           (assoc state :order order :entries (assoc (select-keys entries order) key result)))))
          result))))

(defn display-audio
  "Compact measured columns for the bounded Push IPC packet. Adjacent columns
  retain extrema and sample-weighted RMS; there is no synthetic resampling."
  [waveform]
  (when waveform
    (let [bins (:bins waveform)
          width (max 1 (long (Math/ceil (/ (count bins) 128.0))))
          combine (fn [columns]
                    (let [n (reduce + (map #(:samples % 1) columns))
                          rms (fn [value] (Math/sqrt (/ (reduce + (map #(* (:samples % 1) (Math/pow (double (value %)) 2)) columns)) n)))]
                      {:min (reduce min (map :min columns)) :max (reduce max (map :max columns))
                       :rms (rms :rms)
                       :bands (into {} (map (fn [band] [band (rms #(get-in % [:bands band] 0.0))])) [:low :mid :high])}))]
      (assoc waveform :bins (mapv combine (partition-all width bins))))))

(defn project-display
  "Enrich a queued device view on its capture worker. A stale selection cannot
  display newer-instance data under an old label. Audio data is already bounded."
  [cache snapshot view audio-waveform]
  (let [instance (get-in snapshot [:model :instances (:instance-id view)])
        current? (and instance (= (:epoch view) (get-in snapshot [:model :epoch]))
                      (= (:definition-version view) (:effect-version instance)))
        model (:model snapshot)
        config (get-in model [:settings :controllers :push-display])
        stale? (and (:instance-id view) (not current?))
        clock (when-not stale?
                (assoc (select-keys (:visualization-clock view) [:sample-period-ms])
                       :time-ms (:time-ms model) :beat (get-in model [:transport :beat])))
        view (merge {:display-mode (:mode config :parameters)
                     :parameter-view (:parameter-view config :auto)
                     :history-beats (:history-beats config 4)} view)
        visible? (and current? (= :parameters (:display-mode view)) (not (get-in view [:target-picker :open?])))
        inputs (delay ((resolved-inputs snapshot response/monitor-middleware) instance))
        overrides (or (:resolved-param-overrides snapshot) (get-in snapshot [:modulation-view :overrides]))
        displayed (if (and current? (= :audio (:display-mode view)) (seq (:responses model)))
                    (get-in @inputs [:instance :params])
                    (merge (:params instance) (get overrides (:id instance))))
        reading (when (and visible? (seq (:parameters view)))
                  (parameter-display/readings cache snapshot instance view (fn [_] @inputs)))]
    (cond-> (update (assoc view :visualization-clock clock :runtime-id (or (:runtime-id view) (:runtime-id snapshot) (:show-id model))) :parameters
                    (fn [parameters]
                      (mapv (fn [parameter]
                              (let [id (:param parameter (:id parameter))
                                    parameter (cond-> parameter
                                                stale? (dissoc :value :base-value :effective-value :source-state :source-types :source-error :sample-fixture)
                                                current?
                                                (assoc :value (get-in instance [:params id])
                                                       :base-value (get-in instance [:params id])
                                                       :effective-value (get displayed id))
                                                reading (merge (reading id)))]
                                (if (= :waveform-selector (get-in parameter [:schema :semantic/type]))
                                  (if current?
                                    (try (let [result (cached-preview cache snapshot
                                                                    {:instance-id (:id instance) :parameter id
                                                                     :options {:samples 65}})]
                                           (merge parameter (select-keys result [:preview-samples :preview-status :identity])))
                                         (catch Exception error
                                           (assoc parameter :preview-samples [] :preview-status :unavailable
                                                  :preview-message (.getMessage error))))
                                    (assoc parameter :preview-samples [] :preview-status :stale)) parameter))) parameters)))
      audio-waveform (assoc :audio-waveform (display-audio audio-waveform)))))

(defn new-capture
  "Explicit streaming summary state. Defaults retain 256 columns of 20ms each.
  Raw samples and mutable arrays are never retained."
  ([] (new-capture {}))
  ([{:keys [max-bins bin-ms] :or {max-bins 256 bin-ms 20.0}}]
   (when-not (and (integer? max-bins) (<= 1 max-bins 512)
                  (registry/finite-number? bin-ms) (<= 1 bin-ms 100))
     (fail! "Capture projection requires 1..512 bins and 1..100ms columns" {}))
   {:max-bins max-bins :bin-ms (double bin-ms) :bins [] :markers []
    :generation 0 :sample-end 0 :filters [0.0 0.0] :partial nil}))

(defn- beat-markers [clock start end]
  (if (and (map? clock) (not= false (if (contains? clock :playing?) (:playing? clock) (:running? clock)))
           (every? registry/finite-number? ((juxt :time-ms :beat :bpm) clock))
           (< 0 (:bpm clock) 1000))
    (let [rate (/ (:bpm clock) 60000.0) anchor (:beat clock)
          a (+ anchor (* (- start (:time-ms clock)) rate))
          b (+ anchor (* (- end (:time-ms clock)) rate))
          bar (:beats-per-bar (time/normalize-meter (:meter clock)))
          bar-origin (:bar-origin-beat clock 0.0)
          quarter (take 256 (range (long (Math/ceil a)) (long (Math/ceil b))))
          bars (map #(+ bar-origin (* % bar))
                    (take 256 (range (long (Math/ceil (/ (- a bar-origin) bar)))
                                     (long (Math/ceil (/ (- b bar-origin) bar))))))
          bar-set (set bars)]
      (mapv (fn [beat] {:time-ms (+ (:time-ms clock) (/ (- beat anchor) rate))
                        :beat beat :bar? (boolean (some #(< (Math/abs (double (- beat %))) 1.0e-8) bar-set))
                        :source (:source clock) :epoch (get clock :source-epoch (:epoch clock))})
            (sort (distinct (map double (concat quarter bars)))))) []))

(defn- column [partial]
  (let [n (:n partial)]
    {:min (:min partial) :max (:max partial) :rms (Math/sqrt (/ (:squares partial) n))
     :bands (into {} (map (fn [[k sum]] [k (min 1.0 (Math/sqrt (/ sum n)))])) (:bands partial))
     :start-time-ms (:start partial) :end-time-ms (:end partial) :samples n}))

(defn capture-block
  "Summarize actual mono PCM with continuous 200Hz/2kHz crossover filters.
  Min/max/RMS are measured, band channels are low/mid/high RMS, not mood labels.
  Gaps reset filters/history; no samples or beat markers are fabricated."
  [state {:keys [samples sample-rate channels input-channel source-id started-at-ms clock timing-valid? discontinuity?]
          :or {timing-valid? true channels 1}}]
  (let [^doubles samples samples n (alength samples)
        _ (when-not (and (registry/finite-number? sample-rate) (<= 8000 sample-rate 192000)
                         (registry/finite-number? started-at-ms) (<= 0 started-at-ms)
                         (pcm/valid-layout? channels input-channel))
            (fail! "Invalid captured waveform format or timestamp" {}))
        format [source-id sample-rate channels input-channel]
        end (+ started-at-ms (* 1000.0 (/ n sample-rate)))
        changed? (or discontinuity? (not= format (:format state))
                     (and (:end-time-ms state) (> (Math/abs (double (- started-at-ms (:end-time-ms state)))) 1.0e-5)))
        state (if changed? (assoc state :bins [] :markers [] :filters [0.0 0.0] :partial nil
                                  :generation (inc (:generation state)) :sample-end 0) state)
        size (max 1 (long (Math/round (* sample-rate (/ (:bin-ms state) 1000.0)))))
        low-a (- 1.0 (Math/exp (/ (* -2 Math/PI 200.0) sample-rate)))
        high-a (- 1.0 (Math/exp (/ (* -2 Math/PI 2000.0) sample-rate)))
        result
        (loop [index 0 low (double (first (:filters state))) upper (double (second (:filters state)))
               partial (:partial state) bins (:bins state)]
          (if (= index n)
            (assoc state :filters [low upper] :partial partial :bins bins)
            (let [partial (or partial {:n 0 :min 0.0 :max 0.0 :squares 0.0
                                      :bands {:low 0.0 :mid 0.0 :high 0.0}
                                      :start (+ started-at-ms (* 1000.0 (/ index sample-rate)))})
                  limit (min n (+ index (- size (:n partial))))
                  segment (loop [i index lo low hi upper minimum (double (:min partial)) maximum (double (:max partial))
                                 squares (double (:squares partial)) ls (double (get-in partial [:bands :low]))
                                 ms (double (get-in partial [:bands :mid])) hs (double (get-in partial [:bands :high]))]
                            (if (= i limit) [lo hi minimum maximum squares ls ms hs]
                              (let [x (aget samples i)
                                    _ (when-not (and (Double/isFinite x) (<= -1.0 x 1.0))
                                        (fail! "Captured samples must be finite normalized PCM" {:index i}))
                                    lo (+ lo (* low-a (- x lo))) hi (+ hi (* high-a (- x hi)))
                                    mid (- hi lo) high (- x hi)]
                                (recur (inc i) lo hi (min minimum x) (max maximum x) (+ squares (* x x))
                                       (+ ls (* lo lo)) (+ ms (* mid mid)) (+ hs (* high high))))))
                  [low upper minimum maximum squares ls ms hs] segment
                  partial (assoc partial :n (+ (:n partial) (- limit index)) :min minimum :max maximum :squares squares
                                 :bands {:low ls :mid ms :high hs} :end (+ started-at-ms (* 1000.0 (/ limit sample-rate))))
                  full? (= size (:n partial))]
              (recur limit (double low) (double upper) (when-not full? partial)
                     (if full? (vec (take-last (:max-bins state) (conj bins (column partial)))) bins)))))
        first-time (or (:start-time-ms (first (:bins result))) (get-in result [:partial :start]) started-at-ms)
        markers (if timing-valid? (beat-markers clock started-at-ms end) [])]
    (assoc result :format format :source-id source-id :sample-rate sample-rate :channels channels
           :input-channel input-channel
           :end-time-ms end :timing-valid? timing-valid? :sample-end (+ (:sample-end state) n)
           :markers (if timing-valid?
                      (vec (take-last 256 (filter #(<= first-time (:time-ms %) end) (concat (:markers state) markers)))) []))))

(defn capture-view [state]
  (let [bins (cond-> (:bins state) (:partial state) (conj (assoc (column (:partial state)) :partial? true)))
        bins (vec (take-last (:max-bins state) bins))
        start (:start-time-ms (first bins)) end (:end-time-ms (last bins))
        input-channel (:input-channel state)]
    (cond-> {:bins bins :duration-ms (if start (- end start) 0.0) :sample-rate (:sample-rate state)
             :channels (:channels state) :mix (if input-channel :selected-channel :mono-average) :source-id (:source-id state)
             :start-time-ms start :end-time-ms end :generation (:generation state) :sample-end (:sample-end state)
             :timing-valid? (:timing-valid? state) :available? (boolean (seq bins))
             :beat-markers (if start (mapv #(assoc % :offset-ms (- (:time-ms %) start))
                                           (filter #(<= start (:time-ms %) end) (:markers state))) [])}
      input-channel (assoc :input-channel input-channel))))
