(ns tolglow.waveforms
  "Read-only signal projections and bounded captured PCM summaries. Neither
  path advances an effect, consumes RNG, reads a clock or opens a device."
  (:require [afterglow.control.system :as system]
            [afterglow.render.ir :as ir]
            [afterglow.time :as time]
            [tolglow.composition.response :as response]
            [tolglow.controllers.parameter-display :as parameter-display]
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
        result (sample-signal waveforms selection params options environment)]
    (assoc result :identity {:epoch (:epoch model) :instance-id instance-id :parameter parameter
                             :definition-id (:effect-id instance) :definition-version (:effect-version instance)
                             :waveform-revision (:revision waveforms) :params-digest (str (hash params))
                             :selection-digest (str (hash selection)) :context-digest (str (hash environment)) :options options})))

(defn create-cache [] (atom {:entries {} :order []}))
(defn cached-preview
  "Bounded off-deadline cache. Exact immutable inputs are keys; the public digest
  is only a UI identity, never a cache correctness decision."
  [cache snapshot request]
  (let [model (:model snapshot) instance (get-in model [:instances (:instance-id request)])
        clock-bound? (some #(and (map? %) (#{:time :beat :bar :phrase} (:op %)))
                          (tree-seq coll? seq [(get-in instance [:definition :params]) request]))
        key [(select-keys model [:epoch :waveforms])
             (select-keys instance [:id :effect-id :effect-version :definition])
             (when instance (effective-params snapshot instance))
             (when clock-bound? (signal-environment model instance)) (dissoc request :input-key)]]
    (or (get-in @cache [:entries key])
        (let [result (preview snapshot request)]
          (swap! cache (fn [{:keys [entries order]}]
                         (let [order (vec (take-last 64 (conj (filterv #(not= key %) order) key)))]
                           {:order order :entries (assoc (select-keys entries order) key result)})))
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
                  (parameter-display/readings snapshot instance view (fn [_] @inputs)))]
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
                        :source (:source clock) :epoch (:epoch clock)})
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
  [state {:keys [samples sample-rate channels source-id started-at-ms clock timing-valid? discontinuity?]
          :or {timing-valid? true channels 1}}]
  (let [^doubles samples samples n (alength samples)
        _ (when-not (and (registry/finite-number? sample-rate) (<= 8000 sample-rate 192000)
                         (registry/finite-number? started-at-ms) (<= 0 started-at-ms) (#{1 2} channels))
            (fail! "Invalid captured waveform format or timestamp" {}))
        end (+ started-at-ms (* 1000.0 (/ n sample-rate)))
        changed? (or discontinuity? (not= [source-id sample-rate channels] (:format state))
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
    (assoc result :format [source-id sample-rate channels] :source-id source-id :sample-rate sample-rate :channels channels
           :end-time-ms end :timing-valid? timing-valid? :sample-end (+ (:sample-end state) n)
           :markers (if timing-valid?
                      (vec (take-last 256 (filter #(<= first-time (:time-ms %) end) (concat (:markers state) markers)))) []))))

(defn capture-view [state]
  (let [bins (cond-> (:bins state) (:partial state) (conj (assoc (column (:partial state)) :partial? true)))
        bins (vec (take-last (:max-bins state) bins))
        start (:start-time-ms (first bins)) end (:end-time-ms (last bins))]
    {:bins bins :duration-ms (if start (- end start) 0.0) :sample-rate (:sample-rate state)
     :channels (:channels state) :mix :mono-average :source-id (:source-id state)
     :start-time-ms start :end-time-ms end :generation (:generation state) :sample-end (:sample-end state)
     :timing-valid? (:timing-valid? state) :available? (boolean (seq bins))
     :beat-markers (if start (mapv #(assoc % :offset-ms (- (:time-ms %) start))
                                   (filter #(<= start (:time-ms %) end) (:markers state))) [])}))
