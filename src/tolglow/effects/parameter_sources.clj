(ns tolglow.effects.parameter-sources
  "Musical waveform inputs attached to one running cue. Only declarations live
  on instances. Compiled waveform functions belong to the frame owner."
  (:require [afterglow.control.instance-order :as instance-order]
            [afterglow.control.params :as params]
            [afterglow.control.system :as system]
            [afterglow.render.ir :as ir]
            [afterglow.time :as time]
            [tolglow.audio.shape :as audio-shape]
            [tolglow.audio-lfo :as audio-lfo]
            [tolglow.modulation :as modulation]
            [tolglow.oscillator :as oscillator]
            [tolglow.signals :as signals]))

(def ^:private builtins (delay (signals/new-registry)))

(defn- waveforms [model] (or (:waveforms model) @builtins))

(defn- shape-glyph
  "The picker glyph of a waveform, sampled from its registered definition. A
  waveform that cannot be sampled has no glyph and its picker stays text."
  [registry {:keys [id version]}]
  (try (signals/glyph registry id {:version version})
       (catch Exception _ nil)))

(defn- options [registry]
  (->> (:waveforms registry) vals (sort-by (comp str :id))
       (mapv #(assoc (select-keys % [:id :name :range :params :version]) :glyph (shape-glyph registry %)))))

(def ^:private builtin-options (delay (options @builtins)))

;; The catalog is projected on every snapshot. A registry is immutable data, so
;; the glyphs of the last registry seen are kept until the registry changes.
(def ^:private cached-options (atom [nil nil]))

(defn waveform-options
  "The waveform catalog Studio and the Push display read: identity, name,
  range, parameters and the picker glyph of each registered waveform."
  [model]
  (if-let [registry (:waveforms model)]
    (let [[seen result] @cached-options]
      (if (identical? seen registry)
        result
        (let [result (options registry)]
          (reset! cached-options [registry result])
          result)))
    @builtin-options))

(defn- require! [condition message]
  (when-not condition (throw (ex-info message {:type ::invalid-binding}))))

(def priority-param
  "Reserved waveform destination for an instance's render layer priority. It is
  not an effect parameter, so it never enters effect parameter overlays."
  :layer/priority)

(def priority-schema
  "Typed destination for modulated layer priority: integer render tiers within
  explicit bounds, in the priority category."
  {:type :integer :bounds [-1000 1000] :default 0 :category :priority
   :unit :layer-priority :name "Layer priority"})

(def default-hysteresis
  "A sampled priority must pass a tier boundary by a quarter tier before it
  changes, so an oscillator resting on a boundary cannot swap layers per frame."
  {:band 1/4})

(def ^:private continuous-priority-schema
  "The priority destination sampled before quantization to integer tiers."
  (assoc priority-schema :type :number))

(defn destination-schema
  "Return the typed schema for a waveform destination on one definition."
  [definition param]
  (if (= priority-param param) priority-schema (get-in definition [:params param])))

(defn- checked-hysteresis [{:keys [band dwell] :as hysteresis}]
  (require! (and (map? hysteresis) (every? #{:band :dwell} (keys hysteresis)))
            "Priority hysteresis accepts :band and :dwell")
  (require! (or (nil? band) (and (time/finite-number? band) (<= 0 band 8)))
            "Priority hysteresis band must be between 0 and 8 tiers")
  (require! (or (nil? dwell) (oscillator/musical-period? dwell))
            "Priority dwell needs positive beats, bars or phrases")
  hysteresis)

(defn- checked-binding
  "Validate a binding for one destination. Only the layer priority destination
  itself (priority? true) accepts :hysteresis, whatever a schema's category."
  [schema waveform binding priority?]
  (require! (and (map? binding) (every? #{:type :waveform :period :phase :phase-reference :from :to :inputs :hysteresis} (keys binding))
                 (= :lfo (:type binding))) "Expected a waveform parameter binding")
  (require! (or (not (contains? binding :hysteresis)) priority?)
            "Only a layer priority destination accepts hysteresis")
  (when (contains? binding :hysteresis) (checked-hysteresis (:hysteresis binding)))
  (let [{:keys [period phase inputs from to] :as binding}
        (merge {:period {:beats 1} :phase 0.0 :inputs {}} binding)]
    (require! (and waveform (= (:id waveform) (:waveform binding))) "Unknown waveform")
    (require! (oscillator/musical-period? period)
              "A waveform period needs positive beats, bars or phrases")
    (require! (oscillator/phase-references (:phase-reference binding :launch))
              "Waveform phase reference must be :launch or :transport")
    (require! (time/finite-number? phase) "Waveform phase must be finite")
    (require! (and (map? inputs) (empty? (params/validate-values (:params waveform) inputs)))
              "Waveform inputs must match the registered scalar parameters")
    (require! (and (params/valid-value? schema from) (params/valid-value? schema to))
              "Both waveform endpoints must satisfy the destination parameter schema")
    binding))

(defn- source-identity [entry definition waveform]
  [(:bound-at entry) (:id definition) (:version definition) (:version waveform)])

(defn bind [model {:keys [instance-id param binding from-param expected-source] :as payload}]
  (let [instance (get-in model [:instances instance-id])
        bindings (:parameter-bindings instance)
        moving? (contains? payload :from-param)
        definition (if moving? (modulation/owner-definition model instance) (:definition instance))
        schema (destination-schema definition param)
        registry (waveforms model)
        binding (if moving?
                  (let [source (get bindings from-param)
                        waveform (get-in registry [:waveforms (get-in source [:binding :waveform])])]
                    (require! instance "Unknown running instance or parameter")
                    (require! source "No waveform is bound to the source parameter")
                    (require! (= expected-source (source-identity source definition waveform))
                              "The source or its effect revision changed. Reopen the source and try again.")
                    (require! (not= from-param param) "Choose another parameter for the waveform")
                    (require! (not (contains? bindings param)) "A waveform is already bound to the destination parameter")
                    (require! (and (map? binding) (= #{:from :to} (set (keys binding))))
                              "Moving a waveform requires both destination endpoints")
                    (cond-> (merge (:binding source) binding)
                      (not= priority-param param) (dissoc :hysteresis)))
                  binding)
        waveform (get-in registry [:waveforms (:waveform binding)])]
    (require! (and instance schema) "Unknown running instance or parameter")
    (require! (not (and (= priority-param param) (get-in instance [:source :native-layer])))
              "Compound layer members share one priority. Set it with the layer priority command.")
    ;; A new binding takes the controller period range; saved bindings keep loading.
    (when-let [message (modulation/period-range-error "Waveform period :period" (:period binding))]
      (require! false message))
    (assoc-in model [:instances instance-id :parameter-bindings]
              (assoc (cond-> bindings moving? (dissoc from-param)) param
                     {:binding (checked-binding schema waveform binding (= priority-param param))
                      :schema schema :waveform-version (:version waveform)
                      :bound-at [(:epoch model 0) (inc (:revision model 0))]}))))

(defn unbind [model {:keys [instance-id param]}]
  (require! (get-in model [:instances instance-id :parameter-bindings param]) "No waveform is bound here")
  (update-in model [:instances instance-id]
             (fn [instance]
               (let [remaining (dissoc (:parameter-bindings instance) param)]
                 (if (seq remaining) (assoc instance :parameter-bindings remaining)
                     (dissoc instance :parameter-bindings))))))

(defn validate-saved! [model instance]
  (when (contains? instance :parameter-bindings)
    (let [bindings (:parameter-bindings instance)]
      (require! (map? bindings) "Saved parameter bindings must be a map")
      (doseq [[param {:keys [schema binding waveform-version bound-at]}] bindings]
        (require! (and (keyword? param) (params/valid-schema? schema)
                       (or (nil? bound-at) (and (vector? bound-at) (= 2 (count bound-at)) (every? nat-int? bound-at)))
                       (integer? waveform-version) (pos? waveform-version))
                  "Invalid captured target schema or waveform revision")
        (checked-binding schema (let [registry (waveforms model)
                                      current (get-in registry [:waveforms (:waveform binding)])]
                                  ;; A live waveform keeps only its newest version.
                                  (or (get-in registry [:versions [(:waveform binding) waveform-version]])
                                      (when (:live? current) current)))
                         binding (= priority-param param))))))

(defn committed-outcomes [previous epoch message-receipts]
  (let [completed (keep (fn [[{:keys [type expected-epoch payload]} [_ ack]]]
                          (let [id (:request-id payload) message (:message ack)]
                            (when (and (#{:modulation-bind :modulation-unbind} type)
                                       (= epoch expected-epoch) (#{:accepted :rejected} (:status ack))
                                       (string? id) (<= 1 (count id) 128))
                              (cond-> (assoc (select-keys ack [:status :revision]) :request-id id :epoch epoch)
                                (string? message) (assoc :message (subs message 0 (min (count message) 256)))))))
                        message-receipts)]
    (if (seq completed) (vec (take-last 256 (into previous completed))) previous)))

(def ^:private pulse-expression
  (let [phase {:op :param :key signals/phase-input}
        duty {:op :param :key :duty}
        zero {:op :const :value 0.0}
        one {:op :const :value 1.0}
        fraction {:op :fract :args [phase]}
        margin {:op :subtract :args [one {:op :clamp :args [duty zero one]}]}]
    {:op :subtract :args [one {:op :floor :args [{:op :add :args [fraction margin]}]}]}))

(defn- pulse-cycle [waveform schema binding interpolate evaluate]
  (when (and (= :pulse (:id waveform))
             (.equals ^Object pulse-expression (:expression waveform))
             (.equals ^Object [0.0 1.0] (:range waveform))
             ;; A published vector schema can be arbitrarily wide.
             (not-any? #(and (vector? %) (> (count %) 64)) [(:from binding) (:to binding)]))
    (try
      (let [duty (get (:inputs binding) :duty (get-in waveform [:params :duty :default]))
            margin (- 1.0 (min 1.0 (max 0.0 duty)))
            low (interpolate 0.0)
            high (interpolate 1.0)]
        (when (and (params/valid-value? schema low) (params/valid-value? schema high))
          (fn [phase]
            (let [x (double phase)]
              (if (Double/isFinite x)
                ;; Keep the IR's wrapping and addition order at the discontinuity.
                ;; The range clamp maps every nonpositive pulse result to low.
                (if (< (+ (- x (Math/floor x)) margin) 1.0) high low)
                (evaluate phase))))))
      (catch Exception _ nil))))

(def ^:private triangle-expression
  (let [one {:op :const :value 1.0}
        two {:op :const :value 2.0}
        wrapped {:op :fract :args [{:op :param :key signals/phase-input}]}
        ramp {:op :multiply :args [two wrapped]}]
    {:op :subtract :args [one {:op :abs :args [{:op :subtract :args [ramp one]}]}]}))

(defn- triangle-cycle [waveform schema {:keys [from to]} interpolate evaluate]
  (when (and (= :triangle (:id waveform))
             (.equals ^Object triangle-expression (:expression waveform))
             (.equals ^Object [0.0 1.0] (:range waveform))
             (case (:type schema)
               :boolean true
               :enum (every? #(or (keyword? %) (string? %)) [from to])
               ;; Interpolating from integer 0 to 1 gives the triangle's own
               ;; fraction. Rounding it uses the same half-amplitude switch.
               :integer (and (zero? from) (= 1 to))
               false))
    (let [low (interpolate 0.0)
          high (interpolate 1.0)]
      (fn [phase]
        (let [x (double phase)]
          (if (Double/isFinite x)
            ;; Keep the IR's floating-point operation order at both switches.
            (let [value (- 1.0 (Math/abs (- (* 2.0 (- x (Math/floor x))) 1.0)))]
              (if (< value 0.5) low high))
            (evaluate phase)))))))

(defn- prepare [registry waveform schema binding priority?]
  (try
    (checked-binding schema waveform binding priority?)
    (let [[lo hi] (:range waveform)
          sample (ir/compile-expression (signals/sample registry (:id waveform)
                                          {:phase {:op :param :key signals/phase-input} :params (:inputs binding)}))
          interpolate #(modulation/interpolate-value schema (:from binding) (:to binding) %)
          evaluate (fn [phase]
                     (let [value (sample {:params {signals/phase-input phase}})]
                       (require! (time/finite-number? value) "Waveform returned a non-finite value")
                       (interpolate (if (= lo hi) 0.0 (min 1.0 (max 0.0 (/ (- value lo) (- hi lo))))))))
          pulse (pulse-cycle waveform schema binding interpolate evaluate)
          triangle (triangle-cycle waveform schema binding interpolate evaluate)]
      (assoc (cond
               pulse {:evaluate pulse :cycle :pulse}
               triangle {:evaluate triangle :cycle :triangle}
               :else {:evaluate evaluate})
             :interpolate interpolate))
    (catch Exception error {:error (.getMessage error)})))

(defn quantize-priority
  "Choose one integer priority tier from a continuous sample with explicit
  hysteresis. previous is {:value tier :since-beat beat} or nil. A new tier is
  adopted only after the sample passes the held tier's boundary by :band tiers
  and, with :dwell, after that musical interval has elapsed on the instance
  clock since the last change. The result is a pure function of its inputs."
  [{:keys [band dwell]} meter [lo hi] previous raw beat]
  (let [target (long (max lo (min hi (Math/round (double raw)))))
        held (:value previous)]
    (if (or (nil? held) (= target held))
      (or previous {:value target :since-beat beat})
      (let [crossed? (>= (Math/abs (- (double raw) (double held))) (+ 0.5 (double (or band 0))))
            since (:since-beat previous)
            dwelled? (or (nil? dwell) (< beat since)
                         (>= (- beat since) (time/duration->beats dwell meter)))]
        (if (and crossed? dwelled?) {:value target :since-beat beat} previous)))))

(defn- sample-priority [instance transport-clock private-clock previous-priorities plan source result binding]
  (let [id (:id instance)
        elapsed (when (= :launch (:phase-reference binding :launch))
                  (- (:beat private-clock) (or (:started-at-beat instance) 0.0)))
        raw ((:evaluate plan) (oscillator/phase binding transport-clock elapsed (:timing-meter instance)))
        _ (require! (params/valid-value? continuous-priority-schema raw) "Waveform produced an invalid layer priority")
        prior (get previous-priorities id)
        prior (when (= (:identity source) (:identity prior)) prior)
        beat (or (:beat private-clock) 0.0)
        held (quantize-priority (merge default-hysteresis (:hysteresis binding))
                                (:timing-meter instance) (:bounds priority-schema) prior raw beat)
        tier (:value held)]
    (-> result
        (assoc-in [:priorities id] (assoc held :identity (:identity source)))
        (assoc-in [:priority-overrides id] tier)
        (assoc-in [:sources id priority-param] (assoc source :priority tier :raw-priority raw)))))

(defn- period-beats
  "Beats in one cycle of a binding, in the meter its phase reference follows."
  [{:keys [period phase-reference]} instance transport-clock]
  (try (double (time/duration->beats period (if (= :transport phase-reference)
                                              (:meter transport-clock)
                                              (:timing-meter instance))))
       (catch Exception _ nil)))

(defn- sample-one [model instance transport-clock private-clock previous result param {:keys [binding] :as entry}]
  (let [id (:id instance)
        definition (modulation/owner-definition model instance)
        priority? (= priority-param param)
        schema (if priority? continuous-priority-schema (destination-schema definition param))
        registry (waveforms model)
        waveform (get-in registry [:waveforms (:waveform binding)])
        cache-key [(:show-id model) (:epoch model) (:id definition) (:version definition)
                   waveform schema binding]
        plan (or (get-in previous [:plans cache-key]) (get-in result [:plans cache-key])
                 (prepare registry waveform schema binding priority?))
        ;; A live audio-shaped waveform samples its morphing table at this
        ;; frame's transport beat; the compiled template is its latest target.
        live (when-not (:error plan)
               (audio-lfo/live-table model waveform (:beat transport-clock)))
        sampled (if live
                  (let [interpolate (:interpolate plan)]
                    (assoc plan :evaluate #(interpolate (min 1.0 (max 0.0 (audio-shape/table-value live %))))))
                  plan)
        hold (get-in model [:modulation :holds id param])
        held? (boolean (and hold (or (nil? (:until-beat hold))
                                      (< (get-in model [:transport :beat] (:beat private-clock)) (:until-beat hold)))))
        result (assoc-in result [:plans cache-key] plan)
        period-beats (period-beats binding instance transport-clock)
        source (cond-> {:type :lfo :binding binding :held? held? :identity (source-identity entry definition waveform)}
                 period-beats (assoc :period-beats period-beats)
                 (and held? (:reason hold)) (assoc :hold-reason (:reason hold)))
        diagnostic (fn [result message]
                     (assoc-in result [:sources id param] (assoc source :error message)))]
    (if-let [error (:error plan)]
      (diagnostic result error)
      (let [result (assoc-in result [:sources id param] source)]
        (cond
          held? result
          priority? (try (sample-priority instance transport-clock private-clock (:priorities previous)
                                          sampled source result binding)
                         (catch Exception error (diagnostic result (.getMessage error))))
          :else
          (try
            (let [elapsed (when (= :launch (:phase-reference binding :launch))
                            (- (:beat private-clock) (or (:started-at-beat instance) 0.0)))
                  phase (oscillator/phase binding transport-clock elapsed (:timing-meter instance))
                  value ((:evaluate sampled) phase)]
              (require! (params/valid-value? schema value) "Waveform produced an invalid parameter value")
              (assoc-in result [:overrides id param] value))
            (catch Exception error (diagnostic result (.getMessage error)))))))))

(defn sample
  "Sample every bound waveform once for a frozen owner frame. previous is the
  prior frame's result (or {}); its compiled :plans are reused and its
  :priorities carry layer-priority hysteresis. :overrides holds effect parameter
  values. :priority-overrides holds integer layer priorities for the engine's
  frame-local ordering and never enters effect parameters."
  [model frame previous]
  (let [transport-clock (time/frame-clock frame)]
    (reduce-kv (fn [result _ instance]
                 (if-let [bindings (not-empty (:parameter-bindings instance))]
                   (let [private-clock (system/private-clock instance transport-clock)]
                     (reduce-kv (partial sample-one model instance transport-clock private-clock previous)
                                result bindings))
                   result))
               {:plans {} :sources {} :overrides {} :priorities {} :priority-overrides {}} (:instances model))))

(defn- append [sources id param entry] (update-in sources [id param] (fnil conj []) entry))

(defn- running-cues [model]
  (into {} (for [instance (sort-by :order (vals (:instances model)))
                 :when (= :running (:phase instance))]
             [(:cue-id instance) (:id instance)])))

(defn- patch-sources [sources model]
  (let [routing (:routing model)]
    (if-not (:enabled? routing) sources
      (let [latest (running-cues model)]
        (reduce (fn [out {:keys [id destination source]}]
                  (let [{:keys [contributing? instance-id]} (get-in routing [:state :routes id])
                        {:keys [cue-id param]} (get-in routing [:spec :destinations destination])
                        active? (and contributing? (contains? (get-in routing [:state :last-values]) [destination instance-id]))
                        target (if active? instance-id (get latest cue-id instance-id))]
                    (if (and param (contains? (:instances model) target))
                      (append out target param (cond-> {:type :patch :route-id id :source-id source}
                                                 (not active?) (assoc :pending? true)))
                      out)))
                sources (get-in routing [:spec :routes]))))))

(defn- live-destinations [sources model {:keys [parameters] :as routing}]
  (if-not (seq parameters) sources
    (let [latest (running-cues model)]
      (reduce-kv
        (fn [out controller {:keys [disabled? cue-id instance-id param lane-note]}]
          (let [id (get latest cue-id instance-id)]
            (if (and (not disabled?) (get-in model [:instances id :definition :params param]))
              (append out id param (cond-> (assoc (dissoc routing :parameters) :type :live :controller controller)
                                     lane-note (assoc :lane-note lane-note)))
              out)))
        sources parameters))))

(defn- spatial-sources [sources model overrides]
  (reduce-kv (fn [out id fields]
               (reduce-kv (fn [out param _]
                            (let [source (get-in model [:instances id :definition :spatial-volume :sources param])]
                              (append out id param {:type :spatial :source source})))
                          out fields))
             sources (or overrides {})))

(defn source-index [model {:keys [sources spatial-overrides live-routing diagnostics]}]
  (let [writers (-> {} (patch-sources model) (live-destinations model live-routing))
        random (merge-with (partial merge-with into) writers (modulation/source-index model diagnostics))
        waveforms (reduce-kv (fn [result id inputs]
                               (reduce-kv (fn [out param entry] (append out id param entry)) result inputs))
                             random (or sources {}))]
    (spatial-sources waveforms model spatial-overrides)))

(defn- parameter-blocks [instance]
  (for [[kind param :as path] (keys (get-in instance [:definition :blocks])) :when (= :params kind)]
    [param {:type :blocks :path path}]))

(defn- add-responses [fields responses]
  (reduce (fn [m {:keys [params id error]}]
            (let [source (cond-> {:type :response :id id} error (assoc :error error))]
              (reduce #(update %1 %2 (fnil conj []) source) m params)))
          fields responses))

(defn- priority-reading [instance entries]
  (let [base (instance-order/effective-priority instance)
        driving (some #(when-not (or (:held? %) (:error %)) %) entries)]
    {:base-value base :effective-value (if driving (:priority driving) base)
     :schema priority-schema :sources entries}))

(defn project [instance sources resolve-inputs responses?]
  (let [blocks (parameter-blocks instance)]
    (if (or (seq sources) responses? (seq blocks))
      (let [context (try (resolve-inputs instance)
                         (catch Exception error {:error (.getMessage error)}))
            fields (reduce (fn [m [param source]] (update m param (fnil conj []) source))
                           (add-responses sources (:response-values context)) blocks)]
        (into {} (for [[param entries] fields]
                   (if (= priority-param param)
                     [param (priority-reading instance entries)]
                     (let [error (or (:error context) (some :error (filter #(= :response (:type %)) entries)))]
                       [param (cond-> {:base-value (get-in instance [:params param])
                                       :effective-value (when-not error (get-in context [:instance :params param]))
                                       :sources entries}
                                error (assoc :error error))])))))
      {})))
