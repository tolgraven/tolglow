(ns tolglow.effects.parameter-sources
  "Musical waveform inputs attached to one running cue. Only declarations live
  on instances. Compiled waveform functions belong to the frame owner."
  (:require [afterglow.control.params :as params]
            [afterglow.control.system :as system]
            [afterglow.render.ir :as ir]
            [afterglow.time :as time]
            [tolglow.modulation :as modulation]
            [tolglow.oscillator :as oscillator]
            [tolglow.signals :as signals]))

(def ^:private builtins (delay (signals/new-registry)))

(defn- waveforms [model] (or (:waveforms model) @builtins))

(defn- options [registry]
  (->> (:waveforms registry) vals (sort-by (comp str :id))
       (mapv #(select-keys % [:id :name :range :params :version]))))

(def ^:private builtin-options (delay (options @builtins)))

(defn waveform-options [model]
  (if-let [registry (:waveforms model)] (options registry) @builtin-options))

(defn- require! [condition message]
  (when-not condition (throw (ex-info message {:type ::invalid-binding}))))

(defn- checked-binding [schema waveform binding]
  (require! (and (map? binding) (every? #{:type :waveform :period :phase :phase-reference :from :to :inputs} (keys binding))
                 (= :lfo (:type binding))) "Expected a waveform parameter binding")
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

(defn bind [model {:keys [instance-id param binding from-param] :as payload}]
  (let [instance (get-in model [:instances instance-id])
        bindings (:parameter-bindings instance)
        schema (get-in instance [:definition :params param])
        moving? (contains? payload :from-param)
        binding (if moving?
                  (let [source (get-in bindings [from-param :binding])]
                    (require! source "No waveform is bound to the source parameter")
                    (require! (not= from-param param) "Choose another parameter for the waveform")
                    (require! (not (contains? bindings param)) "A waveform is already bound to the destination parameter")
                    (require! (and (map? binding) (= #{:from :to} (set (keys binding))))
                              "Moving a waveform requires both destination endpoints")
                    (merge source binding))
                  binding)
        waveform (get-in (waveforms model) [:waveforms (:waveform binding)])]
    (require! schema "Unknown running instance or parameter")
    (assoc-in model [:instances instance-id :parameter-bindings]
              (assoc (cond-> bindings moving? (dissoc from-param)) param
                     {:binding (checked-binding schema waveform binding)
                      :schema schema :waveform-version (:version waveform)}))))

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
      (doseq [[param {:keys [schema binding waveform-version]}] bindings]
        (require! (and (keyword? param) (params/valid-schema? schema)
                       (integer? waveform-version) (pos? waveform-version))
                  "Invalid captured target schema or waveform version")
        (checked-binding schema (get-in (waveforms model) [:versions [(:waveform binding) waveform-version]]) binding)))))

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

(defn- prepare [registry waveform schema binding]
  (try
    (checked-binding schema waveform binding)
    (let [[lo hi] (:range waveform)
          sample (ir/compile-expression (signals/sample registry (:id waveform)
                                          {:phase {:op :param :key signals/phase-input} :params (:inputs binding)}))
          interpolate #(modulation/interpolate-value schema (:from binding) (:to binding) %)
          evaluate (fn [phase]
                     (let [value (sample {:params {signals/phase-input phase}})]
                       (require! (time/finite-number? value) "Waveform returned a non-finite value")
                       (interpolate (if (= lo hi) 0.0 (min 1.0 (max 0.0 (/ (- value lo) (- hi lo))))))))
          cycle (pulse-cycle waveform schema binding interpolate evaluate)]
      (cond-> {:evaluate (or cycle evaluate)}
        cycle (assoc :cycle :pulse)))
    (catch Exception error {:error (.getMessage error)})))

(defn- sample-one [model instance transport-clock private-clock previous result param {:keys [binding]}]
  (let [id (:id instance)
        definition (modulation/owner-definition model instance)
        schema (get-in definition [:params param])
        registry (waveforms model)
        waveform (get-in registry [:waveforms (:waveform binding)])
        cache-key [(:show-id model) (:epoch model) (:id definition) (:version definition)
                   waveform schema binding]
        plan (or (get previous cache-key) (get-in result [:plans cache-key])
                 (prepare registry waveform schema binding))
        hold (get-in model [:modulation :holds id param])
        held? (boolean (and hold (or (nil? (:until-beat hold))
                                      (< (get-in model [:transport :beat] (:beat private-clock)) (:until-beat hold)))))
        result (assoc-in result [:plans cache-key] plan)
        source {:type :lfo :binding binding :held? held?}
        diagnostic (fn [result message]
                     (assoc-in result [:sources id param] (assoc source :error message)))]
    (if-let [error (:error plan)]
      (diagnostic result error)
      (let [result (assoc-in result [:sources id param] source)]
        (if held? result
          (try
            (let [elapsed (when (= :launch (:phase-reference binding :launch))
                            (- (:beat private-clock) (or (:started-at-beat instance) 0.0)))
                  phase (oscillator/phase binding transport-clock elapsed (:timing-meter instance))
                  value ((:evaluate plan) phase)]
              (require! (params/valid-value? schema value) "Waveform produced an invalid parameter value")
              (assoc-in result [:overrides id param] value))
            (catch Exception error (diagnostic result (.getMessage error)))))))))

(defn sample [model frame previous-plans]
  (let [transport-clock (time/frame-clock frame)]
    (reduce-kv (fn [result _ instance]
                 (if-let [bindings (not-empty (:parameter-bindings instance))]
                   (let [private-clock (system/private-clock instance transport-clock)]
                     (reduce-kv (partial sample-one model instance transport-clock private-clock previous-plans)
                                result bindings))
                   result))
               {:plans {} :sources {} :overrides {}} (:instances model))))

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

(defn project [instance sources resolve-inputs responses?]
  (let [blocks (parameter-blocks instance)]
    (if (or (seq sources) responses? (seq blocks))
      (let [context (try (resolve-inputs instance)
                         (catch Exception error {:error (.getMessage error)}))
            fields (reduce (fn [m [param source]] (update m param (fnil conj []) source))
                           (add-responses sources (:response-values context)) blocks)]
        (into {} (for [[param entries] fields]
                   (let [error (or (:error context) (some :error (filter #(= :response (:type %)) entries)))]
                     [param (cond-> {:base-value (get-in instance [:params param])
                                     :effective-value (when-not error (get-in context [:instance :params param]))
                                     :sources entries}
                              error (assoc :error error))]))))
      {})))
