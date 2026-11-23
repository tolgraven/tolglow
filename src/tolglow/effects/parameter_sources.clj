(ns tolglow.effects.parameter-sources
  "Musical waveform inputs attached to one running cue. Only declarations live
  on instances. Compiled waveform functions belong to the frame owner."
  (:require [afterglow.control.params :as params]
            [afterglow.control.system :as system]
            [afterglow.render.ir :as ir]
            [afterglow.time :as time]
            [tolglow.modulation :as modulation]
            [tolglow.signals :as signals]))

(def ^:private builtins (delay (signals/new-registry)))
(defn- waveforms [model] (or (:waveforms model) @builtins))

(defn waveform-options [model]
  (->> (:waveforms (waveforms model)) vals (sort-by (comp str :id))
       (mapv #(select-keys % [:id :name :range :params :version]))))

(defn- require! [condition message]
  (when-not condition (throw (ex-info message {:type ::invalid-binding}))))

(defn- checked-binding [schema waveform binding]
  (require! (and (map? binding) (every? #{:type :waveform :period :phase :from :to :inputs} (keys binding))
                 (= :lfo (:type binding))) "Expected a waveform parameter binding")
  (let [{:keys [period phase inputs from to] :as binding}
        (merge {:period {:beats 1} :phase 0.0 :inputs {}} binding)]
    (require! (and waveform (= (:id waveform) (:waveform binding))) "Unknown waveform")
    (require! (and (map? period) (= 1 (count period)) (#{:beats :bars :phrases} (ffirst period))
                   (time/finite-number? (val (first period))) (pos? (val (first period))))
              "A waveform period needs positive beats, bars or phrases")
    (require! (time/finite-number? phase) "Waveform phase must be finite")
    (require! (and (map? inputs) (empty? (params/validate-values (:params waveform) inputs)))
              "Waveform inputs must match the registered scalar parameters")
    (require! (and (params/valid-value? schema from) (params/valid-value? schema to))
              "Both waveform endpoints must satisfy the destination parameter schema")
    binding))

(defn bind [model {:keys [instance-id param binding]}]
  (let [schema (get-in model [:instances instance-id :definition :params param])
        waveform (get-in (waveforms model) [:waveforms (:waveform binding)])]
    (require! schema "Unknown running instance or parameter")
    (assoc-in model [:instances instance-id :parameter-bindings param]
              {:binding (checked-binding schema waveform binding)
               :schema schema :waveform-version (:version waveform)})))

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

(defn- prepare [registry waveform schema binding]
  (try
    (checked-binding schema waveform binding)
    (let [[lo hi] (:range waveform)
          sample (ir/compile-expression (signals/sample registry (:id waveform)
                                          {:phase {:op :param :key signals/phase-input} :params (:inputs binding)}))]
      {:evaluate (fn [phase]
                   (let [value (sample {:params {signals/phase-input phase}})]
                     (require! (time/finite-number? value) "Waveform returned a non-finite value")
                     (let [weight (if (= lo hi) 0.0 (min 1.0 (max 0.0 (/ (- value lo) (- hi lo)))))]
                       (modulation/interpolate-value schema (:from binding) (:to binding) weight))))})
    (catch Exception error {:error (.getMessage error)})))

(defn- sample-one [model instance clock previous result param {:keys [binding]}]
  (let [id (:id instance)
        schema (get-in (modulation/owner-definition model instance) [:params param])
        registry (waveforms model)
        waveform (get-in registry [:waveforms (:waveform binding)])
        cache-key [waveform schema binding]
        plan (or (get previous cache-key) (get-in result [:plans cache-key])
                 (prepare registry waveform schema binding))
        held? (boolean (get-in model [:modulation :holds id param]))
        result (assoc-in result [:plans cache-key] plan)
        source {:type :lfo :binding binding :held? held?}
        diagnostic (fn [result message]
                     (assoc-in result [:sources id param] (assoc source :error message)))]
    (if-let [error (:error plan)]
      (diagnostic result error)
      (let [result (assoc-in result [:sources id param] source)]
        (if held? result
          (try
            (let [period (time/duration->beats (:period binding) (:timing-meter instance))
                  phase (+ (:phase binding) (/ (- (:beat clock) (or (:started-at-beat instance) 0.0)) period))
                  value ((:evaluate plan) phase)]
              (require! (params/valid-value? schema value) "Waveform produced an invalid parameter value")
              (assoc-in result [:overrides id param] value))
            (catch Exception error (diagnostic result (.getMessage error)))))))))

(defn sample [model frame previous-plans]
  (reduce-kv (fn [result _ instance]
               (if-let [bindings (not-empty (:parameter-bindings instance))]
                 (let [clock (system/private-clock instance frame)]
                   (reduce-kv (partial sample-one model instance clock previous-plans) result bindings))
                 result))
             {:plans {} :sources {} :overrides {}} (:instances model)))

(defn- append [sources id param entry] (update-in sources [id param] (fnil conj []) entry))

(defn source-index [model live-sources]
  (let [random (modulation/source-index model)
        live (reduce-kv (fn [result id inputs]
                          (reduce-kv (fn [out param entry] (append out id param entry)) result inputs))
                        random (or live-sources {}))
        routing (:routing model)]
    (if-not (:enabled? routing) live
      (reduce (fn [out {:keys [id destination source]}]
                (let [instance (get-in routing [:state :routes id :instance-id])
                      param (get-in routing [:spec :destinations destination :param])]
                  (if (and param (contains? (:instances model) instance))
                    (append out instance param {:type :patch :route-id id :source-id source})
                    out)))
              live (get-in routing [:spec :routes])))))

(defn project [instance sources resolve-inputs]
  (let [blocks (for [[kind param :as path] (keys (get-in instance [:definition :blocks])) :when (= :params kind)]
                 [param {:type :blocks :path path}])
        fields (reduce (fn [m [param source]] (update m param (fnil conj []) source)) sources blocks)
        {:keys [response-values] resolved :instance} (resolve-inputs instance)
        fields (reduce (fn [m {:keys [params id]}]
                         (reduce #(update %1 %2 (fnil conj []) {:type :response :id id}) m params))
                       fields response-values)]
    (into {} (map (fn [[param entries]]
                    [param {:base-value (get-in instance [:params param])
                            :effective-value (get-in resolved [:params param]) :sources entries}])) fields)))
