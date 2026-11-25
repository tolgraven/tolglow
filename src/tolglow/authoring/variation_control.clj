(ns tolglow.authoring.variation-control
  "Shared projection and owner commands for active cue parameter variations."
  (:require [afterglow.control.model :as control]
            [afterglow.control.parameter-edit :as parameter-edit]
            [tolglow.authoring.cue-variations :as variations])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util HexFormat]))

(def state-key ::cues)

(def operations
  #{:cue-variation/apply :cue-variation/append :cue-variation/overwrite :cue-variation/ratio-mode
    :cue-variation/ratio-set})

(defn- reject! [code message]
  (throw (ex-info message {:control/error code})))

(defn- active-cue [model instance-id]
  (let [instance (get-in model [:instances instance-id])]
    (when-not (and instance (= :running (:phase instance))
                   (get-in model [:registry :cues (:cue-id instance)]))
      (reject! :unknown-instance "Choose a running cue instance"))
    instance))

(defn- authoring-state [model instance-id]
  (let [{:keys [cue-id definition params] :as instance} (active-cue model instance-id)
        schemas (:params definition {})
        {:keys [bank ratio-mode] :or {bank [] ratio-mode :preferred}} (get-in model [:extensions state-key cue-id])]
    {:instance instance :schemas schemas :params (select-keys params (keys schemas))
     :bank bank :ratio-mode ratio-mode}))

(defn- basis [{:keys [instance schemas params bank ratio-mode]}]
  (let [data [(:id instance) (:cue-id instance) (:effect-version instance) schemas params bank ratio-mode]
        digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes (pr-str data) StandardCharsets/UTF_8))]
    (.formatHex (HexFormat/of) digest)))

(defn- saved-state [bank schemas slot]
  (try (variations/recall bank schemas slot)
       (catch clojure.lang.ExceptionInfo _ {:kind :saved :slot slot :stale? true})))

(defn project
  "Project eight columns, seven generated rows and a saved row at y=0. A receipt
  applies a pad only while the authored state that produced it is still current."
  [model {:keys [instance-id seed layout row-page] :or {seed 0 layout :full row-page 0}}]
  (when-not (and (integer? seed) (<= Long/MIN_VALUE seed Long/MAX_VALUE)
                 (#{:full :rows} layout) (integer? row-page) (<= 0 row-page Integer/MAX_VALUE))
    (reject! :invalid-variation "Choose a seeded full or parameter-row variation layout"))
  (let [{:keys [instance schemas params bank ratio-mode] :as state} (authoring-state model instance-id)
        ordered (parameter-edit/ordered-parameters (:definition instance))
        visible-params (take 7 (drop (* 7 row-page) ordered))
        options {:ratio-mode ratio-mode}
        {:keys [rows generated]}
        (if (= :full layout)
          {:generated (:variants (variations/full-variants schemas params seed (assoc options :keys ordered :count 56)))}
          {:rows (into [] (map-indexed (fn [y param]
                                        {:y (inc y) :param param
                                         :variants (:variants (variations/parameter-variants schemas params param seed options))}))
                       visible-params)})
        cells (into (into {} (map (fn [slot] [[slot 0] (saved-state bank schemas slot)])) (range (count bank)))
                    (case layout
                      :full (map-indexed (fn [i choice] [[(mod i 8) (inc (quot i 8))] choice]) generated)
                      :rows (for [{:keys [y variants]} rows [x choice] (map-indexed vector variants)] [[x y] choice])))]
    {:basis (basis state) :instance-id instance-id :cue-id (:cue-id instance) :layout layout
     :seed seed :row-page row-page :row-pages (max 1 (quot (+ 6 (count ordered)) 7)) :ratio-mode ratio-mode
     :rows (mapv #(dissoc % :variants) rows)
     :ratio-parameters (into [] (keep (fn [param]
                                       (when (variations/ratio-schema? (get schemas param))
                                         {:param param :value (get params param)}))) ordered)
     :cells (mapv (fn [[xy cell]] (assoc cell :position xy)) (sort-by (comp vec reverse key) cells))}))

(defn edit-parameter
  "Use the cue's exact beat-ratio preference through normal editor admission."
  [model instance param input {:keys [mode]}]
  (let [{:keys [cue-id definition params]} instance
        ratio-mode (get-in model [:extensions state-key cue-id :ratio-mode] :preferred)]
    (variations/edit-beat-ratio (get-in definition [:params param]) (get params param) input mode ratio-mode)))

(defn- write-params [model instance updates]
  (let [id (:id instance)
        changed (remove (fn [[param value]] (= value (get-in instance [:params param]))) updates)
        next-model (update-in model [:instances id :params] into changed)]
    {:model (control/track-parameter-generations model next-model)
     :events (mapv (fn [[param value]] {:type :param/changed :instance-id id :param param :value value})
                   (sort-by key changed))
     :result {:instance-id id :cue-id (:cue-id instance)}}))

(defn- apply-choice [model {:keys [instance schemas bank]} {:keys [position] :as command}]
  (let [choice (if (= 0 (second position))
                 (saved-state bank schemas (first position))
                 (some #(when (= position (:position %)) %) (:cells (project model command))))]
    (when (or (nil? choice) (:stale? choice))
      (reject! :unavailable-variation "This variation is empty or incompatible with the running cue"))
    (write-params model instance (or (:params choice) {(:param choice) (:value choice)}))))

(defn- operation [model {:keys [op instance-id position slot mode param numerator denominator] :as command}]
  (let [{:keys [instance schemas params bank ratio-mode] :as state} (authoring-state model instance-id)
        path [:extensions state-key (:cue-id instance)]
        result {:instance-id instance-id :cue-id (:cue-id instance)}]
    (when (and (#{:cue-variation/apply :cue-variation/append :cue-variation/overwrite} op)
               (not= (:basis command) (basis state)))
      (reject! :stale-variation "Cue parameters or saved slots changed. Refresh the variations"))
    (case op
      :cue-variation/apply
      (do (when-not (and (vector? position) (= 2 (count position))
                         (every? #(and (integer? %) (<= 0 % 7)) position))
            (reject! :invalid-variation "A variation pad must be in the eight-by-eight grid"))
          (apply-choice model state command))

      :cue-variation/append
      {:model (assoc-in model (conj path :bank) (variations/append bank schemas params))
       :result (assoc result :slot (min (count bank) (dec variations/bank-capacity)))}

      :cue-variation/overwrite
      {:model (assoc-in model (conj path :bank) (variations/overwrite bank schemas slot params))
       :result (assoc result :slot slot)}

      :cue-variation/ratio-mode
      (do (when-not (#{:preferred :free} mode)
            (reject! :invalid-ratio-mode "Choose preferred or free beat ratios"))
          {:model (assoc-in model (conj path :ratio-mode) mode) :result result})

      :cue-variation/ratio-set
      (let [schema (get schemas param)]
        (when-not (variations/valid-beat-ratio-components? schema numerator denominator ratio-mode)
          (reject! :invalid-beat-ratio "Enter positive integer beat-ratio components within the cue's limits"))
        (write-params model instance {param (/ numerator denominator)})))))

(defn apply-command [model command]
  (control/apply-domain-command
   model command
   (fn [model command]
     (try (operation model command)
          (catch clojure.lang.ExceptionInfo error
            (if (:control/error (ex-data error)) (throw error)
                (reject! :invalid-variation (.getMessage error))))))))

(defn valid-saved-state?
  "Check portable bank structure. Old schema states stay available for review;
  individual slots are revalidated against the selected instance on recall."
  [cues]
  (and (map? cues)
       (every? (fn [[id {:keys [bank ratio-mode] :or {bank [] ratio-mode :preferred} :as state}]]
                 (and (or (keyword? id) (and (string? id) (seq id))) (map? state)
                      (every? #{:bank :ratio-mode} (keys state))
                      (#{:preferred :free} ratio-mode)
                      (vector? bank) (<= (count bank) variations/bank-capacity)
                      (every? #(and (map? %) (every? keyword? (keys %))) bank)))
               cues)))
