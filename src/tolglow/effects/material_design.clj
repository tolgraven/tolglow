(ns tolglow.effects.material-design
  "Edit one shared material and sample a private draft on the caller's thread."
  (:require [afterglow.render.blocks :as blocks]
            [afterglow.render.ir :as ir]
            [tolglow.compiler.blocks :as fields]
            [tolglow.effects.prefabs :as prefabs]
            [tolglow.registry :as registry]))

(defn inventory [model]
  {:materials (prefabs/catalog model)
   :presets (select-keys blocks/library [:optics/through-water :optics/gel :optics/ribbed-glass
                                         :optics/fog-transmission :lfo/sine :lfo/triangle])})

(defn edit [model {:keys [id expected material] :as request}]
  (when-not (and (keyword? id) (contains? request :expected) (contains? request :material)
                 (or (nil? material) (map? material)))
    (throw (ex-info "Supply a material ID, its previous definition and a replacement." {})))
  (let [entries (prefabs/catalog model) current (get entries id)]
    (cond
      (= current material) {:model model :ack {:status :accepted :revision (:revision model)}}
      (not= expected current)
      {:model model :ack {:status :rejected :reason :stale-material
                          :message "Another operator changed this material. Reload the library before saving."}}
      :else {:settings {:scope :prefabs :values {:definitions (if material (assoc entries id material)
                                                                  (dissoc entries id))}}})))

(def ^:private columns 40)
(def ^:private rows 18)

(defn- rgb [v]
  (let [channels (if (number? v) [v v v] (take 3 (concat v [0.0])))]
    (mapv #(int (Math/round (* 255.0 (min 1.0 (max 0.0 (double %)))))) channels)))

(defn- draft-params [plan variables]
  (let [model {:variables variables}]
    (doseq [path (keys (:uniforms plan))]
      (when (nil? (get-in model path))
        (throw (ex-info (str "Waiting for show variable " (pr-str (subvec path 1))) {:path path}))))
    (fields/with-variables plan {} model)))

(defn- sample-plane [material beat dry variables]
  (let [plan (prefabs/draft-plan :draft material dry (:mode material))
        expression (get-in plan [:expressions [:varyings :sample]])
        sample (ir/compile-expression expression)
        meter {:beats-per-bar 4 :bars-per-phrase 4}
        env (fields/sample-inputs
             plan {:beat beat :elapsed-beats beat :time-ms (* 500.0 beat) :elapsed-ms (* 500.0 beat)
                   :params (draft-params plan variables) :meter meter}
             {:beat beat :meter meter})
        at (fn [x y]
             (let [u (/ x (double (dec columns))) v (/ y (double (dec rows)))]
               (sample (assoc env :position [(* 4.0 u) (* 2.0 v) 0.0] :surface-uv [u v]))))
        values (if (some #{[:position]} (ir/input-paths expression))
                 (vec (for [y (range rows) x (range columns)] (at x y)))
                 (repeat (* rows columns) (at 0 0)))
        channels (mapcat #(if (number? %) [%] %) values)]
    {:columns columns :rows rows :rgb (mapv rgb values)
     :min (reduce min channels) :max (reduce max channels) :beat beat}))

(defn preview
  ([request] (preview request {}))
  ([{:keys [material beat dry] :or {beat 0.0 dry 1.0}} variables]
   (when-not (and (registry/finite-number? beat) (<= 0.0 beat 4.0))
     (throw (ex-info "Choose a sample from the four-beat bar." {})))
   (when-not (and (registry/finite-number? dry) (<= 0.0 dry 1.0))
     (throw (ex-info "Dry level must be between zero and one." {})))
   (prefabs/validate! {:definitions {:draft material}})
   ;; The material may legitimately read a cue parameter that has no value in
   ;; this standalone preview. The show validates the definition again on save.
   (try (sample-plane material beat dry variables)
        (catch clojure.lang.ExceptionInfo error
          {:unavailable? true :message (str "Draft validated. This sample is unavailable. " (.getMessage error))}))))
