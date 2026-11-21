(ns tolglow.studio.expressive
  "Pure preview of the shared device mapper. No MIDI discovery or activation."
  (:require [afterglow.control.devices.expressive :as mapping]
            [afterglow.control.codecs.mpe :as mpe]
            [tolglow.registry :as registry]))

(def device-axes
  {:push-3 #{:strike :pressure :glide :slide}
   :seaboard-m (set (keys mpe/axes))})

(defn- identifier [value]
  (cond (keyword? value) value
        (and (string? value) (<= 1 (count value) 128)) (keyword value)
        :else (throw (ex-info "Choose a device and cue ID" {}))))

(defn preview
  "Preview saved or draft :policy for a device/cue. A draft replaces this device's
  complete saved policy only for the preview. :instance-id uses that activation's
  captured schema; otherwise resolve the cue's pinned/current effect version.
  Generic :mpe requires explicit :axes. This does not verify physical hardware."
  [model {:keys [device-id cue-id device-kind policy axes instance-id] :as request}]
  (let [device-id (identifier device-id) cue-id (identifier cue-id)
        device-kind (identifier (or device-kind :push-3))
        available (if (= :mpe device-kind) (set (map identifier axes)) (device-axes device-kind))
        _ (when-not (and (seq available) (every? #(contains? mpe/axes %) available))
            (throw (ex-info "Choose Push 3, Seaboard M, or generic MPE with explicit documented axes" {})))
        cue (get-in model [:registry :cues cue-id])
        instance (when instance-id (get-in model [:instances instance-id]))
        _ (when-not (and cue (or (nil? instance-id) (= cue-id (:cue-id instance))))
            (throw (ex-info "Cue or matching active instance not found" {})))
        effect (if instance (:definition instance) (registry/cue-effect (:registry model) cue))
        _ (when-not effect (throw (ex-info "Cue's effect version was not found" {})))
        model (if (contains? request :policy)
                (update-in model [:settings :expressive :devices]
                           #(assoc (dissoc % (subs (str device-id) 1)) device-id (mapping/normalize-policy policy))) model)
        effective (mapping/policy model device-id cue-id {})
        schemas (:params effect {})
        plan (mapping/plan schemas available (:params effective) (dissoc effective :params))
        values (or (:params instance)
                   (merge (into {} (map (fn [[id schema]] [id (:default schema)]) schemas))
                          (:params cue) (:saved-params cue)))
        samples (mapv (fn [unit]
                        (let [axis-values (into {} (map (fn [[axis {:keys [range]}]]
                                                         [axis (+ (first range) (* unit (- (second range) (first range))))]))
                                                (select-keys mpe/axes available))]
                          {:position unit :axes axis-values :values (mapping/values plan axis-values values)}))
                      [0.0 0.25 0.5 0.75 1.0])]
    (merge plan {:device-id device-id :cue-id cue-id :device-kind device-kind :instance-id instance-id
                 :effect-id (:id effect) :effect-version (:version effect)
                 :policy effective :schemas schemas :axes (select-keys mpe/axes available)
                 :curves (vec (sort mapping/curves)) :samples samples
                 :hardware-verified? false :note-lifecycle :unchanged})))
