(ns tolglow.studio.volume-optics
  "Prepare bounded optical meshes from the same signed field used for lighting membership."
  (:require [afterglow.spatial.shape :as shape]))

(def max-visible 12)

(def resolution 30)

(def grid-padding 1.24)

(defn- body-shape [spec]
  (if (= :ball (:shape spec)) (or (:geometry spec) spec) spec))

(defn mesh-spec [volume]
  (let [body (body-shape volume)]
    (case (:shape body)
      (:ball :sphere) {:kind :sphere}
      :box {:kind :box}
      :cylinder {:kind :cylinder}
      :rounded-box {:kind :rounded-box :size (:size body) :rounding (:rounding body 0)}
      ;; A root scale/translation is an inexpensive per-frame mesh transform.
      {:kind :field :form (select-keys body [:shape :parts :planes])})))

(defn- sample-axis [center half-size]
  (mapv #(+ center (* half-size (- (/ (* 2 %) resolution) 1))) (range resolution)))

(defn- field-grid [spec]
  (let [{:keys [distance bounds]} (shape/prepare spec)
        center (shape/center bounds)
        half (mapv #(* 0.5 grid-padding (- %2 %1)) (:min bounds) (:max bounds))
        [xs ys zs] (mapv sample-axis center half)
        samples (js/Float32Array. (* resolution resolution resolution))]
    (dotimes [z resolution]
      (dotimes [y resolution]
        (dotimes [x resolution]
          (aset samples (+ x (* resolution (+ y (* resolution z))))
                (- (distance [(xs x) (ys y) (zs z)]))))))
    {:kind :field :resolution resolution :field samples :padding grid-padding}))

(defn- descriptor [spec]
  (if (= :field (:kind spec)) (field-grid (:form spec)) spec))

(defn- displayed [id volume signal]
  (let [current (if (and (= :ball (:shape volume)) (:position signal))
                  (assoc volume :position (:position signal) :radius (:radius signal (:radius volume))) volume)
        bounds (:bounds (shape/prepare current))]
    (when (and bounds (every? true? (map < (:min bounds) (:max bounds))))
      {:spec (mesh-spec current)
       :object {:id (pr-str id) :position (shape/center bounds)
                :half-size (mapv #(* 0.5 (- %2 %1)) (:min bounds) (:max bounds))
                :color (get-in signal [:paint :color] [0.65 0.85 1])
                :strength (:strength current 1) :deformation (:deformation signal 0)}})))

(defn- surface-entry [[id {:keys [volume] :as signal}]]
  (when volume (displayed id volume signal)))

(defn- retain-mesh [old [lookup next-id] spec]
  (if-let [previous (get (:lookup old) spec)]
    [(assoc lookup spec previous) next-id]
    [(assoc lookup spec {:key (str "surface-" next-id) :mesh (descriptor spec)}) (inc next-id)]))

(defn- mesh-library [old specs]
  (if (= specs (:specs old))
    (select-keys old [:lookup :next-id :meshes :specs])
    (let [[lookup next-id] (reduce (partial retain-mesh old)
                                   [{} (or (:next-id old) 0)] (sort-by pr-str specs))]
      {:lookup lookup :next-id next-id :specs specs
       :meshes (into {} (map (juxt :key :mesh)) (vals lookup))})))

(defn prepare
  "Keep geometry shared across equal authored shapes. Drop absent shapes when their
  last live instance ends. Mesh vertices are independent of body position and paint."
  [old signals]
  (let [candidates (into [] (comp (keep surface-entry) (take (inc max-visible))) (sort-by (comp str key) signals))
        entries (subvec candidates 0 (min max-visible (count candidates)))
        library (mesh-library old (into #{} (map :spec) entries))
        attach-mesh (fn [{:keys [spec object]}]
                      (assoc object :mesh (get-in library [:lookup spec :key])))]
    (assoc library :objects (mapv attach-mesh entries) :limited? (> (count candidates) max-visible))))
