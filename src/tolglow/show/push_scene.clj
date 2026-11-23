(ns tolglow.show.push-scene
  "Project the committed venue frame on the device capture worker. No IO or
  lighting evaluation takes place here. Renderer slot IDs stay compact even
  when show IDs are long, namespaced, or shared across fixture libraries."
  (:require [afterglow.render.frame :as render]
            [afterglow.render.photometry :as photometry]
            [tolglow.rig.reflections :as reflections])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util HexFormat]))

(def ^:private fixture-limit 32)
(defn create-cache [] (volatile! nil))

(defn- spread [items n]
  (let [items (vec items) total (count items)]
    (cond (zero? n) []
          (<= total n) items
          (= n 1) [(first items)]
          :else (mapv #(nth items (quot (* % (dec total)) (dec n))) (range n)))))

(defn- mover? [{:keys [channels]}]
  (and (contains? channels :pan) (contains? channels :tilt)))

(defn- wire-id [id]
  (if (keyword? id) (subs (str id) 1) (str id)))

(defn- short-text [value]
  (let [s (wire-id value)]
    (subs s 0 (min 80 (count s)))))

(defn- display-fixture [id fixture]
  (cond-> (assoc (select-keys fixture [:position :rotation-deg :zoom-range-deg :beam-angle
                                        :lens-radius-m])
                 :id id :relative-brightness (photometry/relative-brightness fixture))
    (:fixture-type fixture) (assoc :fixture-type (short-text (:fixture-type fixture)))
    (:pseudo-fixture fixture) (assoc :pseudo-fixture true)
    (mover? fixture) (assoc :channels {:pan 1 :tilt 1})
    (:pixel-layout-id fixture) (assoc :pixel-layout-id true)))

(defn- stable-id [id]
  (let [encoded (.getBytes (pr-str id) StandardCharsets/UTF_8)
        digest (.digest (MessageDigest/getInstance "SHA-256") encoded)]
    (.formatHex (HexFormat/of) digest)))

(defn- truss-ends [{:keys [position length rotation-deg]}]
  (when position
    (let [[_ y z] (map #(Math/toRadians (double %)) (or rotation-deg [0 0 0]))
          half (* 0.5 (double (or length 0)))
          axis [(* (Math/cos y) (Math/cos z)) (* (Math/cos y) (Math/sin z)) (- (Math/sin y))]
          delta (mapv #(* half %) axis)]
      [(mapv - position delta) (mapv + position delta)])))

(defn- limits [points]
  (when (seq points)
    {:min (reduce #(mapv min %1 %2) points) :max (reduce #(mapv max %1 %2) points)}))

(defn- prepare-rig [{:keys [fixtures trusses surfaces room] :as rig}]
  (let [indices (range (count fixtures))
        {moving true fixed false} (group-by #(boolean (mover? (nth fixtures %))) indices)
        n (min (count moving) (- fixture-limit (min (count fixed) (quot fixture-limit 2))))
        selected (sort (concat (spread moving n) (spread fixed (- fixture-limit n))))
        slots (mapv (fn [i] {:offset i :slot (str i) :fixture (nth fixtures i)}) selected)
        geometry (fn [{:keys [slot fixture]}] (display-fixture slot fixture))]
    {:source rig :slots slots :total (count fixtures)
     :ids (delay (into {} (map-indexed (fn [i f] [(wire-id (:id f)) i])) fixtures))
     :bounds (limits (concat (keep :position fixtures) (mapcat truss-ends trusses)))
     :optics (delay (reflections/plan rig))
     :fog-ids (delay (into {} (keep (fn [{:keys [id fogger]}] (when fogger [id (stable-id id)]))) fixtures))
     :surface-ids (into {:floor "floor" :stage "stage" :back "back" :left "left" :right "right"}
                        (map-indexed (fn [i s] [(:id s) (str i)])) (take 16 surfaces))
     :rig {:room (select-keys room [:width :height :depth])
           :fixtures (mapv geometry slots)
           :trusses (mapv (fn [i t] (assoc (select-keys t [:position :length :rotation-deg]) :id (str i)))
                          (range) (take 16 trusses))
           :surfaces (mapv (fn [i s] (cond-> (assoc (select-keys s [:point :normal :size :u-axis]) :id (str i))
                                       (:reflectance s) (assoc :reflectance (select-keys (:reflectance s) [:method :estimate]))))
                           (range) (take 16 surfaces))}}))

(defn- plan! [cache rig]
  (if (identical? rig (:source @cache)) @cache (vreset! cache (prepare-rig rig))))

(defn- reflected [optics surface-ids fixture values]
  (when-let [hit (reflections/footprint fixture values (:receivers @optics))]
    (when-let [id (get surface-ids (:surface-id hit))]
      (assoc hit :surface-id id))))

(defn- lighting [{:keys [values compact target-order]} {:keys [slots optics surface-ids]}]
  ;; Afterglow preserves the patch's fixture order. A caller may supply a
  ;; reordered compact frame; resolve its actual indices before reading it.
  (let [index (delay (zipmap target-order (range)))
        offset (fn [{:keys [fixture offset]}]
                 (if (= (:id fixture) (nth target-order offset nil)) offset (get @index (:id fixture))))
        rows (when compact
               (let [indices (keep offset slots)]
                 (zipmap (map target-order indices) (render/project-compact compact indices))))
        frame (or values rows {})]
    (into {}
          (keep (fn [{:keys [slot fixture]}]
                  (when-let [row (get frame (:id fixture))]
                    (let [values (merge (select-keys (:channels row) [:pan :tilt :dimmer :zoom :focus :strobe :shutter])
                                        (select-keys row [:emitted-color :aim :direction :pan :tilt :dimmer :zoom :focus
                                                          :strobe :shutter :cut?]))
                          pose (render/fixture-pose fixture row)
                          data (cond-> values pose (assoc :pose (select-keys pose [:pan-deg :tilt-deg :direction])))
                          spot (when (:pseudo-fixture fixture) (reflected optics surface-ids fixture data))]
                      [slot (cond-> data spot (assoc :beam-footprint spot))]))))
          slots)))

(defn- atmosphere [sample fog-ids]
  (when sample
    (assoc (select-keys sample [:sample-time-ms :epoch :extinction-per-m :reference-path-m
                                :fog-level :volume-m3 :mixed-m3 :mixed-density])
           :sources (mapv (fn [source] (-> (select-keys source [:position :direction :flow-m3-s :local-m3 :emitted-m3])
                                           (assoc :fixture-id (or (get @fog-ids (:fixture-id source))
                                                                  (stable-id (:fixture-id source))))))
                          (take 12 (:sources sample))))))

(defn- drawable-shape [spec]
  ;; Shape validation is the show owner's responsibility. Bound the geometry
  ;; handed to a 960-pixel monitor, including nested boolean fields.
  (let [children #(concat (:parts %) (when-let [child (:geometry %)] [child]))
        nodes (take 17 (tree-seq #(seq (children %)) children spec))]
    (when (and (<= (count nodes) 16) (<= (reduce + (map #(count (:planes %)) nodes)) 12))
      (letfn [(portable [v]
                (cond-> (select-keys v [:shape :position :radius :height :size :rounding :min :max
                                        :translation :scale :strength :planes])
                  (:geometry v) (assoc :geometry (portable (:geometry v)))
                  (:parts v) (assoc :parts (mapv portable (:parts v)))))]
        (portable spec)))))

(defn- optical-signals [model]
  (let [configured (get-in model [:settings :spatial :volumes])]
    (into {}
          (keep-indexed
           (fn [i [id signal]]
             (when-let [spec (or (:volume signal) (get configured id))]
               (when-let [geometry (drawable-shape spec)]
                 [(str i) (assoc (select-keys signal [:position :radius :deformation])
                                 :paint (select-keys (:paint signal) [:color]) :volume geometry)]))))
          (take 4 (get-in model [:variables :spatial :signals])))))

(defn- selected-offset [{:keys [ids total]} config view]
  (when (pos? total)
    (let [id (or (:selected-fixture config) (first (get-in view [:effect :targets])))
          baseline (if id (get @ids (wire-id id) 0) 0)]
      (mod (+ baseline (long (or (:fixture-offset view) 0))) total))))

(defn- focus-plan!
  "Page through every patched fixture, even one omitted by the device sample."
  [cache plan offset]
  (if (or (nil? offset) (some #(= offset (:offset %)) (:slots plan))) plan
    (if (= offset (:focused-offset @cache)) (:focused-plan @cache)
      (let [fixture (nth (get-in plan [:source :fixtures]) offset)
            slot (str offset)
            replace-last #(conj (pop %) %2)
            focused (-> (dissoc plan :focused-offset :focused-plan)
                        (update :slots replace-last {:offset offset :slot slot :fixture fixture})
                        (update-in [:rig :fixtures] replace-last (display-fixture slot fixture)))]
        (vswap! cache assoc :focused-offset offset :focused-plan focused)
        focused))))

(defn- rig-editor [slots values offset]
  (let [{:keys [slot fixture] :as selected} (some #(when (= offset (:offset %)) %) slots)
        {:keys [aim pose]} (get values slot)
        origin (:position fixture)
        axis (:direction pose)
        target (or aim (when (and origin axis) (mapv #(+ %1 (* 1.6 %2)) origin axis)))]
    (cond-> {:tool "rig"}
      selected (assoc :selection {:kind "fixtures" :id slot} :fixture-name (short-text (or (:name fixture) (:id fixture))))
      (and origin target) (assoc :points [origin target]))))

(defn project-display [cache {:keys [model output]} view]
  (let [mode (some-> (:display-mode view) keyword)]
    (if (or (not (#{:simulator :rig} mode)) (get-in view [:target-picker :open?])) view
      (let [base (plan! cache (:rig model))
            config (get-in model [:settings :controllers :push-display])
            offset (when (= mode :rig) (selected-offset base config view))
            {:keys [bounds rig total slots] :as plan} (focus-plan! cache base offset)
            values (lighting output plan)
            camera (get #{:fit :panorama :audience} (some-> (or (:camera view) (:camera config)) keyword) :fit)
            editor (when (= mode :rig) (rig-editor slots values offset))]
        {:display-mode mode
         :scene (cond-> (merge (select-keys model [:epoch :revision :frame-number :blackout? :master])
                               {:rig rig :camera camera :bounds bounds :fixture-count total
                                :show-id (short-text (:show-id model)) :runtime-id (:runtime-id view)
                                :optical-signals (optical-signals model)
                                :time-ms (:last-frame-time-ms model (:time-ms model 0))
                                :output {:values values :atmosphere (atmosphere (:atmosphere output) (:fog-ids plan))}})
                  editor (assoc :editor editor))}))))
