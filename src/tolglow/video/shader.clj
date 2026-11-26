(ns tolglow.video.shader
  "Shader cues (#673): the `:shader` block of an ordinary effect definition.

    {:id :plasma :version 1
     :params  {:speed {:type :number :default 1 :bounds [0 4]}
               :tint  {:type :color :default [1 0.4 0.1]}}
     :shader  {:glsl \"void main() { ... fragColor = vec4(c * u_tint, 1.0); }\"
               :size [640 360]                 ; render target, pixels
               :fps 30                         ; video cadence, independent of lighting
               :viewport [0 0 640 360]         ; optional, top-down pixels in the target
               :feed :shader/plasma            ; optional; default :shader/<effect id>
               :uniforms {:u_speed {:type :float :value {:op :param :key :speed}}
                          :u_tint  {:type :vec3 :value {:op :param :key :tint}}}}
     :outputs {:color {:op :video :mapping :plasma-strip}}}

  Each uniform value is an ordinary numeric expression over the cue's
  parameters, clocks and modulation, so the cue's controls drive the shader.
  The effect step evaluates them once per lighting frame and keeps them in its
  instance state, so a paused instance or a stopped transport freezes them
  with every other effect. A separately scheduled producer
  (`tolglow.video.shader-producer`) renders at :fps from the committed state
  and publishes the last completed frame under the feed id, where `{:op
  :video}` mappings, Room surfaces and direct outputs read it.

  Every shader also receives `u_time` (seconds since launch, a deliberate
  physical clock at the render boundary) and `u_beats` (beats since launch),
  plus Afterglow's `u_resolution` and `u_origin`. GLSL rules, uniform types
  and render target limits are Afterglow's (`afterglow.render.shader-frames`)."
  (:require [afterglow.render.ir :as ir]
            [afterglow.render.shader-frames :as frames]
            [clojure.string :as str]))

(def state-key
  "Instance state key holding the uniforms the last lighting frame evaluated."
  ::uniforms)

(def fps-range [1 60])

(def default-fps 30)

(def clock-uniforms
  "Uniforms every shader cue receives from its instance clock."
  {"u_time" :float "u_beats" :float})

(def ^:private ir-types
  {:float :number :int :number :bool :number :vec2 :vec2 :vec3 :vec3 :vec4 :vec4})

(defn shader? [definition] (map? (:shader definition)))

(defn feed-id
  "The feed a shader effect publishes: its :feed, else :shader/<effect id>."
  [definition]
  (or (get-in definition [:shader :feed])
      (keyword "shader" (let [id (:id definition)] (if (keyword? id) (str/replace (subs (str id) 1) "/" ".") (str id))))))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn- uniform-name [k] (if (keyword? k) (name k) k))

(defn afterglow-shader
  "The portable shader map Afterglow renders: GLSL source and uniform types,
  including the clock uniforms."
  [shader]
  {:source (:glsl shader)
   :uniforms (into clock-uniforms
                   (map (fn [[k {:keys [type]}]] [(uniform-name k) type]))
                   (:uniforms shader))})

(defn- check-block! [shader path]
  (when-not (map? shader) (fail! path "A shader is a map with :glsl, :size and :uniforms"))
  (when-not (every? #{:glsl :size :fps :viewport :feed :uniforms :kind} (keys shader))
    (fail! path "A shader takes :glsl, :size, :fps, :viewport, :feed, :kind and :uniforms"))
  (let [{:keys [size fps viewport feed uniforms kind]} shader
        [w h] size]
    (when-not (and (vector? size) (= 2 (count size)))
      (fail! (conj path :size) "Size is [width height] in pixels"))
    (when-let [e (first (frames/target-errors {:width w :height h :viewport viewport}))]
      (fail! (into path (if (= [:viewport] (:path e)) [:viewport] [:size])) (:message e)))
    (when (and (some? fps) (not (and (number? fps) (<= (first fps-range) fps (second fps-range)))))
      (fail! (conj path :fps) "Frames per second is 1–60"))
    ;; Feeds live in their own namespace, so a shader never replaces a hub source.
    (when (and (some? feed) (not (and (keyword? feed) (= "shader" (namespace feed)))))
      (fail! (conj path :feed) "A feed id is a keyword in the shader namespace, such as :shader/wall"))
    (when (and (some? kind) (not (#{:2d :3d} kind)))
      (fail! (conj path :kind) "Kind is :2d or :3d"))
    (when-not (or (nil? uniforms) (map? uniforms))
      (fail! (conj path :uniforms) "Uniforms are a map from name to {:type :value}"))
    (doseq [[k spec] uniforms]
      (when (contains? clock-uniforms (uniform-name k))
        (fail! (conj path :uniforms k) "u_time and u_beats come from the instance clock"))
      (when-not (and (map? spec) (every? #{:type :value} (keys spec)) (contains? spec :value))
        (fail! (conj path :uniforms k) "A uniform is {:type :value}")))
    (when-let [e (first (frames/shader-errors (afterglow-shader shader)))]
      (fail! (into path (case (first (:path e))
                          :source [:glsl]
                          :uniforms (cond-> [:uniforms] (second (:path e)) (conj (keyword (second (:path e)))))
                          []))
             (:message e)))))

(defn compile-shader
  "Validate a definition's :shader block and compile its uniform expressions.
  prepare resolves parameter references (the effect compiler's own). Returns
  a plan whose :evaluate turns a frame environment into GLSL uniform values."
  [definition prepare]
  (let [path [:shader]
        shader (:shader definition)
        _ (check-block! shader path)
        evaluators
        (vec (for [[k {:keys [type value]}] (sort-by (comp uniform-name key) (:uniforms shader))]
               (let [p (into path [:uniforms k :value])
                     prepared (prepare value p)
                     errors (ir/validate-expression prepared)]
                 (when (seq errors) (fail! (into p (:path (first errors))) (:message (first errors))))
                 (when-not (= (ir-types type) (ir/expression-type prepared))
                   (fail! p (str "A " (name type) " uniform needs a " (name (ir-types type)) " expression")))
                 (when (some #{[:position] [:surface-uv]} (ir/input-paths prepared))
                   (fail! p "A uniform is one value per frame; it cannot read fixture position"))
                 [(uniform-name k) type (ir/compile-expression prepared)])))
        [w h] (:size shader)]
    {:feed (feed-id definition)
     :shader (afterglow-shader shader)
     :width w :height h :viewport (:viewport shader)
     :fps (:fps shader default-fps)
     :kind (:kind shader :2d)
     :inputs (into [] (mapcat (fn [[_ {:keys [value]}]] (ir/input-paths (prepare value []))))
                   (:uniforms shader))
     :evaluate
     (fn [env]
       (into {"u_time" (/ (double (:elapsed-ms env 0)) 1000.0)
              "u_beats" (double (or (:elapsed-beats env) 0.0))}
             (map (fn [[n type evaluate]]
                    (let [v (evaluate env)]
                      [n (case type
                           :int (Math/round (double v))
                           :bool (if (>= (double v) 0.5) 1 0)
                           (if (vector? v) (mapv double v) (double v)))])))
             evaluators))}))

(defn uniforms-of
  "The uniforms an instance's last lighting frame evaluated, or nil."
  [instance]
  (let [state (:state instance)] (when (map? state) (get state state-key))))

(defn with-uniforms
  "Keep evaluated uniforms in the instance state the step returns."
  [state uniforms]
  (if (or (nil? state) (map? state)) (assoc state state-key uniforms) state))

(defn requests
  "Portable producer requests for the running shader instances of model:
  instance id -> {:feed :order :shader :width :height :viewport :fps
  :uniforms :paused?}. An instance appears once its first lighting frame has
  evaluated uniforms."
  [model]
  (into {}
        (keep (fn [[id instance]]
                (let [definition (:definition instance)
                      shader (:shader definition)
                      uniforms (uniforms-of instance)]
                  (when (and (map? shader) uniforms)
                    (let [[w h] (:size shader)]
                      [id {:feed (feed-id definition)
                           :effect [(:id definition) (:version definition)]
                           :order (:order instance 0)
                           :shader (afterglow-shader shader)
                           :width w :height h :viewport (:viewport shader)
                           :fps (:fps shader default-fps)
                           :uniforms uniforms
                           :paused? (boolean (get-in instance [:system :paused?]))}])))))
        (:instances model)))
