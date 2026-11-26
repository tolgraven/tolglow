(ns tolglow.compiler.video
  "The `{:op :video}` effect output (#672, #674): colour or luminance sampled
  from a video source, as ordinary per-target assignments. Because it is an
  output of a normal effect, a video cue composes with other cues by priority
  and blend, follows instance opacity and envelopes, and stops with the show.

    {:op :video :mapping :wall}                       ; linear RGB, for :color
    {:op :video :mapping :wall :channel :luma}        ; Rec. 709 luminance, for :dimmer
    {:op :video :mapping :wall
     :placement {:x {:op :param :key :drift} :rotation 15}}
    {:op :video :surface :sheet                       ; a 3D surface (#674)
     :placement {:crop-x {:op :param :key :slide} :phase {:op :beat}}}

  A 2D :mapping placement moves the whole mapping over the source: :x and :y
  in source units, :scale about the source centre, :rotation in degrees.
  A :surface placement takes `tolglow.video.surface/placement-keys`: crop
  offset and zoom, venue offset, scale, pitch/yaw/roll, wave amplitude,
  wavelength and phase, and contact distance. Each is a frame-level numeric
  expression, so cue parameters, modulation and macros can drive it.

  A surface output writes the heads in contact with the surface. A head in
  the contact feather writes with that weight as its opacity, so the look
  below shows through. The evaluated surface is recorded in the instance
  state under `observation-key`; the show's Room projection reads it there
  and never evaluates the placement again. The host supplies the latched
  frames and settings under :tolglow.video/input in the frame; this
  namespace never reads a source."
  (:require [afterglow.render.ir :as ir]
            [afterglow.spatial.geometry :as geometry]
            [tolglow.video.mapping :as mapping]
            [tolglow.video.sampler :as sampler]
            [tolglow.video.surface :as surface]))

(def input-key :tolglow.video/input)

(def observation-key
  "Instance state key holding each surface output's last evaluated surface,
  by output index."
  :tolglow.video/surfaces)

(def channels #{:color :luma})

(def placement-keys #{:x :y :scale :rotation})

(defn video-output? [value] (and (map? value) (= :video (:op value))))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn- candidate-filter
  "The spatial rig's box candidates (#407) for a surface's reach, as an id
  predicate; nil (every head) when the rig cannot be indexed."
  [rig resolved]
  (when rig
    (let [ids (geometry/reduce-box-candidates rig (surface/bounds resolved) (fn [s [id _]] (conj! s id)) (transient #{}))]
      (persistent! ids))))

(defn surface-plan
  "Contacts and a sampling plan for a resolved surface over the rig and a
  frame size. rig is an optional `afterglow.spatial.geometry/prepare-rig`."
  [resolved fixtures rig width height]
  (let [contacts (surface/contacts resolved fixtures (candidate-filter rig resolved))
        prints (surface/footprints resolved contacts width height)]
    {:contacts contacts
     :plan (sampler/plan-footprints (map :id contacts) prints (:filter resolved)
                                    (surface/wrap-span resolved width) width height)}))

(defn- spatial-rig [fixtures]
  (try (geometry/prepare-rig fixtures) (catch clojure.lang.ExceptionInfo _ nil)))

(defn- mapping-sampler [mapping channel placement-fns]
  (let [cache (sampler/plan-cache)]
    (fn [env frame instance]
      (let [input (get frame input-key)
            mapping-spec (get-in input [:mappings mapping])
            frames (or (get-in input [:held (:id instance)]) (:frames input))
            f (get frames (:source mapping-spec))]
        (if-not (and mapping-spec f)
          {:values {}}
          (let [placed (into mapping/default-placement
                             (map (fn [[k evaluate]] [k (double (evaluate env))])) placement-fns)
                fixtures (:fixtures frame [])
                plan (cache [mapping-spec fixtures (:width f) (:height f) placed]
                            #(sampler/plan mapping-spec fixtures (:width f) (:height f) placed))
                colors (sampler/colors plan (sampler/sample plan f))]
            {:values (if (= :luma channel) (update-vals colors sampler/luma) colors)}))))))

(defn- surface-sampler [surface-id channel placement-fns]
  (let [plans (sampler/plan-cache) rigs (sampler/plan-cache 2)]
    (fn [env frame instance]
      (let [input (get frame input-key)
            spec (get-in input [:surfaces surface-id])
            frames (or (get-in input [:held (:id instance)]) (:frames input))
            f (get frames (:source spec))]
        (if-not spec
          {:values {}}
          (let [placed (into {} (map (fn [[k evaluate]] [k (double (evaluate env))])) placement-fns)
                resolved (surface/resolve-surface spec placed (:elapsed-beats env))
                observation {:surface surface-id :resolved resolved}]
            (if-not f
              {:values {} :observation observation}
              (let [fixtures (:fixtures frame [])
                    {:keys [contacts plan]}
                    (plans [resolved fixtures (:width f) (:height f)]
                           #(surface-plan resolved fixtures (rigs fixtures (fn [] (spatial-rig fixtures)))
                                          (:width f) (:height f)))
                    colors (sampler/colors plan (sampler/sample plan f))]
                {:values (if (= :luma channel) (update-vals colors sampler/luma) colors)
                 :opacity (into {} (keep (fn [{:keys [id weight]}] (when (< weight 1.0) [id weight]))) contacts)
                 :observation observation}))))))))

(defn observe
  "Instance state after a frame: each surface output's evaluated surface under
  `observation-key`, by output index. State without surface outputs is
  returned unchanged."
  [state outputs]
  (let [seen (into {} (keep-indexed (fn [i output] (when-let [o (:observation (:sampled-value output))] [i o]))) outputs)]
    (if (and (empty? seen) (not (and (map? state) (contains? state observation-key))))
      state
      (if (empty? seen) (dissoc state observation-key) (assoc state observation-key seen)))))

(defn compile-output
  "Validate a video output and attach its per-frame sampler. prepare resolves
  parameter references in a placement expression."
  [{:keys [attribute value] :as output} prepare path]
  (let [{:keys [mapping surface channel placement] :or {channel :color}} value
        surface? (contains? value :surface)
        allowed (if surface? surface/placement-keys placement-keys)]
    (when-not (every? #{:op :mapping :surface :channel :placement} (keys value))
      (fail! path "A video output takes :mapping or :surface, :channel and :placement"))
    (when (= surface? (contains? value :mapping))
      (fail! path "A video output samples either a 2D :mapping or a 3D :surface"))
    (when-not (mapping/identifier? (if surface? surface mapping))
      (fail! (conj path (if surface? :surface :mapping))
             (if surface? "Name the video surface to sample" "Name the video mapping to sample")))
    (when-not (channels channel) (fail! (conj path :channel) "Video channel is :color or :luma"))
    (when-not (= (= :color attribute) (= :color channel))
      (fail! path (if (= :color channel) "Video colour feeds the :color attribute" "Video luminance feeds a scalar attribute")))
    (when-not (or (nil? placement) (and (map? placement) (every? allowed (keys placement))))
      (fail! (conj path :placement) (if surface?
                                      "Surface placement takes crop, position, scale, rotation, wave and contact keys"
                                      "Placement takes :x, :y, :scale and :rotation")))
    (let [placement-fns
          (into {}
                (map (fn [[k expression]]
                       (let [p (conj path :placement k)
                             prepared (prepare expression p)
                             errors (ir/validate-expression prepared)]
                         (when (seq errors) (fail! p (:message (first errors))))
                         (when-not (= :number (ir/expression-type prepared)) (fail! p "Placement values are numbers"))
                         (when (some #{[:position] [:surface-uv]} (ir/input-paths prepared))
                           (fail! p "Placement moves the whole mapping; it cannot read fixture position"))
                         [k (ir/compile-expression prepared)])))
                placement)]
      (assoc output :blend (:blend output :replace) :video value :frame-only? false
             :inputs (into [] (mapcat #(ir/input-paths (prepare % [])) (vals placement)))
             :sample-video (if surface?
                             (surface-sampler surface channel placement-fns)
                             (mapping-sampler mapping channel placement-fns))))))
