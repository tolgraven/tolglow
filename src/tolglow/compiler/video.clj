(ns tolglow.compiler.video
  "The `{:op :video}` effect output (#672): colour or luminance sampled from a
  mapped video source, as ordinary per-target assignments. Because it is an
  output of a normal effect, a video cue composes with other cues by priority
  and blend, follows instance opacity and envelopes, and stops with the show.

    {:op :video :mapping :wall}                       ; linear RGB, for :color
    {:op :video :mapping :wall :channel :luma}        ; Rec. 709 luminance, for :dimmer
    {:op :video :mapping :wall
     :placement {:x {:op :param :key :drift} :rotation 15}}

  :placement moves the whole mapping over the source: :x and :y in source
  units, :scale about the source centre, :rotation in degrees. Each is a
  frame-level numeric expression, so cue parameters, modulation and macros can
  drive it. The host supplies the latched frames and mapping settings under
  :tolglow.video/input in the frame; this namespace never reads a source."
  (:require [afterglow.render.ir :as ir]
            [tolglow.video.mapping :as mapping]
            [tolglow.video.sampler :as sampler]))

(def input-key :tolglow.video/input)

(def channels #{:color :luma})

(def placement-keys #{:x :y :scale :rotation})

(defn video-output? [value] (and (map? value) (= :video (:op value))))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn compile-output
  "Validate a video output and attach its per-frame sampler. prepare resolves
  parameter references in a placement expression."
  [{:keys [attribute value] :as output} prepare path]
  (let [{:keys [mapping channel placement] :or {channel :color}} value]
    (when-not (every? #{:op :mapping :channel :placement} (keys value))
      (fail! path "A video output takes :mapping, :channel and :placement"))
    (when-not (mapping/identifier? mapping) (fail! (conj path :mapping) "Name the video mapping to sample"))
    (when-not (channels channel) (fail! (conj path :channel) "Video channel is :color or :luma"))
    (when-not (= (= :color attribute) (= :color channel))
      (fail! path (if (= :color channel) "Video colour feeds the :color attribute" "Video luminance feeds a scalar attribute")))
    (when-not (or (nil? placement) (and (map? placement) (every? placement-keys (keys placement))))
      (fail! (conj path :placement) "Placement takes :x, :y, :scale and :rotation"))
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
                placement)
          cache (sampler/plan-cache)]
      (assoc output :blend (:blend output :replace) :video value :frame-only? false
             :inputs (into [] (mapcat #(ir/input-paths (prepare % [])) (vals placement)))
             :sample-video
             (fn [env frame instance]
               (let [input (get frame input-key)
                     mapping-spec (get-in input [:mappings mapping])
                     frames (or (get-in input [:held (:id instance)]) (:frames input))
                     f (get frames (:source mapping-spec))]
                 (if-not (and mapping-spec f)
                   {}
                   (let [placed (into mapping/default-placement
                                      (map (fn [[k evaluate]] [k (double (evaluate env))])) placement-fns)
                         fixtures (:fixtures frame [])
                         plan (cache [mapping-spec fixtures (:width f) (:height f) placed]
                                     #(sampler/plan mapping-spec fixtures (:width f) (:height f) placed))
                         colors (sampler/colors plan (sampler/sample plan f))]
                     (if (= :luma channel) (update-vals colors sampler/luma) colors)))))))))
