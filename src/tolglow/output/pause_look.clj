(ns tolglow.output.pause-look
  "Translate the saved pause look into the final ordered emission contribution.")

(def defaults {:mode :show :color [1.0 0.4 0.12] :level 0.08})

(defn- level? [v] (and (number? v) (<= 0 v 1)))

(defn normalize [input]
  (let [{:keys [mode color level] :as look} (merge defaults input)]
    (when-not (and (map? input) (every? (set (keys defaults)) (keys input))
                   (contains? #{:show :blackout :ambient} mode)
                   (vector? color) (= 3 (count color)) (every? level? color) (level? level))
      (throw (ex-info "Choose a pause mode, an RGB color and a brightness between zero and one."
                      {:path [:settings :output :pause-look]})))
    look))

(defn blackout? [model]
  (or (:blackout? model)
      (and (false? (get-in model [:transport :playing?]))
           (= :blackout (get-in model [:settings :output :pause-look :mode])))))

(defn- fixture-assignments [{:keys [id channels rgbw-policy]} {:keys [color level]}]
  (let [rgb? (every? channels [:red :green :blue])
        [r g b] color
        w (if (and rgb? (= :extract-min rgbw-policy)) (min r g b) 0)
        ;; Explicit emitter writes take precedence over a color in the renderer.
        ;; Replace both so a cue driving individual channels cannot leak through.
        values {:red (- r w) :green (- g w) :blue (- b w) :white (if rgb? w (min r g b))
                :amber 0 :uv 0 :strobe 0}
        attributes (cond-> (select-keys values (keys channels))
                     rgb? (assoc :color color)
                     (or (contains? channels :dimmer) (some channels [:red :green :blue :white :amber :uv]))
                     (assoc :dimmer level))]
    (map (fn [[attribute value]] {:target-id id :attribute attribute :value value}) attributes)))

(defn ambient-layer
  "Compile one immutable final contribution for this rig and pause look."
  [fixtures look]
  (let [settings (merge defaults look)]
    {:kind :assignments :assignments (into [] (mapcat #(fixture-assignments % settings)) fixtures)}))
