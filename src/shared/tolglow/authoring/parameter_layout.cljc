(ns tolglow.authoring.parameter-layout
  "Resolve saved cue and effect layouts against the live schema. A layout
  orders matching controls and chooses a monitor. Newly added controls always
  follow the authored ones.")

(def templates
  {:wash {:order [:color :level :dimmer :white :gain] :monitor :level}
   :movement {:order [:aim :position :bearing :pan :tilt :width :rate :speed] :monitor :aim}
   :pulse {:order [:level :period :waveform :phase :duty] :monitor :level}
   :flash {:order [:level :rate :duration :release] :monitor :level}
   :volume {:order [:position :translation :radius :shape :size :strength :color
                    :scale :velocity :restitution :gravity :inner-m :outer-m]
            :monitor :position}
   :optics {:order [:focus :iris :zoom :spread] :monitor :focus}})

(defn- id-key [id]
  (when (or (string? id) (keyword? id)) (keyword id)))

(defn- suggested-template [definition]
  (let [tags (into #{} (map id-key) (:tags definition))
        params (into #{} (map id-key) (keys (:params definition)))]
    (cond
      (or (:spatial-volume definition) (and (params :translation) (params :strength))) :volume
      (or (tags :movement) (some params [:aim :pan :tilt :position])) :movement
      (or (tags :transient) (params :strobe)) :flash
      (some params [:period :phase :waveform]) :pulse
      (or (tags :emission) (some params [:level :color :dimmer])) :wash
      (some params [:zoom :iris :focus]) :optics)))

(defn- layout-map [spec]
  (cond
    (map? spec) spec
    (id-key spec) {:template (id-key spec)}
    :else {}))

(defn resolve-layout
  "A cue can override the template, order or monitor independently. Individual
  parameter :ui/order takes priority over a template's default order."
  [definition cue]
  (let [effect-layout (layout-map (:ui/layout definition))
        cue-layout (layout-map (:ui/layout cue))
        custom (merge effect-layout cue-layout)
        template (get templates (id-key (or (:template custom) (suggested-template definition))))
        position (fn [ids] (zipmap (distinct (keep id-key (when (sequential? ids) ids))) (range)))
        authored (position (:order custom))
        defaults (position (:order template))
        rank (fn [[id schema]]
               (let [k (id-key id)]
                 [(get authored k 1000000)
                  (if (number? (:ui/order schema)) (:ui/order schema) 1000000)
                  (if (:control/axis schema) 0 1)
                  (get defaults k 1000000)
                  (str k)]))]
    {:parameters (vec (sort-by rank (:params definition)))
     :monitor (id-key (or (:monitor custom) (:monitor template)))}))
