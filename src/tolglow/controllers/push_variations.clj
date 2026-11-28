(ns tolglow.controllers.push-variations
  "Push interaction for the common cue parameter variation projection, and the
  knob-look layout (#547) whose top pad row saves and recalls linked-knob
  positions through the shared tolglow.knob-presets commands."
  (:require [tolglow.authoring.cue-variations :as ratios]
            [tolglow.authoring.variation-control :as variations]
            [tolglow.knob-presets :as knob-presets]))

(def ^:private ratio-grid
  (for [[y values] [[7 [1/8 1/6 1/4 1/3 3/8 1/2 2/3 3/4]]
                    [6 [1 4/3 3/2 2 3 4 6 8]]]
        [x value] (map-indexed vector values)]
    {:position [x y] :value value}))

(defn- ratio-basis [model instance-id param]
  (let [{:keys [cue-id phase definition params effect-version]} (get-in model [:instances instance-id])
        cue (get-in model [:registry :cues cue-id])
        schema (get-in definition [:params param])
        current (get params param)
        mode (get-in model [:extensions variations/state-key cue-id :ratio-mode] :preferred)]
    (when (and (= :running phase) cue (number? current) (ratios/ratio-schema? schema))
      {:instance-id instance-id :param param :cue-id cue-id :cue-version (:version cue)
       :cue-name (or (:name cue) (name cue-id)) :effect-version effect-version
       :schema schema :current current :mode mode})))

(defn- spread [choices limit]
  (let [size (count choices)]
    (cond
      (>= limit size) choices
      (= 1 limit) [(nth choices (quot size 2))]
      :else (mapv #(nth choices (quot (* % (dec size)) (dec limit))) (range limit)))))

(defn- free-ratios [schema current]
  (cond-> (ratios/beat-ratio-choices schema current :free)
    (ratios/valid-beat-ratio? schema (:default schema) :free) (conj (:default schema))))

(defn- free-pad-choices [captured schema current]
  (let [fixed (filterv #(and (= [1 6] (:position %))
                             (ratios/valid-beat-ratio? schema (:value %) :free)) captured)
        fixed-positions (into #{} (map :position) fixed)
        fixed-values (into #{} (map :value) fixed)
        remaining (into [] (comp (map :position) (remove fixed-positions)) captured)
        selected (cond-> [] (and (seq remaining) (not (fixed-values current))
                                  (ratios/valid-beat-ratio? schema current :free)) (conj current))
        limit (- (count remaining) (count selected))
        values (if (pos? limit)
                 (let [excluded (into fixed-values selected)
                       candidates (into [] (comp (remove excluded) (distinct))
                                        (free-ratios schema current))
                       {common true additional false} (group-by #(ratios/valid-beat-ratio? schema % :preferred) candidates)
                       extra (spread (vec (sort additional)) limit)]
                   (into (into selected extra) (spread (vec (sort common)) (- limit (count extra)))))
                 selected)
        choices (into fixed (map (fn [position value] {:position position :value value}) remaining values))]
    ;; A sole captured pad can offer a change even when the current value is allowed.
    (if (and (= 1 (count captured)) (= current (:value (first choices))))
      (if-let [different (first (remove #{current} (free-ratios schema current)))]
        (assoc-in choices [0 :value] different)
        choices)
      choices)))

(defn- ratio-pad-choices [{:keys [schema current mode]} positions]
  (let [captured (filterv #(positions (:position %)) ratio-grid)]
    (if (= :free mode)
      (free-pad-choices captured schema current)
      (filterv #(ratios/valid-beat-ratio? schema (:value %) mode) captured))))

(defn- ratio-projection [{:keys [instance-id param positions] :as selection} model prior]
  (when-let [{:keys [schema current mode cue-name] :as basis} (ratio-basis model instance-id param)]
    (let [key [selection basis]]
      (if (= key (:cache-key prior))
        prior
        (let [choices (ratio-pad-choices basis positions)]
          (when (seq choices)
            {:cache-key key :view (assoc selection :cells choices :basis basis)
             :display {:layout :beat-ratios :param param :title (str cue-name " · " (or (:name schema) (name param)))
                       :ratio-mode mode :current-ratio (str current)
                       :cells (mapv (fn [{:keys [position value]}]
                                      {:position position :label (str value) :selected? (= current value)})
                                    choices)}}))))))

(defn- open-ratio-encoder [model instance-id param positions]
  (let [selection {:layout :beat-ratios :instance-id instance-id :param param :positions (set positions)}]
    (when (ratio-projection selection model nil) selection)))

(defn- ratio-input [selection position presented model]
  (let [{:keys [instance-id param basis positions]} presented
        {:keys [value] :as choice} (some #(when (= position (:position %)) %) (:cells presented))]
    {:state selection
     :encoder-sample? true
     :sample-context {::ratio-pad {:basis basis :positions positions :choice choice}}
     ;; A pad can replace a decimal approximation with an exact ratio.
     :commands (when (and value (not= value (:current basis))
                          (= basis (ratio-basis model instance-id param)))
                 [{:op :cue-variation/ratio-set :instance-id instance-id :param param
                   :value value
                   :numerator (if (ratio? value) (numerator value) value)
                   :denominator (if (ratio? value) (denominator value) 1)}])}))

(defn admissible-ratio-pad-sample?
  "Recheck the presented pad against the show owner's current ratio state."
  [model {:keys [instance-id param sample-context value]}]
  (let [{:keys [basis positions choice]} (::ratio-pad sample-context)
        current (ratio-basis model instance-id param)]
    (and current (= basis current) (set? positions) (some? value) (= value (:value choice))
         (some #{choice} (ratio-pad-choices current positions)))))

(defn- projection [selection model prior]
  (let [{:keys [instance-id]} selection
        instance (get-in model [:instances instance-id])
        cue-id (:cue-id instance)]
    (when (and (= :running (:phase instance)) (get-in model [:registry :cues cue-id]))
      (let [key [selection (:params instance) (:definition instance)
                 (get-in model [:extensions variations/state-key cue-id])]]
        (if (= key (:cache-key prior))
          prior
          (let [view (variations/project model selection)]
            {:cache-key key :view view
             :display (-> (select-keys view [:layout :ratio-mode :rows :row-page :row-pages])
                          (assoc :title (if (= :rows (:layout view)) "Cue parameter rows" "Full cue variations")
                                 :cells (mapv #(select-keys % [:position :kind :stale?]) (:cells view))))
             :colors (into {} (map (fn [{:keys [position kind stale?]}]
                                     [position (cond stale? [100 20 55]
                                                     (= :saved kind) [25 155 200]
                                                     :else [220 110 32])]))
                           (:cells view))}))))))

(def knob-preset-row
  "Pad row (bottom-left origin) that holds the eight knob looks."
  7)

(def knob-preset-colors
  "Pad colours by slot state. A selected look whose knobs have since moved is
  drawn in a paler tint than one that still matches."
  {:empty [14 14 18] :stored [30 190 110] :selected [235 235 235] :selected-moved [120 150 255]})

(defn knob-preset-state
  "A knob-look cell's pad state: :empty, :stored, :selected, or
  :selected-moved for a selected look whose knobs have since moved."
  [{:keys [state matches?]}]
  (cond (= :empty state) :empty
        (and (= :selected state) matches?) :selected
        (= :selected state) :selected-moved
        :else :stored))

(defn- knob-preset-color [cell]
  (knob-preset-colors (knob-preset-state cell)))

(defn- knob-preset-projection
  "Top-row cells from the shared knob-look projection. The view keeps each
  slot's stored positions so a press can be refused if the slot changed."
  [selection model prior]
  (let [{:keys [slots] :as presets} (knob-presets/projection model)
        key [selection presets]]
    (if (= key (:cache-key prior))
      prior
      (let [cells (mapv (fn [{:keys [slot state matches? values knobs]}]
                          (cond-> {:position [slot knob-preset-row] :slot slot :state state}
                            (not= :empty state) (assoc :matches? matches? :values values
                                                       :knobs (mapv #(select-keys % [:name :knob-id :text :missing?])
                                                                    (take 4 knobs)))))
                        slots)]
        {:cache-key key
         :view {:layout :knob-presets :cells cells}
         :display {:layout :knob-presets :title "Knob looks"
                   :cells (mapv #(-> (dissoc % :values) (assoc :label (str "Look " (inc (:slot %))))) cells)}
         :colors (into {} (map (juxt :position knob-preset-color)) cells)}))))

(defn- knob-preset-input
  "Empty pad saves the knobs into it; a saved pad recalls it; Record with a
  saved pad overwrites it. Cycle closes the overlay. Every command carries the
  presented slot contents as :expected. Returns nil for events it leaves to Push."
  [selection {:keys [action position]} {:keys [presented record?]}]
  (cond
    (= :variation/cycle action) {:state (when record? selection)}
    action {:state selection}
    ;; Other controls (encoders, transport) stay with Push.
    (nil? position) nil
    :else
    (let [{:keys [slot state values] :as cell}
          (some #(when (= position (:position %)) %) (:cells presented))]
      {:state selection
       :commands (when cell
                   [(cond
                      (= :empty state) {:op :knob-preset/capture :slot slot :expected nil}
                      record? {:op :knob-preset/capture :slot slot :overwrite? true :expected values}
                      :else {:op :knob-preset/recall :slot slot :expected values})])})))

(defn- admitted-choice [presented op fields]
  (merge (select-keys presented [:instance-id :basis :layout :seed :row-page]) fields {:op op}))

(defn- input [selection {:keys [action position]} model
              {:keys [presented selected-instance record?] :as context}]
  (let [instance-id (:instance-id selection)]
    (cond
      (= :knob-presets (:layout selection))
      (knob-preset-input selection {:action action :position position} context)

      (= :variation/cycle action)
      (cond
        (and record? presented) {:state selection :commands [(admitted-choice presented :cue-variation/append {})]}
        record? {:state selection}
        ;; Knob looks follow the cue layouts, or open first without a selected cue.
        (= :rows (:layout selection)) {:state {:layout :knob-presets}}
        (= :full (:layout selection)) {:state (assoc selection :layout :rows)}
        (and selected-instance (get-in model [:instances selected-instance :cue-id]))
        {:state {:instance-id selected-instance :layout :full :row-page 0 :seed 0}}
        :else {:state {:layout :knob-presets}})

      (and selection (= :variation/ratio-mode action))
      (let [cue-id (get-in model [:instances instance-id :cue-id])
            mode (or (:ratio-mode presented)
                     (get-in model [:extensions variations/state-key cue-id :ratio-mode] :preferred))]
        {:state selection
         :commands [{:op :cue-variation/ratio-mode :instance-id instance-id
                     :mode (if (= :free mode) :preferred :free)}]})

      (and selection (#{:variation/previous-page :variation/next-page} action))
      {:state (if (= :rows (:layout selection))
                (update selection :row-page #(max 0 (min (dec (:row-pages presented 1))
                                                        (+ % (if (= :variation/previous-page action) -1 1)))))
                selection)}

      (and presented position)
      (let [cell (some #(when (= position (:position %)) %) (:cells presented))
            overwrite? (and record? (= :saved (:kind cell)))]
        {:state selection
         :commands (cond overwrite? [(admitted-choice presented :cue-variation/overwrite {:slot (first position)})]
                         (and (not record?) cell (not (:stale? cell)))
                         [(admitted-choice presented :cue-variation/apply {:position position})])}))))

(defn- push-2-input [selection {:keys [type controller raw-value]} model {:keys [position] :as context}]
  (let [event (cond
                (= :note-on type) {:position position}
                (and (= :control-change type) (pos? raw-value))
                {:action ({50 :variation/cycle 58 :variation/ratio-mode
                           62 :variation/previous-page 63 :variation/next-page} controller)})]
    (input selection event model context)))

(defn- overlay-projection [selection model prior]
  (if (= :knob-presets (:layout selection))
    (knob-preset-projection selection model prior)
    (projection selection model prior)))

(def pad-overlay {:input push-2-input :project overlay-projection})

(def push-3-pad-overlay
  {:open-encoder open-ratio-encoder
   :input (fn [selection event model context]
            (if (= :beat-ratios (:layout selection))
              (ratio-input selection (:position event) (:presented context) model)
              (input selection event model context)))
   :project (fn [selection model prior]
              (if (= :beat-ratios (:layout selection))
                (ratio-projection selection model prior)
                (overlay-projection selection model prior)))})
