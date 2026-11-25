(ns tolglow.controllers.push-variations
  "Push interaction for the common cue parameter variation projection."
  (:require [tolglow.authoring.cue-variations :as ratios]
            [tolglow.authoring.variation-control :as variations]))

(def ^:private ratio-grid
  (for [[y values] [[7 [1/8 1/6 1/4 1/3 3/8 1/2 2/3 3/4]]
                    [6 [1 4/3 3/2 2 3 4 6 8]]]
        [x value] (map-indexed vector values)]
    {:position [x y] :value value}))

(defn- ratio-projection [{:keys [instance-id param positions] :as selection} model]
  (let [{:keys [cue-id phase definition params]} (get-in model [:instances instance-id])
        schema (get-in definition [:params param])
        value (get params param)
        mode (get-in model [:extensions variations/state-key cue-id :ratio-mode] :preferred)]
    (when (and (= :running phase) (get-in model [:registry :cues cue-id])
               (number? value) (ratios/ratio-schema? schema))
      (let [choices (filterv #(and (positions (:position %))
                                   (ratios/valid-beat-ratio? schema (:value %) mode)) ratio-grid)
            current (if (decimal? value) (rationalize value) value)]
        (when (seq choices)
          {:view (assoc selection :cells choices :value current :ratio-mode mode)
           :display {:layout :beat-ratios :param param :title (:name schema)
                     :ratio-mode mode :current-ratio (str value)
                     :cells (mapv (fn [{:keys [position value]}]
                                    {:position position :label (str value) :selected? (== current value)})
                                  choices)}})))))

(defn- open-ratio-encoder [model instance-id param positions]
  (let [selection {:layout :beat-ratios :instance-id instance-id :param param :positions (set positions)}]
    (when (ratio-projection selection model) selection)))

(defn- ratio-input [selection position presented model]
  (let [{:keys [instance-id param]} presented
        value (some #(when (= position (:position %)) (:value %)) (:cells presented))]
    {:state selection
     :encoder-sample? true
     :commands (when (and value (not (== value (:value presented)))
                          (= presented (:view (ratio-projection selection model))))
                 [{:op :cue-variation/ratio-set :instance-id instance-id :param param
                   :value value
                   :numerator (if (ratio? value) (numerator value) value)
                   :denominator (if (ratio? value) (denominator value) 1)}])}))

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

(defn- admitted-choice [presented op fields]
  (merge (select-keys presented [:instance-id :basis :layout :seed :row-page]) fields {:op op}))

(defn- input [selection {:keys [action position]} model
              {:keys [presented selected-instance record?]}]
  (let [instance-id (:instance-id selection)]
    (cond
      (= :variation/cycle action)
      (cond
        (and record? presented) {:state selection :commands [(admitted-choice presented :cue-variation/append {})]}
        record? {:state selection}
        (= :rows (:layout selection)) {:state nil}
        (= :full (:layout selection)) {:state (assoc selection :layout :rows)}
        (and selected-instance (get-in model [:instances selected-instance :cue-id]))
        {:state {:instance-id selected-instance :layout :full :row-page 0 :seed 0}}
        :else {:state nil})

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

(def pad-overlay {:input push-2-input :project projection})

(def push-3-pad-overlay
  {:open-encoder open-ratio-encoder
   :input (fn [selection event model context]
            (if (= :beat-ratios (:layout selection))
              (ratio-input selection (:position event) (:presented context) model)
              (input selection event model context)))
   :project (fn [selection model prior]
              (if (= :beat-ratios (:layout selection))
                (ratio-projection selection model)
                (projection selection model prior)))})
