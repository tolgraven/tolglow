(ns tolglow.studio.panel-tiles
  "Panels a layout can add: a parameter tile showing one chosen parameter and
  a visualization panel showing the running effects' parameters.

  Both read the same show projection as the Live cards and write through the
  same `:studio/param-set` command, using the shared parameter widgets and
  visualizations. Their content follows the tile's size tier: :full shows the
  control and a plot, :compact a numeric readout with a compact control, and
  :single (the 1×1 tile) one reading. A tile binds to a cue ID and a parameter
  name, not to an instance, so it follows that cue across launches, reloads and
  layout switches and says so when the cue is not running."
  (:require [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.panel-layout :as panel-layout]
            [tolglow.studio.parameter-source :as sources]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.param-visualization :as visualization]
            [tolglow.studio.performance :as performance]))

(defn- wire [value] (events/wire-id value))

(defn- cue-definition [show cue]
  (some #(when (= (wire (:id %)) (wire (:effect-id cue))) %) (:effects show)))

(defn- cue-name [cue] (or (some-> (:name cue) wire) (widgets/label (wire (:id cue)))))

(defn- param-name [schema param] (or (:name schema) (widgets/label param)))

(defn choices
  "Every cue parameter a tile can show, running cues first, as
  {:cue :param :label :running?} maps."
  [show]
  (let [running (set (map (comp wire :cue-id) (:instances show)))]
    (->> (for [cue (:cues show)
               :let [cue-id (wire (:id cue))
                     definition (cue-definition show cue)]
               [param schema] (:params definition)]
           {:cue cue-id :param (wire param) :running? (contains? running cue-id)
            :label (str (cue-name cue) " · " (param-name schema (wire param)))})
         (sort-by (complement :running?))
         vec)))

(defn resolve-binding
  "The cue, running instance, parameter key and schema a binding refers to.
  The instance is the cue's most recent launch in effective order."
  [show {:keys [cue param]}]
  (when (and cue param)
    (let [cue-record (some #(when (= cue (wire (:id %))) %) (:cues show))
          instance (last (filter #(= cue (wire (:cue-id %))) (performance/effective-instances show)))
          definition (or (when instance (performance/card-definition show instance)) (cue-definition show cue-record))
          [key schema] (some (fn [[k s]] (when (= param (wire k)) [k s])) (:params definition))]
      {:cue cue-record :instance instance :key key :schema schema
       :title (str (if cue-record (cue-name cue-record) cue) " · " (if schema (param-name schema param) param))})))

(defn current-value
  "The value to read out: the live effective value when a source drives the
  parameter, otherwise the committed value or its default."
  [instance key schema]
  (let [reading (sources/reading instance key)]
    (if (some? (:effective-value reading))
      (:effective-value reading)
      (get-in instance [:params key] (:default schema)))))

(defn viewable? [schema]
  (contains? #{:integer :number :range :vector :color} (widgets/parameter-kind schema)))

(defn picker
  "A menu of cue parameters for one tile."
  [show {:keys [id binding]} & [label]]
  (let [options (choices show)
        selected (first (keep-indexed (fn [i {:keys [cue param]}]
                                        (when (= [cue param] [(:cue binding) (:param binding)]) i))
                                      options))]
    [:select.tile-picker {:aria-label (or label (str "Parameter shown in " (panel-layout/title id)))
                          :value (if selected (str selected) "")
                          :on-change #(let [v (.. % -target -value)]
                                        (rf/dispatch [:panel-layout/bind id (when (seq v) (select-keys (nth options (js/parseInt v 10)) [:cue :param]))]))}
     [:option {:value ""} "Choose a parameter…"]
     (for [[i {:keys [label running?]}] (map-indexed vector options)]
       ^{:key i} [:option {:value (str i)} (str label (when running? " · running"))])]))

(defn- readout [schema value]
  [:output.tile-readout (if (some? value) (widgets/value-label schema value) "—")])

(defn param-tile
  "A tile showing one chosen parameter at its tier."
  [db parameter-control {:keys [id binding] :as entry} tier]
  (let [show (:show db)
        {:keys [instance key schema title]} (resolve-binding show binding)
        value (when (and instance key) (current-value instance key schema))]
    [:section.panel.parameter-tile {:aria-label (str "Parameter tile · " (or title "unassigned"))
                                    :data-tile (name id) :data-tier (name tier)
                                    :data-bound (boolean binding) :data-running (boolean instance)}
     [:div.tile-head
      [:span.tile-title (or title "Parameter tile")]
      (when-not (= :single tier) [picker show entry])]
     (cond
       (nil? binding)
       [:div.tile-empty
        (when (= :single tier) [picker show entry])
        [:p.small.muted "Choose a cue parameter to show here."]]

       (nil? schema)
       [:p.small.muted.tile-empty (str "This show has no " (:param binding) " on " (:cue binding) ".")]

       (nil? instance)
       [:div.tile-empty [readout schema nil]
        [:p.small.muted (str (:title (resolve-binding show binding)) " shows here while the cue runs.")]]

       (= :single tier)
       [:div.tile-single [readout schema value]]

       (= :compact tier)
       [:div.tile-compact [readout schema value]
        [parameter-control instance key schema nil {:compact? true}]]

       :else
       [:div.tile-full
        [parameter-control instance key schema]
        (when (and (viewable? schema) (not (sources/driven? instance key)))
          [visualization/studio schema #(current-value instance key schema)])])]))

(def ^:private viz-limit 16)

(defn viz-entries
  "The visualizable parameters of running effects, in effective order."
  [show]
  (->> (for [instance (performance/effective-instances show)
             :let [definition (performance/card-definition show instance)
                   cue (some #(when (= (wire (:cue-id instance)) (wire (:id %))) %) (:cues show))]
             [key schema] (:params definition)
             :when (viewable? schema)]
         {:instance instance :key key :schema schema
          :title (str (if cue (cue-name cue) (wire (:cue-id instance))) " · " (param-name schema (wire key)))})
       (take viz-limit)
       vec))

(defn viz-panel
  "Plots of the running effects' parameters at the panel's tier."
  [db tier]
  (let [entries (viz-entries (:show db))
        shown (if (= :single tier) (take 1 entries) entries)]
    [:section.panel.viz-panel {:aria-label "Parameter visualization" :data-tier (name tier)}
     [:div.panel-top [:span.section-label "PARAMETERS"]
      [:span.small.muted (str (count entries) (if (= 1 (count entries)) " parameter" " parameters"))]]
     (if (empty? entries)
       [:p.small.muted.tile-empty "Running effects' parameters appear here."]
       [:ul.viz-list
        (for [{:keys [instance key schema title]} shown]
          ^{:key [(wire (:id instance)) (wire key)]}
          [:li.viz-item {:data-instance (wire (:id instance)) :data-param (wire key)}
           [:div.viz-label [:span title] [readout schema (current-value instance key schema)]]
           (when (= :full tier)
             [visualization/studio schema #(current-value instance key schema)])])])]))

(defn render
  "Content for an added layout panel."
  [db parameter-control {:keys [id] :as entry} tier]
  (if (= :viz id)
    [viz-panel db tier]
    [param-tile db parameter-control entry tier]))

(defn chrome
  "Edit-mode controls for an added panel: a parameter tile's menu."
  [db {:keys [id] :as entry}]
  (when (not= :viz id)
    [picker (:show db) entry]))
