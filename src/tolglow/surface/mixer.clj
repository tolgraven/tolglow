(ns tolglow.surface.mixer
  "A mixer layout over a strip viewport (#705, #706 slice 2).

  Device-independent: it reads the committed show model and the logical
  surface's layout state (tolglow.surface.link), and returns per-strip desired
  state and owner command templates. Device profiles such as
  tolglow.surface.x-touch only translate addresses and bytes.

  The behaviour follows the operator's Live remote script for the X-Touch
  where a lighting equivalent exists, not plain MCU mode:

  - Strips are the logical surface's rows: effect groups (fader = committed
    group level) and configured cue channels (fader = a cue parameter,
    :level by default, on the cue's latest instance).
  - Encoder modes, committed per logical surface so linked units agree:
      :pan      one per-strip parameter (the channel's :encoder binding, else
                the first other typed parameter of its effect);
      :send     each cue strip's instance opacity (the lighting send);
      :plug-in  the selected strip's parameters across all encoders, paged
                by the logical span (a selected group shows its members'
                opacity, as the script shows rack chains);
      :user     linked knobs (#666) in creation order.
  - Ring modes come from the target's schema; the centre LED marks the
    default value.
  - Numbers map onto their hard bounds, else their recommended range; a
    number with neither is left off the encoders and faders, as modulation
    skips it, instead of being treated as [0 1].
  - SOLO and MUTE are :effect-selection/toggle (#532) on the strip's
    instances in the configured mode; SHIFT adds, CONTROL inverts the mode
    (selection-toggle). Scribble colours follow the cue swatch or effect
    category by hue; selected-strip modes light every item strip cyan, as the script does
    in plug-in mode; empty strips are black.
  - Meters show each strip's effective output level: instance opacity times
    group levels (and the fader parameter for cue strips), zero while solo or
    mute suppresses it. It is the mixer gain that reaches the renderer, not a
    photometric measurement."
  (:require [afterglow.control.categories :as categories]
            [afterglow.control.effect-selection :as effect-selection]
            [afterglow.control.params :as params]
            [clojure.string :as str]
            [tolglow.knobs :as knobs]
            [tolglow.surface.link :as link]))

(def unit-schema {:type :number :bounds [0 1] :default 1.0})

(def encoder-step {:coarse 0.01 :fine 0.001})

(def ^:private scalar-types #{:number :integer :boolean :enum})

;; ---------------------------------------------------------------------------
;; Per-refresh context

(defn- groups-by-id [mixer]
  (into {} (map (juxt :id identity))
        (concat (mapcat #(tree-seq :groups :groups %) (:manual mixer)) (:overlay mixer))))

(defn context
  "Lookups shared by every strip of one refresh: the latest instance and all
  instance IDs per cue, group rows by channel ID with member levels, solo and
  mute flags, and knobs in creation order."
  [model]
  (let [instances (vals (:instances model))
        mixer (link/group-projection model)
        selection (effect-selection/projection model)]
    {:model model
     :latest (reduce (fn [m {:keys [cue-id order] :as i}]
                       (if (and cue-id (or (nil? (get m cue-id)) (> (or order 0) (or (:order (get m cue-id)) 0))))
                         (assoc m cue-id i) m))
                     {} instances)
     :cue-instances (reduce (fn [m {:keys [cue-id id]}] (if cue-id (update m cue-id (fnil conj []) id) m)) {} instances)
     :groups (groups-by-id mixer)
     :member-levels (into {} (map (juxt :instance-id :member-level)) (:instances mixer))
     :flags (into {} (map (juxt :id identity)) (:instances selection))
     :modes (:modes selection)
     :knobs (vec (sort-by :order (vals (get-in model [:knobs :knobs]))))}))

;; ---------------------------------------------------------------------------
;; Targets: {:kind :param|:group-level|:opacity|:master|:knob ... :schema :value}

(defn- domain
  "The range a number or integer maps onto: its hard :bounds, else its
  :recommended range. Nil when it has neither: such a number gets no guessed
  range, as modulation skips it (docs/modulation.md), and such an integer
  only takes relative steps."
  [schema]
  (or (:bounds schema) (:recommended schema)))

(defn- component-schema
  "The scalar schema of one component of a colour, vector or range parameter."
  [schema component]
  (cond-> {:type :number :default (get (:default schema) component)
           :bounds (if (= :color (:type schema)) (params/color-bounds schema) (:bounds schema))}
    (and (not= :color (:type schema)) (:recommended schema)) (assoc :recommended (:recommended schema))))

(defn- unranged-number? [scalar] (and (= :number (:type scalar)) (nil? (domain scalar))))

(defn- param-target
  "A typed parameter (or one component of a vector parameter) on an instance;
  nil when the instance lacks it, its type has no scalar mapping, or it is a
  number without bounds or a recommended range. An integer without either
  is a target for relative encoder steps only (normalized is nil)."
  [instance param component]
  (when instance
    (let [schema (get-in instance [:definition :params param])
          whole (get-in instance [:params param])
          component? (and (integer? component) (not= :gradient (:type schema)) (vector? whole)
                          (< -1 component (count whole)))
          scalar (if component? (component-schema schema component) schema)]
      (when (and scalar (or (nil? component) component?) (contains? scalar-types (:type scalar))
                 (not (unranged-number? scalar)))
        (cond-> {:kind :param :instance-id (:id instance) :param param :schema scalar
                 :value (if component? (nth whole component) whole)}
          component? (assoc :component component :whole-value whole))))))

(defn- binding-entry [entry]
  (cond (keyword? entry) {:param entry}
        (map? entry) entry))

(defn unranged-binding
  "The parameter key when a channel binding (a keyword or {:param :component})
  names a number with neither :bounds nor :recommended in a definition's
  params, else nil. Such a binding binds nothing; hosts report it."
  [params binding]
  (when-let [{:keys [param component]} (binding-entry binding)]
    (when-let [schema (get params param)]
      (when (unranged-number? (if (integer? component) (component-schema schema component) schema))
        param))))

(defn- group-target [{:keys [id level]}]
  {:kind :group-level :channel-id id :schema unit-schema :value (double (or level 1.0))})

(defn- opacity-target [instance]
  (when instance
    {:kind :opacity :instance-id (:id instance) :schema unit-schema
     :value (double (get-in instance [:system :opacity] 1.0))}))

(defn master-target [model] {:kind :master :schema unit-schema :value (double (:master model 1.0))})

(defn hydrate
  "Re-read a captured target from the current model, keeping its address
  (instance, parameter, group). Nil when its instance or group is gone. Only
  a group target builds the group projection (fader moves are frequent)."
  [model {:keys [kind instance-id param component channel-id knob-id]}]
  (case kind
    :param (param-target (get-in model [:instances instance-id]) param component)
    :opacity (opacity-target (get-in model [:instances instance-id]))
    :group-level (some-> (get (groups-by-id (link/group-projection model)) channel-id) group-target)
    :master (master-target model)
    :knob (when-let [{:keys [value]} (get-in model [:knobs :knobs knob-id])]
            {:kind :knob :knob-id knob-id :schema unit-schema :value value})
    nil))

(defn normalized
  "The target's value in [0 1], or nil."
  [{:keys [schema value]}]
  (let [[low high] (domain schema)]
    (case (:type schema)
      (:number :integer) (when (and (number? value) (number? low) (number? high))
                           (if (= low high) 0.0 (min 1.0 (max 0.0 (/ (- (double value) low) (- (double high) low))))))
      :boolean (if value 1.0 0.0)
      :enum (let [values (vec (or (:values schema) (:options schema))) index (.indexOf ^java.util.List values value)]
              (when (<= 0 index) (if (< (count values) 2) 0.0 (/ index (double (dec (count values)))))))
      nil)))

(defn value-at
  "The schema value at a normalized position of its domain; nil for a
  number or integer without one."
  [{:keys [schema]} n]
  (let [[low high] (domain schema) n (min 1.0 (max 0.0 (double n)))]
    (case (:type schema)
      :number (when low (+ low (* n (- high low))))
      :integer (when low (long (Math/round (double (+ low (* n (- high low)))))))
      :boolean (>= n 0.5)
      :enum (let [values (vec (or (:values schema) (:options schema)))]
              (nth values (long (Math/round (* n (dec (count values))))))))))

(defn write
  "The owner command template that sets a target to value."
  [target value]
  (case (:kind target)
    :param {:op :param/set :instance-id (:instance-id target) :param (:param target)
            :value (if-let [component (:component target)] (assoc (:whole-value target) component value) value)}
    :group-level {:op :effect-groups/level-set :channel-id (:channel-id target) :value value}
    :opacity {:op :instance/opacity-set :instance-id (:instance-id target) :value value}
    :master {:op :master/set :value value}
    :knob {:op :knob/set :knob-id (:knob-id target) :value value}))

(defn- step [{:keys [schema]}]
  (/ 1.0 (max 1 (dec (count (or (:values schema) (:options schema)))))))

(defn- within-bounds
  "x clamped to the schema's hard bounds; a recommended range is not a limit."
  [schema x]
  (if-let [[low high] (:bounds schema)] (min high (max low x)) x))

(defn turn
  "Command for a relative encoder turn of delta detents, or nil. Knobs use the
  shared knob nudge. Numbers move from their current value by a coarse or
  fine fraction of their domain per detent and integers by one, both within
  the hard bounds only, so a value outside a recommended range does not jump
  into it; booleans and enums step through their values."
  [target delta fine?]
  (when (and target (integer? delta) (not (zero? delta)))
    (let [{:keys [schema value]} target]
      (cond
        (= :knob (:kind target)) (knobs/encoder-command (:knob-id target) delta fine?)
        (= :number (:type schema))
        (when-let [[low high] (when (number? value) (domain schema))]
          (write target (double (within-bounds schema (+ (double value)
                                                         (* delta (if fine? (:fine encoder-step) (:coarse encoder-step))
                                                            (- (double high) (double low))))))))
        (= :integer (:type schema))
        (when (number? value)
          (write target (long (Math/round (double (within-bounds schema (+ value delta)))))))
        :else
        (when-let [n (normalized target)]
          (write target (value-at target (+ n (* (step target) delta)))))))))

(defn reset
  "Command restoring a target's default (unity for levels and opacity), or nil."
  [target]
  (case (:kind target)
    :param (let [default (get-in target [:schema :default])] (when (some? default) (write target default)))
    (:opacity :group-level) (write target 1.0)
    nil))

(defn ring
  "{:mode :position 1..11 :center?} for an encoder ring, or nil for no target.
  bipolar -> :boost-cut, levels -> :wrap, width/spread -> :spread, enums,
  booleans and angles -> :single. The centre LED marks the default."
  [target]
  (when-let [n (some-> target normalized)]
    (let [{:keys [type default] :as schema} (:schema target)
          [low high] (domain schema)
          label (some-> (:param target) name)
          mode (cond
                 (contains? #{:knob :opacity :group-level :master} (:kind target)) :wrap
                 (contains? #{:boolean :enum} type) :single
                 (and (number? low) (number? high) (neg? low) (pos? high)) :boost-cut
                 (and label (re-find #"width|spread|size|fan" label)) :spread
                 (and label (re-find #"hue|phase|angle|rotat|pan|tilt|position" label)) :single
                 :else :wrap)]
      {:mode mode
       :position (if (= :spread mode) (inc (long (Math/round (* 5.0 n)))) (inc (long (Math/round (* 10.0 n)))))
       ;; Numeric equality: a definition's integer default (0) matches its double value (0.0).
       :center? (let [value (:value target)]
                  (boolean (if (and (number? default) (number? value)) (== default value)
                               (and (some? default) (= default value)))))})))

(defn- trim-number [x]
  (let [x (double x) a (Math/abs x)]
    (cond (< a 10) (format "%.2f" x) (< a 100) (format "%.1f" x) :else (format "%.0f" x))))

(defn value-text
  "Short display text for a target's value: percent for unit levels."
  [{:keys [schema value kind] :as target}]
  (cond
    (nil? target) ""
    (nil? value) ""
    (= :boolean (:type schema)) (if value "On" "Off")
    ;; Enum values are any portable data, not only keywords.
    (= :enum (:type schema)) (if (keyword? value) (name value) (str value))
    (= :integer (:type schema)) (str value)
    (and (number? value) (not= :param kind)
         (let [[low high] (domain schema)] (and (number? low) (number? high) (== 0 low) (== 1 high))))
    (str (Math/round (* 100.0 (double value))) "%")
    (number? value) (trim-number value)
    :else (str value)))

;; ---------------------------------------------------------------------------
;; Strips

(defn- hex-rgb [s]
  (when (and (string? s) (re-matches #"#[\da-fA-F]{6}" s))
    (mapv #(/ (Integer/parseInt (subs s % (+ % 2)) 16) 255.0) [1 3 5])))

(defn- rgb [x]
  (cond (and (vector? x) (= 3 (count x)) (every? #(and (number? %) (<= 0 % 1)) x)) (mapv double x)
        (string? x) (hex-rgb x)))

(defn- cue-color
  "Cue swatch or colour, effect swatch, colour parameter, then the effect's
  first category colour; white when nothing declares one."
  [model cue-id]
  (let [cue (get-in model [:registry :cues cue-id])
        effect (get-in model [:registry :effects (:effect-id cue)])]
    (or (rgb (:swatch cue)) (rgb (:color cue)) (rgb (:swatch effect)) (rgb (get-in cue [:params :color]))
        (some #(hex-rgb (get categories/colors %)) (when effect (categories/definition-categories effect)))
        [1.0 1.0 1.0])))

(defn- pan-target
  "The channel's :encoder binding, else the first other scalar parameter."
  [instance channel fader-param]
  (if-let [{:keys [param component]} (binding-entry (:encoder channel))]
    (param-target instance param component)
    (when instance
      (some (fn [[param schema]]
              (when (and (not= param fader-param) (contains? scalar-types (:type schema)))
                (param-target instance param nil)))
            (sort-by (comp name key) (get-in instance [:definition :params]))))))

(defn- flags [ctx ids]
  (let [rows (keep #(get-in ctx [:flags %]) ids)
        state (fn [field] (cond (empty? rows) :none (every? field rows) :all (some field rows) :some :else :none))]
    {:solo (state :soloed?) :mute (state :muted?)
     :suppressed? (boolean (and (seq rows) (every? #(= :solo (:suppressed-by %)) rows)))}))

(defn- contribution [ctx id]
  (let [instance (get-in ctx [:model :instances id])]
    (if (or (nil? instance) (get-in ctx [:flags id :suppressed-by]) (not (#{:running :releasing} (:phase instance :running))))
      0.0
      (* (double (get-in instance [:system :opacity] 1.0)) (double (get-in ctx [:member-levels id] 1.0))))))

(defn row-key
  "The cell key of a strip row: [:group channel-id] or [:cue cue-id]."
  [{:keys [group-id cue-id]}]
  (cond group-id [:group group-id] cue-id [:cue cue-id]))

(defn strip
  "Resolve one strip row (from tolglow.surface.link/strip-channels) for the
  committed layout. Returns nil for an empty strip, else
  {:key :kind :label :color :instance-ids :selector :fader :encoder :meter
   :solo :mute :suppressed? :selected? :unranged}. :unranged names an
  explicit :fader or :encoder binding to a number without a range, which
  binds nothing. flip? swaps fader and encoder on cue
  strips in :pan mode."
  [ctx {:keys [encoder-mode selected flip?]} row]
  (when row
    (let [{:keys [group-id cue-id]} row
          model (:model ctx)]
      (if group-id
        (let [group (get-in ctx [:groups group-id])
              ids (vec (:instance-ids group))
              fader (some-> group group-target)]
          (merge {:key [:group group-id] :kind :group :label (or (:label row) (:name group) (str group-id))
                  :color (or (rgb (:color row)) [0.0 1.0 1.0]) :instance-ids ids
                  :selector (when (seq ids) {:instance-ids ids})
                  :fader fader :encoder nil :selected? (= selected [:group group-id])
                  :meter (reduce max 0.0 (map #(contribution ctx %) ids))}
                 (flags ctx ids)))
        (let [instance (get-in ctx [:latest cue-id])
              ids (get-in ctx [:cue-instances cue-id] [])
              {:keys [param component]} (or (binding-entry (:fader row)) {:param :level})
              fader (param-target instance param component)
              encoder (case encoder-mode
                        :pan (pan-target instance row param)
                        :send (opacity-target instance)
                        nil)
              [fader encoder] (if (and flip? (= :pan encoder-mode)) [encoder fader] [fader encoder])
              ;; A fader is absolute: an integer without a range cannot be one.
              fader (when (and fader (normalized fader)) fader)
              level (if (= :param (:kind fader)) (or (normalized fader) 1.0) 1.0)
              unranged (when instance
                         (some #(unranged-binding (get-in instance [:definition :params]) (get row %)) [:fader :encoder]))]
          (merge {:key [:cue cue-id] :kind :cue :cue-id cue-id
                  :label (or (:label row) (get-in model [:registry :cues cue-id :name]) (name cue-id))
                  :color (or (rgb (:color row)) (cue-color model cue-id)) :instance-ids ids
                  :selector {:cue-ids [cue-id]} :instance instance
                  :fader fader :encoder encoder :selected? (= selected [:cue cue-id])
                  :unranged unranged
                  :meter (* level (reduce max 0.0 (map #(contribution ctx %) ids)))}
                 (flags ctx ids)))))))

(defn- short-name [k]
  (let [s (name k)] (subs s 0 (min 6 (count s)))))

(defn- param-items [instance]
  (vec (mapcat (fn [[param schema]]
                 (let [value (get-in instance [:params param])]
                   (cond
                     (contains? scalar-types (:type schema))
                     (when-let [t (param-target instance param nil)] [{:label (short-name param) :target t}])
                     (and (= :color (:type schema)) (vector? value) (= 3 (count value)))
                     (keep (fn [[component letter]]
                             (when-let [t (param-target instance param component)]
                               {:label (str (subs (name param) 0 (min 4 (count (name param)))) "." letter) :target t}))
                           [[0 "R"] [1 "G"] [2 "B"]]))))
               (sort-by (comp name key) (get-in instance [:definition :params])))))

(defn items
  "Encoder items of a selected-strip mode, or nil in per-strip modes.
  :plug-in lists the selected strip's parameters (a group lists its members'
  opacity); :user lists linked knobs. selected-row is the channel row of the
  selected cell, wherever it is banked."
  [ctx {:keys [encoder-mode]} selected-row]
  (case encoder-mode
    :plug-in (let [s (strip ctx {:encoder-mode :plug-in} selected-row)]
               (case (:kind s)
                 :cue (param-items (:instance s))
                 :group (vec (keep (fn [id]
                                     (when-let [instance (get-in ctx [:model :instances id])]
                                       {:label (let [n (or (get-in ctx [:model :registry :cues (:cue-id instance) :name])
                                                          (some-> (:cue-id instance) name) (str id))]
                                                 (subs n 0 (min 6 (count n))))
                                        :target (opacity-target instance)}))
                                   (:instance-ids s)))
                 []))
    :user (mapv (fn [{:keys [id name value]}]
                  {:label (subs name 0 (min 6 (count name)))
                   :target {:kind :knob :knob-id id :schema unit-schema :value value}})
                (:knobs ctx))
    nil))

(def item-modes #{:plug-in :user})

(defn layout-command
  "The committed layout change for a surface: {:op :control-surface/layout}."
  [surface-id patch]
  {:op :control-surface/layout :target surface-id :viewport :strips :set patch})

(defn select
  "Commands for SELECT on a strip. A first press selects it (and returns to
  the first page). Pressing the selected cue strip again toggles its launch;
  pressing a selected group again clears the selection."
  [surface-id {:keys [selected]} s]
  (cond
    (nil? s) []
    (not= selected (:key s)) [(layout-command surface-id {:selected (:key s) :page 0})]
    (= :group (:kind s)) [(layout-command surface-id {:selected nil :page 0})]
    :else (let [instance (:instance s)]
            [(if instance
               {:op :instance/end :instance-id (:id instance) :force? (= :releasing (:phase instance))}
               {:op :cue/launch :cue-id (:cue-id s) :launch {:mode :persistent}})])))

(defn- toggle-command [control selector additive?]
  {:op :effect-selection/toggle :control control :selector selector :additive? additive?})

(defn- selected-instance-ids [ctx control]
  (let [field (case control :solo :soloed? :mute :muted?)]
    (into [] (comp (filter (comp field val)) (map key)) (:flags ctx))))

(defn selection-toggle
  "Solo or Mute for a strip's instances: {:commands :exclusive-add?}, with
  no commands when the strip has none. modifier picks the mode, as the
  operator's Live script does:
    nil        the configured mode (:effect-selection/configure)
    :additive  keep the other selected strips (SHIFT)
    :invert    the opposite of the configured mode (CONTROL)
  With a configured :additive mode, :invert replaces the selection with this
  strip's instances, or clears them when they are the whole selection. The
  shared toggle has no per-action exclusive mode, so this takes two additive
  toggles in two owner cycles, and the pair is not atomic:
    - the strip already wholly selected among others: one toggle drops the
      others;
    - otherwise the first toggle adds the strip through its own selector
      (:exclusive-add? true). The host sends exclusive-drop for its
      acknowledgement only after it is accepted, so Solo or Mute stays
      active in every committed frame, and a refused or empty add drops
      nothing. A selection another client makes between the two cycles
      survives the drop."
  [ctx control s modifier]
  (let [selector (:selector s) targets (:instance-ids s)]
    (if-not (and selector (seq targets))
      {:commands []}
      (let [configured (get-in ctx [:modes control] :exclusive)
            mode (case modifier
                   :additive :additive
                   :invert (if (= :additive configured) :exclusive :additive)
                   configured)]
        (if (or (= mode configured) (= :additive mode))
          {:commands [(toggle-command control selector (= :additive mode))]}
          (let [current (selected-instance-ids ctx control)
                target-set (set targets)
                current-set (set current)]
            (cond
              (= target-set current-set) {:commands [(toggle-command control selector true)]}
              (every? current-set targets)
              {:commands [(toggle-command control {:instance-ids (filterv (complement target-set) current)} true)]}
              :else {:commands [(toggle-command control selector true)] :exclusive-add? true})))))))

(defn exclusive-drop
  "The second step of an exclusive selection-toggle: for the accepted
  acknowledgement of its additive add, the toggle that drops every other
  selected instance, or nil. Nothing is dropped when the add was refused,
  matched no instance, or left the strip unselected."
  [{:keys [status control instance-ids selected-instance-ids]}]
  (let [selected (set selected-instance-ids) targets (set instance-ids)]
    (when (and (= :accepted status) (seq targets) (every? selected targets))
      (let [others (filterv (complement targets) selected-instance-ids)]
        (when (seq others) (toggle-command control {:instance-ids others} true))))))

(defn soloed-instance-ids [ctx] (selected-instance-ids ctx :solo))

(defn global-solo
  "The transport SOLO button, as in the script: with any solo, store the set
  and clear it; with none and a stored set, restore what still runs; with
  neither, solo the selected strip. Returns {:commands :stored}."
  [ctx stored selected-strip]
  (let [soloed (soloed-instance-ids ctx)
        alive (filterv #(get-in ctx [:model :instances %]) stored)]
    (cond
      (seq soloed) {:commands [{:op :effect-selection/toggle :control :solo :selector {:instance-ids soloed} :additive? true}]
                    :stored soloed}
      (seq alive) {:commands [{:op :effect-selection/toggle :control :solo :selector {:instance-ids alive} :additive? true}]
                   :stored []}
      :else {:commands (:commands (selection-toggle ctx :solo selected-strip nil)) :stored []})))

;; ---------------------------------------------------------------------------
;; Colours (the script's hue matching with a white cut-off)

(def palette
  "Native scribble colours, in MCU index order."
  [:black :red :green :yellow :blue :magenta :cyan :white])

(def ^:private hue-sectors {0 1 1 3 2 2 3 6 4 4 5 5})

(defn palette-index
  "Nearest of the eight scribble colours. Only exact black is black, greys
  are white. :hue (default) keys on hue and turns colours below the
  saturation cut-off white, so pale and dark tints keep their primary;
  :rgb is nearest in RGB."
  ([rgb] (palette-index rgb :hue 0.19))
  ([[r g b :as rgb] mode cutoff]
   (let [high (max r g b) low (min r g b) chroma (- high low)]
     (cond
       (every? zero? rgb) 0
       (= :off mode) 7
       (= :rgb mode) (first (sort-by (fn [i] (reduce + (map #(let [d (- %1 %2)] (* d d)) rgb
                                                             (nth [[0 0 0] [1 0 0] [0 1 0] [1 1 0] [0 0 1] [1 0 1] [0 1 1] [1 1 1]] i))))
                                     (range 1 8)))
       (< (/ chroma high) cutoff) 7
       :else (let [hue (cond (= high r) (mod (/ (- g b) chroma) 6)
                             (= high g) (+ 2 (/ (- b r) chroma))
                             :else (+ 4 (/ (- r g) chroma)))]
               (hue-sectors (mod (long (Math/round (double hue))) 6)))))))

(defn label-text
  "Fit text to a 7-column scribble slot: six characters and a gap, ASCII."
  [s]
  (let [s (str/replace (str (or s "")) #"[^\x20-\x7e]" "?")
        s (subs s 0 (min 6 (count s)))]
    (str s (apply str (repeat (- 7 (count s)) " ")))))
