(ns tolglow.studio.block-builder
  (:require [cljs.reader :as edn]
            [cljs.tools.reader :as reader]
            [cljs.tools.reader.reader-types :as readers]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.authoring.parameter-controls :as controls]
            [tolglow.studio.block-trace :as trace]
            [tolglow.studio.modulation :as motion]
            [tolglow.studio.pretty :as pretty]
            [tolglow.studio.parameter-widgets :as widget]
            [tolglow.studio.signal-picker :as signal]))

(defn binding-paths
  "Paths a block chain can modulate: existing bindings, the parameters a
  numeric expression can read (not a gradient, enum or boolean) and outputs."
  [definition]
  (let [outputs (:outputs definition)
        outputs (if (map? outputs) (keys outputs) (range (count outputs)))]
    (->> (concat (keys (:blocks definition))
                 (keep (fn [[id schema]] (when (controls/expression-type schema) [:params id])) (:params definition))
                 (map #(vector :outputs %) outputs))
         distinct (sort-by trace/path-label) vec)))

(defn replace-stages [bindings path stages]
  (let [entry (assoc (get bindings path {}) :stages stages)]
    (if (and (empty? stages) (= 1 (count entry)))
      (dissoc bindings path)
      (assoc bindings path entry))))

(defn move-stage [stages from to]
  (if (and (<= 0 from (dec (count stages))) (<= 0 to (dec (count stages))))
    (assoc stages from (get stages to) to (get stages from))
    stages))

(defn- options [label choices value title choose]
  (let [items (vec choices)
        index (first (keep-indexed #(when (= value %2) %1) items))]
    [:label label
     [:select {:aria-label label :value (str index)
               :on-change #(choose (get items (parse-long (.. % -target -value))))}
      (for [[i v] (map-indexed vector items)]
        ^{:key i} [:option {:value i} (title v)])]]))

(defn- scalar [label value update!]
  [:input {:type "number" :step "any" :aria-label label :default-value value
           :on-blur (fn [e]
                      (let [raw (.. e -target -value)
                            n (parse-double raw)]
                        (if (widget/finite? n) (update! n) (set! (.. e -target -value) (str value)))))}])

(defn- numeric-field [label value update!]
  (if (vector? value)
    [:span.block-input-vector
     (for [[i n] (map-indexed vector value)]
       ^{:key [i n]} [scalar (str label " " (inc i)) n #(update! (assoc value i %))])]
    ^{:key (str value)} [scalar label value update!]))

(def ^:private period-units {:beat :beats :bar :bars :phrase :phrases})

(defn oscillator-timing [value transport-only?]
  (let [default {:op :oscillator-phase :period {:beats 1} :phase 0.0
                 :phase-reference (if transport-only? :transport :launch)}]
    (if (= :oscillator-phase (:op value)) (merge default value)
        (if-let [unit (period-units (:op value))] (assoc default :period {unit 1}) default))))

(defn- oscillator-fields [label value transport-only? change!]
  (let [{:keys [phase phase-reference period] :as spec} (oscillator-timing value transport-only?)
        [unit amount] (first period)]
    [:div.form-row
     (when (and transport-only? (= phase-reference :launch))
       [:button.button.secondary {:type "button" :on-click #(change! (assoc spec :phase-reference :transport))}
        "Use show transport"])
     (when-not transport-only?
       [options "Phase reference" [:launch :transport] phase-reference
        #(case % :launch "Cue launch" :transport "Show transport")
        #(change! (assoc spec :phase-reference %))])
     [:label "Period"
      ^{:key (pr-str amount)}
      [:input {:type "text" :aria-label (str label " period") :default-value (str amount)
               :on-blur (fn [e]
                          (let [input (.-target e)]
                            (try
                              (let [n (motion/number-value (.-value input) "Period")]
                                (when-not (pos? n) (throw (js/Error. "Period must be positive.")))
                                (change! (assoc spec :period {unit n})))
                              (catch :default _ (set! (.-value input) (str amount))))))}]]
     [options "Period unit" [:beats :bars :phrases] unit widget/label #(change! (assoc spec :period {% amount}))]
     [:label "Phase (cycles)" ^{:key (pr-str phase)}
      [scalar "Phase (cycles)" phase #(change! (assoc spec :phase %))]]]))

(def extend-modes
  "How a gradient sample treats positions outside [0,1] (`tolglow.compiler.gradient/extends`)."
  [:clamp :repeat :mirror])

(defn input-type
  "The numeric type of an input's value: a gradient sample is a colour."
  [value]
  (if (= :gradient (:op value)) :vec3 (signal/value-type value)))

(defn parameter-choices
  "IDs of the parameters an input of `type` can read with {:op :param}: the
  schema's IR type must match, so a gradient is never offered."
  [schemas type]
  (->> schemas
       (filter #(= type (controls/expression-type (val %))))
       (map key) (sort-by str) vec))

(defn gradient-choices
  "IDs of the gradient parameters a colour input can sample."
  [schemas]
  (->> schemas
       (filter #(= :gradient (controls/control-type (val %) nil)))
       (map key) (sort-by str) vec))

(defn gradient-sample
  "A gradient sample of `key` at `position`. The default :clamp extension is
  left out, as `fx/gradient` leaves it out."
  [key position extend]
  (cond-> {:op :gradient :key key :args [position]}
    (and extend (not= :clamp extend)) (assoc :extend extend)))

(defn with-extend [sample extend]
  (if (= :clamp extend) (dissoc sample :extend) (assoc sample :extend extend)))

(defn input-modes
  "The modes an input offers, in menu order. `value` is the input's current
  value; its mode is always listed so loaded source stays visible."
  [{:keys [type value schemas varyings phase? position?]}]
  (let [mode (cond (= :param (:op value)) :parameter (map? value) (:op value) :else :constant)]
    (vec (distinct (concat [:constant :beat :bar :phrase :time]
                           (when position? [:position])
                           (when phase? [:oscillator-phase])
                           (when (seq (parameter-choices schemas type)) [:parameter])
                           (when (and (= :vec3 type) (seq (gradient-choices schemas))) [:gradient])
                           (when (and (= :number type) (seq varyings)) [:varying])
                           [mode])))))

(defn mode-value
  "The value an input takes when its mode changes to `mode`."
  [mode {:keys [type default value schemas varyings transport-only?]}]
  (case mode
    :constant (if (map? default) (signal/fixed-value (signal/value-type default)) default)
    :parameter {:op :param :key (first (parameter-choices schemas type))}
    :gradient (gradient-sample (first (gradient-choices schemas)) 0.0 nil)
    :position {:op :position :axis :x}
    :varying {:op :varying :key (first varyings)}
    :oscillator-phase (oscillator-timing value transport-only?)
    {:op mode}))

(def ^:private mode-titles
  {:oscillator-phase "Musical period" :position "Fixture position" :gradient "Gradient sample"
   :varying "Private varying"})

(defn- with-current
  "Choices plus the current value when it is not among them, so loaded source
  stays visible (publication then names the problem)."
  [choices current]
  (cond-> (vec choices) (and (some? current) (not (some #{current} choices))) (conj current)))

(defn parameter-title
  "Names a parameter choice. One that loaded source chose but `fits?` refuses
  says why: missing, or the schema type it has."
  [schemas fits?]
  (fn [id]
    (let [schema (get schemas id)
          title (controls/title id schema)]
      (cond (nil? schema) (str title " (missing)")
            (fits? schema) title
            :else (str title " (" (widget/label (:type schema)) ")")))))

(defn edit-op
  "Apply `f` to `value` only while it is still an `op` expression. Edits can
  arrive before the next render (see the stage identity note below), so a
  field edits the current value, and one whose mode has changed meanwhile is
  left alone."
  [op f value]
  (if (= op (:op value)) (f value) value))

(defn edit-position
  "Apply `f` to a gradient sample's position, 0.0 when it has none."
  [f sample]
  (edit-op :gradient #(update % :args (fn [[position]] [(f (if (some? position) position 0.0))])) sample))

(declare input-control)

(defn- gradient-fields
  "Pick the gradient, its extension and a scalar position, which is itself an
  input with its own modes. `edit!` takes a function of the current sample."
  [label value schemas edit! context]
  (let [gradients (gradient-choices schemas)
        key (:key value)
        position (first (:args value))]
    [:div.block-gradient-sample
     [options (str label " gradient") (with-current gradients key) key
      (parameter-title schemas #(= :gradient (controls/control-type % nil)))
      (fn [k] (edit! (partial edit-op :gradient #(assoc % :key k))))]
     (when-let [stops (:default (get schemas key))]
       [:div.param-gradient [widget/color-view stops]])
     [options (str label " extend") extend-modes (:extend value :clamp) widget/label
      (fn [extend] (edit! (partial edit-op :gradient #(with-extend % extend))))]
     [input-control :position 0.0 (if (some? position) {:position position} {})
      (fn [f] (edit! (partial edit-position f))) #(edit! (partial edit-position (constantly 0.0)))
      (assoc context :label (str label " position") :position? true)]]))

(defn- input-control
  "One input's mode and value. `edit!` takes a function of the input's stored
  value (nil while it uses the default) and `clear!` restores the default.
  `context` holds the definition's parameter :schemas, the scalar private
  :varyings it may read, :transport-only? and optional :label and :position?
  (offer the fixture position and an oscillator)."
  [id default inputs edit! clear! {:keys [schemas varyings transport-only? position?] :as context}]
  (let [label (or (:label context) (if (= id :amount) "Wet amount" (widget/label id)))
        value (get inputs id default)
        current! (fn [f] (edit! #(f (if (some? %) % default))))
        set-input! #(current! (constantly %))
        type (input-type default)
        expr? (map? value)
        mode (cond (= :param (:op value)) :parameter expr? (:op value) :else :constant)
        phase? (or position? (= mode :oscillator-phase) (and (= "phase" (name id)) (= :number type)))
        spec {:type type :default default :value value :schemas schemas :varyings varyings
              :phase? phase? :position? position? :transport-only? transport-only?}
        modes (input-modes spec)]
    [:div.block-input
     [options label modes mode #(or (mode-titles %) (widget/label %)) #(set-input! (mode-value % spec))]
     (case mode
       :parameter [options (str label " parameter") (with-current (parameter-choices schemas type) (:key value))
                   (:key value) (parameter-title schemas #(= type (controls/expression-type %)))
                   #(set-input! {:op :param :key %})]
       :gradient nil
       :position [options (str label " axis") [:x :y :z] (:axis value) #(str/upper-case (name %))
                  (fn [axis] (current! (partial edit-op :position #(assoc % :axis axis))))]
       :varying [options (str label " varying") (with-current varyings (:key value)) (:key value) widget/label
                 (fn [k] (current! (partial edit-op :varying #(assoc % :key k))))]
       (cond (and phase? (or (= mode :oscillator-phase) (and (period-units mode) (not (contains? value :clock)))))
             [oscillator-fields label value transport-only? set-input!]
             (or (number? value) (and (vector? value) (every? number? value))) [numeric-field label value set-input!]
             (and expr? (not (#{:variable :beat :bar :phrase :time} mode))) [:code (pr-str value)]))
     [signal/picker label value type set-input!]
     (when (contains? inputs id) [:button.text-button {:type "button" :on-click clear!} "Reset"])
     (when (= mode :gradient) [gradient-fields label value schemas current! (dissoc context :label :position?)])]))

(defn- stage-controls
  "One stage's controls. `change!` takes a function of the stage's current value and
  `move!` a step (-1 earlier, 1 later); both find the stage wherever it is now."
  [i stage template context change! move! remove! total]
  (let [coordinates (or (:coordinates stage) :world)
        title (or (:name template) (trace/path-label [(:block stage)]))
        inputs (or (:inputs stage) {})]
    [:article.block-stage
     [:header [:strong (str (inc i) " · " title)]
      [:span.block-stage-order
       [:button.text-button {:type "button" :aria-label (str "Move " title " earlier") :disabled (zero? i)
                             :on-click #(move! -1)} "↑"]
       [:button.text-button {:type "button" :aria-label (str "Move " title " later") :disabled (= i (dec total))
                             :on-click #(move! 1)} "↓"]
       [:button.text-button {:type "button" :aria-label (str "Remove " title) :on-click remove!} "Remove"]]]
     [:div.block-stage-blend
      [options "Blend" [:replace :add :multiply] (or (:mode stage) (:mode template)) widget/label #(change! assoc :mode %)]
      [options "Coordinates" (distinct [:world :surface-uv coordinates]) coordinates
       #(case % :world "World XYZ" :surface-uv "Surface UV" "Custom") #(change! assoc :coordinates %)]]
     [input-control :amount 1.0 (select-keys stage [:amount])
      #(change! update :amount %) #(change! dissoc :amount) context]
     [:details [:summary "Inputs"]
      (for [[id default] (sort-by (comp str key) (:inputs template))]
        ^{:key id} [input-control id default inputs #(change! update-in [:inputs id] %)
                    #(change! update :inputs dissoc id) context])]]))

(defn update-stage
  "Apply `f` to stage `i` of the current stages; out-of-range indices change nothing."
  [stages i f & args]
  (if (< -1 i (count stages)) (apply update stages i f args) stages))

(defn remove-stage [stages i]
  (if (< -1 i (count stages)) (into (subvec stages 0 i) (subvec stages (inc i))) stages))

;; Stage identity. Edits can reach the stages before the next render (a field
;; committed on blur, then the tap that blurred it, or two quick taps), so a stage
;; is addressed by a stable ID, never by the index it was rendered at. The ID is
;; metadata: it never reaches the EDN (pr-str and pretty/edn omit metadata) and
;; never affects equality. A stage without one, as loaded from source, is known by
;; its position, and the first edit to its list records that position as its ID.

(defn stage-id
  "The ID of stage `stage` rendered at index `i`."
  [i stage]
  (or (::id (meta stage)) [::position i]))

(defn- identified [stages]
  (into [] (map-indexed (fn [i stage]
                          (if (or (::id (meta stage)) (not (satisfies? IWithMeta stage)))
                            stage
                            (vary-meta stage assoc ::id [::position i]))))
        stages))

(defn new-stage
  "A stage to append, with an ID of its own."
  [block]
  (with-meta {:block block} {::id (random-uuid)}))

(defn edit-stage
  "Apply `(f stages index)` to the current stages, where index is where the stage
  with `id` is now. A stage that is gone changes nothing."
  [stages id f]
  (let [stages (identified stages)]
    (if-let [i (first (keep-indexed (fn [i stage] (when (= id (stage-id i stage)) i)) stages))]
      (f stages i)
      stages)))

(defn- stage-list
  "Every edit reaches `save!` as a function of the current stages, not a copy of the
  rendered ones, and finds its stage by ID: edits made before the next render must
  compose, and none may land on whichever stage now sits at a rendered index."
  [catalog stages context save!]
  [:div.block-stage-list
   (for [[i stage] (map-indexed vector stages)
         :let [id (stage-id i stage)]]
     ^{:key (pr-str id)}
     [stage-controls i stage (get catalog (:block stage)) context
      (fn [f & args] (save! #(edit-stage % id (fn [stages i] (apply update-stage stages i f args)))))
      (fn [delta] (save! #(edit-stage % id (fn [stages i] (move-stage stages i (+ i delta))))))
      #(save! (fn [current] (edit-stage current id remove-stage))) (count stages)])])

(defn private-path [kind text]
  (let [parts (str/split (str/trim text) #"\s+")
        valid? #(re-matches #"[A-Za-z][A-Za-z0-9._/-]*" %)]
    (when (and (every? valid? parts)
               (or (= kind :variables) (and (= kind :varyings) (= 1 (count parts)))))
      (into [kind] (map keyword) parts))))

(defn- private-binding [bindings create!]
  (r/with-let [kind (r/atom :varyings) path (r/atom "") type (r/atom :number)]
    (let [destination (private-path @kind @path)
          initial (if (= @kind :variables)
                    {:type @type :stages []}
                    {:base (if (= :vec3 @type) [0.0 0.0 0.0] 0.0) :stages []})]
      [:details.block-private-binding
       [:summary "Add private or variable binding"]
       [options "Binding scope" [:varyings :variables] @kind widget/label #(reset! kind %)]
       [:label "Path" [:input {:aria-label "Binding path" :value @path
                               :placeholder (if (= :variables @kind) "weather density" "ripple")
                               :on-change #(reset! path (.. % -target -value))}]]
       [options "Value type" [:number :vec3] @type #(if (= :vec3 %) "Vector" "Number") #(reset! type %)]
       [:button.button.secondary {:type "button" :disabled (or (nil? destination) (contains? bindings destination))
                           :on-click #(do (create! destination initial) (reset! path ""))}
        "Create binding"]])))

(defn stage-editor
  "A controlled stage list. The caller supplies its catalog and commits edits:
  `on-change` receives a function from the current stages to the edited ones.
  `parameters` maps the definition's parameter IDs to their schemas and
  `varyings` names the scalar private varyings an input may read."
  [{:keys [stages library parameters varyings on-change transport-only?]}]
  (r/with-let [chosen (r/atom nil)]
    (let [ids (sort-by #(get-in library [% :name] (str %)) (keys library))
          id (or (some #{@chosen} ids) (first ids))]
      [:div.block-stage-editor
       [stage-list library (vec stages)
        {:schemas (into {} parameters) :varyings (vec varyings) :transport-only? transport-only?} on-change]
       (if (seq ids)
         [:div.block-stage-add
          [options "New block" ids id #(get-in library [% :name] (str %)) #(reset! chosen %)]
          [:button.button.secondary {:type "button" :disabled (>= (count stages) 32)
                              :on-click #(let [stage (new-stage id)]
                                           (on-change (fn [current] (conj (identified current) stage))))} "Add stage"]]
         [:p.muted {:role "status"} "Waiting for the block library…"])])))

(defn draft-catalog [library {:keys [prefabs imported-prefabs]}]
  (let [with-import (fn [entries id material]
                      (if (= :show (get-in library [id :source]))
                        entries
                        (assoc entries id material)))]
    (merge (reduce-kv with-import library imported-prefabs) prefabs)))

(defn scalar-varyings
  "Names of the private varyings with a scalar base that the binding at `path`
  may read. A binding never reads itself."
  [bindings path]
  (->> (keys bindings)
       (filter #(and (= :varyings (first %)) (not= path %)
                     (= :number (input-type (get-in bindings [% :base] 0.0)))))
       (map second) (sort-by str) vec))

(defn- editor
  "`write!` receives a function from the current binding map to the edited one."
  [definition bindings write!]
  (r/with-let [chosen-path (r/atom nil)]
    (let [shared @(rf/subscribe [:studio/remote [:runtime/block-library]])
          catalog (draft-catalog shared definition)
          paths (binding-paths (assoc definition :blocks bindings))
          path (or (some #{@chosen-path} paths) (first paths))
          schemas (or (:params definition) {})
          base-default (if (= :vec3 (input-type (get-in bindings [path :base]))) [0.0 0.0 0.0] 0.0)]
      [:div.block-builder
       [private-binding bindings (fn [destination initial]
                                   (write! #(assoc % destination initial))
                                   (reset! chosen-path destination))]
       [options "Block binding" paths path trace/path-label #(reset! chosen-path %)]
       (when (= :varyings (first path))
         [input-control :base base-default (get bindings path)
          (fn [f] (write! #(update-in % [path :base] f)))
          #(write! (fn [b] (assoc-in b [path :base] base-default)))
          {:schemas schemas :varyings (scalar-varyings bindings path)}])
       (when (contains? bindings path)
         [:button.text-button {:type "button" :on-click #(write! (fn [b] (dissoc b path)))} "Remove binding"])
       (when path
         [stage-editor {:stages (get-in bindings [path :stages]) :library catalog :parameters schemas
                        :varyings (scalar-varyings bindings path)
                        :on-change (fn [f] (write! #(replace-stages % path (f (vec (get-in % [path :stages]))))))}])])))

(defn field
  "`change!` receives a function from the current binding map to the edited one."
  [definition bindings change!]
  (r/with-let [open? (r/atom false)]
    [:section.block-editor
     [:button.text-button {:type "button" :aria-expanded @open? :on-click #(swap! open? not)} "Assign blocks"]
     (when @open? [editor definition bindings change!])]))

(defn- keyed-block-map
  "`:blocks` and the map printed as pretty/edn lays out a map entry whose key
  starts at `key-column`: after the key when it fits there, otherwise on the
  next line indented two columns past the key."
  [bindings key-column tail]
  (let [indent #(apply str (repeat % " "))
        hung (pretty/edn bindings {:column (+ key-column 8) :tail tail})]
    (str ":blocks"
         (if (str/includes? hung "\n")
           (str "\n" (indent (+ key-column 2)) (pretty/edn bindings {:column (+ key-column 2) :tail tail}))
           (str " " hung)))))

(defn write-bindings
  "Splice the :blocks map using reader coordinates. Preserve other forms,
  comments, indentation and surrounding whitespace in the source editor. In
  pretty-printed source the entry gets the lines a fresh print would give it,
  moving the map after its key or onto the next line as its new size needs. A
  block map that sat on one line among other entries stays on one line; one
  laid out some other way is pretty-printed from the column it starts at."
  [source bindings]
  (let [definition (reader/read (readers/indexing-push-back-reader source))
        old (:blocks definition)
        lines (str/split source #"\n" -1)
        offset (fn [line col] (+ (dec col) (reduce + (map #(inc (count %)) (take (dec line) lines)))))
        {:keys [line column end-line end-column]} (meta (or old definition))
        end (offset end-line end-column)
        ;; The definition's entries start one column after its opening brace.
        entry-column (:column (meta definition))
        result (cond
                 (= (or old {}) bindings) source
                 old (let [start (offset line column)
                           tail (count (re-find #"^[\]})]*" (subs source end)))
                           prefix (subs source (- start (dec column)) start)
                           above (when (> line 1) (nth lines (- line 2)))
                           ;; [key column, index where the key starts] for the two layouts pretty/edn prints.
                           [key-column key-start]
                           (if-let [[_ spaces] (re-matches #"( *):blocks +" prefix)]
                             [(count spaces) (+ (- start (dec column)) (count spaces))]
                             (when-let [[_ spaces] (and (re-matches #" *" prefix) above (re-matches #"( *):blocks *" above))]
                               [(count spaces) (+ (- start (dec column) 1 (count above)) (count spaces))]))]
                       (cond
                         key-column (str (subs source 0 key-start) (keyed-block-map bindings key-column tail) (subs source end))
                         (str/includes? (subs source start end) "\n")
                         (str (subs source 0 start) (pretty/edn bindings {:column (dec column) :tail tail}) (subs source end))
                         :else (str (subs source 0 start) (pretty/edn bindings {:width ##Inf}) (subs source end))))
                 :else (str (subs source 0 (dec end)) "\n" (apply str (repeat entry-column " "))
                            (keyed-block-map bindings entry-column 1)
                            (subs source (dec end))))]
    (when-not (= (assoc (edn/read-string source) :blocks bindings)
                 (update (edn/read-string result) :blocks #(or % {})))
      (throw (js/Error. "Could not locate the block map in this source.")))
    result))

(defn source-field [source replace!]
  (r/with-let [draft (r/atom nil) problem (r/atom nil)]
    (let [load! (fn []
                  (try
                    (let [definition (edn/read-string source)]
                      (when-not (map? definition) (throw (js/Error. "Choose a declarative effect definition.")))
                      (reset! draft {:source source :definition definition :bindings (or (:blocks definition) {})})
                      (reset! problem nil))
                    (catch :default error (reset! problem (.-message error)))))]
      [:section.block-editor
       [:button.text-button {:type "button" :aria-expanded (boolean @draft)
                             :on-click #(if @draft (reset! draft nil) (load!))} "Assign blocks"]
       (when @problem [:p.error {:role "alert"} @problem])
       (when-let [{:keys [definition bindings] baseline :source} @draft]
         (if (not= source baseline)
           [:button.button.secondary {:type "button" :on-click load!} "Reload changed definition"]
           [:<>
            [editor definition bindings #(swap! draft update :bindings %)]
            [:button.button.secondary {:type "button"
                                ;; Read the draft now: a field this tap blurred has just committed to it.
                                :on-click #(try
                                             (replace! (write-bindings source (:bindings @draft)))
                                             (reset! draft nil)
                                             (catch :default error (reset! problem (.-message error))))}
             "Write stages to EDN"]]))])))
