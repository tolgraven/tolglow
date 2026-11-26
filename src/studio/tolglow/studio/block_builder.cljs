(ns tolglow.studio.block-builder
  (:require [cljs.reader :as edn]
            [cljs.tools.reader :as reader]
            [cljs.tools.reader.reader-types :as readers]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.block-trace :as trace]
            [tolglow.studio.modulation :as motion]
            [tolglow.studio.parameter-widgets :as widget]
            [tolglow.studio.signal-picker :as signal]))

(defn binding-paths [definition]
  (let [outputs (:outputs definition)
        outputs (if (map? outputs) (keys outputs) (range (count outputs)))]
    (->> (concat (keys (:blocks definition))
                 (map #(vector :params %) (keys (:params definition)))
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

(defn- input-control [id default inputs set-input! clear! params & [transport-only?]]
  (let [label (if (= id :amount) "Wet amount" (widget/label id))
        value (get inputs id default)
        expr? (map? value)
        param? (= :param (:op value))
        mode (cond param? :parameter expr? (:op value) :else :constant)
        phase? (or (= mode :oscillator-phase) (and (= "phase" (name id)) (= :number (signal/value-type default))))
        modes (vec (distinct (concat [:constant :beat :bar :phrase :time] (when phase? [:oscillator-phase])
                                    (when (seq params) [:parameter]) [mode])))
        choose (fn [next]
                 (set-input! (case next
                               :constant (if (map? default) (signal/fixed-value (signal/value-type default)) default)
                               :parameter {:op :param :key (first params)}
                               :oscillator-phase (oscillator-timing value transport-only?)
                               {:op next})))]
    [:div.block-input
     [options label modes mode #(if (= % :oscillator-phase) "Musical period" (widget/label %)) choose]
     (cond param? [options (str label " parameter") params (:key value) widget/label #(set-input! {:op :param :key %})]
           (and phase? (or (= mode :oscillator-phase) (and (period-units mode) (not (contains? value :clock)))))
           [oscillator-fields label value transport-only? set-input!]
           (or (number? value) (and (vector? value) (every? number? value))) [numeric-field label value set-input!]
           (and expr? (not (#{:variable :beat :bar :phrase :time} mode))) [:code (pr-str value)])
     [signal/picker label value (signal/value-type default) set-input!]
     (when (contains? inputs id) [:button.text-button {:type "button" :on-click clear!} "Reset"])]))

(defn- stage-controls
  "One stage's controls. `change!` takes a function of the stage's current value."
  [i stage template params change! move! remove! total transport-only?]
  (let [coordinates (or (:coordinates stage) :world)
        title (or (:name template) (trace/path-label [(:block stage)]))
        inputs (or (:inputs stage) {})]
    [:article.block-stage
     [:header [:strong (str (inc i) " · " title)]
      [:span.block-stage-order
       [:button.text-button {:type "button" :aria-label (str "Move " title " earlier") :disabled (zero? i)
                             :on-click #(move! (dec i))} "↑"]
       [:button.text-button {:type "button" :aria-label (str "Move " title " later") :disabled (= i (dec total))
                             :on-click #(move! (inc i))} "↓"]
       [:button.text-button {:type "button" :aria-label (str "Remove " title) :on-click remove!} "Remove"]]]
     [:div.block-stage-blend
      [options "Blend" [:replace :add :multiply] (or (:mode stage) (:mode template)) widget/label #(change! assoc :mode %)]
      [options "Coordinates" (distinct [:world :surface-uv coordinates]) coordinates
       #(case % :world "World XYZ" :surface-uv "Surface UV" "Custom") #(change! assoc :coordinates %)]]
     [input-control :amount 1.0 (select-keys stage [:amount])
      #(change! assoc :amount %) #(change! dissoc :amount) params]
     [:details [:summary "Inputs"]
      (for [[id default] (sort-by (comp str key) (:inputs template))]
        ^{:key id} [input-control id default inputs #(change! assoc-in [:inputs id] %)
                    #(change! update :inputs dissoc id) params transport-only?])]]))

(defn update-stage
  "Apply `f` to stage `i` of the current stages; out-of-range indices change nothing."
  [stages i f & args]
  (if (< -1 i (count stages)) (apply update stages i f args) stages))

(defn remove-stage [stages i]
  (if (< -1 i (count stages)) (into (subvec stages 0 i) (subvec stages (inc i))) stages))

(defn- stage-list
  "Every edit reaches `save!` as a function of the current stages, not a copy of the
  rendered ones: a field committed on blur and the tap that caused the blur run before
  the next render, and the second must not discard the first."
  [catalog stages params save! transport-only?]
  [:div.block-stage-list
   (for [[i stage] (map-indexed vector stages)]
     ^{:key [i (:block stage)]}
     [stage-controls i stage (get catalog (:block stage)) params
      (fn [f & args] (save! #(apply update-stage % i f args)))
      (fn [to] (save! #(move-stage % i to)))
      #(save! (fn [current] (remove-stage current i))) (count stages) transport-only?])])

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
  `on-change` receives a function from the current stages to the edited ones."
  [{:keys [stages library parameters on-change transport-only?]}]
  (r/with-let [chosen (r/atom nil)]
    (let [ids (sort-by #(get-in library [% :name] (str %)) (keys library))
          id (or (some #{@chosen} ids) (first ids))]
      [:div.block-stage-editor
       [stage-list library (vec stages) parameters on-change transport-only?]
       (if (seq ids)
         [:div.block-stage-add
          [options "New block" ids id #(get-in library [% :name] (str %)) #(reset! chosen %)]
          [:button.button.secondary {:type "button" :disabled (>= (count stages) 32)
                              :on-click #(on-change (fn [current] (conj (vec current) {:block id})))} "Add stage"]]
         [:p.muted {:role "status"} "Waiting for the block library…"])])))

(defn draft-catalog [library {:keys [prefabs imported-prefabs]}]
  (let [with-import (fn [entries id material]
                      (if (= :show (get-in library [id :source]))
                        entries
                        (assoc entries id material)))]
    (merge (reduce-kv with-import library imported-prefabs) prefabs)))

(defn- editor
  "`write!` receives a function from the current binding map to the edited one."
  [definition bindings write!]
  (r/with-let [chosen-path (r/atom nil)]
    (let [shared @(rf/subscribe [:studio/remote [:runtime/block-library]])
          catalog (draft-catalog shared definition)
          paths (binding-paths (assoc definition :blocks bindings))
          path (or (some #{@chosen-path} paths) (first paths))
          params (vec (keys (:params definition)))]
      [:div.block-builder
       [private-binding bindings (fn [destination initial]
                                   (write! #(assoc % destination initial))
                                   (reset! chosen-path destination))]
       [options "Block binding" paths path trace/path-label #(reset! chosen-path %)]
       (when (= :varyings (first path))
         [input-control :base 0.0 (get bindings path)
          (fn [value] (write! #(assoc-in % [path :base] value))) #(write! (fn [b] (assoc-in b [path :base] 0.0))) params])
       (when (contains? bindings path)
         [:button.text-button {:type "button" :on-click #(write! (fn [b] (dissoc b path)))} "Remove binding"])
       (when path
         [stage-editor {:stages (get-in bindings [path :stages]) :library catalog :parameters params
                        :on-change (fn [f] (write! #(replace-stages % path (f (vec (get-in % [path :stages]))))))}])])))

(defn field
  "`change!` receives a function from the current binding map to the edited one."
  [definition bindings change!]
  (r/with-let [open? (r/atom false)]
    [:section.block-editor
     [:button.text-button {:type "button" :aria-expanded @open? :on-click #(swap! open? not)} "Assign blocks"]
     (when @open? [editor definition bindings change!])]))

(defn write-bindings
  "Splice the :blocks map using reader coordinates. Preserve other forms,
  comments, indentation and surrounding whitespace in the source editor."
  [source bindings]
  (let [definition (reader/read (readers/indexing-push-back-reader source))
        old (:blocks definition)
        lines (str/split source #"\n" -1)
        offset (fn [line col] (+ (dec col) (reduce + (map #(inc (count %)) (take (dec line) lines)))))
        {:keys [line column end-line end-column]} (meta (or old definition))
        end (offset end-line end-column)
        result (cond
                 (= (or old {}) bindings) source
                 old (str (subs source 0 (offset line column)) (pr-str bindings) (subs source end))
                 :else (str (subs source 0 (dec end)) "\n :blocks " (pr-str bindings) (subs source (dec end))))]
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
