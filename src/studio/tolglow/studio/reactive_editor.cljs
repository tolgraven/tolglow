(ns tolglow.studio.reactive-editor
  "Edit portable reactive bindings without reading or owning their signals."
  (:require [cljs.tools.reader.edn :as edn]
            [cljs.tools.reader.reader-types :as readers]
            [clojure.string :as str]
            [tolglow.studio.checkbox :as checkbox]
            [tolglow.studio.fixture-picker :as fixtures]
            [tolglow.studio.keyframe-editor :as keyframes]))

(defn- read-one [source]
  (try
    (let [input (readers/string-push-back-reader source)
          eof (gensym)
          read! #(edn/read {:eof eof} input)
          value (read!)]
      (when (identical? eof (read!)) value))
    (catch :default _ nil)))

(defn- keyword-input [value]
  (when-not (str/blank? value)
    (let [text (str/trim value)
          parsed (read-one (if (str/starts-with? text ":") text (str ":" text)))]
      (when (keyword? parsed) parsed))))

(defn- binding-row [alias kind address]
  {:id (str (random-uuid)) :alias alias :kind kind :address address})

(defn- target-row [fixture-id]
  {:id (str (random-uuid)) :fixture-id fixture-id})

(defn new-draft []
  (let [row (binding-row "level" :source "")]
    {:enabled? true :bindings [row] :dimmer (:id row) :targets [(target-row nil)] :transition ""}))

(defn loaded-draft [definition]
  (let [{:keys [inputs transition]} (:reactive definition)]
    {:enabled? true :base definition
     :bindings (mapv (fn [[alias [kind address :as binding]]]
                       (assoc (binding-row (subs (str alias) 1) kind (pr-str address)) :original binding))
                     (sort-by (comp str key) inputs))
     :targets (mapv target-row (:targets definition))
     :transition (if-let [beats (:beats transition)] (keyframes/duration-text beats) "")}))

(defn- parse-binding [index {:keys [alias kind address original]}]
  (let [number (inc index)
        alias (keyword-input alias)
        value (if (and original (= kind (first original)) (= address (pr-str (second original))))
                (second original)
                (if (= kind :source) (keyword-input address) (read-one address)))]
    (when-not alias (throw (js/Error. (str "Enter a keyword alias for input " number "."))))
    (case kind
      :source (when-not (keyword? value)
                (throw (js/Error. (str "Enter a ratom ID for input " number "."))))
      :subscription (when-not (and (vector? value) (keyword? (first value)))
                      (throw (js/Error. (str "Enter a subscription query beginning with a keyword for input " number "."))))
      (throw (js/Error. (str "Choose a signal type for input " number "."))))
    [alias [kind value]]))

(defn definition-fields [{:keys [bindings targets transition dimmer base]}]
  (let [ids (mapv :fixture-id targets)
        rows (mapv parse-binding (range) bindings)
        inputs (into {} rows)
        beats (when-not (str/blank? transition) (keyframes/parse-interval transition))
        dimmer-alias (some (fn [row] (when (= dimmer (:id row)) (keyword-input (:alias row)))) bindings)]
    (when-not (and (seq ids) (every? #(or (keyword? %) (string? %)) ids) (= (count ids) (count (distinct ids))))
      (throw (js/Error. "Select a fixture for each target and use each fixture once.")))
    (when-not (and (<= 1 (count rows) 64) (= (count rows) (count inputs)))
      (throw (js/Error. "Declare 1–64 inputs and use each alias once.")))
    (when (and (not (str/blank? transition)) (nil? beats))
      (throw (js/Error. "Enter a positive beat duration, such as 1/2, or leave the transition blank.")))
    (when (and (nil? base) (nil? dimmer-alias))
      (throw (js/Error. "Choose an input for the dimmer.")))
    (when-let [missing (some (fn [node]
                               (when (and (map? node) (= :reactive (:op node)) (not (contains? inputs (:key node))))
                                 (:key node)))
                             (tree-seq coll? seq (:outputs base)))]
      (throw (js/Error. (str "The output uses " missing ". Keep its alias or edit the output in EDN."))))
    (cond-> {:targets ids :reactive (cond-> {:inputs inputs} beats (assoc :transition {:beats beats}))}
      dimmer-alias (assoc :dimmer {:op :reactive :key dimmer-alias}))))

(defn- text-value [event] (.. event -target -value))

(defn- set-row [field id values draft]
  (update draft field
          (fn [rows] (mapv #(if (= id (:id %)) (merge % values) %) rows))))

(defn- remove-row [field id draft]
  (update draft field (fn [rows] (filterv #(not= id (:id %)) rows))))

(defn editor [draft busy? edit!]
  (let [{:keys [enabled? targets bindings dimmer transition base]} draft]
    [:fieldset.reactive-effect-editor {:disabled busy?}
     [:label.reactive-enable
      [checkbox/input {:checked (boolean enabled?)
                       :on-change (fn [event]
                                    (let [enabled? (.. event -target -checked)
                                          initial (when-not draft (new-draft))]
                                      (edit! #(assoc (or % initial) :enabled? enabled?))))}]
      "Evaluate when inputs change"]
     (when (and base (not enabled?))
       [:p.small.muted "Update this definition’s output expressions in EDN to turn off reactive evaluation."])
     (when enabled?
       [:div.reactive-fields
        (for [[index {:keys [id fixture-id]}] (map-indexed vector targets)]
          ^{:key id} [:div.reactive-target
           [fixtures/choose (str "Target fixture " (inc index)) "Choose fixture" fixture-id
            #(edit! (partial set-row :targets id {:fixture-id %}))]
           [:button.button {:type "button" :aria-label (str "Remove target " (inc index))
                            :on-click #(edit! (partial remove-row :targets id))} "Remove"]])
        [:button.button {:type "button"
                         :on-click (fn [] (let [row (target-row nil)] (edit! #(update % :targets conj row))))} "Add target"]
        [:h3 "Input bindings"]
        (for [[index {:keys [id alias kind address]}] (map-indexed vector bindings)
              :let [number (inc index) update! #(edit! (partial set-row :bindings id %))]]
          ^{:key id} [:div.reactive-binding
           [:label "Alias" [:input {:aria-label (str "Input " number " alias") :value alias :placeholder "level"
                                   :on-change #(update! {:alias (text-value %)})}]]
           [:label "Signal" [:select {:aria-label (str "Input " number " signal type") :value (name kind)
                                     :on-change #(update! {:kind (keyword (text-value %)) :address ""})}
                             [:option {:value "source"} "Ratom"]
                             [:option {:value "subscription"} "Subscription"]]]
           (let [label (if (= :source kind) "Ratom ID" "Subscription query")]
             [:label.reactive-address label
              [:input {:aria-label (str "Input " number " " label) :value address :spell-check false
                       :placeholder (if (= :source kind) ":wash/level" "[:wash/gain]")
                       :on-change #(update! {:address (text-value %)})}]])
           [:button.button {:type "button" :aria-label (str "Remove input " number)
                            :on-click #(edit! (partial remove-row :bindings id))} "Remove"]])
        [:button.button {:type "button" :disabled (>= (count bindings) 64)
                         :on-click (fn []
                                     (let [row (binding-row "" :subscription "")]
                                       (edit! #(update % :bindings conj row))))} "Add input"]
        [:div.reactive-output-options
         (when-not base
           [:label "Dimmer input"
            [:select {:value (or dimmer "") :on-change #(edit! (fn [draft] (assoc draft :dimmer (text-value %))))}
             [:option {:value ""} "Choose input"]
             (for [[index {:keys [id alias]}] (map-indexed vector bindings)]
               ^{:key id} [:option {:value id} (if (str/blank? alias) (str "Input " (inc index)) alias)])]])
         [:label "Transition (beats)"
          [:input {:value transition :placeholder "Immediate" :spell-check false
                   :on-change #(edit! (fn [draft] (assoc draft :transition (text-value %))))}]]]])]))
