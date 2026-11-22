(ns tolglow.studio.fog-fields
  "Fixture and room atmosphere authoring. Values stay in re-frame drafts until published."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]))

(defn- number! [label value valid?]
  (let [n (js/Number value)]
    (when-not (and (not (str/blank? (str value))) (js/Number.isFinite n) (valid? n))
      (throw (js/Error. (str "Check " label "."))))
    n))

(defn- vector-field [profile edits k defaults label valid?]
  (let [paths (mapv #(vector k %) (range (count defaults)))]
    (if (some #(contains? edits %) paths)
      (let [existing (get profile k defaults)
            values (mapv (fn [[_ index :as path]]
                           (let [v (get edits path (get existing index))]
                             (if (= "" v) (get defaults index) (number! label v valid?)))) paths)]
        (if (= values defaults) (dissoc profile k) (assoc profile k values)))
      profile)))

(defn apply-profile [definition edits]
  (if-not (:enabled? edits (some? (:fogger definition)))
    (dissoc definition :fogger)
    (let [old (:fogger definition {})
          flow (number! "rated fog flow (positive m³/min)" (get edits :flow-m3-min (:flow-m3-min old))
                        #(< 0 % 1e9))
          profile (assoc old :flow-m3-min flow)
          profile (if (contains? edits :mixing-seconds)
                    (if (= "" (:mixing-seconds edits)) (dissoc profile :mixing-seconds)
                      (assoc profile :mixing-seconds (number! "mixing time (0.001–86400 s)" (:mixing-seconds edits)
                                                             #(<= 0.001 % 86400))))
                    profile)
          profile (-> profile
                      (vector-field edits :dmx-range [0 255] "DMX endpoint (0–255)" #(<= 0 % 255))
                      (vector-field edits :direction [0 0 1] "nozzle direction" (constantly true)))]
      (when-not (get-in definition [:channels :fog])
        (throw (js/Error. "Add a fog channel to use a fogger profile.")))
      (when (apply = (:dmx-range profile [0 255]))
        (throw (js/Error. "Off and full DMX values must differ.")))
      (when (every? zero? (:direction profile [0 0 1]))
        (throw (js/Error. "Choose a nonzero nozzle direction.")))
      (assoc definition :fogger profile))))

(defn- input [edits profile field title placeholder]
  (let [path (if (keyword? field) [field] field)
        id (str "fogger-" (str/join "-" (map #(if (keyword? %) (name %) %) path)))]
    [:label {:for id} title
     [:input {:id id :type "text" :input-mode "decimal" :placeholder (str placeholder)
              :value (get edits field (str (get-in profile path)))
              :on-change #(rf/dispatch-sync [:fixtures/edit-fogger field (.. % -target -value)])}]]))

(defn fixture-controls [s]
  (when (contains? (get-in s [:definition :channels]) :fog)
    (let [profile (get-in s [:definition :fogger])
          edits (get-in s [:field-edits :fogger] {})
          enabled? (:enabled? edits (some? profile))]
      [:fieldset.fixture-type-fields {:disabled (boolean (:busy s))}
       [:legend "Fog machine"]
       [:label {:for "model-fog-output"} "Model fog output"
        [:input#model-fog-output {:type "checkbox" :checked enabled?
                                 :on-change #(rf/dispatch-sync [:fixtures/edit-fogger :enabled? (.. % -target -checked)])}]]
       (when enabled?
         [:<>
          [input edits profile :flow-m3-min "Rated fog flow (m³/min)" "Required"]
          [input edits profile :mixing-seconds "Plume mixing time (s)" 8]
          [input edits profile [:dmx-range 0] "Off DMX" 0]
          [input edits profile [:dmx-range 1] "Full DMX" 255]
          [input edits profile [:direction 0] "Nozzle direction · world X" 0]
          [input edits profile [:direction 1] "Nozzle direction · world Y" 0]
          [input edits profile [:direction 2] "Nozzle direction · world Z" 1]])])))

(defn set-room-value [db field value]
  (let [path [:ui :rig-draft :room]
        room (get-in db path)]
    (if (and (nil? value) (not (contains? room field))) db
      (-> db
          (assoc-in path (if (nil? value) (dissoc room field)
                           (assoc (merge {:width 12 :height 5 :depth 10} room) field value)))
          (assoc-in [:ui :rig-dirty?] true)))))

(defn room-error [{:keys [air-changes-per-hour fog-half-life-s]}]
  (cond
    (and (some? air-changes-per-hour)
         (not (and (js/Number.isFinite air-changes-per-hour) (<= 0 air-changes-per-hour 3600))))
    "Ventilation must be between 0 and 3600 air changes per hour."
    (and (some? fog-half-life-s)
         (not (and (js/Number.isFinite fog-half-life-s) (pos? fog-half-life-s))))
    "Fog half-life must be greater than zero seconds."))
