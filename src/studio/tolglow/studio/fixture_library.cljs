(ns tolglow.studio.fixture-library
  "Re-frame fixture discovery, import review, publication and explicit rig patching."
  (:require [clojure.string :as str] [cljs.reader :as reader] [cljs.pprint :as pprint] [re-frame.core :as rf] [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.fog-fields :as fog]
            [afterglow.render.photometry :as photometry]))

(def defaults
  {:query "" :manufacturer "" :results [] :total 0 :busy nil :error nil :notice nil
   :selected nil :mode "" :modes [] :upload nil :draft-source "" :definition nil
   :diagnostics [] :draft-edited? false :published-id nil :edit-revision 0 :request-generation 0
   :field-edits {} :channel-edits {} :add-channel ""
   :patch {:id "" :universe "1" :address "1" :x "0" :y "3" :z "0"
           :rx "0" :ry "0" :rz "0" :rgbw-policy "none"
           :pan-min "" :pan-max "" :tilt-min "" :tilt-max ""}})

(def example-definition
  {:id "rgbw-par" :version 1 :name "RGBW PAR" :manufacturer "Custom"
   :mode "RGBW + dimmer" :footprint 5
   :channels {:red 1 :green 2 :blue 3 :white 4 :dimmer 5} :rgbw-policy :none})

(defn- state [db] (merge defaults (get-in db [:ui :fixture-library])))

(defn- store [db value] (assoc-in db [:ui :fixture-library] value))

(defn- pretty [value] (with-out-str (pprint/pprint value)))

(defn- parse-definition [text] (reader/read-string text))

(defn- event-value [event] (.. event -target -value))

(defn- input! [path value] (rf/dispatch-sync [:fixtures/set path value]))

(defn- wire [value] (events/wire-id value))

(defn request-token [db]
  [(get-in db [:show :runtime-id]) (get-in db [:show :epoch])
   (:request-generation (state db))])

(defn current-request? [db token] (= (request-token db) token))

(defn- callback [db event] (into [(first event) (request-token db)] (rest event)))

(defn- reg-response-db [id handler]
  (rf/reg-event-db id
    (fn [db [_ token & arguments]]
      (if (current-request? db token) (handler db (into [id] arguments)) db))))

(defn- reg-response-fx [id handler]
  (rf/reg-event-fx id
    (fn [{:keys [db] :as coeffects} [_ token & arguments]]
      (if (current-request? db token) (handler coeffects (into [id] arguments)) {}))))

(defn- request [db operation path body success]
  {:db (store db (assoc (state db) :busy operation :error nil :notice nil))
   :studio/http (cond-> {:path path :timeout-ms 20000 :on-success (callback db success)
                        :on-failure (callback db [:fixtures/request-failed])}
                  (some? body) (assoc :body body))})
(declare form-definition)

(rf/reg-sub :fixtures/state (fn [db _] (state db)))
(rf/reg-event-db :fixtures/set
  (fn [db [_ path value]] (store db (assoc-in (state db) path value))))
(rf/reg-event-fx :fixtures/load
  (fn [{:keys [db]} _]
    {:studio/http {:path "/api/fixtures" :on-success (callback db [:fixtures/loaded])
                   :on-failure (callback db [:fixtures/request-failed])}}))
(rf/reg-event-fx :fixtures/runtime-change
  (fn [{:keys [db]} _]
    {:db (store db (-> (state db)
                       (assoc :published-id nil :publication nil :metadata nil :results [] :total 0
                              :selected nil :modes [] :mode "" :upload nil :upload-name nil
                              :busy nil :error nil :notice "Show changed. Review and publish this draft for the current show.")
                       (update :request-generation inc)))
     :dispatch [:fixtures/load]}))
(reg-response-db :fixtures/loaded
  (fn [db [_ metadata]] (store db (assoc (state db) :metadata metadata))))
(rf/reg-event-db :fixtures/error
  (fn [db [_ error]] (store db (assoc (state db) :busy nil :error (str error) :notice nil))))
(reg-response-db :fixtures/request-failed
  (fn [db [_ error]] (store db (assoc (state db) :busy nil :error (str error) :notice nil))))
(rf/reg-event-fx :fixtures/search
  (fn [{:keys [db]} [_ remote?]]
    (let [s (state db)]
      (request db :searching "/api/fixtures/search"
               (cond-> {:query (:query s) :remote? remote? :limit 100}
                 (seq (:manufacturer s)) (assoc :manufacturer (:manufacturer s)))
               [:fixtures/results]))))
(reg-response-db :fixtures/results
  (fn [db [_ result]]
    (store db (-> (state db) (assoc :busy nil :results (:results result []) :total (:total result 0)
                                  :notice (if (:offline? result) "Searching the saved catalog." "OFL results saved for offline search."))
                  (assoc-in [:metadata :catalog-count] (:catalog-count result))))))
(rf/reg-event-fx :fixtures/refresh
  (fn [{:keys [db]} _]
    (let [manufacturer (:manufacturer (state db))]
      (request db :refreshing "/api/fixtures/refresh"
               (if (seq manufacturer) {:manufacturer manufacturer} {}) [:fixtures/refreshed]))))
(reg-response-fx :fixtures/refreshed
  (fn [{:keys [db]} [_ result]]
    {:db (store db (cond-> (assoc (state db) :busy nil :notice "Catalog refreshed.")
                    (:results result) (assoc :results (:results result) :total (count (:results result)))))
     :dispatch [:fixtures/load]}))
(rf/reg-event-fx :fixtures/fetch
  (fn [{:keys [db]} [_ key]]
    (request db :fetching "/api/fixtures/fetch" {:key key} [:fixtures/fetched])))
(reg-response-fx :fixtures/fetched
  (fn [{:keys [db]} [_ result]]
    (let [fixture (:fixture result) modes (:modes fixture)]
      {:db (store db (assoc (state db) :busy nil :selected fixture :modes modes :mode (or (first modes) "")
                           :upload nil :published-id nil :notice "Choose the fixture's DMX mode, then import it for review."))
       :dispatch [:fixtures/load]})))
(rf/reg-event-fx :fixtures/import
  (fn [{:keys [db]} _]
    (let [s (state db)
          body (if (:upload s) (:upload s) {:key (get-in s [:selected :key])})]
      (request db :importing "/api/fixtures/import"
               (cond-> body (seq (:mode s)) (assoc :mode (:mode s))) [:fixtures/imported]))))

(defn- channel-offsets [value]
  (cond (number? value) [value] (vector? value) value
        (map? value) (remove nil? [(:offset value) (:fine-offset value)]) :else []))

(defn- patch-defaults [db definition]
  (let [fixtures (get-in db [:show :rig :fixtures] [])
        end (reduce max 0
                    (map (fn [fixture]
                           (+ (* 512 (dec (:universe fixture 1))) (dec (:address fixture 1))
                              (or (:footprint fixture)
                                  (reduce max 1 (mapcat channel-offsets (vals (:channels fixture))))))) fixtures))
        ids (set (map (comp wire :id) fixtures))
        id (first (remove ids (map #(str (wire (:id definition)) "-" %) (range 1 (+ 2 (count fixtures))))))
        calibration (:calibration definition)]
    (merge (:patch defaults)
           {:id id :universe (str (inc (quot end 512))) :address (str (inc (mod end 512)))
            :rgbw-policy (wire (:rgbw-policy definition "none"))
            :pan-min (str (get-in calibration [:pan :min-deg] ""))
            :pan-max (str (get-in calibration [:pan :max-deg] ""))
            :tilt-min (str (get-in calibration [:tilt :min-deg] ""))
            :tilt-max (str (get-in calibration [:tilt :max-deg] ""))})))

(reg-response-db :fixtures/imported
  (fn [db [_ result]]
    (let [definition (if-let [source (:source result)] (parse-definition source)
                         (or (:definition result) (:candidate result)))
          modes (:modes result)
          s (assoc (state db) :busy nil :diagnostics (:diagnostics result []) :import-valid? (:valid? result)
                   :edit-revision (inc (:edit-revision (state db)))
                   :published-id nil :draft-edited? false :error nil
                   :notice (if definition "Review the channels and diagnostics before publishing." "Choose a mode to preview this fixture."))]
      (store db (cond-> s
                  (seq modes) (assoc :modes modes :mode (if (some #{(:mode s)} modes) (:mode s) (first modes)))
                  definition (assoc :definition definition :draft-source (pretty definition) :channel-edits {} :field-edits {}
                                    :patch (patch-defaults db definition) :parse-error nil))))))
(rf/reg-event-db :fixtures/new
  (fn [db _]
    (store db (assoc (state db) :selected nil :upload nil :mode "" :modes [] :diagnostics []
                    :edit-revision (inc (:edit-revision (state db)))
                    :definition example-definition :draft-source (pretty example-definition) :channel-edits {} :field-edits {}
                    :import-valid? true :draft-edited? false :published-id nil :error nil :parse-error nil
                    :patch (patch-defaults db example-definition) :notice "Set the type ID and channel offsets, then publish."))))
(rf/reg-event-db :fixtures/edit-draft
  (fn [db [_ text]]
    (let [parsed (try {:value (parse-definition text)} (catch :default error {:error (.-message error)}))]
      (store db (assoc (state db) :draft-source text :definition (:value parsed) :parse-error (:error parsed)
                      :edit-revision (inc (:edit-revision (state db)))
                      :draft-edited? true :published-id nil :notice nil :channel-edits {} :field-edits {})))))

(rf/reg-fx :fixtures/read-upload
  (fn [{:keys [file token]}]
    (let [filename (.-name file) lower (str/lower-case filename)
          format (cond (str/ends-with? lower ".gdtf") "gdtf" (str/ends-with? lower ".qxf") "qxf" :else "ofl")
          limit (if (= format "gdtf") (* 16 1024 1024) (* 4 1024 1024))
          success #(rf/dispatch [:fixtures/uploaded token (merge {:format format :source (str "upload:" filename)} %) filename])
          failure #(rf/dispatch [:fixtures/request-failed token %])]
      (if (> (.-size file) limit)
        (failure (str "This file exceeds the " (quot limit (* 1024 1024)) " MiB import limit."))
        (if (= format "gdtf")
          (let [reader (js/FileReader.)]
            (set! (.-onload reader) #(success {:content-base64 (second (str/split (.-result reader) #"," 2))}))
            (set! (.-onerror reader) #(failure "Could not read the GDTF file."))
            (.readAsDataURL reader file))
          (-> (.text file) (.then #(success {:content %}))
              (.catch #(failure (events/error-message %)))))))))
(rf/reg-event-fx :fixtures/upload
  (fn [{:keys [db]} [_ file]]
    {:db (store db (assoc (state db) :busy :reading :error nil :notice nil))
     :fixtures/read-upload {:file file :token (request-token db)}}))
(reg-response-fx :fixtures/uploaded
  (fn [{:keys [db]} [_ upload filename]]
    {:db (store db (assoc (state db) :upload upload :upload-name filename :selected nil :modes [] :mode "" :busy nil))
     :dispatch [:fixtures/import]}))
(rf/reg-event-fx :fixtures/publish
  (fn [{:keys [db]} _]
    (let [s (state db)]
      (try
        (let [definition (form-definition s)
              source (pretty definition)
              db (store db (assoc s :definition definition :draft-source source :field-edits {} :channel-edits {}))]
          (request db :publishing "/api/fixtures/publish" {:source source}
                   [:fixtures/published (:edit-revision s) definition]))
        (catch :default error {:dispatch [:fixtures/error (str "Fix the fixture definition: " (.-message error))]})))))
(reg-response-db :fixtures/published
  (fn [db [_ revision definition result]]
    (let [s (state db)]
      (store db (if (= revision (:edit-revision s))
                  (assoc s :busy nil :published-id (:id definition) :definition definition
                         :notice (str "Published “" (:name definition (:id definition)) "”. Configure an instance below to add it to the rig.")
                         :publication result)
                  (assoc s :busy nil :notice "The previous revision was published. Current edits are still unpublished."))))))
(defn- number! [value label integer?]
  (let [n (js/Number value)]
    (when (or (str/blank? (str value)) (not (js/Number.isFinite n))
              (and integer? (not (js/Number.isInteger n))))
      (throw (js/Error. (str label " needs a finite " (if integer? "integer." "number."))))) n))

(defn form-definition [s]
  (let [definition (parse-definition (:draft-source s))]
    (when-not (map? definition) (throw (js/Error. "Fixture definition must be a map.")))
    (let [definition (reduce-kv
                      (fn [definition key value]
                        (cond
                          (= :fogger key) (fog/apply-profile definition value)
                          (and (some #{key} photometry/fixture-keys) (= "" value)) (dissoc definition key)
                          (#{:beam-angle :max-watts :measured-relative-brightness :lens-radius-m} key)
                          (assoc definition key (number! value (name key) false))
                          (= key :pseudo-fixture) (assoc definition key (= "true" value))
                          (#{:version :footprint} key) (assoc definition key (number! value (name key) true))
                          (and (= :id key) (keyword? (:id definition))) (assoc definition key (keyword value))
                          :else (assoc definition key value)))
                      definition (:field-edits s {}))]
      (when-let [error (first (photometry/errors definition))]
        (throw (js/Error. (:message error))))
      (reduce-kv (fn [definition attribute edits]
                   (let [spec (get-in definition [:channels attribute])
                         offsets (channel-offsets spec)
                         coarse (number! (:coarse edits (str (first offsets))) (str (name attribute) " offset") true)
                         raw-fine (:fine edits (str (or (second offsets) "")))
                         fine (when-not (str/blank? raw-fine) (number! raw-fine "Fine offset" true))
                         raw-inversion (:inversion edits (str (if (map? spec) (:inverted-from spec "") "")))
                         inversion (when-not (str/blank? raw-inversion) (number! raw-inversion "Inversion" false))
                         spec (cond (or (map? spec) (some? inversion))
                                    (cond-> {:offset coarse} fine (assoc :fine-offset fine)
                                      (some? inversion) (assoc :inverted-from inversion))
                                    fine [coarse fine] :else coarse)]
                     (assoc-in definition [:channels attribute] spec))) definition (:channel-edits s {})))))
(rf/reg-event-db :fixtures/edit-field
  (fn [db [_ key value]]
    (store db (-> (state db) (assoc-in [:field-edits key] value)
                  (assoc :draft-edited? true :published-id nil) (update :edit-revision inc)))))
(rf/reg-event-db :fixtures/edit-fogger
  (fn [db [_ field value]]
    (store db (-> (state db) (assoc-in [:field-edits :fogger field] value)
                  (assoc :draft-edited? true :published-id nil) (update :edit-revision inc)))))
(rf/reg-event-db :fixtures/edit-channel
  (fn [db [_ attribute field value]]
    (store db (-> (state db) (assoc-in [:channel-edits attribute field] value)
                  (assoc :draft-edited? true :published-id nil) (update :edit-revision inc)))))
(rf/reg-event-fx :fixtures/apply-form
  (fn [{:keys [db]} _]
    (try
      (let [s (state db) definition (form-definition s)]
        {:db (store db (assoc s :definition definition :draft-source (pretty definition)
                             :field-edits {} :channel-edits {} :parse-error nil))})
      (catch :default error {:dispatch [:fixtures/error (.-message error)]}))))
(rf/reg-event-fx :fixtures/add-channel
  (fn [{:keys [db]} _]
    (let [s (state db) name (str/trim (:add-channel s))]
      (try
        (when-not (re-matches #"[a-z][a-z0-9-]*" name)
          (throw (js/Error. "Use a channel attribute such as red, dimmer, pan or shutter.")))
        (let [attribute (keyword name) definition (form-definition s)]
          (when (contains? (:channels definition) attribute) (throw (js/Error. "That channel already exists.")))
          (let [definition (assoc-in definition [:channels attribute] 0)]
            {:db (store db (-> s (assoc :definition definition :draft-source (pretty definition) :field-edits {}
                                        :channel-edits {attribute {:coarse ""}} :add-channel "" :published-id nil
                                        :draft-edited? true :parse-error nil)
                                (update :edit-revision inc)))}))
        (catch :default error {:dispatch [:fixtures/error (.-message error)]})))))
(rf/reg-event-fx :fixtures/remove-channel
  (fn [{:keys [db]} [_ attribute]]
    (try
      (let [s (state db)
            ;; Removing the controller channel also removes its physical profile.
            source (if (= attribute :fog) (assoc-in s [:field-edits :fogger] {:enabled? false}) s)
            definition (update (form-definition source) :channels dissoc attribute)]
        {:db (store db (-> s (assoc :definition definition :draft-source (pretty definition) :field-edits {}
                                    :channel-edits {} :published-id nil :draft-edited? true :parse-error nil)
                            (update :edit-revision inc)))})
      (catch :default error {:dispatch [:fixtures/error (.-message error)]}))))
(rf/reg-event-fx :fixtures/patch
  (fn [{:keys [db]} _]
    (let [s (state db) patch (:patch s) definition (:definition s)]
      (try
        (when-not (:published-id s) (throw (js/Error. "Publish this fixture type before patching it.")))
        (let [calibration (into {} (for [axis [:pan :tilt] :when (contains? (:channels definition) axis)]
                                     [axis {:min-deg (number! (get patch (keyword (str (name axis) "-min"))) (str (name axis) " minimum") false)
                                            :max-deg (number! (get patch (keyword (str (name axis) "-max"))) (str (name axis) " maximum") false)}]))
              body {:fixture-type-id (wire (:published-id s)) :id (:id patch)
                    :universe (number! (:universe patch) "Universe" true) :address (number! (:address patch) "Address" true)
                    :position (mapv #(number! (get patch %) (str (name %) " position") false) [:x :y :z])
                    :rotation-deg (mapv #(number! (get patch %) (str (name %) " rotation") false) [:rx :ry :rz])
                    :rgbw-policy (:rgbw-policy patch) :calibration calibration}]
          (request db :patching "/api/fixtures/patch" body [:fixtures/patched]))
        (catch :default error {:dispatch [:fixtures/error (.-message error)]})))))
(reg-response-fx :fixtures/patched
  (fn [{:keys [db]} [_ result]]
    (if (false? (:valid? result))
      {:db (store db (assoc (state db) :busy nil :diagnostics (:diagnostics result) :error "Resolve the patch diagnostics below."))}
      {:db (-> db (store (assoc (state db) :busy nil :notice "Fixture added to the rig. Its address and position are now saved."))
               (assoc-in [:ui :rig-dirty?] false))})))

(defn- field [s key label & [attrs]]
  [:label {:for (str "fixture-patch-" (name key))} label
   [:input (merge {:id (str "fixture-patch-" (name key)) :type "text" :input-mode "decimal" :disabled (boolean (:busy s))
                   :value (get-in s [:patch key] "")
                   :on-change #(input! [:patch key] (event-value %))} attrs)]])

(defn- diagnostics [items]
  (when (seq items)
    [:ul.fixture-diagnostics {:aria-label "Fixture import and patch diagnostics"}
     (for [[index item] (map-indexed vector items)]
       ^{:key index} [:li {:class (wire (:severity item))}
                     [:strong (str/capitalize (wire (:severity item)))]
                     [:span (:message item)]
                     (when (seq (:path item)) [:small (str/join " / " (map wire (:path item)))])])]))

(defn- provenance [definition]
  (when-let [source (:provenance definition)]
    [:div.fixture-provenance
     [:span "Source: " (let [url (:source source)]
                         (if (and (string? url) (re-matches #"https?://.+" url))
                           [:a {:href url :target "_blank" :rel "noreferrer noopener"} (wire (or (:provider source) url))]
                           (or url "User authored")))]
     [:span "License: " (if (map? (:license source)) (get-in source [:license :spdx] "See source notice")
                            (or (:license source) "Not specified"))]
     (when-let [hash (:sha256 source)] [:span {:title hash} "SHA-256 " (subs hash 0 (min 12 (count hash))) "…"])]))

(defn- browse [s]
  (let [busy? (boolean (:busy s)) manufacturers (get-in s [:metadata :manufacturers] [])]
    [:section.fixture-browser
     [:div.fixture-step [:span "01"] [:h3 "Find a fixture"]]
     [:form.fixture-search {:on-submit (fn [event] (.preventDefault event) (rf/dispatch [:fixtures/search false]))}
      [:label {:for "fixture-library-query"} "Fixture or manufacturer"
       [:input#fixture-library-query {:type "search" :placeholder "Robe LEDBeam, RGBW wash…" :value (:query s)
                                     :on-change #(input! [:query] (event-value %))}]]
      [:div.button-row
       [:button.button {:type "submit" :disabled busy?} "Search saved"]
       [:button.button.primary {:type "button" :disabled busy? :on-click #(rf/dispatch [:fixtures/search true])} "Search OFL ↗"]]]
     [:div.fixture-catalog-filter
      [:label {:for "fixture-manufacturer"} "Manufacturer catalog"
       [:select#fixture-manufacturer {:value (:manufacturer s) :on-change #(input! [:manufacturer] (event-value %))}
        [:option {:value ""} "All manufacturers"]
        (for [manufacturer manufacturers]
          ^{:key (:key manufacturer)} [:option {:value (:key manufacturer)} (:name manufacturer)])]]
      [:button.button {:type "button" :disabled busy? :on-click #(rf/dispatch [:fixtures/refresh])}
       (if (seq (:manufacturer s)) "Save this catalog" "Load manufacturers")]]
     [:div.fixture-results-heading [:span (str (:total s) " matches")]
      [:span (str (get-in s [:metadata :catalog-count] 0) " saved catalog entries")]]
     [:div.fixture-results {:aria-label "Fixture search results"}
      (if (seq (:results s))
        (for [entry (:results s)]
          ^{:key (:key entry)}
          [:button.fixture-result {:type "button" :disabled busy?
                                   :class (when (= (:key entry) (get-in s [:selected :key])) "selected")
                                   :on-click #(rf/dispatch [:fixtures/fetch (:key entry)])}
           [:span [:strong (:name entry)] [:small (or (:manufacturer entry) (:manufacturer-key entry))]]
           [:span.fixture-result-state (if (:cached? entry) "Saved" "Get modes →")]])
        [:p.small.muted.fixture-empty "Search the saved catalog, or search OFL to discover definitions. Fetching a definition does not add it to your rig."])]
     (when (or (:selected s) (and (:upload s) (seq (:modes s))))
       [:div.fixture-mode
        [:strong (or (get-in s [:selected :name]) (:upload-name s))]
        [:label {:for "fixture-mode"} "DMX mode"
         [:select#fixture-mode {:value (:mode s) :on-change #(input! [:mode] (event-value %))}
          (for [mode (:modes s)] ^{:key mode} [:option {:value mode} mode])]]
        [:button.button.primary {:type "button" :disabled (or busy? (str/blank? (:mode s)))
                                 :on-click #(rf/dispatch [:fixtures/import])} "Import mode for review"]])
     [:div.fixture-import-actions
      [:label.button.fixture-file {:for "fixture-upload"} "Upload OFL / QXF / GDTF"
       [:input#fixture-upload {:type "file" :accept ".json,.qxf,.gdtf,application/json,application/zip,text/xml"
                               :disabled busy?
                               :on-change (fn [event]
                                            (when-let [file (aget (.. event -target -files) 0)]
                                              (rf/dispatch [:fixtures/upload file]))
                                            (set! (.. event -target -value) ""))}]]
      [:button.text-button {:type "button" :disabled busy? :on-click #(rf/dispatch [:fixtures/new])} "Create a fixture type →"]]]))

(defn- type-field [s key title numeric?]
  [:label {:for (str "fixture-type-" (name key))} title
   [:input {:id (str "fixture-type-" (name key)) :type "text" :disabled (boolean (:busy s))
            :input-mode (if numeric? "decimal" "text")
            :value (get-in s [:field-edits key] (wire (get-in s [:definition key] "")))
            :on-change #(rf/dispatch-sync [:fixtures/edit-field key (event-value %)])}]])

(defn- mapping-form [s]
  (when (map? (:definition s))
    [:div.fixture-type-form
     [:div.fixture-type-fields
      [type-field s :id "Fixture type ID" false] [type-field s :name "Display name" false]
      [type-field s :manufacturer "Manufacturer" false] [type-field s :mode "Mode name" false]
      [type-field s :version "Definition version" true] [type-field s :footprint "DMX footprint" true]]
     [:fieldset.fixture-type-fields
      [:legend "Beam and brightness"]
      [type-field s :beam-angle "Full beam angle (°)" true]
      [type-field s :lens-radius-m "Lens radius (m)" true]
      [type-field s :max-watts "Maximum electrical power (W)" true]
      [type-field s :measured-relative-brightness "Measured brightness (× reference)" true]
      [:label {:for "fixture-type-pseudo-fixture"} "Reflected light"
       [:select#fixture-type-pseudo-fixture
        {:value (get-in s [:field-edits :pseudo-fixture]
                        (str (get-in s [:definition :pseudo-fixture])))
         :disabled (boolean (:busy s))
         :on-change #(rf/dispatch-sync [:fixtures/edit-field :pseudo-fixture (event-value %)])}
        [:option {:value ""} "Preview default"]
        [:option {:value "true"} "Enable reflected light"]
        [:option {:value "false"} "Disable reflected light"]]]]
     [fog/fixture-controls s]
     [:div.fixture-channel-heading [:strong "Channel mapping"]
      [:span.small.muted "Offsets start at 1 · fine and inversion are optional"]]
     [:div.fixture-channel-list
      (for [[attribute spec] (sort-by (comp str key) (get-in s [:definition :channels]))
            :let [offsets (channel-offsets spec)
                  fields [[:coarse "Coarse" (str (first offsets))]
                          [:fine "Fine" (str (or (second offsets) ""))]
                          [:inversion "Invert from" (str (if (map? spec) (:inverted-from spec "") ""))]]]]
        ^{:key attribute}
        [:div.fixture-channel-row
         [:strong (str/capitalize (wire attribute))]
         (for [[key title fallback] fields]
           ^{:key key}
           [:label title
            [:input {:type "text" :input-mode "decimal" :disabled (boolean (:busy s))
                     :aria-label (str (wire attribute) " " title)
                     :value (get-in s [:channel-edits attribute key] fallback)
                     :on-change #(rf/dispatch-sync [:fixtures/edit-channel attribute key (event-value %)])}]])
         [:button.icon-button {:type "button" :disabled (boolean (:busy s))
                               :aria-label (str "Remove " (wire attribute) " channel")
                               :on-click #(rf/dispatch [:fixtures/remove-channel attribute])} "×"]])]
     (let [unmapped (filter #(and (some? (:key %)) (nil? (:attribute %)))
                            (get-in s [:definition :channel-definitions]))]
       (when (seq unmapped)
         [:p.small.fixture-unmapped "Unmapped source controls: "
          (str/join "; " (map #(str (:key %) " (" (str/join ", " (or (:offsets %) [(:offset %)])) ")") unmapped))
          ". Check the fixture manual before assigning their function."]))
     [:div.fixture-add-channel
      [:label {:for "fixture-add-attribute"} "Add a channel attribute"
       [:input#fixture-add-attribute {:type "text" :placeholder "red, dimmer, pan…" :value (:add-channel s)
                                      :disabled (boolean (:busy s)) :on-change #(input! [:add-channel] (event-value %))}]]
      [:button.button {:type "button" :disabled (boolean (or (:busy s) (str/blank? (:add-channel s))))
                       :on-click #(rf/dispatch [:fixtures/add-channel])} "Add channel"]]
     (when (or (seq (:channel-edits s)) (seq (:field-edits s)))
       [:button.text-button {:type "button" :disabled (boolean (:busy s))
                             :on-click #(rf/dispatch [:fixtures/apply-form])} "Apply edits to the advanced definition"])]))

(defn- review [s]
  [:section.fixture-review
   [:div.fixture-step [:span "02"] [:h3 "Review & publish"]]
   (if (seq (:draft-source s))
     [:<>
      [provenance (:definition s)]
      [diagnostics (:diagnostics s)]
      [mapping-form s]
      [:details.fixture-advanced
       [:summary "Advanced definition · EDN"]
       [:label.fixture-editor-label {:for "fixture-type-source"} "Typed source, provenance and full capability data"]
       [:textarea#fixture-type-source {:value (:draft-source s) :spell-check false :auto-capitalize "off" :auto-complete "off"
                                       :disabled (boolean (:busy s)) :aria-describedby "fixture-source-status"
                                       :on-change #(rf/dispatch-sync [:fixtures/edit-draft (event-value %)])}]]
      [:div#fixture-source-status.small {:class (if (:parse-error s) "fixture-invalid" "muted") :aria-live "polite"}
       (or (:parse-error s) (if (:published-id s) "This fixture type is published." "Changes are validated by the server when you publish."))]
      [:div.button-row
       [:button.button.primary {:type "button"
                                :disabled (boolean (or (:busy s) (:parse-error s) (:published-id s)
                                                       (and (false? (:import-valid? s)) (not (:draft-edited? s)))))
                                :on-click #(rf/dispatch [:fixtures/publish])}
        (if (:published-id s) "Published ✓" "Validate & publish type")]]]
     [:p.small.muted.fixture-empty "Choose a mode or upload a definition to inspect its channels and diagnostics. You can also create an RGBW fixture type from the starter definition."])])

(defn- patch-panel [s db]
  (when (:published-id s)
    [:section.fixture-patch
     [:div.fixture-step [:span "03"] [:h3 "Add an instance to the rig"]]
     [:p.small.muted "Set its physical address, position and orientation. Existing fixture addresses are checked before the rig changes."]
     [:div.fixture-field-grid
      [field s :id "Instance ID" {:input-mode "text"}]
      [field s :universe "Universe" {:input-mode "numeric"}]
      [field s :address "Start address" {:input-mode "numeric"}]
      [:label {:for "fixture-white-policy"} "RGBW mixing"
       [:select#fixture-white-policy {:value (get-in s [:patch :rgbw-policy])
                                      :on-change #(input! [:patch :rgbw-policy] (event-value %))}
        [:option {:value "none"} "Explicit white channel"]
        [:option {:value "extract-min"} "Extract shared RGB into white"]]]]
     [:div.fixture-field-grid.fixture-position
      [field s :x "X · metres"] [field s :y "Y · metres"] [field s :z "Z · metres"]
      [field s :rx "Rotate X · degrees"] [field s :ry "Rotate Y · degrees"] [field s :rz "Rotate Z · degrees"]]
     (for [axis [:pan :tilt] :when (contains? (get-in s [:definition :channels]) axis)]
       ^{:key axis} [:div.fixture-calibration
                    [:span (str/capitalize (name axis)) " range from the fixture's documentation"]
                    [field s (keyword (str (name axis) "-min")) "Minimum angle · degrees"]
                    [field s (keyword (str (name axis) "-max")) "Maximum angle · degrees"]])
     (when (get-in db [:ui :rig-dirty?])
       [:p.fixture-invalid "Save or discard your current rig edits before adding this instance."])
     [:button.button.primary {:type "button" :disabled (boolean (or (:busy s) (get-in db [:ui :rig-dirty?])))
                              :on-click #(rf/dispatch [:fixtures/patch])} "Add fixture to rig"]]))

(def panel
  (r/create-class
   {:display-name "Fixture library"
    :component-did-mount (fn [_] (rf/dispatch [:fixtures/load]))
    :reagent-render
    (fn [db]
      (let [s @(rf/subscribe [:fixtures/state])]
        [:section.panel.fixture-library {:aria-label "Fixture library and import"}
         [:div.panel-top [:span.section-label "FIXTURE LIBRARY"]
          [:span.small.muted (str (get-in s [:metadata :document-count] 0) " definitions available offline")]]
         [:div.fixture-library-intro [:h2 "Build your rig from real fixture definitions"]
          [:p "Find a fixture, choose its mode, and review its channels before adding it to the room."]]
         (when (:busy s) [:div.fixture-status {:role "status"} (str/capitalize (name (:busy s))) "…"])
         (when (:error s) [:div.fixture-status.fixture-invalid {:role "alert"} (:error s)])
         (when (:notice s) [:div.fixture-status {:role "status"} (:notice s)])
         [:div.fixture-library-columns [browse s] [review s]]
         [patch-panel s db]]))}))
