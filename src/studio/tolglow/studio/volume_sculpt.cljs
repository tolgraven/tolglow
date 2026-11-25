(ns tolglow.studio.volume-sculpt
  "Sculpt an unsaved volume and publish it through Studio's typed command boundary."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.autopilot-signal :as signal]
            [tolglow.studio.spatial-drafts :as drafts]
            [tolglow.studio.spatial-picker :as picker]
            [tolglow.studio.spatial-fields :refer [number-input xyz]]
            [tolglow.studio.volume-geometry :as geo]))

(def initial
  {:volume (assoc (geo/primitive :sphere) :position [0 1.5 0] :selection #{:position}
                  :strength 1 :falloff {:inner-m 0 :outer-m 0 :curve :linear})
   :paint {:color [1 0.35 0.1]} :members [] :path [] :add-shape :sphere :undo []})

(defn model [db] (or (:sculpt (drafts/state db)) initial))

(defn- put [db draft] (drafts/update-ui db assoc :sculpt draft))

(defn- remember [draft]
  (update draft :undo #(vec (take-last 25 (conj % (select-keys draft [:volume :path]))))))

(defn- field [draft path v]
  (let [draft (if (= :volume (first path)) (remember draft) draft)]
    (assoc-in draft path v)))

(rf/reg-sub ::draft (fn [db _] (model db)))
(rf/reg-sub ::volume :<- [::draft] (fn [d _] (:volume d)))
(rf/reg-sub ::guide :<- [::volume] (fn [volume _] (geo/guides volume)))
(rf/reg-sub ::render-guide :<- [::guide] :<- [::draft]
  (fn [[guide draft] _] (when guide (assoc guide :color (get-in draft [:paint :color]) :key (hash [(:volume draft) (:paint draft)])))))
(rf/reg-sub ::signals (fn [db _] (get-in (picker/show db) [:settings :autopilot :signals])))
(rf/reg-event-db ::set (fn [db [_ path v]] (put db (field (model db) path v))))
(rf/reg-event-db ::undo
  (fn [db _]
    (let [{:keys [undo] :as draft} (model db)]
      (if-let [saved (peek undo)]
        (put db (merge (dissoc draft :gesture) saved {:undo (pop undo)})) db))))
(rf/reg-event-db ::reset
  (fn [db _] (put db (assoc initial :undo (:undo (remember (model db)))))))
(rf/reg-event-fx ::open
  (fn [{:keys [db]} [_ id spec]]
    {:db (put db (cond-> initial spec (assoc :volume spec) id (assoc :volume-id id)))
     :dispatch-n [[:rig-editor/open] [:rig-editor/set [:tool] "sculpt-volume"]]}))
(rf/reg-event-db ::moving
  (fn [db [_ enabled?]]
    (let [draft (model db)]
      (put db (-> (remember draft) (update :volume geo/moving enabled?)
                  (assoc :path (if enabled? [:geometry] [])))))))
(rf/reg-event-db ::combine
  (fn [db [_ operation]]
    (let [{:keys [path add-shape] :as draft} (model db)
          node-path (into [:volume] path)
          node (get-in draft node-path)]
      (put db (-> (remember draft)
                  (assoc-in node-path (geo/combine node operation add-shape))
                  (assoc :path (conj path :parts 1)))))))
(rf/reg-event-db ::remove-part
  (fn [db _]
    (let [{:keys [path] :as draft} (model db)]
      (if-not (number? (peek path)) db
        (let [index (peek path) parent (pop (pop path))
              at (into [:volume] parent)
              parts (get-in draft (conj at :parts))
              remaining (into (subvec parts 0 index) (subvec parts (inc index)))
              node (if (= 1 (count remaining))
                     ;; Retain the parent's transforms when collapsing the last pair.
                     (assoc (get-in draft at) :shape :union :parts remaining)
                     (assoc (get-in draft at) :parts remaining))]
          (put db (-> (remember draft) (assoc-in at node) (assoc :path parent))))))))

(defn gesture [db {:keys [type token point delta axis half-extent]}]
  (let [{current :gesture :as draft} (model db)]
    (case type
      "sculpt-begin" (if current db (put db (assoc draft :gesture {:token token :before (:volume draft) :point point})))
      "sculpt-move" (if (and current (= token (:token current)))
                      (let [before (:before current)
                            spec (if (some? axis)
                                   (geo/stretch before axis delta half-extent)
                                   (geo/translate before (mapv - point (:point current))))]
                        (put db (assoc draft :volume spec))) db)
      "sculpt-end" (if (and current (= token (:token current)))
                     (let [done (dissoc draft :gesture)]
                       (put db (if (= (:volume done) (:before current)) done
                                 (assoc done :undo (:undo (remember (assoc done :volume (:before current)))))))) db)
      "sculpt-cancel" (if (and current (= token (:token current)))
                        (put db (-> draft (assoc :volume (:before current)) (dissoc :gesture))) db)
      db)))
(rf/reg-event-db ::gesture
  (fn [db [_ detail]]
    (if (= (:identity detail) (get-in db [:ui :rig-editor :identity])) (gesture db detail) db)))

(defn- page-id [db]
  (let [selected (get-in db [:ui :page-id])
        pages (:pages (picker/show db))]
    (or (some #(when (= (picker/label (:id %)) (picker/label selected)) (:id %)) pages)
        (:id (first pages)))))

(defn parse-source [value]
  (cond (vector? value) (mapv signal/parse-spec value)
        (:hsl value) (update value :hsl #(mapv signal/parse-spec %))
        :else (signal/parse-spec value)))

(defn publish [db action token]
  (let [{:keys [volume volume-id cue-id paint members sources pending] :as draft} (model db)
        owner (picker/identity-of db)
        id (or (when-not (str/blank? (picker/label volume-id)) volume-id) (not-empty cue-id) "sculpted-volume")
        common {:runtime-id (:runtime-id owner) :expected-epoch (:epoch owner)}
        config? (= action :volume)
        body (if config?
               {:scope :spatial :values {:enabled? true :volumes {id volume}}}
               (cond-> {:op :spawn :volume volume :paint paint :members members}
                 (seq cue-id) (assoc :id cue-id)
                 (:scene-id draft) (assoc :scene-id (:scene-id draft))
                 (page-id db) (assoc :page-id (page-id db))
                 (seq sources) (assoc :sources (update-vals sources parse-source))))]
    (when (and (:runtime-id owner) (get-in db [:connection :connected?]) (nil? pending))
      {:db (put db (assoc draft :pending token :message nil))
       :studio/http {:path (if config? "/api/settings" "/api/spatial/cues") :typed? true
                     :body (merge body common) :raw-response? true
                     :on-success [::receipt owner token action]
                     :on-failure [::failure owner token]}})))
(rf/reg-event-fx ::publish
  (fn [{:keys [db]} [_ action]]
    (try (publish db action (str (random-uuid)))
         (catch :default e {:db (put db (assoc (model db) :message (ex-message e)))}))))
(defn- matches? [db owner token]
  (and (= owner (picker/identity-of db)) (= token (:pending (model db)))))
(rf/reg-event-db ::receipt
  (fn [db [_ owner token action receipt]]
    (if (matches? db owner token)
      (put db (-> (model db) (dissoc :pending)
                  (assoc :message (if (= action :volume) "Volume saved."
                                    (str "Launched " (picker/label (:cue-id receipt)) " on "
                                         (picker/label (:page-id receipt)) " at " (pr-str (:cell receipt)) "."))))) db)))
(rf/reg-event-db ::failure
  (fn [db [_ owner token error]]
    (if (matches? db owner token) (put db (-> (model db) (dissoc :pending) (assoc :message (str error)))) db)))

(defn- dispatch! [event] (rf/dispatch-sync event))

(defn- assign! [path value] (dispatch! [::set path value]))

(defn- value [e] (.. e -target -value))

(defn- choices [caption options selected change!]
  [:label caption
   [:select {:aria-label caption :value (pr-str selected)
             :on-change #(when-let [[v] (first (filter (fn [[v]] (= (pr-str v) (value %))) options))] (change! v))}
    (for [[v label] options] ^{:key (pr-str v)} [:option {:value (pr-str v)} label])]])

(def shape-options [[:box "Cube"] [:rounded-box "Rounded box"] [:sphere "Sphere"]])

(defn- controls [draft]
  (let [{:keys [volume path add-shape]} draft
        at (into [:volume] path)
        node (get-in draft at)
        edit #(assign! (conj at %1) %2)
        options (mapv (fn [[p s]] [p (str (if (empty? p) "Whole volume" (if (= :geometry (peek p)) "Ball geometry" (str "Part " (str/join "." (map inc (filter number? p))))))
                                        " · " (name (:shape s)))]) (geo/nodes volume))]
    [:<>
     [:label.spatial-check [:input {:type "checkbox" :checked (= :ball (:shape volume))
                                    :on-change #(dispatch! [::moving (.. % -target -checked)])}] "Carry shape on a bouncing ball"]
     (when (= :ball (:shape volume))
       [:<> [xyz "Initial velocity · m/s" (:velocity volume [0 0 0]) #(assign! [:volume :velocity] %)]
        [number-input "Bounciness" (:restitution volume 0.85) #(assign! [:volume :restitution] %) {:min 0 :max 1 :step 0.05}]])
     [choices "Editing shape" options path #(assign! [:path] %)]
     [choices "Primitive" shape-options add-shape #(assign! [:add-shape] %)]
     [:div.spatial-actions
      [:button.button {:on-click #(assign! at (merge (select-keys node [:translation :scale :selection :strength :falloff])
                                                     (geo/primitive add-shape) {:position (:position node [0 1.5 0])}))}
       "Replace shape"]
      (for [[operation label] [[:union "Union"] [:intersection "Intersect"] [:difference "Subtract"]]]
        ^{:key operation} [:button.button {:on-click #(dispatch! [::combine operation])} label])
      (when (number? (peek path)) [:button.button {:on-click #(dispatch! [::remove-part])} "Remove part"])]
     [xyz "Translation · m" (:translation node [0 0 0]) #(edit :translation %)]
     [xyz "Stretch" (:scale node [1 1 1]) #(edit :scale %) {:min 0.001}]
     (case (:shape node)
       (:sphere :ball) [number-input "Sphere radius · m" (:radius node) #(edit :radius %) {:min 0.001}]
       :rounded-box [:<> [xyz "Full size · m" (:size node) #(edit :size %) {:min 0.001}]
                     [number-input "Corner rounding" (:rounding node 0) #(edit :rounding %) {:min 0 :max 1 :step 0.05}]]
       nil)]))

(defn- weights [volume]
  [:fieldset [:legend "Affected heads"]
   (for [[k caption] [[:position "Head inside"] [:beam "Beam intersects"] [:aimable "Can aim into volume"]]
         :let [selection (set (:selection volume #{:position}))]]
     ^{:key k} [:label.spatial-check [:input {:type "checkbox" :checked (contains? selection k)
                                              :on-change #(assign! [:volume :selection]
                                                                   ((if (.. % -target -checked) conj disj) selection k))}] caption])
   [number-input "Strength" (:strength volume 1) #(assign! [:volume :strength] %) {:min 0 :max 1 :step 0.05}]
   [number-input "Inside falloff · m" (get-in volume [:falloff :inner-m] 0)
    #(assign! [:volume :falloff :inner-m] %) {:min 0}]
   [number-input "Outside falloff · m" (get-in volume [:falloff :outer-m] 0)
    #(assign! [:volume :falloff :outer-m] %) {:min 0}]
   [choices "Falloff curve" [[:linear "Linear"] [:smoothstep "Smooth"]] (get-in volume [:falloff :curve] :linear)
    #(assign! [:volume :falloff :curve] %)]])

(defn- hex-color [rgb]
  (str "#" (apply str (map #(.padStart (.toString (js/Math.round (* 255 %)) 16) 2 "0") rgb))))

(defn- rgb-color [hex] (mapv #(/ (js/parseInt (subs hex % (+ 2 %)) 16) 255) [1 3 5]))
(rf/reg-sub ::scenes
  (fn [db _] (filterv #(= :macro (:mode %)) (:compositions (picker/show db)))))

(defn- source-options [{:keys [volume paint]}]
  (let [common [[:strength "Strength" (:strength volume 1) 0 1]
                [:inner-m "Inner falloff" (get-in volume [:falloff :inner-m] 0) 0 10]
                [:outer-m "Outer falloff" (get-in volume [:falloff :outer-m] 0) 0 10]
                [:translation "Translation" (:translation volume [0 0 0]) -10 10]
                [:color "RGB color" (:color paint) 0 1]
                [:hsl "HSL color" [0.05 0.8 0.5] 0 1]]
        dimensions [[:position "Position" -10 10] [:radius "Radius" 0.01 10] [:size "Size" 0.01 10]
                    [:rounding "Rounding" 0 1] [:restitution "Bounciness" 0 1] [:velocity "Velocity" -10 10]]]
    (into common (keep (fn [[k label lo hi]] (when-let [v (get volume k)] [k label v lo hi]))) dimensions)))

(defn- modulation [{:keys [sources source-target] :as draft}]
  (let [signals @(rf/subscribe [::signals])
        rows (source-options draft)
        [k label baseline lo hi] (or (first (filter #(= source-target (first %)) rows)) (first rows))
        source-key (if (= k :hsl) :color k)
        configured (get sources source-key)
        active? (and (some? configured) (case k :hsl (boolean (:hsl configured)) :color (vector? configured) true))
        source (if active? (if (= k :hsl) (:hsl configured) configured) baseline)
        save! #(assign! [:sources source-key] (if (= k :hsl) {:hsl %} %))
        options {:min lo :max hi :step 0.01}]
    [:details [:summary "Parameter modulation"]
     [choices "Modulate" (mapv #(subvec % 0 2) rows) k #(assign! [:source-target] %)]
     [:label.spatial-check [:input {:type "checkbox" :checked active?
                                    :on-change #(if (.. % -target -checked) (save! baseline)
                                                  (assign! [:sources] (dissoc sources source-key)))}] "Use source"]
     (when active?
       (if (vector? source)
         (let [letters (case k :color ["Red" "Green" "Blue"] :hsl ["Hue" "Saturation" "Lightness"] ["X" "Y" "Z"])]
           [:<> (for [[i v] (map-indexed vector source)]
                  ^{:key [k i]} [signal/field [:sculpt k i] (str label " " (letters i)) v options signals #(save! (assoc source i %))])])
         [signal/field [:sculpt k] label source options signals save!]))]))

(defn- launches [{:keys [paint members] :as draft}]
  [:fieldset [:legend "Cue"]
   [:label "Cue ID" [:input {:value (:cue-id draft "") :placeholder "Generate automatically"
                             :on-change #(assign! [:cue-id] (value %))}]]
   [:label.spatial-check [:input {:type "checkbox" :checked (contains? paint :color)
                                   :on-change #(assign! [:paint] (if (.. % -target -checked)
                                                                  (assoc paint :color [1 0.35 0.1]) (dissoc paint :color)))}] "Paint color"]
   (when (:color paint)
     [:label "Field color" [:input {:type "color" :value (hex-color (:color paint))
                                  :on-change #(assign! [:paint :color] (rgb-color (value %)))}]])
   (when-let [scenes (seq @(rf/subscribe [::scenes]))]
     [choices "Macro to include" (into [[nil "None"]] (map (fn [{:keys [id name]}] [id (or name (picker/label id))])) scenes)
      (:scene-id draft) #(assign! [:scene-id] %)])
   [picker/picker {:title "Add cue or effect" :kinds [:cue :effect]
                   :on-select (fn [{:keys [kind id]}]
                                (assign! [:members] (conj members {(case kind :cue :cue-id :effect :effect-id) id})))}]
   (for [[i member] (map-indexed vector members)]
     ^{:key i} [:div.spatial-actions [:span (picker/label (or (:cue-id member) (:effect-id member)))]
                [:button.text-button {:on-click #(assign! [:members] (into (subvec members 0 i) (subvec members (inc i))))}
                 "Remove"]])
   [modulation draft]])

(defn panel []
  (let [{:keys [volume pending message] :as draft} @(rf/subscribe [::draft])]
    [:section.rig-path-form.spatial-panel {:aria-label "Sculpt volume"}
     [:h3 "Sculpt volume"]
     [:p.small.muted "Drag the centre in the selected plane. Drag a coloured axis handle to stretch the whole volume."]
     [controls draft]
     [weights volume]
     [:label "Volume ID" [:input {:aria-label "Sculpted volume ID" :value (picker/label (:volume-id draft))
                                  :placeholder "sculpted-volume" :on-change #(assign! [:volume-id] (value %))}]]
     [launches draft]
     [:div.spatial-actions
      [:button.button {:disabled (empty? (:undo draft)) :on-click #(dispatch! [::undo])} "Undo sculpt"]
      [:button.button {:on-click #(dispatch! [::reset])} "Reset draft"]
      [:button.button {:disabled (boolean pending) :on-click #(rf/dispatch [::publish :volume])} "Save volume"]
      [:button.button.primary {:disabled (boolean pending) :on-click #(rf/dispatch [::publish :cue])} "Register + launch cue"]]
     (when message [:p.small {:role "status"} message])]))
