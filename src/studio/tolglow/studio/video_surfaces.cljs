(ns tolglow.studio.video-surfaces
  "Video on 3D surfaces (#674): the surface editor under Rig & space and the
  Room projection. Edits are the same `tolglow.video.mapping` operations the
  REPL sends (`show/video-action! runtime :edit op`) through
  /api/video-mapping. The Room draws each surface with
  `tolglow.video.surface/mesh` from the pose the show evaluated, and marks
  the heads the show reports in contact."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.spatial-fields :as fields]
            [tolglow.studio.video-strips :as video-strips]
            [tolglow.video.surface :as surface]))

(def wire-id events/wire-id)

(defn new-surface
  "A surface of the given shape on a source, two metres up at the room
  centre, facing the audience (+z)."
  [id source shape]
  (cond-> {:id id :source source :shape shape :position [0 2 0] :rotation-deg [0 0 0]
           :contact {:distance-m 0.25 :feather-m 0.0 :curve :linear}}
    (#{:plane :wave} shape) (assoc :size-m [4 2.25])
    (= :wave shape) (assoc :wave {:amplitude-m 0.3 :wavelength-m 2.0 :speed 0.25 :axis :u})
    (= :sphere shape) (assoc :radius-m 1.0)))

(defn surface-id
  "The next free :surface-n id."
  [surfaces]
  (first (remove #(contains? surfaces %) (map #(keyword (str "surface-" %)) (iterate inc 1)))))

(defn contact-summary
  "Heads in contact grouped by pixel layout (nil for single heads), with
  counts, for the editor's list."
  [contacts]
  (->> contacts
       (group-by :layout)
       (map (fn [[layout cs]] {:layout layout :count (count cs) :ids (mapv :id cs)}))
       (sort-by (comp str :layout))
       vec))

(defn frame-url
  "The latched frame route for a source (as the route's query reads it) at a
  token that changes when the Room should fetch a newer frame."
  [frame-source token]
  (str "/api/video/frame?source=" (js/encodeURIComponent frame-source) "&width=512&v=" token))

(defn- texture-tokens
  "Throttle each source's frame token to two refreshes a second."
  [previous sources now]
  (reduce (fn [tokens {:keys [id sequence]}]
            (let [{:keys [token at]} (get previous id)]
              (assoc tokens id (if (and (some? token) (or (= token sequence) (< (- now at) 500)))
                                 (get previous id)
                                 {:token sequence :at now}))))
          {} sources))

(defn- js-id [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn room-surfaces
  "Engine data for the Room overlay: one entry per running surface output,
  plus authored surfaces nobody is running when show-authored? (the rig
  editor). positions/uvs/indices come from the shared surface mesh;
  contacts are [x y z weight] for each head the show reports in contact,
  with its output id in the same place of contact-ids. video and rig are
  the typed server projection (keywords intact), not the form presentation."
  [video rig tokens show-authored?]
  (let [positions (into {} (map (juxt :id :position)) (:fixtures rig))
        running (:running (:surfaces video))
        live (set (map :surface running))
        position #(get positions %)
        entry (fn [id view]
                (let [resolved (:resolved view)
                      placed (filterv (comp position :id) (:contacts view))
                      token (get-in tokens [(:source resolved) :token])]
                  (merge (surface/mesh resolved)
                         {:id id
                          ;; A sphere has no open edge to outline.
                          :outline (not= :sphere (:shape resolved))
                          :texture (when (some? token) (frame-url (or (:frame-source view) (str (:source resolved))) token))
                          :contacts (mapv (fn [{:keys [id weight]}] (conj (vec (position id)) weight)) placed)
                          ;; Output values reach the Room keyed like this.
                          :contact-ids (mapv (comp js-id :id) placed)})))]
    (vec (concat (for [view running]
                   (entry (str (:instance-id view) "/" (:output view)) view))
                 (when show-authored?
                   (for [[id view] (sort-by (comp str key) (:authored (:surfaces video)))
                         :when (not (live id))]
                     (entry (str "authored/" (wire-id id)) view)))))))

(defn room-surfaces-cached
  "room-surfaces, reusing the previous value in cache (a volatile owned by
  one Room) while the projection and rig are unchanged, so the Room uploads
  geometry only when it moves."
  [cache video rig show-authored?]
  (let [old @cache
        now (.now js/Date)
        tokens (texture-tokens (:tokens old) (:sources video) now)]
    (if (and old (identical? (:surfaces video) (:surfaces old)) (identical? rig (:rig old))
             (= show-authored? (:show-authored? old)) (= tokens (:tokens old)))
      (:value old)
      (let [value (room-surfaces video rig tokens show-authored?)]
        (vreset! cache {:surfaces (:surfaces video) :rig rig :show-authored? show-authored? :tokens tokens :value value})
        value))))

;; Editor --------------------------------------------------------------------

(rf/reg-sub ::settings (fn [db _] (get-in db [:server-projection :view :settings :video])))
(rf/reg-sub ::status (fn [db _] (get-in db [:server-projection :view :video])))
(rf/reg-sub ::rig (fn [db _] (get-in db [:server-projection :view :rig])))
(rf/reg-sub ::selected (fn [db _] (get-in db [:video-surfaces-ui :selected])))
(rf/reg-event-db ::select (fn [db [_ id]] (assoc-in db [:video-surfaces-ui :selected] id)))

(defn- edit! [op] (rf/dispatch [::video-strips/edit op]))

(defn- put! [surface] (edit! {:op :put-surface :surface surface}))

(defn- number [caption value commit & [options]]
  [fields/number-input caption value commit (merge {:step "any"} options)])

(defn- shape-fields [s]
  (case (:shape s)
    :sphere [:div.form-row [number "Radius · m" (:radius-m s) #(put! (assoc s :radius-m %)) {:min 0.01}]]
    [:div.form-row
     [number "Width · m" (get-in s [:size-m 0]) #(put! (assoc-in s [:size-m 0] %)) {:min 0.01}]
     [number "Height · m" (get-in s [:size-m 1]) #(put! (assoc-in s [:size-m 1] %)) {:min 0.01}]
     (when (= :wave (:shape s))
       [:<>
        [number "Wave amplitude · m" (get-in s [:wave :amplitude-m] 0.25) #(put! (assoc-in s [:wave :amplitude-m] %)) {:min 0}]
        [number "Wavelength · m" (get-in s [:wave :wavelength-m] 2.0) #(put! (assoc-in s [:wave :wavelength-m] %)) {:min 0.01}]
        [number "Travel · cycles per beat" (get-in s [:wave :speed] 0) #(put! (assoc-in s [:wave :speed] %))]
        [:label "Wave runs along"
         [:select {:value (name (get-in s [:wave :axis] :u))
                   :on-change #(put! (assoc-in s [:wave :axis] (keyword (.. % -target -value))))}
          [:option {:value "u"} "Width (u)"] [:option {:value "v"} "Height (v)"]]]])]))

(defn- crop-fields [s]
  (let [crop (merge surface/default-crop (:crop s))
        setter (fn [k] #(put! (assoc s :crop (assoc crop k %))))]
    [:fieldset.video-surface-crop [:legend "Source crop · fraction of the frame"]
     [number "Left" (:x crop) (setter :x)] [number "Top" (:y crop) (setter :y)]
     [number "Width" (:width crop) (setter :width) {:min 0.01}] [number "Height" (:height crop) (setter :height) {:min 0.01}]]))

(defn- contact-fields [s]
  (let [contact (merge surface/default-contact (:contact s))
        footprint (merge surface/default-footprint (:footprint s))]
    [:div.form-row
     [number "Contact distance · m" (:distance-m contact) #(put! (assoc s :contact (assoc contact :distance-m %))) {:min 0}]
     [number "Feather · m" (:feather-m contact) #(put! (assoc s :contact (assoc contact :feather-m %))) {:min 0}]
     [:label "Feather curve"
      [:select {:value (name (:curve contact))
                :on-change #(put! (assoc s :contact (assoc contact :curve (keyword (.. % -target -value)))))}
       [:option {:value "linear"} "Linear"] [:option {:value "smoothstep"} "Smooth"]]]
     [number "Head footprint width" (:width footprint) #(put! (assoc s :footprint (assoc footprint :width %))) {:min 0 :max 1}]
     [number "Sampling height" (:height footprint) #(put! (assoc s :footprint (assoc footprint :height %))) {:min 0 :max 1}]]))

(defn- contacts-list [s view]
  (let [groups (contact-summary (:contacts view))]
    [:div.video-surface-contacts
     [:h4 (str (:count view 0) " heads in contact")
      (when (> (:count view 0) (count (:contacts view))) [:span.small.muted (str " · first " (count (:contacts view)) " listed")])]
     (if (empty? groups)
       [:p.small.muted "No heads within the contact distance. Move the surface or widen the contact distance."]
       [:ul
        (for [{:keys [layout count ids]} groups]
          ^{:key (str layout)}
          [:li (if layout
                 [:<> [:strong (wire-id layout)] (str " · " count " pixels")
                  [number "Source height for this strip" (get-in s [:strips layout :height] (get-in s [:footprint :height] 0))
                   #(put! (assoc-in s [:strips layout :height] %)) {:min 0 :max 1}]]
                 [:<> [:strong "Heads"] (str " · " (str/join ", " (map wire-id (take 12 ids))) (when (> count 12) " …"))])])])]))

(defn- surface-editor [s view]
  [:div.video-surface-editor
   [:h3 (str "Surface " (wire-id (:id s))) [:span.small.muted (str " · " (name (:shape s)) " on " (wire-id (:source s)))]]
   [:div.form-row
    [:label "Shape"
     [:select {:value (name (:shape s))
               :on-change #(let [shape (keyword (.. % -target -value))]
                             ;; Keep placement, crop, contact and a sheet's size across the change.
                             (put! (merge (new-surface (:id s) (:source s) shape)
                                          (select-keys s [:position :rotation-deg :crop :contact :footprint :strips :targets])
                                          (when (and (#{:plane :wave} shape) (:size-m s)) {:size-m (:size-m s)}))))}
      [:option {:value "plane"} "Plane"] [:option {:value "wave"} "Wavy sheet"] [:option {:value "sphere"} "Sphere"]]]]
   [fields/xyz "Position · m" (:position s) #(put! (assoc s :position %))]
   [:fieldset.spatial-xyz [:legend "Rotation · degrees"]
    (for [[caption i] [["Pitch" 0] ["Yaw" 1] ["Roll" 2]]]
      ^{:key i} [number caption (get (:rotation-deg s [0 0 0]) i) #(put! (assoc s :rotation-deg (assoc (:rotation-deg s [0 0 0]) i %)))])]
   [shape-fields s]
   [crop-fields s]
   [contact-fields s]
   [contacts-list s view]
   [:div.form-row
    [:button.button {:type "button" :on-click #(edit! {:op :surface-cue :surface (:id s)})} "Make a cue and launch it"]
    [:button.text-button.danger {:type "button" :on-click #(edit! {:op :delete-surface :id (:id s)})} "Remove surface"]]])

(defn panel []
  (let [settings @(rf/subscribe [::settings]) status @(rf/subscribe [::status])
        selected @(rf/subscribe [::selected])
        surfaces (:surfaces settings)
        current (or (get surfaces selected) (second (first (sort-by (comp str key) surfaces))))
        source (first (sort-by str (keys (:sources settings))))]
    [:section#video-surfaces.panel {:aria-label "Video surfaces"}
     [:div.panel-top [:span.section-label "VIDEO SURFACES"]
      [:span.small.muted "Project a source onto a plane, wavy sheet or sphere in the room"]]
     [:div.form-body
      [:div.form-row
       (for [[id s] (sort-by (comp str key) surfaces)]
         ^{:key (str id)}
         [:button.text-button {:type "button" :aria-pressed (= id (:id current)) :on-click #(rf/dispatch [::select id])}
          (str (wire-id id) " · " (name (:shape s)) " · " (get-in status [:surfaces :authored id :count] 0) " in contact")])]
      (if source
        [:div.form-row
         (for [[shape caption] [[:plane "Add plane"] [:wave "Add wavy sheet"] [:sphere "Add sphere"]]]
           ^{:key (name shape)}
           [:button.text-button {:type "button"
                                 :on-click #(let [id (surface-id surfaces)]
                                              (put! (new-surface id source shape))
                                              (rf/dispatch [::select id]))}
            caption])]
        [:p.small.muted "Add a video source above first."])
      (when current [surface-editor current (get-in status [:surfaces :authored (:id current)])])]]))
