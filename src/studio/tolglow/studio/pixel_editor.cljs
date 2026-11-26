(ns tolglow.studio.pixel-editor
  "Pixel layout authoring. Positions, addressing and validation come from the shared rig model."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.studio.checkbox :as checkbox]
            [tolglow.studio.rig-fields :as fields]))

(defn layout-index [rig id]
  (first (keep-indexed (fn [i spec] (when (= id (:id spec)) i)) (:pixel-layouts rig))))

(defn selected [rig state]
  (get (:pixel-layouts rig) (layout-index rig (:pixel-id state))))

(defn- used-ids [rig]
  (set (map :id (concat (:pixel-layouts rig) (:fixtures rig)))))

(defn- next-id [rig shape]
  (let [ids (used-ids rig)]
    (first (remove ids (map #(str "pixel-" (name shape) "-" %) (iterate inc 1))))))

(defn- fixture-end [{:keys [universe address footprint channels]}]
  (when universe
    (let [width (or footprint (reduce max 1 (filter int? (flatten (vals channels)))))]
      (+ universe (quot (+ (or address 1) width -2) 512)))))

(defn- next-universe [rig]
  (let [layout-end (fn [spec]
                     (try (first (:end (layout/patch-range spec)))
                          (catch :default _ (:universe spec))))]
    (inc (reduce max 0 (keep identity (concat (map fixture-end (remove :pixel-layout-id (:fixtures rig)))
                                              (map layout-end (:pixel-layouts rig))))))))

(defn- new-layout [rig shape id]
  (merge {:id id :shape shape :color-layout :rgb
          :universe (next-universe rig) :address 1}
         (case shape
           :path {:points [] :pixels 32}
           :grid {:origin [0 1 0] :rows 4 :columns 8 :row-step [0 0.2 0]
                  :column-step [0.2 0 0] :serpentine? true}
           :helix {:origin [0 0 0] :radius 1 :height 2 :turns 2 :phase-deg 0 :pixels 64})))

(defn- remove-at [xs i] (into (subvec xs 0 i) (subvec xs (inc i))))

(defn activate [state id]
  (assoc state :tool "pixels" :pixel-id id :pixel-mode :shape :pixel-index 0 :anchor-index 0))

(defn- move-anchor [spec index position]
  (if (= :path (keyword (:shape spec :path)))
    (assoc-in spec [:points index] position)
    (assoc spec :origin position)))

(defn edit
  "Return the rig draft and selection after a UI edit. The caller checkpoints rig changes."
  [rig state [action & args]]
  (let [id (:pixel-id state)
        i (layout-index rig id)
        path [:pixel-layouts i]
        spec (get-in rig path)
        [k v] args]
    (case action
      :new (let [id (or (not-empty (str/trim (:new-pixel-id state ""))) (next-id rig k))]
             (if ((used-ids rig) id)
               [rig (assoc state :pixel-error "Choose a unique layout ID.")]
               [(update rig :pixel-layouts (fnil conj []) (new-layout rig k id))
                (-> state (dissoc :pixel-error :new-pixel-id) (activate id))]))
      :new-id [rig (-> state (assoc :new-pixel-id k) (dissoc :pixel-error))]
      :open [rig (activate state k)]
      :mode [rig (assoc state :pixel-mode k)]
      :select-pixel [rig (assoc state :pixel-index k)]
      :select-anchor [rig (assoc state :anchor-index k)]
      (if-not spec [rig state]
        (case action
          :set [(assoc-in rig (into path k) v) state]
          :group [(if (str/blank? k) (update-in rig path dissoc :group) (assoc-in rig (conj path :group) k)) state]
          :remove [(update rig :pixel-layouts remove-at i) (activate state nil)]
          :append (if (< (count (:points spec)) 256)
                    [(update-in rig (conj path :points) conj k)
                     (assoc state :anchor-index (count (:points spec)))]
                    [rig state])
          :remove-anchor [(update-in rig (conj path :points) remove-at k) (assoc state :anchor-index 0)]
          :anchor [(update-in rig path move-anchor k v) (assoc state :anchor-index k)]
          :pixel [(assoc-in rig (into path [:position-overrides k]) v) (assoc state :pixel-index k)]
          :reset-pixel [(update-in rig (conj path :position-overrides) dissoc k) state]
          [rig state])))))

(def preview-limit 2048)

(defn- preview-indices [n limit]
  (if (<= n limit) (vec (range n))
    (mapv #(quot (* % (dec n)) (dec limit)) (range limit))))

(defn point-at [spec index]
  (try (first (layout/positions spec [index]))
       (catch :default _ nil)))

(defn geometry [spec index]
  (when spec
    (try (let [n (layout/pixel-count spec)
               indices (preview-indices n preview-limit)
               selected (max 0 (min (or index 0) (dec n)))]
           {:pixel-count n :indices indices :samples (layout/positions spec indices)
            :selected-index selected :selected-position (point-at spec selected)
            :patch (layout/patch-range spec)})
         (catch :default error {:error (ex-message error)}))))

(defn- anchor-points [spec]
  (if (= :path (keyword (:shape spec :path))) (:points spec) (when spec [(:origin spec)])))

(defn projection [state spec {:keys [samples indices selected-index selected-position]}]
  (let [pixels? (= :pixel (:pixel-mode state))
        anchors (anchor-points spec)
        selected (:anchor-index state)
        sampled (when (> (count anchors) 256)
                  (->> (conj (preview-indices (count anchors) 255) selected)
                       (filter some?) distinct sort vec))]
    {:tool "pixels" :layout-id (:id spec)
     :pixel-mode (if pixels? "pixel" "shape")
     :append? (and spec (not pixels?) (= :path (keyword (:shape spec :path))))
     :points (if sampled (mapv anchors sampled) anchors) :point-indices sampled
     :selected-point (if sampled (first (keep-indexed #(when (= %2 selected) %1) sampled)) selected)
     :pixel-positions samples :pixel-indices indices
     :selected-pixel selected-index :pixel-position selected-position
     :closed? (:closed? spec)}))

(defn begin-action [spec state selection point]
  (case (:kind selection)
    "pixels" [:select-pixel (:index selection)]
    "points" [:select-anchor (:index selection)]
    (when (and spec (= :shape (:pixel-mode state)) (= :path (keyword (:shape spec :path))) (< (count (:points spec)) 256))
      [:append point])))

(defn move-action [state point]
  (if (= :pixel (:pixel-mode state))
    [:pixel (:pixel-index state) point]
    [:anchor (:anchor-index state) point]))

(defn- emit! [& command] (rf/dispatch-sync [:rig-editor/pixels command]))

(defn- input-value [e] (.. e -target -value))

(defn- numeric [label value path & [options]]
  [fields/number-field label value [:rig-editor/pixel-value path] options])

(defn- vector-fields [label values path]
  [:div.rig-vector-fields
   (for [[axis letter] (map-indexed vector ["X" "Y" "Z"])]
     ^{:key letter} [numeric (str label " " letter " · m") (get values axis) (conj path axis)])])

(defn- path-fields [spec state]
  (let [points (:points spec)
        index (:anchor-index state 0)
        anchor (get points index)]
    [:<>
     [:div.rig-editor-actions
      [:button.button {:disabled (>= (count points) 256)
                       :on-click #(emit! :append (if-let [last-point (peek points)] (update last-point 0 inc) [0 0 0]))}
       "Add anchor"]
      [:button.button {:disabled (nil? anchor) :on-click #(emit! :remove-anchor index)} "Remove anchor"]]
     (when (seq points)
       [:<>
        (if (> (count points) 256)
          [fields/number-field "Path anchor" (inc index) [:rig-editor/select-anchor] {:min 1 :max (count points) :step 1}]
          [:label "Path anchor"
           [:select {:aria-label "Path anchor" :value index :on-change #(emit! :select-anchor (parse-long (input-value %)))}
          (for [i (range (count points))] ^{:key i} [:option {:value i} (str "Anchor " (inc i))])]])
        (when anchor [vector-fields "Anchor" anchor [:points index]])])
     [:label.rig-checkbox [checkbox/input {:checked (boolean (:closed? spec))
                                           :on-change #(emit! :set [:closed?] (.. % -target -checked))}] "Close the path"]]))

(defn- shape-fields [spec state]
  (let [shape (keyword (:shape spec :path))]
    [:<>
     (when (not= shape :path) [vector-fields "Origin" (:origin spec) [:origin]])
     (case shape
       :path [path-fields spec state]
       :grid [:<> [:div.rig-form-grid
                   [numeric "Rows" (:rows spec) [:rows] {:min 1 :step 1}]
                   [numeric "Columns" (:columns spec) [:columns] {:min 1 :step 1}]]
              [vector-fields "Row step" (:row-step spec) [:row-step]]
              [vector-fields "Column step" (:column-step spec) [:column-step]]
              [:label.rig-checkbox [checkbox/input {:checked (:serpentine? spec true)
                                                    :on-change #(emit! :set [:serpentine?] (.. % -target -checked))}]
               "Alternate row direction"]]
       :helix [:div.rig-form-grid
               (for [[k label] [[:radius "Radius · m"] [:height "Height · m"] [:turns "Turns"] [:phase-deg "Phase · °"]]]
                 ^{:key k} [numeric label (k spec 0) [k]])]
       nil)]))

(defn- override-fields [spec {:keys [pixel-count selected-index selected-position]}]
  (let [i (or selected-index 0)
        point selected-position]
    [:<>
     [fields/number-field "Pixel number" (inc i) [:rig-editor/select-pixel] {:min 1 :max pixel-count :step 1}]
     (when point
       [:<>
        [:div.rig-vector-fields {:role "group" :aria-label (str "Coordinates for pixel " (inc i))}
         (for [[axis letter] (map-indexed vector ["X" "Y" "Z"])]
           ^{:key letter} [fields/number-field (str "Pixel " letter " · m") (get point axis)
                          [:rig-editor/pixel-coordinate (:id spec) i axis]])]
        [:button.button {:disabled (not (contains? (:position-overrides spec) i))
                         :on-click #(emit! :reset-pixel i)} "Reset pixel to shape"]])]))

(defn- address [[universe slot]] (str "U" universe " / " slot))

(defn panel [rig state spec {:keys [pixel-count patch error] :as geometry}]
  [:section.rig-path-form {:aria-label "Pixel layout editor"}
   [:div.panel-top [:span.section-label "PIXEL LAYOUTS"]]
   [:label "Layout"
    [:select {:aria-label "Pixel layout" :value (or (:id spec) "") :on-change #(emit! :open (input-value %))}
     [:option {:value ""} "Choose a layout"]
     (for [{:keys [id]} (:pixel-layouts rig)] ^{:key id} [:option {:value id} id])]]
   [:label "New layout ID" [:input {:value (:new-pixel-id state "") :placeholder "Generate automatically"
                                   :on-change #(emit! :new-id (input-value %))}]]
   (when-let [error (:pixel-error state)] [:p.rig-editor-message {:role "alert"} error])
   [:div.rig-editor-actions
    (for [[shape label] [[:path "New path"] [:grid "New grid"] [:helix "New helix"]]]
      ^{:key shape} [:button.button {:on-click #(emit! :new shape)} label])]
   (when spec
     [:<>
      [:div.rig-form-grid
       [:label "Fixture group" [:input {:value (:group spec "") :on-change #(emit! :group (input-value %))}]]
       (when (not= :grid (keyword (:shape spec :path)))
         [numeric "Pixels" (:pixels spec) [:pixels] {:min 1 :step 1}])
       [:label "Channel order"
        [:select {:aria-label "Channel order" :value (name (:color-layout spec :rgbw))
                  :on-change #(emit! :set [:color-layout] (keyword (input-value %)))}
         (for [order (sort (keys layout/color-layouts))]
           ^{:key order} [:option {:value (name order)} (str/upper-case (name order))])]]
       [numeric "Start universe" (:universe spec 1) [:universe] {:min 0 :step 1}]
       [numeric "Start address" (:address spec 1) [:address] {:min 1 :max 512 :step 1}]]
      (if patch
        [:p.small {:role "status"} (str pixel-count " pixels · " (address (:start patch)) " → " (address (:end patch))
                                       (when (> pixel-count preview-limit) (str " · " preview-limit " preview dots")))]
        [:p.rig-editor-message {:role "status"} error])
      [:div.rig-editor-actions
       (for [[mode label] [[:shape "Shape handles"] [:pixel "Individual pixels"]]]
         ^{:key mode} [:button.button {:aria-pressed (= mode (:pixel-mode state)) :on-click #(emit! :mode mode)} label])]
      (if (= :pixel (:pixel-mode state))
        [:<> [:p.small.muted "Tap a pixel and drag it in the editing plane, or enter its number and coordinates."]
         [override-fields spec geometry]]
        [:<> [:p.small.muted (if (= :path (keyword (:shape spec :path)))
                              "Tap the plane to add an anchor. Drag an anchor to reshape the strip."
                              "Drag the origin to place the layout. Adjust its dimensions below.")]
         [shape-fields spec state]])
      [:div.rig-editor-actions
       [:button.button.primary {:disabled (or error (nil? patch)) :on-click #(rf/dispatch [:studio/rig-save])} "Save rig layout"]
       [:button.button {:on-click #(emit! :remove)} "Remove layout"]]])])
