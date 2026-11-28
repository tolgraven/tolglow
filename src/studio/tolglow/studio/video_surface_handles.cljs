(ns tolglow.studio.video-surface-handles
  "Room drag handles for video surfaces (#674), as the rig editor's
  \"Move surface\" tool. Each saved surface shows three handles: its centre
  moves it in the editing plane, the handle above it raises or lowers it,
  and the handle beside it turns it about the vertical (yaw). The scene
  engine only picks handles and reports plane points or axis distances
  (js/preview/editor.js); this namespace turns them into a local surface
  draft that the Room draws at once, and commits the finished drag as one
  `{:op :put-surface}` edit: the validated `:video-mapping` message the REPL
  sends, recorded as one undo step."
  (:require [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.spatial-fields :as fields]
            [tolglow.studio.video-strip-tools :as tools]))

(def wire-id events/wire-id)

(def ^:private handle-gap-m 0.35)

(defn- radians [degrees] (* degrees (/ js/Math.PI 180)))

(defn extents
  "Half width and half height of a surface in metres, at scale 1."
  [{:keys [shape size-m radius-m]}]
  (if (= :sphere shape)
    [(or radius-m 1) (or radius-m 1)]
    [(/ (or (first size-m) 1) 2) (/ (or (second size-m) 1) 2)]))

(defn handles
  "The three handles of a surface: :move at its centre, :raise above it
  (a vertical axis drag) and :turn beside it along its yawed width."
  [{:keys [id position rotation-deg] :as surface}]
  (let [[hw hh] (extents surface)
        [x y z] (or position [0 0 0])
        yaw (radians (get rotation-deg 1 0))
        reach (+ hw handle-gap-m)]
    [{:kind "move" :id (wire-id id) :position [x y z]}
     {:kind "raise" :id (wire-id id) :position [x (+ y hh handle-gap-m) z] :axis 1}
     ;; Ry(yaw) · [1 0 0] = [cos yaw, 0, −sin yaw], the surface's u direction.
     {:kind "turn" :id (wire-id id) :position [(+ x (* reach (js/Math.cos yaw))) y (- z (* reach (js/Math.sin yaw)))]}]))

(defn- round-mm [v] (/ (js/Math.round (* 1000 v)) 1000))

(defn- heading
  "Yaw in degrees that points a surface's u direction from center at point."
  [[cx _ cz] [px _ pz]]
  (* (js/Math.atan2 (- cz pz) (- px cx)) (/ 180 js/Math.PI)))

(defn- wrap-degrees [d] (- (mod (+ d 180) 360) 180))

(defn dragged
  "The surface after a handle drag: before is the surface when the drag
  began, start the plane point it began at."
  [before {:keys [kind start point delta]}]
  (case kind
    "move" (if (and start point)
             (assoc before :position (mapv #(round-mm (+ %1 (- %3 %2))) (:position before) start point))
             before)
    "raise" (if (number? delta)
              (assoc-in before [:position 1] (round-mm (+ (get-in before [:position 1]) delta)))
              before)
    "turn" (if (and start point)
             (let [center (:position before)
                   turned (+ (get (:rotation-deg before [0 0 0]) 1) (- (heading center point) (heading center start)))]
               (assoc before :rotation-deg (assoc (vec (:rotation-deg before [0 0 0])) 1 (round-mm (wrap-degrees turned)))))
             before)
    before))

;; State ---------------------------------------------------------------------------

(defn- state [db] (get-in db [:video-surfaces-ui :room] {}))

(defn- put [db s] (assoc-in db [:video-surfaces-ui :room] s))

(defn- settings-surfaces [db] (get-in db [:server-projection :view :settings :video :surfaces]))

(defn drafts
  "Surfaces the Room draws ahead of the show: the one being dragged, and a
  committed drag until the show's settings carry it."
  [db]
  (let [{:keys [gesture committing]} (state db)
        saved (settings-surfaces db)]
    (cond
      gesture {(:id gesture) (:draft gesture (:before gesture))}
      ;; Only the pose is the Room's; other keys come from the saved surface.
      (and committing (let [pose #(select-keys % [:position :rotation-deg])]
                        (not= (pose (:surface committing)) (pose (get saved (:id committing))))))
      {(:id committing) (merge (get saved (:id committing)) (select-keys (:surface committing) [:position :rotation-deg]))}
      :else {})))

(rf/reg-sub ::drafts (fn [db _] (drafts db)))

(rf/reg-sub ::selected (fn [db _] (get-in db [:video-surfaces-ui :selected])))

(defn projection
  "The editor data the scene engine picks from: every saved surface's
  handles, drawn where the drafts put them."
  [db]
  (let [shown (merge (settings-surfaces db) (drafts db))
        selected (get-in db [:video-surfaces-ui :selected])]
    {:surface-handles (vec (mapcat handles (vals (sort-by (comp str key) shown))))
     :selected-surface (some-> selected wire-id)}))

(rf/reg-sub ::projection (fn [db _] (projection db)))

(defn- surface-by-wire
  "The surface a handle names, as the Room draws it: a drag that has not
  been echoed yet is the starting point of the next one."
  [db id]
  (some (fn [[k v]] (when (= id (wire-id k)) [k v])) (merge (settings-surfaces db) (drafts db))))

(defn pose-patch
  "The edit a finished drag sends: only the pose keys it changed, so a size,
  source or contact another writer set meanwhile is kept."
  [before after]
  (let [changes (into {} (for [k [:position :rotation-deg] :when (not= (get before k) (get after k))] [k (get after k)]))]
    (when (seq changes) {:op :patch-surface :id (:id after) :changes changes})))

(defn gesture
  "Apply one scene engine event: surface-begin, surface-move, surface-end
  or surface-cancel. Returns {:db} and, at the end of a drag that moved the
  surface, the edit to send."
  [db {:keys [type token id kind point delta]}]
  (let [{current :gesture :as s} (state db)]
    (case type
      "surface-begin" (if-let [[k surface] (when-not current (surface-by-wire db id))]
                        {:db (-> db (put (-> s (assoc :gesture {:token token :id k :kind kind :before surface :start point})
                                             (dissoc :error)))
                                 (assoc-in [:video-surfaces-ui :selected] k))}
                        {:db db})
      "surface-move" (if (and current (= token (:token current)))
                       {:db (put db (assoc-in s [:gesture :draft]
                                              (dragged (:before current) {:kind (:kind current) :start (:start current)
                                                                          :point point :delta delta})))}
                       {:db db})
      "surface-end" (if (and current (= token (:token current)))
                      (let [draft (:draft current)
                            edit (when draft (pose-patch (:before current) draft))]
                        (cond-> {:db (put db (cond-> (dissoc s :gesture)
                                               edit (assoc :committing {:id (:id current) :surface draft})))}
                          edit (assoc :edit edit)))
                      {:db db})
      "surface-cancel" (if (and current (= token (:token current)))
                         {:db (put db (dissoc s :gesture))}
                         {:db db})
      {:db db})))

(defn- request
  "A Room edit through the same validated route as the editors, tied to this
  show session; the reply settles the draft."
  [db edit token]
  (let [v (get-in db [:server-projection :view])]
    {:path "/api/video-mapping" :typed? true
     :body (assoc edit :expected-epoch (:epoch v) :runtime-id (:runtime-id v))
     :on-success [::settled token nil] :on-failure [::settled token]}))

(rf/reg-event-fx ::gesture
  (fn [{:keys [db]} [_ detail]]
    (if (= (:identity detail) (get-in db [:ui :rig-editor :identity]))
      (let [{:keys [db edit]} (gesture db detail)
            token (str (random-uuid))]
        (cond-> {:db db}
          edit (-> (update :db #(put % (assoc-in (state %) [:committing :token] token)))
                   (assoc :studio/http (request db edit token)))))
      {})))

(rf/reg-event-fx ::patch
  ;; A numeric position or yaw from the panel: one edit per committed field.
  (fn [{:keys [db]} [_ surface changes]]
    (let [token (str (random-uuid))
          edit {:op :patch-surface :id (:id surface) :changes changes}]
      {:db (put db (-> (state db) (assoc :committing {:id (:id surface) :surface (merge surface changes) :token token})
                       (dissoc :error)))
       :studio/http (request db edit token)})))

(rf/reg-event-db ::settled
  ;; The show answered. On a refusal stop drawing the draft and say why; on
  ;; success the draft stays until the state stream carries the surface.
  (fn [db [_ token error]]
    (let [s (state db)]
      (if (= token (get-in s [:committing :token]))
        (put db (cond-> s error (-> (dissoc :committing) (assoc :error (str error)))))
        db))))

(rf/reg-event-fx ::open
  ;; Rig & space → Video surfaces → "Move in the Room".
  (fn [{:keys [db]} [_ id]]
    {:db (assoc-in db [:video-surfaces-ui :selected] id)
     :dispatch-n [[:rig-editor/open] [:rig-editor/set [:tool] "video-surface"]]}))

;; Panel -------------------------------------------------------------------------

(rf/reg-sub ::surfaces (fn [db _] (settings-surfaces db)))

(rf/reg-sub ::history (fn [db _] (get-in db [:server-projection :view :video :history])))

(rf/reg-sub ::error (fn [db _] (:error (state db))))

(defn panel
  "The rig editor's side panel for the Move surface tool."
  []
  (let [surfaces @(rf/subscribe [::surfaces]) shown (merge surfaces @(rf/subscribe [::drafts]))
        selected @(rf/subscribe [::selected])
        current (or (get shown selected) (second (first (sort-by (comp str key) shown))))
        error @(rf/subscribe [::error])
        patch! #(rf/dispatch [::patch current %])]
    [:div.video-surface-handles {:aria-label "Video surface handles"}
     [tools/history-bar @(rf/subscribe [::history])]
     (if (empty? surfaces)
       [:p.small "Add a video surface under Rig & space → Video surfaces first."]
       [:<>
        [:p.small "Drag a surface's centre to move it in the editing plane, the handle above it to raise or lower it, and the handle beside it to turn it. Each drag is one edit; Undo puts it back."]
        [:div.form-row {:role "group" :aria-label "Surface to move"}
         (for [[id s] (sort-by (comp str key) surfaces)]
           ^{:key (str id)}
           [:button.text-button {:type "button" :aria-pressed (= id (:id current))
                                 :on-click #(rf/dispatch [:tolglow.studio.video-surfaces/select id])}
            (str (wire-id id) " · " (name (:shape s)))])]
        (when current
          [:<>
           ;; Each field commits on blur or Enter: one edit per entry.
           [fields/xyz "Surface position · m" (:position current) #(patch! {:position %})]
           [fields/number-input "Surface yaw · degrees" (get (:rotation-deg current [0 0 0]) 1)
            #(patch! {:rotation-deg (assoc (vec (:rotation-deg current [0 0 0])) 1 %)}) {:step "any"}]])])
     (when error [:p.performance-error {:role "alert"} error])]))
