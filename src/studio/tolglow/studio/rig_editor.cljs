(ns tolglow.studio.rig-editor
  "Re-frame authoring drafts; the shared scene owns only picking and drawing."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.checkbox :as checkbox]
            [tolglow.studio.interop :as interop]
            [tolglow.studio.pixel-editor :as pixels]
            [tolglow.rig.placement :as placement]
            [tolglow.studio.rig-placement :as rp]
            [tolglow.studio.rig-fields :as fields]
            [tolglow.studio.camera-controls :as camera]
            [tolglow.studio.spatial-preview :as spatial-preview]
            [tolglow.studio.volume-sculpt :as sculpt]))

(defn identity-key [db]
  (js/JSON.stringify (clj->js (mapv #(get-in db [:show %]) [:runtime-id :show-id :epoch]))))

(def defaults {:tool "navigate" :plane "xz" :plane-value 0 :snap 0.1 :points [] :targets [] :undo []
               :aim-mode "together" :along "u" :surface-id "floor" :aim-point [0 0 0]
               :place-clearance 0.25 :calibration {:samples []}
               :path {:id "drawn-path" :name "Drawn path" :duration-value 4 :duration-unit "bars" :mode "loop" :curve "linear" :closed? false
                      :sync-arrival? false}})

(def path-types
  "Editor types Studio lists and reopens. Synchronized paths share the host operation."
  #{"aim-path" "synchronized-aim-path"})

(defn path-effects [db]
  (filter #(contains? path-types (some-> (get-in % [:editor :type]) name)) (get-in db [:show :effects])))

(defn- own-calibration
  "Calibration observations, checks and fits belong to one fixture. Selecting
  another drops them, including requests still in flight; the target point stays."
  [state]
  (let [selected (when (= "fixtures" (get-in state [:selection :kind])) (rp/wire (get-in state [:selection :id])))
        calibration (:calibration state)]
    (if (= selected (:fixture-id calibration)) state
      (assoc state :calibration (cond-> {:fixture-id selected :samples []}
                                  (:target calibration) (assoc :target (:target calibration)))))))

(defn editor [db]
  (let [state (get-in db [:ui :rig-editor])]
    (own-calibration (if (= (:identity state) (identity-key db)) state (assoc defaults :identity (identity-key db))))))

(defn put-editor [db state] (assoc-in db [:ui :rig-editor] state))

(defn rebase-editor [db]
  (if-let [previous (get-in db [:ui :rig-editor :identity])]
    (if (= previous (identity-key db)) db
      (-> db (assoc-in [:ui :rig-draft] (get-in db [:show :rig])) (assoc-in [:ui :rig-dirty?] false)
          (put-editor (assoc defaults :identity (identity-key db) :message "Show changed; editing follows the new rig.")))) db))
(rf/reg-global-interceptor
  (rf/->interceptor :id :rig-editor/show-identity
    :after (fn [context] (if (get-in context [:effects :db]) (update-in context [:effects :db] rebase-editor) context))))

(defn finite-point? [point] (and (vector? point) (= 3 (count point)) (every? #(and (number? %) (js/Number.isFinite %) (<= (js/Math.abs %) 10000)) point)))

(defn item-index [db selection]
  (first (keep-indexed #(when (= (str (:id %2)) (str (:id selection))) %1)
                      (get-in db [:ui :rig-draft (keyword (:kind selection))]))))

(def selection-keys [:points :selected-point :pixel-id :pixel-mode :pixel-index :anchor-index])

(defn checkpoint [db state]
  (assoc (select-keys state selection-keys) :rig (get-in db [:ui :rig-draft])))

(defn remember [db state]
  (update state :undo #(vec (take-last 30 (conj (or % []) (checkpoint db state))))))

(defn restore-checkpoint [db state saved]
  (-> db (assoc-in [:ui :rig-draft] (:rig saved))
      (assoc-in [:ui :rig-dirty?] (not= (:rig saved) (get-in db [:show :rig])))
      (put-editor (merge (apply dissoc state selection-keys) (select-keys saved selection-keys)))))

(defn set-pose [db selection field value]
  (if-let [index (item-index db selection)]
    (if (and (#{:position :rotation-deg} field) (finite-point? value))
      (-> db (assoc-in [:ui :rig-draft (keyword (:kind selection)) index field] value)
          (assoc-in [:ui :rig-dirty?] true)) db) db))

(defn selected-fixture [db state]
  (when (= "fixtures" (get-in state [:selection :kind]))
    (when-let [index (item-index db (:selection state))]
      (get-in db [:ui :rig-draft :fixtures index]))))

(defn edit-selected
  "Apply f to the selected draft fixture as one undoable edit."
  [db f]
  (let [state (editor db) index (when (= "fixtures" (get-in state [:selection :kind])) (item-index db (:selection state)))]
    (if-let [fixture (when index (get-in db [:ui :rig-draft :fixtures index]))]
      (let [changed (f fixture)]
        (if (= changed fixture) db
          (-> db (put-editor (remember db state))
              (assoc-in [:ui :rig-draft :fixtures index] changed)
              (assoc-in [:ui :rig-dirty?] true))))
      db)))

(defn place-fixture
  "Place the chosen library type at a picked mount as one undoable draft edit."
  [db hit]
  (let [state (editor db)
        result (rp/place (get-in db [:ui :rig-draft]) (rp/find-type db (:place-type state)) hit (:place-clearance state 0.25)
                   (map :id (get-in db [:show :rig :fixtures])))]
    (if-let [error (:error result)]
      (put-editor db (assoc state :message error))
      (let [fixture (:fixture result)]
        (-> db (assoc-in [:ui :rig-draft] (:rig result)) (assoc-in [:ui :rig-dirty?] true)
            (put-editor (assoc (remember db state)
                               :selection {:kind "fixtures" :id (:id fixture)}
                               :message (str "Placed " (:id fixture) " on the " (rp/surface-label hit)
                                             " at universe " (:universe fixture) ", address " (:address fixture) ". "
                                             (if-let [axes (rp/missing-calibration fixture)]
                                               (str "Enter its " (str/join " and " axes) " range, then save the rig.")
                                               "Save the rig to patch it.")))))))))

(defn- pixel-edit [db state command checkpoint?]
  (let [rig (get-in db [:ui :rig-draft])
        [next-rig next-state] (pixels/edit rig state command)]
    (if (identical? rig next-rig)
      (put-editor db next-state)
      (-> db
          (assoc-in [:ui :rig-draft] next-rig)
          (assoc-in [:ui :rig-dirty?] true)
          (put-editor (cond-> next-state checkpoint? (assoc :undo (:undo (remember db state)))))))))

(defn- pixel-gesture [db state {:keys [type selection point token]}]
  (let [gesture (:gesture state)
        spec (pixels/selected (get-in db [:ui :rig-draft]) state)]
    (case type
      "begin" (if (or gesture (not (finite-point? point))) db
                (if-let [action (pixels/begin-action spec state selection point)]
                  (let [state (assoc (remember db state) :gesture {:token token :before (checkpoint db state)})]
                    (pixel-edit db state action false))
                  db))
      "move" (if (and gesture (= token (:token gesture)) (finite-point? point))
               (pixel-edit db state (pixels/move-action state point) false) db)
      db)))

(defn interaction [db {:keys [type identity token selection point angle] :as detail}]
  (let [state (editor db) gesture (:gesture state) tool (:tool state)]
    (cond
      (not= identity (identity-key db)) db
      (and (= tool "pixels") (#{"begin" "move"} type)) (pixel-gesture db state detail)
      :else (case type
        "select" (put-editor db (assoc state :selection (select-keys selection [:kind :id])))
        "navigate" (put-editor db (assoc state :tool "navigate"))
        "unreachable" (put-editor db (assoc state :message "This plane is edge-on. Change the camera or drawing plane."))
        "begin" (if (or gesture (not (finite-point? point)) (and (= tool "draw") (nil? selection) (>= (count (:points state)) 256))
                        ;; Aim home turns fixtures only; a truss drag would be an empty undo step.
                        (and (= tool "mount") (not= "fixtures" (:kind selection)))) db
                  (let [before (checkpoint db state) state (remember db state)
                        index (when (= tool "draw") (if (= "points" (:kind selection)) (:index selection) (count (:points state))))
                        state (cond-> (assoc state :gesture {:token token :before before :selection selection :index index})
                                (= tool "draw") (assoc :selected-point index)
                                (= tool "draw") (update :points #(if (< index (count %)) % (conj % point)))
                                (not= tool "draw") (assoc :selection (select-keys selection [:kind :id])))]
                    (put-editor db state)))
        "move" (if (and gesture (= token (:token gesture)) (finite-point? point))
                 (case tool
                   "draw" (put-editor db (assoc-in state [:points (:index gesture)] point))
                   "move" (set-pose db (:selection gesture) :position point)
                   "rotate" (let [index (item-index db (:selection gesture))
                                  base (get-in gesture [:before :rig (keyword (get-in gesture [:selection :kind])) index :rotation-deg] [0 0 0])
                                  axis (get {"xz" 1 "xy" 2 "yz" 0} (:plane state) 1)]
                              (if (js/Number.isFinite angle) (set-pose db (:selection gesture) :rotation-deg (update base axis + angle)) db))
                   "mount" (let [selection (:selection gesture) index (item-index db selection)
                                 base (get-in gesture [:before :rig :fixtures index :rotation-deg] [0 0 0])
                                 position (get-in db [:ui :rig-draft :fixtures index :position])
                                 rotation (when (= "fixtures" (:kind selection)) (placement/face base position point))]
                             (if rotation (set-pose db selection :rotation-deg rotation) db)) db) db)
        "end" (if (= token (:token gesture)) (put-editor db (dissoc state :gesture)) db)
        "cancel" (if (and gesture (= token (:token gesture)))
                   (restore-checkpoint db (-> state (dissoc :gesture) (update :undo pop)) (:before gesture)) db)
        db))))
(rf/reg-event-db :rig-editor/open (fn [db _] (put-editor db (editor db))))
(defn surface-command [db mode point surface-id & [targets]]
  (let [state (editor db) targets (or targets (:targets state))
        request (str (random-uuid))
        error (cond
                (get-in db [:ui :rig-dirty?]) "Save or discard your rig edits before aiming."
                (empty? targets) "Choose at least one moving head."
                (and (#{"edge-to-edge" "follow"} mode) (< (count targets) 2)) "Choose the leader first, then its followers."
                (and (not= mode "release") (not (finite-point? point))) "Enter a finite world coordinate.")]
    (cond
      (:aim-request state) {}
      error {:db (put-editor db (assoc state :message error))}
      :else {:db (put-editor db (cond-> (assoc state :aim-request request :message nil)
                                  point (assoc :aim-point point :surface-id surface-id)))
             :studio/http {:path "/api/surface-aim"
                           :body {:mode mode :targets targets :point point :surface-id (not-empty surface-id)
                                  :along (:along state) :runtime-id (get-in db [:show :runtime-id])
                                  :expected-epoch (get-in db [:show :epoch])}
                           :on-success [:rig-editor/aim-result (identity-key db) request true]
                           :on-failure [:rig-editor/aim-result (identity-key db) request false]}})))
(rf/reg-event-fx :rig-editor/interaction
  (fn [{:keys [db]} [_ {:keys [type point surface-id identity] :as detail}]]
    (cond
      (not= identity (identity-key db)) {}
      (#{"sculpt-begin" "sculpt-move" "sculpt-end" "sculpt-cancel"} type) {:dispatch [::sculpt/gesture detail]}
      (= type "throw") {:dispatch [:tolglow.studio.spatial/throw identity point (:velocity detail)]}
      (= type "select-volume") {:dispatch [:tolglow.studio.spatial/select-preview (:volume-id detail)]}
      (= type "surface") (case (:tool (editor db))
                           "aim" (surface-command db (:aim-mode (editor db)) point surface-id)
                           "calibrate" {:db (put-editor db (-> (editor db) (assoc-in [:calibration :target] (mapv #(/ (js/Math.round (* 1000 %)) 1000) point))
                                                               (update :calibration dissoc :check)))}
                           {})
      (= type "place") (if (= "place" (:tool (editor db))) {:db (place-fixture db detail)} {})
      (= type "missed-place") {:db (put-editor db (assoc (editor db) :message "Tap the floor, a wall, the ceiling or a truss inside the room."))}
      (= type "missed-surface") {:db (put-editor db (assoc (editor db) :message "Tap a receiving surface within the room."))}
      :else {:db (interaction db detail)})))
(rf/reg-event-fx :rig-editor/aim
  (fn [{:keys [db]} [_ mode]]
    (let [state (editor db)]
      (surface-command db (or mode (:aim-mode state)) (:aim-point state) (:surface-id state)))))
(rf/reg-event-db :rig-editor/aim-result
  (fn [db [_ identity request success? result]]
    (let [state (editor db)]
      (if (and (= identity (identity-key db)) (= request (:aim-request state)))
        (put-editor db (-> state (dissoc :aim-request)
                           (assoc :message (if success? (case (:mode result)
                                                        "release" "Surface aim released."
                                                        "follow" "Tracking the leader’s beam on this surface."
                                                        "Surface aim applied.") result)))) db))))
(rf/reg-event-db :rig-editor/set
  (fn [db [_ path value]] (put-editor db (assoc-in (editor db) path value))))

(rf/reg-event-db :rig-editor/mount
  (fn [db [_ key]] (edit-selected db #(rp/mount % key))))

(rf/reg-event-db :rig-editor/axis-range
  (fn [db [_ axis bound value]]
    (let [db (edit-selected db #(rp/set-range % axis bound value))]
      (put-editor db (assoc (editor db) :message (some-> (selected-fixture db (editor db)) rp/range-error))))))

(rf/reg-event-db :rig-editor/reverse-axis
  (fn [db [_ axis on?]] (edit-selected db #(rp/reverse-axis % axis on?))))

(rf/reg-event-db :rig-editor/calibration-target
  (fn [db [_ axis value]]
    (let [state (editor db) target (or (get-in state [:calibration :target]) [0 0 0])]
      (put-editor db (-> state (assoc-in [:calibration :target] (assoc target axis value)) (update :calibration dissoc :check))))))

(defn calibration-request
  "Read-only aim check or fit for the selected head, with its draft mounting
  and travel as overrides of the committed fixture."
  [db operation request extra]
  (let [state (editor db) fixture (selected-fixture db state)]
    {:path (str "/api/fixtures/" operation)
     :body (merge {:fixture-id (rp/wire (:id fixture))}
                  (select-keys fixture [:position :rotation-deg :calibration]) extra)
     :on-success [:rig-editor/calibration-result (identity-key db) request true]
     :on-failure [:rig-editor/calibration-result (identity-key db) request false]}))

(rf/reg-event-fx :rig-editor/aim-check
  (fn [{:keys [db]} _]
    (let [state (editor db) target (get-in state [:calibration :target]) request (str (random-uuid))]
      (if (and (selected-fixture db state) (finite-point? target))
        {:db (put-editor db (-> state (assoc-in [:calibration :checking] request) (assoc :message nil)))
         :studio/http (calibration-request db "aim-check" request {:point target})}
        {:db (put-editor db (assoc state :message "Select a moving head and enter a finite target."))}))))

(rf/reg-event-fx :rig-editor/aim-fit
  (fn [{:keys [db]} _]
    (let [state (editor db) {:keys [samples verified?]} (:calibration state) request (str (random-uuid))]
      (if (and (selected-fixture db state) (<= 3 (count samples)))
        {:db (put-editor db (-> state (assoc-in [:calibration :fitting] request) (assoc :message nil)))
         :studio/http (calibration-request db "aim-fit" request {:samples samples :axis-verified? (true? verified?)})}
        {:db (put-editor db (assoc state :message "Record at least three observations first."))}))))

(rf/reg-event-db :rig-editor/calibration-result
  (fn [db [_ identity request success? result]]
    (let [state (editor db) {:keys [checking fitting]} (:calibration state)
          kind (cond (= request checking) :check (= request fitting) :fit)]
      (if (and kind (= identity (identity-key db)))
        (put-editor db (cond-> (update state :calibration dissoc :checking :fitting)
                         success? (assoc-in [:calibration kind] result)
                         (not success?) (assoc :message result)))
        db))))

(rf/reg-event-db :rig-editor/record-sample
  (fn [db _]
    (let [state (editor db) fixture (selected-fixture db state) target (get-in state [:calibration :target])]
      (cond
        (not (and fixture (finite-point? target))) (put-editor db (assoc state :message "Select a moving head and set its target first."))
        (>= (count (get-in state [:calibration :samples])) 64) (put-editor db (assoc state :message "Use at most 64 observations."))
        :else
        (let [observed (rp/output-sample db (:id fixture))]
          (put-editor db (-> state
                             (update-in [:calibration :samples] (fnil conj []) (merge {:target target :pan 0.5 :tilt 0.5} observed))
                             (update :calibration dissoc :fit)
                             (assoc :message (if observed "Recorded the head's current pan and tilt."
                                               "This head has no pan/tilt output yet; enter the DMX values you jogged it to.")))))))))

(rf/reg-event-db :rig-editor/sample-value
  ;; The field holds the DMX the channel sends; observations keep model values.
  (fn [db [_ index axis dmx]]
    (let [state (editor db) channel (get-in (selected-fixture db state) [:channels axis])]
      (if (and (get-in state [:calibration :samples index]) (<= 0 dmx 255))
        (put-editor db (-> state (assoc-in [:calibration :samples index axis] (placement/dmx-model channel dmx))
                           (update :calibration dissoc :fit))) db))))

(rf/reg-event-db :rig-editor/delete-sample
  (fn [db [_ index]]
    (let [state (editor db) samples (get-in state [:calibration :samples])]
      (if (get samples index)
        (put-editor db (-> state (assoc-in [:calibration :samples] (into (subvec samples 0 index) (subvec samples (inc index))))
                           (update :calibration dissoc :fit))) db))))

(rf/reg-event-db :rig-editor/apply-fit
  (fn [db _]
    (let [state (editor db) fit (get-in state [:calibration :fit]) rotation (:rotation-deg fit)
          fixture (selected-fixture db state)]
      (if (and (finite-point? rotation) fixture (= (rp/wire (:fixture-id fit)) (rp/wire (:id fixture))))
        (let [db (edit-selected db #(assoc % :rotation-deg rotation))]
          (put-editor db (assoc (editor db) :message "Fitted rotation applied to the draft. Save the rig to use it."))) db))))

(rf/reg-event-fx :rig-editor/test-aim
  (fn [{:keys [db]} _]
    (let [state (editor db) fixture (selected-fixture db state)]
      (if fixture
        (surface-command db "together" (get-in state [:calibration :target]) "" [(:id fixture)])
        {:db (put-editor db (assoc state :message "Select a moving head first."))}))))
(defn set-sync-arrival
  "Toggle synchronized arrival. An ordinary path needs a positive duration, so
  leaving sync with a zero minimum restores the default musical length."
  [path on?]
  (cond-> (assoc path :sync-arrival? on?)
    (and (not on?) (not (pos? (:duration-value path))))
    (merge (select-keys (:path defaults) [:duration-value :duration-unit]))))

(rf/reg-event-db :rig-editor/sync-arrival
  (fn [db [_ on?]] (put-editor db (update (editor db) :path set-sync-arrival on?))))
(rf/reg-event-db :rig-editor/pixels
  (fn [db [_ command]] (pixel-edit db (editor db) command true)))
(rf/reg-event-db :rig-editor/pixel-value
  (fn [db [_ path value]] (pixel-edit db (editor db) [:set path value] true)))
(rf/reg-event-db :rig-editor/select-anchor
  (fn [db [_ n]]
    (let [state (editor db)
          spec (pixels/selected (get-in db [:ui :rig-draft]) state)
          index (dec n)]
      (if (and (int? index) (get (:points spec) index))
        (pixel-edit db state [:select-anchor index] false) db))))
(rf/reg-event-db :rig-editor/select-pixel
  (fn [db [_ n]]
    (let [state (editor db)
          spec (pixels/selected (get-in db [:ui :rig-draft]) state)
          index (dec n)]
      (if (and (int? index) (<= 0 index) (pixels/point-at spec index))
        (pixel-edit db state [:select-pixel index] false) db))))
(rf/reg-event-db :rig-editor/pixel-coordinate
  (fn [db [_ id index axis value]]
    (let [state (editor db)
          spec (pixels/selected (get-in db [:ui :rig-draft]) state)
          position (pixels/point-at spec index)]
      (if (and (= id (:id spec)) position (finite-point? (assoc position axis value)))
        (pixel-edit db state [:pixel index (assoc position axis value)] true) db))))
(rf/reg-event-db :rig-editor/pose
  (fn [db [_ field axis value]]
    (let [state (editor db) selection (:selection state) index (item-index db selection)
          current (get-in db [:ui :rig-draft (keyword (:kind selection)) index field] [0 0 0])]
      (if (and index (js/Number.isFinite value))
        (set-pose (put-editor db (remember db state)) selection field (assoc current axis value)) db))))
(rf/reg-event-db :rig-editor/nudge
  (fn [db [_ axis amount]]
    (let [state (editor db) selection (:selection state) index (item-index db selection)
          point (get-in db [:ui :rig-draft (keyword (:kind selection)) index :position])]
      (if point (set-pose (put-editor db (remember db state)) selection :position (update point axis + amount)) db))))
(rf/reg-event-db :rig-editor/point
  (fn [db [_ index axis value]]
    (let [state (editor db) point (get-in state [:points index])]
      (if (and point (js/Number.isFinite value))
        (put-editor db (assoc-in (remember db state) [:points index] (assoc point axis value))) db))))
(rf/reg-event-db :rig-editor/add-point
  (fn [db _] (let [state (editor db)]
               (if (< (count (:points state)) 256)
                 (let [point (or (peek (:points state)) [0 0 0]) index (count (:points state))]
                   (put-editor db (-> (remember db state) (update :points conj (update point 0 + 1)) (assoc :selected-point index)))) db))))
(rf/reg-event-db :rig-editor/delete-point
  (fn [db [_ index]] (let [state (editor db)]
                      (if (get-in state [:points index])
                        (put-editor db (-> (remember db state) (update :points #(vec (concat (subvec % 0 index) (subvec % (inc index))))) (dissoc :selected-point))) db))))
(rf/reg-event-db :rig-editor/clear
  (fn [db _] (put-editor db (assoc (remember db (editor db)) :points [] :selected-point nil))))
(rf/reg-event-db :rig-editor/undo
  (fn [db _] (let [state (editor db)]
               (if-let [saved (peek (:undo state))]
                 (restore-checkpoint db (-> state (update :undo pop) (dissoc :gesture)) saved) db))))
(rf/reg-event-db :rig-editor/cancel
  (fn [db _] (-> db (assoc-in [:ui :rig-draft] (get-in db [:show :rig])) (assoc-in [:ui :rig-dirty?] false)
                (put-editor (assoc defaults :identity (identity-key db))))))
(rf/reg-event-db :rig-editor/target
  (fn [db [_ id active?]]
    (put-editor db (update (editor db) :targets #(if active? (vec (distinct (conj % id))) (filterv (fn [target] (not= id target)) %))))))
(rf/reg-event-db :rig-editor/load-path
  (fn [db [_ id]]
    (if-let [effect (first (filter #(= id (:id %)) (path-effects db)))]
      (let [source (:editor effect) [unit amount] (first (:duration source))
            cue (first (filter #(= id (:effect-id %)) (get-in db [:show :cues])))]
        (put-editor db (-> (remember db (editor db))
                          (assoc :points (:points source) :targets (:targets effect) :tool "draw" :selected-point nil
                                 :published {:effect-id id :cue-id (:id cue) :version (:version effect)}
                                 :path (merge (select-keys source [:mode :curve :closed?])
                                              {:id id :name (:name effect) :duration-unit (name unit) :duration-value amount
                                               :sync-arrival? (true? (:sync-arrival? source))}))))) db)))
(defn publish-request [db run? request]
  (let [state (editor db) path (:path state)]
    {:path "/api/aim-path"
     :body (merge (select-keys path [:id :name :mode :curve :closed?])
                  ;; Always explicit: an omitted flag would inherit the saved path's choice.
                  {:sync-arrival? (true? (:sync-arrival? path))}
                  (when (:sync-arrival? path) {:mode "once" :curve "linear" :closed? false})
                  {:points (:points state) :targets (:targets state)
                   :duration {(keyword (:duration-unit path)) (:duration-value path)}
                   :page-id (get-in db [:ui :page-id])
                   :expected-epoch (get-in db [:show :epoch]) :runtime-id (get-in db [:show :runtime-id])
                   :expected-version nil}
                  (when (= (:id path) (get-in state [:published :effect-id])) {:expected-version (get-in state [:published :version]) :cue-id (get-in state [:published :cue-id])}))
     :on-success [:rig-editor/published (identity-key db) request
                  (when run? (if (and (= "once" (:mode path)) (not (:sync-arrival? path))) "one-shot" "persistent"))]
     :on-failure [:rig-editor/failed (identity-key db) request]}))
(rf/reg-event-fx :rig-editor/publish
  (fn [{:keys [db]} [_ run?]]
    (let [state (editor db) request (str (random-uuid))]
      (cond
        (get-in db [:ui :rig-dirty?]) {:db (put-editor db (assoc state :message "Save the rig before publishing its aim path."))}
        (:publishing state) {}
        :else {:db (put-editor db (assoc state :publishing request :message "Validating path…"))
               :studio/http (publish-request db run? request)}))))
(defn published-message [result]
  (str (if (:page-full? result) "Cue created. The page is full; find it in the cue library." "Aim path cue created.")
       (when-let [ms (:arrival-duration-ms result)]
         (str " Heads arrive together after " (.toFixed (/ ms 1000) 2) " s at the current tempo."))))

(rf/reg-event-fx :rig-editor/published
  (fn [{:keys [db]} [_ identity request launch-mode result]]
    (let [state (editor db)]
      (when (and (= identity (identity-key db)) (= request (:publishing state)))
        (cond-> {:db (put-editor db (-> state (dissoc :publishing) (assoc :published result :message (published-message result))))}
          launch-mode (assoc :studio/send-command {:op "cue/launch" :fields {:cue-id (:cue-id result) :launch {:mode launch-mode}}
                                            :epoch (get-in db [:show :epoch]) :runtime-id (get-in db [:show :runtime-id]) :session (:session db)
                                            :on-success [:studio/success "Aim path running."]}))))))
(rf/reg-event-db :rig-editor/failed
  (fn [db [_ identity request message]]
    (let [state (editor db)] (if (and (= identity (identity-key db)) (= request (:publishing state)))
                             (put-editor db (-> state (dissoc :publishing) (assoc :message message))) db))))

(defn- dispatch! [event] (rf/dispatch-sync event))

(defn- value [event] (.. event -target -value))

(defn- choice [label selected choices path]
  [:label label [:select {:aria-label label :value selected :on-change #(dispatch! [:rig-editor/set path (value %)])}
                 (for [[id caption] choices] ^{:key id} [:option {:value id} caption])]])

(defn- pose-fields [db state]
  (let [selection (:selection state) index (item-index db selection) item (get-in db [:ui :rig-draft (keyword (:kind selection)) index])]
    [:div.rig-selection
     [:label "Selected object"
      [:select {:aria-label "Selected rig object" :value (if item (str (:kind selection) ":" index) "")
                :on-change #(let [v (value %) [kind i] (.split v ":") selected (get-in db [:ui :rig-draft (keyword kind) (js/Number i)])]
                              (dispatch! [:rig-editor/set [:selection] (when selected {:kind kind :id (:id selected)})]))}
       [:option {:value ""} "Pick a fixture or truss"]
       (for [kind [:fixtures :trusses] [i object] (map-indexed vector (get-in db [:ui :rig-draft kind]))
             :when (nil? (:pixel-layout-id object))]
         ^{:key (str kind (:id object))} [:option {:value (str (name kind) ":" i)} (str (or (:name object) (:id object)) " · " (name kind))])]]
     (when item
       [:<>
        [:div.rig-vector-fields (for [[axis label] [[0 "X · m"] [1 "Y · m"] [2 "Z · m"]]]
                                 ^{:key label} [fields/number-field label (get-in item [:position axis] 0) [:rig-editor/pose :position axis]])]
        [:div.rig-vector-fields (for [[axis label] [[0 "Rotate X · °"] [1 "Rotate Y · °"] [2 "Rotate Z · °"]]]
                                 ^{:key label} [fields/number-field label (get-in item [:rotation-deg axis] 0) [:rig-editor/pose :rotation-deg axis] {:step 1}])]
        [:div.rig-nudge {:role "group" :aria-label "Move selected object"}
         (for [[label axis amount] [["← X" 0 -0.1] ["X →" 0 0.1] ["↓ Y" 1 -0.1] ["Y ↑" 1 0.1] ["− Z" 2 -0.1] ["+ Z" 2 0.1]]]
           ^{:key label} [:button.button {:on-click #(dispatch! [:rig-editor/nudge axis amount])} label])]])]))

(defn- target-fields [db state]
  (let [fixtures (filter #(and (get-in % [:channels :pan]) (get-in % [:channels :tilt])) (get-in db [:ui :rig-draft :fixtures]))]
    [:fieldset.rig-targets [:legend "Moving heads to aim"]
      (if (seq fixtures)
        (for [fixture fixtures] ^{:key (:id fixture)} [:label.rig-checkbox [checkbox/input {:checked (boolean (some #{(:id fixture)} (:targets state)))
                                                                                 :on-change #(dispatch! [:rig-editor/target (:id fixture) (.. % -target -checked)])}] (or (:name fixture) (:id fixture))])
        [:p.small.muted "Patch a fixture with pan and tilt below to add a target."])]))

(defn- aim-fields [db state]
  [:section.rig-path-form
   [:div.panel-top [:span.section-label "SURFACE AIM"]]
   [choice "Aiming mode" (:aim-mode state) [["together" "Aim at one point"] ["edge-to-edge" "Place beams edge to edge"]
                                           ["follow" "Follow the moving leader"]] [:aim-mode]]
   (when (not= "together" (:aim-mode state))
     [:<> [choice "Spread along surface" (:along state) [["u" "Across +U"] ["negative-u" "Across −U"] ["v" "Across +V"] ["negative-v" "Across −V"]] [:along]]
      [:p.small.muted "Select heads in order. The first leads and each following beam meets the edge of the previous one."]])
   [target-fields db state]
   (when (seq (:targets state)) [:p.small (apply str (interpose " → " (:targets state)))])
   [:p.small.muted (if (= "follow" (:aim-mode state))
                     "Tap the receiving surface. The leader keeps following its running movement cues."
                     "Tap a receiving surface to aim the selected heads there. Existing color and dimmer cues keep playing.")]
   [choice "Receiver" (:surface-id state)
    (into (if (= "together" (:aim-mode state)) [["" "World coordinates"]] [])
          (map (fn [{:keys [id]}] [id id])) (get-in db [:show :receiver-surfaces])) [:surface-id]]
   (for [[i label] (map-indexed vector ["X" "Y" "Z"])]
     ^{:key label} [fields/number-field (str "Aim " label " · m") (get (:aim-point state) i) [:rig-editor/set [:aim-point i]]])
   [:div.rig-editor-actions
    [:button.button.primary {:disabled (boolean (:aim-request state)) :on-click #(rf/dispatch [:rig-editor/aim])} "Apply aim"]
    [:button.button {:disabled (boolean (:aim-request state)) :on-click #(rf/dispatch [:rig-editor/aim "release"])} "Release selected"]]
   (when (:message state) [:p.rig-editor-message {:role "status"} (:message state)])])

(defn- path-fields [db state]
  (let [path (:path state)]
    [:section.rig-path-form
     [:div.panel-top [:span.section-label "MOVEMENT PATH"] [:span (str (count (:points state)) " / 256 points")]]
     [:label "Saved paths"
      [:select {:aria-label "Saved aim paths" :value "" :on-change #(dispatch! [:rig-editor/load-path (value %)])}
       [:option {:value ""} "Open a saved path…"]
       (for [effect (path-effects db)]
         ^{:key (:id effect)} [:option {:value (:id effect)} (or (:name effect) (:id effect))])]]
     [:div.rig-form-grid
      [:label "Path ID" [:input {:aria-label "Path ID" :value (:id path) :on-change #(dispatch! [:rig-editor/set [:path :id] (value %)])}]]
      [:label "Name" [:input {:aria-label "Path name" :value (:name path) :on-change #(dispatch! [:rig-editor/set [:path :name] (value %)])}]]
      [fields/number-field (if (:sync-arrival? path) "Minimum duration" "Duration") (:duration-value path)
       [:rig-editor/set [:path :duration-value]] {:min (if (:sync-arrival? path) 0 0.015625) :step 0.25}]
      [choice "Musical unit" (:duration-unit path)
       (cond-> [["beats" "Beats"] ["bars" "Bars"] ["phrases" "Phrases"]]
         (or (:sync-arrival? path) (= "ms" (:duration-unit path))) (conj ["ms" "Milliseconds"])) [:path :duration-unit]]
      (when-not (:sync-arrival? path)
        [:<>
         [choice "Playback" (:mode path) [["loop" "Loop"] ["ping-pong" "Back and forth"] ["once" "Once"]] [:path :mode]]
         [choice "Curve" (:curve path) [["linear" "Linear"] ["smoothstep" "Smooth"] ["sine" "Sine"] ["ease-in" "Ease in"] ["ease-out" "Ease out"]] [:path :curve]]])]
     [:label.rig-checkbox [checkbox/input {:checked (true? (:sync-arrival? path))
                                           :on-change #(dispatch! [:rig-editor/sync-arrival (.. % -target -checked)])}]
      "Arrive together using calibrated motor rates"]
     (if (:sync-arrival? path)
       [:p.small.muted "Two points, played once. Every selected head needs pan and tilt motor rates in its calibration. The slowest head sets the travel time; the duration is a lower bound."]
       [:label.rig-checkbox [checkbox/input {:checked (:closed? path) :on-change #(dispatch! [:rig-editor/set [:path :closed?] (.. % -target -checked)])}] "Close the path"])
     [target-fields db state]
     [:div.rig-editor-actions
      [:button.button {:on-click #(dispatch! [:rig-editor/add-point]) :disabled (>= (count (:points state)) 256)} "Add point"]
      [:button.button {:on-click #(dispatch! [:rig-editor/clear]) :disabled (empty? (:points state))} "Clear path"]]
     [:div.rig-path-points
      (for [[i point] (map-indexed vector (:points state))]
        ^{:key i} [:div.rig-path-point {:class (when (= i (:selected-point state)) "selected")}
                   [:button.text-button {:aria-label (str "Select path point " (inc i)) :on-click #(dispatch! [:rig-editor/set [:selected-point] i])} (str (inc i))]
                   (for [[axis label] [[0 "X"] [1 "Y"] [2 "Z"]]] ^{:key axis} [fields/number-field (str "Point " (inc i) " " label) (nth point axis) [:rig-editor/point i axis]])
                   [:button.icon-button {:aria-label (str "Delete path point " (inc i)) :on-click #(dispatch! [:rig-editor/delete-point i])} "×"]])]
     [:p.small.muted "Click the drawing plane to place points; drag a point to reshape the path. Durations follow the show's musical clock. This cue controls aim; combine it with a light-emitting cue."]
     [:div.rig-editor-actions
      [:button.button.primary {:disabled (or (:publishing state) (< (count (:points state)) 2) (empty? (:targets state))) :on-click #(rf/dispatch [:rig-editor/publish false])} "Create cue"]
      [:button.button {:disabled (or (:publishing state) (< (count (:points state)) 2) (empty? (:targets state))) :on-click #(rf/dispatch [:rig-editor/publish true])} "Create + run"]]
     (when (:message state) [:p.rig-editor-message {:role "status"} (:message state)])]))

(defn- editor-panel [db]
  (r/with-let [cached (volatile! nil)]
    (let [state (editor db)
          rig (get-in db [:ui :rig-draft])
          spec (when (= "pixels" (:tool state)) (pixels/selected rig state))
          index (:pixel-index state)
          geometry (if (and (identical? spec (:spec @cached)) (= index (:index @cached)))
                     (:geometry @cached)
                     (:geometry (vreset! cached {:spec spec :index index :geometry (pixels/geometry spec index)})))
          tool-data (if (= "pixels" (:tool state))
                      (pixels/projection state spec geometry)
                      (assoc (select-keys state [:selection :points :selected-point]) :closed? (get-in state [:path :closed?])
                             :calibration-target (get-in state [:calibration :target])
                             :calibration-points (mapv :target (get-in state [:calibration :samples]))))
          projection (merge (select-keys state [:tool :plane :plane-value :snap]) tool-data)
          show (assoc (or (:server-presentation db) (:show db)) :rig rig :editor projection)]
    [:section.panel.rig-editor {:aria-label "Three dimensional rig and movement editor" :data-tool (:tool state)}
     [:div.panel-top [:span.section-label "Rig editor"] [:span.small.muted (if (get-in db [:ui :rig-dirty?]) "Unsaved rig draft" "Rig saved")]]
     [:div.rig-editor-toolbar
      [:div.rig-editor-tools {:role "group" :aria-label "3D editor tool"}
       (for [[tool label] [["navigate" "Explore"] ["place" "Place fixture"] ["move" "Move"] ["rotate" "Rotate"] ["mount" "Aim home"]
                           ["calibrate" "Calibrate aim"] ["draw" "Draw path"] ["pixels" "Pixel layouts"] ["aim" "Aim at surface"] ["volume-select" "Select volume"] ["sculpt-volume" "Sculpt volume"]]]
         ^{:key tool} [:button.button {:aria-pressed (= tool (:tool state)) :on-click #(dispatch! [:rig-editor/set [:tool] tool])} label])]
      [:div.rig-editor-actions
       [:button.button {:disabled (empty? (:undo state)) :on-click #(dispatch! [:rig-editor/undo])} "Undo"]
       [:button.button {:on-click #(dispatch! [:rig-editor/cancel])} "Discard draft"]]]
     [:div.rig-editor-layout
      [:div.rig-editor-scene
       [spatial-preview/scene (interop/room-input show) (get-in db [:server-projection :view :settings :spatial])]
       [camera/controls]
       [:div.rig-view-controls
        [:label "Camera view" [:select {:aria-label "Rig camera view" :value (get-in db [:ui :preview-settings :camera] "audience")
                                         :on-change #(rf/dispatch [:studio/preview-setting :camera (value %)])}
                                [:option {:value "audience"} "Audience"] [:option {:value "overview"} "Overview"]]]
        [:label "Work light · preview only" [:input {:type "range" :aria-label "Rig preview work light" :min 0 :max 1 :step 0.01
                                                       :value (get-in db [:ui :preview-settings :worklight] 0.12)
                                                       :on-change #(rf/dispatch [:studio/preview-setting :worklight (js/Number (value %))])}]]]
       [:div.rig-plane-controls
        [choice "Editing plane" (:plane state) [["xz" "Floor · XZ"] ["xy" "Front · XY"] ["yz" "Side · YZ"]] [:plane]]
        [fields/number-field "Plane offset · m" (:plane-value state) [:rig-editor/set [:plane-value]]]
        [fields/number-field "Snap · m" (:snap state) [:rig-editor/set [:snap]] {:min 0 :step 0.05}]]
       [:p.small.muted "Move keeps an object's depth in the chosen plane. Rotate turns around the plane normal. Choose Explore to move the camera; Escape cancels a drag."]
       (when (not= "pixels" (:tool state)) [pose-fields db state])]
      (case (:tool state) "aim" [aim-fields db state]
        "place" [:div [rp/place-panel db state] (when-let [fixture (selected-fixture db state)] [rp/mount-panel fixture])]
        "mount" (if-let [fixture (selected-fixture db state)] [rp/mount-panel fixture]
                  [:p.small "Select a fixture, then drag in the scene to point its home beam. Its mount stays the same."])
        "calibrate" [:div [rp/calibrate-panel db state (selected-fixture db state)]
                     (when-let [fixture (selected-fixture db state)] [rp/mount-panel fixture])]
        "pixels" [pixels/panel rig state spec geometry]
        "throw" [:p.small "Drag from the launch point in the selected plane. Release to throw; Escape cancels."]
        "sculpt-volume" [sculpt/panel]
        "volume-select" [:p.small "Tap the centre of a volume to edit it below."]
        "draw" [path-fields db state]
        nil)]])))


(def panel
  (r/create-class
    {:display-name "rig-and-path-editor"
     :component-did-mount (fn [_] (rf/dispatch [:rig-editor/open]))
     :reagent-render editor-panel}))
