(ns tolglow.studio.rig-placement
  "Place library fixtures in the Room, edit their mounting and calibrate aim.
  Pure draft transitions and Reagent views; `tolglow.studio.rig-editor` owns the
  re-frame events, undo checkpoints and requests. Geometry is the shared
  `tolglow.rig.placement`, the same functions the REPL uses."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.rig.placement :as placement]
            [tolglow.studio.checkbox :as checkbox]
            [tolglow.studio.rig-fields :as fields]))

(defn wire [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn fixture-types [db] (get-in db [:show :fixture-types] []))

(defn find-type [db id] (first (filter #(= (wire id) (wire (:id %))) (fixture-types db))))

(defn moving? [fixture] (every? #(contains? (:channels fixture) %) [:pan :tilt]))

(defn surface-label [{:keys [kind] :as hit}]
  (if (= "truss" kind) (str "truss " (get hit :truss-id)) (str/replace (str (get hit :surface-id "surface")) "-" " ")))

(defn place
  "Add an instance of the chosen type to the draft rig at a picked mount. Its ID
  also avoids `taken-ids` (the committed rig), so a fixture removed in the same
  draft never lends its ID to a placement. Returns {:rig :fixture} or {:error}."
  [rig definition hit clearance taken-ids]
  (if-not definition
    {:error "Choose a published fixture type to place."}
    (if-let [fixture (placement/place definition (:fixtures rig) hit {:clearance-m clearance :taken-ids taken-ids})]
      ;; Keep the draft in Studio's presentation form; the host re-derives type data on save.
      {:rig (update rig :fixtures (fnil conj []) (update fixture :rgbw-policy wire)) :fixture fixture}
      {:error "That spot has no usable mounting direction."})))

(defn missing-calibration
  "Moving axes that still need a documented range before the rig can be saved."
  [fixture]
  (when (moving? fixture)
    (seq (for [axis [:pan :tilt] :let [calibration (get-in fixture [:calibration axis])]
               :when (not (or (placement/axis-range calibration) (placement/unreverse-legacy calibration nil)))]
           (name axis)))))

(defn mount [fixture key]
  (if-let [rotation (placement/remount (:rotation-deg fixture [0 0 0]) (get placement/mounts key))]
    (assoc fixture :rotation-deg rotation) fixture))

(defn set-range
  "Set one end of an axis range, starting from the existing range when present.
  A reversed legacy calibration becomes a range plus channel reversal first, so
  the head keeps sending the same DMX for every angle."
  [fixture axis bound value]
  (let [calibration (get-in fixture [:calibration axis])
        [legacy channel] (placement/unreverse-legacy calibration (get-in fixture [:channels axis]))
        current (or legacy (placement/axis-range calibration)
                    (if (= axis :pan) {:min-deg -270 :max-deg 270} {:min-deg -135 :max-deg 135}))]
    (cond-> (assoc-in fixture [:calibration axis] (assoc current bound value))
      (and legacy (contains? (:channels fixture) axis)) (assoc-in [:channels axis] channel))))

(defn range-error [fixture]
  (some (fn [axis]
          (let [{:keys [min-deg max-deg]} (get-in fixture [:calibration axis])]
            (when (and (map? (get-in fixture [:calibration axis])) (not (< min-deg max-deg)))
              (str (str/capitalize (name axis)) " minimum must be below its maximum."))))
        [:pan :tilt]))

(defn reverse-axis [fixture axis on?]
  (if (contains? (:channels fixture) axis)
    (update-in fixture [:channels axis] placement/reverse-channel on?) fixture))

(defn output-sample
  "The head's current normalized pan/tilt from the committed output, when known."
  [db id]
  (let [values (get-in db [:show :output :values])
        value (or (get values (keyword (wire id))) (get values (wire id)))
        read (fn [axis] (let [v (or (get value axis) (get-in value [:channels axis]))] (when (number? v) v)))]
    (when (and (read :pan) (read :tilt)) {:pan (read :pan) :tilt (read :tilt)})))

(defn- dispatch! [event] (rf/dispatch-sync event))

(defn- value [event] (.. event -target -value))

(defn- round [n digits] (when (number? n) (.toFixed n digits)))

(defn place-panel [db state]
  (let [types (fixture-types db) chosen (:place-type state)]
    [:section.rig-path-form {:aria-label "Place fixtures from the library"}
     [:div.panel-top [:span.section-label "PLACE FROM LIBRARY"] [:span.small.muted (str (count types) " published types")]]
     (if (seq types)
       [:<>
        [:label "Fixture type"
         [:select {:aria-label "Fixture type to place" :value (or chosen "")
                   :on-change #(dispatch! [:rig-editor/set [:place-type] (not-empty (value %))])}
          [:option {:value ""} "Choose a published type…"]
          (for [t types] ^{:key (wire (:id t))}
            [:option {:value (wire (:id t))} (str (or (:name t) (wire (:id t))) (when (moving? t) " · moving head"))])]]
        [fields/number-field "Distance from surface · m" (:place-clearance state) [:rig-editor/set [:place-clearance]] {:min 0 :step 0.05}]
        [:p.small.muted "Tap the floor, a wall, the ceiling or a truss. The fixture stands on floors, hangs below the ceiling and trusses, and mounts flat on walls. It takes the next free address. Undo removes it; Save rig patches it."]]
       [:p.small.muted "Publish a fixture type in the fixture library below, then place it here."])
     (when (:message state) [:p.rig-editor-message {:role "status"} (:message state)])]))

(defn- axis-fields [fixture axis]
  (let [calibration (get-in fixture [:calibration axis])
        legacy (first (placement/unreverse-legacy calibration (get-in fixture [:channels axis])))
        range (or (placement/axis-range calibration) legacy)
        home (some-> range placement/home-position)]
    [:fieldset.rig-axis {:aria-label (str (str/capitalize (name axis)) " calibration")}
     [:legend (str (str/capitalize (name axis)) " travel")]
     [:div.rig-vector-fields
      [fields/number-field (str (str/capitalize (name axis)) " minimum · °") (:min-deg range) [:rig-editor/axis-range axis :min-deg] {:step 1}]
      [fields/number-field (str (str/capitalize (name axis)) " maximum · °") (:max-deg range) [:rig-editor/axis-range axis :max-deg] {:step 1}]]
     (if (placement/switchable-reversal? (get-in fixture [:channels axis]))
       [:label.rig-checkbox [checkbox/input {:checked (placement/reversed? (get-in fixture [:channels axis]))
                                             :on-change #(dispatch! [:rig-editor/reverse-axis axis (.. % -target -checked)])}]
        (str "Reverse " (name axis) " (DMX runs the other way)")]
       [:p.small.muted (str "The fixture type reverses " (name axis) " from DMX " (get-in fixture [:channels axis :inverted-from]) "; that pivot stays.")])
     (when legacy [:p.small.muted "Reversed legacy calibration. Editing the range stores it as this range with Reverse switched, sending the same DMX."])
     [:p.small.muted (cond (nil? range) "Enter the range from the fixture's manual."
                           (<= 0 home 1) (str "Home (0°) is at " (round (* 100 home) 1) " % of travel, DMX " (js/Math.round (* 255 home)) ".")
                           :else "Home (0°) is outside this travel range.")]]))

(defn mount-panel
  "Mounting and axis calibration of the selected fixture."
  [fixture]
  (let [kind (placement/mounting (:rotation-deg fixture [0 0 0]))]
    [:section.rig-path-form {:aria-label "Fixture mounting"}
     [:div.panel-top [:span.section-label "MOUNTING"] [:span.small.muted (name kind)]]
     [:div.rig-editor-actions {:role "group" :aria-label "Mount the selected fixture"}
      (for [[key label] [[:standing "Standing"] [:hanging "Hanging"] [:wall-back "Back wall"]
                         [:wall-left "Left wall"] [:wall-right "Right wall"] [:wall-front "Front wall"]]]
        ^{:key key} [:button.button {:on-click #(dispatch! [:rig-editor/mount key])} label])]
     [:p.small.muted "The orange line is the home beam (pan 0°, tilt 0°); the blue line is the mounting axis. With Aim home, drag a fixture to point its home beam. The rotation fields above give exact angles."]
     (when (moving? fixture) [:<> [axis-fields fixture :pan] [axis-fields fixture :tilt]])
     (when-let [axes (missing-calibration fixture)]
       [:p.fixture-invalid (str "Enter the documented " (str/join " and " axes) " range before saving.")])]))

(defn- sample-row [fixture i {:keys [target pan tilt]}]
  ;; Observations hold model values; the fields show the DMX the channel sends,
  ;; after any reversal, which is what a console shows while jogging.
  [:div.rig-path-point
   [:span.small (str (inc i))]
   [:span.small (str/join ", " (map #(round % 2) target))]
   [fields/number-field (str "Observation " (inc i) " pan · DMX sent") (some->> pan (placement/dmx-sent (get-in fixture [:channels :pan]))) [:rig-editor/sample-value i :pan] {:min 0 :max 255 :step 1}]
   [fields/number-field (str "Observation " (inc i) " tilt · DMX sent") (some->> tilt (placement/dmx-sent (get-in fixture [:channels :tilt]))) [:rig-editor/sample-value i :tilt] {:min 0 :max 255 :step 1}]
   [:button.icon-button {:aria-label (str "Delete observation " (inc i)) :on-click #(dispatch! [:rig-editor/delete-sample i])} "×"]])

(defn calibrate-panel
  "Verify and derive a moving head's aim against known points in the room."
  [db state fixture]
  (let [{:keys [target check samples verified? fit checking fitting]} (:calibration state)]
    ;; The caller passes the selected fixture's own calibration state.
    [:section.rig-path-form {:aria-label "Aim calibration"}
     [:div.panel-top [:span.section-label "AIM CALIBRATION"]]
     (cond
       (nil? fixture) [:p.small.muted "Select a moving head (pick it in the scene or the object list)."]
       (not (moving? fixture)) [:p.small.muted "The selected fixture has no pan and tilt."]
       :else
       [:<>
        [:p.small.muted "Tap a floor or wall point you have measured, or enter it. Check aim solves the pan/tilt the head needs from the draft mounting and travel. Send test aim points the real head there; save the rig first."]
        [:div.rig-vector-fields
         (for [[i label] (map-indexed vector ["X" "Y" "Z"])]
           ^{:key label} [fields/number-field (str "Target " label " · m") (get target i) [:rig-editor/calibration-target i]])]
        [:div.rig-editor-actions
         [:button.button.primary {:disabled (boolean (or checking (not target))) :on-click #(rf/dispatch [:rig-editor/aim-check])} "Check aim"]
         [:button.button {:disabled (boolean (or (:aim-request state) (not target) (get-in db [:ui :rig-dirty?])))
                          :on-click #(rf/dispatch [:rig-editor/test-aim])} "Send test aim"]]
        (when check
          [:p.small {:role "status" :aria-label "Aim check result"}
           (if (:reachable? check)
             (str "Pan " (round (:pan-deg check) 1) "° (DMX sent " (get-in check [:dmx :pan]) "), tilt " (round (:tilt-deg check) 1) "° (DMX sent " (get-in check [:dmx :tilt]) ").")
             (str "Out of reach" (when (:pan-deg check) (str ": needs pan " (round (:pan-deg check) 1) "°, tilt " (round (:tilt-deg check) 1) "°")) "."))])
        [:div.panel-top [:span.section-label "DERIVE MOUNTING"] [:span.small.muted (str (count samples) " observations")]]
        [:p.small.muted "Jog the head until its beam centre hits the target, then record it. Three or more targets spread around the head let Studio fit its mounting, including hanging versus standing and the home direction."]
        [:div.rig-editor-actions
         [:button.button {:disabled (not target) :on-click #(dispatch! [:rig-editor/record-sample])} "Record current pan/tilt"]]
        [:div.rig-path-points (for [[i sample] (map-indexed vector samples)] ^{:key i} [sample-row fixture i sample])]
        [:label.rig-checkbox [checkbox/input {:checked (true? verified?) :on-change #(dispatch! [:rig-editor/set [:calibration :verified?] (.. % -target -checked)])}]
         "Pan/tilt direction and travel match the manual"]
        [:div.rig-editor-actions
         [:button.button {:disabled (boolean (or fitting (< (count samples) 3))) :on-click #(rf/dispatch [:rig-editor/aim-fit])} "Fit mounting"]
         [:button.button {:disabled (not (:rotation-deg fit)) :on-click #(dispatch! [:rig-editor/apply-fit])} "Apply fitted rotation"]]
        (when fit
          [:p.small {:role "status" :aria-label "Mounting fit result"}
           (if (:rotation-deg fit)
             (str (if (:accepted? fit) "Fit accepted: " "Candidate only: ") (name (or (:mounting-kind fit) "")) ", rotation "
                  (str/join " / " (map #(round % 1) (:rotation-deg fit))) "°, error " (round (:rms-deg fit) 2) "° over " (:inliers fit) " targets"
                  (when-not (:accepted? fit) (if (:axis-profile-verified? fit) " (spread or error too large)." " (confirm the axis direction first).")))
             (str "No fit: " (name (or (:reason fit) "insufficient observations")) "."))])])
     (when (:message state) [:p.rig-editor-message {:role "status"} (:message state)])]))
