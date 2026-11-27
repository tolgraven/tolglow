(ns tolglow.studio.surfaces
  "Control surfaces, links and their session boxes (#705, #706). Studio sends
  the same :control-surface/* owner commands as the REPL and devices and reads
  the committed :control-surfaces projection."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.checkbox :as checkbox]
            [tolglow.studio.projection :as projection]))

(def ^:private ui-path [:ui :surfaces])

(defn- show-view [db] (get-in db [:server-projection :view]))

(defn- connected? [db] (and (get-in db [:connection :connected?]) (not (:awaiting-full? db))))

(defn surfaces-view [db] (:control-surfaces (show-view db)))

(defn- wire-id [value] (if (keyword? value) (subs (str value) 1) (str value)))

(defn cue-grid?
  "A logical surface whose content is the running launches (#545)."
  [logical]
  (= :active-cues (:content-type logical)))

(defn on-surface
  "Cells shown by an attached member: {:groups {group-id label}
  :instances {wire instance-id label} :selected {wire instance-id surface}},
  labelled with the surface and strip, or row and pad on a cue grid.
  :selected names the logical surface whose committed selection is that
  launch. Instance keys use the text IDs of Studio's presented instances."
  [surfaces]
  (let [found (reduce (fn [found logical]
                        (reduce (fn [found {:keys [surface-id cells]}]
                                  (reduce (fn [found {:keys [group-id instance-id position]}]
                                            (let [label (if (cue-grid? logical)
                                                          (str surface-id " " (inc (:y position)) "·" (inc (:x position)))
                                                          (str surface-id " " (inc (:x position))))]
                                              (cond-> found
                                                group-id (assoc-in [:groups group-id] label)
                                                instance-id (assoc-in [:instances (wire-id instance-id)] label))))
                                          found cells))
                                found (:members logical)))
                      {:groups {} :instances {} :selected {}}
                      (:logical surfaces))]
    (reduce (fn [found {:keys [id layout]}]
              (let [[kind selected] (:selected layout)]
                (cond-> found (= :instance kind) (assoc-in [:selected (wire-id selected)] id))))
            found (:logical surfaces))))

(rf/reg-sub :surfaces/view (fn [db _] (surfaces-view db)))

(rf/reg-sub :surfaces/on-surface :<- [:surfaces/view] (fn [surfaces _] (on-surface surfaces)))

(rf/reg-sub :surfaces/status (fn [db _] (get-in db ui-path)))

(defn- command [db body]
  (let [id (str (random-uuid))]
    {:db (assoc-in db (conj ui-path :pending) id)
     :fx [[:studio/http {:path "/api/command" :typed? true :raw-response? true :timeout-ms 6500
                         :body (merge (projection/identity-of (show-view db))
                                      {:command-id id :source {:adapter :web :session-id (:session db)}}
                                      body)
                         :on-success [:surfaces/received id]
                         :on-failure [:surfaces/failed id]}]]}))

(rf/reg-event-fx :surfaces/bank
  (fn [{:keys [db]} [_ target viewport move]]
    (when (connected? db)
      (command db (cond-> {:op :control-surface/bank :target target :viewport viewport}
                    (keyword? move) (assoc :move move)
                    (map? move) (merge move))))))

(rf/reg-event-fx :surfaces/layout
  ;; The same :control-surface/layout command the X-Touch mode buttons commit.
  (fn [{:keys [db]} [_ target viewport patch]]
    (when (connected? db)
      (command db {:op :control-surface/layout :target target :viewport viewport :set patch}))))

(def encoder-modes
  "Encoder modes of the mixer layout (#706 slice 2), as Studio names them."
  [[:pan "Pan"] [:send "Send"] [:plug-in "Plug-in"] [:user "Knobs"]])

(defn layout-label
  "Short text for a logical surface's layout state, such as
  \"Plug-in · page 2 · cue ember\"."
  [{:keys [encoder-mode page selected flip?]}]
  (str/join " · " (cond-> [(or (some (fn [[k label]] (when (= k encoder-mode) label)) encoder-modes) "Pan")]
                    (pos? (or page 0)) (conj (str "page " (inc page)))
                    selected (conj (str (name (first selected)) " "
                                        (let [id (second selected)] (if (vector? id) (wire-id (second id)) (wire-id id)))))
                    flip? (conj "flipped"))))

(def link-choices
  "The Studio link form's choices: the shared :control-surface/link payload
  for a leader and one more surface."
  {"right" {:mode :extend :direction :right} "left" {:mode :extend :direction :left}
   "mirror" {:mode :mirror} "follow" {:mode :follow :offset {:x 1 :y 0}}})

(defn link-payload [link-id leader other choice]
  (let [{:keys [mode direction offset]} (get link-choices choice)]
    (cond-> {:op :control-surface/link :link-id link-id :mode mode
             :members [{:surface-id leader} (cond-> {:surface-id other} offset (assoc :offset offset))]}
      direction (assoc :direction direction))))

(rf/reg-event-fx :surfaces/link
  (fn [{:keys [db]} [_ link-id leader other choice]]
    (when (and (connected? db) (not (str/blank? link-id)) (not= leader other) (contains? link-choices choice))
      (command db (link-payload link-id leader other choice)))))

(rf/reg-event-fx :surfaces/unlink
  (fn [{:keys [db]} [_ link-id]]
    (when (connected? db) (command db {:op :control-surface/unlink :link-id link-id}))))

(rf/reg-event-fx :surfaces/follow
  ;; Studio selected a group or cue. With follow on, each logical surface whose
  ;; content holds it reveals it; the owner leaves a visible cell where it is.
  (fn [{:keys [db]} [_ selected]]
    (let [surfaces (surfaces-view db)
          instance (when (= :instance (first selected))
                     (some #(when (= (second selected) (wire-id (:id %))) %) (:instances (show-view db))))
          ;; A presented instance ID resolves to the exact typed cue ID on a mixer.
          cell-key (if (= :instance (first selected))
                     (when-let [cue-id (:cue-id instance)] [:cue cue-id])
                     selected)
          ;; A cue grid shows the launch itself, and takes it as its selection (#545).
          launch-key (when instance [:instance (:id instance)])]
      (when (and (connected? db) (get-in surfaces [:settings :follow-selection?]))
        (let [fx (vec (mapcat (fn [{:keys [id viewport] :as logical}]
                                (if (cue-grid? logical)
                                  (when launch-key
                                    [[:dispatch [:surfaces/reveal id viewport launch-key]]
                                     [:dispatch [:surfaces/layout id viewport {:selected launch-key}]]])
                                  (when cell-key [[:dispatch [:surfaces/reveal id viewport cell-key]]])))
                              (:logical surfaces)))]
          (when (seq fx) {:fx fx}))))))

(rf/reg-event-fx :surfaces/reveal
  (fn [{:keys [db]} [_ target viewport cell-key]]
    (when (connected? db)
      (assoc (command db {:op :control-surface/bank :target target :viewport viewport :reveal cell-key})
             :db (assoc-in db (conj ui-path :quiet) true)))))

(rf/reg-event-fx :surfaces/follow-toggle
  (fn [{:keys [db]} [_ enabled?]]
    (when (connected? db)
      {:fx [[:studio/http {:path "/api/settings" :timeout-ms 6000
                           :body {:scope "controllers"
                                  :values {:surfaces (assoc (get-in (show-view db) [:settings :controllers :surfaces] {})
                                                            :follow-selection? (boolean enabled?))}}
                           :on-success [:surfaces/received nil]
                           :on-failure [:surfaces/failed nil]}]]})))

(rf/reg-event-db :surfaces/received
  (fn [db [_ id _receipt]]
    (cond-> db
      (= id (get-in db (conj ui-path :pending))) (update-in ui-path dissoc :pending :error :quiet))))

(rf/reg-event-db :surfaces/failed
  (fn [db [_ id message]]
    (if (or (nil? id) (= id (get-in db (conj ui-path :pending))))
      (update-in db ui-path #(cond-> (dissoc % :pending :quiet)
                               ;; A follow reveal for a cell outside a surface is not an error to show.
                               (not (:quiet %)) (assoc :error message)))
      db)))

(defn range-label
  "Human strip range of a logical surface, such as \"Strips 1–16 of 20\",
  or the launches a cue grid page shows, such as \"Cues 65–70 of 70\"."
  [{:keys [window span size] :as logical}]
  (if (cue-grid? logical)
    (let [columns (max 1 (:columns size)) total (:count logical 0)
          first-cell (inc (* columns (:y window)))
          last-cell (min total (* columns (+ (:y window) (:rows span))))]
      (if (pos? total) (str "Cues " first-cell "–" (max first-cell last-cell) " of " total) "No running cues"))
    (let [first-cell (inc (:x window)) last-cell (min (:columns size) (+ (:x window) (:columns span)))]
      (if (pos? (:columns size))
        (str "Strips " first-cell "–" (max first-cell last-cell) " of " (:columns size))
        "No strips"))))

(defn- member-names [mode members]
  (str/join (if (= :mirror mode) " = " " · ")
            (map (fn [{:keys [surface-id attached?]}] (str surface-id (when-not attached? " (unavailable)"))) members)))

(defn session-box
  "Bank buttons and the strip range for one logical surface."
  [{:keys [id viewport mode window limit members] :as logical} disabled?]
  (let [bank #(rf/dispatch [:surfaces/bank id viewport %])
        grid? (cue-grid? logical)
        ;; A cue grid pages down through rows of launches; a mixer pages across strips.
        [axis first-move previous-move next-move last-move] (if grid? [:y :top :up :down :bottom]
                                                              [:x :first :previous :next :last])
        at-start? (zero? (get window axis 0))
        at-end? (>= (get window axis 0) (get limit axis 0))
        unit (if grid? "page" "bank")]
    [:div.surface-box {:data-surface id :data-mode (name mode)}
     [:span.surface-box-name {:title (member-names mode members)}
      (str id (when (not= :single mode) (str " · " (name mode))))]
     [:span.surface-box-range {:role "status"} (range-label logical)]
     (if grid?
       [:span.surface-box-layout {:title "Pads select or reorder running cues on this grid"}
        (if-let [[_ selected :as cell-key] (get-in logical [:layout :selected])]
          ;; The cue name when the selected launch is on a shown page.
          (str "Selected " (if-let [label (some #(when (= cell-key (:key %)) (:label %)) (mapcat :cells members))]
                             (str label " · " (wire-id selected))
                             (wire-id selected)))
          "Nothing selected")]
       [:label.surface-box-layout {:title (layout-label (:layout logical))}
        [:select {:value (name (get-in logical [:layout :encoder-mode] :pan)) :disabled disabled?
                  :aria-label (str "Encoder mode on " id)
                  :on-change #(rf/dispatch [:surfaces/layout id viewport {:encoder-mode (keyword (.. % -target -value)) :page 0}])}
         (for [[k label] encoder-modes] ^{:key k} [:option {:value (name k)} label])]])
     [:span.surface-box-buttons
      [:button.surface-box-button {:type "button" :aria-label (str "First " unit " on " id)
                                   :disabled (or disabled? at-start?) :on-click #(bank first-move)} (if grid? "⏫" "⏮")]
      [:button.surface-box-button {:type "button" :aria-label (str "Previous " unit " on " id)
                                   :disabled (or disabled? at-start?) :on-click #(bank previous-move)} (if grid? "⏶" "‹")]
      [:button.surface-box-button {:type "button" :aria-label (str "Next " unit " on " id)
                                   :disabled (or disabled? at-end?) :on-click #(bank next-move)} (if grid? "⏷" "›")]
      [:button.surface-box-button {:type "button" :aria-label (str "Last " unit " on " id)
                                   :disabled (or disabled? at-end?) :on-click #(bank last-move)} (if grid? "⏬" "⏭")]]]))

(defn- link-form [surfaces disabled?]
  (let [unlinked (keep (fn [{:keys [id link?]}] (when-not link? id)) (:logical surfaces))
        attached (set (keep #(when (:attached? %) (:id %)) (:surfaces surfaces)))
        choices (filterv attached unlinked)]
    (when (< 1 (count choices))
      [:form.surface-link-form
       {:on-submit (fn [event]
                     (.preventDefault event)
                     (let [fields (js/FormData. (.-currentTarget event))
                           first-id (.get fields "first") second-id (.get fields "second")
                           choice (.get fields "mode")]
                       (when (not= first-id second-id)
                         (rf/dispatch [:surfaces/link (str first-id "+" second-id) first-id second-id choice]))))}
       [:select {:name "first" :aria-label "First surface" :default-value (first choices)}
        (for [id choices] ^{:key id} [:option {:value id} id])]
       [:select {:name "mode" :aria-label "How to link" :default-value "right"}
        [:option {:value "right"} "extended to the right by"] [:option {:value "left"} "extended to the left by"]
        [:option {:value "mirror"} "mirrored by"] [:option {:value "follow"} "followed one page right by"]]
       [:select {:name "second" :aria-label "Second surface" :default-value (second choices)}
        (for [id choices] ^{:key id} [:option {:value id} id])]
       [:button.button {:type "submit" :disabled disabled?} "Link"]])))

(defn panel
  "Session boxes for every logical surface, links, and the follow setting."
  []
  (let [surfaces @(rf/subscribe [:surfaces/view])
        {:keys [pending error]} @(rf/subscribe [:surfaces/status])
        disabled? (boolean pending)]
    (when (seq (:surfaces surfaces))
      [:section.surface-panel {:aria-label "Control surfaces"}
       [:header.surface-panel-heading
        [:h3 "Surfaces"]
        [:label.checkbox-label
         [checkbox/input {:checked (boolean (get-in surfaces [:settings :follow-selection?]))
                          :on-change #(rf/dispatch [:surfaces/follow-toggle (.. % -target -checked)])}]
         "Follow selection"]]
       (when error [:p.surface-panel-error {:role "alert"} error])
       (for [logical (:logical surfaces)]
         ^{:key (pr-str [(:id logical) (:viewport logical)])}
         [:div.surface-panel-row
          [session-box logical disabled?]
          (when (:link? logical)
            [:button.surface-unlink {:type "button" :disabled disabled?
                                     :on-click #(rf/dispatch [:surfaces/unlink (:id logical)])} "Unlink"])])
       [link-form surfaces disabled?]])))
