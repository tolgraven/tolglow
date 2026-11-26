(ns tolglow.studio.shader-cues
  "Shader cues (#673): every effect with a :shader block, its live feed, its
  producer's cadence and drops, launch and stop, an enlarged view, the
  borderless projector page, and a GLSL editor that auditions a compile on
  the show's GPU worker before saving a new version. Saving and launching use
  the ordinary definition and cue commands, as the REPL does; the feed image
  is the frame the lighting latch holds."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.video.shader-examples :as examples]))

(defn- wire-id [value] (events/wire-id value))

(defn feed-id
  "The feed a shader effect publishes; mirrors `tolglow.video.shader/feed-id`."
  [effect]
  (or (get-in effect [:shader :feed])
      (keyword "shader" (str/replace (wire-id (:id effect)) "/" "."))))

(defn shader-effects [effects] (vec (sort-by (comp str :id) (filter #(map? (:shader %)) effects))))

(defn output-url [feed] (str "/studio/video-output.html?source=" (js/encodeURIComponent (str feed))))

(defn frame-url [feed width token]
  (str "/api/video/frame?source=" (js/encodeURIComponent (str feed)) "&width=" width "&v=" token))

(defn- ui [db] (:shader-cues-ui db {}))

(rf/reg-sub ::ui (fn [db _] (ui db)))

(rf/reg-sub ::effects (fn [db _] (shader-effects (get-in db [:show :effects]))))

(rf/reg-sub ::cues (fn [db _] (get-in db [:show :cues])))

(rf/reg-sub ::instances (fn [db _] (get-in db [:show :instances])))

(rf/reg-sub ::status (fn [db _] (get-in db [:server-projection :view :video :shaders])))

(rf/reg-event-db ::ui (fn [db [_ path value]] (assoc-in db (into [:shader-cues-ui] path) value)))

(rf/reg-event-db ::edit-text
  ;; An audition result describes the text it compiled, so an edit clears it.
  (fn [db [_ id glsl]]
    (update-in db [:shader-cues-ui :editor id] #(-> % (assoc :glsl glsl) (dissoc :audition)))))

(rf/reg-event-fx ::audition
  (fn [{:keys [db]} [_ effect glsl]]
    (let [token (str (random-uuid))]
      {:db (assoc-in db [:shader-cues-ui :editor (:id effect) :audition] {:busy? true :token token})
       :studio/http {:path "/api/video/shader/audition" :typed? true :timeout-ms 15000
                     :body {:effect-id (:id effect) :glsl glsl}
                     :on-success [::auditioned (:id effect) token]
                     :on-failure [::auditioned (:id effect) token]}})))

(rf/reg-event-db ::auditioned
  (fn [db [_ id token result]]
    (if (= token (get-in db [:shader-cues-ui :editor id :audition :token]))
      (assoc-in db [:shader-cues-ui :editor id :audition]
                (if (map? result) result {:compiled? false :message (str result)}))
      db)))

(rf/reg-event-fx ::save
  (fn [{:keys [db]} [_ effect glsl]]
    {:db (assoc-in db [:shader-cues-ui :editor (:id effect) :saving?] true)
     :studio/http {:path "/api/video/shader/save" :typed? true
                   :body {:effect-id (:id effect) :glsl glsl :expected-version (:version effect)}
                   :on-success [::saved (:id effect)]
                   :on-failure [::save-failed (:id effect)]}}))

(rf/reg-event-fx ::saved
  (fn [{:keys [db]} [_ id _]]
    {:db (update-in db [:shader-cues-ui :editor id] dissoc :saving? :error)
     :fx [(events/notice "Shader saved. Instances whose cue reloads pick it up; a version that does not compile keeps the last working one.")]}))

(rf/reg-event-db ::save-failed
  (fn [db [_ id error]] (update-in db [:shader-cues-ui :editor id] #(-> % (dissoc :saving?) (assoc :error (str error))))))

(rf/reg-event-fx ::add-example
  ;; An example is an ordinary effect plus a cue of the same id.
  (fn [{:keys [db]} [_ template]]
    (let [definition (get (examples/templates) template)
          existing (events/effect db (:id definition))
          definition (cond-> definition existing (assoc :version (inc (:version existing))))]
      {:fx [[:studio/http {:path "/api/definition" :typed? true
                           :body (cond-> {:kind :effects :definition definition}
                                   existing (assoc :expected-version (:version existing)))
                           :on-success [::example-published definition]}]]})))

(rf/reg-event-fx ::example-published
  (fn [{:keys [db]} [_ definition _]]
    (if (events/cue db (:id definition))
      {:fx [(events/notice "Shader example updated.")]}
      {:fx [[:studio/http {:path "/api/definition" :typed? true
                           ;; A saved GLSL edit reloads running instances of this cue.
                           :body {:kind :cues :definition {:id (:id definition) :effect-id (:id definition)
                                                           :name (:name definition) :priority 10
                                                           :reload-policy :preserve-state}}
                           :on-success [::ui [:added] (:id definition)]}]
            (events/notice "Shader example and cue added.")]})))

(defn next-shown
  "The frame token to show: the newest sequence, at most every 125 ms."
  [{:keys [token at] :as shown} sequence now]
  (if (and sequence (not= sequence token) (> (- now at) 125))
    {:token sequence :at now}
    shown))

(defn- live-image [_feed _width _sequence _class]
  ;; Refresh at most eight times a second while the feed advances. The
  ;; token moves after mount and update, never during render.
  (let [shown (r/atom {:token nil :at 0})
        advance! (fn [this]
                   (let [[_ _ _ sequence] (r/argv this)
                         next (next-shown @shown sequence (.now js/Date))]
                     (when-not (= next @shown) (reset! shown next))))]
    (r/create-class
     {:display-name "shader-live-image"
      :component-did-mount advance!
      :component-did-update advance!
      :reagent-render
      (fn [feed width _sequence class]
        (if (:token @shown)
          [:img {:class class :alt (str "Live frame of " (wire-id feed)) :draggable false
                 :src (frame-url feed width (:token @shown))}]
          [:div {:class (str class " shader-empty")} "Not running"]))})))

(defn- stats-line [rows]
  (when (seq rows)
    (let [{:keys [fps width height rendered dropped render-ms-median]} (first rows)]
      (str (count rows) " running · " width "×" height " at " fps " fps · "
           rendered " rendered, " dropped " dropped"
           (when render-ms-median (str " · " (.toFixed render-ms-median 1) " ms per frame"))))))

(defn- editor [effect state]
  (let [glsl (or (:glsl state) (get-in effect [:shader :glsl]))
        audition (:audition state)
        edit! #(rf/dispatch-sync [::edit-text (:id effect) %])]
    [:div.shader-editor
     [:label.shader-source "GLSL fragment body"
      [:textarea {:value glsl :spell-check false :rows 14 :aria-label (str "GLSL for " (wire-id (:id effect)))
                  :on-change #(edit! (.. % -target -value))}]]
     [:p.small.muted "Uniforms: " (str/join ", " (concat ["u_time" "u_beats" "u_resolution" "u_origin"]
                                                        (map wire-id (keys (get-in effect [:shader :uniforms])))))
      ". Write fragColor."]
     [:div.form-row
      [:button.text-button {:type "button" :disabled (:busy? audition)
                            :on-click #(rf/dispatch [::audition effect glsl])} "Audition compile"]
      [:button.button {:type "button" :disabled (or (:saving? state) (= glsl (get-in effect [:shader :glsl])))
                       :on-click #(rf/dispatch [::save effect glsl])} "Save version"]
      [:button.text-button {:type "button" :on-click #(rf/dispatch [::ui [:editor (:id effect)] nil])} "Close"]]
     (cond
       (:busy? audition) [:p.small.muted "Compiling on the show's GPU worker…"]
       (true? (:compiled? audition)) [:p.shader-ok "Compiles. Save to run it."]
       (false? (:compiled? audition)) [:div.shader-log {:role "alert"} [:strong (or (:message audition) "Does not compile")]
                                       (when (:log audition) [:pre (:log audition)])])
     (when (:error state) [:p.performance-error {:role "alert"} (:error state)])]))

(defn- card [effect cue instances rows ui]
  (let [feed (feed-id effect)
        running (filterv #(= (:effect-id %) (:id effect)) instances)
        sequence (some :sequence rows)
        error (some :error rows)
        editing (get-in ui [:editor (:id effect)])]
    [:article.shader-card {:class (when (seq running) "running")}
     [:div.shader-card-top
      [:strong (or (:name effect) (wire-id (:id effect)))]
      [:span.small.muted (str (wire-id feed) " · v" (:version effect) " · " (name (get-in effect [:shader :kind] :2d)))]]
     [:button.shader-preview {:type "button" :aria-label (str "Enlarge " (wire-id feed))
                              :on-click #(rf/dispatch [::ui [:enlarged] feed])}
      ;; A strip target (200 × 1) is stretched so its pixels stay visible.
      [live-image feed 320 sequence (if (some #(< (:height % 8) 8) rows) "shader-frame thin" "shader-frame")]]
     [:p.small.muted (or (stats-line rows) (if (seq running) "Waiting for the first frame…" "Stopped"))]
     (when error
       [:div.shader-log {:role "alert"}
        [:strong (if (some :last-working? rows) "New version does not compile; the last working version is running." (:message error))]
        (when (:log error) [:pre (:log error)])])
     [:div.form-row
      (if (seq running)
        [:button.button {:type "button"
                         :on-click #(doseq [i running] (rf/dispatch [::stop (:id i)]))} "Stop"]
        [:button.button {:type "button" :disabled (nil? cue)
                         :on-click #(rf/dispatch [:studio/launch (:id cue) "persistent" {:grid "off"}])} "Launch"])
      [:button.text-button {:type "button" :on-click #(rf/dispatch [::ui [:enlarged] feed])} "Enlarge"]
      [:a.text-button {:href (output-url feed) :target "_blank" :rel "noopener"} "Projector output"]
      [:button.text-button {:type "button" :on-click #(rf/dispatch [::ui [:editor (:id effect)] (or editing {})])} "Edit GLSL"]]
     (when editing [editor effect editing])]))

(rf/reg-event-fx ::stop
  (fn [{:keys [db]} [_ instance-id]] {:fx [(events/command db "instance/end" {:instance-id instance-id})]}))

(defn- enlarged [feed rows]
  [:div.shader-enlarged {:role "dialog" :aria-label (str "Enlarged " (wire-id feed))
                         :on-click #(rf/dispatch [::ui [:enlarged] nil])}
   [live-image feed 1280 (some :sequence rows) "shader-frame-large"]
   [:div.shader-enlarged-bar
    [:span (wire-id feed)]
    [:a.text-button {:href (output-url feed) :target "_blank" :rel "noopener"
                     :on-click #(.stopPropagation %)} "Open projector output"]
    [:button.text-button {:type "button"} "Close"]]])

(defn panel []
  (let [effects @(rf/subscribe [::effects]) cues @(rf/subscribe [::cues])
        instances @(rf/subscribe [::instances]) status @(rf/subscribe [::status])
        ui @(rf/subscribe [::ui])
        rows (group-by :feed (:instances status))]
    [:section#shader-cues.panel {:aria-label "Shader cues"}
     [:div.panel-top [:span.section-label "SHADER CUES"]
      [:span.small.muted (if-let [r (get-in status [:renderer :renderer])]
                           (str "GPU · " r)
                           (or (get-in status [:error :message]) "Rendered on the show's GPU worker"))]]
     [:div.form-body
      (when (seq (:refused status))
        [:p.shader-log {:role "status"}
         (str (count (:refused status)) " older shader instance" (when (< 1 (count (:refused status))) "s")
              " not rendering: the newest 8 render, within the GPU pixel budget.")])
      (if (empty? effects)
        [:p.small.muted "No shader cues yet. Add an example, or publish an effect with a :shader block from the REPL or the library."]
        [:div.shader-grid
         (for [effect effects
               :let [feed (feed-id effect)]]
           ^{:key (str (:id effect))}
           [card effect (some #(when (= (:id effect) (:effect-id %)) %) cues) instances (get rows feed) ui])])
      [:div.form-row
       [:label "Add an example"
        [:select {:value "" :aria-label "Add an example shader cue"
                  :on-change #(let [v (.. % -target -value)] (when (seq v) (rf/dispatch [::add-example (keyword v)])))}
         [:option {:value ""} "Choose…"]
         [:option {:value "plasma"} "2D plasma · 640×360"]
         [:option {:value "scene"} "3D ray-marched scene · 640×360"]]]]
      (when-let [feed (:enlarged ui)] [enlarged feed (get rows feed)])]]))
