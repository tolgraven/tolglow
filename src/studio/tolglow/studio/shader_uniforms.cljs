(ns tolglow.studio.shader-uniforms
  "Studio editing of a shader cue's uniforms, render size and rate (#673).

  Two kinds of edit, each through a validated command the REPL also has:
  - Live: the controls a running instance's uniforms read are its ordinary
    parameters, so turning one sends `param/set` for that instance, with
    the operator history's undo, exactly as a running card does.
  - Definition: uniforms (type, a constant or a control, added or
    removed), size and rate form a draft that **Save version** publishes as
    the effect's next version (`show/video-action! runtime :save-shader`).
    **Undo** and **Redo** republish the previous or undone block as the
    next version (`:undo-shader`, `:redo-shader`). The show validates each
    version like any effect and refuses a bad one, saying what is wrong."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.video-controls :as video-controls]))

(def wire-id events/wire-id)

(def uniform-types [:float :int :bool :vec2 :vec3 :vec4])

(def ^:private reserved #{"u_time" "u_beats" "u_resolution" "u_origin"})

(defn components [type] (case type (:vec2) 2 (:vec3) 3 (:vec4) 4 1))

(defn typed-effect
  "The effect with keyword types and ids, from the typed projection."
  [db id]
  (some #(when (= (wire-id (:id %)) (wire-id id)) %) (get-in db [:server-projection :view :effects])))

(defn draft-of
  "The editable part of a shader effect."
  [effect]
  {:size (get-in effect [:shader :size]) :fps (get-in effect [:shader :fps] 30)
   :uniforms (get-in effect [:shader :uniforms] {}) :params (:params effect {})})

(defn value-kind
  "How a uniform's value is authored: :constant (a number or vector of
  numbers), :control (one parameter) or :expression (anything else, kept)."
  [value]
  (cond (number? value) :constant
        (and (vector? value) (every? number? value)) :constant
        (and (map? value) (= :param (:op value)) (= #{:op :key} (set (keys value)))) :control
        :else :expression))

(defn constant-for
  "A constant of a uniform type, from the old value where it fits."
  [type value]
  (let [n (components type)
        numbers (cond (number? value) [value] (and (vector? value) (every? number? value)) value :else [])]
    (if (= 1 n)
      (or (first numbers) 0)
      (vec (take n (concat numbers (repeat 0)))))))

(defn uniform-name-error
  "Why a new uniform name is refused, or nil."
  [uniforms name]
  (cond (not (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (or name ""))) "Use letters, digits and _, not starting with a digit."
        (reserved name) (str name " is built in.")
        (contains? uniforms (keyword name)) (str name " already exists.")
        (>= (count uniforms) 32) "A shader has at most 32 uniforms."))

(defn control-param
  "The parameter id a uniform's new control takes: its name without a u_
  prefix, made unique among params."
  [params uniform]
  (let [base (str/replace (name uniform) #"^u_" "")
        base (if (str/blank? base) (name uniform) base)]
    (first (remove #(contains? params %) (cons (keyword base) (map #(keyword (str base "-" %)) (iterate inc 2)))))))

(defn make-control
  "Bind a float constant uniform to a new number parameter around its value."
  [draft uniform]
  (let [value (get-in draft [:uniforms uniform :value])
        v (if (number? value) value 0)
        param (control-param (:params draft) uniform)]
    (-> draft
        (assoc-in [:params param] {:type :number :default v :bounds [(min 0 v) (max 1 v)]})
        (assoc-in [:uniforms uniform :value] {:op :param :key param}))))

(defn edit-request
  "The :save-shader payload for a draft started on base (the effect as it
  was when editing began): the shader keys it changes, params added or
  changed (a removed param is sent as nil), and base's version as the
  expected version, so a version published meanwhile refuses the save
  instead of being overwritten."
  [base draft]
  (let [effect base
        old (draft-of base)
        shader (cond-> {}
                 (not= (:size draft) (:size old)) (assoc :size (:size draft))
                 (not= (:fps draft) (:fps old)) (assoc :fps (:fps draft))
                 (not= (:uniforms draft) (:uniforms old)) (assoc :uniforms (:uniforms draft)))
        params (merge (into {} (for [k (keys (:params old)) :when (not (contains? (:params draft) k))] [k nil]))
                      (into {} (for [[k v] (:params draft) :when (not= v (get (:params old) k))] [k v])))]
    (cond-> {:effect-id (:id effect) :expected-version (:version effect) :shader shader}
      (seq params) (assoc :params params))))

;; Events ---------------------------------------------------------------------------

(defn- state [db id] (get-in db [:shader-cues-ui :uniforms (wire-id id)]))

(defn- put [db id f & args] (apply update-in db [:shader-cues-ui :uniforms (wire-id id)] f args))

(rf/reg-sub ::state (fn [db [_ id]] (state db id)))

(rf/reg-sub ::effect (fn [db [_ id]] (typed-effect db id)))

(rf/reg-sub ::history (fn [db [_ id]] (get-in db [:server-projection :view :video :history :shaders (:id (typed-effect db id))])))

(defn active
  "The draft in state s and the effect it started on, as {:draft :base}, or
  nil once a saved draft's version has reached the effect (or none was
  started)."
  [s effect]
  (when (and (:draft s) (not (and (:acked s) (>= (:version effect) (:acked s)))))
    (select-keys s [:draft :base])))

(rf/reg-event-db ::edit
  ;; f applies to the draft. A new draft records the effect it starts on.
  (fn [db [_ id f & args]]
    (let [effect (typed-effect db id)]
      (put db id (fn [s] (let [{:keys [draft base]} (or (active s effect) {:draft (draft-of effect) :base effect})]
                           (-> s (assoc :draft (apply f draft args) :base base) (dissoc :error :acked))))))))

(rf/reg-event-db ::discard (fn [db [_ id]] (put db id dissoc :draft :base :acked :error)))

(defn- send [db id path body]
  (let [token (str (random-uuid))]
    {:db (put db id #(-> % (assoc :pending token) (dissoc :error)))
     :studio/http {:path path :typed? true :body body
                   :on-success [::done id token] :on-failure [::failed id token]}}))

(rf/reg-event-fx ::save
  (fn [{:keys [db]} [_ id]]
    (let [{:keys [draft base]} (active (state db id) (typed-effect db id))]
      (if draft (send db id "/api/video/shader/edit" (edit-request base draft)) {}))))

(rf/reg-event-fx ::step
  ;; direction is "undo" or "redo".
  (fn [{:keys [db]} [_ id direction]]
    (let [effect (typed-effect db id)]
      (if effect
        (send db id (str "/api/video/shader/" direction) {:effect-id (:id effect) :expected-version (:version effect)})
        {}))))

(rf/reg-event-db ::done
  ;; Keep showing the draft until its version arrives on the state stream.
  (fn [db [_ id token ack]]
    (if (= token (:pending (state db id)))
      (put db id #(-> % (dissoc :pending :error) (assoc :acked (or (:version ack) 0))))
      db)))

(rf/reg-event-db ::failed
  (fn [db [_ id token error]]
    (if (= token (:pending (state db id)))
      (put db id #(-> % (dissoc :pending) (assoc :error (str error))))
      db)))

;; Views ------------------------------------------------------------------------------

(defn- edit! [id f & args] (rf/dispatch-sync (into [::edit id f] args)))

(defn- size-grid [id draft]
  (let [[w h] (or (:size draft) [640 360])]
    [video-controls/grid "Render size and rate"
     [{:id "width" :title "Width" :unit "px" :schema {:type :integer :bounds [1 4096]} :value w
       :change! #(edit! id assoc-in [:size 0] (js/Math.round %))}
      {:id "height" :title "Height" :unit "px" :schema {:type :integer :bounds [1 4096]} :value h
       :change! #(edit! id assoc-in [:size 1] (js/Math.round %))}
      {:id "fps" :title "Rate" :unit "fps" :schema {:type :integer :bounds [1 60]} :value (:fps draft)
       :change! #(edit! id assoc :fps (js/Math.round %))}]]))

(defn- constant-inputs [id uniform {:keys [type value]}]
  (let [n (components type) values (if (= 1 n) [value] value)]
    [:span.shader-uniform-constant
     (for [i (range n)]
       ^{:key i}
       [:input {:type "number" :step (if (#{:int :bool} type) 1 "any") :value (get values i 0)
                :aria-label (str (name uniform) (when (> n 1) (str " component " (inc i))))
                :on-change #(let [v (js/Number (.. % -target -value))]
                              (when (js/Number.isFinite v)
                                (edit! id assoc-in (cond-> [:uniforms uniform :value] (> n 1) (conj i)) v)))}])]))

(defn- uniform-row [id draft uniform {:keys [type value] :as spec}]
  (let [kind (value-kind value)
        params (sort-by str (keys (:params draft)))]
    [:li.shader-uniform
     [:code (name uniform)]
     [:select {:aria-label (str (name uniform) " type") :value (name type)
               :on-change #(let [t (keyword (.. % -target -value))]
                             (edit! id assoc-in [:uniforms uniform]
                                    {:type t :value (if (= :constant kind) (constant-for t value) value)}))}
      (for [t uniform-types] ^{:key t} [:option {:value (name t)} (name t)])]
     [:select {:aria-label (str (name uniform) " value")
               :value (case kind :constant "constant" :control (str "param:" (wire-id (:key value))) "expression")
               :on-change #(let [v (.. % -target -value)]
                             (cond
                               (= v "constant") (edit! id assoc-in [:uniforms uniform :value] (constant-for type nil))
                               (str/starts-with? v "param:")
                               (edit! id assoc-in [:uniforms uniform :value]
                                      {:op :param :key (some (fn [p] (when (= (subs v 6) (wire-id p)) p)) params)})))}
      [:option {:value "constant"} "Constant"]
      (for [p params] ^{:key (str p)} [:option {:value (str "param:" (wire-id p))} (str "Control · " (wire-id p))])
      (when (= :expression kind) [:option {:value "expression"} "Expression (kept)"])]
     (when (= :constant kind) [constant-inputs id uniform spec])
     (when (and (= :constant kind) (= :float type))
       [:button.text-button {:type "button" :on-click #(edit! id make-control uniform)} "Make it a control"])
     [:button.icon-button {:type "button" :aria-label (str "Remove " (name uniform))
                           :on-click #(edit! id update :uniforms dissoc uniform)} "×"]]))

(defn- add-uniform [id draft]
  (let [s @(rf/subscribe [::state id])
        name (:new-name s "")
        type (:new-type s :float)
        error (when (seq name) (uniform-name-error (:uniforms draft) name))
        set-field! #(rf/dispatch-sync [::field id %1 %2])]
    [:div.form-row.shader-uniform-add
     [:label "New uniform" [:input {:type "text" :value name :placeholder "u_glow" :aria-label "New uniform name"
                                    :on-change #(set-field! :new-name (.. % -target -value))}]]
     [:select {:aria-label "New uniform type" :value (cljs.core/name type)
               :on-change #(set-field! :new-type (keyword (.. % -target -value)))}
      (for [t uniform-types] ^{:key t} [:option {:value (cljs.core/name t)} (cljs.core/name t)])]
     [:button.text-button {:type "button" :disabled (or (empty? name) (some? error))
                           :on-click #(do (edit! id assoc-in [:uniforms (keyword name)] {:type type :value (constant-for type nil)})
                                          (set-field! :new-name ""))}
      "Add uniform"]
     (when error [:span.small.muted error])]))

(rf/reg-event-db ::field (fn [db [_ id k v]] (put db id assoc k v)))

(defn- live-controls
  "Number controls of the running instances that the uniforms read, as
  `param/set` edits of each instance."
  [effect instances]
  (let [read (set (keep (fn [[_ {:keys [value]}]] (when (= :control (value-kind value)) (:key value)))
                        (get-in effect [:shader :uniforms])))
        params (filter (fn [[k schema]] (and (read k) (#{:number :integer} (:type schema)) (:bounds schema)))
                       (sort-by (comp str key) (:params effect)))]
    (when (and (seq params) (seq instances))
      [:div.shader-live-controls
       (for [instance instances]
         ^{:key (str (:id instance))}
         [:div
          [:h4.small (str "Live · instance " (:id instance))]
          [video-controls/grid (str "Live uniforms of " (:id instance))
           (vec (for [[k schema] params]
                  {:id (wire-id k) :title (wire-id k) :schema schema
                   :value (get-in instance [:params k] (:default schema))
                   :change! #(rf/dispatch [:studio/param-set (:id instance) k %])}))]])])))

(defn editor
  "The uniform, size and rate editor of one shader effect (by id), with
  the live controls of its running instances."
  [id instances]
  (let [effect @(rf/subscribe [::effect id])
        s @(rf/subscribe [::state id])
        history @(rf/subscribe [::history id])
        {:keys [base] :as current} (when effect (active s effect))
        draft (or (:draft current) (when effect (draft-of effect)))
        stale? (and base (not= (:version base) (:version effect)))]
    (when effect
      [:div.shader-uniforms {:role "group" :aria-label (str "Uniforms of " (wire-id id))}
       [live-controls effect instances]
       [size-grid id draft]
       [:ul.shader-uniform-list
        (for [[uniform spec] (sort-by (comp str key) (:uniforms draft))]
          ^{:key (str uniform)} [uniform-row id draft uniform spec])]
       [add-uniform id draft]
       [:div.form-row
        [:button.button {:type "button" :disabled (or (nil? current) (:pending s) (= draft (draft-of base)))
                         :on-click #(rf/dispatch [::save id])} "Save version"]
        [:button.text-button {:type "button" :disabled (nil? current) :on-click #(rf/dispatch [::discard id])} "Discard changes"]
        [:button.text-button {:type "button" :disabled (or (:pending s) (zero? (:undo history 0)))
                              :on-click #(rf/dispatch [::step id "undo"])} "Undo version"]
        [:button.text-button {:type "button" :disabled (or (:pending s) (zero? (:redo history 0)))
                              :on-click #(rf/dispatch [::step id "redo"])} "Redo version"]
        [:span.small.muted (str "v" (:version effect))]]
       (when stale?
         [:p.small.muted {:role "status"}
          (str "v" (:version effect) " was published since you started from v" (:version base)
               ". Saving is refused so it is not overwritten; discard to edit the current version.")])
       (when (:error s) [:p.performance-error {:role "alert"} (:error s)])])))
