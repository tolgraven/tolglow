(ns tolglow.studio.audio-lfo
  "Audio-shaped LFOs (#676). Reads the shared :audio-lfos projection and sends
  the same :audio-lfo/* owner commands as the REPL. The source chart draws the
  last window the capture worker measured; the LFO chart draws the morph from
  the previous shape to the target, as the owner samples it at this frame's
  transport beat. A linked knob can drive an LFO's band and morph time: the
  card links one with the same :knob/* commands and turns it like the Knobs
  panel. See docs/audio-shaped-lfo.md."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.knobs :as knobs]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.visualization.musical-grid :as musical-grid]))

(def ^:private ui-path [:ui :audio-lfo])

(def bands [["sub" "Sub · 20–60 Hz"] ["bass" "Bass · 60–250 Hz"] ["low-mid" "Low mid · 250–800 Hz"]
            ["mid" "Mid · 0.8–2.5 kHz"] ["high" "High · 2.5–8 kHz"] ["full" "Full range"]])

(def extract-modes [["envelope" "Envelope across the window"] ["cycle" "One pitch cycle"]])

(def windows [["beats-1" "1 beat" {:beats 1}] ["bars-1" "1 bar" {:bars 1}] ["bars-2" "2 bars" {:bars 2}]
              ["phrases-1" "1 phrase" {:phrases 1}]])

(def morphs [["beats-1" "1 beat" {:beats 1}] ["bars-1/2" "½ bar" {:bars 0.5}] ["bars-1" "1 bar" {:bars 1}]
             ["bars-2" "2 bars" {:bars 2}] ["bars-4" "4 bars" {:bars 4}]])

(def silence-policies [["hold" "Hold the last shape"] ["release" "Release to rest"]])

(def statuses {"ok" "Shaping" "silent" "Silent" "low-confidence" "Low confidence" "waiting" "Waiting for a whole window"})

(defn- finite? [x] (and (number? x) (js/isFinite x)))

(defn duration-key
  "The select key of a musical duration, matched against `choices`."
  [choices duration]
  (or (some (fn [[key _ value]] (when (= value (update-keys (or duration {}) keyword)) key)) choices)
      (first (first choices))))

(defn- amount-text [amount]
  (if (== amount (js/Math.round amount)) (str amount) (str/replace (.toFixed amount 3) #"0+$" "")))

(defn window-label
  "A musical duration such as {:bars 1} as \"1 bar\" or {:beats 2} as \"2 beats\"."
  [duration]
  (if-let [[unit amount] (first duration)]
    (let [unit (events/wire-id unit)
          singular (subs unit 0 (dec (count unit)))]
      (str (amount-text amount) " " (if (== 1 amount) singular unit)))
    "—"))

(defn morph-choices
  "Morph choices, plus the configured time when it is not one of them (a knob
  sets any musical ratio), so the form shows and keeps what the LFO uses."
  [morph]
  (let [morph (when (map? morph) (update-keys morph keyword))]
    (if (or (nil? morph) (some (fn [[_ _ value]] (= value morph)) morphs))
      morphs
      (conj morphs ["current" (window-label morph) morph]))))

(defn- choice-value [choices key] (some (fn [[k _ value]] (when (= k key) value)) choices))

(defn form-of
  "Editable form values from a projected LFO configuration."
  [config]
  {:band (events/wire-id (:band config "bass")) :extract (events/wire-id (:extract config "envelope"))
   :window (duration-key windows (:window config)) :morph (duration-key (morph-choices (:morph config)) (:morph config))
   :smoothing (str (:smoothing config 0)) :on-silence (events/wire-id (:on-silence config "hold"))})

(defn configure-payload
  "The :audio-lfo/configure fields for a form, or {:error message}. config is
  the projected configuration, whose morph time the form may keep."
  ([lfo-id form] (configure-payload lfo-id form nil))
  ([lfo-id form config]
   (let [smoothing (js/parseFloat (:smoothing form))]
     (if-not (and (finite? smoothing) (<= 0 smoothing 0.95))
       {:error "Smoothing keeps 0 to 0.95 of the previous shape."}
       {:fields {:lfo-id (keyword (events/wire-id lfo-id))
                 :config {:band (keyword (:band form)) :extract (keyword (:extract form))
                          :window (choice-value windows (:window form))
                          :morph (choice-value (morph-choices (:morph config)) (:morph form))
                          :smoothing smoothing :on-silence (keyword (:on-silence form))}}}))))

;; Linked knobs (#676) ------------------------------------------------------------

(def knob-fields [["band" "Band"] ["morph" "Morph time"]])

(def morph-beats
  "Morph endpoints for a knob link, in beats. The link steps through preferred
  musical ratios between them."
  [["0.25" "¼ beat" 0.25] ["0.5" "½ beat" 0.5] ["1" "1 beat" 1] ["2" "2 beats" 2] ["4" "4 beats" 4] ["8" "8 beats" 8]])

(defn knob-form-defaults [field]
  (if (= "morph" field) {:field "morph" :from "1" :to "8"} {:field "band" :from "sub" :to "high"}))

(defn- knob-label [field] (some (fn [[k text]] (when (= k field) text)) knob-fields))

(defn knob-link-payload
  "The knob command that links a knob to an LFO's band or morph time:
  [:knob/link fields] for an existing knob, [:knob/create fields] for a new
  one, or {:error message}. knobs are the projected knobs."
  [lfo form knobs]
  (let [{:keys [field from to knob]} (merge (knob-form-defaults (:field form "band")) form)
        lfo-key (events/wire-id (:id lfo))
        range (if (= "morph" field)
                [(choice-value morph-beats from) (choice-value morph-beats to)]
                [(keyword from) (keyword to)])
        link (cond-> {:id (str lfo-key "-" field)
                      :destination {:kind :audio-lfo :lfo-id (keyword lfo-key) :field (keyword field)}
                      :range range}
               (= "morph" field) (assoc :quantize {:ratios :preferred}))
        taken (set (map (comp events/wire-id :id) knobs))]
    (cond
      (str/blank? (str knob)) {:error "Choose a knob, or a new one."}
      (or (some nil? range) (= (first range) (second range))) {:error "Choose two different endpoints."}
      (= "new" knob) [:knob/create {:knob-id (first (remove taken (map #(str "knob-" %) (iterate inc 1))))
                                    :name (str (get-in lfo [:config :name] lfo-key) " " (str/lower-case (knob-label field)))
                                    :links [link]}]
      :else [:knob/link {:knob-id (or (some #(when (= knob (events/wire-id (:id %))) (:id %)) knobs) knob) :link link}])))

(defn new-lfo-id
  "A keyword ID from a display name that no projected LFO already uses."
  [name lfos]
  (let [base (events/slug name)
        base (str/replace base #"[^a-z0-9-]" "-")
        taken (set (map (comp events/wire-id :id) lfos))]
    (first (remove taken (cons base (map #(str base "-" %) (iterate inc 2)))))))

(defn trace-points
  "SVG polyline points for a shape in a 0 0 100 40 box. Signed values span
  -1..1 around the middle; unsigned values 0..1 from the bottom."
  [values signed?]
  (let [values (vec (filter finite? values))
        n (count values)]
    (when (> n 1)
      (let [peak (if signed? (max 1.0e-9 (apply max (map js/Math.abs values))) 1.0)]
        (str/join " " (map-indexed (fn [i v]
                                     (str (.toFixed (* 100 (/ i (dec n))) 2) ","
                                          (.toFixed (if signed? (- 20 (* 18 (/ v peak))) (- 38 (* 36 v))) 2)))
                                   values))))))

(defn- cyclic [table] (when (seq table) (conj (vec table) (first table))))

;; Events

(defn- send [db op fields]
  (let [[effect request] (events/command db op fields nil nil)]
    {:db (update-in db ui-path dissoc :error)
     :fx [[effect (assoc request :on-failure [::failed])]]}))

(rf/reg-event-db ::failed (fn [db [_ message]] (assoc-in db (conj ui-path :error) (str message))))

(rf/reg-event-db ::edit (fn [db [_ path value]] (assoc-in db (into ui-path path) value)))

(rf/reg-event-fx ::apply
  (fn [{:keys [db]} [_ lfo]]
    (let [key (events/wire-id (:id lfo))
          form (merge (form-of (:config lfo)) (get-in db (into ui-path [:forms key])))
          {:keys [error fields]} (configure-payload (:id lfo) form (:config lfo))]
      (if error
        {:db (assoc-in db (conj ui-path :error) error)}
        (-> (send db :audio-lfo/configure fields)
            (update-in [:db :ui :audio-lfo :forms] dissoc key))))))

(rf/reg-event-fx ::create
  (fn [{:keys [db]} _]
    (let [name (str/trim (str (get-in db (conj ui-path :new-name))))]
      (if (str/blank? name)
        {:db (assoc-in db (conj ui-path :error) "Name the LFO first.")}
        (-> (send db :audio-lfo/configure {:lfo-id (keyword (new-lfo-id name (get-in db [:show :audio-lfos])))
                                          :name name :config {}})
            (assoc-in [:db :ui :audio-lfo :new-name] ""))))))

(rf/reg-event-fx ::remove
  (fn [{:keys [db]} [_ lfo-id]]
    (send db :audio-lfo/remove {:lfo-id (keyword (events/wire-id lfo-id))})))

(rf/reg-event-fx ::link-knob
  (fn [{:keys [db]} [_ lfo]]
    (let [key (events/wire-id (:id lfo))
          result (knob-link-payload lfo (get-in db (into ui-path [:knob-forms key])) (get-in db [:show :knobs :knobs]))]
      (if-let [error (:error result)]
        {:db (assoc-in db (conj ui-path :error) error)}
        (-> (send db (first result) (second result))
            (update-in [:db :ui :audio-lfo :knob-forms] dissoc key))))))

;; Views

(defn- input-value [event] (.. event -target -value))

(defn- source-chart [{:keys [input last config]} meter]
  (let [{:keys [kind values duration-ms]} input
        pcm? (= "pcm" (events/wire-id kind))
        window (:window last)
        span (when (and (finite? (:start-beat window)) (finite? (:end-beat window)))
               (- (:end-beat window) (:start-beat window)))
        grid (when (and span (not pcm?))
               (musical-grid/grid {:start-beat 0.0 :span-beats span :meter meter}))]
    [:figure.audio-lfo-chart.source
     [:figcaption [:span "Source band"]
      [:small (cond pcm? (str "two periods · " (.toFixed (or duration-ms 0) 1) " ms")
                    span (str "envelope over " (window-label (:window config)))
                    :else "no window yet")]]
     (if-let [points (trace-points (if pcm? values (let [peak (apply max 1.0e-9 values)] (map #(/ % peak) values))) pcm?)]
       [:svg {:view-box "0 0 100 40" :preserve-aspect-ratio "none" :role "img"
              :aria-label (if pcm? "Band-isolated waveform at the end of the last window"
                              "Band level across the last window")}
        [widgets/grid-lines grid #(* 100 (:position %)) 0 40]
        (when pcm? [:line.zero {:x1 0 :x2 100 :y1 20 :y2 20 :vector-effect "non-scaling-stroke"}])
        [:polyline.source-trace {:points points :vector-effect "non-scaling-stroke"}]]
       [:div.audio-lfo-empty "Waiting for a whole window of captured audio"])]))

(defn- lfo-chart [{:keys [from to current history progress morph-beats]}]
  (let [line (fn [class table] (when-let [points (trace-points (cyclic table) false)]
                                 [:polyline {:class class :points points :vector-effect "non-scaling-stroke"}]))
        p (if (finite? progress) progress 1)]
    [:figure.audio-lfo-chart.shape
     [:figcaption [:span "LFO cycle"]
      [:small (if (< p 1)
                (str "morphing " (js/Math.round (* 100 p)) "% of " (some-> morph-beats (.toFixed 2)) " beats")
                "settled on the target")]]
     [:svg {:view-box "0 0 100 40" :preserve-aspect-ratio "none" :role "img"
            :aria-label (str "One LFO cycle: previous shapes, target and the current morph at "
                             (js/Math.round (* 100 p)) " percent")}
      (for [[index table] (map-indexed vector history)]
        ^{:key index} [line "history" table])
      [line "from" from]
      [line "target" to]
      [line "current" current]]
     [:div.audio-lfo-progress {:role "progressbar" :aria-valuemin 0 :aria-valuemax 100
                               :aria-valuenow (js/Math.round (* 100 p)) :aria-label "Morph progress"}
      [:i {:style {:width (str (* 100 p) "%")}}]]]))

(defn- metric [label value] [:span.audio-lfo-metric [:small label] [:strong value]])

(defn- select [label value choices change!]
  [:label label
   [:select {:value value :on-change change!}
    (for [[key text] choices] ^{:key key} [:option {:value key} text])]])

(defn- knob-section
  "The knobs that drive this LFO, each with its slider, and a form to link one."
  [ui lfo knob-list knob-values connected?]
  (let [key (events/wire-id (:id lfo))
        form (get-in ui [:knob-forms key])
        {:keys [field from to knob]} (merge (knob-form-defaults (:field form "band")) form)
        edit! (fn [k] #(rf/dispatch [::edit [:knob-forms key k] (input-value %)]))
        endpoints (if (= "morph" field) morph-beats bands)]
    [:div.audio-lfo-knobs
     (for [{:keys [knob-id knob-name link-id] :as link} (:knobs lfo)
           :let [value (get knob-values (events/wire-id knob-id) (:value link))]]
       ^{:key (str (events/wire-id knob-id) "/" (events/wire-id link-id))}
       [:label.audio-lfo-knob {:data-knob-id (events/wire-id knob-id) :data-field (events/wire-id (:field link))}
        [:span (str knob-name " → " (str/lower-case (or (knob-label (events/wire-id (:field link))) "")))
         [:output (.toFixed (js/Number value) 3)]]
        [:input {:type "range" :min 0 :max 1 :step 0.001 :value value :disabled (not connected?)
                 :aria-label (str knob-name " knob on " (get-in lfo [:config :name]) " " (events/wire-id (:field link)))
                 :on-change #(rf/dispatch [::knobs/set-value knob-id (js/parseFloat (input-value %))])}]])
     [:div.audio-lfo-form.audio-lfo-knob-form
      [select "Knob drives" field knob-fields
       #(rf/dispatch [::edit [:knob-forms key] (assoc (knob-form-defaults (input-value %)) :knob knob)])]
      [select "From" from endpoints (edit! :from)]
      [select "To" to endpoints (edit! :to)]
      [select "Knob" (or knob "") (into [["" "Choose…"] ["new" "New knob"]]
                                        (map (fn [k] [(events/wire-id (:id k)) (:name k)]) knob-list))
       (edit! :knob)]
      [:button.button {:type "button" :disabled (or (not connected?) (str/blank? (str knob)))
                       :on-click #(rf/dispatch [::link-knob lfo])} "Link knob"]]]))

(defn- lfo-card [ui lfo meter connected? knob-list knob-values]
  (let [key (events/wire-id (:id lfo))
        {:keys [config last]} lfo
        form (merge (form-of config) (get-in ui [:forms key]))
        edit! (fn [field] #(rf/dispatch [::edit [:forms key field] (input-value %)]))
        status (events/wire-id (:status lfo))]
    [:article.audio-lfo-card {:data-lfo-id key :data-status status}
     [:div.audio-lfo-head
      [:strong (:name config)]
      [:span.audio-lfo-band (:band-label lfo)]
      [:span.audio-lfo-status {:data-status status} (get statuses status status)]
      [:button.text-button {:type "button" :disabled (not connected?) :aria-label (str "Remove audio LFO " (:name config))
                            :on-click #(rf/dispatch [::remove (:id lfo)])} "Remove"]]
     [:div.audio-lfo-metrics
      [metric "Waveform" (events/wire-id (:waveform-id lfo))]
      [metric "Level" (if (finite? (:level-db last)) (str (.toFixed (:level-db last) 1) " dBFS") "—")]
      [metric "Confidence" (if (finite? (:confidence last)) (str (js/Math.round (* 100 (:confidence last))) "%") "—")]
      [metric "Pitch" (if (finite? (:frequency-hz last)) (str (.toFixed (:frequency-hz last) 1) " Hz") "—")]
      [metric "Late by" (if (finite? (:latency-beats last)) (str (.toFixed (:latency-beats last) 3) " beats") "—")]
      [metric "Shapes" (str (:commits lfo 0))]]
     [:div.audio-lfo-charts [source-chart lfo meter] [lfo-chart lfo]]
     [:div.audio-lfo-form
      [select "Band" (:band form) bands (edit! :band)]
      [select "Shape from" (:extract form) extract-modes (edit! :extract)]
      [select "Window" (:window form) windows (edit! :window)]
      [select "Morph over" (:morph form) (morph-choices (:morph config)) (edit! :morph)]
      [:label "Smoothing"
       [:input {:type "range" :min 0 :max 0.95 :step 0.05 :value (:smoothing form) :on-change (edit! :smoothing)}]]
      [select "In silence" (:on-silence form) silence-policies (edit! :on-silence)]
      [:button.button {:type "button" :disabled (not connected?) :on-click #(rf/dispatch [::apply lfo])} "Apply"]]
     [knob-section ui lfo knob-list knob-values connected?]]))

(rf/reg-sub ::view
  (fn [db _]
    (let [knob-list (get-in db [:show :knobs :knobs])]
      {:lfos (get-in db [:show :audio-lfos]) :meter (get-in db [:show :transport :meter])
       :connected? (get-in db [:connection :connected?]) :ui (get-in db ui-path)
       :knob-list (mapv #(select-keys % [:id :name]) knob-list)
       ;; A slider shows an unconfirmed local turn, as the Knobs panel does.
       :knob-values (into {} (map (fn [k] [(events/wire-id (:id k)) (or (knobs/draft-value db k) (:value k))])) knob-list)})))

(defn panel
  "Connections panel listing every audio-shaped LFO. It subscribes to its own
  slice of the projection, since the Connections view passes only a few keys."
  []
  (let [{:keys [lfos meter connected? ui knob-list knob-values]} @(rf/subscribe [::view])
        error (:error ui)]
    [:section.audio-lfo-panel.panel {:aria-label "Audio-shaped LFOs"}
     [:div.panel-top [:strong "Audio LFOs"]
      [:span.small.muted "A band of the captured input, one shape per window, morphing into a waveform for any LFO"]]
     [:div.audio-lfo-body
      (if (seq lfos)
        (for [lfo lfos] ^{:key (events/wire-id (:id lfo))} [lfo-card ui lfo meter connected? knob-list knob-values])
        [:p.small.muted "No audio LFOs yet. Each one appears as a waveform in the LFO picker."])
      [:div.audio-lfo-create
       [:input {:type "text" :placeholder "New audio LFO name" :aria-label "New audio LFO name"
                :value (or (:new-name ui) "")
                :on-change #(rf/dispatch [::edit [:new-name] (input-value %)])
                :on-key-down #(when (= "Enter" (.-key %)) (rf/dispatch [::create]))}]
       [:button.button {:type "button" :disabled (not connected?) :on-click #(rf/dispatch [::create])} "Add audio LFO"]]
      (when error [:p.knob-error {:role "alert"} error])]]))
