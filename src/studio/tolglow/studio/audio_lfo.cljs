(ns tolglow.studio.audio-lfo
  "Audio-shaped LFOs (#676). Reads the shared :audio-lfos projection and sends
  the same :audio-lfo/* owner commands as the REPL. The source chart draws the
  last window the capture worker measured; the LFO chart draws the morph from
  the previous shape to the target, as the owner samples it at this frame's
  transport beat. See docs/audio-shaped-lfo.md."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
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

(defn window-label
  "A musical duration such as {:bars 1} as \"1 bar\" or {:beats 2} as \"2 beats\"."
  [duration]
  (if-let [[unit amount] (first duration)]
    (let [unit (events/wire-id unit)
          singular (subs unit 0 (dec (count unit)))]
      (str amount " " (if (== 1 amount) singular unit)))
    "—"))

(defn- choice-value [choices key] (some (fn [[k _ value]] (when (= k key) value)) choices))

(defn form-of
  "Editable form values from a projected LFO configuration."
  [config]
  {:band (events/wire-id (:band config "bass")) :extract (events/wire-id (:extract config "envelope"))
   :window (duration-key windows (:window config)) :morph (duration-key morphs (:morph config))
   :smoothing (str (:smoothing config 0)) :on-silence (events/wire-id (:on-silence config "hold"))})

(defn configure-payload
  "The :audio-lfo/configure fields for a form, or {:error message}."
  [lfo-id form]
  (let [smoothing (js/parseFloat (:smoothing form))]
    (if-not (and (finite? smoothing) (<= 0 smoothing 0.95))
      {:error "Smoothing keeps 0 to 0.95 of the previous shape."}
      {:fields {:lfo-id (keyword (events/wire-id lfo-id))
                :config {:band (keyword (:band form)) :extract (keyword (:extract form))
                         :window (choice-value windows (:window form)) :morph (choice-value morphs (:morph form))
                         :smoothing smoothing :on-silence (keyword (:on-silence form))}}})))

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
          {:keys [error fields]} (configure-payload (:id lfo) form)]
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

(defn- lfo-card [ui lfo meter connected?]
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
      [select "Morph over" (:morph form) morphs (edit! :morph)]
      [:label "Smoothing"
       [:input {:type "range" :min 0 :max 0.95 :step 0.05 :value (:smoothing form) :on-change (edit! :smoothing)}]]
      [select "In silence" (:on-silence form) silence-policies (edit! :on-silence)]
      [:button.button {:type "button" :disabled (not connected?) :on-click #(rf/dispatch [::apply lfo])} "Apply"]]]))

(rf/reg-sub ::view
  (fn [db _]
    {:lfos (get-in db [:show :audio-lfos]) :meter (get-in db [:show :transport :meter])
     :connected? (get-in db [:connection :connected?]) :ui (get-in db ui-path)}))

(defn panel
  "Connections panel listing every audio-shaped LFO. It subscribes to its own
  slice of the projection, since the Connections view passes only a few keys."
  []
  (let [{:keys [lfos meter connected? ui]} @(rf/subscribe [::view])
        error (:error ui)]
    [:section.audio-lfo-panel.panel {:aria-label "Audio-shaped LFOs"}
     [:div.panel-top [:strong "Audio LFOs"]
      [:span.small.muted "A band of the captured input, one shape per window, morphing into a waveform for any LFO"]]
     [:div.audio-lfo-body
      (if (seq lfos)
        (for [lfo lfos] ^{:key (events/wire-id (:id lfo))} [lfo-card ui lfo meter connected?])
        [:p.small.muted "No audio LFOs yet. Each one appears as a waveform in the LFO picker."])
      [:div.audio-lfo-create
       [:input {:type "text" :placeholder "New audio LFO name" :aria-label "New audio LFO name"
                :value (or (:new-name ui) "")
                :on-change #(rf/dispatch [::edit [:new-name] (input-value %)])
                :on-key-down #(when (= "Enter" (.-key %)) (rf/dispatch [::create]))}]
       [:button.button {:type "button" :disabled (not connected?) :on-click #(rf/dispatch [::create])} "Add audio LFO"]]
      (when error [:p.knob-error {:role "alert"} error])]]))
