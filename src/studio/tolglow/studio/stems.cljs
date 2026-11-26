(ns tolglow.studio.stems
  "Instrument stems (#675) in the audio input panel. The editor adds :stems to
  the capture configuration; each live card shows the stem's level, onsets,
  bleed and tempo gate from the capture status, and what it drives. Routing
  buttons submit the same event subscriber and patch saves as tolglow.stems
  in the REPL, built by tolglow.audio.stem-routes. See docs/stems.md."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.audio.stem-routes :as stem-routes]
            [tolglow.studio.events :as events]
            [tolglow.studio.patchbay :as patchbay]
            [tolglow.studio.subscriber-editor]))

(def max-stems 16)

(def preset-labels
  {:kick "Kick" :snare "Snare" :hats "Hats" :bass "Bass" :guitar "Guitar" :vocals "Vocals" :keys "Keys" :full "Full range"})

(defn- wire [x] (events/wire-id x))

(defn- text [event] (.. event -target -value))

(defn form-rows
  "Editable stem rows from a saved capture configuration."
  [config]
  (mapv (fn [{:keys [id channel preset]}] {:id (wire id) :channel (str channel) :preset (if preset (wire preset) "full")})
        (:stems config)))

(defn payload
  "Validated :stems for a capture of `channels` channels, or nil without rows.
  Throws a readable error, as the rest of the audio form does."
  [rows channels]
  (when (seq rows)
    (when (> (count rows) max-stems) (throw (js/Error. "Use at most 16 stems.")))
    (let [stems (mapv (fn [{:keys [id channel preset]}]
                        (let [id (str/trim (str id)) n (js/Number channel)]
                          (when-not (re-matches #"[a-z][a-z0-9-]{0,31}" id)
                            (throw (js/Error. (str "Name stems in lowercase, such as kick or bass-di (\"" id "\")."))))
                          (when-not (and (js/Number.isInteger n) (<= 1 n channels))
                            (throw (js/Error. (str "Stem " id " needs a channel from 1 to " channels "."))))
                          {:id (keyword id) :channel n :preset (keyword (or (not-empty preset) "full"))}))
                      rows)]
      (when-not (apply distinct? (map :id stems)) (throw (js/Error. "Each stem needs its own name.")))
      stems)))

(defn- edit! [path value] (rf/dispatch-sync [:connections/edit path value]))

(defn- show-identity [db] {:runtime-id (get-in db [:show :runtime-id]) :expected-epoch (get-in db [:show :epoch])})

(defn current-spec
  "The running patch exactly as saved: the projection's EDN source keeps
  keyword identities that the JSON view turns into text."
  [db]
  (when-let [source (get-in db [:show :patch :source])]
    (try (reader/read-string source) (catch :default _ nil))))

(rf/reg-event-fx :stems/load-subscribers
  (fn [_ _] {:subscribers/http {:ok [:stems/subscribers] :fail [:stems/failed]}}))

(rf/reg-event-db :stems/subscribers
  (fn [db [_ catalog]] (-> db (assoc-in [:stems :subscribers] (vec (:subscribers catalog))) (update :stems dissoc :error))))

(rf/reg-event-db :stems/failed
  (fn [db [_ message]] (-> db (assoc-in [:stems :error] (events/error-message message)) (update :stems dissoc :pending))))

(rf/reg-event-fx :stems/route-onset
  (fn [{:keys [db]} [_ stem cue-id]]
    {:db (assoc-in db [:stems :pending] [stem :onset])
     :subscribers/http {:body (merge (show-identity db)
                                     {:op :save :subscriber (stem-routes/onset-subscriber (keyword stem) (keyword cue-id))})
                        :ok [:stems/routed (str "Every " stem " onset now launches " cue-id ".")] :fail [:stems/failed]}}))

(rf/reg-event-fx :stems/unroute-onset
  (fn [{:keys [db]} [_ subscriber-id]]
    {:subscribers/http {:body (merge (show-identity db) {:op :remove :id (keyword (wire subscriber-id))})
                        :ok [:stems/routed (str "Removed " (wire subscriber-id) ".")] :fail [:stems/failed]}}))

(rf/reg-event-fx :stems/route-level
  (fn [{:keys [db]} [_ stem cue-id param]]
    (let [spec (stem-routes/with-level-route (current-spec db) (keyword stem) (keyword cue-id) (keyword param) {})]
      {:db (assoc-in db [:stems :pending] [stem :level])
       :studio/http {:path "/api/patch/save" :body (merge (show-identity db) {:source (pr-str spec)})
                     :on-success [:stems/level-saved stem cue-id param] :on-failure [:stems/failed]}})))

(rf/reg-event-fx :stems/level-saved
  (fn [{:keys [db]} [_ stem cue-id param]]
    {:studio/http {:path "/api/patch/enabled" :body (merge (show-identity db) {:enabled? true})
                   :on-success [:stems/routed (str stem " level now drives " cue-id " · " param ".")]
                   :on-failure [:stems/failed]}}))

(rf/reg-event-fx :stems/routed
  (fn [{:keys [db]} [_ notice]]
    {:db (-> db (assoc-in [:stems :notice] notice) (update :stems dissoc :pending :error))
     :dispatch [:stems/load-subscribers]}))

(rf/reg-sub :stems/state (fn [db _] (:stems db)))

(rf/reg-sub :stems/show (fn [db _] {:show (:show db) :spec (current-spec db)}))

(defn- level-text [level-db] (if (number? level-db) (str (.toFixed level-db 1) " dB") "—"))

(defn- route-lines [cues {:keys [onsets ports]}]
  (let [cue-name (fn [id] (or (some #(when (= (wire id) (wire (:id %))) (:name %)) cues) (wire id)))]
    (concat
     (for [{:keys [subscriber-id enabled? actions]} onsets
           {:keys [op select]} actions :when (= "cue/launch" (wire op))]
       ^{:key (str "onset-" (wire subscriber-id))}
       [:li.stem-route {:data-kind "onset"}
        [:span (str "Onset → launch " (str/join ", " (map cue-name (:ids select))) (when-not enabled? " · disabled"))]
        [:button.icon-button {:type "button" :aria-label (str "Remove " (wire subscriber-id))
                              :on-click #(rf/dispatch [:stems/unroute-onset subscriber-id])} "×"]])
     (for [{:keys [route-id feature cue-id param]} ports]
       ^{:key (str "port-" (wire route-id))}
       [:li.stem-route {:data-kind "level"} [:span (str (str/capitalize (wire feature)) " → " (cue-name cue-id) " · " (wire param))]]))))

(defn- route-form [stem show busy?]
  (let [choice (r/atom {:cue "" :param ""})]
    (fn [stem show busy?]
      (let [{:keys [cue param]} @choice
            params (patchbay/parameter-options show cue)]
        [:div.stem-route-form
         [:label [:span "Cue"]
          [:select {:aria-label (str stem " cue") :value cue :on-change #(reset! choice {:cue (text %) :param ""})}
           [:option {:value ""} "Choose a cue"]
           (for [c (:cues show)] ^{:key (wire (:id c))} [:option {:value (wire (:id c))} (or (:name c) (wire (:id c)))])]]
         [:button.button {:type "button" :disabled (or busy? (str/blank? cue))
                          :on-click #(rf/dispatch [:stems/route-onset stem cue])} "Launch on onset"]
         [:label [:span "Parameter"]
          [:select {:aria-label (str stem " parameter") :value param :disabled (str/blank? cue)
                    :on-change #(swap! choice assoc :param (text %))}
           [:option {:value ""} "Numeric parameter"]
           (for [{:keys [id label]} params] ^{:key id} [:option {:value id} label])]]
         [:button.button {:type "button" :disabled (or busy? (str/blank? cue) (str/blank? param))
                          :on-click #(rf/dispatch [:stems/route-level stem cue param])} "Drive with level"]]))))

(defn- stem-card
  "One live stem. The onset lamp lights for a moment whenever the onset count
  advances between status updates."
  []
  (let [seen (r/atom {:count nil :lit-until 0})]
    (fn [{:keys [id channel preset]} feature routes show busy?]
      (let [count (:onset-count feature) now (js/Date.now)
            _ (when (and (number? count) (not= count (:count @seen)))
                (reset! seen {:count count :lit-until (if (some? (:count @seen)) (+ now 180) 0)})
                (js/setTimeout #(swap! seen assoc :tick (js/Date.now)) 200))
            lit? (< now (:lit-until @seen))
            stem (wire id) level (max 0 (min 1 (or (:level feature) 0)))]
        [:article.stem-card {:aria-label (str "Stem " stem)}
         [:div.stem-head
          [:i.stem-onset {:class (when lit? "lit") :aria-hidden true}]
          [:strong stem]
          [:span.stem-meta (str "Ch " channel " · " (get preset-labels (keyword (wire preset)) (wire preset)))]]
         [:div.stem-level
          [:meter {:min 0 :max 1 :value level :aria-label (str stem " level")}]
          [:span (level-text (:level-db feature))]]
         [:div.stem-metrics
          [:div [:small "Onsets"] [:strong (or count "—")]]
          [:div [:small "Bleed rejected"] [:strong (or (:bleed-count feature) "—")]]
          [:div [:small "Gate"] [:strong (if (number? (:gate-ms feature))
                                           (str (js/Math.round (:gate-ms feature)) " ms"
                                                (when (number? (:bpm feature)) (str " @ " (js/Math.round (:bpm feature)) " BPM")))
                                           "—")]]
          [:div [:small "Last onset"] [:strong (if (number? (:last-onset-beat feature))
                                                 (str "beat " (.toFixed (:last-onset-beat feature) 2)) "—")]]]
         (let [lines (route-lines (:cues show) routes)]
           (if (seq lines) [:ul.stem-routes lines] [:p.small.muted "Not routed yet."]))
         [route-form stem show busy?]]))))

(defn- editor [s width running?]
  (let [rows (get-in s [:audio-form :stems] [])
        disabled? (or running? (contains? (:pending s) :audio-start))]
    [:div.stem-editor
     (for [[index {:keys [id channel preset]}] (map-indexed vector rows)]
       ^{:key index}
       [:div.stem-row
        [:label [:span "Name"] [:input {:value id :placeholder "kick" :disabled disabled? :max-length 32
                                        :on-change #(edit! [:audio-form :stems index :id] (str/lower-case (text %)))}]]
        [:label [:span "Channel"]
         [:select {:value channel :disabled disabled? :on-change #(edit! [:audio-form :stems index :channel] (text %))}
          (for [n (range 1 (inc (max 1 width)))] ^{:key n} [:option {:value (str n)} n])]]
        [:label [:span "Instrument"]
         [:select {:value preset :disabled disabled? :on-change #(edit! [:audio-form :stems index :preset] (text %))}
          (for [p stem-routes/presets] ^{:key p} [:option {:value (name p)} (preset-labels p)])]]
        [:button.icon-button {:type "button" :disabled disabled? :aria-label (str "Remove stem " (inc index))
                              :on-click #(edit! [:audio-form :stems] (vec (concat (take index rows) (drop (inc index) rows))))} "×"]])
     [:button.text-button {:type "button" :disabled (or disabled? (>= (count rows) max-stems))
                           :on-click #(edit! [:audio-form :stems]
                                             (conj (vec rows) {:id "" :channel (str (min (max 1 width) (inc (count rows)))) :preset "full"}))}
      "+ Add stem"]]))

(def panel
  (r/create-class
   {:display-name "InstrumentStems"
    :component-did-mount (fn [_] (rf/dispatch [:stems/load-subscribers]))
    :reagent-render
    (fn [s]
      (let [{:keys [subscribers pending error notice]} @(rf/subscribe [:stems/state])
            {:keys [show spec]} @(rf/subscribe [:stems/show])
            audio (:audio s) running? (contains? #{"starting" "running" "stopping"} (:state audio))
            width (js/Number (get-in s [:audio-form :channels]))
            configured (get-in audio [:config :stems])
            features (get-in audio [:features :stems])
            routes (stem-routes/routes subscribers spec)
            feature-of (fn [id] (or (get features (keyword (wire id))) (get features (wire id))))]
        [:details.stem-panel {:open (boolean (or (seq configured) (seq (get-in s [:audio-form :stems]))))}
         [:summary [:span.section-label "INSTRUMENT STEMS"]
          [:span.small.muted (if (seq configured) (str (count configured) " stems from this capture") "Name channels of a multichannel input")]]
         [:p.small.muted "Each stem reads one channel: its onsets can launch cues and its level can drive a parameter. While the transport is stopped, stems launch nothing and their routes hold."]
         ;; Stems are part of the capture configuration: edit them while stopped.
         (if (and running? (seq configured))
           [:p.small.muted "Stop capture to rename stems or change their channels."]
           [editor s width running?])
         (when (and running? (seq configured))
           [:div.stem-cards
            (for [stem configured]
              ^{:key (wire (:id stem))}
              [stem-card stem (feature-of (:id stem)) (get routes (keyword (wire (:id stem)))) show (some? pending)])])
         (when error [:p.connection-error {:role "alert"} error])
         (when notice [:p.small {:role "status"} notice])]))}))
