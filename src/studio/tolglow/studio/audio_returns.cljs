(ns tolglow.studio.audio-returns
  "Per-channel carrier and return assignments in the Studio signal patchbay."
  (:require [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.transit :as wire]))

(def empty-config {:channels 2 :block-frames 1024 :sample-rate 48000 :lanes []})
(defn- epoch [db] ((juxt :runtime-id :epoch) (:show db)))
(defn- saved [db] (merge empty-config (get-in db [:server-projection :view :settings :audio-returns])))
(defn- state [db] (get-in db [:audio-returns (epoch db)]))
(defn- draft [db] (or (:draft (state db)) (saved db)))
(defn assign-lane [config]
  (let [lanes (:lanes config)
        next-free (fn [key] (first (remove (set (map key lanes)) (range 1 (inc (:channels config))))))
        id (first (remove (set (map :id lanes)) (map #(str "return-" %) (range 1 65))))
        send (next-free :send-channel) receive (next-free :return-channel)]
    (if (and send receive)
      (update config :lanes conj {:id id :send-channel send :return-channel receive
                                  :feature :pitch :curve :linear :max-tail {:beats 8}})
      config)))
(defn- checked-params [effects cue]
  (let [effect (get effects (:effect-id cue))]
    (keep (fn [[id schema]] (when (and (#{"number" "integer"} (events/wire-id (:type schema)))
                                       (= 2 (count (:bounds schema)))) id)) (:params effect))))
(defn- ui [db]
  (let [show (:show db) typed (get-in db [:server-projection :view])
        effects (into {} (map (juxt :id identity)) (:effects typed))]
    {:config (draft db) :local (state db) :status (get-in show [:status :audio-returns])
     :saved? (some? (get-in db [:show :settings :audio-returns]))
     :cues (into {} (keep (fn [cue] (when-let [params (seq (checked-params effects cue))]
                                      [(:id cue) {:label (or (:name cue) (events/wire-id (:id cue))) :params params}])) (:cues typed)))}))
(rf/reg-sub :audio-returns/view (fn [db _] (ui db)))
(rf/reg-event-db :audio-returns/edit
  (fn [db [_ path value]]
    (assoc db :audio-returns {(epoch db) (assoc (state db) :draft (assoc-in (draft db) path value))})))
(rf/reg-event-db :audio-returns/lanes
  (fn [db [_ operation i]]
    (let [config (draft db) config (case operation :add (assign-lane config)
                                        :remove (update config :lanes #(into (subvec % 0 i) (subvec % (inc i)))))]
      (assoc db :audio-returns {(epoch db) (assoc (state db) :draft config)}))))
(rf/reg-event-fx :audio-returns/request
  (fn [{:keys [db]} [_ action]]
    (let [identity (epoch db) request (str (random-uuid)) config (draft db)
          metadata {:runtime-id (first identity) :expected-epoch (second identity)}
          partial? (= action :mixers)]
      {:db (assoc db :audio-returns {identity (assoc (state db) :request request :error nil)})
       :studio/http (cond-> {:path (str "/api/audio/returns/" (name action))
                             :typed? (= action :configure)
                             :on-success [:audio-returns/result identity request action config true]
                             :on-failure [:audio-returns/result identity request action config false]}
                      (not partial?) (assoc :body (cond-> metadata (= action :configure) (assoc :config config))))})))
(rf/reg-event-db :audio-returns/result
  (fn [db [_ identity request action submitted success? reply]]
    (if (or (not= (epoch db) identity) (not= request (:request (state db)))) db
      (update-in db [:audio-returns identity]
                 #(cond-> (dissoc % :request)
                    (not success?) (assoc :error reply)
                    (and success? (= :mixers action)) (assoc :mixers reply)
                    (and success? (= :configure action) (= submitted (:draft %))) (dissoc :draft))))))
(defn- select-field [label value options update!]
  [:label [:span label]
   [:select {:aria-label label :value (if (nil? value) "" (events/wire-id value))
             :on-change #(let [v (.. % -target -value)]
                           (update! (some (fn [[id _]] (when (= v (events/wire-id id)) id)) options)))}
    [:option {:value ""} "Choose…"]
    (for [[id title] options] ^{:key (events/wire-id id)} [:option {:value (events/wire-id id)} title])]])
(defn- edit [path value] (rf/dispatch [:audio-returns/edit path value]))
(defn- numeric-input [label value min max change!]
  [:label [:span label] [:input {:type "number" :min min :max max :step "any" :value (if (wire/fraction? value) (wire/fraction-value value) (or value ""))
                                  :on-change #(let [n (.. % -target -valueAsNumber)]
                                                (when (js/Number.isFinite n) (change! n)))}]])
(defn- numeric [label value min max path] [numeric-input label value min max #(edit path %)])
(defn- endpoint [lane index cues side]
  (let [dry? (= side :dry) cue-key (if dry? :dry-cue :wet-cue) param-key (if dry? :dry-param :wet-param)
        path [:lanes index] cue (get lane cue-key) select-cue #(do (edit (conj path cue-key) %)
                                                                   (edit (conj path param-key) nil))]
    [:<>
     [select-field (if dry? "Carrier cue" "Return cue") cue (map (fn [[id c]] [id (:label c)]) (sort-by (comp str key) cues)) select-cue]
     [select-field (if dry? "Carrier parameter" "Return parameter") (get lane param-key)
      (map (fn [p] [p (events/wire-id p)]) (get-in cues [cue :params]))
      #(edit (conj path param-key) %)]]))
(defn- mixer-field [label key config mixers send?]
  (let [list-id (str "live-audio-" (name key))]
    [:label [:span label]
     [:input {:list list-id :value (get config key "") :on-change #(edit [key] (.. % -target -value))}]
     [:datalist {:id list-id}
      (for [m (filter (if send? :send? :return?) mixers)]
        ^{:key (:name m)} [:option {:value (:name m)}])]]))
(defn panel []
  (let [{:keys [config local status cues saved?]} @(rf/subscribe [:audio-returns/view])
        request! #(rf/dispatch [:audio-returns/request %])
        running? (#{"starting" "running"} (:status status))
        busy? (boolean (:request local)) lanes (:lanes config)]
    [:details.patch-envelope.audio-returns
     [:summary "Live audio sends & returns"]
     [:p.small.muted "Send a cue parameter as pitch. Route the processed audio back from a Live track or Drum Rack chain to play a separate return cue."]
     [:div.patch-toolbar
      [:button.button {:disabled busy? :on-click #(request! :mixers)} "List audio mixers"]
      [mixer-field "Carrier output" :output-mixer config (:mixers local) true]
      [mixer-field "Processed input" :input-mixer config (:mixers local) false]
      [numeric "Device channels" (:channels config) 1 64 [:channels]]
      [select-field "Sample rate" (:sample-rate config) (map #(vector % (str % " Hz")) (sort (conj #{44100 48000 88200 96000} (:sample-rate config)))) #(edit [:sample-rate] %)]
      [select-field "Block size" (:block-frames config) (map #(vector % (str % " samples")) (sort (conj #{256 512 1024 2048 4096} (:block-frames config)))) #(edit [:block-frames] %)]]
     (when (and (contains? local :mixers) (empty? (:mixers local))) [:p.small "No audio mixers available on the Studio host."])
     (for [[i lane] (map-indexed vector lanes)]
       ^{:key (:id lane)}
       [:fieldset.patch-route [:legend (:id lane)]
        [:div.patch-controls
         [endpoint lane i cues :dry] [numeric "Send channel" (:send-channel lane) 1 (:channels config) [:lanes i :send-channel]]
         [endpoint lane i cues :wet] [numeric "Return channel" (:return-channel lane) 1 (:channels config) [:lanes i :return-channel]]
         [select-field "Return parameter follows" (:feature lane :pitch) [[:pitch "Pitch"] [:level "Level"] [:texture "Texture"]]
          #(edit [:lanes i :feature] %)]
         [select-field "Response curve" (:curve lane :linear) [[:linear "Linear"] [:smoothstep "Smooth"] [:ease-in "Ease in"] [:ease-out "Ease out"]]
          #(edit [:lanes i :curve] %)]
         (let [[unit value] (first (:max-tail lane {:beats 8})) path [:lanes i :max-tail]]
           [:<> [numeric-input "Tail duration" value 0.001 120000 #(edit path {unit %})]
            [select-field "Tail unit" unit [[:beats "Beats"] [:bars "Bars"] [:phrases "Phrases"] [:ms "Milliseconds"]]
             #(edit path {% value})]])]
        [:button.text-button {:on-click #(rf/dispatch [:audio-returns/lanes :remove i])} "Remove lane"]])
     [:div.patch-actions
      [:button.text-button {:disabled (>= (count lanes) (min 32 (:channels config))) :on-click #(rf/dispatch [:audio-returns/lanes :add])} "+ Add audio lane"]
      [:button.button {:disabled (or busy? (nil? (:draft local))) :on-click #(request! :configure)} "Save audio routing"]
      [:button.button {:disabled (or busy? (and (not running?) (or (:draft local) (empty? lanes) (not saved?))))
                       :on-click #(request! (if running? :stop :start))}
       (if running? "Disconnect audio" "Connect audio")]]
     (when-let [message (or (:error local) (:error status))] [:p.error {:role "alert"} message])
     (when status [:p.small {:role "status"} (str "Audio return " (:status status)
                                                  (when (pos? (:dropped-windows status 0))
                                                    (str " · " (:dropped-windows status) " analysis windows skipped")))])]))
