(ns tolglow.authoring.variation-control-test
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.display :as presentation]
            [afterglow.control.devices.push-3.display :as push-3-display]
            [afterglow.control.devices.session :as session]
            [clojure.test :refer [deftest is testing]]
            [tolglow.authoring.variation-control :as variations]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.display :as display]
            [tolglow.transit :as transit])
  (:import [java.awt.image BufferedImage]
           [java.io ByteArrayInputStream]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defn- fixture-model []
  (-> (demo/model)
      (assoc-in [:registry :effects :ember :params :period]
                {:type :number :default 1 :bounds [1/4 4] :semantic/type :beat-ratio
                 :ratio-components {:numerator [1 9] :denominator [1 11]}})
      (assoc-in [:registry :effects :ember :params :tint]
                {:type :color :default [0.9 0.4 0.15]})
      (assoc-in [:registry :effects :ember :param-order] [:period :level :tint])))

(defn- model [runtime]
  (:model @(:state runtime)))

(defn- submit! [runtime payload]
  (let [reply (show/submit! runtime payload)]
    (show/tick! runtime 0)
    @reply))

(defn- while-ticking
  ([runtime f] (while-ticking runtime 0 f))
  ([runtime time f]
   (let [operation (future (f))
         deadline (+ (System/nanoTime) (long 6000000000))]
     (try
       (loop []
         (cond (realized? operation) @operation
               (> (System/nanoTime) deadline) (throw (ex-info "Owner command did not settle" {}))
               :else (do (show/tick! runtime time) (Thread/sleep 2) (recur))))
       (finally (future-cancel operation))))))

(defn- post [runtime handler uri payload]
  (let [response (while-ticking runtime
                   #(handler {:request-method :post :uri uri
                              :headers {"host" "localhost" "content-type" "application/transit+json"
                                        "accept" "application/transit+json"}
                              :body (ByteArrayInputStream. (.getBytes (transit/encode payload) "UTF-8"))}))]
    [(:status response) (transit/decode (:body response))]))

(defn- capture []
  (let [receive (atom nil) sent (atom []) views (atom []) awaiting? (atom false)
        ack (codec/sysex 0 [0x09])]
    {:midi {:open! #(do (reset! receive %) :capture)
            :send! (fn [_ batch] (swap! sent into batch)
                     (when (= ack (peek batch)) (reset! awaiting? true)))
            :close! (fn [_])}
     :display {:open! (constantly :capture) :render! (fn [_ view] (swap! views conj view)) :close! (fn [_])}
     :receive! #(@receive %)
     :messages sent :views views
     :setup! (fn [] (doseq [index (range 128)]
                      (@receive (assoc (codec/palette-write 0 index [0 0 0 73]) 6 4))))
     :settle! (fn []
                (loop [count 0]
                  (when (compare-and-set! awaiting? true false)
                    (when (>= count 9) (throw (ex-info "Push feedback did not settle" {})))
                    (@receive ack)
                    (recur (inc count)))))}))

(defn- choice [projection op position]
  (assoc (select-keys projection [:basis :layout :seed :row-page :instance-id]) :op op :position position))

(deftest cue-variations-use-one-owner-from-push-midi-and-studio-ring
  (let [runtime (show/create-runtime {:model (fixture-model) :workspace-options nil})
        {:keys [receive! setup! settle! messages views] :as sink} (capture)]
    (try
      (let [handler (studio/handler runtime)
            device (show/connect-device! runtime :push-2
                     (merge {:session-id "variations-capture" :schedule! nil :timeout-ms 60000
                             :active-cue-grid? true :cue-variations? true}
                            (select-keys sink [:midi :display])))
            raw! #(while-ticking runtime (fn [] (receive! %)))
            current #(get-in (model runtime) [:instances (:selected-instance (:local (session/snapshot device)))])
            button! (fn [controller] (raw! [0xb0 controller 127]) (receive! [0xb0 controller 0]))
            pad! (fn [[x y]] (let [note (codec/pad-note x y)] (raw! [0x90 note 100]) (receive! [0x80 note 0])))
            envelope #(select-keys (model runtime) [:show-id :epoch])]
        (setup!)
        (settle!)
        (is (true? (get-in (session/snapshot device) [:local :ready?]))
            (pr-str (select-keys (session/snapshot device) [:errors :failure])))
        (pad! [0 0])
        (settle!)
        (is (some? (current))
            (pr-str (select-keys (session/snapshot device) [:last-ack :errors :failure])))
        (let [{instance-id :id before :params} (current)
              other (:instance-id (submit! runtime {:op :cue/launch :cue-id :ice}))
              _ (submit! runtime {:op :instance/opacity-set :instance-id instance-id :value 0.31})
              binding {:type :lfo :waveform :saw :period {:beats 1} :from 0.0 :to 1.0}
              bound (show/submit! runtime :modulation-bind {:instance-id instance-id :param :level :binding binding})]
          (show/tick! runtime 0)
          (is (= :accepted (:status @bound)))
          (is (string? instance-id))
          (is (not= instance-id other))
          (let [project! (fn [layout] (show/cue-variation-preview runtime {:instance-id instance-id :layout layout}))
                full (project! :full)
                [status preview] (post runtime handler "/api/cue-variations/preview"
                                       (assoc (envelope) :instance-id instance-id :layout :full))
                [position params] ((juxt :position :params) (first (:cells full)))
                bindings-before (:parameter-bindings (current))]
            (is (= [200 full] [status preview]))
            (is (= #{:level :period :tint} (set (keys params))))
            (button! 50)
            (is (= :full (get-in (peek @views) [:pad-overlay :layout])))
            (pad! position)
            (is (= before (:params (current))) "Unacknowledged paints cannot audition")
            (settle!)
            (is (some #{(codec/palette-write 0 (apply codec/pad-note position) [220 110 32 73])} @messages))
            (pad! position)
            (is (= params (:params (current)))
                "The MIDI cell commits the same full prospective state returned to Studio")
            (is (= 0.31 (get-in (current) [:system :opacity])))
            (is (= bindings-before (:parameter-bindings (current))))
            (is (nil? (get-in (model runtime) [:modulation :holds instance-id :level])))
            (is (= 0.7 (get-in (model runtime) [:instances other :params :level])))
            (is (= 422 (first (post runtime handler "/api/command"
                               (merge (envelope) (choice full :cue-variation/apply position))))))
            (settle!)
            (receive! [0xb0 86 127])
            (button! 50)
            (receive! [0xb0 86 0])
            (settle!)
            (is (= [{:position [0 0] :kind :saved}]
                   (filterv #(= :saved (:kind %)) (get-in (peek @views) [:pad-overlay :cells]))))
            (is (some #{(codec/palette-write 0 36 [25 155 200 73])} @messages))
            (button! 50)
            (settle!)
            (is (= :rows (get-in (peek @views) [:pad-overlay :layout])))
            (let [rows (project! :rows)
                  cell (first (filter #(= :level (:param %)) (:cells rows)))
                  other-params (dissoc (:params (current)) :level)]
              (is (= [:period :level :tint] (mapv :param (:rows rows))))
              (is (= 200 (first (post runtime handler "/api/command"
                                 (merge (envelope) (choice rows :cue-variation/apply (:position cell)))))))
              (is (= other-params (dissoc (:params (current)) :level)))
              (is (= (:value cell) (get-in (current) [:params :level]))))
            (session/refresh! device)
            (settle!)
            (button! 48)
            (is (nil? (get-in (session/snapshot device) [:local :active-grid :mode])))
            (let [saved-position [0 0] note (apply codec/pad-note saved-position)]
              (raw! [0x90 note 100])
              (is (= params (:params (current))))
              (button! 50)
              (settle!)
              (receive! [0xa0 note 127])
              (receive! [0x80 note 0])
              (is (= 2 (count (:instances (model runtime))))))
            (let [base {:op :cue-variation/ratio-set :instance-id instance-id :param :period
                        :numerator 1 :denominator 1}]
              (is (= :accepted (:status (submit! runtime base))))
              (raw! [0xb0 71 1])
              (let [ratio (get-in (current) [:params :period])]
                (is (and (ratio? ratio) (< 1 ratio)))
                (is (= :accepted (:status (submit! runtime base))))
                (receive! [0xb0 49 127])
                (raw! [0xb0 71 1])
                (receive! [0xb0 49 0])
                (is (= ratio (get-in (current) [:params :period])))
                (is (= :accepted (:status (submit! runtime base))))
                (raw! [0x90 0 127])
                (raw! [0xb0 71 1])
                (raw! [0x80 0 0])
                (let [{:keys [cursor nodes open position]} (get-in (model runtime) [:control :edit-history])
                      {:keys [target before after actor]} (get nodes cursor)
                      undo (show/submit! runtime :command
                             {:op :history/undo :expected-cursor cursor :expected-history-position position}
                             {:operator-id "variation-capture-repl"})]
                  (is (= [:instance-param instance-id :period] target))
                  (is (= [1 ratio] [before after]))
                  (is (re-find #"^push-2/variations-capture/" actor))
                  (is (empty? open))
                  (show/tick! runtime 0)
                  (is (= :accepted (:status @undo)))
                  (is (= 1 (get-in (current) [:params :period]))))))
            (testing "a captured temporary turn also uses the exact editor and restores its starting ratio"
              (let [history #(get-in (model runtime) [:control :edit-history])
                    position (select-keys (history) [:cursor :position :nodes])]
                (is (= :accepted (:status (submit! runtime {:op :cue-variation/ratio-mode
                                                           :instance-id instance-id :mode :free}))))
                (is (= :accepted (:status (submit! runtime {:op :cue-variation/ratio-set :instance-id instance-id
                                                           :param :period :numerator 5 :denominator 11}))))
                (receive! [0xb0 57 127])
                (raw! [0x90 0 127])
                (receive! [0xb0 57 0])
                (raw! [0xb0 71 1])
                (let [audition (get-in (current) [:params :period])]
                  (is (or (ratio? audition) (integer? audition)))
                  (is (not= 5/11 audition)))
                (raw! [0x80 0 0])
                (is (= 5/11 (get-in (current) [:params :period])))
                (is (= position (select-keys (history) [:cursor :position :nodes])))
                (is (empty? (:open (history))))))
            (is (empty? (:errors (session/snapshot device)))))))
      (finally (show/stop! runtime)))))

(deftest push-3-encoder-uses-the-same-exact-ratio-editor
  (let [runtime (show/create-runtime {:model (fixture-model) :workspace-options nil})
        receive (atom nil)
        options {:session-id "push3-ratio-capture" :user-mode? true :expression-mode :poly-aftertouch
                 :cue-variations? true :schedule! nil
                 :layout {:encoder-touches {:period {:channel 2 :note 9}}
                          :encoders {:period {:channel 2 :controller 19}}}
                 :encoder-bindings {:period {:param :period}}
                 :display {:device :push-3 :input :view :open! (constantly :capture-display)
                           :render! (fn [_ _]) :close! (fn [_])}
                 :midi {:open! (fn [callback] (reset! receive callback) :capture)
                        :send! (fn [_ _] (throw (ex-info "Unexpected Push 3 MIDI output" {})))
                        :close! (fn [_])}}]
    (try
      (let [{id :instance-id} (submit! runtime {:op :cue/launch :cue-id :ember})
            free-mode {:op :cue-variation/ratio-mode :instance-id id :mode :free}
            set-ratio {:op :cue-variation/ratio-set :instance-id id :param :period :numerator 5 :denominator 11}
            _ (is (= :accepted (:status (submit! runtime free-mode))))
            _ (is (= :accepted (:status (submit! runtime set-ratio))))
            device (show/connect-device! runtime :push-3 options)
            current #(get-in (model runtime) [:instances id])
            ratio #(get-in (current) [:params :period])
            expected (:value (variations/edit-parameter (model runtime) (current) :period 1 {:mode :relative}))
            input! (fn [time bytes]
                       (while-ticking runtime time #(@receive bytes))
                       (show/tick! runtime time))]
        (is (= id (get-in (session/snapshot device) [:local :selected-instance])))
        (is (= 5/11 (ratio)))
        (is (and (ratio? expected) (not= 5/11 expected)))
        (input! 20 [0xb2 19 1])
        (is (= expected (ratio)))
        (let [reply (show/submit! runtime set-ratio)]
          (show/tick! runtime 30)
          (is (= :accepted (:status @reply))))
        (input! 40 [0xb0 49 127])
        (input! 50 [0xb0 57 127])
        (input! 60 [0x92 9 127])
        (input! 70 [0xb2 19 1])
        (is (= expected (ratio)))
        (input! 80 [0xb0 57 0])
        (input! 90 [0x82 9 0])
        (is (= 5/11 (ratio)))
        (input! 100 [0xb0 49 0])
        (is (empty? (get-in (model runtime) [:control :edit-history :open])))
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest push-3-pad-selection-waits-for-transfer-and-compares-the-presented-owner-basis
  (let [runtime (show/create-runtime {:model (fixture-model) :workspace-options nil})
        receive (atom nil)
        transfer (atom nil)
        staged (LinkedBlockingQueue.)
        image (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB)
        sink (push-3-display/sink {:backend {:open! (constantly :captured)
                                            :write! (fn [_ _]) :close! (fn [_])}
                                  :schedule! #(do (reset! transfer %) (fn []))})
        screen (assoc (display/transport
                        {:presentation-receipts? true :max-fps 60
                         :capture {:open! (constantly :capture) :capture! (fn [_ view] view) :close! (fn [_])}
                         :sink (assoc sink :render-with-receipt!
                                           (fn [handle view receipt]
                                             ((:render-with-receipt! sink) handle image receipt)
                                             (.add staged
                                                   (if-let [overlay (:pad-overlay view)]
                                                     (assoc overlay :captured-level
                                                            (some #(when (= :level (:param %)) (:value %))
                                                                  (:parameters view)))
                                                     {}))))})
                      :device :push-3 :input :view)
        options {:session-id "push3-variation-capture" :user-mode? true :expression-mode :poly-aftertouch
                 :cue-variation-pads? true :schedule! nil :display screen
                 :layout {:pads {[0 0] {:channel 3 :note 12} [0 1] {:channel 3 :note 13}}
                          :encoder-touches {:period {:channel 2 :note 9}}
                          :encoders {:period {:channel 2 :controller 19}}
                          :buttons {:cycle {:channel 4 :controller 28} :record {:channel 4 :controller 29}
                                    :ratio {:channel 4 :controller 30}}}
                 :encoder-bindings {:period {:param :period}}
                 :button-bindings {:cycle :variation/cycle :record :variation/record
                                   :ratio :variation/ratio-mode}
                 :midi {:open! #(do (reset! receive %) :captured)
                        :send! (fn [_ _] (throw (ex-info "Unexpected Push 3 MIDI output" {})))
                        :close! (fn [_])}}
        next-frame! (fn [expected?]
                      (loop [attempt 0]
                        (let [frame (.poll staged 2 TimeUnit/SECONDS)]
                          (cond
                            (and frame (expected? frame)) frame
                            (or (nil? frame) (= 15 attempt))
                            (throw (ex-info "Expected captured display projection" {:frame frame}))
                            :else (recur (inc attempt))))))]
    (try
      (let [{id :instance-id} (submit! runtime {:op :cue/launch :cue-id :ember})
            device (show/connect-device! runtime :push-3 options)
            current #(get-in (model runtime) [:instances id :params])
            projection (fn [layout] (show/cue-variation-preview runtime {:instance-id id :layout layout}))
            before (current)
            packet! #(while-ticking runtime (fn [] (@receive %)))
            cc! (fn [button value] (packet! [0xb4 button value]))
            button! (fn [button] (cc! button 127) (cc! button 0))
            pad! (fn [note] (packet! [0x93 note 100]) (packet! [0x93 note 0]))
            saved? #(some (fn [cell] (= :saved (:kind cell))) (:cells %))]
        (next-frame! empty?)
        (button! 28)
        (let [full (next-frame! #(= :full (:layout %)))]
          (is (= [[0 1]] (mapv :position (:cells full)))))
        (pad! 13)
        (is (= before (current)))
        (@transfer)
        (cc! 29 127)
        (button! 28)
        (cc! 29 0)
        (let [frame (next-frame! saved?)]
          (is (= [[:saved [0 0]] [:generated [0 1]]]
                 (mapv (juxt :kind :position) (:cells frame)))))
        (@transfer)
        (is (= :accepted (:status (submit! runtime {:op :param/set :instance-id id
                                                   :param :level :value 0.77}))))
        (packet! [0x93 13 100])
        (is (= :stale-variation (get-in (session/snapshot device) [:last-ack :code])))
        (is (= 0.77 (:level (current))))
        (next-frame! #(and (saved? %) (= 0.77 (:captured-level %))))
        (@transfer)
        (packet! [0x93 13 100])
        (is (= 0.77 (:level (current))) "A rejected pad remains overlay-owned until lift")
        (packet! [0x93 13 0])
        (let [choice (some #(when (= [0 1] (:position %)) %) (:cells (projection :full)))]
          (pad! 13)
          (is (= (:params choice) (select-keys (current) (keys (:params choice))))))
        (next-frame! #(and (saved? %) (= (:level (current)) (:captured-level %))))
        (@transfer)
        (pad! 12)
        (is (= before (current)))
        (button! 28)
        (let [rows (next-frame! #(= :rows (:layout %)))]
          (is (= :period (get-in rows [:rows 0 :param]))))
        (button! 30)
        (is (= :preferred (:ratio-mode (projection :rows))))
        (@transfer)
        (button! 30)
        (is (= :free (:ratio-mode (projection :rows))))
        (next-frame! #(= :free (:ratio-mode %)))
        (@transfer)
        (is (= :accepted (:status (submit! runtime {:op :param/set :instance-id id
                                                   :param :level :value 0.61}))))
        (session/refresh! device)
        (next-frame! #(and (saved? %) (= 0.61 (:captured-level %))))
        (@transfer)
        (cc! 29 127)
        (pad! 12)
        (cc! 29 0)
        (is (= :accepted (get-in (session/snapshot device) [:last-ack :status])))
        (let [saved (filter #(= :saved (:kind %)) (:cells (projection :rows)))]
          (is (= [0.61] (mapv #(get-in % [:params :level]) saved))))
        (let [period (:period (current))
              encoder! (fn [time bytes] (while-ticking runtime time #(@receive bytes)) (show/tick! runtime time))]
          (encoder! 10 [0xb0 49 127])
          (encoder! 20 [0xb0 57 127])
          (encoder! 30 [0x92 9 127])
          (encoder! 40 [0xb2 19 1])
          (is (and (ratio? (:period (current))) (not= period (:period (current)))))
          (encoder! 50 [0x82 9 0])
          (is (= period (:period (current))))
          (encoder! 60 [0xb0 57 0])
          (encoder! 70 [0xb0 49 0]))
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! runtime)))))

(defn- ratio-pad-session [runtime mode pads options]
  (let [callback (atom nil) surface (atom nil)
        captured {:session-id "push3-encoder-chooser" :user-mode? true :schedule! nil :expression-mode mode
                  :cue-variation-pads? true
                  :layout {:pads (update-vals pads #(hash-map :channel 3 :note %))
                           :encoder-touches {:period {:channel 2 :note 9} :level {:channel 2 :note 10}}
                           :encoders {:period {:channel 2 :controller 19} :level {:channel 2 :controller 20}}
                           :buttons {:cycle {:channel 4 :controller 28}}}
                  :encoder-bindings {:period {:param :period} :level {:param :level}}
                  :button-bindings {:cycle :variation/cycle}
                  :display {:device :push-3 :input :view :presentation-receipts? true
                            :open! (constantly :capture) :render! (fn [_ view] (reset! surface view))
                            :close! (fn [_])}
                  :midi {:open! #(do (reset! callback %) :capture) :close! (fn [_])
                         :send! (fn [_ _] (throw (ex-info "Unexpected Push 3 MIDI output" {})))}}
        device (show/connect-device! runtime :push-3 (merge captured options))
        receive! #(@callback %)
        packet! #(while-ticking runtime (fn [] (receive! %)))
        overlay #(:pad-overlay @surface)]
    {:device device :receive! receive! :packet! packet! :view #(deref surface)
     :press! #(packet! [0x93 % 100]) :lift! #(packet! [0x83 % 0])
     :present! #(presentation/presented! @surface) :overlay overlay
     :cell (fn [position] (some #(when (= position (:position %)) %) (:cells (overlay))))}))

(defn- exercise-ratio-pads [mode]
  (let [runtime (show/create-runtime {:model (fixture-model) :workspace-options nil})]
    (try
      (let [{id :instance-id} (submit! runtime {:op :cue/launch :cue-id :ember})
            {:keys [device packet! press! lift! present! overlay cell]}
            (ratio-pad-session runtime mode {[0 7] 13 [1 6] 14 [3 7] 15 [0 6] 16} {})
            instance #(get-in (model runtime) [:instances id])
            param #(get-in (instance) [:params %])
            history #(get-in (model runtime) [:control :edit-history])]
        (packet! [0x92 9 127])
        (is (= :beat-ratios (:layout (overlay))))
        (is (= {:position [1 6] :label "4/3" :selected? false} (cell [1 6])))
        (is (nil? (cell [0 7])))
        (is (true? (:selected? (cell [0 6]))))
        (press! 14)
        (present!)
        (press! 14)
        (is (= 1 (param :period)))
        (lift! 14)
        (packet! [0x92 10 127])
        (let [level (param :level)]
          (packet! [0xb2 20 1])
          (is (> (param :level) level)))
        (packet! [0x82 10 0])
        (is (= :beat-ratios (:layout (overlay))))
        (press! 13)
        (lift! 13)
        (is (= 1 (param :period)))
        (present!)
        (press! 14)
        (is (= 4/3 (param :period)))
        (is (= "4/3" (:current-ratio (overlay))))
        (is (true? (:selected? (cell [1 6]))))
        (is (= #{id} (set (keys (:instances (model runtime))))))
        (let [expected (:value (variations/edit-parameter (model runtime) (instance) :period 1 {:mode :relative}))]
          (packet! [0xb2 19 1])
          (is (= expected (param :period)))
          (is (not= 4/3 expected)))
        (packet! [0x82 9 0])
        (is (nil? (overlay)))
        (present!)
        (lift! 14)
        (is (empty? (:open (history))))
        (let [last-change (get-in (history) [:nodes (:cursor (history))])]
          (is (= [:instance-param id :period] (:target last-change)))
          (is (= 1 (:before last-change)))
          (is (= (param :period) (:after last-change))))
        (doseq [[approximate exact position note] [[0.3333333333333333 1/3 [3 7] 15]
                                                   [1.0 1 [0 6] 16]]]
          (is (= :accepted (:status (submit! runtime {:op :param/set :instance-id id
                                                      :param :period :value approximate}))))
          (is (= approximate (param :period)))
          (packet! [0x92 9 127])
          (is (false? (:selected? (cell position))))
          (present!)
          (press! note)
          (is (= exact (param :period)))
          (is (= (str exact) (:current-ratio (overlay))))
          (is (true? (:selected? (cell position))))
          (lift! note)
          (packet! [0x82 9 0])
          (present!))
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest push-3-ratio-pads-share-the-encoder-gesture-and-exact-owner-value
  (doseq [mode [:poly-aftertouch :mono-aftertouch]]
    (testing (name mode)
      (exercise-ratio-pads mode))))

(deftest push-3-free-ratios-include-the-current-value-at-a-bounded-grid-position
  (let [runtime (show/create-runtime {:model (fixture-model) :workspace-options nil})]
    (try
      (let [{id :instance-id} (submit! runtime {:op :cue/launch :cue-id :ember})
            {:keys [packet! overlay cell]} (ratio-pad-session runtime :poly-aftertouch
                                             {[0 6] 16 [1 6] 14} {})]
        (is (= :accepted (:status (submit! runtime {:op :cue-variation/ratio-mode :instance-id id :mode :free}))))
        (is (= :accepted (:status (submit! runtime {:op :cue-variation/ratio-set :instance-id id
                                                    :param :period :numerator 5 :denominator 11}))))
        (packet! [0x92 9 127])
        (is (= :free (:ratio-mode (overlay))))
        (is (= "4/3" (:label (cell [1 6]))))
        (is (= "5/11" (:label (cell [0 6]))))
        (is (= "5/11" (:current-ratio (overlay))))
        (is (= ["5/11"] (mapv :label (filter :selected? (:cells (overlay))))))
        (is (= 2 (count (:cells (overlay)))))
        (packet! [0x82 9 0]))
      (finally (show/stop! runtime)))))

(deftest push-3-free-mode-reserves-the-only-captured-pad-for-an-actionable-ratio
  (doseq [[position current] [[[0 6] 1/3] [[1 6] 4/3]]]
    (let [runtime (show/create-runtime {:model (fixture-model) :workspace-options nil})]
      (try
        (let [{id :instance-id} (submit! runtime {:op :cue/launch :cue-id :ember})
              {:keys [packet! press! lift! present! overlay cell]}
              (ratio-pad-session runtime :poly-aftertouch {position 14} {})
              period #(get-in (model runtime) [:instances id :params :period])]
          (is (= :accepted (:status (submit! runtime {:op :cue-variation/ratio-mode :instance-id id :mode :free}))))
          (is (= :accepted (:status (submit! runtime {:op :param/set :instance-id id :param :period :value current}))))
          (packet! [0x92 9 127])
          (is (= :free (:ratio-mode (overlay))))
          (let [{:keys [label selected?]} (cell position)]
            (is (string? label))
            (is (false? selected?))
            (present!)
            (press! 14)
            (is (not= current (period)))
            (is (= label (str (period)))))
          (lift! 14)
          (packet! [0x82 9 0]))
        (finally (show/stop! runtime))))))

(deftest push-3-ratio-chooser-names-its-cue-through-a-scheduled-selection-change
  (let [runtime (show/create-runtime {:model (fixture-model) :workspace-options nil})]
    (try
      (let [{ember :instance-id} (submit! runtime {:op :cue/launch :cue-id :ember})
            {:keys [device receive! packet! press! lift! present! view overlay]}
            (ratio-pad-session runtime :poly-aftertouch {[1 0] 13 [1 6] 14} {:launch-grid {:beats 1}})]
        (show/tick! runtime 100)
        (receive! [0x93 13 100])
        (show/tick! runtime 100)
        (let [pending (:pending-cue-launches (show/snapshot runtime))]
          (is (= [:merlot] (mapv :cue-id pending))))
        (packet! [0x92 9 127])
        (is (= "Ember · period" (:title (overlay))))
        (show/tick! runtime 510)
        (session/refresh! device)
        (is (= :merlot (get-in (view) [:cue :id])))
        (is (= "Ember · period" (:title (overlay))))
        (present!)
        (press! 14)
        (is (= 4/3 (get-in (model runtime) [:instances ember :params :period])))
        (is (= #{:ember :merlot} (set (map :cue-id (vals (:instances (model runtime)))))))
        (lift! 14)
        (packet! [0x82 9 0])
        (is (nil? (overlay)))
        (lift! 13)
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! runtime)))))

(defn- queue-captured-pad! [runtime receive! note]
  (let [before @(:ingress-order runtime)
        result (future (receive! [0x93 note 100]))
        deadline (+ (System/nanoTime) 3000000000)]
    (try
      (loop []
        (when (= before @(:ingress-order runtime))
          (when (> (System/nanoTime) deadline)
            (throw (ex-info "Captured pad did not enqueue its sample" {})))
          (Thread/sleep 2)
          (recur)))
      (show/tick! runtime 0)
      (when (= ::timeout (deref result 6000 ::timeout))
        (throw (ex-info "Captured sample did not receive an owner receipt" {})))
      (finally (future-cancel result)))))

(deftest push-3-free-only-schema-and-stale-queued-pad-samples
  (let [schema {:type :number :default 5/4 :bounds [1 2] :semantic/type :beat-ratio
                :ratio-components {:numerator [5 5] :denominator [4 4]}}
        configured (assoc-in (fixture-model) [:registry :effects :ember :params :period] schema)
        runtime (show/create-runtime {:model configured :workspace-options nil})]
    (try
      (let [{id :instance-id} (submit! runtime {:op :cue/launch :cue-id :ember})
            {:keys [device receive! packet! press! lift! present! overlay cell]}
            (ratio-pad-session runtime :mono-aftertouch {[1 6] 14} {})
            period #(get-in (model runtime) [:instances id :params :period])
            set-mode {:op :cue-variation/ratio-mode :instance-id id}
            write-approximation {:op :param/set :instance-id id :param :period :value 1.25}
            touch! #(packet! [0x92 9 127])
            release! #(do (packet! [0x82 9 0]) (present!))]
        (touch!)
        (is (nil? (overlay)))
        (release!)
        (is (= :accepted (:status (submit! runtime (assoc set-mode :mode :free)))))
        (is (= :accepted (:status (submit! runtime write-approximation))))
        (touch!)
        (is (= [{:position [1 6] :label "5/4" :selected? false}] (:cells (overlay))))
        (present!)
        (press! 14)
        (is (= 5/4 (period)))
        (is (true? (:selected? (cell [1 6]))))
        (lift! 14)
        (release!)
        (doseq [earlier [(assoc set-mode :mode :preferred) {:op :cue/delete :cue-id :ember}]]
          (is (= :accepted (:status (submit! runtime write-approximation))))
          (is (= :accepted (:status (submit! runtime (assoc set-mode :mode :free)))))
          (touch!)
          (is (= :free (:ratio-mode (overlay))))
          (present!)
          (let [prior (show/submit! runtime earlier)]
            (is (not (realized? prior)))
            (queue-captured-pad! runtime receive! 14)
            (is (= :accepted (:status @prior))))
          (is (= :stale-ratio-pad (get-in (session/snapshot device) [:last-ack :code])))
          (is (= 1.25 (period)))
          (is (nil? (overlay)))
          (lift! 14)
          (release!)
          (is (empty? (get-in (model runtime) [:control :edit-history :open]))))
        (let [forged (submit! runtime {:op :gesture/sample :instance-id id :param :period :value 5/4
                                       :source {:adapter :push-3 :session-id "push3-encoder-chooser"}
                                       :sample-context {}})]
          (is (= :operator-required (:code forged)))
          (is (= 1.25 (period))))
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest exact-ratios-and-banked-states-survive-the-owned-save-restore
  (let [runtime (show/create-runtime {:model (fixture-model) :workspace-options nil})]
    (try
      (let [{id :instance-id} (submit! runtime {:op :cue/launch :cue-id :ember})
            ratio {:op :cue-variation/ratio-set :instance-id id :param :period :numerator 5 :denominator 11}
            generation (:epoch (model runtime))
            projection #(show/cue-variation-preview runtime {:instance-id % :layout :rows})]
        (is (= :invalid-beat-ratio (:code (submit! runtime ratio))))
        (is (= :accepted (:status (submit! runtime {:op :cue-variation/ratio-mode :instance-id id :mode :free}))))
        (is (= :accepted (:status (submit! runtime ratio))))
        (is (= 5/11 (get-in (model runtime) [:instances id :params :period])))
        (is (= :invalid-beat-ratio (:code (submit! runtime (assoc ratio :denominator 11.0)))))
        (is (= :invalid-beat-ratio (:code (submit! runtime (assoc ratio :numerator 10 :denominator 22)))))
        (let [advance (fn [fine?] (:value (variations/edit-parameter (model runtime)
                                                (get-in (model runtime) [:instances id]) :period 1
                                                {:mode :relative :fine? fine?})))]
          (is (= (advance false) (advance true)))
          (is (not= 5/11 (advance true)))
          (is (or (integer? (advance true)) (ratio? (advance true)))))
        (let [p (projection id) command (assoc (choice p :cue-variation/append nil) :command-id "append-once")]
          (is (= (submit! runtime command) (submit! runtime command))))
        (let [source (show/export-show-source runtime)
              restore (show/submit! runtime :import {:source source})]
          (show/tick! runtime 0)
          (is (= :accepted (:status @restore)))
          (let [restored-id (:id (first (vals (:instances (model runtime)))))
                p (projection restored-id)
                old (assoc (choice p :cue-variation/apply [0 0]) :epoch generation)]
            (is (= :free (:ratio-mode p)))
            (is (= :stale-epoch (:code (submit! runtime old))))
            (is (= :accepted (:status (submit! runtime (assoc ratio :instance-id restored-id :numerator 1 :denominator 1)))))
            (is (= 1 (get-in (model runtime) [:instances restored-id :params :period])))
            (is (= :accepted (:status (submit! runtime (choice (projection restored-id) :cue-variation/apply [0 0])))))
            (is (= 5/11 (get-in (model runtime) [:instances restored-id :params :period]))))))
      (finally (show/stop! runtime)))))
