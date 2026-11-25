(ns tolglow.authoring.variation-control-test
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.devices.session :as session]
            [clojure.test :refer [deftest is testing]]
            [tolglow.authoring.variation-control :as variations]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.transit :as transit])
  (:import [java.io ByteArrayInputStream]))

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

(defn- while-ticking [runtime f]
  (let [operation (future (f))
        deadline (+ (System/nanoTime) (long 6000000000))]
    (try
      (loop []
        (cond (realized? operation) @operation
              (> (System/nanoTime) deadline) (throw (ex-info "Owner command did not settle" {}))
              :else (do (show/tick! runtime 0) (Thread/sleep 2) (recur))))
      (finally (future-cancel operation)))))

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
                (is (= ratio (get-in (current) [:params :period])))))
            (is (empty? (:errors (session/snapshot device)))))))
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
