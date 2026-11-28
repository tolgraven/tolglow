(ns tolglow.knob-presets-test
  "Knob looks (#547): eight saved sets of linked-knob positions, captured and
  recalled from the owner, Studio's command route, a virtual Push 2 and a
  captured Push 3 User Mode layout, with recall checked on captured ArtDmx."
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.devices.session :as session]
            [afterglow.control.display :as presentation]
            [afterglow.control.model :as control]
            [afterglow.output.artnet :as artnet]
            [afterglow.render.easing :as easing]
            [clojure.test :refer [deftest is testing]]
            [tolglow.controllers.push-variations :as push-variations]
            [tolglow.knob-presets :as presets]
            [tolglow.knobs :as knobs]
            [tolglow.persistence :as files]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.transit :as transit])
  (:import [java.io ByteArrayInputStream]))

(def ^:private effects
  {:tint {:id :tint :version 1 :category :color
          :params {:saturation {:type :number :default 0.0 :bounds [0.0 1.0]}
                   :hue {:type :color :default [1.0 0.0 0.0]}}
          :outputs {:color {:op :mix :args [{:op :vec3 :args [{:op :const :value 1.0} {:op :const :value 1.0}
                                                                {:op :const :value 1.0}]}
                                              {:op :param :key :hue :type :vec3}
                                              {:op :param :key :saturation}]}}}
   :aim {:id :aim :version 1 :category :movement
         :params {:pan {:type :number :default 0.5 :bounds [0.0 1.0]}
                  :tilt {:type :number :default 0.5 :bounds [0.0 1.0]}}
         :outputs {:pan {:op :param :key :pan} :tilt {:op :param :key :tilt}}}
   :pulse {:id :pulse :version 1 :category :light
           :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
           :outputs {:dimmer {:op :param :key :level}}}})

(defn- live-model []
  (control/new-show
   {:show-id :knob-looks :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects effects
               :cues (into {} (for [id (keys effects)] [id {:id id :effect-id id :version 1}]))}
    :rig {:fixtures [{:id :head :universe 1 :address 1
                      :channels {:red 1 :green 2 :blue 3 :dimmer 4 :pan 5 :tilt 6}}]}}))

(defn- harness []
  (let [packets (atom [])
        sink {:send! (fn [universes _]
                       (swap! packets conj (vec (artnet/artdmx-packet 1 1 (get universes 1)))))}]
    {:rt (show/create-runtime {:model (live-model) :sink sink :workspace-options nil})
     :packets packets}))

;; ArtDmx data starts at byte 18.
(defn- channel [packet n] (bit-and 255 (nth packet (+ 17 n))))

(def ^:private studio-source {:adapter :studio :session-id "knob-looks"})

(defn- submit! [rt time payload]
  (let [reply (show/submit! rt :command (merge {:source studio-source} payload))]
    (show/tick! rt time)
    (deref reply 2000 ::missing)))

(defn- model [rt] (:model @(:state rt)))

(def ^:private look-links
  "Colour saturation (linear) and an eased pan on one knob."
  [{:id "sat" :destination {:kind :param :cue-id :tint :param :saturation} :range [0.2 1.0]}
   {:id "pan" :destination {:kind :param :cue-id :aim :param :pan} :range [0.25 0.75] :curve :ease-in-out}])

(def ^:private rate-links
  "A musical LFO period on the dimmer, in preferred beat ratios."
  [{:id "rate" :destination {:kind :lfo :cue-id :pulse :param :level :field :period}
    :range [2 1/2] :quantize {:ratios :preferred}
    ;; #666 steps at once on a turn; #667 would give a stepped link its envelope by default.
    :envelope {}}])

(def ^:private preferred-steps [2 3/2 4/3 1 3/4 2/3 1/2])

(defn- expected [look rate]
  {:saturation (+ 0.2 (* 0.8 look))
   :pan (+ 0.25 (* 0.5 (easing/sample :ease-in-out look)))
   :period (nth preferred-steps (min 6 (long (Math/floor (+ (* rate 7) 1e-9)))))})

(defn- setup! [rt]
  (let [launch #(:instance-id (submit! rt %2 {:op :cue/launch :cue-id % :launch {:mode :persistent}}))
        ids {:tint (launch :tint 0) :aim (launch :aim 10) :pulse (launch :pulse 20)}
        reply (show/submit! rt :modulation-bind
                            {:instance-id (:pulse ids) :param :level
                             :binding {:type :lfo :waveform :pulse :period {:beats 1} :phase-reference :transport
                                       :inputs {:duty 0.5} :from 0.0 :to 1.0}})
        _ (show/tick! rt 25)
        bound (deref reply 2000 ::missing)]
    (assert (= :accepted (:status bound)) (pr-str bound))
    (doseq [[t command] [[30 {:op :knob/create :knob-id :look :name "Look" :links look-links}]
                         [40 {:op :knob/create :knob-id :rate :name "Rate" :links rate-links}]]]
      (let [ack (submit! rt t command)] (assert (= :accepted (:status ack)) (pr-str ack))))
    ids))

(defn- params [rt id] (get-in (model rt) [:instances id :params]))

(defn- period [rt id]
  (get-in (model rt) [:instances id :parameter-bindings :level :binding :period :beats]))

(defn- slot-view [rt slot] (get-in (show/snapshot rt) [:knob-presets :slots slot]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest capture-two-looks-and-recall-each-in-one-command-on-captured-dmx
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [{:keys [tint aim pulse]} (setup! rt)
            set-knob! (fn [t knob value] (is (= :accepted (:status (submit! rt t {:op :knob/set :knob-id knob :value value})))))
            check-look! (fn [look rate context]
                          (let [e (expected look rate)
                                packet (peek @packets)]
                            (is (close? (:saturation e) (:saturation (params rt tint))) context)
                            (is (close? (:pan e) (:pan (params rt aim))) context)
                            (is (= (:period e) (period rt pulse)) context)
                            (testing "the frame rendered right after the command already shows every destination"
                              (is (<= (Math/abs (- (channel packet 2) (* 255 (- 1.0 (:saturation e))))) 1.0) context)
                              (is (<= (Math/abs (- (channel packet 5) (* 255 (:pan e)))) 1.0) context))))]
        (testing "an empty bank projects eight empty slots"
          (is (= (repeat 8 :empty) (map :state (:slots (presets/projection (model rt)))))))
        (set-knob! 50 :look 0.2)
        (set-knob! 60 :rate 0.1)
        (let [ack (submit! rt 70 {:op :knob-preset/capture :slot 0 :expected nil})]
          (is (= :accepted (:status ack)))
          (is (= {:look 0.2 :rate 0.1} (:values ack))))
        (set-knob! 80 :look 0.8)
        (set-knob! 90 :rate 0.9)
        (is (= :accepted (:status (submit! rt 100 {:op :knob-preset/capture :slot 1}))))
        (is (= [:stored :selected :empty] (map #(:state (slot-view rt %)) [0 1 2])))
        (testing "an unrelated live parameter and a diverged destination before recall"
          (is (= :accepted (:status (submit! rt 110 {:op :param/set :instance-id tint :param :hue :value [0.0 0.0 1.0]}))))
          (is (= :accepted (:status (submit! rt 120 {:op :param/set :instance-id aim :param :tilt :value 0.9}))))
          (is (= :accepted (:status (submit! rt 130 {:op :param/set :instance-id aim :param :pan :value 0.05})))))
        (let [revision (:revision (model rt))
              ack (submit! rt 140 {:op :knob-preset/recall :slot 0 :expected {:look 0.2 :rate 0.1}})]
          (is (= :accepted (:status ack)) (pr-str ack))
          (is (= [[:look "sat"] [:look "pan"] [:rate "rate"]] (mapv :link-id (:links ack))))
          (is (every? #(= :applied (:status %)) (:links ack)) (pr-str (:links ack)))
          (is (< revision (:revision ack)))
          (is (= {:look 0.2 :rate 0.1} (update-vals (get-in (model rt) [:knobs :knobs]) :value)))
          (check-look! 0.2 0.1 "recall slot 0")
          (testing "unrelated parameters stay where they were put"
            (is (= [0.0 0.0 1.0] (:hue (params rt tint))))
            (is (= 0.9 (:tilt (params rt aim)))))
          (testing "mapping curves are links, not stored values, and survive recall"
            (is (= :ease-in-out (get-in (model rt) [:knobs :knobs :look :links 1 :curve])))
            (is (not (close? (+ 0.25 (* 0.5 0.2)) (:pan (params rt aim)))) "the pan is eased, not linear")))
        (is (= [:selected :stored] (map #(:state (slot-view rt %)) [0 1])))
        (is (true? (:matches? (slot-view rt 0))))
        (is (= :accepted (:status (submit! rt 150 {:op :knob-preset/recall :slot 1
                                                   :expected {"look" 0.8 "rate" 0.9}})))
            "a browser may send knob IDs back as text")
        (check-look! 0.8 0.9 "recall slot 1")
        (is (= [0.0 0.0 1.0] (:hue (params rt tint))))
        (testing "turning a knob keeps the selection but marks it moved"
          (set-knob! 160 :look 0.5)
          (is (= {:state :selected :matches? false} (select-keys (slot-view rt 1) [:state :matches?]))))
        (testing "refusals leave the bank and knobs untouched"
          (let [bank (presets/bank (model rt))]
            (is (= :knob-preset-occupied (:code (submit! rt 170 {:op :knob-preset/capture :slot 0}))))
            (is (= :stale-knob-preset (:code (submit! rt 180 {:op :knob-preset/recall :slot 0 :expected {:look 0.3}}))))
            (is (= :stale-knob-preset (:code (submit! rt 185 {:op :knob-preset/capture :slot 0 :overwrite? true
                                                              :expected nil}))))
            (is (= :stale-knob-preset (:code (submit! rt 186 {:op :knob-preset/recall :slot 0
                                                              :expected {:look 0.2 :rate 0.1 :extra 0.5}}))))
            (is (= :empty-knob-preset (:code (submit! rt 190 {:op :knob-preset/recall :slot 5}))))
            (is (= :invalid-knob-preset (:code (submit! rt 200 {:op :knob-preset/recall :slot 8}))))
            (is (= bank (presets/bank (model rt))))
            (is (= 0.5 (get-in (model rt) [:knobs :knobs :look :value])))))
        (testing "overwrite replaces a slot in place and selects it"
          (let [ack (submit! rt 210 {:op :knob-preset/capture :slot 0 :overwrite? true :expected {:look 0.2 :rate 0.1}})]
            (is (= [:accepted true] ((juxt :status :replaced?) ack))))
          (is (= {:look 0.5 :rate 0.9} (:values (slot-view rt 0))))
          (is (= [:selected :stored] (map #(:state (slot-view rt %)) [0 1]))))
        (testing "a knob created after capture is not touched; a removed knob is reported"
          (is (= :accepted (:status (submit! rt 220 {:op :knob/create :knob-id :tilt :name "Tilt" :value 0.3
                                                     :links [{:id "tilt" :destination {:kind :param :cue-id :aim :param :tilt}
                                                              :range [0.0 1.0]}]}))))
          (is (= :accepted (:status (submit! rt 230 {:op :knob/remove :knob-id :rate}))))
          (let [ack (submit! rt 240 {:op :knob-preset/recall :slot 1})]
            (is (= [:accepted [:rate]] ((juxt :status :missing) ack)))
            (is (= 0.9 (:tilt (params rt aim))) "the new knob never wrote, so tilt keeps its manual value")
            (is (= 0.3 (get-in (model rt) [:knobs :knobs :tilt :value]))))
          (is (= [{:knob-id :look :missing? false} {:knob-id :rate :missing? true}]
                 (mapv #(-> (select-keys % [:knob-id :missing?]) (update :missing? boolean)) (:knobs (slot-view rt 1)))))
          (is (false? (:matches? (slot-view rt 1)))))
        (testing "clearing the selected slot clears the selection"
          (is (= :accepted (:status (submit! rt 250 {:op :knob-preset/clear :slot 1}))))
          (is (= :empty (:state (slot-view rt 1))))
          (is (nil? (get-in (show/snapshot rt) [:knob-presets :selected])))))
      (finally (show/stop! rt)))))

(deftest a-look-with-no-surviving-knob-and-capture-without-knobs-are-refused
  (let [{:keys [rt]} (harness)]
    (try
      (is (= :invalid-knob-preset (:code (submit! rt 0 {:op :knob-preset/capture :slot 0}))))
      (is (= :accepted (:status (submit! rt 10 {:op :knob/create :knob-id :solo :name "Solo" :value 0.4}))))
      (is (= :accepted (:status (submit! rt 20 {:op :knob-preset/capture :slot 3}))))
      (is (= :accepted (:status (submit! rt 30 {:op :knob/remove :knob-id :solo}))))
      (is (= :knob-preset-unavailable (:code (submit! rt 40 {:op :knob-preset/recall :slot 3}))))
      (finally (show/stop! rt)))))

(deftest the-stale-check-compares-by-text-only-when-ids-stay-distinct
  (let [create (fn [model id value]
                 (:model (knobs/apply-operation model {:op :knob/create :knob-id id :name (str id) :value value})))
        model (-> (live-model) (create :look 0.2) (create "look" 0.2))
        saved (:model (presets/apply-operation model {:op :knob-preset/capture :slot 0}))
        recall (fn [expected]
                 (try (presets/apply-operation saved {:op :knob-preset/recall :slot 0 :expected expected})
                      :accepted
                      (catch clojure.lang.ExceptionInfo e (:control/error (ex-data e)))))]
    (is (= {:look 0.2 "look" 0.2} (get-in (presets/bank saved) [:slots 0 :values])))
    (is (= :accepted (recall {:look 0.2 "look" 0.2})) "the exact shown contents")
    (is (= :stale-knob-preset (recall {:look 0.2 :other 0.2}))
        "a knob the slot does not hold cannot pass by colliding as text")
    (is (= :stale-knob-preset (recall {"look" 0.2 :other 0.2})))
    (let [single (-> (live-model) (create :look 1.0)
                     (as-> m (:model (presets/apply-operation m {:op :knob-preset/capture :slot 0}))))]
      (is (= 0 (:slot (:result (presets/apply-operation single {:op :knob-preset/recall :slot 0
                                                                 :expected {"look" 1}}))))
          "text IDs and whole numbers from a browser still match"))))

(deftest replaying-a-recall-command-id-replays-its-receipt
  (let [{:keys [rt]} (harness)]
    (try
      (let [{:keys [aim]} (setup! rt)]
        (submit! rt 50 {:op :knob/set :knob-id :look :value 1.0})
        (submit! rt 60 {:op :knob-preset/capture :slot 2})
        (submit! rt 70 {:op :knob/set :knob-id :look :value 0.0})
        (let [command {:op :knob-preset/recall :slot 2 :command-id "recall-once"}
              first-ack (submit! rt 80 command)
              _ (submit! rt 90 {:op :param/set :instance-id aim :param :pan :value 0.1})
              replay (submit! rt 100 command)]
          (is (= :accepted (:status first-ack)))
          (is (= (:revision first-ack) (:revision replay)))
          (is (= (:links first-ack) (:links replay)))
          (is (= 0.1 (:pan (params rt aim))) "a replay does not write again")))
      (finally (show/stop! rt)))))

(deftest knob-looks-persist-and-recall-after-restore
  (let [{:keys [rt]} (harness)]
    (try
      (let [{:keys [tint]} (setup! rt)]
        (submit! rt 50 {:op :knob/set :knob-id :look :value 0.75})
        (submit! rt 60 {:op :knob-preset/capture :slot 4})
        (submit! rt 70 {:op :knob/set :knob-id :look :value 0.1})
        (submit! rt 80 {:op :knob-preset/capture :slot 6})
        (let [saved (model rt)
              restored (files/thaw (live-model) (files/freeze saved))]
          (testing "a full backup keeps the slots and the selection"
            (is (= (presets/bank saved) (presets/bank restored)))
            (is (= (presets/projection saved) (presets/projection restored)))))
        (testing "malformed banks are refused by show validation"
          (doseq [bad [{:slots {8 {:values {:look 0.5}}}}
                       {:slots {0 {:values {:look 1.5}}}}
                       {:slots {0 {:values {}}}}
                       {:slots {} :selected 0}
                       {:slots {0 {:values {:look 0.5} :extra 1}}}]]
            (is (thrown? clojure.lang.ExceptionInfo
                         (files/validate-model (assoc-in (model rt) [:extensions presets/state-key] bad)))
                (pr-str bad))))
        (let [source (show/export-show-source rt)
              restore (show/submit! rt :import {:source source})]
          (show/tick! rt 90)
          (is (= :accepted (:status @restore)))
          (let [tint (some #(when (= :tint (:cue-id %)) (:id %)) (vals (:instances (model rt))))]
            (is (= [:stored :selected] (map #(:state (slot-view rt %)) [4 6])))
            (is (= :accepted (:status (submit! rt 100 {:op :knob-preset/recall :slot 4}))))
            (is (close? (:saturation (expected 0.75 0)) (:saturation (params rt tint))))
            (is (= :selected (:state (slot-view rt 4)))))))
      (finally (show/stop! rt)))))

(defn- while-ticking [rt f]
  (let [operation (future (f))
        deadline (+ (System/nanoTime) 6000000000)]
    (try
      (loop []
        (cond (realized? operation) @operation
              (> (System/nanoTime) deadline) (throw (ex-info "Owner command did not settle" {}))
              :else (do (show/tick! rt 0) (Thread/sleep 2) (recur))))
      (finally (future-cancel operation)))))

(deftest studio-command-route-and-snapshot-read-the-same-bank
  (let [{:keys [rt]} (harness)]
    (try
      (setup! rt)
      (submit! rt 50 {:op :knob/set :knob-id :look :value 0.6})
      (let [handler (studio/handler rt)
            post (fn [payload]
                   (let [response (while-ticking rt
                                    #(handler {:request-method :post :uri "/api/command"
                                               :headers {"host" "localhost" "content-type" "application/transit+json"
                                                         "accept" "application/transit+json"}
                                               :body (ByteArrayInputStream.
                                                      (.getBytes (transit/encode (merge (select-keys (model rt) [:show-id :epoch])
                                                                                        payload)) "UTF-8"))}))]
                     [(:status response) (transit/decode (:body response))]))
            state (fn [] (let [response (handler {:request-method :get :uri "/api/state"
                                                  :headers {"host" "localhost" "accept" "application/transit+json"}})]
                           (transit/decode (:body response))))]
        (is (= 200 (first (post {:op :knob-preset/capture :slot 7}))))
        (submit! rt 60 {:op :knob/set :knob-id :look :value 0.0})
        (is (= 200 (first (post {:op :knob-preset/recall :slot 7}))))
        (is (= 0.6 (get-in (model rt) [:knobs :knobs :look :value])))
        (let [slot (get-in (state) [:knob-presets :slots 7])]
          (is (= {:slot 7 :state :selected :matches? true} (select-keys slot [:slot :state :matches?])))
          (is (= ["Look" "Rate"] (mapv :name (:knobs slot)))))
        (is (= 422 (first (post {:op :knob-preset/capture :slot 7})))))
      (finally (show/stop! rt)))))

;; ---------------------------------------------------------------------------
;; Push surfaces

(deftest overlay-cycle-reaches-knob-looks-after-cue-layouts-or-first-without-a-cue
  (let [input (:input push-variations/pad-overlay)
        model (live-model)
        step (fn [selection context] (:state (input selection {:type :control-change :controller 50 :raw-value 127}
                                                    model context)))]
    (is (= {:layout :knob-presets} (step nil {})))
    (is (= {:layout :knob-presets} (step {:instance-id "x" :layout :rows :row-page 0 :seed 0} {})))
    (is (nil? (step {:layout :knob-presets} {})))
    (is (= {:layout :knob-presets} (step {:layout :knob-presets} {:record? true})) "Record + Cycle does not close")
    (testing "non-pad controls stay with Push while knob looks are open"
      (is (nil? (input {:layout :knob-presets} {:type :control-change :controller 71 :raw-value 1} model {})))
      (is (nil? (input {:layout :knob-presets} {:type :note-off :note 92} model {}))))))

(defn- push-2-capture []
  (let [receive (atom nil) sent (atom []) views (atom []) awaiting? (atom false)
        ack (codec/sysex 0 [0x09])]
    {:midi {:open! #(do (reset! receive %) :capture)
            :send! (fn [_ batch] (swap! sent into batch)
                     (when (= ack (peek batch)) (reset! awaiting? true)))
            :close! (fn [_])}
     :display {:open! (constantly :capture) :render! (fn [_ view] (swap! views conj view)) :close! (fn [_])}
     :receive! #(@receive %)
     :messages sent :views views
     :palette! (fn [] (doseq [index (range 128)]
                      (@receive (assoc (codec/palette-write 0 index [0 0 0 73]) 6 4))))
     :settle! (fn []
                (loop [count 0]
                  (when (compare-and-set! awaiting? true false)
                    (when (>= count 9) (throw (ex-info "Push feedback did not settle" {})))
                    (@receive ack)
                    (recur (inc count)))))}))

(defn- last-pad-color
  "The palette colour most recently written for one pad."
  [messages x y]
  (let [note (codec/pad-note x y)
        by-message (into {} (map (fn [[state color]] [(codec/palette-write 0 note (conj color 73)) state]))
                         push-variations/knob-preset-colors)]
    (some by-message (rseq (vec messages)))))

(deftest virtual-push-2-saves-and-recalls-knob-looks-from-the-top-pad-row
  (let [{:keys [rt]} (harness)
        {:keys [receive! palette! settle! messages views] :as sink} (push-2-capture)]
    (try
      (let [{:keys [tint aim pulse]} (setup! rt)
            device (show/connect-device! rt :push-2
                     (merge {:session-id "knob-looks-push-2" :schedule! nil :timeout-ms 60000 :cue-variations? true}
                            (select-keys sink [:midi :display])))
            raw! #(while-ticking rt (fn [] (receive! %)))
            button! (fn [controller] (raw! [0xb0 controller 127]) (receive! [0xb0 controller 0]))
            pad! (fn [[x y]] (let [note (codec/pad-note x y)] (raw! [0x90 note 100]) (receive! [0x80 note 0])))
            overlay #(:pad-overlay (peek @views))
            cell-states #(mapv :state (:cells (overlay)))
            knob-values #(update-vals (get-in (model rt) [:knobs :knobs]) :value)]
        (palette!)
        (settle!)
        (is (true? (get-in (session/snapshot device) [:local :ready?])))
        (submit! rt 50 {:op :knob/set :knob-id :look :value 0.2})
        (submit! rt 60 {:op :knob/set :knob-id :rate :value 0.1})
        (testing "Note cycles the selected cue's full states and rows, then knob looks"
          (button! 50)
          (is (= :full (:layout (overlay))))
          (button! 50)
          (is (= :rows (:layout (overlay))))
          (button! 50))
        (is (= :knob-presets (:layout (overlay))))
        (is (= "Knob looks" (:title (overlay))))
        (pad! [0 7])
        (is (= :empty (:state (slot-view rt 0))) "a press before the pads are lit is ignored")
        (settle!)
        (is (= (repeat 8 :empty) (map #(last-pad-color @messages % 7) (range 8))))
        (pad! [0 7])
        (is (= {:look 0.2 :rate 0.1} (:values (slot-view rt 0))))
        (settle!)
        (is (= :selected (last-pad-color @messages 0 7)))
        (testing "change two knobs and save the second look"
          (submit! rt 70 {:op :knob/set :knob-id :look :value 0.8})
          (submit! rt 80 {:op :knob/set :knob-id :rate :value 0.9})
          (session/refresh! device)
          (settle!)
          (is (= :selected-moved (last-pad-color @messages 0 7)))
          (pad! [1 7])
          (settle!)
          (is (= [:stored :selected :empty] (take 3 (cell-states))))
          (is (= [:stored :selected] (map #(last-pad-color @messages % 7) [0 1]))))
        (is (= :accepted (:status (submit! rt 90 {:op :param/set :instance-id tint :param :hue :value [0.0 1.0 0.0]}))))
        (session/refresh! device)
        (settle!)
        (testing "one press recalls the first look across every linked target"
          (pad! [0 7])
          (is (= {:look 0.2 :rate 0.1} (knob-values)))
          (let [e (expected 0.2 0.1)]
            (is (close? (:saturation e) (:saturation (params rt tint))))
            (is (close? (:pan e) (:pan (params rt aim))))
            (is (= (:period e) (period rt pulse))))
          (is (= [0.0 1.0 0.0] (:hue (params rt tint))) "the unrelated hue stays")
          (is (= :push-2 (get-in (model rt) [:control :history (get-in (session/snapshot device) [:last-ack :command-id])
                                             :command :source :adapter]))))
        (settle!)
        (is (= [:selected :stored] (map #(last-pad-color @messages % 7) [0 1])))
        (pad! [1 7])
        (is (= {:look 0.8 :rate 0.9} (knob-values)))
        (settle!)
        (testing "Record with a saved pad overwrites it"
          (submit! rt 100 {:op :knob/set :knob-id :look :value 0.4})
          (session/refresh! device)
          (settle!)
          (receive! [0xb0 86 127])
          (pad! [0 7])
          (receive! [0xb0 86 0])
          (is (= {:look 0.4 :rate 0.9} (:values (slot-view rt 0))))
          (is (= {:look 0.8 :rate 0.9} (:values (slot-view rt 1)))))
        (settle!)
        (testing "a Studio write repaints the pads before Push can press the slot again"
          (is (= :accepted (:status (submit! rt 110 {:op :knob-preset/clear :slot 1}))))
          (session/refresh! device)
          (pad! [1 7])
          (is (= :empty (:state (slot-view rt 1))) "the stale lit pad is ignored until the repaint settles")
          (settle!)
          (is (= :empty (last-pad-color @messages 1 7))))
        (button! 50)
        (is (nil? (overlay)) "Cycle closes the knob looks")
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! rt)))))

(deftest captured-push-3-cycles-to-knob-looks-and-recalls-from-captured-top-row-pads
  (let [{:keys [rt]} (harness)
        callback (atom nil) surface (atom nil)]
    (try
      (let [{:keys [tint aim pulse]} (setup! rt)
            options {:session-id "knob-looks-push-3" :user-mode? true :schedule! nil :expression-mode :poly-aftertouch
                     :cue-variation-pads? true
                     :layout {:pads {[0 7] {:channel 3 :note 12} [1 7] {:channel 3 :note 13}
                                     [0 0] {:channel 3 :note 14}}
                              :buttons {:cycle {:channel 4 :controller 28} :record {:channel 4 :controller 29}}}
                     :button-bindings {:cycle :variation/cycle :record :variation/record}
                     :display {:device :push-3 :input :view :presentation-receipts? true
                               :open! (constantly :capture) :render! (fn [_ view] (reset! surface view))
                               :close! (fn [_])}
                     :midi {:open! #(do (reset! callback %) :capture) :close! (fn [_])
                            :send! (fn [_ _] (throw (ex-info "Unexpected Push 3 MIDI output" {})))}}
            device (show/connect-device! rt :push-3 options)
            packet! #(while-ticking rt (fn [] (@callback %)))
            button! (fn [cc] (packet! [0xb4 cc 127]) (packet! [0xb4 cc 0]))
            press! (fn [note] (packet! [0x93 note 100]) (packet! [0x83 note 0]))
            present! #(presentation/presented! @surface)
            overlay #(:pad-overlay @surface)
            knob-values #(update-vals (get-in (model rt) [:knobs :knobs]) :value)]
        (submit! rt 50 {:op :knob/set :knob-id :look :value 0.2})
        (submit! rt 60 {:op :knob/set :knob-id :rate :value 0.1})
        (button! 28)
        (is (= :full (:layout (overlay))) "the selected cue's layouts still come first")
        (present!)
        (button! 28)
        (is (= :rows (:layout (overlay))))
        (present!)
        (button! 28)
        (is (= :knob-presets (:layout (overlay))))
        (testing "only captured top-row pads are shown"
          (is (= [[0 7] [1 7]] (mapv :position (:cells (overlay)))))
          (is (= [:empty :empty] (mapv :state (:cells (overlay))))))
        (press! 12)
        (is (= :empty (:state (slot-view rt 0))) "a press before the frame is presented is ignored")
        (present!)
        (press! 12)
        (is (= {:look 0.2 :rate 0.1} (:values (slot-view rt 0))))
        (submit! rt 70 {:op :knob/set :knob-id :look :value 0.8})
        (submit! rt 80 {:op :knob/set :knob-id :rate :value 0.9})
        (session/refresh! device)
        (is (= [{:state :selected :matches? false} {:state :empty}]
               (mapv #(select-keys % [:state :matches?]) (:cells (overlay)))))
        (present!)
        (press! 13)
        (is (= {:look 0.8 :rate 0.9} (:values (slot-view rt 1))))
        (is (= :accepted (:status (submit! rt 90 {:op :param/set :instance-id tint :param :hue :value [0.0 1.0 0.0]}))))
        (session/refresh! device)
        (present!)
        (press! 12)
        (is (= {:look 0.2 :rate 0.1} (knob-values)))
        (let [e (expected 0.2 0.1)]
          (is (close? (:saturation e) (:saturation (params rt tint))))
          (is (close? (:pan e) (:pan (params rt aim))))
          (is (= (:period e) (period rt pulse))))
        (is (= [0.0 1.0 0.0] (:hue (params rt tint))))
        (is (= [:selected :stored] (mapv :state (:cells (overlay)))))
        (testing "Record held with a saved pad overwrites it"
          (submit! rt 100 {:op :knob/set :knob-id :look :value 0.4})
          (session/refresh! device)
          (present!)
          (packet! [0xb4 29 127])
          (press! 13)
          (packet! [0xb4 29 0])
          (is (= {:look 0.4 :rate 0.1} (:values (slot-view rt 1)))))
        (present!)
        (button! 28)
        (is (nil? (overlay)))
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! rt)))))
