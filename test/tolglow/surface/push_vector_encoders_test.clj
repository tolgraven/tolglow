(ns tolglow.surface.push-vector-encoders-test
  "Vector parameters spread over Push encoders (#813) through
  show/connect-device!: recorded Push 2 and Push 3 MIDI into the Afterglow
  codecs, profiles and session shell, the Tolglow surface add-on and a real
  show owner. Capture transports record every MIDI byte and display view;
  no device is opened."
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.devices.session :as device-session]
            [afterglow.control.devices.transports :as transports]
            [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.show :as show]
            [tolglow.surface.push-2 :as push-2]
            [tolglow.surface.push-2-display :as push-2-display]
            [tolglow.surface.push-3 :as push-3]
            [tolglow.surface.vector-encoders :as vectors])
  (:import [java.awt.image BufferedImage]))

(def ^:private bank
  "A full eight-parameter bank with a four-component ring vector in slot 2."
  [:a :b :offset :c :d :e :f :g])

(defn- model [bank]
  (control/new-show
   {:show-id :push-vectors :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects {:rig {:id :rig :version 1 :param-order bank
                               :params (assoc (into {} (for [k bank :when (not= k :offset)]
                                                         [k {:type :number :default 0.5 :bounds [0 1]}]))
                                              :offset {:type :vector :size 4 :default [0.0 0.0 0.0 0.0]
                                                       :bounds [0 1] :step 0.05 :name "Offset"})
                               :outputs {:dimmer {:op :param :key :a}}}}
               :cues {:c0 {:id :c0 :version 1 :effect-id :rig :name "Rig"}}}
    :rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer 1}}]}}))

(def ^:private led-query (codec/sysex 0 [0x09]))

(defn- harness [& [order]]
  (let [runtime (show/create-runtime {:model (model (or order bank)) :workspace-options nil})
        time (atom 0)
        receivers (atom {})
        sent (atom {})
        views (atom {})
        awaiting (atom #{})
        submit! show/submit!
        owner! (fn [f]
                 (with-redefs [show/now-ms (fn [& _] @time)
                               show/submit! (fn submit-and-tick
                                              ([owner command] (submit-and-tick owner :command command nil))
                                              ([owner type command] (submit-and-tick owner type command nil))
                                              ([owner type command authority]
                                               (let [receipt (submit! owner type command authority)]
                                                 (show/tick! owner (swap! time inc))
                                                 receipt)))]
                   (f)))]
    {:runtime runtime :sent sent :views views :time time :owner! owner! :receivers receivers
     :transport (fn [unit]
                  {:open! (fn [receive!] (swap! receivers assoc unit receive!) unit)
                   :send! (fn [_ batch]
                            (swap! sent update unit (fnil into []) batch)
                            (when (some #{led-query} batch) (swap! awaiting conj unit)))
                   :close! (fn [_] (swap! sent update unit (fnil conj []) :closed))})
     :display (fn [unit]
                {:open! (constantly unit)
                 :render! (fn [_ view] (swap! views update unit (fnil conj []) view))
                 :close! (fn [_])})
     :input! (fn [unit & messages] (owner! #(doseq [bytes messages] ((get @receivers unit) bytes))))
     :settle! (fn [unit]
                (owner! #(loop [count 0]
                           (when (contains? @awaiting unit)
                             (when (> count 30) (throw (ex-info "Push 2 LEDs did not settle" {:unit unit})))
                             (swap! awaiting disj unit)
                             ((get @receivers unit) led-query)
                             (recur (inc count))))))
     :tick! #(show/tick! runtime (swap! time inc))}))

(defn- submit! [{:keys [runtime tick!]} payload]
  (let [receipt (show/submit! runtime :command (merge {:source {:adapter :web :session-id "studio"}} payload))]
    (tick!)
    (deref receipt 1000 ::missing)))

(defn- params [{:keys [runtime]} id] (get-in @(:state runtime) [:model :instances id :params]))

(defn- view [{:keys [views]} unit] (peek (get @views unit)))

(defn- take-sent! [{:keys [sent]} unit]
  (let [messages (get @sent unit)]
    (swap! sent assoc unit [])
    (vec (filter vector? messages))))

(defn- buttons
  "The last [status value] each upper-row button CC (102-109) was sent, on
  any channel, in messages."
  [messages]
  (reduce (fn [acc [status cc value :as m]]
            (if (and (= 3 (count m)) (<= 0xb0 status 0xbf) (<= 102 cc 109)) (assoc acc cc [status value]) acc))
          {} messages))

(def ^:private palette
  "The palette this recorded Push 2 reports at connect, index -> [r g b w]:
  the four component LED colours (push/default-vector-colors) at 110-113,
  the factory white and red at 122 and 127, and every other entry black, as
  a previous owner might leave it."
  {110 [255 64 0 0] 111 [64 255 0 0] 112 [0 102 255 0] 113 [179 51 255 0]
   122 [255 255 255 0] 127 [255 0 0 0]})

(defn- palette-replies []
  (for [index (range 128)] (assoc (codec/palette-write 0 index (get palette index [0 0 0 0])) 6 4)))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(defn- close-vector? [expected actual]
  (and (= (count expected) (count actual)) (every? true? (map close? expected actual))))

(defn- connect-2! [{:keys [runtime transport display tick! input! settle!]} id & [options]]
  (let [device (show/connect-device! runtime :push-2
                                     (merge {:session-id id :schedule! nil :timeout-ms 60000 :led-batch-size 16
                                             :vector-encoders true :midi (transport id) :display (display id)}
                                            options))]
    (tick!)
    (apply input! id (palette-replies))
    (settle! id)
    device))

(defn- refresh! [{:keys [owner! settle!]} id device]
  (owner! #(push-2/refresh! device))
  (settle! id))

(defn- encoder-turn [slot detents] [0xb0 (+ 71 slot) (if (neg? detents) (+ 128 detents) detents)])

(defn- columns [v] (mapv #(when % (select-keys % [:param :component :component-label])) (:parameters v)))

(deftest a-full-push-2-bank-spreads-a-ring-onto-four-encoders-and-returns-exactly
  (let [{:keys [runtime time] :as h} (harness)]
    (try
      (let [id (:instance-id (submit! h {:op :cue/launch :cue-id :c0 :launch {:mode :persistent}}))
            device (connect-2! h "push2")
            _ (refresh! h "push2" device)
            before (view h "push2")]
        (testing "the bank is full: eight parameters, the ring vector in slot 2"
          (is (= bank (mapv :param (:parameters before))))
          (is (= {:index 0 :count 1 :total 8 :offset 0} (:parameter-bank before)))
          (is (nil? (:vector-encoders before))))
        (take-sent! h "push2")
        (testing "touching the ring's encoder spreads its four components over encoders 2-5"
          ((:input! h) "push2" [0x90 2 127])
          (refresh! h "push2" device)
          (let [v (view h "push2")]
            (is (= [{:param :a :component 0 :component-label :value} {:param :b :component 0 :component-label :value}
                    {:param :offset :component 0 :component-label "X"} {:param :offset :component 1 :component-label "Y"}
                    {:param :offset :component 2 :component-label "Z"} {:param :offset :component 3 :component-label "W"}
                    {:param :f :component 0 :component-label :value} {:param :g :component 0 :component-label :value}]
                   (columns v)))
            (is (= {:index 0 :count 1 :total 8 :offset 0} (:parameter-bank v)) "The bank itself is not changed")
            (is (= [{:instance-id id :param :offset :component 0 :encoder 2}] (:touched-parameters v)))
            (is (= [2 3 4 5] (mapv :encoder (get-in v [:vector-encoders :components]))))
            (is (= ["X" "Y" "Z" "W"] (mapv :label (get-in v [:vector-encoders :components]))))
            (is (= [true false false false] (mapv :touched? (get-in v [:vector-encoders :components]))))
            (testing "the REPL reads the open takeover and the projection last sent from the device snapshot"
              (let [local (:local (device-session/snapshot device))]
                (is (= {:instance-id id :param :offset :start 2 :touched #{2}}
                       (select-keys (get-in local [:tolglow.surface.push/vector :open]) [:instance-id :param :start :touched])))
                (is (= (:vector-encoders v) (get-in local [:tolglow.surface.push-2/vector-shown :vector-encoders]))))))
          (testing "the button under each taken encoder lights in the device palette's entry nearest its
                    component colour, and the touched one pulses (channel 9)"
            (is (= {104 [0xb9 110] 105 [0xb0 111] 106 [0xb0 112] 107 [0xb0 113]} (buttons (take-sent! h "push2"))))))
        (testing "each taken encoder edits only its component, by its step, through param/set"
          ((:input! h) "push2" (encoder-turn 3 2))
          (is (close-vector? [0.0 0.1 0.0 0.0] (:offset (params h id))))
          ((:input! h) "push2" (encoder-turn 5 1))
          (is (close-vector? [0.0 0.1 0.0 0.05] (:offset (params h id))))
          (testing "Shift is a tenth of the step; bounds clamp"
            ((:input! h) "push2" [0xb0 49 127] (encoder-turn 2 1) [0xb0 49 0])
            (is (close-vector? [0.005 0.1 0.0 0.05] (:offset (params h id))))
            ((:input! h) "push2" (encoder-turn 4 -3))
            (is (close-vector? [0.005 0.1 0.0 0.05] (:offset (params h id))) "Z stays at its lower bound"))
          (testing "the parameters the bank has under those encoders are untouched"
            (is (= [0.5 0.5 0.5] ((juxt :c :d :e) (params h id))))))
        (testing "the display shows each component's value"
          (refresh! h "push2" device)
          (is (= ["0.005" "0.100" "0.000" "0.050"]
                 (mapv :text (get-in (view h "push2") [:vector-encoders :components])))))
        (testing "a second touch inside the takeover, then lifting the first, keeps it open"
          ((:input! h) "push2" [0x90 4 127] [0x80 2 0])
          (refresh! h "push2" device)
          (is (= [{:instance-id id :param :offset :component 2 :encoder 4}] (:touched-parameters (view h "push2"))))
          (is (= {104 [0xb0 110] 106 [0xb9 112]} (buttons (take-sent! h "push2")))))
        (testing "a Studio edit of the vector shows on the Push and the next turn starts from it"
          (is (= :accepted (:status (submit! h {:op :param/set :instance-id id :param :offset :value [0.5 0.5 0.5 0.5]}))))
          (refresh! h "push2" device)
          (is (= ["0.500" "0.500" "0.500" "0.500"] (mapv :text (get-in (view h "push2") [:vector-encoders :components]))))
          ((:input! h) "push2" (encoder-turn 4 1))
          (is (close-vector? [0.5 0.5 0.55 0.5] (:offset (params h id)))))
        (testing "after the last lift the takeover lingers, then gives the encoders back"
          ((:input! h) "push2" [0x80 4 0])
          (refresh! h "push2" device)
          (is (some? (:vector-encoders (view h "push2"))) "still open while a finger could move to the next encoder")
          (swap! time + (inc vectors/default-linger-ms))
          (refresh! h "push2" device)
          (let [after (view h "push2")]
            (is (nil? (:vector-encoders after)))
            (is (= (columns before) (columns after)) "the exact bank is back")
            (is (= (:parameter-bank before) (:parameter-bank after)))
            (is (= (dissoc before :parameters :epoch :frame-number :active-effects :cells :preset)
                   (dissoc after :parameters :epoch :frame-number :active-effects :cells :preset))))
          (is (= {104 [0xb0 0] 105 [0xb0 0] 106 [0xb0 0] 107 [0xb0 0]} (buttons (take-sent! h "push2")))
              "only the lit buttons go dark")
          ((:input! h) "push2" (encoder-turn 3 10))
          (is (close? 0.6 (:c (params h id))) "encoder 3 edits the bank's parameter again")
          (is (close-vector? [0.5 0.5 0.55 0.5] (:offset (params h id)))))
        (testing "a touched encoder inside the window keeps the takeover closed"
          ((:input! h) "push2" [0x90 4 127] [0x90 2 127])
          (refresh! h "push2" device)
          (is (nil? (:vector-encoders (view h "push2"))))
          ((:input! h) "push2" [0x80 4 0] [0x80 2 0]))
        (is (empty? (:errors (device-session/snapshot device))))
        (show/disconnect-device! runtime "push2")
        (is (empty? (filter #(and (vector? %) (<= 102 (second %) 109)) (get @(:sent h) "push2")))
            "nothing was lit, so the disconnect sends no button bytes"))
      (finally (show/stop! runtime)))))

(deftest a-ring-near-the-right-edge-spreads-left-and-a-disconnect-darkens-its-buttons
  (let [{:keys [runtime] :as h} (harness [:a :b :c :d :e :f :offset :g])]
    (try
      (do
        (let [id (:instance-id (submit! h {:op :cue/launch :cue-id :c0 :launch {:mode :persistent}}))
              device (connect-2! h "push2" {:vector-encoders {:linger-ms 200}})]
          (refresh! h "push2" device)
          (take-sent! h "push2")
          ((:input! h) "push2" [0x90 6 127])
          (refresh! h "push2" device)
          (let [v (view h "push2")]
            (is (= [4 5 6 7] (mapv :encoder (get-in v [:vector-encoders :components]))))
            (is (= [{:instance-id id :param :offset :component 2 :encoder 6}] (:touched-parameters v))
                "the touched encoder edits the component under it"))
          ((:input! h) "push2" (encoder-turn 6 1))
          (is (close-vector? [0.0 0.0 0.05 0.0] (:offset (params h id))))
          (is (= {106 [0xb0 110] 107 [0xb0 111] 108 [0xb9 112] 109 [0xb0 113]} (buttons (take-sent! h "push2"))))
          (show/disconnect-device! runtime "push2")
          (is (= {106 [0xb0 0] 107 [0xb0 0] 108 [0xb0 0] 109 [0xb0 0]} (buttons (take-sent! h "push2"))))
          (is (empty? (:errors (device-session/snapshot device))))))
      (finally (show/stop! runtime)))))

(deftest the-native-push-2-display-draws-each-component-in-its-ring-colour
  ;; The native Push 2 display with Afterglow's Wayang transport given an
  ;; image backend in place of the USB library: the frames it would send.
  (transports/render-view! (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB) {:title "Push 2"})
  (let [{:keys [runtime transport tick! input! settle!] :as h} (harness)
        image (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB)
        frames (java.util.concurrent.LinkedBlockingQueue.)
        pixel (fn [x y] (let [c (.getRGB image x y)]
                          [(bit-and 0xff (bit-shift-right c 16)) (bit-and 0xff (bit-shift-right c 8)) (bit-and 0xff c)]))
        ;; Just inside the start of the value bar of columns 2-5.
        bars #(mapv (fn [column] (pixel (+ 8 (* 120 column) 1) 126)) [2 3 4 5])
        backend {:open! (constantly image) :send! #(.add frames (bars)) :close! (fn [])}
        frame! (fn [expected]
                 (loop [n 0]
                   (let [frame (.poll frames 2 java.util.concurrent.TimeUnit/SECONDS)]
                     (cond (= expected frame) frame
                           (or (nil? frame) (> n 50)) (throw (ex-info "Expected native frame" {:frame frame}))
                           :else (recur (inc n))))))
        track [45 55 70]]
    (try
      (let [id (:instance-id (submit! h {:op :cue/launch :cue-id :c0 :launch {:mode :persistent}}))
            device (with-redefs-fn {#'transports/native-wayang (constantly backend)}
                     #(show/connect-device! runtime :push-2 {:session-id "push2" :schedule! nil :timeout-ms 60000
                                                             :led-batch-size 16 :vector-encoders true
                                                             :native-display? true :midi (transport "push2")}))]
        (tick!)
        (apply input! "push2" (palette-replies))
        (settle! "push2")
        (is (= :accepted (:status (submit! h {:op :param/set :instance-id id :param :offset :value [0.5 0.0 0.5 0.0]}))))
        (testing "while open, each taken column has a bar in its ring colour; an empty component shows its track"
          (input! "push2" [0x90 2 127])
          (refresh! h "push2" device)
          (is (= [[232 160 131] track [137 185 224] track] (frame! [[232 160 131] track [137 185 224] track]))))
        (testing "a turn fills the component's own bar"
          (input! "push2" (encoder-turn 3 4))
          (refresh! h "push2" device)
          (is (= [[232 160 131] [174 211 154] [137 185 224] track]
                 (frame! [[232 160 131] [174 211 154] [137 185 224] track]))))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

;; ---------------------------------------------------------------------------
;; Push 3 User Mode

(def ^:private push-3-encoders (mapv #(keyword (str "e" %)) (range 8)))

(def ^:private push-3-layout
  "Captured User Mode addresses for eight encoders (touch notes 0-7, turn CCs
  71-78) and the eight buttons under them (CC 102-109), as a capture would
  name them. The addresses are the test's own, not a Push 3 capture."
  {:pads {[0 0] {:channel 0 :note 36}}
   :encoder-touches (into {} (map-indexed (fn [i id] [id {:channel 0 :note i}]) push-3-encoders))
   :encoders (into {} (map-indexed (fn [i id] [id {:channel 0 :controller (+ 71 i)}]) push-3-encoders))
   :buttons (into {} (for [i (range 8)] [(keyword (str "b" i)) {:channel 0 :controller (+ 102 i)}]))})

(defn- connect-3! [{:keys [runtime transport display tick!]} id & [options]]
  (let [device (show/connect-device! runtime :push-3
                                     (merge {:session-id id :user-mode? true :expression-mode :poly-aftertouch
                                             :schedule! nil :layout push-3-layout :transport (transport id)
                                             :display (assoc (display id) :device :push-3 :input :view)
                                             :encoder-bindings (zipmap push-3-encoders (map #(hash-map :param %) bank))
                                             :vector-encoders {:encoders push-3-encoders
                                                               :buttons (mapv #(keyword (str "b" %)) (range 8))}}
                                            options))]
    (tick!)
    device))

(defn- refresh-3! [{:keys [owner!]} device] (owner! #(push-3/refresh! device)))

(deftest a-full-push-3-bank-spreads-a-ring-onto-four-encoders-and-returns-exactly
  (let [{:keys [runtime time] :as h} (harness)]
    (try
      (let [id (:instance-id (submit! h {:op :cue/launch :cue-id :c0 :launch {:mode :persistent}}))
            device (connect-3! h "push3")
            _ (refresh-3! h device)
            before (view h "push3")]
        (is (= bank (mapv :param (:parameters before))) "every encoder is bound: a full bank")
        (take-sent! h "push3")
        (testing "touching the encoder bound to the ring spreads it over the next four encoders"
          ((:input! h) "push3" [0x90 2 127])
          (refresh-3! h device)
          (let [v (view h "push3")]
            (is (= [:a :b :offset :offset :offset :offset :f :g] (mapv :param (:parameters v))))
            (is (= [0 0 0 1 2 3 0 0] (mapv :component (:parameters v))))
            (is (= [{:instance-id id :param :offset :component 0 :encoder 2}] (:touched-parameters v))))
          (is (= {104 [0xb0 122] 105 [0xb0 126] 106 [0xb0 125] 107 [0xb0 123]} (buttons (take-sent! h "push3")))
              "nearest default-palette entries; W takes grey, not the touched white"))
        (testing "each encoder edits its own component; the scalars bound under them do not move"
          ((:input! h) "push3" (encoder-turn 4 2) (encoder-turn 5 1))
          (is (close-vector? [0.0 0.0 0.1 0.05] (:offset (params h id))))
          (is (= [0.5 0.5 0.5] ((juxt :c :d :e) (params h id)))))
        (testing "the lift starts the linger; after it the encoders edit their bindings again"
          ((:input! h) "push3" [0x80 2 0])
          (refresh-3! h device)
          (is (some? (:vector-encoders (view h "push3"))))
          (swap! time + (inc vectors/default-linger-ms))
          (refresh-3! h device)
          (is (= (columns before) (columns (view h "push3"))) "the exact bank is back")
          (is (= {104 [0xb0 0] 105 [0xb0 0] 106 [0xb0 0] 107 [0xb0 0]} (buttons (take-sent! h "push3"))))
          ((:input! h) "push3" (encoder-turn 4 10))
          (is (close? 0.6 (:d (params h id))))
          (is (close-vector? [0.0 0.0 0.1 0.05] (:offset (params h id)))))
        (testing "a lift that arrives after a forced close is still swallowed"
          ((:input! h) "push3" [0x90 2 127])
          (refresh-3! h device)
          (is (= :accepted (:status (submit! h {:op :instance/end :instance-id id :force? true}))))
          (refresh-3! h device)
          (is (nil? (:vector-encoders (view h "push3"))) "the launch ended, so the takeover closed")
          ((:input! h) "push3" [0x80 2 0])
          (is (empty? (get-in (device-session/snapshot device) [:local :tolglow.surface.push/vector :held]))))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest vector-encoder-options-are-checked-before-any-port-opens
  (let [{:keys [runtime] :as h} (harness)]
    (try
      (testing "Push 3 needs captured encoders and free buttons"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"vector-encoders"
                              (connect-3! h "p3" {:vector-encoders {:encoders [:e0 :nope]}})))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"vector-encoders"
                              (connect-3! h "p3" {:vector-encoders {:encoders [:e0 :e1] :buttons [:b0]}})))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"vector-encoders"
                              (connect-3! h "p3" {:vector-encoders true}))))
      (testing "Push 2 LED buttons must be eight CCs no other control uses"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"vector-encoders"
                              (connect-2! h "p2" {:vector-encoders {:buttons [20 21 22 23 24 25 26 27]}})))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"vector-encoders"
                              (connect-2! h "p2" {:vector-encoders {:linger-ms 0}})))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"vector-encoders"
                              (connect-2! h "p2" {:vector-encoders {:buttons (vec (range 102 110))
                                                                    :leds {:touched 300}}}))))
      (is (empty? @(:receivers h)) "no transport was opened")
      (finally (show/stop! runtime)))))

;; ---------------------------------------------------------------------------
;; Commands in flight, forced closes, channels and refusals

(deftest push-turns-in-flight-with-a-studio-set-keep-every-detent-and-the-studio-edit
  ;; The device input runs on its own thread and waits for each receipt
  ;; while this thread ticks the owner, as the show clock does: nothing
  ;; commits between a Push turn and the Studio set queued before it.
  (let [{:keys [runtime receivers] :as h} (harness)]
    (try
      (let [id (:instance-id (submit! h {:op :cue/launch :cue-id :c0 :launch {:mode :persistent}}))
            device (connect-2! h "push2")
            receive! (get @receivers "push2")]
        ((:input! h) "push2" [0x90 2 127])
        (refresh! h "push2" device)
        (let [studio (show/submit! runtime :command {:op :param/set :instance-id id :param :offset
                                                     :value [0.0 0.5 0.0 0.0]
                                                     :source {:adapter :web :session-id "studio"}})
              turns (future (dotimes [_ 3] (receive! (encoder-turn 2 1))) :done)
              deadline (+ (System/nanoTime) 6000000000)]
          (loop []
            (when-not (realized? turns)
              (when (> (System/nanoTime) deadline) (throw (ex-info "Push turns did not settle" {})))
              ((:tick! h))
              (Thread/sleep 1)
              (recur)))
          (is (= :done @turns))
          (is (= :accepted (:status (deref studio 1000 ::missing))))
          (is (close-vector? [0.15 0.5 0.0 0.0] (:offset (params h id)))
              "three detents moved X by three steps and the Studio edit of Y is kept"))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest a-push-2-takeover-closes-for-a-bank-page-the-knob-page-and-accent
  (let [{:keys [runtime] :as h} (harness (conj bank :h))]
    (try
      (let [id (:instance-id (submit! h {:op :cue/launch :cue-id :c0 :launch {:mode :persistent}}))
            device (connect-2! h "push2" {:knob-page {:button 52 :knob-ids [:look]}})
            open! (fn []
                    ((:input! h) "push2" [0x90 2 127])
                    (refresh! h "push2" device)
                    (is (some? (:vector-encoders (view h "push2")))))
            held (fn [] (get-in (device-session/snapshot device) [:local :tolglow.surface.push/vector :held]))]
        (testing "Shift + Page Right moves to the second bank: the takeover closes and that bank shows"
          (open!)
          ((:input! h) "push2" [0xb0 49 127] [0xb0 63 127] [0xb0 63 0] [0xb0 49 0])
          (refresh! h "push2" device)
          (is (nil? (:vector-encoders (view h "push2"))))
          (is (= 1 (get-in (view h "push2") [:parameter-bank :index])))
          (testing "the finger still on encoder 2 stays the takeover's until it lifts"
            (is (= #{2} (held)))
            ((:input! h) "push2" (encoder-turn 2 5))
            (is (close-vector? [0.0 0.0 0.0 0.0] (:offset (params h id))))
            (is (close? 0.5 (:h (params h id))) "the second bank's parameter under it does not move")
            ((:input! h) "push2" [0x80 2 0])
            (is (empty? (held))))
          ((:input! h) "push2" [0xb0 49 127] [0xb0 62 127] [0xb0 62 0] [0xb0 49 0])
          (refresh! h "push2" device)
          (is (= 0 (get-in (view h "push2") [:parameter-bank :index]))))
        (testing "the knob page takes the encoders: the takeover closes"
          (open!)
          ((:input! h) "push2" [0xb0 52 127] [0xb0 52 0])
          (refresh! h "push2" device)
          (is (nil? (:vector-encoders (view h "push2"))))
          (is (= [:look] (get-in (view h "push2") [:knob-page :knob-ids])))
          ((:input! h) "push2" [0x80 2 0] [0xb0 52 127] [0xb0 52 0])
          (refresh! h "push2" device))
        (testing "holding Accent (temporary edits) closes it; a turn right after the press is the profile's again"
          (open!)
          ((:input! h) "push2" [0xb0 57 127] (encoder-turn 3 4))
          (is (close-vector? [0.0 0.0 0.0 0.0] (:offset (params h id))) "encoder 3 no longer edits Y")
          (refresh! h "push2" device)
          (is (nil? (:vector-encoders (view h "push2"))))
          ((:input! h) "push2" [0x80 2 0] [0xb0 57 0]))
        (testing "a turn after the linger ran out, before any refresh, goes to the bank again"
          (open!)
          (let [c (:c (params h id))]
            ((:input! h) "push2" [0x80 2 0])
            (swap! (:time h) + (inc vectors/default-linger-ms))
            ((:input! h) "push2" (encoder-turn 3 10))
            (is (close-vector? [0.0 0.0 0.0 0.0] (:offset (params h id))))
            (is (close? (+ c 0.1) (:c (params h id))))))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest push-3-buttons-light-on-their-captured-channel-and-go-dark-on-disconnect
  (let [{:keys [runtime] :as h} (harness)
        layout (update push-3-layout :buttons update-vals #(assoc % :channel 3))]
    (try
      (submit! h {:op :cue/launch :cue-id :c0 :launch {:mode :persistent}})
      (let [device (connect-3! h "push3" {:layout layout})]
        (refresh-3! h device)
        (take-sent! h "push3")
        ((:input! h) "push3" [0x90 2 127])
        (refresh-3! h device)
        (is (= {104 [0xb3 122] 105 [0xb3 126] 106 [0xb3 125] 107 [0xb3 123]} (buttons (take-sent! h "push3"))))
        (show/disconnect-device! runtime "push3")
        (is (= {104 [0xb3 0] 105 [0xb3 0] 106 [0xb3 0] 107 [0xb3 0]} (buttons (take-sent! h "push3"))))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest vector-encoders-are-refused-in-push-3-mpe-mode-before-any-port-opens
  (let [{:keys [runtime receivers] :as h} (harness)]
    (try
      (let [error (try (connect-3! h "p3" {:expression-mode :mpe :zone {:manager-channel 0 :member-channels #{1 2}}
                                           :mpe-note-cells {36 [0 0]}})
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
        (is (= :invalid-vector-encoders (:push-3/error (ex-data error))))
        (is (empty? @receivers)))
      (finally (show/stop! runtime)))))
