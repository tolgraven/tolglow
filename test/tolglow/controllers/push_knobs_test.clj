(ns tolglow.controllers.push-knobs-test
  "Linked knobs on virtual Push 2 and Push 3 surfaces (#666): captured MIDI
  through show/connect-device! and the Afterglow adapters, compared with the
  :knobs projection Studio reads."
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.devices.session :as session]
            [afterglow.control.devices.transports :as transports]
            [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.controllers.push-knobs :as push-knobs]
            [tolglow.show :as show])
  (:import [java.awt.image BufferedImage]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(def ^:private effects
  {:tint {:id :tint :version 1 :category :color
          :params {:saturation {:type :number :default 0.0 :bounds [0.0 1.0]}}
          :outputs {:dimmer {:op :param :key :saturation}}}
   :pulse {:id :pulse :version 1 :category :light
           :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
           :outputs {:dimmer {:op :param :key :level}}}})

(defn- live-model []
  (control/new-show
   {:show-id :push-knobs :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects effects
               :cues (into {} (for [id (keys effects)] [id {:id id :effect-id id :version 1}]))}
    :rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer 1}}]}}))

(def ^:private links
  [{:id "sat" :destination {:kind :param :cue-id :tint :param :saturation} :range [0.2 1.0]}
   {:id "rate" :destination {:kind :lfo :cue-id :pulse :param :level :field :period}
    :range [2 1/2] :quantize {:ratios :preferred}}])

(defn- submit! [rt time payload]
  (let [reply (show/submit! rt :command (merge {:source {:adapter :studio :session-id "t"}} payload))]
    (show/tick! rt time)
    (deref reply 2000 ::missing)))

(defn- setup! [rt]
  (let [launch #(:instance-id (submit! rt %2 {:op :cue/launch :cue-id % :launch {:mode :persistent}}))
        tint (launch :tint 0)
        pulse (launch :pulse 10)
        bound (let [reply (show/submit! rt :modulation-bind
                                        {:instance-id pulse :param :level
                                         :binding {:type :lfo :waveform :pulse :period {:beats 1}
                                                   :phase-reference :transport :inputs {:duty 0.5}
                                                   :from 0.0 :to 0.5}})]
                (show/tick! rt 20)
                (deref reply 2000 ::missing))
        created (submit! rt 30 {:op :knob/create :knob-id :look :name "Look" :links links})]
    (assert (= :accepted (:status bound)) (pr-str bound))
    (assert (= :accepted (:status created)) (pr-str created))
    {:tint tint :pulse pulse}))

(defn- while-ticking
  "Device input dispatches to the owner and waits for its receipt, so the
  owner must keep ticking while the captured callback runs."
  [rt time f]
  (let [operation (future (f))
        deadline (+ (System/nanoTime) 6000000000)]
    (try
      (loop []
        (cond (realized? operation) @operation
              (> (System/nanoTime) deadline) (throw (ex-info "Owner command did not settle" {}))
              :else (do (show/tick! rt time) (Thread/sleep 2) (recur))))
      (finally (future-cancel operation)))))

(defn- studio-knob
  "The knob as Studio sees it in the snapshot projection."
  [rt]
  (some #(when (= :look (:id %)) %) (get-in (show/snapshot rt) [:knobs :knobs])))

(defn- saturation [rt id]
  (get-in @(:state rt) [:model :instances id :params :saturation]))

(defn- same-values?
  "The device display shows the knob value and every link's resolved text and
  exact ratio as Studio's projection does."
  [display knob]
  (and (= (:value knob) (:value display))
       (= (mapv #(get-in % [:resolved :text]) (:links knob)) (mapv :text (:columns display)))
       (= (mapv #(get-in % [:resolved :ratio]) (:links knob)) (mapv :ratio (:columns display)))))

(deftest a-push-3-encoder-bound-to-a-knob-turns-it-and-shows-the-studio-values
  (let [rt (show/create-runtime {:model (live-model) :workspace-options nil})
        receive (atom nil)
        views (atom [])
        options {:session-id "push3-knob" :user-mode? true :expression-mode :poly-aftertouch :schedule! nil
                 :layout {:pads {[0 0] {:channel 3 :note 12}}
                          :encoder-touches {:look {:channel 2 :note 10}}
                          :encoders {:look {:channel 2 :controller 21}}}
                 :encoder-bindings {:look {:target :knob :knob-id :look}}
                 :display {:device :push-3 :input :view :open! (constantly :capture-display)
                           :render! (fn [_ view] (swap! views conj view)) :close! (fn [_])}
                 :midi {:open! (fn [callback] (reset! receive callback) :capture)
                        :send! (fn [_ _] (throw (ex-info "Unexpected Push 3 MIDI output" {})))
                        :close! (fn [_])}}]
    (try
      (let [{:keys [tint]} (setup! rt)
            device (show/connect-device! rt :push-3 options)
            input! (fn [time bytes] (while-ticking rt time #(@receive bytes)) (show/tick! rt time))
            projected #(push-knobs/project-display (:model @(:state rt)) (peek @views))]
        (testing "touching the encoder shows the knob with Studio's resolved values"
          (input! 100 [0x92 10 127])
          (is (= {:target :knob :knob-id :look :editable? true} (:touched-control (peek @views))))
          (is (same-values? (:knob (projected)) (studio-knob rt)))
          (is (= ["0.200" "2 beats"] (mapv :text (:columns (:knob (projected)))))))
        (testing "fifty detents move half the range and write every link"
          (input! 110 [0xb2 21 50])
          (is (< (Math/abs (- 0.5 (:value (studio-knob rt)))) 1e-9))
          (is (< (Math/abs (- 0.6 (saturation rt tint))) 1e-9))
          (is (same-values? (:knob (projected)) (studio-knob rt)))
          (is (= [1 1] (:ratio (second (:columns (:knob (projected))))))))
        (testing "a turn back is a relative nudge from the committed value"
          (input! 120 [0xb2 21 (- 128 10)])
          (is (< (Math/abs (- 0.4 (:value (studio-knob rt)))) 1e-9))
          (is (same-values? (:knob (projected)) (studio-knob rt))))
        (testing "the lift clears the knob display"
          (input! 130 [0x82 10 0])
          (is (nil? (:touched-control (peek @views))))
          (is (nil? (:knob (projected)))))
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! rt)))))

(deftest a-push-2-knob-page-turns-knobs-and-shows-the-studio-values
  (let [rt (show/create-runtime {:model (live-model) :workspace-options nil})
        input (atom nil)
        views (atom [])
        sent (atom [])
        capture {:open! (fn [receive!] (reset! input receive!) :capture)
                 :send! (fn [_ batch] (swap! sent into batch)) :close! (fn [_])}
        ;; The last value the page button's LED was sent (tolglow.surface.push-2).
        page-led #(last (filter (fn [m] (= [0xb0 52] (subvec m 0 2))) (filter vector? @sent)))
        display {:open! (constantly :capture-display)
                 :render! (fn [_ view] (swap! views conj view)) :close! (fn [_])}]
    (try
      (let [{:keys [tint]} (setup! rt)
            device (show/connect-device! rt :push-2 {:session-id "push2-knob" :midi capture :display display
                                                     :led-batch-size 16 :schedule! nil
                                                     :knob-page {:button 52 :knob-ids [:look :gone]}})
            input! (fn [time bytes] (while-ticking rt time #(@input bytes)) (show/tick! rt time))
            projected #(push-knobs/project-display (:model @(:state rt)) (peek @views))]
        (doseq [index (range 128)]
          (@input (assoc (codec/palette-write 0 index [0 0 0 0]) 6 4)))
        (is (= [0xb0 52 0] (page-led)) "The page button is dark until the page is shown")
        (testing "the page button shows the knobs, marking one that does not exist"
          (input! 100 [0xb0 52 127])
          (input! 101 [0xb0 52 0])
          (is (= [0xb0 52 127] (page-led)) "and lights while they are on the encoders")
          (is (= [{:knob-id :look :name "Look" :value 0.0 :text "0.000" :links 2}
                  {:knob-id :gone :missing? true}]
                 (get-in (projected) [:knob-page :knobs]))))
        (testing "a track encoder touch shows that knob as Studio does"
          (input! 110 [0x90 0 127])
          (is (= {:target :knob :knob-id :look :editable? true} (:touched-control (peek @views))))
          (is (same-values? (:knob (projected)) (studio-knob rt))))
        (testing "turns nudge the knob, a tenth with Shift, and write its links"
          (input! 120 [0xb0 71 25])
          (input! 130 [0xb0 49 127])
          (input! 131 [0xb0 71 5])
          (input! 132 [0xb0 49 0])
          (is (< (Math/abs (- 0.255 (:value (studio-knob rt)))) 1e-9))
          (is (< (Math/abs (- (+ 0.2 (* 0.8 0.255)) (saturation rt tint))) 1e-9))
          (is (same-values? (:knob (projected)) (studio-knob rt))))
        (testing "the empty slot and the missing knob change nothing"
          (input! 140 [0xb0 73 3])
          (is (< (Math/abs (- 0.255 (:value (studio-knob rt)))) 1e-9)))
        (testing "lifting and toggling the page off returns the encoders to parameters"
          (input! 150 [0x80 0 0])
          (input! 151 [0xb0 52 127])
          (is (nil? (:knob-page (peek @views))))
          (is (= [0xb0 52 0] (page-led)))
          (is (< (Math/abs (- 0.255 (:value (studio-knob rt)))) 1e-9)))
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! rt)))))

(deftest a-push-2-knob-page-draws-on-the-native-display
  ;; The native Push 2 display, with Afterglow's Wayang transport given an
  ;; image backend in place of the USB library: the frames it would send.
  (transports/render-view! (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB) {:title "Push 2"})
  (let [rt (show/create-runtime {:model (live-model) :workspace-options nil})
        input (atom nil)
        image (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB)
        frames (LinkedBlockingQueue.)
        pixel (fn [x y] (let [c (.getRGB image x y)]
                          [(bit-and 0xff (bit-shift-right c 16)) (bit-and 0xff (bit-shift-right c 8)) (bit-and 0xff c)]))
        backend {:open! (constantly image)
                 ;; The first knob's value bar, the second (missing) knob's column.
                 :send! #(.add frames [(pixel 20 126) (pixel 80 126) (pixel 148 126)])
                 :close! (fn [])}
        frame! (fn [expected]
                 (loop [n 0]
                   (let [frame (.poll frames 2 TimeUnit/SECONDS)]
                     (cond (= expected frame) frame
                           (or (nil? frame) (> n 50)) (throw (ex-info "Expected native frame" {:frame frame}))
                           :else (recur (inc n))))))]
    (try
      (setup! rt)
      (let [device (with-redefs-fn {#'transports/native-wayang (constantly backend)}
                     #(show/connect-device! rt :push-2 {:session-id "push2-native" :native-display? true
                                                        :led-batch-size 16 :schedule! nil
                                                        :midi {:open! (fn [receive!] (reset! input receive!) :capture)
                                                               :send! (fn [_ _]) :close! (fn [_])}
                                                        :knob-page {:button 52 :knob-ids [:look :gone]}}))
            input! (fn [time bytes] (while-ticking rt time #(@input bytes)) (show/tick! rt time))]
        (doseq [index (range 128)]
          (@input (assoc (codec/palette-write 0 index [0 0 0 0]) 6 4)))
        (input! 100 [0xb0 52 127])
        (input! 101 [0xb0 52 0])
        (testing "the page shows each knob's value as a bar; the missing knob has none"
          (is (= [[45 55 70] [45 55 70] [9 12 18]] (frame! [[45 55 70] [45 55 70] [9 12 18]]))))
        (testing "fifty detents fill half the bar"
          (input! 110 [0xb0 71 50])
          (is (= [[105 192 236] [45 55 70] [9 12 18]] (frame! [[105 192 236] [45 55 70] [9 12 18]]))))
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! rt)))))

(deftest the-knob-page-button-cannot-take-a-cue-variation-button
  (let [rt (show/create-runtime {:model (live-model) :workspace-options nil})]
    (try
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"knob page button"
                            (show/connect-device! rt :push-2 {:session-id "p" :cue-variations? true
                                                              :knob-page {:button 50 :knob-ids [:look]}})))
      (finally (show/stop! rt)))))

(deftest a-touched-knob-that-was-removed-is-marked-not-dropped
  (let [view {:touched-control {:target :knob :knob-id :gone} :knob-page {:knob-ids [:gone]}}
        projected (push-knobs/project-display (live-model) view)]
    (is (= {:knob-id :gone :missing? true} (:knob projected)))
    (is (= [{:knob-id :gone :missing? true}] (get-in projected [:knob-page :knobs])))
    (is (= {:title "Tolglow"} (push-knobs/project-display (live-model) {:title "Tolglow"}))
        "a view without knobs is unchanged")))

(deftest push-3-shift-turns-are-ten-times-finer-and-a-binding-can-set-its-step
  (let [rt (show/create-runtime {:model (live-model) :workspace-options nil})
        receive (atom nil)
        options {:session-id "push3-knob-shift" :user-mode? true :expression-mode :poly-aftertouch :schedule! nil
                 :layout {:pads {[0 0] {:channel 3 :note 12}}
                          :encoders {:look {:channel 2 :controller 21} :coarse {:channel 2 :controller 22}}
                          :buttons {:shift {:channel 4 :controller 24}}}
                 :encoder-bindings {:look {:target :knob :knob-id :look}
                                    :coarse {:target :knob :knob-id :look :step 1/4}}
                 :button-bindings {:shift :shift}
                 :midi {:open! (fn [callback] (reset! receive callback) :capture)
                        :send! (fn [_ _] (throw (ex-info "Unexpected Push 3 MIDI output" {})))
                        :close! (fn [_])}}]
    (try
      (setup! rt)
      (let [device (show/connect-device! rt :push-3 options)
            input! (fn [time bytes] (while-ticking rt time #(@receive bytes)) (show/tick! rt time))
            value #(:value (studio-knob rt))]
        (input! 100 [0xb2 21 10])
        (is (< (Math/abs (- 0.1 (value))) 1e-9) "ten coarse detents are a tenth of the range")
        (input! 110 [0xb4 24 127])
        (input! 120 [0xb2 21 10])
        (is (< (Math/abs (- 0.11 (value))) 1e-9) "with Shift held ten detents are a hundredth")
        (input! 130 [0xb2 22 1])
        (is (< (Math/abs (- 0.135 (value))) 1e-9) "a binding's own step, a tenth of it with Shift")
        (input! 140 [0xb4 24 0])
        (input! 150 [0xb2 22 2])
        (is (< (Math/abs (- 0.635 (value))) 1e-9))
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! rt)))))

(deftest push-3-knob-bindings-are-validated-before-connecting
  (doseq [binding [{:target :knob} {:target :knob :knob-id ""} {:target :knob :knob-id :a :step 0}
                   {:target :knob :knob-id :a :param :level}]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Knob encoder bindings"
                          (push-knobs/push-3-options {:encoder-bindings {:e binding}}))
        (pr-str binding)))
  (is (= {:encoder-bindings {:p {:param :level}}} (push-knobs/push-3-options {:encoder-bindings {:p {:param :level}}}))
      "options without knob bindings are unchanged"))

(deftest the-push-2-knob-page-options-are-validated
  (doseq [[page options] [[{:button 49 :knob-ids [:a]} {}] [{:button 75 :knob-ids [:a]} {}]
                          [{:button 57 :knob-ids [:a]} {}] [{:button 52 :knob-ids [:a]} {:temporary-encoder-modifier 52}]
                          [{:button 52 :knob-ids []} {}] [{:button 52 :knob-ids (vec (range 9))} {}]
                          [{:button 52 :knob-ids [:a] :step 0} {}] [{:button 52 :knob-ids [""]} {}]
                          [{:button 128 :knob-ids [:a]} {}] [{:button 52 :knob-ids [:a] :extra 1} {}]
                          [{:button 52 :knob-ids '(:a)} {}]]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Knob page" (push-knobs/push-2-encoder-overlay page options))
        (pr-str page options)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"knob page button"
                        (push-knobs/push-2-encoder-overlay {:button 58 :knob-ids [:a]} {:cue-variations? true})))
  (is (map? (push-knobs/push-2-encoder-overlay {:button 50 :knob-ids [:a "b"] :step 1/20} {}))))

(deftest the-push-2-knob-page-waits-for-every-track-encoder-touch-to-lift
  ;; A touch on a slot with no parameter records only its touch mode in
  ;; Afterglow. Switching the page under it would swallow the next parameter
  ;; touch on that slot; a held knob touch would be left behind.
  (let [rt (show/create-runtime {:model (live-model) :workspace-options nil})
        input (atom nil)
        views (atom [])]
    (try
      (setup! rt)
      (let [device (show/connect-device! rt :push-2 {:session-id "push2-knob-wait" :led-batch-size 16 :schedule! nil
                                                     :midi {:open! (fn [receive!] (reset! input receive!) :capture)
                                                            :send! (fn [_ _]) :close! (fn [_])}
                                                     :display {:open! (constantly :capture-display)
                                                               :render! (fn [_ view] (swap! views conj view))
                                                               :close! (fn [_])}
                                                     :knob-page {:button 52 :knob-ids [:look]}})
            input! (fn [time bytes] (while-ticking rt time #(@input bytes)) (show/tick! rt time))
            page? #(some? (:knob-page (peek @views)))]
        (doseq [index (range 128)]
          (@input (assoc (codec/palette-write 0 index [0 0 0 0]) 6 4)))
        (input! 100 [0xb0 57 127])
        (input! 101 [0x90 5 127])
        (input! 102 [0xb0 57 0])
        (input! 103 [0xb0 52 127])
        (input! 104 [0xb0 52 0])
        (is (not (page?)) "the page stays off while an empty slot is held with Accent")
        (input! 105 [0x80 5 0])
        (is (empty? (get-in (session/snapshot device) [:local :encoder-touch-modes])))
        (input! 106 [0xb0 52 127])
        (input! 107 [0xb0 52 0])
        (is (page?))
        (input! 110 [0x90 0 127])
        (input! 111 [0xb0 52 127])
        (input! 112 [0xb0 52 0])
        (is (page?) "the page stays on while a knob touch is held")
        (input! 113 [0x80 0 0])
        (input! 114 [0xb0 52 127])
        (is (not (page?)))
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! rt)))))
