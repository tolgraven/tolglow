(ns tolglow.surface.push-capture-test
  "The Push Capture button (#671): recorded Push 2 and Push 3 MIDI through
  show/connect-device! runs the same retrospective capture as Studio's
  Capture button and show/capture-phrase!. Capture transports only; Live is
  not involved (sending a capture to Live is separate work)."
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.devices.session :as session]
            [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.show :as show]))

(def ^:private registry
  {:effects {:wash {:id :wash :version 1 :category :light
                    :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
                    :outputs {:dimmer {:op :param :key :level}}}}
   :cues {:wash {:id :wash :effect-id :wash :version 1}}})

(defn- runtime [options]
  (show/create-runtime
   (merge {:model (control/new-show {:show-id :push-capture :transport {:playing? true :beat 0.0 :bpm 120.0}
                                     :registry registry
                                     :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
           :sink {:send! (fn [_ _])} :workspace-options nil}
          options)))

(defn- at! [rt beat payload]
  (let [reply (show/submit! rt :command (assoc payload :source {:adapter :repl :session-id "capture"}))]
    (show/tick! rt (long (* 500 beat)))
    (deref reply 1000 ::missing)))

(defn- perform!
  "One launch of :wash from beat 1 to beat 3, then four more beats."
  [rt]
  (show/tick! rt 0)
  (let [id (:instance-id (at! rt 1 {:op :cue/launch :cue-id :wash :launch {:mode :persistent :params {:level 0.25}}}))]
    (at! rt 3 {:op :instance/end :instance-id id})
    (doseq [b (range 3 8)] (show/tick! rt (* 500 b)))))

(defn- transport [receiver sent]
  {:open! (fn [receive!] (reset! receiver receive!) :capture)
   :send! (fn [_ batch] (swap! sent into batch))
   :close! (fn [_])})

(defn- captures [rt] (:captures (show/capture-action! rt :status {})))

(deftest a-push-2-capture-button-captures-the-last-bars
  (let [rt (runtime {:phrase-capture {:bars 2}})
        receiver (atom nil) sent (atom [])]
    (try
      (perform! rt)
      (let [device (show/connect-device! rt :push-2 {:session-id "push2-capture" :schedule! nil :led-batch-size 16
                                                     :capture-button 87 :midi (transport receiver sent)})
            input! #(@receiver %)]
        (doseq [index (range 128)] (input! (assoc (codec/palette-write 0 index [0 0 0 0]) 6 4)))
        (is (zero? (captures rt)))
        (testing "a press captures once; the release does nothing"
          (input! [0xb0 87 127])
          (is (= 1 (captures rt)))
          (input! [0xb0 87 0])
          (is (= 1 (captures rt)))
          (is (= {:capture-id 1 :bars 2 :notes 1 :cues [:wash]}
                 (select-keys (:last (show/capture-action! rt :status {})) [:capture-id :bars :notes :cues]))
              "The same capture Studio shows, from the same buffer"))
        (testing "the next press is the next capture, and Studio's MIDI download is that capture"
          (input! [0xb0 87 127])
          (input! [0xb0 87 0])
          (is (= 2 (captures rt)))
          (is (= "tolglow-capture-2.mid" (:filename (show/capture-action! rt :smf {})))))
        (is (not-any? #(= [0xb0 87] (when (vector? %) (subvec % 0 2))) @sent) "The button sends nothing back")
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! rt)))))

(deftest a-push-3-capture-button-is-a-captured-user-mode-button
  (let [rt (runtime {:phrase-capture {:bars 2}})
        receiver (atom nil) sent (atom [])]
    (try
      (perform! rt)
      (let [device (show/connect-device! rt :push-3
                                         {:session-id "push3-capture" :user-mode? true :schedule! nil
                                          :expression-mode :poly-aftertouch
                                          :layout {:pads {[0 0] {:channel 0 :note 36}}
                                                   :buttons {:capture {:channel 0 :controller 65}}}
                                          :capture-button :capture :transport (transport receiver sent)})]
        (@receiver [0xb0 65 127])
        (@receiver [0xb0 65 0])
        (is (= 1 (captures rt)))
        (is (= [:wash] (get-in (show/capture-action! rt :status {}) [:last :cues])))
        (is (empty? @sent) "Nothing is sent to the Push")
        (is (empty? (:errors (session/snapshot device)))))
      (finally (show/stop! rt)))))

(deftest capture-buttons-are-checked-and-a-disabled-capture-keeps-the-session
  (let [rt (runtime {:phrase-capture false})
        receiver (atom nil) sent (atom [])]
    (try
      (testing "a Push 3 Capture button must be captured and unbound"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Capture button"
                              (show/connect-device! rt :push-3 {:session-id "p3" :user-mode? true :schedule! nil
                                                                :expression-mode :poly-aftertouch
                                                                :layout {:pads {[0 0] {:channel 0 :note 36}}}
                                                                :capture-button :capture
                                                                :transport (transport (atom nil) (atom []))}))))
      (let [device (show/connect-device! rt :push-2 {:session-id "push2-off" :schedule! nil
                                                     :capture-button 87 :midi (transport receiver sent)})]
        (doseq [index (range 128)] (@receiver (assoc (codec/palette-write 0 index [0 0 0 0]) 6 4)))
        (@receiver [0xb0 87 127])
        (is (empty? (:errors (session/snapshot device))) "A refused capture is logged, not a device error")
        (is (= :active (:status (session/snapshot device)))))
      (finally (show/stop! rt)))))
