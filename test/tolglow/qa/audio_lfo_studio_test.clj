(ns tolglow.qa.audio-lfo-studio-test
  "The audio LFO browser check's paced input (#878) through the real capture
  worker and shape worker. Its passage must sit on the transport's bar lines
  as the capture timestamps them, or the cycle-mode LFO, which reads the band
  just before each bar line, hears the next bar's first note."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.audio-lfo :as lfo]
            [tolglow.audio.capture :as capture]
            [tolglow.audio.fixtures :as fixtures]
            [tolglow.qa.audio-lfo-studio :as studio]))

(deftest the-paced-input-keeps-the-passage-on-the-captured-bar-lines
  (let [bpm 120.0
        sample-rate 48000.0
        config {:extract :cycle :band :full :morph {:bars 2} :smoothing 0.25}
        specs (lfo/extractor-specs (lfo/configure {:transport {:beat 0.0 :bpm bpm :playing? true :meter {:beats-per-bar 4}}}
                                                  {:lfo-id :synth-cycle :config config}))
        passage (fixtures/passage :delay-synth {:bpm bpm :bars 4 :sample-rate sample-rate})
        origin (System/nanoTime)
        ;; A show clock that is not the JVM's monotonic clock, as in a show.
        now-ms #(+ 5000.0 (/ (- (System/nanoTime) origin) 1.0e6))
        ;; The first bar line is 70 ms after capture starts.
        transport {:beat (- 4.0 (* 0.07 (/ bpm 60.0))) :time-ms 5000.0
                   :bpm bpm :playing? true :meter {:beats-per-bar 4} :source :manual}
        shapes (promise)
        manager (capture/create-manager {:now-ms now-ms
                                         :open-source! (studio/paced-source now-ms (constantly transport) passage bpm
                                                                            (constantly false))})]
    (try
      (capture/start! manager {:default? true :sample-rate sample-rate :channels 1 :block-frames 480}
                      {:events! (constantly true) :clock! (constantly transport)
                       :shape-specs! (constantly specs) :shapes! #(deliver shapes %)})
      (let [extraction (:synth-cycle (deref shapes 10000 nil))]
        (testing "the first whole bar closes on the transport's grid"
          (is (= {:index 1 :start-beat 4.0 :end-beat 8.0}
                 (select-keys (:window extraction) [:index :start-beat :end-beat]))
              (pr-str extraction)))
        (testing "the bar's last note, 82.5 Hz, is found with the default confidence"
          (is (= :ok (:status extraction)) (pr-str (dissoc extraction :shape :input)))
          (is (<= 0.8 (or (:confidence extraction) 0.0)))
          (is (< (Math/abs (- 82.5 (or (:frequency-hz extraction) 0.0))) 1.0))))
      (finally (capture/close! manager)))))
