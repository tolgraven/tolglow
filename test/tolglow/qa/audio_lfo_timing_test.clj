(ns tolglow.qa.audio-lfo-timing-test
  "Cycle-mode audio LFOs when the capture clock and the transport disagree
  (#912), through the offline replay and through the real capture worker and
  shape worker on simulated time: a transport running early or late, and a
  device sample clock drifting against the host clock."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.qa.audio-lfo :as qa]))

(deftest cycle-windows-stay-locked-across-transport-offsets
  (let [rows (qa/offset-sweep {:passages [:delay-synth] :bars 4 :offsets [-30.0 -20.0 -10.0 0.0 2.0 5.0 10.0]})
        known (fn [{:keys [bpm offset-ms]}] (and (= 180.0 bpm) (= -30.0 offset-ms)))]
    (is (= 42 (count rows)) "three tempos, seven offsets, two paths")
    (doseq [row (remove known rows)]
      (is (qa/sweep-passes? row) (pr-str row)))
    (testing "the known guard-related limit, so 180 BPM is accepted from 25 ms early to 10 ms late: audio 30 ms late puts the tail on an echo's attack"
      ;; The tail ends 40.4 ms before the bar line, where the delayed synth's
      ;; 3/8-beat echo of the previous note starts (41.7 ms at 180 BPM).
      (is (= 2 (count (filter known rows)))))
    (testing "the real workers shape the same windows as the replay, and the last one the replay stops short of"
      (doseq [[_ [replay capture]] (group-by (juxt :bpm :offset-ms) rows)]
        (is (<= 0 (- (:played capture) (:played replay)) 1) (pr-str replay capture))))))

(deftest a-drifting-device-clock-is-corrected-for-ten-minutes
  (doseq [ppm [-100.0 100.0]]
    (testing (str ppm " ppm")
      (let [{:keys [result status timestamp-error-ms extractions summary]} (qa/drift-run ppm {:minutes 10.0})
            drift (get-in status [:metrics :clock-drift])]
        (is (= {:status :completed} result))
        (is (< (:settled timestamp-error-ms) 1.0) "the newest blocks stay within 1 ms of their real capture times")
        (is (= :locked (:state drift)))
        (is (< (Math/abs (- ppm (:ppm drift))) 15.0) (pr-str drift))
        (is (< (Math/abs (- (* -1.0e-6 ppm 600000.0) (:correction-ms drift))) 1.0) "about 60 ms after ten minutes")
        (is (true? (:timing-valid? status)))
        (is (< 250 (count extractions)) "a two-second bar for ten minutes; the worker may skip one under load")
        (is (= (:played summary) (get-in summary [:statuses :ok])) (pr-str summary))
        (is (<= 0.8 (:min-confidence summary)))
        (is (every? #(< (Math/abs (- 82.5 %)) 1.0) (:hz summary)))))))

(deftest a-device-clock-beyond-the-bound-makes-timing-invalid
  (let [{:keys [result status extractions]} (qa/drift-run -1000.0 {:minutes 1.0})]
    (is (= {:status :completed} result))
    (is (= :out-of-bound (get-in status [:metrics :clock-drift :state])))
    (is (false? (:timing-valid? status)))
    (is (< (count extractions) 5) "no window is shaped once timing is invalid")))

(deftest a-line-that-falls-behind-is-measured-to-its-newest-frame
  ;; The line still holds what arrived late, so the delay to its newest frame
  ;; is unchanged: no step, no invalid timing, no timestamp error (#912).
  (let [{:keys [result status timestamp-error-ms summary]}
        (qa/drift-run -50.0 {:minutes 2.0 :backlog {:from-s 60.0 :to-s 63.0 :ms 20.0}})
        drift (get-in status [:metrics :clock-drift])]
    (is (= {:status :completed} result))
    (is (< (:settled timestamp-error-ms) 1.0))
    (is (= [:locked 0] [(:state drift) (:steps drift)]))
    (is (= (:played summary) (get-in summary [:statuses :ok])))))

(deftest a-device-clock-refuses-blocks-it-cannot-correct
  (let [{:keys [result]} (qa/drift-run 0.0 {:minutes 0.1 :block-frames 2048})]
    (is (= :error (:status result)))
    (is (= :invalid-config (get-in result [:error :code])))
    (is (re-find #"at most 1024" (get-in result [:error :message]))))
  (let [{:keys [result status]} (qa/drift-run 0.0 {:minutes 0.1 :block-frames 2048 :sample-clock :host})]
    (is (= {:status :completed} result) "a host-paced source keeps any block size")
    (is (false? (get-in status [:metrics :clock-drift :tracking?])))))

(deftest a-host-paced-source-is-not-corrected
  (let [{:keys [status timestamp-error-ms]} (qa/drift-run 0.0 {:minutes 0.25 :sample-clock :host})]
    (is (= {:tracking? false :sample-clock :host} (get-in status [:metrics :clock-drift])))
    (is (< (:max timestamp-error-ms) 1.0e-6))))
