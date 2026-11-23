(ns tolglow.perf.weighted-live-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.perf.weighted-live :as live]
            [tolglow.perf.weighted-live.transport :as transport]))

(deftest a-delayed-render-never-builds-a-queue-of-move-batches
  (let [issued (atom []) send (fn [] (let [r (vec (repeatedly 4 promise))] (swap! issued conj r) r))
        start {:pending [] :accepted 0 :rejected 0 :batches 0 :coalesced-ticks 0 :max-batch-size 0}
        one (live/advance-controls start send)]
    (doseq [p (take 3 (:pending one))] (deliver p {:status :accepted}))
    (let [held (nth (iterate #(live/advance-controls % send) one) 20)]
      (is (= 1 (count @issued)))
      (is (= 20 (:coalesced-ticks held)))
      (deliver (last (:pending one)) {:status :rejected})
      (is (= {:batches 2 :accepted 3 :rejected 1 :max-batch-size 4}
             (select-keys (live/advance-controls held send) [:batches :accepted :rejected :max-batch-size]))))))

(deftest artdmx-witnesses-use-relative-channel-offsets-and-signed-wire-bytes
  (let [rows (atom []) sink (transport/sink 128 (constantly 10.0) rows)
        pixel (byte-array 512) mover (byte-array 512)
        write! (fn [id] ((:send! sink) {1 pixel 2 mover} id))]
    (write! 1)
    (aset-byte pixel 511 (unchecked-byte 190))
    (aset-byte mover 5 (unchecked-byte 255))
    (aset-byte mover 6 (unchecked-byte 164))
    (write! 2)
    (let [[before after] @rows]
      (is (nil? (:lit-pixel before)))
      (is (= {:universe 1 :channel 512 :byte 190} (:lit-pixel after)))
      (is (= [255 164 0 0] (get-in after [:aim :first-head-bytes])))
      (is (not= (get-in before [:aim :crc32]) (get-in after [:aim :crc32]))))))

(deftest repeated-older-current-and-late-datagrams-have-distinct-outcomes
  (let [out (fn [slot id source]
              {:slot slot :deadline-ms (* 25 slot) :started-at-ms (* 25 slot) :finished-at-ms (+ 30 (* 25 slot)) :frame-id id :source-time-ms source
               :sent-at-ms (* 25 slot) :sent-by-ms (+ 24 (* 25 slot))})
        outputs [(out 1 0 nil) (out 2 3 50) (out 3 3 50)
                 {:slot 4 :deadline-ms 100 :started-at-ms 100 :finished-at-ms 101 :expired-before-send? true}
                 (out 5 4 100) (assoc (out 6 5 150) :sent-by-ms 178)]
        wire (mapv (fn [id t] {:id id :at-ms t}) [0 3 3 4 5] [25.5 52 76 126 177])
        joined (live/measured-outputs outputs wire)]
    (is (= [:idle :current :held nil :older :late-delivery] (mapv :source joined)))
    (is (= [nil 26.5 24 nil 50 51] (mapv :sink-gap-ms joined)))
    (is (nil? (:transport (nth joined 3))))
    (is (= 1 (:late-sink-entries (live/summarize [] joined 1 6))))))
