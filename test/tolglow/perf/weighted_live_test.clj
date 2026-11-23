(ns tolglow.perf.weighted-live-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [tolglow.perf.weighted-live :as live]
            [tolglow.perf.weighted-live.transport :as transport])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

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

(deftest a-lit-and-moving-partial-show-cannot-pass-universe-coverage
  (let [rows (atom []) t (atom 24.0) sink (transport/sink 256 #(swap! t + 0.1) rows)
        pixel (byte-array 512) movers (byte-array 512) last-universe (byte-array 256)
        packets {1 pixel 3 movers 4 last-universe}
        send! (:send! sink)]
    (aset-byte pixel 1 (unchecked-byte 192))
    (send! {} 0)
    (send! packets 1)
    (aset-byte movers 5 (unchecked-byte 128))
    (send! packets 2)
    (let [[idle a b] @rows]
      (is (nil? (:incomplete-universes idle)))
      (is (= [2] (get-in a [:incomplete-universes :missing])))
      (is (seq (:lit-pixel a)))
      (is (not= (:aim a) (:aim b)))
      (let [pack (fn [slot delivery]
                   {:slot slot :deadline-ms (* 25 slot) :started-at-ms (* 25 slot)
                    :finished-at-ms (* 25 slot) :frame-id (:id delivery) :source :current
                    :sink-at-ms (* 25 slot) :transport delivery})
            ready (fn [i]
                    {:slot i :deadline-ms (* 25 i) :ready-at-ms (* 25 i) :cost-ms 1
                     :volume-positions (vec (repeat 4 [0 i 0]))})
            summary (live/summarize (map ready [1 2]) [(pack 1 a) (pack 2 b)] 1 2)]
        (is (= [2 2 2 2] (:distinct-volume-poses summary)))
        (is (= 2 (:distinct-delivered-aim-states summary) (:incomplete-packets summary)))
        (is (false? (:show-output-exercised? summary)))))
    (send! (assoc packets 2 (byte-array 32) 5 (byte-array 4)) 3)
    (is (= [5] (get-in (last @rows) [:incomplete-universes :unexpected])))
    (is (= {:minimum 512 :actual 32} (get-in (last @rows) [:incomplete-universes :short-payloads 2])))
    (send! (assoc packets 2 (byte-array 512)) 4)
    (is (nil? (:incomplete-universes (last @rows))))))

(deftest an-unqualified-capture-is-saved-before-the-command-fails
  (let [file (Files/createTempFile "weighted-capture" ".edn" (make-array FileAttribute 0))
        report {:summary {:show-output-exercised? false :incomplete-packets 1}}]
    (try
      (with-redefs [live/capture (constantly report)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"complete lit and moving output"
                             (live/-main (str file))))
        (is (= report (edn/read-string (slurp (.toFile file))))))
      (finally (Files/deleteIfExists file)))))

(deftest failed-receipt-matching-saves-the-unmatched-observations
  (let [file (Files/createTempFile "weighted-receipts" ".edn" (make-array FileAttribute 0))
        output {:slot 2 :frame-id 3 :sent-at-ms 50 :sent-by-ms 52}
        wire-row {:id 3 :at-ms 51}
        observations {:renders [{:slot 2 :ready-at-ms 49}]
                      :outputs [output] :missed-wakes [{:slot 1}]
                      :first-slot 1 :last-slot 2}
        cases [[[] "Sink receipt does not match its admission" {:output output :sink nil}]
               [[(assoc wire-row :id 7)] "Sink receipt does not match its admission"
                {:output output :sink (assoc wire-row :id 7)}]
               [[wire-row wire-row] "Unexpected capture-sink call" {:entry wire-row}]]]
    (try
      (doseq [[wire message data] cases]
        (with-redefs [live/capture (fn [_] (live/with-measured-outputs observations wire))]
          (let [failure (try (live/-main (str file))
                             (catch clojure.lang.ExceptionInfo e e))
                error {:message message :data data}]
            (is (= message (ex-message failure)))
            (is (= {:report (str file) :receipt-error error} (ex-data failure)))
            (is (= (assoc observations :wire wire :receipt-error error
                                       :summary {:show-output-exercised? false})
                   (edn/read-string (slurp (.toFile file))))))))
      (finally (Files/deleteIfExists file)))))
