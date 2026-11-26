(ns tolglow.qa.naut-hardware-test
  "Runs every Naut hardware-kit check against scripted virtual peers at the MIDI
  transport boundary. The addresses below are synthetic stand-ins for a device
  capture; they are not a claim about real Push 3 numbering."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.qa.naut-hardware :as kit]))

(def synthetic-push-3
  "Bytes a virtual Push 3 sends for each plan item."
  (merge (into {} (for [y (range 8) x (range 8)] [[:pad [x y]] [0x90 (+ 36 x (* 8 y)) 100]]))
         (into {} (for [i (range 1 9)]
                    {[:encoder-touch (keyword (str "encoder-" i))] [0x90 (dec i) 127]
                     [:encoder (keyword (str "encoder-" i))] [0xb0 (+ 70 i) 1]}))
         {[:encoder-touch :master] [0x90 8 127] [:encoder :master] [0xb0 79 1]
          [:button :left] [0xb0 44 127] [:button :right] [0xb0 45 127]
          [:button :up] [0xb0 46 127] [:button :down] [0xb0 47 127]
          [:button :mute] [0xb0 60 127] [:button :select] [0xb0 48 127] [:touch-strip :touch-strip] [0xe0 0 64]}))

(defn- capture-with-replay [plan]
  (let [{:keys [transport emit!]} (kit/virtual-peer)
        operator {:prompt! (fn [{:keys [kind id]} index _]
                             ;; A bounce of the previous pad and a release must not be learned.
                             (when (pos? index) (emit! [0x90 36 90]))
                             (let [bytes (synthetic-push-3 [kind id])]
                               (emit! bytes)
                               (when (= 0x90 (first bytes)) (emit! [0x80 (second bytes) 0]))))}]
    (kit/capture-layout! {:transport transport :plan plan :operator operator :timeout-ms 2000 :settle-ms 30})))

(def push-3-identity-reply
  "Push 3 (firmware 1.0 build 91) reply captured on 2026-11-26 from both the
  User and Live ports with Live quit. The four serial bytes are zeroed here."
  [0xf0 0x7e 0x01 0x06 0x02 0x00 0x21 0x1d 0x69 0x32 0x03 0x00 0x01 0x00 0x5b 0x00
   0x00 0x00 0x00 0x00 0x00 0x03 0x01 0x14 0x04 0xf7])

(def device-answers
  "Replies in the shape the hardware gave on 2026-11-26 (X-Touch/XT firmware
  V1.22 verbatim; MCU serial and challenge replaced with ASCII zeros)."
  {kit/identity-request [push-3-identity-reply]
   (kit/mcu-version-request 0x14) [[0xf0 0 0 0x66 0x14 0x14 0x56 0x31 0x2e 0x32 0x32 0xf7]]
   (kit/mcu-version-request 0x15) [[0xf0 0 0 0x66 0x15 0x14 0x56 0x31 0x2e 0x32 0x32 0xf7]]
   (kit/mcu-device-query 0x14) [(into [0xf0 0 0 0x66 0x14 0x01] (conj (vec (repeat 11 0x30)) 0xf7))]
   (kit/mcu-device-query 0x15) [(into [0xf0 0 0 0x66 0x15 0x01] (conj (vec (repeat 11 0x30)) 0xf7))]})

(deftest probes-record-device-replies-and-silence
  (doseq [check (keys kit/probes)]
    (testing check
      (let [{:keys [transport]} (kit/virtual-peer (fn [m] (concat [[0xfe] [0x90 104 127]] (get device-answers m))))
            receipt (kit/probe! check {:transport transport :timeout-ms 1000})]
        (is (= :answered (:verdict receipt)) (pr-str receipt))
        (is (= 2 (:ignored receipt)) "unrelated input arriving first is skipped")
        (is (= {"90" 1 "FE" 1} (get-in receipt [:other-input :by-status])))
        (is (= ["90 68 7F"] (mapv :hex (get-in receipt [:other-input :sample]))) "Active Sensing is counted, not sampled"))))
  (is (= {:device-id 1 :manufacturer "00 21 1D" :family "69 32" :model "03 00"
          :version "01 00 5B 00 00 00 00 00 00 03 01 14 04"
          :ableton {:software "1.0" :build 91 :serial-bytes 4 :trailing "00 03 01 14 04"}}
         (:decoded (kit/probe! :push-3/identity {:transport (:transport (kit/virtual-peer device-answers))}))))
  (is (= "V1.22" (get-in (kit/probe! :x-touch/firmware {:transport (:transport (kit/virtual-peer device-answers))})
                         [:decoded :ascii])))
  (is (= {:payload-bytes 11 :payload-ascii "00000000000" :serial-ascii "0000000" :challenge "30 30 30 30"}
         (:decoded (kit/probe! :x-touch-ext/device-query {:transport (:transport (kit/virtual-peer device-answers))}))))
  (is (= :no-reply (:verdict (kit/probe! :x-touch-ext/firmware {:transport (:transport (kit/virtual-peer))
                                                                 :timeout-ms 100})))))

(deftest probes-report-transport-failures-instead-of-throwing
  (let [failing {:open! (fn [_] (throw (ex-info "MIDI device discovery is still running." {:reason :midi/discovery-pending})))
                 :send! (fn [& _]) :close! (fn [_])}
        receipt (kit/probe! :x-touch/firmware {:transport failing})]
    (is (= :error (:verdict receipt)))
    (is (re-find #"discovery" (get-in receipt [:error :message]))))
  (let [closed (atom false)
        receipt (kit/probe! :x-touch/firmware {:transport {:open! (fn [_] :h)
                                                           :send! (fn [& _] (throw (java.io.IOException. "gone")))
                                                           :close! (fn [_] (reset! closed true))}})]
    (is (= :error (:verdict receipt)))
    (is @closed "the transport is closed after a failed send")))

(deftest layout-capture-learns-each-control-once
  (let [{:keys [layout skipped samples]} (capture-with-replay (kit/push-3-capture-plan))]
    (is (= [] skipped))
    (is (= 89 (count samples)))
    (is (= {:channel 0 :note 36} (get-in layout [:pads [0 0]])))
    (is (= {:channel 0 :note 99} (get-in layout [:pads [7 7]])))
    (is (= {:channel 0 :controller 71} (get-in layout [:encoders :encoder-1])))
    (is (= {:channel 0 :note 8} (get-in layout [:encoder-touches :master])))
    (is (= {:channel 0} (:touch-strip layout)))
    (is (= 64 (count (set (map :note (vals (:pads layout))))))))
  (testing "a silent control is skipped, not guessed"
    (let [{:keys [transport]} (kit/virtual-peer)
          result (kit/capture-layout! {:transport transport :plan (take 1 (kit/push-3-capture-plan))
                                       :operator {:prompt! (fn [& _])} :timeout-ms 100 :settle-ms 10})]
      (is (= [{:kind :pad :id [0 0]}] (:skipped result))))))

(deftest input-only-transport-refuses-to-send
  (let [transport (with-redefs [kit/port (fn [& _] {:index 0 :name "x" :description "y"})]
                    (kit/input-transport :push-3))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"never sends" ((:send! transport) nil [[0x90 1 1]])))))

(deftest push-3-pad-scenario-passes-on-the-captured-replay
  (let [{:keys [layout]} (capture-with-replay (kit/push-3-capture-plan {:pad-rows 1}))
        {:keys [transport emit! sent]} (kit/virtual-peer)
        receipt (kit/run-scenario! :push-3/pads {:transport transport :layout layout :step-timeout-ms 3000
                                                  :operator (kit/replay-operator emit! kit/push-3-bytes)})]
    (is (= :passed (:verdict receipt)) (pr-str (:steps receipt) (:session receipt)))
    (is (= [:tap :tap-ends :hold :pressure :select-running :encoder :blackout :blackout-off] (mapv :step (:steps receipt))))
    (is (= [] @sent) "the Push 3 session emits no bytes")
    (is (empty? (get-in receipt [:session :errors])))))

(deftest x-touch-scenarios-pass-on-replay
  (doseq [id [:x-touch/strips :x-touch-ext/strips]]
    (testing id
      (let [{:keys [transport emit! sent]} (kit/virtual-peer)
            receipt (kit/run-scenario! id {:transport transport :step-timeout-ms 3000
                                           :operator (kit/replay-operator emit! kit/x-touch-bytes)})]
        (is (= :passed (:verdict receipt)) (pr-str (:steps receipt) (:session receipt)))
        (is (every? #(= true (:answer %)) (filter :question (:steps receipt))))
        (is (seq @sent) "feedback reached the virtual unit")
        (is (every? #(= (if (= id :x-touch-ext/strips) 0x15 0x14) (nth % 4))
                    (filter #(= 0xf0 (first %)) @sent))
            "each unit's SysEx carries its own device ID")))))

(deftest motor-check-sweeps-and-records-silence-as-no-readback
  (let [{:keys [transport sent]} (kit/virtual-peer device-answers)
        receipt (kit/motor-check! {:transport transport :positions [0.75 0.0] :settle-ms 20})]
    (is (= :no-position-readback (:verdict receipt)))
    (is (= [0.75 0.0] (mapv :position (:steps receipt))))
    (is (= 12287 (:raw-target (first (:steps receipt)))) "check 4's 75% target")
    (is (= [0xe0 0 0] (last (filter #(= 0xe0 (first %)) @sent))) "strip 1 ends at zero")
    (is (= (kit/rest-state false) (vec (take-last (count (kit/rest-state false)) (butlast @sent))))
        "the unit is left at rest (every fader, master included, at zero) before the closing firmware probe")
    (is (= 9 (count (filter #(<= 0xe0 (first %) 0xe8) (kit/rest-state false))))))
  (let [{:keys [transport emit!]} (kit/virtual-peer (fn [m] (when (= 0xe0 (first m)) [[0xe0 (second m) (nth m 2)]])))
        receipt (kit/motor-check! {:transport transport :positions [0.5] :settle-ms 20})]
    (is (= :unresponsive (:verdict receipt)) "no firmware answer afterwards")
    (is (seq (:readback (first (:steps receipt)))) "echoed position messages are decoded")))

(deftest lcd-check-ends-at-rest
  (let [{:keys [transport sent]} (kit/virtual-peer device-answers)
        receipt (kit/lcd-check! {:transport transport :extender? true :hold-ms 5})]
    (is (= :sent (:verdict receipt)))
    (is (every? #(= 0x15 (nth % 4)) (filter #(= 0xf0 (first %)) @sent)))
    (is (= (kit/rest-state true) (vec (take-last (count (kit/rest-state true)) (remove #(= 0x13 (nth % 5 nil)) @sent)))))))

(deftest display-chart-receipt-counts-transfers
  (let [writes (atom [])
        backend {:open! (fn [_] {:descriptor {:display {:interface 0}}})
                 :write! (fn [_ ^bytes data] (swap! writes conj (alength data)))
                 :close! (fn [_])}
        receipt (kit/display-chart! {:bus 1 :address 12 :seconds 1 :backend backend})]
    (is (= :presented (:verdict receipt)) (pr-str receipt))
    (is (pos? (:frames receipt)))
    (is (= (:frames receipt) (get-in receipt [:transfers :header])))
    (is (= (* (:frames receipt) (+ 16 327680)) (get-in receipt [:transfers :bytes])))
    (is (number? (:first-frame-ms receipt)))
    (is (= 1 (get-in receipt [:transfers :closes]))))
  (let [receipt (kit/display-chart! {:bus 1 :address 12 :seconds 1
                                     :backend {:open! (fn [_] {}) :close! (fn [_])
                                               :write! (fn [& _] (throw (ex-info "Push 3 USB bulk-out failed (-7)"
                                                                                 {:operation :bulk-out :libusb-code -7})))}})]
    (is (= :failed (:verdict receipt)))
    (is (= 1 (count (:errors receipt))))
    (is (re-find #"-7" (get-in receipt [:failure :message])))))

(deftest push-3-mode-switches-and-reads-aftertouch-back
  (let [aftertouch (atom 2)
        {:keys [transport sent]} (kit/virtual-peer
                                   (fn [[_ _ _ _ _ _ command value]]
                                     (case command
                                       0x0a [[0xf0 0 0x21 0x1d 1 1 0x0a value 0xf7]]
                                       0x1e (do (reset! aftertouch value) nil)
                                       0x1f [[0xf0 0 0x21 0x1d 1 1 0x1f @aftertouch 0xf7]]
                                       nil)))]
    (is (= :passed (:verdict (kit/push-3-mode! :user {:transport transport :settle-ms 30}))))
    (is (= 1 @aftertouch))
    (let [receipt (kit/push-3-mode! :live {:transport transport :settle-ms 30})]
      (is (= :passed (:verdict receipt)))
      (is (= 2 (:aftertouch-mode receipt))))
    (is (= "F0 00 21 1D 01 01 0A 00 F7" (kit/hex (last (butlast @sent)))) "Live Mode is restored last, before the query")))

(deftest rate-check-measures-and-reprobes
  (let [{:keys [transport sent]} (kit/virtual-peer {(kit/mcu-version-request 0x14) [[0xf0 0 0 0x66 0x14 0x14 0x31 0xf7]]})
        receipt (kit/rate-check! {:transport transport :rates-hz [20] :seconds 1 :alternation-ms 50
                                  :operator {:confirm! (constantly true)}})]
    (is (= :responsive (:verdict receipt)))
    (is (< 10 (get-in receipt [:text 0 :messages]) 25))
    (is (< 10 (get-in receipt [:alternation :messages]) 25))
    (is (= [0xf0 0 0 0x66 0x14 0x72 7 7 7 7 7 7 7 7 0xf7]
           (last (filter #(= 0x72 (nth % 5 nil)) @sent))) "the alternation ends on stable white")))
