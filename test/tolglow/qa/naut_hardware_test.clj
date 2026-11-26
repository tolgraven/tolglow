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

(deftest probes-record-device-replies-and-silence
  (let [answers {kit/identity-request [[0xf0 0x7e 0x00 0x06 0x02 0x00 0x21 0x1d 0x01 0x01 0x02 0x00 0x01 0x05 0x04 0x00 0xf7]]
                 (kit/mcu-version-request 0x14) [[0xf0 0 0 0x66 0x14 0x14 0x31 0x2e 0x32 0x32 0x00 0xf7]]
                 (kit/mcu-version-request 0x15) [[0xf0 0 0 0x66 0x15 0x14 0x31 0x2e 0x30 0x30 0x00 0xf7]]}]
    (doseq [check (keys kit/probes)]
      (testing check
        (let [{:keys [transport]} (kit/virtual-peer (fn [m] (cons [0x90 104 127] (get answers m))))
              receipt (kit/probe! check {:transport transport :timeout-ms 1000})]
          (is (= :answered (:verdict receipt)) (pr-str receipt))
          (is (pos? (:ignored receipt)) "unrelated input arriving first is skipped"))))
    (is (= {:device-id 0 :manufacturer "00 21 1D" :family "01 01" :model "02 00" :version "01 05 04 00"}
           (:decoded (kit/probe! :push-3/identity {:transport (:transport (kit/virtual-peer answers))}))))
    (is (= "1.22?" (get-in (kit/probe! :x-touch/firmware {:transport (:transport (kit/virtual-peer answers))})
                           [:decoded :ascii])))
    (is (= :no-reply (:verdict (kit/probe! :x-touch-ext/firmware {:transport (:transport (kit/virtual-peer))
                                                                   :timeout-ms 100}))))))

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

(deftest rate-check-measures-and-reprobes
  (let [{:keys [transport sent]} (kit/virtual-peer {(kit/mcu-version-request 0x14) [[0xf0 0 0 0x66 0x14 0x14 0x31 0xf7]]})
        receipt (kit/rate-check! {:transport transport :rates-hz [20] :seconds 1 :alternation-ms 50
                                  :operator {:confirm! (constantly true)}})]
    (is (= :responsive (:verdict receipt)))
    (is (< 10 (get-in receipt [:text 0 :messages]) 25))
    (is (< 10 (get-in receipt [:alternation :messages]) 25))
    (is (= [0xf0 0 0 0x66 0x14 0x72 7 7 7 7 7 7 7 7 0xf7]
           (last (filter #(= 0x72 (nth % 5 nil)) @sent))) "the alternation ends on stable white")))
