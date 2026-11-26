(ns tolglow.qa.naut-display-features-test
  "Runs check 8 of the Naut kit with a replay USB backend and an injected
  capture in place of headless Chromium. The real projection, device
  sessions, overlays, browser catalog and shared sink all run."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [tolglow.qa.naut-display-features :as features])
  (:import [java.awt.image BufferedImage]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest every-state-is-presented-through-one-claim
  (let [opens (atom 0) closes (atom 0) writes (atom 0)
        out-dir (.toFile (Files/createTempDirectory "naut-check-8-" (make-array FileAttribute 0)))
        image (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB)
        receipt (try
                  (features/display-features!
                    {:bus 1 :address 12 :port 0 :hold-ms 300 :out-dir out-dir
                     :backend {:open! (fn [_] (swap! opens inc) {:descriptor {:replay true}})
                               :write! (fn [_ _] (swap! writes inc))
                               :close! (fn [_] (swap! closes inc))}
                     :capture {:open! (constantly :capture) :close! (fn [_])
                               :capture! (fn [_ _] image)}})
                  (finally (doseq [file (reverse (file-seq out-dir))] (io/delete-file file true))))
        by-id (into {} (map (juxt :state identity)) (:states receipt))]
    (is (= [1 1] [@opens @closes]) "one claim for every session")
    (is (pos? @writes))
    (is (= (mapv :id (features/states 300)) (mapv :state (:states receipt))))
    (is (= :presented (:verdict receipt)) (pr-str (:timetable receipt)))
    (is (every? #(pos? (:captured-frames %)) (:states receipt)))
    (is (every? #(pos? (get-in % [:transfers :payload])) (:states receipt)))
    (is (true? (get-in by-id [:variation-full :early-press :params-unchanged?]))
        "a pad pressed before the overlay was presented changes nothing")
    (is (seq (get-in by-id [:variation-full :presented-press :changed])) "the presented pad applies a variation")
    (is (true? (get-in by-id [:variation-rows :closed?])))
    (is (not= (get-in by-id [:ratio-chooser :chosen :period-before])
              (get-in by-id [:ratio-chooser :chosen :period-after])))
    (is (= ["Naut rehearsal" "Naut tour"]
           (sort (map #(re-find #"Naut \w+" %) (get-in by-id [:browser-shows :view :catalog :rows])))))
    (is (some #(re-find #"\[selected\]" %) (get-in by-id [:browser-back :view :catalog :rows])))
    (is (= :effect (get-in by-id [:browser-effects :view :catalog :kind])))
    (is (= ["encoder-2" "encoder-3"] (get-in by-id [:room-two-touch :device-touches])))
    (is (= [] (get-in by-id [:room-two-touch :view :touched])) "the Room projection has no touched parameters yet (#530)")
    (is (= [:level :tint] (get-in by-id [:parameters-two-touch :view :touched])))))
