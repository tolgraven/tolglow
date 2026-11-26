(ns tolglow.qa.xtouch-link-hardware
  "Hardware run for #705/#706: attach a real X-Touch and X-Touch-Ext as two
  independent surfaces, link them, bank, set one fader per unit and mirror,
  then leave both units at rest. Explicit operator action only:

    lein run -m tolglow.qa.xtouch-link-hardware [receipt.edn]

  Only the two MCU ports are opened. The show runtime has no rig output, so no
  DMX, Art-Net or sACN is produced. Every sent and received MIDI message is
  recorded with its time, and a timetable of what the units should show is
  printed as it happens."
  (:require [afterglow.control.codecs.x-touch :as mcu]
            [afterglow.control.devices.session :as session]
            [afterglow.control.devices.transports :as transports]
            [afterglow.control.model :as control]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [tolglow.show :as show])
  (:import [java.time Instant]))

(def ^:private units
  {:main {:match "X-Touch INT" :session-id "xt-main" :extender? false}
   :ext {:match "X-Touch-Ext" :session-id "xt-ext" :extender? true}})

(defn- port
  "Exactly one input and one output whose name ends with the unit's match,
  preferring CoreMIDI4J ports when both providers list the device."
  [listing match direction]
  (let [candidates (filterv #(and (str/ends-with? (:name %) match) (get % direction)) listing)
        preferred (filterv #(str/starts-with? (:name %) "CoreMIDI4J") candidates)
        chosen (if (seq preferred) preferred candidates)]
    (when-not (= 1 (count chosen))
      (throw (ex-info "Expected exactly one matching MIDI port" {:match match :direction direction :found chosen})))
    (first chosen)))

(defn- recording [transport unit log started]
  (let [stamp #(- (System/currentTimeMillis) started)]
    (assoc transport
           :open! (fn [receive!]
                    ((:open! transport) (fn [bytes]
                                          (swap! log conj {:ms (stamp) :unit unit :dir :in :bytes (vec bytes)})
                                          (receive! bytes))))
           :send! (fn [handle messages]
                    (swap! log into (map #(hash-map :ms (stamp) :unit unit :dir :out :bytes (vec %)) messages))
                    ((:send! transport) handle messages)))))

(defn- lcd-row
  "Decode the last top-row LCD text sent to a unit since `since` ms."
  [log unit since]
  (some->> @log
           (filter #(and (= unit (:unit %)) (= :out (:dir %)) (>= (:ms %) since)
                         (= [240 0 0 102] (take 4 (:bytes %))) (= [18 0] (take 2 (drop 5 (:bytes %))))))
           last :bytes (drop 7) butlast (map char) (apply str)))

(defn- model []
  (let [cues (mapv #(keyword (str "cue-" %)) (range 13 21))]
    (reduce (fn [m [n id]]
              (-> m
                  (assoc-in [:registry :effects id] {:id id :version 1 :params {:level {:type :number :default 0 :bounds [0 1]}}
                                                     :outputs {:dimmer {:op :param :key :level}}})
                  (assoc-in [:registry :cues id] {:id id :version 1 :effect-id id :priority 0 :name (format "S%02d" n)})))
            (control/new-show {:rig {:fixtures []}})
            (map vector (range 13 21) cues))))

(defn- wait [ms] (Thread/sleep (long ms)))

(defn -main [& [receipt-path]]
  (let [listing (transports/midi-ports)
        ports (into {} (for [[unit {:keys [match]}] units]
                         [unit {:in (port listing match :input?) :out (port listing match :output?)}]))
        started (System/currentTimeMillis)
        clock #(- (System/currentTimeMillis) started)
        log (atom [])
        timetable (atom [])
        runtime (show/create-runtime {:model (model) :workspace-options nil})
        running (atom true)
        ticker (doto (Thread. #(while @running (show/tick! runtime (clock)) (Thread/sleep 10)) "xtouch-705-owner")
                 (.setDaemon true) (.start))
        submit! (fn [payload] (deref (show/submit! runtime (merge {:source {:adapter :repl :session-id "xtouch-705"}} payload))
                                     3000 {:status :timeout}))
        step! (fn [label expected hold-ms f]
                (let [at (clock) result (f)]
                  (wait 300)
                  (let [entry {:t-s (/ (Math/round (/ at 100.0)) 10.0) :step label :expected expected
                               :main-lcd (lcd-row log :main at) :ext-lcd (lcd-row log :ext at)
                               :status (or (:status result) result)}]
                    (swap! timetable conj entry)
                    (println (format "%6.1fs  %-34s main=[%s] ext=[%s]  (%s)" (double (:t-s entry)) label
                                     (or (:main-lcd entry) "-") (or (:ext-lcd entry) "-") expected))
                    (flush)))
                (wait hold-ms))
        devices (atom {})]
    (println "Ports:" (pr-str (update-vals ports #(update-vals % :name))))
    (try
      (submit! {:op :effect-groups/configure
                :config {:manual (mapv #(hash-map :id (keyword (format "s%02d" %)) :name (format "S%02d" %) :level 0.0)
                                       (range 1 13))}})
      (step! "1 attach both (unlinked)" "main S01-S08, ext blank, faders at 0" 5000
             #(doseq [[unit {:keys [session-id extender?]}] units]
                (let [{:keys [in out]} (get ports unit)
                      transport (recording (transports/midi-transport {:input-index (:index in) :output-index (:index out)
                                                                       :expected-input-name (:name in)
                                                                       :expected-output-name (:name out)})
                                           unit log started)]
                  (swap! devices assoc unit
                         (show/connect-device! runtime :x-touch
                                               (cond-> {:session-id session-id :transport transport :refresh-ms 40
                                                        :extender? extender?}
                                                 (not extender?) (assoc :effect-group-view :manual
                                                                        :channels (mapv (fn [n] {:cue-id (keyword (str "cue-" n))})
                                                                                        (range 13 21)))))))))
      (step! "2 link extend right" "main S01-S08, ext S09-S16" 5000
             #(deref (show/link-surfaces! runtime "desk" ["xt-main" "xt-ext"]) 3000 :timeout))
      (step! "3 bank next (REPL/Studio command)" "main S17-S20 + 4 blank, ext blank" 5000
             #(deref (show/bank-surface! runtime "desk" :next) 3000 :timeout))
      (step! "4 SHIFT+BANK LEFT (main unit input)" "main S01-S08, ext S09-S16" 5000
             #(let [main (:main @devices)]
                (doseq [bytes [[0x90 70 127] [0x90 46 127] [0x80 46 0] [0x80 70 0]]]
                  (session/receive! main bytes))
                :injected))
      (step! "5 fader S01=0.75, S09=0.25" "main fader 1 at 75%, ext fader 1 at 25%" 6000
             #(do (submit! {:op :effect-groups/level-set :channel-id [:manual :s01] :value 0.75})
                  (submit! {:op :effect-groups/level-set :channel-id [:manual :s09] :value 0.25})))
      (step! "6 mirror link" "both S01-S08; ext fader 1 at 75%" 6000
             #(deref (show/link-surfaces! runtime "desk" ["xt-main" "xt-ext"] {:mode :mirror}) 3000 :timeout))
      (step! "7 levels back to 0, unlink" "faders at 0; main S01-S08, ext blank" 3000
             #(do (submit! {:op :effect-groups/level-set :channel-id [:manual :s01] :value 0.0})
                  (submit! {:op :effect-groups/level-set :channel-id [:manual :s09] :value 0.0})
                  (deref (show/unlink-surfaces! runtime "desk") 3000 :timeout)))
      (let [projection (:control-surfaces (show/snapshot runtime))]
        (swap! timetable conj {:step "projection before disconnect" :projection projection}))
      (step! "8 disconnect both" "LCDs blank, scribble colours off" 1000
             #(do (show/disconnect-device! runtime "xt-main") (show/disconnect-device! runtime "xt-ext") :closed))
      (finally
        (doseq [[unit device] @devices] (try (session/close! device) (catch Exception _)))
        (reset! running false)
        (show/stop! runtime)
        ;; Leave both units at rest: faders at 0, rings and LEDs off, LCD blank, colours black.
        (doseq [[unit {:keys [extender?]}] units]
          (let [{:keys [in out]} (get ports unit)
                transport (recording (transports/midi-transport {:input-index (:index in) :output-index (:index out)})
                                     unit log started)
                handle ((:open! transport) (fn [_]))]
            (try
              ((:send! transport) handle
               (concat (for [strip (range (if extender? 8 9))] (mcu/fader strip 0.0))
                       (for [strip (range 8)] (mcu/encoder-ring strip 0))
                       (for [note (range 0 104)] (mcu/button-led note :off))
                       [(mcu/colors extender? (vec (repeat 8 0)))
                        (mcu/text extender? 0 0 (apply str (repeat 56 " ")))
                        (mcu/text extender? 1 0 (apply str (repeat 56 " ")))]))
              (finally ((:close! transport) handle)))))
        (swap! timetable conj {:t-s (/ (Math/round (/ (clock) 100.0)) 10.0) :step "9 rest"
                               :expected "faders 0, LCD blank, LEDs off, rings off"})
        (println (format "%6.1fs  9 rest: faders 0, rings/LEDs off, LCD blank" (/ (clock) 1000.0)))
        (let [path (or receipt-path "target/xtouch-705/hardware-receipt.edn")
              receipt {:at (str (Instant/ofEpochMilli started)) :ports (update-vals ports #(update-vals % :name))
                       :timetable @timetable
                       :sent (frequencies (map (juxt :unit :dir) @log))
                       :inputs (filterv #(= :in (:dir %)) @log)
                       :log @log}]
          (io/make-parents path)
          (spit path (with-out-str (pprint/pprint receipt)))
          (println "Receipt:" path (pr-str (:sent receipt))))))))
