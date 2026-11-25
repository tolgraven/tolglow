(require '[clojure.edn :as edn])

(defn grid-slot [origin period time]
  (long (Math/floor (/ (- time origin) period))))

(defn classify [[previous next] origin period]
  (let [slot (:slot previous)
        gateway (or (:sent-by-ms previous)
                    (when (:failed? previous) (:finished-at-ms previous))
                    (:started-at-ms previous))
        requested (inc slot)
        after-finish (inc (grid-slot origin period (:finished-at-ms previous)))
        after-gap (long (Math/ceil (/ (- (+ gateway (/ period 2.0)) origin) period)))
        planned (max requested after-finish after-gap)
        actual (:slot next)]
    (mapv (fn [missing]
            {:slot missing :reason (cond
                                     (< missing after-finish) :previous-wake-or-send-overran
                                     (< missing after-gap) :minimum-handoff-spacing
                                     (>= missing planned) :subsequent-wake-or-lock-skipped)})
          (range requested actual))))

(doseq [path *command-line-args*]
  (let [{:keys [first-slot last-slot period-ms outputs summary]} (edn/read-string (slurp path))
        origin (- (:deadline-ms (first outputs)) (* (:slot (first outputs)) period-ms))
        missed (->> (partition 2 1 outputs)
                    (mapcat #(classify % origin period-ms))
                    (filter #(<= first-slot (:slot %) last-slot)))
        skipped (:no-publisher-row summary)]
    (prn {:report path :missing-publisher-slots skipped :matched (count missed)
          :causes (frequencies (map :reason missed))
          :on-wire-late (get-in summary [:source-counts :late-delivery] 0)})))
