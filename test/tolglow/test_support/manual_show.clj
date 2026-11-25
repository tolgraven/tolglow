(ns tolglow.test-support.manual-show
  (:require [tolglow.show :as show]))

(defn with-runtime! [options exercise!]
  (let [owner (show/create-runtime options)
        time-ms (get-in @(:state owner) [:model :time-ms])
        now-ms @#'show/now-ms
        submit! show/submit!]
    ;; Keep device startup on one logical frame. Run that same owner as soon
    ;; as a command is queued so synchronous MIDI and Ring can read its receipt.
    (with-redefs [show/now-ms (fn [runtime] (if (identical? runtime owner) time-ms (now-ms runtime)))
                  show/submit! (fn submit-at!
                                 ([runtime payload] (submit-at! runtime :command payload nil))
                                 ([runtime type payload] (submit-at! runtime type payload nil))
                                 ([runtime type payload authority]
                                  (let [receipt (submit! runtime type payload authority)]
                                    (when (identical? runtime owner) (show/tick! owner time-ms))
                                    receipt)))]
      (try
        (show/tick! owner time-ms)
        (exercise! owner)
        (finally (show/stop! owner))))))
