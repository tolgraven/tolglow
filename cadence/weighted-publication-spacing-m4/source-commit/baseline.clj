(in-ns 'afterglow.output.scheduler)

(defn- baseline-start-publications! [owner executor grid]
  (let [now (:now-ms owner)]
    (letfn [(queue-slot [wanted previous]
              (at! owner executor (timeline/deadline grid wanted) #(wake wanted previous)))
            (wake [wanted previous]
              ;; Repatch and blackout share this gate. Select the eligible slot
              ;; after any wait for them, so a stalled wake cannot send old work.
              (locking (:lock owner)
                (let [started (now) slot (timeline/slot-at grid started)]
                  (if (< slot wanted)
                    (queue-slot wanted previous)
                    (let [timing (send-slot! owner grid slot previous started)
                          ;; A thread can be preempted between checking the clock and
                          ;; entering the sink. It can also send part of a frame
                          ;; before throwing. Space from either completion.
                          handed-off (or (:sent-by-ms timing)
                                         (when (:failed? timing) (:finished-at-ms timing))
                                         started)]
                      (queue-slot (timeline/next-output-slot grid slot handed-off (now)) slot))))))]
      (queue-slot 1 0))))
